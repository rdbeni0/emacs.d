;;; general.el --- Minimal in-house general.el replacement -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Lightweight drop-in replacement for noctuid/general.el covering only the
;; features used in this configuration:
;;
;;   - general-define-key  (:states :keymaps :major-modes :prefix)
;;   - which-key metadata  (:which-key / :ignore)
;;   - general-override-mode
;;   - general-auto-unbind-keys
;;
;; No external dependencies.  Pure Emacs Lisp + evil + which-key (optional).
;;
;; Design notes:
;;   * When :states is given, bindings are installed ONLY via
;;     evil-define-key* so they stay out of insert / minibuffer.
;;   * Nested key sequences share a single hierarchy of sparse keymaps
;;     built in the target keymap; that same hierarchy is then mirrored
;;     into each evil state.  This is required for which-key to show
;;     all children under a prefix (e.g. SPC p -> project commands).
;;
;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'cl-lib))


;;; Customization / state

(defvar general-override-mode-map (make-sparse-keymap)
  "Keymap used by `general-override-mode'.")

(define-minor-mode general-override-mode
  "Minor mode whose keymap overrides almost everything else.
Activate with (general-override-mode 1)."
  :global t
  :keymap general-override-mode-map
  :group 'general)

(with-eval-after-load 'evil
  (evil-make-overriding-map general-override-mode-map 'normal)
  (evil-make-overriding-map general-override-mode-map 'visual)
  (evil-make-overriding-map general-override-mode-map 'insert)
  (evil-make-overriding-map general-override-mode-map 'emacs)
  (evil-make-overriding-map general-override-mode-map 'motion)
  (evil-make-overriding-map general-override-mode-map 'operator)
  (evil-make-overriding-map general-override-mode-map 'replace)
  (add-hook 'general-override-mode-hook #'evil-normalize-keymaps))


;;; Automatic key unbinding

(defvar general--auto-unbind nil)

(defun general--unbind-prefix-keys (keymap key)
  "Unbind every proper prefix of KEY in KEYMAP so KEY can be bound."
  (when (and (keymapp keymap) (vectorp key) (> (length key) 1))
    (dotimes (i (1- (length key)))
      (let ((prefix (substring key 0 (1+ i))))
        (when (and (lookup-key keymap prefix)
                   (not (keymapp (lookup-key keymap prefix))))
          (define-key keymap prefix nil))))))

(defun general--define-key-advice (orig-fun keymap key def &rest args)
  "Advice for `define-key' that auto-unbinds conflicting prefixes."
  (when (and general--auto-unbind (keymapp keymap))
    (let ((raw (cond
                ((vectorp key) key)
                ((stringp key) (kbd key))
                (t (kbd (format "%s" key))))))
      (general--unbind-prefix-keys keymap raw)))
  (apply orig-fun keymap key def args))

;;;###autoload
(defun general-auto-unbind-keys (&optional disable)
  "Advise `define-key' so binding a key auto-unbinds its prefixes."
  (if disable
      (progn
        (advice-remove 'define-key #'general--define-key-advice)
        (setq general--auto-unbind nil))
    (advice-add 'define-key :around #'general--define-key-advice)
    (setq general--auto-unbind t)))


;;; Helpers

(defun general--normalize-list (x)
  "Return X as a flat list of items.
Expand symbols whose value is a list (e.g. list-gen-mode-map-*)."
  (cond
   ((and (symbolp x) (boundp x) (listp (symbol-value x)))
    (symbol-value x))
   ((listp x) x)
   (t (list x))))

(defun general--resolve-keymap (sym)
  "Return the keymap object for SYM, or nil."
  (cond
   ((keymapp sym) sym)
   ((eq sym 'global) (current-global-map))
   ((eq sym 'override) general-override-mode-map)
   ((and (symbolp sym) (boundp sym) (keymapp (symbol-value sym)))
    (symbol-value sym))
   (t nil)))

(defun general--parse-def (def)
  "Parse DEF into (COMMAND . WHICH-KEY-DESCRIPTION)."
  (cond
   ((and (listp def) (keywordp (car def)))
    (let ((desc (or (plist-get def :which-key)
                    (plist-get def :wk))))
      (cons :ignore desc)))
   ((and (listp def)
         (or (plist-member (cdr def) :which-key)
             (plist-member (cdr def) :wk)))
    (let* ((cmd  (car def))
           (desc (or (plist-get (cdr def) :which-key)
                     (plist-get (cdr def) :wk))))
      (cons cmd desc)))
   (t (cons def nil))))

(defun general--which-key-register (key-str desc)
  "Register KEY-STR -> DESC with which-key (key-based)."
  (when (and desc (featurep 'which-key)
             (fboundp 'which-key-add-key-based-replacements))
    (condition-case nil
        (which-key-add-key-based-replacements key-str desc)
      (error nil))))

(defun general--unwrap (def)
  "If DEF is a (STRING . REAL) cons, return REAL; else DEF."
  (if (and (consp def) (stringp (car def)))
      (cdr def)
    def))

(defun general--lookup-raw (keymap keyseq)
  "Look up KEYSEQ in KEYMAP, unwrapping a which-key cons if present."
  (general--unwrap (lookup-key keymap keyseq t)))

(defun general--ensure-path (keymap keyseq)
  "Ensure KEYSEQ is a path of sparse keymaps inside KEYMAP.
Returns the final (innermost) sparse keymap.
Does not overwrite an existing keymap at the last position;
creates missing intermediate maps as needed."
  (let ((map keymap)
        (len (length keyseq)))
    (dotimes (i len)
      (let* ((ev  (aref keyseq i))
             (vec (vector ev))
             (cur (general--lookup-raw map vec)))
        (if (keymapp cur)
            (setq map cur)
          (let ((new-map (make-sparse-keymap)))
            (define-key map vec new-map)
            (setq map new-map)))))
    map))

(defun general--bind-in-hierarchy (keymap keyseq cmd desc)
  "Bind KEYSEQ inside KEYMAP, sharing the nested keymap hierarchy.
CMD is :ignore or a command symbol/function.
DESC is an optional which-key label.

Returns the definition that was actually stored (possibly a cons)."
  (let ((len (length keyseq)))
    (if (= len 0)
        nil
      (if (= len 1)
          ;; Single key
          (let ((def (if (eq cmd :ignore)
                         (let ((m (or (and (keymapp (general--lookup-raw keymap keyseq))
                                           (general--lookup-raw keymap keyseq))
                                      (make-sparse-keymap))))
                           (if desc (cons desc m) m))
                       (if desc (cons desc cmd) cmd))))
            (define-key keymap keyseq def)
            def)
        ;; Multi-key: ensure parent path, then bind final event into parent
        (let* ((parent-seq (substring keyseq 0 (1- len)))
               (parent     (general--ensure-path keymap parent-seq))
               (final-ev   (vector (aref keyseq (1- len))))
               (existing   (general--lookup-raw parent final-ev))
               (def (if (eq cmd :ignore)
                        (let ((m (if (keymapp existing) existing
                                   (make-sparse-keymap))))
                          (if desc (cons desc m) m))
                      (if desc (cons desc cmd) cmd))))
          (define-key parent final-ev def)
          def)))))


;;; Core function

;;;###autoload
(cl-defun general-define-key
    (&rest args
           &key
           (states nil)
           (keymaps 'global)
           (major-modes nil)
           (prefix nil)
           &allow-other-keys)
  "Define keybindings in the style of general.el (minimal subset).

Supported keywords:
  :states   – list of evil states.  Bindings go ONLY into those states
              via evil-define-key* (not insert / minibuffer unless listed).
  :keymaps  – keymap symbol, list, 'global or 'override
  :prefix   – string prepended to every key
  :major-modes – accepted for API compat (ignored)

Nested sequences share one keymap hierarchy so which-key sees all
children under a prefix (SPC p, SPC f, …)."
  (declare (indent defun))
  (let* ((plist-keys '(:states :keymaps :major-modes :prefix))
         (bindings
          (let ((rest args) (acc nil))
            (while rest
              (if (memq (car rest) plist-keys)
                  (setq rest (cddr rest))
                (push (pop rest) acc)
                (when rest (push (pop rest) acc))))
            (nreverse acc)))
         (prefix-str (cond
                      ((null prefix) "")
                      ((stringp prefix) prefix)
                      (t (key-description prefix))))
         (states-list (when states (general--normalize-list states)))
         (keymap-syms (general--normalize-list keymaps))
         (use-evil (and states-list (fboundp 'evil-define-key*))))

    (ignore major-modes)

    (dolist (kmap-sym keymap-syms)
      (let ((kmap (general--resolve-keymap kmap-sym)))
        (unless kmap
          (when (symbolp kmap-sym)
            (unless (boundp kmap-sym)
              (set kmap-sym (make-sparse-keymap)))
            (setq kmap (symbol-value kmap-sym))))
        (when kmap
          (let ((pairs bindings))
            (while pairs
              (let* ((raw-key  (pop pairs))
                     (def      (pop pairs))
                     (key-str  (if (stringp raw-key) raw-key
                                 (key-description raw-key)))
                     (full-str (if (string-empty-p prefix-str)
                                   key-str
                                 (concat prefix-str " " key-str)))
                     (keyseq   (kbd full-str))
                     (parsed   (general--parse-def def))
                     (cmd      (car parsed))
                     (desc     (cdr parsed)))

                ;; 1. Build / update the shared hierarchy in the base keymap
                (let ((stored (general--bind-in-hierarchy kmap keyseq cmd desc)))

                  ;; 2. Mirror into evil states (state-scoped – stays out of
                  ;;    insert and minibuffer unless those states are listed)
                  (when use-evil
                    (dolist (state states-list)
                      (evil-define-key* state kmap keyseq stored))))

                ;; 3. which-key key-based replacement (safety net)
                (when desc
                  (general--which-key-register full-str desc))))))))))

;;;###autoload
(defalias 'general-emacs-define-key #'general-define-key)
;;;###autoload
(defalias 'general-def #'general-define-key)

(provide 'general)
;;; general.el ends here
