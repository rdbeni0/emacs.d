;;; general.el --- Minimal in-house general.el replacement -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Lightweight drop-in replacement for noctuid/general.el:
;;
;;   - general-define-key
;;       :states :keymaps :major-modes :prefix :non-normal-prefix
;;   - which-key via keymap-based cons cells (fast – no key-based alist)
;;   - general-override-mode
;;   - general-auto-unbind-keys
;;
;; Performance notes:
;;   * Descriptions use (cons "label" def) – which-key keymap-based
;;     replacement.  We deliberately do NOT call
;;     which-key-add-key-based-replacements (that alist is scanned on
;;     every which-key popup and causes input lag with many bindings).
;;   * Prefix maps are created once and reused; children are defined
;;     inside them so nested sequences share structure.
;;   * When :states is given, only evil-define-key* is used (nothing
;;     written to the base keymap → insert/minibuffer stay clean).
;;
;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'cl-lib))


;;; Customization / state

(defvar general-override-mode-map (make-sparse-keymap)
  "Keymap used by `general-override-mode'.")

(define-minor-mode general-override-mode
  "Minor mode whose keymap overrides almost everything else."
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

(defvar general-non-normal-states
  '(insert replace emacs hybrid iedit-insert)
  "Evil states that receive :non-normal-prefix instead of :prefix.")


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

(defun general--prefix-string (prefix)
  "Normalize PREFIX to a string (or \"\")."
  (cond
   ((null prefix) "")
   ((stringp prefix) prefix)
   (t (key-description prefix))))

(defun general--full-key-str (prefix-str key-str)
  "Join PREFIX-STR and KEY-STR into a single key-description string."
  (if (or (null prefix-str) (string-empty-p prefix-str))
      key-str
    (concat prefix-str " " key-str)))

(defun general--non-normal-state-p (state)
  "Return non-nil if STATE is a non-normal evil state."
  (memq state general-non-normal-states))

(defun general--unwrap (def)
  "Unwrap a which-key (STRING . REAL) cons to REAL; else return DEF."
  (if (and (consp def) (stringp (car def)))
      (cdr def)
    def))

(defun general--aux-map (kmap state)
  "Return the evil auxiliary keymap for STATE on KMAP, creating it."
  (evil-get-auxiliary-keymap kmap state t t))

(defun general--lookup-in-aux (kmap state keyseq)
  "Look up KEYSEQ in the aux map for STATE on KMAP, unwrapping cons."
  (general--unwrap (lookup-key (general--aux-map kmap state) keyseq t)))

(defun general--ensure-prefix-in-aux (kmap state keyseq &optional desc)
  "Ensure KEYSEQ is a prefix keymap in STATE's aux map for KMAP.
Reuses an existing map when present.  Returns the (innermost) map.
If DESC is non-nil the binding is stored as (cons DESC map)."
  (let ((aux (general--aux-map kmap state))
        (map aux)
        (len (length keyseq)))
    (dotimes (i len)
      (let* ((ev  (aref keyseq i))
             (vec (vector ev))
             (cur (general--unwrap (lookup-key map vec t)))
             (is-last (= i (1- len))))
        (if (keymapp cur)
            (setq map cur)
          (let ((new-map (make-sparse-keymap)))
            (define-key map vec
              (if (and is-last desc) (cons desc new-map) new-map))
            (setq map new-map)))))
    ;; Refresh description on an already-existing final map
    (when (and desc (> len 0))
      (let* ((parent (if (= len 1) aux
                       (general--unwrap
                        (lookup-key aux (substring keyseq 0 (1- len)) t))))
             (final-ev (vector (aref keyseq (1- len))))
             (existing (lookup-key parent final-ev t)))
        (when (keymapp parent)
          (let ((raw (general--unwrap existing)))
            (when (keymapp raw)
              (define-key parent final-ev (cons desc raw)))))))
    map))

(defun general--bind-leaf-in-aux (kmap state keyseq cmd desc)
  "Bind KEYSEQ to CMD in STATE's aux map, with optional which-key DESC.
Intermediate prefix maps are created/reused as plain keymaps (or
named conses).  The leaf is stored as (cons DESC CMD) when DESC
is non-nil."
  (let ((len (length keyseq)))
    (if (= len 0)
        nil
      (if (= len 1)
          (let ((aux (general--aux-map kmap state))
                (def (if desc (cons desc cmd) cmd)))
            (define-key aux keyseq def))
        ;; Ensure parent path exists, then bind final event into parent
        (let* ((parent-seq (substring keyseq 0 (1- len)))
               (parent     (general--ensure-prefix-in-aux kmap state parent-seq))
               (final-ev   (vector (aref keyseq (1- len))))
               (def        (if desc (cons desc cmd) cmd)))
          (define-key parent final-ev def))))))

(defun general--bind-prefix-in-aux (kmap state keyseq desc)
  "Bind KEYSEQ as a named prefix in STATE's aux map."
  (general--ensure-prefix-in-aux kmap state keyseq desc))


;;; Non-evil (plain define-key) helpers

(defun general--ensure-prefix-plain (keymap keyseq &optional desc)
  "Like `general--ensure-prefix-in-aux' but on a plain KEYMAP."
  (let ((map keymap)
        (len (length keyseq)))
    (dotimes (i len)
      (let* ((ev  (aref keyseq i))
             (vec (vector ev))
             (cur (general--unwrap (lookup-key map vec t)))
             (is-last (= i (1- len))))
        (if (keymapp cur)
            (setq map cur)
          (let ((new-map (make-sparse-keymap)))
            (define-key map vec
              (if (and is-last desc) (cons desc new-map) new-map))
            (setq map new-map)))))
    (when (and desc (> len 0))
      (let* ((parent (if (= len 1) keymap
                       (general--unwrap
                        (lookup-key keymap (substring keyseq 0 (1- len)) t))))
             (final-ev (vector (aref keyseq (1- len))))
             (existing (lookup-key parent final-ev t)))
        (when (keymapp parent)
          (let ((raw (general--unwrap existing)))
            (when (keymapp raw)
              (define-key parent final-ev (cons desc raw)))))))
    map))

(defun general--bind-leaf-plain (keymap keyseq cmd desc)
  "Bind KEYSEQ to CMD on KEYMAP with optional DESC."
  (let ((len (length keyseq)))
    (if (= len 1)
        (define-key keymap keyseq (if desc (cons desc cmd) cmd))
      (let* ((parent (general--ensure-prefix-plain
                      keymap (substring keyseq 0 (1- len))))
             (final-ev (vector (aref keyseq (1- len)))))
        (define-key parent final-ev (if desc (cons desc cmd) cmd))))))


;;; Core function

;;;###autoload
(cl-defun general-define-key
    (&rest args
           &key
           (states nil)
           (keymaps 'global)
           (major-modes nil)
           (prefix nil)
           (non-normal-prefix nil)
           &allow-other-keys)
  "Define keybindings in the style of general.el (minimal subset).

Supported keywords:
  :states             – evil states (bindings only in those states)
  :keymaps            – keymap symbol, list, 'global or 'override
  :prefix             – prefix for normal-ish states
  :non-normal-prefix  – alternate prefix for insert/emacs/replace/…
  :major-modes        – accepted for API compat (ignored)

which-key descriptions use keymap-based cons cells only (fast)."
  (declare (indent defun))
  (let* ((plist-keys '(:states :keymaps :major-modes :prefix
                               :non-normal-prefix))
         (bindings
          (let ((rest args) (acc nil))
            (while rest
              (if (memq (car rest) plist-keys)
                  (setq rest (cddr rest))
                (push (pop rest) acc)
                (when rest (push (pop rest) acc))))
            (nreverse acc)))
         (prefix-str    (general--prefix-string prefix))
         (nn-prefix-str (general--prefix-string non-normal-prefix))
         (has-nn-prefix (and non-normal-prefix
                             (not (string-empty-p nn-prefix-str))))
         (states-list   (when states (general--normalize-list states)))
         (keymap-syms   (general--normalize-list keymaps))
         (use-evil      (and states-list (fboundp 'evil-define-key*)
                             (fboundp 'evil-get-auxiliary-keymap))))

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
              (let* ((raw-key (pop pairs))
                     (def     (pop pairs))
                     (key-str (if (stringp raw-key) raw-key
                                (key-description raw-key)))
                     (parsed  (general--parse-def def))
                     (cmd     (car parsed))
                     (desc    (cdr parsed)))

                (if use-evil
                    ;; State-scoped: operate directly on aux maps so we
                    ;; can reuse prefix keymaps and avoid base-map pollution.
                    (dolist (state states-list)
                      (let* ((use-nn (and has-nn-prefix
                                          (general--non-normal-state-p state)))
                             (p-str  (if use-nn nn-prefix-str prefix-str))
                             (full   (general--full-key-str p-str key-str))
                             (keyseq (kbd full)))
                        (if (eq cmd :ignore)
                            (general--bind-prefix-in-aux kmap state keyseq desc)
                          (general--bind-leaf-in-aux kmap state keyseq cmd desc))))

                  ;; No :states – plain define-key on the keymap
                  (let* ((full   (general--full-key-str prefix-str key-str))
                         (keyseq (kbd full)))
                    (if (eq cmd :ignore)
                        (general--ensure-prefix-plain kmap keyseq desc)
                      (general--bind-leaf-plain kmap keyseq cmd desc))))))))))))

;;;###autoload
(defalias 'general-emacs-define-key #'general-define-key)
;;;###autoload
(defalias 'general-def #'general-define-key)

(provide 'general)
;;; general.el ends here
