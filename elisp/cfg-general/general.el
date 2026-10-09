;;; general.el --- Minimal in-house general.el replacement -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Lightweight drop-in replacement for noctuid/general.el.
;;
;; Custom evil states (treemacs, …):
;;   Bindings go to mode-map aux maps and to `evil-STATE-state-map'.
;;   If that state map does not exist yet (load order), the binding is
;;   queued and applied once via `after-load-functions' – only for the
;;   few pending custom-state entries, never mass-deferred mode maps.
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

(defvar general-standard-states
  '(normal visual insert emacs motion operator replace hybrid)
  "Built-in evil states.  Non-standard states also bind on evil-STATE-state-map.")


;;; Automatic key unbinding

(defvar general--auto-unbind nil)

(defun general--unbind-prefix-keys (keymap key)
  "Unbind non-keymap prefixes of KEY in KEYMAP so KEY can be bound."
  (let ((len (length key)))
    (when (and (keymapp keymap) (vectorp key) (> len 1))
      (dotimes (i (1- len))
        (let* ((prefix (substring key 0 (1+ i)))
               (b (lookup-key keymap prefix)))
          (cond
           ((or (null b) (numberp b)) nil)
           ((keymapp b) nil)
           ((and (consp b) (stringp (car b)) (keymapp (cdr b))) nil)
           (t (define-key keymap prefix nil))))))))

(defun general--define-key-advice (orig-fun keymap key def &rest args)
  "Thin advice: unbind conflicting prefixes only when auto-unbind is on."
  (when (and general--auto-unbind (keymapp keymap) key)
    (let ((raw (if (vectorp key) key
                 (ignore-errors
                   (kbd (if (stringp key) key (format "%s" key)))))))
      (when (vectorp raw)
        (general--unbind-prefix-keys keymap raw))))
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
  "Return X as a flat list of items."
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
    (cons :ignore (or (plist-get def :which-key) (plist-get def :wk))))
   ((and (listp def)
         (or (plist-member (cdr def) :which-key)
             (plist-member (cdr def) :wk)))
    (cons (car def)
          (or (plist-get (cdr def) :which-key)
              (plist-get (cdr def) :wk))))
   (t (cons def nil))))

(defsubst general--prefix-string (prefix)
  (cond ((null prefix) "")
        ((stringp prefix) prefix)
        (t (key-description prefix))))

(defsubst general--full-key-str (prefix-str key-str)
  (if (string-empty-p prefix-str) key-str
    (concat prefix-str " " key-str)))

(defsubst general--non-normal-state-p (state)
  (memq state general-non-normal-states))

(defsubst general--unwrap (def)
  (if (and (consp def) (stringp (car def))) (cdr def) def))

(defun general--state-map-sym (state)
  "Symbol `evil-STATE-state-map'."
  (intern (format "evil-%s-state-map" state)))

(defun general--state-map (state)
  "Return evil-STATE-state-map if it is a keymap; else nil.
Only for non-standard states."
  (unless (memq state general-standard-states)
    (let ((sym (general--state-map-sym state)))
      (and (boundp sym) (keymapp (symbol-value sym)) (symbol-value sym)))))


;;; Bind primitives (defined before deferred flush uses them)

(defun general--ensure-path (root keyseq &optional _desc)
  "Ensure KEYSEQ is a path of plain keymaps under ROOT."
  (let ((parent root)
        (map root)
        (len (length keyseq)))
    (dotimes (i len)
      (let* ((vec (vector (aref keyseq i)))
             (cur (general--unwrap (lookup-key parent vec t))))
        (if (keymapp cur)
            (setq map cur)
          (setq map (make-sparse-keymap))
          (define-key parent vec map))
        (setq parent map)))
    map))

(defun general--bind (root keyseq cmd desc)
  "Bind KEYSEQ under ROOT.  Prefix maps are plain; leaves may use cons."
  (let ((len (length keyseq)))
    (cond
     ((= len 0) nil)
     ((eq cmd :ignore)
      (general--ensure-path root keyseq)
      (when (and desc (fboundp 'which-key-add-key-based-replacements))
        (condition-case nil
            (which-key-add-key-based-replacements
             (key-description keyseq) desc)
          (error nil))))
     ((= len 1)
      (define-key root keyseq (if desc (cons desc cmd) cmd)))
     (t
      (let ((parent (general--ensure-path root (substring keyseq 0 (1- len)))))
        (define-key parent (vector (aref keyseq (1- len)))
          (if desc (cons desc cmd) cmd)))))))


;;; Deferred bindings for custom state maps (load-order safe, no freeze)

(defvar general--pending-state-bindings nil
  "List of (STATE KEYSEQ CMD DESC) waiting for evil-STATE-state-map.")

(defvar general--pending-hook-added nil)

(defun general--flush-pending-state-bindings (&rest _)
  "Apply pending custom-state bindings whose state maps now exist."
  (let ((remaining nil))
    (dolist (item general--pending-state-bindings)
      (let* ((state  (nth 0 item))
             (keyseq (nth 1 item))
             (cmd    (nth 2 item))
             (desc   (nth 3 item))
             (map    (general--state-map state)))
        (if map
            (general--bind map keyseq cmd desc)
          (push item remaining))))
    (setq general--pending-state-bindings (nreverse remaining))
    (when (null general--pending-state-bindings)
      (remove-hook 'after-load-functions #'general--flush-pending-state-bindings)
      (setq general--pending-hook-added nil))))

(defun general--queue-state-binding (state keyseq cmd desc)
  "Bind on state map now, or queue until the map exists."
  (let ((map (general--state-map state)))
    (if map
        (general--bind map keyseq cmd desc)
      (unless (memq state general-standard-states)
        (push (list state keyseq cmd desc) general--pending-state-bindings)
        (unless general--pending-hook-added
          (setq general--pending-hook-added t)
          (add-hook 'after-load-functions
                    #'general--flush-pending-state-bindings))))))


;;; Aux cache

(defvar general--aux-cache nil)

(defun general--aux-map (kmap state)
  "Return evil auxiliary keymap for STATE on KMAP (cached)."
  (let* ((key (cons kmap state))
         (cached (assoc key general--aux-cache)))
    (if cached
        (cdr cached)
      (let ((aux (evil-get-auxiliary-keymap kmap state t t)))
        (push (cons key aux) general--aux-cache)
        aux))))


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

Custom states (e.g. treemacs): bindings go to mode-map aux maps and to
`evil-STATE-state-map'.  If the state map is not loaded yet, the binding
is deferred via `after-load-functions' (only those pending entries)."
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
                             (fboundp 'evil-get-auxiliary-keymap)))
         (general--aux-cache nil))

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
                    (dolist (state states-list)
                      (let* ((use-nn (and has-nn-prefix
                                          (general--non-normal-state-p state)))
                             (p-str  (if use-nn nn-prefix-str prefix-str))
                             (full   (general--full-key-str p-str key-str))
                             (keyseq (kbd full)))
                        ;; Mode-map auxiliary (always, when kmap exists)
                        (general--bind (general--aux-map kmap state)
                                       keyseq cmd desc)
                        ;; Custom state global map (now or deferred)
                        (general--queue-state-binding state keyseq cmd desc)))
                  (let* ((full   (general--full-key-str prefix-str key-str))
                         (keyseq (kbd full)))
                    (general--bind kmap keyseq cmd desc)))))))))))

;;;###autoload
(defalias 'general-emacs-define-key #'general-define-key)
;;;###autoload
(defalias 'general-def #'general-define-key)

(provide 'general)
;;; general.el ends here
