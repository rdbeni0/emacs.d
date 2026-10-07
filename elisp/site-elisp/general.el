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
;; Performance:
;;   * Descriptions: (cons "label" def) – keymap-based which-key, O(1).
;;   * Prefix maps created once and reused.
;;   * :states → only evil aux maps (insert/minibuffer stay clean).
;;   * define-key advice is cheap (one lookup per prefix level).
;;   * Aux maps cached per (keymap . state) within a single call.
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
    (cons :ignore (or (plist-get def :which-key) (plist-get def :wk))))
   ((and (listp def)
         (or (plist-member (cdr def) :which-key)
             (plist-member (cdr def) :wk)))
    (cons (car def)
          (or (plist-get (cdr def) :which-key)
              (plist-get (cdr def) :wk))))
   (t (cons def nil))))

(defsubst general--prefix-string (prefix)
  "Normalize PREFIX to a string (or \"\")."
  (cond ((null prefix) "")
        ((stringp prefix) prefix)
        (t (key-description prefix))))

(defsubst general--full-key-str (prefix-str key-str)
  "Join PREFIX-STR and KEY-STR."
  (if (string-empty-p prefix-str) key-str
    (concat prefix-str " " key-str)))

(defsubst general--non-normal-state-p (state)
  "Return non-nil if STATE is a non-normal evil state."
  (memq state general-non-normal-states))

(defsubst general--unwrap (def)
  "Unwrap (STRING . REAL) cons to REAL; else DEF."
  (if (and (consp def) (stringp (car def))) (cdr def) def))

;; Per-call cache: avoids repeated evil-get-auxiliary-keymap.
(defvar general--aux-cache nil
  "Alist ((KEYMAP . STATE) . AUX-MAP) for the current define-key call.")

(defun general--aux-map (kmap state)
  "Return evil auxiliary keymap for STATE on KMAP (cached)."
  (let* ((key (cons kmap state))
         (cached (assoc key general--aux-cache)))
    (if cached
        (cdr cached)
      (let ((aux (evil-get-auxiliary-keymap kmap state t t)))
        (push (cons key aux) general--aux-cache)
        aux))))

(defun general--ensure-path (root keyseq &optional desc)
  "Ensure KEYSEQ is a path of keymaps under ROOT.  Return innermost map.
Reuses existing maps.  If DESC is given, the final binding is
\(cons DESC map)."
  (let ((parent root)
        (map root)
        (len (length keyseq)))
    (dotimes (i len)
      (let* ((vec (vector (aref keyseq i)))
             (cur (general--unwrap (lookup-key parent vec t)))
             (last-p (= i (1- len))))
        (if (keymapp cur)
            (setq map cur)
          (setq map (make-sparse-keymap)))
        (define-key parent vec
          (if (and last-p desc) (cons desc map) map))
        (setq parent map)))
    map))

(defun general--bind (root keyseq cmd desc)
  "Bind KEYSEQ under ROOT to CMD (or :ignore prefix) with optional DESC."
  (let ((len (length keyseq)))
    (cond
     ((= len 0) nil)
     ((eq cmd :ignore)
      (general--ensure-path root keyseq desc))
     ((= len 1)
      (define-key root keyseq (if desc (cons desc cmd) cmd)))
     (t
      (let ((parent (general--ensure-path root (substring keyseq 0 (1- len)))))
        (define-key parent (vector (aref keyseq (1- len)))
          (if desc (cons desc cmd) cmd)))))))


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
                             (keyseq (kbd full))
                             (root   (general--aux-map kmap state)))
                        (general--bind root keyseq cmd desc)))

                  (let* ((full   (general--full-key-str prefix-str key-str))
                         (keyseq (kbd full)))
                    (general--bind kmap keyseq cmd desc)))))))))))

;;;###autoload
(defalias 'general-emacs-define-key #'general-define-key)
;;;###autoload
(defalias 'general-def #'general-define-key)

(provide 'general)
;;; general.el ends here
