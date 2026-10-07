;;; general.el --- Minimal in-house general.el replacement -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Lightweight drop-in replacement for noctuid/general.el covering only the
;; features used in this configuration:
;;
;;   - general-define-key
;;       :states :keymaps :major-modes :prefix :non-normal-prefix
;;   - which-key metadata  (:which-key / :ignore)
;;   - general-override-mode
;;   - general-auto-unbind-keys
;;
;; Design:
;;   * When :states is given → ONLY evil-define-key* (state-scoped).
;;     Nothing is written into the base keymap, so insert / minibuffer
;;     stay clean unless those states are listed.
;;   * When :states is omitted → define-key on the base keymap.
;;   * :non-normal-prefix – alternate prefix for non-normal states
;;     (insert, emacs, replace, hybrid, iedit-insert).
;;   * Prefix labels use (cons "label" keymap) so which-key shows
;;     "projects" instead of "+prefix".
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

;; States considered "non-normal" for :non-normal-prefix (same default as
;; upstream general.el).
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

(defun general--which-key-register (key-str desc)
  "Register KEY-STR -> DESC with which-key (key-based)."
  (when (and desc (featurep 'which-key)
             (fboundp 'which-key-add-key-based-replacements))
    (condition-case nil
        (which-key-add-key-based-replacements key-str desc)
      (error nil))))

(defun general--make-def (cmd desc)
  "Build definition: (cons DESC map/cmd) for which-key, or plain value."
  (cond
   ((eq cmd :ignore)
    (let ((m (make-sparse-keymap)))
      (if desc (cons desc m) m)))
   (desc (cons desc cmd))
   (t cmd)))

(defun general--prefix-string (prefix)
  "Normalize PREFIX to a string (or \"\")."
  (cond
   ((null prefix) "")
   ((stringp prefix) prefix)
   (t (key-description prefix))))

(defun general--full-key-str (prefix-str key-str)
  "Join PREFIX-STR and KEY-STR into a single key-description string."
  (if (string-empty-p prefix-str)
      key-str
    (concat prefix-str " " key-str)))

(defun general--non-normal-state-p (state)
  "Return non-nil if STATE is a non-normal evil state."
  (memq state general-non-normal-states))


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
  :states             – list of evil states.  Bindings go ONLY into
                        those states via evil-define-key*.
  :keymaps            – keymap symbol, list, 'global or 'override
  :prefix             – string prepended to every key (normal-ish states)
  :non-normal-prefix  – alternate prefix for non-normal states
                        (insert, emacs, replace, hybrid, iedit-insert).
                        When set, those states get this prefix instead
                        of :prefix.  Example:

                          (general-define-key
                           :states '(normal insert emacs)
                           :prefix \"SPC\"
                           :non-normal-prefix \"M-SPC\"
                           \"f\" 'find-file)

                        → SPC f in normal, M-SPC f in insert/emacs.

  :major-modes        – accepted for API compat (ignored)

Prefix keys use (cons \"label\" keymap) so which-key shows the
custom name instead of \"+prefix\"."
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
         (prefix-str     (general--prefix-string prefix))
         (nn-prefix-str  (general--prefix-string non-normal-prefix))
         (has-nn-prefix  (and non-normal-prefix
                              (not (string-empty-p nn-prefix-str))))
         (states-list    (when states (general--normalize-list states)))
         (keymap-syms    (general--normalize-list keymaps))
         (use-evil       (and states-list (fboundp 'evil-define-key*))))

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
                     (desc    (cdr parsed))
                     (bind-def (general--make-def cmd desc)))

                (if use-evil
                    ;; ---- State-scoped path ----
                    ;; Split states: normal-ish get :prefix,
                    ;; non-normal get :non-normal-prefix (when set).
                    (dolist (state states-list)
                      (let* ((use-nn (and has-nn-prefix
                                          (general--non-normal-state-p state)))
                             (p-str  (if use-nn nn-prefix-str prefix-str))
                             (full   (general--full-key-str p-str key-str))
                             (keyseq (kbd full)))
                        (evil-define-key* state kmap keyseq bind-def)
                        (when desc
                          (general--which-key-register full desc))))

                  ;; ---- No :states – bind on base keymap ----
                  ;; :non-normal-prefix is irrelevant without states;
                  ;; just use :prefix.
                  (let* ((full   (general--full-key-str prefix-str key-str))
                         (keyseq (kbd full)))
                    (define-key kmap keyseq bind-def)
                    (when desc
                      (general--which-key-register full desc))))))))))))

;;;###autoload
(defalias 'general-emacs-define-key #'general-define-key)
;;;###autoload
(defalias 'general-def #'general-define-key)

(provide 'general)
;;; general.el ends here
