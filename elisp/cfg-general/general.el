;;; general.el --- Minimal in-house general.el replacement -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Drop-in subset of noctuid/general.el used by this config:
;;   general-define-key, general-override-mode, general-auto-unbind-keys
;;
;; Keywords: :states :keymaps :prefix :non-normal-prefix :major-modes
;; which-key: (:which-key / :wk) – cons on commands, plain maps for prefixes
;;
;; Custom evil states (e.g. treemacs): bind on mode-map aux maps and on
;; `evil-STATE-state-map'.  Missing state maps are queued and flushed via
;; `after-load-functions' (only those few entries – no mass deferral).
;;
;;; Code:

(require 'cl-lib)


;;; Override mode

(defvar general-override-mode-map (make-sparse-keymap)
  "Keymap used by `general-override-mode'.")

(define-minor-mode general-override-mode
  "Minor mode whose keymap overrides almost everything else."
  :global t
  :keymap general-override-mode-map
  :group 'general)

(with-eval-after-load 'evil
  (dolist (state '(normal visual insert emacs motion operator replace))
    (evil-make-overriding-map general-override-mode-map state))
  (add-hook 'general-override-mode-hook #'evil-normalize-keymaps))

(defvar general-non-normal-states
  '(insert replace emacs hybrid iedit-insert)
  "States that use :non-normal-prefix instead of :prefix.")

(defvar general-standard-states
  '(normal visual insert emacs motion operator replace hybrid)
  "Built-in evil states.  Others also bind on `evil-STATE-state-map'.")


;;; Auto-unbind

(defvar general--auto-unbind nil)

(defun general--unbind-prefix-keys (keymap key)
  "Unbind non-keymap prefixes of KEY in KEYMAP so KEY can be bound."
  (when (and (keymapp keymap) (vectorp key) (> (length key) 1))
    (dotimes (i (1- (length key)))
      (let* ((prefix (substring key 0 (1+ i)))
             (b (lookup-key keymap prefix)))
        (unless (or (null b) (numberp b) (keymapp b)
                    (and (consp b) (stringp (car b)) (keymapp (cdr b))))
          (define-key keymap prefix nil))))))

(defun general--define-key-advice (orig-fun keymap key def &rest args)
  "Around advice for `define-key': auto-unbind conflicting prefixes."
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
  "Return X as a list.  Expand bound list-valued symbols."
  (cond
   ((and (symbolp x) (boundp x) (listp (symbol-value x))) (symbol-value x))
   ((listp x) x)
   (t (list x))))

(defun general--resolve-keymap (sym)
  "Resolve SYM to a keymap object, or nil."
  (cond
   ((keymapp sym) sym)
   ((eq sym 'global) (current-global-map))
   ((eq sym 'override) general-override-mode-map)
   ((and (symbolp sym) (boundp sym) (keymapp (symbol-value sym)))
    (symbol-value sym))))

(defun general--parse-def (def)
  "Parse DEF into (CMD . WHICH-KEY-DESC).  CMD may be :ignore."
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

(defsubst general--unwrap (def)
  "Unwrap which-key (STRING . REAL); else DEF."
  (if (and (consp def) (stringp (car def))) (cdr def) def))

(defun general--state-map (state)
  "Return `evil-STATE-state-map' for a non-standard STATE, or nil."
  (unless (memq state general-standard-states)
    (let ((sym (intern (format "evil-%s-state-map" state))))
      (and (boundp sym) (keymapp (symbol-value sym)) (symbol-value sym)))))


;;; Bind primitives

(defun general--ensure-path (root keyseq)
  "Ensure KEYSEQ is a path of plain keymaps under ROOT; return innermost."
  (let ((map root)
        (len (length keyseq)))
    (dotimes (i len)
      (let* ((vec (vector (aref keyseq i)))
             (cur (general--unwrap (lookup-key map vec t))))
        (unless (keymapp cur)
          (setq cur (make-sparse-keymap))
          (define-key map vec cur))
        (setq map cur)))
    map))

(defun general--bind (root keyseq cmd desc)
  "Bind KEYSEQ under ROOT.  Prefixes are plain maps; leaves may use cons."
  (cond
   ((zerop (length keyseq)) nil)
   ((eq cmd :ignore)
    (general--ensure-path root keyseq)
    (when (and desc (fboundp 'which-key-add-key-based-replacements))
      (condition-case nil
          (which-key-add-key-based-replacements
           (key-description keyseq) desc)
        (error nil))))
   (t
    (let* ((len (length keyseq))
           (parent (if (= len 1) root
                     (general--ensure-path root (substring keyseq 0 (1- len)))))
           (event (if (= len 1) keyseq
                    (vector (aref keyseq (1- len)))))
           (def (if desc (cons desc cmd) cmd)))
      (define-key parent event def)))))


;;; Deferred custom-state maps

(defvar general--pending-state-bindings nil
  "List of (STATE KEYSEQ CMD DESC) waiting for `evil-STATE-state-map'.")

(defvar general--pending-hook-added nil)

(defun general--flush-pending-state-bindings (&rest _)
  "Apply pending bindings whose custom state maps now exist."
  (let (remaining)
    (dolist (item general--pending-state-bindings)
      (let ((map (general--state-map (car item))))
        (if map
            (apply #'general--bind map (cdr item))
          (push item remaining))))
    (setq general--pending-state-bindings (nreverse remaining))
    (when (null general--pending-state-bindings)
      (remove-hook 'after-load-functions #'general--flush-pending-state-bindings)
      (setq general--pending-hook-added nil))))

(defun general--queue-state-binding (state keyseq cmd desc)
  "Bind on custom state map now, or queue until it exists."
  (let ((map (general--state-map state)))
    (if map
        (general--bind map keyseq cmd desc)
      (unless (memq state general-standard-states)
        (push (list state keyseq cmd desc) general--pending-state-bindings)
        (unless general--pending-hook-added
          (setq general--pending-hook-added t)
          (add-hook 'after-load-functions
                    #'general--flush-pending-state-bindings))))))


;;; Aux-map cache

(defvar general--aux-cache nil
  "Alist ((KEYMAP . STATE) . AUX) for one `general-define-key' call.")

(defun general--aux-map (kmap state)
  "Cached `evil-get-auxiliary-keymap' for STATE on KMAP."
  (let* ((key (cons kmap state))
         (hit (assoc key general--aux-cache)))
    (if hit (cdr hit)
      (let ((aux (evil-get-auxiliary-keymap kmap state t t)))
        (push (cons key aux) general--aux-cache)
        aux))))


;;; Core

(defconst general--plist-keys
  '(:states :keymaps :major-modes :prefix :non-normal-prefix)
  "Keyword args stripped from the key/definition body.")

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
  "Define keybindings (minimal general.el-compatible API).

:states   – evil states; bindings only in those states (aux maps)
:keymaps  – keymap symbol/list, 'global, or 'override
:prefix / :non-normal-prefix – string prefixes
:major-modes – accepted, ignored

Custom states also bind on `evil-STATE-state-map' (deferred if needed)."
  (declare (indent defun))
  (ignore major-modes)
  (let* ((bindings
          (let ((rest args) acc)
            (while rest
              (if (memq (car rest) general--plist-keys)
                  (setq rest (cddr rest))
                (push (pop rest) acc)
                (when rest (push (pop rest) acc))))
            (nreverse acc)))
         (prefix-str (if (stringp prefix) prefix
                       (if prefix (key-description prefix) "")))
         (nn-str (if (stringp non-normal-prefix) non-normal-prefix
                   (if non-normal-prefix
                       (key-description non-normal-prefix) "")))
         (has-nn (and non-normal-prefix (not (string-empty-p nn-str))))
         (states-list (and states (general--normalize-list states)))
         (keymap-syms (general--normalize-list keymaps))
         (use-evil (and states-list
                        (fboundp 'evil-define-key*)
                        (fboundp 'evil-get-auxiliary-keymap)))
         (general--aux-cache nil))

    (dolist (kmap-sym keymap-syms)
      (let ((kmap (or (general--resolve-keymap kmap-sym)
                      (and (symbolp kmap-sym)
                           (progn
                             (unless (boundp kmap-sym)
                               (set kmap-sym (make-sparse-keymap)))
                             (symbol-value kmap-sym))))))
        (when (keymapp kmap)
          (let ((pairs bindings))
            (while pairs
              (let* ((raw (pop pairs))
                     (def (pop pairs))
                     (key-str (if (stringp raw) raw (key-description raw)))
                     (parsed (general--parse-def def))
                     (cmd (car parsed))
                     (desc (cdr parsed)))
                (if use-evil
                    (dolist (state states-list)
                      (let* ((pstr (if (and has-nn
                                            (memq state general-non-normal-states))
                                       nn-str prefix-str))
                             (full (if (string-empty-p pstr) key-str
                                     (concat pstr " " key-str)))
                             (keyseq (kbd full)))
                        (general--bind (general--aux-map kmap state)
                                       keyseq cmd desc)
                        (general--queue-state-binding state keyseq cmd desc)))
                  (let* ((full (if (string-empty-p prefix-str) key-str
                                 (concat prefix-str " " key-str)))
                         (keyseq (kbd full)))
                    (general--bind kmap keyseq cmd desc)))))))))))

;;;###autoload
(defalias 'general-emacs-define-key #'general-define-key)
;;;###autoload
(defalias 'general-def #'general-define-key)

(provide 'general)
;;; general.el ends here
