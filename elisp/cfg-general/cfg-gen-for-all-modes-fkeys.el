;;; cfg-gen-for-all-modes-fkeys.el --- general.el for all modes and f-keys -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for <f1> to <f12> keys
;;
;;; Code:

(use-package general
  :functions
  (general-define-key))

;; no space + no which-key

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 ;; switch-to-buffer and ibuffer
 "<f4>"     '(cfg/toggle-ibuffer :which-key "ibuffer")
 "<f3>"     '(cfg/switch-to-buffer :which-key "switch-to-buffer")

 ;; completions
 "<f5><f4>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f5>" '(cfg/expand-abbrev :which-key "expand-abbrev")
 "<f5><f6>" '(completion-at-point :which-key "completion-at-point-capf"))

(use-package minibuffer
  :ensure nil
  :bind
  ;; Bind <f3> inside main minibuffer
  (:map minibuffer-local-map
        ("<f3>" . cfg/switch-to-buffer))
  :bind
  ;; And also in completion minibuffers (like M-x, find-file, etc.)
  (:map minibuffer-local-completion-map
        ("<f3>" . cfg/switch-to-buffer)))

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; switch-to-buffer and ibuffer
 "<f4>"     '(cfg/toggle-ibuffer :which-key "ibuffer")
 "<f3>"     '(cfg/switch-to-buffer :which-key "switch-to-buffer")

 ;; completions
 "<f5>"     '(:ignore t :which-key "completions")
 "<f5><f4>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f5>" '(cfg/expand-abbrev :which-key "expand-abbrev")
 "<f5><f6>" '(completion-at-point :which-key "completion-at-point-capf"))

(provide 'cfg-gen-for-all-modes-fkeys)
;;; cfg-gen-for-all-modes-fkeys.el ends here
