;;; cfg-gen-for-all-modes-fkeys.el --- general.el for all modes and f-keys -*- lexical-binding: t -*-
;; Configuration for <f1> to <f12> keys

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

;; Bind <f3> inside main minibuffer
(define-key minibuffer-local-map (kbd "<f3>") #'cfg/switch-to-buffer)
;; And also in completion minibuffers (like M-x, find-file, etc.)
(define-key minibuffer-local-completion-map (kbd "<f3>") #'cfg/switch-to-buffer)

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
