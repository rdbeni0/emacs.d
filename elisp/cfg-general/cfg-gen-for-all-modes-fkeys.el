;;; cfg-gen-for-all-modes-fkeys.el --- general.el for all modes and f-keys -*- lexical-binding: t -*-
;; Configuration for <f1> to <f12> keys

;; no space + no which-key

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 ;; ibuffer
 "<f4>"     '(cfg/toggle-ibuffer :which-key "ibuffer")

 ;; completions
 "<f5><f4>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f5>" '(cfg/expand-abbrev :which-key "expand-abbrev")
 "<f5><f6>" '(completion-at-point :which-key "completion-at-point-capf"))

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; ibuffer
 "<f4>"     '(cfg/toggle-ibuffer :which-key "ibuffer")

 ;; completions
 "<f5>"     '(:ignore t :which-key "completions")
 "<f5><f4>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f5>" '(cfg/expand-abbrev :which-key "expand-abbrev")
 "<f5><f6>" '(completion-at-point :which-key "completion-at-point-capf"))

(provide 'cfg-gen-for-all-modes-fkeys)
;;; cfg-gen-for-all-modes-fkeys.el ends here
