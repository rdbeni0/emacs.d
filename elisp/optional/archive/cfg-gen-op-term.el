;;; cfg-gen-op-term.el --- general.el for various terms and shells -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "asm"  '(cfg/multi-term-buffer-rn :which-key "multi-term_bash")
 "ast"  '(tramp-term :which-key "tramp-term"))

(provide 'cfg-gen-op-term)
;;; cfg-gen-op-term.el ends here
