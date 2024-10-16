;;; cfg-gen-op-term.el --- general.el for various tems and shells -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "asm"  '(cfg/multi-term-buffer-rn :which-key "multi-term_bash")
 "asv"  '(multi-vterm :which-key "multi-vterm")
 "ast"  '(tramp-term :which-key "tramp-term"))

(provide 'cfg-gen-op-term)
;;; cfg-gen-op-term.el ends here
