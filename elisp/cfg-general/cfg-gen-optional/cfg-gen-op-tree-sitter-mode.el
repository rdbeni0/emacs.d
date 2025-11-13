;;; cfg-gen-op-tree-sitter-mode.el --- general for tree-sitter -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 "0"   '(:ignore t :which-key "tree-smacs")
 "0s"  '(:ignore t :which-key "tree-sitter")
 "0sl"  '(cfg/treesit-link-ts-langs :which-key "ts-link-ts-langs")
 "0sr"  '(cfg/treesit-reinstall-grammar :which-key "ts-reinstall-grammar")
 "0sa"  '(cfg/treesit-reinstall-all-grammars :which-key "ts-reinstall-all-grammars")
 "0sp"  '(php-ts-mode-install-parsers :which-key "php-ts-mode-install-parsers"))

(provide 'cfg-gen-op-tree-sitter-mode)
;;; cfg-gen-op-tree-sitter-mode.el ends here
