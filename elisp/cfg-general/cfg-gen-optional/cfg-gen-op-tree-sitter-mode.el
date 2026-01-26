;;; cfg-gen-op-tree-sitter-mode.el --- general for tree-sitter -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 "0"   '(:ignore t :which-key "tree-smacs")
 "0s"  '(:ignore t :which-key "tree-sitter")
 "0sg"  '(cfg/treesit-reinstall-grammar :which-key "ts-reinstall-grammar")
 "0ss"  '(cfg/treesit-reinstall-full :which-key "ts-reinstall-full")
 "0sr"  '(cfg/treesit-reinstall-full :which-key "ts-reinstall-full")
 "0sa"  '(treesit-auto-install-all :which-key "ts-auto-install-all")
 "0sp"  '(cfg/php-ts-mode-install-parsers :which-key "php-ts-mode-install-parsers"))

(provide 'cfg-gen-op-tree-sitter-mode)
;;; cfg-gen-op-tree-sitter-mode.el ends here
