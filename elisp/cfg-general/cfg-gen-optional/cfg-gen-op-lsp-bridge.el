;;; cfg-gen-op-lsp-bridge.el --- general.el for lsp and dap -*- lexical-binding: t -*-

;; https://github.com/manateelazycat/lsp-bridge?tab=readme-ov-file#keymap
(general-define-key
 :prefix "\\"
 :states '(normal visual emacs)
 :keymaps 'override
 "=" '(lsp-bridge-code-format :which-key "format")
 ";" '(lsp-bridge-rename :which-key "rename")
 "]" '(lsp-bridge-find-type-def :which-key "jump-typeDefinition")
 "[" '(lsp-bridge-find-impl :which-key "jump-implementation")
 "p" '(lsp-bridge-find-def :which-key "def-find")
 "/" '(lsp-bridge-find-def-return :which-key "def-return")
 "l" '(lsp-bridge-find-references :which-key "find-references-rg")
 "'" '(lsp-bridge-code-action :which-key "code-actions")
 "f" '(lsp-bridge-diagnostic-list :which-key "diagnostic")
 "\\" '(:ignore t :which-key "server")
 "\\r" '(lsp-bridge-restart-process :which-key "reconnect"))

(provide 'cfg-gen-op-lsp-bridge)
;;; cfg-gen-op-lsp-bridge.el ends here
