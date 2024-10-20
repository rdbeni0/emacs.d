;;; cfg-gen-op-lsp-dap-mode.el --- general.el for lsp and dap -*- lexical-binding: t -*-

;; general-lsp-dap-mode : TODO
;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://emacs-lsp.github.io/lsp-mode/page/installation/
;; https://github.com/emacs-lsp/lsp-mode/issues/1530
;; ^ evil modes and lsp-mode prefix-key on Gitter
;; So in that case it should be mapped as: *evil-defined-key*
;; (evil-define-key 'normal lsp-mode-map (kbd "\\") lsp-command-map)

(general-def 'normal lsp-mode :definer 'minor-mode "\\" lsp-command-map)

(general-define-key
 :prefix "\\"
 :states '(normal visual emacs)
 :keymaps 'override
 "=" '(:ignore t :which-key "formatting")
 "F" '(:ignore t :which-key "Folders")
 "G" '(:ignore t :which-key "peek")
 "T" '(:ignore t :which-key "Toggle")
 "a" '(:ignore t :which-key "code actions")
 "g" '(:ignore t :which-key "goto")
 "h" '(:ignore t :which-key "help")
 "r" '(:ignore t :which-key "refactor")
 "w" '(:ignore t :which-key "sessions"))

(provide 'cfg-gen-op-lsp-dap-mode)
;;; cfg-gen-op-lsp-dap-mode.el ends here
