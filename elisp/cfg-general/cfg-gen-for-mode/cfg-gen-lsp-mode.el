;; general-lsp-mode : TODO

;; https://github.com/emacs-lsp/lsp-mode/issues/1530
;; ^ evil modes and lsp-mode prefix-key on Gitter
;; So in that case it should be mapped as: *evil-defined-key*

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
 "w" '(:ignore t :which-key "sessions")

 "\\" '(:ignore t :which-key "eglot")
 "\\\\" '(eglot :which-key "eglot")
 "\\/" '(eglot-rename :which-key "rename")
 "\\b" '(eglot-format-buffer :which-key "format-buffer")
 "\\p" '(eglot-help-at-point :which-key "help-at-point")
 "\\o" '(eglot-code-actions :which-key "code-actions")
 "\\c" '(eglot-clear-status :which-key "clear-status")
 "\\=" '(eglot-format :which-key "format")
 "\\w" '(eglot-shutdown :which-key "shutdown")
 "\\r" '(eglot-reconnect :which-key "reconnect")
 "\\e" '(eglot-stderr-buffer :which-key "stderr-buffer")
 "\\t" '(eglot-events-buffer :which-key "events-buffer")
 "\\l" '(eglot-find-declaration :which-key "find-declaration")
 "\\n" '(eglot-find-typeDefinition :which-key "find-typeDefinition")
 "\\m" '(eglot--managed-mode :which-key "managed-mode")
 "\\s" '(eglot-forget-pending-continuations :which-key "forget-pending-continuations")
 "\\i" '(eglot-find-implementation :which-key "find-implementation")
 "\\a" '(eglot-signal-didChangeConfiguration :which-key "didChangeConfiguration"))
