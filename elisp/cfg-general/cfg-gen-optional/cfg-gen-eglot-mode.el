;; general-eglot-mode : TODO

(general-define-key
 :prefix "\\"
 :states '(normal visual emacs)
 :keymaps 'override

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

(provide 'cfg-gen-eglot-mode)
;;; cfg-gen-eglot-mode.el ends here
