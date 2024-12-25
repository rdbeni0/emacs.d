;;; cfg-gen-co-eglot-mode.el --- eglot keybindings -*- lexical-binding: t -*-

;; https://joaotavora.github.io/eglot/#Eglot-Commands
;; https://www.gnu.org/software/emacs/manual/html_node/eglot/Eglot-Commands.html
(general-define-key
 :prefix "\\"
 :states '(normal visual emacs)
 :keymaps 'override
 "=" '(eglot-format :which-key "format")
 "0" '(eglot-format-buffer :which-key "format-buffer")
 ";" '(eglot-rename :which-key "rename")
 "'" '(eglot-code-actions :which-key "code-actions")
 "[" '(eglot-find-typeDefinition :which-key "find-typeDefinition")
 "]" '(eglot-find-implementation :which-key "find-implementation")
 "p" '(eglot-find-declaration :which-key "find-declaration")
 "m" '(eglot--managed-mode :which-key "managed-mode")
 "\\" '(:ignore t :which-key "server")
 "\\a" '(eglot-signal-didChangeConfiguration :which-key "didChangeConfiguration")
 "\\c" '(eglot-clear-status :which-key "clear-status")
 "\\e" '(eglot-stderr-buffer :which-key "stderr-buffer")
 "\\p" '(eglot-forget-pending-continuations :which-key "forget-pending-continuations")
 "\\r" '(eglot-reconnect :which-key "reconnect")
 "\\s" '(eglot :which-key "eglot")
 "\\t" '(eglot-events-buffer :which-key "events-buffer")
 "\\w" '(eglot-shutdown :which-key "shutdown"))

(provide 'cfg-gen-co-eglot-mode)
;;; cfg-gen-co-eglot-mode.el ends here
