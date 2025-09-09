;;; cfg-gen-op-embark.el --- general.el for various embark modes -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(embark-collect-mode-map)
 :major-modes '(embark-collect-mode)
 :prefix ","
 "/" '(ffap :which-key "act_ffap")
 "g" '(grep-mode :which-key "grep-mode")
 "p" '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
 "e" '(wgrep-exit :which-key "wg-exit")
 "a" '(wgrep-save-all-buffers :which-key "wg-save-all-buffers")
 "Z" '(wgrep-finish-edit :which-key "wg-finish-edit")
 "Q" '(wgrep-abort-changes :which-key "wg-abort-changes")
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "r" '(wgrep-toggle-readonly-area :which-key "wg-toggle-readonly-area"))

(provide 'cfg-gen-op-embark)
;;; cfg-gen-op-embark.el ends here
