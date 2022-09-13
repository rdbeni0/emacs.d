;; general-grep-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(grep-mode-map wgrep-mode-map helm-grep-mode-map)
 :major-modes '(grep-mode wgrep-mode helm-grep-mode)
 :prefix ","
 "g" '(grep-mode :which-key "grep-mode")
 "p" '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
 "e" '(wgrep-exit :which-key "wg-exit")
 "a" '(wgrep-save-all-buffers :which-key "wg-save-all-buffers")
 "Z" '(wgrep-finish-edit :which-key "wg-finish-edit")
 "Q" '(wgrep-abort-changes :which-key "wg-abort-changes")
 "r" '(wgrep-toggle-readonly-area :which-key "wg-toggle-readonly-area"))
