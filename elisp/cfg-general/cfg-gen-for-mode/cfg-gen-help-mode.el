;; general-help-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(help-mode-map)
 :major-modes '(help-mode)
 :prefix ","
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(help-mode-map)
 :major-modes '(help-mode)
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window"))
