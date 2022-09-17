;; general-Man-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(Man-mode-map)
 :major-modes '(Man-mode)
 :prefix ","
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(Man-mode-map)
 :major-modes '(Man-mode)
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer"))
