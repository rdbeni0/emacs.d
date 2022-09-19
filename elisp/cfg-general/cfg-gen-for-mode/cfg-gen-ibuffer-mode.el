;; general-Man-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(ibuffer-mode-map)
 :major-modes '(ibuffer-mode)
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))
