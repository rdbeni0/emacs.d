;; general-compilation-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(compilation-mode-map)
 :major-modes '(compilation-mode)
 :prefix ","
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs Man-mode)
 :keymaps 'compilation-mode-map
 :major-modes 'compilation-mode
 "Q" 'kill-this-buffer
 "q" 'kill-buffer-and-window)
