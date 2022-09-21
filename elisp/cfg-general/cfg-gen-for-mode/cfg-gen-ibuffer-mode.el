;; general-ibuffer-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(ibuffer-mode-map)
 :major-modes '(ibuffer-mode)
 "q"  'kill-buffer-and-window
 "Q"  'kill-this-buffer)
