;; general-help-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(help-mode-map)
 :major-modes '(help-mode)
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(help-mode-map)
 :major-modes '(help-mode)
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer"))
