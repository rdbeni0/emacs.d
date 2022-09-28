;; general-Man-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(Man-mode-map)
 :major-modes '(Man-mode)
 :prefix ","
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs Man-mode)
 :keymaps 'Man-mode-map
 :major-modes 'Man-mode
 "gc" '(Man-goto-section :which-key "Man-goto-section")
 "gb" '(Man-goto-see-also-section :which-key "Man-goto-see-also")
 "Q" 'kill-this-buffer
 "q" 'kill-buffer-and-window)
