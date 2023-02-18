;; general-shell-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(shell-mode-map)
 :major-modes 'shell-mode
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "c" '(comint-clear-buffer :which-key "clear")
 "i" '(comint-send-invisible :which-key "send-invisible")
 "f" '(find-file :which-key "find-file"))
