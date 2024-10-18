;;; cfg-gen-co-compilation-mode.el --- general.el for compilation-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(compilation-mode-map)
 :major-modes '(compilation-mode)
 :prefix ","
 "k"  '(kill-compilation :which-key "kill-compilation")
 "]"  '(compilation-next-error :which-key "next-error")
 "["  '(compilation-previous-error :which-key "prev-error")
 "j"  '(compilation-recompile :which-key "recompile")
 "s"  '(compilation-shell-minor-mode :which-key "compilation-shell-minor-mode")
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

 (general-define-key
 :states '(normal visual emacs Man-mode)
 :keymaps 'compilation-mode-map
 :major-modes 'compilation-mode
 "Q" 'kill-this-buffer
 "q" 'kill-buffer-and-window)

(provide 'cfg-gen-co-compilation-mode)
;;; cfg-gen-co-compilation-mode.el ends here
