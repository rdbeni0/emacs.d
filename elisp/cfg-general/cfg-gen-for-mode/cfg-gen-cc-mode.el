;; general-cc-mode (c++, c):

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(cc-mode-map c-mode-map c++-mode-map)
 :major-modes '(cc-mode c-mode c++-mode)
 :prefix ","
 "c"  '(compile :which-key "compile"))


(general-define-key
 :states '(normal visual emacs)
 :keymaps '(compilation-mode-map)
 :major-modes '(compilation-mode)
 :prefix ","
 "k"  '(kill-compilation :which-key "kill-compilation")
 "]"  '(compilation-next-error :which-key "next-error")
 "["  '(compilation-previous-error :which-key "prev-error")
 "j"  '(compilation-recompile :which-key "recompile")
 "s"  '(compilation-shell-minor-mode :which-key "compilation-shell-minor-mode"))
