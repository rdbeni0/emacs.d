;; general-cc-mode (c++, c):

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(cc-mode-map c-mode-map c++-mode-map)
 :major-modes '(cc-mode c-mode c++-mode)
 :prefix ","
 "c"  '(compile :which-key "compile")
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
 )
