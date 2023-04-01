;; format the code:

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-format
 :major-modes list-gen-mode-format
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
 "=b" '(format-all-buffer :which-key "format-all-buffer")
 "=-" '(indent-region :which-key "indent-region")
 "=]" '(format-all-region :which-key "format-all-region"))
