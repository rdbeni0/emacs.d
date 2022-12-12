;; general-cc-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(cc-mode-map c-mode-map)
 :major-modes '(cc-mode c-mode)
 :prefix ","
 "`"  '(compile :which-key "compile")
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
)
