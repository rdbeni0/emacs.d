;; general-fish-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'fish-mode-map
 :major-modes 'fish-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-buffer :which-key "format-all-buffer")
 "=t" '(fish_indent :which-key "fish_indent"))
