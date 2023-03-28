;; general-fish-mode:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'fish-mode-map
 :major-modes 'fish-mode
 :prefix ","
 "=t" '(fish_indent :which-key "fish_indent"))
