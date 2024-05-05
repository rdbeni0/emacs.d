(general-define-key
 :states '(normal visual emacs)
 :keymaps 'nix-mode-map
 :major-modes 'nix-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "=n" '(nix-format-buffer :which-key "nix-format-buffer"))
