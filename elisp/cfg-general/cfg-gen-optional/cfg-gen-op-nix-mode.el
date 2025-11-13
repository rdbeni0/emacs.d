;;; cfg-gen-op-nix-mode.el --- general.el for nix-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(nix-mode-map nix-ts-mode-map)
 :major-modes '(nix-mode nix-ts-mode)
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "=n" '(nix-format-buffer :which-key "nix-format-buffer"))

(provide 'cfg-gen-op-nix-mode)
;;; cfg-gen-op-nix-mode.el ends here
