;;; cfg-gen-op-fish-mode.el --- general.el for fish-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'fish-mode-map
 :major-modes 'fish-mode
 :prefix ","
 "=t" '(fish_indent :which-key "fish_indent"))

 (provide 'cfg-gen-op-fish-mode)
;;; cfg-gen-op-fish-mode.el ends here
