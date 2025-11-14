;;; cfg-gen-co-cc-mode.el --- general.el for cc-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(cc-mode-map c-mode-map c++-mode-map c-ts-mode-map)
 :major-modes '(cc-mode c-mode c++-mode c-ts-mode)
 :prefix ","
 "c"  '(compile :which-key "compile"))

(provide 'cfg-gen-co-cc-mode)
;;; cfg-gen-co-cc-mode.el ends here
