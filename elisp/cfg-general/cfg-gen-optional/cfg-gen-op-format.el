;;; cfg-gen-op-format.el ---  general.el for code formatters -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-format-optional
 :major-modes list-gen-mode-format-optional
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "==" '(format-all-region-or-buffer :which-key "format-all")
 "=]" '(format-all-buffer :which-key "format-all-buffer")
 "=[" '(format-all-region :which-key "format-all-region"))

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "bF"  '(format-all-buffer :which-key "format-all-buffer")
 "bmf" '(format-all-mode :which-key "format-all-mode"))

(provide 'cfg-gen-op-format)
;;; cfg-gen-op-format.el ends here
