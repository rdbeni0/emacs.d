;;; cfg-gen-op-quelpa.el --- general.el for quelpa -*- lexical-binding: t -*-

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"
 "apq"  '(:ignore 1 :which-key "quelpa")
 "apqu" '(quelpa-upgrade-all :which-key "quelpa-upgrade-all")
 "apq1" '(quelpa-upgrade :which-key "quelpa-upgrade")
 "apqc" '(quelpa-checkout-melpa :which-key "checkout-melpa"))

(provide 'cfg-gen-op-quelpa)
;;; cfg-gen-op-quelpa.el ends here
