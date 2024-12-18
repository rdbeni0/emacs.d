;;; cfg-gen-op-ssh-config-mode.el --- general.el ssh-config-mode -*- lexical-binding: t -*-

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'ssh-config-mode-map
 :major-modes 'ssh-config-mode
 :prefix ","
 "m"  '(ssh-config-host-next :which-key "host-next")
 "n"  '(ssh-config-host-prev :which-key "host-prev")
 "l"  '(ssh-config-completion-at-point :which-key "completion-at-point")
 "."  '(ssh-config-compute-indent :which-key "compute-indent"))

(provide 'cfg-gen-op-ssh-config-mode)
;;; cfg-gen-op-ssh-config-mode.el ends here
