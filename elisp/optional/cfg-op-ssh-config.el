;;; cfg-op-ssh-config.el --- configfuration for editing ssh files -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;;;;;; ssh-config-mode
;; https://github.com/jhgorrell/ssh-config-mode-el
(use-package ssh-config-mode
  :ensure t
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-ssh-config-mode))

(provide 'cfg-op-ssh-config)
;;; cfg-op-ssh-config.el ends here
