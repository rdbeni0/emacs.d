;;; cfg-op-emmet.el --- configuration for emmet -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook
            (lambda ()
	      (emmet-mode)
	      ))
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-emmet))

(provide 'cfg-op-emmet)
;;; cfg-op-emmet.el ends here
