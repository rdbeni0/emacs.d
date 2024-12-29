;;; cfg-op-emmet.el --- configuration for emmet -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package emmet-mode
  :ensure t
  :init
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-emmet)
  :hook
  (web-mode-hook . emmet-mode)
  :defer t ;; Defer loading until needed
  :after (web-mode))

(provide 'cfg-op-emmet)
;;; cfg-op-emmet.el ends here
