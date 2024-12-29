;;; cfg-op-emmet.el --- configuration for emmet -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package emmet-mode
  :ensure t
  :init
  :config
  ;; Code to run after package is loaded
  (my-package-initialize)
  :bind
  :hook
  (web-mode-hook . emmet-mode)
  my-package-command
  :defer t
  ;; Defer loading until needed
  :after (web-mode))


(provide 'cfg-op-emmet)
;;; cfg-op-emmet.el ends here
