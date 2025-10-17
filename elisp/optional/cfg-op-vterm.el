;;; cfg-op-vterm.el --- configuration for vterm -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; vterm and multi-vterm:
(use-package vterm
  :ensure t
  :config
  (setq vterm-clear-scrollback t)
  (setq vterm-max-scrollback 99999)

  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq hl-line-mode -1)
	      (setq-local global-hl-line-mode nil)
	      ))
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-vterm-mode))

(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  (setq multi-vterm-buffer-name "vterm"))

(provide 'cfg-op-vterm)
;;; cfg-op-vterm.el ends here
