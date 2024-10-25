;;; cfg-op-fish-mode.el --- configfuration for fish-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; fish-mode: Emacs major mode for fish shell scripts
(use-package fish-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-fish-mode))

(add-hook 'fish-mode-hook (lambda () (add-hook 'before-save-hook 'fish_indent-before-save)))

(provide 'cfg-op-fish-mode)
;;; cfg-op-fish-mode.el ends here
