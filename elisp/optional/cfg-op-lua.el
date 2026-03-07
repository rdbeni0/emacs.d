;;; cfg-op-lua.el --- configuration for lua -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package lua-mode
  :ensure t
  :config
  (add-hook 'lua-mode-hook
            (lambda ()
              (setq lua-indent-level 4)
              (setq indent-tabs-mode nil)   ;; use spaces
              (setq tab-width 4)))

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-lua-mode))


(provide 'cfg-op-lua)
;;; cfg-op-lua.el ends here
