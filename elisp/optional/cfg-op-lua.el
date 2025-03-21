;;; cfg-op-lua.el --- configuration for lua -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package lua-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-lua-mode))


(provide 'cfg-op-lua)
;;; cfg-op-lua.el ends here
