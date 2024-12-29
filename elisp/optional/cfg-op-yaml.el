;;; cfg-op-yaml.el --- configuration for yaml-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-yaml-mode)
  )

(provide 'cfg-op-yaml)
;;; cfg-op-yaml.el ends here
