;;; cfg-op-pdf-tools.el --- configfuration for pdf-tools  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Config for pdf-tools
;;
;;; Code:

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-pdf-tools))

(provide 'cfg-op-pdf-tools)
;;; cfg-op-pdf-tools.el ends here
