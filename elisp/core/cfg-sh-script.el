;;; cfg-sh-script.el --- configfuration for sh-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; shell-script-mode (shell, bash - for editing files):
;; "shell-script-mode is an alias for ‘sh-mode’ in ‘sh-script.el’."
;;
;;; Code:

(use-package sh-script
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-sh-mode))

(provide 'cfg-sh-script)
;;; cfg-sh-script.el ends here
