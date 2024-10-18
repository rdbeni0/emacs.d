;;; cfg-man-help.el --- configfuration for man and help-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(defun cfg/show-major-mode ()
  "Display the current major mode in the minibuffer."
  (interactive)
  (message "Current major mode: %s" major-mode))

(use-package help-mode
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-help-mode))

(use-package man
  :config
  (require 'cfg-gen-co-man-mode))

(provide 'cfg-man-help)
;;; cfg-man-help.el ends here
