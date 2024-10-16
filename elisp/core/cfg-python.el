;;; cfg-python.el --- configfuration for python -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with built-in python programming.
;;
;;; Code:

(use-package python
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-python-mode))

(provide 'cfg-python)
;;; cfg-python.el ends here
