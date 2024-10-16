;;; cfg-emacs-lisp.el --- configfuration for emacs-lisp mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package elisp-mode
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-emacs-lisp-mode))

(provide 'cfg-emacs-lisp)
;;; cfg-emacs-lisp.el ends here
