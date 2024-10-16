;;; cfg-compile.el --- configfuration for compilation-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for "compilation-mode" and various building's under emacs.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Building.html
;;
;;; Code:

(use-package compile
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-compilation-mode))

(provide 'cfg-compile)
;;; cfg-compile.el ends here
