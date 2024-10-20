;;; cfg-term.el --- configuration for term-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customization for term-mode:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html
;;
;;; Code:

(use-package term
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-term))

(provide 'cfg-term)
;;; cfg-term.el ends here
