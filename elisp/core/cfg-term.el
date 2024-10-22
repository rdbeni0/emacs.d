;;; cfg-term.el --- configuration for term-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customization for term-mode:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Term-Mode.html
;;
;;; Code:

(use-package term
  :config
  ;; The maximum size in lines for term buffers.
  ;; Term buffers are truncated from the top to be no greater than this number.
  ;; Notice that a setting of "0" means "don’t truncate anything".
  (setq term-buffer-maximum-size 0)
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-term))

(provide 'cfg-term)
;;; cfg-term.el ends here
