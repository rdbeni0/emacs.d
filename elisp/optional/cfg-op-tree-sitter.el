;;; cfg-op-core-tree-sitter.el --- tree sitter -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree sitter for Emacs.
;;
;; https://emacs-tree-sitter.github.io/installation/
;; https://tree-sitter.github.io/tree-sitter/
;; "For Emacs 29+, please use the built-in integration instead."
;; https://lists.gnu.org/archive/html/emacs-devel/2022-11/msg01443.html

;;; Code:

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

(global-tree-sitter-mode)

(provide 'cfg-op-core-tree-sitter)
;;; cfg-op-core-tree-sitter.el ends here
