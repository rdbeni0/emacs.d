;;; cfg-op-tree-sitter.el --- tree sitter -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Tree sitter for Emacs.
;;
;; https://emacs-tree-sitter.github.io/installation/
;; https://tree-sitter.github.io/tree-sitter/
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; "For Emacs 29+, please use the built-in integration instead."
;; https://lists.gnu.org/archive/html/emacs-devel/2022-11/msg01443.html

;;; Code:

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :ensure t)

;; (global-tree-sitter-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/emacs-php/php-ts-mode
;; Since php 8.4 is broken

;; (if (require 'php-ts-mode nil 'noerror)
;;     (message "php-ts-mode is already installed")
;;   (package-vc-install "https://github.com/emacs-php/php-ts-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   ;; (php-mode . php-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)))

(provide 'cfg-op-tree-sitter)
;;; cfg-op-tree-sitter.el ends here
