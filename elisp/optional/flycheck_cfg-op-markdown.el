;;; cfg-op-markdown.el --- markdown-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for markdown-mode
;;
;;; Code:

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc")
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-markdown-mode))

;; evil-markdown-mode
;; https://github.com/Somelauw/evil-markdown
;; this is local package
(use-package evil-markdown
  :ensure t
  :after '(evil markdown-mode))

(provide 'cfg-op-markdown)
;;; cfg-op-markdown.el ends here
