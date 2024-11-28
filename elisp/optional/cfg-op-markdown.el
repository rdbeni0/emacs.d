;;; cfg-op-markdown.el --- markdown-mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for markdown-mode.
;; https://jblevins.org/projects/markdown-mode/
;;
;;; Code:

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc")

  ;; https://github.com/Somelauw/evil-markdown
  ;; this is local package (should be loaded from site-elisp):
  (add-hook 'markdown-mode-hook (lambda ()
				  (require 'evil-markdown)
				  (evil-markdown-mode)))
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-markdown-mode))

(provide 'cfg-op-markdown)
;;; cfg-op-markdown.el ends here
