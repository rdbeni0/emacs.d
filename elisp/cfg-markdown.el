;;; cfg-markdown.el --- markdown-mode -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for markdown-mode

;;; Code:

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "/usr/sbin/pandoc")
  )

(provide 'cfg-markdown)
;;; cfg-markdown.el ends here
