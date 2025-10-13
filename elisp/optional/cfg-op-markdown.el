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

(define-minor-mode markdown-live-preview-bottom-mode
  "Toggle native previewing on save for a specific markdown file, with preview at the bottom."
  :lighter " MD-Preview-Bottom"
  (let ((markdown-split-window-direction 'below))
    (if markdown-live-preview-bottom-mode
        (if (markdown-live-preview-get-filename)
            (markdown-display-buffer-other-window (markdown-live-preview-export))
          (markdown-live-preview-bottom-mode -1)
          (user-error "Buffer %s does not visit a file" (current-buffer)))
      (markdown-live-preview-remove))))

(provide 'cfg-op-markdown)
;;; cfg-op-markdown.el ends here
