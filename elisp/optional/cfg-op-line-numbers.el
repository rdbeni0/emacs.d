;;; cfg-op-line-numbers.el --- configure lines and line numbers -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;;  https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'bitmap)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(provide 'cfg-op-line-numbers)
;;; cfg-op-line-numbers.el ends here
