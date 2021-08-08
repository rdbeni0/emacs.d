;;; cfg-vdiff-diff-ediff.el --- configure vdiff, diff-mode and ediff -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for vdiff, diff-mode and ediff

;;; Code:

(use-package vdiff
  :after evil
  :ensure t
  :config
  (evil-define-key 'normal vdiff-mode-map "Z" vdiff-mode-prefix-map)
  )

;; https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
(setq-default ediff-forward-word-function 'forward-char)

(provide 'cfg-vdiff-diff-ediff)
;;; cfg-vdiff-diff-ediff.el ends here
