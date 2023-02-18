;;; cfg-ediff-diff-vdiff.el --- configure ediff, diff and vdiff -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for ediff, vdiff, diff-mode

;;; Code:

;; ;; ediff
;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html#Customization

(use-package ediff
  :after magit ;; for magit-ediff
  :config
  ;; https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
  (setq-default ediff-forward-word-function 'forward-char)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'cfg-ediff-diff-vdiff)
;;; cfg-ediff-diff-vdiff.el ends here
