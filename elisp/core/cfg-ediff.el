;;; cfg-ediff.el --- configure ediff, diff and vdiff -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Customization for ediff:
;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html#Customization
;;
;;; Code:

(use-package ediff
  :config
  ;; https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
  (setq-default ediff-forward-word-function 'forward-char)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-ediff-mode))

(provide 'cfg-ediff)
;;; cfg-ediff.el ends here
