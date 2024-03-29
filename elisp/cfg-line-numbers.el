;;; cfg-line-numbers.el --- configure line numbers -*- lexical-binding: t -*-
;;; Commentary:

;; More options:
;; https://www.emacswiki.org/emacs/LineNumbers

;;; Code:

(require 'display-line-numbers)

;; emacswiki:
;; "To disable this in certain major modes you can redefine display-line-numbers--turn-on:"

(defcustom display-line-numbers-exempt-modes '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode erc-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on () ;; no cfg/  here
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)

;;  https://github.com/DarthFennec/highlight-indent-guides

(use-package highlight-indent-guides
  :ensure t
  :config
  (setq highlight-indent-guides-method 'bitmap)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
  ;; (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(provide 'cfg-line-numbers)
;;; cfg-line-numbers.el ends here
