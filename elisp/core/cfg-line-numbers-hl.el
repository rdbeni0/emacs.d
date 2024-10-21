;;; cfg-line-numbers-hl.el --- configure line numbers and highlighting -*- lexical-binding: t -*-
;;; Commentary:
;;
;; More options:
;; https://www.emacswiki.org/emacs/LineNumbers
;; Disable line numbers in some major modes - via emacswiki:
;; "To disable this in certain major modes you can redefine display-line-numbers--turn-on:"
;;
;;; Code:

(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes '(eshell-mode shell-mode ansi-term-mode erc-mode)
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
(global-hl-line-mode 1) ; highlight current line

(provide 'cfg-line-numbers-hl)
;;; cfg-line-numbers-hl.el ends here
