;;; cfg-shell-comint.el --- configfuration for shell-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; "shell-mode" is major mode for interacting with an inferior shell.
;;
;;; Code:

;; M-x shell default shell:
(use-package shell
  :config
  ;; https://stackoverflow.com/questions/9514495/how-to-define-a-function-to-run-multiple-shells-on-emacs
  (defun cfg/C-u-M-x-shell ()
    "Equivalent to C-u M-x shell RET"
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'shell)))
  ;; dirtrack-mode:
  (shell-dirtrack-mode t)
  (setq explicit-shell-file-name "bash")
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-shell-mode))

;; comint
(setq comint-prompt-read-only t)
(setq comint-process-echoes t)

(provide 'cfg-shell-comint)
;;; cfg-shell-comint.el ends here
