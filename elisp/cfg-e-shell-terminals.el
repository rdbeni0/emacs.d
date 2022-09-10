;;; cfg-e-shell-terminals.el --- configfuration for eshell, shell and terminals -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for: multi-term, ansi-term, shell, terminals, eshell...
;; ... but NOT for shell script editing.

;;; Code:

;; vterm and multi-vterm:

(use-package vterm
  :ensure t
  :config
  (setq vterm-clear-scrollback t)

  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq hl-line-mode -1)
	      (setq-local global-hl-line-mode nil)
	      )))

(use-package multi-vterm
  :ensure t
  ;; :config
  )

;; ansi-term and multi-term and:

(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/bin/bash") ;; bash, but not fish
  (setq shell-default-shell 'multi-term)
  (setq term-buffer-maximum-size 0))

;; https://unix.stackexchange.com/questions/182211/automaticaly-rename-new-buffer-in-term-mode

(defun cfg/multi-term-buffer-rn ()
  "Open multi-term buffer with special name and prefix, it could be useful for custom buffer filtering (via tabs or helm)"
  (interactive)
  (require 'multi-term)
  (command-execute 'multi-term)
  (setq-default truncate-lines nil)
  (if (not (boundp 'term-number))
      (defvar term-number 1 "term index in the current emacs session") )
  (rename-buffer (concat ">" (int-to-string term-number) " term" ))
  (setq term-number (+ 1 term-number)))

(setq term-suppress-hard-newline t)

;; fish-mode: Emacs major mode for fish shell scripts.

(use-package fish-mode
  :ensure t
  )

(add-hook 'fish-mode-hook (lambda () (add-hook 'before-save-hook 'fish_indent-before-save)))

;; comint

(setq comint-prompt-read-only t)
(setq comint-process-echoes t)

;; M-x shell mode :

;; colors:
(use-package xterm-color
  :ensure t
  :config
  (setq comint-output-filter-functions
	(remove 'ansi-color-process-output comint-output-filter-functions))

  (add-hook 'shell-mode-hook
	    (lambda ()
	      ;; Disable font-locking in this buffer to improve performance
	      (font-lock-mode -1)
	      ;; Prevent font-locking from being re-enabled in this buffer
	      (make-local-variable 'font-lock-function)
	      (setq font-lock-function (lambda (_) nil))
	      (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))


(setq explicit-shell-file-name "/bin/bash") ;; M-x shell default shell:

;; https://stackoverflow.com/questions/9514495/how-to-define-a-function-to-run-multiple-shells-on-emacs

(defun cfg/my-named-shell ()
  "Equivalent to C-u M-x shell RET"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'shell)))

(lambda () (shell-dirtrack-mode t) (setq dirtrackp nil))


(provide 'cfg-e-shell-terminals)
;;; cfg-e-shell-terminals.el ends here
