;;; cfg-op-e-shell-terminals.el --- configfuration for eshell, shell and terminals -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for: multi-term, ansi-term, shell, terminals, eshell...
;; ... but NOT for shell script editing.
;;
;;; Code:

;; ansi-term and multi-term and:
(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "bash") ;; bash, but not fish
  (setq shell-default-shell 'multi-term)
  (setq term-buffer-maximum-size 0))

;; https://unix.stackexchange.com/questions/182211/automaticaly-rename-new-buffer-in-term-mode
(defun cfg/multi-term-buffer-rn ()
  "Open multi-term buffer with special name and prefix, it could be useful for custom buffer filtering (via tabs or list-buffers)"
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
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-fish-mode))

(add-hook 'fish-mode-hook (lambda () (add-hook 'before-save-hook 'fish_indent-before-save)))

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

(provide 'cfg-op-e-shell-terminals)
;;; cfg-op-e-shell-terminals.el ends here
