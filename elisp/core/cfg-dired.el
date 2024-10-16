;;; cfg-dired.el --- configfuration for dired -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with "dired".
;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
;;
;;; Code:

(use-package dired
  :config
  ;; for dired-jump
  (when (< emacs-major-version 28) (require 'dired-x))
  ;; http://xahlee.info/emacs/emacs/emacs_dired_tips.html
  ;; "Make dired use the same buffer for viewing directory":
  (when (>= emacs-major-version 28)
    (setq dired-kill-when-opening-new-dired-buffer t))

  (when (< emacs-major-version 28)
    (progn
      (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
      (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
      ))

  ;; When you do copy files, emacs prompts for a target dir.
  ;; You can make emacs automatically suggest the target dir on the split pane.
  (setq dired-dwim-target t)

  ;; load general.el and keybindings:
  (require 'cfg-gen-co-dired-mode))

(defcustom list-of-dired-switches
  '("-l" "-la" "-lA" "-lA --group-directories-first")
  "List of ls switches for dired to cycle among.")


(defun cfg/cycle-dired-switches ()
  "Cycle through the list `list-of-dired-switches' of switches for ls"
  (interactive)
  (setq list-of-dired-switches
        (append (cdr list-of-dired-switches)
                (list (car list-of-dired-switches))))
  (dired-sort-other (car list-of-dired-switches)))

(provide 'cfg-dired)
;;; cfg-dired.el ends here
