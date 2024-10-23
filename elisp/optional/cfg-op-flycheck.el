;;; cfg-op-flycheck.el --- configfuration for flycheck -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with flycheck.
;;
;;; Code:

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-flycheck-mode))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  (global-flycheck-eglot-mode 1))

(use-package flycheck-checkbashisms
  :ensure t
  :config
  (flycheck-checkbashisms-setup))

;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
(defun cfg/flycheck-list-errors-below ()
  (interactive)
  (flycheck-list-errors)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer "*Flycheck errors*")))))

;;;; python
;;;; python-mode - linters, checkers...
;;;; we need to know that flycheck has excellent support for python-mode in emacs, but it needs some executables to be installed
(when (require 'cfg-op-python nil 'noerror)
  (progn
   (setq flycheck-python-pylint-executable "~/.local/bin/pylint")
   (setq flycheck-python-mypy-executable "~/.local/bin/mypy")
   (setq flycheck-python-mypy-cache-dir (expand-file-name ".cache/mypy" user-emacs-directory)) ;; https://github.com/python/mypy
   (setq flycheck-pylintrc (expand-file-name ".pylintrc" user-emacs-directory))
   ;; https://www.reddit.com/r/emacs/comments/gqymvz/how_to_force_flycheck_to_select_a_specific_syntax/
   ;; https://www.flycheck.org/en/latest/languages.html#python
   (flycheck-add-next-checker 'python-flake8 'python-pylint 'python-mypy)
   ;; (flycheck-add-next-checker 'python-flake8)
   ))

;;;; php
(when (require 'cfg-op-php nil 'noerror)
  (progn
   ;; https://melpa.org/#/flycheck-phpstan
   (use-package flycheck-phpstan
     :ensure t
     )

   ;; https://melpa.org/#/flycheck-psalm
   (use-package flycheck-psalm
     :ensure t
     )

   (defun cfg/-my-php-mode-setup ()
     "My PHP-mode hook - integration with flycheck."
     (require 'flycheck-phpstan)
     (require 'flycheck-psalm)

     ;;
     ;; In any given buffer where Flycheck is enabled, only one checker may be run at a time.
     ;;
     ;; However, any number of checkers can be run in sequence:
     ;; (defun flycheck-add-next-checker checker next &optional append)
     ;;
     ;; In such a sequence, after the first checker has finished running and its errors have been reported, the next checker of the sequence runs and its errors are reported, etc. until there are no more checkers in the sequence. This sequence is called a checker chain.

     ;; e.g. run "psalm" after "php":
     (flycheck-add-next-checker 'php 'psalm)
     ;; etc:
     (flycheck-add-next-checker 'psalm 'php-phpmd)

     ;; Next may also be a cons cell (level . next-checker), where next-checker is a symbol denoting the syntax checker to run after checker, and level is an error level. The next-checker will then only be run if there is no current error whose level is more severe than level.
     ;; If level is t, then next-checker is run regardless of the current errors.
     ;;
     ;; Run 'phpstan only if 'php-phpmd produced no errors (only warnings and info diagnostics):
     (flycheck-add-next-checker 'php-phpmd '(warning . phpstan))

     ;; (add-to-list 'flycheck-disabled-checkers 'phpstan)
     ;; (flycheck-mode t) ; not required, should be enabled globally
     )

   (add-hook 'php-mode-hook 'cfg/-my-php-mode-setup)))

(provide 'cfg-op-flycheck)
;;; cfg-op-flycheck.el ends here
