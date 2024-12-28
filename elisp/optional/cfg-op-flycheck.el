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

  ;; Maximum errors allowed per syntax checker.
  ;; The value of this variable is either an integer denoting the
  ;; maximum number of errors per syntax checker and buffer, or nil to
  ;; not limit the errors reported from a syntax checker.
  ;; If this variable is a number and a syntax checker reports more
  ;; errors than the value of this variable, its errors are not
  ;; discarded, and not highlighted in the buffer or available in the
  ;; error list.  The affected syntax checker is also disabled for
  ;; future syntax checks of the buffer.
  ;; Do NOT disable checker:
  (setq flycheck-checker-error-threshold nil)

  ;; add some delay for better performance
  (setq flycheck-display-errors-delay 0.8)

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-flycheck-mode))

;; https://github.com/flycheck/flycheck-eglot
(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  ;; By default, the Flycheck-Eglot considers the Eglot to be the only provider of syntax checks.
  ;; The LSP-mode with Flycheck and the Eglot with Flymake behave in a similar way. It is assumed that all suitable checkers are plugged in the LSP server. In most cases, this is what you need. However, in case you need to use an Eglot checker in parallel with regular Flycheck checkers, there is a variable flycheck-eglot-exclusive that controls this. You can override it system wide:
  ;; :custom (flycheck-eglot-exclusive nil)
  ;; ... or "per major mode" (via hook).
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  ;; should be enabled/disabled manually via "M-x flycheck-eglot-mode":
  ;; After quite thorough checking, it turned out that this mode must be enabled:
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

(defun cfg/flycheck-enable-checker ()
  "Enable disabled checker in flycheck."
  (interactive)
  (let ((current-prefix-arg '(4))) ; Sets the prefix argument to C-u
    (call-interactively #'flycheck-disable-checker)))

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

(provide 'cfg-op-flycheck)
;;; cfg-op-flycheck.el ends here
