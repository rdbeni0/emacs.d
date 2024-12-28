;;; cfg-op-python.el --- configfuration for python programming -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with python programming (but without lsp and code completion).
;; Additional and useful packages.
;;
;;; Code:

;;;; https://github.com/pythonic-emacs/anaconda-mode#pythonpath
;; probably something like this should be used in dir-locals.el:
;; (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")

;; load general.el and keybindings:
(require 'cfg-gen-op-python-mode)

;;;; pyimport
;; https://melpa.org/#/pyimport
(use-package pyimport
  :ensure t
  )

;;;; sphinx-doc - docstring generation
(use-package sphinx-doc
  :ensure t
  :config
  (add-hook 'python-mode-hook
	    (lambda ()
	      (sphinx-doc-mode t))))

;;;; integration with flycheck
;; python-mode - linters, checkers...
;; we need to know that flycheck has excellent support for python-mode in emacs, but it needs some executables to be installed
(when (require 'flycheck nil 'noerror)
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

(provide 'cfg-op-python)
;;; cfg-op-python.el ends here
