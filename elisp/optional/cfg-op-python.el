;;; cfg-op-python.el --- configfuration for python programming -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with python programming (but without lsp and code completion).
;; Additional and useful packages.
;;
;;; Code:

;;;; https://github.com/pythonic-emacs/anaconda-mode#pythonpath
;; probably something like this can be used in .dir-locals.el:
;; (add-to-list 'python-shell-extra-pythonpaths "/path/to/the/project")


;; https://melpa.org/#/pyimport
(use-package pyimport
  :ensure t
  )

;;;; sphinx-doc - docstring generation
;; https://github.com/naiquevin/sphinx-doc.el
;; https://www.sphinx-doc.org/
(use-package sphinx-doc
  :ensure t
  :config
  (dolist (py-hook '(python-mode-hook python-ts-mode-hook))
    (add-hook py-hook
	      (lambda ()
                (require 'sphinx-doc)
	        (sphinx-doc-mode t))))

;;;; flycheck: https://www.flycheck.org/en/latest/languages.html#python
  ;; we need to know that flycheck has excellent support for python-mode in emacs,
  ;; but it needs some executables to be installed
  (when (require 'flycheck nil 'noerror)

    ;; Additional `flycheck-*' variables:
    ;;
    ;; (setq flycheck-python-pylint-executable "~/.local/bin/pylint")
    ;; (setq flycheck-python-mypy-executable "~/.local/bin/mypy")
    ;; (setq flycheck-python-ruff-executable "/run/current-system/sw/bin/ruff")
    ;; (setq flycheck-pylintrc (expand-file-name ".pylintrc" user-emacs-directory))

    ;; https://github.com/python/mypy
    (setq flycheck-python-mypy-cache-dir
          (expand-file-name ".cache/mypy" user-emacs-directory))

    ;; Ruff additional arguments: turn off line length
    (setq flycheck-python-ruff-args '("--ignore=E501"))

    (dolist (py-hook '(python-mode-hook python-ts-mode-hook))
      (add-hook py-hook
                (lambda ()
                  ;; We usually do NOT use ruff like we do in LSP.
                  ;; But sometimes implementation in LSP can be broken:
                  (flycheck-select-checker 'python-ruff)
                  ;;
                  ;; https://github.com/pylint-dev/pylint
                  ;; (flycheck-select-checker 'python-pylint)
                  ;;
                  )))

    ;; Setting the checker order
    (with-eval-after-load 'flycheck
      ;; Do NOT use ruff and pyright: this is good, but it duplicates with LSP:
      ;; (flycheck-add-next-checker 'python-ruff 'python-pyright)
      ;;
      (flycheck-add-next-checker 'python-ruff 'python-pylint)
      ;; So in this case we will use other engines and linters:
      (flycheck-add-next-checker 'python-pylint 'python-flake8)
      (flycheck-add-next-checker 'python-flake8 'python-pycompile 'python-mypy)
      ))

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-python-mode)

  (provide 'cfg-op-python)
;;; cfg-op-python.el ends here
