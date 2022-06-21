;;; cfg-fly-modes.el --- configfuration for flycheck, flyspell, flymake -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with "fly-modes": flycheck, flyspell and flymake

;;; Code:

;; Dependencies for flyspell: *aspell* dictionaries
;; For example - Arch Linux packages: aspell-en aspell-pl

(use-package flyspell
  :init
  (setq flyspell-default-dictionary "english")
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)

  ;; https://www.reddit.com/r/emacs/comments/gqymvz/how_to_force_flycheck_to_select_a_specific_syntax/
  ;; https://www.flycheck.org/en/latest/languages.html#python
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  ;; (flycheck-add-next-checker 'python-flake8)
  )

(use-package flycheck-checkbashisms
  :ensure t
  :config
  (flycheck-checkbashisms-setup)
  )

(provide 'cfg-fly-modes)
;;; cfg-fly-modes.el ends here
