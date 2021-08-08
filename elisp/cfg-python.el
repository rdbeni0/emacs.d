;;; cfg-python.el --- configfuration for python programming -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with python programming.

;;; Code:

(use-package yapfify
  :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'yapf-mode)
  )

(provide 'cfg-python)
;;; cfg-python.el ends here
