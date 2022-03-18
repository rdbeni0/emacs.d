;;; cfg-python.el --- configfuration for python programming -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with python programming. But without LSP.

;;; Code:

(use-package yapfify
  :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'yapf-mode)
  )

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  )

(provide 'cfg-python)
;;; cfg-python.el ends here
