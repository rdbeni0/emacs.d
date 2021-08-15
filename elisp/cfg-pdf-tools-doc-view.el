;;; cfg-pdf-tools-doc-view.el --- configfuration for pdf-tools and DocView  -*- lexical-binding: t -*-
;;; Commentary:

;; Config for: pdf-tools and DocView

;;; Code:

;; DocVIew

(use-package doc-view
  :config
  (setq doc-view-resolution 150)
  (setq doc-view-scale-internally nil)
  )

;; pdf-tools

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  )

(provide 'cfg-pdf-tools-doc-view)
;;; cfg-pdf-tools-doc-view.el ends here
