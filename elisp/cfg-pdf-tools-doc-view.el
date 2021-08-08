;;; cfg-pdf-tools-doc-view.el --- configfuration for pdf-tools and DocView  -*- lexical-binding: t -*-
;;; Commentary:

;; Config for: pdf-tools and DocView

;;; Code:

;; DocVIew

(use-package doc-view
  :ensure t
  :config
  (define-key doc-view-mode-map (kbd "C-<mouse-4>") 'doc-view-enlarge)
  (define-key doc-view-mode-map (kbd "C-<mouse-5>") 'doc-view-shrink)
  (define-key doc-view-mode-map (kbd "<right>") 'doc-view-next-page)
  (define-key doc-view-mode-map (kbd "<left>") 'doc-view-previous-page)
  (define-key doc-view-mode-map (kbd "<home>") 'doc-view-first-page)
  (define-key doc-view-mode-map (kbd "<end>") 'doc-view-last-page)
  (define-key doc-view-mode-map (kbd "k") 'doc-view-shrink)
  (define-key doc-view-mode-map (kbd "l") 'doc-view-enlarge)
  (define-key doc-view-mode-map (kbd "/") 'doc-view-search)
  (define-key doc-view-mode-map (kbd "W") 'doc-view-fit-width-to-window)
  (define-key doc-view-mode-map (kbd "H") 'doc-view-fit-height-to-window)
  (define-key doc-view-mode-map (kbd "P") 'doc-view-fit-page-to-window)
  (define-key doc-view-mode-map (kbd "0") 'doc-view-scale-reset)

  (setq doc-view-resolution 150)
  (setq doc-view-scale-internally nil)
  )

;; pdf-tools

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (define-key pdf-view-mode-map (kbd "C-<mouse-4>") 'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "C-<mouse-5>") 'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "<right>") 'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "<left>") 'pdf-view-previous-page)
  (define-key pdf-view-mode-map (kbd "<home>") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "<end>") 'pdf-view-last-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-shrink)
  (define-key pdf-view-mode-map (kbd "l") 'pdf-view-enlarge)
  (define-key pdf-view-mode-map (kbd "/") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "W") 'pdf-view-fit-width-to-window)
  (define-key pdf-view-mode-map (kbd "H") 'pdf-view-fit-height-to-window)
  (define-key pdf-view-mode-map (kbd "P") 'pdf-view-fit-page-to-window)
  (define-key pdf-view-mode-map (kbd "0") 'pdf-view-scale-reset)
  )

(provide 'cfg-pdf-tools-doc-view)
;;; cfg-pdf-tools-doc-view.el ends here
