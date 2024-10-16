;;; cfg-doc-view-image.el --- configfuration for doc-view and image mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;
;;; Code:

(use-package doc-view
  :config
  (setq doc-view-resolution 150)
  (setq doc-view-scale-internally nil)
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-doc-view-mode))

(use-package image-mode
  :config
  (require 'cfg-gen-co-image-mode))

(provide 'cfg-doc-view-image)
;;; cfg-doc-view-image.el ends here
