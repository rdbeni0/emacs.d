;;; cfg-op-webpaste-htmlize.el --- configfuration for webpaste -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for https://github.com/etu/webpaste.el
;;
;;; Code:

(use-package webpaste
  :ensure t
  :init
  (require 'request)
  :config
  (progn
    (setq webpaste-provider-priority '("ix.io" "dpaste.de")))
  (add-hook 'webpaste-return-url-hook
            (lambda (url)
              (message "Copied URL to clipboard: %S" url)
              (simpleclip-set-contents url)))
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-webpaste))

;; https://github.com/hniksic/emacs-htmlize
(use-package htmlize
  :ensure t)

(provide 'cfg-op-webpaste-htmlize)
;;; cfg-op-webpaste-htmlize.el ends here
