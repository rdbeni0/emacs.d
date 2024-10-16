;;; cfg-op-webpaste.el --- configfuration for webpaste -*- lexical-binding: t -*-
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

(provide 'cfg-op-webpaste)
;;; cfg-op-webpaste.el ends here
