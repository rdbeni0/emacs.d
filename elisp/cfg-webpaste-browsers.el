;;; cfg-webpaste-browsers.el --- configfuration for webpaste and browsers -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for https://github.com/etu/webpaste.el
;; also for integration with firefox and other browsers

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
              (simpleclip-set-contents url))))

;; default browser: firefox
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "firefox")

(provide 'cfg-webpaste-browsers)
;;; cfg-webpaste-browsers.el ends here
