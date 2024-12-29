;;; cfg-op-web-mode.el --- configfuration for front-end development (web) -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with web-mode
;; https://web-mode.org/
;; https://github.com/fxbois/web-mode
;; https://themkat.net/2022/10/04/emacs_web_mode_mixed.html
;; https://github.com/smihica/emmet-mode - unmaintained, but still useful
;;
;;; Code:

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-engines-alist
	'(("php"    . "\\.phtml\\'")
          ("blade"  . "\\.blade\\.")))
  (setq web-mode-enable-css-colorization t
	web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-quoting t
        web-mode-enable-current-column-highlight t
        web-mode-enable-current-element-highlight t)
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-web-mode))

;; OPTIONAL: https://github.com/skeeto/skewer-mode - REPL inside the web browser

;; https://github.com/osv/company-web
(use-package company-web
  :ensure t
  :config
  (require 'company-web-html)
  ;;;; Additional company-web- backends should be loaded if required:
  ;; (require 'company-web-jade) ; load company mode jade backend
  ;; (require 'company-web-slim)
  :defer t
  :after (company))

(provide 'cfg-op-web-mode)
;;; cfg-op-web-mode.el ends here
