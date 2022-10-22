;;; cfg-web-front-end.el --- configfuration for front-end development (web) -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with: js/html/css, and modern front end (web stack)

;;; Code:

;; https://github.com/fxbois/web-mode
;; https://web-mode.org/
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
        web-mode-enable-current-element-highlight t))

;; OPTIONAL: https://github.com/skeeto/skewer-mode - REPL inside the firefox

(provide 'cfg-web-front-end)
;;; cfg-web-front-end.el ends here
