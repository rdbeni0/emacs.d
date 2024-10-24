;;; cfg-op-dark-theme.el --- configuration for additional (optional) dark theme -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package reverse-theme :ensure t)
(load-theme 'reverse t)

(use-package catppuccin-theme :ensure t)
;; (setq catppuccin-flavor 'mocha) ; or ' frappe 'latte, 'macchiato, or 'mocha
;; (load-theme 'catppuccin t)

(use-package zenburn-theme :ensure t)
;; (load-theme 'zenburn t)

;; (load-theme 'wheatgrass t)
;; (load-theme 'deeper-blue t)

(provide 'cfg-op-dark-theme)
;;; cfg-op-dark-theme.el ends here
