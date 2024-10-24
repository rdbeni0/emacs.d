;;; cfg-op-evil.el --- evil mode add ons -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Various add ons for evil-mode for emacs.
;;
;;; Code:

;; https://github.com/emacsorphanage/evil-anzu
;; Please also configure modeline after usage of this plugin...
(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode +1))

;; https://github.com/TheBB/evil-indent-plus
(use-package evil-indent-plus
  :after 'evil
  :config
  (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" 'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" 'evil-indent-plus-a-indent-up-down))

;; this simple package will allow paste and overwrite during visual mode in evil package
(use-package simpleclip
  :ensure t
  :config
  (simpleclip-mode 1))

(provide 'cfg-op-evil)
;;; cfg-op-evil.el ends here
