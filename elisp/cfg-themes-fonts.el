;;; cfg-themes-fonts.el --- configure themes and fonts -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for themes (black) and fonts.
;; List of interesting themes:
;; toxi, tangotango, tango-2, odersky, wilson, reverse, monokai, tsdh-dark, afternoon, ample-zen, darkokai
;; good themes-fonts for small screen:  tango-dark, wheatgrass

;;; Code:

;; themes

(use-package reverse-theme
  :ensure t
  )

(use-package doom-themes
  :ensure t
  :init
  :config
  (progn
    (doom-themes-neotree-config)
    (setq doom-neotree-line-spacing 0)
    (doom-themes-org-config)))

;; (use-package moe-theme
;;    :ensure t
;; )

;; (load-theme 'deeper-blue t)
;; (load-theme 'doom-one t)

;; tested:
(load-theme 'reverse t)
;; (load-theme 'wheatgrass t)

;; FONTS
;; issues and workarounds:
;; https://github.com/syl20bnr/spacemacs/issues/3477
;; https://www.emacswiki.org/emacs/SetFonts
;; (set-frame-font "Fantasque Sans Mono-16" nil t)

;; default fonts - optional:

;; (set-face-attribute 'default nil :family "DejaVu Sans Mono")
;; (set-face-attribute 'default nil :family "Source Code Pro")
;; (set-face-attribute 'default nil :family "Fantasque Sans Mono")
;; (set-face-attribute 'default nil :family "Verdana")
;; (set-face-attribute 'default nil :family "DejaVu Sans")
;; (set-face-attribute 'default nil :family "Consolas")
;; https://github.com/source-foundry/Hack
(set-face-attribute 'default nil :family "Hack")

;; height fpr default font:
(set-face-attribute 'default nil :height 130)

;; symbols and emojis:
(setq use-default-font-for-symbols nil)
(set-fontset-font t 'symbol "Noto Color Emoji")

;; https://www.masteringemacs.org/article/unicode-ligatures-color-emoji
;; https://github.com/rolandwalker/unicode-fonts
(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(provide 'cfg-themes-fonts)
;;; cfg-themes-fonts.el ends here
