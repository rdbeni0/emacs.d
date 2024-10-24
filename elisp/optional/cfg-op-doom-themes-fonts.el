;;; cfg-op-doom-themes-fonts.el --- configure themes and fonts -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Setup for (black/dark) doom themes and fonts.
;;
;;; Code:

(use-package doom-themes
  :ensure t
  :init
  :config
  (progn
    (doom-themes-neotree-config)
    (setq doom-neotree-line-spacing 0)
    (doom-themes-org-config)))

(load-theme 'doom-one t)

;; FONTS
;; https://github.com/rainstormstudio/nerd-icons.el
(use-package nerd-icons
  :ensure t
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )


;; issues and workarounds:
;; https://github.com/syl20bnr/spacemacs/issues/3477
;; https://www.emacswiki.org/emacs/SetFonts
;; (set-frame-font "Fantasque Sans Mono-16" nil t)

;; default fonts:

;; (set-face-attribute 'default nil :family "DejaVu Sans Mono")
;; (set-face-attribute 'default nil :family "Source Code Pro")
;; (set-face-attribute 'default nil :family "Fantasque Sans Mono")
;; (set-face-attribute 'default nil :family "Verdana")
;; (set-face-attribute 'default nil :family "DejaVu Sans")
;; (set-face-attribute 'default nil :family "Consolas")
;; https://github.com/source-foundry/Hack
(set-face-attribute 'default nil :family "Hack")

;; height for default font:
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

;; https://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs
;; add prefix arg to show more detailed output of font:
(defun cfg/-adv-show-face-under-point-detailed (orig &rest args)
  "Add prefix argument and always choose directory for consult-grep"
  (setq prefix-arg '(4))
  (funcall orig args))
(advice-add 'what-cursor-position :around #'cfg/-adv-show-face-under-point-detailed)

;; custom-set-faces compatible with dark doom-theme:

(custom-set-faces
 '(dired-mark ((t nil)))
 '(dired-marked ((t (:foreground "DarkOrange" :weight bold))))
 '(erc-button ((t (:background "gray0" :foreground "dodger blue" :underline "dodger blue" :weight bold))))
 '(erc-default-face ((t (:background "gray7" :foreground "yellow"))))
 '(erc-direct-msg-face ((t (:foreground "wheat"))))
 '(erc-input-face ((t (:foreground "red"))))
 '(erc-inverse-face ((t (:background "Black" :foreground "yellow"))))
 '(erc-keyword-face ((t (:background "lawn green" :foreground "medium blue" :weight bold))))
 '(erc-my-nick-face ((t (:foreground "red" :weight bold))))
 '(erc-my-nick-prefix-face ((t (:foreground "dark orange" :weight bold))))
 '(erc-nick-default-face ((t (:background "black" :foreground "lawn green" :weight bold))))
 '(erc-nick-msg-face ((t (:foreground "orange red" :weight bold))))
 '(erc-nick-prefix-face ((t (:foreground "dark salmon" :weight bold))))
 '(erc-notice-face ((t (:background "gray21" :foreground "deep sky blue" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "cyan" :weight bold))))
 '(fg:erc-color-face1 ((t (:background "magenta" :foreground "black"))))
 '(font-lock-comment-face ((t (:background "gray6" :foreground "dark orange"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :background "gray6" :foreground "darl salmon"))))
 '(font-lock-keyword-face ((t (:background "gray0" :foreground "medium sea green"))))
 '(font-lock-type-face ((t (:background "black" :foreground "gold"))))
 '(helm-ff-dotted-directory ((t (:background "chartreuse" :foreground "black"))))
 '(helm-helper ((t (:foreground "cyan"))))
 '(helm-selection ((t (:background "white" :foreground "black"))))
 '(helm-source-header ((t (:background "#202328" :foreground "wheat"))))
 '(notmuch-message-summary-face ((t (:extend t :background "gray11" :foreground "sandy brown"))))
 '(notmuch-search-flagged-face ((t (:background "gray5" :foreground "chartreuse"))))
 '(notmuch-tag-face ((t (:background "navy" :foreground "violet"))))
 '(org-block ((t (:extend t :background "gray8" :foreground "gold"))))
 '(org-block-begin-line ((t (:extend t :foreground "red"))))
 '(org-document-info-keyword ((t (:background "dark blue" :foreground "chocolate"))))
 '(org-drawer ((t (:background "light coral" :foreground "dark violet"))))
 '(org-meta-line ((t (:background "dark slate gray" :foreground "green"))))
 '(org-verbatim ((t (:background "saddle brown" :foreground "peach puff"))))
 '(rcirc-my-nick ((t (:foreground "red"))))
 '(rcirc-server ((t (:foreground "PaleGreen1"))))
 '(rcirc-timestamp ((t (:inherit default :foreground "magenta"))))
 '(rcirc-url ((t (:foreground "light sky blue" :box nil :underline t :weight bold))))
 '(tab-bar ((t (:background "#21242b" :foreground "#21242b" :box (:line-width (2 . 2) :color "white smoke" :style released-button)))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "lawn green" :foreground "black" :box (:line-width 2 :color "magenta" :style released-button)))))
 '(tab-bar-tab-inactive ((t (:inherit tab-line-tab :background "#024c61" :foreground "#ff91ff" :box nil))))
 '(tabbar-default ((t (:background "dark blue" :foreground "cornsilk"))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "magenta" :box (:line-width 1 :color "white" :style pressed-button)))))
 '(tabbar-separator ((t (:background "black" :foreground "light steel blue"))))
 '(tabbar-unselected ((t (:inherit tabbar-default :foreground "yellow"))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "DeepSkyBlue1"))))
 '(term-color-cyan ((t (:background "cyan" :foreground "cyan"))))
 '(term-color-green ((t (:background "lawn green" :foreground "lawn green"))))
 '(term-color-magenta ((t (:background "magenta" :foreground "magenta"))))
 '(term-color-red ((t (:background "red1" :foreground "red1"))))
 '(web-mode-comment-face ((t (:inherit font-lock-comment-face :background "gray1")))))

(provide 'cfg-op-doom-themes-fonts)
;;; cfg-op-doom-themes-fonts.el ends here
