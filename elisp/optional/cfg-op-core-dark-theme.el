;;; cfg-op-core-dark-theme.el --- configure ediff, diff and vdiff -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(load-theme 'modus-vivendi t) ;; currently the best one - black background
;; (load-theme 'modus-vivendi-tinted t) ;; currently the best one - night sky background

;;; Older but not recommended:

;; (load-theme 'misterioso t)
;; (load-theme 'wombat t)
;; (load-theme 'tango-dark t)

;; custom-set-faces compatible with dark modus-vivendi themes:

(set-face-attribute 'default nil :height 130)

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
 '(font-lock-comment-face ((t (:background "gray15" :foreground "dark orange"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :background "gray15" :foreground "darl salmon"))))
 '(font-lock-keyword-face ((t (:background "gray0" :foreground "medium sea green"))))
 '(font-lock-type-face ((t (:background "black" :foreground "gold"))))
 '(markdown-markup-face ((t (:background "gray15" :foreground "yellow"))))
 '(notmuch-message-summary-face ((t (:extend t :background "gray11" :foreground "sandy brown"))))
 '(notmuch-search-flagged-face ((t (:background "gray5" :foreground "chartreuse"))))
 '(notmuch-tag-face ((t (:background "navy" :foreground "violet"))))
 '(org-block ((t (:extend t :background "gray8" :foreground "gold"))))
 '(org-block-begin-line ((t (:extend t :foreground "red"))))
 '(org-document-info-keyword ((t (:background "dark blue" :foreground "chocolate"))))
 '(org-drawer ((t (:background "light coral" :foreground "dark violet"))))
 '(org-meta-line ((t (:background "gray16" :foreground "peru" :box (:line-width (1 . 1) :color "magenta" :style released-button)))))
 '(org-verbatim ((t (:background "saddle brown" :foreground "peach puff"))))
 '(rcirc-my-nick ((t (:foreground "red"))))
 '(rcirc-server ((t (:foreground "PaleGreen1"))))
 '(rcirc-timestamp ((t (:inherit default :foreground "magenta"))))
 '(rcirc-url ((t (:foreground "light sky blue" :box nil :underline t :weight bold))))
 '(tab-bar ((t (:background "#21242b" :foreground "#21242b" :box (:line-width (2 . 2) :color "white smoke" :style released-button)))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "lawn green" :foreground "black" :box (:line-width 2 :color "magenta" :style released-button)))))
 '(tab-bar-tab-inactive ((t (:inherit tab-line-tab :background "#024c61" :foreground "#ff91ff" :box nil))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "DeepSkyBlue1"))))
 '(term-color-cyan ((t (:background "cyan" :foreground "cyan"))))
 '(term-color-green ((t (:background "lawn green" :foreground "lawn green"))))
 '(term-color-magenta ((t (:background "magenta" :foreground "magenta"))))
 '(term-color-red ((t (:background "red1" :foreground "red1"))))
 '(web-mode-comment-face ((t (:inherit font-lock-comment-face :background "gray1"))))
 '(web-mode-doctype-face ((t (:foreground "gray70")))))

;; mode-line:
;; (set-face-background #'mode-line-inactive "white")
;; (set-face-background #'mode-line-inactive "burlywood")
;; (set-face-foreground #'mode-line-inactive "black")
(set-face-background #'mode-line "DeepPink4")
(set-face-foreground #'mode-line "yellow")

(provide 'cfg-op-core-dark-theme)
;;; cfg-op-core-dark-theme.el ends here
