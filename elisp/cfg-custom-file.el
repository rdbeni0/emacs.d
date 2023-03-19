;;; cfg-custom-file.el --- configfuration for custom variables/faces -*- lexical-binding: t -*-
;;; Commentary:

;; pre-configuration for custom.el file

;;; Code:

;; custom-set-variables

(custom-set-variables
 '(doc-view-resolution 150)
 '(doc-view-scale-internally nil)
 '(erc-image-inline-rescale 330)
 '(erc-nicklist-use-icons nil)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line t)
 '(evil-auto-indent nil)
 '(flyspell-default-dictionary "english")
 '(mode-line-format
   '("%e" mode-line-mule-info mode-line-modified " " mode-line-buffer-identification " %I L%l_C%c <%m>"
     (vc-mode vc-mode)
     mode-line-misc-info mode-line-end-spaces))
 '(multi-term-program "/usr/sbin/bash")
 '(multi-vterm-buffer-name "vterm")
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe org-mouse ol-rmail ol-w3m))
 '(package-selected-packages '(perl-quote use-package))
 '(tab-bar-close-button-show nil)
 '(tab-bar-mode t)
 '(tab-bar-tab-hints t)
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-separator '(0.2))
 '(term-buffer-maximum-size 0))

;; custom-set-faces

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
 '(rcirc-my-nick ((t (:foreground "red"))))
 '(rcirc-server ((t (:foreground "PaleGreen1"))))
 '(rcirc-timestamp ((t (:inherit default :foreground "magenta"))))
 '(rcirc-url ((t (:foreground "light sky blue" :box nil :underline t :weight bold))))
 '(tab-bar-tab ((t (:inherit tab-bar :background "lawn green" :foreground "black" :box (:line-width 2 :color "magenta" :style released-button)))))
 '(tabbar-default ((t (:background "dark blue" :foreground "cornsilk"))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "magenta" :box (:line-width 1 :color "white" :style pressed-button)))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "DeepSkyBlue1"))))
 '(term-color-cyan ((t (:background "cyan" :foreground "cyan"))))
 '(term-color-green ((t (:background "lawn green" :foreground "lawn green"))))
 '(term-color-magenta ((t (:background "magenta" :foreground "magenta"))))
 '(term-color-red ((t (:background "red1" :foreground "red1")))))

;; !!WARNING!! FROM NOW ALL ABOVE SETTINGS COULD BE OVERWRITTEN !!
;; AND NOW : finally load default custom.el file...
;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs
;; https://www.reddit.com/r/emacs/comments/9rrhy8/emacsers_with_beautiful_initel_files_what_about/

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(provide 'cfg-custom-file)
;;; cfg-custom-file.el ends here
