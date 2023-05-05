;;; cfg-persp-desktop.el --- configfuration for perspectives and desktop sessions -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs session management:
;; https://www.emacswiki.org/emacs/SessionManagement
;; Desktop sessions:
;; https://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
;; perspectives:

;;; Code:

;; desktop+
;; https://github.com/ffevotte/desktop-plus
;; https://emacs.stackexchange.com/questions/19190/desktop-save-mode-fails-to-save-window-layout/45829#45829
;; optional and interesting tweaks: 
;; https://stackoverflow.com/questions/4477376/some-emacs-desktop-save-questions-how-to-change-it-to-save-in-emacs-d-emacs
;; desktop+ is simpler package and approach than perspective.el:
(use-package desktop+
  :ensure t
  :config
  (setq desktop-restore-forces-onscreen nil)
  (add-hook 'desktop-after-read-hook
	    (lambda ()
	      (frameset-restore
	       desktop-saved-frameset
	       :reuse-frames (eq desktop-restore-reuses-frames t)
	       :cleanup-frames (not (eq desktop-restore-reuses-frames 'keep))
	       :force-display desktop-restore-in-current-display
	       :force-onscreen desktop-restore-forces-onscreen))))

;; https://github.com/Bad-ptr/persp-mode.el
;; https://github.com/nex3/perspective-el - newest version of persp-mode
;; WARNING! perspective-el is incompatible with desktop+ or desktop save session
;; Perspective does not work with Emacs desktop.el. This is because Perspective state stores buffer and window information in frame parameters, and desktop-save-mode does not support saving those types of data.
;; so use it as OPTIONAL functionality:
(use-package perspective
  :ensure t
  :config
  ;; MUST be here to avoid ugly warning:
  ;; more: https://stackoverflow.com/questions/58615798/how-to-use-leader-key-as-part-of-package-prefix
  (setq persp-mode-prefix-key (kbd "C-c p"))
  (setq persp-state-default-file (expand-file-name "desktops/.persp" user-emacs-directory))

  ;; TURN OFF by default - comment/uncomment below section:

  ;; (persp-mode t)
  ;; (add-hook 'kill-emacs-hook #'persp-state-save)
  ;; https://github.com/nex3/perspective-el#sample-use-cases
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source)

  )

;; load keybindings from general.el framework:
(require 'cfg-gen-persp-desktop)

(provide 'cfg-persp-desktop)
;;; cfg-persp-desktop.el ends here
