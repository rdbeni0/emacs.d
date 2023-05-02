;;; cfg-persp-desktop.el --- configfuration for perspectives and desktop sessions -*- lexical-binding: t -*-
;;; Commentary:

;; Emacs session management:
;; https://www.emacswiki.org/emacs/SessionManagement
;; Desktop sessions:
;; https://www.emacswiki.org/emacs?action=browse;oldid=DeskTop;id=Desktop
;; perspectives:
;; https://github.com/Bad-ptr/persp-mode.el

;;; Code:

;; desktop+
;; https://github.com/ffevotte/desktop-plus
(use-package desktop+
  :ensure t
  )

;; https://github.com/Bad-ptr/persp-mode.el
;; https://github.com/nex3/perspective-el
(use-package perspective
  :ensure t
  :config
  ;; https://stackoverflow.com/questions/58615798/how-to-use-leader-key-as-part-of-package-prefix
  (setq persp-mode-prefix-key (kbd "C-c p"))
  (define-key evil-normal-state-map (kbd "SPC b p") 'perspective-map)
  (setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory))
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save))

(provide 'cfg-persp-desktop)
;;; cfg-persp-desktop.el ends here
