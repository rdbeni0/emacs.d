;;; cfg-ediff-diff-vdiff.el --- configure ediff, diff and vdiff -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for ediff, vdiff, diff-mode

;;; Code:

;; ;; ediff
;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html#Customization

(use-package ediff
  :after magit ;; for magit-ediff
  :ensure t
  :config
  ;; https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
  (setq-default ediff-forward-word-function 'forward-char)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))



;; OPTIONAL - vdiff:

;; (use-package vdiff
;;   :after evil
;;   :ensure t
;;   :config
;;   (evil-define-key 'normal vdiff-mode-map "Z" vdiff-mode-prefix-map))

;; https://github.com/justbur/emacs-vdiff-magit
;; (use-package vdiff-magit
;;   :ensure t
;;   :config
;;   ;; (define-key magit-mode-map "e" 'vdiff-magit-dwim)
;;   ;; (define-key magit-mode-map "E" 'vdiff-magit)
;;   (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
;;   (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
;;   (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
;;   (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(provide 'cfg-ediff-diff-vdiff)
;;; cfg-ediff-diff-vdiff.el ends here
