;;; cfg-vdiff-diff-ediff.el --- configure vdiff, diff-mode and ediff -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for vdiff, diff-mode and ediff

;;; Code:

(use-package vdiff
  :after evil
  :ensure t
  :config
  (evil-define-key 'normal vdiff-mode-map "Z" vdiff-mode-prefix-map)
  )

;; https://emacs.stackexchange.com/questions/7362/how-to-show-a-diff-between-two-buffers-with-character-level-diffs
(setq-default ediff-forward-word-function 'forward-char)

;; https://github.com/justbur/emacs-vdiff-magit
(use-package vdiff-magit
  :ensure t
  :config
  ;; (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  ;; (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)
  )


(provide 'cfg-vdiff-diff-ediff)
;;; cfg-vdiff-diff-ediff.el ends here
