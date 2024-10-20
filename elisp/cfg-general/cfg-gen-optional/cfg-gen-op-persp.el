;;; cfg-gen-op-persp.el --- general.el for perspective -*- lexical-binding: t -*-

;; https://stackoverflow.com/questions/58615798/how-to-use-leader-key-as-part-of-package-prefix
(define-key evil-normal-state-map (kbd "SPC b p") 'perspective-map)
(setq persp-state-default-file (expand-file-name ".persp" user-emacs-directory))

(provide 'cfg-gen-op-persp)
;;; cfg-gen-op-persp.el ends here
