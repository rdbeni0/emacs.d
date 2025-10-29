;;; cfg-gen-op-perspective.el --- general.el for perspective -*- lexical-binding: t -*-

;; https://stackoverflow.com/questions/58615798/how-to-use-leader-key-as-part-of-package-prefix
(define-key evil-normal-state-map (kbd "SPC b n") 'perspective-map)
(define-key evil-normal-state-map (kbd "SPC b p") 'perspective-map)


(provide 'cfg-gen-op-perspective)
;;; cfg-gen-op-perspective.el ends here
