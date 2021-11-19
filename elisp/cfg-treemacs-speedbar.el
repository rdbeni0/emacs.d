;;; cfg-treemacs-speedbar.el --- configfuration for treemacs and speedbar -*- lexical-binding: t -*-
;;; Commentary:

;; treemacs:
;; https://github.com/Alexander-Miller/treemacs
;;
;; sr-speedbar:
;; https://github.com/emacsorphanage/sr-speedbar/tree/77a83fb50f763a465c021eca7343243f465b4a47
;;
;;; Code:

(use-package treemacs
  :ensure t
  :config
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (setq
   treemacs-show-hidden-files               t
   treemacs-indentation                     1
   )
  (treemacs-resize-icons 18) ;; icon's size
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  )

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t
  :config
  )

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  :config
  )

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t
  :config
  )

(use-package treemacs-icons-dired
  :after treemacs
  :ensure t
  :config
  )

(use-package sr-speedbar
  :ensure t
  :config
  )



(provide 'cfg-treemacs-speedbar)
;;; cfg-treemacs-speedbar.el ends here
