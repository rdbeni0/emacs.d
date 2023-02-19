;;; cfg-groovy-jenkins.el --- groovy or jenkins mode -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with groovy-mode, Grails, and jenkings configuration files for Emacs.

;;; Code:

(use-package groovy-mode
  :ensure t)

(use-package jenkinsfile-mode
  :ensure t
  :after groovy-mode)

(provide 'cfg-groovy-jenkins)
;;; cfg-groovy-jenkins.el ends here
