;;; cfg-op-groovy-jenkins.el --- groovy or jenkins mode -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with groovy-mode, Grails, and jenkings configuration files for Emacs.
;;
;;; Code:

(use-package groovy-mode
  :ensure t
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-op-jenkins-groovy))

(use-package jenkinsfile-mode
  :ensure t
  :after groovy-mode)

(provide 'cfg-op-groovy-jenkins)
;;; cfg-op-groovy-jenkins.el ends here
