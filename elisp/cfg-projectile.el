;;; cfg-projectile.el --- configfuration for projectile -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with "projectile"

;;; Code:

(use-package projectile
  :ensure t
  ;;  :requires subr-x
  :init
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)

  ;; Currently if the ignore list has to be taken from .projectile, then it only works with "indexing-method native".
  ;; It can be tried with "indexing-method alien"; but primarly alien works only with ".gitignore".
  ;; And "indexing-method alien" is much faster from performance point of view.


  ;;   (setq projectile-indexing-method 'native)
  (setq projectile-indexing-method 'alien)
  ;;
  ;; There is also "turbo-alien" :
  ;; https://www.reddit.com/r/emacs/comments/9jvn0f/projectile_gets_a_turboalien_indexing_mode/

  ;;
  ;; ...and finally - you can also use workarounds with "generic-command" and "method alien" - and for unix this is the best option:
  ;; fd is very fast "find" alternative, howewer it must be installed:
  ;; https://github.com/sharkdp/fd
  ;;
  (setq projectile-generic-command "fd -H --ignore-file .projectile -t f -0")

  ;; An alternative option with "generic-command" is here:
  ;; https://github.com/kaushalmodi/.emacs.d/blob/c7da9469e9de3aff83e3e3b09596ef3665b5ab95/setup-files/setup-projectile.el#L64-L77
  ;; ^ in that case we need to use ".agignore" file.

  ;; (defvar projectile-ag-command
  ;; (concat "\\ag" ; used unaliased version of `ag': \ag
  ;;         " -i" ; case insensitive
  ;;         " -f" ; follow symbolic links
  ;;         " --skip-vcs-ignores" ; Ignore files/dirs ONLY from `.agignore',
  ;;         " -0" ; output null separated results
  ;;         " -g ''") ; get file names matching the regex ''
  ;; "Ag command to be used by projectile to generate file cache.")

  ;;
  :config
  (add-hook 'projectile-after-switch-project-hook (lambda ()
						    (projectile-invalidate-cache nil)))
  )


;; org-projectile
;; ^ optional package, not yet implemented:
;; https://github.com/IvanMalison/org-projectile



(provide 'cfg-projectile)
;;; cfg-projectile.el ends here
