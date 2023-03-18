;;; cfg-projectile.el --- configfuration for projectile -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with "projectile"

;;; Code:

(use-package projectile
  :ensure t
  :bind (([project-shell-command]             . projectile-run-shell-command-in-root)
         ([project-shell]                     . projectile-run-shell)
         ([project-async-shell-command]       . projectile-run-async-shell-command-in-root)
         ([project-query-replace-regexp]      . projectile-replace-regexp)
         ([project-switch-to-buffer]          . projectile-switch-to-buffer)
         ([project-compile]                   . projectile-compile-project)
         ([project-find-dir]                  . projectile-find-dir)
         ([project-dired]                     . projectile-dired)
         ([project-find-file]                 . projectile-find-file)
         ([project-or-external-find-file]     . projectile-find-other-file)
         ([project-kill-buffers]              . projectile-kill-buffers)
         ([project-switch-project]            . projectile-switch-project)
         ([project-vc-dir]                    . projectile-vc)
	 )
  :init
  (projectile-mode)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t)

  ;; Currently if the ignore list has to be taken from .projectile, then it only works with "indexing-method native".
  ;; It can be tried with "indexing-method alien", but primarly alien works only with ".gitignore".
  ;; And "indexing-method alien" is much faster from performance point of view.

  ;;   (setq projectile-indexing-method 'native)
  (setq projectile-indexing-method 'alien)
  ;;
  ;; btw. there is also "turbo-alien" :
  ;; https://www.reddit.com/r/emacs/comments/9jvn0f/projectile_gets_a_turboalien_indexing_mode/

  ;;
  ;; ...and finally - you can also use workarounds with "generic-command" and "method alien" - and for unix this is the best option:
  ;; fd is very fast "find" alternative, howewer it must be installed:
  ;; https://github.com/sharkdp/fd
  ;;
  (setq projectile-generic-command "fd -L -H --ignore-file .gitignore -t f -0")

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

  ;; https://www.reddit.com/r/emacs/comments/ihvv0s/projectile_how_to_manually_registering_a_project/
  ;; "To disable automatic detection of projects you have to use(setq projectile-track-known-projects-automatically nil). Then you can add projects into projectile with M-x projectile-add-known-project."
  (setq projectile-track-known-projects-automatically nil)

  ;; this will allow to use projects by .gitignore files
  (projectile-register-project-type 'gitign '(".gitignore")
                                    :project-file ".gitignore")
  :config
  ;; remap if consult is installed:
  (when (require 'consult nil 'noerror)
    (define-key projectile-mode-map [remap projectile-ripgrep] 'consult-ripgrep)
    )

  (add-hook 'projectile-after-switch-project-hook (lambda ()
						    (projectile-invalidate-cache nil))))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  :after general
  :config
  )

;; def advice
;; https://stackoverflow.com/questions/70042843/how-to-advice-add-a-function-with-no-arguments-to-a-function-that-takes-argument

(defun cfg/-adv-projectile-buffer-file (&optional _args)
  (when (projectile-project-p)
    (call-interactively #'projectile-invalidate-cache)))

(advice-add 'cfg/-adv-projectile-buffer-file :after #'cfg/delete-current-buffer-file)
(advice-add 'cfg/-adv-projectile-buffer-file :after #'cfg/rename-current-buffer-file)

;; optional package - org-projectile:
;; https://github.com/IvanMalison/org-projectile

;; additional defuns for projectile (from spacemacs):

(defun cfg/-projectile-directory-path ()
  "Retrieve the directory path relative to project root.
  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'.

  Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-relative-name
     (file-truename directory-name)
     (projectile-project-root))))

(defun cfg/-projectile-file-path ()
  "Retrieve the file path relative to project root.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-name (buffer-file-name))
    (file-relative-name (file-truename file-name) (projectile-project-root))))

(defun cfg/-projectile-file-path-with-line ()
  "Retrieve the file path relative to project root, including line number.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (cfg/-projectile-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun cfg/-projectile-file-path-with-line-column ()
  "Retrieve the file path relative to project root, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based' variable.

  Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not visit a file."
  (when-let (file-path (cfg/-projectile-file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
                            ;; Emacs 26 introduced this variable.
                            ;; Remove this check once 26 becomes the minimum version.
                            (boundp column-number-indicator-zero-based)
                            (not column-number-indicator-zero-based))
                           (1+ (current-column))
                         (current-column))))))

(defun cfg/projectile-copy-directory-path ()
  "Copy and show the directory path relative to project root.

  If the buffer is not visiting a file, use the `list-buffers-directory'
  variable as a fallback to display the directory, useful in buffers like the
  ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (cfg/-projectile-directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun cfg/projectile-copy-file-path ()
  "Copy and show the file path relative to project root."
  (interactive)
  (if-let (file-path (cfg/-projectile-file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun cfg/projectile-copy-file-path-with-line ()
  "Copy and show the file path relative to project root, including line number."
  (interactive)
  (if-let (file-path (cfg/-projectile-file-path-with-line))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

(defun cfg/projectile-copy-file-path-with-line-column ()
  "Copy and show the file path relative to project root, including line and column number.

  This function respects the value of the `column-number-indicator-zero-based' variable."
  (interactive)
  (if-let (file-path (cfg/-projectile-file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))


;; general.el - configuration for projectile:

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 "fyC" '(cfg/projectile-copy-file-path-with-line-column :which-key "projectile-copy-file-path-with-line-column")
 "fyD" '(cfg/projectile-copy-directory-path :which-key "projectile-copy-directory-path")
 "fyL" '(cfg/projectile-copy-file-path-with-line :which-key "projectile-copy-file-path-with-line")
 "fyY" '(cfg/projectile-copy-file-path :which-key "projectile-copy-file-path")

 "po"  '(:ignore t :which-key "projectile")
 "poT" '(projectile-test-project :which-key "pe-test-project")
 "pe"  '(projectile-edit-dir-locals :which-key "pe-edit-dir-locals")
 "poa" '(projectile-toggle-between-implementation-and-test :which-key "pe-toggle-between-implementation-and-test")
 "poR" '(projectile-replace :which-key "pe-replace")
 "pog" '(projectile-find-tag :which-key "pe-find-tag")
 "poG" '(projectile-regenerate-tags :which-key "pe-regenerate-tags")
 "pot" '(treemacs-projectile :which-key "treemacs-projectile")
 "poK" '(projectile-add-known-project :which-key "pe-add-known-project")
 "poM" '(projectile-remove-known-project :which-key "pe-remove-known-project")
 "poI" '(projectile-invalidate-cache :which-key "pe-invalidate-cache")
 "por" '(projectile-recentf :which-key "pe-recentf")
 "p;"  '(:ignore t :which-key "projectile_search/grep")
 "p;;" '(projectile-grep :which-key "grep")
 "p;g" '(projectile-ag :which-key "ag")
 "p;r" '(projectile-ripgrep :which-key "ripgrep"))


(provide 'cfg-projectile)
;;; cfg-projectile.el ends here
