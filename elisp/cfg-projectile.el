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
  (setq projectile-generic-command "fd -H --ignore-file .gitignore -t f -0")

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

  ;; this will allow to use projects by .gitignore files
  (projectile-register-project-type 'gitign '(".gitignore")
                                    :project-file ".gitignore"
				    )
  :config
  (add-hook 'projectile-after-switch-project-hook (lambda ()
						    (projectile-invalidate-cache nil)))


  )


;; org-projectile
;; ^ optional package, not yet implemented:
;; https://github.com/IvanMalison/org-projectile
;; additional funcs (from spacemacs):


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

This function respects the value of the `column-number-indicator-zero-based'
variable.

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

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (if-let (file-path (cfg/-projectile-file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not visiting a file!")))

;; Copy file path

(defun cfg/-directory-path ()
  "Retrieve the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory' variable
as a fallback to display the directory, useful in buffers like the ones created
by `magit' and `dired'.

Returns:
  - A string containing the directory path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (directory-name (if-let (file-name (buffer-file-name))
                                (file-name-directory file-name)
                              list-buffers-directory))
    (file-truename directory-name)))

(defun cfg/-file-path ()
  "Retrieve the file path of the current buffer.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (buffer-file-name))
    (file-truename file-path)))

(defun cfg/-file-path-with-line ()
  "Retrieve the file path of the current buffer, including line number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (cfg/-file-path))
    (concat file-path ":" (number-to-string (line-number-at-pos)))))

(defun cfg/-file-path-with-line-column ()
  "Retrieve the file path of the current buffer,
including line and column number.

Returns:
  - A string containing the file path in case of success.
  - `nil' in case the current buffer does not have a directory."
  (when-let (file-path (cfg/-file-path-with-line))
    (concat
     file-path
     ":"
     (number-to-string (if (and
                            ;; Emacs 26 introduced this variable. Remove this
                            ;; check once 26 becomes the minimum version.
                            (boundp column-number-indicator-zero-based)
                            (not column-number-indicator-zero-based))
                           (1+ (current-column))
                         (current-column))))))

(defun cfg/copy-directory-path ()
  "Copy and show the directory path of the current buffer.

If the buffer is not visiting a file, use the `list-buffers-directory'
variable as a fallback to display the directory, useful in buffers like the
ones created by `magit' and `dired'."
  (interactive)
  (if-let (directory-path (cfg/-directory-path))
      (progn
        (kill-new directory-path)
        (message "%s" directory-path))
    (message "WARNING: Current buffer does not have a directory!")))

(defun cfg/copy-file-path ()
  "Copy and show the file path of the current buffer."
  (interactive)
  (if-let (file-path (cfg/-file-path))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun cfg/copy-file-name ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (cfg/-file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun cfg/copy-buffer-name ()
  "Copy and show the name of the current buffer."
  (interactive)
  (kill-new (buffer-name))
  (message "%s" (buffer-name)))

(defun cfg/copy-file-name-base ()
  "Copy and show the file name without its final extension of the current
buffer."
  (interactive)
  (if-let (file-name (file-name-base (cfg/-file-path)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun cfg/copy-file-path-with-line ()
  "Copy and show the file path of the current buffer, including line number."
  (interactive)
  (if-let (file-path (cfg/-file-path-with-line))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(defun cfg/copy-file-path-with-line-column ()
  "Copy and show the file path of the current buffer,
including line and column number.

This function respects the value of the `column-number-indicator-zero-based'
variable."
  (interactive)
  (if-let (file-path (cfg/-file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))


(provide 'cfg-projectile)
;;; cfg-projectile.el ends here
