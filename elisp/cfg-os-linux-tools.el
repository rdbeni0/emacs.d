;;; cfg-os-linux-tools.el --- tools for operating system, linux, windows -*- lexical-binding: t -*-
;;; Commentary:

;; Various tools for operating systems

;;; Code:

;; Arch Linux
;; https://github.com/UndeadKernel/pacfiles-mode
(use-package pacfiles-mode
  :ensure t)

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
  "Retrieve the file path of the current buffer, including line and column number.

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
  "Copy and show the file name without its final extension of the current buffer."
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
  "Copy and show the file path of the current buffer, including line and column number.
  This function respects the value of the `column-number-indicator-zero-based' variable."
  (interactive)
  (if-let (file-path (cfg/-file-path-with-line-column))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))

(provide 'cfg-os-linux-tools)
;;; cfg-os-linux-tools.el ends here
