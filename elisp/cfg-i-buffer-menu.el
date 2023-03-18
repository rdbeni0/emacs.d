;;; cfg-i-buffer-menu.el --- configuration for ibuffer and buffer-menu -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for tempbuf (tempbuf will kill unused buffers after XX seconds), ibuffer and buffer-menu (M-x Buffer-menu).

;;; Code:

;; tempbuf

;; (WARNING - this option could be aggresive!)
;;
;; More examples of configuration:
;; https://www.emacswiki.org/emacs/TempbufMode
;; https://www.emacswiki.org/emacs/tempbuf.el < source
;; https://github.com/DarwinAwardWinner/dotemacs-old/blob/master/site-lisp/settings/tempbuf-settings.el
;; https://github.com/biern/.emacs.d/blob/master/conf/tempbuf.el
;;

(require 'tempbuf)

;; clean native-compile buffer
(when (get-buffer "*Async-native-compile-log*")
  (setq tempbuf-minimum-timeout 5) ;; 5 seconds
  (switch-to-buffer "*Async-native-compile-log*")
  (turn-on-tempbuf-mode)
  (switch-to-buffer "*scratch*"))

;; tempbuf is working well and it will clean junk buffers:
(setq tempbuf-minimum-timeout 30)

;; example of usage:
;; (add-hook 'foo-mode-hook 'turn-on-tempbuf-mode)

;; defuns

(defun cfg/kill-other-buffers (&optional arg)
  "Kill all other buffers.
  If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun cfg/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window)))
     nil t)))

(defun cfg/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
           (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

(defun cfg/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
  If the buffer isn't visiting a file, ask if it should
  be saved to a file, or just renamed.

  If called without a prefix argument, the prompt is
  initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

(defun cfg/new-empty-buffer ()
  "Create a new empty buffer.
  New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
  It returns the buffer (for elisp programing).
  URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
  Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf))

(defun cfg/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

;; https://stackoverflow.com/questions/12715376/emacs-copy-pwd-of-the-current-buffer-to-clipboard

(defun cfg/show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;; ibuffer

;; turn off prompts "yes" or "no":

(setq ibuffer-expert t)

(provide 'cfg-i-buffer-menu)
;;; cfg-i-buffer-menu.el ends here
