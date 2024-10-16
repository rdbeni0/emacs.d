;;; cfg-project.el --- configfuration for project.el -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with "project.el".
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html
;;
;; projectl.el vs projectile:
;; https://www.youtube.com/watch?v=1sn8m5u5VaE
;;
;; Potential enhancements:
;; https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
;; https://emacs.stackexchange.com/questions/58463/project-el-override-project-root-with-dir-local-var
;; https://github.com/karthink/project-x/
;; https://manueluberti.eu/emacs/2020/11/14/extending-project/
;; https://github.com/darcamo/simple_emacs_config/blob/c569210b3663de877a8977ad48a8e8a7686dbad4/config.org
;; https://grtcdr.tn/posts/2023-03-01.html
;;
;;; Code:

(setq project-vc-extra-root-markers '(".gitignore" ".dir-locals.el" ".project"))
(setq project-vc-ignores '(".ccls-cache/" ".git/"))
;; (add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom project-root-markers
  '(".gitignore" ".dir-locals.el" ".project")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(cl-defmethod project-root ((project (head local)))
  (cdr project))


;; this part is not working >>>>
(defun cfg/-project-files-in-directory (dir)
  "Use `fd' to list files in DIR. `.gitignore' will be used as default ignore list."
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -L -H --ignore-file .gitignore -t f -0 . %s" localdir)))
    (project--remote-file-names
     (sort (split-string (shell-command-to-string command) "\0" t)
           #'string<))))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects."
  (mapcan #'cfg/-project-files-in-directory
          (or dirs (list (project-root project)))))
;; <<<<

(defun cfg/-project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun cfg/-project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (let ((path (expand-file-name path)))
    (catch 'found
      (while (not (equal "/" path))
        (if (not (cfg/-project-root-p path))
            (setq path (file-name-directory (directory-file-name path)))
          (throw 'found (cons 'transient path)))))))

(add-hook 'project-find-functions #'cfg/-project-find-root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Optional:
;; Its working, but above solution is better.

;; (add-to-list 'project-find-functions 'cfg/-project-try-local)

;; (defun cfg/-project-try-local (dir)
;;   "Determine if DIR is a non-Git project.
;; DIR must include a `.gitignore' file to be considered a project."
;;   (let ((root (locate-dominating-file dir ".gitignore")))
;;     (and root (cons 'local root))))

;; (add-hook 'project-find-functions #'cfg/-project-try-local)

(provide 'cfg-project)
;;; cfg-project.el ends here
