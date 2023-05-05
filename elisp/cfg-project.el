;;; cfg-project.el --- configfuration for project.el -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with "project.el".
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html

;; Potential enhancements:
;; https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
;; https://emacs.stackexchange.com/questions/58463/project-el-override-project-root-with-dir-local-var
;; https://github.com/karthink/project-x/

;;; Code:

(defcustom project-root-markers
  '(".gitignore" ".dir-locals.el")
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

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

;; https://grtcdr.tn/posts/2023-03-01.html

(setq project-vc-extra-root-markers '(".gitignore" ".dir-locals.el"))
(setq project-vc-ignores '(".ccls-cache/" ".git/"))

(provide 'cfg-project)
;;; cfg-project.el ends here
