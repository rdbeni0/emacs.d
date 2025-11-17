;;; cfg-project.el --- configuration for project.el with fd backend -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Fast and idiomatic fd backend for project.el.
;;
;;; Code:

(require 'project)
(require 'vc)
(require 'cl-lib)
(require 'seq)

;;;; ------------------------------------------------------------------
;;;; User options
;;;; ------------------------------------------------------------------

(defgroup project-fd nil
  "Improved project.el backend powered by fd."
  :group 'project)

(defcustom project-root-markers
  '(".gitignore" ".dir-locals.el" ".project")
  "Markers that indicate the root of a non-VC project."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-vc-extra-root-markers
  '(".gitignore" ".dir-locals.el" ".project")
  "Markers that reinforce identifying the VC project root."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-vc-ignores
  '(".ccls-cache/" ".git/")
  "Patterns excluded from fd results for VC projects."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-ignores
  '(".ccls-cache/" ".git/")
  "Patterns excluded from fd results in non-VC projects."
  :type '(repeat string)
  :group 'project-fd)

(defcustom project-build-dirs
  '("build/" "out/" "dist/" "target/")
  "Common build directories automatically excluded."
  :type '(repeat string)
  :group 'project-fd)

(defvar project-fd-debug nil
  "If non-nil, output project/fd debug logs.")

(defun project-fd--debug (fmt &rest args)
  (when project-fd-debug
    (apply #'message (concat "[project-fd] " fmt) args)))


;;;; ------------------------------------------------------------------
;;;; Helpers
;;;; ------------------------------------------------------------------

(defun project-fd--marker-exists-p (dir marker)
  "Return t if MARKER exists inside DIR (follow symlinks)."
  (file-exists-p (expand-file-name marker (file-truename dir))))

(defun project-fd--dir-has-markers (dir markers)
  "Return non-nil if DIR contains one of MARKERS."
  (seq-some (lambda (m) (project-fd--marker-exists-p dir m))
            markers))

;;;; ------------------------------------------------------------------
;;;; Project root finder
;;;; ------------------------------------------------------------------

(defun project-fd--find-root (path)
  "Find project root starting from PATH. Return (TYPE . ROOT)."
  (let* ((path (file-name-as-directory (expand-file-name path)))
         ;; NOTE: vc-root-dir is slow — call *once*
         (vc-root (let ((default-directory path)) (vc-root-dir))))
    (project-fd--debug "Searching root from %s (vc=%s)" path vc-root)

    (catch 'found
      (while (not (equal path "/"))
        (project-fd--debug "Checking dir: %s" path)

        (let ((tru (file-truename path)))

          ;; --- VC project root ---
          (when (and vc-root
                     (equal tru (file-truename vc-root))
                     (project-fd--dir-has-markers path project-vc-extra-root-markers))
            (project-fd--debug "VC project root found: %s" path)
            (throw 'found (cons 'vc path)))

          ;; --- Local .project ---
          (when (project-fd--marker-exists-p path ".project")
            (project-fd--debug "Local project root found: %s" path)
            (throw 'found (cons 'local path)))

          ;; --- Transient markers ---
          (when (project-fd--dir-has-markers path project-root-markers)
            (project-fd--debug "Transient project root found: %s" path)
            (throw 'found (cons 'transient path))))

        ;; Go up
        (setq path (file-name-directory (directory-file-name path)))))))

(add-hook 'project-find-functions #'project-fd--find-root)


;;;; ------------------------------------------------------------------
;;;; project-root methods
;;;; ------------------------------------------------------------------

(cl-defmethod project-root ((project (head vc)))       (cdr project))
(cl-defmethod project-root ((project (head local)))    (cdr project))
(cl-defmethod project-root ((project (head transient))) (cdr project))

;;;; ------------------------------------------------------------------
;;;; fd command builder — simplified and unified
;;;; ------------------------------------------------------------------

(defun project-fd--make-fd-command (project root)
  "Return list of arguments for fd search in ROOT."
  (let* ((is-vc (eq (car project) 'vc))
         (ignores (if is-vc project-vc-ignores project-ignores))
         (cmd '("fd" "--type" "f" "--hidden" "--strip-cwd-prefix" ".")))

    ;; .gitignore must be respected ALWAYS (even for non-VC!)
    (when (project-fd--marker-exists-p root ".gitignore")
      (setq cmd (append cmd '("--ignore-file" ".gitignore"))))

    ;; Excludes from VC or non-VC sets
    (setq cmd
          (append cmd
                  (mapcan (lambda (p) (list "--exclude" p)) ignores)))

    ;; Exclude build directories
    (setq cmd
          (append cmd
                  (mapcan (lambda (p) (list "--exclude" p))
                          project-build-dirs)))

    (project-fd--debug "FD(%s) → %S" (car project) cmd)
    cmd))


;;;; ------------------------------------------------------------------
;;;; fd-based project-files implementation
;;;; ------------------------------------------------------------------

(defun project-fd-files (project &optional _dirs)
  "Return list of files in PROJECT using fd."
  (let ((root (project-root project)))
    (unless (executable-find "fd")
      (user-error "Program 'fd' not found"))

    (let* ((default-directory root)
           (cmd (project-fd--make-fd-command project root)))
      (project-fd--debug "Running fd in %s" root)
      (apply #'process-lines cmd))))


(cl-defmethod project-files ((project (head vc)) &optional dirs)
  (project-fd-files project dirs))

(cl-defmethod project-files ((project (head transient)) &optional dirs)
  (project-fd-files project dirs))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  (project-fd-files project dirs))

(provide 'cfg-project)
;;; cfg-project.el ends here
