;;; treemacs.el --- Minimal in-house drop-in treemacs replacement -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Minimalist in-house / drop-in replacement for Alexander-Miller's treemacs.
;; Not a full port – basic file tree, git colours, icons, RMB menu, per-tab
;; buffers, smart root detection (no workspace config files).
;;
;; Root priority:
;;   1. project.el   2. .git root   3. file directory
;;   4. previous file buffer   5. $HOME
;;
;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'hl-line)
(require 'image)

(defgroup treemacs nil
  "Minimal in-house file tree (treemacs-compatible API surface)."
  :group 'files
  :prefix "treemacs-")

(defcustom treemacs-width 30
  "Width of the tree window."
  :type 'integer :group 'treemacs)

(defcustom treemacs-indentation 1
  "Spaces per depth level."
  :type 'integer :group 'treemacs)

(defcustom treemacs-show-hidden-files t
  "When non-nil, show dotfiles."
  :type 'boolean :group 'treemacs)

(defcustom treemacs-follow-mode t
  "When non-nil, rescan root when the selected window's buffer changes."
  :type 'boolean :group 'treemacs)

(defcustom treemacs-git-mode t
  "When non-nil, colour entries by git status."
  :type 'boolean :group 'treemacs)

(defcustom treemacs-icon-size 18
  "Pixel size for PNG icons."
  :type 'integer :group 'treemacs)

(defun treemacs--find-icons-dir ()
  "Locate treemacs icons/default (prefer real package, not this file)."
  (or
   ;; walk load-path for a dir that actually contains icons
   (cl-loop for dir in load-path
            for candidate = (expand-file-name "icons/default" dir)
            when (and (file-directory-p candidate)
                      (file-directory-p (expand-file-name "vsc" candidate)))
            return candidate)
   ;; package-user-dir / elpa
   (cl-loop for dir in (when (boundp 'package-user-dir)
                         (directory-files package-user-dir t "treemacs-" t))
            for candidate = (expand-file-name "icons/default" dir)
            when (file-directory-p candidate)
            return candidate)))

(defcustom treemacs-icons-dir (treemacs--find-icons-dir)
  "Directory with treemacs-style PNG icons (…/treemacs/icons/default)."
  :type '(choice (directory :tag "Icons directory") (const :tag "None" nil))
  :group 'treemacs)


;;; Faces

(defface treemacs-root-face
  '((t :inherit font-lock-constant-face :underline t :bold t :height 1.15))
  "Root node." :group 'treemacs)
(defface treemacs-directory-face
  '((t :inherit font-lock-function-name-face))
  "Directories." :group 'treemacs)
(defface treemacs-file-face
  '((t :inherit default))
  "Files." :group 'treemacs)
(defface treemacs-git-modified-face
  '((t :inherit font-lock-variable-name-face))
  "Git modified." :group 'treemacs)
(defface treemacs-git-untracked-face
  '((t :inherit font-lock-string-face))
  "Git untracked." :group 'treemacs)
(defface treemacs-git-added-face
  '((t :inherit font-lock-type-face))
  "Git added." :group 'treemacs)
(defface treemacs-git-ignored-face
  '((t :inherit shadow))
  "Git ignored." :group 'treemacs)
(defface treemacs-git-conflict-face
  '((t :inherit error))
  "Git conflict." :group 'treemacs)
(defface treemacs-git-renamed-face
  '((t :inherit font-lock-doc-face))
  "Git renamed." :group 'treemacs)
(defface treemacs-term-node-face
  '((t :inherit font-lock-string-face))
  "Terminal fallback for nodes." :group 'treemacs)


;;; Buffer / tab state

(defvar-local treemacs--root nil)
(defvar-local treemacs--expanded nil)
(defvar-local treemacs--git-cache nil)

(defvar treemacs--buffers nil
  "Alist (TAB-ID . BUFFER).")

(defvar treemacs--last-file-buffer nil
  "Last non-treemacs file/dired buffer we successfully followed.")

(defvar treemacs--follow-inhibit nil
  "Non-nil while we drive selection ourselves (avoid recursive follow).")

(defun treemacs--tab-id ()
  "Stable-ish id for the current tab-bar tab (or frame)."
  (let* ((tab (condition-case nil (tab-bar--current-tab) (error nil)))
         (name (and tab (alist-get 'name tab))))
    (or name (format "frame-%s" (sxhash-eq (selected-frame))))))

(defun treemacs--buffer-name ()
  (format "*treemacs:%s*" (treemacs--tab-id)))

(defun treemacs--get-buffer (&optional create)
  "Return (and optionally create) the treemacs buffer for the current tab-id."
  (let* ((id  (treemacs--tab-id))
         (buf (cdr (assoc id treemacs--buffers))))
    (when (and buf (not (buffer-live-p buf)))
      (setq treemacs--buffers (cl-remove id treemacs--buffers :key #'car :test #'equal)
            buf nil))
    (when (and (not buf) create)
      (setq buf (get-buffer-create (treemacs--buffer-name)))
      (with-current-buffer buf
        (treemacs-mode)
        (setq treemacs--expanded (make-hash-table :test #'equal)))
      (push (cons id buf) treemacs--buffers))
    buf))

(defun treemacs--visible-treemacs-window ()
  "Return the first live window in the selected frame that shows a treemacs buffer.
This does *not* rely on the current tab-id, so it still finds the side-window
after a buffer switch that would otherwise change the tab name."
  (cl-loop for win being the windows of (selected-frame)
           for buf = (window-buffer win)
           when (and (buffer-live-p buf)
                     (with-current-buffer buf
                       (eq major-mode 'treemacs-mode)))
           return win))

(defun treemacs-get-local-window ()
  "Compatibility wrapper – prefer the actually visible treemacs window."
  (or (treemacs--visible-treemacs-window)
      (let ((buf (treemacs--get-buffer)))
        (and buf (get-buffer-window buf)))))

(defun treemacs-get-local-buffer ()
  "Return the treemacs buffer that is currently visible, or the one for this tab."
  (or (let ((win (treemacs--visible-treemacs-window)))
        (and win (window-buffer win)))
      (treemacs--get-buffer)))

(defun treemacs-current-visibility ()
  "Return `visible', `exists', or `none'."
  (cond
   ((treemacs--visible-treemacs-window) 'visible)
   ((treemacs--get-buffer) 'exists)
   (t 'none)))


;;; Root detection

(defun treemacs--project-root (&optional buf)
  (when (fboundp 'project-current)
    (let ((target (or buf (current-buffer))))
      (condition-case nil
          (let ((proj (with-current-buffer target (project-current nil))))
            (when proj
              (let ((root (if (fboundp 'project-root)
                              (project-root proj)
                            (car (project-roots proj)))))
                (when (stringp root)
                  (file-name-as-directory (expand-file-name root))))))
        (error nil)))))

(defun treemacs--git-root (&optional dir)
  (let ((dir (expand-file-name (or dir default-directory)))
        root)
    (while (and dir (not root))
      (if (file-exists-p (expand-file-name ".git" dir))
          (setq root dir)
        (let ((parent (file-name-directory (directory-file-name dir))))
          (setq dir (and parent (not (equal parent dir)) parent)))))
    root))

(defun treemacs--dired-dir (&optional buf)
  (let ((buf (or buf (current-buffer))))
    (with-current-buffer buf
      (when (derived-mode-p 'dired-mode)
        (expand-file-name
         (if (fboundp 'dired-current-directory)
             (dired-current-directory)
           default-directory))))))

(defun treemacs--root-from-buffer (buf)
  "Resolve root from BUF only (no previous-buffer fallback)."
  (when (buffer-live-p buf)
    (condition-case nil
        (let* ((file      (buffer-local-value 'buffer-file-name buf))
               (dired-dir (treemacs--dired-dir buf))
               (dd        (buffer-local-value 'default-directory buf))
               (project-root (treemacs--project-root buf)))
          (or project-root
              (cond
               (file
                (let ((dir (file-name-directory (expand-file-name file))))
                  (or (treemacs--git-root dir) dir)))
               (dired-dir
                (or (treemacs--git-root dired-dir) dired-dir))
               ((and dd (file-directory-p dd))
                (treemacs--git-root dd)))))
      (error nil))))

(defun treemacs--detect-root (&optional buf)
  "Smart root for BUF.
File/Dired → project.el → .git → dir → last-file-buffer → $HOME."
  (let* ((buf  (or buf (current-buffer)))
         (root (treemacs--root-from-buffer buf)))
    (unless root
      (when (and treemacs--last-file-buffer
                 (buffer-live-p treemacs--last-file-buffer)
                 (not (eq treemacs--last-file-buffer buf)))
        (setq root (treemacs--root-from-buffer treemacs--last-file-buffer))))
    (or root (expand-file-name "~"))))


;;; Follow – robust buffer handling

(defun treemacs--norm-root (dir)
  "Normalize DIR for stable root comparison."
  (when dir
    (ignore-errors
      (directory-file-name (file-truename (expand-file-name dir))))))

(defvar treemacs--follow-timer nil)
(defvar treemacs--follow-generation 0)
(defvar treemacs--follow-last-root nil)

(defun treemacs--best-follow-candidate ()
  "Best live file/dired buffer that is *not* a treemacs buffer."
  (let ((ready-p
         (lambda (b)
           (and (buffer-live-p b)
                (not (with-current-buffer b (eq major-mode 'treemacs-mode)))
                (with-current-buffer b
                  (or buffer-file-name
                      (derived-mode-p 'dired-mode)))))))
    (or
     ;; 1. selected window (most common case)
     (let ((sel (window-buffer (selected-window))))
       (and (funcall ready-p sel) sel))
     ;; 2. MRU non-treemacs window
     (let ((win (get-mru-window nil t t)))
       (and (window-live-p win)
            (let ((b (window-buffer win)))
              (and (funcall ready-p b) b))))
     ;; 3. last known good
     (and (buffer-live-p treemacs--last-file-buffer)
          treemacs--last-file-buffer))))

(defun treemacs--follow (&optional generation)
  "Re-detect root from the best candidate and redraw the *visible* tree.
GENERATION discards stale scheduled calls."
  (when (or (null generation)
            (eq generation treemacs--follow-generation))
    (setq treemacs--follow-timer nil)
    (when (and treemacs-follow-mode (not treemacs--follow-inhibit))
      (let ((tree-win (treemacs--visible-treemacs-window)))
        (when (and tree-win (window-live-p tree-win))
          (let* ((tree-buf  (window-buffer tree-win))
                 (candidate (treemacs--best-follow-candidate))
                 (root      (and candidate (treemacs--detect-root candidate)))
                 (normalized (and root (treemacs--norm-root root))))
            (when (and candidate root (buffer-live-p tree-buf))
              (setq treemacs--last-file-buffer candidate)
              (with-current-buffer tree-buf
                (let ((changed (not (equal (treemacs--norm-root treemacs--root)
                                           normalized))))
                  (when changed
                    (setq treemacs--root root
                          treemacs--expanded (make-hash-table :test #'equal)))
                  (setq treemacs--follow-last-root normalized)
                  ;; Always re-render when the side window is visible.
                  (condition-case err
                      (treemacs--render)
                    (error
                     (message "treemacs--follow: render failed: %s"
                              (error-message-string err)))))))))))))

(defun treemacs--schedule-follow (&rest _)
  "True last-wins debounce via generation counter (no artificial delay)."
  (when (and treemacs-follow-mode (not treemacs--follow-inhibit))
    (when treemacs--follow-timer
      (cancel-timer treemacs--follow-timer)
      (setq treemacs--follow-timer nil))
    (cl-incf treemacs--follow-generation)
    (let ((gen treemacs--follow-generation))
      (setq treemacs--follow-timer
            (run-with-timer 0 nil
                            (lambda () (treemacs--follow gen)))))))

(defun treemacs--follow-now (&rest _)
  "Immediate follow (find-file / dired / project-switch)."
  (when treemacs--follow-timer
    (cancel-timer treemacs--follow-timer)
    (setq treemacs--follow-timer nil))
  (cl-incf treemacs--follow-generation)
  (treemacs--follow treemacs--follow-generation))

(defun treemacs--on-after-save ()
  (when-let ((win (treemacs--visible-treemacs-window)))
    (with-current-buffer (window-buffer win)
      (treemacs--render))))

(defun treemacs--after-switch-to-buffer (&rest _)
  "Advice for pure buffer switches (recentf, consult, switch-to-buffer…)."
  (when (and treemacs-follow-mode
             (not treemacs--follow-inhibit)
             (or buffer-file-name (derived-mode-p 'dired-mode)))
    (treemacs--schedule-follow)))

(defvar treemacs--hooks-installed nil)

(defun treemacs--install-hooks ()
  (unless treemacs--hooks-installed
    (add-hook 'window-buffer-change-functions
              (lambda (_) (treemacs--schedule-follow)))
    (add-hook 'window-selection-change-functions
              (lambda (_) (treemacs--schedule-follow)))
    (add-hook 'find-file-hook        #'treemacs--follow-now)
    (add-hook 'dired-mode-hook       #'treemacs--follow-now)
    (add-hook 'after-save-hook       #'treemacs--on-after-save)
    (advice-add 'project-switch-project :after #'treemacs--follow-now)
    (advice-add 'switch-to-buffer          :after #'treemacs--after-switch-to-buffer)
    (advice-add 'pop-to-buffer-same-window :after #'treemacs--after-switch-to-buffer)
    (advice-add 'display-buffer :after
                (lambda (buffer &rest _)
                  (when (and (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (or buffer-file-name
                                   (derived-mode-p 'dired-mode))))
                    (treemacs--schedule-follow))))
    (setq treemacs--hooks-installed t)))


;;; Git

(defun treemacs--git-face (status)
  (pcase status
    ("M" 'treemacs-git-modified-face)
    ("A" 'treemacs-git-added-face)
    ("?" 'treemacs-git-untracked-face)
    ("!" 'treemacs-git-ignored-face)
    ("U" 'treemacs-git-conflict-face)
    ("R" 'treemacs-git-renamed-face)
    ("D" 'treemacs-git-modified-face)
    (_ nil)))

(defun treemacs--refresh-git (root)
  (setq treemacs--git-cache nil)
  (when (and treemacs-git-mode
             (file-exists-p (expand-file-name ".git" root))
             (executable-find "git"))
    (let ((default-directory root)
          (out (ignore-errors
                 (process-lines "git" "status" "--porcelain" "--ignored=matching"))))
      (dolist (line out)
        (when (>= (length line) 3)
          (let* ((xy (substring line 0 2))
                 (path (string-trim (substring line 3)))
                 (path (if (string-match " -> " path)
                           (substring path (match-end 0)) path))
                 (path (string-trim path "\"" "\""))
                 (char (cond
                        ((string-match-p "U" xy) "U")
                        ((string-match-p "\\?" xy) "?")
                        ((string-match-p "!" xy) "!")
                        ((string-match-p "A" xy) "A")
                        ((string-match-p "R" xy) "R")
                        ((string-match-p "[MD]" xy) "M"))))
            (when (and char path)
              (push (cons path char) treemacs--git-cache))))))))

(defun treemacs--git-status-for (abs-path root)
  (when treemacs--git-cache
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (abs (expand-file-name abs-path))
           (rel (when (string-prefix-p root abs)
                  (substring abs (length root)))))
      (when rel
        (or (cdr (assoc rel treemacs--git-cache))
            (when (file-directory-p abs)
              (let (found)
                (dolist (cell treemacs--git-cache)
                  (when (string-prefix-p rel (car cell))
                    (setq found (or found (cdr cell)))))
                found)))))))


;;; Icons – treemacs PNG style

(defvar treemacs--icon-cache (make-hash-table :test #'equal)
  "Cache of propertized icon strings keyed by (file . open-p).")

(defvar treemacs--icons-map nil
  "Alist of (KEY . PNG-FILENAME) for known extensions / dir states.")

(defun treemacs--init-icons-map ()
  (setq treemacs--icons-map
        '(("dir-closed" . "vsc/dir-closed.png")
          ("dir-open"   . "vsc/dir-open.png")
          ("root"       . "vsc/root-open.png")
          ("fallback"   . "txt.png")
          ("el" . "elx.png") ("elc" . "elx.png")
          ("py" . "python.png")
          ("js" . "js.png") ("ts" . "ts.png")
          ("json" . "json.png") ("yml" . "yaml.png") ("yaml" . "yaml.png")
          ("md" . "markdown.png")
          ("html" . "html.png") ("css" . "css.png")
          ("c" . "c.png")
          ("go" . "go.png") ("rs" . "rust.png")
          ("kt" . "kotlin.png")
          ("png" . "image.png") ("jpg" . "image.png") ("gif" . "image.png")
          ("pdf" . "pdf.png")
          ("git" . "git.png") ("gitignore" . "git.png")
          ("sh" . "shell.png") ("bash" . "shell.png")
          ("txt" . "txt.png"))))

(defun treemacs--icon-file (key)
  (when (and treemacs-icons-dir (file-directory-p treemacs-icons-dir))
    (unless treemacs--icons-map (treemacs--init-icons-map))
    (let* ((name (or (cdr (assoc key treemacs--icons-map))
                     (and (stringp key) (concat key ".png"))))
           (path (and name (expand-file-name name treemacs-icons-dir))))
      (when (and path (file-readable-p path)) path))))

(defun treemacs--create-image (file-path)
  (let ((h treemacs-icon-size)
        (w treemacs-icon-size)
        (type (if (image-type-available-p 'png) 'png
                (intern (or (file-name-extension file-path) "png")))))
    (condition-case nil
        (create-image file-path type nil
                      :ascent 'center
                      :width w
                      :height h
                      :mask 'heuristic)
      (error nil))))

(defun treemacs--png-icon (key fallback)
  (let* ((cache-key (cons key treemacs-icon-size))
         (cached (gethash cache-key treemacs--icon-cache)))
    (or cached
        (let* ((file (treemacs--icon-file key))
               (img (and file (treemacs--create-image file)))
               (str (if img
                        (concat (propertize " " 'display img) " ")
                      fallback)))
          (puthash cache-key str treemacs--icon-cache)
          str))))

(defun treemacs--ext-key (path)
  (let* ((base (downcase (file-name-nondirectory path)))
         (ext  (downcase (or (file-name-extension path) ""))))
    (cond
     ((member base '(".gitignore" ".gitattributes")) "git")
     ((member base '("makefile" "gnumakefile")) "makefile")
     ((and ext (not (string-empty-p ext))) ext)
     (t "fallback"))))

(defun treemacs--icon (path dir-p open-p &optional root-p)
  (cond
   ((and treemacs-icons-dir (display-graphic-p))
    (cond
     (root-p (treemacs--png-icon "root" (propertize "• " 'face 'treemacs-root-face)))
     (dir-p  (treemacs--png-icon (if open-p "dir-open" "dir-closed")
                                 (propertize (if open-p "▾ " "▸ ")
                                             'face 'treemacs-term-node-face)))
     (t      (treemacs--png-icon (treemacs--ext-key path)
                                 (propertize "  " 'face 'treemacs-file-face)))))
   (t (if dir-p (propertize (if open-p "▾ " "▸ ") 'face 'treemacs-term-node-face) "  "))))


;;; Listing / render

(defun treemacs--list-dir (dir)
  (let (entries)
    (dolist (name (directory-files dir nil nil t))
      (unless (or (member name '("." ".."))
                  (and (not treemacs-show-hidden-files)
                       (string-prefix-p "." name)))
        (push (cons name (expand-file-name name dir)) entries)))
    (sort entries
          (lambda (a b)
            (let ((da (file-directory-p (cdr a)))
                  (db (file-directory-p (cdr b))))
              (cond ((and da (not db)) t)
                    ((and db (not da)) nil)
                    (t (string-lessp (car a) (car b)))))))))

(defun treemacs--insert-node (path depth)
  (let* ((dir-p  (file-directory-p path))
         (name   (file-name-nondirectory (directory-file-name path)))
         (open-p (and dir-p (gethash path treemacs--expanded)))
         (git    (treemacs--git-status-for path treemacs--root))
         (gface  (and git (treemacs--git-face git)))
         (face   (or gface (if dir-p 'treemacs-directory-face 'treemacs-file-face)))
         (indent (make-string (* depth treemacs-indentation) ?\s))
         (icon   (treemacs--icon path dir-p open-p))
         (start  (point)))
    (insert indent icon name "\n")
    (add-text-properties
     start (1- (point))
     (list 'treemacs-path path
           'treemacs-dir-p dir-p
           'treemacs-depth depth
           'mouse-face 'highlight
           'help-echo path
           'face face))
    (when open-p
      (dolist (e (treemacs--list-dir path))
        (treemacs--insert-node (cdr e) (1+ depth))))))

(defun treemacs--render ()
  (let ((inhibit-read-only t)
        (root treemacs--root)
        (pos (point)))
    (erase-buffer)
    (when root
      (treemacs--refresh-git root)
      (let ((start (point))
            (label (file-name-nondirectory (directory-file-name root))))
        (insert (treemacs--icon root t t t) label "\n")
        (add-text-properties
         start (1- (point))
         (list 'treemacs-path root
               'treemacs-dir-p t
               'treemacs-depth 0
               'treemacs-root t
               'face 'treemacs-root-face
               'mouse-face 'highlight
               'help-echo root))
        (puthash root t treemacs--expanded)
        (dolist (e (treemacs--list-dir root))
          (treemacs--insert-node (cdr e) 1))))
    (goto-char (min pos (point-max)))))

(defun treemacs--path-at-point ()
  (get-text-property (point) 'treemacs-path))

(defun treemacs--dir-p-at-point ()
  (get-text-property (point) 'treemacs-dir-p))

(defun treemacs--goto-path (path)
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal (treemacs--path-at-point) path)
        (throw 'found t))
      (forward-line 1))
    nil))


;;; Commands

(defun treemacs-toggle-node (&optional path)
  (interactive)
  (let ((path (or path (treemacs--path-at-point))))
    (when (and path (file-directory-p path))
      (if (gethash path treemacs--expanded)
          (remhash path treemacs--expanded)
        (puthash path t treemacs--expanded))
      (treemacs--render)
      (treemacs--goto-path path))))

(defun treemacs-visit-node-no-split ()
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (cond
     ((null path) nil)
     ((file-directory-p path) (treemacs-toggle-node path))
     (t
      (let ((treemacs--follow-inhibit t)
            (win (get-mru-window nil t t)))
        (when win (select-window win))
        (find-file path))))))

(defun treemacs-visit-node-vertical-split ()
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (when (and path (not (file-directory-p path)))
      (let ((treemacs--follow-inhibit t))
        (select-window (split-window-right))
        (find-file path)))))

(defun treemacs-visit-node-horizontal-split ()
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (when (and path (not (file-directory-p path)))
      (let ((treemacs--follow-inhibit t))
        (select-window (split-window-below))
        (find-file path)))))

(defun treemacs-visit-node-in-external-application ()
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (when path
      (pcase system-type
        ('darwin (start-process "open" nil "open" path))
        ('gnu/linux (start-process "xdg-open" nil "xdg-open" path))
        ('windows-nt (w32-shell-execute "open" path))
        (_ (message "No external opener for %s" system-type))))))

(defun treemacs-TAB-action ()
  "Toggle node (TAB)."
  (interactive)
  (treemacs-toggle-node))

(defun treemacs-RET-action ()
  "Visit or toggle node (RET)."
  (interactive)
  (treemacs-visit-node-no-split))

(defun treemacs-refresh ()
  (interactive)
  (when treemacs--root
    (treemacs--render)
    (message "treemacs: refreshed")))

(defun treemacs-create-file ()
  (interactive)
  (let* ((path (treemacs--path-at-point))
         (dir (cond ((and path (file-directory-p path)) path)
                    (path (file-name-directory path))
                    (t treemacs--root)))
         (name (read-string "New file: "))
         (full (expand-file-name name dir)))
    (when (and name (not (string-empty-p name)))
      (write-region "" nil full nil nil nil 'excl)
      (puthash dir t treemacs--expanded)
      (treemacs--render)
      (treemacs--goto-path full))))

(defun treemacs-create-dir ()
  (interactive)
  (let* ((path (treemacs--path-at-point))
         (dir (cond ((and path (file-directory-p path)) path)
                    (path (file-name-directory path))
                    (t treemacs--root)))
         (name (read-string "New directory: "))
         (full (expand-file-name name dir)))
    (when (and name (not (string-empty-p name)))
      (make-directory full t)
      (puthash dir t treemacs--expanded)
      (treemacs--render)
      (treemacs--goto-path full))))

(defun treemacs-rename-file ()
  (interactive)
  (let* ((path (treemacs--path-at-point))
         (dir (and path (file-name-directory path)))
         (old (and path (file-name-nondirectory (directory-file-name path))))
         (new (and old (read-string "Rename to: " old)))
         (dest (and new dir (expand-file-name new dir))))
    (when (and path new (not (string-empty-p new)))
      (rename-file path dest t)
      (treemacs--render)
      (treemacs--goto-path dest))))

(defun treemacs-delete-file ()
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (when (and path (y-or-n-p (format "Delete %s? " path)))
      (if (file-directory-p path)
          (delete-directory path t)
        (delete-file path))
      (treemacs--render))))

(defun treemacs-move-file ()
  (interactive)
  (let* ((path (treemacs--path-at-point))
         (dest-dir (and path (read-directory-name "Move to: ")))
         (dest (and dest-dir
                    (expand-file-name
                     (file-name-nondirectory (directory-file-name path))
                     dest-dir))))
    (when (and path dest)
      (rename-file path dest t)
      (treemacs--render)
      (treemacs--goto-path dest))))

(defun treemacs-copy-file ()
  (interactive)
  (let* ((path (treemacs--path-at-point))
         (dest-dir (and path (read-directory-name "Copy to: ")))
         (dest (and dest-dir
                    (expand-file-name
                     (file-name-nondirectory (directory-file-name path))
                     dest-dir))))
    (when (and path dest)
      (if (file-directory-p path)
          (copy-directory path dest t t t)
        (copy-file path dest t))
      (treemacs--render)
      (treemacs--goto-path dest))))

(defun treemacs-copy-absolute-path-at-point ()
  (interactive)
  (when-let ((path (treemacs--path-at-point)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun treemacs-copy-relative-path-at-point ()
  (interactive)
  (when-let ((path (treemacs--path-at-point))
             (root treemacs--root))
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (rel (if (string-prefix-p root path)
                    (substring path (length root))
                  path)))
      (kill-new rel)
      (message "Copied: %s" rel))))

(defun treemacs-copy-filename-at-point ()
  (interactive)
  (when-let ((path (treemacs--path-at-point)))
    (let ((name (file-name-nondirectory (directory-file-name path))))
      (kill-new name)
      (message "Copied: %s" name))))

(defun treemacs-copy-project-path-at-point ()
  (interactive)
  (treemacs-copy-relative-path-at-point))

(defun treemacs-toggle-show-dotfiles ()
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (treemacs-refresh)
  (message "Dotfiles %s" (if treemacs-show-hidden-files "shown" "hidden")))

(defun treemacs-next-line () (interactive) (forward-line 1))
(defun treemacs-previous-line () (interactive) (forward-line -1))

(defun treemacs-COLLAPSE-action ()
  (interactive)
  (let* ((path (treemacs--path-at-point))
         (parent (and path (file-name-directory (directory-file-name path)))))
    (when (and parent treemacs--root
               (string-prefix-p treemacs--root parent))
      (remhash (directory-file-name parent) treemacs--expanded)
      (treemacs--render)
      (treemacs--goto-path (directory-file-name parent)))))

(defun treemacs-quit (&optional _arg)
  "Hide the treemacs window for this tab.
Does not call `kill-this-buffer' (that command requires a mouse event
and is a common accidental binding for `q' in special-mode buffers)."
  (interactive "P")
  (let ((win (or (treemacs-get-local-window)
                 (and (eq major-mode 'treemacs-mode)
                      (get-buffer-window (current-buffer))))))
    (cond
     ((and win (window-live-p win))
      (quit-window nil win))
     ((eq major-mode 'treemacs-mode)
      (bury-buffer)))))

;; Workspace stubs (no config-file workspaces – per-tab isolation instead)
(defun treemacs-switch-workspace ()
  (interactive)
  (message "treemacs: workspaces are tab-bar tabs – switch tabs instead"))
(defun treemacs-create-workspace ()
  (interactive) (message "treemacs: use tab-bar-new-tab for a new tree scope"))
(defun treemacs-remove-workspace ()
  (interactive) (message "treemacs: use tab-bar-close-tab"))
(defun treemacs-rename-workspace ()
  (interactive) (message "treemacs: use tab-rename"))
(defun treemacs-edit-workspaces ()
  (interactive) (message "treemacs: no workspace file – isolation is per tab-bar"))
(defun treemacs-set-fallback-workspace ()
  (interactive) (message "treemacs: no fallback workspace in minimal build"))
(defun treemacs-next-workspace ()
  (interactive)
  (when (fboundp 'tab-next) (tab-next)))
(defalias 'treemacs-next-workspaces #'treemacs-next-workspace)
(defun treemacs-add-project ()
  (interactive)
  (treemacs-rescan-root)
  (message "Root rescanned from project.el / git / file"))
(defun treemacs-add-project-to-workspace ()
  (interactive) (treemacs-add-project))
(defun treemacs-remove-project-from-workspace ()
  (interactive) (message "treemacs: single automatic root – nothing to remove"))
(defun treemacs-rename-project ()
  (interactive) (message "treemacs: root name follows the directory"))


;;; Right-click menu (treemacs-compatible)

(defun treemacs-rightclick-menu (event)
  "Context menu for mouse EVENT."
  (interactive "e")
  (let ((win (posn-window (event-start event))))
    (when (window-live-p win)
      (select-window win)))
  (when (eq major-mode 'treemacs-mode)
    (let* ((pos (posn-point (event-start event))))
      (when (number-or-marker-p pos)
        (goto-char pos)))
    (hl-line-highlight)
    (redisplay))
  (let* ((path (treemacs--path-at-point))
         (dir-p (treemacs--dir-p-at-point))
         (open-p (and path dir-p (gethash path treemacs--expanded)))
         (menu
          (easy-menu-create-menu
           nil
           `(("New"
              ["New File"      treemacs-create-file t]
              ["New Directory" treemacs-create-dir t])
             ["Open" treemacs-visit-node-no-split :active ,(and path (not dir-p))]
             ("Open With" :active ,path
              ["Open Directly"                treemacs-visit-node-no-split]
              ["Open In External Application" treemacs-visit-node-in-external-application]
              ["Open With Vertical Split"     treemacs-visit-node-vertical-split]
              ["Open With Horizontal Split"   treemacs-visit-node-horizontal-split])
             ["Open Tags"  treemacs-toggle-node :active ,(and path dir-p (not open-p))]
             ["Close Tags" treemacs-toggle-node :active ,(and path dir-p open-p)]
             ["--" :active ,path]
             ["Rename" treemacs-rename-file :active ,path]
             ["Delete" treemacs-delete-file :active ,path]
             ["Move"   treemacs-move-file   :active ,path]
             ("Copy"
              ["Copy File"          treemacs-copy-file                    :active ,path]
              ["Copy Absolute Path" treemacs-copy-absolute-path-at-point  :active ,path]
              ["Copy Relative Path" treemacs-copy-relative-path-at-point  :active ,path]
              ["Copy Project Path"  treemacs-copy-project-path-at-point   :active ,path]
              ["Copy Filename"      treemacs-copy-filename-at-point       :active ,path])
             ["--" t]
             ("Projects"
              ["Rescan Root (project.el / git)" treemacs-rescan-root]
              ["Refresh" treemacs-refresh])
             ("Workspaces"
              ["Switch Workspace (tab)" treemacs-switch-workspace]
              ["Create Workspace (tab)" treemacs-create-workspace]
              ["Next Workspace (tab)"   treemacs-next-workspace])
             ("Toggles"
              [,(format "Dotfile Visibility (Currently %s)"
                        (if treemacs-show-hidden-files "Enabled" "Disabled"))
               treemacs-toggle-show-dotfiles]
              [,(format "Follow-Mode (Currently %s)"
                        (if treemacs-follow-mode "Enabled" "Disabled"))
               (lambda ()
                 (interactive)
                 (setq treemacs-follow-mode (not treemacs-follow-mode))
                 (message "Follow %s" (if treemacs-follow-mode "on" "off")))]
              [,(format "Git Colours (Currently %s)"
                        (if treemacs-git-mode "Enabled" "Disabled"))
               (lambda ()
                 (interactive)
                 (setq treemacs-git-mode (not treemacs-git-mode))
                 (treemacs-refresh))])
             ("Help"
              ["Refresh" treemacs-refresh]
              ["Quit"    treemacs-quit]))))
         (choice (x-popup-menu event menu)))
    (when choice
      (let ((cmd (lookup-key menu (apply #'vector choice))))
        (when (commandp cmd)
          (call-interactively cmd))))))


;;; Display / toggle

(defun treemacs--display-buffer (buf)
  (display-buffer
   buf
   `((display-buffer-in-side-window)
     (side . left)
     (slot . -1)
     (window-width . ,treemacs-width)
     (window-parameters . ((no-other-window . t)
                          (no-delete-other-windows . t))))))

;;;###autoload
(defun treemacs ()
  "Toggle treemacs for the current tab-bar tab."
  (interactive)
  (let* ((buf (treemacs--get-buffer t))
         (win (get-buffer-window buf)))
    (if (and win (window-live-p win))
        (delete-window win)
      (with-current-buffer buf
        (setq treemacs--root (treemacs--detect-root
                              (window-buffer (selected-window))))
        (setq treemacs--follow-last-root (treemacs--norm-root treemacs--root))
        (when (hash-table-p treemacs--expanded)
          (clrhash treemacs--expanded))
        (treemacs--render))
      (treemacs--display-buffer buf))))

(defun treemacs-select-window ()
  (interactive)
  (let* ((buf (treemacs--get-buffer t))
         (win (get-buffer-window buf)))
    (if win (select-window win)
      (treemacs)
      (when-let ((w (get-buffer-window buf)))
        (select-window w)))))

(defun treemacs-rescan-root (&optional buf)
  "Re-detect the root from BUF (or the best candidate) and redraw the visible tree."
  (interactive)
  (let* ((tree-win (or (treemacs--visible-treemacs-window)
                       (let ((b (treemacs--get-buffer t)))
                         (get-buffer-window b))))
         (tree     (and tree-win (window-buffer tree-win)))
         (source   (or buf (treemacs--best-follow-candidate)))
         (root     (and source (treemacs--detect-root source))))
    (when (and tree root)
      (setq treemacs--last-file-buffer source)
      (with-current-buffer tree
        (setq treemacs--root root
              treemacs--follow-last-root (treemacs--norm-root root))
        (when (hash-table-p treemacs--expanded)
          (clrhash treemacs--expanded))
        (treemacs--render)))
    root))


;;; Mode map – one persistent object (required for general.el)

(defvar treemacs-mode-map (make-sparse-keymap)
  "Keymap for `treemacs-mode'.  Do not reassign; only define-key into it.")

(defun treemacs--populate-mode-map ()
  "Define treemacs commands on the existing `treemacs-mode-map' object."
  (let ((map treemacs-mode-map))
    (define-key map (kbd "RET") #'treemacs-RET-action)
    (define-key map (kbd "TAB") #'treemacs-TAB-action)
    (define-key map (kbd "<tab>") #'treemacs-TAB-action)
    (define-key map (kbd "g") #'treemacs-refresh)
    (define-key map (kbd "r") #'treemacs-refresh)
    (define-key map (kbd "q") #'treemacs-quit)
    (define-key map (kbd "n") #'treemacs-next-line)
    (define-key map (kbd "p") #'treemacs-previous-line)
    (define-key map (kbd "j") #'treemacs-next-line)
    (define-key map (kbd "k") #'treemacs-previous-line)
    (define-key map (kbd "h") #'treemacs-COLLAPSE-action)
    (define-key map (kbd "l") #'treemacs-RET-action)
    (define-key map (kbd "R") #'treemacs-rename-file)
    (define-key map (kbd "d") #'treemacs-delete-file)
    (define-key map (kbd "m") #'treemacs-move-file)
    (define-key map (kbd "ya") #'treemacs-copy-absolute-path-at-point)
    (define-key map (kbd "yr") #'treemacs-copy-relative-path-at-point)
    (define-key map (kbd "yn") #'treemacs-copy-filename-at-point)
    (define-key map (kbd "yf") #'treemacs-copy-file)
    (define-key map (kbd "cf") #'treemacs-create-file)
    (define-key map (kbd "cd") #'treemacs-create-dir)
    (define-key map (kbd "ov") #'treemacs-visit-node-vertical-split)
    (define-key map (kbd "oh") #'treemacs-visit-node-horizontal-split)
    (define-key map (kbd "oo") #'treemacs-visit-node-no-split)
    (define-key map (kbd "ox") #'treemacs-visit-node-in-external-application)
    (define-key map [mouse-1] #'treemacs-RET-action)
    (define-key map [double-mouse-1] #'treemacs-TAB-action)
    (define-key map [down-mouse-3] #'ignore)
    (define-key map [mouse-3] #'treemacs-rightclick-menu)
    map))

(treemacs--populate-mode-map)

(define-derived-mode treemacs-mode special-mode "Treemacs"
  "Major mode for the in-house treemacs file tree."
  (setq buffer-read-only t
        truncate-lines t
        desktop-save-buffer nil
        cursor-type nil)
  (setq-local display-line-numbers nil)
  (when (fboundp 'display-line-numbers-mode)
    (display-line-numbers-mode -1))
  (when (fboundp 'linum-mode) (linum-mode -1))
  (when (fboundp 'nlinum-mode) (nlinum-mode -1))
  (when (boundp 'context-menu-functions)
    (setq-local context-menu-functions nil))
  (hl-line-mode 1)
  (setq-local mode-line-format
              '("  "
                (:eval (file-name-nondirectory
                        (directory-file-name (or treemacs--root "?"))))))
  (use-local-map treemacs-mode-map)
  (define-key treemacs-mode-map (kbd "q") #'treemacs-quit)
  (treemacs--install-hooks)
  (treemacs--setup-evil-keys))


;;; Evil – normal state only (no custom treemacs state)

(defun treemacs--setup-evil-keys ()
  "Buffer-local evil keys so `q' is never `kill-this-buffer'."
  (when (and (bound-and-true-p evil-local-mode)
             (fboundp 'evil-local-set-key))
    (evil-local-set-key 'normal (kbd "q") #'treemacs-quit)
    (evil-local-set-key 'visual (kbd "q") #'treemacs-quit)
    (evil-local-set-key 'emacs  (kbd "q") #'treemacs-quit)
    (evil-local-set-key 'normal (kbd "j") #'treemacs-next-line)
    (evil-local-set-key 'normal (kbd "k") #'treemacs-previous-line)
    (evil-local-set-key 'normal (kbd "h") #'treemacs-COLLAPSE-action)
    (evil-local-set-key 'normal (kbd "l") #'treemacs-RET-action)
    (evil-local-set-key 'normal (kbd "g") #'treemacs-refresh)
    (evil-local-set-key 'normal (kbd "r") #'treemacs-refresh)))

(with-eval-after-load 'evil
  (evil-set-initial-state 'treemacs-mode 'normal)
  (when (fboundp 'evil-define-key*)
    (evil-define-key* 'normal treemacs-mode-map
      (kbd "j") #'treemacs-next-line
      (kbd "k") #'treemacs-previous-line
      (kbd "h") #'treemacs-COLLAPSE-action
      (kbd "l") #'treemacs-RET-action
      (kbd "g") #'treemacs-refresh
      (kbd "r") #'treemacs-refresh
      (kbd "q") #'treemacs-quit
      (kbd "R") #'treemacs-rename-file
      (kbd "d") #'treemacs-delete-file
      (kbd "m") #'treemacs-move-file
      (kbd "RET") #'treemacs-RET-action
      (kbd "TAB") #'treemacs-TAB-action)
    (evil-define-key* 'visual treemacs-mode-map
      (kbd "j") #'treemacs-next-line
      (kbd "k") #'treemacs-previous-line
      (kbd "q") #'treemacs-quit)
    (evil-define-key* 'emacs treemacs-mode-map
      (kbd "j") #'treemacs-next-line
      (kbd "k") #'treemacs-previous-line
      (kbd "q") #'treemacs-quit))
  (when (fboundp 'evil-normalize-keymaps)
    (evil-normalize-keymaps)))

(add-hook 'treemacs-mode-hook #'treemacs--setup-evil-keys)

(provide 'treemacs)
;;; treemacs.el ends here
