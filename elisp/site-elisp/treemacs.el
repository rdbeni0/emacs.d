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
;; Identification: `treemacs-in-house' is non-nil for this build.
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

(defcustom treemacs-icon-size 20
  "Pixel size for PNG icons."
  :type 'integer :group 'treemacs)

(defconst treemacs-in-house t
  "Non-nil when this is the in-house minimal treemacs replacement.
Other packages can use (bound-and-true-p treemacs-in-house).")

(defcustom treemacs-icons-directory nil
  "Explicit directory with treemacs-style PNG icons, or nil to auto-detect."
  :type '(choice (directory :tag "Icons directory")
                 (const :tag "Auto-detect (default)" nil))
  :group 'treemacs)

(defcustom treemacs-double-click-time 0
  "Reserved for compatibility; activation uses explicit selection, not time.
A click on `treemacs--selected-path' always opens/toggles that node.
Emacs `double-click-time' is not used."
  :type 'number
  :group 'treemacs)

(defun treemacs--find-icons-dir ()
  "Locate treemacs icons/default when `treemacs-icons-directory' is nil."
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

(defun treemacs--icons-dir ()
  "Return effective icons directory: override or auto-detect."
  (let ((override treemacs-icons-directory))
    (cond
     ((and (stringp override)
           (not (string-empty-p override))
           (file-directory-p override))
      override)
     (t (treemacs--find-icons-dir)))))


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

(defvar-local treemacs--root nil
  "Root directory shown in this treemacs buffer.")
(defvar-local treemacs--expanded nil
  "Hash table of expanded directory paths.")
(defvar-local treemacs--git-cache nil
  "Alist of relative path to git status char.")

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
  "Return the buffer name for the current tab-id."
  (format "*treemacs:%s*" (treemacs--tab-id)))

(defun treemacs--get-buffer (&optional create)
  "Return the treemacs buffer for the current tab-id.
When CREATE is non-nil, create the buffer if it does not exist."
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

;; ---------------------------------------------------------------------------
;; WARNING – DO NOT REPLACE WITH TAB-ID / BUFFER-NAME LOOKUP
;; When the side window was visible but unselected, root follow failed often;
;; lookup used a volatile tab-id.  Fix: scan the frame for treemacs-mode.
;; Do not simplify back to (get-buffer-window (treemacs--get-buffer)).
;; ---------------------------------------------------------------------------
(defun treemacs--visible-treemacs-window ()
  "Return a live window on this frame showing a treemacs buffer.
Do not rely on tab-id; still find the side-window after a buffer switch."
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
  "Return the project.el root of BUF, or nil."
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
  "Return the nearest .git root above DIR, or nil."
  (let ((dir (expand-file-name (or dir default-directory)))
        root)
    (while (and dir (not root))
      (if (file-exists-p (expand-file-name ".git" dir))
          (setq root dir)
        (let ((parent (file-name-directory (directory-file-name dir))))
          (setq dir (and parent (not (equal parent dir)) parent)))))
    root))

(defun treemacs--dired-dir (&optional buf)
  "Return the Dired directory of BUF, or nil."
  (let ((buf (or buf (current-buffer))))
    (with-current-buffer buf
      (when (derived-mode-p 'dired-mode)
        (expand-file-name
         (if (fboundp 'dired-current-directory)
             (dired-current-directory)
           default-directory))))))

(defun treemacs--root-from-buffer (buf)
  "Resolve root from BUF only (no `previous-buffer' fallback)."
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
  "Return a smart project root for BUF.
Order: project.el, .git, file/Dired directory, previous file buffer, $HOME."
  (let* ((buf  (or buf (current-buffer)))
         (root (treemacs--root-from-buffer buf)))
    (unless root
      (when (and treemacs--last-file-buffer
                 (buffer-live-p treemacs--last-file-buffer)
                 (not (eq treemacs--last-file-buffer buf)))
        (setq root (treemacs--root-from-buffer treemacs--last-file-buffer))))
    (or root (expand-file-name "~"))))


;;; Follow – robust buffer handling
;; WARNING: use `treemacs--visible-treemacs-window', generation debounce,
;; and switch-to-buffer advice.  Changing only one piece reintroduces bugs.

(defun treemacs--norm-root (dir)
  "Normalize DIR for stable root comparison."
  (when dir
    (ignore-errors
      (directory-file-name (file-truename (expand-file-name dir))))))

(defvar treemacs--follow-timer nil
  "Pending follow timer object, or nil.")
(defvar treemacs--follow-generation 0
  "Monotonic generation for last-wins follow scheduling.")
(defvar treemacs--follow-last-root nil
  "Normalized root last applied by follow.")

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
                  (setq treemacs--follow-last-root normalized)
                  ;; Redraw only when the root changes (same-project switches
                  ;; no longer erase/rebuild the whole tree).
                  (when changed
                    (setq treemacs--root root
                          treemacs--expanded (make-hash-table :test #'equal))
                    (condition-case err
                        (treemacs--render)
                      (error
                       (message "treemacs--follow: render failed: %s"
                                (error-message-string err))))))))))))))

(defun treemacs--schedule-follow (&rest _)
  "Schedule last-wins follow via generation counter (no artificial delay)."
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
  "Run follow immediately after `find-file', Dired, or project-switch."
  (when treemacs--follow-timer
    (cancel-timer treemacs--follow-timer)
    (setq treemacs--follow-timer nil))
  (cl-incf treemacs--follow-generation)
  (treemacs--follow treemacs--follow-generation))

(defun treemacs--on-after-save ()
  "Refresh the visible treemacs window after save."
  (when-let ((win (treemacs--visible-treemacs-window)))
    (with-current-buffer (window-buffer win)
      (treemacs--render))))

(defun treemacs--after-switch-to-buffer (&rest _)
  "Advise buffer switch events such as recentf, consult, and `switch-to-buffer'."
  (when (and treemacs-follow-mode
             (not treemacs--follow-inhibit)
             (or buffer-file-name (derived-mode-p 'dired-mode)))
    (treemacs--schedule-follow)))

(defvar treemacs--hooks-installed nil
  "Non-nil after follow hooks and advice are installed.")

(defun treemacs--install-hooks ()
  "Install follow hooks and advice once."
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
  "Return a face symbol for git STATUS, or nil."
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
  "Refresh git status cache for ROOT."
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
  "Return git status for ABS-PATH under ROOT, or nil."
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
  "Alist of (KEY . RELATIVE-FILENAME) under `treemacs--icons-dir'.")

(defun treemacs--init-icons-map ()
  "Initialize icons map covering the in-tree icons/default layout."
  (setq treemacs--icons-map
        '(;; Directory / root states (prefer vsc PNGs)
          ("dir-closed" . "vsc/dir-closed.png")
          ("dir-open"   . "vsc/dir-open.png")
          ("root"       . "vsc/root-open.png")
          ("root-open"  . "vsc/root-open.png")
          ("root-closed" . "vsc/root-closed.png")
          ;; Named directory variants
          ("dir-docs-open"      . "vsc/dir-docs-open.png")
          ("dir-docs-closed"    . "vsc/dir-docs-closed.png")
          ("dir-test-open"      . "vsc/dir-test-open.svg")
          ("dir-test-closed"    . "vsc/dir-test-closed.svg")
          ("dir-src-open"       . "vsc/dir-src-open.svg")
          ("dir-src-closed"     . "vsc/dir-src-closed.svg")
          ("dir-git-open"       . "vsc/dir-git-open.svg")
          ("dir-git-closed"     . "vsc/dir-git-closed.svg")
          ("dir-github-open"    . "vsc/dir-github-open.png")
          ("dir-github-closed"  . "vsc/dir-github-closed.png")
          ("dir-images-open"    . "vsc/dir-images-open.png")
          ("dir-images-closed"  . "vsc/dir-images-closed.png")
          ("dir-private-open"   . "vsc/dir-private-open.png")
          ("dir-private-closed" . "vsc/dir-private-closed.png")
          ("dir-public-open"    . "vsc/dir-public-open.png")
          ("dir-public-closed"  . "vsc/dir-public-closed.png")
          ("dir-services-open"  . "vsc/dir-services-open.png")
          ("dir-services-closed" . "vsc/dir-services-closed.png")
          ("dir-temp-open"      . "vsc/dir-temp-open.png")
          ("dir-temp-closed"    . "vsc/dir-temp-closed.png")
          ("dir-binary-open"    . "vsc/dir-binary-open.png")
          ("dir-binary-closed"  . "vsc/dir-binary-closed.png")
          ("fallback"   . "txt.png")
          ("txt"        . "txt.png")
          ;; Top-level default/*.png
          ("el" . "elx.png") ("elc" . "elx.png") ("eln" . "elx.png")
          ("elx" . "elx.png")
          ("py" . "python.png") ("pyw" . "python.png") ("pyc" . "python.png")
          ("python" . "python.png")
          ("js" . "js.png") ("mjs" . "js.png") ("cjs" . "js.png")
          ("ts" . "ts.png") ("tsx" . "ts.png")
          ("json" . "json.png")
          ("yml" . "yaml.png") ("yaml" . "yaml.png")
          ("md" . "markdown.png") ("markdown" . "markdown.png") ("rst" . "markdown.png")
          ("html" . "html.png") ("htm" . "html.png")
          ("css" . "css.png")
          ("c" . "c.png") ("h" . "c.png")
          ("go" . "go.png")
          ("rs" . "rust.png") ("rust" . "rust.png")
          ("kt" . "kotlin.png") ("kts" . "kotlin.png") ("kotlin" . "kotlin.png")
          ("png" . "image.png") ("jpg" . "image.png") ("jpeg" . "image.png")
          ("gif" . "image.png") ("webp" . "image.png") ("bmp" . "image.png")
          ("ico" . "image.png") ("svg" . "image.png") ("image" . "image.png")
          ("pdf" . "pdf.png")
          ("git" . "git.png") ("gitignore" . "git.png") ("gitattributes" . "git.png")
          ("gitmodules" . "git.png")
          ("sh" . "shell.png") ("bash" . "shell.png") ("zsh" . "shell.png")
          ("fish" . "shell.png") ("shell" . "shell.png")
          ("ps1" . "powershell.png") ("psm1" . "powershell.png")
          ("powershell" . "powershell.png")
          ("mp3" . "audio.png") ("wav" . "audio.png") ("flac" . "audio.png")
          ("ogg" . "audio.png") ("audio" . "audio.png")
          ("mp4" . "video.png") ("mkv" . "video.png") ("webm" . "video.png")
          ("mov" . "video.png") ("video" . "video.png")
          ("vue" . "vue.png")
          ("direnv" . "direnv.png") ("envrc" . "direnv.png")
          ("scala" . "scala.png")
          ("emacs" . "emacs.png")
          ("vagrant" . "vagrant.png") ("vagrantfile" . "vagrant.png")
          ("bookmark" . "bookmark.png")
          ("hs" . "haskell.png") ("lhs" . "haskell.png") ("haskell" . "haskell.png")
          ("gradle" . "gradle.png")
          ("conf" . "conf.png") ("cfg" . "conf.png") ("ini" . "conf.png")
          ("sbt" . "sbt.png")
          ("babel" . "babel.png") ("babelrc" . "babel.png")
          ("purs" . "purescript.png") ("purescript" . "purescript.png")
          ("clj" . "clojure.png") ("cljs" . "clojure.png") ("cljc" . "clojure.png")
          ("edn" . "clojure.png") ("clojure" . "clojure.png")
          ("jar" . "jar.png")
          ("cabal" . "cabal.png")
          ("j2" . "jinja2.png") ("jinja" . "jinja2.png") ("jinja2" . "jinja2.png")
          ("nix" . "nix.png")
          ("php" . "php.png")
          ("tex" . "tex.png") ("latex" . "tex.png") ("ltx" . "tex.png")
          ("dart" . "dart.png")
          ("erl" . "erlang.png") ("hrl" . "erlang.png") ("erlang" . "erlang.png")
          ("docker" . "docker.png") ("dockerfile" . "docker.png")
          ("racket" . "racket.png") ("rkt" . "racket.png")
          ("scons" . "scons.png")
          ("ml" . "ocaml.png") ("mli" . "ocaml.png") ("ocaml" . "ocaml.png")
          ("project" . "project.png")
          ("ledger" . "ledger.png")
          ("lock" . "lock.png")
          ("puppet" . "puppet.png") ("pp" . "vsc/pp.png")
          ("systemd" . "systemd.png") ("service" . "systemd.png")
          ("jl" . "julia.png") ("julia" . "julia.png")
          ("hy" . "hy.png")
          ("asciidoc" . "asciidoc.png") ("adoc" . "asciidoc.png")
          ("info" . "info.png")
          ("warning" . "warning.png")
          ("error" . "error.png")
          ("buffer-group-open" . "buffer-group-open.png")
          ("buffer-group-closed" . "buffer-group-closed.png")
          ("tags-open" . "tags-open.png")
          ("tags-closed" . "tags-closed.png")
          ("tags-leaf" . "tags-leaf.png")
          ;; vsc/* extensions and tools
          ("java" . "vsc/java.png")
          ("class" . "vsc/class.png")
          ("cpp" . "vsc/cpp.png") ("cc" . "vsc/cpp.png") ("cxx" . "vsc/cpp.png")
          ("hpp" . "vsc/cpph.png") ("hh" . "vsc/cpph.png") ("hxx" . "vsc/cpph.png")
          ("cpph" . "vsc/cpph.png")
          ("cs" . "vsc/class.png")
          ("rb" . "vsc/ruby.png") ("ruby" . "vsc/ruby.png") ("erb" . "vsc/erb.png")
          ("phpunit" . "vsc/phpunit.png")
          ("swift" . "vsc/swift.png")
          ("lua" . "vsc/lua.png")
          ("sql" . "vsc/sql.png") ("pgsql" . "vsc/pgsql.png") ("plsql" . "vsc/plsql.png")
          ("sqlite" . "vsc/sqlite.png")
          ("xml" . "vsc/xml.png") ("xsl" . "vsc/xml.png") ("xslt" . "vsc/xml.png")
          ("toml" . "vsc/toml.png")
          ("scheme" . "scheme-scm.png")
          ("scm" . "scheme-scm.png")
          ("sld" . "scheme-sld.png")
          ("scss" . "vsc/scss.png") ("sass" . "vsc/scss.png") ("less" . "vsc/less.png")
          ("bat" . "vsc/bat.png") ("cmd" . "vsc/bat.png")
          ("pl" . "vsc/perl.png") ("pm" . "vsc/perl.png") ("perl" . "vsc/perl.png")
          ("perl6" . "vsc/perl6.png") ("raku" . "vsc/perl6.png")
          ("r" . "vsc/r.png") ("rdata" . "vsc/r.png")
          ("fs" . "vsc/fsharp.png") ("fsi" . "vsc/fsharp.png") ("fsproj" . "vsc/fsproj.png")
          ("fsharp" . "vsc/fsharp.png")
          ("ex" . "vsc/elx.png") ("exs" . "vsc/elx.png")
          ("vim" . "vsc/vim.png") ("vimrc" . "vsc/vim.png")
          ("elm" . "vsc/elm.png")
          ("nim" . "vsc/nim.png")
          ("zig" . "vsc/any.png")
          ("wasm" . "vsc/wasm.png")
          ("proto" . "vsc/protobuf.png") ("protobuf" . "vsc/protobuf.png")
          ("graphql" . "vsc/graphql.png") ("gql" . "vsc/graphql.png")
          ("tf" . "vsc/terraform.png") ("tfvars" . "vsc/terraform.png")
          ("terraform" . "vsc/terraform.png")
          ("cmake" . "vsc/cmake.png")
          ("make" . "vsc/make.png") ("makefile" . "vsc/make.png")
          ("meson" . "vsc/meson.png")
          ("bazel" . "vsc/bazel.png") ("bzl" . "vsc/bazel.png")
          ("zip" . "vsc/zip.png") ("tar" . "vsc/zip.png") ("gz" . "vsc/zip.png")
          ("7z" . "vsc/zip.png") ("rar" . "vsc/zip.png")
          ("bin" . "vsc/binary.png") ("exe" . "vsc/binary.png") ("dll" . "vsc/binary.png")
          ("so" . "vsc/binary.png") ("o" . "vsc/binary.png") ("binary" . "vsc/binary.png")
          ("diff" . "vsc/diff.png") ("patch" . "vsc/patch.png")
          ("log" . "vsc/log.png")
          ("key" . "vsc/key.png") ("pem" . "vsc/key.png") ("crt" . "vsc/cert.png")
          ("cert" . "vsc/cert.png") ("cer" . "vsc/cert.png")
          ("license" . "vsc/license.png")
          ("npm" . "vsc/npm.png") ("package" . "vsc/npm.png")
          ("yarn" . "vsc/yarn.png")
          ("eslint" . "vsc/eslint.png")
          ("prettier" . "vsc/prettier.png")
          ("webpack" . "vsc/npm.png")
          ("bower" . "vsc/bower.png")
          ("pip" . "vsc/pip.png") ("requirements" . "vsc/pip.png")
          ("django" . "vsc/django.png")
          ("jupyter" . "vsc/jupyter.png") ("ipynb" . "vsc/jupyter.png")
          ("maven" . "vsc/maven.png") ("pom" . "vsc/maven.png")
          ("gradle-vsc" . "gradle.png")
          ("nginx" . "vsc/nginx.png")
          ("apache" . "vsc/apache.png")
          ("ansible" . "vsc/ansible.png")
          ("helm" . "vsc/helm.png")
          ("jenkins" . "vsc/jenkins.png") ("jenkinsfile" . "vsc/jenkins.png")
          ("cargo" . "vsc/cargo.png")
          ("bundler" . "vsc/bundler.png") ("gemfile" . "vsc/bundler.png")
          ("rake" . "vsc/rake.png") ("rakefile" . "vsc/rake.png")
          ("godot" . "vsc/godot.png") ("gd" . "vsc/godot.png")
          ("unity" . "vsc/any.png")
          ("arduino" . "vsc/arduino.png") ("ino" . "vsc/arduino.png")
          ("asm" . "vsc/asm.png") ("s" . "vsc/asm.png")
          ("lisp" . "vsc/lisp.png") ("lsp" . "vsc/lisp.png") ("cl" . "vsc/lisp.png")
          ("prolog" . "vsc/prolog.png")
          ("cobol" . "vsc/cobol.png") ("cbl" . "vsc/cobol.png")
          ("fortran" . "vsc/fortran.png") ("f90" . "vsc/fortran.png") ("f95" . "vsc/fortran.png")
          ("d" . "vsc/dlang.png") ("dlang" . "vsc/dlang.png")
          ("reason" . "vsc/reason.png") ("re" . "vsc/reason.png")
          ("org" . "vsc/org.png")
          ("font" . "vsc/font.png") ("ttf" . "vsc/font.png") ("otf" . "vsc/font.png")
          ("woff" . "vsc/font.png") ("woff2" . "vsc/font.png")
          ("locale" . "vsc/locale.png") ("po" . "vsc/locale.png") ("pot" . "vsc/locale.png")
          ("mo" . "vsc/locale.png")
          ("excel" . "vsc/excel.png") ("xls" . "vsc/excel.png") ("xlsx" . "vsc/excel.png")
          ("csv" . "vsc/excel.png")
          ("access" . "vsc/access.png")
          ("apple" . "vsc/apple.png")
          ("angular" . "vsc/angular.png")
          ("swagger" . "vsc/swagger.png")
          ("cucumber" . "vsc/cucumber.png") ("feature" . "vsc/cucumber.png")
          ("cython" . "vsc/cython.png") ("pyx" . "vsc/cython.png")
          ("delphi" . "vsc/delphi.png") ("pas" . "vsc/delphi.png")
          ("asp" . "vsc/asp.png") ("aspx" . "vsc/asp.png")
          ("ai" . "vsc/ai.png")
          ("antlr" . "vsc/antlr.png") ("g4" . "vsc/antlr.png")
          ("appveyor" . "vsc/appveyor.png")
          ("editorconfig" . "vsc/editorcfg.png") ("editorcfg" . "vsc/editorcfg.png")
          ("manifest" . "vsc/manifest.png")
          ("deps" . "vsc/deps.png")
          ("pkg" . "vsc/pkg.png")
          ("alaw" . "vsc/alaw.png")
          ("autohk" . "vsc/autohk.png") ("ahk" . "vsc/autohk.png")
          ("ascript" . "vsc/ascript.png")
          ("cfscript" . "vsc/cfscript.png")
          ("any" . "vsc/any.png"))))

(defun treemacs--icon-candidates (key)
  "Return list of relative icon paths to try for KEY."
  (unless treemacs--icons-map (treemacs--init-icons-map))
  (let* ((mapped (cdr (assoc key treemacs--icons-map)))
         (k (and (stringp key) key)))
    (delq nil
          (list mapped
                (and k (concat k ".png"))
                (and k (concat "vsc/" k ".png"))
                (and k (concat "vsc/" k ".svg"))
                (and k (concat "svgrepo/" k ".png"))
                (and k (concat "svgrepo/" k ".svg"))
                (and k (concat k ".svg"))))))

(defun treemacs--icon-file (key)
  "Return absolute path to icon image for KEY, or nil.
Tries mapped path, then KEY.png / vsc/KEY.png / .svg variants."
  (let ((icons (treemacs--icons-dir)))
    (when (and icons (file-directory-p icons))
      (cl-loop for name in (treemacs--icon-candidates key)
               for path = (expand-file-name name icons)
               when (file-readable-p path)
               return path))))

(defun treemacs--create-image (file-path)
  "Create a scaled image from FILE-PATH, or nil."
  (let* ((h treemacs-icon-size)
         (w treemacs-icon-size)
         (ext (downcase (or (file-name-extension file-path) "png")))
         (type (cond
                ((member ext '("png" "jpg" "jpeg" "gif" "tiff" "xpm"))
                 (if (image-type-available-p (intern ext)) (intern ext) 'png))
                ((string= ext "svg")
                 (if (image-type-available-p 'svg) 'svg 'png))
                (t (if (image-type-available-p 'png) 'png (intern ext))))))
    (condition-case nil
        (apply #'create-image file-path type nil
               :ascent 'center
               :width w
               :height h
               (when (eq type 'png) (list :mask 'heuristic)))
      (error nil))))

(defun treemacs--png-icon (key fallback)
  "Return a propertized icon string for KEY with FALLBACK."
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

(defun treemacs--basename-key (base)
  "Map full basename BASE (no dir) to an icons-map key, or nil."
  (let ((b (downcase base)))
    (cond
     ((member b '(".gitignore" ".gitattributes" ".gitmodules")) "git")
     ((member b '("makefile" "gnumakefile" "cmakelists.txt")) "makefile")
     ((member b '("dockerfile" "docker-compose.yml" "docker-compose.yaml")) "docker")
     ((member b '("vagrantfile")) "vagrant")
     ((member b '("gemfile" "gemfile.lock")) "bundler")
     ((member b '("rakefile")) "rake")
     ((member b '("jenkinsfile")) "jenkins")
     ((member b '("package.json" "package-lock.json")) "npm")
     ((member b '("yarn.lock")) "yarn")
     ((member b '("cargo.toml" "cargo.lock")) "cargo")
     ((member b '("pom.xml")) "maven")
     ((member b '("requirements.txt" "pipfile" "pyproject.toml")) "pip")
     ((member b '(".editorconfig")) "editorconfig")
     ((member b '(".babelrc" "babel.config.js")) "babel")
     ((member b '(".eslintrc" ".eslintrc.js" ".eslintrc.json")) "eslint")
     ((member b '(".envrc")) "direnv")
     ((member b '("license" "license.md" "license.txt" "copying")) "license")
     ((string-suffix-p ".service" b) "systemd")
     (t nil))))

(defun treemacs--ext-key (path)
  "Return icons-map key derived from PATH."
  (let* ((base (file-name-nondirectory path))
         (base-key (treemacs--basename-key base))
         (ext (downcase (or (file-name-extension path) ""))))
    (or base-key
        (and ext (not (string-empty-p ext)) ext)
        "fallback")))

(defun treemacs--dir-icon-key (path open-p)
  "Return icons-map key for directory PATH given OPEN-P."
  (let ((name (downcase (file-name-nondirectory (directory-file-name path)))))
    (cond
     ((member name '("docs" "doc" "documentation"))
      (if open-p "dir-docs-open" "dir-docs-closed"))
     ((member name '("test" "tests" "__tests__" "spec" "specs"))
      (if open-p "dir-test-open" "dir-test-closed"))
     ((member name '("src" "source" "sources" "lib" "library"))
      (if open-p "dir-src-open" "dir-src-closed"))
     ((member name '(".git"))
      (if open-p "dir-git-open" "dir-git-closed"))
     ((member name '(".github" ".gitlab"))
      (if open-p "dir-github-open" "dir-github-closed"))
     ((member name '("images" "img" "icons" "assets" "static"))
      (if open-p "dir-images-open" "dir-images-closed"))
     ((member name '("private" "secret" "secrets"))
      (if open-p "dir-private-open" "dir-private-closed"))
     ((member name '("public" "www" "web" "dist" "out" "output" "build"))
      (if open-p "dir-public-open" "dir-public-closed"))
     ((member name '("services" "service" "api" "server"))
      (if open-p "dir-services-open" "dir-services-closed"))
     ((member name '("tmp" "temp" "cache" ".cache"))
      (if open-p "dir-temp-open" "dir-temp-closed"))
     ((member name '("bin" "obj" "binary" "binaries"))
      (if open-p "dir-binary-open" "dir-binary-closed"))
     (t (if open-p "dir-open" "dir-closed")))))

(defun treemacs--icon (path dir-p open-p &optional root-p)
  "Return icon string for PATH.
DIR-P non-nil means directory, OPEN-P means expanded, ROOT-P means root node."
  (cond
   ((and (treemacs--icons-dir) (display-graphic-p))
    (cond
     (root-p
      (treemacs--png-icon (if open-p "root-open" "root-closed")
                          (propertize "• " 'face 'treemacs-root-face)))
     (dir-p
      (treemacs--png-icon (treemacs--dir-icon-key path open-p)
                          (propertize (if open-p "▾ " "▸ ")
                                      'face 'treemacs-term-node-face)))
     (t
      (treemacs--png-icon (treemacs--ext-key path)
                          (propertize "  " 'face 'treemacs-file-face)))))
   (t (if dir-p (propertize (if open-p "▾ " "▸ ") 'face 'treemacs-term-node-face) "  "))))


;;; Listing / render

(defun treemacs--list-dir (dir)
  "Return sorted directory entries for DIR."
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
  "Insert PATH into the tree at DEPTH."
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
  "Redraw the current treemacs buffer without intermediate flicker.

Run git I/O before `erase-buffer', and keep `inhibit-redisplay' set until
the new tree is fully inserted so an empty buffer is never shown."
  (let* ((inhibit-read-only t)
         (inhibit-redisplay t)
         (inhibit-modification-hooks t)
         (root treemacs--root)
         (pos (point)))
    ;; Heavy work while the previous tree is still visible.
    (when root
      (treemacs--refresh-git root))
    (erase-buffer)
    (when root
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
    (goto-char (min pos (point-max)))
    (when (bound-and-true-p hl-line-mode)
      (hl-line-highlight))))

(defun treemacs--path-at-point ()
  "Return filesystem path at point, or nil."
  (get-text-property (point) 'treemacs-path))

(defun treemacs--dir-p-at-point ()
  "Return non-nil if point is on a directory node."
  (get-text-property (point) 'treemacs-dir-p))

(defun treemacs--goto-path (path)
  "Move point to PATH in the tree; return non-nil if found."
  (goto-char (point-min))
  (catch 'found
    (while (not (eobp))
      (when (equal (treemacs--path-at-point) path)
        (throw 'found t))
      (forward-line 1))
    nil))


;;; Commands

(defun treemacs-toggle-node (&optional path)
  "Toggle expand or collapse of PATH or the node at point."
  (interactive)
  (let ((path (or path (treemacs--path-at-point))))
    (when (and path (file-directory-p path))
      (if (gethash path treemacs--expanded)
          (remhash path treemacs--expanded)
        (puthash path t treemacs--expanded))
      (treemacs--render)
      (treemacs--goto-path path))))

(defun treemacs-visit-node-no-split ()
  "Visit file at point or toggle a directory."
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
  "Visit file at point in a vertical split."
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (when (and path (not (file-directory-p path)))
      (let ((treemacs--follow-inhibit t))
        (select-window (split-window-right))
        (find-file path)))))

(defun treemacs-visit-node-horizontal-split ()
  "Visit file at point in a horizontal split."
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (when (and path (not (file-directory-p path)))
      (let ((treemacs--follow-inhibit t))
        (select-window (split-window-below))
        (find-file path)))))

(defun treemacs-visit-node-in-external-application ()
  "Open path at point with the OS handler."
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

(defun treemacs--clear-mouse-region ()
  "Drop any active region; show selection with `hl-line' only."
  (when (region-active-p)
    (deactivate-mark t))
  (setq mark-active nil)
  (when (and (fboundp 'evil-visual-state-p) (evil-visual-state-p)
             (fboundp 'evil-normal-state))
    (evil-normal-state)))

(defvar-local treemacs--selected-path nil
  "Path explicitly selected by the last mouse click (highlight target).
Only a click on this same path opens the node; path-at-point alone is
not enough (avoids accidental open when point already sits on a line).")

(defun treemacs--event-window-pos (event)
  "Return (WINDOW POS BUFFER) for mouse EVENT, or nils."
  (let* ((start (event-start event))
         (window (posn-window start))
         (pos (posn-point start))
         (buf (and (window-live-p window) (window-buffer window))))
    (list window pos buf)))

(defun treemacs--path-at-pos (pos &optional buffer)
  "Return treemacs path text-property at POS in BUFFER."
  (when (and (number-or-marker-p pos) buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (get-text-property pos 'treemacs-path))))

(defun treemacs--select-node-at (pos &optional buffer)
  "Move point to POS in BUFFER, clear region, update hl-line.
Return path or nil."
  (when (and (number-or-marker-p pos) buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (goto-char pos)
      (treemacs--clear-mouse-region)
      (when (fboundp 'hl-line-highlight)
        (hl-line-highlight))
      (treemacs--path-at-point))))

(defun treemacs--open-at-point ()
  "Visit or toggle node at point; keep selection + hl-line in treemacs."
  (treemacs--clear-mouse-region)
  (let ((path (treemacs--path-at-point)))
    (when path
      (setq treemacs--selected-path path))
    (treemacs-RET-action)
    (when (eq major-mode 'treemacs-mode)
      (treemacs--clear-mouse-region)
      (when path
        (setq treemacs--selected-path path))
      (when (fboundp 'hl-line-highlight)
        (hl-line-highlight)))))

(defun treemacs-leftclick-action (event)
  "Highlight the node under EVENT, or open it if already selected.

Precise mixed model (no timing):
1. Paths are always read from the event window buffer (not the
   previously selected window) – fixes focus-switch races.
2. Selection is an explicit `treemacs--selected-path', not mere
   point position – first click never opens just because point
   was already on that line.
3. Click on the selected path → always open/toggle (works after
   leaving and returning to the treemacs window).
4. Click on another path → only select/highlight it."
  (interactive "e")
  (pcase-let* ((`(,window ,pos ,buf) (treemacs--event-window-pos event))
               (clicked (treemacs--path-at-pos pos buf))
               ;; Read selection from the *treemacs* buffer, not the
               ;; previously focused window (critical when clicking in).
               (selected (and buf (buffer-live-p buf)
                              (buffer-local-value 'treemacs--selected-path buf)))
               (already (and clicked selected (equal clicked selected))))
    (when (window-live-p window)
      (select-window window))
    (treemacs--select-node-at pos buf)
    (if (and already clicked)
        (treemacs--open-at-point)
      ;; Pure selection – never open on the first click on a node.
      (when buf
        (with-current-buffer buf
          (setq treemacs--selected-path clicked))))))

(defun treemacs-doubleclick-action (event)
  "Open or toggle the node under EVENT (system double-click)."
  (interactive "e")
  (pcase-let* ((`(,window ,pos ,buf) (treemacs--event-window-pos event)))
    (when (window-live-p window)
      (select-window window))
    (treemacs--select-node-at pos buf)
    (setq treemacs--selected-path (treemacs--path-at-point))
    (treemacs--open-at-point)))

(defun treemacs-refresh ()
  "Redraw the current treemacs buffer."
  (interactive)
  (when treemacs--root
    (treemacs--render)
    (message "treemacs: refreshed")))

(defun treemacs-create-file ()
  "Create a new file under point or the tree root."
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
  "Create a new directory under point or the tree root."
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
  "Rename the filesystem entry at point."
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
  "Delete the filesystem entry at point after confirm."
  (interactive)
  (let ((path (treemacs--path-at-point)))
    (when (and path (y-or-n-p (format "Delete %s? " path)))
      (if (file-directory-p path)
          (delete-directory path t)
        (delete-file path))
      (treemacs--render))))

(defun treemacs-move-file ()
  "Move the filesystem entry at point."
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
  "Copy the filesystem entry at point."
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
  "Copy absolute path at point to the kill ring."
  (interactive)
  (when-let ((path (treemacs--path-at-point)))
    (kill-new path)
    (message "Copied: %s" path)))

(defun treemacs-copy-relative-path-at-point ()
  "Copy root-relative path at point to the kill ring."
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
  "Copy filename at point to the kill ring."
  (interactive)
  (when-let ((path (treemacs--path-at-point)))
    (let ((name (file-name-nondirectory (directory-file-name path))))
      (kill-new name)
      (message "Copied: %s" name))))

(defun treemacs-copy-project-path-at-point ()
  "Copy project-relative path at point."
  (interactive)
  (treemacs-copy-relative-path-at-point))

(defun treemacs-toggle-show-dotfiles ()
  "Toggle dotfile visibility and refresh."
  (interactive)
  (setq treemacs-show-hidden-files (not treemacs-show-hidden-files))
  (treemacs-refresh)
  (message "Dotfiles %s" (if treemacs-show-hidden-files "shown" "hidden")))

(defun treemacs-next-line ()
  "Move to the next line."
  (interactive)
  (forward-line 1))
(defun treemacs-previous-line ()
  "Move to the previous line."
  (interactive)
  (forward-line -1))
(defun treemacs-COLLAPSE-action ()
  "Collapse the parent of the node at point."
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
Do not call `kill-this-buffer' (needs a mouse event; common accidental
binding for `q' in `special-mode' buffers)."
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
  "Stub: workspaces map to tab-bar tabs."
  (interactive)
  (message "treemacs: workspaces are tab-bar tabs – switch tabs instead"))
(defun treemacs-create-workspace ()
  "Stub: use `tab-bar-new-tab' for a new scope."
  (interactive) (message "treemacs: use tab-bar-new-tab for a new tree scope"))
(defun treemacs-remove-workspace ()
  "Stub: use `tab-bar-close-tab'."
  (interactive) (message "treemacs: use tab-bar-close-tab"))
(defun treemacs-rename-workspace ()
  "Stub: use `tab-rename'."
  (interactive) (message "treemacs: use tab-rename"))
(defun treemacs-edit-workspaces ()
  "Stub: no workspace file in this build."
  (interactive) (message "treemacs: no workspace file – isolation is per tab-bar"))
(defun treemacs-set-fallback-workspace ()
  "Stub: no fallback workspace in this build."
  (interactive) (message "treemacs: no fallback workspace in minimal build"))
(defun treemacs-next-workspace ()
  "Switch to the next tab-bar tab when available."
  (interactive)
  (when (fboundp 'tab-next) (tab-next)))
(defalias 'treemacs-next-workspaces #'treemacs-next-workspace)
(defun treemacs-add-project ()
  "Rescan root from project.el, git, or file path."
  (interactive)
  (treemacs-rescan-root)
  (message "Root rescanned from project.el / git / file"))
(defun treemacs-add-project-to-workspace ()
  "Rescan root (alias of treemacs-add-project)."
  (interactive) (treemacs-add-project))
(defun treemacs-remove-project-from-workspace ()
  "Stub: single automatic root cannot be removed."
  (interactive) (message "treemacs: single automatic root – nothing to remove"))
(defun treemacs-rename-project ()
  "Stub: root label follows the directory name."
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
  "Display BUF in a left side window."
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
  "Select the treemacs window, opening it if needed."
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


;;; Mode map – bindings live in cfg-gen-op-treemacs-mode.el
;; Intentionally empty here: all keys (keyboard, mouse, evil) are
;; installed from cfg-gen-op-treemacs-mode.el so they stay centralized.

(defvar treemacs-mode-map (make-sparse-keymap)
  "Keymap for `treemacs-mode'.
Do not reassign this object.  Bindings are added externally
\(see cfg-gen-op-treemacs-mode.el).")

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
  (treemacs--install-hooks))

(with-eval-after-load 'evil
  (evil-set-initial-state 'treemacs-mode 'normal))


(provide 'treemacs)
;;; treemacs.el ends here
