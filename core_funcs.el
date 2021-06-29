(defun sudo-edit (&optional arg)
"Edit as sudo user"
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name parsed-method
                                                     parsed-user
                                                     parsed-host
                                                     nil
                                                     parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name "sudo"
                                                       "root"
                                                       parsed-host
                                                       parsed-localname
                                                       new-hop)))
           new-fname))))))

(defun kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (yes-or-no-p (format "Killing all buffers except \"%s\"? "
                             (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(defun alternate-buffer (&optional window)
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

(defun delete-current-buffer-file ()
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
            (when (projectile-project-p)
              (call-interactively #'projectile-invalidate-cache))
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

(defun rename-current-buffer-file (&optional arg)
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
                 (when (projectile-project-p)
                   (call-interactively #'projectile-invalidate-cache))
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

(defun new-empty-buffer ()
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

(defun sudired ()
  (interactive)
  (require 'tramp)
  (let ((dir (expand-file-name default-directory)))
    (if (string-match "^/sudo:" dir)
        (user-error "Already in sudo")
      (dired (concat "/sudo::" dir)))))

(defun grep-recentf (filepattern pattern)
  (interactive "sFiles regexp: \nsSearch regexp: ")
  (let ((files (if filepattern
                   (cl-remove-if-not (lambda (item) (string-match filepattern item))
                                     recentf-list)
                 recentf-list))
        (limit 50)
        (grep-use-null-device nil))
    (if (> (length files) limit)
        (subseq files 0 limit))

    (let* ((tempfile (make-temp-file "emacs"))
           (orig compilation-finish-functions))
      (add-to-list 'compilation-finish-functions
                   (lambda (buf result)
                     (setq font-lock-keywords-case-fold-search t)
                     (highlight-regexp pattern 'hi-yellow)
                     (delete-file tempfile)
                     (setq compilation-finish-functions orig)))

      (write-region  (mapconcat 'identity files (char-to-string 0))
                     nil tempfile)

      (grep (format "%s %s | xargs -0 grep -n -i \"%s\" " pattern)))))

(defun company-show-doc-buffer-f1 ()
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (or (company-call-backend 'doc-buffer selected)
                         (error "No documentation available"))))
    (with-current-buffer doc-buffer
      (goto-char (point-min)))
    (display-buffer doc-buffer t)))

(defun multi-term-buffer-rn ()
    "Open multi-term buffer with special name and prefix, it could be useful for custom buffer filtering (via tabs or helm)"
    (interactive)
    (require 'multi-term)
    (command-execute 'multi-term)
    (setq-default truncate-lines nil)
    (if (not (boundp 'term-number))
        (defvar term-number 1 "term index in the current emacs session") )
    (rename-buffer (concat ">" (int-to-string term-number) " term" ))
    (setq term-number (+ 1 term-number)))

(defun px-tabbar-buffer-help-on-tab (tab)
    "Return the help string shown when mouse is onto TAB."
    (if tabbar--buffer-show-groups
        (let* ((tabset (tabbar-tab-tabset tab))
               (tab (tabbar-selected-tab tabset)))
          (format "mouse-1: switch to buffer %S in group [%s]"
                  (buffer-name (tabbar-tab-value tab)) tabset))
      (format "\
mouse-1: switch to %S\n\
mouse-2: kill %S"
              (buffer-name (tabbar-tab-value tab))
              (buffer-name (tabbar-tab-value tab))
              (buffer-name (tabbar-tab-value tab)))))

(defun px-tabbar-buffer-select-tab (event tab)
    "On mouse EVENT, select TAB."
    (let ((mouse-button (event-basic-type event))
          (buffer (tabbar-tab-value tab)))
      (cond
       ((eq mouse-button 'mouse-3) (with-current-buffer buffer (kill-buffer)))
       (t (switch-to-buffer buffer)))
      (tabbar-buffer-show-groups nil)))

(defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
  That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
                      (format " [%s] " (tabbar-tab-tabset tab))
                    (format " %s " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset)))))))))

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

(defun my-named-shell ()
  "Equivalent to C-u M-x shell RET"
  (interactive)
  (let ((current-prefix-arg '(4)))
  (call-interactively 'shell)))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

(defun agamacs//helm-do-grep-region-or-symbol
    (&optional targs use-region-or-symbol-p)
  "Version of `helm-do-grep' with a default input."
  (interactive)
  (require 'helm)
  (cl-letf*
      (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
       ((symbol-function 'helm-do-grep-1)
        (lambda (targets &optional recurse zgrep exts
                         default-input region-or-symbol-p)
          (let* ((new-input (when region-or-symbol-p
                              (if (region-active-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))
                                (thing-at-point 'symbol t))))
                 (quoted-input (when new-input
                                 (rxt-quote-pcre new-input))))
            (this-fn targets recurse zgrep exts
                     default-input quoted-input))))
       (preselection (or (dired-get-filename nil t)
                         (buffer-file-name (current-buffer))))
       (targets   (if targs
                      targs
                    (helm-read-file-name
                     "Search in file(s): "
                     :marked-candidates t
                     :preselect (when preselection
                                  (if helm-ff-transformer-show-only-basename
                                      (helm-basename preselection)
                                    preselection))))))
    (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

(defun agamacs/helm-file-do-grep ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (agamacs//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) nil))

(defun agamacs/helm-file-do-grep-region-or-symbol ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (agamacs//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) t))

(defun agamacs/helm-files-do-grep ()
  "Search in files with `grep'."
  (interactive)
  (agamacs//helm-do-grep-region-or-symbol nil nil))

(defun agamacs/helm-files-do-grep-region-or-symbol ()
  "Search in files with `grep' using a default input."
  (interactive)
  (agamacs//helm-do-grep-region-or-symbol nil t))

(defun agamacs/helm-buffers-do-grep ()
  "Search in opened buffers with `grep'."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (agamacs//helm-do-grep-region-or-symbol buffers nil)))

(defun agamacs/helm-buffers-do-grep-region-or-symbol ()
  "Search in opened buffers with `grep' with a default input."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (agamacs//helm-do-grep-region-or-symbol buffers t)))

(defun agamacs/helm-jump-in-buffer ()
  "Jump in buffer using `imenu' facilities and helm."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'helm-org-in-buffer-headings)
    (t 'helm-semantic-or-imenu))))
