;;; cfg-op-treemacs-speedbar.el --- configfuration for treemacs and speedbar -*- lexical-binding: t -*-
;;; Commentary:
;;
;; treemacs:
;; https://github.com/Alexander-Miller/treemacs
;;
;; sr-speedbar:
;; https://github.com/emacsorphanage/sr-speedbar/tree/77a83fb50f763a465c021eca7343243f465b4a47
;;
;;; Code:

(use-package treemacs
  :ensure t
  :config

  ;; ---------------------------------------------------------------------------
  ;; Basic configuration
  ;; ---------------------------------------------------------------------------

  ;; Disable line numbers inside Treemacs buffer.
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))

  (setq
   ;; Show hidden files.
   treemacs-show-hidden-files   t

   ;; Reduce indentation width.
   treemacs-indentation         1

   ;; Disable automatic file following.
   treemacs-follow-mode         nil

   ;; Enable automatic project following.
   treemacs-project-follow-mode t)

  ;; Set icon size.
  (treemacs-resize-icons 18)

  ;; Treemacs filewatch mode is known to be unstable when
  ;; workspaces are switched frequently.
  ;;
  ;; It is the primary source of errors like:
  ;;
  ;;   wrong-type-argument integer-or-marker-p nil
  ;;
  (treemacs-filewatch-mode -1)

  ;; ---------------------------------------------------------------------------
  ;; Helper functions
  ;; ---------------------------------------------------------------------------

  (defun cfg/-safe-project-root ()
    "Return current project root or nil without throwing errors."
    (when-let ((proj (project-current nil)))
      (project-root proj)))

  (defun cfg/-treemacs-find-matching-workspace (project-dir)
    "Find a Treemacs workspace whose name ends with the project directory name.

The function extracts the project name from PROJECT-DIR and searches
through all available Treemacs workspaces. It returns the first workspace
whose name matches the pattern:

    <workspace-name> ends with <project-name>

If no matching workspace is found, it returns nil.

Example:

  PROJECT-DIR:
    /home/user/projects/my-app

  Extracted project name:
    my-app

  Workspace names:
    \"DEV-my-app\"
    \"my-app\"
    \"legacy-app\"

  Match result:
    => \"DEV-my-app\" (first match found)

Another example:

  PROJECT-DIR:
    /home/user/code/alpha

  Workspace names:
    \"alpha\"
    \"beta\"
    \"gamma-alpha\"

  Match result:
    => \"alpha\" (exact ending match)

Matching rule:
  (string-match (concat project-name \"$\") workspace-name)

This ensures only workspaces that END exactly with the project name are considered.
For example, \"foo-bar\" will NOT match \"foobar\" or \"foo-bar-baz\"."

    (let* ((project-name
            (file-name-nondirectory
             (directory-file-name project-dir)))

           (workspaces
            (treemacs-workspaces)))

      (catch 'found
        (dolist (ws workspaces)
          (let ((name (treemacs-workspace->name ws)))
            (when (string-match
                   (concat (regexp-quote project-name) "$")
                   name)
              (throw 'found name))))
        nil)))

  ;; ---------------------------------------------------------------------------
  ;; Safe workspace switching
  ;; ---------------------------------------------------------------------------

  (defun cfg/-treemacs-switch-workspace-on-project-switch (project-dir)
    "Switch Treemacs workspace based on PROJECT-DIR.

If no matching workspace exists, fallback to \"MAIN\"."

    (let ((workspace-name
           (or (cfg/-treemacs-find-matching-workspace project-dir)
               "MAIN")))

      (condition-case err
          (when (treemacs-current-visibility)
            (treemacs-do-switch-workspace workspace-name))

        (error
         (message "Treemacs switch error: %s" err)))))

  ;; ---------------------------------------------------------------------------
  ;; project-switch-project integration
  ;; ---------------------------------------------------------------------------

  ;; Automatically switch Treemacs workspace after project switch.
  (advice-add
   'project-switch-project
   :after
   (lambda (&rest _)
     (when-let ((dir (cfg/-safe-project-root)))
       (cfg/-treemacs-switch-workspace-on-project-switch dir))))

  ;; ---------------------------------------------------------------------------
  ;; Debounced automatic workspace switching
  ;; ---------------------------------------------------------------------------

  ;; Timer used for debouncing workspace switches.
  (defvar cfg/-treemacs-switch-timer nil)

  (defun cfg/-treemacs-auto-switch-on-buffer-change ()
    "Debounced Treemacs workspace switch."

    ;; Cancel previous pending timer.
    (when cfg/-treemacs-switch-timer
      (cancel-timer cfg/-treemacs-switch-timer))

    ;; Schedule workspace switch after Emacs becomes idle.
    (setq cfg/-treemacs-switch-timer
          (run-with-idle-timer
           0.3
           nil
           #'cfg/-treemacs--do-buffer-switch)))

  (defun cfg/-treemacs--do-buffer-switch ()
    "Perform safe Treemacs workspace switch."

    (when-let* ((file (buffer-file-name))
                (dir  (cfg/-safe-project-root)))

      ;; Only continue if Treemacs is visible.
      (when (treemacs-current-visibility)

        (let* ((target-workspace
                (or (cfg/-treemacs-find-matching-workspace dir)
                    "MAIN"))

               (current-workspace
                (when-let ((ws (treemacs-current-workspace)))
                  (treemacs-workspace->name ws))))

          ;; Avoid unnecessary workspace switches.
          (unless (equal target-workspace current-workspace)

            (condition-case err
                (treemacs-do-switch-workspace target-workspace)

              (error
               (message "Treemacs switch error: %s" err))))))))

  ;; buffer-list-update-hook is safer than find-file-hook.
  ;;
  ;; find-file-hook is too aggressive and may trigger race conditions
  ;; with Treemacs refresh timers and filewatch events.
  (add-hook 'buffer-list-update-hook
            #'cfg/-treemacs-auto-switch-on-buffer-change)

  ;; ---------------------------------------------------------------------------
  ;; Safe refresh wrapper
  ;; ---------------------------------------------------------------------------

  (defun cfg/-treemacs-safe-refresh (orig &rest args)
    "Protect Treemacs refresh from internal marker errors."

    (condition-case err
        (apply orig args)

      (error
       (message
        "Treemacs refresh error suppressed: %s"
        err))))

  (advice-add
   'treemacs-refresh
   :around
   #'cfg/-treemacs-safe-refresh)

  ;; ---------------------------------------------------------------------------
  ;; Safe project position wrapper
  ;; ---------------------------------------------------------------------------

  (defun cfg/-treemacs-safe-project-position (orig project)
    "Protect `treemacs-project->position' from invalid state."
    (when (and project
               (treemacs-current-visibility))
      (ignore-errors
        (funcall orig project))))

  (advice-add
   'treemacs-project->position
   :around
   #'cfg/-treemacs-safe-project-position)

  ;; ---------------------------------------------------------------------------
  ;; general.el keybindings
  ;; ---------------------------------------------------------------------------

  (require 'cfg-gen-op-treemacs-mode))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t
  :config
  )

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t
  :config
  )

(use-package treemacs-icons-dired
  :after treemacs
  :ensure t
  :config
  )

(use-package treemacs-tab-bar
  :after (treemacs tab-bar)
  :ensure t
  :config
  (treemacs-set-scope-type 'Tabs))

(use-package sr-speedbar
  :ensure t
  :config
  )

(provide 'cfg-op-treemacs-speedbar)
;;; cfg-op-treemacs-speedbar.el ends here
