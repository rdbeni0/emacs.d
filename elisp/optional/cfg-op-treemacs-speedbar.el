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

  ;; Disable line numbers inside Treemacs buffers.
  ;;
  ;; Treemacs is a special-purpose side window and line numbers
  ;; do not provide useful information there.
  ;;
  ;; Disabling them also slightly reduces rendering overhead.
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)))

  (setq
   ;; Show hidden files such as:
   ;;
   ;;   .git
   ;;   .env
   ;;   .clang-format
   ;;
   ;; This is useful for development-oriented workflows.
   treemacs-show-hidden-files t

   ;; Use smaller indentation width in the tree.
   ;;
   ;; This allows more content to fit horizontally.
   treemacs-indentation 1

   ;; Disable automatic file following.
   ;;
   ;; Treemacs follow mode attempts to synchronize the tree
   ;; with the currently selected buffer.
   ;;
   ;; In this configuration we implement our own workspace
   ;; switching logic, therefore built-in follow mode is disabled
   ;; to avoid conflicting behaviors.
   treemacs-follow-mode nil

   ;; Disable built-in project follow mode.
   ;;
   ;; This mode internally reacts to project changes and may
   ;; trigger additional workspace or tree updates.
   ;;
   ;; Since this configuration already performs explicit
   ;; workspace switching, enabling this mode could introduce
   ;; duplicated logic, race conditions, or unstable internal
   ;; Treemacs state transitions.
   treemacs-project-follow-mode nil)

  ;; Set icon size.
  ;;
  ;; Larger icons improve readability on high-DPI displays.
  (treemacs-resize-icons 18)

  ;; Disable Treemacs filewatch mode.
  ;;
  ;; Filewatch mode uses asynchronous filesystem notifications
  ;; to refresh Treemacs automatically when files or directories
  ;; change on disk.
  ;;
  ;; Unfortunately this subsystem is known to become unstable
  ;; when:
  ;;
  ;;   - workspaces are switched frequently
  ;;   - projects refresh while switching buffers
  ;;   - filesystem events arrive during tree rendering
  ;;
  ;; The most common failure looks like:
  ;;
  ;;   wrong-type-argument integer-or-marker-p nil
  ;;
  ;; originating from:
  ;;
  ;;   treemacs--process-file-events
  ;;
  ;; Disabling filewatch mode significantly improves stability
  ;; at the cost of losing automatic refresh behavior.
  ;;
  ;; Manual refresh remains available through:
  ;;
  ;;   M-x treemacs-refresh
  ;;
  (treemacs-filewatch-mode -1)

  ;; ---------------------------------------------------------------------------
  ;; Helper functions
  ;; ---------------------------------------------------------------------------

  (defun cfg/-safe-project-root ()
    "Return current project root or nil without throwing errors.

This function safely wraps `project-current'.

If the current buffer does not belong to a project,
the function returns nil instead of throwing errors."

    (when-let ((proj (project-current nil)))
      (project-root proj)))

  (defun cfg/-treemacs-find-matching-workspace (project-dir)
    "Find a Treemacs workspace whose name ends with project name.

Example:

  PROJECT-DIR:
    /home/user/projects/my-app

  Extracted project name:
    my-app

Matching workspaces:
  DEV-my-app
  my-app

Non-matching workspaces:
  my-app-test
  foobar

Matching rule:
  workspace-name ends with project-name"

    (let* ((project-name
            (file-name-nondirectory
             (directory-file-name project-dir)))

           (workspaces
            (treemacs-workspaces)))

      (catch 'found
        (dolist (ws workspaces)
          (let ((name (treemacs-workspace->name ws)))
            (when (string-match
                   (concat
                    (regexp-quote project-name)
                    "$")
                   name)
              (throw 'found name))))
        nil)))

  ;; ---------------------------------------------------------------------------
  ;; Workspace switching protection
  ;; ---------------------------------------------------------------------------

  ;; Prevent recursive or overlapping workspace switches.
  ;;
  ;; Some Treemacs operations internally trigger:
  ;;
  ;;   - buffer changes
  ;;   - window updates
  ;;   - refresh operations
  ;;
  ;; which may recursively invoke switching logic again.
  ;;
  ;; This variable acts as a reentrancy guard.
  (defvar cfg/-treemacs-switch-in-progress nil)

  ;; Timer used for debouncing workspace switching.
  ;;
  ;; Frequent buffer changes can otherwise trigger large numbers
  ;; of workspace switch attempts in a short time period.
  (defvar cfg/-treemacs-switch-timer nil)

  ;; ---------------------------------------------------------------------------
  ;; Safe workspace switching
  ;; ---------------------------------------------------------------------------

  (defun cfg/-treemacs-switch-workspace (workspace-name)
    "Safely switch Treemacs to WORKSPACE-NAME.

This function applies several protections:

  - avoids recursive switching
  - avoids switching when Treemacs is hidden
  - suppresses internal Treemacs errors
  - avoids minibuffer interference"

    (unless cfg/-treemacs-switch-in-progress

      ;; Avoid workspace switching during minibuffer activity.
      ;;
      ;; Certain commands temporarily alter window state while
      ;; minibuffer is active.
      ;;
      ;; Avoiding switches here improves overall stability.
      (unless (active-minibuffer-window)

        ;; Only continue when Treemacs is visible.
        ;;
        ;; Attempting workspace operations while Treemacs is
        ;; hidden may trigger invalid internal state.
        (when (treemacs-current-visibility)

          (let ((cfg/-treemacs-switch-in-progress t))

            (condition-case err

                (treemacs-do-switch-workspace workspace-name)

              (error
               (message
                "Treemacs workspace switch error: %s"
                err))))))))

  (defun cfg/-treemacs-switch-workspace-on-project-switch (project-dir)
    "Switch Treemacs workspace based on PROJECT-DIR.

If no matching workspace exists,
fallback to workspace named \"MAIN\"."

    (let ((workspace-name
           (or (cfg/-treemacs-find-matching-workspace project-dir)
               "MAIN")))

      (cfg/-treemacs-switch-workspace workspace-name)))

  ;; ---------------------------------------------------------------------------
  ;; project.el integration
  ;; ---------------------------------------------------------------------------

  ;; Automatically switch Treemacs workspace after:
  ;;
  ;;   M-x project-switch-project
  ;;
  ;; completes.
  ;;
  ;; This keeps Treemacs synchronized with project.el workflow.
  (advice-add
   'project-switch-project
   :after
   (lambda (&rest _)
     (when-let ((dir (cfg/-safe-project-root)))
       (cfg/-treemacs-switch-workspace-on-project-switch dir))))

  ;; ---------------------------------------------------------------------------
  ;; Debounced automatic workspace switching
  ;; ---------------------------------------------------------------------------

  (defun cfg/-treemacs-auto-switch-on-buffer-change ()
    "Schedule debounced Treemacs workspace switch.

This function intentionally does not switch immediately.

Instead it waits until Emacs becomes idle for a short period.
This dramatically reduces race conditions involving:

  - buffer switching
  - tree rendering
  - refresh timers
  - asynchronous Treemacs updates"

    ;; Cancel previous pending timer.
    ;;
    ;; This ensures that many rapid buffer changes collapse
    ;; into a single workspace switch operation.
    (when cfg/-treemacs-switch-timer
      (cancel-timer cfg/-treemacs-switch-timer))

    ;; Schedule actual switch operation.
    ;;
    ;; 0.3 seconds is usually enough to allow:
    ;;
    ;;   - window changes
    ;;   - mode hooks
    ;;   - project detection
    ;;
    ;; to stabilize before touching Treemacs state.
    (setq cfg/-treemacs-switch-timer
          (run-with-idle-timer
           0.3
           nil
           #'cfg/-treemacs--do-buffer-switch)))

  (defun cfg/-treemacs--do-buffer-switch ()
    "Perform actual safe workspace switch."

    ;; Only continue for file-backed buffers.
    ;;
    ;; Buffers like:
    ;;
    ;;   *Messages*
    ;;   *scratch*
    ;;   minibuffer
    ;;
    ;; should not affect workspace selection.
    (when-let* ((file (buffer-file-name))
                (dir  (cfg/-safe-project-root)))

      (let* ((target-workspace
              (or (cfg/-treemacs-find-matching-workspace dir)
                  "MAIN"))

             (current-workspace
              (when-let ((ws (treemacs-current-workspace)))
                (treemacs-workspace->name ws))))

        ;; Avoid unnecessary workspace switches.
        ;;
        ;; Re-switching to the same workspace creates
        ;; unnecessary Treemacs updates and increases the risk
        ;; of hitting internal race conditions.
        (unless (equal target-workspace current-workspace)

          (cfg/-treemacs-switch-workspace
           target-workspace)))))

  ;; Use buffer-list-update-hook instead of find-file-hook.
  ;;
  ;; find-file-hook executes too early and too aggressively.
  ;;
  ;; It may run during:
  ;;
  ;;   - file opening
  ;;   - tree refresh
  ;;   - async rendering
  ;;   - workspace updates
  ;;
  ;; which can trigger invalid Treemacs internal marker states.
  ;;
  ;; buffer-list-update-hook is much safer because it executes
  ;; after buffer/window state stabilizes.
  (add-hook 'buffer-list-update-hook
            #'cfg/-treemacs-auto-switch-on-buffer-change)

  ;; ---------------------------------------------------------------------------
  ;; Safe refresh wrapper
  ;; ---------------------------------------------------------------------------

  (defun cfg/-treemacs-safe-refresh (orig &rest args)
    "Protect Treemacs refresh from internal marker errors.

Treemacs occasionally throws internal errors during refresh,
especially after asynchronous state changes.

This wrapper prevents such errors from interrupting normal
editor workflow."

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
    "Protect `treemacs-project->position' from invalid state.

This function is one of the common sources of errors like:

  wrong-type-argument integer-or-marker-p nil

The wrapper suppresses failures caused by invalid internal
Treemacs markers."

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

  ;; Load user-defined Treemacs keybindings.
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
