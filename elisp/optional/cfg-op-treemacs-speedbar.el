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
  ;;;; Probably not required.
  ;; (require 'treemacs-project-follow-mode)
  ;; (setq treemacs-project-follow-mode t)
  (add-hook 'treemacs-mode-hook (lambda() (display-line-numbers-mode -1)))
  (setq
   treemacs-show-hidden-files               t
   treemacs-indentation                     1
   treemacs-follow-mode                     nil
   treemacs-project-follow-mode             t
   )
  (treemacs-resize-icons 18) ;; icon's size
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
  ;; load general.el and keybindings:
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
  (treemacs-set-scope-type 'Tabs)

  (defun cfg/-safe-project-root ()
    "Return project root or nil without throwing."
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
    (let* ((project-name (file-name-nondirectory
                          (directory-file-name project-dir)))
           (workspaces (treemacs-workspaces)))
      (catch 'found
        (dolist (ws workspaces)
          (let ((name (treemacs-workspace->name ws)))
            (when (string-match (concat project-name "$") name)
              (throw 'found name))))
        nil)))

  (defun cfg/-treemacs-switch-workspace-on-project-switch (project-dir)
    "Switch Treemacs workspace based on PROJECT-DIR.
If no matching workspace is found, fall back to \"MAIN\".
Never creates new workspaces."
    (let* ((workspace-name
            (or (cfg/-treemacs-find-matching-workspace project-dir)
                "MAIN")))
      (condition-case err
          (treemacs-do-switch-workspace workspace-name)
        (error
         (message "Treemacs switch error: %s" err)))))

  (advice-add 'project-switch-project :after
              (lambda (dir)
                (when-let ((dir (cfg/-safe-project-root)))
                  (cfg/-treemacs-switch-workspace-on-project-switch dir))))


  (defun cfg/-treemacs-auto-switch-on-buffer-change ()
    "Switch Treemacs workspace based on current buffer.
Do nothing if file is not part of a project."
    (when-let* ((file (buffer-file-name))
                (dir (cfg/-safe-project-root)))
      (let* ((target-workspace
              (or (cfg/-treemacs-find-matching-workspace dir)
                  "MAIN"))
             (current-workspace
              (treemacs-workspace->name
               (treemacs-current-workspace))))
        (unless (equal target-workspace current-workspace)
          (condition-case err
              (treemacs-do-switch-workspace target-workspace)
            (error
             (message "Treemacs switch error: %s" err)))))))


  (add-hook 'find-file-hook #'cfg/-treemacs-auto-switch-on-buffer-change)

  (defun cfg/-treemacs-safe-project-position (orig project)
    (when project
      (funcall orig project)))

  (advice-add 'treemacs-project->position :around #'cfg/-treemacs-safe-project-position))

(use-package sr-speedbar
  :ensure t
  :config
  )

(provide 'cfg-op-treemacs-speedbar)
;;; cfg-op-treemacs-speedbar.el ends here
