;;; cfg-completion-systems.el --- configfuration for completion styles and engines -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with completion styles in bottom minibuffer.

;;; Code:

(use-package vertico
  :ensure t
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
            ;; (vertico-directory-mode 1)
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))))


(use-package orderless
  :ensure t
  :after vertico
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  orderless-component-separator #'orderless-escapable-split-on-space)

            ;; Use the built-in "partial-completion" style to complete
            ;; file inputs such as "/e/ni/co.nix" into
            ;; "/etc/nixos/configuration.nix".  The "basic" style is
            ;; needed to support the hostname completion in the TRAMP
            ;; inputs such as "/sshx:HOSTNAME".
            (setq completion-category-defaults nil
                  completion-category-overrides '((file (styles basic partial-completion))))

            (setq completion-styles '(orderless basic))))

(use-package marginalia
  :ensure t
  :after vertico
  ;; :demand t
  :config (marginalia-mode 1))

;; https://github.com/minad/consult
(use-package consult
  :ensure t
  :bind (     ;; Remaps
         ([remap Info-search]                   . consult-info)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap man]                           . consult-man)
         ([remap org-goto]                      . consult-org-heading)
         ([remap project-switch-to-buffer]      . consult-project-buffer)
         ([remap projectile-switch-to-buffer]   . consult-project-buffer)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap repeat-complex-command]        . consult-complex-command)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap rgrep]                         . consult-grep)
         ([remap vc-git-grep]                   . consult-git-grep)
         ([remap projectile-ripgrep]            . consult-ripgrep)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap kmacro-view-macro-repeat]      . consult-kmacro)
	 )
  :config (progn
	    ;; remove automatic previev of selected entry:
            (consult-customize
             consult-ripgrep consult-grep
             consult-buffer consult-recent-file
             :preview-key "M-.")

            (defun cfg/orderless-fix-consult-tofu (pattern index total)
              "Ignore the last character which is hidden and used only internally."
              (when (string-suffix-p "$" pattern)
		`(orderless-regexp . ,(concat (substring pattern 0 -1)
                                              "[\x200000-\x300000]*$"))))

            (dolist (command '(consult-buffer consult-line))
              (advice-add command :around
                          (lambda (orig &rest args)
                            (let ((orderless-style-dispatchers (cons #'cfg/orderless-fix-consult-tofu
                                                                     orderless-style-dispatchers)))
                              (apply orig args)))))

            ;; Disable consult-buffer project-related capabilities as
            ;; they are very slow in TRAMP.
            (setq consult-buffer-sources
                  (delq 'consult--source-project-buffer
			(delq 'consult--source-project-file consult-buffer-sources)))

            (setq consult--source-hidden-buffer
                  (plist-put consult--source-hidden-buffer :narrow ?h))

            (defun cfg/isearch-to-consult-line ()
              "Search using `consult-line' what was being searched with `isearch'."
              (interactive)
              (isearch-exit)
              (let ((query (if isearch-regexp
                               isearch-string
                             (regexp-quote isearch-string))))
		(consult-line query)))))

(provide 'cfg-completion-systems)
;;; cfg-completion-systems.el ends here
