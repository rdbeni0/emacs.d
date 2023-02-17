;;; cfg-completion-systems.el --- configfuration for completion styles and engines -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with completion styles in bottom minibuffer.

;;; Code:

(use-package vertico
  :ensure t
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
	 ;; M- keys for changing view:
         ("M-v"     . vertico-multiform-vertical)
         ("M-g"     . vertico-multiform-grid)
         ("M-f"     . vertico-multiform-flat)
         ("M-r"     . vertico-multiform-reverse)
         ("M-u"     . vertico-multiform-unobtrusive))
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
	    (setq vertico-count 20) ;; number of candidates and also size of minibuffer
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
	      ;; left/right arrows for changing directory:
              ("<right>"   . vertico-directory-enter)
              ("<left>"    . vertico-directory-delete-word)
              ("M-<left>"  . vertico-directory-delete-char))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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
  :bind (;; Remaps - emacs native:
         ([remap Info-search]                     . consult-info)
         ([remap bookmark-jump]                   . consult-bookmark)
         ([remap goto-line]                       . consult-goto-line)
         ([remap imenu]                           . consult-imenu)
         ([remap locate]                          . consult-locate)
         ([remap man]                             . consult-man)
         ([remap org-goto]                        . consult-org-heading)
         ([remap project-switch-to-buffer]        . consult-project-buffer)
         ([remap recentf-open-files]              . consult-recent-file)
         ([remap repeat-complex-command]          . consult-complex-command)
         ([remap switch-to-buffer-other-frame]    . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-window]   . consult-buffer-other-window)
         ([remap switch-to-buffer]                . consult-buffer)
         ([remap rgrep]                           . consult-grep)
         ([remap vc-git-grep]                     . consult-git-grep)
         ([remap find-dired]                      . consult-find)
         ([remap yank-pop]                        . consult-yank-pop)
         ([remap kmacro-view-macro-repeat]        . consult-kmacro)
         ([remap xref-show-xrefs-function]        . consult-xref)
         ([remap xref-show-definitions-function]  . consult-xref)
         ([remap xref-find-references]            . consult-xref)
         ([remap xref-find-definitions]           . consult-xref)
         ([remap flymake-show-diagnostic]         . consult-flymake)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake)
         ([remap flymake-show-diagnostics-buffer] . consult-flymake)
         ([remap customize]                       . consult-customize)
         ([remap load-theme]                      . consult-theme)
         ([remap isearch-forward]                 . consult-line)
	 ;; Remaps - emacs plugins:
         ([remap projectile-switch-to-buffer]     . consult-project-buffer)
         ([remap projectile-ripgrep]              . consult-ripgrep)
         ([remap evil-paste-pop]                  . consult-yank-pop)
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


;; https://github.com/oantolin/embark

(use-package embark
  :ensure t
  :bind (("C-h B"    . embark-bindings)
	 :map vertico-map
	 ("<tab>"    . embark-act)
	 ("C-<tab>"  . embark-export)
	 ;; keys for scrolling mixed indicator buffer:
	 ("C-d"      . scroll-other-window)       ; vim-style
	 ("C-u"      . scroll-other-window-down)  ; vim style
	 ("<next>"   . scroll-other-window)       ; page down key
	 ("<prior>"  . scroll-other-window-down)  ; page up key
	 ("M-<down>" . scroll-other-window)       ; Alt-down arrow
	 ("M-<up>"   . scroll-other-window-down)  ; Alt-up arrow
	 ("C-<down>" . scroll-other-window)       ; Ctrl-down arrow
	 ("C-<up>"   . scroll-other-window-down)  ; Ctrl-up arrow
	 )
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; (setq embark-prompter 'embark-completing-read-prompter)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Optionally - use which-key as indicator:

  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
	(which-key--show-keymap
	 (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
	 (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
	 nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; configure indicator:

  ;; use as mixed-indicator as default:
  (setq embark-mixed-indicator-delay 2)

  (setq embark-indicators '(embark-mixed-indicator
			    ;; embark-which-key-indicator ;; put it somewhere if you want use it
			    embark--vertico-indicator
			    embark-highlight-indicator
			    embark-isearch-highlight-indicator))
  ;;
  (setq resize-mini-windows t)
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'cfg-completion-systems)
;;; cfg-completion-systems.el ends here
