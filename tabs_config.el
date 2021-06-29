(tab-bar-mode 1)

(use-package tabbar
  :ensure t
  :init
  (tabbar-mode 1)
;;  (add-hook 'helm-mode-hook 'tabbar-local-mode) ;;probably not working correctly... bugs
  :config
;;  (setq tabbar-separator (quote (1.0)))

(defun px-tabbar-buffer-groups ()
    "Sort tab groups."
    (list (cond 
 ;;   ((or (string-equal "*grep" (substring (buffer-name) 0 5))) "GREP")
    ((or (eq major-mode 'grep-mode)) "GREP")
    ((or (eq major-mode 'helm-grep-mode)) "GREP")
    
    ((or (eq major-mode 'magit-status-mode)) "GIT")
    ((or (eq major-mode 'magit-blame-mode)) "GIT")
    ((or (eq major-mode 'magit-log-mode)) "GIT")
    ((or (eq major-mode 'magit-wip-mode)) "GIT")
    ((or (eq major-mode 'magit-file-mode)) "GIT")
    ((or (eq major-mode 'magit-diff-mode)) "GIT")
    ((or (eq major-mode 'magit-blob-mode)) "GIT")
    ((or (eq major-mode 'magit-refs-mode)) "GIT")
    ((or (eq major-mode 'magit-stash-mode)) "GIT")
    ((or (eq major-mode 'magit-reflog-mode)) "GIT")
    ((or (eq major-mode 'magit-cherry-mode)) "GIT")
    ((or (eq major-mode 'magit-process-mode)) "GIT")
    ((or (eq major-mode 'magit-section-mode)) "GIT")
    ((or (eq major-mode 'magit-stashes-mode)) "GIT")
    ((or (eq major-mode 'magit-repolist-mode)) "GIT")
    ((or (eq major-mode 'magit-revision-mode)) "GIT")

    ((or (eq major-mode 'erc-mode) (string-equal "#" (substring (buffer-name) 0 1))) "IRC")

    ((or (eq major-mode 'pdf-view-mode)) "PDF")

    ((or (eq major-mode 'org-mode)) "ORG")
    ((or (eq major-mode 'shell-mode)) "SHELL")

    ((or (eq major-mode 'term-mode)) "TERM")

    ((or (eq major-mode 'fundamental-mode)) "TXT")
    ((or (string= (file-name-extension (buffer-name)) "txt")) "TXT")

    ((or (eq major-mode 'perl-mode)) "PERL")
    ((or (eq major-mode 'cperl-mode)) "PERL")

    ((or (eq major-mode 'sh-mode)) "SH")
    ((or (string= (file-name-extension (buffer-name)) "sh")) "SH")

    ((or (string= (file-name-extension (buffer-name)) "el")) "ELISP")
    ((or (eq major-mode 'emacs-lisp-mode)) "ELISP")

    ((or (string= (file-name-extension (buffer-name)) "xml")) "XML")
    ((or (string= (file-name-extension (buffer-name)) "gcs")) "XML")

    ((or (eq major-mode 'json-mode)) "JSON")

    ((or (eq major-mode 'dired-mode)) "DIRED")

    ((or (eq major-mode 'help-mode)) "HELP")
    ((or (eq major-mode 'info-mode)) "HELP")

    ((or (eq major-mode 'python-mode)) "PYTHON")

    ((or (eq major-mode 'regex-tool-mode)) "REGEXP")
    ((or (string-equal "*Groups*" (substring (buffer-name) 0 8))) "REGEXP")

    ((or (eq major-mode 'helm-mode) (string-equal "*helm" (substring (buffer-name) 0 5))) "HELM")

    (t "user")
    ))
  )



;; end of use-package....

  (setq tabbar-help-on-tab-function 'px-tabbar-buffer-help-on-tab)
  (setq tabbar-select-tab-function 'px-tabbar-buffer-select-tab)
  (setq tabbar-buffer-groups-function 'px-tabbar-buffer-groups)

)

(global-set-key [M-tab] 'alternate-buffer)

;; tabbar legacy

(global-set-key [header-line mouse-1] 'tabbar-press-home)
(global-set-key [header-line mouse-2] 'tabbar-press-home)
;; (global-set-key [header-line mouse-3] 'tabbar-press-home)
(global-set-key [header-line drag-mouse-9] 'tabbar-press-home)
(global-set-key [header-line mouse-9] 'tabbar-press-home)
(global-set-key [header-line mouse-8] 'tabbar-backward-group)
(global-set-key [header-line drag-mouse-8] 'tabbar-backward-group)

(global-set-key [S-next] 'tabbar-backward)
(global-set-key [S-prior] 'tabbar-forward)


;; tab (emacs 27++)

(global-set-key [mouse-9] 'tab-next)
(global-set-key [drag-mouse-9] 'tab-next)
(global-set-key [mouse-8] 'tab-previous)
(global-set-key [drag-mouse-8] 'tab-previous)

(global-set-key [M-mouse-9] 'tab-new)
(global-set-key [M-drag-mouse-9] 'tab-new)

(global-set-key [M-mouse-8] 'tab-close)
(global-set-key [M-drag-mouse-8] 'tab-close)
(global-set-key [M-mouse-2] 'tab-close)
(global-set-key [M-drag-mouse-2] 'tab-close)
