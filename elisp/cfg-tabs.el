;;; cfg-tabs.el --- tabs in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for various options for tabs.

;;; Code:


;; tab-bar : since emacs 27 version
(tab-bar-mode 1)

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
            (buffer-name (tabbar-tab-value tab))))
  )

(defun px-tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-3) (with-current-buffer buffer (kill-buffer)))
     (t (switch-to-buffer buffer)))
    (tabbar-buffer-show-groups nil))
  )

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
                                (tabbar-current-tabset))))))))
  )

;; tabbar legacy
;; Introduction:
;; https://emacs.stackexchange.com/questions/10081/browser-style-tabs-for-emacs
;; https://stackoverflow.com/questions/3811126/do-you-use-emacs-tabbar
;; https://gist.github.com/3demax/1264635

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

(provide 'cfg-tabs)
;;; cfg-tabs.el ends here