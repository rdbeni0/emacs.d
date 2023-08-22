;;; cfg-tabs.el --- tabs in Emacs -*- lexical-binding: t -*-
;;; Commentary:

;; Configuration for various options for tabs.

;;; Code:

;; tab-bar : since emacs 27 version
(tab-bar-mode 1)

;; tabbar legacy plugin
;; Introduction:
;; https://emacs.stackexchange.com/questions/10081/browser-style-tabs-for-emacs
;; https://stackoverflow.com/questions/3811126/do-you-use-emacs-tabbar
;; https://gist.github.com/3demax/1264635

(defun cfg/px-tabbar-buffer-help-on-tab (tab)
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

(defun cfg/px-tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-3) (with-current-buffer buffer (kill-buffer)))
     (t (switch-to-buffer buffer)))
    (tabbar-buffer-show-groups nil)))

(defun cfg/tabbar-buffer-tab-label (tab)
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
(use-package tabbar
  :ensure t
  :config
  ;; (tabbar-mode 1)
  ;;  (setq tabbar-separator (quote (1.0)))

  (defun cfg/px-tabbar-buffer-groups ()
    "Sort tab groups."
    (list (cond
	   ((or (eq major-mode 'grep-mode)) "GREP")

	   ((or (eq major-mode 'magit-mode)) "GIT")
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

	   ((or (eq major-mode 'conf-mode)) "CONF")
	   ((or (eq major-mode 'conf-unix-mode)) "CONF")
	   ((or (eq major-mode 'conf-space-mode)) "CONF")
	   ((or (eq major-mode 'ssh-config-mode)) "CONF")
	   ((or (eq major-mode 'conf-windows-mode)) "CONF")

	   ((or (eq major-mode 'org-mode)) "ORG")

	   ((or (eq major-mode 'shell-mode)) "TERM")
	   ((or (eq major-mode 'term-mode)) "TERM")
	   ((or (eq major-mode 'vterm-mode)) "TERM")

	   ((or (eq major-mode 'fundamental-mode)) "TXT")
	   ((or (string= (file-name-extension (buffer-name)) "txt")) "TXT")

	   ((or (eq major-mode 'web-mode)) "WEB")
	   ((or (eq major-mode 'mhtml-mode)) "WEB")
	   ((or (eq major-mode 'html-mode)) "WEB")
	   ((or (eq major-mode 'css-mode)) "WEB")
	   ((or (eq major-mode 'js-mode)) "WEB")
	   ((or (eq major-mode 'json-mode)) "WEB")
	   ((or (eq major-mode 'json-navigator-mode)) "WEB")

	   ((or (eq major-mode 'php-mode)) "PHP")
	   ((or (string= (file-name-extension (buffer-name)) "php")) "PHP")

	   ((or (eq major-mode 'cc-mode)) "C")
	   ((or (eq major-mode 'c++-mode)) "C")
	   ((or (eq major-mode 'makefile-gmake-mode)) "C")
	   ((or (eq major-mode 'c-mode)) "C")
	   ((or (string= (file-name-extension (buffer-name)) "c")) "C")
	   ((or (string-equal "*compilation*" (buffer-name))) "C")

	   ((or (eq major-mode 'perl-mode)) "PERL")
	   ((or (eq major-mode 'cperl-mode)) "PERL")

	   ((or (eq major-mode 'sh-mode)) "SH")
	   ((or (string= (file-name-extension (buffer-name)) "sh")) "SH")

	   ((or (string= (file-name-extension (buffer-name)) "el")) "ELISP")
	   ((or (eq major-mode 'emacs-lisp-mode)) "ELISP")

	   ((or (string= (file-name-extension (buffer-name)) "md")) "MARKDOWN")
	   ((or (eq major-mode 'gfm-mode)) "MARKDOWN")
	   ((or (eq major-mode 'markdown-mode)) "MARKDOWN")

	   ((or (eq major-mode 'snippet-mode)) "SNIPPETS")

	   ((or (string= (file-name-extension (buffer-name)) "xml")) "XML")
	   ((or (eq major-mode 'nxml-mode)) "XML")
	   ((or (string-equal "*XSD Validator*" (buffer-name))) "XML")
	   ((or (string= (file-name-extension (buffer-name)) "gcs")) "XML")

	   ((or (eq major-mode 'dired-mode)) "DIRED")

	   ((or (eq major-mode 'recentf-dialog-mode)) "RECENTF-BKM")
	   ((or (eq major-mode 'bookmark-bmenu-mode)) "RECENTF-BKM")

	   ((or (eq major-mode 'ediff-meta-mode)) "DIFF")
	   ((or (eq major-mode 'diff-mode)) "DIFF")

	   ((or (eq major-mode 'image-mode)) "IMAGE")

	   ((or (eq major-mode 'help-mode)) "HELP")
	   ((or (eq major-mode 'info-mode)) "HELP")

  	   ((or (eq major-mode 'notmuch-search-mode)) "NOTMUCH")
  	   ((or (eq major-mode 'notmuch-hello-mode)) "NOTMUCH")
  	   ((or (eq major-mode 'notmuch-show-mode)) "NOTMUCH")
  	   ((or (eq major-mode 'notmuch-tree-mode)) "NOTMUCH")
  	   ((or (eq major-mode 'notmuch-message-mode)) "NOTMUCH")

	   ((or (eq major-mode 'ibuffer-mode)) "BUFFERS")
	   ((or (eq major-mode 'Buffer-menu-mode)) "BUFFERS")

	   ((or (eq major-mode 'python-mode)) "PYTHON")
	   ((or (eq major-mode 'inferior-python-mode)) "PYTHON")

	   ((or (eq major-mode 'groovy-mode)) "GROOVY")
	   ((or (eq major-mode 'jenkinsfile-mode)) "GROOVY")
	   ((or (string-equal "*groovy*" (buffer-name))) "GROOVY")

	   ((or (eq major-mode 'regex-tool-mode)) "REGEXP")
	   ((or (string-equal "*Groups*" (substring (buffer-name) 0 8))) "REGEXP")

	   (t "user"))))

  ;; keys
  ;; TODO : migrate to general.el

  (setq tabbar-help-on-tab-function 'cfg/px-tabbar-buffer-help-on-tab)
  (setq tabbar-select-tab-function 'cfg/px-tabbar-buffer-select-tab)
  (setq tabbar-buffer-groups-function 'cfg/px-tabbar-buffer-groups)
  (global-set-key [header-line mouse-1] 'tabbar-press-home)
  (global-set-key [header-line mouse-2] 'tabbar-press-home)
  (global-set-key [header-line mouse-3] 'tabbar-press-home)

  ;; end of use-package....
  )


(provide 'cfg-tabs)
;;; cfg-tabs.el ends here
