;;; cfg-general-which-keys.el --- general.el, which-key and all keys -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for general.el, which-key mode and all keybindings

;;; Code:

;; which-key

(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-idle-delay 0.0)
  :config
  (which-key-mode)
  )

;; evil-ex-define-cmd
;; unfortunately it seems that binding : vim commands needs to be done via dedicated function, but not via "general.el":
;; https://stackoverflow.com/questions/12913713/rebinding-b-and-e-in-evil-normal-state-map

(evil-ex-define-cmd "e[dit]" 'helm-find-files)
(evil-ex-define-cmd "b[uffers]" 'ibuffer)
(evil-ex-define-cmd "E[x]" 'dired-jump)


;; general

;; https://github.com/noctuid/general.el/issues/99
;; general-override-mode
;; :keymaps 'override
;; ^override evil keybindings

(use-package general
  :demand t
  :ensure t
  :config
  (general-override-mode 1)

  ;; general: no space + no which-key

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'global

   "<M-tab>" '(alternate-buffer :wk t)
   "<C-mouse-4>" '(text-scale-increase :wk t)
   "<C-mouse-5>" '(text-scale-decrease :wk t)

   ;; tabbar legacy plugin

   "<S-next>" '(tabbar-backward :wk t)
   "<S-prior>" '(tabbar-forward :wk t)
   ;;   "<header-line> <mouse-1>" '(tabbar-press-home :wk t)
   ;;   "<header-line> <mouse-2>" '(tabbar-press-home :wk t)
   ;;   "<header-line> <mouse-3>" '(tabbar-press-home :wk t)
   "<header-line> <mouse-9>" '(tabbar-forward-group :wk t)
   "<header-line> <drag-mouse-9>" '(tabbar-forward-group :wk t)
   "<header-line> <mouse-8>" '(tabbar-backward-group :wk t)
   "<header-line> <drag-mouse-8>" '(tabbar-backward-group :wk t)

   ;; tab (emacs 27++)

   "<mouse-9>" '(tab-next :wk t)
   "<drag-mouse-9>" '(tab-next :wk t)
   "<mouse-8>" '(tab-previous :wk t)
   "<drag-mouse-8>" '(tab-previous :wk t)
   "<M-mouse-9>" '(tab-new :wk t)
   "<M-drag-mouse-9>" '(tab-new :wk t)
   "<M-mouse-8>" '(tab-close :wk t)
   "<M-drag-mouse-8>" '(tab-close :wk t)
   "<M-mouse-2>" '(tab-close :wk t)
   "<M-drag-mouse-2>" '(tab-close :wk t)
   )

  ;; global: space as leader-key + which-key

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC"
   "TAB" '(alternate-buffer :which-key "alternate-buffer")
   "<mouse-1>" '(tabbar-mode :which-key "tbb-mode")
   "<mouse-3>" '(tabbar-press-home :which-key "tbb-home")
   "<mouse-2>" '(tabbar-backward-group :which-key "tbb-backward")

   ;; tabs and mouse commands

   "<up>" '(tab-rename :wk t)
   "<down>" '(tab-new :wk t)
   "<left>" '(tab-previous :wk t)
   "<right>" '(tab-next :wk t)
   "<deletechar>" '(tab-close :wk t)
   "<next>" '(tabbar-backward :wk t)
   "<prior>" '(tabbar-forward :wk t)

   ;;

   "SPC" '(helm-M-x :which-key "M-x")
   "?" '(helm-descbinds :which-key "descbinds")
   "!" '(shell-command :which-key "shell cmd")
   ;;     "0" '(neotree-toggle :which-key "neotree-toggle")

   "."   '(:ignore t :which-key "webpaste")
   ".k"   '(webpaste-paste-buffer :which-key "webpaste-paste-buffer")
   ".l"   '(webpaste-paste-region :which-key "webpaste-paste-region")

   ;; treemacs main

   "0"   '(:ignore t :which-key "treemacs")
   "00"  '(treemacs :which-key "treemacs")
   "0w"  '(:ignore t :which-key "treemacs-workspaces")
   "0ww"  '(treemacs-switch-workspace :which-key "switch-workspace")
   "0wa"  '(treemacs-create-workspace :which-key "create-workspace")
   "0wd"  '(treemacs-remove-workspace :which-key "remove-workspace")
   "0wr"  '(treemacs-rename-workspace :which-key "rename-workspace")
   "0wf"  '(treemacs-set-fallback-workspace :which-key "set-fallback-workspace")
   "0we"  '(treemacs-edit-workspaces :which-key "edit-workspaces")
   "0wn"  '(treemacs-next-workspaces :which-key "next-workspace")
   "0p"  '(:ignore t :which-key "treemacs-projects")
   "0pa" '(treemacs-add-project :which-key "add-project")
   "0pA" '(treemacs-add-project-to-workspace :which-key "add-project-to-workspace")
   "0pd" '(treemacs-remove-project-from-workspace :which-key "remove-project")
   "0pr" '(treemacs-rename-project :which-key "rename-project")
   "0pp" '(treemacs-projectile :which-key "projectile")

   ;; files

   "f"   '(:ignore t :which-key "files")
   "ff"  '(helm-find-files :which-key "find files")
   "fz"  '(fzf :which-key "fzf")
   "fR"  '(rename-current-buffer-file :which-key "rename-current-buffer-file")
   "fE"  '(sudo-edit :which-key "sudo-edit")
   "fv"  '(revert-buffer :which-key "revert-buffer")
   "fV"  '(auto-revert-mode :which-key "auto-revert-mode")
   "fr"  '(helm-recentf :which-key "helm-recentf")
   "fp"  '(show-file-name :which-key "show-file-name")
   "fs"  '(save-buffer :which-key "save buffer")
   "fo"  '(save-some-buffers :which-key "save-some-buffers")
   "fb"  '(helm-bookmarks :which-key "bookmarks")
   "fD"  '(delete-current-buffer-file :which-key "delete-current-buffer-file")

   ;; others

   "o"    '(:ignore t :which-key "others")

   "om"   '(:ignore t :which-key "modes")
   "omt"  '(text-mode :which-key "text-mode")
   "oma"  '(auto-revert-mode :which-key "auto-revert-mode")
   "omo"  '(org-mode :which-key "org-mode")
   "omc"  '(conf-mode :which-key "conf-mode")
   "omk"  '(calculator :which-key "calculator")
   "ome"  '(emacs-lisp-mode :which-key "emacs-lisp")

   ;; buffers

   "b"   '(:ignore t :which-key "buffers")
   "bb"  '(helm-buffers-list :which-key "buffers list")
   "bF"  '(format-all-buffer :which-key "format-all-buffer")
   "bm"  '(kill-other-buffers :which-key "kill-other-buffers")
   "bv"  '(ibuffer :which-key "ibuffer")
   "bt"  '(eval-buffer :which-key "eval-buffer")
   "bd"  '(kill-this-buffer :which-key "kill-this-buffer")
   "bN"  '(new-empty-buffer :which-key "new-empty-buffer")
   "bx"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")

   ;; windows

   "w"   '(:ignore t :which-key "windows")
   "wt"   '(:ignore t :which-key "tabs")
   "wt <deletechar>"   '(tab-close :which-key "tab-close")
   "wt <up>" '(tab-rename :which-key "tab-rename")
   "wt <down>" '(tab-new :which-key "tab-new")
   "wt <left>" '(tab-previous :which-key "tab-previous")
   "wt <right>" '(tab-next :which-key "tab-next")
   "wt <next>" '(tabbar-backward :which-key "tabbar-backward")
   "wt <prior>" '(tabbar-forward :which-key "tabbar-forward")
   "ww"  '(other-window :which-key "other-window")
   "we"  '(delete-other-windows :which-key "delete-other-windows")
   "ws"  '(split-window-below :which-key "split-window-below")
   "wo"  '(scroll-bar-mode :which-key "scroll-bar-mode")
   "wv"  '(split-window-right :which-key "split-window-right")
   "wm"  '(toggle-maximize-buffer :which-key "maximize-buffer")
   "wl"  '(windmove-right :which-key "right")
   "wh"  '(windmove-left :which-key "left")
   "wk"  '(windmove-up :which-key "up")
   "wj"  '(windmove-down :which-key "down")
   "wq"  '(evil-quit :which-key "evil-quit")
   "w <left>"  '(windmove-left :which-key "left")
   "w <right>"  '(windmove-right :which-key "right")
   "w <down>"  '(windmove-down :which-key "down")
   "w <up>"  '(windmove-up :which-key "up")
   "w/"  '(split-window-right :which-key "split right")
   "w-"  '(split-window-below :which-key "split bottom")
   "wx"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")

   ;; apps

   "a"    '(:ignore t :which-key "apps")
   "ae"   '(:ignore t :which-key "erc/irc")
   "ac"   '(calc-dispatch :which-key "calc-dispatch")
   "aq"   '(quick-calc :which-key "quick-calc")
   "as"   '(:ignore t :which-key "shells")
   "ar"   '(:ignore t :which-key "regexp")
   "asm"  '(multi-term-buffer-rn :which-key "multi-term")
   "asa"  '(ansi-term :which-key "ansi-term")
   "ass"  '(shell :which-key "shell")
   "asS"  '(my-named-shell :which-key "my-named-shell")
   "ast"  '(tramp-term :which-key "tramp-term")
   "aeD"  '(erc-default-servers :which-key "erc-default-servers")
   "aeE"  '(erc-tls :which-key "erc-tls")
   "aer"  '(rcirc :which-key "rcirc")
   "aee"  '(erc :which-key "erc")
   "ael"  '(:ignore t :which-key "erc-view-log-mode")
   "aelf" '(erc-find-logfile :which-key "erc-find-logfile")
   "aelv" '(erc-view-log-mode :which-key "erc-view-log-mode")
   "aelr" '(erc-view-log-reload-file :which-key "erc-view-log-reload-file")
   "ael>" '(erc-view-log-next-mention :which-key "erc-view-log-next-mention")
   "ael<" '(erc-view-log-previous-mention :which-key "erc-view-log-previous-mention")

   "arr"  '(regex-tool :which-key "regex-tool")
   "arq"  '(regex-tool-quit :which-key "regex-tool-quit")

   "ad"   '(:ignore t :which-key "dired")
   "adw"  '(wdired-change-to-wdired-mode :which-key "wdired-mode")
   "adW"  '(wdired-exit :which-key "wdired-exit")
   "add"  '(dired :which-key "dired")
   "adD"  '(sudired :which-key "sudired")

   ;; search

   "s"   '(:ignore t :which-key "search")
   "sj"  '(my//helm-jump-in-buffer :which-key "helm-jump-in-buffer")
   "si"  '(find-name-dired :which-key "find-name-dired")
   "sd"  '(find-grep-dired :which-key "find-grep-dired")
   "sz"  '(zrgrep :which-key "zrgrep")
   "sr"  '(rgrep :which-key "rgrep")
   "sp"  '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
   "sg"  '(:ignore t :which-key "helm-grep")
   "sgi" '(helm-grep-do-git-grep :which-key "helm-grep-do-git-grep")
   "sgb" '(my//helm-buffers-do-grep :which-key "helm-buffers-do-grep")
   "sgB" '(my//helm-buffers-do-grep-region-or-symbol :which-key "helm-buffers-do-grep-reg-or-sym")
   "sgf" '(my//helm-files-do-grep :which-key "helm-files-do-grep")
   "sgF" '(my//helm-files-do-grep-region-or-symbol :which-key "helm-files-do-grep-reg-or-sym")
   "sgg" '(my//helm-file-do-grep :which-key "helm-file-do-grep")
   "sgG" '(my//helm-file-do-grep-region-or-symbol :which-key "helm-file-do-grep-reg-or-sym")

   ;; help

   "h"   '(:ignore t :which-key "help")
   "hi"  '(info :which-key "info")
   "hd"  '(:ignore t :which-key "describe")
   "hdf" '(describe-function :which-key "describe-function")
   "hdo" '(describe-font :which-key "describe-font")
   "hdm" '(describe-mode :which-key "describe-mode")
   "hda" '(helm-apropos :which-key "helm-apropos")
   "hdt" '(describe-theme :which-key "describe-theme")
   "hdv" '(describe-variable :which-key "describe-variable")
   "hdp" '(describe-package :which-key "describe-package")
   "hdc" '(describe-char :which-key "describe-char")

   ;; quit

   "q"   '(:ignore t :which-key "quit")
   "qQ"  '(kill-emacs :which-key "kill-emacs")

   ;; diff

   "D"   '(:ignore t :which-key "diff")
   "Db"  '(vdiff-buffers :which-key "vdiff-buffers")
   "D3"  '(vdiff-buffers3 :which-key "vdiff-buffers3")
   "Df"  '(vdiff-files :which-key "vdiff-files")
   "Di"  '(vdiff-files3 :which-key "vdiff-files3")
   "Dm"  '(vdiff-mode :which-key "vdiff-mode")
   "Dv"  '(vdiff-hydra/body :which-key "vdiff-hydra")
   "Dw"  '(vdiff-save-buffers :which-key "vdiff-save-buffers")
   "Du"  '(vdiff-refresh :which-key "vdiff-refresh")
   "Dq"  '(vdiff-quit :which-key "vdiff-quit")

   ;; projects, projectile

   "p"   '(:ignore t :which-key "projects")
   "p;"  '(:ignore t :which-key "search/grep")
   "p;;" '(helm-projectile-grep :which-key "grep")
   "p;a" '(helm-projectile-ack :which-key "ack")
   "p;g" '(helm-projectile-ag :which-key "ag")
   "p;r" '(helm-projectile-rg :which-key "rg")
   "p!" '(projectile-run-shell-command-in-root :which-key "run-shell-command-in-root")
   "p&" '(projectile-run-async-shell-command-in-root :which-key "run-async-shell-command-in-root")
   "p%" '(projectile-replace-regexp :which-key "replace-regexp")
   "pa" '(projectile-toggle-between-implementation-and-test :which-key "toggle-between-implementation-and-test")
   "pA" '(projectile-reset-cached-project-root :which-key "reset-cached-project-root")
   "pb" '(helm-projectile-switch-to-buffer :which-key "switch-to-buffer")
   "pc" '(projectile-compile-project :which-key "compile-project")
   "pd" '(helm-projectile-find-dir :which-key "find-dir")
   "pD" '(projectile-dired :which-key "dired")
   "pe" '(projectile-edit-dir-locals :which-key "edit-dir-locals")
   "pf" '(helm-projectile-find-file :which-key "find-file")
   "pF" '(helm-projectile-find-file-dwim :which-key "find-file-dwim")
   "pg" '(projectile-find-tag :which-key "find-tag")
   "pG" '(projectile-regenerate-tags :which-key "regenerate-tags")
   "ph" '(helm-projectile :which-key "helm-projectile")
   "pI" '(projectile-invalidate-cache :which-key "invalidate-cache")
   "pk" '(projectile-kill-buffers :which-key "kill-buffers")
   "pK" '(projectile-add-known-project :which-key "add-known-project")
   "pp" '(helm-projectile-switch-project :which-key "switch-project")
   "pr" '(helm-projectile-recentf :which-key "recentf")
   "pR" '(projectile-replace :which-key "replace")
   "pM" '(projectile-remove-known-project :which-key "remove-known-project")
   "pT" '(projectile-test-project :which-key "test-project")
   "pv" '(projectile-vc :which-key "vc")

   ;; git

   "g"   '(:ignore t :which-key "git")
   "g*"  '(helm-git-grep-at-point :which-key "helm-git-grep-at-point")
   "g/"  '(helm-git-grep :which-key "helm-git-grep")
   "gb"  '(magit-blame :which-key "magit-blame")
   "gc"  '(magit-clone :which-key "magit-clone")
   "gi"  '(magit-init :which-key "magit-init")
   "gs"  '(magit-status :which-key "magit-status")
   "gm"  '(magit-dispatch :which-key "magit-dispatch")
   "gL"  '(magit-list-repositories :which-key "magit-list-repositories")
   "gS"  '(magit-stage-file :which-key "magit-stage-file")
   "gU"  '(magit-unstage-file :which-key "magit-unstage-file")
   "gr"  '(magit-refresh :which-key "magit-refresh")
   "gd"  '(magit-diff :which-key "magit-diff")
   "gf"  '(magit-find-file :which-key "magit-find-file")

   )

  ;; LSP mode : TODO

  ;; https://github.com/emacs-lsp/lsp-mode/issues/1530
  ;; ^ evil modes and lsp-mode prefix-key on Gitter
  ;; So in that case it should be mapped as: *evil-defined-key*

  (general-define-key
   :prefix "\\"
   :states '(normal visual emacs)
   :keymaps 'override
   "=" '(:ignore t :which-key "formatting")
   "F" '(:ignore t :which-key "Folders")
   "G" '(:ignore t :which-key "peek")
   "T" '(:ignore t :which-key "Toggle")
   "a" '(:ignore t :which-key "code actions")
   "g" '(:ignore t :which-key "goto")
   "h" '(:ignore t :which-key "help")
   "r" '(:ignore t :which-key "refactor")
   "s" '(:ignore t :which-key "sessions")

   "\\" '(:ignore t :which-key "eglot")
   "\\\\" '(eglot :which-key "eglot")
   "\\/" '(eglot-rename :which-key "rename")
   "\\b" '(eglot-format-buffer :which-key "format-buffer")
   "\\p" '(eglot-help-at-point :which-key "help-at-point")
   "\\o" '(eglot-code-actions :which-key "code-actions")
   "\\c" '(eglot-clear-status :which-key "clear-status")
   "\\=" '(eglot-format :which-key "format")
   "\\w" '(eglot-shutdown :which-key "shutdown")
   "\\r" '(eglot-reconnect :which-key "reconnect")
   "\\e" '(eglot-stderr-buffer :which-key "stderr-buffer")
   "\\t" '(eglot-events-buffer :which-key "events-buffer")
   "\\l" '(eglot-find-declaration :which-key "find-declaration")
   "\\n" '(eglot-find-typeDefinition :which-key "find-typeDefinition")
   "\\m" '(eglot--managed-mode :which-key "managed-mode")
   "\\s" '(eglot-forget-pending-continuations :which-key "forget-pending-continuations")
   "\\i" '(eglot-find-implementation :which-key "find-implementation")
   "\\a" '(eglot-signal-didChangeConfiguration :which-key "didChangeConfiguration")
   )

  ;; org mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(org-mode-map org-static-blog-mode-map)
   :major-modes '(org-mode org-static-blog-mode)
   :prefix ","
   "p" '(org-priority :which-key "org-priority")
   "j"  '(my//helm-jump-in-buffer :which-key "helm-jump-in-buffer")
   "," '(org-ctrl-c-ctrl-c :which-key "org-ctrl-c-ctrl-c")
   "A" '(org-attach :which-key "org-attach")

   "e" '(:ignore t :which-key "export")
   "ed" '(org-md-export-to-markdown :which-key "export-to-md")

   "i" '(:ignore t :which-key "insert")
   "ib" '(org-insert-structure-template :which-key "org-insert-structure-template")
   "id" '(org-insert-drawer :which-key "org-insert-drawer")
   "ie" '(org-set-effort :which-key "org-set-effort")
   "if" '(org-footnote-new :which-key "org-footnote-new")
   "ih" '(org-insert-heading :which-key "org-insert-heading")
   "iH" '(org-insert-heading-after-current :which-key "org-insert-heading-after-current")
   "ii" '(org-insert-item :which-key "org-insert-item")
   "il" '(org-insert-link :which-key "org-insert-link")
   "in" '(org-add-note :which-key "org-add-note")
   "ip" '(org-set-property :which-key "org-set-property")
   "is" '(org-insert-subheading :which-key "org-insert-subheading")
   "it" '(org-set-tags-command :which-key "org-set-tags-command")

   "l"   '(:ignore t :which-key "org-static-blog")
   "ld"  '(org-static-blog-mode :which-key "osb-mode")
   "lh"  '(org-static-blog-publish :which-key "osb-publish")
   "ll"  '(org-static-blog-publish-file :which-key "osb-publish-file")
   "la"  '(org-static-blog-open-matching-publish-file :which-key "osb-open-matching-pub-file")
   "ln"  '(org-static-blog-open-next-post :which-key "osb-open-next-post")
   "lm"  '(org-static-blog-open-previous-post :which-key "osb-open-prev-post")
   "lo"  '(org-static-blog-create-new-post :which-key "osb-create-new-post")
   "lj"  '(org-static-blog-create-new-draft :which-key "osb-create-new-draft")

   "b" '(:ignore t :which-key "babel")
   "bt" '(org-babel-tangle :which-key "org-babel-tangle")
   )

  ;; json-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'json-mode-map
   :major-modes 'json-mode
   :prefix ","
   "p" '(json-pretty-print-buffer :which-key "pretty-print-buffer")
   "b" '(json-mode-beautify :which-key "jm-beautify")
   )

  ;; doc-view mode ; with prefix

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'doc-view-mode-map
   :major-modes 'doc-view-mode
   :prefix ","
   "," '(doc-view-goto-page :which-key "goto-page")
   "<home>"'(doc-view-first-page :which-key "first-page")
   "<end>" '(doc-view-last-page :which-key "last-page")
   "k" '(doc-view-shrink :which-key "shrink")
   "l" '(doc-view-enlarge :which-key "enlarge")
   "0" '(doc-view-scale-reset :which-key "scale-reset")
   "P" '(doc-view-fit-page-to-window :which-key "fit-page-to-window")
   "W" '(doc-view-fit-width-to-window :which-key "fit-width-to-window")
   "H" '(doc-view-fit-height-to-window :which-key "fit-height-to-window")
   "/" '(doc-view-search :which-key "doc-view-search")
   "s" '(:ignore t :which-key "slice")
   "sr" '(doc-view-reset-slice :which-key "reset-slice")
   "ss" '(doc-view-set-slice-using-mouse :which-key "set-slice-using-mouse")
   "sb" '(doc-view-set-slice-from-bounding-box :which-key "set-slice-from-bounding-box")
   "sl" '(doc-view-set-slice :which-key "set-slice")
   )

  ;; doc-view mode ; without prefix

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'doc-view-mode-map
   :major-modes 'doc-view-mode
   "C-<mouse-4>" '(doc-view-enlarge :wk t)
   "C-<mouse-5>"'(doc-view-shrink :wk t)
   "<right>" '(doc-view-next-page :wk t)
   "<left>" '(doc-view-previous-page :wk t)
   "<home>" '(doc-view-first-page :wk t)
   "<end>" '(doc-view-last-page :wk t)
   "k" '(doc-view-shrink :wk t)
   "l" '(doc-view-enlarge :wk t)
   "/" '(doc-view-search :wk t)
   "W" '(doc-view-fit-width-to-window :wk t)
   "H" '(doc-view-fit-height-to-window :wk t)
   "P" '(doc-view-fit-page-to-window :wk t)
   "0" '(doc-view-scale-reset :wk t)
   )

  ;; pdf-view mode ; with prefix

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'pdf-view-mode-map
   :major-modes 'pdf-view-mode
   :prefix ","
   "," '(pdf-view-goto-page :which-key "goto-page")
   "<home>"'(pdf-view-first-page :which-key "first-page")
   "<end>" '(pdf-view-last-page :which-key "last-page")
   "k" '(pdf-view-shrink :which-key "shrink")
   "l" '(pdf-view-enlarge :which-key "enlarge")
   "0" '(pdf-view-scale-reset :which-key "scale-reset")
   "P" '(pdf-view-fit-page-to-window :which-key "fit-page-to-window")
   "W" '(pdf-view-fit-width-to-window :which-key "fit-width-to-window")
   "H" '(pdf-view-fit-height-to-window :which-key "fit-height-to-window")
   "/" '(isearch-forward :which-key "isearch-forward")
   "s" '(:ignore t :which-key "slice")
   "sr" '(pdf-view-reset-slice :which-key "reset-slice")
   "ss" '(pdf-view-set-slice-using-mouse :which-key "set-slice-using-mouse")
   "sb" '(pdf-view-set-slice-from-bounding-box :which-key "set-slice-from-bounding-box")
   "sl" '(pdf-view-set-slice :which-key "set-slice")
   )

  ;; pdf-view mode ; without prefix

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'pdf-view-mode-map
   :major-modes 'pdf-view-mode
   "C-<mouse-4>"'(pdf-view-enlarge :wk t)
   "C-<mouse-5>"'(pdf-view-shrink :wk t)
   "<right>"'(pdf-view-next-page :wk t)
   "<left>"'(pdf-view-previous-page :wk t)
   "<home>" '(pdf-view-first-page :wk t)
   "<end>" '(pdf-view-last-page :wk t)
   "k" '(pdf-view-shrink :wk t)
   "l" '(pdf-view-enlarge :wk t)
   "0" '(pdf-view-scale-reset :wk t)
   "P" '(pdf-view-fit-page-to-window :wk t)
   "W" '(pdf-view-fit-width-to-window :wk t)
   "H" '(pdf-view-fit-height-to-window :wk t)
   "/" '(isearch-forward :wk t)
   )

  ;; cperl-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(perl-mode-map cperl-mode-map)
   :major-modes '(perl-mode cperl-mode)
   :prefix ","
   ","  '(cperl-perldoc-at-point :which-key "perldoc-at-point")
   "o"  '(cperl-perldoc :which-key "cperl-perldoc")
   "t"  '(:ignore t :which-key "toggle")
   "te" '(cperl-toggle-electric :which-key "toggle-electric")
   "tf" '(flycheck-mode :which-key "toggle flycheck")
   "d"  '(cperl-db :which-key "debugger")
   "q"  '(:ignore t :which-key "quotes")
   "qi" '(perl-quote-single :which-key "quote-single")
   "qo" '(perl-quote-double :which-key "quote-double")
   "m"  '(:ignore t :which-key "modules")
   "mm" '(find-perl-module :which-key "find-perl-module")
   "mn" '(ffap :which-key "ffap")
   "n" '(mark-defun :which-key "mark-sub")
   "="  '(:ignore t :which-key "format")
   "==" '(perltidy-format-buffer :which-key "perltidy-format-buffer")
   "=b" '(perltidy-format-buffer :which-key "perltidy-format-buffer")
   "=o" '(perltidy-format :which-key "perltidy-format")
   "=f" '(perltidy-format-function :which-key "perltidy-format-function")
   )

  ;; erc-mode
  ;; https://www.emacswiki.org/emacs/ErcBindings

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'erc-mode-map
   :major-modes 'erc-mode
   :prefix ","
   "b" '(erc-iswitchb :which-key "iswitchb")
   "a" '(erc-input-action :which-key "input-action")
   "c" '(erc-chanlist :which-key "chanlist")
   "j" '(erc-join-channel :which-key "join-channel")
   "n" '(erc-channel-names :which-key "channel-names")
   "Q" '(erc-quit-server :which-key "quit-server")
   "f" '(erc-find-logfile :which-key "find-logfile")
   "o" '(erc-status-sidebar-open :which-key "status-sidebar-open")
   "i" '(erc-status-sidebar-kill :which-key "status-sidebar-kill")
   "." '(quoted-insert :which-key "quoted-insert")
   "," '(erc-cmd-QUERY :which-key "cmd-QUERY-private-msg")
   "g" '(erc-image-mode :which-key "toggle-image-mode")
   )

  ;; emacs-lisp-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'emacs-lisp-mode-map
   :major-modes 'emacs-lisp-mode
   :prefix ","
   "c"   '(emacs-lisp-native-compile-and-load :which-key "compile-and-load")
   "e"   '(eval-region :which-key "eval-region")
   "E"   '(eval-buffer :which-key "eval-buffer")
   "="  '(:ignore t :which-key "format")
   "=="   '(format-all-buffer :which-key "format-all-buffer")
   "=b"   '(format-all-buffer :which-key "format-all-buffer")
   "=o"   '(format-all-region :which-key "format-all-region")
   ","  '(ffap :which-key "ffap")
   )

  ;; dired-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'dired-mode-map
   :major-modes 'dired-mode
   "^" (lambda () (interactive) (find-alternate-file ".."))
   "<RET>" (lambda () (interactive) (dired-find-alternate-file))
   )

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'dired-mode-map
   :major-modes 'dired-mode
   :prefix ","
   ","  '(dired-narrow :which-key "dired-narrow")
   "l"  '(cycle-dired-switches :which-key "cycle-dired-switches")
   "d"  '(:ignore t :which-key "dired-du")
   "dc" '(dired-du-count-sizes :which-key "count-sizes")
   "dr" '(dired-du-recompute-dir-size :which-key "recompute-dir-size")
   "dd" '(dired-du-mode :which-key "dired-du-mode")
   "dh" '(dired-du--toggle-human-readable :which-key "toggle-human-readable")
   )

  ;; treemacs-mode

  (general-define-key
   :states '(treemacs normal visual emacs)
   :keymaps 'treemacs-mode-map
   :major-modes 'treemacs-mode
   "0"  '(treemacs :which-key "treemacs")
   )

  (general-define-key
   :states '(treemacs normal visual emacs)
   :keymaps 'treemacs-mode-map
   :major-modes 'treemacs-mode
   :prefix ","
   "0"  '(treemacs :which-key "treemacs")
   "w"  '(:ignore t :which-key "treemacs-workspaces")
   "ww"  '(treemacs-switch-workspace :which-key "switch-workspace")
   "wa"  '(treemacs-create-workspace :which-key "create-workspace")
   "wd"  '(treemacs-remove-workspace :which-key "remove-workspace")
   "wr"  '(treemacs-rename-workspace :which-key "rename-workspace")
   "wf"  '(treemacs-set-fallback-workspace :which-key "set-fallback-workspace")
   "we"  '(treemacs-edit-workspaces :which-key "edit-workspaces")
   "wn"  '(treemacs-next-workspaces :which-key "next-workspace")
   "p"  '(:ignore t :which-key "treemacs-projects")
   "pa" '(treemacs-add-project :which-key "add-project-to-workspace")
   "pA" '(treemacs-add-project-to-workspace :which-key "add-project")
   "pd" '(treemacs-remove-project-from-workspace :which-key "remove-project")
   "pr" '(treemacs-rename-project :which-key "rename-project")
   "pp" '(treemacs-projectile :which-key "projectile")
   )

  ;; term-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(term-mode-map term-raw-map)
   :major-modes 'term-mode
   :prefix ","
   "f" '(find-file :which-key "find-file")
   )

  (general-define-key
   :states '(normal visual emacs insert)
   :keymaps '(term-mode-map term-raw-map)
   :major-modes 'term-mode
   "S-<up>" 'rename-buffer
   "S-<down>" 'multi-term-buffer-rn
   "S-<left>" 'multi-term-prev
   "S-<right>" 'multi-term-next
   "<delete>" 'term-send-del
   "<backspace>" 'term-send-backspace
   "<home>" 'term-send-home
   "<end>" 'term-send-end
   "<up>" 'term-send-up
   "<down>" 'term-send-down
   "<left>" 'term-send-left
   "<right>" 'term-send-right
   "C-c" 'term-interrupt-subjob
   "C-z" 'term-stop-subjob
   "C-." 'term-send-esc
   "C-," 'term-send-eof
   )

  ;; shell-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(shell-mode-map)
   :major-modes 'shell-mode
   :prefix ","
   "c" '(comint-clear-buffer :which-key "clear")
   "i" '(comint-send-invisible :which-key "send-invisible")
   "f" '(find-file :which-key "find-file")
   )

  ;; shell-script-mode ; sh-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(sh-mode-map)
   :major-modes 'sh-mode
   :prefix ","
   "\\" '(sh-backslash-region :which-key "backslash-region")
   "#" '(sh-set-shell :which-key "set-shell")
   "="  '(:ignore t :which-key "format")
   "=b" '(format-all-buffer :which-key "format-all-buffer")
   "==" '(format-all-buffer :which-key "format-all-buffer")
   "c"  '(:ignore t :which-key "code_templates")
   "ci" '(sh-if :which-key "if")
   "co" '(sh-for :which-key "for")
   "cc" '(sh-case :which-key "case")
   "cw" '(sh-while :which-key "while")
   "cf" '(sh-function :which-key "function")
   "cu" '(sh-until :which-key "until")
   "ce" '(sh-indexed-loop :which-key "indexed-loop")
   "cr" '(sh-repeat :which-key "repeat")
   "cs" '(sh-select :which-key "select")
   "cg" '(sh-while-getopts :which-key "while-getopts")
   )

  ;; grep-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(grep-mode-map wgrep-mode-map helm-grep-mode-map)
   :major-modes '(grep-mode wgrep-mode helm-grep-mode)
   :prefix ","
   "g" '(grep-mode :which-key "grep-mode")
   "p" '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
   "e" '(wgrep-exit :which-key "wg-exit")
   "a" '(wgrep-save-all-buffers :which-key "wg-save-all-buffers")
   "Z" '(wgrep-finish-edit :which-key "wg-finish-edit")
   "Q" '(wgrep-abort-changes :which-key "wg-abort-changes")
   "r" '(wgrep-toggle-readonly-area :which-key "wg-toggle-readonly-area")
   )

  ;; python-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'python-mode-map
   :major-modes 'python-mode
   :prefix ","
   "=" '(yapfify-buffer :which-key "format-buffer")
   "j"  '(my//helm-jump-in-buffer :which-key "helm-jump-in-buffer")
   "n"  '(:ignore t :which-key "navigate")
   "nb" '(python-nav-beginning-of-block :which-key "beginnning-of-block")
   "nn" '(python-nav-end-of-block :which-key "end-of-block")
   "ne" '(python-nav-beginning-of-statement :which-key "beginnning-of-statement")
   "nd" '(python-nav-end-of-statement :which-key "end-of-statement")
   "nk" '(python-nav-backward-defun :which-key "backward-defun")
   "no" '(python-nav-forward-defun :which-key "forward-defun")
   "nl" '(python-nav-up-list :which-key "up-list")
   "ni" '(python-nav-backward-up-list :which-key "backward-up-list")
   "nc" '(python-nav-backward-sexp-safe :which-key "backward-sexp-safe")
   "nv" '(python-nav-forward-sexp-safe :which-key "forward-sexp-safe")
   "nw" '(python-nav-forward-sexp :which-key "forward-sexp")
   "na" '(python-nav-backward-sexp :which-key "backward-sexp")
   )

  ;; php-mode

  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'php-mode-map
   :major-modes 'php-mode
   :prefix ","
   "="  '(:ignore t :which-key "format")
   "==" '(php-cs-fixer-fix :which-key "php-cs-fixer")
   "=b" '(php-cs-fixer-fix :which-key "php-cs-fixer")
   "=s" '(php-set-style :which-key "set-style")
   "=p" '(format-all-buffer :which-key "prettier-plugin-php")
   )

  ;;;;; for major modes ; the same hotkeys for all mentioned modes

  ;; comments

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map)
   :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode)
   :prefix ","
   "k"  '(:ignore t :which-key "comments, marks")
   "kl" '(comment-dwim :which-key "comment-dwim")
   "kk" '(mark-defun :which-key "mark-fun")
   "km" '(comment-region :which-key "comment-region")
   "kn" '(uncomment-region :which-key "uncomment-region")
   "kj" '(comment-line :which-key "comment-line")
   "kt" '(comment-kill :which-key "comment-kill")
   )

  ;; helm-gtags / ggtags / ctags:

  (general-define-key
   :states '(normal visual emacs)
   :keymaps '(sh-mode-map perl-mode-map cperl-mode-map emacs-lisp-mode-map python-mode-map php-mode-map)
   :major-modes '(sh-mode perl-mode cperl-mode emacs-lisp-mode python-mode php-mode)
   :prefix ","
   "g"  '(:ignore t :which-key "ggtags")
   "gc" '(helm-gtags-create-tags :which-key "create-tags")
   "gg" '(helm-gtags-find-pattern :which-key "find-pattern")
   "gm" '(helm-gtags-mode :which-key "helm-gtags-mode")
   "gl" '(helm-gtags-select :which-key "gtags-select")
   "gd" '(helm-gtags-find-tag :which-key "find-def")
   "gD" '(helm-gtags-find-tag-from-here :which-key "find-def-from-here")
   "gf" '(helm-gtags-find-files :which-key "find-file")
   "gr" '(helm-gtags-find-rtag :which-key "find-refr")
   "gi" '(helm-gtags-tags-in-this-function :which-key "tags-in-func")
   "gP" '(helm-gtags-parse-file :which-key "parse-file")
   "g." '(helm-gtags-dwim :which-key "gtags-dwim")
   "go" '(helm-gtags-find-symbol :which-key "find-symbol")
   "gu" '(helm-gtags-update-tags :which-key "update-tags")
   "gs" '(helm-gtags-find-symbol :which-key "find-symbol")
   "ga" '(helm-gtags-clear-cache :which-key "clear-cache")
   "gq" '(helm-gtags-resume :which-key "gtags-resume")
   "gA" '(helm-gtags-clear-all-cache :which-key "clear-all-cache")

   ;;;; helm-gtags : navigation :

   "g]" '(helm-gtags-next-history :which-key "next-history")
   "g}" '(helm-gtags-next-history :which-key "next-history")
   "g[" '(helm-gtags-previous-history :which-key "previous-history")
   "g{" '(helm-gtags-previous-history :which-key "previous-history")
   "g/" '(helm-gtags-show-stack :which-key "show-stack")
   "g0" '(helm-gtags-clear-stack :which-key "clear-stack")
   "g)" '(helm-gtags-clear-all-stacks :which-key "clear-all-stacks")
   "g-" '(helm-gtags-pop-stack :which-key "pop-stack")
   "g_" '(helm-gtags-pop-stack :which-key "pop-stack")
   "g=" '(helm-gtags-push-stack :which-key "push-stack")
   "g+" '(helm-gtags-push-stack :which-key "push-stack")

    ;;;; ggtags raw // no helm:

   "gt"  '(:ignore t :which-key "ggtags_raw")
   "gtt" '(ggtags-find-tag-regexp :which-key "find-tag-regexp")
   "gt." '(ggtags-find-tag-dwim :which-key "find-tag-dwim")
   "gtm" '(ggtags-mode :which-key "ggtags-mode")
   "gte" '(ggtags-grep :which-key "grep")
   "gtc" '(ggtags-create-tags :which-key "create-tags")
   "gtf" '(ggtags-find-file :which-key "find-file")
   "gtd" '(ggtags-find-definition :which-key "find-definition")
   "gtD" '(ggtags-show-definition :which-key "show-definition")
   "gtr" '(ggtags-find-reference :which-key "find-reference")
   "gts" '(ggtags-find-other-symbol :which-key "find-other-symbol")
   "gtu" '(ggtags-update-tags :which-key "update-tags")
   "gt/" '(ggtags-view-search-history :which-key "view-search-history")
   "gt%" '(ggtags-query-replace :which-key "query-replace")
   )


  ;; commented code..

  ;; (general-create-definer private/with-leader
  ;;                         :prefix "SPC"
  ;;                         :keymaps 'override
  ;;                         :states '(normal visual emacs))
  ;; (general-create-definer private/with-local-leader
  ;;                         :prefix ","
  ;;                         :states '(normal visual emacs))
  )

(provide 'cfg-general-which-keys)
;;; cfg-general-which-keys.el ends here
