;; general-global: no space + no which-key

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 "<M-tab>" 'cfg/alternate-buffer
 "S-<iso-lefttab>" 'indent-for-tab-command
 "S-<tab>" 'cfg/company-backend-with-yas
 "<C-mouse-4>" 'text-scale-increase
 "<C-mouse-5>" 'text-scale-decrease

 ;; tabbar legacy plugin

 "<S-next>" 'tabbar-backward
 "<S-prior>" 'tabbar-forward
 ;;   "<header-line> <mouse-1>" '(tabbar-press-home :wk t)
 ;;   "<header-line> <mouse-2>" '(tabbar-press-home :wk t)
 ;;   "<header-line> <mouse-3>" '(tabbar-press-home :wk t)
 "<header-line> <mouse-9>" 'tabbar-forward-group
 "<header-line> <drag-mouse-9>" 'tabbar-forward-group
 "<header-line> <mouse-8>" 'tabbar-backward-group
 "<header-line> <drag-mouse-8>" 'tabbar-backward-group

 ;; tab (emacs 27++)

 "<mouse-9>" 'tab-next
 "<drag-mouse-9>" 'tab-next
 "<mouse-8>" 'tab-previous
 "<drag-mouse-8>" 'tab-previous
 "<M-mouse-9>" 'tab-new
 "<M-drag-mouse-9>" 'tab-new
 "<M-mouse-8>" 'tab-close
 "<M-drag-mouse-8>" 'tab-close
 "<M-mouse-2>" 'tab-close
 "<M-drag-mouse-2>" 'tab-close)

;; general-global: no space + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'global
 "g/"  '(cfg/helm-git-grep-at-point :which-key "helm-git-grep-at-point")
 "g'"  '(cfg/helm-git-grep :which-key "helm-git-grep"))

;; global-global: space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; GLOBAL and no prefix:
 
 "TAB" '(cfg/alternate-buffer :which-key "alternate-buffer")
 "<mouse-1>" '(tabbar-mode :which-key "tbb-mode")
 "<mouse-3>" '(tabbar-press-home :which-key "tbb-home")
 "<mouse-2>" '(tabbar-backward-group :which-key "tbb-backward")
 "SPC" '(helm-M-x :which-key "M-x")
 "!" '(shell-command :which-key "shell cmd")
 "<up>" 'tab-rename
 "<down>" 'tab-new
 "<left>" 'tab-previous
 "<right>" 'tab-next
 "<deletechar>" 'tab-close
 "<next>" 'tabbar-backward
 "<prior>" 'tabbar-forward

 ;; treemacs:

 "0"   '(:ignore t :which-key "treemacs")
 "00"  '(treemacs :which-key "treemacs")
 "0o"  '(treemacs-switch-workspace :which-key "switch-workspace")
 "0w"  '(:ignore t :which-key "treemacs-workspaces")
 "0ww" '(treemacs-switch-workspace :which-key "switch-workspace")
 "0wa" '(treemacs-create-workspace :which-key "create-workspace")
 "0wd" '(treemacs-remove-workspace :which-key "remove-workspace")
 "0wr" '(treemacs-rename-workspace :which-key "rename-workspace")
 "0wf" '(treemacs-set-fallback-workspace :which-key "set-fallback-workspace")
 "0we" '(treemacs-edit-workspaces :which-key "edit-workspaces")
 "0wn" '(treemacs-next-workspaces :which-key "next-workspace")
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
 "fR"  '(cfg/rename-current-buffer-file :which-key "rename-current-buffer-file")
 "fE"  '(cfg/sudo-edit :which-key "sudo-edit")
 "fv"  '(revert-buffer :which-key "revert-buffer")
 "fV"  '(auto-revert-mode :which-key "auto-revert-mode")
 "fr"  '(helm-recentf :which-key "helm-recentf")
 "fs"  '(save-buffer :which-key "save buffer")
 "fo"  '(save-some-buffers :which-key "save-some-buffers")
 "fb"  '(helm-bookmarks :which-key "bookmarks")
 "fD"  '(cfg/delete-current-buffer-file :which-key "delete-current-buffer-file")
 "fy"  '(:ignore t :which-key "Yank/Copy")
 "fyp" '(cfg/show-file-name :which-key "show-file-name")
 "fyb" '(cfg/copy-buffer-name :which-key "Buffer Name")
 "fyc" '(cfg/copy-file-path-with-line-column :which-key "File path with line and column")
 "fyd" '(cfg/copy-directory-path :which-key "Directory path")
 "fyl" '(cfg/copy-file-path-with-line :which-key "File path with line number")
 "fyn" '(cfg/copy-file-name :which-key "File name")
 "fyN" '(cfg/copy-file-name-base :which-key "File name, no ext")
 "fyy" '(cfg/copy-file-path :which-key "File full path")
 "fyC" '(cfg/projectile-copy-file-path-with-line-column :which-key "projectile-copy-file-path-with-line-column")
 "fyD" '(cfg/projectile-copy-directory-path :which-key "projectile-copy-directory-path")
 "fyL" '(cfg/projectile-copy-file-path-with-line :which-key "projectile-copy-file-path-with-line")
 "fyY" '(cfg/projectile-copy-file-path :which-key "projectile-copy-file-path")

 ;; buffers

 "b"   '(:ignore t :which-key "buffers")
 "bm"  '(:ignore t :which-key "modes")
 "bma" '(auto-revert-mode :which-key "auto-revert-mode")
 "bmg" '(ggtags-mode :which-key "ggtags-mode")
 "bmA" '(anaconda-mode :which-key "anaconda-mode")
 "bmt" '(text-mode :which-key "text-mode")
 "bmo" '(org-mode :which-key "org-mode")
 "bmc" '(conf-mode :which-key "conf-mode")
 "bme" '(emacs-lisp-mode :which-key "emacs-lisp-mode")
 "bb"  '(helm-buffers-list :which-key "buffers list")
 "bF"  '(format-all-buffer :which-key "format-all-buffer")
 "bv"  '(ibuffer :which-key "ibuffer")
 "bt"  '(eval-buffer :which-key "eval-buffer")
 "bd"  '(kill-this-buffer :which-key "kill-this-buffer")
 "bN"  '(cfg/new-empty-buffer :which-key "new-empty-buffer")
 "bx"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "bX"  '(cfg/kill-other-buffers :which-key "kill-other-buffers")
 "by"  '(:ignore t :which-key "yas_company")
 "byy" '(yas-describe-tables :which-key "yas-describe-tables")

 ;; windows

 "w"   '(:ignore t :which-key "windows")
 "wt"   '(:ignore t :which-key "tabs")
 "wt <deletechar>" '(tab-close :which-key "tab-close")
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
 "wm"  '(cfg/toggle-maximize-buffer :which-key "maximize-buffer")
 "wl"  '(windmove-right :which-key "right")
 "wh"  '(windmove-left :which-key "left")
 "wk"  '(windmove-up :which-key "up")
 "wj"  '(windmove-down :which-key "down")
 "wq"  '(evil-quit :which-key "evil-quit")
 "wQ"  '(kill-emacs :which-key "kill-emacs")
 "w <left>"  '(windmove-left :which-key "left")
 "w <right>"  '(windmove-right :which-key "right")
 "w <down>"  '(windmove-down :which-key "down")
 "w <up>"  '(windmove-up :which-key "up")
 "w/"  '(split-window-right :which-key "split right")
 "w-"  '(split-window-below :which-key "split bottom")
 "wx"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")

 ;; apps

 "a"    '(:ignore t :which-key "apps")
 "al"   '(:ignore t :which-key "eln")
 "all"  '(cfg/eln-compile-only-safe :which-key "eln-compile-only-safe")
 "als"  '(cfg/eln-compile-site-elisp :which-key "eln-compile-site-elisp")
 "alg"  '(cfg/eln-compile-elisp-general :which-key "eln-compile-elisp-general")
 "ale"  '(cfg/eln-compile-elisp :which-key "eln-compile-elisp")
 "aw"   '(:ignore t :which-key "webpaste")
 "awk"  '(webpaste-paste-buffer :which-key "webpaste-paste-buffer")
 "awl"  '(webpaste-paste-region :which-key "webpaste-paste-region")
 "aa"   '(:ignore t :which-key "notmuch")
 "aan"  '(notmuch-mua-new-mail :which-key "notmuch-mua-new-mail")
 "aaa"  '(notmuch-jump-search :which-key "notmuch-jump-search")
 "aaj"  '(notmuch-jump-search :which-key "notmuch-jump-search")
 "aah"  '(notmuch :which-key "notmuch-hello")
 "aap"  '(cfg/notmuch-poll-mbsync :which-key "notmuch-poll-mbsync")
 "aam"  '(helm-notmuch :which-key "helm-notmuch")
 "ae"   '(:ignore t :which-key "erc/irc")
 "ac"   '(:ignore t :which-key "calculators")
 "acc"  '(calculator :which-key "calculator")
 "acd"  '(calc-dispatch :which-key "calc-dispatch")
 "aq"   '(quick-calc :which-key "quick-calc")
 "as"   '(:ignore t :which-key "shells")
 "ar"   '(:ignore t :which-key "regexp")
 "asm"  '(cfg/multi-term-buffer-rn :which-key "multi-term_bash")
 "asa"  '(ansi-term :which-key "ansi-term_bash")
 "asv"  '(multi-vterm :which-key "multi-vterm_fish")
 "ass"  '(multi-vterm :which-key "multi-vterm_fish")
 "asx"  '(cfg/my-named-shell :which-key "my-named-m-x-shell_bash")
 "asX"  '(shell :which-key "m-x-shell_bash")
 "asp"  '(run-python :which-key "python-shell")
 "ast"  '(tramp-term :which-key "tramp-term")
 "aeD"  '(cfg/erc-default-servers :which-key "erc-default-servers")
 "aeE"  '(erc-tls :which-key "erc-tls")
 "aer"  '(rcirc :which-key "rcirc")
 "aee"  '(erc :which-key "erc")
 "ael"  '(:ignore t :which-key "erc-view-log-mode")
 "aelf" '(cfg/erc-find-logfile :which-key "erc-find-logfile")
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
 "adD"  '(cfg/sudired :which-key "sudired")

 ;; search

 "s"   '(:ignore t :which-key "search")
 "s/"  '(cfg/helm-git-grep-at-point :which-key "helm-git-grep-at-point")
 "s'"  '(cfg/helm-git-grep :which-key "helm-git-grep")
 "sj"  '(cfg/helm-jump-in-buffer :which-key "helm-jump-in-buffer")
 "si"  '(find-name-dired :which-key "find-name-dired")
 "sd"  '(find-grep-dired :which-key "find-grep-dired")
 "sz"  '(zrgrep :which-key "zrgrep")
 "sr"  '(rgrep :which-key "rgrep")
 "se"  '(cfg/grep-recentf :which-key "grep-recentf")
 "sp"  '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
 "sq"  '(query-replace-regexp :which-key "query-replace-regexp")
 "st"  '(project-query-replace-regexp :which-key "project-query-replace-regexp")
 "sf"  '(dired-do-find-regexp-and-replace :which-key "dired-do-find-regexp-and-replace")
 "sm"  '(map-query-replace-regexp :which-key "map-query-replace-regexp")
 "sg"  '(:ignore t :which-key "helm-grep")
 "sg/" '(cfg/helm-git-grep-at-point :which-key "helm-git-grep-at-point")
 "sg'" '(cfg/helm-git-grep :which-key "helm-git-grep")
 "sgi" '(helm-grep-do-git-grep :which-key "helm-grep-do-git-grep")
 "sgb" '(cfg/helm-buffers-do-grep :which-key "helm-buffers-do-grep")
 "sgB" '(cfg/helm-buffers-do-grep-region-or-symbol :which-key "helm-buffers-do-grep-reg-or-sym")
 "sgf" '(cfg/helm-files-do-grep :which-key "helm-files-do-grep")
 "sgF" '(cfg/helm-files-do-grep-region-or-symbol :which-key "helm-files-do-grep-reg-or-sym")
 "sgg" '(cfg/helm-file-do-grep :which-key "helm-file-do-grep")
 "sgG" '(cfg/helm-file-do-grep-region-or-symbol :which-key "helm-file-do-grep-reg-or-sym")

 ;; help

 "h"   '(:ignore t :which-key "help")
 "h?"  '(helm-descbinds :which-key "descbinds")
 "hi"  '(info :which-key "info")
 "hd"  '(:ignore t :which-key "describe")
 "hdf" '(describe-function :which-key "describe-function")
 "hdk" '(describe-key :which-key "describe-key")
 "hdo" '(describe-font :which-key "describe-font")
 "hdm" '(describe-mode :which-key "describe-mode")
 "hda" '(helm-apropos :which-key "helm-apropos")
 "hdt" '(describe-theme :which-key "describe-theme")
 "hdv" '(describe-variable :which-key "describe-variable")
 "hdp" '(describe-package :which-key "describe-package")
 "hdc" '(describe-char :which-key "describe-char")

 ;; diff

 "D"  '(:ignore t :which-key "diff")
 "Db" '(vdiff-buffers :which-key "vdiff-buffers")
 "D3" '(vdiff-buffers3 :which-key "vdiff-buffers3")
 "Df" '(vdiff-files :which-key "vdiff-files")
 "Di" '(vdiff-files3 :which-key "vdiff-files3")
 "Dm" '(vdiff-mode :which-key "vdiff-mode")
 "Dv" '(vdiff-hydra/body :which-key "vdiff-hydra")
 "Dw" '(vdiff-save-buffers :which-key "vdiff-save-buffers")
 "Du" '(vdiff-refresh :which-key "vdiff-refresh")
 "Dq" '(vdiff-quit :which-key "vdiff-quit")

 ;; projects, projectile

 "p"   '(:ignore t :which-key "projects")
 "p;"  '(:ignore t :which-key "search/grep")
 "p;;" '(helm-projectile-grep :which-key "grep")
 "p;a" '(helm-projectile-ack :which-key "ack")
 "p;g" '(helm-projectile-ag :which-key "ag")
 "p;r" '(helm-projectile-rg :which-key "rg")
 "p!"  '(projectile-run-shell-command-in-root :which-key "run-shell-command-in-root")
 "p&"  '(projectile-run-async-shell-command-in-root :which-key "run-async-shell-command-in-root")
 "p%"  '(projectile-replace-regexp :which-key "replace-regexp")
 "pa"  '(projectile-toggle-between-implementation-and-test :which-key "toggle-between-implementation-and-test")
 "pA"  '(projectile-reset-cached-project-root :which-key "reset-cached-project-root")
 "pb"  '(helm-projectile-switch-to-buffer :which-key "switch-to-buffer")
 "pc"  '(projectile-compile-project :which-key "compile-project")
 "pd"  '(helm-projectile-find-dir :which-key "find-dir")
 "pD"  '(projectile-dired :which-key "dired")
 "pe"  '(projectile-edit-dir-locals :which-key "edit-dir-locals")
 "pf"  '(helm-projectile-find-file :which-key "find-file")
 "pF"  '(helm-projectile-find-file-dwim :which-key "find-file-dwim")
 "pg"  '(projectile-find-tag :which-key "find-tag")
 "pG"  '(projectile-regenerate-tags :which-key "regenerate-tags")
 "ph"  '(helm-projectile :which-key "helm-projectile")
 "pI"  '(projectile-invalidate-cache :which-key "invalidate-cache")
 "pk"  '(projectile-kill-buffers :which-key "kill-buffers")
 "pK"  '(projectile-add-known-project :which-key "add-known-project")
 "pp"  '(helm-projectile-switch-project :which-key "switch-project")
 "pr"  '(helm-projectile-recentf :which-key "recentf")
 "pR"  '(projectile-replace :which-key "replace")
 "pM"  '(projectile-remove-known-project :which-key "remove-known-project")
 "pT"  '(projectile-test-project :which-key "test-project")
 "pv"  '(projectile-vc :which-key "vc")

 ;; git, magit

 "g"  '(:ignore t :which-key "git")
 "g/" '(cfg/helm-git-grep-at-point :which-key "helm-git-grep-at-point")
 "g'" '(cfg/helm-git-grep :which-key "helm-git-grep")
 "gb" '(magit-blame :which-key "magit-blame")
 "gc" '(magit-clone :which-key "magit-clone")
 "gi" '(magit-init :which-key "magit-init")
 "gs" '(magit-status :which-key "magit-status")
 "gm" '(magit-dispatch :which-key "magit-dispatch")
 "gL" '(magit-list-repositories :which-key "magit-list-repositories")
 "gS" '(magit-stage-file :which-key "magit-stage-file")
 "gU" '(magit-unstage-file :which-key "magit-unstage-file")
 "gr" '(magit-refresh :which-key "magit-refresh")
 "gd" '(magit-diff :which-key "magit-diff")
 "gf" '(magit-find-file :which-key "magit-find-file"))
