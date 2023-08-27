;; no space + no which-key

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 "<M-tab>" 'cfg/alternate-buffer
 "<C-mouse-4>" 'text-scale-increase
 "<C-mouse-5>" 'text-scale-decrease

 "<f5><f4>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f6>" '(completion-at-point :which-key "completion-at-point-capf")
 "C-<tab>" 'completion-at-point

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

;; no space + which-key + no whick-key : normal mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'global
 "gl" '(goto-line :which-key "goto-line")
 "g'" '(vc-git-grep :which-key "git-grep")
 "q"  'kill-this-buffer
 "zq" '(evil-quit :which-key "evil-quit")
 "Q"  'evil-record-macro)

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; GLOBAL and no prefix:

 "TAB" '(cfg/alternate-buffer :which-key "alternate-buffer")
 "<mouse-1>" '(tabbar-mode :which-key "tbb-mode")
 "<mouse-3>" '(tabbar-press-home :which-key "tbb-home")
 "<mouse-2>" '(tabbar-backward-group :which-key "tbb-backward")
 "SPC" '(execute-extended-command :which-key "M-x")
 "!" '(shell-command :which-key "shell cmd")
 "<up>" 'tab-rename
 "<down>" 'tab-new
 "<left>" 'tab-previous
 "<right>" 'tab-next
 "<deletechar>" 'tab-close
 "<next>" 'tabbar-backward
 "<prior>" 'tabbar-forward

 ;; macros:

 "Q"   '(:ignore t :which-key "macros")
 "QQ"  '(evil-record-macro :which-key "evil-record-macro")
 "Q@"  '(evil-execute-macro :which-key "evil-execute-maco")
 "Qp"  '(kmacro-cycle-ring-next :which-key "kmacro-next")
 "Qn"  '(kmacro-cycle-ring-previous :which-key "kmacro-previous")
 "Qv"  '(kmacro-view-macro-repeat :which-key "kmacro-view-macro")

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

 ;; files

 "f"   '(:ignore t :which-key "files")
 "ff"  '(find-file :which-key "find-file")
 "fa"  '(cfg/ffap :which-key "ffap")
 "fz"  '(fzf :which-key "fzf")
 "fR"  '(cfg/rename-current-buffer-file :which-key "rename-current-buffer-file")
 "fE"  '(cfg/sudo-edit :which-key "sudo-edit")
 "fv"  '(revert-buffer :which-key "revert-refresh-buffer")
 "fV"  '(auto-revert-mode :which-key "auto-revert-mode")
 "fr"  '(recentf-open-files :which-key "recentf")
 "fs"  '(save-buffer :which-key "save buffer")
 "fo"  '(save-some-buffers :which-key "save-some-buffers")
 "fD"  '(cfg/delete-current-buffer-file :which-key "delete-current-buffer-file")
 "fd"  '(dired :which-key "dired")
 "fy"  '(:ignore t :which-key "Yank/Copy")
 "fyp" '(cfg/show-file-name :which-key "show-file-name")
 "fyb" '(cfg/copy-buffer-name :which-key "Buffer Name")
 "fyc" '(cfg/copy-file-path-with-line-column :which-key "File path with line and column")
 "fyd" '(cfg/copy-directory-path :which-key "Directory path")
 "fyl" '(cfg/copy-file-path-with-line :which-key "File path with line number")
 "fyn" '(cfg/copy-file-name :which-key "File name")
 "fyN" '(cfg/copy-file-name-base :which-key "File name, no ext")
 "fyy" '(cfg/copy-file-path :which-key "File full path")
 "fb"  '(bookmark-jump :which-key "bookmark-jump")
 "fB"  '(:ignore t :which-key "bookmarks")
 "fBb" '(bookmark-jump :which-key "bookmark-jump")
 "fBl" '(bookmark-bmenu-list :which-key "bmenu-list")
 "fBL" '(list-bookmarks :which-key "list-bookmarks")
 "fBs" '(bookmark-set :which-key "bookmark-add-set")
 "fBd" '(bookmark-delete :which-key "bookmark-delete")
 "fBS" '(bookmark-save :which-key "bookmark-save-to-file")

 ;; buffers

 "b"   '(:ignore t :which-key "buffers")
 "bu"  '(cfg/dos2unix :which-key "dos2unix")
 "be"  '(revert-buffer :which-key "revert-refresh-buffer")
 "by"  '(evil-paste-pop :which-key "yank-pop")
 "bb"  '(switch-to-buffer :which-key "switch-to-buffer")
 "bB"  '(switch-to-buffer-other-window :which-key "switch-to-buffer-other-window")
 "bs"  '(sort-lines :which-key "sort-lines")
 "bF"  '(format-all-buffer :which-key "format-all-buffer")
 "bv"  '(ibuffer :which-key "ibuffer")
 "bt"  '(eval-buffer :which-key "eval-buffer")
 "bd"  '(kill-this-buffer :which-key "kill-this-buffer")
 "bN"  '(cfg/new-empty-buffer :which-key "new-empty-buffer")
 "bx"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "bX"  '(cfg/kill-other-buffers :which-key "kill-other-buffers")
 "ba"  '(:ignore t :which-key "yasnippet")
 "byy" '(yas-describe-tables :which-key "yas-describe-tables")
 "bm"  '(:ignore t :which-key "modes")
 "bmv" '(auto-revert-mode :which-key "auto-revert-mode")
 "bmp" '(python-mode :which-key "python-mode")
 "bmP" '(anaconda-mode :which-key "anaconda-mode")
 "bmg" '(ggtags-mode :which-key "ggtags-mode")
 "bmt" '(text-mode :which-key "text-mode")
 "bmo" '(org-mode :which-key "org-mode")
 "bmc" '(conf-mode :which-key "conf-mode")
 "bme" '(emacs-lisp-mode :which-key "emacs-lisp-mode")
 "bmw" '(web-mode :which-key "web-mode")
 "bmf" '(format-all-mode :which-key "format-all-mode")

 ;; completions

 "<f5>"     '(:ignore t :which-key "completions")
 "<f5><f4>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f6>" '(completion-at-point :which-key "completion-at-point-capf")

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
 "am"   '(man :which-key "man")
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
 "ae"   '(:ignore t :which-key "erc/irc")
 "ac"   '(:ignore t :which-key "calculators")
 "acc"  '(calculator :which-key "calculator")
 "acd"  '(calc-dispatch :which-key "calc-dispatch")
 "aq"   '(quick-calc :which-key "quick-calc")
 "ao"   '(:ignore t :which-key "operating system")
 "aop"  '(pacfiles :which-key "pacfiles")
 "as"   '(:ignore t :which-key "shells")
 "ar"   '(:ignore t :which-key "regexp")
 "asm"  '(cfg/multi-term-buffer-rn :which-key "multi-term_bash")
 "asa"  '(ansi-term :which-key "ansi-term_bash")
 "asv"  '(multi-vterm :which-key "multi-vterm_fish")
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
 "so"  '(locate :which-key "locate")
 "sl"  '(goto-line :which-key "goto-line")
 "si"  '(:ignore t :which-key "isearch")
 "sii" '(isearch-forward :which-key "isearch-forward")
 "sib" '(isearch-backward :which-key "isearch-backward")
 "s'"  '(vc-git-grep :which-key "git-grep")
 "sj"  '(imenu :which-key "imenu")
 "sn"  '(find-name-dired :which-key "find-name-dired")
 "sd"  '(find-grep-dired :which-key "find-grep-dired")
 "sz"  '(zrgrep :which-key "zrgrep")
 "sr"  '(rgrep :which-key "rgrep")
 "se"  '(cfg/grep-recentf :which-key "grep-recentf")
 "sp"  '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
 "sf"  '(find-dired :which-key "find")
 "sq"  '(:ignore t :which-key "query-replace")
 "sqq" '(query-replace-regexp :which-key "query-replace-regexp")
 "sqp" '(project-query-replace-regexp :which-key "project-query-replace-regexp")
 "sqr" '(map-query-replace-regexp :which-key "map-query-replace-regexp")
 "sqf" '(dired-do-find-regexp-and-replace :which-key "dired-do-find-regexp-and-replace")
 "sg'" '(vc-git-grep :which-key "git-grep")

 ;; help

 "h"   '(:ignore t :which-key "help")
 "hi"  '(info :which-key "info")
 "hc"  '(customize :which-key "customize")
 "ht"  '(load-theme :which-key "load-theme")
 "hI"  '(Info-search :which-key "info-search")
 "hd"  '(:ignore t :which-key "describe")
 "hdf" '(describe-function :which-key "describe-function")
 "hdk" '(describe-key :which-key "describe-key")
 "hdy" '(describe-keymap :which-key "describe-mode")
 "hdo" '(describe-font :which-key "describe-font")
 "hda" '(what-cursor-position :which-key "show-face-font-under-point")
 "hdm" '(describe-mode :which-key "describe-mode")
 "hdi" '(describe-minor-mode :which-key "describe-minor-mode")
 "hdt" '(describe-theme :which-key "describe-theme")
 "hdv" '(describe-variable :which-key "describe-variable")
 "hdp" '(describe-package :which-key "describe-package")
 "hdc" '(describe-char :which-key "describe-char")

 ;; diff

 "D"  '(:ignore t :which-key "diff")
 "Db" '(ediff-buffers :which-key "ediff-buffers")
 "DB" '(ediff-buffers3 :which-key "ediff-buffers3")
 "D3" '(ediff-buffers3 :which-key "ediff-buffers3")
 "Df" '(ediff-files :which-key "ediff-files")
 "DF" '(ediff-files3 :which-key "ediff-files3")
 "De" '(ediff :which-key "ediff")
 "Ds" '(ediff-save-buffer :which-key "ediff-save-buffer")
 "Du" '(ediff-update-diffs :which-key "ediff-update-diffs")
 "Dq" '(ediff-quit :which-key "ediff-quit")

 ;; projects

 "p"   '(:ignore t :which-key "projects")

 "p/"  '(vc-git-grep :which-key "git-grep")
 "p!"  '(project-shell-command :which-key "shell-command")
 "ps"  '(project-shell :which-key "shell")
 "p&"  '(project-async-shell-command :which-key "async-shell-command")
 "p%"  '(project-query-replace-regexp :which-key "replace-regexp")
 "pb"  '(project-switch-to-buffer :which-key "switch-to-buffer")
 "pc"  '(project-compile :which-key "project-compile")
 "pd"  '(project-find-dir :which-key "find-dir")
 "pD"  '(project-dired :which-key "dired")
 "pf"  '(project-find-file :which-key "find-file")
 "pF"  '(project-or-external-find-file :which-key "find-file-or-external")
 "pk"  '(project-kill-buffers :which-key "kill-buffers")
 "pp"  '(project-switch-project :which-key "switch-project")
 "pv"  '(project-vc-dir :which-key "vc")
 "pM"  '(project-forget-project :which-key "forget-project")
 "p;"  '(:ignore t :which-key "search/grep")
 "p;;" '(project-find-regexp :which-key "find-regexp")


 ;; git, magit

 "g"  '(:ignore t :which-key "git")
 "g/" '(vc-git-grep :which-key "git-grep")
 "g'" '(vc-git-grep :which-key "git-grep")
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
