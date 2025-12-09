;;; cfg-gen-for-all-modes.el --- general.el for all modes -*- lexical-binding: t -*-

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; no space + no which-key

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps 'global

 "<M-tab>" 'cfg/alternate-buffer
 "<C-mouse-4>" 'text-scale-increase
 "<C-mouse-5>" 'text-scale-decrease

 "C-<tab>" 'completion-at-point

 ;; tab-bar-mode (emacs 27++)

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

;; no space + which-key + no which-key : normal mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'global
 "gl" '(goto-line :which-key "goto-line")
 "gr" '(revert-buffer :which-key "revert-buffer")
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
 "SPC" '(execute-extended-command :which-key "M-x")
 "<up>" 'tab-rename
 "<down>" 'tab-new
 "<left>" 'tab-previous
 "<right>" 'tab-next
 "<deletechar>" 'tab-close

 ;; macros:

 "Q"   '(:ignore t :which-key "macros")
 "QQ"  '(evil-record-macro :which-key "evil-record-macro")
 "Q@"  '(evil-execute-macro :which-key "evil-execute-maco")
 "Qp"  '(kmacro-cycle-ring-next :which-key "kmacro-next")
 "Qn"  '(kmacro-cycle-ring-previous :which-key "kmacro-previous")
 "Qv"  '(kmacro-view-macro-repeat :which-key "kmacro-view-macro")

 ;; files

 "f"   '(:ignore t :which-key "files")
 "ff"  '(find-file :which-key "find-file")
 "fa"  '(cfg/ffap :which-key "ffap")
 "fR"  '(cfg/rename-current-buffer-file :which-key "rename-current-buffer-file")
 "fE"  '(cfg/sudo-edit :which-key "sudo-edit")
 "fv"  '(revert-buffer :which-key "revert-refresh-buffer")
 "fV"  '(auto-revert-mode :which-key "auto-revert-mode")
 "fe"  '(recentf-open-files :which-key "recentf")
 "fp"  '(pwd :which-key "pwd")
 "fr"  '(cfg/recentf-jump-open :which-key "recentf jump")
 "fs"  '(save-buffer :which-key "save buffer")
 "fo"  '(save-some-buffers :which-key "save-some-buffers")
 "fD"  '(cfg/delete-current-buffer-file :which-key "delete-current-buffer-file")
 "fd"  '(:ignore t :which-key "dired")
 "fds"  '(cfg/sudired :which-key "sudired")
 "fdd"  '(dired :which-key "dired")
 "fy"  '(:ignore t :which-key "Yank/Copy")
 "fw"  '(cfg/open-with :which-key "open-with")
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
 "b SPC" '(execute-extended-command-for-buffer :which-key "M-X")
 "bu"  '(cfg/dos2unix :which-key "dos2unix")
 "br"  '(revert-buffer :which-key "revert-refresh-buffer")
 "by"  '(evil-paste-pop :which-key "yank-pop")
 "bb"  '(switch-to-buffer :which-key "switch-to-buffer")
 "bB"  '(switch-to-buffer-other-window :which-key "switch-to-buffer-other-window")
 "bs"  '(sort-lines :which-key "sort-lines")
 "bv"  '(ibuffer :which-key "ibuffer")
 "bt"  '(eval-buffer :which-key "eval-buffer")
 "bd"  '(kill-this-buffer :which-key "kill-this-buffer")
 "bN"  '(cfg/new-empty-buffer :which-key "new-empty-buffer")
 "bx"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "bX"  '(cfg/kill-other-buffers :which-key "kill-other-buffers")
 "ba"  '(auto-revert-mode :which-key "auto-revert")
 "bA"  '(global-auto-revert-mode :which-key "global-auto-revert")
 "bm"  '(:ignore t :which-key "core modes")
 "bmn" '(normal-mode :which-key "normal-mode")
 "bmp" '(python-mode :which-key "python-mode")
 "bml" '(cperl-mode :which-key "cperl-mode")
 "bmt" '(text-mode :which-key "text-mode")
 "bmo" '(org-mode :which-key "org-mode")
 "bmc" '(conf-mode :which-key "conf-mode")
 "bme" '(emacs-lisp-mode :which-key "emacs-lisp-mode")
 "bmx" '(nxml-mode :which-key "nxml-mode")
 "bmv" '(visual-line-mode :which-key "visual-line-mode")
 "bmj" '(js-json-mode :which-key "js-json-mode")

 ;; windows

 "w"   '(:ignore t :which-key "windows")
 "wt"   '(:ignore t :which-key "tabs")
 "wt <deletechar>" '(tab-close :which-key "tab-close")
 "wt <up>" '(tab-rename :which-key "tab-rename")
 "wt <down>" '(tab-new :which-key "tab-new")
 "wt <left>" '(tab-previous :which-key "tab-previous")
 "wt <right>" '(tab-next :which-key "tab-next")
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
 "a!"   '(shell-command :which-key "shell cmd")
 "ad"   '(dired :which-key "dired")
 "am"   '(man :which-key "man")
 "an"   '(calendar :which-key "calendar")
 "al"   '(:ignore t :which-key "eln")
 "all"  '(cfg/eln-compile-only-safe :which-key "eln-compile-only-safe")
 "als"  '(cfg/eln-compile-site-elisp :which-key "eln-compile-site-elisp")
 "alg"  '(cfg/eln-compile-elisp-general :which-key "eln-compile-elisp-general")
 "ale"  '(cfg/eln-compile-elisp :which-key "eln-compile-elisp")
 "ae"   '(:ignore t :which-key "irc")
 "aer"  '(rcirc :which-key "rcirc")
 "ac"   '(:ignore t :which-key "calculators")
 "acc"  '(calculator :which-key "calculator")
 "acd"  '(calc-dispatch :which-key "calc-dispatch")
 "acq"  '(quick-calc :which-key "quick-calc")
 "as"   '(:ignore t :which-key "shells")
 "ase"  '(eshell :which-key "eshell")
 "asa"  '(ansi-term :which-key "ansi-term_bash")
 "asx"  '(cfg/C-u-M-x-shell :which-key "C-u-M-x-shell_bash")
 "asX"  '(shell :which-key "M-x-shell_bash")
 "asp"  '(run-python :which-key "python-shell")
 "ap"   '(:ignore t :which-key "packages")
 "apq"  '(:ignore t :which-key "quelpa") ;; reserved
 "ap1"  '(package-upgrade :which-key "upgrade")
 "apu"  '(package-upgrade-all :which-key "upgrade-all")
 "apl"  '(package-list-packages :which-key "list-packages")
 "api"  '(package-install :which-key "install")
 "apd"  '(package-delete :which-key "delete")
 "apr"  '(package-refresh-contents :which-key "refresh")
 "apa"  '(package-autoremove :which-key "autoremove")

 ;; search

 "s"   '(:ignore t :which-key "search")
 "so"  '(locate :which-key "locate")
 "sl"  '(goto-line :which-key "goto-line")
 "si"  '(:ignore t :which-key "isearch")
 "sii" '(isearch-forward :which-key "isearch-forward")
 "sib" '(isearch-backward :which-key "isearch-backward")
 "s'"  '(vc-git-grep :which-key "git-grep")
 "sm"  '(imenu :which-key "imenu")
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

 ;; help
 "h"   '(:ignore t :which-key "help")

 ;; Old and legacy keybindings, rarely used, based and compatible with `C-c'
 "h="   '(:ignore t :which-key "old_legacy")
 "h= C-o" '(describe-distribution :which-key "d-distribution")
 "h=h" '(view-hello-file :which-key "hello-file")
 "h=g" '(describe-gnu-project :which-key "gnu-project")
 "h= C-e" '(view-external-packages :which-key "external-packages")
 "h= C-c" '(describe-copying :which-key "d-copying")
 "h= <return>" '(view-order-manuals :which-key "order-manuals")
 "h= C-t" '(view-emacs-todo :which-key "emacs-todo")
 "h= C-w" '(describe-no-warranty :which-key "d-no-warranty")
 "h= <help>" '(help-for-help :which-key "help-for-help")
 "h= C-h" '(help-for-help :which-key "help-for-help")
 "h= <f1>" '(help-for-help :which-key "help-for-help")

 ;; help bindings, converted from `C-h' table
 "h C-a" '(about-emacs :which-key "about-emacs")
 "h C-d" '(view-emacs-debugging :which-key "emacs-debugging")
 "h C-f" '(view-emacs-FAQ :which-key "emacs-FAQ")
 "h C-n" '(view-emacs-news :which-key "emacs-news")
 "h C-p" '(view-emacs-problems :which-key "emacs-problems")
 "h C-q" '(help-quick-toggle :which-key "quick-toggle")
 "h C-s" '(search-forward-help-for-help :which-key "search-help")
 "h." '(display-local-help :which-key "local-help")
 "h?" '(help-for-help :which-key "help-for-help")
 "hB" '(embark-bindings :which-key "embark-bindings")
 "hF" '(Info-goto-emacs-command-node :which-key "info-command-node")
 "hK" '(Info-goto-emacs-key-command-node :which-key "info-key-node")
 "hR" '(info-display-manual :which-key "info-manual")
 "hS" '(info-lookup-symbol :which-key "lookup-symbol")
 "hA" '(apropos :which-key "apropos")
 "ha" '(apropos-command :which-key "apropos-command")
 "hd" '(apropos-documentation :which-key "apropos-doc")
 "he" '(view-echo-area-messages :which-key "echo-messages")
 "hi" '(info :which-key "info")
 "hl" '(view-lossage :which-key "lossage")
 "hn" '(view-emacs-news :which-key "emacs-news")
 "hp" '(finder-by-keyword :which-key "finder-keyword")
 "hq" '(help-quit :which-key "quit")
 "hr" '(info-emacs-manual :which-key "emacs-manual")
 "ht" '(help-with-tutorial :which-key "tutorial")
 "hw" '(where-is :which-key "where-is")

 ;; describe (again: same as `Ctrl-c')
 "hC" '(describe-coding-system :which-key "d-coding-system")
 "hb" '(describe-bindings :which-key "d-bindings")
 "hc" '(describe-key-briefly :which-key "d-key-briefly")
 "ho" '(describe-symbol :which-key "d-symbol")
 "hk" '(describe-key :which-key "d-key")
 "hs" '(describe-syntax :which-key "d-syntax")
 "hL" '(describe-language-environment :which-key "d-language-env")
 "hP" '(describe-package :which-key "d-package")
 "hm" '(describe-mode :which-key "d-mode")
 "hx" '(describe-command :which-key "d-command")
 "hf" '(describe-function :which-key "d-function")
 "hI" '(describe-input-method :which-key "d-input-method")
 "hv" '(describe-variable :which-key "d-variable")
 "h C-\\" '(describe-input-method :which-key "d-input-method")

 ;; additional describe group
 "hy"   '(:ignore t :which-key "describe")
 "hym"  '(describe-mode :which-key "describe-mode")
 "hyc"  '(describe-char :which-key "describe-char")
 "hye"  '(what-cursor-position :which-key "describe-cursor-position")
 "hyf"  '(describe-function :which-key "describe-function")
 "hyi"  '(describe-minor-mode :which-key "describe-minor-mode")
 "hyk"  '(describe-key :which-key "describe-key")
 "hyo"  '(describe-font :which-key "describe-font")
 "hyP"  '(describe-package :which-key "describe-package")
 "hyt"  '(describe-theme :which-key "describe-theme")
 "hyv"  '(describe-variable :which-key "describe-variable")
 "hyy"  '(describe-keymap :which-key "describe-keymap")
 "hyC"  '(describe-coding-system :which-key "describe-coding-system")
 "hyb"  '(describe-bindings :which-key "describe-bindings")
 "hyB"  '(describe-key-briefly :which-key "describe-key-briefly")
 "hys"  '(describe-syntax :which-key "describe-syntax")
 "hyL"  '(describe-language-environment :which-key "describe-language-environment")
 "hyx"  '(describe-command :which-key "describe-command")
 "hyI"  '(describe-input-method :which-key "describe-input-method")
 "hyl"  '(describe-symbol :which-key "describe-symbol")

 ;; additional keybindings overriding and changing the traditional `C-h' namespace
 "h4" '(info-other-window :which-key "info-other-window")
 "h3" '(help-find-source :which-key "find-source")
 "h,"  '(cfg/show-major-mode :which-key "show-major-mode")
 "h\\" '(customize :which-key "customize")
 "h="  '(load-theme :which-key "load-theme")

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
 "g'" '(vc-git-grep :which-key "git-grep"))

(provide 'cfg-gen-for-all-modes)
;;; cfg-gen-for-all-modes.el ends here
