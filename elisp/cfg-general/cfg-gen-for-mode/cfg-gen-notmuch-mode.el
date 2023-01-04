;; general-notmuch-mode: with prefix and which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-search-mode-map notmuch-hello-mode-map notmuch-show-mode-map notmuch-tree-mode-map)
 :major-modes '(notmuch-search-mode notmuch-hello-mode notmuch-show-mode notmuch-tree-mode)
 :prefix ","
 "p"  '(:ignore t :which-key "polls")
 "pa" '(cfg/notmuch-nmuch-sort-archive :which-key "poll-sort-archive")
 "pu" '(cfg/notmuch-poll-mbsync-full-sort :which-key "poll-mbsync-full-sort")
 "pp" '(cfg/notmuch-poll-mbsync :which-key "poll-mbsync")
 "pl" '(notmuch-poll-and-refresh-this-buffer :which-key "poll")
 "pb" '(cfg/notmuch-poll-empty-bin :which-key "poll-empty-bin")
 "ps" '(cfg/notmuch-poll-empty-spam :which-key "poll-empty-spam")
 "c"  '(:ignore t :which-key "compose,create")
 "cn" '(notmuch-mua-new-mail :which-key "mua-new-mail")
 "cr" '(notmuch-search-reply-to-thread-sender :which-key "reply-to-thread-sender")
 "cy" '(notmuch-search-reply-to-thread-sender :which-key "reply-to-thread-sender")
 "cR" '(notmuch-search-reply-to-thread :which-key "reply-to-thread")
 "R" '(notmuch-refresh-all-buffers :which-key "refresh-all-buffers")
 "r" '(notmuch-refresh-this-buffer :which-key "refresh-this-buffer")
 "q" '(notmuch-bury-or-kill-this-buffer :which-key "bury-or-kill-this-buffer")
 "j" '(notmuch-jump-search :which-key "jump-search")
 "i" '(visual-line-mode :which-key "toggle-visual-line-mode")
 "J" '(notmuch-jump-search :which-key "jump-search")
 "A" '(notmuch-search :which-key "search")
 "a" '(notmuch-search :which-key "search")
 "s" '(notmuch-search :which-key "search")
 "m" '(helm-notmuch :which-key "helm-notmuch")
 "?"  '(notmuch-help :which-key "notmuch-help"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-show-mode-map)
 :major-modes '(notmuch-show-mode)
 :prefix ","
 "u" '(cfg/notmuch-toggle-tag-show-unread :which-key "toggle-unread")
 "a" '(cfg/notmuch-toggle-tag-show-arch :which-key "toggle-arch")
 "b" '(cfg/notmuch-toggle-tag-show-bin :which-key "toggle-bin")
 "d" '(cfg/notmuch-toggle-tag-show-bin :which-key "toggle-bin")
 "e" '(cfg/notmuch-toggle-tag-show-new :which-key "toggle-new")
 "w" '(cfg/notmuch-toggle-tag-show-new :which-key "toggle-new")
 "}" '(notmuch-show-next-message :which-key "show-next-msg")
 "]" '(notmuch-show-next-open-message :which-key "show-next-open-msg")
 "{" '(notmuch-show-previous-message :which-key "show-previous-msg")
 "[" '(notmuch-show-previous-open-message :which-key "show-previous-open-msg")
 "o" '(cfg/notmuch-show-close-all :which-key "show-close-all")
 "O" '(notmuch-show-open-or-close-all :which-key "show-open-all")
 "." '(cfg/notmuch-show-view-html :which-key "show-view-html")
 "t" '(notmuch-show-save-attachments :which-key "show-save-attachments")
 "c"  '(:ignore t :which-key "compose,create")
 "cr" '(notmuch-show-reply-sender :which-key "reply-sender")
 "cy" '(notmuch-show-reply-sender :which-key "reply-sender")
 "cR" '(notmuch-show-reply :which-key "reply-all")
 "cf" '(notmuch-show-forward-message :which-key "show-forward")
 "cF" '(notmuch-show-forward-open-messages :which-key "show-forward-open-msgs")
 "s"  '(:ignore t :which-key "show-stash (copy)")
 "sG" '(notmuch-show-stash-git-send-email :which-key "git-send-mail")
 "sL" '(notmuch-show-stash-mlarchive-link-and-go :which-key "mlarchive-link-and-go")
 "sl" '(notmuch-show-stash-mlarchive-link :which-key "mlarchive-link")
 "st" '(notmuch-show-stash-to :which-key "to")
 "sT" '(notmuch-show-stash-tags :which-key "tags")
 "ss" '(notmuch-show-stash-subject :which-key "subject")
 "sI" '(notmuch-show-stash-message-id-stripped :which-key "message-id-stripped")
 "si" '(notmuch-show-stash-message-id :which-key "message-id")
 "sf" '(notmuch-show-stash-from :which-key "from")
 "sF" '(notmuch-show-stash-filename :which-key "filename")
 "sd" '(notmuch-show-stash-date :which-key "date")
 "sc" '(notmuch-show-stash-cc :which-key "cc")
 "'"  '(:ignore t :which-key "show-part")
 "'?"  '(notmuch-subkeymap-help "notmuch-subkeymap-help")
 "'v"  '(notmuch-show-view-part :which-key "show-view-part")
 "'s"  '(notmuch-show-save-part :which-key "show-save-part")
 "'|"  '(notmuch-show-pipe-part :which-key "show-pipe-part")
 "'m"  '(notmuch-show-choose-mime-of-part :which-key "show-choose-mime-of-part")
 "'o"  '(notmuch-show-interactively-view-part :which-key "show-interactively-view-part"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-search-mode-map)
 :major-modes '(notmuch-search-mode)
 :prefix ","
 "S"  '(notmuch-search-filter :which-key "search-filter")
 "T"  '(notmuch-search-filter-by-tag :which-key "search-filter-by-tag")
 "v" '(notmuch-tree-from-search-current-query :which-key "tree-from-search-current-query")
 "u" '(cfg/notmuch-toggle-tag-search-unread :which-key "toggle-unread")
 "a" '(cfg/notmuch-toggle-tag-search-arch :which-key "toggle-arch")
 "b" '(cfg/notmuch-toggle-tag-search-bin :which-key "toggle-bin")
 "d" '(cfg/notmuch-toggle-tag-search-bin :which-key "toggle-bin")
 "e" '(cfg/notmuch-toggle-tag-search-new :which-key "toggle-new")
 "w" '(cfg/notmuch-toggle-tag-search-new :which-key "toggle-new"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-tree-mode-map)
 :major-modes '(notmuch-tree-mode)
 :prefix ","
 "u" '(cfg/notmuch-toggle-tag-tree-unread :which-key "toggle-unread")
 "a" '(cfg/notmuch-toggle-tag-tree-arch :which-key "toggle-arch")
 "b" '(cfg/notmuch-toggle-tag-tree-bin :which-key "toggle-bin")
 "d" '(cfg/notmuch-toggle-tag-tree-bin :which-key "toggle-bin")
 "w" '(cfg/notmuch-toggle-tag-tree-new :which-key "toggle-new")
 "e" '(cfg/notmuch-toggle-tag-tree-new :which-key "toggle-new")
 "v" '(notmuch-search-from-tree-current-query :which-key "search-from-tree-current-query")
 "c"  '(:ignore t :which-key "compose,create")
 "cr" '(notmuch-tree-reply-sender :which-key "tree-reply-to-sender")
 "cy" '(notmuch-tree-reply-sender :which-key "tree-reply-to-sender")
 "cR" '(notmuch-tree-reply :which-key "tree-reply-all"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-message-mode-map)
 :major-modes '(notmuch-message-mode)
 :prefix ","
 "F"  '(cfg/notmuch-fcc-replace :which-key "fcc-replace")
 "w"  '(message-insert-signature :which-key "insert-signature")
 ;; "p"  '(message-insert-screenshot :which-key "attach-take-screenshot") ;; optional: not working in text-mode
 "a"  '(mml-attach-file :which-key "mml-attach-file")
 "c"  '(notmuch-mua-send-and-exit :which-key "send-and-exit")
 "z"  '(message-kill-to-signature :which-key "kill-to-signature")
 "q"  '(notmuch-mua-kill-buffer :which-key "quit-kill-buffer")
 ","  '(:ignore t :which-key "goto-or-create")
 ",b"  '(message-goto-body :which-key "goto-body")
 ",s"  '(message-goto-signature :which-key "goto-signature")
 ",c"  '(message-goto-cc :which-key "goto-cc")
 ",f"  '(message-goto-from :which-key "goto-from")
 ",t"  '(message-goto-to :which-key "goto-to")
 ",r"  '(message-goto-reply-to :which-key "goto-reply-to"))

;; general-notmuch-mode: without prefix and which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-hello-mode-map)
 :major-modes '(notmuch-hello-mode)
 "r" 'notmuch-refresh-this-buffer
 "R" 'notmuch-refresh-all-buffers
 "J" 'notmuch-jump-search
 "A" 'notmuch-search
 "S-<left>" 'tabbar-backward
 "S-<right>" 'tabbar-forward
 "?" 'notmuch-help)

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-tree-mode-map)
 :major-modes '(notmuch-tree-mode)
 "r" 'notmuch-refresh-this-buffer
 "R" 'notmuch-refresh-all-buffers
 "J" 'notmuch-jump-search
 "A" 'notmuch-search
 "u" 'cfg/notmuch-toggle-tag-tree-unread
 "a" 'cfg/notmuch-toggle-tag-tree-arch
 "b" 'cfg/notmuch-toggle-tag-tree-bin
 "d" 'cfg/notmuch-toggle-tag-tree-bin
 "e" 'cfg/notmuch-toggle-tag-tree-new
 "w" 'cfg/notmuch-toggle-tag-tree-new
 "S-<left>" 'tabbar-backward
 "S-<right>" 'tabbar-forward)

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-show-mode-map)
 :major-modes '(notmuch-show-mode)
 "}" 'notmuch-show-next-message
 "]" 'notmuch-show-next-open-message
 "{" 'notmuch-show-previous-message
 "[" 'notmuch-show-previous-open-message
 "o" 'cfg/notmuch-show-close-all
 "O" 'notmuch-show-open-or-close-all
 "J" 'notmuch-jump-search
 "A" 'notmuch-search
 "r" 'notmuch-refresh-this-buffer
 "R" 'notmuch-refresh-all-buffers
 "u" 'cfg/notmuch-toggle-tag-show-unread
 "a" 'cfg/notmuch-toggle-tag-show-arch
 "b" 'cfg/notmuch-toggle-tag-show-bin
 "d" 'cfg/notmuch-toggle-tag-show-bin
 "e" 'cfg/notmuch-toggle-tag-show-new
 "w" 'cfg/notmuch-toggle-tag-show-new
 "t" 'notmuch-show-save-attachments
 "." 'cfg/notmuch-show-view-html
 "S-<left>" 'tabbar-backward
 "S-<right>" 'tabbar-forward
 "?" 'notmuch-help)

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(notmuch-search-mode-map)
 :major-modes '(notmuch-search-mode)
 "r" 'notmuch-refresh-this-buffer
 "R" 'notmuch-refresh-all-buffers
 "T" 'notmuch-search-filter-by-tag
 "u" 'cfg/notmuch-toggle-tag-search-unread
 "a" 'cfg/notmuch-toggle-tag-search-arch
 "b" 'cfg/notmuch-toggle-tag-search-bin
 "d" 'cfg/notmuch-toggle-tag-search-bin
 "e" 'cfg/notmuch-toggle-tag-search-new
 "w" 'cfg/notmuch-toggle-tag-search-new
 "J" 'notmuch-jump-search
 "A" 'notmuch-search
 "S"  'notmuch-search-filter
 "T"  'notmuch-search-filter-by-tag
 "S-<left>" 'tabbar-backward
 "S-<right>" 'tabbar-forward
 "?" 'notmuch-help)
