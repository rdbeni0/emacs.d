;;; cfg-gen-core.el --- general.el configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; "shell-script-mode is an alias for `sh-mode' in `sh-script.el'."
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-format-core
 :major-modes list-gen-mode-format-core
 :prefix ","
 "="    '(:ignore t :which-key "format")
 "=-"   '(cfg/built-in-format-via-indent :which-key "built-in-format"))

;; perl/cperl
(general-define-key
 :states '(normal visual emacs)
 :keymaps '(perl-mode-map cperl-mode-map)
 :major-modes '(perl-mode cperl-mode)
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "=-" '(cfg/built-in-format-perl :which-key "built-in-format")
 "=p" '(cfg/perltidy-format-buffer :which-key "perltidy-format-buffer")
 "=o" '(cfg/perltidy-format :which-key "perltidy-format")
 "=f" '(cfg/perltidy-format-function :which-key "perltidy-format-function"))

;; js-json-mode
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'js-json-mode-map
 :major-modes 'js-json-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "=-" '(json-pretty-print-buffer :which-key "built-in-format")
 "=p" '(json-pretty-print-buffer :which-key "pretty-p-buffer")
 "=o" '(json-pretty-print-ordered :which-key "pretty-p-ordered")
 "=r" '(json-pretty-print-buffer-ordered :which-key "pretty-p-buffer-ordered"))

;; nxml-mode
(general-define-key
 :states '(normal visual emacs)
 :keymaps 'nxml-mode-map
 :major-modes 'nxml-mode
 :prefix ","
 "="  '(:ignore t :which-key "format")
 "==" '(cfg/xmllint-format-buffer :which-key "xmllint-format-buffer")
 "=-" '(cfg/built-in-format-nxml :which-key "built-in-format")
 "=p" '(cfg/xmllint-format-buffer :which-key "xmllint-format-buffer"))

;; general.el for comments and marks
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-comment
 :major-modes list-gen-mode-comment
 :prefix ","
 "k"  '(:ignore t :which-key "comments")
 "kl" '(comment-dwim :which-key "comment-dwim")
 "ke" '(cfg/duplicate-and-comment-current-line-or-region :which-key "comment-dup-line-or-reg")
 "kk" '(mark-defun :which-key "mark-fun")
 "km" '(comment-region :which-key "comment-region")
 "kn" '(uncomment-region :which-key "uncomment-region")
 "kj" '(comment-line :which-key "comment-line")
 "kt" '(comment-kill :which-key "comment-kill"))

;; xref and ffap

;; ffap and imenu
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-ffap
 :major-modes list-gen-mode-ffap
 :prefix ","
 "/"  '(:ignore t :which-key "goto")
 "/."  '(ffap :which-key "act_ffap")
 "/'"  '(goto-line :which-key "goto-line")
 "//"  '(imenu :which-key "imenu"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(perl-mode-map cperl-mode-map)
 :major-modes '(perl-mode cperl-mode)
 :prefix ","
 "/." '(cfg/ffap :which-key "ffap_perl"))

;; xref
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-xref
 :major-modes list-gen-mode-xref
 :prefix ","
 ;; xref remapping with dumb-jump as a backend:
 "/"  '(:ignore t :which-key "goto")
 "/d" '(xref-find-definitions :which-key "xref-def")
 "/h" '(xref-find-references :which-key "xref-ref")
 "/b" '(xref-go-back :which-key "xref-go-back")
 "/B" '(xref-go-forward :which-key "xref-go-forward")
 "/s" '(xref-find-apropos :which-key "xref-apropos"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-xref
 :major-modes list-gen-mode-xref
 :prefix "\\"
 "d" '(xref-find-definitions :which-key "xref-def")
 "h" '(xref-find-references :which-key "xref-ref")
 "b" '(xref-go-back :which-key "xref-go-back")
 "B" '(xref-go-forward :which-key "xref-go-forward")
 "s" '(xref-find-apropos :which-key "xref-apropos"))

;; without prefix:
(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-xref
 :major-modes list-gen-mode-xref

 "gd" '(xref-find-definitions :which-key "xref-def")
 "gh" '(xref-find-references :which-key "xref-ref")
 "gb" '(xref-go-back :which-key "xref-go-back")
 "gB" '(xref-go-forward :which-key "xref-go-forward")
 "gs" '(xref-find-apropos :which-key "xref-apropos"))

;; various text manipulations

(general-define-key
 :states '(normal visual emacs)
 :keymaps list-gen-mode-map-txtman
 :major-modes list-gen-mode-txtman
 :prefix ","
 "."   '(:ignore t :which-key "txt-man")
 ".%"  '(string-rectangle :which-key "string-rectangle")
 ".'"  '(transpose-words :which-key "swap-words")
 ".;"  '(sort-words :which-key "sort-words")
 ".A"  '(align-regexp :which-key "align-regexp")
 ".G"  '(sort-paragraphs :which-key "sort-paragraphs")
 ".J"  '(cfg/join-lines-in-region :which-key "join-lines")
 ".T"  '(cfg/toggle-case-active :which-key "toggle-case-active")
 ".U"  '(cfg/unix2dos :which-key "unix2dos")
 ".a"  '(align :which-key "align")
 ".b"  '(delete-blank-lines :which-key "del-blank-lines")
 ".c"  '(center-region :which-key "center-region")
 ".d"  '(cfg/downcase-region :which-key "downcase")
 ".e"  '(cfg/duplicate-current-line-or-region :which-key "dup-line-or-reg")
 ".g"  '(fill-paragraph :which-key "fill-paragraph")
 ".j"  '(cfg/join-lines-in-region-add-spc :which-key "join-lines-spc")
 ".l"  '(kill-whole-line :which-key "kill-line")
 ".n"  '(transpose-lines :which-key "swap-lines")
 ".o"  '(cfg/eol-analyze :which-key "eol-analyze")
 ".p"  '(delete-duplicate-lines :which-key "del-dup-lines")
 ".r"  '(reverse-region :which-key "reverse-lines")
 ".s"  '(sort-lines :which-key "sort-lines")
 ".t"  '(cfg/toggle-case :which-key "toggle-case")
 ".u"  '(cfg/dos2unix :which-key "dos2unix")
 ".w"  '(delete-trailing-whitespace :which-key "delete-tr-whitespace")
 ".y"  '(untabify :which-key "untabify")
 ".i"  '(whitespace-mode :which-key "whitespace-mode")
 ".0"  '(whitespace-newline-mode :which-key "whitespace-newline-mode")
 ".z"  '(cfg/capitalize-region :which-key "capitalize"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(fundamental-mode-map text-mode-map)
 :major-modes '(fundamental-mode text-mode)
 :prefix ","
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(fundamental-mode-map text-mode-map)
 :major-modes '(fundamental-mode text-mode)
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer"))

;; ediff mode

;; ediff-mode (without general.el)
;; This mode is special - unfortunately, general.el doesn't work in the usual way in ediff-mode...

;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html#Customization

(require 'ediff)
(require 'evil-collection)

;; remove evil-collection:
(remove-hook 'ediff-keymap-setup-hook 'evil-collection-ediff-startup-hook)

;; Adjust long help messages to reflect cfg/ediff-bindings bindings:

(defconst ediff-long-help-message-compare3
  "
[c,k  -previous diff |     | -vert/horiz split   | dp/a -copy A to B/C
]c,j  -next diff     |     h -highlighting       | do/b -copy B to A/B
zd,zj -jump to diff  |     @ -auto-refinement    | dl/c -copy C to A/B
   gx -goto X's point|    ## -ignore whitespace  | rx   -restore buf X's old diff
  C-l -recenter      |    #c -ignore case        | *    -refine current region
C-u/d -scroll up/dn  | #f/#h -focus/hide regions | !    -update diff regions
zh/zl -scroll lt/rt  |     X -read-only in buf X | wx   -save buf X
    ~ -rotate buffers|     m -wide display       | wd   -save diff output
"
  "Help message usually used for 3-way comparison.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-compare2
  "
[c,k  -previous diff |     | -vert/horiz split   |dp/a do/b -copy A/B's to B/A
]c,j  -next diff     |     H -highlighting       | h j k l  -move/copy (vim style)
zd,zj -jump to diff  |     @ -auto-refinement    | rx       -restore old buf X 
   gx -goto X's point|    ## -ignore whitespace  |  !       -update diff regions
  C-l -recenter      |    #c -ignore case        |  *       -refine current region
C-u/d -scroll up/dn  | #f/#h -focus/hide regions |
zh/zl -scroll lt/rt  |     X -read-only in buf X | wx -save buf X
    ~ -swap variants |     m -wide display       | wd -save diff output
"
  "Help message usually used for 2-way comparison.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-tail
  "=====================|===========================|=============================
    R -show registry |    = -compare regions     |  ? -help off/on 
    D -diff output   |    E -browse Ediff manual |  q -quit (exit)
    i -status info   |    M -show session group  |C-z -suspend (drop to registry)"
  "The tail of the full-help message.")

(defconst ediff-long-help-message-merge
  "
[c,k  -previous diff |     | -vert/horiz split   |dX,x -copy buf X's region to C
]c,j  -next diff     |     h -highlighting       |   r -restore buf C's old diff
zd,zj -jump to diff  |     @ -auto-refinement    |   * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |   ! -update diff regions
  C-l -recenter      | #f/#h -focus/hide regions |   + -combine diff regions
C-u/d -scroll up/dn  |     X -read-only in buf X |  wx -save buf X (wc is for C)
zh/zl -scroll lt/rt  |     m -wide display       |  wd -save diff output
    ~ -swap variants |     s -shrink window C    |   / -show/hide ancestor buff
                     |  $$ -show clashes only    |   & -merge w/new default
                     |  $* -skip changed regions | j k -move (vim style)
"
  "Help message for merge sessions.
Normally, not a user option.  See `ediff-help-message' for details.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cfg/ediff-bindings
  '(("H"    . ediff-toggle-hilit)
    ("\C-e" . evil-collection-ediff-scroll-down-1)
    ("\C-y" . evil-collection-ediff-scroll-up-1)
    ("j"    . ediff-next-difference)
    ("k"    . ediff-previous-difference)
    ("N"    . ediff-previous-difference)
    ("gg"   . evil-collection-ediff-first-difference)
    ("G"    . evil-collection-ediff-last-difference)
    ("\C-d" . evil-collection-ediff-scroll-down)
    ("\C-u" . evil-collection-ediff-scroll-up)
    ("\C-z" . ediff-suspend)
    ("z"    . nil)
    ("zl"   . evil-collection-ediff-scroll-right)
    ("zh"   . evil-collection-ediff-scroll-left)

    ;; vimdiff compability:
    ("]c"   . ediff-next-difference)
    ("[c"   . ediff-previous-difference)

    ("dp"   . ediff-copy-A-to-B)
    ("dP"   . ediff-copy-A-to-C)
    ("da"   . ediff-copy-A-to-B)
    ("dA"   . ediff-copy-A-to-C)

    ("do"   . ediff-copy-B-to-A)
    ("dO"   . ediff-copy-B-to-C)
    ("db"   . ediff-copy-B-to-A)
    ("dB"   . ediff-copy-B-to-C)

    ("dl"   . ediff-copy-C-to-A)
    ("dL"   . ediff-copy-C-to-B)
    ("dc"   . ediff-copy-C-to-A)
    ("dC"   . ediff-copy-C-to-B)

    ("zd"   . ediff-jump-to-difference)
    ("zj"   . ediff-jump-to-difference))
  "A list of bindings changed/added in evil-ediff.")

(defun cfg/ediff-startup-hook ()
  "Place cfg/ediff-bindings in `ediff-mode-map'."
  (evil-make-overriding-map ediff-mode-map 'normal)
  (dolist (entry cfg/ediff-bindings)
    (define-key ediff-mode-map (car entry) (cdr entry)))
  (unless (or ediff-3way-comparison-job
              (eq ediff-split-window-function 'split-window-vertically))
    (define-key ediff-mode-map "l" 'ediff-copy-A-to-B)
    (define-key ediff-mode-map "h" 'ediff-copy-B-to-A))
  (evil-normalize-keymaps)
  nil)

;; add above keybindings:
(evil-set-initial-state 'ediff-mode 'normal)
(add-hook 'ediff-keymap-setup-hook 'cfg/ediff-startup-hook)

;; tweaks for other modes:

(defvar cfg/ediff-help-changed nil)

(defvar cfg/ediff-initial-state-backup (evil-initial-state 'ediff-mode))
(defvar cfg/ediff-long-help-message-compare2-backup ediff-long-help-message-compare2)
(defvar cfg/ediff-long-help-message-compare3-backup  ediff-long-help-message-compare3)
(defvar cfg/ediff-long-help-message-narrow2-backup  ediff-long-help-message-narrow2)
(defvar cfg/ediff-long-help-message-word-backup  ediff-long-help-message-word-mode)
(defvar cfg/ediff-long-help-message-merge-backup  ediff-long-help-message-merge)
(defvar cfg/ediff-long-help-message-head-backup  ediff-long-help-message-head)
(defvar cfg/ediff-long-help-message-tail-backup  ediff-long-help-message-tail)

(defun cfg/ediff-adjust-help ()
  "Adjust long help messages to reflect cfg/ediff-bindings bindings: for evil-collection and pure ediff."
  (unless cfg/ediff-help-changed
    (dolist (msg '(ediff-long-help-message-compare2
                   ediff-long-help-message-compare3
                   ediff-long-help-message-narrow2
                   ediff-long-help-message-word-mode
                   ediff-long-help-message-merge
                   ediff-long-help-message-head
                   ediff-long-help-message-tail))
      (dolist (chng '(( " rx -restore buf X's old diff" . " rx       -restore old buf X")
                      ;; ( "* -refine current region"     .    "*       -refine current region")
                      ;; ( "  ! -update diff regions" .      "  !       -update diff regions")
		      ( "p,DEL -previous diff " . "[c,k  -previous diff ")
		      ( "k,N,p -previous diff " . "[c,k  -previous diff ")
		      ( "n,SPC -next diff     " . "]c,j  -next diff     ")
		      ( "  j,n -next diff     " . "]c,j  -next diff     ")
                      ("    j -jump to diff  " . "zd,zj -jump to diff  ")
                      ("    d -jump to diff  " . "zd,zj -jump to diff  ")
                      ;; ("    h -highlighting  " . "    H -highlighting  ")
                      ("  v/V -scroll up/dn  " . "C-u/d -scroll up/dn  ")
                      ("  </> -scroll lt/rt  " . "zh/zl -scroll lt/rt  ")
                      ("  z/q -suspend/quit"   . "C-z/q -suspend/quit")))
        (setf (symbol-value msg)
              (replace-regexp-in-string (car chng) (cdr chng) (symbol-value msg))))))
  (setq cfg/ediff-help-changed t))

(cfg/ediff-adjust-help)

;; ibuffer-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(ibuffer-mode-map)
 :major-modes '(ibuffer-mode)
 :prefix ","
 ","  '(ibuffer-toggle-sorting-mode :which-key "toggle-sorting-mode")
 "!"  '(ibuffer-do-shell-command-file :which-key "do-shell-command-file")
 "*"  '(:ignore t :which-key "mark")
 "**" '(ibuffer-unmark-all :which-key "unmark-all")
 "*." '(ibuffer-mark-old-buffers :which-key "mark-old-buffers")
 "*/" '(ibuffer-mark-dired-buffers :which-key "mark-dired-buffers")
 "*M" '(ibuffer-mark-by-mode :which-key "mark-by-mode")
 "*c" '(ibuffer-change-marks :which-key "change-marks")
 "*e" '(ibuffer-mark-dissociated-buffers :which-key "mark-dissociated")
 "*h" '(ibuffer-mark-help-buffers :which-key "mark-help")
 "*m" '(ibuffer-mark-modified-buffers :which-key "mark-modified")
 "*r" '(ibuffer-mark-read-only-buffers :which-key "mark-read-only")
 "*s" '(ibuffer-mark-special-buffers :which-key "mark-special")
 "*t" '(ibuffer-toggle-marks :which-key "toggle-marks")
 "*u" '(ibuffer-mark-unsaved-buffers :which-key "mark-unsaved")
 "*z" '(ibuffer-mark-compressed-file-buffers :which-key "mark-compressed-file")
 "/"  '(:ignore t :which-key "filter")
 "/ <RET>" '(ibuffer-filter-by-mode :which-key "by-mode")
 "/ <SPC>" '(ibuffer-filter-chosen-by-completion :which-key "filter-by-completion")
 "/ <TAB>"   '(ibuffer-exchange-filters :which-key "exchange-filters")
 "/!"  '(ibuffer-negate-filter :which-key "negate-filter")
 "/&"  '(ibuffer-and-filter :which-key "and-filter")
 "/*"  '(ibuffer-filter-by-starred-name :which-key "by-starred-name")
 "//"  '(ibuffer-filter-disable :which-key "disable-filter")
 "/-"  '(ibuffer-filter-disable :which-key "disable-filter")
 "/." '(ibuffer-filter-by-file-extension :which-key "by-extension")
 "/<"  '(ibuffer-filter-by-size-lt :which-key "size <")
 "/>"  '(ibuffer-filter-by-size-gt :which-key "size >")
 "/C"  '(ibuffer-filter-by-content :which-key "by-content")
 "/D"  '(ibuffer-decompose-filter-group :which-key "decompose-group")
 "/E"  '(ibuffer-filter-by-process :which-key "by-process")
 "/F"  '(ibuffer-filter-by-directory :which-key "by-directory")
 "/M"  '(ibuffer-filter-by-derived-mode :which-key "by-derived-mode")
 "/P"  '(ibuffer-pop-filter-group :which-key "pop-filter-group")
 "/R"  '(ibuffer-switch-to-saved-filter-groups :which-key "switch-to-saved-groups")
 "/S <up>"   '(ibuffer-pop-filter :which-key "pop-filter")
 "/S"  '(ibuffer-save-filter-groups :which-key "save-filter-groups")
 "/S" '(:ignore t :which-key "pop-filter")
 "/Sg" '(ibuffer-pop-filter-group :which-key "pop-filter-group")
 "/X"  '(ibuffer-delete-saved-filter-groups :which-key "delete-saved-groups")
 "/a"  '(ibuffer-add-saved-filters :which-key "add-saved-filters")
 "/b"  '(ibuffer-filter-by-basename :which-key "by-basename")
 "/c"  '(ibuffer-clear-filter-groups :which-key "clear-filter-groups")
 "/d"  '(ibuffer-decompose-filter :which-key "decompose-filter")
 "/e"  '(ibuffer-filter-by-predicate :which-key "by-predicate")
 "/f"  '(ibuffer-filter-by-filename :which-key "by-filename")
 "/g"  '(ibuffer-filters-to-filter-group :which-key "filters->group")
 "/i"  '(ibuffer-filter-by-modified :which-key "by-modified")
 "/m"  '(ibuffer-filter-by-used-mode :which-key "by-used-mode")
 "/n"  '(ibuffer-filter-by-name :which-key "by-name")
 "/o"  '(ibuffer-or-filter :which-key "or-filter")
 "/p"  '(ibuffer-pop-filter :which-key "pop-filter")
 "/r"  '(ibuffer-switch-to-saved-filters :which-key "switch-to-saved")
 "/s"  '(ibuffer-save-filters :which-key "save-filters")
 "/t"  '(ibuffer-exchange-filters :which-key "exchange-filters")
 "/v"  '(ibuffer-filter-by-visiting-file :which-key "by-visiting-file")
 "/x"  '(ibuffer-delete-saved-filters :which-key "delete-saved")
 "/y"  '(ibuffer-or-filter :which-key "or-filter")
 "="  '(ibuffer-diff-with-file :which-key "diff-with-file")
 "D"  '(ibuffer-do-delete :which-key "delete/kill buffer")
 "I"  '(ibuffer-do-query-replace-regexp :which-key "query-replace-regexp")
 "L"  '(ibuffer-do-toggle-lock :which-key "toggle-lock")
 "M"  '(ibuffer-do-toggle-modified :which-key "toggle-modified")
 "O"  '(ibuffer-do-occur :which-key "occur")
 "Q"  '(ibuffer-do-query-replace :which-key "query-replace")
 "R"  '(ibuffer-do-rename-uniquely :which-key "rename-uniquely")
 "RET" '(ibuffer-visit-buffer :which-key "visit-buffer")
 "S"  '(ibuffer-do-save :which-key "save")
 "T"  '(ibuffer-do-toggle-read-only :which-key "toggle-read-only")
 "d"  '(ibuffer-mark-for-delete :which-key "mark-delete")
 "f <up>" '(ibuffer-pop-filter :which-key "pop-filter")
 "f"  '(:ignore t :which-key "basic filters")
 "f*" '(ibuffer-filter-by-starred-name :which-key "by-starred-name")
 "f-" '(ibuffer-filter-disable :which-key "disable-filter")
 "f." '(ibuffer-filter-by-file-extension :which-key "by-extension")
 "fC"  '(ibuffer-filter-by-content :which-key "by-content")
 "fF" '(ibuffer-filter-by-directory :which-key "by-directory")
 "fb" '(ibuffer-filter-by-basename :which-key "by-basename")
 "fi" '(ibuffer-filter-by-modified :which-key "by-modified")
 "fm" '(ibuffer-filter-by-used-mode :which-key "by-mode")
 "fn" '(ibuffer-filter-by-name :which-key "by-name")
 "g"  '(ibuffer-update :which-key "update")
 "j"  '(ibuffer-jump-to-buffer :which-key "jump-to-buffer")
 "m"  '(ibuffer-mark-forward :which-key "mark")
 "o"  '(ibuffer-visit-buffer-other-window :which-key "visit-other-window")
 "p" '(:ignore t :which-key "groups")
 "pd" '(ibuffer-delete-saved-filter-groups :which-key "delete-filter-groups")
 "pr" '(ibuffer-switch-to-saved-filter-groups :which-key "restore-filter-groups")
 "ps" '(ibuffer-save-filter-groups :which-key "save-filter-groups")
 "q"  '(kill-buffer-and-window :which-key "quit")
 "r"  '(ibuffer-do-replace-regexp :which-key "replace-regexp")
 "s"  '(:ignore t :which-key "sort")
 "sS" 'ibuffer-do-sort-by-size
 "sa" '(ibuffer-do-sort-by-alphabetic :which-key "s-by-name")
 "sf" '(ibuffer-do-sort-by-filename/process :which-key "s-by-filename")
 "si" '(ibuffer-invert-sorting :which-key "invert-sorting")
 "sm" '(ibuffer-do-sort-by-major-mode :which-key "s-by-major-mode")
 "so" '(ibuffer-do-sort-by-mode-name :which-key "s-by-mode-name")
 "sp" '(ibuffer-do-sort-by-filename/process :which-key "s-by-process")
 "ss" '(ibuffer-do-sort-by-size :which-key "s-by-size")
 "st" '(ibuffer-toggle-sorting-mode :which-key "toggle-sorting-mode")
 "sv" '(ibuffer-do-sort-by-recency :which-key "s-by-recency")
 "t"  '(:ignore t :which-key "toggle")
 "tt" '(ibuffer-toggle-marks :which-key "t-marks")
 "t*" '(ibuffer-toggle-marks :which-key "t-marks")
 "ts" '(ibuffer-toggle-sorting-mode :which-key "t-sorting-mode")
 "tT" '(ibuffer-do-toggle-read-only :which-key "t-read-only")
 "tr" '(ibuffer-do-toggle-read-only :which-key "t-read-only")
 "tM" '(ibuffer-do-toggle-modified :which-key "t-modified")
 "tm" '(ibuffer-do-toggle-modified :which-key "t-modified")
 "tL" '(ibuffer-do-toggle-lock :which-key "t-lock")
 "tl" '(ibuffer-do-toggle-lock :which-key "t-lock")
 "u"  '(ibuffer-unmark-forward :which-key "unmark")
 "v"  '(ibuffer-do-view :which-key "view")
 "x"  '(ibuffer-do-kill-on-deletion-marks :which-key "execute-delete"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(ibuffer-mode-map)
 :major-modes '(ibuffer-mode)
 "q"  'kill-buffer-and-window
 "Q"  'kill-this-buffer
 )

(with-eval-after-load 'ibuffer

  ;; Binds in the correct map (ibuffer-name-map)
  (define-key ibuffer-name-map [mouse-1] #'ibuffer-mouse-visit-buffer)  ; Mouse Left Click: visit-buffer
  (define-key ibuffer-name-map [mouse-2] #'ibuffer-mouse-toggle-mark)
  (define-key ibuffer-name-map [mouse-8] #'ibuffer-mouse-toggle-mark)
  (define-key ibuffer-name-map [mouse-9] #'ibuffer-mouse-toggle-mark)

  ;; (define-key ibuffer-name-map [down-mouse-3] nil)  ; Disable popup on right click
  ;; (define-key ibuffer-name-map [mouse-3] #'cfg/ibuffer-mouse-mark)  ; Mouse Right Click: select buffer
  )

(defun cfg/ibuffer-disable-mouse-tooltips ()
  "Disable mouse tooltips (help-echo) in `ibuffer-mode`."
  (setq-local help-at-pt-display-when-idle nil)
  (setq-local tooltip-mode nil)
  (with-silent-modifications
    (remove-text-properties (point-min) (point-max) '(help-echo nil))))
(add-hook 'ibuffer-mode-hook #'cfg/ibuffer-disable-mouse-tooltips)

;; emacs-lisp-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 :major-modes '(emacs-lisp-mode lisp-interaction-mode)
 :prefix ","
 "e"    '(:ignore t :which-key "eval-elisp")
 "ee"   '(eval-region :which-key "eval-region")
 "er"   '(eval-buffer :which-key "eval-buffer"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(emacs-lisp-mode-map)
 :major-modes '(emacs-lisp-mode)
 :prefix ","
 "c"    '(emacs-lisp-native-compile-and-load :which-key "compile-and-load"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(emacs-lisp-mode-map)
 :major-modes '(emacs-lisp-mode)
 "gr" '(revert-buffer :which-key "revert-buffer"))

;; python-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'python-mode-map
 :major-modes 'python-mode
 :prefix ","
 "h"  '(python-check :which-key "python-check")
 "s"  '(run-python :which-key "python-shell")
 "v"  '(:ignore t :which-key "venv")
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
 "e"  '(:ignore t :which-key "eval")
 "ee" '(python-shell-send-region :which-key "eval-region")
 "ef" '(python-shell-send-file :which-key "select-and-eval-file")
 "eb" '(python-shell-send-buffer :which-key "eval-buffer")
 "es" '(python-shell-send-string :which-key "eval-given-string")
 "et" '(python-shell-send-statement :which-key "eval-statement")
 "ed" '(python-shell-send-defun :which-key "eval-defun")
 "ep" '(python-shell-package-enable :which-key "package-enable"))

;; xref should be used with `anaconda-mode' (because of improved functionality):

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'python-mode-map
 :major-modes 'python-mode
 "gb" '(xref-go-back :which-key "xref-go-back")
 "gB" '(xref-go-forward :which-key "xref-go-forward"))

;; inferior-python-mode - no prefix

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'inferior-python-mode-map
 :major-modes 'inferior-python-mode
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer"))

;; cperl

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(perl-mode-map cperl-mode-map perl-ts-mode-map)
 :major-modes '(perl-mode cperl-mode perl-ts-mode)
 :prefix ","
 "!"  '(executable-interpret :which-key "exec-script")
 "o"  '(cperl-perldoc-at-point :which-key "perldoc-at-point")
 "O"  '(cperl-perldoc :which-key "cperl-perldoc")
 "t"  '(:ignore t :which-key "toggle")
 "te" '(cperl-toggle-electric :which-key "toggle-electric")
 "d"  '(cperl-db :which-key "debugger")
 "q"  '(:ignore t :which-key "quotes")
 "qi" '(perl-quote-single :which-key "quote-single")
 "qo" '(perl-quote-double :which-key "quote-double")
 "m"  '(:ignore t :which-key "modules")
 "ma" '(cfg/ffap :which-key "ffap_perl")
 "mm" '(cfg/find-perl-module :which-key "find-perl-module")
 "n"  '(mark-defun :which-key "mark-sub"))

;; cc-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(cc-mode-map c-mode-map c++-mode-map c-ts-mode-map)
 :major-modes '(cc-mode c-mode c++-mode c-ts-mode)
 :prefix ","
 "c"  '(compile :which-key "compile"))

;; compilation-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(compilation-mode-map)
 :major-modes '(compilation-mode)
 :prefix ","
 "k"  '(kill-compilation :which-key "kill-compilation")
 "]"  '(compilation-next-error :which-key "next-error")
 "["  '(compilation-previous-error :which-key "prev-error")
 "j"  '(compilation-recompile :which-key "recompile")
 "s"  '(compilation-shell-minor-mode :which-key "compilation-shell-minor-mode")
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs Man-mode)
 :keymaps 'compilation-mode-map
 :major-modes 'compilation-mode
 "Q" 'kill-this-buffer
 "q" 'kill-buffer-and-window)

;; sgml-mode and html and mhtml

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(web-mode-map mhtml-mode-map html-mode-map html-ts-mode-map sgml-mode-map nxml-mode-map)
 :major-modes '(web-mode mhtml-mode html-mode html-ts-mode sgml-mode nxml-mode)
 :prefix ","

 "g"  '(:ignore t :which-key "sgml")
 "g/" '(sgml-close-tag :which-key "close-tag")
 "g8" '(sgml-name-8bit-mode :which-key "name-8bit")
 "g<" '(sgml-skip-tag-backward :which-key "skip-backward")
 "g>" '(sgml-skip-tag-forward :which-key "skip-forward")
 "g?" '(sgml-tag-help :which-key "help")
 "ga" '(sgml-attributes :which-key "attributes")
 "gb" '(sgml-skip-tag-backward :which-key "skip-backward")
 "gd" '(sgml-delete-tag :which-key "delete-tag")
 "gf" '(sgml-skip-tag-forward :which-key "skip-forward")
 "gi" '(sgml-tags-invisible :which-key "tags-invisible")
 "gn" '(sgml-name-char :which-key "name-char")
 "gt" '(sgml-tag :which-key "tag")

 "h"  '(:ignore t :which-key "html")
 "h#" '(html-id-anchor :which-key "id-anchor")
 "h-" '(html-horizontal-rule :which-key "hrule")
 "h1" '(html-headline-1 :which-key "headline-1")
 "h2" '(html-headline-2 :which-key "headline-2")
 "h3" '(html-headline-3 :which-key "headline-3")
 "h4" '(html-headline-4 :which-key "headline-4")
 "h5" '(html-headline-5 :which-key "headline-5")
 "h6" '(html-headline-6 :which-key "headline-6")
 "hc" '(html-checkboxes :which-key "checkboxes")
 "hf" '(html-href-anchor-file :which-key "href-file")
 "hh" '(html-href-anchor :which-key "href")
 "hi" '(html-image :which-key "image")
 "hj" '(html-line :which-key "line-break")
 "hl" '(html-list-item :which-key "list-item")
 "hn" '(html-name-anchor :which-key "name-anchor")
 "ho" '(html-ordered-list :which-key "orlist")
 "hp" '(html-paragraph :which-key "paragraph")
 "hr" '(html-radio-buttons :which-key "radio")
 "hs" '(html-autoview-mode :which-key "autoview")
 "hu" '(html-unordered-list :which-key "unlist")
 "hv" '(browse-url-of-buffer :which-key "browse-url"))

;; nxml-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'nxml-mode-map
 :major-modes 'nxml-mode
 :prefix ","
 "v"  '(cfg/xml-xsd-validate :which-key "validate-xsd-xmllint"))

;; javascript/json

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(js-json-mode-map js-ts-mode-map js-mode-map)
 :major-modes '(js-json-mode js-ts-mode js-mode)
 :prefix ",")

;; "shell-script-mode is an alias for `sh-mode' in `sh-script.el'."

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(sh-mode-map bash-ts-mode-map)
 :major-modes '(sh-mode bash-ts-mode)
 :prefix ","
 "b" '(sh-backslash-region :which-key "backslash-region")
 "#"  '(sh-set-shell :which-key "set-shell")
 "!"  '(executable-interpret :which-key "exec-script")
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
 "cg" '(sh-while-getopts :which-key "while-getopts"))

;; shell-mode - interactive terminal (but not sh-mode)

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(shell-mode-map)
 :major-modes 'shell-mode
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "c" '(comint-clear-buffer :which-key "clear")
 "i" '(comint-send-invisible :which-key "send-invisible")
 "f" '(find-file :which-key "find-file"))

;; term

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(term-mode-map term-raw-map)
 :major-modes 'term-mode
 :prefix ","
 "," '(ffap :which-key "act_ffap")
 "f" '(find-file :which-key "find-file"))

(general-define-key
 :states '(normal visual emacs insert)
 :keymaps '(term-mode-map term-raw-map)
 :major-modes 'term-mode
 "S-<up>" 'rename-buffer
 "S-<down>" 'cfg/multi-term-buffer-rn
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
 "C-," 'term-send-eof)

;; grep/wgrep mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(grep-mode-map wgrep-mode-map)
 :major-modes '(grep-mode wgrep-mode)
 :prefix ","
 "/" '(ffap :which-key "act_ffap")
 "g" '(grep-mode :which-key "grep-mode")
 "p" '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
 "e" '(wgrep-exit :which-key "wg-exit")
 "a" '(wgrep-save-all-buffers :which-key "wg-save-all-buffers")
 "Z" '(wgrep-finish-edit :which-key "wg-finish-edit")
 "Q" '(wgrep-abort-changes :which-key "wg-abort-changes")
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "r" '(wgrep-toggle-readonly-area :which-key "wg-toggle-readonly-area"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(grep-mode-map)
 :major-modes '(grep-mode)
 "g" '(grep-mode :which-key "grep-mode")
 "p" '(wgrep-change-to-wgrep-mode :which-key "wgrep-change-to-wgrep-mode")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer")
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "r" '(wgrep-toggle-readonly-area :which-key "wg-toggle-readonly-area"))

;; recentf

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(recentf-dialog-mode-map)
 :major-modes '(recentf-dialog-mode)
 "/" 'evil-ex-search-forward
 "N" 'evil-ex-search-previous
 "n" 'evil-ex-search-next
 )

;; dired

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(dired-mode-map wdired-mode-map)
 :major-modes '(dired-mode wdired-mode)
 "q" 'kill-this-buffer)

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(dired-mode-map wdired-mode-map)
 :major-modes '(dired-mode wdired-mode)
 :prefix ","
 "l"  '(cfg/cycle-dired-switches :which-key "cycle-dired-switches")
 ";"  '(wdired-change-to-wdired-mode :which-key "wdired-mode")
 "'"  '(wdired-exit :which-key "wdired-exit")
 "C"  '(dired-do-copy :which-key "do-copy")
 "E"  '(dired-do-open :which-key "do-open")
 "="  '(dired-diff :which-key "diff")
 "M"  '(dired-do-chmod :which-key "do-chmod")
 "O"  '(dired-do-chown :which-key "do-chown")
 "T"  '(dired-do-touch :which-key "do-touch")
 "R"  '(dired-do-rename :which-key "do-rename")
 "D"  '(dired-do-delete :which-key "do-delete")
 "S"  '(dired-do-symlink :which-key "do-symlink")
 "Z"  '(dired-do-compress :which-key "do-compress")
 "H"  '(dired-do-hardlink :which-key "do-hardlink")
 "|"  '(dired-do-redisplay :which-key "do-redisplay")
 "+"  '(dired-create-directory :which-key "create-directory")
 "^"  '(dired-up-directory :which-key "up-directory")
 "Q"  '(dired-do-find-regexp-and-replace :which-key "find-regexp-and-replace")
 "!"  '(dired-do-shell-command :which-key "do-shell-command")
 "m"  '(dired-mark :which-key "mark")
 "x"  '(dired-do-flagged-delete :which-key "do-flagged-delete")
 "u"  '(dired-umark :which-key "umark")
 "U"  '(dired-unmark-all-marks :which-key "unmark-all-marks")
 "t"  '(dired-toggle-marks :which-key "toggle-marks")
 "*"  '(:ignore t :which-key "mark")
 "**" '(dired-mark-executables :which-key "mark-executables")
 "*!" '(dired-unmark-all-marks :which-key "unmark-all-marks")
 "*%" '(dired-mark-files-regexp :which-key "mark-files-regexp")
 "*/" '(dired-mark-directories :which-key "mark-directories")
 "*?" '(dired-unmark-all-files :which-key "unmark-all-files")
 "*@" '(dired-mark-symlinks :which-key "mark-symlinks")
 "*c" '(dired-change-marks :which-key "change-marks")
 "*m" '(dired-mark :which-key "dired-mark")
 "*t" '(dired-toggle-marks :which-key "toggle-marks")
 "*u" '(dired-unmark :which-key "unmark"))

;; org-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(org-mode-map org-static-blog-mode-map)
 :major-modes '(org-mode org-static-blog-mode)
 :prefix ","
 "c" '(org-ctrl-c-ctrl-c :which-key "org-ctrl-c-ctrl-c")
 "p" '(org-priority :which-key "priority")
 "g" '(org-goto :which-key "goto")
 "y" '(org-cycle :which-key "cycle")

 "r" '(:ignore t :which-key "regions")

 "t" '(:ignore t :which-key "table")
 "te" '(org-table-export :which-key "export")
 "tt" '(org-table-create :which-key "create")
 "ty" '(org-table-import :which-key "import")
 "ts" '(org-table-sort-lines :which-key "sort-lines")

 "e" '(:ignore t :which-key "export")
 "ed" '(org-md-export-to-markdown :which-key "export-to-md")
 "eh" '(org-html-export-to-html :which-key "export-to-html")

 "i" '(:ignore t :which-key "insert")
 "iH" '(org-insert-heading-after-current :which-key "heading-after-current")
 "ia" '(org-set-tags-command :which-key "tags-command")
 "ib" '(org-insert-structure-template :which-key "structure-template")
 "ic" '(org-attach :which-key "org-attach")
 "id" '(org-insert-drawer :which-key "drawer")
 "ie" '(org-set-effort :which-key "set-effort")
 "if" '(org-footnote-new :which-key "footnote-new")
 "ih" '(org-insert-heading :which-key "heading")
 "ii" '(org-insert-item :which-key "item")
 "il" '(org-insert-link :which-key "link")
 "in" '(org-add-note :which-key "add-note")
 "ip" '(org-set-property :which-key "property")
 "is" '(org-insert-subheading :which-key "subheading")
 "it" '(org-timer :which-key "timer")

 "b"  '(:ignore t :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle"))

;; help-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(help-mode-map)
 :major-modes '(help-mode)
 :prefix ","
 "/" '(ffap :which-key "act_ffap")
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(help-mode-map)
 :major-modes '(help-mode)
 "q" '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q" '(kill-this-buffer :which-key "kill-this-buffer"))

;; Man-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps '(Man-mode-map)
 :major-modes '(Man-mode)
 :prefix ","
 "q"  '(kill-buffer-and-window :which-key "kill-buffer-and-window")
 "Q"  '(kill-this-buffer :which-key "kill-this-buffer"))

(general-define-key
 :states '(normal visual emacs Man-mode)
 :keymaps 'Man-mode-map
 :major-modes 'Man-mode
 "gc" '(Man-goto-section :which-key "Man-goto-section")
 "gb" '(Man-goto-see-also-section :which-key "Man-goto-see-also")
 "Q" 'kill-this-buffer
 "q" 'kill-buffer-and-window)

;; doc-view mode

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
 "sl" '(doc-view-set-slice :which-key "set-slice"))

;; doc-view mode ; without prefix

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'doc-view-mode-map
 :major-modes 'doc-view-mode
 "C-<mouse-4>" 'doc-view-enlarge
 "C-<mouse-5>"'doc-view-shrink
 "<right>" 'doc-view-next-page
 "<left>" 'doc-view-previous-page
 "<home>" 'doc-view-first-page
 "<end>" 'doc-view-last-page
 "k" 'doc-view-shrink
 "l" 'doc-view-enlarge
 "/" 'doc-view-search
 "W" 'doc-view-fit-width-to-window
 "H" 'doc-view-fit-height-to-window
 "P" 'doc-view-fit-page-to-window
 "0" 'doc-view-scale-reset)

;; image-mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'image-mode-map
 :major-modes 'image-mode
 "q" 'kill-this-buffer
 "<left>" 'image-previous-file
 "<right>" 'image-next-file
 "L" 'image-rotate
 "R" 'image-rotate
 "." 'image-previous-frame)

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'image-mode-map
 :major-modes 'image-mode
 :prefix ","
 "/" '(ffap :which-key "act_ffap")
 "d" '(image-dired :which-key "image-dired")
 "c" '(image-toggle-display :which-key "toggle-display")
 "<left>" '(image-previous-file :which-key "prev-file")
 "<right>" '(image-next-file  :which-key "next-file")
 "L" '(image-rotate :which-key "rotate")
 "R" '(image-rotate  :which-key "rotate")
 "H" '(image-transform-fit-to-height :which-key "fit-to-height")
 "W" '(image-transform-fit-to-width :which-key "fit-to-width")
 "O" '(image-transform-original :which-key "original-size")
 "RET" '(image-toggle-animation :which-key "animation-toggle")
 "." '(image-previous-frame :which-key "animation-prev-frame")
 ";" '(image-next-frame  :which-key "animation-next-frame")

 ;; animations, gifs...

 "a"   '(:ignore t :which-key "animations")
 "aa" '(image-toggle-animation :which-key "animation-RET-toggle")
 "a." '(image-previous-frame :which-key "animation-prev-frame")
 "a;" '(image-next-frame  :which-key "animation-next-frame")
 "a0" '(image-reset-speed :which-key "animation-reset-speed")
 "ar" '(image-reverse-speed  :which-key "animation-reverse-speed")
 "aF" '(image-goto-frame :which-key "animation-goto-frame")
 "a{" '(image-decrease-speed  :which-key "animation-decrease-speed")
 "a}" '(image-increase-speed :which-key "animation-increase-speed"))

(provide 'cfg-gen-core)
;;; cfg-gen-core.el ends here
