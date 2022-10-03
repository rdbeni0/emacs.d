;; ediff-mode (without general.el)
;; This mode is special - unfortunately, general.el doesn't work in the usual way in ediff-mode...

;; https://www.gnu.org/software/emacs/manual/html_mono/ediff.html#Customization

(require 'ediff)
(require 'evil-collection)

;; remove evil-collection:
(remove-hook 'ediff-keymap-setup-hook 'evil-collection-ediff-startup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
