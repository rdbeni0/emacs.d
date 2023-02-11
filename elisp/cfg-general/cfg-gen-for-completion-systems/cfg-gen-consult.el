;; no space + which-key + no whick-key : normal mode

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'global
 "g/" '(consult-git-grep :which-key "git-grep")
 "g'" '(consult-git-grep :which-key "git-grep")
 )

;; space as leader-key + which-key

(general-define-key
 :states '(normal visual emacs)
 :keymaps 'override
 :prefix "SPC"

 ;; macros:

 "Q"   '(:ignore t :which-key "macros")
 "QQ"  '(evil-record-macro :which-key "evil-record-macro")
 "Q@"  '(evil-execute-macro :which-key "evil-execute-maco")

 ;; files

 "fr"  '(consult-recent-file :which-key "recentf")
 "fb"  '(consult-bookmark :which-key "bookmark-jump")
 "fBb" '(consult-bookmark :which-key "bookmark-jump")

 ;; buffers

 "bb"  '(consult-buffer :which-key "switch-to-buffer-buffers")

 ;; completions

 "<f5>"     '(:ignore t :which-key "completions")
 "<f5><f4>" '(completion-at-point :which-key "completion-at-point-capf")
 "<f5><f5>" '(company-files :which-key "company-files")
 "<f5><f6>" '(dabbrev-expand :which-key "dabbrev-expand")
 "<f5><f7>" '(company-ispell :which-key "company-ispell")

 ;; search

 "s"   '(:ignore t :which-key "search")
 "s/"  '(consult-git-grep :which-key "git-grep")
 "s'"  '(consult-git-grep :which-key "git-grep")
 "sj"  '(consult-imenu :which-key "imenu")
 "sl"  '(consult-line :which-key "consult-line")
 "sL"  '(consult-line-multi :which-key "consult-line-multi")
 "sg"  '(:ignore t :which-key "git-grep")
 "sg/" '(consult-git-grep :which-key "git-grep")
 "sg'" '(consult-git-grep :which-key "git-grep")

 ;; help

 "h"   '(:ignore t :which-key "help")
 "hi"  '(info :which-key "info")
 "hd"  '(:ignore t :which-key "describe")
 "hdf" '(describe-function :which-key "describe-function")
 "hdk" '(describe-key :which-key "describe-key")
 "hdy" '(describe-keymap :which-key "describe-mode")
 "hdo" '(describe-font :which-key "describe-font")
 "hdm" '(describe-mode :which-key "describe-mode")
 "hdi" '(describe-minor-mode :which-key "describe-minor-mode")
 "hdt" '(describe-theme :which-key "describe-theme")
 "hdv" '(describe-variable :which-key "describe-variable")
 "hdp" '(describe-package :which-key "describe-package")
 "hdc" '(describe-char :which-key "describe-char")

 ;; projects, projectile

 "p/"  '(consult-git-grep :which-key "consult-git-grep")

 ;; git, magit

 "g/" '(consult-git-grep :which-key "git-grep")
 "g'" '(consult-git-grep :which-key "git-grep"))
