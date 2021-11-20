;;; cfg-dired.el --- configfuration for dired -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with "dired"

;;; Code:

(defun cfg/dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory)
  )

(defcustom list-of-dired-switches
  '("-l" "-la" "-lA" "-lA --group-directories-first")
  "List of ls switches for dired to cycle among.")


(defun cfg/cycle-dired-switches ()
  "Cycle through the list `list-of-dired-switches' of switches for ls"
  (interactive)
  (setq list-of-dired-switches
        (append (cdr list-of-dired-switches)
                (list (car list-of-dired-switches))))
  (dired-sort-other (car list-of-dired-switches))
  )

;; dired - reuse buffer
;; http://ergoemacs.org/emacs/emacs_dired_tips.html
;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
;; Possible issues with mouse integration:
;; https://emacs.stackexchange.com/questions/35536/dired-mouse-click-open-folder-in-the-same-window
;; ... so the best option seems to be dired-single:

(use-package dired-single
  :ensure t
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  ;; if dired's already loaded, then the keymap will be bound
  (if (boundp 'dired-mode-map)
      ;; we're good to go; just add our bindings
      (cfg/dired-init)
    ;; it's not loaded yet, so add our bindings to the load-hook
    (add-hook 'dired-load-hook 'cfg/dired-init))
  )

;; https://elpa.gnu.org/packages/dired-du.html
;; optional: (add-hook 'dired-mode-hook #'dired-du-mode)

(use-package dired-du
  :ensure t
  :config
  ;;
  )

;; more info:
;; https://github.com/Fuco1/dired-hacks#dired-rainbow
;; https://github.com/Fuco1/dired-hacks#dired-list

(use-package dired-hacks-utils
  :ensure t
  :config
  ;;
  )

(use-package dired-narrow
  :ensure t
  :config
  ;;
  )

(use-package dired-rainbow
  :ensure t
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    )
  )

(use-package diredfl
  :ensure t
  :config
  (add-hook 'dired-mode-hook #'diredfl-mode)
  )
(provide 'cfg-dired)
;;; cfg-dired.el ends here
