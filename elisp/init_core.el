(add-to-list 'load-path (expand-file-name "elisp/core" user-emacs-directory)) ; additional/optional load-path will be configured in cfg-use-package

;; order is important:
(require 'cfg-performance-native-comp)
(require 'cfg-use-package)
(require 'cfg-list-core-packages)
(require 'cfg-list-auto-mode-alist)
(require 'cfg-evil)
(require 'cfg-general)
(require 'cfg-format)
(require 'cfg-comment)
(require 'cfg-xref-ffap)
(require 'cfg-txt-manipulations)
(require 'cfg-hideshow)
(require 'cfg-which-key)
(require 'cfg-tab-bar)
(require 'cfg-eglot)
(require 'cfg-ediff)
(require 'cfg-i-buffer-menu)
(require 'cfg-diagnose)
(require 'cfg-common-options)
(require 'cfg-coding-systems)
(require 'cfg-backup-savehist)
(require 'cfg-line-numbers-hl)
(require 'cfg-emacs-lisp)
(require 'cfg-python)
(require 'cfg-perl)
(require 'cfg-cc)
(require 'cfg-compile)
(require 'cfg-nxml)
(require 'cfg-js-json)
(require 'cfg-sh-script)
(require 'cfg-shell-comint)
(require 'cfg-term)
(require 'cfg-grep)
(require 'cfg-recentf)
(require 'cfg-custom-file)
(require 'cfg-project)
(require 'cfg-vcs)
(require 'cfg-dired)
(require 'cfg-tramp-sudo)
(require 'cfg-org)
(require 'cfg-irc)
(require 'cfg-man-help)
(require 'cfg-flyspell)
(require 'cfg-doc-view-image)
(require 'cfg-links-web-browsers)
(require 'cfg-abbrevs-tempo) ;; should be loaded AFTER any prog- mode (bcz will overwrite existing abbrev table)
