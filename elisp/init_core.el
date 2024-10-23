(add-to-list 'load-path (expand-file-name "elisp/core" user-emacs-directory)) ; additional/optional load-path will be configured in cfg-use-package

;; order is important:

(require 'cfg-performance-native-comp) ;; OK
(require 'cfg-use-package) ;; OK
(require 'cfg-abbrevs) ;; OK
(require 'cfg-list-core-packages) ;; OK
(require 'cfg-list-auto-mode-alist) ;; OK
(require 'cfg-evil) ;; OK
(require 'cfg-general) ;; OK
(require 'cfg-format) ;; OK
(require 'cfg-hideshow) ;; OK
(require 'cfg-which-key) ;; OK
(require 'cfg-tab-bar) ;; OK
(require 'cfg-eglot) ;; OK
(require 'cfg-ediff) ;; OK
(require 'cfg-i-buffer-menu) ;; OK
(require 'cfg-diagnose) ;; OK
(require 'cfg-common-options) ;;
(require 'cfg-line-numbers-hl) ;; TODO - 1/ "defcustom display-line-numbers-exempt-mode", 2/ "highlight-indent-guides"
(require 'cfg-emacs-lisp) ;; OK
(require 'cfg-python) ;; OK
(require 'cfg-perl) ;; OK
(require 'cfg-cc) ;; OK
(require 'cfg-compile) ;; OK
(require 'cfg-nxml) ;; OK
(require 'cfg-js-json) ;; OK
(require 'cfg-sh-script) ;; OK
(require 'cfg-shell-comint) ;; OK
(require 'cfg-term) ;; OK
(require 'cfg-grep) ;; OK
(require 'cfg-recentf) ;; OK
(require 'cfg-custom-file) ;; OK
(require 'cfg-project) ;; OK
(require 'cfg-vcs) ;; OK
(require 'cfg-dired) ;; OK
(require 'cfg-tramp-sudo) ;; OK
(require 'cfg-org) ;; OK
(require 'cfg-irc) ;; OK
(require 'cfg-man-help) ;; OK
(require 'cfg-flyspell) ;; OK
(require 'cfg-doc-view-image) ;; OK
(require 'cfg-links-web-browsers) ;; OK
