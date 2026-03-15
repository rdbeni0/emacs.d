;;; cfg-op-core-compat.el --- Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Compatibility layer for older versions of Emacs.
;;
;;; Code:

(when (< emacs-major-version 28)
  ;; DIRED - reuse buffer:
  ;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer

  ;; required for dired-jump
  (require 'dired-x)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] 'dired-find-alternate-file))

(if (version< emacs-version "28.5")
    (progn
      ;; BYTE COMPILATION:
      ;; Byte-compiling these directories is safe, but when using
      ;; native compilation (native-comp), outdated .elc files may
      ;; sometimes cause warnings or errors. In such cases, removing
      ;; old .elc files can help ensure that native compilation
      ;; produces fresh .eln files without conflicts.
      ;;
      ;; References:
      ;; "One snag I hit after trying to recompile native compilation after using it for a while
      ;; was needing to delete the old .elc files in the source directory"
      ;; https://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory
      ;; https://www.reddit.com/r/emacs/comments/myej3z/the_nativecompilation_branch_was_just_merged_into/

      (byte-recompile-directory (expand-file-name "elisp/site-elisp" user-emacs-directory) 0)
      (byte-recompile-directory (expand-file-name "elisp/optional" user-emacs-directory) 0)
      (byte-recompile-directory (expand-file-name "elisp" user-emacs-directory) 0)
      ;;
      )
  ;; optional else:
  ;; ()
  )

;; BUG with native compilation - fixed in Emacs 30+:
;; https://emacs.stackexchange.com/questions/82010/why-is-emacs-recompiling-some-packages-on-every-startup
(when (and (version<= "28.5" emacs-version)
           (version< emacs-version "30.0"))
  (defun fixed-native-compile-async-skip-p
      (native-compile-async-skip-p file load selector)
    (let* ((naive-elc-file (file-name-with-extension file "elc"))
           (elc-file (replace-regexp-in-string
                      "\\.el\\.elc$" ".elc" naive-elc-file)))
      (or (gethash elc-file comp--no-native-compile)
          (funcall native-compile-async-skip-p file load selector))))

  (advice-add 'native-compile-async-skip-p
              :around #'fixed-native-compile-async-skip-p))

(when (version< emacs-version "29.0")
  ;; USE-PACKAGE:
  ;; Unless it is already installed - update packages archive and install the most recent version of use-package:
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(when (version< emacs-version "30.0")

  ;; ESC workaround for evil: use "ESC" everywhere
  ;; This workaround is old, but it worked for many years (around 2016-2026).
  ;; While it doesn't look "pretty", it has been thoroughly tested throughout that time.
  (require 'evil)

  (defun cfg/minibuffer-keyboard-quit ()
    "Abort recursive edit. In Delete Selection mode, if the mark is active,
just deactivate it; then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'cfg/minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'cfg/minibuffer-keyboard-quit)
  (global-set-key [escape] 'evil-exit-emacs-state))

(provide 'cfg-op-core-compat)
;;; cfg-op-core-compat.el ends here
