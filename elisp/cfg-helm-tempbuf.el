;;; cfg-helm-tempbuf.el --- configure helm and tempbuf -*- lexical-binding: t -*-
;;; Commentary:

;; Setup for helm-mode,
;; but also tempbuf : *tempbuf* will kill unused buffers after XX seconds.

;;; Code:

;; https://github.com/syl20bnr/spacemacs/issues/11873

(use-package helm
  :ensure t
  :init
  (setq helm-candidate-number-list 1000)
  (setq helm-M-x-fuzzy-match                  t
        helm-bookmark-show-location           t
        helm-buffers-fuzzy-matching           t
        helm-completion-in-region-fuzzy-match t
        helm-file-cache-fuzzy-match           t
        helm-allow-mouse                      t
        helm-imenu-fuzzy-match                t
        helm-mode-fuzzy-match                 t
        helm-locate-fuzzy-match               t
        helm-quick-update                     t
        helm-recentf-fuzzy-match              t
        helm-semantic-fuzzy-match             t)
  :config
  (define-key helm-map (kbd "ESC") 'helm-keyboard-quit)
  (helm-mode)
  (with-eval-after-load 'helm-buffers
    (add-to-list 'helm-boring-buffer-regexp-list "*scratch*")
    (add-to-list 'helm-boring-buffer-regexp-list "*Messages*")
    (add-to-list 'helm-boring-buffer-regexp-list "*spacemacs*"))
  )

(setq helm-candidate-number-limit 150) ;; default is 150

(use-package helm-themes
  :ensure t
  )

(use-package helm-descbinds
  :ensure t
  )

;; https://github.com/bbatsov/helm-projectile

(use-package helm-projectile
  :after projectile
  :ensure t
  :config
  (helm-projectile-on)
  )

(use-package helm-org
  :after org
  :ensure t
  :config
  )

;; https://github.com/yasuyk/helm-git-grep     

(use-package helm-git-grep
  :after magit
  :ensure t
  :config
  ;;
  )

;; tempbuf

;; (WARNING - this option could be aggresive!)
;;
;; More examples of configuration:
;; https://www.emacswiki.org/emacs/TempbufMode
;; https://www.emacswiki.org/emacs/tempbuf.el < source
;; https://github.com/DarwinAwardWinner/dotemacs-old/blob/master/site-lisp/settings/tempbuf-settings.el
;; https://github.com/biern/.emacs.d/blob/master/conf/tempbuf.el
;;
;; tempbuf is working well with helm and it will clean junk buffers:

(require 'tempbuf)
(setq tempbuf-minimum-timeout 30)
(add-hook 'helm-major-mode-hook 'turn-on-tempbuf-mode)

;; ... for other customization (less agressive), please take a look for MidnightMode:
;; https://www.emacswiki.org/emacs/MidnightMode
;; https://www.emacswiki.org/emacs/CleanBufferList

;; defuns

(defun my//helm-do-grep-region-or-symbol
    (&optional targs use-region-or-symbol-p)
  "Version of `helm-do-grep' with a default input."
  (interactive)
  (require 'helm)
  (cl-letf*
      (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
       ((symbol-function 'helm-do-grep-1)
        (lambda (targets &optional recurse zgrep exts
                         default-input region-or-symbol-p)
          (let* ((new-input (when region-or-symbol-p
                              (if (region-active-p)
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end))
                                (thing-at-point 'symbol t))))
                 (quoted-input (when new-input
                                 (rxt-quote-pcre new-input))))
            (this-fn targets recurse zgrep exts
                     default-input quoted-input))))
       (preselection (or (dired-get-filename nil t)
                         (buffer-file-name (current-buffer))))
       (targets   (if targs
                      targs
                    (helm-read-file-name
                     "Search in file(s): "
                     :marked-candidates t
                     :preselect (when preselection
                                  (if helm-ff-transformer-show-only-basename
                                      (helm-basename preselection)
                                    preselection))))))
    (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p))
  )

(defun my//helm-file-do-grep ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (my//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) nil)
  )

(defun my//helm-file-do-grep-region-or-symbol ()
  "Search in current file with `grep' using a default input."
  (interactive)
  (my//helm-do-grep-region-or-symbol
   (list (buffer-file-name (current-buffer))) t)
  )

(defun my//helm-files-do-grep ()
  "Search in files with `grep'."
  (interactive)
  (my//helm-do-grep-region-or-symbol nil nil)
  )

(defun my//helm-files-do-grep-region-or-symbol ()
  "Search in files with `grep' using a default input."
  (interactive)
  (my//helm-do-grep-region-or-symbol nil t)
  )

(defun my//helm-buffers-do-grep ()
  "Search in opened buffers with `grep'."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (my//helm-do-grep-region-or-symbol buffers nil))
  )

(defun my//helm-buffers-do-grep-region-or-symbol ()
  "Search in opened buffers with `grep' with a default input."
  (interactive)
  (let ((buffers (cl-loop for buffer in (buffer-list)
                          when (buffer-file-name buffer)
                          collect (buffer-file-name buffer))))
    (my//helm-do-grep-region-or-symbol buffers t))
  )

(defun my//helm-jump-in-buffer ()
  "Jump in buffer using `imenu' facilities and helm."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'helm-org-in-buffer-headings)
    (t 'helm-semantic-or-imenu)))
  )

(provide 'cfg-helm-tempbuf)
;;; cfg-helm-tempbuf.el ends here
