;;; cfg-op-email.el --- configfuration for emails -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Everything what is connected with reading and composing emails via emacs.
;;
;;; Code:

;; https://gist.github.com/vedang/26a94c459c46e45bc3a9ec935457c80f
(use-package notmuch
  :ensure t
  :config
  (setq-default
   ;; do not load all the messages on search, needs:
   ;; https://notmuchmail.org/pipermail/notmuch/2019/027309.html
   ;; notmuch-progressive-search t

   ;; adjust hello sections
   notmuch-hello-sections (quote (
				  notmuch-hello-insert-saved-searches
				  notmuch-hello-insert-recent-searches
				  notmuch-hello-insert-alltags
				  ))

   ;; drop the logo on the front page
   notmuch-show-logo nil

   ;; notmuch-search-oldest-first defines the sort order
   notmuch-search-oldest-first nil)

  ;; turn off auto-fill mode for message-mode in emacs
  ;; https://stackoverflow.com/questions/9878623/how-can-i-disable-auto-fill-mode-in-emacs
  (add-hook 'notmuch-message-mode-hook (lambda () (auto-fill-mode -1)))

  ;; gnus-w3m as internal html renderer
  ;; more options: https://www.gnu.org/software/emacs/manual/html_node/mh-e/HTML.html

  (setq mm-text-html-renderer 'gnus-w3m)
  ;; (setq mm-text-html-renderer 'shr) ;; also a good choice
  (setq shr-use-fonts nil)
  (setq shr-use-colors nil)
  (setq shr-max-width nil)

  ;; should be declared in a private file (will be described later)
  (setq notmuch-saved-searches nil)

  ;; https://notmuchmail.org/emacstips/#index5h2
  ;;
  ;; TOGGLING tag:
  ;;
  ;; "You can do the same for threads in notmuch-search-mode by just replacing "show" with "search" in the keymap and called functions, or for messages in notmuch-tree-mode by replacing "show" by "tree". If you want to tag a whole thread in notmuch-tree-mode use notmuch-tree-tag-thread instead of notmuch-tree-tag."

  (defun cfg/notmuch-toggle-tag-show-unread ()
    "toggle unread tag for message"
    (interactive)
    (if (member "unread" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-unread"))
      (notmuch-show-tag (list "+unread"))))

  (defun cfg/notmuch-toggle-tag-search-unread ()
    "toggle unread tag for message"
    (interactive)
    (if (member "unread" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-unread"))
      (notmuch-search-tag (list "+unread"))))

  (defun cfg/notmuch-toggle-tag-tree-unread ()
    "toggle unread tag for message"
    (interactive)
    (if (member "unread" (notmuch-tree-get-tags))
        (notmuch-tree-tag (list "-unread"))
      (notmuch-tree-tag (list "+unread"))))

  (defun cfg/notmuch-toggle-tag-show-flag ()
    "toggle new tag for message"
    (interactive)
    (if (member "flagged" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-flagged"))
      (notmuch-show-tag (list "+flagged"))))

  (defun cfg/notmuch-toggle-tag-search-flag ()
    "toggle new tag for message"
    (interactive)
    (evil-collection-notmuch-search-toggle-flagged))

  (defun cfg/notmuch-toggle-tag-tree-flag ()
    "toggle new tag for message"
    (interactive)
    (evil-collection-notmuch-tree-toggle-flagged))

  (defun cfg/notmuch-toggle-tag-show-bin ()
    "toggle unread tag for message"
    (interactive)
    (evil-collection-notmuch-show-toggle-flagged))

  (defun cfg/notmuch-toggle-tag-search-bin ()
    "toggle unread tag for message"
    (interactive)
    (if (member "BIN" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-BIN"))
      (notmuch-search-tag (list "+BIN"))))

  (defun cfg/notmuch-toggle-tag-tree-bin ()
    "toggle unread tag for message"
    (interactive)
    (if (member "BIN" (notmuch-tree-get-tags))
        (notmuch-tree-tag (list "-BIN"))
      (notmuch-tree-tag (list "+BIN"))))

  (defun cfg/notmuch-toggle-tag-show-arch ()
    "toggle unread tag for message"
    (interactive)
    (if (member "ARCH" (notmuch-show-get-tags))
        (notmuch-show-tag (list "-ARCH"))
      (notmuch-show-tag (list "+ARCH"))))

  (defun cfg/notmuch-toggle-tag-search-arch ()
    "toggle unread tag for message"
    (interactive)
    (if (member "ARCH" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-ARCH"))
      (notmuch-search-tag (list "+ARCH"))))

  (defun cfg/notmuch-toggle-tag-tree-arch ()
    "toggle unread tag for message"
    (interactive)
    (if (member "ARCH" (notmuch-tree-get-tags))
        (notmuch-tree-tag (list "-ARCH"))
      (notmuch-tree-tag (list "+ARCH"))))

  ;; https://emacs.stackexchange.com/questions/63436/is-there-some-way-to-view-the-html-part-of-an-email-in-an-external-browser-or-as
  (defun cfg/notmuch-show-view-html ()
    "Open the text/html part of the current message using `notmuch-show-view-part'."
    (interactive)
    (save-excursion
      (goto-char
       (prop-match-beginning
	(text-property-search-forward
	 :notmuch-part
	 "text/html"
	 (lambda (value notmuch-part)
           (equal (plist-get notmuch-part :content-type)
                  value)))))
      (notmuch-show-view-part)))

  (defun cfg/notmuch-show-close-all ()
    "Close all msgs."
    (interactive)
    (goto-char (point-min))
    (let ((current-prefix-arg '(4)))
      (call-interactively 'notmuch-show-open-or-close-all)))

  (defun cfg/notmuch-fuzzy-search ()
    "Fuzzy search (using completion) for notmuch-saves-searches entries."
    (interactive)
    (notmuch-search (completing-read "Notmuch search: " (mapcar (lambda (x) (plist-get x :query)) notmuch-saved-searches))))

  (defun cfg/notmuch-fcc-replace ()
    (interactive)
    (message-remove-header "Fcc")
    (notmuch-fcc-header-setup))

  ;; There are some problems with fonts and frames:
  ;; https://stackoverflow.com/questions/25221960/emacsclient-font-check-not-working
  ;; https://stackoverflow.com/questions/3984730/emacs-gui-with-emacs-daemon-not-loading-fonts-correctly
  ;; https://github.com/vifon/emacs-config/blob/master/emacs.d/theme.el#L124-L133
  ;; and we can try for example:
  ;; (add-to-list 'default-frame-alist '(font . "Font Name"))
  ;; (if (daemonp) )
  ;;
  ;; ... but this solution is working:
  (add-hook 'notmuch-search-mode-hook
	    (lambda ()
	      (set-fontset-font t 'symbol "Noto Color Emoji")))

  ;; how columns should look like:
  (setq notmuch-search-result-format '(("date" . "%12s ")
				       ("count" . "%-7s ")
				       ("authors" . "%-20s ")
				       ("tags" . "(%s) ")
				       ("subject" . "%s")
				       ))

  ;; Please create correct "lo-email.el" file inside ~/.emacs.d/data/local/lo-email.el (or other emacs dir)
  ;; Please implement email management defuns and add below variables (and any other config):
  ;;
  ;; (defun cfg/notmuch-poll-mbsync () ... ;; function to download emails from the servers
  ;; (defun cfg/notmuch-poll-mbsync-full-sort () ... ;; function for sorting downloaded emails
  ;; (defun cfg/notmuch-nmuch-sort-archive () ... ;; archiving and backup function
  ;; (defun cfg/notmuch-poll-empty-bin () ... ;; function for emptying the trash
  ;; (defun cfg/notmuch-poll-empty-spam () ;; function to empty spam
  ;;
  ;; (setq notmuch-saved-searches ...
  ;; (setq notmuch-fcc-dirs ... 
  ;; (setq message-signature ...

  (if (file-readable-p (expand-file-name "data/local/lo-email.el" user-emacs-directory))
      (require 'lo-email) ; if true, load additional elisp for email
					; if false, then message with "WARNING" will appear during initialization of email:
    (message "WARNING! File data/local/lo-email.el inside your emacs.d is not readable (or not exist)! Please create it and add correct email options!"))

  ;; load general.el and keybindings:
  (require 'cfg-gen-op-notmuch-mode)
  ;; end of use-package
  )

;; SENDING EMAIL:

(setq send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
(setq notmuch-always-prompt-for-sender t)
(setq message-auto-save-directory nil)
(setq message-confirm-send t)
(setq notmuch-wash-signature-lines-max 3)

;; https://emacs.stackexchange.com/questions/57720/how-can-i-get-emacs-notmuch-to-format-replies-the-same-way-gmail-does
(with-eval-after-load 'message
  (setq message-cite-style message-cite-style-gmail)
  (setq message-citation-line-function 'message-insert-formatted-citation-line))

(provide 'cfg-op-email)
;;; cfg-op-email.el ends here
