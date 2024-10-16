;;; cfg-links-web-browsers.el --- configfuration for hyperlinks and web browsers -*- lexical-binding: t -*-
;;; Commentary:
;;
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Hyperlinking.html
;;
;;; Code:

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "brave") ;; default browser: firefox or brave

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Goto-Address-mode.html
(global-goto-address-mode)

(provide 'cfg-links-web-browsers)
;;; cfg-links-web-browsers.el ends here
