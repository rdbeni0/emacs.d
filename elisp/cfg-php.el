;;; cfg-php.el --- configfuration for php programming -*- lexical-binding: t -*-
;;; Commentary:

;; Everything what is connected with PHP programming
;; 2021 year: https://www.reddit.com/r/emacs/comments/ms8nvc/emacs_php_mode/
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cfg/-php-format ()
  "Run custom PHP formatter on the current file (via CLI)."
  (shell-command-on-region (point) (mark) (concat "prettier --parser php --print-width 180 --stdin-filepath " (buffer-file-name)) nil t)
  )

(defun cfg/php-custom-file-format ()
  "Format current php buffer."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (cfg/-php-format)
    (message "Reformatted! In case of formatting errors, please undo.")
    )
  )

(use-package php-mode
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/OVYA/php-cs-fixer

(use-package php-cs-fixer
  :ensure t
  )

(provide 'cfg-php)
;;; cfg-php.el ends here
