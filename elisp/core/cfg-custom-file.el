;;; cfg-custom-file.el --- configfuration for custom variables/faces -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Configuration for custom.el file
;;
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

(provide 'cfg-custom-file)
;;; cfg-custom-file.el ends here
