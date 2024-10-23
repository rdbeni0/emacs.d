;;; cfg-custom-file.el --- pre configfuration for custom.el file -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(setq custom-file (expand-file-name "data/custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file) (load custom-file))

(provide 'cfg-custom-file)
;;; cfg-custom-file.el ends here
