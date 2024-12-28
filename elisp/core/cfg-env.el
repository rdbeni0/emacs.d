;;; cfg-env.el --- configuration for environment variables -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

					; if true, load additional, local env variables:
(if (file-readable-p (expand-file-name "data/local/lo-env.el" user-emacs-directory))
    (require 'lo-env))

(provide 'cfg-env)
;;; cfg-env.el ends here
