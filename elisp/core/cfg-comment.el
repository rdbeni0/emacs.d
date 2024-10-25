;;; cfg-comment.el --- configfuration for commenting  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

(use-package newcomment
  :config
  ;; load general.el and keybindings:
  (require 'cfg-gen-co-comment-mark))

(provide 'cfg-comment)
;;; cfg-comment.el ends here
