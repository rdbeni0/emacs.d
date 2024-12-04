;;; cfg-coding-systems.el --- configure options for coding systems -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;; use utf-8 by default:
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8 )
(setq default-process-coding-system '(utf-8 . utf-8))

(provide 'cfg-coding-systems)
;;; cfg-coding-systems.el ends here
