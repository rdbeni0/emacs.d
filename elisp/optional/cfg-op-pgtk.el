;;; cfg-op-pgtk.el --- configfuration for pgtk -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Manipulations and experiments for pgtk:
;; https://www.reddit.com/r/emacs/comments/1hlj04t/emacs_using_the_lucid_toolkit_is_blazingly_fast/
;; https://www.reddit.com/r/emacs/comments/14c4l8j/way_to_make_emacs_feel_smoother/
;;; Code:

;; In PGTK, this timeout introduces latency. Reducing it from the default 0.1
;; improves responsiveness of childframes and related packages.
(when (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

(provide 'cfg-op-pgtk)
;;; cfg-op-pgtk.el ends here
