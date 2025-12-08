;;; cfg-tab-bar.el --- configfuration for tab-bar-mode  -*- lexical-binding: t -*-
;;; Commentary:
;;
;;; Code:

;;; TAB-BAR
(use-package tab-bar
  :ensure nil
  :defer t
  :bind
  (("C-x t <left>" . tab-bar-history-back)
   ("C-x t <right>" . tab-bar-history-forward)
   ("C-x t P" . #'cfg/tab-group-from-project)
   ("C-x t g" . #'cfg/tab-switch-to-group))
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  (tab-bar-separator "  ")
  (tab-bar-format '(tab-bar-format-tabs-groups
                    tab-bar-separator))
  :init
  ;;; --- OPTIONAL INTERNAL FN OVERRIDES TO DECORATE NAMES
  (defun tab-bar-tab-name-format-hints (name _tab i)
    (if tab-bar-tab-hints (concat (format "»%d«" i) "") name))

  (defun tab-bar-tab-group-format-default (tab _i &optional current-p)
    (propertize
     (concat (funcall tab-bar-tab-group-function tab))
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

  ;;; --- UTILITIES FUNCTIONS
  (defun cfg/tab-group-from-project ()
    "Call `tab-group' with the current project name as the group."
    (interactive)
    (when-let* ((proj (project-current))
                (name (file-name-nondirectory
                       (directory-file-name (project-root proj)))))
      (tab-group (format "[%s]" name))))

  (defun cfg/tab-switch-to-group ()
    "Prompt for a tab group and switch to its first tab.
Uses position instead of index field."
    (interactive)
    (let* ((tabs (funcall tab-bar-tabs-function)))
      (let* ((groups (delete-dups (mapcar (lambda (tab)
                                            (funcall tab-bar-tab-group-function tab))
                                          tabs)))
             (group (completing-read "Switch to group: " groups nil t)))
        (let ((i 1) (found nil))
          (dolist (tab tabs)
            (let ((tab-group (funcall tab-bar-tab-group-function tab)))
              (when (and (not found)
                         (string= tab-group group))
                (setq found t)
                (tab-bar-select-tab i)))
            (setq i (1+ i)))))))

  ;;; --- TURNS ON BY DEFAULT
  (tab-bar-mode 1)
  (tab-bar-history-mode 1))

;; '(tab-bar ((t (:background "#21242b" :foreground "#21242b" :box (:line-width (2 . 2) :color "white smoke" :style released-button)))))
;; '(tab-bar-tab ((t (:inherit tab-bar :background "lawn green" :foreground "black" :box (:line-width 2 :color "magenta" :style released-button)))))
;; '(tab-bar-tab-inactive ((t (:inherit tab-line-tab :background "#024c61" :foreground "#ff91ff" :box nil))))

(provide 'cfg-tab-bar)
;;; cfg-tab-bar.el ends here
