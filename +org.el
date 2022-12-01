;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/+org.el

(after! org
  (use-package! org-present)
  )


;; Used in org-present mode, to hide the draws
;; souce /r/emacs
(defun prems-org/toggle-properties ()
  ;; toggle visibility of properties in current header if it exists
  (save-excursion
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (when (org-header-property-p)
      (let* ((a (re-search-forward "\n\\:" nil t)))
        (if (outline-invisible-p (point))
            (outline-show-entry)
          (org-cycle-hide-drawers 'all))))))


;; Setup org-present to start default setting
(after! org-present
  (setq org-present-text-scale 2)
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)
              (prems-org/toggle-properties)
              ;;(toggle-frame-fullscreen)
              ))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write)
              ;;(toggle-frame-fullscreen)
              )))
