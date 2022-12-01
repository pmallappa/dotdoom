;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/+org.el


;; DOESNT WORK
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
(after! org-present
  (add-hook 'org-present-mode-hook
            (lambda ()
              (prems-org/toggle-properties))))
;; DOESNT WORK
