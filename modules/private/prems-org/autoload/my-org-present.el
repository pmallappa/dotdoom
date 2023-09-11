;;;  -*- lexical-binding: t -*-
;; modules/private/prems-org/autoload/my-org-present.el
;;

;;;###autoload
(defun pm/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children)
  )

;;;###autoload
(defun pm/org-present-hook ()
  "Tweak font sizes"
  (interactive)
  (setq-local face-remapping-alist '((default (:height 2.5) variable-pitch)
                                     (header-line (:height 4.0) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.15) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.15) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-display-inline-images)
  (when (foundp 'visual-fill-column-mode)
    (visual-fill-column-mode 1)
  (when (foundp 'visual-line-mode)
    (setq visual-line-mode 1))
  (when (foundp 'org-appear-mode)
    (org-appear-mode -1))
  (pm/org-present-prepare-slide)
  )

;;;###autoload
(defun pm/org-present-quit-hook ()
  (interactive)
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (when (foundp 'visual-fill-column-mode)
    (visual-fill-column-mode 0)
  (when (foundp 'org-appear-mode)
    (org-appear-mode 1))
  )

(defun pm/org-present-prev ()
  (interactive)
  (pm/org-present-prepare-slide)
  )

(defun pm/org-present-next ()
  (interactive)
  (pm/org-present-prepare-slide)
  (when (foundp 'live-crafter-add-timestamp)
    (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t))))
  )
