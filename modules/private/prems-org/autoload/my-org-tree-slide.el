;;;  -*- lexical-binding: t -*- prems-org/autoload.el

;;;
;;TREE_SLIDE presentation mode
;;
;;;###autoload
(defun pm/tree-slide-faces-at-start-present ()

  ;; (1) - custom set faces
  ;; (setq-local face-remapping-alist '(
  ;;                                    (default (:height 1.55) variable-pitch)
  ;;                                    (header-line (:height 1.55) variable-pitch)
  ;;                                    (org-document-title (:height 1.5) org-document-title)
  ;;                                    (org-code (:height 1.55) org-code)
  ;;                                    (org-verbatim (:height 1.55) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block)
  ;;                                    (org-block-begin-line (:height 0.7) org-block))
  ;;              )
  (org-display-inline-images)
  (hide-mode-line-mode 1)

  ;; (2) - Autoscaling
  (text-scale-mode 1)
  (setq-local text-scale-amount 2.0)
)

(defun pm/tree-slide-faces-at-start-present-term ()
  (interactive)
  ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
  ;;                                    (org-verbatim (:height 1.75) org-verbatim)
  ;;                                   (org-block (:height 1.25) org-block)))
  )

;;;###autoload
(defun pm/tree-slide-faces-at-stop-present ()
  ;; Use this when using (1)
  ;;(setq-local face-remapping-alist '((default variable-pitch default)))

  (hide-mode-line-mode 0)

  ;; Use this when using (2)
  (text-scale-mode 0)
  (setq-local text-scale-amount 1)
  )

