;;;  -*- lexical-binding: t -*-
;; prems-org/config.el --- Configure what was loaded from prems-org/packages.el

;;
;; Fixup org mode with variable pitch
;;
(after! org
  (add-hook! org-mode :append
             #'mixed-pitch-mode
           #'solaire-mode
           #'variable-pitch-mode)
  )
(setq mixed-pitch-variable-pitch-cursor nil)

;;;
;;Setup org-present to start default setting
;;;
(after! org-present
  (setq-default org-present-text-scale 2)
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)
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
