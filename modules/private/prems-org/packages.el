;;; -*- lexical-binding: t -*-
;; prems-org/packages.el ---  Additional ORG packages

;;;
;; ORG-DOWNLOAD
;;   Helps with downloading images off the internet
;;;
(package! org-download
  :recipe (:host github
                 :repo "abo-abo/org-download")
  )

;;;
;; Mixed pitch to support varable width and fixed-width in the
;;  same buffer
;;;
(package! mixed-pitch)

;;;
;; ORG-APPEAR to reveal emphasis markers when moving the cursor over them
;;;
(package! org-appear
  :recipe (:host github
           :repo "awth13/org-appear"))

;;;
;; ORG-SHOW to reveal emphasis markers when moving the cursor over them
;;;
(use-package! org-show
  :load-path "lisp/org-show"
  :config
  )

;;;
;; ORG-PRESENT
;;   Make any org file an presentation
;;;
(package! org-present)
(after! org
  (use-package! org-present)
  )