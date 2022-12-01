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
  (package! org-show
    :recipe (:local-repo (expand-file-name "lisp/org-show" doom-private-dir)
             :files ("org-show.el"))
    ;; :config
  )

(after! org
  (use-package! org-show)
 )

;;;
;; ORG-PRESENT
;;   Make any org file an presentation
;;;
(package! org-present)
(after! org
  (use-package! org-present)
  )
