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
(package! org-slideshow
  :recipe (:host github
           :repo  "pmallappa/org-slideshow"
           :files ("org-slideshow.el"))
           ;;; Avoid having to run "doom sync" everytime, when the package
           ;;; is changed, only useful if the package is bieng developed
           ;;;:build (:not compile)
  )

;;;
;; VISUAL FILL COLUMN
;; helps in presentations
(package! visual-fill-column)

;;;
;; ORG-PRESENT
;;   Make any org file an presentation
;;;
;;(package! org-present)

(package! org-superstar)

(package! org-super-agenda)

(package! org-tree-slide)

;;;
;; Use contributed packages
(package! org-contrib
  :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
           :files ("lisp/*.el"))
  ;;pin: "e6cb6ca42f5bbb74825e81fd645b00e17a673da4"
  )
