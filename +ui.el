;;; +ui.el -- UI related configuration

;; Miscellaneous
;;Disable GUI elements

(unless window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

;; Transparency. Not setting it now, as I’m using picom.
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))


;;Prettify symbols. Also not setting it, ligatures seem to be enough for me.
;; (global-prettify-symbols-mode)

;; Do not show GUI dialogs
(setq use-dialog-box nil)

;;No start screen
(setq inhibit-startup-screen t)

;; Visual bell
(setq visible-bell 0)

;; y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Hide mouse cursor while typing
(setq make-pointer-invisible t)

;; Show pairs
(show-paren-mode 1)

;; Highlight the current line
(global-hl-line-mode 1)


;;;;;;;;;;;;;;
;;FONT CONFIG
;;;;;;;;;;;;;
(defvar required-fonts '("JetBrainsMono.*" "Overpass" "JuliaMono" "IBM Plex Mono" "Merriweather" "Alegreya"))

(defvar available-fonts
  (delete-dups (or (font-family-list)
                   (split-string (shell-command-to-string "fc-list : family")
                                 "[,\n]"))))

(defvar missing-fonts
  (delq nil (mapcar
             (lambda (font)
               (unless (delq nil (mapcar (lambda (f)
                                           (string-match-p (format "^%s$" font) f))
                                         available-fonts))
                 font))
             required-fonts)))

(if missing-fonts
    (pp-to-string
     `(unless noninteractive
        (add-hook! 'doom-init-ui-hook
          (run-at-time nil nil
                       (lambda ()
                         (message "%s missing the following fonts: %s"
                                  (propertize "Warning!" 'face '(bold warning))
                                  (mapconcat (lambda (font)
                                               (propertize font 'face 'font-lock-variable-name-face))
                                             ',missing-fonts
                                             ", "))
                         (sleep-for 0.5))))))
  ";; No missing fonts detected")
