;;;  -*- lexical-binding: t -*-
;; prems-org/config.el --- Configure what was loaded from prems-org/packages.el

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


(defun pm/_org-files-setup()
  ;; Agenda org-mode files
  org-agenda-files
  `(,(file-truename (expand-file-name "refile.org" org-directory))
    ,(file-truename (expand-file-name "todo.org" org-directory))
    ,(file-truename (expand-file-name "work.org" org-directory))
    ,(file-truename (expand-file-name "notes.org" org-directory))
    ,(file-truename (expand-file-name "personal.org" org-directory))
    ,(file-truename (expand-file-name "journal.org" org-directory))
    )
  )


(defun pm/_org-misc-setup()
  ;; add or remove tags on state change
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  ;; refile targets all level 1 and 2 headers in current file and agenda files
  (setq org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2)))


  ;; quick access to common tags
  (setq org-tag-alist
        '(("oss" . ?o)
          ("home" . ?h)
          ("work" . ?w)
          ("xplugins" . ?x)
          ("book" . ?b)
          ("support" . ?s)
          ("docs" . ?d)
          ("export" . ?e)
          ("noexport" . ?n)
          ("recurring" . ?r)))
  )


(defun pm/_org-agenda-setup()
  (use-package org-super-agenda
    :init (setq org-super-agenda-groups
                '(
                  (:discard (:scheduled future))
                  (:property ("PROJECT" "2023Goals") :name "2023 Goals" :order 12)
                  (:property ("PROJECT" "2024Goals") :name "2024 Goals" :order 13)
                  (:todo "NEXT")
                  (:priority "A" :name "High priority")
                  (:deadline t :name "Upcoming deadlines")
                  (:scheduled today :name "today")
                  (:scheduled past :name "past")
                  (:file-path "/notes.org" :order 11 :name "Home stuff")
                  (:tag "writing" :order 10)
                  (:discard (:scheduled future))
                  (:auto-property "WAITING_ON" :log t :name "Waiting on people")
                  (:auto-property "PROJECT" :log t)
                  (:auto-tags)
                  (:auto-todo)))

    :config (org-super-agenda-mode t)
    (variable-pitch-mode t))

  (setq
   ;; Overwrite the current window with the agenda
   org-agenda-window-setup 'current-window
   ;; allow changing between todo stats directly by hotkey
   org-use-fast-todo-selection t
   ;;show me tasks scheduled or due in next fortnight
   org-agenda-span 'fortnight
   ;; Start on Sunday
   org-agenda-start-on-weekday 0
   ;; Do not dim blocked tasks
   org-agenda-dim-blocked-tasks nil
   org-agenda-include-diary t
   org-agenda-skip-scheduled-if-done t
   ;;don't show tasks as scheduled if they are already shown as a deadline
   org-agenda-skip-scheduled-if-deadline-is-shown t
   ;;don't give awarning colour to tasks with impending deadlines
   ;;if they are scheduled to be done
   org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
   ;;don't show tasks that are scheduled or have deadlines in the
   ;;normal todo list
   org-agenda-todo-ignore-deadlines (quote all)
   org-agenda-todo-ignore-scheduled (quote all)

   ;; For tag searches ignore tasks with scheduled and deadline dates
   org-agenda-tags-todo-honor-ignore-options t

   ;; Sorting order for tasks on the agenda
   org-agenda-sorting-strategy
   '((agenda habit-down
      time-up
      priority-down
      user-defined-up
      effort-up
      category-keep)
     (todo priority-down category-up effort-up)
     (tags priority-down category-up effort-up)
     (search priority-down category-up))
   )

  ;; Org todo keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "TODO(t)"
                    "SOMEDAY(s)"
                    "INPROGRESS(i)"
                    "HOLD(h)"
                    "WAITING(w@/!)"
                    "NEEDSREVIEW(n@/!)"
                    "|" "DONE(d)")
          (sequence "TODO(t)" "INPROGRESS(i)" "|" "CANCELLED(c@/!)")))

  ;; add or remove tags on state change
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
  )


(defun pm/org-toggle-hide-emphasis-markers ()
  "Toggle org-hide-emphasis-markers"
  (interactive)
  (if org-hide-emphasis-markers
      (setq org-hide-emphasis-markers nil)
    (setq org-hide-emphasis-markers t)))

;;;;;;;;;;;;;;;;;;
;; capture templates
;;;;;;;;;;;;;;;;;;
(defun pm/_org-capture-setup()
  (setq org-capture-templates
        '(("t" "Todo" entry (file (expand-file-name "refile.org" org-directory)
                                  "* TODO %?\n%U\n")
           ("n" "Notes" entry (file+headline (expand-file-name "notes.org" org-directory) "Notes")
            "* %? :NOTE:\n%U\n")
           ("e" "Emacs note" entry
            (file+headline "~/org/notes.org" "Emacs Links")
            "* %? :NOTE:\n%U\n")
           ("j" "Journal" entry (file+datetree "~/org/journal.org")
            "* %?\n%U\n")
           ("b" "Book/Bibliography" entry
            (file+headline "~/org/bibliography.org" "Refile")
            "* %?%^{TITLE}p%^{AUTHOR}p%^{TYPE}p")))
        )
  )

(defun pm/_org-default-setup()
  (setq
   ;; special begin/end of line to skip tags and stars
   org-special-ctrl-a/e t
   ;; special keys for killing a headline
   org-special-ctrl-k t
   ;; follow links by pressing ENTER on them
   org-return-follows-link t
   ;; Don't like the leading stars
   org-hide-leading-stars t
   ;; don't adjust subtrees that I copy
   org-yank-adjusted-subtrees nil
   ;; try to be smart when editing hidden things
   org-fold-catch-invisible-edits 'smart
   ;; This no longer required
   ;;org-hide-leading-stars-before-indent-mode t
   ;; force UTF-8
   org-export-coding-system 'utf-8
   ;; start up org files with indentation (same as #+STARTUP: indent)
   org-startup-indented t
   ;; don't adapt indentation
   org-adapt-indentation nil

   org-indent-indentation-per-level 4
   ;; tags should start in 60th column
   org-tags-column -60

   org-log-done 'note
   ;; put state change log messages into a drawer
   org-log-into-drawer t

   org-clock-mode-line-total 'today

   ;;warn me of any deadlines in next 7 days
   org-deadline-warning-days 7

   ;;org-fast-tag-selection-single-key (quote expert)

   variable-pitch-mode t

   visual-line-mode t

   auto-fill-mode nil
   )

  (remove-hook 'text-mode-hook #'turn-on-auto-fill)
  )


(use-package org-superstar              ; supersedes `org-bullets'
  :ensure
  :after org
  :config
  ;; Every non-TODO headline now have no bullet
  ;;(setq org-superstar-headline-bullets-list '("\u200b"))
  ;;(setq org-superstar-leading-bullet "\u200b")
  (setq org-superstar-item-bullet-alist
        '((?+ . ?+)
          (?* . ?➤)
          (?- . ?•)))
  ;; Enable custom bullets for TODO items
  (setq org-superstar-special-todo-items t)
  )

(defun pm/_org-latex-setup()
  ;; Minted = spiced up source code, colors
  (setq org-latex-src-block-backend 'minted)
  (setq org-latex-minted-options
        '(;;("frame" "lines")
          ("fontsize" "\\footnotesize")
          ("mathescape" "")
          ("samepage" "")
          ("xrightmargin" "0.5cm")
          ("xleftmargin"  "0.5cm")
          ))
  )

(defun pm/_org-src-setup()
  (setq
   ;; syntax highlight code in source blocks
   org-src-fontify-natively t
   ;; don't indent source code
   org-edit-src-content-indentation 0
   ;; preserve the indentation inside of source blocks
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   ;; blank lines are removed when exiting the code edit buffer
   org-src--preserve-blank-line nil
   ;; how org-src windows are set up when hitting C-c '
   org-src-window-setup 'current-window
   )
  )


(defun pm/_org-hooks-setup()
  ;; complex version: change font as well
  (add-hook 'org-mode-hook 'visual-line-mode)
  (with-eval-after-load 'org
    (setq word-wrap t))
  )


(defun pm/_org-babel-setup()

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     ;;(R . t)
     (C . t)
     (shell . t)
     (rust . t)
     ;;(ipython . t)
     ;;(perl . t)
     )
   )

  ;; disable confirmation
  (setq
   ;; don't run stuff automatically on export
   org-export-use-babel nil
   ;; always enable noweb, results as code and exporting both
   org-babel-default-header-args
   (cons '(:noweb . "yes")
         (assq-delete-all :noweb org-babel-default-header-args))
   org-babel-default-header-args
   (cons '(:exports . "both")
         (assq-delete-all :exports org-babel-default-header-args))
   ;; Security: to be prompted on every code block evaluation
   org-confirm-babel-evaluate t

   ;; Set only the ones that dont need confirmation
   org-confirm-python-evaluate nil
   org-confirm-sh-evaluate nil
   org-confirm-C++-evaluate nil
   org-confirm-rust-evaulate nil
   )

  ;; If you get the message org-babel-execute-src-block: No org-babel-execute
  ;; function for sh! then you need to enable shell mode for babel. Do that by
  ;; running this code block:
  ;; (require 'ob-shell)

  )

;;;
(defun pm/unused()
  (setq
          ;; Use full outline paths for refile targets - we file directly with IDO
          org-refile-use-outline-path t
          ;; Targets complete directly with IDO
          org-outline-path-complete-in-steps nil
          ;; Allow refile to create parent tasks with confirmation
          org-refile-allow-creating-parent-nodes 'confirm
          ;; never leave empty lines in collapsed view
          org-cycle-separator-lines 0

          ;; start up showing images
          org-startup-with-inline-images t

          ;; Enable display of the time grid so we can see the marker for the
          ;; current time
          org-agenda-time-grid
          '((daily today remove-match)
            #("----------------" 0 16 (org-heading t))
            (0900 1100 1300 1500 1700))
          ;; keep the agenda filter until manually removed
          org-agenda-persistent-filter t

          ;; Use sticky agenda's so they persist
          org-agenda-sticky t

          ;; Compact the block agenda view
          org-agenda-compact-blocks t
          ;; Show all agenda dates - even if they are empty
          org-agenda-show-all-dates t
          )
  )

;;;
;; UI / Font
;;;
(defun pm/_org-mode-font-setup()
  (setq
  ;; for the leuven theme, fontify the whole heading line
  org-fontify-whole-heading-line t)

  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.0)
                  ))
    (set-face-attribute (car face) nil
                        ;;:inherit 'doom-variable-pitch-font
                        :weight 'bold
                        :height (cdr face)))


  ;; Make sure org-indent face is available,
  ;; otherwise (error Invalid face org-indent)
  (require 'org-indent)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  ;; Get rid of the background on column views
  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil)

  ;; Org faces
  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("INPROGRESS" :foreground "deep sky blue" :weight bold)
          ("SOMEDAY" :foreground "purple" :weight bold)
          ("NEEDSREVIEW" :foreground "#edd400" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The big setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package! org
  :config
  (pm/_org-default-setup)
  (pm/_org-mode-font-setup)
  (pm/_org-agenda-setup)
  (pm/_org-latex-setup)
  (pm/_org-src-setup)
  (pm/_org-hooks-setup)
  (pm/_org-babel-setup)
  )

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

;; Pretty mode !!!
(add-hook 'org-mode-hook #'+org-pretty-mode)



;;;
;; ORG-SUPERSTAR
;;;
(use-package! org-superstar
  :config
  (setq org-superstar-special-todo-items t
        org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-superstar-prettify-item-bullets t )
        )
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1)))

(setq org-ellipsis " ▾ "
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'all-the-icons-red)
        (?B . 'all-the-icons-orange)
        (?C . 'all-the-icons-yellow)
        (?D . 'all-the-icons-green)
        (?E . 'all-the-icons-blue)))

(appendq! +ligatures-extra-symbols
          `(:checkbox      "☐"
            :pending       "◼"
            :checkedbox    "☑"
            :list_property "∷"
            :em_dash       "—"
            :ellipses      "…"
            :arrow_right   "→"
            :arrow_left    "←"
            :title         "𝙏"
            :subtitle      "𝙩"
            :author        "𝘼"
            :date          "𝘿"
            :property      "☸"
            :options       "⌥"
            :startup       "⏻"
            :macro         "𝓜"
            :html_head     "🅷"
            :html          "🅗"
            :latex_class   "🄻"
            :latex_header  "🅻"
            :beamer_header "🅑"
            :latex         "🅛"
            :attr_latex    "🄛"
            :attr_html     "🄗"
            :attr_org      "⒪"
            :begin_quote   "❝"
            :end_quote     "❞"
            :caption       "☰"
            :header        "›"
            :results       ""
            :begin_export  "⏩"
            :end_export    "⏪"
            :properties    "⚙"
            :end           "∎"
            :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
            :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
            :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
            :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
            :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))

(set-ligatures! 'org-mode
  :merge t
  :checkbox      "[ ]"
  :pending       "[-]"
  :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :startup       "#+startup:"
  :macro         "#+macro:"
  :html_head     "#+html_head:"
  :html          "#+html:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :beamer_header "#+beamer_header:"
  :latex         "#+latex:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_html:"
  :attr_org      "#+attr_org:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :property      ":PROPERTIES:"
  :end           ":END:"
  :priority_a    "[#A]"
  :priority_b    "[#B]"
  :priority_c    "[#C]"
  :priority_d    "[#D]"
  :priority_e    "[#E]")

(plist-put +ligatures-extra-symbols :name "⁍")

;;;;;;;;;;;;;;;;
;; Taken from abrahm/weekly-report()
;;;;;;;;;;;;;;;;
(defun pm/org-weekly-report ()
  "Tasks that are changed or added in the last 7 days?"
  (interactive)
  (let ((start (format-time-string "%Y-%m-%d" (time-add (current-time) (seconds-to-time (- (* 60 60 24 7))))))
        (end (format-time-string "%Y-%m-%d")))
    (org-ql-search
     (org-agenda-files)
     `(or
       (and (todo) (ts :from ,start :to ,end))
       (and (todo "DONE") (closed :from ,start :to ,end)))
     :sort '(todo reverse)
     )))

;;;;;;;;;;;;;;;
;; ORG-APPEAR :
;; While org-hide-emphasis-markers is very nice, it can sometimes make edits
;; which occur at the border a bit more fiddley. We can improve this situation
;; without sacrificing visual amenities with the org-appear package.
;;;;;;;;;;;;;;;
(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))


(use-package! org-slideshow
  :after org
  :config
  ;;:hook (org-mode . org-slideshow-mode)
  )
