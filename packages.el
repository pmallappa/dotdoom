;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
(unpin! 
  ;;org
  ;;org-roam
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RESTORE EMACS to work in EMACS mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restoring old substitution behavior on s/S
;; Doom replaces the s and S keys with the evil-snipe package (a port of
;; vim-seek/vim-sneak for 2-character versions of f/F/t/T).
(package! evil-snipe :disable t)

;; Doom changes the behavior of the Y key in normal mode to yank-to-EOL
;; (equivalent to y$). This was to make it consistent with the C and D capital
;; operators, and because it was redundant with yy, which is easier to type than y$.
(setq! evil-want-Y-yank-to-eol nil)

;; Vim (and evil) move the cursor one character back when exiting insert mode
(setq evil-move-cursor-back nil)

(package! dracula-theme
  :recipe (:host github
          :repo "dracula/emacs"
          :files("dracula-theme.el")))

(package! solair-mode :disable t)
(after! solair-mode 
  (global-solair-mode -1 )
)

;;;
(package! emacs-snippets
  :recipe (:host github
           :repo "hlissner/emacs-snippets"
           :files ("*")))


(package! yasnippet-snippets
  :recipe (:host github
           :repo "AndreaCrotti/yasnippet-snippets"
           :files ("*")))

(package! leetcode
  :recipe (:host github :repo "kaiwk/leetcode.el"
           :files ("leetcode.el")
           )
  )
