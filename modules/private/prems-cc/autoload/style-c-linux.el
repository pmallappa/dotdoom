;;; private/prems-cc/autoload/style-linux.el -*- lexical-binding: t; -*-
;;;###autoload
(defconst prems-c-style-linux
  '("linux"
    (c-recognize-knr-p                      . nil)
    (indent-tabs-mode                       . t)
    (c-tab-always-indent                    . t)
    (c-basic-offset                         . 8)
    (tab-stop-list                          . (8 16 24 32 40 48 56 64 72 80 88 96 104 112 120))
    (c-comment-only-line-offset             . 0)
    (comment-column                         . 40)
    (c-indent-comments-syntactically-p            . nil)
    (c-toggle-auto-newline                  . t)
    (c-hanging-braces-alist                 . ((defun-open after)
                                               (defun-close before after)
                                               (class-open after)
                                               (class-close before after)
                                               (namespace-open after)
                                               (inline-open after)
                                               (inline-close before after)
                                               (block-open after)
                                               (block-close     . c-snug-do-while)
                                               (statement-case-open after)
                                               (substatement-open after)))
    (c-hanging-colons-alist                 . ((member-init-intro before)
                                               (inher-intro)
                                               (case-label after)
                                               (label after)
                                               (access-label after)))
    (c-cleanup-list                         . (scope-operator
                                               brace-else-brace
                                               brace-elseif-brace
                                               brace-catch-brace
                                               empty-defun-braces
                                               list-close-comma
                                               defun-close-semi))
    (c-offsets-alist                        . (
                                               (arglist-close
                                                .
                                                prems-c/lineup-arglist-tabs-only)
                                               (substatement-open                 . 0)
                                               (statement-cont                    . (c-lineup-assignments +))
                                               ;;(arglist-cont-nonempty)
                                               ;;(c-lineup-gcc-asm-reg)
                                               (label                             . /)
                                               (case-label                        . 0)
                                               (statement-case-open               . +)
                                               (statement-case-intro              . +) ; case w/o {
                                               (block-open                        . 0)
                                               (func-decl-cont                    . 0)
                                               (cpp-define-intro c-lineup-cpp-define +)
                                               (cpp-macro-cont                    . +)
                                               (cpp-macro                         . [0])
                                               (knr-argdecl-intro                 . -)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (modify-syntax-entry                    . (?_  . "w")) ; now '_' is not considered word-delimeter
    (c-echo-syntactic-information-p                   . t)
    ))
