;;; private/prems-cc/autoload/style-c-bsd.el -*- lexical-binding: t; -*-

;;;###autoload
(defconst prems-c-style-bsd
  '("bsd"
    (c-basic-offset                 . 2)
    (c-tab-always-indent            . t)
    (c-comment-only-line-offset     . 0)
    (comment-column                 . 48)
    (c-hanging-braces-alist         . ((substatement-open before after)
                                       (brace-list-open)))
    (c-hanging-colons-alist         . ((member-init-intro before)
                                       (inher-intro)
                                       (case-label after)
                                       (label after)
                                       (access-label after)))
    (c-cleanup-list                 . (scope-operator
                                       empty-defun-braces
                                       defun-close-semi))
    (c-hanging-semi&comma-criteria  . nil)
    (c-offsets-alist                . ((statement-block-intro . +)
                                       (knr-argdecl-intro     . 0)
                                       (substatement-open     . 0)
                                       (label                 . -)
                                       (statement-cont        . +)
                                       (arglist-intro         . c-lineup-arglist-intro-after-paren)
                                       (arglist-close         . c-lineup-arglist)
                                       ))
    (c-indent-comment-alist         .  ((anchored-comment column . 0)
                                        (end-block space         . 1)
                                        (cpp-end-block space     . 2)
                                        (other  column           . 48) ))
    ;; outdent # (and not Â£!) at beginning of line
    (c-electric-pound-behaviour     . ((alignleft)))
    ;; these are default for C mode, but not for C++
    (comment-start                  .  "/* ")
    (comment-end                    .  " */")
    (c-echo-syntactic-information-p . t)
    (fill-column                    . 80)
    ))
