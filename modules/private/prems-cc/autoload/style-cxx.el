;;; private/prems-cc/autoload/style-cxx.el -*- lexical-binding: t; -*-
;;;###autoload
(defun followed-by (cases)
  (cond ((null cases) nil)
        ((assq (car cases)
               (cdr (memq c-syntactic-element c-syntactic-context))) t)
        (t (followed-by (cdr cases)))))
;;;###autoload
(defconst prems-cxx-style
  `("ellemtel"
    (c-recognize-knr-p  . nil)
    (c-basic-offset     . 4)
    (indent-tabs-mode   . nil)
    (c-lineup-C-comments . t)
    (c-comment-only-line-offset . (0 . 0))
    (c-backslash-column         .  72)
    (c-backslash-max-column     . 128)
    (c-basic-offset             . 4)  ; Amount of basic offset used by `+'and
                                      ; `-' symbols in `c-offsets-alist'.
    (c-block-comment-prefix     .  "")
    (c-doc-comment-style        . javadoc)

    (c-hanging-braces-alist
     . ((defun-open             . (after)) ; Brace that opens a functions def
        (defun-close            . (before after)) ; Brace that closes a function definition
        (class-open             . (after))         ; Brace that opens a class definition
        (class-close            . (before after)) ; Brace that closes a class definition
        (inexpr-class-open      . (after))
        (inexpr-class-close     . (before))
        (brace-list-open        . (after))         ; Open brace of an enum or static array list.
        (brace-list-close       . (before after))  ; Close brace of an enum or static array list.
        (brace-entry-open       . (before after))  ; Subsequent lines in an enum or static array

        (namespace-open         . (after)) ; open brace for namespace
        (namespace-close        . ())      ; close brace for a namespace
        (inline-open            . (after)) ; open brace for in-class inline method
        (inline-close           . (before after)) ; close brace for in-class inline method
        (block-open             . (after))        ; open brace for statement block
        (block-close            . c-snug-do-while) ; close brace for statement block
        (extern-lang-open       . (after))         ; brace that opens an "extern" block
        (extern-lang-close      . (after))         ; brace that closes an "extern" block
        (statement-case-open    . (after)) ; first line in a switch-case block starting with a brace
        (substatement-open      . (after)) ; brace that opens a sub-statement block
        (module-open            . (after))
        (module-close           . ())
        (composition-open       . (after))
        (composition-close)     . ())
     )

    ;; other emacs variables:
    ;; (c-comment-continuation-stars "" t)
    ;; (c-echo-syntactic-information-p t)
    ;; (c-hanging-comment-ender-p nil t)
    ;; (c-hanging-comment-starter-p nil t)
    ;; (c-macro-shrink-window-flag          . t)

    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p  . t)
    (comment-column                     . 40)
    (c-indent-comment-alist             . ((other . (space . 2))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist
     . (
        (string         . 0)            ;inside a multi-line string
        (c              . 0)            ;inside a multi-line c style block comment

        ;; Brace that opens a function definition.
        (defun-open            . 0)

        ;; Brace that closes a function definition.
        (defun-close           . 0)

        ;; The first line in a top-level defun.
        (defun-block-intro     . +)

        ;; Brace that opens a class definition.
        (class-open            . 0)

        ;; Brace that closes a class definition.
        (class-close           . 0)

        ;; Brace that opens an in-class inline method.
        (inline-open           . 0)

        ;; Brace that closes an in-class inline method.
        (inline-close          . 0)

        ;; The region between a function definition's
        ;; argument list and the function opening brace
        ;; (excluding K&R argument declarations).  In C, you
        ;; cannot put anything but whitespace and comments
        ;; between them; in C++ and Java, throws declarations
        ;; and other things can appear in this context.
        (func-decl-cont         . ++)

        ;; First line of a K&R C argument declaration.
        (knr-argdecl-intro      . +)

        ;; Subsequent lines in a K&R C argument declaration.
        (knr-argdecl            . +)

        ;; The first line in a topmost construct definition.
        (topmost-intro          . 0)

        ;; Topmost definition continuation lines.
        (topmost-intro-cont     . (c-lineup-string-cont
                                  0))

        ;; First line in a member initialization list.
        (member-init-intro      . ++)

        ;; Subsequent member initialization list lines.
        (member-init-cont       . ++)

        ;; First line of a multiple inheritance list.
        (inher-intro            . ++)

        ;; A line containing only a comment introduction.
        (comment-intro          . 0)

        ;; A line containing only a comment introduction.
        (arglist-close          . c-lineup-arglist)

        ;; statement block open brace
        (block-open             . 0)

        ;; Statement block close brace.
        (block-close           . 0)

        ;; Open brace of an enum or static array list.
        (brace-list-open       . 0)

        ;; Close brace of an enum or static array list.
        (brace-list-close      . 0)

        ;; First line in an enum or static array list.
        (brace-list-intro      . +)

        ;; Subsequent lines in an enum or static array list.
        (brace-list-entry      . 0)

        ;; Subsequent lines in an enum or static array
        ;; list that start with an open brace.
        (brace-entry-open      . +)

        ;; A C (or like) statement.
        (statement             . (c-lineup-runin-statements
                                  0))

        ;; A continuation of a C (or like) statement.
        (statement-cont         .
                                (c-lineup-assignments
                                 c-lineup-math
                                 c-lineup-string-cont
                                 c-lineup-cascaded-calls
                                 +))

        ;; The first line in a new statement block.
        (statement-block-intro  . +)

        ;; The first line in a case "block".
        (statement-case-intro    . +)

        ;; The first line in a case block starting with brace.
        (statement-case-open     . +)

        ;; The first line after an if/while/for/do/else.
        (substatement           . +)

        ;; The brace that opens a substatement block.
        (substatement-open      . 0)

        ;; Labelled line after an if/while/for/do/else.
        (substatement-label     . /)


        (inline-open            . 0)
        (substatement-open . 0)

        ;; An ordinary label
        (label                  . /)

        ;; A "case" or "default" label.
        (case-label             . +)

        ;; C++ private/protected/public access label.
        (access-label           . /)

        ;; The "while" that ends a do/while construct.
        (do-while-closure      . 0)

        ;; The "else" of an if/else construct.
        (else-clause           . 0)

        ;; The "catch" or "finally" of a try/catch construct.
        (catch-clause          . 0)

        ;; A line containing only a comment introduction.
        (comment-intro         . 0)

        ;; The first line in an argument list.
        (arglist-intro         . (c-lineup-arglist-intro-after-paren
                                  +))

        ;; Subsequent argument list lines when no
        ;; arguments follow on the same line as the
        ;; arglist opening paren.
        (arglist-cont          . (c-lineup-string-cont
                                  c-lineup-arglist-intro-after-paren
                                  c-lineup-argcont
                                  +))

        ;; Subsequent argument list lines when at
        ;; least one argument follows on the same
        ;; line as the arglist opening paren.
        (arglist-cont-nonempty . (c-lineup-string-cont
                                  c-lineup-arglist-intro-after-paren
                                  c-lineup-argcont
                                  +))

        ;; The solo close paren of an argument list.
        (arglist-close         . (c-lineup-argcont
                                  c-lineup-arglist-intro-after-paren
                                  -))

        ;; Lines continuing a stream operator construct.
        (stream-op             . (c-lineup-streamop +))

        ;; The construct is nested inside a class definition.
        ;; Used together with e.g. `topmost-intro'.
        (inclass               . +)

        ;; The start of a C preprocessor macro definition.
        (cpp-macro             . [0])

        ;; Inside a multi-line C preprocessor macro definition.
        (cpp-macro-cont        . [8])

        ;; A C++ friend declaration.
        (friend                . 0)

        ;; Brace that opens an "extern" block.
       (extern-lang-open      . 0)

       ;; Brace that closes an "extern" block.
       (extern-lang-close     . 0)

       ;; Analogous to the `inclass' syntactic symbol,
       ;; but used inside "extern" blocks.
       (inextern-lang         . +)

       (namespace-open        . 0)

       (namespace-close       . 0)

       ;; Similar to the three `extern-lang' symbols, but for
       ;; C++ "namespace" blocks.
       (innamespace           . 0)
       ;; (innamespace .
       ;;    (lambda (x)
       ;;      (if (followed-by
       ;;         '(innamespace namespace-close)) 0 '+)))

       (module-open           . 0)

       (module-close          . 0)

       ;; Similar to the three `extern-lang' symbols, but for
       ;; CORBA IDL "module" blocks.
       (inmodule              . +)

       (composition-open      . 0)

       (composition-close     . 0)

       ;; Similar to the three `extern-lang' symbols, but for
       ;; CORBA CIDL "composition" blocks.
       (incomposition         . +)

       ;; C++ template argument list continuations.
       (template-args-cont    . (c-lineup-template-args +))

       ;; In the header or body of a lambda function.
       (inlambda              . +)

       ;; Continuation of the header of a lambda function.
       (lambda-intro-cont     . ++)

       ;; The statement is inside an expression.
       (inexpr-statement      . +)

       ;; The class is inside an expression.  Used e.g. for
       ;; Java anonymous classes.
       (inexpr-class          . +)
       )
     ))
  "Prems C++ Programming Style.")
