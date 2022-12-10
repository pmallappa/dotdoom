;;; private/prems-cc/autoload/style-c.el -*- lexical-binding: t; -*-

;;;###autoload
(defconst prems-c-style
  '("stroustrup"
    (indent-tabs-mode              . nil)
    (c-basic-offset                . 4)
    (tab-width                     . 4)
    (c-comment-only-line-offset    . 0)
    (c-offsets-alist               . ((statement-block-intro . +)
                                      (substatement-open         . 0)
                                      (label                     . 0)
                                      (statement-cont            . +)
                                      (innamespace               . 0)
                                      (inline-open               . 0)
                                      ))
    (c-hanging-braces-alist        . ((brace-list-open)
                                      (brace-list-intro)
                                      (brace-list-entry)
                                      (brace-list-close)
                                      (brace-entry-open)
                                      (block-close       . c-snug-do-while)
                                      (substatement-open before after)
                                      ;; structs have hanging braces on open
                                      (class-open        . (after))
                                      ;;
                                      (substatement-open . (after))
                                      ;;
                                      (class-close)
                                      ))
    )
  "Prems C Programming Style")

