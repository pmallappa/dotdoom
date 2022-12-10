;;;###autoload
(defun pm/cpp-highlight-if0 ()
  "highlight c/c++ #if 0 #endif macros"
  ;; (interactive)
  (setq cpp-known-face 'default)
  (setq cpp-unknown-face 'default)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list '(("0" font-lock-comment-face default both)
                        ("1" default font-lock-comment-face both)))
  (cpp-highlight-buffer t))

;;;###autoload
(add-hook 'c-mode-common-hook 'pm/cpp-highlight-if0)
