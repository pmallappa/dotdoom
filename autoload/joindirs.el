;;; autoload
(defun pm/joindirs (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (dotemacs-joindirs \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"

  (if (not dirs)
      root
    (apply 'pm/joindirs
           (expand-file-name (car dirs) root)
           (cdr dirs))))
