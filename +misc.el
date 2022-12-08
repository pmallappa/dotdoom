;; Open scratch buffer in lisp-interaction-mode
(defun pm/create-scratch-buffer nil
   "Create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "* scratch(elisp) *"))
   (lisp-interaction-mode))

