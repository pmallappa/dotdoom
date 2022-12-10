;;; prems-cc/config.el 

(defun pm/c-style-setup()
  "Meant to be addeded to `c-mode-common-hook'"
  (interactive)
  (c-add-style "prems/linux" prems-c-style-linux t)
  (c-add-style "prems/bsd" prems-c-style-bsd t)
  (c-add-style "prems/c" prems-c-style t)
  (c-add-style "prems/cxx" prems-cxx-style t)
  )

(add-hook 'c-mode-common-hook (lambda()
                                  (pm/c-style-setup)))
