
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(visual-fill-column))
 '(safe-local-variable-values
   '((eval setq-local flycheck-clang-include-path
      (list
       (expand-file-name "include"
                         (projectile-project-root))
       (expand-file-name "lib/include"
                         (projectile-project-root))))
     (projectile-enable-caching . t)
     (projectile-project-name . "aocl-crypto")
     (setq enable-local-eval t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
