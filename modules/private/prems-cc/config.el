;;; prems-cc/config.el  -*- lexical-binding: t; -*-

(use-package! ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  )

(use-package! modern-cpp-font-lock
  :after cc-mode
  :hook (c++-mode . modern-c++-font-lock-mode))

(setq  auto-mode-alist (append
             '(("\\.cc$"  . c++-mode)
               ("\\.cpp$" . c++-mode)
               ("\\.inl$" . c++-mode)
               ("\\.hh$"  . c++-mode)
               ("\\.hpp$" . c++-mode)
               )
             auto-mode-alist))

(after! cc-mode
  (setq +cc-default-header-file-mode 'c++-mode)

  ;; disable this as it causes issues when do doom/reload
  ;;(c-toggle-hungry-state 1)

  (add-hook! '(c-mode) (lambda()
                         ;;(c-add-style "prems/c" prems-c-style t)
                         (setq c-default-style "prems/c")))

  (add-hook! '(c++-mode) (lambda()
                           ;;(c-add-style "prems/cxx" prems-c-style)
                           (setq c-default-style "prems/cxx")))
  )


(use-package! highlight-doxygen
  :hook ((c-mode c++-mode) . highlight-doxygen-mode))
