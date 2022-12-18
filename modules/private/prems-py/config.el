;;; prems-py/config.el

(use-package! python-black
  :demand t
  :after python
  :config
  (add-hook! 'python-mode-hook #'python-black-on-save-mode)
  (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
  (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
  (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement))


(setq flycheck-python-pylint-executable "pylint")
;;(use-package! lsp-pyright
;;  :config
;;  (setq lsp-clients-python-command "pyright")
;;  :hook (python-mode . (lambda ()
;;                         (require 'lsp-pyright)
;;                         (lsp))))


(use-package! lsp
  :init
  (setq lsp-pyls-plugins-pylint-enabled t)
  (setq lsp-pyls-plugins-autopep8-enabled nil)
  (setq lsp-pyls-plugins-yapf-enabled t)
  (setq lsp-pyls-plugins-pyflakes-enabled nil)
)

