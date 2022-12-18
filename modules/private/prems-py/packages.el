;;; prems-py/packages.el

;; LSP support for python
;;(package! lsp-pylsp)

;; Just trying to get black formatting working with my Doom setup. +black seems to do nothing.
(package! python-black)

;; Disable microsoft lsp server for python
(package! lsp-python-ms
	  :disable t)

