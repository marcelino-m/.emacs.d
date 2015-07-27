(require-package 'sparql-mode)
(require 'sparql-mode)

(add-to-list 'auto-mode-alist '("\\.rq$" . sparql-mode))
(provide 'init-sparkql)
