(require-package 'f)
(require-package 'company)
(require-package 'company-tern)
(require-package 'ycmd)
(require-package 'company-ycmd)
(require-package 'company-web)
(require-package 'company-shell)

(require 'company)
(require 'ycmd)
(require 'company-web-html)
(require 'init-cmake)
(require 'company-shell)


;;(require 'company-cmake)

(require 'company-ycmd)
(set-variable 'ycmd-server-command '("python" "/home/marcelo/src/ycmd/ycmd"))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'cmake-mode-hook      'company-mode)
(add-hook 'js2-mode-hook        'company-mode)
(add-hook 'sparql-mode-hook     'company-mode)
(add-hook 'c++-mode-hook        'company-mode)
(add-hook 'c-mode-hook          'company-mode)
(add-hook 'c++-mode-hook        'ycmd-mode)
(add-hook 'c-mode-hook          'ycmd-mode)
(add-hook 'sh-mode-hook         'company-mode)

(add-to-list 'company-backends 'company-tern)
(company-ycmd-setup)
(add-to-list 'company-backends 'company-shell)

(provide 'init-company)
