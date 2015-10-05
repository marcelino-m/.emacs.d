(require-package 'company)
(require-package 'company-tern)
(require-package 'ycmd)
(require-package 'company-ycmd)
(require-package 'company-web)

(require 'company)
(require 'ycmd)
(require 'company-web-html)
(require 'init-cmake)


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

(add-to-list 'company-backends 'company-tern)

(add-hook 'web-mode-hook (lambda ()
                           (set (make-local-variable 'company-backends) '(company-web-html company-css))
                           (company-mode t)))


(company-ycmd-setup)


(provide 'init-company)
