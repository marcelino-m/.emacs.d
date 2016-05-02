(require-package 'company-anaconda)
(require-package 'pyenv-mode)

(add-hook 'python-mode-hook '(lambda () (company-mode 1)))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'pyenv-mode)

(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))


(provide 'init-python)
