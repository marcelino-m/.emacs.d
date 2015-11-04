(require-package 'zeal-at-point)
(require 'zeal-at-point)

(global-set-key "\C-cd" 'zeal-at-point)
(add-to-list 'zeal-at-point-mode-alist '(python-mode . "opencv python,python,python 3"))
(add-to-list 'zeal-at-point-mode-alist '(c++-mode . "boost,qt,qt 5,c++"))

zeal-at-point-mode-alist

(provide 'init-zeal)
