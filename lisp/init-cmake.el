(require-package 'cmake-mode)
(require-package 'company)

(require 'cmake-mode)
(require 'company-cmake)
(add-hook 'cmake-mode-hook  (lambda ()
                              (company-mode)))


(add-to-list 'auto-mode-alist '("CMakeList.txt" . cmake-mode))
(provide 'init-cmake)
