(require-package 'cmake-mode)
(require 'cmake-mode)

(add-to-list 'auto-mode-alist '("CMakeList.txt" . cmake-mode))
(provide 'init-cmake)
