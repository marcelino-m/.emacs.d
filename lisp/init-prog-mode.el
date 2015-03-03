(require-package 'hlinum)
(require 'hlinum)
(require 'linum)

(add-hook 'prog-mode-hook
          '(lambda ()
             (require 'autopair)
             (linum-mode 1)
             (setq tab-width 4
                   indent-tabs-mode nil)))


(provide 'init-prog-mode)
