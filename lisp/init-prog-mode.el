(require-package 'hlinum)
(require-package 'yasnippet)
(require-package 'linum)
(require-package 'ethan-wspace)

(require 'hlinum)
(require 'linum)
(require 'yasnippet)
(require 'fringe)
(require 'diminish)
(require 'ethan-wspace)

;;; Whitespace
(setq mode-require-final-newline nil)

(add-hook 'prog-mode-hook
          '(lambda ()
             (yas-reload-all)
             (yas-minor-mode)
             (define-key yas-minor-mode-map (kbd "<tab>") nil)
             (define-key yas-minor-mode-map (kbd "TAB") nil)
             (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
             (electric-pair-mode)
             (ethan-wspace-mode 1)
             (define-key prog-mode-map (kbd "\C-s") 'swiper)
             (setq tab-width 4
                   indent-tabs-mode nil)))


(provide 'init-prog-mode)
