(require-package 'hlinum)
(require-package 'yasnippet)
(require-package 'linum)

(require 'hlinum)
(require 'linum)
(require 'yasnippet)
(require 'fringe)
(require 'init-vimish-fold)

(add-hook 'prog-mode-hook
          '(lambda ()
;;             (fringe-mode '(8 . 6))
             (yas-reload-all)
             (yas-minor-mode)
             (define-key yas-minor-mode-map (kbd "<tab>") nil)
             (define-key yas-minor-mode-map (kbd "TAB") nil)
             (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
             (electric-pair-mode)
             (vimish-fold-mode)
             (setq tab-width 4
                   indent-tabs-mode nil)))


(provide 'init-prog-mode)
