(require-package 'helm)
(require-package 'ag)
(require-package 'helm-ag)
(require-package 'helm-swoop)


(require 'helm)
(require 'ag)
(require 'helm-ag)
(require 'helm-swoop)

(helm-autoresize-mode 1)
(setq helm-split-window-in-side-p t)
(setq helm-autoresize-max-height 40)
(setq helm-autoresize-min-height 40)
(setq helm-buffers-fuzzy-matching t)


(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(setq helm-ag-insert-at-point 'symbol)

(provide 'init-helm)
