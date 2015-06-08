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
(setq helm-M-x-fuzzy-match t)

;;(global-set-key (kbd "M-s") 'helm-swoop)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-m") 'helm-all-mark-rings)


(provide 'init-helm)
