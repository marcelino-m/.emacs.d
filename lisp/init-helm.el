(require-package 'helm)
(require-package 'ag)
(require-package 'helm-ag)
(require-package 'helm-swoop)

(setq helm-buffers-fuzzy-matching t)

(global-set-key (kbd "M-s") 'helm-swoop)
(global-set-key (kbd "M-o") 'helm-swoop)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-m") 'helm-all-mark-rings)


(provide 'init-helm)
