(require-package 'projectile)
(require-package 'helm-projectile)
;;(require-package 'perspective)
;;(require-package 'persp-projectile)

(require 'helm-projectile)
;; (persp-mode)
;; (require 'persp-projectile)

(setq projectile-keymap-prefix (kbd "C-c p"))


;;(define-key projectile-mode-map [?\s-p] 'projectile-persp-switch-project)
(define-key projectile-mode-map [?\s-j] 'projectile-find-file-dwim)
(define-key projectile-mode-map [?\s-d] 'helm-projectile-find-dir)
(define-key projectile-mode-map [?\s-o] 'helm-swoop)
(define-key projectile-mode-map [?\s-a] 'helm-projectile-find-other-file)
;;(global-set-key [(control tab)] 'persp-switch)

(define-key projectile-mode-map [?\s-f] 'helm-projectile-find-file)
(define-key projectile-mode-map [?\s-b] 'helm-projectile-switch-to-buffer)
(define-key projectile-mode-map [?\s-g] 'helm-projectile-grep)
(define-key projectile-mode-map [?\s-.] 'helm-etags-select)
(helm-projectile-on)

(projectile-global-mode)
(setq projectile-switch-project-action 'projectile-dired)

(setq projectile-mode-line '(:eval (format " Prj[%s]" (projectile-project-name))))
(provide 'init-projectile)
