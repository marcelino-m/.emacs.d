
(require 'projectile)
(require 'helm-projectile)

(setq projectile-keymap-prefix (kbd "C-c p"))
(projectile-global-mode)

(define-key projectile-mode-map (kbd "C-c p s s") 'helm-projectile-ag)
(setq projectile-switch-project-action '(lambda ()
                                          (projectile-dired)
                                          (projectile-find-file)
                                          ))

;;(define-key projectile-mode-map [?\s-p] 'projectile-persp-switch-project)
;; (define-key projectile-mode-map [?\s-j] 'projectile-find-file-dwim)
;; (define-key projectile-mode-map [?\s-d] 'helm-projectile-find-dir)
;; (define-key projectile-mode-map [?\s-o] 'helm-swoop)
;; ;;(define-key projectile-mode-map (kbd "C-c p a") 'projectile-find-other-file-maybe-create)
;; (define-key projectile-mode-map [?\s-a] 'helm-projectile-find-other-file)

;; ;;(global-set-key [(control tab)] 'persp-switch)

;; (define-key projectile-mode-map [?\s-f] 'helm-projectile-find-file)
;; (define-key projectile-mode-map [?\s-b] 'helm-projectile-switch-to-buffer)
;; (define-key projectile-mode-map [?\s-g] 'helm-projectile-grep)
;; (define-key projectile-mode-map [?\s-.] 'helm-etags-select)
;; (helm-projectile-on)

;; (projectile-global-mode)

(setq projectile-mode-line '(:eval (format " 𝚷(%s)" (projectile-project-name))))
(provide 'init-projectile)
