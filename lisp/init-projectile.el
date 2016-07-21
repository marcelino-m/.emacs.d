(require-package 'projectile)
(require-package 'helm-projectile)

(require 'projectile)
(require 'helm-projectile)

(setq projectile-keymap-prefix (kbd "C-c p"))
(projectile-global-mode)

(define-key projectile-mode-map (kbd "C-c p s s") 'helm-projectile-ag)
(setq projectile-switch-project-action '(lambda ()
                                          (projectile-dired)
                                          (projectile-find-file)
                                          ))

(setq projectile-enable-caching t)

(setq projectile-mode-line '(:eval (format " 𝚷(%s)" (projectile-project-name))))
(provide 'init-projectile)
