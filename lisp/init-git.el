(require-package 'gitignore-mode)
(require-package 'git-timemachine)
(require-package 'magit)
(require-package 'fullframe)

(require 'init-ispell)

(global-set-key [(f12)] 'magit-status)
(setq magit-save-repository-buffers 'dontask)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode 1)))

(provide 'init-git)
