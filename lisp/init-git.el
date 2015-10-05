
(require-package 'magit)
(require 'init-ispell)
(global-set-key [(f12)] 'magit-status)

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode 1)))

(provide 'init-git)
(require-package 'gitignore-mode)
(require-package 'git-timemachine)
