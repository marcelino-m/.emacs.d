(require-package 'js2-mode)
(require-package 'ac-js2)
(add-to-list 'load-path "/home/marcelo/.nvm/versions/io.js/v1.3.0/lib/node_modules/tern/emacs/")
(require 'tern)
(require 'tern-auto-complete)
(require-package 'js2-refactor)
(require-package 'rainbow-delimiters)


(setenv "PATH" (concat "/home/marcelo/.nvm/versions/io.js/v1.3.0/bin/:" (getenv "PATH")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (setq mode-name "js2")
                           (auto-complete-mode t)
                           (tern-mode t)))

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
(require 'js2-refactor)

(provide 'init-js2)
