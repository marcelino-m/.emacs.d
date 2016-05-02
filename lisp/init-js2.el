(require-package 'js2-mode)
(require-package 'js2-refactor)
(require-package 'tern)
(require-package 'js-doc)

(add-to-list 'load-path  (concat (getenv "NVM_PATH") "/../node_modules/tern/emacs"))
;;(add-to-list 'load-path "/home/marcelo/.nvm/versions/node/v5.7.0/lib/node_modules/tern/emacs")


(require 'tern)
(require 'js2-refactor)
(require 'js-doc)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                           (setq mode-name "js2")
                           (tern-mode t)))



(provide 'init-js2)
