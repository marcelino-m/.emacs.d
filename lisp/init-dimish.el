(require-package 'diminish)
(require 'diminish)

(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))

(eval-after-load "ethan-wspace"
  '(diminish 'ethan-wspace-mode))

(add-hook 'emacs-lisp-mode-hook
  (lambda()
    (setq mode-name "el")))

(add-hook 'lisp-interaction-mode-hook
  (lambda()
    (setq mode-name "el-i")))

(eval-after-load "tern"
  '(diminish 'tern-mode))

(eval-after-load "company"
  '(diminish 'company-mode))

(eval-after-load "page-break-lines"
  '(diminish 'page-break-lines-mode))



(provide 'init-dimish)
