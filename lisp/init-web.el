(require-package 'web-mode)
(require-package 'emmet-mode)
(require 'web-mode)
(require 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(
        ("angular"    . "\\.html\\'")
        )
      )


(eval-after-load "company" '(add-to-list 'company-backends 'company-css))
(eval-after-load "company" '(add-to-list 'company-backends 'company-web-html))
(add-hook 'web-mode-hook '(lambda () (company-mode 1)))

(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-code-indent-offset 4)
                           (emmet-mode 1)))




(provide 'init-web)
