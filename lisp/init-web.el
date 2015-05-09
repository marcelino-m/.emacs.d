(require-package 'web-mode)
(require-package 'ac-html)
(require-package 'auto-complete)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'ac-html)
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(add-hook 'web-mode-hook (lambda ()
                           (ac-config-default)
                           (setq web-mode-markup-indent-offset 2)
                           (setq web-mode-code-indent-offset 4)))

(add-hook 'html-mode-hook 'ac-html-enable)
(add-to-list 'web-mode-ac-sources-alist
             '("html" . (
                         ;; attribute-value better to be first
                         ac-source-html-attribute-value
                         ac-source-html-tag
                         ac-source-html-attribute)))


(provide 'init-web)
