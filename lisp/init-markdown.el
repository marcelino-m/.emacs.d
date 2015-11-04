(require-package 'mkdown)
(setq markdown-content-type "text/html")
(setq path (cons mkdown-css-file-name ()))
(setq markdown-css-paths path)

(provide 'init-markdown)
