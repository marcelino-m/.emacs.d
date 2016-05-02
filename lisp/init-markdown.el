(require-package 'markdown-mode)
(add-hook 'markdown-mode-hook (lambda ()
                                (ethan-wspace-mode 1)))
(provide 'init-markdown)
