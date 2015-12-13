

(add-hook 'python-mode-hook '(lambda ()
                               (setq prettify-symbols-alist
                                     '(
                                       ("lambda" . 955)))))
(provide 'init-python)
