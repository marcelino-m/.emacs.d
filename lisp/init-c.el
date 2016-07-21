(require-package 'ggtags)
(require 'ggtags)

(c-add-style "my-style"
             '("k&r"
               (c-offsets-alist . ((innamespace . [0])))))


(add-hook 'c-mode-hook (lambda ()
                           (c-set-style "my-style")
                           (setq c-basic-offset 4
                                 tab-width 4
                                 indent-tabs-mode nil)

                           (setq compilation-skip-threshold 2)
                           (setq compilation-scroll-output 'first-error)
                           (define-key c-mode-map (kbd "\C-s") 'swiper)
                           ))


(provide 'init-c)
