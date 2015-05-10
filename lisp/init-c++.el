
(require-package 'ggtags)
(require 'ggtags)


(c-add-style "my-style"
             '("stroustrup"
               (c-offsets-alist . ((innamespace . [0])))))


(add-hook 'c++-mode-hook (lambda ()
                           (setq tab-width 4
                                 indent-tabs-mode nil)
                           (c-set-style "my-style")
                           (setq comment-start "/* " comment-end " */")
                           (add-to-list  'c-cleanup-list 'comment-close-slash)
                           ;; yas
                           (define-key yas-minor-mode-map (kbd "<tab>") nil)
                           (define-key yas-minor-mode-map (kbd "TAB") nil)
                           (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
                           ))


(defun my-pretty ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("->" . 8594)    ;
          ("<=" . 10877)    ;
          (">=" . 10878)    ;
          )))


(add-hook 'c++-mode-hook 'my-pretty)


(provide 'init-c++)
