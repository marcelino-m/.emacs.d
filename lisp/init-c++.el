
(require-package 'auto-complete)

(require 'auto-complete)
(require 'auto-complete-config)


(add-hook 'c++-mode-hook (lambda ()
                           (ac-config-default)
                           (setq tab-width 4
                                 indent-tabs-mode nil)
                           (c-set-style "Stroustrup")))


(defun my-pretty ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("->" . 10230)    ;
          ("<=" . 10877)    ;
          (">=" . 10878)    ;
          )))


(add-hook 'c++-mode-hook 'my-pretty)

(provide 'init-c++)
