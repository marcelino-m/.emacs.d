
(require-package 'ggtags)
(require 'ggtags)

(c-add-style "my-style"
             '("k&r"
               (c-offsets-alist . ((innamespace . [0])))))

(add-hook 'c++-mode-hook (lambda ()
                           (c-set-style "my-style")
                           (setq c-basic-offset 4
                                 tab-width 4
                                 indent-tabs-mode nil)

                           (setq compilation-skip-threshold 2)
                           (setq compilation-scroll-output 'first-error)
                           (define-key c++-mode-map (kbd "\C-s") 'swiper)
                           (font-lock-add-keywords nil
                                                   '(("\\."  . font-lock-keyword-face)
                                                     ("\\-\>". font-lock-keyword-face)
                                                     ("=="   . font-lock-keyword-face)
                                                     ("!="   . font-lock-keyword-face)
                                                     ("!"    . font-lock-keyword-face)
                                                     ("\<"   . font-lock-keyword-face)
                                                     ("\<="  . font-lock-keyword-face)
                                                     ("\>"   . font-lock-keyword-face)
                                                     ("\|"   . font-lock-keyword-face)
                                                     ("&"    . font-lock-keyword-face)
                                                     ("\>="  . font-lock-keyword-face)))
                           ))


(defun my-pretty ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("->" . 8594)    ;
          ("<=" . 10877)    ;
          (">=" . 10878)    ;
          )))


;;(add-hook 'c++-mode-hook 'my-pretty)
(setq compilation-ask-about-save nil)

(provide 'init-c++)
