
;; (require-package 'auto-complete)
;; (require-package 'ggtags)
;; (require-package 'ac-etags)

;; (require 'ggtags)
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (require 'ac-etags)


;; (c-add-style "my-style"
;;              '("stroustrup"
;;                (c-offsets-alist . ((innamespace . [0])))))

;; (defun my-ac-mode ()
;;   (setq ac-quick-help-delay 0.5)
;;   (setq ac-auto-start 1)
;;   (setq ac-sources '(ac-source-words-in-same-mode-buffers  ac-source-dictionary ac-source-gtags ac-source-semantic)))

;; (add-hook 'c++-mode-hook (lambda ()
;;                            (ac-config-default)

;;                            (my-ac-mode)
;;                            (setq tab-width 4
;;                                  indent-tabs-mode nil)
;;                            (c-set-style "my-style")
;;                            (setq comment-start "/* " comment-end " */")
;;                            (add-to-list  'c-cleanup-list 'comment-close-slash)
;;                            ;; yas
;;                            (define-key yas-minor-mode-map (kbd "<tab>") nil)
;;                            (define-key yas-minor-mode-map (kbd "TAB") nil)
;;                            (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

;;                            ))


;; (defun my-pretty ()
;;   "make some word or string show as pretty Unicode symbols"
;;   (setq prettify-symbols-alist
;;         '(
;;           ("->" . 10230)    ;
;;           ("<=" . 10877)    ;
;;           (">=" . 10878)    ;
;;           )))


;; (add-hook 'c++-mode-hook 'my-pretty)

(require-package 'company)
(require 'company)
(require 'company-irony)
(require 'irony)
(require-package 'company-c-headers)
(require 'company-c-headers)


(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)


(add-to-list 'company-backends 'company-c-headers)

(provide 'init-c++)
