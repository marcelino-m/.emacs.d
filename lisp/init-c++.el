(require-package 'company)
(require-package 'irony)
(require-package 'company-irony)


(add-hook 'c++-mode-hook (lambda ()
                           (setq tab-width 4
                                 indent-tabs-mode nil)
                           (c-set-style "Stroustrup")))



(require 'company)
(add-hook 'c++-mode-hook 'company-mode)

;; for irony
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; then company-irony
(require 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))


(provide 'init-c++)
