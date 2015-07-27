(require-package 'company)
(require-package 'company-c-headers)
(require-package 'company-irony)
(require-package 'company-tern)

(require 'company)
(require 'company-irony)
(require 'irony)

;;(require 'company-cmake)
(require 'init-cmake)
(require 'company-c-headers)


(add-hook 'c++-mode-hook  '(lambda ()
                                  (irony-mode)
                                  (company-mode)
                                  ))

(add-hook 'c-mode-hook          '(lambda ()
                                        (irony-mode)
                                        (company-mode)))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'cmake-mode-hook      'company-mode)
(add-hook 'js2-mode-hook        'company-mode)
(add-hook 'sparql-mode-hook     'company-mode)

(add-to-list 'company-backends 'company-tern)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8")

(provide 'init-company)
