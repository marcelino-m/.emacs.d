(require-package 'openwith)
(require 'openwith)

(setq openwith-associations
            (list
             (list (openwith-make-extension-regexp
                    '("db"))
                   "sqlitebrowser"
                   '(file))
             ))

(openwith-mode 1)

(provide 'init-openwith)
