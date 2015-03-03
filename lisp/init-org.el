(require 'org-capture)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(setq org-log-done 'time)
(setq org-todo-keywords
       '((sequence "TODO" "DOING" "DONE")))

(setq org-capture-templates
      '(
        ("l" "Task log TiM" item (file+datetree "~/Documents/org/tim/tasklog.org")
         "- %?\n  %i\n")
        ("s" "Smartsupply timelog TiM" item (file+datetree "~/Documents/org/smartsupply/timelog.org")
         "- %?\n  %i\n")
        ("o" "Hacer cuando tengas tiempo " item (file+headline "~/Documents/org/personal/para-cuando-bueque-que-hacer.org" "Tareas")
         "** TODO %?\n  %i\n")

        ))



(provide 'init-org)
