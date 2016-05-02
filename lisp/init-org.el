(require-package 'org-bullets)

;; Bullets mode
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; Capture mode
(require 'org-capture)
(setq org-directory "~/org-files")
(setq org-default-notes-file "~/org-files/refile.org")


(setq org-capture-templates
      (quote
       (
        ("n" "Note" entry (file "~/org-files/notes.org")
         (file "~/.emacs.d/org-templates/note.org.tmpl"))
        ("t" "Todo" entry (file "~/org-files/refile.org")
         (file "~/.emacs.d/org-templates/todo.org.tmpl"))
        ("j" "Journal" entry (file+datetree "~/org-files/journal.org")
             (file "~/.emacs.d/org-templates/journal.org.tmpl"))
        )))

(setq org-todo-keywords
       '((sequence "TODO(t)" "DOING(a)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))


;; Export to LaTex

;; (quote (("t" "todo" entry (file "~/org-files/refile.org")
;;                "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("r" "respond" entry (file "~/org-files/refile.org")
;;                "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
;;               ("n" "note" entry (file "~/org-files/notes.org")
;;                "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;;               ("j" "Journal" entry (filfe+datetree "~/org-files/diary.org")
;;                "* %?\n%U\n" :clock-in t :clock-resume t)
;;               ("m" "Meeting" entry (file "~/org-files/refile.org")
;;                "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;;               ("p" "Phone call" entry (file "~/org-files/refile.org")
;;                "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;;               ("h" "Habit" entry (file "~/org-files/refile.org")
;;                "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))

(provide 'init-org)
