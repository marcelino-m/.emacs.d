;;  adjust the garbage collection param
(setq gc-cons-threshold (* 128 (expt 2 20))) ;; 128mb

;; boostrap straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


(straight-use-package 'use-package)


;; taken from https://gustafwaldemarson.com/posts/set-variable-in-hook/
(defmacro set-variable-in-hook (hook variable value &optional name)
  "Create a proper  HOOK function for setting  VARIABLE to VALUE.
NAME can be used to set the name of the defined function."
  (let* ((hname (symbol-name hook))
         (vname (symbol-name variable))
         (fname (intern (or name (format "set-%s-in-%s" vname hname)))))
    `(progn
       (defun ,fname ()
         (setq-local ,variable ,value))
       (add-hook (quote ,hook) (function ,fname)))))


(define-key global-map (kbd "C-<backspace>")
  (lambda (args)
    "First delete blanks before point if there is at least three blanks"
    (interactive "p")
    (if (looking-back "[ \t]\\{3,\\}" nil t)
        (replace-match "" nil nil)
      (delete-region (point) (progn (forward-word (- args)) (point))))))

(define-key global-map (kbd "M-d")
  (lambda (args)
    "First delete blanks after point if there is at least three blanks"
    (interactive "p")
    (if (looking-at "[ \t]\\{3,\\}")
        (replace-match "" nil nil)
      (delete-region (point) (progn (forward-word args) (point))))))


(use-package frame
  :init
  ;; in gnome stealing focus doesn't work correctly
  (add-hook 'after-make-frame-functions
            (lambda (frame) (interactive) (select-frame-set-input-focus frame))))


(use-package simple
  :after hydra

  :config
  (advice-add 'yank
              :around
              (lambda (origfn &rest args)
                "flashing after yanked text"
                (let ((beg (point)))
                  (apply origfn args)
                  (flash-region beg (point) 'highlight 0.1))))



  (defun ma/kill-ring-save-line-or-region (beg end &optional region)
    "Save current  line to kill ring  if no region is  active, with
feedback."
    (interactive (list (mark) (point)))
    (if mark-active
        (kill-ring-save beg end region)
      (let (beg end)
        (save-excursion
          (back-to-indentation)
          (setq beg (point))
          (end-of-line)
          (skip-syntax-backward " ")
          (setq end (point))
          (flash-region beg end 'highlight 0.1)
          (kill-ring-save beg end)))))
  (define-key global-map (kbd "C-w") 'ma/kill-line-or-region))


(use-package flash-region
  :straight t)


(use-package use-package-chords
  :straight t
  :config (key-chord-mode 1))

(use-package delight
  :straight t)

(use-package diminish
  :straight t)

;; notify when emacs is ready
;; I run emacs in server mode set a systemd units
(use-package notifications
  :init
  (add-hook 'after-init-hook #'(lambda ()
                                 (notifications-notify
                                  :title "Emacs"
                                  :body "I am ready to hack!"
                                  :urgency 'low))))


(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; global options

(tool-bar-mode     -1)
(menu-bar-mode     -1)
(blink-cursor-mode  1)
(delete-selection-mode)
(show-paren-mode t)
(xterm-mouse-mode)

(setq-default
 tab-width                            4
 cursor-type                          'box
 case-fold-search                     t
 column-number-mode                   t
 mouse-yank-at-point                  t
 set-mark-command-repeat-pop          t
 tooltip-delay                        0
 save-interprogram-paste-before-kill  t
 inhibit-startup-screen               t
 truncate-lines                       t
 truncate-partial-width-windows       nil
 visible-bell                         nil
 ediff-window-setup-function          'ediff-setup-windows-plain
 ediff-split-window-function          'split-window-horizontally
 ring-bell-function                   'ignore
 mode-require-final-newline           nil
 mouse-wheel-progressive-speed        nil
 mouse-wheel-scroll-amount            '(2 ((shift) . 1) ((control) . nil))
 imenu-auto-rescan                    t
 indent-tabs-mode                     nil
 scroll-preserve-screen-position      t
 scroll-step                          1
 hscroll-step                         1
 vc-follow-symlinks                   t
 auto-hscroll-mode                    'current-line
 async-shell-command-buffer           'new-buffer
 default-input-method                 "latin-prefix"
 frame-resize-pixelwise                t
 browse-url-browser-function          'browse-url-firefox
 default-frame-alist                  '((width                . 0.80)
                                        (height               . 0.65)
                                        (vertical-scroll-bars .  nil)
                                        (cursor-color         . "#e52b50")
                                        (font                 . "Fira Code-9")))

;; unbind from global map
(global-unset-key (kbd "<menu>"))

(defalias 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; save backups
(setq
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions -1
 version-control t
 vc-make-backup-files t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))


;; Custom options history
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)


;; Setup packages
(global-unset-key (kbd "S-<down-mouse-1>"))
(global-unset-key (kbd "S-<mouse-3>"))


(use-package exec-path-from-shell
  :straight t
  :config
  (dolist (var '("GOPATH" "PYTHONUSERBASE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "~/.local/bin/")
  (add-to-list 'exec-path "/home/marce/.nvm/versions/node/v22.7.0/bin")
  (add-to-list 'exec-path (concat (getenv "PYTHONUSERBASE") "/bin")))


(use-package org
  :straight t
  :load-path "./defuns"
  :mode (("\\.org\\'" . org-mode))
  :bind (:map org-mode-map
              ("C-c C-v t" . ma/toggle-current-src-block))

  :custom-face
  (org-ellipsis  ((t (:underline nil :foreground nil))))

  :config
  (require 'org-defun)

  (unbind-key "C-c C->" org-mode-map)
  (unbind-key "C-," org-mode-map)
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (setq org-ellipsis "...▼")

  (setq org-emphasis-alist '(("*" (:inherit bold :bold t :foreground "#df6967"))
                            ("/" italic)
                            ("_" underline)
                            ("=" (:inherit org-verbatim :foreground "#8D8D8D"))
                            ("~" (:inherit org-code :foreground "#8D8D8D"))
                            ("+"
                             (:strike-through t))))


  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; new line behavior on new item
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

  (setq org-startup-folded t)
  (setq org-cycle-separator-lines 0)
  ;; prevent org mode repositioning text when cicle visibility
  (remove-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change)

  ;; log state chages into a drawer
  (setq org-log-done 'time)

  ;; block parent todo's until subtask are marked as done
  (setq org-enforce-todo-dependencies t)

  ;; control inline image size,
  (setq org-image-actual-width 300)

  ;; priority range
  (setq org-priority-highest ?A)
  (setq org-priority-lowest  ?G)
  (setq org-priority-default ?D)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "DOING(d)" "|" )
          (sequence
           "WAITING(w)" "|" )
          (type
           "|" "CANCELED(c)"  "DONE(e)")))

  ;; (setq org-todo-keyword-faces
  ;;       '(("WANT"   . "gray")
  ;;         ("WAIT"   . "Yellow")
  ;;         ("TODO"   . "OrangeRed")
  ;;         ("DOING"  . "Orange")
  ;;         ("STOPPED" . "Yellow")
  ;;         ("DONE"    . "SpringGreen")
  ;;         ("DELEGATED" . "SpringGreen")
  ;;         ("CANCELED"  . "SpringGreen")))



  (setq org-tag-alist '((:startgrouptag)
                        ("@read")
                        (:grouptags)
                        ("article")
                        ("book")
                        (:endgrouptag)

                        (:startgrouptag)
                        ("@play")
                        (:grouptags)
                        ("podcast")
                        ("video")
                        ("audiobook")
                        (:endgrouptag)

                        (:startgrouptag)
                        ("@code")
                        (:grouptags)
                        ("chore" . ?c)
                        ("fix" . ?F)
                        ("bug" . ?b)
                        ("feat" . ?f)
                        ("refactor" . ?r)
                        ("check"   . ?R)
                        ("next"    . ?N)
                        (:endgrouptag)

                        (:startgrouptag)
                        ("@feedback")
                        (:grouptags)
                        ("lean" . ?l)
                        ("one2one")
                        ("leadmeeting")
                        (:endgrouptag)

                        (:startgrouptag)
                        ("@people")
                        (:grouptags)
                        ("{p_.+}")
                        (:endgrouptag)

                        (:startgrouptag)
                        ("@team")
                        (:grouptags)
                        ("{t_.+}")
                        (:endgrouptag)

                        ("interesting")
                        ("emacs")
                        ("org")
                        ("need")
                        ("idea")
                        ("home")
                        ("finance")
                        ("english")
                        ("trekking")
                        ("gis")

                        ("work" . ?w)
                        ("pined" . ?p)
                        ("standup")
                        ("iteracion")
                        ("office" . ?o)))


  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "python3")
  (setq org-babel-results-keyword "results")

  ;; redisply omages inline when image change
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell   . t)
     (js      . t)
     (R       . t)
     (sql     . t)
     (sqlite  . t)
     (python  . t)
     (ditaa   . t)
     (calc    . t)
     (go      . t))))


(use-package org-contrib
  :straight t
  :after org
  :diminish)

(use-package org-inlinetask
  :after org
  :diminish
  :init
  (setq org-inlinetask-min-level 5))

(use-package ob-go
  :straight t
  :diminish)



(use-package org-src
  :custom
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2018-08/msg00127.html
  ;; prevent problem with diff src block
  (org-src-preserve-indentation t)
  (org-src-fontify-natively   t)
  (org-src-tab-acts-natively  t)
  (org-src-window-setup       'current-window)

  :config
  (add-hook
   'org-src-mode-hook
   (lambda () (setq org-src--saved-temp-window-config nil))))


(use-package org-agenda
  :config
  (add-hook 'org-agenda-mode-hook #'hl-line-mode)

  ;; tags are bad aligned when display-line-numbers mode is enabled or
  ;; when   fringe  are   disabled,   the   former  happened   because
  ;; window-text-width  function  no  subtract the  columns  used  for
  ;; display-line-numbers and the latest are because the latest column
  ;; is not used for display text, in both cases tags appear truncated
  ;; at end of line in agenda views
  (advice-add 'org-agenda-align-tags
              :around
              (lambda (origfn &rest args)
                (if (fboundp 'line-number-display-width)
                    (cl-letf* ((winw (window-text-width))
                               (dlnw (line-number-display-width 'columns))
                               (lf (car (window-fringes)))
                               (rf (cadr (window-fringes)))
                               (maybe1 (if (or (zerop lf) (zerop rf))
                                           1
                                         0))
                               ((symbol-function 'window-text-width) (lambda (&optional _ _) (round (- winw dlnw maybe1)))))
                      (apply origfn args))
                  (apply origfn args))))


  ;; narrow item when go to it
  (advice-add 'org-agenda-goto :after
              (lambda (&rest args)
                (org-narrow-to-subtree)))

  (setq org-agenda-tags-column         82
        org-tags-column                82
        org-agenda-todo-list-sublevels nil
        org-agenda-block-separator (make-string org-tags-column ?=))


  (setq org-agenda-files
      (mapcar 'abbreviate-file-name
              (split-string
               (shell-command-to-string "find ~/syncthing/org/ -type f -name \"*.org\"") "\n" t)))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda . " %i %?-12t% s")
  ;;         (todo . " %i> ")
  ;;         (tags . " %i> ")
  ;;         (search . " %i> ")))

  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down category-down todo-state-up)
          (tags priority-down category-keep)
          (search category-keep)))

  (setq org-agenda-custom-commands
        '(("A" "Personal agenda for current day or week"
           ((agenda "")
            (tags "+pined"
                  ((org-use-tag-inheritance nil)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down)))))
           ((org-agenda-tag-filter  '("-work"))))

          ("I" "Very Personal related task" tags-todo "-work-home"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))))

          ("i" "Personal related task" tags-todo "-work"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))))

          ("n" "Quick notes" tags "-work"
           ((org-use-tag-inheritance nil)
            (org-agenda-files '("~/syncthing/org/capture/quick-notes.org"))))

          ("h" "Home related task" tags-todo "+home"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))))

          ("w" . "Work related comand")

          ("wa" "Agenda for current day or week"
           ((agenda "")
            (tags "+pined"
                  ((org-use-tag-inheritance nil)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down))))
            ;; (tags-todo "+iteracion")
            ;; (tags-todo "+standup")
            ;; (tags-todo "+office-pined")
            )
           ((org-agenda-tag-filter-preset '("+work"))))

          ("wt" "All todos" tags-todo "+work"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))))

          ("wc" "Coded related todos"
           ((tags-todo "+next")
            (tags-todo "+bug")
            (tags-todo "+fix")
            (tags-todo "+feat")
            (tags-todo "+refactor")
            (tags-todo "+check")
            (tags-todo "+@code-bug-fix-feat-chore-refactor-check-next"))
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-tag-filter-preset '("+work"))))

          ("wn" "Quick notes" tags "+work"
           ((org-use-tag-inheritance nil)
            (org-agenda-include-inactive-timestamps nil)
            (org-agenda-files '("~/syncthing/org/capture/work/quick-notes.org"))))

          ("wr" "Retro and Leand" tags "+work"
           ((org-use-tag-inheritance nil)
            (org-agenda-files '("~/syncthing/org/capture/work/retro-and-leancoffe.org"))))

          ("wj" "Journal personal" search "{[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-}"
           ((org-agenda-sorting-strategy '(alpha-down))
            (org-use-tag-inheritance nil)
            (org-agenda-files '("~/syncthing/org/capture/work/journal.org"))))

          ("wJ" "Journal team" tags "+work"
           ((org-agenda-max-entries 20)
            (org-use-tag-inheritance nil)
            (org-agenda-files '("~/syncthing/org/capture/work/journal-team.org"))))))
  )


(use-package org-capture
  :config
  (define-key global-map (kbd "C-c x") 'org-capture)

  (setq org-capture-templates
        '(("t" "Task"
           entry (file "~/syncthing/org/capture/task.org")
           "* TODO %? \n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("f" "Would be nice doing it... some time"
           entry (file "~/syncthing/org/capture/wanted.org")
           "* TODO [#G] %? :@void:want:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("n" "Note: Quick and misc note about anything"
           entry (file "~/syncthing/org/capture/quick-notes.org")
           "* %? \n:LOGBOOK:\n:CREATED: %U \n:END:" :prepend t :empty-lines-after 2)

          ("h" "Home and domestic related task"
           entry (file "~/syncthing/org/capture/home-task.org")
           "* TODO %? :home:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("w" "work related captures")
          ("wt" "Task"
           entry (file "~/syncthing/org/capture/work/task.org")
           "* TODO %? :work:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wn" "Note: Quick and misc note about anything"
           entry (file "~/syncthing/org/capture/work/quick-notes.org")
           "* %T %? :work:\n:LOGBOOK:\n:CREATED: %U \n:END:" :prepend t :empty-lines-after 2)

          ;;;;;;;;;;;; one to one related capture
          ("wo" "one to one related task")
          ("wom" "thinhg that I want to share"
           entry (file+headline "~/syncthing/org/capture/work/oneone-meeting.org" "Marcelo")
           "* TODO %? :work:one2one:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wof" "thinhg that I want to share to felipe"
           entry (file+headline "~/syncthing/org/capture/work/oneone-meeting.org" "Felipe")
           "* TODO %? :work:one2one:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wor" "thinhg that I want to share to roberto"
           entry (file+headline "~/syncthing/org/capture/work/oneone-meeting.org" "Roberto")
           "* TODO %? :work:one2one:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("woc" "thinhg that I want to share to fernando"
           entry (file+headline "~/syncthing/org/capture/work/oneone-meeting.org" "Fernando")
           "* TODO %? :work:one2one:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wol" "thinhg that I want to share to lord"
           entry (file+headline "~/syncthing/org/capture/work/oneone-meeting.org" "Edgardo")
           "* TODO %? :work:one2one:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wop" "thinhg that I want to share to rosario"
           entry (file+headline "~/syncthing/org/capture/work/oneone-meeting.org" "Rosario")
           "* TODO %? :work:one2one:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ;;;;;;;;;;;

          ("wr" "Retro and Lean"
           entry (file+headline "~/syncthing/org/capture/work/retro-and-leancoffe.org" "Retrospective")
           "* TODO %? :work:retro:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wl" "Lead meeting"
           entry (file+headline "~/syncthing/org/capture/work/lead-meeting.org" "Topics to deal with")
           "* TODO %? :work:leadmeeting:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wj" "Journal"
           item (file+olp+datetree "~/syncthing/org/capture/work/journal.org")
           "%?" :tree-type week)

          ("wJ" "Journal team"
           item (file+olp+datetree "~/syncthing/org/capture/work/journal-team.org")
           "%?" :tree-type week))))


(use-package org-indent
  :diminish
  :hook (org-mode . org-indent-mode)
  :custom
  (org-indent-indentation-per-level 2))

(use-package org-bullets
  :straight t
  :diminish
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("჻")))

(use-package org-refile
  :init
  (setq org-refile-targets  '((nil :maxlevel . 3)
                              (org-agenda-files :maxlevel . 3)))

  ;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm))


(use-package org-journal
  :straight t
  :bind (("C-c C-j" . org-journal-new-entry))

  :config
  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+title: Daily Journal")
       (`weekly "#+title: Weekly Journal")
       (`monthly "#+title: Monthly Journal")
       (`yearly "#+title: Yearly Journal"))))


  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d.journal")
  (setq org-journal-file-header 'org-journal-file-header-func)
  (setq org-journal-date-format "%A, %Y/%m/%d")
  (setq org-journal-dir "~/syncthing/org/journal"))

(use-package calendar
  :custom
  (calendar-week-start-day 1))

(use-package eldoc
  :diminish eldoc-mode)

(use-package info
  :config
  (define-key Info-mode-map (kbd "<prior>") 'scroll-down-1)
  (define-key Info-mode-map (kbd "<next>") 'scroll-up-1))

(use-package info+
  :straight (info+ :fork  (:host github :repo "SrAceves/info-plus")))


(use-package ethan-wspace
  :straight t
  :diminish ethan-wspace-mode
  :init
  (add-hook 'org-mode            #'ethan-wspace-mode)
  (add-hook 'org-src-mode-hook   #'ethan-wspace-clean-all)
  (add-hook 'prog-mode-hook      #'ethan-wspace-mode)
  (add-hook 'markdown-mode-hook  #'ethan-wspace-mode)
  (add-hook 'LaTeX-mode-hook     #'ethan-wspace-mode)
  (add-hook 'yaml-mode-hook      #'ethan-wspace-mode)
  (add-hook 'see-mode-hook       #'ethan-wspace-mode)
  (add-hook 'ledger-mode-hook    #'ethan-wspace-mode)
  (add-hook 'yaml-mode-hook      #'ethan-wspace-mode))

(use-package spacemacs-theme
  :straight t
  :disabled
  :defer
  :custom
  (spacemacs-theme-org-height nil)
  (spacemacs-theme-comment-bg nil)
  (spacemacs-theme-comment-italic t)

  :custom-face
  (org-block            ((t :background nil  :extend nil)))
  (org-block-begin-line ((t (:underline "#bc6ec5" :background nil  :foreground "#5d4d7a" :extend nil))))
  (org-block-end-line   ((t (:overline nil  :underline nil  :foreground "#5d4d7a" :background nil :extend nil))))
  (org-checkbox         ((t :box nil)))

  :init
  (defun ma/setup-appearence (frame)
    (with-selected-frame frame
      (remove-hook 'after-make-frame-functions #'ma/setup-appearence)
      (load-theme 'spacemacs-dark t)
      (set-face-attribute 'region nil :background "#3d3b40")
      (add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-block-begin-line 'unspecified :extend nil)))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'ma/setup-appearence)
    (load-theme 'spacemacs-dark t)
    (set-face-attribute 'region nil :background "#3d3b40")))

(use-package solarized-theme
  :straight t
  :custom
  (solarized-use-variable-pitch      nil)
  (solarized-high-contrast-mode-line t)
  (solarized-use-less-bold           t)
  (solarized-use-more-italic         t)
  (solarized-emphasize-indicators    nil)
  (solarized-scale-org-headlines     nil)
  (solarized-height-minus-1          1.0)
  (solarized-height-plus-1           1.0)
  (solarized-height-plus-2           1.0)
  (solarized-height-plus-3           1.0)
  (solarized-height-plus-4           1.0)

  :custom-face
  (org-block            ((t :background nil  :extend t)))
  (org-block-begin-line ((t (:underline "#c2bdb2"  :foreground "#c2bdb2"))))
  (org-block-end-line   ((t (:overline nil  :underline nil  :foreground "#c2bdb2"))))
  (org-checkbox         ((t :box nil)))
  ;;(magit-diff-added     ((t (:background nil  :foreground "#2f4321"))))
  ;; (magit-diff-changed   ((t (:background nil  :foreground nil))))
  ;; (magit-diff-removed   ((t (:background nil  :foreground nil))))
  ;; (magit-diff-added-highlight    ((t (:background nil  :foreground nil))))
  ;; (magit-diff-changed-highlight  ((t (:background nil  :foreground nil))))
  ;; (magit-diff-removed-highlight  ((t (:background nil  :foreground nil))))
  (diff-refine-added   ((t (:background nil  :foreground "#9fd702"))))
  (diff-refine-changed ((t (:background nil  :foreground "#0000ff"))))
  (diff-refine-removed ((t (:background nil  :foreground "#ff0000"))))



  :config
  (load-theme 'solarized-light t))


(use-package zenburn-theme
  :disabled
  :straight t
  :config
  (setq zenburn-override-colors-alist
        '(("zenburn-fg-2"     . "#6D6D6D")
          ("zenburn-fg-1"     . "#878787")
          ("zenburn-fg-05"    . "#A2A2A2")
          ("zenburn-fg"       . "#BDBDBD")
          ("zenburn-fg+1"     . "#D7D7D7")
          ("zenburn-fg+2"     . "#F2F2F2")
          ("zenburn-bg-2"     . "#1f1f1f")
          ("zenburn-bg-1"     . "#252525")
          ("zenburn-bg-08"    . "#2b2b2b")
          ("zenburn-bg-05"    . "#313131")
          ("zenburn-bg"       . "#383838")
          ("zenburn-bg+05"    . "#3e3e3e")
          ("zenburn-bg+1"     . "#414141")
          ("zenburn-bg+2"     . "#4c4c4c")
          ("zenburn-bg+3"     . "#575757")
    ))

  (load-theme 'zenburn t)
  (let ((custom--inhibit-theme-enable nil))
    (zenburn-with-color-variables
      (custom-theme-set-faces
       'zenburn

       `(cursor ((t (:foreground ,zenburn-fg :background ,zenburn-fg))))
       `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))

       ;; display-line-numbers
       `(line-number ((t (:foreground ,zenburn-bg+3 :background ,zenburn-bg :bold nil :slant italic :box nil))))
       `(line-number-current-line ((t (:inherit line-number :foreground ,zenburn-yellow-2 ))))

       ;; magit
       `(magit-diff-added    ((t (:background unspecified  :foreground ,zenburn-green))))
       `(magit-diff-changed  ((t (:background unspecified  :foreground ,zenburn-yellow-1))))
       `(magit-diff-removed  ((t (:background unspecified  :foreground ,zenburn-red-2))))
       `(magit-diff-added-highlight    ((t (:background ,zenburn-bg+05  :foreground ,zenburn-green))))
       `(magit-diff-changed-highlight  ((t (:background ,zenburn-bg+05  :foreground ,zenburn-yellow-1))))
       `(magit-diff-removed-highlight  ((t (:background ,zenburn-bg+05  :foreground ,zenburn-red-2))))

       ;; diff
       `(diff-refine-added   ((t (:background unspecified  :foreground ,zenburn-green+4))))
       `(diff-refine-changed ((t (:background unspecified  :foreground ,zenburn-yellow))))
       `(diff-refine-removed ((t (:background unspecified  :foreground ,zenburn-red+1))))

       ;; ediff
       `(ediff-current-diff-A ((t (:foreground ,zenburn-red-4  :background ,zenburn-bg+05))))
       `(ediff-current-diff-Ancestor ((t (:foreground ,zenburn-red-4 :background ,zenburn-bg+05))))
       `(ediff-current-diff-B ((t (:foreground ,zenburn-green-2 :background ,zenburn-bg+05))))
       `(ediff-current-diff-C ((t (:foreground ,zenburn-blue-5 :background ,zenburn-bg+05))))
       `(ediff-even-diff-A ((t (:background ,zenburn-bg-05))))
       `(ediff-even-diff-Ancestor ((t (:background ,zenburn-bg-05))))
       `(ediff-even-diff-B ((t (:background ,zenburn-bg-05))))
       `(ediff-even-diff-C ((t (:background ,zenburn-bg-05))))
       `(ediff-fine-diff-A ((t (:foreground ,zenburn-red-2 :background nil :weight bold))))
       `(ediff-fine-diff-Ancestor ((t (:foreground ,zenburn-red-2 :background nil weight bold))))
       `(ediff-fine-diff-B ((t (:foreground ,zenburn-green :background nil :weight bold))))
       `(ediff-fine-diff-C ((t (:foreground ,zenburn-blue-3 :background nil :weight bold ))))
       `(ediff-odd-diff-A ((t (:background ,zenburn-bg-05))))
       `(ediff-odd-diff-Ancestor ((t (:background ,zenburn-bg-05))))
       `(ediff-odd-diff-B ((t (:background ,zenburn-bg-05))))
       `(ediff-odd-diff-C ((t (:background ,zenburn-bg-05))))


       `(font-lock-comment-face ((t (:foreground ,zenburn-fg-2))))
       `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-fg-2))))
       `(font-lock-doc-face ((t (:foreground ,zenburn-green-1))))

       `(ledger-font-xact-highlight-face ((t (:background ,zenburn-bg+05))))
       ;; org mode
       `(org-checkbox ((t (:foreground ,zenburn-fg+1 :weight bold))))
       `(org-link ((t (:foreground ,zenburn-yellow-2 :underline nil :bold t))))))))


(use-package csv-mode
  :straight t
  :mode   "\\.csv\\'")

(use-package expand-region
  :straight t
  :after hydra
  :commands hydra-er/body

  :custom
  (expand-region-fast-keys-enabled  nil)

  :init
  (defhydra hydra-er nil
    "Expand region hydra"
    ("e" er/expand-region)
    ("d" er/contract-region     :bind nil))
  (hydra-set-property 'hydra-er :verbosity 0)

  (defun ma/expand-region ()
    (interactive)
    (er/expand-region 1)
    (hydra-er/body))

  (global-set-key (kbd "C-*") 'ma/expand-region))

(use-package move-text
  :straight t
  :bind
  (("C-M-s-<up>" . move-text-up)
   ("C-M-s-<down>" . move-text-down)))

(use-package spaceline
  :straight t
  :config
  (use-package spaceline-config
    :config
    (spaceline-define-segment version-control
      "Show vc-mode in downcase"
      (when vc-mode
        (powerline-raw
         (s-trim (concat (downcase vc-mode)
                         (when (buffer-file-name)
                           (pcase (vc-state (buffer-file-name))
                             (`up-to-date " ")
                             (`edited  " Mod")
                             (`added " Add")
                             (`unregistered " ??")
                             (`removed " Del")
                             (`needs-merge " Con")
                             (`needs-update " Upd")
                             (`ignored " Ign")
                             (_ " Unk"))))))))

    (spaceline-define-segment workspace-number
      "Show windows config number always when use eyebrowse"
      (when (bound-and-true-p eyebrowse-mode)
        (let* ((wsc (length (eyebrowse--get 'window-configs)))
               (num (eyebrowse--get 'current-slot))
               (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
               (str (if (and tag (< 0 (length tag)))
                        (format "%d:%s" num tag)
                      (when num (int-to-string num)))))
          (setq str (format "%s/%d" str wsc))
          (propertize str 'face 'bold))))



    (spaceline-compile)
    (spaceline-spacemacs-theme)))

(use-package ivy
  :straight t
  :diminish ivy-mode
  :bind (:map
         ivy-minibuffer-map
         ("C-<return>" . ivy-immediate-done)
         :map
         ivy-mode-map
         ("C-c r"   . ivy-resume)
         ("C-c W"   . ivy-pop-view)
         ("C-c w"   . ivy-push-view))
  :custom
  (ivy-use-virtual-buffers      t)
  (enable-recursive-minibuffers t)
  (ivy-fixed-height-minibuffer  t)

  :init
  ;; hint: invoking  the  completion  command  you're  interested
  ;; M-: (ivy-state-caller ivy-last)     RET
  ;; M-: (ivy-state-collection ivy-last) RET
  ;; https://github.com/abo-abo/swiper/issues/2620#issuecomment-645665878
  (setq ivy-re-builders-alist '((t  . ivy--regex-ignore-order)))
  (ivy-mode))

(use-package ivy-avy
  :straight t
  :bind (:map ivy-minibuffer-map
              ("'" . ivy-avy)))

(use-package swiper
  :straight t
  :bind (("C-s" . swiper)
         (("C-S-s" . (lambda (&optional initial-input)
                       (interactive)
                       (let ((search-invisible nil))
                         (swiper initial-input)))))))

(use-package counsel
  :straight t
  :diminish
  :requires ivy
  :bind (:map counsel-mode-map
              ("M-x" . counsel-M-x)
              ("C-." . counsel-imenu))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode))

(use-package imenu-anywhere
  :straight t)


(use-package ivy-posframe
  :disabled
  :straight t
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-border-width 2)

  :config
  (defun ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
          (width (min (or ivy-posframe-width 200) (round (* .75 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))
  (ivy-posframe-mode 1))

(use-package ivy-rich
  :straight t
  :config
  (ivy-rich-mode 1))

(use-package smex
  :straight t
  ;; :bind (("M-x" . smex)
  ;;        ("M-X" . smex-major-mode-commands))

  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package smooth-scroll
  :straight t
  :diminish smooth-scroll-mode
  :bind (("M-p"   . scroll-down-1)
         ("M-n"   . scroll-up-1)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style    'reverse)
  (uniquify-separator            " • ")
  (uniquify-after-kill-buffer-p  t)
  (uniquify-ignore-buffers-re    "^\\*"))

(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode yas-reload-all)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)

  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
  (yas-reload-all)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
  (add-hook 'tsx-mode-hook 'yas-minor-mode)
  (add-hook 'tsx-ts-mode-hook 'yas-minor-mode))


(use-package prog-mode
  :init)

(use-package elec-pair
  :custom
  (electric-pair-inhibit-predicate #'(lambda (char) (window-minibuffer-p)))
  :init
  (add-hook 'prog-mode-hook #'electric-pair-mode))

(use-package saveplace
  :custom
  (save-place-file  "~/.emacs.d/saveplace")
  :init
  (save-place-mode))

(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t))

(use-package edit-indirect
  :after markdown
  :straight t)


(use-package projectile
  :straight t
  :bind-keymap ("C-,"   . projectile-command-map)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (([f6] .  next-error)
         ([f7] . 'recompile)
         ([f8] .  projectile-compile-project)
         :map projectile-command-map
              ("s a" . counsel-projectile-ag)
              (","   . projectile-switch-project))


  :custom
  (projectile-completion-system      'ivy)
  (projectile-indexing-method        'hybrid)
  (projectile-sort-order             'modification-time)
  (projectile-switch-project-action  (lambda () (projectile-dired) (projectile-commander)))
  (projectile-mode-line-function     (lambda ()  (format "proj: %s" (projectile-project-name))))
  (projectile-project-search-path    '("~/lab"))
  (projectile-find-dir-includes-top-level t)

  :config
  ;; (defun projectile--file-name-sans-extensions (file-name)
  ;;   "Return FILE-NAME sans any extensions."
  ;;   (file-name-base file-name))

  ;; (defun projectile--file-name-extensions (file-name)
  ;;   "Return FILE-NAME's extensions."
  ;;   (file-name-extension file-name))

  ;; projectile slows down tramp-mode
  ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  ;; (advice-add 'counsel-projectile-switch-project-action :override 'counsel-projectile-switch-project-action-find-file)
  ;; (advice-add 'counsel-projectile-find-file :override '+projectile-find-file)

  (add-to-list 'projectile-other-file-alist '("ts"   . ("css" "html")))
  (add-to-list 'projectile-other-file-alist '("html" . ("css" "ts")))
  (add-to-list 'projectile-other-file-alist '("css"  . ("ts" "html")))

  (projectile-global-mode))

(use-package counsel-projectile
  :straight t
  :after projectile)

(use-package helm-projectile
  :disabled
  :straight t)

(use-package ggtags
  :straight t)

(use-package cmake-mode
  :disabled
  :straight t
  :bind ((:map cmake-mode-map
               ("<f5>" . 'recompile)))
  :mode ("CMakeList.txt" . cmake-mode))

(use-package ispell
  :custom
  (ispell-program-name  "hunspell")
  (ispell-dictionary "es_CL,en_US")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "es_CL,en_US"))

(use-package flyspell
  :diminish
  :bind (:map flyspell-mode-map
              ("C-,"   . nil)
              ("C-."   . nil)
              ("C-c $" . nil))
  :hook
  (org-mode      . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (prog-mode     . flyspell-prog-mode)
  (yaml-mode     . flyspell-prog-mode)

  :config
  (set-variable-in-hook prog-mode-hook flyspell-persistent-highlight nil)
  (set-variable-in-hook yaml-mode-hook flyspell-persistent-highlight nil)
  )

(use-package flyspell-correct
  :straight t
  :bind (:map flyspell-mode-map ("C-c c" . flyspell-correct-wrapper)))

(use-package flyspell-correct-popup
  :straight t
  :custom
  (flyspell-correct-interface #'flyspell-correct-popup))

(use-package magit
  :straight t
  :bind (("C-c g" . magit-status)
         ("C-c d" . ma/magit-diff-buffer-file))
  :custom
  (magit-save-repository-buffers          'dontask)
  (magit-display-buffer-function          'magit-display-buffer-same-window-except-diff-v1)
  (magit-section-visibility-indicator     nil)
  (magit-diff-adjust-tab-width            tab-width)
  (magit-diff-refine-hunk                 'all)
  (magit-copy-revision-abbreviated        t)
  (magit-section-initial-visibility-alist '((untracked . hide)
                                            (unstaged  . show)
                                            (staged    . show)
                                            (stashes   . hide)
                                            (recent    . show)))

  ;; :custom-face
  ;; (smerge-refined-added   ((t :inverse-video nil)))
  ;; (smerge-refined-removed ((t :inverse-video nil)))

  :hook
  (git-commit-mode . git-commit-turn-on-flyspell)

  :preface
  (defun ma/display-buffer-same-windows (buffer)
    (display-buffer buffer '(display-buffer-same-window)))

  (defun ma/magit-diff-buffer-file ()
    (interactive)
    (let ((magit-display-buffer-function  #'ma/display-buffer-same-windows))
      (magit-diff-buffer-file)))

  :config
  (global-git-commit-mode)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-recent-commits
                          'magit-insert-unpushed-to-upstream
                          'append))

(use-package forge
  :straight t
  :after magit)

(use-package git-modes
  :straight t)

(use-package git-timemachine
  :straight t)

(use-package smerge-mode
  :custom
  (smerge-command-prefix  "\C-cm"))


(use-package helm
  :disabled
  :straight t
  :diminish helm-ff-cache-mode
  :bind ("M-y" . helm-show-kill-ring)
  :custom
  (helm-split-window-in-side-p  t)
  (helm-autoresize-max-height  40)
  (helm-autoresize-min-height  40)
  (helm-buffers-fuzzy-matching  t)

  ;; :custom-face
  ;; (helm-selection ((t (:extend t :background "#eee8d5" :underline nil))))

  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4))))

(use-package helm-ag
  :disabled
  :straight t
  :custom
  (helm-ag-fuzzy-match     t)
  (helm-ag-insert-at-point 'symbol))

(use-package helm-descbinds
  :disabled
  :straight t
  :bind ("C-h b" . helm-descbinds)
  :config
  (helm-descbinds-mode))

(use-package window
  :after hydra
  :init
  (defhydra hydra-window (global-map "C-x")
    "Some hydra for windows related command"
    ("<left>"  previous-buffer)
    ("<right>" next-buffer))

  :config
  :bind (("s-r" . (lambda () (interactive) (delete-window) (balance-windows)))
         ("s-x" . delete-window)
         ("s-c" . delete-other-windows)))

(use-package ace-window
  :straight t
  :custom
  (aw-ignore-current t)
  (aw-keys '(?a ?s ?d ?f ?g ?h))
  :bind (("s-o" . ace-window)
         ("s-z" . ace-delete-window)))


(use-package windmove
  :custom
  (windmove-create-window  t)

  :bind (("s-f"       . (lambda () (interactive) (windmove-right)))
         ("s-F"       . ma/show-current-after-move-to-right)
         ("s-s"       . (lambda () (interactive) (windmove-left)))
         ("s-S"       . ma/show-current-after-move-to-left)
         ("s-e"       . (lambda () (interactive) (windmove-up)))
         ("s-E"       . ma/show-current-after-move-to-up)
         ("s-d"       . (lambda () (interactive) (windmove-down)))
         ("s-D"       . ma/show-current-after-move-to-down)
         ("s-b"       . balance-windows))

  :config
  (defun ma/show-current-after-move-to (dir)
    (let ( (buff (window-buffer)))
      (windmove-do-window-select dir nil)
      (switch-to-buffer buff)))
  (defun ma/show-current-after-move-to-left ()
    (interactive)
    (ma/show-current-after-move-to 'left))
  (defun ma/show-current-after-move-to-right ()
    (interactive)
    (ma/show-current-after-move-to 'right))
  (defun ma/show-current-after-move-to-up ()
    (interactive)
    (ma/show-current-after-move-to 'up))
  (defun ma/show-current-after-move-to-down ()
    (interactive)
    (ma/show-current-after-move-to 'down)))

(use-package emmet-mode
  :straight t
  :hook web-mode)

(use-package web-mode
  :straight t
  :bind (:map web-mode-map ("C-=" . web-mode-mark-and-expand))

  :mode
  (("\\.phtml\\'"    . web-mode)
   ("\\.tpl\\.php\\'" . web-mode)
   ("\\.[gj]sp\\'"    . web-mode)
   ("\\.as[cp]x\\'"   . web-mode)
   ("\\.erb\\'"       . web-mode)
   ("\\.mustache\\'"  . web-mode)
   ("\\.djhtml\\'"    . web-mode)
   ("\\.html?\\'"     . web-mode))

  :custom
  (web-mode-engines-alist '(("angular"    . "\\.html\\'")))
  (web-mode-enable-current-element-highlight     t)
  (web-mode-enable-element-content-fontification t)
  (web-mode-enable-element-tag-fontification     t)
  (web-mode-markup-indent-offset                 2)
  ;;(web-mode-code-indent-offset                   4)
  )


(use-package company
  :straight t
  :diminish company-mode
  :bind (:map company-mode-map
         ("C-M-s-c" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))

  :hook ((emacs-lisp-mode    . company-mode)
         (web-mode           . company-mode)
         (css-mode           . company-mode)
         (c++-mode           . company-mode)
         (cider-repl-mode    . company-mode)
         (cider-mode         . company-mode)
         (sh-mode            . company-mode)
         (typescript-mode    . company-mode)
         (inferior-ess-mode  . company-mode)
         (ledger-mode        . company-mode)
         (org-mode           . company-mode))


  :custom
  (company-idle-delay            nil)
  (company-tooltip-idle-delay    0)
  (company-minimum-prefix-length 3)
  (company-show-numbers          'left)
  (company-dabbrev-downcase      nil)
  (company-selection-wrap-around t)

  :config
  (add-to-list 'company-backend 'company-ispell))

(use-package company-web
  :straight t)

(use-package company-ycmd
  :straight t
  :init
  (company-ycmd-setup))

;; (use-package company-auctex
;;   :straight t
;;   :init
;;   (company-auctex-init)
;;   (eval-after-load "company-auctex"
;;     ;; override this function, bad alignament in company
;;     '(defun company-auctex-symbol-annotation (candidate)
;;        nil)))

(use-package winner
  :after hydra
  :custom
  (winner-dont-bind-my-keys t)

  :config
  (setq winner-boring-buffers
        '("*Completions*"
          "*Compile-Log*"
          "*inferior-lisp*"
          "*Fuzzy Completions*"
          "*Apropos*"
          "*dvc-error*"
          "*Help*"
          "*cvs*"
          "*Buffer List*"
          "*Ibuffer*"
          "*helm projectile*"
          "*helm Swoop*"
          "*helm grep*"
          "*helm imenu*"
          "*helm etags*"
          "*helm-mt*"
          "\\*magit*"))
  (winner-mode 1)

  (defhydra hydra-winner (global-map "C-c")
    "Winner"
    ("p" (progn
           (winner-undo)
           (setq this-command 'winner-undo))
     "back")
    ("o" winner-redo "forward" :exit t :bind nil)))

(use-package transpose-frame
  :straight t
  :commands hydra-transpose-frame/body
  :after (hydra windmove)
  :init
  (defun ma/hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun ma/hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun ma/hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun ma/hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (defhydra hydra-transpose-frame (global-map "C-c f")
    "Frame comands"
    ("t" transpose-frame)
    ("v" flop-frame)
    ("b" flip-frame)
    ("f" windmove-right)
    ("s" windmove-left)
    ("e" windmove-up)
    ("d" windmove-down)
    ("x" delete-window)
    ("c" delete-other-windows)
    ("r" rotate-frame-anticlockwise)
    ("n" make-frame-command)
    ("j" ma/hydra-move-splitter-left)
    ("k" ma/hydra-move-splitter-down)
    ("i" ma/hydra-move-splitter-up)
    ("l" ma/hydra-move-splitter-right)))

(use-package json-mode
  :straight t
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))

  :mode (("\\.geojson\\'" . json-mode)
         ("\\.json\\'"    . json-mode)))

(use-package recentf
  :config
  (run-at-time nil (* 10 60) (lambda () (let ((inhibit-message t)) (recentf-save-list))))
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (setq recentf-exclude
        '("/\\.emacs\\.d/\\(elpa/\\|backups/\\)"
          ".gitignore"))

  (recentf-mode +1))




(use-package beacon
  :disabled
  :straight t
  :diminish beacon-mode
  :init
  (setq
   beacon-color       "#6F6F6F"
   beacon-blink-delay 0.1
   beacon-dont-blink-commands '(next-line previous-line forward-line mwheel-scroll scroll-down-1 scroll-up-1))
  (beacon-mode 1))

(use-package avy
  :straight t
  :bind (("C-M-s-e" . avy-goto-line)
         ("C-M-s-r" . avy-goto-end-of-line)
         ("C-M-s-f" . avy-goto-subword-1)
         ("C-M-s-v" . avy-goto-char-timer)
         ("C-M-s-g" . avy-goto-char-in-line))

  :custom-face
  (avy-goto-char-timer-face ((t (:inherit isearch))))

  :custom
  (avy-timeout-seconds 10.0) ;; confirm with RET
  (avy-indent-line-overlay t)

  :config
  ;; filter blank lines when use avy-goto-char
  (advice-add 'avy--line-cands
              :filter-return
              (lambda (lines)
                (save-excursion
                  (let (filtered)
                    (dolist (l lines filtered)
                      (let ((buffer (window-buffer (cdr l))))
                        (set-buffer buffer)
                        (goto-char (car l))
                        (unless  (string-blank-p (thing-at-point 'line))
                          (add-to-list 'filtered l))))))))

  ;; put overlays at end of line when using avy-goto-end-of-line
  (advice-add 'avy-goto-end-of-line
              :around
              (lambda (origfn &rest args)
                (let ((avy-style 'post))
                  (apply origfn args))))

  :init
  (global-set-key (kbd "C-z") nil))

(use-package misc-defuns
  :load-path "./defuns/"
  :init
  (global-set-key (kbd "C-o")           #'ma/open-line-and-indent)
  (global-set-key (kbd "<C-return>")    #'ma/open-line-below)
  (global-set-key (kbd "<C-S-return>")  #'ma/open-line-above)
  (global-set-key (kbd "<M-backspace>") #'ma/kill-line)
  (global-set-key (kbd "C-c e")         #'ma/eval-and-replace)
  (global-set-key (kbd "C-c j")         #'ma/join-line)
  (global-set-key (kbd "C-c J")         (lambda () (interactive) (ma/join-line t)))
  (global-set-key (kbd "M-w")           #'ma/kill-ring-save-line-or-region)
  (global-set-key (kbd "C-c C-SPC")     #'ma/jump-to-mark-skip-same-line))

(use-package crux
  :straight t
  :init
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

(use-package autorevert
  :delight auto-revert-mode
  :hook ((dired-mode) . auto-revert-mode)
  :custom
  (auto-revert-verbose  nil))

(use-package dired-x
  :custom
  (dired-omit-verbose       nil)
  (dired-omit-files      "^\\.")
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-omit-mode)
              (diminish 'dired-omit-mode))))

(use-package dired
  :bind (:map dired-mode-map
              ("k"    . ma/dired-kill-or-up-subdir)
              ("I"    . dired-find-file))

  :custom
  (dired-dwim-target                       t)
  (wdired-allow-to-change-permissions      t)
  (dired-listing-switches             "-aGlhv --group-directories-first --time-style=long-iso")


  :init
  (defun ma/dired-kill-or-up-subdir ()
    "Kill  current  subtree  but  if   it's  top  level  so  call
`dired-up-directory'"

    (interactive)
    (let ((in-header (dired-get-subdir))
          (cur-dir   (dired-current-directory)))

      (if (equal cur-dir (expand-file-name default-directory))
          (dired-up-directory)
        (progn
          (when (not in-header)
            (call-interactively 'dired-prev-subdir)
            (setq dir (dired-get-subdir)))
          (dired-do-kill-lines '(4))
          (dired-goto-file dir))))))

(use-package dired-narrow
  :straight t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-subtree
  :disabled
  :straight t
  :bind (:map dired-mode-map
              ("TAB"  . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix      ""))

(use-package all-the-icons
  ;; after install run the command (all-the-icons-install-fonts)
  :defer t
  :diminish
  :straight t)

(use-package all-the-icons-dired
  :straight t
  :diminish
  :custom
  (all-the-icons-dired-monochrome  nil)

  :hook
  (dired-mode . all-the-icons-dired-mode)

  :init
  (defface all-the-icons-dired-dir-face
    '((((background dark)) :foreground "#BDBDBD"))
    "Face for the directory icon"
    :group 'all-the-icons-faces)

  :config
  (add-to-list 'all-the-icons-extension-icon-alist
               '("go" all-the-icons-alltheicon "go" :height 1.0  :face all-the-icons-blue)))

(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :config

  (defun ma/run-latex ()
    "Compile latex whithout prompt"
    (interactive)
    (TeX-save-document (TeX-master-file))
    (TeX-command "LaTeX" 'TeX-master-file -1))

  (defun ma/run-biber ()
    "Compile biber whithout prompt"
    (interactive)
    (TeX-save-document (TeX-master-file))
    (TeX-command "Biber" 'TeX-master-file -1))

  ;; AUCTeX configuration
  (setq TeX-master      nil
        TeX-auto-save   t
        TeX-auto-local  ".tmpfiles/"
        TeX-parse-self  t
        TeX-save-query nil
        ;; use pdflatex
        TeX-PDF-mode    t
        ;; jump from an to viewer
        TeX-source-correlate-mode t
        )

  ;; default viewer
  (setq TeX-view-program-selection
        (quote
         (((output-dvi has-no-display-manager)
           "dvi2tty")
          ((output-dvi style-pstricks)
           "dvips and gv")
          (output-dvi "xdvi")
          (output-pdf "Okular")
          (output-html "xdg-open"))))

  (add-hook
   'LaTeX-mode-hook (lambda ()
                      (turn-on-auto-fill)
                      (TeX-fold-mode 1)
                      (abbrev-mode +1)
                      (LaTeX-math-mode 1)
                      (flyspell-mode)
                      (company-mode)
                      (turn-on-reftex)
                      (local-set-key (kbd "<f5>") 'TeX-view)
                      (local-set-key (kbd "<f6>") 'ma/run-latex)
                      (local-set-key (kbd "<f7>") 'ma/run-biber))))

(use-package gist
  :disabled
  :straight t
  :init
  (setq
   gist-ask-for-description t
   gist-ask-for-filename t))

(use-package simple-httpd
  :straight t)


(use-package qt-pro-mode
  :straight t
  :mode ("\\.pro\\'" "\\.pri\\'"))

(use-package typescript-mode
  :straight t)


(use-package mocha-snippets
  :straight t)

(use-package npm-mode
  :straight t
  :defer t)

(use-package deft
  :straight t
  :bind ([f2] . ma/deft-in-new-frame)
  :custom
  (deft-directory "~/syncthing/org/deft")
  (deft-extensions '("org" "md" "markdown" "txt" "text"))
  :config
  (defun ma/deft-in-new-frame (&optional arg)
    "Launch deft in a new frame"
    (interactive "P")
    (if (not arg)
        (deft)
      (select-frame (make-frame-command))
      (deft))))

(use-package rainbow-mode
  :straight t)

(use-package google-translate
  :straight t
  :bind ("C-c t" . google-translate-smooth-translate)
  :init
  (use-package google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist '(("en" . "es") ("es" . "en")))
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))


(use-package paradox
  :disabled
  :straight t
  :config
  (setq paradox-display-download-count t))

(use-package geiser
  :straight t)

(use-package dockerfile-mode
  :straight t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :straight t)

(use-package multiple-cursors
  :straight t
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-next-like-this-word))
  :init
  (global-unset-key (kbd "C-<down-mouse-1>")))

(use-package iy-go-to-char
  :after (:all multiple-cursors)
  :straight t
  :init
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))

(use-package yaml-mode
  :straight t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package toml-mode
  :straight t
  :mode ("\\.toml\\'" "/Pipfile\\'"))

(use-package iss-mode
  :mode "\\.iss\\'"
  :straight t)

(use-package hl-todo
  :straight t
  :init (global-hl-todo-mode)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))

(use-package nginx-mode
  :straight t)

(use-package sed-mode
  :load-path "site-lisp/"
  :mode "\\.sed\\'")

(use-package flymd
  :straight t
  :init
  (setq flymd-output-directory "/tmp"))

(use-package subword
  :diminish
  :hook ((prog-mode) .  subword-mode))

(use-package go-mode
  :straight t
  :bind (:map go-mode-map
              ("C-=" . (lambda () (interactive) (insert ":="))))
  :init
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save))))

(use-package go-playground
  :straight t
  :diminish
  :custom
  (go-playground-basedir "~/lab/go-playgrounds")
  (go-playground-init-command "go mod init whoiscare.com/m"))


(use-package highlight-symbol
  :straight t
  :bind (("H-h" . ma/highlight-symbol)
         ("H-<mouse-1>" . ma/highlight-symbol-at-point-click))
  :init
  (defun ma/highlight-symbol-at-point-click (event)
    "Highlight symbol at point use mouse click"
    (interactive "e")
    (mouse-set-point event nil)
    (unless (ignore-errors (highlight-symbol))
      (highlight-symbol-remove-all)))

  (defun ma/highlight-symbol (remove)
    (interactive "P")
    (if remove
        (highlight-symbol-remove-all)
      (highlight-symbol))))

(use-package highlight-numbers
  :straight t
  :hook (prog-mode . highlight-numbers-mode))

(use-package lorem-ipsum
  :straight t)

;; jade mode
(use-package pug-mode
  :straight t)

(use-package savekill
  :straight (savekill :fork  (:host github :repo "marcelino-m/savekill"))
  :init
  (setq savekill-keep-text-properties t
        savekill-max-saved-items 100))

(use-package auto-yasnippet
  :straight t
  :init
  (global-set-key (kbd "H-w") #'aya-create)
  (global-set-key (kbd "H-y") #'aya-expand))

(use-package uuidgen
  :straight t)

(use-package see-mode
  :disabled
  :straight t
  :init
  (setq see-use-align-quotes t))

(use-package adoc-mode
  :straight t)

(use-package pyvenv
  :straight t
  :config
  (pyvenv-mode 1))

(use-package python
  :bind (:map python-mode-map
              ("<backtab>" . nil)
              ("C-c C-n"   . ma/python-eval-current-line))

  :custom
  (python-indent-offset                 4)
  (python-shell-interpreter      "ipython")
  (python-shell-interpreter-args "--simple-prompt -i")


  :config
  (defun ma/python-eval-current-line (&optional keep)
    (interactive "P")
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (python-shell-send-region beg end nil t)
      (unless keep
        (forward-line))))

  (defun ma/python-newline-advice (orig-fun &rest args)
    "A better newline for  python-mode.
this  advice  understand if  point  is  currently at  indentation
level, so when press RET, indentation level is keeps.  If point is not
at indentation  level the behavior  is the  same as if  press RET
which call (newline) command"
    (let ((levels (python-indent-calculate-levels))
          (cur    (current-column)))
      (if (member cur levels)
          (progn
            (funcall orig-fun)
            (indent-to-column cur))
        (apply orig-fun args))))

  (defun ma/python-shell-calculate-command-advice (orig-fun &rest args)
    "If ipython is not found in path use python"
    (if (executable-find  python-shell-interpreter)
        (apply orig-fun args)
      (let ((python-shell-interpreter "python3")
            (python-shell-interpreter-args "-i"))
        (apply orig-fun args))))

  ;; advising newline behavior in python mode
  (let ((nline-fn #'newline))
    (add-function :around nline-fn  #'ma/python-newline-advice)
    (define-key python-mode-map (kbd "RET")  nline-fn))

  ;; advising if ipython not found then use  python3
  (advice-add 'python-shell-calculate-command :around #'ma/python-shell-calculate-command-advice))

(use-package py-isort
  :straight t
  :hook (python-mode . ma/enable-py-isort)
  :preface
  (defun ma/enable-py-isort()
    (add-hook 'before-save-hook 'py-isort-before-save nil t)))

(use-package blacken
  :straight t
  :diminish
  :hook (python-mode . blacken-mode))

(use-package elfeed
  :straight t
  :custom
  (elfeed-db-directory  "~/syncthing/elfeed/elfeed-db")
  :init
  (setq shr-width 100))

(use-package shr
  :config
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.5)
  (setq shr-width (current-fill-column)))

(use-package elfeed-org
  :straight t
  :after (elfeed)
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/syncthing/elfeed/feeds.org")))

(use-package qml-mode
  :straight t
  :mode "\\.qml\\'")

;; (use-package popwin
;;   :straight t
;;   :init
;;   (defun ma/bury-compile-buffer  (buf str)
;;     (when (null (string-match ".*exited abnormally.*" str))
;;       ;;no errors, make the compilation window go away in a 1 seconds
;;       (run-with-timer 1 nil #'popwin:close-popup-window)
;;       (message "No Compilation Errors!")))

;;   (add-hook 'compilation-finish-functions #'ma/bury-compile-buffer)

;;   :config
;;   (push '(inferior-python-mode :height 20 :noselect t :tail t :stick t) popwin:special-display-config)
;;   (push '("*Google Translate*" :noselect t :height 20) popwin:special-display-config)
;;   (push '("*Projectile Commander Help*"  :height 22) popwin:special-display-config)
;;   (popwin-mode 1))

(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-window-height (lambda ()
                          (fit-window-to-buffer
                           win
                           (floor (frame-height) 2)
                           (floor (frame-height) 3))))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Google Translate\\*"
          help-mode
          inferior-python-mode
          compilation-mode))
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode)))
  (popper-mode +1)
  (popper-echo-mode +1))


(use-package eros
  :straight t
  :init
  (eros-mode 1))

(use-package lsp-mode
  :straight t
  :diminish
  :hook ((go-mode python-mode c-mode c++-mode ess-r-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-warn-no-matched-clients nil)
  (lsp-diagnostic-package :none)
  (lsp-signature-auto-activate nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-lens
  :after lsp-lens
  :diminish)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :bind-keymap ("C-c C-l" . lsp-command-map)
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references)
              ("C-c C-l d"                   . lsp-ui-doc-show))
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable      nil)
  (lsp-ui-doc-position   'top)
  (lsp-ui-peek-list-width 80)

  :custom-face
  (lsp-ui-peek-peek    ((t :background "#494949")))
  (lsp-ui-peek-list    ((t :background "#494949")))
  (lsp-ui-peek-header  ((t :background "#5d4d7a" :foreground "white")))

  :config
  (setq lsp-ui-peek--buffer nil)
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      (--map (lsp-ui-peek--adjust win-width it) it)
                      (-map-indexed 'lsp-ui-peek--make-line it)
                      (-concat it (lsp-ui-peek--make-footer)))))

      (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
      (posframe-show lsp-ui-peek--buffer
                     :string (mapconcat 'identity string "")
                     :min-width (frame-width)
                     :poshandler #'posframe-poshandler-frame-center)))

  (defun lsp-ui-peek--peek-destroy ()
    (when (bufferp lsp-ui-peek--buffer)
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
          lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))

  (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy)
  )

(use-package posframe ;; for lsp-ui-peek
  :straight t)

(use-package systemd
  :straight t
  :mode (("\\.service\\'" . systemd-mode)))

(use-package goto-line-preview
  :straight t
  :bind ("H-p" . goto-line-preview))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
  (setq remote-file-name-inhibit-cache nil)
  (setq emote-file-name-inhibit-cache  nil)
  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "~/.ssh/config"))))

(use-package gnuplot
  :straight t
  :init
  (add-to-list 'interpreter-mode-alist '("gnuplot" . gnuplot-mode)))

(use-package crontab-mode
  :straight t)

(use-package xref
  :custom
  (xref-after-jump-hook  '(recenter))
  (xref-after-return-hook  nil))

(use-package page-break-lines
  :straight t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

(use-package restclient
  :straight t
  :mode ("\\.http\\'" . restclient-mode))

(use-package glsl-mode
  :straight t)

(use-package visual-fill-column
  :straight t)

(use-package gnus-dired
  :config
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))

(use-package smtpmail
  :config
  (setq  message-send-mail-function 'smtpmail-send-it))

(use-package mu4e
  :disabled
  :after smtpmail
  :load-path "~/.local/share/emacs/site-lisp/mu4e"
  :preface
  (defun ma/toggle-visual-line ()
    (interactive)
    (visual-fill-column-mode 'toggle)
    (visual-line-mode 'toggle))

  :bind (:map  mu4e-view-mode-map
         ("w" . #'ma/toggle-visual-line)
         :map  mu4e-headers-mode-map
         ("M" . #'mu4e-headers-mark-for-move)
         ("m" . #'mu4e-headers-mark-for-something)
         )
  :init
  (load "~/.emacs.d/.no-shared.el")
  :config
  (setq
   mu4e-completing-read-function 'completing-read
   mu4e-get-mail-command    "offlineimap"
   mu4e-update-interval     300
   mu4e-context-policy      'pick-first
   compose-context-policy   'nil
   mu4e-headers-date-format "%F"
   mu4e-headers-time-format "%R"
   mu4e-headers-include-related nil
   ;; mu4e contex!
   mu4e-contexts
   `(,(make-mu4e-context
       :name "proton"

       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-contact-field-matches msg
                               :to ma/email-address-proton)))

       :vars `((user-full-name         . "Marcelo Muñoz Araya")
               (mu4e-compose-signature . "Marcelo\n")
               (user-mail-address      . ,(setq user-mail-address ma/email-address-proton))
               (mu4e-sent-folder       . ,(concat "/" user-mail-address "/sent"))
               (mu4e-drafts-folder     . ,(concat "/" user-mail-address "/drafts"))
               (mu4e-trash-folder      . ,(concat "/" user-mail-address "/trash"))
               (mu4e-refile-folder     . ,(concat "/" user-mail-address "/archive"))
               ;; smtp
               (smtpmail-smtp-server   . "127.0.0.1")
               (smtpmail-stream-type   . starttls)
               (smtpmail-smtp-service  . 1025)
               (smtpmail-queue-mail    . nil)))

     ,(make-mu4e-context
       :name "gmail"
       :match-func (lambda (msg)
                     (when msg
                       (mu4e-message-contact-field-matches msg
                               :to ma/email-address-gmail)))

       :vars `((user-full-name         . "Marcelo Muñoz Araya")
               (mu4e-compose-signature . "Marcelo\n")
               (user-mail-address      . ,(setq user-mail-address  ma/email-address-gmail))
               (mu4e-sent-folder       . ,(concat "/" user-mail-address "/sent"))
               (mu4e-drafts-folder     . ,(concat "/" user-mail-address "/drafts"))
               (mu4e-trash-folder      . ,(concat "/" user-mail-address "/trash"))
               (mu4e-refile-folder     . ,(concat "/" user-mail-address "/archive"))
               (mu4e-sent-messages-behavior .  delete)
               ;; smtp
               (smtpmail-smtp-server   . "smtp.gmail.com")
               (smtpmail-stream-type   . ssl)
               (smtpmail-smtp-service  . 465)
               (smtpmail-queue-mail    . nil))))))

(use-package backup-walker
  :straight t)

(use-package compile
  :custom
  (compilation-ask-about-save   nil)
  (compilation-scroll-output   'first-error))

(use-package ledger-mode
  :straight t
  :mode "\\.journal\\'")

(use-package display-line-numbers
  :custom
  (display-line-numbers-type  'visual)
  (display-line-numbers-current-absolute nil)

  :config
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode)
  (add-hook 'text-mode-hook 'display-line-numbers-mode))

(use-package hydra
  :straight t)

(use-package eyebrowse
  :straight t
  :custom
  (eyebrowse-new-workspace  t)
  (eyebrowse-wrap-around    t)
  (eyebrowse-default-workspace-slot 1)

  :config
  (eyebrowse-mode 1)
  (defhydra hydra-eye (eyebrowse-mode-map "s-a")
    "eyebrowse"
    ("a"  eyebrowse-switch-to-window-config-1)
    ("s"  eyebrowse-switch-to-window-config-2)
    ("d"  eyebrowse-switch-to-window-config-3)
    ("f"  eyebrowse-switch-to-window-config-4)
    ("g"  eyebrowse-switch-to-window-config-5)

    ("x"  eyebrowse-close-window-config)
    ("r"  eyebrowse-rename-window-config)
    ("c"  eyebrowse-create-window-config)
    ("t"  eyebrowse-rename-window-config)
    ("p"  projectile-switch-project)
    ("<left>"  eyebrowse-prev-window-config)
    ("<right>" eyebrowse-next-window-config))
  (hydra-set-property 'hydra-eye :verbosity 0))

(use-package mpv
  :disabled
  :straight t)

(use-package engtool
  :disabled
  :diminish
  :after hydra
  :config
  (engtool-mode 1)
  (defhydra hyadra-engtool (engtool-mpv-mode-map "C-c m")
    "engtool"
    ("SPC" mpv-pause)
    ("q"   mpv-kill :exit t)
    ("F"   mpv-seek-forward)
    ("f"   (lambda () (interactive) (mpv-seek-forward 2)))
    ("m"   engtool-mpv-mark-position)
    ("b"   (lambda () (interactive) (mpv-seek-backward 2)))
    ("B"   mpv-seek-backward)
    ("d"   mpv-speed-decrease)
    ("e"   mpv-speed-increase)
    ("r"   (lambda () (interactive) (mpv-speed-set 1))))
  )

(use-package selected
  :straight t
  :diminish selected-minor-mode
  :init
  (selected-global-mode 1)
  (setq selected-org-mode-map (make-sparse-keymap))


  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("l" . downcase-region)
              ("w" . kill-ring-save)
              ("W" . kill-region)
              ("y" . yank)
              ("g" . google-this-region)
              (";" . comment-dwim)
              ("i" . ispell-region)
              ("s" . (lambda (beg end)
                       (interactive "r")
                       (setq mark-active nil)
                       (swiper (buffer-substring beg end))))
              ("S" . (lambda ()
                       (interactive)
                       (let ((counsel-projectile-ag-initial-input '(projectile-symbol-or-selection-at-point)))
                         (counsel-projectile-ag))))

              :map selected-org-mode-map
              ("*" . (lambda () (interactive) (org-emphasize ?*)))
              ("/" . (lambda () (interactive) (org-emphasize ?/)))
              ("_" . (lambda () (interactive) (org-emphasize ?_)))
              ("=" . (lambda () (interactive) (org-emphasize ?=)))
              ("~" . (lambda () (interactive) (org-emphasize ?~)))
              ("+" . (lambda () (interactive) (org-emphasize ?+)))))

(use-package orgit
  :straight t
  :custom
  (orgit-log-save-arguments  t))

(use-package bookmark
  :defer t
  :custom
  (bookmark-fontify nil)
  (bookmark-set-fringe-mark nil))

(use-package iedit
  :straight t
  :diminish)

(use-package wgrep
  :straight t
  :diminish)

(use-package string-inflection
  :straight t
  :diminish)

(use-package google-this
  :straight t
  :diminish)

(use-package undo-tree
  :straight t
  :diminish
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package ess
  :disabled
  :straight t
  :custom
  (ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                              (ess-R-fl-keyword:constants . t)
                              (ess-R-fl-keyword:modifiers . t)
                              (ess-R-fl-keyword:fun-defs . t)
                              (ess-R-fl-keyword:assign-ops . t)
                              (ess-R-fl-keyword:%op% . t)
                              (ess-fl-keyword:fun-calls . t)
                              (ess-fl-keyword:numbers)
                              (ess-fl-keyword:operators)
                              (ess-fl-keyword:delimiters)
                              (ess-fl-keyword:=)
                              (ess-R-fl-keyword:F&T))))

(use-package ess-r-mode
  :disabled
  :commands R)

(use-package org-download
  :disabled
  :straight t
  :diminish
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-transform-tree-table
  :disabled
  :straight t)

(use-package dotenv-mode
  :straight t
  :diminish)


(use-package clang-format+
  :straight t
  :diminish
  :config
  (add-hook 'c-mode-common-hook #'clang-format+-mode))

(setq warning-minimum-level :error)
(use-package tsx-ts-mode
  :custom
  (typescript-ts-mode-indent-offset 4)
  (typescript-indent-level 4)
  (js-indent-level 4)

  :hook
  (tsx-ts-mode . lsp-deferred)
  :init
  (add-to-list 'treesit-language-source-alist '(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
  ;;(dolist (lang '(tsx)) (treesit-install-language-grammar lang))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'"  . tsx-ts-mode)))

(use-package apheleia
  :straight t
  :diminish
  :hook
  (tsx-ts-mode . apheleia-mode)
  (typescript-mode . apheleia-mode)
  (typescript-ts-mode . apheleia-mode))

((straight-use-package 'apheleia))
;; (use-package jtsx
;;   :straight t
;;   :hook
;;   (jtsx-jsx-mode . lsp-deferred)
;;   (jtsx-tsx-mode . lsp-deferred)

;;   :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
;;          ("\\.tsx\\'" . jtsx-tsx-mode)
;;          ("\\.ts\\'" . jtsx-typescript-mode))
;;   :commands jtsx-install-treesit-language
;;   ;; :hook ((jtsx-jsx-mode . hs-minor-mode)
;;   ;;        (jtsx-tsx-mode . hs-minor-mode)
;;   ;;        (jtsx-typescript-mode . hs-minor-mode))
;;   :custom
;;   (js-indent-level 2)
;;   (typescript-ts-mode-indent-offset 2)
;;   ;; (jtsx-switch-indent-offset 0)
;;   ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
;;   ;; (jtsx-jsx-element-move-allow-step-out t)
;;   ;; (jtsx-enable-jsx-electric-closing-element t)
;;   ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
;;   ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
;;   ;; (jtsx-enable-all-syntax-highlighting-features t)
;;   ;; :config
;;   ;; (defun jtsx-bind-keys-to-mode-map (mode-map)
;;   ;;   "Bind keys to MODE-MAP."
;;   ;;   (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
;;   ;;   (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
;;   ;;   (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
;;   ;;   (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
;;   ;;   (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
;;   ;;   (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
;;   ;;   (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
;;   ;;   (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
;;   ;;   (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
;;   ;;   (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
;;   ;;   (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
;;   ;;   (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
;;   ;;   (define-key mode-map (kbd "C-c j d") 'jtsx-delete-jsx-node)
;;   ;;   (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
;;   ;;   (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
;;   ;;   (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))

;;   ;; (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
;;   ;;     (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

;;   ;; (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
;;   ;;     (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

;;   ;; (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
;;   ;; (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
;;   )

(use-package insert-shebang
  :straight t
  :diminish)
