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


(defmacro lambda-i (args body)
  "Define an interactive lambda"
  `(lambda ,args
     (interactive)
     ,body))


(use-package frame
  :init
  ;; in gnome stealing focus doesn't work correctly
  (add-hook 'after-make-frame-functions
            (lambda-i (frame) (select-frame-set-input-focus frame))))

(use-package simple
  :config
  (advice-add 'backward-kill-word
              :around
              (lambda (origfn &rest args)
                "First delete blanks before point if there is at least three blanks"
                (if (looking-back "[ \t]\\{3,\\}" nil t)
                    (replace-match "" nil nil)
                  (apply origfn args))))

  (advice-add 'kill-word
              :around
              (lambda (origfn &rest args)
                "First delete blanks after point if there is at least three blanks"
                (if (looking-at "[ \t]\\{3,\\}")
                    (replace-match "" nil nil)
                  (apply origfn args)))))


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


;; global options

(tool-bar-mode     -1)
(menu-bar-mode     -1)
(blink-cursor-mode  1)
(delete-selection-mode)
(show-paren-mode t)
(xterm-mouse-mode)

(setq-default
 cursor-type                          'bar
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
 vc-follow-symlinks                   t
 auto-hscroll-mode                    'current-line
 hscroll-step 1
 async-shell-command-buffer           'new-buffer
 default-input-method                 "latin-prefix"
 frame-resize-pixelwise                t
 browse-url-browser-function          'browse-url-firefox
 default-frame-alist                  '((cursor-color         . "#CCDC90")
                                        (width                . 0.80)
                                        (height               . 0.65)
                                        (vertical-scroll-bars .  nil)
                                        (font                 . "Source Code Pro-9.3:weight=semi-bold:width=normal")))

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
  (add-to-list 'org-modules 'org-habit)
  (require 'org-defun)

  (unbind-key "C-c C->" org-mode-map)
  (unbind-key "C-," org-mode-map)
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (setq org-ellipsis "...▼")
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; new line behavior on new item
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

  (setq org-startup-folded t)
  (setq org-cycle-separator-lines 0)
  ;; prevent org mode repositioning text when cicle visibility
  (remove-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change)

  ;; log state chages into a drawer
  (setq org-log-into-drawer t)

  ;; priority range
  (setq org-priority-highest ?A)
  (setq org-priority-lowest  ?G)
  (setq org-priority-default ?D)

  (setq org-todo-keywords
        '((sequence
           "WANT(a)" "WAIT(s@/!)" "TODO(d)" "STOPED(f@)" "DOING(j!)"  "|" "CANCELED(;@)" "DELEGATED(l@)" "DONE(k@)")))

  (setq org-todo-keyword-faces
        '(("WANT"   . "gray")
          ("WAIT"   . "Yellow")
          ("TODO"   . "OrangeRed")
          ("DOING"  . "Orange")
          ("STOPPED" . "Yellow")
          ("DONE"    . "SpringGreen")
          ("DELEGATED" . "SpringGreen")
          ("CANCELED"  . "SpringGreen")))


  ;; position of tags in right margin
  (setq org-tags-column -95)

  (setq org-tag-alist '((:startgroup)
                        ("@personal" . ?P) ("@work" . ?W)
                        (:endgroup)
                        (:startgroup)
                        ("@void" . ?V) ("@collecting" . ?C) ("@ready" . ?R)
                        (:endgroup)

                        ("read" . ?r)
                        ("interesting" . ?i)
                        ("emacs" . ?e)
                        ("idea" . ?t)
                        ("home" . ?h)
                        ("finance" . ?f)
                        ("week" . ?w)
                        ("english" . ?e)
                        ("office"  . ?o)
                        ("chore" . ?c)
                        ("fix" . ?F)
                        ("bug" . ?b)
                        ("feat" . ?a)
                        ("codrev" . ?v)))


  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-python-command "ipython --no-banner --nosep --simple-prompt  -i")
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
     (latex   . t)
     (ditaa   . t)
     (calc    . t)
     (ruby    . t))))



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

  ;; window-text-width not sustract line-number-display-width from the
  ;; width of window-text
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

  (setq org-agenda-files
      (mapcar 'abbreviate-file-name
              (split-string
               (shell-command-to-string "find ~/syncthing/org/ -type f -name \"*.org\"") "\n" t)))

  (setq org-agenda-sorting-strategy
        '((agenda habit-down time-up priority-down category-keep)
          (todo priority-down category-down todo-state-up)
          (tags priority-down category-keep)
          (search category-keep)))

  (setq org-agenda-custom-commands
        '(("A" "Personal agenda for current day or week"
           ((agenda "")
            (tags "+week"
                  ((org-use-tag-inheritance nil)
                   (org-agenda-sorting-strategy '(todo-state-down priority-down))
                   (org-agenda-prefix-format "> "))))
           ((org-agenda-tag-filter  '("-@work"))))

          ("!" "To work this week" tags "-@work+week"
           ((org-agenda-sorting-strategy '(todo-state-down  priority-down))
            (org-agenda-prefix-format " > ")))

          ("I" "Very Personal related task" tags-todo "-@work-home"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-prefix-format " ")))

          ("i" "Personal related task" tags-todo "-@work"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-prefix-format " ")))

          ("h" "Home related task" tags-todo "+home"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-prefix-format " ")))

          ("w" . "Work related comand")

          ("wa" "Agenda for current day or week"
           ((agenda "")
            (tags "+week"
            ((org-use-tag-inheritance nil)
             (org-agenda-sorting-strategy '(todo-state-down priority-down))
             (org-agenda-prefix-format "> "))))
           ((org-agenda-tag-filter-preset '("+@work"))))

          ("wt" "All todos" tags-todo "+@work"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-prefix-format " ")))

          ("wc" "Coded related todos"
           ((tags-todo "+bug")
            (tags-todo "+fix")
            (tags-todo "+feat")
            (tags-todo "+chore"))
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-tag-filter-preset '("+@work"))
            (org-agenda-prefix-format "> ")))

          ("w!" "To work this week" tags "+@work+week"
           ((org-use-tag-inheritance nil)
            (org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-prefix-format " ")))

          ("wn" "Quick notes" tags "+@work"
           ((org-use-tag-inheritance nil)
            (org-agenda-prefix-format "> ")
            (org-agenda-files '("~/syncthing/org/capture/work/quick-notes.org"))))

          ("wj" "Journal personal" search "{[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-}"
           ((org-agenda-sorting-strategy '(alpha-down))
            (org-agenda-max-entries 20)
            (org-use-tag-inheritance nil)
            (org-agenda-prefix-format "> ")
            (org-agenda-files '("~/syncthing/org/capture/work/journal.org"))))

          ("wJ" "Journal team" tags "+@work"
           ((org-agenda-max-entries 20)
            (org-use-tag-inheritance nil)
            (org-agenda-prefix-format "> ")
            (org-agenda-files '("~/syncthing/org/capture/work/journal-team.org"))))

          ("wz" "Code review notes" tags-todo "+@work+codrev"
           ((org-agenda-sorting-strategy '(todo-state-down priority-down))
            (org-agenda-prefix-format "> "))))))

(use-package org-habit
  :custom
  (org-habit-graph-column 70)
  (org-habit-show-habits-only-for-today nil))

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
           "* %? \n:LOGBOOK:\n:CREATED: %U \n:END:" :prepend t :empty-lines-before 2)

          ("h" "Home and domestic related task"
           entry (file "~/syncthing/org/capture/home-task.org")
           "* TODO %? :home:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("w" "work related captures")
          ("wt" "Task"
           entry (file "~/syncthing/org/capture/work/task.org")
           "* TODO %? :@work:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wm" "Meetings notes"
           entry (file "~/syncthing/org/capture/work/meeting.org" )
           "* Meeting %? :@work:\n:LOGBOOK:\n:CREATED: %U \n:END:" :prepend t :empty-lines-before 2)

          ("wn" "Note: Quick and misc note about anything"
           entry (file "~/syncthing/org/capture/work/quick-notes.org")
           "* %? :@work:\n:LOGBOOK:\n:CREATED: %U \n:END:" :prepend t :empty-lines-before 2)

          ("wz" "Code review: Saved notes when review code"
           entry (file "~/syncthing/org/capture/work/code-review.org")
           "* Feat:  %? :@work:codrev:\n:LOGBOOK:\n:CREATED: %U \n:END:" :prepend t :empty-lines-before 2)

          ("wo" "One to one meeting"
           entry (file+headline "~/syncthing/org/capture/work/for-next-meeting.org" "One to One")
           "* TODO %? :@work:one2one:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wl" "Lead meeting"
           entry (file+headline "~/syncthing/org/capture/work/for-next-meeting.org" "Lead meeting")
           "* TODO %? :@work:lead:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("ws" "Stand up"
           entry (file+headline "~/syncthing/org/capture/work/for-next-meeting.org" "Lead meeting")
           "* TODO %? :@work:standup:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wr" "Retro"
           entry (file+headline "~/syncthing/org/capture/work/for-next-meeting.org" "Retrospective")
           "* TODO %? :@work:retro:\n:LOGBOOK:\n:CREATED: %U \n:END:" :empty-lines-before 2)

          ("wj" "Journal"
           item (file+olp+datetree "~/syncthing/org/capture/work/journal.org")
           "%?" :tree-type week)

          ("wJ" "Journal team"
           entry (file "~/syncthing/org/capture/work/journal-team.org" )
           "* Iter %? :@work:\n:LOGBOOK:\n:CREATED: %U \n:END:" :prepend t :empty-lines-before 2))))


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
      (add-hook 'org-mode-hook (lambda () (set-face-attribute 'org-block-begin-line nil :extend nil)))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'ma/setup-appearence)
    (load-theme 'spacemacs-dark t)
    (set-face-attribute 'region nil :background "#3d3b40")))

(use-package solarized-theme
  :disabled
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

  :config
  (load-theme 'solarized-light t))


(use-package zenburn-theme
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

       ;; fringe
       `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))

       ;; display-line-numbers
       `(line-number ((t (:foreground ,zenburn-bg+3 :background ,zenburn-bg :bold nil :slant italic :box nil))))
       `(line-number-current-line ((t (:inherit line-number :foreground ,zenburn-yellow-2 ))))

       `(org-link ((t (:foreground ,zenburn-yellow-2 :underline nil :bold t))))

       ;; magit
       `(magit-diff-added    ((t (:background nil  :foreground ,zenburn-green))))
       `(magit-diff-changed  ((t (:background nil  :foreground ,zenburn-yellow-1))))
       `(magit-diff-removed  ((t (:background nil  :foreground ,zenburn-red-2))))
       `(magit-diff-added-highlight    ((t (:background ,zenburn-bg+05  :foreground ,zenburn-green))))
       `(magit-diff-changed-highlight  ((t (:background ,zenburn-bg+05  :foreground ,zenburn-yellow-1))))
       `(magit-diff-removed-highlight  ((t (:background ,zenburn-bg+05  :foreground ,zenburn-red-2))))

       ;; diff
       `(diff-refine-added   ((t (:background nil  :foreground ,zenburn-green+4))))
       `(diff-refine-changed ((t (:background nil  :foreground ,zenburn-yellow))))
       `(diff-refine-removed ((t (:background nil  :foreground ,zenburn-red+1))))

       ;; edif
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

       `(ledger-font-xact-highlight-face ((t (:background ,zenburn-bg+05))))))))


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
    ("d" er/contract-region     :bind nil)
    ("w" kill-ring-save :exit t :bind nil)
    ("W" kill-region    :exit t :bind nil)
    ("s" (lambda (beg end)
           (interactive "r")
           (setq mark-active nil)
           (swiper (buffer-substring beg end)))))

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
  :bind ("C-s" . swiper))

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
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode))


(use-package prog-mode
  :init
  (set-variable-in-hook prog-mode-hook tab-width 4))

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
         ("\\.markdown\\'" . markdown-mode)))

(use-package projectile
  :straight t
  :bind-keymap ("C-,"   . projectile-command-map)
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (([f3]  . next-error)
         ([f4] . 'recompile)
         ([f5]  . projectile-compile-project)
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

  :config
  (defun projectile--file-name-sans-extensions (file-name)
    "Return FILE-NAME sans any extensions."
    (file-name-base file-name))

  (defun projectile--file-name-extensions (file-name)
    "Return FILE-NAME's extensions."
    (file-name-extension file-name))

  ;; projectile slows down tramp-mode
  ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (add-to-list 'projectile-other-file-alist '("ts"   . ("css" "html")))
  (add-to-list 'projectile-other-file-alist '("html" . ("css" "ts")))
  (add-to-list 'projectile-other-file-alist '("css"  . ("ts" "html")))

  (projectile-global-mode))

(use-package counsel-projectile
  :straight t
  :after projectile)

(use-package helm-projectile
  :straight t)

(use-package ggtags
  :straight t)

(use-package cmake-mode
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

  :custom
  (flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face)))

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
  (magit-display-buffer-function          'magit-display-buffer-fullframe-status-v1)
  (magit-section-visibility-indicator     nil)
  (magit-diff-adjust-tab-width            'always)
  (magit-diff-refine-hunk                 'all)
  (magit-section-initial-visibility-alist '((untracked . hide)
                                            (unstaged  . show)
                                            (staged    . show)
                                            (stashes   . hide)
                                            (recent    . show)))

  :custom-face
  (smerge-refined-added   ((t :inverse-video nil)))
  (smerge-refined-removed ((t :inverse-video nil)))

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

(use-package gitignore-mode
  :straight t)

(use-package git-timemachine
  :straight t)

(use-package smerge-mode
  :custom
  (smerge-command-prefix  "\C-cm"))

(use-package helm
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
  :straight t
  :custom
  (helm-ag-fuzzy-match     t)
  (helm-ag-insert-at-point 'symbol))

(use-package helm-descbinds
  :straight t
  :bind ("C-h b" . helm-descbinds)
  :config
  (helm-descbinds-mode))

(use-package window
  :config
  :bind (("s-x" . delete-window)
         ("s-c" . delete-other-windows)))

(use-package ace-window
  :straight t
  :custom
  (aw-ignore-current t)
  (aw-keys '(?a ?s ?d ?f ?g ?h))
  :bind (("s-z" . ace-window)))

(use-package windmove
  :custom
  (windmove-create-window  t)
  :bind (("s-f"       . windmove-right)
         ("s-s"       . windmove-left)
         ("s-e"       . windmove-up)
         ("s-d"       . windmove-down)))

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
  (web-mode-code-indent-offset                   4))

(use-package js2-mode
  :straight t
  :mode        "\\.js\\'")

(use-package windmove
  :custom
  (windmove-create-window  t)

  :bind (("s-f"       . windmove-right)
         ("s-F"       . ma/show-current-after-move-to-right)
         ("s-s"       . windmove-left)
         ("s-S"       . ma/show-current-after-move-to-left)
         ("s-e"       . windmove-up)
         ("s-E"       . ma/show-current-after-move-to-up)
         ("s-d"       . windmove-down)
         ("s-D"       . ma/show-current-after-move-to-down))

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

(use-package company
  :straight t
  :diminish company-mode
  :bind (:map company-mode-map
         ("C-M-s-c" . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))

  :hook ((emacs-lisp-mode    . company-mode)
         (js2-mode           . company-mode)
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

(use-package company-auctex
  :straight t
  :init
  (company-auctex-init)
  (eval-after-load "company-auctex"
    ;; override this function, bad alignament in company
    '(defun company-auctex-symbol-annotation (candidate)
       nil)))

(use-package winner
  :defer 5
  :init
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
  (winner-mode 1))

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
  (global-set-key (kbd "H-l")           #'ma/goto-line-with-feedback)
  (global-set-key (kbd "<M-backspace>") #'ma/kill-line)
  (global-set-key (kbd "C-c e")         #'ma/eval-and-replace)
  (global-set-key (kbd "C-c f c")       #'make-frame-command)
  (global-set-key (kbd "C-c j")         #'ma/join-line)
  (global-set-key (kbd "C-c J")         (lambda-i () (ma/join-line t)))
  (global-set-key (kbd "M-w")           #'ma/kill-ring-save-line-or-region)
  (global-set-key (kbd "C-w")           #'ma/kill-line-or-region)
  (global-set-key (kbd "C-y")           #'ma/yank-with-feedback)
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
  :custom
  (dired-dwim-target                       t)
  (wdired-allow-to-change-permissions      t)
  (dired-listing-switches             "-alh"))

(use-package dired-narrow
  :straight t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-subtree
  :straight t
  :bind (:map dired-mode-map
              ("TAB"  . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix      ""))

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

(use-package tide
  :straight t
  :after (typescript-mode company)
  :hook ((typescript-mode . tide-setup)))

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
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (setq google-translate-translation-directions-alist '(("en" . "es") ("es" . "en")))
  (use-package google-translate-smooth-ui))

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

(use-package scss-mode
  :straight t
  :init
  (setq scss-compile-at-save nil))

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
              ("C-:" . (lambda () (interactive) (insert ":="))))
  :init
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4))))

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

(use-package popwin
  :straight t
  :init
  (defun ma/bury-compile-buffer  (buf str)
    (when (null (string-match ".*exited abnormally.*" str))
      ;;no errors, make the compilation window go away in a 1 seconds
      (run-with-timer 1 nil #'popwin:close-popup-window)
      (message "No Compilation Errors!")))

  (add-hook 'compilation-finish-functions #'ma/bury-compile-buffer)

  :config
  (push '(inferior-python-mode :height 20 :noselect t :tail t :stick t) popwin:special-display-config)
  (push '("*Google Translate*" :noselect t :height 20) popwin:special-display-config)
  (push '("*Projectile Commander Help*"  :height 22) popwin:special-display-config)
  (popwin-mode 1))

(use-package protobuf-mode
  :straight t)

(use-package eros
  :straight t
  :init
  (eros-mode 1))

(use-package lsp-mode
  :straight t
  :diminish
  :hook ((go-mode python-mode c-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-diagnostic-package :none)
  (lsp-signature-auto-activate nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-headerline-breadcrumb-enable nil))

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
  (compilation-ask-about-save  nil))

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
    ("a"  eyebrowse-switch-to-window-config-1 :exit t)
    ("s"  eyebrowse-switch-to-window-config-2 :exit t)
    ("d"  eyebrowse-switch-to-window-config-3 :exit t)
    ("f"  eyebrowse-switch-to-window-config-4 :exit t)
    ("g"  eyebrowse-switch-to-window-config-5 :exit t)
    ("C-a"  eyebrowse-switch-to-window-config-1)
    ("C-s"  eyebrowse-switch-to-window-config-2)
    ("C-d"  eyebrowse-switch-to-window-config-3)
    ("C-f"  eyebrowse-switch-to-window-config-4)
    ("g"  eyebrowse-switch-to-window-config-5)

    ("x"  eyebrowse-close-window-config)
    ("r"  eyebrowse-rename-window-config)
    ("c"  eyebrowse-create-window-config)
    ("t"  eyebrowse-rename-window-config)
    ("p"  projectile-switch-project)
    ("<left>"  eyebrowse-prev-window-config)
    ("<right>" eyebrowse-next-window-config))
  (hydra-set-property 'hydra-eye :verbosity 0))


(use-package engtool
  :diminish
  :config
  (engtool-mode 1))

(use-package orgit
  :straight t
  :custom
  (orgit-log-save-arguments  t))
