;;  adjust the garbage collection param
(setq gc-cons-threshold (* 50 1024 1024))

(require 'package)

(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))


;; (package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package delight :ensure t)
(use-package diminish :ensure t)

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
 ;; disable screen jump when the cursor moves out of the screen
 ;; scroll-conservatively                101
 scroll-preserve-screen-position      t
 scroll-step                          1
 vc-follow-symlinks                   t
 auto-hscroll-mode                    'current-line
 hscroll-step 1
 async-shell-command-buffer           'new-buffer
 default-input-method                 "latin-prefix"
 frame-resize-pixelwise                t
 browse-url-browser-function          'browse-url-firefox
 browse-url-firefox-program           "~/.src/firefox/firefox"
 default-frame-alist                  '((width                . 0.80)
                                        (height               . 0.75)
                                        (vertical-scroll-bars .  nil)
                                        (font                 . "Source Code Pro-9.3:weight=semi-bold:width=normal")))


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

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("PATH" "GOPATH" "GOROOT"  "TIMLIB_SRC_ROOT"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)

  (add-to-list 'exec-path "~/.local/bin/")
  (add-to-list 'exec-path (concat (getenv "PYTHONUSERBASE") "/bin"))
  (add-to-list 'exec-path (concat (getenv "GOROOT") "/bin")))

(use-package eldoc
  :diminish eldoc-mode)

(use-package info
  :defer t
  :config
  (define-key Info-mode-map (kbd "<prior>") 'scroll-down-1)
  (define-key Info-mode-map (kbd "<next>") 'scroll-up-1)
  (use-package info+
    :load-path "~/.emacs.d/site-lisp/"))


(use-package ethan-wspace
  :ensure t
  :diminish ethan-wspace-mode
  :init
  (add-hook 'org-src-mode-hook   #'ethan-wspace-clean-all)
  (add-hook 'prog-mode-hook      #'ethan-wspace-mode)
  (add-hook 'markdown-mode-hook  #'ethan-wspace-mode)
  (add-hook 'LaTeX-mode-hook     #'ethan-wspace-mode)
  (add-hook 'yaml-mode-hook      #'ethan-wspace-mode)
  (add-hook 'see-mode-hook       #'ethan-wspace-mode)
  (add-hook 'ledger-mode-hook    #'ethan-wspace-mode)
  (add-hook 'yaml-mode-hook      #'ethan-wspace-mode))

(use-package zenburn-theme
  :ensure t
  :disabled
  :init
  (set-cursor-color "#b8860b")
  :config
  (load-theme 'zenburn t))

(use-package solarized-theme
  :ensure t
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
  (org-block            ((t :background "#f4eddb" :extend t)))
  (org-block-begin-line ((t (:underline "#6c71c4"))))
  (org-block-end-line   ((t (:overline nil :underline "#6c71c4"))))
  (org-checkbox         ((t :box nil)))

  :config
  (load-theme 'solarized-light t))

(use-package csv-mode
  :ensure t
  :mode   "\\.csv\\'")

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :ensure t
  :bind
  (([(hyper up)] . move-text-up)
   ([(hyper down)] . move-text-down)))

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (:map ivy-minibuffer-map
              ("C-<return>" . ivy-immediate-done))
  :init (ivy-mode)
  (setq
   ivy-use-virtual-buffers t
   enable-recursive-minibuffers t))

(use-package smex
  :ensure t
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package counsel
  :ensure t
  :requires ivy
  :bind (:map ivy-mode-map
              ("C-c M-x" . ivy-resume)
              ("C-c <menu>" . ivy-resume)
              ("C-s" . swiper)
              ("M-x" . counsel-M-x)
              ("C-c W" . ivy-pop-view)
              ("C-c w" . ivy-push-view))
  :config
  (setf (cdr (assoc 'counsel-M-x ivy-initial-inputs-alist)) ""))

(use-package smooth-scroll
  :ensure t
  :diminish smooth-scroll-mode
  :bind (("M-p"   . scroll-down-1)
         ("M-n"   . scroll-up-1)))

(use-package smooth-scrolling
  :disabled
  :ensure t
  :init
  (smooth-scrolling-mode -1))

(use-package uniquify
  :init
  (setq
   uniquify-buffer-name-style   'reverse
   uniquify-separator           " • "
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re   "^\\*"))


(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :bind (([backtab]   . yas-expand))
  :config
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 '("~/.emacs.d/snippets/")))

  (yas-global-mode 1))



(use-package prog-mode
  :init
  (setq tab-width 4))


(use-package elec-pair
  :init
  ;; disable electric pairing in minibuffer
  (setq electric-pair-inhibit-predicate #'(lambda (char) (window-minibuffer-p)))
  (add-hook 'prog-mode-hook #'electric-pair-mode))


(use-package saveplace
  :init
  (setq-default
   save-place-mode t
   save-place-file "~/.emacs.d/saveplace")
  :config
  (save-place-mode))


(use-package markdown-mode
  :ensure t
  :init
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))


(use-package projectile
  :ensure t
  :delight '(:eval (format " [:prj %s]" (projectile-project-name)))
  :load-path "./defuns/"
  :bind-keymap ("C-," . projectile-command-map)
  :bind (:map projectile-command-map
              ("s a" . helm-projectile-ag)
              ("o"   . helm-occur)
              ("5 p" . ma/projectile-switch-to-project-other-frame))
  :custom
  (projectile-enable-caching            t)
  (projectile-completion-system      'ivy)
  (projectile-switch-project-action  (lambda () (projectile-dired) (projectile-commander)))

  :config
  (defun projectile--file-name-sans-extensions (file-name)
    "Return FILE-NAME sans any extensions."
    (file-name-base file-name))

  (defun projectile--file-name-extensions (file-name)
    "Return FILE-NAME's extensions."
    (file-name-extension file-name))

  (defun ma/projectile-switch-to-project-other-frame (&optional arg)
    (interactive "P")
    (select-frame (make-frame-command))
    (projectile-switch-project arg))

  ;; projectile slows down tramp-mode
  ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (add-to-list 'projectile-other-file-alist '("ts"   . ("css" "html")))
  (add-to-list 'projectile-other-file-alist '("html" . ("css" "ts")))
  (add-to-list 'projectile-other-file-alist '("css"  . ("ts" "html")))

  (projectile-global-mode))


(use-package helm-projectile
  :ensure t)

(use-package ggtags
  :ensure t)

(use-package cmake-mode
  :ensure t
  :bind ((:map cmake-mode-map
               ("<f5>" . 'recompile)))
  :mode ("CMakeList.txt" . cmake-mode))


(use-package flyspell
  :init
  (setq ispell-dictionary "castellano")
  :config
  (define-key flyspell-mode-map [(control ?\,)] nil)
  (define-key flyspell-mode-map [(control ?\.)] nil)
  (define-key flyspell-mode-map [?\C-c ?$]      nil))


(use-package flyspell-correct
  :ensure t
  :bind (:map flyspell-mode-map ("C-c c" . flyspell-correct-wrapper)))


(use-package flyspell-correct-ivy
  :disabled
  :ensure t
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy))


(use-package flyspell-correct-popup
  :ensure t
  :init
  (setq flyspell-correct-interface #'flyspell-correct-popup))


(use-package magit
  :ensure t
  :bind (([f12] . magit-status))
  :custom
  (magit-save-repository-buffers          'dontask)
  (magit-display-buffer-function          'magit-display-buffer-fullcolumn-most-v1)
  (magit-section-visibility-indicator     nil)
  (magit-diff-adjust-tab-width            'always)
  (magit-section-initial-visibility-alist '((untracked . hide)
                                            (unstaged  . show)
                                            (staged    . show)
                                            (stashes   . hide)
                                            (recent    . show)))

  :hook
  (git-commit-mode . git-commit-turn-on-flyspell)

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

(use-package gitignore-mode
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package smerge-mode
  :custom
  (smerge-command-prefix  "\C-cm"))

(use-package helm
  :ensure t
  :init
  (setq
   helm-split-window-in-side-p t
   helm-autoresize-max-height  40
   helm-autoresize-min-height  40
   helm-buffers-fuzzy-matching t)

  :config
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4))))


(use-package helm-ag
  :ensure t
  :config
  (setq helm-ag-fuzzy-match     t
        helm-ag-insert-at-point 'symbol))

(use-package helm-gtags
  :ensure t
  :diminish helm-gtags-mode
  :config
  (setq  helm-gtags-pulse-at-cursor nil)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

(use-package helm-descbinds
  :ensure t
  :bind ("C-h b" . helm-descbinds)
  :config
  (helm-descbinds-mode))


(use-package window
  :config
  :bind (("s-x" . delete-window)
         ("s-c" . delete-other-windows)))

(use-package ace-window
  :ensure t
  :custom
  (aw-ignore-current t)
  (aw-keys '(?x ?c ?v ?b ?n ?m ?a))
  :bind (("s-z" . ace-window)))

(use-package windmove
  :custom
  (windmove-create-window  t)
  :bind (("s-d"       . windmove-right)
         ("s-a"       . windmove-left)
         ("s-w"       . windmove-up)
         ("s-s"       . windmove-down)))


(use-package emmet-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :bind (:map web-mode-map ("C-=" . web-mode-mark-and-expand))
  :init
  (setq web-mode-engines-alist '(("angular"    . "\\.html\\'"))
        web-mode-enable-current-element-highlight     t
        web-mode-enable-element-content-fontification t
        web-mode-enable-element-tag-fontification     t)

  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-hook (lambda ()
                             (setq web-mode-markup-indent-offset 2)
                             (setq web-mode-code-indent-offset 4)
                             (emmet-mode 1)))

  :config
  (add-to-list 'company-backends 'company-css)
  (add-to-list 'company-backends 'company-web-html))


(use-package js2-mode
  :ensure t
  :mode        "\\.js\\'"
  :interpreter "node"
  :bind (:map js2-mode-map
              ("C-'" . toggle-quotes)
              ;; ("C->" . ma/insert-arrow)
              )
  :init
  (add-hook 'js2-mode-hook 'tern-setup)

  :config
  (use-package toggle-quotes :ensure t)

  (use-package tern :ensure t
    :defer  t
    :load-path "~/.nvm/versions/node/v10.5.0/lib/node_modules/tern/emacs/"
    :init
    (add-to-list 'exec-path "~/.nvm/versions/node/v10.5.0/bin/")
    (defun tern-setup ()
      (interactive)
      (when (string-match "\\.js\\'" buffer-file-name)
        (tern-mode 1)))))


(use-package company
  :ensure t
  :diminish company-mode
  :bind (:map company-mode-map ("<s-tab>" . company-complete))
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
         (ledger-mode        . company-mode))


  :init
  (setq
   company-idle-delay            0
   company-tooltip-idle-delay    0
   company-minimum-prefix-length 1
   company-show-numbers          t
   company-dabbrev-downcase      nil)

  (defun  ma/reorder-argument-company-fill-propertize (orig-fun &rest args)
    "This advice is for show number of company to left side"
    (if (string= " " (car (last args)))
        (apply orig-fun args)
      (apply orig-fun (append (butlast args 2) (reverse (last args 2))))))

  (advice-add
   #'company-fill-propertize
   :around
   #'ma/reorder-argument-company-fill-propertize)

  :config
  (use-package company-web
    :ensure t)

  (use-package company-shell
    :ensure t
    :disabled
    :init
    (add-to-list 'company-backends 'company-shell))

  (use-package company-ycmd
    :ensure
    :init
    (company-ycmd-setup))

  (use-package company-auctex
    :ensure t
    :init
    (company-auctex-init)
    (eval-after-load "company-auctex"
      ;; override this function, bad alignament in company
      '(defun company-auctex-symbol-annotation (candidate)
         nil)))

  (add-to-list 'company-backend 'company-ispell))

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
  :ensure t
  :bind ("C-c f t" . transpose-frame))

(use-package json-mode
  :ensure t
  :init
  (make-local-variable 'js-indent-level)
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))

  :mode (("\\.geojson\\'" . json-mode)
         ("\\.json\\'"    . json-mode)))


(use-package openwith
  :disabled
  :ensure t
  :defer  3
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("db"))
               "sqlitebrowser"
               '(file))
         ;; (list (openwith-make-extension-regexp
         ;;        '("ui"))
         ;;       "designer"
         ;;       '(file))
         (list (openwith-make-extension-regexp
                '("odg"))
               "lodraw"
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf"))
               "okular"
               '(file))
         ;; (list (openwith-make-extension-regexp
         ;;        '("dbm"))
         ;;       "pgmodeler"
         ;;       '(file))
         ))
  (openwith-mode 1))

(use-package recentf
  :config
  (run-at-time nil (* 10 60) (lambda () (let ((inhibit-message t)) (recentf-save-list))))
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude
               (format "%s/\\.emacs\\.d/elpa/.*" (getenv "HOME")))
  (add-to-list 'recentf-exclude
               (format "%s/\\.emacs\\.d/\\(ido\\.last\\|recentf\\|\\.gitignore\\)" (getenv "HOME")))
  (recentf-mode +1))


(use-package org-indent
  :diminish
  :hook (org-mode . org-indent-mode)
  :custom
  (org-indent-indentation-per-level 4))

(use-package org-journal
  :ensure t
  :bind (("C-c C-j" . org-journal-new-entry))

  :preface
  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+title: Daily Journal")
       (`weekly "#+title: Weekly Journal")
       (`monthly "#+title: Monthly Journal")
       (`yearly "#+title: Yearly Journal"))))

  :config
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y-%m-%d.journal")
  (setq org-journal-file-header 'org-journal-file-header-func)
  (setq org-journal-date-format "%A, %Y/%m/%d")
  (if (string= (system-name) "jaylah")
      (setq org-journal-dir "~/syncthing/journal/personal/")
    (setq org-journal-dir "~/syncthing/journal/work/")))

(use-package org
  :load-path "./defuns"
  :mode (("\\.org\\'" . org-mode))
  :bind (:map org-mode-map
              ("C-c C-v t" . ma/toggle-current-src-block))

  :config
  (add-to-list 'org-modules 'org-habit)
  (require 'org-defun)

  (unbind-key "C-c C->" org-mode-map)
  (unbind-key "C-," org-mode-map)
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; no add new line on new item
  (setq org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))

  (setq org-startup-folded t)
  (setq org-cycle-separator-lines 0)
  ;; prevent org mode repositioning text when cicle visibility
  (remove-hook 'org-cycle-hook #'org-optimize-window-after-visibility-change)

  (add-hook
   'org-src-mode-hook
   (lambda () (setq org-src--saved-temp-window-config nil)))

  (setq org-src-fontify-natively   t
        org-src-tab-acts-natively  t
        org-src-window-setup       'current-window)


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
     (ruby    . t)))

  ;; use ivy with org-goto
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil))


(use-package org-habit
  :custom
  (org-habit-graph-column 70))


(use-package org-capture
  :config
  ;; org capture
  (define-key global-map (kbd "C-c x") 'org-capture)

  (setq org-capture-templates
        (quote (("t" "Todo: Need be done soon" entry (file+headline "~/syncthing/capture/personal-task.org"  "Need be done soon") "* TODO %?\n%i")
                ("l" "Todo: Need be done sometime" entry (file+headline "~/syncthing/capture/personal-task.org"  "Sometime in the future") "* TODO %?\n%i")
                ("w" "work related captures")
                ("wt" "Todo" entry (file+headline "~/syncthing/capture/work-task.org" "Tasks need be done soon") "* TODO %?\n%i")))))



(use-package beacon
  :ensure t
  :diminish beacon-mode
  :init
  (setq
   beacon-color       "#eead0e"
   beacon-blink-delay 0.1
   beacon-dont-blink-commands '(next-line previous-line forward-line mwheel-scroll scroll-down-1 scroll-up-1))
  (beacon-mode 1))

(use-package avy
  :ensure t
  :bind (("C-z z"  . avy-goto-char-timer)
         ("C-z x"  . avy-goto-char-in-line))
  :custom
  (avy-timeout-seconds 10.0) ;; confirm with RET
  :init
  (global-set-key (kbd "C-z") nil))

(use-package misc-defuns
  :load-path "./defuns/"
  :init
  (global-set-key (kbd "C-o")           'ma/open-line-and-indent)
  (global-set-key (kbd "<C-return>")    'ma/open-line-below)
  (global-set-key (kbd "<C-S-return>")  'ma/open-line-above)
  (global-set-key (kbd "H-l")           'ma/goto-line-with-feedback)
  (global-set-key (kbd "<M-backspace>") 'ma/kill-line)
  (global-set-key (kbd "C-c e")         'ma/eval-and-replace)
  (global-set-key (kbd "C-c f c")       'make-frame-command)
  ;; (global-set-key (kbd "C-c i f")       'ma/insert-file-path-at-point)
  ;; (global-set-key (kbd "C-c c f")       'ma/save-file-path-to-kill-ring)
  (global-set-key (kbd "C-c j ")        'ma/join-line)
  (global-set-key (kbd "M-w")           'ma/kill-ring-save-line-or-region)
  (global-set-key (kbd "C-w")           'ma/kill-line-or-region)
  (global-set-key (kbd "C-y")           'ma/yank-with-feedback))

(use-package crux
  :ensure t
  :init
  (global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

(use-package sql-indent
  :ensure t
  :pin gnu
  :hook (sql-mode . sqlind-minor-mode)
  :config
  (defvar my-sql-indentation-offsets-alist
    `((select-clause 0)
      (insert-clause 0)
      (delete-clause 0)
      (update-clause 0)
      ,@sqlind-default-indentation-offsets-alist))

  (add-hook 'sqlind-minor-mode-hook
            (lambda ()
              (setq sqlind-indentation-offsets-alist
                    my-sql-indentation-offsets-alist))))

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
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("i"  . dired-subtree-insert)
              ("k"  . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds  t)
  :custom-face
  (dired-subtree-depth-1-face ((t (:background "#f4eddb"))))
  (dired-subtree-depth-2-face ((t (:background "#f2e9d3"))))
  (dired-subtree-depth-3-face ((t (:background "#efe5cb"))))
  (dired-subtree-depth-4-face ((t (:background "#ede1c4"))))
  (dired-subtree-depth-5-face ((t (:background "#eaddbc"))))
  (dired-subtree-depth-6-face ((t (:background "#e8d9b4")))))

(use-package tex
  :ensure auctex
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
  :ensure t
  :init
  (setq
   gist-ask-for-description t
   gist-ask-for-filename t))

(use-package simple-httpd
  :ensure t)

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package qt-pro-mode
  :load-path "site-lisp/"
  :mode "\\.pro\\'" )

(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (typescript-mode company)
  :hook ((typescript-mode . tide-setup)))

;; (use-package tide
;;   :ensure t
;;   ;; :mode ("\\.ts\\'" . js2-mode)
;;   :init
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (when (string-match "\\.ts\\'" buffer-file-name)
;;       ;;(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
;;       (tide-setup)
;;       (flycheck-mode +1)
;;       (setq flycheck-check-syntax-automatically '(save mode-enabled)
;;             js2-mode-show-parse-errors          nil
;;             js2-mode-show-strict-warnings       nil
;;             )
;;       (eldoc-mode +1)
;;       (company-mode +1)))

;;   ;; aligns annotation to the right hand side
;;   (setq company-tooltip-align-annotations t)
;;   (add-hook 'js2-mode-hook #'setup-tide-mode))

(use-package mocha-snippets
  :ensure t)

(use-package npm-mode
  :ensure t
  :defer t)

(use-package deft
  :ensure
  :load-path "./defuns/"
  :bind ([f9] . ma/deft-in-new-frame)
  :init
  (setq deft-directory "~/syncthing/deft")
  (setq deft-extensions '("org" "md" "txt")))

(use-package rainbow-mode
  :ensure t)

(use-package google-translate
  :load-path "./site-lisp/google-translate"
  :bind ("C-c t" . google-translate-smooth-translate)
  :init
  (use-package google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist '(("en" . "es") ("es" . "en"))))

(use-package paradox
  :ensure t
  :config
  (setq paradox-display-download-count t))


(use-package geiser
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t)

(use-package font-lock+
  :disabled
  :load-path "~/.emacs.d/site-lisp/")


(use-package multiple-cursors
  :ensure t
  :bind (("C-<mouse-1>" . mc/add-cursor-on-click)
         ("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-next-like-this-word))
  :init
  (global-unset-key (kbd "C-<down-mouse-1>")))


(use-package iy-go-to-char
  :after (:all multiple-cursors)
  :ensure t
  :init
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))


(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" "/Pipfile\\'"))

(use-package xclip
  :ensure t
  :disabled
  :init
  (xclip-mode 1))

(use-package iss-mode
  :mode "\\.iss\\'"
  :ensure t)


(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))


(use-package scss-mode
  :ensure t
  :init
  (setq scss-compile-at-save nil))

(use-package nginx-mode
  :ensure t)

(use-package sed-mode
  :load-path "site-lisp/"
  :mode "\\.sed\\'")


(use-package flymd
  :ensure t
  :init
  (setq flymd-output-directory "/tmp"))


(use-package subword
  :diminish
  :hook ((prog-mode) .  subword-mode))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-:" . (lambda () (interactive) (insert ":="))))
  :init
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4))))

(use-package go-eldoc
  :disabled
  :ensure t
  :hook (go-mode . go-eldoc-setup))



(use-package highlight-symbol
  :ensure t
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
  :ensure t
  :hook (prog-mode . highlight-numbers-mode))


(use-package lorem-ipsum
  :ensure t)

;; jade mode
(use-package pug-mode
  :ensure t)

(use-package savekill
  :load-path "~/lab/savekill"
  :init
  (setq savekill-keep-text-properties t
        savekill-max-saved-items 100))

(use-package auto-yasnippet
  :ensure t
  :init
  (global-set-key (kbd "H-w") #'aya-create)
  (global-set-key (kbd "H-y") #'aya-expand))

(use-package paredit
  :ensure t
  :disabled
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode))


(use-package uuidgen
  :ensure t)

(use-package see-mode
  :ensure t
  :init
  (setq see-use-align-quotes t))


(use-package adoc-mode
  :ensure)

(use-package python
  :preface
  (defun ma/python-eval-current-line (&optional keep)
    (interactive "P")
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (python-shell-send-region beg end nil t)
      (unless keep
        (forward-line))))

  :bind
  (:map python-mode-map
        ("C-c C-n" . ma/python-eval-current-line))
  :init
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--nosep --simple-prompt -i"))


(use-package htmlize
  :ensure t)

(use-package elfeed
  :ensure t
  :init
  (setq shr-width 100)
  (setq elfeed-feeds
        '("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-teaser.xml"
          "https://blog.codinghorror.com/rss/"
          "https://www.joelonsoftware.com/feed/"
          "http://irreal.org/blog/?feed=rss2"
          "http://sachachua.com/blog/feed/"
          "http://cherian.net/rss.xml"
          "http://emacsninja.com/feed.atom"
          "http://mbork.pl/?action=rss;days=30;all=0;showedit=0;full=1"
          "https://simblob.blogspot.com/feeds/posts/default"
          "http://cachestocaches.com/feed")))

(use-package shr
  :config
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.5)
  (setq shr-width (current-fill-column)))

(use-package qml-mode
  :ensure t
  :mode "\\.qml\\'")


(use-package popwin
  :ensure t
  :config
  (push '(inferior-python-mode :height 20 :noselect t :tail t :stick t) popwin:special-display-config)
  (popwin-mode 1))

(use-package protobuf-mode
  :ensure t)

(use-package eros
  :ensure t
  :init
  (eros-mode 1))


(use-package lsp-mode
  :ensure t
  :hook ((go-mode python-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-diagnostic-package :none)
  (lsp-signature-auto-activate nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references]  . lsp-ui-peek-find-references)
              ("C-."                         . lsp-ui-imenu))
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable      nil)
  (lsp-ui-doc-position   'top))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package systemd
  :ensure t
  :mode (("\\.service\\'" . systemd-mode)))

(use-package goto-line-preview
  :ensure t
  :bind ("H-p" . goto-line-preview))

(use-package tramp
  :ensure t
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
  :ensure t
  :init
  (add-to-list 'interpreter-mode-alist '("gnuplot" . gnuplot-mode)))

(use-package crontab-mode
  :ensure t)

(use-package xref
  :custom
  (xref-after-jump-hook  '(recenter))
  (xref-after-return-hook  nil))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))


(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))


(use-package glsl-mode
  :ensure t)

(use-package visual-fill-column
  :ensure t)


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
  :ensure t)
