(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; global options

(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 190))
(add-to-list 'default-frame-alist '(cursor-color . "Olivedrab3"))
;; (add-to-list 'default-frame-alist '(cursor-type . bar))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(tool-bar-mode     -1)
(menu-bar-mode     -1)
(blink-cursor-mode  1)
(delete-selection-mode)
(toggle-frame-maximized)
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
 scroll-preserve-screen-position      t
 scroll-step                          1)


(add-to-list 'default-frame-alist '(height . 47))
(add-to-list 'default-frame-alist '(width . 200))

(setq gc-cons-threshold 100000000)

(defalias 'yes-or-no-p 'y-or-n-p)

(custom-set-faces
 '(default ((t (:family "Inconsolata" :foundry "unknown" :slant normal :weight normal :height 119 :width normal)))))

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

(defun ma/neotree-toggle ()
  "Toggle show the NeoTree window."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (or (ignore-errors
          (neotree-dir (projectile-project-root)))
        (neotree-show))))

(defun ma/update-neotree-root ()
  (interactive)
  (when (neo-global--window-exists-p)
    (progn
      (neotree-dir (projectile-project-root))
      (other-window 1))))

;; Setup packages
(use-package exec-path-from-shell
  :ensure t
  :defer  2
  :config
  (dolist (var '("GOPATH"  "NVM_BIN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package info
  :defer t
  :config
  (define-key Info-mode-map (kbd "<prior>") 'scroll-down-1)
  (define-key Info-mode-map (kbd "<next>") 'scroll-up-1)
  (use-package info+
    :ensure t))


(use-package ethan-wspace
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(use-package solarized-theme
  :ensure t
  :disabled t
  :config
  (setq
   ;; make the fringe stand out from the background
   solarized-distinct-fringe-background t
   ;; make the modeline high contrast
   solarized-use-variable-pitch nil
   solarized-high-contrast-mode-line t
   ;; Use less bolding
   solarized-use-less-bold t
   ;; Use more italics
   solarized-use-more-italic t
   ;; Use less colors for indicators such as git:gutter, flycheck and similar
   solarized-emphasize-indicators nil
   ;; Don't change size of org-mode headlines (but keep other size-changes)
   solarized-scale-org-headlines nil
   ;; Avoid all font-size changes
   solarized-height-minus-1  1
   solarized-height-plus-1   1
   solarized-height-plus-2   1
   solarized-height-plus-3   1
   solarized-height-plus-4   1

   x-underline-at-descent-line t)
  (load-theme 'solarized-light))

(use-package csv-mode
  :ensure t
  :mode   "\\.csv\\'")

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


(use-package drag-stuff
  :ensure t
  :bind (([(hyper  up)] . drag-stuff-up)
         ([(hyper  down)] . drag-stuff-down)))



(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))


(use-package swiper
  :ensure t
  :bind ("C-s" . swiper)
  :init
  (setq
   ivy-use-virtual-buffers t))

(use-package smooth-scroll
  :ensure t
  :diminish smooth-scroll-mode
  :init
  :bind (("M-p"   . scroll-down-1)
         ("M-n"   . scroll-up-1)))

(use-package uniquify
  :init
  (setq
   uniquify-buffer-name-style   'reverse
   uniquify-separator           " • "
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re   "^\\*"))


(use-package yasnippet
  :ensure t
  :demand t
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :bind (
         ([backtab]   . yas-expand)
         ("C-c y s"   . yas-insert-snippet)
         ("C-c y n"   . yas-new-snippet)
         ("C-c y v"   . yas-visit-snippet-file)
         )
  :config
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  (setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets/")))

  (yas-global-mode 1))

(use-package prog-mode
  :init
  (setq
   tab-width 4
   ;; disable electric pairing in minibuffer
   electric-pair-inhibit-predicate (lambda (char) (window-minibuffer-p))
   )

  (define-key prog-mode-map (kbd "C-c s") 'ace-jump-char-mode)
  (add-hook 'prog-mode-hook
            '(lambda ()
               (electric-pair-mode)
               (ethan-wspace-mode 1))))



(use-package saveplace
  :init
  (setq-default
   save-place t
   save-place-file "~/.emacs.d/saveplace")
  :config
  (save-place-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init
  (global-undo-tree-mode)
  (global-set-key (kbd "C-\\") 'undo-tree-redo))

(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook (lambda ()
                                  (ethan-wspace-mode 1)
                                  (flyspell-mode)))
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))



(use-package projectile
  :ensure t
  :load-path "./defuns/misc-defuns.el"
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c p s a" . helm-projectile-ag))
  :config
  (use-package helm-projectile
    :ensure t)

  (use-package ggtags
    :ensure t)

  (global-set-key [f7] 'ma/make-frame-command)
  (define-key projectile-mode-map (kbd "C-c p s s") 'helm-projectile-ag)
  (define-key projectile-mode-map (kbd "C-c p o") 'helm-occur)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format "❪℘ %s❫" (projectile-project-name))))
  (setq projectile-switch-project-action '(lambda ()
                                            (projectile-dired)
                                            (ma/update-neotree-root)
                                            (projectile-commander)))


  ;;
  (defun projectile--file-name-sans-extensions (file-name)
    "Return FILE-NAME sans any extensions."
    (file-name-base file-name))

  (defun projectile--file-name-extensions (file-name)
    "Return FILE-NAME's extensions."
    (file-name-extension file-name))

  (add-to-list 'projectile-other-file-alist '("ts"   . ("css" "html")))
  (add-to-list 'projectile-other-file-alist '("html" . ("css" "ts")))
  (add-to-list 'projectile-other-file-alist '("css"  . ("ts" "html")))

  (projectile-global-mode))


(use-package smex
  :ensure t
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))


(use-package ido
  :ensure t
  :defer  3
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10
        ido-use-faces t)

  (define-key ido-file-completion-map (kbd "~") '(lambda ()
                                                   (interactive)
                                                   (cond
                                                    ((looking-back "/") (insert "~/"))
                                                    (:else (call-interactively 'self-insert-command)))))
  :config
  (ido-mode t)

  (use-package flx-ido
    :ensure t
    :config
    (flx-ido-mode 1))

  (use-package ido-vertical-mode
    :ensure t
    :init
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right
          ido-vertical-indicator "•➤")
    :config
    (set-face-attribute 'ido-vertical-first-match-face nil
                        :foreground "DarkOliveGreen1" :weight 'bold)
    (set-face-attribute 'ido-vertical-only-match-face nil
                        :foreground "DarkOliveGreen1" :weight 'bold)
    (set-face-attribute 'ido-vertical-match-face nil
                        :foreground "DarkOliveGreen4")
    (ido-vertical-mode))

  (use-package ido-completing-read+
    :ensure t
    :config
    (ido-ubiquitous-mode 1)))


(use-package imenu-anywhere :ensure t
  :bind (("C-." . imenu-anywhere)))


(use-package cmake-mode
  :ensure t
  :mode ("CMakeList.txt" . cmake-mode))


(use-package flyspell
  :ensure t
  :defer  t
  :init
  (setq ispell-dictionary "castellano")
  :config
  (use-package flyspell-popup
    :ensure t
    :init
    :bind (("C-c c" . flyspell-popup-correct))))

(use-package magit
  :ensure t
  :init
  (use-package gitignore-mode
    :ensure t)
  (use-package git-timemachine
    :ensure t)

  (setq
   magit-save-repository-buffers 'dontask
   magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  (add-hook 'git-commit-mode-hook 'git-commit-turn-on-flyspell)
  (global-git-commit-mode)

  :bind (([f12] . magit-status)))



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
                 (window-height . 0.4)))

  (use-package helm-gtags
    :ensure t
    :config
    (setq  helm-gtags-pulse-at-cursor nil)
    (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
    (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack))

  (use-package helm-ag
    :ensure t
    :config
    (setq helm-ag-fuzzy-match     t
          helm-ag-insert-at-point 'symbol)))

(use-package windmove
  :ensure t
  :bind (("s-d"       . windmove-right)
         ("s-a"       . windmove-left)
         ("s-w"       . windmove-up)
         ("s-s"       . windmove-down)))


(use-package emmet-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer  t
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
              ("C->" . ma/insert-arrow))
  :init
  (add-hook 'js2-mode-hook 'subword-mode)
  (add-hook 'js2-mode-hook 'tern-setup)

  :config
  (use-package toggle-quotes :ensure t)
  (use-package tern :ensure t
    :defer  t
    :load-path (lambda () (expand-file-name
                           (concat (getenv "NVM_PATH")
                                   "/../node_modules/tern/emacs/")))
    :init
    (defun tern-setup ()
      (interactive)
      (when (string-match "\\.js\\'" buffer-file-name)
        (tern-mode 1)))))




(use-package company
  :ensure t
  :defer  t
  :diminish company-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'js2-mode-hook        'company-mode)
  (add-hook 'web-mode-hook        'company-mode)
  (add-hook 'css-mode-hook        'company-mode)
  (add-hook 'c++-mode-hook        'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook      'company-mode)
  (add-hook 'sh-mode-hook         'company-mode)


  (setq
   company-idle-delay            0
   company-tooltip-idle-delay    0
   company-minimum-prefix-length 1
   company-show-numbers          t
   )

  (use-package company-flx
    :ensure t
    :defer  t
    :init
    (with-eval-after-load 'company
      (company-flx-mode +1)))

  :config
  (use-package company-tern
    :ensure t
    :init
    (add-to-list 'company-backends 'company-tern))

  (use-package company-web
    :ensure t)

  (use-package company-shell
    :ensure t
    :disabled
    :init
    (add-to-list 'company-backends 'company-shell))

  (use-package ycmd
    :ensure t
    :defer    f
    :diminish ycmd-mode
    :init
    (setq ycmd-server-command '("python" "/home/marcelo/.emacs.d/vendors/ycmd/ycmd"))
    (add-hook 'c++-mode-hook 'ycmd-mode))

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
  :defer 5)

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
  :ensure t
  :defer  3
  :config
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("db"))
               "sqlitebrowser"
               '(file))
         (list (openwith-make-extension-regexp
                '("ui"))
               "designer"
               '(file))
         (list (openwith-make-extension-regexp
                '("odg"))
               "lodraw"
               '(file))))
  (openwith-mode 1))

(use-package recentf
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  :config
  (add-to-list 'recentf-exclude "~/.emacs.d/elpa/")
  (recentf-mode 1))

(use-package cc-mode
  :defer t
  :init
  (setq compilation-ask-about-save nil)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (c-add-style "my-style"
               '("k&r"
                 (c-offsets-alist . ((innamespace . [0])))))
  (add-hook 'c++-mode-hook (lambda ()
                             (c-set-style "my-style")
                             (setq c-basic-offset 4
                                   tab-width 4
                                   indent-tabs-mode nil)

                             (setq compilation-skip-threshold 2)
                             (setq compilation-scroll-output 'first-error)))
  :config
  (define-key c++-mode-map (kbd "<f5>") 'recompile)
  (define-key c++-mode-map (kbd "C->")  'ma/insert-arrow))



(use-package org-bullets
  :ensure t
  :defer  t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-mode
  :defer t
  :init
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(a)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))


(use-package beacon
  :ensure t
  ;; :disabled t
  :init
  (setq
   beacon-color "olive drab"
   beacon-dont-blink-commands '(next-line previous-line forward-line mwheel-scroll scroll-down-1 scroll-up-1))

  (beacon-mode 1))

(use-package ace-jump-mode
  :ensure t
  :bind (("H-s" . ace-jump-char-mode)
         ("H-w" . ace-jump-word-mode)))

(use-package misc-defuns
  :load-path "defuns/"
  :config
  (global-set-key (kbd "C-o")           'ma/open-line-and-indent)
  (global-set-key (kbd "<C-return>")    'ma/open-line-below)
  (global-set-key (kbd "<C-S-return>")  'ma/open-line-above)
  (global-set-key (kbd "H-l")           'ma/goto-line-with-feedback)
  (global-set-key (kbd "<M-backspace>") 'ma/kill-line)
  (global-set-key (kbd "C-c o")         'ma/show-current-buffer-other-windows))


(use-package sql-indent
  :ensure t
  :defer t
  :init
  (eval-after-load "sql"
    '(load-library "sql-indent")))

(use-package dired
  :init
  (setq dired-dwim-target t))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

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
                      (ethan-wspace-mode 1)
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


(use-package anaconda-mode
  :ensure t
  :init
  (use-package pyenv-mode
    :ensure
    :defer)

  (use-package company-anaconda
    :ensure t)

  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'pyenv-mode)

  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda)))

(use-package qt-pro-mode
  :load-path "site-lisp/"
  :mode "\\.pro\\'" )

(use-package ace-window
  :ensure t
  :bind (("H-j" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package tide
  :ensure t
  :mode ("\\.ts\\'" . js2-mode)
  :init
  (defun setup-tide-mode ()
    (interactive)
    (when (string-match "\\.ts\\'" buffer-file-name)
      ;;(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled)
            js2-mode-show-parse-errors          nil
            js2-mode-show-strict-warnings       nil
            )
      (eldoc-mode +1)
      (company-mode +1)))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (add-hook 'js2-mode-hook #'setup-tide-mode))

(use-package mocha-snippets
  :ensure t)

(use-package npm-mode
  :ensure t
  :defer t)

(use-package deft
  :ensure
  :bind ([f9] . deft)
  :init
  (setq deft-directory "~/Dropbox/notes")
  (setq deft-extensions '("org" "md" "txt")))

(use-package rainbow-mode
  :ensure t)

(use-package google-translate
  :ensure t
  :bind ("C-c t" . google-translate-smooth-translate)
  :init
  (setq
   google-translate-translation-directions-alist '(("en" . "es") ("es" . "en"))))

(use-package paradox
  :ensure t
  :config
  (setq paradox-display-download-count t))


(use-package geiser
  :ensure t)


(use-package zeal-at-point
  :ensure t
  :bind ("C-c d" . zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(python-mode . ("python"))))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package font-lock+
  :ensure t)

(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'ma/neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package  all-the-icons
  :load-path "~/.emacs.d/vendors/all-the-icons/")


(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package multiple-cursors
  :ensure t)

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :init
  (add-hook 'yaml-mode-hook (lambda ()
                              (ethan-wspace-mode 1))))

(use-package xclip
  :ensure t
  :disabled
  :init
  (xclip-mode 1))


(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode))

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

(use-package restart-emacs
  :ensure t)

(use-package flymd
  :ensure t
  :init
  (setq flymd-output-directory "/tmp"))


(use-package go-mode
  :ensure t
  :init
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save)
              (setq tab-width 4)))
  :config
  (use-package go-guru :ensure t)
  (use-package go-rename :ensure t
    :config
    (define-key go-mode-map (kbd "C-c C-r") 'go-rename))

  (use-package company-go :ensure t
    :init
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode))))

  (use-package go-eldoc  :ensure t
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup))


  (defun godef-jump (point &optional other-window)
    "Redefine with prefix argument"
    (interactive "d\nP")
    (condition-case nil
        (let ((file (car (godef--call point))))
          (if (not (godef--successful-p file))
              (message "%s" (godef--error file))
            (push-mark)
            (if (eval-when-compile (fboundp 'xref-push-marker-stack))
                ;; TODO: Integrate this facility with XRef.
                (xref-push-marker-stack)
              (ring-insert find-tag-marker-ring (point-marker)))
            (godef--find-file-line-column file other-window)))
      (file-error (message "Could not run godef binary"))))

  (define-key go-mode-map (kbd "C-c C-e") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "C-<") (lambda ()
                                        (interactive)
                                        (insert "<-")))

  (define-key go-mode-map (kbd "C-:") (lambda ()
                                        (interactive)
                                        (insert ":="))))



(use-package guru-mode
  :ensure t
  :init
  (guru-global-mode +1))

(use-package linum-relative
  :ensure t
  :init
  (linum-relative-mode t)
  (linum-on)
  (setq linum-relative-current-symbol "->"))

(use-package slime
  :ensure t
  :init
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (slime-setup '(slime-fancy slime-company))

  :config
  (use-package slime-company  :ensure t)
  (slime))


(use-package beginend
  :ensure t
  :init
  (beginend-global-mode))

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
