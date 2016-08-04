(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; global options
(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(blink-cursor-mode -1)
(delete-selection-mode)
(toggle-frame-maximized)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default
 case-fold-search                     t
 column-number-mode                   t
 mouse-yank-at-point                  t
 set-mark-command-repeat-pop          t
 tooltip-delay                        0
 save-interprogram-paste-before-kill  t
 inhibit-startup-screen               t
 truncate-lines                       nil
 truncate-partial-width-windows       nil
 visible-bell                         nil
 ediff-window-setup-function          'ediff-setup-windows-plain
 ediff-split-window-function          'split-window-horizontally
 ring-bell-function                   'ignore
 mode-require-final-newline           nil
 scroll-conservatively                1
 imenu-auto-rescan                    t
 )


(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 157))
(setq gc-cons-threshold 100000000)

(add-to-list 'default-frame-alist '(font . "Inconsolata 13"))

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
  :defer  2
  :config
  (dolist (var '("TIMLIB_SRC_ROOT" "LIB_V4D_INCLUDE" "LIB_V4D_TO_LINK" "NVM_BIN"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

(use-package info
  :defer t
  :config
  (define-key Info-mode-map (kbd "<prior>") 'scroll-down-1)
  (define-key Info-mode-map (kbd "<next>") 'scroll-up-1)
  (use-package info+
    :ensure t))


(use-package zenburn-theme
  :ensure t)

(use-package csv-mode
  :ensure t
  :mode   "\\.csv\\'")

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))


(use-package drag-stuff
  :ensure t
  :bind (([(meta  up)] . drag-stuff-up)
         ([(meta  down)] . drag-stuff-down)
         ([(meta  left)] . drag-stuff-left)
         ([(meta  right)] . drag-stuff-right)))


(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))


(use-package swiper
  :ensure t
  :defer  t
  :init
  (setq
   ivy-use-virtual-buffers t))

(use-package smooth-scroll
  :ensure   t
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
  :preface
  (defun yas-new-snippet (&optional choose-instead-of-guess)
    (interactive "P")
    (let ((guessed-directories (yas-guess-snippet-directories)))
      (switch-to-buffer "*new snippet*")
      (erase-buffer)
      (kill-all-local-variables)
      (snippet-mode)
      (set (make-local-variable 'yas-guessed-modes)
           (mapcar #'(lambda (d) (intern (yas-table-name (car d))))
                   guessed-directories))
      (unless (and choose-instead-of-guess
                   (not (y-or-n-p "Insert a snippet with useful headers? ")))
        (yas-expand-snippet
         (concat "\n"
                 "# -*- mode: snippet -*-\n"
                 "# name: $1\n"
                 "# --\n"
                 "$0\n")))))

  :config
  (yas-load-directory "~/.emacs.d/snippets/")
  (yas-global-mode 1))


(use-package prog-mode
  :defer t
  :init
  (setq
   tab-width 4
   indent-tabs-mode nil )
  (add-hook 'prog-mode-hook
            '(lambda ()
               (electric-pair-mode)
               (ethan-wspace-mode 1)
               (define-key prog-mode-map (kbd "\C-s") 'swiper)
               )))



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
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook (lambda ()
                                  (ethan-wspace-mode 1)))
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))


(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map
              ("C-c p s a" . helm-projectile-ag))
  :config
  (use-package helm-projectile
    :ensure t)

  (use-package ggtags
    :ensure t)

  (define-key projectile-mode-map (kbd "C-c p s s") 'helm-projectile-ag)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (format " 𝚷(%s)" (projectile-project-name))))
  (setq projectile-switch-project-action '(lambda ()
                                            (projectile-dired)
                                            (projectile-find-file)))
  (projectile-global-mode))



(use-package smex
  :ensure t
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))



(use-package ido
  :ensure t
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-case-fold nil
        ido-auto-merge-work-directories-length -1
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10
        ido-use-faces nil)
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
    (flx-ido-mode 1)
    )
  (use-package ido-vertical-mode
    :ensure t
    :init
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    :config
    (ido-vertical-mode))
  (use-package ido-ubiquitous
    :ensure t
    :config
    (ido-ubiquitous-mode 1)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-." . imenu-anywhere)))



(use-package cmake-mode
  :ensure t
  :mode ("CMakeList.txt" . cmake-mode))

(use-package fullframe
  :ensure t)

(use-package flyspell
  :ensure t
  :defer  t
  :init
  (setq ispell-dictionary "castellano")
  :config
  (use-package flyspell-popup
    :ensure t
    :init
    (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)))

(use-package magit
  :ensure t
  :init
  (setq magit-save-repository-buffers 'dontask)
  (add-hook 'git-commit-mode-hook (lambda () (flyspell-mode 1)))
  :bind (([f12] . magit-status))
  :config
  (fullframe magit-status magit-mode-quit-window)
  (use-package gitignore-mode
    :ensure t)
  (use-package git-timemachine
    :ensure t))

(use-package helm
  :ensure t
  :init
  (setq
   helm-split-window-in-side-p t
   helm-autoresize-max-height  40
   helm-autoresize-min-height  40
   helm-buffers-fuzzy-matching t
   helm-ag-insert-at-point     'symbol)
  :config
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4))))


(use-package helm-gtags
  :ensure t
  :defer  t
  :config
  (setq  helm-gtags-pulse-at-cursor nil)
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  )

(use-package windmove
  :ensure t
  :defer  5
  :bind (("s-<right>" . windmove-right)
         ("s-<left>"  . windmove-left)
         ("s-<up>"    . windmove-up)
         ("s-<down>"  . windmove-down)
         ("s-s"       . windmove-right)
         ("s-a"       . windmove-left)
         ("s-w"       . windmove-up)
         ("s-z"       . windmove-down)))


(use-package emmet-mode
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer  t
  :init
  (setq web-mode-engines-alist
        '(
          ("angular"    . "\\.html\\'")
          )
        )
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
  :init
  (add-hook 'js2-mode-hook (lambda ()
                             (tern-mode t)))
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (use-package tern
    :ensure t
    :load-path (lambda () (concat (getenv "NVM_PATH") "/../node_modules/tern/emacs/"))))

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

  :config
  (use-package company-tern
    :ensure t
    :init
    (add-to-list 'company-backends 'company-tern))
  (use-package company-web
    :ensure t)
  (use-package company-shell
    :ensure t)

  (use-package ycmd
    :ensure   t
    :defer    f
    :diminish ycmd-mode
    :init
    (setq ycmd-server-command '("python" "/home/marcelo/src/ycmd/ycmd"))
    (add-hook 'c++-mode-hook 'ycmd-mode))

  (use-package company-ycmd
    :ensure
    :init
    (company-ycmd-setup)))




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
          "\\*magit*"
          ))
  (winner-mode 1))

(use-package transpose-frame
  :ensure t
  :defer 5)

(use-package json-mode
  :ensure t
  :init
  (setq
   js-indent-level 2)
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
         ))
  (openwith-mode 1))

(use-package recentf
  :defer 4
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
  (recentf-mode 1))



(use-package cc-mode
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
                             (setq compilation-scroll-output 'first-error)
                             (define-key c++-mode-map (kbd "\C-s") 'swiper))))



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
  :config
  (beacon-mode 1))

(use-package ace-jump-mode
  :ensure t
  :init
  (global-set-key [remap goto-char] 'ace-jump-char-mode)
  (global-set-key [remap goto-line] 'ace-jump-word-mode)
  (global-set-key (kbd "M-g l")     'goto-line-with-feedback))

(use-package misc-defuns
  :load-path "defuns/"
  :config
  (global-set-key (kbd "C-o")          'open-line-and-indent)
  (global-set-key (kbd "<C-return>")   'open-line-below)
  (global-set-key (kbd "<C-S-return>") 'open-line-above))


(use-package sql-indent
  :ensure t
  :defer t
  :init
  (eval-after-load "sql"
    '(load-library "sql-indent")))



;; Functions (load all files in defuns-dir)
;; (setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
;; (dolist (file (directory-files defuns-dir t "\\w+"))
;;   (when (file-regular-p file)

;;     (load file)))

;; (setq settings-dir
;;       (expand-file-name "lisp" user-emacs-directory))


;; ;; Set up load path
;; (add-to-list 'load-path settings-dir)


;; ;; exuberant ctag
;; (setq path-to-ctags "/usr/bin/ctags")

;; ;; write here cust var.

;; (require 'init-utils)
;; (require 'init-dired)



;; (require 'init-qtpro)
;; (require 'init-translate)
;; (require 'init-global-keybinding)
;; (require 'init-zeal)
;; ;;(require 'init-python)
;; (require-package 'ace-jump-mode)
;; (require 'init-keybinding)
;; (require 'init-c)



;; (require 'init-latex)
;; (require 'init-ts)
