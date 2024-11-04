(require 'package)
(customize-set-variable 'package-archives
                        `(("melpa" . "https://melpa.org/packages/")
                          ,@package-archives))
(customize-set-variable 'package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(use-package diminish
  :ensure t)

(setq warning-minimum-level :error)

(use-package solarized-theme
  :disabled
  :ensure t
  :custom
  (solarized-use-variable-pitch      nil)
  (solarized-high-contrast-mode-line t)
  (solarized-use-less-bold           nil)
  (solarized-use-more-italic         t)
  (solarized-emphasize-indicators    t)
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

  (magit-diff-added     ((t (:background "#f1ead8"  :foreground "#1b5e20"))))
  ;; (magit-diff-changed   ((t (:background "#f1ead8"  :foreground nil))))
  (magit-diff-removed   ((t (:background "#f1ead8"  :foreground "#b0554c"))))
  ;; (magit-section-highlight ((t (:background nil  :foreground "#f1ead8"))))
   (magit-diff-added-highlight    ((t (:background "#efeac7"  :foreground "#1b5e20"))))
  ;; ;; (magit-diff-changed-highlight  ((t (:background "#f1ead8"  :foreground "blue"))))
  (magit-diff-removed-highlight  ((t (:background "#fedfc5"  :foreground "#8e433d"))))
  (diff-refine-added   ((t (:background nil  :foreground "#00cd00"))))
  (diff-refine-changed ((t (:background nil  :foreground "#0000ff"))))
  (diff-refine-removed ((t (:background nil  :foreground "#ff0009"))))

  :config
  (load-theme 'solarized-light t))


(use-package dracula-theme
  :vc (:url "https://github.com/dracula/emacs"
            :rev :newest
            :branch "main")
  :config
  (load-theme 'dracula t))

(use-package vertico
  :ensure t
  :custom
  (setq vertico-count 10)
  (vertico-resize nil)
  (vertico-cycle nil) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; :custom
  ;; (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-s" . consult-line)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-y" . yank)
         ("M-y" . consult-yank-from-kill-ring)
         ("M-g f" . consult-flycheck)
         ("M-g e" . consult-compile-error)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump

         )
  ;;        ("C-c M-x" . consult-mode-command)
  ;;        ("C-c h" . consult-history)
  ;;        ("C-c k" . consult-kmacro)
  ;;        ("C-c m" . consult-man)
  ;;        ("C-c i" . consult-info)
  ;;        ([remap Info-search] . consult-info)
  ;;        ;; C-x bindings in `ctl-x-map'
  ;;        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
  ;;        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
  ;;        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  ;;        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  ;;        ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
  ;;        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
  ;;        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
  ;;        ;; Custom M-# bindings for fast register access
  ;;        ("M-#" . consult-register-load)
  ;;        ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
  ;;        ("C-M-#" . consult-register)
  ;;        ;; Other custom bindings
  ;;        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
  ;;        ;; M-g bindings in `goto-map'
  ;;        ("M-g e" . consult-compile-error)
  ;;        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
  ;;        ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ;;        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ;;        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ;;        ("M-g m" . consult-mark)
  ;;        ("M-g k" . consult-global-mark)
  ;;        ("M-g i" . consult-imenu)
  ;;        ("M-g I" . consult-imenu-multi)
  ;;        ;; M-s bindings in `search-map'
  ;;        ("M-s d" . consult-find)                  ;; Alternative: consult-fd
  ;;        ("M-s c" . consult-locate)
  ;;        ("M-s g" . consult-grep)
  ;;        ("M-s G" . consult-git-grep)
  ;;        ("M-s r" . consult-ripgrep)
  ;;        ("M-s l" . consult-line)
  ;;        ("M-s L" . consult-line-multi)
  ;;        ("M-s k" . consult-keep-lines)
  ;;        ("M-s u" . consult-focus-lines)
  ;;        ;; Isearch integration
  ;;        ("M-s e" . consult-isearch-history)
  ;;        :map isearch-mode-map
  ;;        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
  ;;        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
  ;;        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
  ;;        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
  ;;        ;; Minibuffer history
  ;;        :map minibuffer-local-map
  ;;        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
  ;;        ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function
        (lambda (_) (projectile-project-root))))

;; Disable the damn thing by making it disposable.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(setq  default-input-method  "latin-prefix")
(setq vc-follow-symlinks t)


;; If there enough blanks before/after  the point then treat
;; them as a  "word"
(define-key global-map (kbd "C-<backspace>")
  (lambda (args)
    "First delete blanks before point if there is at least two blanks"
    (interactive "p")
    (if (looking-back "[ \t]\\{2,\\}" nil t)
        (replace-match "" nil nil)
      (delete-region (point) (progn (forward-word (- args)) (point))))))

(define-key global-map (kbd "M-d")
  (lambda (args)
    "First delete blanks after point if there is at least three blanks"
    (interactive "p")
    (if (looking-at "[ \t]\\{3,\\}")
        (replace-match "" nil nil)
      (delete-region (point) (progn (forward-word args) (point))))))


(use-package move-text
  :ensure t
  :bind
  (("C-M-s-<up>" . move-text-up)
   ("C-M-s-<down>" . move-text-down)))


;; fix focus
(use-package frame
  :init
  ;; in gnome, stealing focus doesn't work correctly
  (add-hook 'after-make-frame-functions
            (lambda (frame) (interactive) (select-frame-set-input-focus frame))))

;; flashing when yanking
(use-package simple
  :after flash-region
  :config
  (setq-default indent-tabs-mode nil)
  (advice-add 'yank
              :around
              (lambda (origfn &rest args)
                "flashing after yanked text"
                (let ((beg (point)))
                  (apply origfn args)
                  (flash-region beg (point) 'highlight 0.1))))

  (defun ma/kill-line-or-region (beg end &optional region)
    "Kill line if no region is active"
    (interactive (list (mark) (point)))
    (if mark-active
        (kill-region beg end region)
      (let (beg end empty-line (cc (current-column)))
        (save-excursion
          (beginning-of-line)
          (if (= (progn (skip-chars-forward " \t") (point))
                 (progn (end-of-line) (point)))
              (setq empty-line t)))
        (if empty-line
            (progn
              (beginning-of-line)
              (kill-line)
              (move-to-column cc))
          (progn
            (back-to-indentation)
            (setq beg (point))
            (end-of-line)
            (skip-syntax-backward " ")
            (setq end (point))
            (kill-region beg end region))))))
  (define-key global-map (kbd "C-w") 'ma/kill-line-or-region))

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

(use-package flash-region
   :ensure t)

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))

;; notify when emacs is ready
;; I run emacs in server mode set a systemd units
(use-package notifications
  :init
  (add-hook 'after-init-hook #'(lambda ()
                                 (notifications-notify
                                  :title "Emacs"
                                  :body "I am ready to hack!"
                                  :urgency 'low))))

(delete-selection-mode)

(use-package recentf
  :config
  (run-at-time nil (* 10 60) (lambda () (let ((inhibit-message t)) (recentf-save-list))))
  (setq recentf-max-saved-items 100
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (setq recentf-exclude
        '("/\\.emacs\\.d/\\(elpa/\\|backups/\\)"
          ".gitignore"))

  (recentf-mode +1))

;; save backups
(setq
 backup-directory-alist '(("." . "~/.emacs.d/backups"))
 delete-old-versions -1
 version-control t
 vc-make-backup-files t
 auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("GOPATH" "PYTHONUSERBASE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "~/.local/bin/")
  (add-to-list 'exec-path "/home/marcelo/.nvm/versions/node/v22.7.0/bin")
  (add-to-list 'exec-path (concat (getenv "PYTHONUSERBASE") "/bin")))


(use-package ethan-wspace
  :ensure t
  :diminish ethan-wspace-mode
  :init
  (setq mode-require-final-newline nil)
  (add-hook 'org-mode            #'ethan-wspace-mode)
  (add-hook 'org-src-mode-hook   #'ethan-wspace-clean-all)
  (add-hook 'prog-mode-hook      #'ethan-wspace-mode)
  (add-hook 'markdown-mode-hook  #'ethan-wspace-mode)
  (add-hook 'LaTeX-mode-hook     #'ethan-wspace-mode)
  (add-hook 'yaml-mode-hook      #'ethan-wspace-mode)
  (add-hook 'see-mode-hook       #'ethan-wspace-mode)
  (add-hook 'ledger-mode-hook    #'ethan-wspace-mode)
  (add-hook 'yaml-mode-hook      #'ethan-wspace-mode))

(use-package expand-region
  :ensure t
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


(use-package hydra
  :ensure t)

(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package smooth-scroll
  :ensure t
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
  :ensure t
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode yas-reload-all)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :custom
  (yas-triggers-in-field t)


 :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/")
  (yas-reload-all)
  (add-hook 'go-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
  (add-hook 'tsx-ts-mode-hook 'yas-minor-mode)
  (add-hook 'web-mode-hook 'yas-minor-mode))


(use-package saveplace
  :custom
  (save-place-file  "~/.emacs.d/saveplace")
  :init
  (save-place-mode))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t))

(use-package edit-indirect
  :after markdown
  :ensure t)

(use-package projectile
  :ensure t
  :bind-keymap ("C-,"   . projectile-command-map)
  :bind (
         :map projectile-command-map
              ;; ("s a" . counsel-projectile-ag)
              (","   . projectile-switch-project))


  :custom
  ;; (projectile-completion-system      'ivy)
  (projectile-indexing-method        'hybrid)
  (projectile-sort-order             'modification-time)
  (projectile-switch-project-action  (lambda () (projectile-dired) (projectile-commander)))
  ;; (projectile-mode-line-function     (lambda ()  (format "proj: %s" (projectile-project-name))))
  (projectile-project-search-path    '("~/lab"))
  (projectile-find-dir-includes-top-level t)

  :config

  ;; projectile slows down tramp-mode
  ;; https://www.reddit.com/r/emacs/comments/320cvb/projectile_slows_tramp_mode_to_a_crawl_is_there_a/
  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

  (add-to-list 'projectile-other-file-alist '("tsx"   . ("sass" "scss" "css")))
  (add-to-list 'projectile-other-file-alist '("scss"  . ("tsx" "ts")))
  (add-to-list 'projectile-other-file-alist '("sass"  . ("tsx" "ts")))
  (add-to-list 'projectile-other-file-alist '("css"  . ("tsx" "ts")))

  (projectile-global-mode))

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
  (add-hook 'prog-mode-hook (lambda () (setq flyspell-persistent-highlight nil)))
  (add-hook 'yaml-mode-hook (lambda () (setq flyspell-persistent-highlight nil)))
  (add-hook 'python-mode-hook (lambda () (setq flyspell-persistent-highlight nil)))
  )

(use-package flyspell-correct
  :ensure t
  :bind (:map flyspell-mode-map ("C-c c" . flyspell-correct-wrapper)))

(use-package flyspell-correct-popup
  :ensure t
  :custom
  (flyspell-correct-interface #'flyspell-correct-popup))

(use-package magit
  :ensure t
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
  :ensure t
  :after magit)

(use-package git-modes
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package window
  :after hydra
  :init
  (defhydra hydra-window (global-map "C-x")
    "Some hydra for windows related command"
    ("<left>"  previous-buffer)
    ("<right>" next-buffer))

  :config
  :bind (("s-x" . delete-window)
         ("s-c" . delete-other-windows)))

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
  :ensure t
  :diminish
  :hook (web-mode tsx-ts-mode typescript-ts-mode))

(use-package web-mode
  :ensure t
  :bind (:map web-mode-map ("C-=" . web-mode-mark-and-expand))

  :mode
  (("\\.html?\\'"     . web-mode))

  :custom
  (web-mode-enable-current-element-highlight     t)
  (web-mode-enable-element-content-fontification t)
  (web-mode-enable-element-tag-fontification     t)
  (web-mode-markup-indent-offset                 2))

(use-package json-mode
  :ensure t
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))

  :mode (("\\.geojson\\'" . json-mode)
         ("\\.json\\'"    . json-mode)))


(use-package company
  ;; TODO: try corfu
  :ensure t
  ;; :after lsp-mode
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
         (jtsx-jsx-mode      . company-mode)
         (org-mode           . company-mode))


  :custom
  (company-idle-delay            nil)
  (company-tooltip-idle-delay    0)
  (company-minimum-prefix-length 3)
  (company-show-numbers          'left)
  (company-dabbrev-downcase      nil)
  (company-selection-wrap-around t)

  :config
  (setq company-backends '(company-capf))
  ;; (add-to-list 'company-backend 'company-ispell)
  )

(use-package company-box
  :ensure t
  :custom
  (company-box-doc-enable  nil)
  :hook (company-mode . company-box-mode))

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
              ("k"    . ma/dired-kill-or-up-subdir))

  :custom
  (dired-dwim-target                       t)
  (wdired-allow-to-change-permissions      t)
  (dired-listing-switches             "-AGFlhv --group-directories-first --time-style=long-iso")


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
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package all-the-icons
  ;; after install run the command (all-the-icons-install-fonts)
  :ensure t
  :defer t
  :diminish)

(use-package all-the-icons-dired
  :ensure t
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


(use-package deft
  :ensure t
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
  :ensure t)

(use-package google-translate
  :ensure t
  :bind ("C-c t" . google-translate-smooth-translate)
  :init
  (use-package google-translate-smooth-ui)
  (setq google-translate-translation-directions-alist '(("en" . "es") ("es" . "en")))
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-next-like-this-word))
  :init
  (global-unset-key (kbd "C-<down-mouse-1>")))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'"  . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" "/Pipfile\\'"))

(use-package subword
  :diminish
  :hook ((prog-mode) .  subword-mode))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-=" . (lambda () (interactive) (insert ":="))))
  :init
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'gofmt-before-save))))


(use-package pyvenv
  :ensure t
  :config
  (require 'pyvenv)

  (defun pyvenv-autoload ()
    (interactive)
    "auto activate venv directory if exists"
    (f-traverse-upwards (lambda (path)
                          (let ((venv-path (f-expand "venv" path)))
                            (when (f-exists? venv-path)
                              (pyvenv-activate venv-path))))))

  ;; (add-hook 'python-mode-hook 'pyvenv-autoload)
  (pyvenv-mode 1))

(use-package python
  :bind (:map python-mode-map
              ("<backtab>" . nil)
              ("C-c C-n"   . ma/python-eval-current-line))

  :custom
  (python-eldoc-get-doc nil)
  (python-indent-offset 4)
  (python-shell-interpreter "ipython")
  ;; (python-shell-interpreter-args "--simple-prompt -i")

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
  :ensure t
  :hook (python-mode . ma/enable-py-isort)
  :preface
  (defun ma/enable-py-isort()
    (add-hook 'before-save-hook 'py-isort-before-save nil t)))

(use-package blacken
  :ensure t
  :diminish
  :hook (python-mode . blacken-mode))

(use-package popper
  :ensure t
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
          compilation-mode
          flycheck-error-list-mode
          ))
  (setq popper-reference-buffers
        (append popper-reference-buffers
                '("^\\*eshell.*\\*$" eshell-mode)))
  (popper-mode +1)
  (popper-echo-mode +1))


(use-package lsp-mode
  :ensure t
  :diminish
  :hook ((go-mode python-mode c-mode c++-mode ess-r-mode) . lsp-deferred)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-warn-no-matched-clients nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-diagnostics-disabled-modes '(python-mode))
  (lsp-signature-auto-activate nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-eldoc-render-all nil)
  (lsp-apply-edits-after-file-operations nil)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-lens
  :after lsp-lens
  :diminish)

(use-package lsp-ui
  :ensure t
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

(use-package flycheck
  :ensure t
  :hook ((python-mode) . flycheck-mode)
  :custom
  (flycheck-display-errors-function  nil)
  :init
  (add-hook 'python-mode-hook #'(lambda ()
                                  (setq-local flycheck-disabled-checkers '(python-mypy))
                                  (setq-local flycheck-checker 'python-ruff)))
  (add-hook 'jtsx-jsx-mode-hook #'(lambda ()
                                    (setq-local flycheck-checker 'javascript-eslint)))
  (add-hook 'jtsx-tsx-mode-hook #'(lambda ()
                                  (setq-local flycheck-checker 'javascript-eslint)))
  )

(use-package posframe ;; for lsp-ui-peek
  :ensure t)

(use-package compile
  :custom
  (compilation-ask-about-save   nil)
  (compilation-scroll-output   'first-error))


(use-package selected
  :ensure t
  :diminish selected-minor-mode
  :init
  (add-hook 'prog-mode-hook (lambda ()
                              (selected-minor-mode 1)))

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
                       (consult-line (buffer-substring beg end))
                       (selected-off)))

              :map selected-org-mode-map
              ("*" . (lambda () (interactive) (org-emphasize ?*)))
              ("/" . (lambda () (interactive) (org-emphasize ?/)))
              ("_" . (lambda () (interactive) (org-emphasize ?_)))
              ("=" . (lambda () (interactive) (org-emphasize ?=)))
              ("~" . (lambda () (interactive) (org-emphasize ?~)))
              ("+" . (lambda () (interactive) (org-emphasize ?+)))))

(use-package google-this
  :ensure t
  :diminish)

(use-package undo-tree
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package dotenv-mode
  :ensure t
  :diminish)

(use-package jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  :hook
  (jtsx-jsx-mode . hs-minor-mode)
  (jtsx-tsx-mode . hs-minor-mode)
  (jtsx-typescript-mode . hs-minor-mode)
  (jtsx-tsx-mode        . flycheck-mode)
  (jtsx-tsx-mode        . lsp-deferred)
  (jtsx-jsx-mode        . flycheck-mode)
  (jtsx-jsx-mode        . lsp-deferred)
  (jtsx-typescript-mode . flycheck-mode)
  (jtsx-typescript-mode . lsp-deferred)

  :custom
  (typescript-ts-mode-indent-offset 4)
  (typescript-indent-level 4)
  (js-indent-level 4)
  :config
   (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C--") '(lambda () (interactive) (insert "=>"))))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
      (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
 )

(use-package apheleia
  :ensure t
  :diminish
  :hook
  (tsx-ts-mode . apheleia-mode)
  (typescript-mode . apheleia-mode)
  (typescript-ts-mode . apheleia-mode))

(use-package verb
  :ensure t
  :diminish
  :init
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

(use-package editorconfig
  :ensure t)

(use-package jsonrpc
  :ensure t)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :bind (("C-M-s-v" . copilot-complete)
         :map copilot-completion-map
         ("V" . copilot-accept-completion)
         ("<right>" . copilot-accept-completion-by-word)
         ("L" . copilot-accept-completion-by-line))
  :hook ((web-mode           . copilot-mode)
         (css-mode           . copilot-mode)
         (sh-mode            . copilot-mode)
         (typescript-mode    . copilot-mode)
         (jtsx-jsx-mode      . copilot-mode)
         (python-mode        . copilot-mode)
         (org-mode           . copilot-mode)))


(use-package crux
  :ensure t
  :bind ([remap move-beginning-of-line] . #'crux-move-beginning-of-line))


(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
