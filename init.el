;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
(package-initialize)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(toggle-frame-maximized)
(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist '(height . 45))
(add-to-list 'default-frame-alist '(width . 157))


;;(desktop-save-mode 1)
;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Set path to dependencies
;; (setq site-lisp-dir
;;       (expand-file-name "site-lisp" user-emacs-directory))

(setq settings-dir
      (expand-file-name "lisp" user-emacs-directory))

;; Add external projects to load path
;; (dolist (project (directory-files site-lisp-dir t "\\w+"))
;;   (when (file-directory-p project)
;;     (add-to-list 'load-path project)))


;; Set up load path
(add-to-list 'load-path settings-dir)
;; (add-to-list 'load-path site-lisp-dir)

;; exuberant ctag
(setq path-to-ctags "/usr/bin/ctags")

;; write here cust var.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; if custom-file don't exist, then create empty file
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

(require 'init-utils)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-dimish)
(require 'init-exec-path) ;; Set up $PATH
(require 'init-c++)
(require 'init-themes)
(require 'init-dired)
(require 'init-ido)
(require 'init-editing-utils)
(require 'init-uniquify)
(require 'init-windows)
(require 'init-git)
(require 'init-helm)
(require 'init-projectile)
(require 'init-company)
(require 'init-backup)
(require 'init-imenu-anywhere)
(require 'init-cmake)
(require 'init-csv)
(require 'init-save-places)
(require 'init-org)
(require 'init-ispell)
(require 'init-js2)
(require 'init-php)
(require 'init-markdown)
(require 'init-qtpro)
(require 'init-web)
(require 'init-translate)
(require 'init-powerline)
(require 'init-prog-mode)
(require 'init-helm-gtags)
(require 'init-global-keybinding)
(require 'init-sparkql)
(require 'init-zeal)
;;(require 'init-python)
(require-package 'ace-jump-mode)
(require 'init-keybinding)
(require-package 'tomatinho)
(require 'tomatinho)
(require-package 'smooth-scrolling)
(require 'smooth-scrolling)
(require-package 'unicode-fonts)
;; (require 'unicode-fonts)
;; (unicode-fonts-setup)
(require-package 'page-break-lines)
(global-page-break-lines-mode)
(set-default 'indicate-empty-lines t)
(delete-selection-mode)
(require 'init-c)

(require-package 'bury-successful-compilation)
(require 'init-swiper)
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

(require-package 'paradox)
(require 'paradox)
(require-package 'transpose-frame)
(require 'transpose-frame)
(require 'init-sql)

;; Use smex to handle M-x
(when (maybe-require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))
(require 'init-json)

(require 'init-css)
(require 'init-latex)
(require 'init-openwith)
(require 'init-ts)
