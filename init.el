
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
(package-initialize)
(toggle-frame-maximized)
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

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
(require 'init-exec-path) ;; Set up $PATH
(require-package 'diminish)
(require 'init-key-chord)
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
(require 'mapserver-mode)
(require 'init-vimish-fold)

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; Use smex to handle M-x
(when (maybe-require-package 'smex)
  ;; Change path for ~/.smex-items
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(global-set-key (kbd "<escape>")      'keyboard-quit)
