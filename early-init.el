(defun prot-emacs-add-to-list (list element)
  "Add to symbol of LIST the given ELEMENT.
Simplified version of `add-to-list'."
  (set list (cons element (symbol-value list))))


(mapc
 (lambda (var)
  (prot-emacs-add-to-list var '(width . 0.80))
  (prot-emacs-add-to-list var '(height . 0.65))
  (prot-emacs-add-to-list var '(cursor-color . "#e52b50"))
  (prot-emacs-add-to-list var '(font . "Fira Code Nerd Font-10"))
  (prot-emacs-add-to-list var '(vertical-scroll-bars . nil)))
   '(default-frame-alist initial-frame-alist))


(setq-default tab-width 4
              ring-bell-function 'ignore
              truncate-lines t)


(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '((:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b")))

      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      column-number-mode t

      truncate-partial-width-windows  t
      scroll-preserve-screen-position nil
      mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      hscroll-step 1
      auto-hscroll-mode 'current-line)


;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.  NEVER tell a beginner to disable any of these.  They
;; are helpful.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar prot-emacs--file-name-handler-alist file-name-handler-alist)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 256)
                  gc-cons-percentage 0.1
                  file-name-handler-alist prot-emacs--file-name-handler-alist
                  vc-handled-backends prot-emacs--vc-handled-backends)))
