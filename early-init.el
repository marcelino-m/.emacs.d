;; -*- lexical-binding: t; -*-

;; Loaded before init.el and before the first frame is created: this is
;; the place for anything that affects the initial frame or startup speed.


;;; Frames

;; `default-frame-alist' applies to every frame, the initial one
;; included, so there is no need to repeat it in `initial-frame-alist'.
(setq default-frame-alist
      (append '((width  . 0.80)         ; fraction of the screen width
                (height . 0.65)         ; fraction of the screen height
                (font   . "IosevkaNerdFontMono-9.5"))
              default-frame-alist))

;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.  NEVER tell a beginner to disable any of these.  They
;; are helpful.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '((:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b"))))


;;; General behavior

;; Buffer-local variables: set their default value.
(setq-default tab-width 4
              truncate-lines t)

(setq ring-bell-function 'ignore
      use-file-dialog nil
      use-short-answers t
      inhibit-startup-screen t
      ;; Truncate in side-by-side windows even in buffers that set
      ;; `truncate-lines' to nil (the default only does it below 50 columns).
      truncate-partial-width-windows t)

(column-number-mode 1)


;;; Scrolling

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      hscroll-step 1
      auto-hscroll-mode 'current-line)


;;; Garbage collection

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 256 1024 1024)
                  gc-cons-percentage 0.1)))
