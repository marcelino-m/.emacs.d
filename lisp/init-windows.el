;; Navigate window layouts with "C-c <left>" and "C-c <right>"

(winner-mode 1)
(setq winner-boring-buffers '("*Completions*"
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
                              "\\*magit.*"
                              ))


(require-package 'smooth-scroll)
(require 'smooth-scroll)
(smooth-scroll-mode t)
(global-set-key [(control  down)]  'scroll-up-1)
(global-set-key [(control  up)]    'scroll-down-1)
(global-set-key [(control  left)]  'scroll-right-1)
(global-set-key [(control  right)] 'scroll-left-1)
(diminish 'smooth-scroll-mode)

(defun my/scroll-other-window-one-line ()
  (interactive)
  (scroll-other-window 1))

(defun my/scroll-other-window-down-one-line ()
  (interactive)
  (scroll-other-window-down 1))


(global-set-key [(control meta down)]  'my/scroll-other-window-one-line)
(global-set-key [(control meta up)]  'my/scroll-other-window-down-one-line)

(provide 'init-windows)
