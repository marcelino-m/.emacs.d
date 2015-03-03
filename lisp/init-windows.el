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
                              ))

;; ace-windows
(require-package 'ace-window)
(require 'ace-window)
(key-chord-define-global "oo" 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))


(provide 'init-windows)
