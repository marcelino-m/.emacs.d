;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
;; (setq use-file-dialog nil)
;; (setq use-dialog-box nil)

;;(setq inhibit-startup-echo-area-message t)


;; ;;----------------------------------------------------------------------------
;; ;; Show a marker in the left fringe for lines not in the buffer
;; ;;----------------------------------------------------------------------------
;; (setq indicate-empty-lines t)


;; ;;----------------------------------------------------------------------------
;; ;; Window size and features
;; ;;----------------------------------------------------------------------------
;; (when (fboundp 'tool-bar-mode)
;;   (tool-bar-mode -1))
;; (when (fboundp 'set-scroll-bar-mode)
;;   (set-scroll-bar-mode nil))

;; (let ((no-border '(internal-border-width . 0)))
;;   (add-to-list 'default-frame-alist no-border)
;;   (add-to-list 'initial-frame-alist no-border))


;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (with-selected-frame frame
;;               (unless window-system
;;                 (set-frame-parameter nil 'menu-bar-lines 0)))))

;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))

;; ;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; ;; so we zero it explicitly in those cases.
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             (setq line-spacing 0)))


(provide 'init-gui-frames)
