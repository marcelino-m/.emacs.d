;; Like isearch, but adds region (if any) to history and deactivates mark
(global-set-key (kbd "C-s") 'isearch-forward-use-region)
(global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Navigation bindings

(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key [remap goto-char] 'ace-jump-char-mode)


;; Clever newlines
(global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Move windows, even in org-mode

(require-package 'windmove)
(require 'windmove)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)
(global-set-key (kbd "s-s") 'windmove-right)
(global-set-key (kbd "s-a") 'windmove-left)
(global-set-key (kbd "s-w") 'windmove-up)
(global-set-key (kbd "s-z") 'windmove-down)


(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "M-m") 'helm-all-mark-rings)



(provide 'init-keybinding)
