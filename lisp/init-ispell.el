(require 'flyspell)
(require-package 'flyspell-popup)

(setq ispell-dictionary "castellano")
(define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)
(provide 'init-ispell)
