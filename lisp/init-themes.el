(require-package 'solarized-theme)

(setq x-underline-at-descent-line t)

(defun my/theme-light ()
  "Activate a light color theme."
  (interactive)
  (load-theme 'solarized-light t))

(defun my/theme-dark ()
  "Activate a dark color theme."
  (interactive)
  (load-theme 'solarized-dark t))



(defun my/is-night ()
  "Return t if is night ([21 - 7] hrs), nil otherwise"
  (or (>= (string-to-number (format-time-string "%H"
                                             (current-time)))
          21)

      (<= (string-to-number (format-time-string "%H"
                                             (current-time)))
          6)))


(if (my/is-night)
    (my/theme-dark)
  (my/theme-light))





(provide 'init-themes)
