;; Something taken from magnars .emacs.d (https://github.com/magnars/.emacs.d.git)

;; Misc defuns go here
;; It wouldn't hurt to look for patterns and extract once in a while


;; Add spaces and proper formatting to linum-mode. It uses more room than
;; necessary, but that's not a problem since it's only in use when going to
;; lines.

(provide 'misc-defuns)

(setq linum-format (lambda (line)
                     (propertize
                      (format (concat " %"
                                      (number-to-string
                                       (length (number-to-string
                                                (line-number-at-pos (point-max)))))
                                      "d ")
                              line)
                      'face 'linum)))


(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun open-line-and-indent ()
  (interactive)
  (newline-and-indent)
  (end-of-line 0)
  (indent-for-tab-command))


(defun open-line-below (&optional keep)
  "Open line below current line, whithout breack current line, if given
prefix argument keep point in curretn position"
  (interactive "P")
  (let ((point (point)))
    (end-of-line)
    (newline)
    (indent-for-tab-command)
    (if keep
        (goto-char point))))

(defun open-line-above (&optional keep)
  "Open line above current line, whithout breack current line, if given
prefix argument keep point in curretn position"
  (interactive "P")
  (let (point)
    (save-excursion
      (beginning-of-line)
      (newline)
      (forward-line -1)
      (indent-for-tab-command)
      (setq point (point)))
    (unless keep
      (goto-char point))))

;; start a httpd-server in current directory
(defun httpd-start-here (directory port)
  (interactive (list (read-directory-name "Root directory: " default-directory nil t)
                     (read-number "Port: " 8017)))
  (setq httpd-root directory)
  (setq httpd-port port)
  (httpd-start)
  (browse-url (concat "http://localhost:" (number-to-string port) "/")))

;; shorthand for interactive lambdas
;; (defmacro λ (&rest body)
;;   `(lambda ()
;;      (interactive)
;;      ,@body))

;; (global-set-key (kbd "s-l") (λ (insert "\u03bb")))


(defun buffer-to-html (buffer)
  (with-current-buffer (htmlize-buffer buffer)
    (buffer-string)))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or (not (eql  arg 1)) (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
