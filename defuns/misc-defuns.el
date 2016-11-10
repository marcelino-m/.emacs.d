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


(defun ma/goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun ma/open-line-and-indent ()
  (interactive)
  (newline-and-indent)
  (end-of-line 0)
  (indent-for-tab-command))


(defun ma/open-line-below (&optional keep)
  "Open line below current line, whithout breack current line, if given
prefix argument keep point in curretn position"
  (interactive "P")
  (let ((point (point)))
    (end-of-line)
    (newline)
    (indent-for-tab-command)
    (if keep
        (goto-char point))))

(defun ma/open-line-above (&optional keep)
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



;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun ma/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun ma/rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
         (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))




;; start a httpd-server in current directory
(defun ma/httpd-start-here (directory port)
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


(defun ma/buffer-to-html (buffer)
  (with-current-buffer (htmlize-buffer buffer)
    (buffer-string)))

(defun ma/sudo-edit (&optional arg)
  (interactive "p")
  (if (or (not (eql  arg 1)) (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Change dictionary ispell
(defun ma/switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "castellano") "english" "castellano")))
    (ispell-change-dictionary change)
    (flyspell-buffer)
    (message "Dictionary switched from %s to %s" dic change)
    ))

;; Create a new empy buffer
(defun ma/new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-08-11"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

(defun ma/insert-arrow ()
  "Insert arrow acording mode"
  (interactive)
  (cond
   ((or (eq major-mode 'c++-mode) (eq major-mode 'c-mode))
    (insert "->"))
   ((eq major-mode 'js2-mode)
    (insert "=>"))))
