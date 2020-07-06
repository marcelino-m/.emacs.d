(require 'flash-region)

;; Add spaces and proper formatting to linum-mode. It uses more room than
;; necessary, but that's not a problem since it's only in use when going to
;; lines.
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
  "Show line numbers temporarily, while prompting for the line number input, this func
require linum-relative"
  (interactive)
  (let ((is-linum-on (bound-and-true-p display-line-numbers-mode))
        (linumnum-avail (boundp 'display-line-numbers))
        (linum-fn nil))
    (if linumnum-avail
        (setq linum-fn 'display-line-numbers-mode)
      (setq linum-fn 'linum-mode))
    (unwind-protect
        (progn
          (unless is-linum-on
            (funcall linum-fn))
          (call-interactively (if (and linumnum-avail
                                       (or (eq display-line-numbers-type 'relative)
                                           (eq display-line-numbers-type 'visual)))
                                  (forward-line (read-number "Goto line: " 0))
                                'goto-line)))
      (unless is-linum-on
        (funcall linum-fn -1)))))

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
    (message "Dictionary switched from %s to %s" dic change)))

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


(defun ma/goto-lab ()
  (interactive)
  (dired "~/lab/"))


(defun ma/delete-frame ()
  (interactive)
  (delete-frame))

(defun ma/kill-line ()
  (interactive)
  (kill-region
   (point)
   (progn
     (back-to-indentation)
     (point))))


(defun ma/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun ma/save-file-path-to-kill-ring ()
  "Copy file path to kill ring"
  (interactive)
  (let ((file (ido-read-file-name "Pick a file: ")))
    (kill-new file)
    (message "Copy file to kill ring: %s" file)
    file))

(defun ma/insert-file-path-at-point ()
  "Insert file path at point and copy it to kill ring "
  (interactive)
  (insert (ma/save-file-path-to-kill-ring)))

(defun ma/join-line (&optional joinjoin)
  "Join current line and next"
  (interactive "P")
  (let (pbegin
        pend)
    (save-excursion
      (end-of-line)
      (skip-syntax-backward " ")
      (unless joinjoin
        (insert " "))
      (setq pbegin (point))
      (re-search-forward "[[:graph:]]" nil t)
      (setq pend (1- (point)))
      (delete-region pbegin pend))))

(defun ma/kill-ring-save-line-or-region (beg end &optional region)
  "Save current  line to kill ring  if no region is  active, with
feedback. Otherwise call `mouse-kill-ring-save'"
 (interactive (list (mark) (point)))
  (if mark-active
      (kill-ring-save beg end region)
    (let (beg end)
      (save-excursion
        (back-to-indentation)
        (setq beg (point))
        (end-of-line)
        (skip-syntax-backward " ")
        (setq end (point))
        (flash-region beg end 'highlight 0.1)
        (kill-ring-save beg end)))))


(defun ma/kill-line-or-region (beg end &optional region)
  "Kill line if no region is active"
  (interactive (list (mark) (point)))
  (if mark-active
      (kill-region beg end region)
    (let (beg end empty-line (cc (current-column)))
      (save-excursion
        (beginning-of-line)
        (if (= (progn (skip-chars-forward " \t") (point))
               (progn (end-of-line) (point)))
            (setq empty-line t)))
      (if empty-line
          (progn
            (beginning-of-line)
            (kill-line)
            (move-to-column cc))
        (progn
          (back-to-indentation)
          (setq beg (point))
          (end-of-line)
          (skip-syntax-backward " ")
          (setq end (point))
          (kill-region beg end region))))))


(defun ma/yank-with-feedback (&optional arg)
  (interactive "*P")
  (let ((beg (point)))
    (yank arg)
    (flash-region beg (point) 'highlight 0.1)))

;; in case delete-selection-mode (delsel.el) is being used
(put 'ma/yank-with-feedback 'delete-selection t)


(defun ma/deft-in-new-frame (&optional arg)
  "Launch deft in a new frame"
  (interactive "P")
  (if (not arg)
      (deft)
    (select-frame (make-frame-command))
    (deft)))


(defun ma/org-toggle-view ()
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (org-do-emphasis-faces nil))



(defun  ma/jump-to-mark-skip-same-line ()
  "Like C-u SPC but skip marks in current line"
  (interactive)
  (let ((startl (line-number-at-pos)))
    (while (progn
             (set-mark-command 4)
             (= startl (line-number-at-pos))))))

(provide 'misc-defuns)
