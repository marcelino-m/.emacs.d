(defun ma/toggle-current-src-block ()
  (interactive)
  (let ((head (org-babel-where-is-src-block-head)))
    (if head
        (progn
          (goto-char head)
          (org-cycle))
      (error "Not currently in a code block"))))

(defun ma/org-table-map-tables (function &optional quietly)
  "Apply FUNCTION to the start of all tables in the buffer. buffer is not widen."
  (goto-char (point-min))
  (while (re-search-forward org-table-any-line-regexp nil t)
    (unless quietly
      (message "Mapping tables: %d%%"
               (floor (* 100.0 (point)) (buffer-size))))
    (beginning-of-line 1)
    (when (and (looking-at org-table-line-regexp)
               ;; Exclude tables in src/example/verbatim/clocktable blocks
               (not (org-in-block-p '("src" "example" "verbatim" "clocktable"))))
      (save-excursion (funcall function))
      (or (looking-at org-table-line-regexp)
          (forward-char 1)))
    (re-search-forward org-table-any-border-regexp nil 1))
  (unless quietly (message "Mapping tables: done")))

(defun ma/org-table-recalculate-tables ()
  "Recalculate all tables in buffer. buffer is not widen."
  (interactive)
  (ma/org-table-map-tables
   (lambda ()
     ;; Reason for separate `org-table-align': When repeating
     ;; (org-table-recalculate t) `org-table-may-need-update' gets in
     ;; the way.
     (org-table-recalculate t t)
     (org-table-align))
   t))

(defun org-babel-switch-to-session (&optional arg info)
  "Switch to the session of the current code block.
Uses `org-babel-initiate-session' to start the session.  If called
with a prefix argument then this is passed on to
`org-babel-initiate-session'."
  (interactive "P")
  (save-selected-window
    (pop-to-buffer (org-babel-initiate-session arg info) nil t)))

(provide 'org-defun)
