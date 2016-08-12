(require 's)

(defun q/quote-line-inplace (col-1 col-2 &optional padding)
  "Quote current line in place"
  (let ((padding (or padding 0)))
    (move-to-column col-1 t)
    (insert (concat "\"" (s-repeat padding " ")))
    (move-to-column (+ col-2 padding) t)
    (insert (concat (s-repeat padding " ") "\""))))


(defun q/quote-region-inplace (rbeg rend &optional padding)
  "Quote region"
  (interactive "r\nP")
  (let ((col-left  (q/mincol-no-blanc-in-region rbeg rend))
        (col-right (q/maxcol-no-blanc-in-region rbeg rend))
        (last-line (line-number-at-pos rend)))
    (save-excursion
      (goto-char rbeg)
      (while
          (progn
            (if (not (q/current-line-empty-p))
                (q/quote-line-inplace col-left (+ col-right 1)))
            (and (zerop (forward-line 1))
                 (<= (line-number-at-pos) last-line)))))))

(defun q/mincol-no-blanc-in-region (rbeg rend)
  "Get min colum no blanc char in region, empty lines not
count"
  (interactive "r")
  (let ((max-line         (line-number-at-pos rend))
        (min-col          (point-max))
        (end-of-file-flag nil))

    (save-excursion
      (goto-char rbeg)
      (while
          (progn
            (back-to-indentation)
            (if (and (> min-col (current-column))
                     (not (q/current-line-empty-p)))
                (setq min-col (current-column)))
            (and (zerop (forward-line))
                 (<= (line-number-at-pos) max-line)))))
    (message "%d" min-col)
    min-col))

(defun q/maxcol-no-blanc-in-region (rbeg rend)
  "Get max colum no blanc char in region, empty lines not
count"
  (interactive "r")
  (let ((max-line         (line-number-at-pos rend))
        (max-col          (point-min)))
    (save-excursion
      (goto-char rbeg)
      (while
          (progn
            (end-of-line 1)
            (skip-syntax-backward " ")
            (if (< max-col (current-column))
                (setq max-col (current-column)))
            (and (zerop (forward-line 1))
                 (<= (line-number-at-pos) max-line)))))
    (message "%d" max-col)
    max-col))

(defun q/current-line-empty-p ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))
