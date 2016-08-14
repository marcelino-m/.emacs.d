(require 's)

(defun q/quote-line (col-1 col-2 &optional padding)
  "Quote current line in place"
  (let ((padding (or padding 0)))
    (save-excursion
      (move-to-column col-1 t)
      (insert (concat "\"" (s-repeat padding " ")))
      (move-to-column (+ col-2 padding) t)
      (insert (concat (s-repeat padding " ") "\"")) )
    ))


(defun q/quote-region (rbeg rend &optional padding)
  "Quote region"
  (interactive "r\nP")
  (let ((col-left  (q/mincol-no-blanc-in-region rbeg rend))
        (col-right (q/maxcol-no-blanc-in-region rbeg rend))
        (last-line (line-number-at-pos rend))
        (padd      nil))
    (save-excursion
      (if (and padding (not (consp padding)))
          (setq padd (prefix-numeric-value padding)))

      (goto-char rend)

      (when (not (q/empty-before-point-in-line-p rend))
        (if (> col-right (current-column))
            (q/quote-line col-left (current-column) padd)
          (q/quote-line col-left col-right padd)))

      (goto-char rbeg)

      (when (not (q/empty-after-point-in-line-p rend))
        (if (< col-left (current-column))
            (q/quote-line (current-column) col-right  padd)
          (q/quote-line col-left col-right  padd)))

      (while (and (forward-line 1)
                  (< (line-number-at-pos) last-line))
        (if (not (q/current-line-empty-p))
            (q/quote-line col-left col-right  padd))))))


(defun q/mincol-no-blanc-in-region (rbeg rend)
  "Get min colum no blanc char in region, empty lines not
count"
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
    min-col))

(defun q/maxcol-no-blanc-in-region (rbeg rend)
  "Get max colum no blanc char in region, empty lines not
count"
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
    max-col))

(defun q/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun q/empty-after-point-in-line-p (arg)
  "docstring"
  (save-excursion
    (goto-char arg)
    (looking-at "[[:space:]]*$")))

(defun q/empty-before-point-in-line-p (arg)
  "docstring"
  (save-excursion
    (goto-char arg)
    (looking-back "^[[:space:]]*")))
