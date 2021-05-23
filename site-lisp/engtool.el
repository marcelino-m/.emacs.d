;;; engtool.el-*- lexical-binding: t; -*-

(require 'org-timer)

(defcustom engtool-irregular-verbs-file "~/lab/irregular/list-irregular-verbs.csv"
  "Path to file with irregular verbs")

(defvar engtool-last-used-file-path nil)
(defvar engtool-last-used-file-start nil)
(defvar engtool-last-used-file-end nil)

(defun engtool--pronounce-url-from-oxford-dict (word)
  (let* ((baseurl "https://www.oxfordlearnersdictionaries.com/media/english/us_pron")
         (file   (concat word "__us_1.mp3"))
         (head1  (substring file 0 1))
         (head2  (substring file 0 3))
         (head3  (substring file 0 5)))
    (s-lex-format  "${baseurl}/${head1}/${head2}/${head3}/${file}")))


(defun engtool--pronounce-url-from-google-dict (word)
  (let ((baseurl "https://ssl.gstatic.com/dictionary/static/pronunciation/2021-03-01/audio")
        (head1  (substring word 0 2)))
    (s-lex-format  "${baseurl}/${head1}/${word}_en_us_1.mp3")))


(defun engtool--definition-url-from-oxford-dict (word)
  (concat "https://www.oxfordlearnersdictionaries.com/definition/english/" word))


(defun engtool--build-link-to-segment-file (path start end)
  (cond
   ((and start end)
    (insert (format "[[say:%s::%s--%s][<%s %s>]]" path start end start end)))
   (start
    (insert (format "[[say:%s::%s--][<%s end>]]" path start start)))
   (end
    (insert (format "[[say:%s::--%s][<start %s>]]" path end end)))
   (t
    (insert (format "[[say:%s][<all>]]" path)))))



(defun engtool--play-with-mpv-shell-commnad (file start end)
  (if (file-exists-p file)
      (let ((spos "")
            (epos  ""))
        (when start
          (setq spos (format " --start=%s " start)))
        (when end
          (setq epos (format " --end=%s " end)))
        (start-process-shell-command "mpv" nil (concat "mpv " file spos epos)))
    (message "file not found: %s" path)))


(defun engtool-pronounce-url (word)
  (engtool--pronounce-url-from-oxford-dict (downcase word)))


(defun engtool-definition-url (word)
  (engtool--definition-url-from-oxford-dict (downcase word)))


(defun engtool-irregular-verb-list ()
  (interactive)
  (let (verbs)
    (setq verbs (split-string (with-temp-buffer
                               (insert-file-contents-literally engtool-irregular-verbs-file)
                               (buffer-string))
                             "\n" t))
    (completing-read "Irregular verb: " verb)))


(defun engtool-linkify-word-atpoint-to-sound-dict (word)
  (interactive (let ((word (bounds-of-thing-at-point 'word)))
                 (if word
                     (list word)
                   (list (read-string "Word: ")))))
  (when (consp word)
    (setq word (delete-and-extract-region (car word) (cdr word))))

  (insert (format "[[say:%s][%s]]" (engtool-pronounce-url word) word)))


(defun engtool-unlikify-atpoint ()
    "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))


(defun engtool-insert-link-to-file-within-segment (path start end)
  (interactive (let ((path engtool-last-used-file-path))
                 (list
                  (read-file-name "select a file: " nil nil t path)
                  (org-timer-fix-incomplete
                   (read-string "start in (h:mm:ss): " engtool-last-used-file-end))
                  (org-timer-fix-incomplete
                   (read-string "end at (h:mm:ss): " )))))
  (setq engtool-last-used-file-path  (expand-file-name path)
        engtool-last-used-file-start start
        engtool-last-used-file-end   end)

  (insert (engtool--build-link-to-segment-file
           engtool-last-used-file-path
           engtool-last-used-file-start
           engtool-last-used-file-end)))


(defun engtool-parse-path (filepath)
  "[http[s]://]/path/tofile[::[h:mm:ss]--[h:mm:ss]]"

  (let* ((parts (split-string filepath "::" t))
         (path    (nth 0 parts))
         (segment (split-string (or (nth 1 parts) "--") "--"))
         (start   (nth 0 segment))
         (end     (nth 1 segment)))
    (list
     start end
     :path path
     :start (unless (string-empty-p start)
              start)
     :end (unless (string-empty-p end)
            end))))


(defun engtool-play-local-file (path start end)
  (if (file-exists-p path)
      (engtool-play-file path start end)
    (message "file not found: %s" path)))


(defun engtool-play-file (file start end)
  (engtool--play-with-mpv-shell-commnad file start end))


(defun engtool-play-remote-file (url start end  &optional succfn errfn)
  (request  url
    :encoding 'binary
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((tmpfile (make-temp-file "engtool")))
                  (with-temp-file tmpfile
                    (set-buffer-file-coding-system 'raw-text)
                    (insert data))
                  (engtool-play-file tmpfile start end))))

    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (let ((code (request-response-status-code response)))
                (if errfn
                    (funcall errfn code)
                  (message "url wasn't found")))))))


(defun engtool-pronounce-this-word (word)
  (interactive (let ((word (thing-at-point 'word)))
                 (if word
                      (progn
                        (set-text-properties 0 (length word) nil word)
                        (list (read-string "Word: " word)))
                   (list (read-string "Word: ")))))

  (let ((url (engtool-pronounce-url word)))
    (engtool-play-remote-file
     url nil nil nil (lambda (code)
                   (if (= code 404)
                       (message "Word: %s, wasn't found" word)
                     (message "An expected error occur getting the word: http code: %d" code))))))


(defun engtool-goto-definition-this-word (word)
  (interactive (let ((word (thing-at-point 'word)))
                 (if word
                     (progn
                        (set-text-properties 0 (length word) nil word)
                        (list (read-string "Word: " word)))
                   (list (read-string "Word: ")))))
  (browse-url (engtool-definition-url word)))


(defun engtool-pronounce-this-word-on-click (event)
  (interactive "e")
  (let* ((e (event-start event)))
    (select-window (posn-window e))
    (goto-char (posn-point e))
    (when-let ((word (thing-at-point 'word)))
        (engtool-pronounce-this-word word))))


(defun engtool-goto-definition-this-on-click (event)
  (interactive "e")
  (let* ((e (event-start event)))
    (select-window (posn-window e))
    (goto-char (posn-point e))
    (when-let ((word (thing-at-point 'word)))
        (engtool-goto-definition-this-word word))))


(define-minor-mode engtool-mode
  "Function collection to help english learning"
  :lighter " engtool "
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "S-<mouse-1>") 'engtool-pronounce-this-word-on-click)
            (define-key map (kbd "S-<mouse-3>") 'engtool-goto-definition-this-on-click)
            map))

(provide 'engtool)
