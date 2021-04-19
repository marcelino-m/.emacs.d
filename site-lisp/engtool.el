

(defcustom engtool-irregular-verbs-file "~/lab/irregular/list-irregular-verbs.csv"
  "Path to file with irregular verbs")


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


(defun engtool-linkify-word-dwim (word)
  (interactive (let ((word (bounds-of-thing-at-point 'word)))
                 (if word
                     (list word)
                   (list (read-string "Word: ")))))
  (when (consp word)
    (setq word (delete-and-extract-region (car word) (cdr word))))

  (insert (format "[[say:%s][%s]]" (engtool-pronounce-url word) word)))


(defun engtool-play-local-file (path)
  (if (file-exists-p path)
      (start-process-shell-command "mplayer" nil (concat "mplayer " path))
    (message "file not found: %s" path)))


(defun engtool-play-remote-file (url)
  (request  url
    :encoding 'binary
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((tmpfile (make-temp-file "engtool")))
                  (with-temp-file tmpfile
                    (set-buffer-file-coding-system 'raw-text)
                    (insert data))
                  (start-process-shell-command "mplayer" nil (concat "mplayer " tmpfile)))))


    :error (cl-function
            (lambda (&key response &allow-other-keys)
              (let ((code (request-response-status-code response)))
                (if (= 404 code)
                    (message "Word wasn't found")
                  (message "An expected error occur getting the word")))))))


(defun engtool-pronounce-this-word (word)
  (interactive (let ((word (thing-at-point 'word)))
                 (if word
                      (progn
                        (set-text-properties 0 (length word) nil word)
                        (list (read-string "Word: " word)))
                   (list (read-string "Word: ")))))
  (engtool-play-remote-file (engtool-pronounce-url word)))


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
