;;; engtool.el-*- lexical-binding: t; -*-

(require 'org)
(require 'mpv)


(defcustom engtool-irregular-verbs-file "~/lab/irregular/list-irregular-verbs.csv"
  "Path to file with irregular verbs")

(defvar engtool-mpv-last-used-file-path nil)
(defvar engtool-mpv-mark-queue nil)
(defvar engtool-mpv-mark-queue-max-size 2)

(setq mpv-default-options
      (list "--audio-display=no"))

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
    (format "[[say:%s::%s--%s][<%s %s>]]" path start end start end))
   (start
    (format "[[say:%s::%s--][<%s END>]]" path start start))
   (end
    (format "[[say:%s::--%s][<START %s>]]" path end end))
   (t
    (format "[[say:%s][<all>]]" path))))


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


(defun engtool--play-with-mpv-process (file start end)
  (cond
   ((and start end)
    (mpv-start file (format "--start=%s" start) (format "--end=%s" end)))
   (start
    (mpv-start file (format "--start=%s" start)))
   (end
    (mpv-start file (format "--end=%s" end)))
   (t
    (mpv-start file))))

(defun engtool-pronounce-url (word)
  (engtool--pronounce-url-from-oxford-dict (downcase word)))


(defun engtool-definition-url (word)
  (engtool--definition-url-from-oxford-dict (downcase word)))


(defun engtool-irregular-verb-list ()
  (interactive)
  (let (verbs)
    (setq verbs (split-string
                 (with-temp-buffer
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
  (interactive
   (let* ((path   engtool-mpv-last-used-file-path)
          (queue  engtool-mpv-mark-queue)
          (start  (if (length>  queue 1)
                      (cadr queue)
                    (car queue)))
          (end    (when (length> queue 1)
                    (car queue))))
     (list
      (read-file-name "select a file: " nil nil t path)
      (read-string "start in (h:mm:ss): " (when start
                                            (org-timer-secs-to-hms (round start))))
      (read-string "end at (h:mm:ss): " (when end
                                          (org-timer-secs-to-hms (round end)))))))

  (if (string-empty-p start)
      (setq start nil)
    (setq start (org-timer-fix-incomplete start)))

  (if (string-empty-p end)
      (setq end nil)
    (setq end (org-timer-fix-incomplete end)))

  (setq engtool-mpv-mark-queue (nbutlast engtool-mpv-mark-queue))
  (insert (engtool--build-link-to-segment-file path start end)))

(defun engtool-mpv-last-start ()
  (let ((queue engtool-mpv-mark-queue))
    (if (length>  queue 1)
        (cadr queue)
      (car queue))))

(defun engtool-mpv-last-end ()
  (let ((queue engtool-mpv-mark-queue))
    (when (length>  queue 1)
      (car queue))))

(defun engtool-play-from-last ()
  (interactive)
  (if (and engtool-mpv-last-used-file-path (file-exists-p engtool-mpv-last-used-file-path))
      (engtool-play-file engtool-mpv-last-used-file-path
                         (engtool-mpv-last-start)
                         (engtool-mpv-last-end))
    (message "no previus file was played")))

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


(defun engtool-select-and-play-file (path start end)
  (interactive (list
                (read-file-name "select a file: " nil nil t)
                (let ((start (read-string "start in (h:mm:ss): ")))
                  (unless (string-empty-p start)
                    (org-timer-fix-incomplete start)))
                (let ((end (read-string "end at (h:mm:ss): ")))
                  (unless (string-empty-p end)
                    (org-timer-fix-incomplete end)))))

  (if (file-exists-p path)
      (progn
        (engtool-play-file path start end)
        (setq engtool-mpv-last-used-file-path path))
    (message "file not found: %s" path)))


(defun engtool-play-file (file start end)
   (if (file-exists-p file)
       (engtool--play-with-mpv-process file start end)
     (error "file not found: %s" file)))


(defun engtool-play-url (url start end  &optional succfn errfn)
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
    (engtool-play-url
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

(defvar engtool-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S-<mouse-1>") 'engtool-pronounce-this-word-on-click)
    (define-key map (kbd "S-<mouse-3>") 'engtool-goto-definition-this-on-click)
    map))


(defun engtool-follow-link(fpath _)
  (let* ((pp    (engtool-parse-path fpath))
         (path  (plist-get pp :path))
         (start (plist-get pp :start))
         (end   (plist-get pp :end)))
    (if (or (string-prefix-p "http://" path)
            (string-prefix-p "https://" path))
        (engtool-play-url path start end)
      (engtool-play-file path start end))))

(org-link-set-parameters "say" :follow #'engtool-follow-link)


(define-minor-mode engtool-mode
  "Function collection to help english learning"
  :lighter " engtool"
  :global t
  :keymap engtool-mode-map)


(defun engtool-mpv-mark-position ()
  (interactive)
  (if-let ((pos    (mpv-get-playback-position))
           (qmax   engtool-mpv-mark-queue-max-size))
      (progn
        (setq engtool-mpv-mark-queue (cons pos engtool-mpv-mark-queue))
        (when (length>  engtool-mpv-mark-queue qmax)
          (setq engtool-mpv-mark-queue (nbutlast engtool-mpv-mark-queue)))
        (message "mark: %s" (org-timer-fix-incomplete
                             (number-to-string (round pos)))))
    (message "nothing currently playing")))



(defvar engtool-mpv-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") 'mpv-pause)
    (define-key map (kbd "q")   'mpv-kill)
    (define-key map (kbd "f")   'mpv-seek-forward)
    (define-key map (kbd "b")   'mpv-seek-backward)
    map))


(defvar engtool-mpv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m") engtool-mpv-command-map)
    map))

(define-minor-mode engtool-mpv-mode
  "Custom key binding while play file"
  :lighter " mpv"
  :keymap engtool-mpv-mode-map)

(defun engtool-mpv-enable-on-start-hook (arg)
  (engtool-mpv-mode 1))

(defun engtool-mpv-disable-on-exit-hook ()
  (engtool-mpv-mode -1))


(add-hook 'mpv-on-start-hook 'engtool-mpv-enable-on-start-hook)
(add-hook 'mpv-on-exit-hook  'engtool-mpv-disable-on-exit-hook)

(provide 'engtool)
