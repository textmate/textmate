;;; mpc.el --- A client for the Music Player Daemon   -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2006-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: multimedia

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an Emacs front end to the Music Player Daemon.

;; It mostly provides a browser inspired from Rhythmbox for your music
;; collection and also allows you to play the music you select.  The basic
;; interface is somewhat unusual in that it does not focus on the
;; playlist as much as on the browser.
;; I play albums rather than songs and thus don't have much need for
;; playlists, and it shows.  Playlist support exists, but is still limited.

;; Bugs:

;; - when reaching end/start of song while ffwd/rewind, it may get wedged,
;;   signal an error, ... or when mpc-next/prev is called while ffwd/rewind.
;; - MPD errors are not reported to the user.

;; Todo:

;; - add bindings/buttons/menuentries for the various commands.
;; - mpc-undo
;; - visual feedback for drag'n'drop
;; - display/set `repeat' and `random' state (and maybe also `crossfade').
;; - allow multiple *mpc* sessions in the same Emacs to control different mpds.
;; - look for .folder.png (freedesktop) or folder.jpg (XP) as well.
;; - fetch album covers and lyrics from the web?
;; - improve MPC-Status: better volume control, add a way to show/hide the
;;   rest, plus add the buttons currently in the toolbar.
;; - improve mpc-songs-mode's header-line column-headings so they can be
;;   dragged to resize.
;; - allow selecting several entries by drag-mouse.
;; - poll less often
;;   - use the `idle' command
;;   - do the time-ticking locally (and sync every once in a while)
;;   - look at the end of play time to make sure we notice the end
;;     as soon as possible
;; - better volume widget.
;; - add synthesized tags.
;;   e.g. pseudo-artist = artist + composer + performer.
;;   e.g. pseudo-performer = performer or artist
;;   e.g. rewrite artist "Foo bar & baz" to "Foo bar".
;;   e.g. filename regexp -> compilation flag
;; - window/buffer management.
;; - menubar, tooltips, ...
;; - add mpc-describe-song, mpc-describe-album, ...
;; - add import/export commands (especially export to an MP3 player).
;; - add a real notion of album (as opposed to just album-name):
;;   if all songs with same album-name have same artist -> it's an album
;;   else it's either several albums or a compilation album (or both),
;;   in which case we could use heuristics or user provided info:
;;   - if the user followed the 1-album = 1-dir idea, then we can group songs
;;     by their directory to create albums.
;;   - if a `compilation' flag is available, and if <=1 of the songs have it
;;     set, then we can group songs by their artist to create albums.
;;   - if two songs have the same track-nb and disk-nb, they're not in the
;;     same album.  So from the set of songs with identical album names, we
;;     can get a lower bound on the number of albums involved, and then see
;;     which of those may be non-compilations, etc...
;;   - use a special directory name for compilations.
;;   - ask the web ;-)

;;; Code:

;; Prefixes used in this code:
;; mpc-proc   : management of connection (in/out formatting, ...)
;; mpc-status : auto-updated status info
;; mpc-volume : stuff handling the volume widget
;; mpc-cmd    : mpdlib abstraction

;; UI-commands       : mpc-
;; internal          : mpc--

(eval-when-compile (require 'cl))

(defgroup mpc ()
  "A Client for the Music Player Daemon."
  :prefix "mpc-"
  :group 'multimedia
  :group 'applications)

(defcustom mpc-browser-tags '(Genre Artist|Composer|Performer
                              Album|Playlist)
  "Tags for which a browser buffer should be created by default."
  ;; FIXME: provide a list of tags, for completion.
  :type '(repeat symbol))

;;; Misc utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mpc-assq-all (key alist)
  (let ((res ()) val)
    (dolist (elem alist)
      (if (and (eq (car elem) key)
               (not (member (setq val (cdr elem)) res)))
          (push val res)))
    (nreverse res)))

(defun mpc-union (&rest lists)
  (let ((res (nreverse (pop lists))))
    (dolist (list lists)
      (let ((seen res))           ;Don't remove duplicates within each list.
        (dolist (elem list)
          (unless (member elem seen) (push elem res)))))
    (nreverse res)))

(defun mpc-intersection (l1 l2 &optional selectfun)
  "Return L1 after removing all elements not found in L2.
If SELECTFUN is non-nil, elements aren't compared directly, but instead
they are passed through SELECTFUN before comparison."
  (let ((res ()))
    (if selectfun (setq l2 (mapcar selectfun l2)))
    (dolist (elem l1)
      (when (member (if selectfun (funcall selectfun elem) elem) l2)
        (push elem res)))
    (nreverse res)))

(defun mpc-event-set-point (event)
  (condition-case nil (posn-set-point (event-end event))
    (error (condition-case nil (mouse-set-point event)
             (error nil)))))

(defun mpc-compare-strings (str1 str2 &optional ignore-case)
  "Compare strings STR1 and STR2.
Contrary to `compare-strings', this tries to get numbers sorted
numerically rather than lexicographically."
  (let ((res (compare-strings str1 nil nil str2 nil nil ignore-case)))
    (if (not (integerp res)) res
      (let ((index (1- (abs res))))
        (if (or (>= index (length str1)) (>= index (length str2)))
            res
          (let ((digit1 (memq (aref str1 index)
                              '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
                (digit2 (memq (aref str2 index)
                              '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
            (if digit1
                (if digit2
                    (let ((num1 (progn (string-match "[0-9]+" str1 index)
                                       (match-string 0 str1)))
                          (num2 (progn (string-match "[0-9]+" str2 index)
                                       (match-string 0 str2))))
                      (cond
                       ;; Here we presume that leading zeroes are only used
                       ;; for same-length numbers.  So we'll incorrectly
                       ;; consider that "000" comes after "01", but I don't
                       ;; think it matters.
                       ((< (length num1) (length num2)) (- (abs res)))
                       ((> (length num1) (length num2)) (abs res))
                       ((< (string-to-number num1) (string-to-number num2))
                        (- (abs res)))
                       (t (abs res))))
                  ;; "1a" comes before "10", but "0" comes before "a".
                  (if (and (not (zerop index))
                           (memq (aref str1 (1- index))
                                 '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
                      (abs res)
                    (- (abs res))))
              (if digit2
                  ;; "1a" comes before "10", but "0" comes before "a".
                  (if (and (not (zerop index))
                           (memq (aref str1 (1- index))
                                 '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
                      (- (abs res))
                    (abs res))
                res))))))))

(defun mpc-string-prefix-p (str1 str2)
  ;; FIXME: copied from pcvs-util.el.
  "Tell whether STR1 is a prefix of STR2."
  (eq t (compare-strings str2 nil (length str1) str1 nil nil)))

;; This can speed up mpc--song-search significantly.  The table may grow
;; very large, tho.  It's only bounded by the fact that it gets flushed
;; whenever the connection is established; which seems to work OK thanks
;; to the fact that MPD tends to disconnect fairly often, although our
;; constant polling often prevents disconnection.
(defvar mpc--find-memoize (make-hash-table :test 'equal)) ;; :weakness t
(defvar mpc-tag nil) (make-variable-buffer-local 'mpc-tag)

;;; Support for the actual connection and MPD command execution ;;;;;;;;;;;;

(defcustom mpc-host
  (concat (or (getenv "MPD_HOST") "localhost")
          (if (getenv "MPD_PORT") (concat ":" (getenv "MPD_PORT"))))
  "Host (and port) where the Music Player Daemon is running.
The format is \"HOST\" or \"HOST:PORT\" where PORT defaults to 6600
and HOST defaults to localhost."
  :type 'string)

(defvar mpc-proc nil)

(defconst mpc--proc-end-re "^\\(?:OK\\(?: MPD .*\\)?\\|ACK \\(.*\\)\\)\n")

(put 'mpc-proc-error 'error-conditions '(mpc-proc-error error))
(put 'mpc-proc-error 'error-message "MPD error")

(defun mpc--debug (format &rest args)
  (if (get-buffer "*MPC-debug*")
      (with-current-buffer "*MPC-debug*"
        (goto-char (point-max))
        (insert-before-markers          ;So it scrolls.
         (replace-regexp-in-string "\n" "\n	"
                                   (apply 'format format args))
         "\n"))))

(defun mpc--proc-filter (proc string)
  (mpc--debug "Receive \"%s\"" string)
  (with-current-buffer (process-buffer proc)
    (if (process-get proc 'ready)
        (if nil ;; (string-match "\\`\\(OK\n\\)+\\'" string)
            ;; I haven't figured out yet why I get those extraneous OKs,
            ;; so I'll just ignore them for now.
            nil
          (delete-process proc)
          (set-process-buffer proc nil)
          (pop-to-buffer (clone-buffer))
          (error "MPD output while idle!?"))
      (save-excursion
        (let ((start (or (marker-position (process-mark proc)) (point-min))))
          (goto-char start)
          (insert string)
          (move-marker (process-mark proc) (point))
          (beginning-of-line)
          (when (and (< start (point))
                     (re-search-backward mpc--proc-end-re start t))
            (process-put proc 'ready t)
            (unless (eq (match-end 0) (point-max))
              (error "Unexpected trailing text"))
            (let ((error-text (match-string 1)))
              (delete-region (point) (point-max))
              (let ((callback (process-get proc 'callback)))
                (process-put proc 'callback nil)
                (if error-text
                    (process-put proc 'mpc-proc-error error-text))
                (funcall callback)))))))))

(defun mpc--proc-connect (host)
  (mpc--debug "Connecting to %s..." host)
  (with-current-buffer (get-buffer-create (format " *mpc-%s*" host))
    ;; (pop-to-buffer (current-buffer))
    (let (proc)
      (while (and (setq proc (get-buffer-process (current-buffer)))
                  (progn ;; (debug)
                         (delete-process proc)))))
    (erase-buffer)
    (let ((port 6600))
      (when (string-match ":[^.]+\\'" host)
        (setq port (substring host (1+ (match-beginning 0))))
        (setq host (substring host 0 (match-beginning 0)))
        (unless (string-match "[^[:digit:]]" port)
          (setq port (string-to-number port))))
      (let* ((coding-system-for-read 'utf-8-unix)
             (coding-system-for-write 'utf-8-unix)
             (proc (open-network-stream "MPC" (current-buffer) host port)))
        (when (processp mpc-proc)
          ;; Inherit the properties of the previous connection.
          (let ((plist (process-plist mpc-proc)))
            (while plist (process-put proc (pop plist) (pop plist)))))
        (mpc-proc-buffer proc 'mpd-commands (current-buffer))
        (process-put proc 'callback 'ignore)
        (process-put proc 'ready nil)
        (clrhash mpc--find-memoize)
        (set-process-filter proc 'mpc--proc-filter)
        (set-process-sentinel proc 'ignore)
        (set-process-query-on-exit-flag proc nil)
        ;; This may be called within a process filter ;-(
        (with-local-quit (mpc-proc-sync proc))
        proc))))

(defun mpc--proc-quote-string (s)
  (if (numberp s) (number-to-string s)
    (setq s (replace-regexp-in-string "[\"\\]" "\\\\\\&" s))
    (if (string-match " " s) (concat "\"" s "\"") s)))

(defconst mpc--proc-alist-to-alists-starters '(file directory))

(defun mpc--proc-alist-to-alists (alist)
  (assert (or (null alist)
              (memq (caar alist) mpc--proc-alist-to-alists-starters)))
  (let ((starter (caar alist))
        (alists ())
        tmp)
    (dolist (pair alist)
      (when (eq (car pair) starter)
        (if tmp (push (nreverse tmp) alists))
        (setq tmp ()))
      (push pair tmp))
    (if tmp (push (nreverse tmp) alists))
    (nreverse alists)))

(defun mpc-proc ()
  (or (and mpc-proc
           (buffer-live-p (process-buffer mpc-proc))
           (not (memq (process-status mpc-proc) '(closed)))
           mpc-proc)
      (setq mpc-proc (mpc--proc-connect mpc-host))))

(defun mpc-proc-check (proc)
  (let ((error-text (process-get proc 'mpc-proc-error)))
    (when error-text
      (process-put proc 'mpc-proc-error nil)
      (signal 'mpc-proc-error error-text))))

(defun mpc-proc-sync (&optional proc)
  "Wait for MPC process until it is idle again.
Return the buffer in which the process is/was running."
  (unless proc (setq proc (mpc-proc)))
  (unwind-protect
      (progn
        (while (and (not (process-get proc 'ready))
                    (accept-process-output proc)))
        (mpc-proc-check proc)
        (if (process-get proc 'ready) (process-buffer proc)
          (error "No response from MPD")))
    (unless (process-get proc 'ready)
      ;; (debug)
      (message "Killing hung process")
      (delete-process proc))))

(defun mpc-proc-cmd (cmd &optional callback)
  "Send command CMD to the MPD server.
If CALLBACK is nil, wait for the command to finish before returning,
otherwise return immediately and call CALLBACK with no argument
when the command terminates.
CMD can be a string which is passed as-is to MPD or a list of strings
which will be concatenated with proper quoting before passing them to MPD."
  (let ((proc (mpc-proc)))
    (if (and callback (not (process-get proc 'ready)))
        (let ((old (process-get proc 'callback)))
          (process-put proc 'callback
                       (lambda ()
                         (funcall old)
                         (mpc-proc-cmd cmd callback))))
      ;; Wait for any pending async command to terminate.
      (mpc-proc-sync proc)
      (process-put proc 'ready nil)
      (with-current-buffer (process-buffer proc)
        (erase-buffer)
        (mpc--debug "Send \"%s\"" cmd)
        (process-send-string
         proc (concat (if (stringp cmd) cmd
                        (mapconcat 'mpc--proc-quote-string cmd " "))
                      "\n")))
      (if callback
          ;; (let ((buf (current-buffer)))
          (process-put proc 'callback
                       callback
                       ;; (lambda ()
                       ;;   (funcall callback
                       ;;            (prog1 (current-buffer)
                       ;;              (set-buffer buf)))))
                       )
        ;; If `callback' is nil, we're executing synchronously.
        (process-put proc 'callback 'ignore)
        ;; This returns the process's buffer.
        (mpc-proc-sync proc)))))

;; This function doesn't exist in Emacs-21.
;; (put 'mpc-proc-cmd-list 'byte-optimizer 'byte-optimize-pure-func)
(defun mpc-proc-cmd-list (cmds)
  (concat "command_list_begin\n"
          (mapconcat (lambda (cmd)
                       (if (stringp cmd) cmd
                         (mapconcat 'mpc--proc-quote-string cmd " ")))
                     cmds
                     "\n")
          "\ncommand_list_end"))

(defun mpc-proc-cmd-list-ok ()
  ;; To implement this, we'll need to tweak the process filter since we'd
  ;; then sometimes get "trailing" text after "OK\n".
  (error "Not implemented yet"))

(defun mpc-proc-buf-to-alist (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (let ((res ()))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^:]+\\): \\(.*\\)\n" nil t)
        (push (cons (intern (match-string 1)) (match-string 2)) res))
      (nreverse res))))

(defun mpc-proc-buf-to-alists (buf)
  (mpc--proc-alist-to-alists (mpc-proc-buf-to-alist buf)))

(defun mpc-proc-cmd-to-alist (cmd &optional callback)
  (if callback
      (let ((buf (current-buffer)))
        (mpc-proc-cmd cmd (lambda ()
                            (funcall callback (prog1 (mpc-proc-buf-to-alist
                                                      (current-buffer))
                                                (set-buffer buf))))))
    ;; (lexical-let ((res nil))
    ;;   (mpc-proc-cmd-to-alist cmd (lambda (alist) (setq res alist)))
    ;;   (mpc-proc-sync)
    ;;   res)
    (mpc-proc-buf-to-alist (mpc-proc-cmd cmd))))

(defun mpc-proc-tag-string-to-sym (tag)
  (intern (capitalize tag)))

(defun mpc-proc-buffer (proc use &optional buffer)
  (let* ((bufs (process-get proc 'buffers))
         (buf (cdr (assoc use bufs))))
    (cond
     ((and buffer (buffer-live-p buf) (not (eq buffer buf)))
      (error "Duplicate MPC buffer for %s" use))
     (buffer
      (if buf
          (setcdr (assoc use bufs) buffer)
        (process-put proc 'buffers (cons (cons use buffer) bufs))))
     (t buf))))

;;; Support for regularly updated current status information ;;;;;;;;;;;;;;;

;; Exported elements:
;; `mpc-status' holds the uptodate data.
;; `mpc-status-callbacks' holds the registered callback functions.
;; `mpc-status-refresh' forces a refresh of the data.
;; `mpc-status-stop' stops the automatic updating.

(defvar mpc-status nil)
(defvar mpc-status-callbacks
  '((state  . mpc--status-timers-refresh)
    ;; (song   . mpc--queue-refresh)
    ;; (state  . mpc--queue-refresh)       ;To detect the end of the last song.
    (state  . mpc--faster-toggle-refresh) ;Only ffwd/rewind while play/pause.
    (volume . mpc-volume-refresh)
    (file   . mpc-songpointer-refresh)
    ;; The song pointer may need updating even if the file doesn't change,
    ;; if the same song appears multiple times in a row.
    (song   . mpc-songpointer-refresh)
    (updating_db . mpc-updated-db)
    (updating_db . mpc--status-timers-refresh)
    (t      . mpc-current-refresh))
  "Alist associating properties to the functions that care about them.
Each entry has the form (PROP . FUN) where PROP can be t to mean
to call FUN for any change whatsoever.")

(defun mpc--status-callback ()
  (let ((old-status mpc-status))
    ;; Update the alist.
    (setq mpc-status (mpc-proc-buf-to-alist))
    (assert mpc-status)
    (unless (equal old-status mpc-status)
      ;; Run the relevant refresher functions.
      (dolist (pair mpc-status-callbacks)
        (when (or (eq t (car pair))
                  (not (equal (cdr (assq (car pair) old-status))
                              (cdr (assq (car pair) mpc-status)))))
          (funcall (cdr pair)))))))

(defvar mpc--status-timer nil)
(defun mpc--status-timer-start ()
  (add-hook 'pre-command-hook 'mpc--status-timer-stop)
  (unless mpc--status-timer
    (setq mpc--status-timer (run-with-timer 1 1 'mpc--status-timer-run))))
(defun mpc--status-timer-stop ()
  (when mpc--status-timer
    (cancel-timer mpc--status-timer)
    (setq mpc--status-timer nil)))
(defun mpc--status-timer-run ()
  (when (process-get (mpc-proc) 'ready)
    (condition-case err
        (with-local-quit (mpc-status-refresh))
      (error (message "MPC: %s" err)))))

(defvar mpc--status-idle-timer nil)
(defun mpc--status-idle-timer-start ()
  (when mpc--status-idle-timer
    ;; Turn it off even if we'll start it again, in case it changes the delay.
    (cancel-timer mpc--status-idle-timer))
  (setq mpc--status-idle-timer
        (run-with-idle-timer 1 t 'mpc--status-idle-timer-run))
  ;; Typically, the idle timer is started from the mpc--status-callback,
  ;; which is run asynchronously while we're already idle (we typically
  ;; just started idling), so the timer itself will only be run the next
  ;; time we idle :-(
  ;; To work around that, we immediately start the repeat timer.
  (mpc--status-timer-start))
(defun mpc--status-idle-timer-stop (&optional really)
  (when mpc--status-idle-timer
    ;; Turn it off even if we'll start it again, in case it changes the delay.
    (cancel-timer mpc--status-idle-timer))
  (setq mpc--status-idle-timer
        (unless really
          ;; We don't completely stop the timer, so that if some other MPD
          ;; client starts playback, we may get a chance to notice it.
          (run-with-idle-timer 10 t 'mpc--status-idle-timer-run))))
(defun mpc--status-idle-timer-run ()
  (when (process-get (mpc-proc) 'ready)
    (condition-case err
        (with-local-quit (mpc-status-refresh))
      (error (message "MPC: %s" err))))
  (mpc--status-timer-start))

(defun mpc--status-timers-refresh ()
  "Start/stop the timers according to whether a song is playing."
  (if (or (member (cdr (assq 'state mpc-status)) '("play"))
          (cdr (assq 'updating_db mpc-status)))
      (mpc--status-idle-timer-start)
    (mpc--status-idle-timer-stop)
    (mpc--status-timer-stop)))

(defun mpc-status-refresh (&optional callback)
  "Refresh `mpc-status'."
  (let ((cb callback))
    (mpc-proc-cmd (mpc-proc-cmd-list '("status" "currentsong"))
                  (lambda ()
                    (mpc--status-callback)
                    (if cb (funcall cb))))))

(defun mpc-status-stop ()
  "Stop the autorefresh of `mpc-status'.
This is normally used only when quitting MPC.
Any call to `mpc-status-refresh' may cause it to be restarted."
  (setq mpc-status nil)
  (mpc--status-idle-timer-stop 'really)
  (mpc--status-timer-stop))

;;; A thin layer above the raw protocol commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar mpc-queue nil)
;; (defvar mpc-queue-back nil)

;; (defun mpc--queue-head ()
;;   (if (stringp (car mpc-queue)) (car mpc-queue) (cadar mpc-queue)))
;; (defun mpc--queue-pop ()
;;   (when mpc-queue                       ;Can be nil if out of sync.
;;     (let ((song (car mpc-queue)))
;;       (assert song)
;;       (push (if (and (consp song) (cddr song))
;;                 ;; The queue's first element is itself a list of
;;                 ;; songs, where the first element isn't itself a song
;;                 ;; but a description of the list.
;;                 (prog1 (cadr song) (setcdr song (cddr song)))
;;               (prog1 (if (consp song) (cadr song) song)
;;                 (setq mpc-queue (cdr mpc-queue))))
;;             mpc-queue-back)
;;       (assert (stringp (car mpc-queue-back))))))

;; (defun mpc--queue-refresh ()
;;   ;; Maintain the queue.
;;   (mpc--debug "mpc--queue-refresh")
;;   (let ((pos (cdr (or (assq 'Pos mpc-status) (assq 'song mpc-status)))))
;;     (cond
;;      ((null pos)
;;       (mpc-cmd-clear 'ignore))
;;      ((or (not (member pos '("0" nil)))
;;           ;; There's only one song in the playlist and we've stopped.
;;           ;; Maybe it's because of some external client that set the
;;           ;; playlist like that and/or manually stopped the playback, but
;;           ;; it's more likely that we've simply reached the end of
;;           ;; the song.  So remove it.
;;           (and (equal (assq 'state mpc-status) "stop")
;;                (equal (assq 'playlistlength mpc-status) "1")
;;                (setq pos "1")))
;;       ;; We're not playing the first song in the queue/playlist any
;;       ;; more, so update the queue.
;;       (dotimes (i (string-to-number pos)) (mpc--queue-pop))
;;       (mpc-proc-cmd (mpc-proc-cmd-list
;;                      (make-list (string-to-number pos) "delete 0"))
;;                     'ignore)
;;       (if (not (equal (cdr (assq 'file mpc-status))
;;                       (mpc--queue-head)))
;;           (message "MPC's queue is out of sync"))))))

(defvar mpc--find-memoize-union-tags nil)

(defun mpc-cmd-flush (tag value)
  (puthash (cons tag value) nil mpc--find-memoize)
  (dolist (uniontag mpc--find-memoize-union-tags)
    (if (member (symbol-name tag) (split-string (symbol-name uniontag) "|"))
        (puthash (cons uniontag value) nil mpc--find-memoize))))


(defun mpc-cmd-special-tag-p (tag)
  (or (memq tag '(Playlist Search Directory))
      (string-match "|" (symbol-name tag))))

(defun mpc-cmd-find (tag value)
  "Return a list of all songs whose tag TAG has value VALUE.
The songs are returned as alists."
  (or (gethash (cons tag value) mpc--find-memoize)
      (puthash (cons tag value)
               (cond
                ((eq tag 'Playlist)
                 ;; Special case for pseudo-tag playlist.
                 (let ((l (condition-case nil
                              (mpc-proc-buf-to-alists
                               (mpc-proc-cmd (list "listplaylistinfo" value)))
                            (mpc-proc-error
                             ;; "[50@0] {listplaylistinfo} No such playlist"
                             nil)))
                       (i 0))
                   (mapcar (lambda (s)
                             (prog1 (cons (cons 'Pos (number-to-string i)) s)
                               (incf i)))
                           l)))
                ((eq tag 'Search)
                 (mpc-proc-buf-to-alists
                  (mpc-proc-cmd (list "search" "any" value))))
                ((eq tag 'Directory)
                 (let ((pairs
                        (mpc-proc-buf-to-alist
                         (mpc-proc-cmd (list "listallinfo" value)))))
                   (mpc--proc-alist-to-alists
                    ;; Strip away the `directory' entries.
                    (delq nil (mapcar (lambda (pair)
                                        (if (eq (car pair) 'directory)
                                            nil pair))
                                      pairs)))))
                ((string-match "|" (symbol-name tag))
                 (add-to-list 'mpc--find-memoize-union-tags tag)
                 (let ((tag1 (intern (substring (symbol-name tag)
                                                0 (match-beginning 0))))
                       (tag2 (intern (substring (symbol-name tag)
                                                (match-end 0)))))
                   (mpc-union (mpc-cmd-find tag1 value)
                              (mpc-cmd-find tag2 value))))
                (t
                 (condition-case nil
                     (mpc-proc-buf-to-alists
                      (mpc-proc-cmd (list "find" (symbol-name tag) value)))
                   (mpc-proc-error
                    ;; If `tag' is not one of the expected tags, MPD burps
                    ;; about not having the relevant table.  FIXME: check
                    ;; the kind of error.
                    (error "Unknown tag %s" tag)
                    (let ((res ()))
                      (setq value (cons tag value))
                      (dolist (song (mpc-proc-buf-to-alists
                                     (mpc-proc-cmd "listallinfo")))
                        (if (member value song) (push song res)))
                      res)))))
               mpc--find-memoize)))

(defun mpc-cmd-list (tag &optional other-tag value)
  ;; FIXME: we could also provide a `mpc-cmd-list' alternative which
  ;; doesn't take an "other-tag value" constraint but a "song-list" instead.
  ;; That might be more efficient in some cases.
  (cond
   ((eq tag 'Playlist)
    (let ((pls (mpc-assq-all 'playlist (mpc-proc-cmd-to-alist "lsinfo"))))
      (when other-tag
        (dolist (pl (prog1 pls (setq pls nil)))
          (let ((plsongs (mpc-cmd-find 'Playlist pl)))
            (if (not (mpc-cmd-special-tag-p other-tag))
                (when (member (cons other-tag value)
                              (apply 'append plsongs))
                  (push pl pls))
              ;; Problem N°2: we compute the intersection whereas all
              ;; we care about is whether it's empty.  So we could
              ;; speed this up significantly.
              ;; We only compare file names, because the full song-entries
              ;; are slightly different (the ones in plsongs include
              ;; position and id info specific to the playlist), and it's
              ;; good enough because this is only used with "search", which
              ;; doesn't pay attention to playlists and URLs anyway.
              (let* ((osongs (mpc-cmd-find other-tag value))
                     (ofiles (mpc-assq-all 'file (apply 'append osongs)))
                     (plfiles (mpc-assq-all 'file (apply 'append plsongs))))
                (when (mpc-intersection plfiles ofiles)
                  (push pl pls)))))))
      pls))

   ((eq tag 'Directory)
    (if (null other-tag)
        (apply 'nconc
               (mpc-assq-all 'directory
                             (mpc-proc-buf-to-alist
                              (mpc-proc-cmd "lsinfo")))
               (mapcar (lambda (dir)
                         (let ((shortdir
                                (if (get-text-property 0 'display dir)
                                    (concat "   "
                                            (get-text-property 0 'display dir))
                                  " ↪ "))
                               (subdirs
                                (mpc-assq-all 'directory
                                              (mpc-proc-buf-to-alist
                                               (mpc-proc-cmd (list "lsinfo" dir))))))
                           (dolist (subdir subdirs)
                             (put-text-property 0 (1+ (length dir))
                                                'display shortdir
                                                subdir))
                           subdirs))
                       (process-get (mpc-proc) 'Directory)))
      ;; If there's an other-tag, then just extract the dir info from the
      ;; list of other-tag's songs.
      (let* ((other-songs (mpc-cmd-find other-tag value))
             (files (mpc-assq-all 'file (apply 'append other-songs)))
             (dirs '()))
        (dolist (file files)
          (let ((dir (file-name-directory file)))
            (if (and dir (setq dir (directory-file-name dir))
                     (not (equal dir (car dirs))))
                (push dir dirs))))
        ;; Dirs might have duplicates still.
        (setq dirs (delete-dups dirs))
        (let ((newdirs dirs))
          (while newdirs
            (let ((dir (file-name-directory (pop newdirs))))
              (when (and dir (setq dir (directory-file-name dir))
                         (not (member dir dirs)))
                (push dir newdirs)
                (push dir dirs)))))
        dirs)))

   ;; The UI should not provide access to such a thing anyway currently.
   ;; But I could imagine adding in the future a browser for the "search"
   ;; tag, which would provide things like previous searches.  Not sure how
   ;; useful that would be tho.
   ((eq tag 'Search) (error "Not supported"))

   ((string-match "|" (symbol-name tag))
    (let ((tag1 (intern (substring (symbol-name tag)
                                   0 (match-beginning 0))))
          (tag2 (intern (substring (symbol-name tag)
                                   (match-end 0)))))
      (mpc-union (mpc-cmd-list tag1 other-tag value)
                 (mpc-cmd-list tag2 other-tag value))))

   ((null other-tag)
    (condition-case nil
        (mapcar 'cdr (mpc-proc-cmd-to-alist (list "list" (symbol-name tag))))
      (mpc-proc-error
       ;; If `tag' is not one of the expected tags, MPD burps about not
       ;; having the relevant table.
       ;; FIXME: check the kind of error.
       (error "MPD does not know this tag %s" tag)
       (mpc-assq-all tag (mpc-proc-cmd-to-alist "listallinfo")))))
   (t
    (condition-case nil
        (if (mpc-cmd-special-tag-p other-tag)
            (signal 'mpc-proc-error "Not implemented")
          (mapcar 'cdr
                  (mpc-proc-cmd-to-alist
                   (list "list" (symbol-name tag)
                         (symbol-name other-tag) value))))
      (mpc-proc-error
       ;; DAMN!! the 3-arg form of `list' is new in 0.12 !!
       ;; FIXME: check the kind of error.
       (let ((other-songs (mpc-cmd-find other-tag value)))
         (mpc-assq-all tag
                       ;; Don't use `nconc' now that mpc-cmd-find may
                       ;; return a memoized result.
                       (apply 'append other-songs))))))))

(defun mpc-cmd-stop (&optional callback)
  (mpc-proc-cmd "stop" callback))

(defun mpc-cmd-clear (&optional callback)
  (mpc-proc-cmd "clear" callback)
  ;; (setq mpc-queue-back nil mpc-queue nil)
  )

(defun mpc-cmd-pause (&optional arg callback)
  "Pause or resume playback of the queue of songs."
  (let ((cb callback))
    (mpc-proc-cmd (list "pause" arg)
                  (lambda () (mpc-status-refresh) (if cb (funcall cb))))
    (unless callback (mpc-proc-sync))))

(defun mpc-cmd-status ()
  (mpc-proc-cmd-to-alist "status"))

(defun mpc-cmd-play ()
  (mpc-proc-cmd "play")
  (mpc-status-refresh))

(defun mpc-cmd-add (files &optional playlist)
  "Add the songs FILES to PLAYLIST.
If PLAYLIST is t or nil or missing, use the main playlist."
  (mpc-proc-cmd (mpc-proc-cmd-list
                 (mapcar (lambda (file)
                           (if (stringp playlist)
                               (list "playlistadd" playlist file)
                             (list "add" file)))
                         files)))
    (if (stringp playlist)
        (mpc-cmd-flush 'Playlist playlist)))

(defun mpc-cmd-delete (song-poss &optional playlist)
  "Delete the songs at positions SONG-POSS from PLAYLIST.
If PLAYLIST is t or nil or missing, use the main playlist."
  (mpc-proc-cmd (mpc-proc-cmd-list
                 (mapcar (lambda (song-pos)
                           (if (stringp playlist)
                               (list "playlistdelete" playlist song-pos)
                             (list "delete" song-pos)))
                         ;; Sort them from last to first, so the renumbering
                         ;; caused by the earlier deletions don't affect
                         ;; later ones.
                         (sort song-poss '>))))
    (if (stringp playlist)
        (puthash (cons 'Playlist playlist) nil mpc--find-memoize)))


(defun mpc-cmd-move (song-poss dest-pos &optional playlist)
  (let ((i 0))
    (mpc-proc-cmd
     (mpc-proc-cmd-list
      (mapcar (lambda (song-pos)
                (if (>= song-pos dest-pos)
                    ;; positions past dest-pos have been
                    ;; shifted by i.
                    (setq song-pos (+ song-pos i)))
                (prog1 (if (stringp playlist)
                           (list "playlistmove" playlist song-pos dest-pos)
                         (list "move" song-pos dest-pos))
                  (if (< song-pos dest-pos)
                      ;; This move has shifted dest-pos by 1.
                      (decf dest-pos))
                  (incf i)))
              ;; Sort them from last to first, so the renumbering
              ;; caused by the earlier deletions affect
              ;; later ones a bit less.
              (sort song-poss '>))))
    (if (stringp playlist)
        (puthash (cons 'Playlist playlist) nil mpc--find-memoize))))

(defun mpc-cmd-update (&optional arg callback)
  (let ((cb callback))
    (mpc-proc-cmd (if arg (list "update" arg) "update")
                  (lambda () (mpc-status-refresh) (if cb (funcall cb))))
    (unless callback (mpc-proc-sync))))

(defun mpc-cmd-tagtypes ()
  (mapcar 'cdr (mpc-proc-cmd-to-alist "tagtypes")))

;; This was never integrated into MPD.
;; (defun mpc-cmd-download (file)
;;   (with-current-buffer (generate-new-buffer " *mpc download*")
;;     (set-buffer-multibyte nil)
;;     (let* ((proc (mpc-proc))
;;            (stdbuf (process-buffer proc))
;;            (markpos (marker-position (process-mark proc)))
;;            (stdcoding (process-coding-system proc)))
;;       (unwind-protect
;;           (progn
;;             (set-process-buffer proc (current-buffer))
;;             (set-process-coding-system proc 'binary (cdr stdcoding))
;;             (set-marker (process-mark proc) (point))
;;             (mpc-proc-cmd (list "download" file)))
;;         (set-process-buffer proc stdbuf)
;;         (set-marker (process-mark proc) markpos stdbuf)
;;         (set-process-coding-system proc (car stdcoding) (cdr stdcoding)))
;;       ;; The command has completed, let's decode.
;;       (goto-char (point-max))
;;       (delete-char -1)                    ;Delete final newline.
;;       (while (re-search-backward "^>" nil t)
;;         (delete-char 1))
;;       (current-buffer))))

;;; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom mpc-mpd-music-directory nil
  "Location of MPD's music directory."
  :type '(choice (const nil) directory))

(defcustom mpc-data-directory
  (if (and (not (file-directory-p "~/.mpc"))
           (file-directory-p "~/.emacs.d"))
      "~/.emacs.d/mpc" "~/.mpc")
  "Directory where MPC.el stores auxiliary data."
  :type 'directory)

(defun mpc-data-directory ()
  (unless (file-directory-p mpc-data-directory)
    (make-directory mpc-data-directory))
  mpc-data-directory)

(defun mpc-file-local-copy (file)
  ;; Try to set mpc-mpd-music-directory.
  (when (and (null mpc-mpd-music-directory)
             (string-match "\\`localhost" mpc-host))
    (let ((files '("~/.mpdconf" "/etc/mpd.conf"))
          file)
      (while (and files (not file))
        (if (file-exists-p (car files)) (setq file (car files)))
        (setq files (cdr files)))
      (with-temp-buffer
        (ignore-errors (insert-file-contents file))
        (goto-char (point-min))
        (if (re-search-forward "^music_directory[ 	]+\"\\([^\"]+\\)\"")
            (setq mpc-mpd-music-directory
                  (match-string 1))))))
  ;; Use mpc-mpd-music-directory if applicable, or else try to use the
  ;; `download' command, although it's never been accepted in `mpd' :-(
  (if (and mpc-mpd-music-directory
           (file-exists-p (expand-file-name file mpc-mpd-music-directory)))
      (expand-file-name file mpc-mpd-music-directory)
    ;; (let ((aux (expand-file-name (replace-regexp-in-string "[/]" "|" file)
    ;;                              (mpc-data-directory))))
    ;;   (unless (file-exists-p aux)
    ;;     (condition-case err
    ;;         (with-local-quit
    ;;           (with-current-buffer (mpc-cmd-download file)
    ;;             (write-region (point-min) (point-max) aux)
    ;;             (kill-buffer (current-buffer))))
    ;;       (mpc-proc-error (message "Download error: %s" err) (setq aux nil))))
    ;;   aux)
    ))

;;; Formatter ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mpc-secs-to-time (secs)
  ;; We could use `format-seconds', but it doesn't seem worth the trouble
  ;; because we'd still need to check (>= secs (* 60 100)) since the special
  ;; %z only allows us to drop the large units for small values but
  ;; not to drop the small units for large values.
  (if (stringp secs) (setq secs (string-to-number secs)))
  (if (>= secs (* 60 100))              ;More than 100 minutes.
      (format "%dh%02d" ;"%d:%02d:%02d"
              (/ secs 3600) (% (/ secs 60) 60)) ;; (% secs 60)
    (format "%d:%02d" (/ secs 60) (% secs 60))))

(defvar mpc-tempfiles nil)
(defconst mpc-tempfiles-reftable (make-hash-table :weakness 'key))

(defun mpc-tempfiles-clean ()
  (let ((live ()))
    (maphash (lambda (_k v) (push v live)) mpc-tempfiles-reftable)
    (dolist (f mpc-tempfiles)
      (unless (member f live) (ignore-errors (delete-file f))))
    (setq mpc-tempfiles live)))

(defun mpc-tempfiles-add (key file)
  (mpc-tempfiles-clean)
  (puthash key file mpc-tempfiles-reftable)
  (push file mpc-tempfiles))

(defun mpc-format (format-spec info &optional hscroll)
  "Format the INFO according to FORMAT-SPEC, inserting the result at point."
  (let* ((pos 0)
         (start (point))
         (col (if hscroll (- hscroll) 0))
         (insert (lambda (str)
                   (cond
                    ((>= col 0) (insert str))
                    (t (insert (substring str (min (length str) (- col))))))))
         (pred nil))
    (while (string-match "%\\(?:%\\|\\(-\\)?\\([0-9]+\\)?{\\([[:alpha:]][[:alnum:]]*\\)\\(?:-\\([^}]+\\)\\)?}\\)" format-spec pos)
      (let ((pre-text (substring format-spec pos (match-beginning 0))))
        (funcall insert pre-text)
        (setq col (+ col (string-width pre-text))))
      (setq pos (match-end 0))
      (if (null (match-end 3))
          (progn
            (funcall insert "%")
            (setq col (+ col 1)))
        (let* ((size (match-string 2 format-spec))
               (tag (intern (match-string 3 format-spec)))
               (post (match-string 4 format-spec))
               (right-align (match-end 1))
               (text
                (if (eq info 'self) (symbol-name tag)
                  (case tag
                    ((Time Duration)
                     (let ((time (cdr (or (assq 'time info) (assq 'Time info)))))
                       (setq pred (list nil)) ;Just assume it's never eq.
                       (when time
                         (mpc-secs-to-time (if (and (eq tag 'Duration)
                                                    (string-match ":" time))
                                               (substring time (match-end 0))
                                             time)))))
                    (Cover
                     (let* ((dir (file-name-directory (cdr (assq 'file info))))
                            (cover (concat dir "cover.jpg"))
                            (file (condition-case err
                                      (mpc-file-local-copy cover)
                                    (error (message "MPC: %s" err))))
                            image)
                       ;; (debug)
                       (push `(equal ',dir (file-name-directory (cdr (assq 'file info)))) pred)
                       (if (null file)
                           ;; Make sure we return something on which we can
                           ;; place the `mpc-pred' property, as
                           ;; a negative-cache.  We could also use
                           ;; a default cover.
                           (progn (setq size nil) " ")
                         (if (null size) (setq image (create-image file))
                           (let ((tempfile (make-temp-file "mpc" nil ".jpg")))
                             (call-process "convert" nil nil nil
                                           "-scale" size file tempfile)
                             (setq image (create-image tempfile))
                             (mpc-tempfiles-add image tempfile)))
                         (setq size nil)
                         (propertize dir 'display image))))
                    (t (let ((val (cdr (assq tag info))))
                         ;; For Streaming URLs, there's no other info
                         ;; than the URL in `file'.  Pretend it's in `Title'.
                         (when (and (null val) (eq tag 'Title))
                           (setq val (cdr (assq 'file info))))
                         (push `(equal ',val (cdr (assq ',tag info))) pred)
                         val)))))
               (space (when size
                        (setq size (string-to-number size))
                        (propertize " " 'display
                                    (list 'space :align-to (+ col size)))))
               (textwidth (if text (string-width text) 0))
               (postwidth (if post (string-width post) 0)))
          (when text
            (let ((display
                   (if (and size
                            (> (+ postwidth textwidth) size))
                       ;; This doesn't even obey double-width chars :-(
                       (propertize
                        (if (zerop (- size postwidth 1))
                            (substring text 0 1)
                          (concat (substring text 0 (- size postwidth textwidth 1)) "…"))
                        'help-echo text)
                     text)))
              (when (memq tag '(Artist Album Composer)) ;FIXME: wrong list.
                (setq display
                      (propertize display
                                  'mouse-face 'highlight
                                  'follow-link t
                                  'keymap `(keymap
                                            (mouse-2
                                             . (lambda ()
                                                 (interactive)
                                                 (mpc-constraints-push 'noerror)
                                                 (mpc-constraints-restore
                                                  ',(list (list tag text)))))))))
              (funcall insert
                       (concat (when size
                                 (propertize " " 'display
                                             (list 'space :align-to
                                                   (+ col
                                                      (if (and size right-align)
                                                          (- size postwidth textwidth)
                                                        0)))))
                               display post))))
          (if (null size) (setq col (+ col textwidth postwidth))
            (insert space)
            (setq col (+ col size))))))
    (put-text-property start (point) 'mpc-pred
                       `(lambda (info) (and ,@(nreverse pred))))))

;;; The actual UI code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    ;; (define-key map "\e" 'mpc-stop)
    (define-key map "q" 'mpc-quit)
    (define-key map "\r" 'mpc-select)
    (define-key map [(shift return)] 'mpc-select-toggle)
    (define-key map [mouse-2] 'mpc-select)
    (define-key map [S-mouse-2] 'mpc-select-extend)
    (define-key map [C-mouse-2] 'mpc-select-toggle)
    (define-key map [drag-mouse-2] 'mpc-drag-n-drop)
    ;; We use `always' because a binding to t is like a binding to nil.
    (define-key map [follow-link] 'always)
    ;; Doesn't work because the first click changes the buffer, so the second
    ;; is applied elsewhere :-(
    ;; (define-key map [(double mouse-2)] 'mpc-play-at-point)
    (define-key map "p" 'mpc-pause)
    map))

(easy-menu-define mpc-mode-menu mpc-mode-map
  "Menu for MPC.el."
  '("MPC.el"
    ["Add new browser" mpc-tagbrowser]
    ["Update DB" mpc-update]
    ["Quit" mpc-quit]))

(defvar mpc-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "mpc/prev" 'mpc-prev 'prev map
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop"))
     :label "Prev" :vert-only t)
    ;; FIXME: how can we bind it to the down-event?
    (tool-bar-local-item "mpc/rewind" 'mpc-rewind 'rewind map
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop"))
     :label "Rew" :vert-only t
     :button '(:toggle . (and mpc--faster-toggle-timer
                             (not mpc--faster-toggle-forward))))
    ;; We could use a single toggle command for pause/play, with 2 different
    ;; icons depending on whether or not it's selected, but then it'd have
    ;; to be a toggle-button, thus displayed depressed in one of the
    ;; two states :-(
    (tool-bar-local-item "mpc/pause" 'mpc-pause 'pause map
     :label "Pause" :vert-only t
     :visible '(equal (cdr (assq 'state mpc-status)) "play")
     :help "Pause/play")
    (tool-bar-local-item "mpc/play" 'mpc-play 'play map
     :label "Play" :vert-only t
     :visible '(not (equal (cdr (assq 'state mpc-status)) "play"))
     :help "Play/pause")
    ;; FIXME: how can we bind it to the down-event?
    (tool-bar-local-item "mpc/ffwd" 'mpc-ffwd 'ffwd map
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop"))
     :label "Ffwd" :vert-only t
     :button '(:toggle . (and mpc--faster-toggle-timer
                             mpc--faster-toggle-forward)))
    (tool-bar-local-item "mpc/next" 'mpc-next 'next map
     :label "Next" :vert-only t
     :enable '(not (equal (cdr (assq 'state mpc-status)) "stop")))
    (tool-bar-local-item "mpc/stop" 'mpc-stop 'stop map
     :label "Stop" :vert-only t)
    (tool-bar-local-item "mpc/add" 'mpc-playlist-add 'add map
     :label "Add" :vert-only t
     :help "Append to the playlist")
    map))

(define-derived-mode mpc-mode fundamental-mode "MPC"
  "Major mode for the features common to all buffers of MPC."
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (set (make-local-variable 'tool-bar-map) mpc-tool-bar-map)
  (set (make-local-variable 'truncate-lines) t))

;;; The mpc-status-mode buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode mpc-status-mode mpc-mode "MPC-Status"
  "Major mode to display MPC status info."
  (set (make-local-variable 'mode-line-format)
       '("%e" mode-line-frame-identification mode-line-buffer-identification))
  (set (make-local-variable 'window-area-factor) 3)
  (set (make-local-variable 'header-line-format) '("MPC " mpc-volume)))

(defvar mpc-status-buffer-format
  '("%-5{Time} / %{Duration} %2{Disc--}%4{Track}" "%{Title}" "%{Album}" "%{Artist}" "%128{Cover}"))

(defun mpc-status-buffer-refresh ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'status)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (when (assq 'file mpc-status)
            (let ((inhibit-read-only t))
              (dolist (spec mpc-status-buffer-format)
                (let ((pred (get-text-property (point) 'mpc-pred)))
                  (if (and pred (funcall pred mpc-status))
                      (forward-line)
                    (delete-region (point) (line-beginning-position 2))
                    (ignore-errors (mpc-format spec mpc-status))
                    (insert "\n"))))
              (unless (eobp) (delete-region (point) (point-max))))))))))

(defun mpc-status-buffer-show ()
  (interactive)
  (let* ((buf (mpc-proc-buffer (mpc-proc) 'status))
         (songs-buf (mpc-proc-buffer (mpc-proc) 'songs))
         (songs-win (if songs-buf (get-buffer-window songs-buf 0))))
    (unless (buffer-live-p buf)
      (setq buf (get-buffer-create "*MPC-Status*"))
      (with-current-buffer buf
        (mpc-status-mode))
      (mpc-proc-buffer (mpc-proc) 'status buf))
    (if (null songs-win) (pop-to-buffer buf)
      (let ((_win (split-window songs-win 20 t)))
        (set-window-dedicated-p songs-win nil)
        (set-window-buffer songs-win buf)
        (set-window-dedicated-p songs-win 'soft)))))

;;; Selection management;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-separator-ol nil)

(defvar mpc-select nil)
(make-variable-buffer-local 'mpc-select)

(defmacro mpc-select-save (&rest body)
  "Execute BODY and restore the selection afterwards."
  (declare (indent 0) (debug t))
  `(let ((selection (mpc-select-get-selection))
         (position (cons (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))
                         (current-column))))
     ,@body
     (mpc-select-restore selection)
     (goto-char (point-min))
     (if (re-search-forward
          (concat "^" (regexp-quote (car position)) "$")
          (if (overlayp mpc-separator-ol)
              (overlay-end mpc-separator-ol))
          t)
         (move-to-column (cdr position)))
     (let ((win (get-buffer-window (current-buffer) 0)))
       (if win (set-window-point win (point))))))

(defun mpc-select-get-selection ()
  (mapcar (lambda (ol)
            (buffer-substring-no-properties
             (overlay-start ol) (1- (overlay-end ol))))
          mpc-select))

(defun mpc-select-restore (selection)
  ;; Restore the selection.  I.e. move the overlays back to their
  ;; corresponding location.  Actually which overlay is used for what
  ;; doesn't matter.
  (mapc 'delete-overlay mpc-select)
  (setq mpc-select nil)
  (dolist (elem selection)
    ;; After an update, some elements may have disappeared.
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^" (regexp-quote elem) "$") nil t)
      (mpc-select-make-overlay)))
  (when mpc-tag (mpc-tagbrowser-all-select))
  (beginning-of-line))

(defun mpc-select-make-overlay ()
  (assert (not (get-char-property (point) 'mpc-select)))
  (let ((ol (make-overlay
             (line-beginning-position) (line-beginning-position 2))))
    (overlay-put ol 'mpc-select t)
    (overlay-put ol 'face 'region)
    (overlay-put ol 'evaporate t)
    (push ol mpc-select)))

(defun mpc-select (&optional event)
  "Select the tag value at point."
  (interactive (list last-nonmenu-event))
  (mpc-event-set-point event)
  (if (and (bolp) (eobp)) (forward-line -1))
  (mapc 'delete-overlay mpc-select)
  (setq mpc-select nil)
  (if (mpc-tagbrowser-all-p)
      nil
    (mpc-select-make-overlay))
  (when mpc-tag
    (mpc-tagbrowser-all-select)
    (mpc-selection-refresh)))

(defun mpc-select-toggle (&optional event)
  "Toggle the selection of the tag value at point."
  (interactive (list last-nonmenu-event))
  (mpc-event-set-point event)
  (save-excursion
    (cond
     ;; The line is already selected: deselect it.
     ((get-char-property (point) 'mpc-select)
      (let ((ols nil))
        (dolist (ol mpc-select)
          (if (and (<= (overlay-start ol) (point))
                   (> (overlay-end ol) (point)))
              (delete-overlay ol)
            (push ol ols)))
        (assert (= (1+ (length ols)) (length mpc-select)))
        (setq mpc-select ols)))
     ;; We're trying to select *ALL* additionally to others.
     ((mpc-tagbrowser-all-p) nil)
     ;; Select the current line.
     (t (mpc-select-make-overlay))))
  (when mpc-tag
    (mpc-tagbrowser-all-select)
    (mpc-selection-refresh)))

(defun mpc-select-extend (&optional event)
  "Extend the selection up to point."
  (interactive (list last-nonmenu-event))
  (mpc-event-set-point event)
  (if (null mpc-select)
      ;; If nothing's selected yet, fallback to selecting the elem at point.
      (mpc-select event)
    (save-excursion
      (cond
       ;; The line is already in a selected area; truncate the area.
       ((get-char-property (point) 'mpc-select)
        (let ((before 0)
              (after 0)
              (mid (line-beginning-position))
              start end)
          (while (and (zerop (forward-line 1))
                      (get-char-property (point) 'mpc-select))
            (setq end (1+ (point)))
            (incf after))
          (goto-char mid)
          (while (and (zerop (forward-line -1))
                      (get-char-property (point) 'mpc-select))
            (setq start (point))
            (incf before))
          (if (and (= after 0) (= before 0))
              ;; Shortening an already minimum-size region: do nothing.
              nil
            (if (> after before)
                (setq end mid)
              (setq start (1+ mid)))
            (let ((ols '()))
              (dolist (ol mpc-select)
                (if (and (>= (overlay-start ol) start)
                         (< (overlay-start ol) end))
                    (delete-overlay ol)
                  (push ol ols)))
              (setq mpc-select (nreverse ols))))))
       ;; Extending a prior area.  Look for the closest selection.
       (t
        (when (mpc-tagbrowser-all-p)
          (forward-line 1))
        (let ((before 0)
              (count 0)
              (dir 1)
              (start (line-beginning-position)))
          (while (and (zerop (forward-line 1))
                      (not (get-char-property (point) 'mpc-select)))
            (incf count))
          (unless (get-char-property (point) 'mpc-select)
            (setq count nil))
          (goto-char start)
          (while (and (zerop (forward-line -1))
                      (not (get-char-property (point) 'mpc-select)))
            (incf before))
          (unless (get-char-property (point) 'mpc-select)
            (setq before nil))
          (when (and before (or (null count) (< before count)))
            (setq count before)
            (setq dir -1))
          (goto-char start)
          (dotimes (_i (1+ (or count 0)))
            (mpc-select-make-overlay)
            (forward-line dir))))))
    (when mpc-tag
      (mpc-tagbrowser-all-select)
      (mpc-selection-refresh))))

;;; Constraint sets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc--song-search nil)

(defun mpc-constraints-get-current (&optional avoid-buf)
  "Return currently selected set of constraints.
If AVOID-BUF is non-nil, it specifies a buffer which should be ignored
when constructing the set of constraints."
  (let ((constraints (if mpc--song-search `((Search ,mpc--song-search))))
        tag select)
    (dolist (buf (process-get (mpc-proc) 'buffers))
      (setq buf (cdr buf))
      (when (and (setq tag (buffer-local-value 'mpc-tag buf))
                 (not (eq buf avoid-buf))
                 (setq select
                       (with-current-buffer buf (mpc-select-get-selection))))
        (push (cons tag select) constraints)))
    constraints))

(defun mpc-constraints-tag-lookup (buffer-tag constraints)
  (let (res)
    (dolist (constraint constraints)
      (when (or (eq (car constraint) buffer-tag)
                (and (string-match "|" (symbol-name buffer-tag))
                     (member (symbol-name (car constraint))
                             (split-string (symbol-name buffer-tag) "|"))))
        (setq res (cdr constraint))))
    res))

(defun mpc-constraints-restore (constraints)
  (let ((search (assq 'Search constraints)))
    (setq mpc--song-search (cadr search))
    (when search (setq constraints (delq search constraints))))
  (dolist (buf (process-get (mpc-proc) 'buffers))
    (setq buf (cdr buf))
    (when (buffer-live-p buf)
      (let* ((tag (buffer-local-value 'mpc-tag buf))
             (constraint (mpc-constraints-tag-lookup tag constraints)))
        (when tag
          (with-current-buffer buf
            (mpc-select-restore constraint))))))
  (mpc-selection-refresh))

;; I don't get the ring.el code.  I think it doesn't do what I need, but
;; then I don't understand when what it does would be useful.
(defun mpc-ring-make (size) (cons 0 (cons 0 (make-vector size nil))))
(defun mpc-ring-push (ring val)
  (aset (cddr ring) (car ring) val)
  (setcar (cdr ring) (max (cadr ring) (1+ (car ring))))
  (setcar ring (mod (1+ (car ring)) (length (cddr ring)))))
(defun mpc-ring-pop (ring)
  (setcar ring (mod (1- (car ring)) (cadr ring)))
  (aref (cddr ring) (car ring)))

(defvar mpc-constraints-ring (mpc-ring-make 10))

(defun mpc-constraints-push (&optional noerror)
  "Push the current selection on the ring for later."
  (interactive)
  (let ((constraints (mpc-constraints-get-current)))
    (if (null constraints)
        (unless noerror (error "No selection to push"))
      (mpc-ring-push mpc-constraints-ring constraints))))

(defun mpc-constraints-pop ()
  "Recall the most recently pushed selection."
  (interactive)
  (let ((constraints (mpc-ring-pop mpc-constraints-ring)))
    (if (null constraints)
        (error "No selection to return to")
      (mpc-constraints-restore constraints))))

;;; The TagBrowser mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst mpc-tagbrowser-all-name (propertize "*ALL*" 'face 'italic))
(defvar mpc-tagbrowser-all-ol nil)
(make-variable-buffer-local 'mpc-tagbrowser-all-ol)
(defvar mpc-tag-name nil) (make-variable-buffer-local 'mpc-tag-name)
(defun mpc-tagbrowser-all-p ()
  (and (eq (point-min) (line-beginning-position))
       (equal mpc-tagbrowser-all-name
              (buffer-substring (point-min) (line-end-position)))))

(define-derived-mode mpc-tagbrowser-mode mpc-mode '("MPC-" mpc-tag-name)
  (set (make-local-variable 'mode-line-process) '("" mpc-tag-name))
  (set (make-local-variable 'mode-line-format) nil)
  (set (make-local-variable 'header-line-format) '("" mpc-tag-name ;; "s"
                                                   ))
  (set (make-local-variable 'buffer-undo-list) t)
  )

(defun mpc-tagbrowser-refresh ()
  (mpc-select-save
    (widen)
    (goto-char (point-min))
    (assert (looking-at (regexp-quote mpc-tagbrowser-all-name)))
    (forward-line 1)
    (let ((inhibit-read-only t))
      (delete-region (point) (point-max))
      (dolist (val (mpc-cmd-list mpc-tag)) (insert val "\n")))
    (set-buffer-modified-p nil))
  (mpc-reorder))

(defun mpc-updated-db ()
  ;; FIXME: This is not asynchronous, but is run from a process filter.
  (unless (assq 'updating_db mpc-status)
    (clrhash mpc--find-memoize)
    (dolist (buf (process-get (mpc-proc) 'buffers))
      (setq buf (cdr buf))
      (when (buffer-local-value 'mpc-tag buf)
        (with-current-buffer buf (with-local-quit (mpc-tagbrowser-refresh)))))
    (with-local-quit (mpc-songs-refresh))))

(defun mpc-tagbrowser-tag-name (tag)
  (cond
   ((string-match "|" (symbol-name tag))
    (let ((tag1 (intern (substring (symbol-name tag)
                                   0 (match-beginning 0))))
          (tag2 (intern (substring (symbol-name tag)
                                   (match-end 0)))))
      (concat (mpc-tagbrowser-tag-name tag1)
              " | "
              (mpc-tagbrowser-tag-name tag2))))
   ((string-match "y\\'" (symbol-name tag))
    (concat (substring (symbol-name tag) 0 -1) "ies"))
   (t (concat (symbol-name tag) "s"))))

(defun mpc-tagbrowser-buf (tag)
  (let ((buf (mpc-proc-buffer (mpc-proc) tag)))
    (if (buffer-live-p buf) buf
      (setq buf (get-buffer-create (format "*MPC %ss*" tag)))
      (mpc-proc-buffer (mpc-proc) tag buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if (member tag '(Directory))
              (mpc-tagbrowser-dir-mode)
            (mpc-tagbrowser-mode))
          (insert mpc-tagbrowser-all-name "\n"))
        (forward-line -1)
        (setq mpc-tag tag)
        (setq mpc-tag-name (mpc-tagbrowser-tag-name tag))
        (mpc-tagbrowser-all-select)
        (mpc-tagbrowser-refresh)
        buf))))

(defvar tag-browser-tagtypes
  (lazy-completion-table tag-browser-tagtypes
                         (lambda ()
                           (append '("Playlist" "Directory")
                                   (mpc-cmd-tagtypes)))))

(defun mpc-tagbrowser (tag)
  "Create a new browser for TAG."
  (interactive
   (list
    (let ((completion-ignore-case t))
      (intern
       (completing-read "Tag: " tag-browser-tagtypes nil 'require-match)))))
  (let* ((newbuf (mpc-tagbrowser-buf tag))
         (win (get-buffer-window newbuf 0)))
    (if win (select-window win)
      (if (with-current-buffer (window-buffer (selected-window))
            (derived-mode-p 'mpc-tagbrowser-mode))
          (setq win (selected-window))
        ;; Find a tagbrowser-mode buffer.
        (let ((buffers (process-get (mpc-proc) 'buffers))
              buffer)
          (while
              (and buffers
                   (not (and (buffer-live-p (setq buffer (cdr (pop buffers))))
                             (with-current-buffer buffer
                               (derived-mode-p 'mpc-tagbrowser-mode))
                             (setq win (get-buffer-window buffer 0))))))))
      (if (not win)
          (pop-to-buffer newbuf)
        (setq win (split-window win nil 'horiz))
        (set-window-buffer win newbuf)
        (set-window-dedicated-p win 'soft)
        (select-window win)
        (balance-windows-area)))))

(defun mpc-tagbrowser-all-select ()
  "Select the special *ALL* entry if no other is selected."
  (if mpc-select
      (delete-overlay mpc-tagbrowser-all-ol)
    (save-excursion
      (goto-char (point-min))
      (if mpc-tagbrowser-all-ol
          (move-overlay mpc-tagbrowser-all-ol
                        (point) (line-beginning-position 2))
        (let ((ol (make-overlay (point) (line-beginning-position 2))))
          (overlay-put ol 'face 'region)
          (overlay-put ol 'evaporate t)
          (set (make-local-variable 'mpc-tagbrowser-all-ol) ol))))))

;; (defvar mpc-constraints nil)
(defun mpc-separator (active)
  ;; Place a separator mark.
  (unless mpc-separator-ol
    (set (make-local-variable 'mpc-separator-ol)
         (make-overlay (point) (point)))
    (overlay-put mpc-separator-ol 'after-string
                 (propertize "\n"
                             'face '(:height 0.05 :inverse-video t))))
  (goto-char (point-min))
  (forward-line 1)
  (while
      (and (member (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position))
                   active)
           (zerop (forward-line 1))))
  (if (or (eobp) (null active))
      (delete-overlay mpc-separator-ol)
    (move-overlay mpc-separator-ol (1- (point)) (point))))

(defun mpc-sort (active)
  ;; Sort the active elements at the front.
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    (if (mpc-tagbrowser-all-p) (forward-line 1))
    (condition-case nil
        (sort-subr nil 'forward-line 'end-of-line
                   nil nil
                   (lambda (s1 s2)
                     (setq s1 (buffer-substring-no-properties
                               (car s1) (cdr s1)))
                     (setq s2 (buffer-substring-no-properties
                               (car s2) (cdr s2)))
                     (cond
                      ((member s1 active)
                       (if (member s2 active)
                           (let ((cmp (mpc-compare-strings s1 s2 t)))
                             (and (numberp cmp) (< cmp 0)))
                         t))
                      ((member s2 active) nil)
                      (t (let ((cmp (mpc-compare-strings s1 s2 t)))
                           (and (numberp cmp) (< cmp 0)))))))
      ;; The comparison predicate arg is new in Emacs-22.
      (wrong-number-of-arguments
        (sort-subr nil 'forward-line 'end-of-line
                   (lambda ()
                     (let ((name (buffer-substring-no-properties
                                  (point) (line-end-position))))
                       (cond
                        ((member name active) (concat "1" name))
                        (t (concat "2" "name"))))))))))

(defvar mpc--changed-selection)

(defun mpc-reorder (&optional nodeactivate)
  "Reorder entries based on the currently active selections.
I.e. split the current browser buffer into a first part containing the
entries included in the selection, then a separator, and then the entries
not included in the selection.
Return non-nil if a selection was deactivated."
  (mpc-select-save
    (let ((constraints (mpc-constraints-get-current (current-buffer)))
          (active 'all))
      ;; (unless (equal constraints mpc-constraints)
      ;;   (set (make-local-variable 'mpc-constraints) constraints)
      (dolist (cst constraints)
        (let ((vals (apply 'mpc-union
                           (mapcar (lambda (val)
                                     (mpc-cmd-list mpc-tag (car cst) val))
                                   (cdr cst)))))
          (setq active
                (if (listp active) (mpc-intersection active vals) vals))))

      (when (and (listp active))
        ;; Remove the selections if they are all in conflict with
        ;; other constraints.
        (let ((deactivate t))
          (dolist (sel selection)
            (when (member sel active) (setq deactivate nil)))
          (when deactivate
            ;; Variable declared/used by `mpc-select-save'.
            (when selection
              (setq mpc--changed-selection t))
            (unless nodeactivate
              (setq selection nil)
              (mapc 'delete-overlay mpc-select)
              (setq mpc-select nil)
              (mpc-tagbrowser-all-select)))))

      ;; FIXME: This `mpc-sort' takes a lot of time.  Maybe we should
      ;; be more clever and presume the buffer is mostly sorted already.
      (mpc-sort (if (listp active) active))
      (mpc-separator (if (listp active) active)))))

(defun mpc-selection-refresh ()
  (let ((mpc--changed-selection t))
    (while mpc--changed-selection
      (setq mpc--changed-selection nil)
      (dolist (buf (process-get (mpc-proc) 'buffers))
              (setq buf (cdr buf))
              (when (and (buffer-local-value 'mpc-tag buf)
                         (not (eq buf (current-buffer))))
                (with-current-buffer buf (mpc-reorder)))))
    ;; FIXME: reorder the current buffer last and prevent deactivation,
    ;; since whatever selection we made here is the most recent one
    ;; and should hence take precedence.
    (when mpc-tag (mpc-reorder 'nodeactivate))
    ;; FIXME: comment?
    (if (and mpc--song-search mpc--changed-selection)
        (progn
          (setq mpc--song-search nil)
          (mpc-selection-refresh))
      (mpc-songs-refresh))))

;;; Hierarchical tagbrowser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo:
;; - Add a button on each dir to open/close it (?)
;; - add the parent dir on the previous line, grayed-out, if it's not
;;   present (because we're in the non-selected part and the parent is
;;   in the selected part).

(defvar mpc-tagbrowser-dir-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mpc-tagbrowser-mode-map)
    (define-key map [?\M-\C-m] 'mpc-tagbrowser-dir-toggle)
    map))

;; (defvar mpc-tagbrowser-dir-keywords
;;   '(mpc-tagbrowser-dir-hide-prefix))

(define-derived-mode mpc-tagbrowser-dir-mode mpc-tagbrowser-mode '("MPC-" mpc-tag-name)
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      '(mpc-tagbrowser-dir-keywords t))
  )

;; (defun mpc-tagbrowser-dir-hide-prefix (limit)
;;   (while
;;       (let ((prev (buffer-substring (line-beginning-position 0)
;;                                     (line-end-position 0))))
;;         (

(defun mpc-tagbrowser-dir-toggle (event)
  "Open or close the element at point."
  (interactive (list last-nonmenu-event))
  (mpc-event-set-point event)
  (let ((name (buffer-substring (line-beginning-position)
                                (line-end-position)))
        (prop (intern mpc-tag)))
    (if (not (member name (process-get (mpc-proc) prop)))
        (process-put (mpc-proc) prop
                     (cons name (process-get (mpc-proc) prop)))
      (let ((new (delete name (process-get (mpc-proc) prop))))
        (setq name (concat name "/"))
        (process-put (mpc-proc) prop
                     (delq nil
                           (mapcar (lambda (x)
                                     (if (mpc-string-prefix-p name x)
                                         nil x))
                                   new)))))
    (mpc-tagbrowser-refresh)))


;;; Playlist management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-songs-playlist nil
  "Name of the currently selected playlist, if any.
A value of t means the main playlist.")
(make-variable-buffer-local 'mpc-songs-playlist)

(defun mpc-playlist-create (name)
  "Save current playlist under name NAME."
  (interactive "sPlaylist name: ")
  (mpc-proc-cmd (list "save" name))
  (let ((buf (mpc-proc-buffer (mpc-proc) 'Playlist)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (mpc-tagbrowser-refresh)))))

(defun mpc-playlist-destroy (name)
  "Delete playlist named NAME."
  (interactive
   (list (completing-read "Delete playlist: " (mpc-cmd-list 'Playlist)
                          nil 'require-match)))
  (mpc-proc-cmd (list "rm" name))
  (let ((buf (mpc-proc-buffer (mpc-proc) 'Playlist)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (mpc-tagbrowser-refresh)))))

(defun mpc-playlist-rename (oldname newname)
  "Rename playlist OLDNAME to NEWNAME."
  (interactive
   (let* ((oldname (if (and (eq mpc-tag 'Playlist) (null current-prefix-arg))
                       (buffer-substring (line-beginning-position)
                                         (line-end-position))
                     (completing-read "Rename playlist: "
                                      (mpc-cmd-list 'Playlist)
                                      nil 'require-match)))
          (newname (read-string (format "Rename '%s' to: " oldname))))
     (if (zerop (length newname))
         (error "Aborted")
       (list oldname newname))))
  (mpc-proc-cmd (list "rename" oldname newname))
  (let ((buf (mpc-proc-buffer (mpc-proc) 'Playlist)))
    (if (buffer-live-p buf)
        (with-current-buffer buf (mpc-tagbrowser-refresh)))))

(defun mpc-playlist ()
  "Show the current playlist."
  (interactive)
  (mpc-constraints-push 'noerror)
  (mpc-constraints-restore '()))

(defun mpc-playlist-add ()
  "Add the selection to the playlist."
  (interactive)
  (let ((songs (mapcar #'car (mpc-songs-selection))))
    (mpc-cmd-add songs)
    (message "Appended %d songs" (length songs))
    ;; Return the songs added.  Used in `mpc-play'.
    songs))

(defun mpc-playlist-delete ()
  "Remove the selected songs from the playlist."
  (interactive)
  (unless mpc-songs-playlist
    (error "The selected songs aren't part of a playlist"))
  (let ((song-poss (mapcar #'cdr (mpc-songs-selection))))
    (mpc-cmd-delete song-poss mpc-songs-playlist)
    (mpc-songs-refresh)
    (message "Deleted %d songs" (length song-poss))))

;;; Volume management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-volume-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'mpc-volume-mouse-set)
    (define-key map [mouse-1] 'ignore)
    (define-key map [header-line down-mouse-1] 'mpc-volume-mouse-set)
    (define-key map [header-line mouse-1] 'ignore)
    (define-key map [mode-line down-mouse-1] 'mpc-volume-mouse-set)
    (define-key map [mode-line mouse-1] 'ignore)
    map))

(defvar mpc-volume nil) (put 'mpc-volume 'risky-local-variable t)

(defun mpc-volume-refresh ()
  ;; Maintain the volume.
  (setq mpc-volume
        (mpc-volume-widget
         (string-to-number (cdr (assq 'volume mpc-status))))))

(defvar mpc-volume-step 5)

(defun mpc-volume-mouse-set (&optional event)
  "Change volume setting."
  (interactive (list last-nonmenu-event))
  (let* ((posn (event-start event))
         (diff
          (if (memq (if (stringp (car-safe (posn-object posn)))
                        (aref (car (posn-object posn)) (cdr (posn-object posn)))
                      (with-current-buffer (window-buffer (posn-window posn))
                        (char-after (posn-point posn))))
                    '(?◁ ?<))
              (- mpc-volume-step) mpc-volume-step))
         (newvol (+ (string-to-number (cdr (assq 'volume mpc-status))) diff)))
    (mpc-proc-cmd (list "setvol" newvol) 'mpc-status-refresh)
    (message "Set MPD volume to %s%%" newvol)))

(defun mpc-volume-widget (vol &optional size)
  (unless size (setq size 12.5))
  (let ((scaledvol (* (/ vol 100.0) size)))
    ;; (message "Volume sizes: %s - %s" (/ vol fact) (/ (- 100 vol) fact))
    (list (propertize "<" ;; "◁"
                      ;; 'face 'default
                      'keymap mpc-volume-map
                      'face '(:box (:line-width -2 :style pressed-button))
                      'mouse-face '(:box (:line-width -2 :style released-button)))
          " "
          (propertize "a"
                      'display (list 'space :width scaledvol)
                      'face '(:inverse-video t
                              :box (:line-width -2 :style released-button)))
          (propertize "a"
                      'display (list 'space :width (- size scaledvol))
                      'face '(:box (:line-width -2 :style released-button)))
          " "
          (propertize ">" ;; "▷"
                      ;; 'face 'default
                      'keymap mpc-volume-map
                      'face '(:box (:line-width -2 :style pressed-button))
                      'mouse-face '(:box (:line-width -2 :style released-button))))))

;;; MPC songs mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mpc-current-song nil) (put 'mpc-current-song 'risky-local-variable t)
(defvar mpc-current-updating nil) (put 'mpc-current-updating 'risky-local-variable t)
(defvar mpc-songs-format-description nil) (put 'mpc-songs-format-description 'risky-local-variable t)

(defvar mpc-previous-window-config nil)

(defvar mpc-songs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map mpc-mode-map)
    (define-key map [remap mpc-select] 'mpc-songs-jump-to)
    map))

(defvar mpc-songpointer-set-visible nil)

(defvar mpc-songs-hashcons (make-hash-table :test 'equal :weakness t)
  "Make song file name objects unique via hash consing.
This is used so that they can be compared with `eq', which is needed for
`text-property-any'.")
(defun mpc-songs-hashcons (name)
  (or (gethash name mpc-songs-hashcons) (puthash name name mpc-songs-hashcons)))
(defcustom mpc-songs-format "%2{Disc--}%3{Track} %-5{Time} %25{Title} %20{Album} %20{Artist} %10{Date}"
  "Format used to display each song in the list of songs."
  :type 'string)

(defvar mpc-songs-totaltime)

(defun mpc-songs-refresh ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'songs)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((constraints (mpc-constraints-get-current (current-buffer)))
              (dontsort nil)
              (inhibit-read-only t)
              (totaltime 0)
              (curline (cons (count-lines (point-min)
                                          (line-beginning-position))
                             (buffer-substring (line-beginning-position)
                                               (line-end-position))))
              active)
          (setq mpc-songs-playlist nil)
          (if (null constraints)
              ;; When there are no constraints, rather than show the list of
              ;; all songs (which could take a while to download and
              ;; format), we show the current playlist.
              ;; FIXME: it would be good to be able to show the complete
              ;; list, but that would probably require us to format it
              ;; on-the-fly to make it bearable.
              (setq dontsort t
                    mpc-songs-playlist t
                    active (mpc-proc-buf-to-alists
                            (mpc-proc-cmd "playlistinfo")))
            (dolist (cst constraints)
              (if (and (eq (car cst) 'Playlist)
                       (= 1 (length (cdr cst))))
                  (setq mpc-songs-playlist (cadr cst)))
              ;; We don't do anything really special here for playlists,
              ;; because it's unclear what's a correct "union" of playlists.
              (let ((vals (apply 'mpc-union
                                 (mapcar (lambda (val)
                                           (mpc-cmd-find (car cst) val))
                                         (cdr cst)))))
                (setq active (cond
                              ((null active)
                                   (if (eq (car cst) 'Playlist)
                                       (setq dontsort t))
                                   vals)
                              ((or dontsort
                                       ;; Try to preserve ordering and
                                       ;; repetitions from playlists.
                                       (not (eq (car cst) 'Playlist)))
                                   (mpc-intersection active vals
                                                 (lambda (x) (assq 'file x))))
                              (t
                                 (setq dontsort t)
                                 (mpc-intersection vals active
                                                 (lambda (x)
                                                   (assq 'file x)))))))))
          (mpc-select-save
            (erase-buffer)
            ;; Sorting songs is surprisingly difficult: when comparing two
            ;; songs with the same album name but different artist name, you
            ;; have to know whether these are two different albums (with the
            ;; same name) or a single album (typically a compilation).
            ;; I punt on it and just use file-name sorting, which does the
            ;; right thing if your library is properly arranged.
            (dolist (song (if dontsort active
                            (sort active
                                  (lambda (song1 song2)
                                    (let ((cmp (mpc-compare-strings
                                                (cdr (assq 'file song1))
                                                (cdr (assq 'file song2)))))
                                      (and (integerp cmp) (< cmp 0)))))))
              (incf totaltime (string-to-number (or (cdr (assq 'Time song)) "0")))
              (mpc-format mpc-songs-format song)
              (delete-char (- (skip-chars-backward " "))) ;Remove trailing space.
              (insert "\n")
              (put-text-property
               (line-beginning-position 0) (line-beginning-position)
               'mpc-file (mpc-songs-hashcons (cdr (assq 'file song))))
              (let ((pos (assq 'Pos song)))
                (if pos
                    (put-text-property
                     (line-beginning-position 0) (line-beginning-position)
                     'mpc-file-pos (string-to-number (cdr pos)))))
              ))
          (goto-char (point-min))
          (forward-line (car curline))
          (if (or (search-forward (cdr curline) nil t)
                    (search-backward (cdr curline) nil t))
              (beginning-of-line)
            (goto-char (point-min)))
          (set (make-local-variable 'mpc-songs-totaltime)
               (unless (zerop totaltime)
                 (list " " (mpc-secs-to-time totaltime))))
          ))))
  (let ((mpc-songpointer-set-visible t))
    (mpc-songpointer-refresh)))

(defun mpc-songs-search (string)
  "Filter songs to those who include STRING in their metadata."
  (interactive "sSearch for: ")
  (setq mpc--song-search
        (if (zerop (length string)) nil string))
  (let ((mpc--changed-selection t))
    (while mpc--changed-selection
      (setq mpc--changed-selection nil)
      (dolist (buf (process-get (mpc-proc) 'buffers))
        (setq buf (cdr buf))
        (when (buffer-local-value 'mpc-tag buf)
          (with-current-buffer buf (mpc-reorder))))
      (mpc-songs-refresh))))

(defun mpc-songs-kill-search ()
  "Turn off the current search restriction."
  (interactive)
  (mpc-songs-search nil))

(defun mpc-songs-selection ()
  "Return the list of songs currently selected."
  (let ((buf (mpc-proc-buffer (mpc-proc) 'songs)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (save-excursion
          (let ((files ()))
            (if mpc-select
                (dolist (ol mpc-select)
                  (push (cons
                         (get-text-property (overlay-start ol) 'mpc-file)
                         (get-text-property (overlay-start ol) 'mpc-file-pos))
                        files))
              (goto-char (point-min))
              (while (not (eobp))
                (push (cons
                       (get-text-property (point) 'mpc-file)
                       (get-text-property (point) 'mpc-file-pos))
                      files)
                (forward-line 1)))
            (nreverse files)))))))

(defun mpc-songs-jump-to (song-file &optional posn)
  "Jump to song SONG-FILE; interactively, this is the song at point."
  (interactive
   (let* ((event last-nonmenu-event)
          (posn (event-end event)))
     (with-selected-window (posn-window posn)
       (goto-char (posn-point posn))
       (list (get-text-property (point) 'mpc-file)
             posn))))
  (let* ((plbuf (mpc-proc-cmd "playlist"))
         (re (if song-file
		 (concat "^\\([0-9]+\\):" (regexp-quote song-file) "$")))
         (sn (with-current-buffer plbuf
               (goto-char (point-min))
               (when (and re (re-search-forward re nil t))
                 (match-string 1)))))
    (cond
     ((null re) (posn-set-point posn))
     ((null sn) (error "This song is not in the playlist"))
     ((null (with-current-buffer plbuf (re-search-forward re nil t)))
      ;; song-file only appears once in the playlist: no ambiguity,
      ;; we're good to go!
      (mpc-proc-cmd (list "play" sn)))
     (t
      ;; The song appears multiple times in the playlist.  If the current
      ;; buffer holds not only the destination song but also the current
      ;; song, then we will move in the playlist to the same relative
      ;; position as in the buffer.  Otherwise, we will simply choose the
      ;; song occurrence closest to the current song.
      (with-selected-window (posn-window posn)
        (let* ((cur (and (markerp overlay-arrow-position)
                         (marker-position overlay-arrow-position)))
               (dest (save-excursion
                       (goto-char (posn-point posn))
                       (line-beginning-position)))
               (lines (when cur (* (if (< cur dest) 1 -1)
                                   (count-lines cur dest)))))
          (with-current-buffer plbuf
            (goto-char (point-min))
            ;; Start the search from the current song.
            (forward-line (string-to-number
                           (or (cdr (assq 'song mpc-status)) "0")))
            ;; If the current song is also displayed in the buffer,
            ;; then try to move to the same relative position.
            (if lines (forward-line lines))
            ;; Now search the closest occurrence.
            (let* ((next (save-excursion
                           (when (re-search-forward re nil t)
                             (cons (point) (match-string 1)))))
                   (prev (save-excursion
                           (when (re-search-backward re nil t)
                             (cons (point) (match-string 1)))))
                   (sn (cdr (if (and next prev)
                                (if (< (- (car next) (point))
                                       (- (point) (car prev)))
                                    next prev)
                              (or next prev)))))
              (assert sn)
              (mpc-proc-cmd (concat "play " sn))))))))))

(define-derived-mode mpc-songs-mode mpc-mode "MPC-song"
  (setq mpc-songs-format-description
        (with-temp-buffer (mpc-format mpc-songs-format 'self) (buffer-string)))
  (set (make-local-variable 'header-line-format)
       ;; '("MPC " mpc-volume " " mpc-current-song)
       (list (propertize " " 'display '(space :align-to 0))
             ;; 'mpc-songs-format-description
             '(:eval
               (let ((hscroll (window-hscroll)))
                 (with-temp-buffer
                   (mpc-format mpc-songs-format 'self hscroll)
                   ;; That would be simpler than the hscroll handling in
                   ;; mpc-format, but currently move-to-column does not
                   ;; recognize :space display properties.
                   ;; (move-to-column hscroll)
                   ;; (delete-region (point-min) (point))
                   (buffer-string))))))
  (set (make-local-variable 'mode-line-format)
       '("%e" mode-line-frame-identification mode-line-buffer-identification
         #("   " 0 3
           (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
         mode-line-position
         #("  " 0 2
           (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
         mpc-songs-totaltime
         mpc-current-updating
         #("   " 0 2
           (help-echo "mouse-1: Select (drag to resize)\nmouse-2: Make current window occupy the whole frame\nmouse-3: Remove current window from display"))
         (mpc--song-search
          (:propertize
           ("Search=\"" mpc--song-search "\"")
           help-echo "mouse-2: kill this search"
           follow-link t
           mouse-face mode-line-highlight
           keymap (keymap (mode-line keymap
                                     (mouse-2 . mpc-songs-kill-search))))
          (:propertize "NoSearch"
           help-echo "mouse-2: set a search restriction"
           follow-link t
           mouse-face mode-line-highlight
           keymap (keymap (mode-line keymap (mouse-2 . mpc-songs-search)))))))

  ;; (set (make-local-variable 'mode-line-process)
  ;;      '("" ;; mpc-volume " "
  ;;        mpc-songs-totaltime
  ;;        mpc-current-updating))
  )

(defun mpc-songpointer-set (pos)
  (let* ((win (get-buffer-window (current-buffer) t))
         (visible (when win
                    (or mpc-songpointer-set-visible
                        (and (markerp overlay-arrow-position)
                             (eq (marker-buffer overlay-arrow-position)
                                 (current-buffer))
                             (<= (window-start win) overlay-arrow-position)
                             (< overlay-arrow-position (window-end win)))))))
    (unless (local-variable-p 'overlay-arrow-position)
      (set (make-local-variable 'overlay-arrow-position) (make-marker)))
    (move-marker overlay-arrow-position pos)
    ;; If the arrow was visible, try to keep it that way.
    (if (and visible pos
             (or (> (window-start win) pos) (>= pos (window-end win t))))
      (set-window-point win pos))))

(defun mpc-songpointer-refresh ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'songs)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((pos (text-property-any
                     (point-min) (point-max)
                     'mpc-file (mpc-songs-hashcons
                                (cdr (assq 'file mpc-status)))))
               (other (when pos
                        (save-excursion
                          (goto-char pos)
                          (text-property-any
                           (line-beginning-position 2) (point-max)
                           'mpc-file (mpc-songs-hashcons
                                      (cdr (assq 'file mpc-status))))))))
          (if other
              ;; The song appears multiple times in the buffer.
              ;; We need to be careful to choose the right occurrence.
              (mpc-proc-cmd "playlist" 'mpc-songpointer-refresh-hairy)
            (mpc-songpointer-set pos)))))))

(defun mpc-songpointer-context (size plbuf)
  (with-current-buffer plbuf
    (goto-char (point-min))
    (forward-line (string-to-number (or (cdr (assq 'song mpc-status)) "0")))
    (let ((context-before '())
          (context-after '()))
      (save-excursion
        (dotimes (_i size)
          (when (re-search-backward "^[0-9]+:\\(.*\\)" nil t)
            (push (mpc-songs-hashcons (match-string 1)) context-before))))
      ;; Skip the actual current song.
      (forward-line 1)
      (dotimes (_i size)
        (when (re-search-forward "^[0-9]+:\\(.*\\)" nil t)
          (push (mpc-songs-hashcons (match-string 1)) context-after)))
      ;; If there isn't `size' context, then return nil.
      (unless (and (< (length context-before) size)
                   (< (length context-after) size))
        (cons (nreverse context-before) (nreverse context-after))))))

(defun mpc-songpointer-score (context pos)
  (let ((count 0))
    (goto-char pos)
    (dolist (song (car context))
      (and (zerop (forward-line -1))
           (eq (get-text-property (point) 'mpc-file) song)
           (incf count)))
    (goto-char pos)
    (dolist (song (cdr context))
      (and (zerop (forward-line 1))
           (eq (get-text-property (point) 'mpc-file) song)
           (incf count)))
    count))

(defun mpc-songpointer-refresh-hairy ()
  ;; Based on the complete playlist, we should figure out where in the
  ;; song buffer is the currently playing song.
  (let ((plbuf (current-buffer))
        (buf (mpc-proc-buffer (mpc-proc) 'songs)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((context-size 0)
               (context '(() . ()))
               (pos (text-property-any
                     (point-min) (point-max)
                     'mpc-file (mpc-songs-hashcons
                                (cdr (assq 'file mpc-status)))))
               (score 0)
               (other pos))
          (while
              (setq other
                    (save-excursion
                      (goto-char other)
                      (text-property-any
                       (line-beginning-position 2) (point-max)
                       'mpc-file (mpc-songs-hashcons
                                  (cdr (assq 'file mpc-status))))))
            ;; There is an `other' contestant.
            (let ((other-score (mpc-songpointer-score context other)))
              (cond
               ;; `other' is worse: try the next one.
               ((< other-score score) nil)
               ;; `other' is better: remember it and then search further.
               ((> other-score score)
                (setq pos other)
                (setq score other-score))
               ;; Both are equal and increasing the context size won't help.
               ;; Arbitrarily choose one of the two and keep looking
               ;; for a better match.
               ((< score context-size) nil)
               (t
                ;; Score is equal and increasing context might help: try it.
                (incf context-size)
                (let ((new-context
                       (mpc-songpointer-context context-size plbuf)))
                  (if (null new-context)
                      ;; There isn't more context: choose one arbitrarily
                      ;; and keep looking for a better match elsewhere.
                      (decf context-size)
                    (setq context new-context)
                    (setq score (mpc-songpointer-score context pos))
                    (save-excursion
                      (goto-char other)
                      ;; Go back one line so we find `other' again.
                      (setq other (line-beginning-position 0)))))))))
          (mpc-songpointer-set pos))))))

(defun mpc-current-refresh ()
  ;; Maintain the current data.
  (mpc-status-buffer-refresh)
  (setq mpc-current-updating
        (if (assq 'updating_db mpc-status) " Updating-DB"))
  (ignore-errors
    (setq mpc-current-song
          (when (assq 'file mpc-status)
            (concat " "
                    (mpc-secs-to-time (cdr (assq 'time mpc-status)))
                    " "
                    (cdr (assq 'Title mpc-status))
                    " ("
                    (cdr (assq 'Artist mpc-status))
                    " / "
                    (cdr (assq 'Album mpc-status))
                    ")"))))
  (force-mode-line-update t))

(defun mpc-songs-buf ()
  (let ((buf (mpc-proc-buffer (mpc-proc) 'songs)))
    (if (buffer-live-p buf) buf
      (with-current-buffer (setq buf (get-buffer-create "*MPC-Songs*"))
        (mpc-proc-buffer (mpc-proc) 'songs buf)
        (mpc-songs-mode)
        buf))))

(defun mpc-update ()
  "Tell MPD to refresh its database."
  (interactive)
  (mpc-cmd-update))

(defun mpc-quit ()
  "Quit Music Player Daemon."
  (interactive)
  (let* ((proc mpc-proc)
         (bufs (mapcar 'cdr (if proc (process-get proc 'buffers))))
         (wins (mapcar (lambda (buf) (get-buffer-window buf 0)) bufs))
         (song-buf (mpc-songs-buf))
         frames)
    ;; Collect all the frames where MPC buffers appear.
    (dolist (win wins)
      (when (and win (not (memq (window-frame win) frames)))
        (push (window-frame win) frames)))
    (if (and frames song-buf
             (with-current-buffer song-buf mpc-previous-window-config))
        (progn
          (select-frame (car frames))
          (set-window-configuration
           (with-current-buffer song-buf mpc-previous-window-config)))
      ;; Now delete the ones that show nothing else than MPC buffers.
      (dolist (frame frames)
        (let ((delete t))
          (dolist (win (window-list frame))
            (unless (memq (window-buffer win) bufs) (setq delete nil)))
          (if delete (ignore-errors (delete-frame frame))))))
    ;; Then kill the buffers.
    (mapc 'kill-buffer bufs)
    (mpc-status-stop)
    (if proc (delete-process proc))))

(defun mpc-stop ()
  "Stop playing the current queue of songs."
  (interactive)
  (mpc-cmd-stop)
  (mpc-cmd-clear)
  (mpc-status-refresh))

(defun mpc-pause ()
  "Pause playing."
  (interactive)
  (mpc-cmd-pause "1"))

(defun mpc-resume ()
  "Resume playing."
  (interactive)
  (mpc-cmd-pause "0"))

(defun mpc-play ()
  "Start playing whatever is selected."
  (interactive)
  (if (member (cdr (assq 'state (mpc-cmd-status))) '("pause"))
      (mpc-resume)
    ;; When playing the playlist ends, the playlist isn't cleared, but the
    ;; user probably doesn't want to re-listen to it before getting to
    ;; listen to what he just selected.
    ;; (if (member (cdr (assq 'state (mpc-cmd-status))) '("stop"))
    ;;     (mpc-cmd-clear))
    ;; Actually, we don't use mpc-play to append to the playlist any more,
    ;; so we can just always empty the playlist.
    (mpc-cmd-clear)
    (if (mpc-playlist-add)
        (if (member (cdr (assq 'state (mpc-cmd-status))) '("stop"))
            (mpc-cmd-play))
      (error "Don't know what to play"))))

(defun mpc-next ()
  "Jump to the next song in the queue."
  (interactive)
  (mpc-proc-cmd "next")
  (mpc-status-refresh))

(defun mpc-prev ()
  "Jump to the beginning of the current song, or to the previous song."
  (interactive)
  (let ((time (cdr (assq 'time mpc-status))))
    ;; Here we rely on the fact that string-to-number silently ignores
    ;; everything after a non-digit char.
    (cond
     ;; Go back to the beginning of current song.
     ((and time (> (string-to-number time) 0))
      (mpc-proc-cmd (list "seekid" (cdr (assq 'songid mpc-status)) 0)))
     ;; We're at the beginning of the first song of the playlist.
     ;; Fetch the previous one from `mpc-queue-back'.
     ;; ((and (zerop (string-to-number (cdr (assq 'song mpc-status))))
     ;;       mpc-queue-back)
     ;;  ;; Because we use cmd-list rather than cmd-play, the queue is not
     ;;  ;; automatically updated.
     ;;  (let ((prev (pop mpc-queue-back)))
     ;;    (push prev mpc-queue)
     ;;    (mpc-proc-cmd
     ;;     (mpc-proc-cmd-list
     ;;      (list (list "add" prev)
     ;;            (list "move" (cdr (assq 'playlistlength mpc-status)) "0")
     ;;            "previous")))))
     ;; We're at the beginning of a song, but not the first one.
     (t (mpc-proc-cmd "previous")))
    (mpc-status-refresh)))

(defvar mpc-last-seek-time '(0 . 0))

(defun mpc--faster (event speedup step)
  "Fast forward."
  (interactive (list last-nonmenu-event))
  (let ((repeat-delay (/ (abs (float step)) speedup)))
    (if (not (memq 'down (event-modifiers event)))
        (let* ((currenttime (float-time))
               (last-time (- currenttime (car mpc-last-seek-time))))
          (if (< last-time (* 0.9 repeat-delay))
              nil ;; Throttle
            (let* ((status (if (< last-time 1.0)
                               mpc-status (mpc-cmd-status)))
                   (songid (cdr (assq 'songid status)))
                   (time (if songid
                             (if (< last-time 1.0)
                                 (cdr mpc-last-seek-time)
                               (string-to-number
                                (cdr (assq 'time status)))))))
              (setq mpc-last-seek-time
                    (cons currenttime (setq time (+ time step))))
              (mpc-proc-cmd (list "seekid" songid time)
                            'mpc-status-refresh))))
      (let ((status (mpc-cmd-status)))
        (let* ((songid (cdr (assq 'songid status)))
                       (time (if songid (string-to-number
                                         (cdr (assq 'time status))))))
          (let ((timer (run-with-timer
                        t repeat-delay
                        (lambda ()
                          (mpc-proc-cmd (list "seekid" songid
                                              (setq time (+ time step)))
                                        'mpc-status-refresh)))))
            (while (mouse-movement-p
                    (event-basic-type (setq event (read-event)))))
            (cancel-timer timer)))))))

(defvar mpc--faster-toggle-timer nil)
(defun mpc--faster-stop ()
  (when mpc--faster-toggle-timer
    (cancel-timer mpc--faster-toggle-timer)
    (setq mpc--faster-toggle-timer nil)))

(defun mpc--faster-toggle-refresh ()
  (if (equal (cdr (assq 'state mpc-status)) "stop")
      (mpc--faster-stop)))

(defun mpc--songduration ()
  (string-to-number
   (let ((s (cdr (assq 'time mpc-status))))
     (if (not (string-match ":" s))
         (error "Unexpected time format %S" s)
       (substring s (match-end 0))))))

(defvar mpc--faster-toggle-forward nil)
(defvar mpc--faster-acceleration 0.5)
(defun mpc--faster-toggle (speedup step)
  (setq speedup (float speedup))
  (if mpc--faster-toggle-timer
      (mpc--faster-stop)
    (mpc-status-refresh) (mpc-proc-sync)
    (let* (songid       ;The ID of the currently ffwd/rewinding song.
           songduration ;The duration of that song.
           songtime     ;The time of the song last time we ran.
           oldtime      ;The time of day last time we ran.
           prevsongid)  ;The song we're in the process leaving.
      (let ((fun
             (lambda ()
               (let ((newsongid (cdr (assq 'songid mpc-status))))

                 (if (and (equal prevsongid newsongid)
                          (not (equal prevsongid songid)))
                     ;; We left prevsongid and came back to it.  Pretend it
                     ;; didn't happen.
                     (setq newsongid songid))

                 (cond
                  ((null newsongid) (mpc--faster-stop))
                  ((not (equal songid newsongid))
                   ;; We jumped to another song: reset.
                   (setq songid newsongid)
                   (setq songtime (string-to-number
                                   (cdr (assq 'time mpc-status))))
                   (setq songduration (mpc--songduration))
                   (setq oldtime (float-time)))
                  ((and (>= songtime songduration) mpc--faster-toggle-forward)
                   ;; Skip to the beginning of the next song.
                   (if (not (equal (cdr (assq 'state mpc-status)) "play"))
                       (mpc-proc-cmd "next" 'mpc-status-refresh)
                     ;; If we're playing, this is done automatically, so we
                     ;; don't need to do anything, or rather we *shouldn't*
                     ;; do anything otherwise there's a race condition where
                     ;; we could skip straight to the next next song.
                     nil))
                  ((and (<= songtime 0) (not mpc--faster-toggle-forward))
                   ;; Skip to the end of the previous song.
                   (setq prevsongid songid)
                   (mpc-proc-cmd "previous"
                    (lambda ()
                      (mpc-status-refresh
                       (lambda ()
                         (setq songid (cdr (assq 'songid mpc-status)))
                         (setq songtime (setq songduration (mpc--songduration)))
                         (setq oldtime (float-time))
                         (mpc-proc-cmd (list "seekid" songid songtime)))))))
                  (t
                   (setq speedup (+ speedup mpc--faster-acceleration))
                   (let ((newstep
                          (truncate (* speedup (- (float-time) oldtime)))))
                     (if (<= newstep 1) (setq newstep 1))
                     (setq oldtime (+ oldtime (/ newstep speedup)))
                     (if (not mpc--faster-toggle-forward)
                         (setq newstep (- newstep)))
                     (setq songtime (min songduration (+ songtime newstep)))
                     (unless (>= songtime songduration)
                       (condition-case nil
                           (mpc-proc-cmd
                            (list "seekid" songid songtime)
                            'mpc-status-refresh)
                         (mpc-proc-error (mpc-status-refresh)))))))))))
        (setq mpc--faster-toggle-forward (> step 0))
        (funcall fun)                   ;Initialize values.
        (setq mpc--faster-toggle-timer
              (run-with-timer t 0.3 fun))))))



(defvar mpc-faster-speedup 8)

(defun mpc-ffwd (_event)
  "Fast forward."
  (interactive (list last-nonmenu-event))
  ;; (mpc--faster event 4.0 1)
  (mpc--faster-toggle mpc-faster-speedup 1))

(defun mpc-rewind (_event)
  "Fast rewind."
  (interactive (list last-nonmenu-event))
  ;; (mpc--faster event 4.0 -1)
  (mpc--faster-toggle mpc-faster-speedup -1))


(defun mpc-play-at-point (&optional event)
  (interactive (list last-nonmenu-event))
  (mpc-select event)
  (mpc-play))

;; (defun mpc-play-tagval ()
;;   "Play all the songs of the tag at point."
;;   (interactive)
;;   (let* ((val (buffer-substring (line-beginning-position) (line-end-position)))
;;          (songs (mapcar 'cdar
;;                         (mpc-proc-buf-to-alists
;;                          (mpc-proc-cmd (list "find" mpc-tag val))))))
;;     (mpc-cmd-add songs)
;;     (if (member (cdr (assq 'state (mpc-cmd-status))) '("stop"))
;;         (mpc-cmd-play))))

;;; Drag'n'drop support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo:
;; the main thing to do here, is to provide visual feedback during the drag:
;; - change the mouse-cursor.
;; - highlight/select the source and the current destination.

(defun mpc-drag-n-drop (event)
  "DWIM for a drag EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (end (event-end event))
         (start-buf (window-buffer (posn-window start)))
         (end-buf (window-buffer (posn-window end)))
         (songs
          (with-current-buffer start-buf
            (goto-char (posn-point start))
            (if (get-text-property (point) 'mpc-select)
                ;; FIXME: actually we should only consider the constraints
                ;; corresponding to the selection in this particular buffer.
                (mpc-songs-selection)
              (cond
               ((and (derived-mode-p 'mpc-songs-mode)
                     (get-text-property (point) 'mpc-file))
                (list (cons (get-text-property (point) 'mpc-file)
                            (get-text-property (point) 'mpc-file-pos))))
               ((and mpc-tag (not (mpc-tagbrowser-all-p)))
                (mapcar (lambda (song)
                          (list (cdr (assq 'file song))))
                        (mpc-cmd-find
                         mpc-tag
                         (buffer-substring (line-beginning-position)
                                           (line-end-position)))))
               (t
                (error "Unsupported starting position for drag'n'drop gesture")))))))
    (with-current-buffer end-buf
      (goto-char (posn-point end))
      (cond
       ((eq mpc-tag 'Playlist)
        ;; Adding elements to a named playlist.
        (let ((playlist (if (or (mpc-tagbrowser-all-p)
                               (and (bolp) (eolp)))
                           (error "Not a playlist")
                         (buffer-substring (line-beginning-position)
                                           (line-end-position)))))
         (mpc-cmd-add (mapcar 'car songs) playlist)
         (message "Added %d songs to %s" (length songs) playlist)
         (if (member playlist
                     (cdr (assq 'Playlist (mpc-constraints-get-current))))
             (mpc-songs-refresh))))
       ((derived-mode-p 'mpc-songs-mode)
        (cond
         ((null mpc-songs-playlist)
          (error "The songs shown do not belong to a playlist"))
         ((eq start-buf end-buf)
          ;; Moving songs within the shown playlist.
          (let ((dest-pos (get-text-property (point) 'mpc-file-pos)))
            (mpc-cmd-move (mapcar 'cdr songs) dest-pos mpc-songs-playlist)
            (message "Moved %d songs" (length songs))))
         (t
          ;; Adding songs to the shown playlist.
          (let ((dest-pos (get-text-property (point) 'mpc-file-pos))
                (pl (if (stringp mpc-songs-playlist)
                        (mpc-cmd-find 'Playlist mpc-songs-playlist)
                      (mpc-proc-cmd-to-alist "playlist"))))
            ;; MPD's protocol does not let us add songs at a particular
            ;; position in a playlist, so we first have to add them to the
            ;; end, and then move them to their final destination.
            (mpc-cmd-add (mapcar 'car songs) mpc-songs-playlist)
            (mpc-cmd-move (let ((poss '()))
                            (dotimes (i (length songs))
                                     (push (+ i (length pl)) poss))
                            (nreverse poss)) dest-pos mpc-songs-playlist)
            (message "Added %d songs" (length songs)))))
        (mpc-songs-refresh))
      (t
       (error "Unsupported drag'n'drop gesture"))))))

;;; Toplevel ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom mpc-frame-alist '((name . "MPC") (tool-bar-lines . 1)
                             (font . "Sans"))
  "Alist of frame parameters for the MPC frame."
  :type 'alist)

;;;###autoload
(defun mpc ()
  "Main entry point for MPC."
  (interactive
   (progn
     (if current-prefix-arg
         (setq mpc-host (read-string "MPD host and port: " nil nil mpc-host)))
     nil))
  (let* ((song-buf (mpc-songs-buf))
         (song-win (get-buffer-window song-buf 0)))
    (if song-win
        (select-window song-win)
      (if (or (window-dedicated-p (selected-window))
              (window-minibuffer-p))
          (ignore-errors (select-frame (make-frame mpc-frame-alist)))
        (with-current-buffer song-buf
          (set (make-local-variable 'mpc-previous-window-config)
               (current-window-configuration))))
      (let* ((win1 (selected-window))
             (win2 (split-window))
             (tags mpc-browser-tags))
        (unless tags (error "Need at least one entry in `mpc-browser-tags'"))
        (set-window-buffer win2 song-buf)
        (set-window-dedicated-p win2 'soft)
        (mpc-status-buffer-show)
        (while
            (progn
              (set-window-buffer win1 (mpc-tagbrowser-buf (pop tags)))
              (set-window-dedicated-p win1 'soft)
              tags)
          (setq win1 (split-window win1 nil 'horiz)))))
    (balance-windows-area))
  (mpc-songs-refresh)
  (mpc-status-refresh))

(provide 'mpc)

;;; mpc.el ends here
