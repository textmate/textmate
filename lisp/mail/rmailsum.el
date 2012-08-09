;;; rmailsum.el --- make summary buffers for the mail reader

;; Copyright (C) 1985, 1993-1996, 2000-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail
;; Package: rmail

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

;; Extended by Bob Weiner of Motorola
;;   Provided all commands from rmail-mode in rmail-summary-mode and made key
;;   bindings in both modes wholly compatible.

;;; Code:

;; For rmail-select-summary.
(require 'rmail)
(require 'rfc2047)

(defcustom rmail-summary-scroll-between-messages t
  "Non-nil means Rmail summary scroll commands move between messages.
That is, after `rmail-summary-scroll-msg-up' reaches the end of a
message, it moves to the next message; and similarly for
`rmail-summary-scroll-msg-down'."
  :type 'boolean
  :group 'rmail-summary)

;; FIXME could do with a :set function that regenerates the summary
;; and updates rmail-summary-vector.
(defcustom rmail-summary-line-count-flag t
  "Non-nil means Rmail summary should show the number of lines in each message.
Setting this option to nil might speed up the generation of summaries."
  :type 'boolean
  :group 'rmail-summary)

(defvar rmail-summary-font-lock-keywords
  '(("^.....D.*" . font-lock-string-face)			; Deleted.
    ("^.....-.*" . font-lock-type-face)				; Unread.
    ;; Neither of the below will be highlighted if either of the above are:
    ("^.....[^D-] \\(......\\)" 1 font-lock-keyword-face)	; Date.
    ("{ \\([^\n}]+\\) }" 1 font-lock-comment-face))		; Labels.
  "Additional expressions to highlight in Rmail Summary mode.")

(defvar rmail-summary-redo nil
  "(FUNCTION . ARGS) to regenerate this Rmail summary buffer.")

(defvar rmail-summary-overlay nil
  "Overlay used to highlight the current message in the Rmail summary.")
(put 'rmail-summary-overlay 'permanent-local t)

(defvar rmail-summary-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map [mouse-2] 'rmail-summary-mouse-goto-message)
    (define-key map "a"      'rmail-summary-add-label)
    (define-key map "b"      'rmail-summary-bury)
    (define-key map "c"      'rmail-summary-continue)
    (define-key map "d"      'rmail-summary-delete-forward)
    (define-key map "\C-d"   'rmail-summary-delete-backward)
    (define-key map "e"      'rmail-summary-edit-current-message)
    (define-key map "f"      'rmail-summary-forward)
    (define-key map "g"      'rmail-summary-get-new-mail)
    (define-key map "h"      'rmail-summary)
    (define-key map "i"      'rmail-summary-input)
    (define-key map "j"      'rmail-summary-goto-msg)
    (define-key map "\C-m"   'rmail-summary-goto-msg)
    (define-key map "k"      'rmail-summary-kill-label)
    (define-key map "l"      'rmail-summary-by-labels)
    (define-key map "\e\C-h" 'rmail-summary)
    (define-key map "\e\C-l" 'rmail-summary-by-labels)
    (define-key map "\e\C-r" 'rmail-summary-by-recipients)
    (define-key map "\e\C-s" 'rmail-summary-by-regexp)
    ;; `f' for "from".
    (define-key map "\e\C-f" 'rmail-summary-by-senders)
    (define-key map "\e\C-t" 'rmail-summary-by-topic)
    (define-key map "m"      'rmail-summary-mail)
    (define-key map "\M-m"   'rmail-summary-retry-failure)
    (define-key map "n"      'rmail-summary-next-msg)
    (define-key map "\en"    'rmail-summary-next-all)
    (define-key map "\e\C-n" 'rmail-summary-next-labeled-message)
    (define-key map "o"      'rmail-summary-output)
    (define-key map "\C-o"   'rmail-summary-output-as-seen)
    (define-key map "p"      'rmail-summary-previous-msg)
    (define-key map "\ep"    'rmail-summary-previous-all)
    (define-key map "\e\C-p" 'rmail-summary-previous-labeled-message)
    (define-key map "q"      'rmail-summary-quit)
    (define-key map "Q"      'rmail-summary-wipe)
    (define-key map "r"      'rmail-summary-reply)
    (define-key map "s"      'rmail-summary-expunge-and-save)
    ;; See rms's comment in rmail.el
    ;; (define-key map "\er"    'rmail-summary-search-backward)
    (define-key map "\es"    'rmail-summary-search)
    (define-key map "t"      'rmail-summary-toggle-header)
    (define-key map "u"      'rmail-summary-undelete)
    (define-key map "\M-u"   'rmail-summary-undelete-many)
    (define-key map "x"      'rmail-summary-expunge)
    (define-key map "w"      'rmail-summary-output-body)
    (define-key map "v"      'rmail-mime)
    (define-key map "."      'rmail-summary-beginning-of-message)
    (define-key map "/"      'rmail-summary-end-of-message)
    (define-key map "<"      'rmail-summary-first-message)
    (define-key map ">"      'rmail-summary-last-message)
    (define-key map " "      'rmail-summary-scroll-msg-up)
    (define-key map "\177"   'rmail-summary-scroll-msg-down)
    (define-key map "?"      'describe-mode)
    (define-key map "\C-c\C-n" 'rmail-summary-next-same-subject)
    (define-key map "\C-c\C-p" 'rmail-summary-previous-same-subject)
    (define-key map "\C-c\C-s\C-d" 'rmail-summary-sort-by-date)
    (define-key map "\C-c\C-s\C-s" 'rmail-summary-sort-by-subject)
    (define-key map "\C-c\C-s\C-a" 'rmail-summary-sort-by-author)
    (define-key map "\C-c\C-s\C-r" 'rmail-summary-sort-by-recipient)
    (define-key map "\C-c\C-s\C-c" 'rmail-summary-sort-by-correspondent)
    (define-key map "\C-c\C-s\C-l" 'rmail-summary-sort-by-lines)
    (define-key map "\C-c\C-s\C-k" 'rmail-summary-sort-by-labels)
    (define-key map "\C-x\C-s" 'rmail-summary-save-buffer)

    ;; Menu bar bindings.

    (define-key map [menu-bar] (make-sparse-keymap))

    (define-key map [menu-bar classify]
      (cons "Classify" (make-sparse-keymap "Classify")))

    (define-key map [menu-bar classify output-menu]
      '("Output (Rmail Menu)..." . rmail-summary-output-menu))

    (define-key map [menu-bar classify input-menu]
      '("Input Rmail File (menu)..." . rmail-input-menu))

    (define-key map [menu-bar classify input-menu]
      '(nil))

    (define-key map [menu-bar classify output-menu]
      '(nil))

    (define-key map [menu-bar classify output-body]
      '("Output body..." . rmail-summary-output-body))

    (define-key map [menu-bar classify output-inbox]
      '("Output..." . rmail-summary-output))

    (define-key map [menu-bar classify output]
      '("Output as seen..." . rmail-summary-output-as-seen))

    (define-key map [menu-bar classify kill-label]
      '("Kill Label..." . rmail-summary-kill-label))

    (define-key map [menu-bar classify add-label]
      '("Add Label..." . rmail-summary-add-label))

    (define-key map [menu-bar summary]
      (cons "Summary" (make-sparse-keymap "Summary")))

    (define-key map [menu-bar summary senders]
      '("By Senders..." . rmail-summary-by-senders))

    (define-key map [menu-bar summary labels]
      '("By Labels..." . rmail-summary-by-labels))

    (define-key map [menu-bar summary recipients]
      '("By Recipients..." . rmail-summary-by-recipients))

    (define-key map [menu-bar summary topic]
      '("By Topic..." . rmail-summary-by-topic))

    (define-key map [menu-bar summary regexp]
      '("By Regexp..." . rmail-summary-by-regexp))

    (define-key map [menu-bar summary all]
      '("All" . rmail-summary))

    (define-key map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))

    (define-key map [menu-bar mail rmail-summary-get-new-mail]
      '("Get New Mail" . rmail-summary-get-new-mail))

    (define-key map [menu-bar mail lambda]
      '("----"))

    (define-key map [menu-bar mail continue]
      '("Continue" . rmail-summary-continue))

    (define-key map [menu-bar mail resend]
      '("Re-send..." . rmail-summary-resend))

    (define-key map [menu-bar mail forward]
      '("Forward" . rmail-summary-forward))

    (define-key map [menu-bar mail retry]
      '("Retry" . rmail-summary-retry-failure))

    (define-key map [menu-bar mail reply]
      '("Reply" . rmail-summary-reply))

    (define-key map [menu-bar mail mail]
      '("Mail" . rmail-summary-mail))

    (define-key map [menu-bar delete]
      (cons "Delete" (make-sparse-keymap "Delete")))

    (define-key map [menu-bar delete expunge/save]
      '("Expunge/Save" . rmail-summary-expunge-and-save))

    (define-key map [menu-bar delete expunge]
      '("Expunge" . rmail-summary-expunge))

    (define-key map [menu-bar delete undelete]
      '("Undelete" . rmail-summary-undelete))

    (define-key map [menu-bar delete delete]
      '("Delete" . rmail-summary-delete-forward))

    (define-key map [menu-bar move]
      (cons "Move" (make-sparse-keymap "Move")))

    (define-key map [menu-bar move search-back]
      '("Search Back..." . rmail-summary-search-backward))

    (define-key map [menu-bar move search]
      '("Search..." . rmail-summary-search))

    (define-key map [menu-bar move previous]
      '("Previous Nondeleted" . rmail-summary-previous-msg))

    (define-key map [menu-bar move next]
      '("Next Nondeleted" . rmail-summary-next-msg))

    (define-key map [menu-bar move last]
      '("Last" . rmail-summary-last-message))

    (define-key map [menu-bar move first]
      '("First" . rmail-summary-first-message))

    (define-key map [menu-bar move previous]
      '("Previous" . rmail-summary-previous-all))

    (define-key map [menu-bar move next]
      '("Next" . rmail-summary-next-all))
    map)
  "Keymap used in Rmail summary mode.")

;; Entry points for making a summary buffer.

;; Regenerate the contents of the summary
;; using the same selection criterion as last time.
;; M-x revert-buffer in a summary buffer calls this function.
(defun rmail-update-summary (&rest ignore)
  (apply (car rmail-summary-redo) (cdr rmail-summary-redo)))

;;;###autoload
(defun rmail-summary ()
  "Display a summary of all messages, one line per message."
  (interactive)
  (rmail-new-summary "All" '(rmail-summary) nil))

;;;###autoload
(defun rmail-summary-by-labels (labels)
  "Display a summary of all messages with one or more LABELS.
LABELS should be a string containing the desired labels, separated by commas."
  (interactive "sLabels to summarize by: ")
  (if (string= labels "")
      (setq labels (or rmail-last-multi-labels
		       (error "No label specified"))))
  (setq rmail-last-multi-labels labels)
  (rmail-new-summary (concat "labels " labels)
		     (list 'rmail-summary-by-labels labels)
		     'rmail-message-labels-p
		     (concat " \\("
			     (mail-comma-list-regexp labels)
			     "\\)\\(,\\|\\'\\)")))

;; FIXME "a string of regexps separated by commas" makes no sense because:
;;  i) it's pointless (you can just use \\|)
;; ii) it's broken (you can't specify a literal comma)
;; rmail-summary-by-topic and rmail-summary-by-senders have the same issue.
;;;###autoload
(defun rmail-summary-by-recipients (recipients &optional primary-only)
  "Display a summary of all messages with the given RECIPIENTS.
Normally checks the To, From and Cc fields of headers;
but if PRIMARY-ONLY is non-nil (prefix arg given),
 only look in the To and From fields.
RECIPIENTS is a string of regexps separated by commas."
  (interactive "sRecipients to summarize by: \nP")
  (rmail-new-summary
   (concat "recipients " recipients)
   (list 'rmail-summary-by-recipients recipients primary-only)
   'rmail-message-recipients-p
   (mail-comma-list-regexp recipients) primary-only))

(defun rmail-message-recipients-p (msg recipients &optional primary-only)
  (rmail-apply-in-message msg 'rmail-message-recipients-p-1
			  recipients primary-only))

(defun rmail-message-recipients-p-1 (recipients &optional primary-only)
  ;; mail-fetch-field does not care where it starts from.
  (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
  (or (string-match recipients (or (mail-fetch-field "To") ""))
      (string-match recipients (or (mail-fetch-field "From") ""))
      (if (not primary-only)
	  (string-match recipients (or (mail-fetch-field "Cc") "")))))

;; FIXME I find this a non-obvious name for what this function does.
;; Also, the optional WHOLE-MESSAGE argument of r-s-by-topic would
;; seem more natural here.
;;;###autoload
(defun rmail-summary-by-regexp (regexp)
  "Display a summary of all messages according to regexp REGEXP.
If the regular expression is found in the header of the message
\(including in the date and other lines, as well as the subject line),
Emacs will list the message in the summary."
  (interactive "sRegexp to summarize by: ")
  (if (string= regexp "")
      (setq regexp (or rmail-last-regexp
			 (error "No regexp specified"))))
  (setq rmail-last-regexp regexp)
  (rmail-new-summary (concat "regexp " regexp)
		     (list 'rmail-summary-by-regexp regexp)
		     'rmail-message-regexp-p
                     regexp))

(defun rmail-message-regexp-p (msg regexp)
  "Return t, if for message number MSG, regexp REGEXP matches in the header."
  (rmail-apply-in-message msg 'rmail-message-regexp-p-1 msg regexp))

(defun rmail-message-regexp-p-1 (msg regexp)
  ;; Search functions can expect to start from the beginning.
  (narrow-to-region (point) (save-excursion (search-forward "\n\n") (point)))
  (if (and rmail-enable-mime
	   rmail-search-mime-header-function)
      (funcall rmail-search-mime-header-function msg regexp (point))
    (re-search-forward regexp nil t)))

;;;###autoload
(defun rmail-summary-by-topic (subject &optional whole-message)
  "Display a summary of all messages with the given SUBJECT.
Normally checks just the Subject field of headers; but with prefix
argument WHOLE-MESSAGE is non-nil, looks in the whole message.
SUBJECT is a string of regexps separated by commas."
  (interactive
   ;; We quote the default subject, because if it contains regexp
   ;; special characters (eg "?"), it can fail to match itself.  (Bug#2333)
   (let* ((subject (regexp-quote (rmail-simplified-subject)))
	  (prompt (concat "Topics to summarize by (regexp"
			  (if subject ", default current subject" "")
			  "): ")))
     (list (read-string prompt nil nil subject) current-prefix-arg)))
  (rmail-new-summary
   (concat "about " subject)
   (list 'rmail-summary-by-topic subject whole-message)
   'rmail-message-subject-p
   (mail-comma-list-regexp subject) whole-message))

(defun rmail-message-subject-p (msg subject &optional whole-message)
  (if whole-message
      (rmail-apply-in-message msg 're-search-forward subject nil t)
    (string-match subject (rmail-simplified-subject msg))))

;;;###autoload
(defun rmail-summary-by-senders (senders)
  "Display a summary of all messages whose \"From\" field matches SENDERS.
SENDERS is a string of regexps separated by commas."
  (interactive "sSenders to summarize by: ")
  (rmail-new-summary
   (concat "senders " senders)
   (list 'rmail-summary-by-senders senders)
   'rmail-message-senders-p
   (mail-comma-list-regexp senders)))

(defun rmail-message-senders-p (msg senders)
  (string-match senders (or (rmail-get-header "From" msg) "")))

;; General making of a summary buffer.

(defvar rmail-summary-symbol-number 0)

(defvar rmail-new-summary-line-count)

(defun rmail-new-summary (desc redo func &rest args)
  "Create a summary of selected messages.
DESC makes part of the mode line of the summary buffer. REDO is form ...
For each message, FUNC is applied to the message number and ARGS...
and if the result is non-nil, that message is included.
nil for FUNCTION means all messages."
  (message "Computing summary lines...")
  (unless rmail-buffer
    (error "No RMAIL buffer found"))
  (let (mesg was-in-summary sumbuf)
    (if (eq major-mode 'rmail-summary-mode)
	(setq was-in-summary t))
    (with-current-buffer rmail-buffer
      (setq rmail-summary-buffer (rmail-new-summary-1 desc redo func args)
	    ;; r-s-b is buffer-local.
	    sumbuf rmail-summary-buffer
	    mesg rmail-current-message))
    ;; Now display the summary buffer and go to the right place in it.
    (unless was-in-summary
      (if (and (one-window-p)
	       pop-up-windows
	       (not pop-up-frames))
	  ;; If there is just one window, put the summary on the top.
	  (progn
	    (split-window (selected-window) rmail-summary-window-size)
	    (select-window (next-window (frame-first-window)))
	    (rmail-pop-to-buffer sumbuf)
	    ;; If pop-to-buffer did not use that window, delete that
	    ;; window.  (This can happen if it uses another frame.)
	    (if (not (eq sumbuf (window-buffer (frame-first-window))))
		(delete-other-windows)))
	(rmail-pop-to-buffer sumbuf))
      (set-buffer rmail-buffer)
      ;; This is how rmail makes the summary buffer reappear.
      ;; We do this here to make the window the proper size.
      (rmail-select-summary nil)
      (set-buffer rmail-summary-buffer))
    (rmail-summary-goto-msg mesg t t)
    (rmail-summary-construct-io-menu)
    (message "Computing summary lines...done")))

(defun rmail-new-summary-1 (description form function args)
  "Filter messages to obtain summary lines.
DESCRIPTION is added to the mode line.

Return the summary buffer by invoking FUNCTION on each message
passing the message number and ARGS...

REDO is a form ...

The current buffer must be a Rmail buffer either containing a
collection of mbox formatted messages or displaying a single
message."
  (let ((summary-msgs ())
	(rmail-new-summary-line-count 0)
	(sumbuf (rmail-get-create-summary-buffer)))
    ;; Scan the messages, getting their summary strings
    ;; and putting the list of them in SUMMARY-MSGS.
    (let ((msgnum 1)
	  (main-buffer (current-buffer))
	  (total rmail-total-messages)
	  (inhibit-read-only t))
      (save-excursion
	;; Go where the mbox text is.
	(if (rmail-buffers-swapped-p)
	    (set-buffer rmail-view-buffer))
	(let ((old-min (point-min-marker))
	      (old-max (point-max-marker)))
	  (unwind-protect
	      ;; Can't use save-restriction here; that doesn't work if we
	      ;; plan to modify text outside the original restriction.
	      (save-excursion
		(widen)
		(goto-char (point-min))
		(while (>= total msgnum)
		  ;; Go back to the Rmail buffer so
		  ;; so FUNCTION and rmail-get-summary can see its local vars.
		  (with-current-buffer main-buffer
		    ;; First test whether to include this message.
		    (if (or (null function)
			    (apply function msgnum args))
			(setq summary-msgs
			      (cons (cons msgnum (rmail-get-summary msgnum))
				    summary-msgs))))
		  (setq msgnum (1+ msgnum))
		  ;; Provide a periodic User progress message.
		  (if (and (not (zerop rmail-new-summary-line-count))
			   (zerop (% rmail-new-summary-line-count 10)))
		      (message "Computing summary lines...%d"
			       rmail-new-summary-line-count)))
		(setq summary-msgs (nreverse summary-msgs)))
	    (narrow-to-region old-min old-max)))))
    ;; Temporarily, while summary buffer is unfinished,
    ;; we "don't have" a summary.
    (setq rmail-summary-buffer nil)
    ;; I have not a clue what this clause is doing.  If you read this
    ;; chunk of code and have a clue, then please email that clue to
    ;; pmr@pajato.com
    (if rmail-enable-mime
	(with-current-buffer rmail-buffer
	  (setq rmail-summary-buffer nil)))
    (save-excursion
      (let ((rbuf (current-buffer))
	    (total rmail-total-messages))
	(set-buffer sumbuf)
	;; Set up the summary buffer's contents.
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (while summary-msgs
	    (princ (cdr (car summary-msgs)) sumbuf)
	    (setq summary-msgs (cdr summary-msgs)))
	  (goto-char (point-min)))
	;; Set up the rest of its state and local variables.
	(setq buffer-read-only t)
	(rmail-summary-mode)
	(make-local-variable 'minor-mode-alist)
	(setq minor-mode-alist (list (list t (concat ": " description))))
	(setq rmail-buffer rbuf
	      rmail-summary-redo form
	      rmail-total-messages total)))
    sumbuf))

(defun rmail-get-create-summary-buffer ()
  "Return the Rmail summary buffer.
If necessary, it is created and undo is disabled."
  (if (and rmail-summary-buffer (buffer-name rmail-summary-buffer))
      rmail-summary-buffer
    (let ((buff (generate-new-buffer (concat (buffer-name) "-summary"))))
      (with-current-buffer buff
	(setq buffer-undo-list t))
      buff)))


;; Low levels of generating a summary.

(defun rmail-get-summary (msgnum)
  "Return the summary line for message MSGNUM.
The mbox buffer must be current when you call this function
even if its text is swapped.

If the message has a summary line already, it will be stored in
the message as a header and simply returned, otherwise the
summary line is created, saved in the message header, cached and
returned.

The current buffer contains the unrestricted message collection."
  (let ((line (aref rmail-summary-vector (1- msgnum))))
    (unless line
      ;; Register a summary line for MSGNUM.
      (setq rmail-new-summary-line-count (1+ rmail-new-summary-line-count)
	    line (rmail-create-summary-line msgnum))
      ;; Cache the summary line for use during this Rmail session.
      (aset rmail-summary-vector (1- msgnum) line))
    line))

(defcustom rmail-summary-line-decoder (function rfc2047-decode-string)
  "Function to decode a Rmail summary line.
It receives the summary line for one message as a string
and should return the decoded string.

By default, it is `rfc2047-decode-string', which decodes MIME-encoded
subject."
  :type 'function
  :version "23.3"
  :group 'rmail-summary)

(defun rmail-create-summary-line (msgnum)
  "Return the summary line for message MSGNUM.
Obtain the message summary from the header if it is available
otherwise create it and store it in the message header.

The mbox buffer must be current when you call this function
even if its text is swapped."
  (let ((beg (rmail-msgbeg msgnum))
	(end (rmail-msgend msgnum))
	(deleted (rmail-message-deleted-p msgnum))
	;; Does not work (swapped?)
;;;	(unseen (rmail-message-unseen-p msgnum))
	unseen lines)
    (save-excursion
      ;; Switch to the buffer that has the whole mbox text.
      (if (rmail-buffers-swapped-p)
	  (set-buffer rmail-view-buffer))
      ;; Now we can compute the line count.
      (if rmail-summary-line-count-flag
	  (setq lines (count-lines beg end)))
      ;; Narrow to the message header.
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char beg)
	  (if (search-forward "\n\n" end t)
	      (progn
		(narrow-to-region beg (point))
		;; Replace rmail-message-unseen-p from above.
		(goto-char beg)
		(setq unseen (and (search-forward
				   (concat rmail-attribute-header ": ") nil t)
				  (looking-at "......U")))
		;; Generate a status line from the message.
		(rmail-create-summary msgnum deleted unseen lines))
	    (rmail-error-bad-format msgnum)))))))

;; FIXME this is now unused.
;; The intention was to display in the summary something like {E}
;; for an edited messaged, similarly for answered, etc.
;; But that conflicts with the previous rmail usage, where
;; any user-defined { labels } occupied this space.
;; So whilst it would be nice to have this information in the summary,
;; it would need to go somewhere else.
(defun rmail-get-summary-status ()
  "Return a coded string wrapped in curly braces denoting the status.

The current buffer must already be narrowed to the message headers for
the message being processed."
  (let ((status (mail-fetch-field rmail-attribute-header))
	(index 0)
	(result "")
	char)
    ;; Strip off the read/unread and the deleted attribute which are
    ;; handled separately.
    (setq status
	  (if status
	      (concat (substring status 0 1) (substring status 2 6))
	    ""))
    (while (< index (length status))
      (unless (string= "-" (setq char (substring status index (1+ index))))
	(setq result (concat result char)))
      (setq index (1+ index)))
    (when (> (length result) 0)
      (setq result (concat "{" result "}")))
    result))

(autoload 'rmail-make-label "rmailkwd")

(defun rmail-get-summary-labels ()
  "Return a string wrapped in curly braces with the current message labels.
Returns nil if there are no labels.  The current buffer must
already be narrowed to the message headers for the message being
processed."
  (let ((labels (mail-fetch-field rmail-keyword-header)))
    (and labels
	 (not (string-equal labels ""))
	 (progn
	   ;; Intern so that rmail-read-label can offer completion.
	   (mapc 'rmail-make-label (split-string labels ", "))
	   (format "{ %s } " labels)))))

(defun rmail-create-summary (msgnum deleted unseen lines)
  "Return the summary line for message MSGNUM.
The current buffer should already be narrowed to the header for that message.
It could be either buffer, so don't access Rmail local variables.
DELETED is t if this message is marked deleted.
UNSEEN is t if it is marked unseen.
LINES is the number of lines in the message (if we should display that)
 or else nil."
  (goto-char (point-min))
  (let ((line (rmail-header-summary))
	(labels (rmail-get-summary-labels))
	pos status prefix basic-start basic-end linecount-string)

    (setq linecount-string
	  (cond
	   ((not lines)       " ")
	   ((<= lines      9) (format "   [%d]" lines))
	   ((<= lines     99) (format "  [%d]" lines))
	   ((<= lines    999) (format " [%d]" lines))
	   ((<= lines   9999) (format "  [%dk]" (/ lines 1000)))
	   ((<= lines  99999) (format " [%dk]" (/ lines 1000)))
	   (t                 (format "[%dk]" (/ lines 1000)))))

    (setq status (cond
		  (deleted ?D)
		  (unseen ?-)
		  (t ? ))
	  prefix (format "%5d%c " msgnum status)
	  basic-start (car line)
	  basic-end (cadr line))
    (funcall rmail-summary-line-decoder
	     (concat prefix basic-start linecount-string " "
		     labels basic-end))))

(defun rmail-header-summary ()
  "Return a message summary based on the message headers.
The value is a list of two strings, the first and second parts of the summary.

The current buffer must already be narrowed to the message headers for
the message being processed."
  (goto-char (point-min))
  (list
   (concat (save-excursion
	     (if (not (re-search-forward "^Date:" nil t))
		 "      "
	       ;; Match month names case-insensitively
	       (cond ((let ((case-fold-search t))
			(re-search-forward "\\([^0-9:]\\)\\([0-3]?[0-9]\\)\\([- \t_]+\\)\\([adfjmnos][aceopu][bcglnprtvy]\\)"
					   (line-end-position) t))
		      (format "%2d-%3s"
			      (string-to-number (buffer-substring
						 (match-beginning 2)
						 (match-end 2)))
			      (buffer-substring
			       (match-beginning 4) (match-end 4))))
		     ((let ((case-fold-search t))
			(re-search-forward "\\([^a-z]\\)\\([adfjmnos][acepou][bcglnprtvy]\\)\\([-a-z \t_]*\\)\\([0-9][0-9]?\\)"
					   (line-end-position) t))
		      (format "%2d-%3s"
			      (string-to-number (buffer-substring
						 (match-beginning 4)
						 (match-end 4)))
			      (buffer-substring
			       (match-beginning 2) (match-end 2))))
		     ((re-search-forward "\\(19\\|20\\)\\([0-9][0-9]\\)-\\([01][0-9]\\)-\\([0-3][0-9]\\)"
		       (line-end-position) t)
		      (format "%2s%2s%2s"
			      (buffer-substring
			       (match-beginning 2) (match-end 2))
			      (buffer-substring
			       (match-beginning 3) (match-end 3))
			      (buffer-substring
			       (match-beginning 4) (match-end 4))))
		     (t "??????"))))
	   "  "
	   (save-excursion
	     (let* ((from (and (re-search-forward "^From:[ \t]*" nil t)
			       (mail-strip-quoted-names
				(buffer-substring
				 (1- (point))
				 ;; Get all the lines of the From field
				 ;; so that we get a whole comment if there is one,
				 ;; so that mail-strip-quoted-names can discard it.
				 (let ((opoint (point)))
				   (while (progn (forward-line 1)
						 (looking-at "[ \t]")))
				   ;; Back up over newline, then trailing spaces or tabs
				   (forward-char -1)
				   (skip-chars-backward " \t")
				   (point))))))
		    len mch lo)
	       (if (or (null from)
		       (string-match
			(or rmail-user-mail-address-regexp
			    (concat "^\\("
				    (regexp-quote (user-login-name))
				    "\\($\\|@\\)\\|"
				    (regexp-quote
				     ;; Don't lose if run from init file
				     ;; where user-mail-address is not
				     ;; set yet.
				     (or user-mail-address
					 (concat (user-login-name) "@"
						 (or mail-host-address
						     (system-name)))))
				    "\\>\\)"))
			from))
		   ;; No From field, or it's this user.
		   (save-excursion
		     (goto-char (point-min))
		     (if (not (re-search-forward "^To:[ \t]*" nil t))
			 nil
		       (setq from
			     (concat "to: "
				     (mail-strip-quoted-names
				      (buffer-substring
				       (point)
				       (progn (end-of-line)
					      (skip-chars-backward " \t")
					      (point)))))))))
	       (if (null from)
		   "                         "
		 ;; We are going to return only 25 characters of the
		 ;; address, so make sure it is RFC2047 decoded before
		 ;; taking its substring.  This is important when the address is not on the same line as the name, e.g.:
		 ;; To: =?UTF-8?Q?=C5=A0t=C4=9Bp=C3=A1n_?= =?UTF-8?Q?N=C4=9Bmec?=
		 ;; <stepnem@gmail.com>
		 (setq from (rfc2047-decode-string from))
		 (setq len (length from))
		 (setq mch (string-match "[@%]" from))
		 (format "%25s"
			 (if (or (not mch) (<= len 25))
			     (substring from (max 0 (- len 25)))
			   (substring from
				      (setq lo (cond ((< (- mch 14) 0) 0)
						     ((< len (+ mch 11))
						      (- len 25))
						     (t (- mch 14))))
				      (min len (+ lo 25)))))))))
   (concat (if (re-search-forward "^Subject:" nil t)
	       (let (pos str)
		 (skip-chars-forward " \t")
		 (setq pos (point))
		 (forward-line 1)
		 (setq str (buffer-substring pos (1- (point))))
		 (while (looking-at "\\s ")
		   (setq str (concat str " " 
				     (buffer-substring (match-end 0)
						       (line-end-position))))
		   (forward-line 1))
		 str)
	     (re-search-forward "[\n][\n]+" nil t)
	     (buffer-substring (point) (progn (end-of-line) (point))))
	   "\n")))

;; Simple motion in a summary buffer.

(defun rmail-summary-next-all (&optional number)
  (interactive "p")
  (forward-line (if number number 1))
  ;; It doesn't look nice to move forward past the last message line.
  (and (eobp) (> number 0)
       (forward-line -1))
  (display-buffer rmail-buffer))

(defun rmail-summary-previous-all (&optional number)
  (interactive "p")
  (forward-line (- (if number number 1)))
  ;; It doesn't look nice to move forward past the last message line.
  (and (eobp) (< number 0)
       (forward-line -1))
  (display-buffer rmail-buffer))

(defun rmail-summary-next-msg (&optional number)
  "Display next non-deleted msg from rmail file.
With optional prefix argument NUMBER, moves forward this number of non-deleted
messages, or backward if NUMBER is negative."
  (interactive "p")
  (forward-line 0)
  (and (> number 0) (end-of-line))
  (let ((count (if (< number 0) (- number) number))
	(search (if (> number 0) 're-search-forward 're-search-backward))
	(non-del-msg-found nil))
    (while (and (> count 0) (setq non-del-msg-found
				  (or (funcall search "^.....[^D]" nil t)
				      non-del-msg-found)))
      (setq count (1- count))))
  (beginning-of-line)
  (display-buffer rmail-buffer))

(defun rmail-summary-previous-msg (&optional number)
  "Display previous non-deleted msg from rmail file.
With optional prefix argument NUMBER, moves backward this number of
non-deleted messages."
  (interactive "p")
  (rmail-summary-next-msg (- (if number number 1))))

(defun rmail-summary-next-labeled-message (n labels)
  "Show next message with LABELS.  Defaults to last labels used.
With prefix argument N moves forward N messages with these labels."
  (interactive "p\nsMove to next msg with labels: ")
  (let (msg)
    (with-current-buffer rmail-buffer
      (rmail-next-labeled-message n labels)
      (setq msg rmail-current-message))
    (rmail-summary-goto-msg msg)))

(defun rmail-summary-previous-labeled-message (n labels)
  "Show previous message with LABELS.  Defaults to last labels used.
With prefix argument N moves backward N messages with these labels."
  (interactive "p\nsMove to previous msg with labels: ")
  (let (msg)
    (with-current-buffer rmail-buffer
      (rmail-previous-labeled-message n labels)
      (setq msg rmail-current-message))
    (rmail-summary-goto-msg msg)))

(defun rmail-summary-next-same-subject (n)
  "Go to the next message in the summary having the same subject.
With prefix argument N, do this N times.
If N is negative, go backwards."
  (interactive "p")
  (let ((forward (> n 0))
	subject i found)
    (with-current-buffer rmail-buffer
      (setq subject (rmail-simplified-subject)
	    i rmail-current-message))
    (save-excursion
      (while (and (/= n 0)
		  (if forward
		      (not (eobp))
		    (not (bobp))))
	(let (done)
	  (while (and (not done)
		      (if forward
			  (not (eobp))
			(not (bobp))))
	    ;; Advance thru summary.
	    (forward-line (if forward 1 -1))
	    ;; Get msg number of this line.
	    (setq i (string-to-number
		     (buffer-substring (point)
				       (min (point-max) (+ 6 (point))))))
	    (setq done (string-equal subject (rmail-simplified-subject i))))
	  (if done (setq found i)))
	(setq n (if forward (1- n) (1+ n)))))
    (if found
	(rmail-summary-goto-msg found)
      (error "No %s message with same subject"
	     (if forward "following" "previous")))))

(defun rmail-summary-previous-same-subject (n)
  "Go to the previous message in the summary having the same subject.
With prefix argument N, do this N times.
If N is negative, go forwards instead."
  (interactive "p")
  (rmail-summary-next-same-subject (- n)))

;; Delete and undelete summary commands.

(defun rmail-summary-delete-forward (&optional count)
  "Delete this message and move to next nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
A prefix argument serves as a repeat count;
a negative argument means to delete and move backward."
  (interactive "p")
  (unless (numberp count) (setq count 1))
  (let (end del-msg
	    (backward (< count 0)))
    (while (/= count 0)
      (rmail-summary-goto-msg)
      (with-current-buffer rmail-buffer
	(rmail-delete-message)
	(setq del-msg rmail-current-message))
      (rmail-summary-mark-deleted del-msg)
      (while (and (not (if backward (bobp) (eobp)))
		  (save-excursion (beginning-of-line)
				  (looking-at " *[0-9]+D")))
	(forward-line (if backward -1 1)))
      ;; It looks ugly to move to the empty line at end of buffer.
      (and (eobp) (not backward)
	   (forward-line -1))
      (setq count
	    (if (> count 0) (1- count) (1+ count))))))

(defun rmail-summary-delete-backward (&optional count)
  "Delete this message and move to previous nondeleted one.
Deleted messages stay in the file until the \\[rmail-expunge] command is given.
A prefix argument serves as a repeat count;
a negative argument means to delete and move forward."
  (interactive "p")
  (rmail-summary-delete-forward (- count)))

(defun rmail-summary-mark-deleted (&optional n undel)
  ;; Since third arg is t, this only alters the summary, not the Rmail buf.
  (and n (rmail-summary-goto-msg n t t))
  (or (eobp)
      (not (overlay-get rmail-summary-overlay 'face))
      (let ((buffer-read-only nil))
	(skip-chars-forward " ")
	(skip-chars-forward "0-9")
	(if undel
	    (if (looking-at "D")
		(progn (delete-char 1) (insert " ")))
	  (delete-char 1)
	  (insert "D"))
	;; Register a new summary line.
	(with-current-buffer rmail-buffer
	  (aset rmail-summary-vector (1- n) (rmail-create-summary-line n)))))
  (beginning-of-line))

(defun rmail-summary-update-line (n)
  "Update the summary line for message N."
  (when (rmail-summary-goto-msg n t t)
    (let* ((buffer-read-only nil)
	   (start (line-beginning-position))
	   (end (line-beginning-position 2))
	   (overlays (overlays-in start end))
	   high ov)
      (while (and (setq ov (car overlays))
		  (not (setq high (overlay-get ov 'rmail-summary))))
	(setq overlays (cdr overlays)))
      (delete-region start end)
      (princ
       (with-current-buffer rmail-buffer
	 (aset rmail-summary-vector (1- n) (rmail-create-summary-line n)))
       (current-buffer))
      (when high
	(forward-line -1)
	(rmail-summary-update-highlight nil)))))

(defun rmail-summary-mark-undeleted (n)
  (rmail-summary-mark-deleted n t))

(defun rmail-summary-deleted-p (&optional n)
  (save-excursion
    (and n (rmail-summary-goto-msg n nil t))
    (skip-chars-forward " ")
    (skip-chars-forward "0-9")
    (looking-at "D")))

(defun rmail-summary-undelete (&optional arg)
  "Undelete current message.
Optional prefix ARG means undelete ARG previous messages."
  (interactive "p")
  (if (/= arg 1)
      (rmail-summary-undelete-many arg)
    (let ((buffer-read-only nil)
	  (opoint (point)))
      (end-of-line)
      (cond ((re-search-backward "\\(^ *[0-9]*\\)\\(D\\)" nil t)
	     (replace-match "\\1 ")
	     (rmail-summary-goto-msg)
	     (if rmail-enable-mime
		 (set-buffer rmail-buffer)
	       (rmail-pop-to-buffer rmail-buffer))
	     (and (rmail-message-deleted-p rmail-current-message)
		  (rmail-undelete-previous-message))
	     (if rmail-enable-mime
		 (rmail-pop-to-buffer rmail-buffer))
	     (rmail-pop-to-buffer rmail-summary-buffer))
	    (t (goto-char opoint))))))

(defun rmail-summary-undelete-many (&optional n)
  "Undelete all deleted msgs, optional prefix arg N means undelete N prev msgs."
  (interactive "P")
  (with-current-buffer rmail-buffer
    (let* ((init-msg (if n rmail-current-message rmail-total-messages))
	   (rmail-current-message init-msg)
	   (n (or n rmail-total-messages))
	   (msgs-undeled 0))
      (while (and (> rmail-current-message 0)
		  (< msgs-undeled n))
	(if (rmail-message-deleted-p rmail-current-message)
	    (progn (rmail-set-attribute rmail-deleted-attr-index nil)
		   (setq msgs-undeled (1+ msgs-undeled))))
	(setq rmail-current-message (1- rmail-current-message)))
      (set-buffer rmail-summary-buffer)
      (setq rmail-current-message init-msg msgs-undeled 0)
      (while (and (> rmail-current-message 0)
		  (< msgs-undeled n))
	(if (rmail-summary-deleted-p rmail-current-message)
	    (progn (rmail-summary-mark-undeleted rmail-current-message)
		   (setq msgs-undeled (1+ msgs-undeled))))
	(setq rmail-current-message (1- rmail-current-message))))
    (rmail-summary-goto-msg)))

;; Rmail Summary mode is suitable only for specially formatted data.
(put 'rmail-summary-mode 'mode-class 'special)

(defun rmail-summary-mode ()
  "Rmail Summary Mode is invoked from Rmail Mode by using \\<rmail-mode-map>\\[rmail-summary].
As commands are issued in the summary buffer, they are applied to the
corresponding mail messages in the rmail buffer.

All normal editing commands are turned off.
Instead, nearly all the Rmail mode commands are available,
though many of them move only among the messages in the summary.

These additional commands exist:

\\[rmail-summary-undelete-many]	Undelete all or prefix arg deleted messages.
\\[rmail-summary-wipe] Delete the summary and go to the Rmail buffer.

Commands for sorting the summary:

\\[rmail-summary-sort-by-date] Sort by date.
\\[rmail-summary-sort-by-subject] Sort by subject.
\\[rmail-summary-sort-by-author] Sort by author.
\\[rmail-summary-sort-by-recipient] Sort by recipient.
\\[rmail-summary-sort-by-correspondent] Sort by correspondent.
\\[rmail-summary-sort-by-lines] Sort by lines.
\\[rmail-summary-sort-by-labels] Sort by labels."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'rmail-summary-mode)
  (setq mode-name "RMAIL Summary")
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'rmail-buffer)
  (make-local-variable 'rmail-total-messages)
  (make-local-variable 'rmail-current-message)
  (setq rmail-current-message nil)
  (make-local-variable 'rmail-summary-redo)
  (setq rmail-summary-redo nil)
  (make-local-variable 'revert-buffer-function)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(rmail-summary-font-lock-keywords t))
  (rmail-summary-enable)
  (run-mode-hooks 'rmail-summary-mode-hook))

;; Summary features need to be disabled during edit mode.
(defun rmail-summary-disable ()
  (use-local-map text-mode-map)
  (remove-hook 'post-command-hook 'rmail-summary-rmail-update t)
  (setq revert-buffer-function nil))

(defun rmail-summary-enable ()
  (use-local-map rmail-summary-mode-map)
  (add-hook 'post-command-hook 'rmail-summary-rmail-update nil t)
  (setq revert-buffer-function 'rmail-update-summary))

(defun rmail-summary-mark-seen (n &optional nomove unseen)
  "Remove the unseen mark from the current message, update the summary vector.
N is the number of the current message.  Optional argument NOMOVE
non-nil means we are already at the right column.  Optional argument
UNSEEN non-nil means mark the message as unseen."
  (save-excursion
    (unless nomove
      (beginning-of-line)
      (skip-chars-forward " ")
      (skip-chars-forward "0-9"))
    (when (char-equal (following-char) (if unseen ?\s ?-))
      (let ((buffer-read-only nil))
	(delete-char 1)
	(insert (if unseen "-" " ")))
      (let ((line (buffer-substring-no-properties (line-beginning-position)
						  (line-beginning-position 2))))
      (with-current-buffer rmail-buffer
	(aset rmail-summary-vector (1- n) line))))))

(defvar rmail-summary-put-back-unseen nil
  "Used for communicating between calls to `rmail-summary-rmail-update'.
If it moves to a message within an Incremental Search, and removes
the `unseen' attribute from that message, it sets this flag
so that if the next motion between messages is in the same Incremental
Search, the `unseen' attribute is restored.")

;; Show in Rmail the message described by the summary line that point is on,
;; but only if the Rmail buffer is already visible.
;; This is a post-command-hook in summary buffers.
(defun rmail-summary-rmail-update ()
  (let (buffer-read-only)
    (save-excursion
      ;; If at end of buffer, pretend we are on the last text line.
      (if (eobp)
	  (forward-line -1))
      (beginning-of-line)
      (skip-chars-forward " ")
      (let ((msg-num (string-to-number (buffer-substring
                                        (point)
                                        (progn (skip-chars-forward "0-9")
                                               (point))))))
	;; Always leave `unseen' removed
	;; if we get out of isearch mode.
	;; Don't let a subsequent isearch restore that `unseen'.
	(if (not isearch-mode)
	    (setq rmail-summary-put-back-unseen nil))

	(or (eq rmail-current-message msg-num)
	    (let ((window (get-buffer-window rmail-buffer t))
		  (owin (selected-window)))
	      (if isearch-mode
		  (progn
		    ;; If we first saw the previous message in this search,
		    ;; and we have gone to a different message while searching,
		    ;; put back `unseen' on the former one.
		    (when rmail-summary-put-back-unseen
		      (rmail-set-attribute rmail-unseen-attr-index t
					   rmail-current-message)
		      (save-excursion
			(goto-char rmail-summary-put-back-unseen)
			(rmail-summary-mark-seen rmail-current-message t t)))
		    ;; Arrange to do that later, for the new current message,
		    ;; if it still has `unseen'.
		    (setq rmail-summary-put-back-unseen
			  (if (rmail-message-unseen-p msg-num)
			      (point))))
		(setq rmail-summary-put-back-unseen nil))
	      ;; Go to the desired message.
	      (setq rmail-current-message msg-num)
	      ;; Update the summary to show the message has been seen.
	      (rmail-summary-mark-seen msg-num t)
	      (if window
		  ;; Using save-window-excursion would cause the new value
		  ;; of point to get lost.
		  (unwind-protect
		      (progn
			(select-window window)
			(rmail-show-message msg-num t))
		    (select-window owin))
		(if (buffer-name rmail-buffer)
		    (with-current-buffer rmail-buffer
		      (rmail-show-message msg-num t))))
	      ;; In linum mode, the message buffer must be specially
	      ;; updated (Bug#4878).
	      (and (fboundp 'linum-update)
		   (buffer-name rmail-buffer)
		   (linum-update rmail-buffer))))
	(rmail-summary-update-highlight nil)))))

(defun rmail-summary-save-buffer ()
  "Save the buffer associated with this RMAIL summary."
  (interactive)
  (save-window-excursion
    (save-excursion
      (switch-to-buffer rmail-buffer)
      (save-buffer))))

(defun rmail-summary-mouse-goto-message (event)
  "Select the message whose summary line you click on."
  (interactive "@e")
  (goto-char (posn-point (event-end event)))
  (rmail-summary-goto-msg))

(defun rmail-summary-goto-msg (&optional n nowarn skip-rmail)
  "Go to message N in the summary buffer and the Rmail buffer.
If N is nil, use the message corresponding to point in the summary
and move to that message in the Rmail buffer.

If NOWARN, don't say anything if N is out of range.
If SKIP-RMAIL, don't do anything to the Rmail buffer.
Returns non-nil if message N was found."
  (interactive "P")
  (if (consp n) (setq n (prefix-numeric-value n)))
  (if (eobp) (forward-line -1))
  (beginning-of-line)
  (let* ((obuf (current-buffer))
	 (buf rmail-buffer)
	 (cur (point))
	 message-not-found
	 (curmsg (string-to-number
		  (buffer-substring (point)
				    (min (point-max) (+ 6 (point))))))
	 (total (with-current-buffer buf rmail-total-messages)))
    ;; If message number N was specified, find that message's line
    ;; or set message-not-found.
    ;; If N wasn't specified or that message can't be found.
    ;; set N by default.
    (if (not n)
	(setq n curmsg)
      (if (< n 1)
	  (progn (message "No preceding message")
		 (setq n 1)))
      (if (and (> n total)
	       (> total 0))
	  (progn (message "No following message")
		 (goto-char (point-max))
		 (rmail-summary-goto-msg nil nowarn skip-rmail)))
      (goto-char (point-min))
      (if (not (re-search-forward (format "^%5d[^0-9]" n) nil t))
	  (progn (or nowarn (message "Message %d not found" n))
		 (setq n curmsg)
		 (setq message-not-found t)
		 (goto-char cur))))
    (rmail-summary-mark-seen n)
    (rmail-summary-update-highlight message-not-found)
    (beginning-of-line)
    (unless skip-rmail
      (let ((selwin (selected-window)))
	(unwind-protect
	    (progn (rmail-pop-to-buffer buf)
		   (rmail-show-message n))
	  (select-window selwin)
	  ;; The actions above can alter the current buffer.  Preserve it.
	  (set-buffer obuf))))
    (not message-not-found)))

;; Update the highlighted line in an rmail summary buffer.
;; That should be current.  We highlight the line point is on.
;; If NOT-FOUND is non-nil, we turn off highlighting.
(defun rmail-summary-update-highlight (not-found)
  ;; Make sure we have an overlay to use.
  (or rmail-summary-overlay
      (progn
	(make-local-variable 'rmail-summary-overlay)
	(setq rmail-summary-overlay (make-overlay (point) (point)))
	(overlay-put rmail-summary-overlay 'rmail-summary t)))
  ;; If this message is in the summary, use the overlay to highlight it.
  ;; Otherwise, don't highlight anything.
  (if not-found
      (overlay-put rmail-summary-overlay 'face nil)
    (move-overlay rmail-summary-overlay
		  (save-excursion (beginning-of-line)
				  (skip-chars-forward " ")
				  (point))
		  (line-end-position))
    (overlay-put rmail-summary-overlay 'face 'highlight)))

(defun rmail-summary-scroll-msg-up (&optional dist)
  "Scroll the Rmail window forward.
If the Rmail window is displaying the end of a message,
advance to the next message."
  (interactive "P")
  (if (eq dist '-)
      (rmail-summary-scroll-msg-down nil)
    (let ((rmail-buffer-window (get-buffer-window rmail-buffer)))
      (if rmail-buffer-window
	  (if (let ((rmail-summary-window (selected-window)))
		(select-window rmail-buffer-window)
		(prog1
		    ;; Is EOB visible in the buffer?
		    (save-excursion
		      (let ((ht (window-height (selected-window))))
			(move-to-window-line (- ht 2))
			(end-of-line)
			(eobp)))
		  (select-window rmail-summary-window)))
	      (if (not rmail-summary-scroll-between-messages)
		  (error "End of buffer")
		(rmail-summary-next-msg (or dist 1)))
	    (let ((other-window-scroll-buffer rmail-buffer))
	      (scroll-other-window dist)))
	;; If it isn't visible at all, show the beginning.
	(rmail-summary-beginning-of-message)))))

(defun rmail-summary-scroll-msg-down (&optional dist)
  "Scroll the Rmail window backward.
If the Rmail window is now displaying the beginning of a message,
move to the previous message."
  (interactive "P")
  (if (eq dist '-)
      (rmail-summary-scroll-msg-up nil)
    (let ((rmail-buffer-window (get-buffer-window rmail-buffer)))
      (if rmail-buffer-window
	  (if (let ((rmail-summary-window (selected-window)))
		(select-window rmail-buffer-window)
		(prog1
		    ;; Is BOB visible in the buffer?
		    (save-excursion
		      (move-to-window-line 0)
		      (beginning-of-line)
		      (bobp))
		  (select-window rmail-summary-window)))
	      (if (not rmail-summary-scroll-between-messages)
		  (error "Beginning of buffer")
		(rmail-summary-previous-msg (or dist 1)))
	    (let ((other-window-scroll-buffer rmail-buffer))
	      (scroll-other-window-down dist)))
	;; If it isn't visible at all, show the beginning.
	(rmail-summary-beginning-of-message)))))

(defun rmail-summary-beginning-of-message ()
  "Show current message from the beginning."
  (interactive)
  (rmail-summary-show-message 'BEG))

(defun rmail-summary-end-of-message ()
  "Show bottom of current message."
  (interactive)
  (rmail-summary-show-message 'END))

(defun rmail-summary-show-message (where)
  "Show current mail message.
Position it according to WHERE which can be BEG or END"
  (if (and (one-window-p) (not pop-up-frames))
      ;; If there is just one window, put the summary on the top.
      (let ((buffer rmail-buffer))
	(split-window (selected-window) rmail-summary-window-size)
	(select-window (frame-first-window))
	(rmail-pop-to-buffer rmail-buffer)
	;; If pop-to-buffer did not use that window, delete that
	;; window.  (This can happen if it uses another frame.)
	(or (eq buffer (window-buffer (next-window (frame-first-window))))
	    (delete-other-windows)))
    (rmail-pop-to-buffer rmail-buffer))
  (cond
   ((eq where 'BEG)
	(goto-char (point-min))
	(search-forward "\n\n"))
   ((eq where 'END)
	(goto-char (point-max))
	(recenter (1- (window-height))))
   )
  (rmail-pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-bury ()
  "Bury the Rmail buffer and the Rmail summary buffer."
  (interactive)
  (let ((buffer-to-bury (current-buffer)))
    (let (window)
      (while (setq window (get-buffer-window rmail-buffer))
	(set-window-buffer window (other-buffer rmail-buffer)))
      (bury-buffer rmail-buffer))
    (switch-to-buffer (other-buffer buffer-to-bury))
    (bury-buffer buffer-to-bury)))

(defun rmail-summary-quit ()
  "Quit out of Rmail and Rmail summary."
  (interactive)
  (rmail-summary-wipe)
  (rmail-quit))

(defun rmail-summary-wipe ()
  "Kill and wipe away Rmail summary, remaining within Rmail."
  (interactive)
  (with-current-buffer rmail-buffer (setq rmail-summary-buffer nil))
  (let ((local-rmail-buffer rmail-buffer))
    (kill-buffer (current-buffer))
    ;; Delete window if not only one.
    (if (not (eq (selected-window) (next-window nil 'no-minibuf)))
	(delete-window))
    ;; Switch windows to the rmail buffer, or switch to it in this window.
    (rmail-pop-to-buffer local-rmail-buffer)))

(defun rmail-summary-expunge ()
  "Actually erase all deleted messages and recompute summary headers."
  (interactive)
  (with-current-buffer rmail-buffer
    (when (rmail-expunge-confirmed)
      (rmail-only-expunge)))
  (rmail-update-summary))

(defun rmail-summary-expunge-and-save ()
  "Expunge and save RMAIL file."
  (interactive)
  (save-excursion
    (rmail-expunge-and-save))
  (rmail-update-summary)
  (set-buffer-modified-p nil))

(defun rmail-summary-get-new-mail (&optional file-name)
  "Get new mail and recompute summary headers.

Optionally you can specify the file to get new mail from.  In this case,
the file of new mail is not changed or deleted.  Noninteractively, you can
pass the inbox file name as an argument.  Interactively, a prefix
argument says to read a file name and use that file as the inbox."
  (interactive
   (list (if current-prefix-arg
	     (read-file-name "Get new mail from file: "))))
  (let (msg)
    (with-current-buffer rmail-buffer
      (rmail-get-new-mail file-name)
      ;; Get the proper new message number.
      (setq msg rmail-current-message))
    ;; Make sure that message is displayed.
    (or (zerop msg)
	(rmail-summary-goto-msg msg))))

(defun rmail-summary-input (filename)
  "Run Rmail on file FILENAME."
  (interactive "FRun rmail on RMAIL file: ")
  ;; We switch windows here, then display the other Rmail file there.
  (rmail-pop-to-buffer rmail-buffer)
  (rmail filename))

(defun rmail-summary-first-message ()
  "Show first message in Rmail file from summary buffer."
  (interactive)
  (with-no-warnings
    (beginning-of-buffer)))

(defun rmail-summary-last-message ()
  "Show last message in Rmail file from summary buffer."
  (interactive)
  (with-no-warnings
    (end-of-buffer))
  (forward-line -1))

(declare-function rmail-abort-edit "rmailedit" ())
(declare-function rmail-cease-edit "rmailedit"())
(declare-function rmail-set-label "rmailkwd" (l state &optional n))
(declare-function rmail-output-read-file-name "rmailout" ())
(declare-function mail-send-and-exit "sendmail" (&optional arg))

(defvar rmail-summary-edit-map nil)
(if rmail-summary-edit-map
    nil
  (setq rmail-summary-edit-map
	(nconc (make-sparse-keymap) text-mode-map))
  (define-key rmail-summary-edit-map "\C-c\C-c" 'rmail-cease-edit)
  (define-key rmail-summary-edit-map "\C-c\C-]" 'rmail-abort-edit))

(defun rmail-summary-edit-current-message ()
  "Edit the contents of this message."
  (interactive)
  (rmail-pop-to-buffer rmail-buffer)
  (rmail-edit-current-message)
  (use-local-map rmail-summary-edit-map))

(defun rmail-summary-cease-edit ()
  "Finish editing message, then go back to Rmail summary buffer."
  (interactive)
  (rmail-cease-edit)
  (rmail-pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-abort-edit ()
  "Abort edit of current message; restore original contents.
Go back to summary buffer."
  (interactive)
  (rmail-abort-edit)
  (rmail-pop-to-buffer rmail-summary-buffer))

(defun rmail-summary-search-backward (regexp &optional n)
  "Show message containing next match for REGEXP.
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (>= (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Rmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if rmail-search-last-regexp
			(concat ", default "
				rmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  ;; Don't use save-excursion because that prevents point from moving
  ;; properly in the summary buffer.
  (with-current-buffer rmail-buffer
    (rmail-search regexp (- n))))

(defun rmail-summary-search (regexp &optional n)
  "Show message containing next match for REGEXP.
Prefix argument gives repeat count; negative argument means search
backwards (through earlier messages).
Interactively, empty argument means use same regexp used last time."
  (interactive
    (let* ((reversep (< (prefix-numeric-value current-prefix-arg) 0))
	   (prompt
	    (concat (if reversep "Reverse " "") "Rmail search (regexp"))
	   regexp)
      (setq prompt
	    (concat prompt
		    (if rmail-search-last-regexp
			(concat ", default "
				rmail-search-last-regexp "): ")
		      "): ")))
      (setq regexp (read-string prompt))
      (cond ((not (equal regexp ""))
	     (setq rmail-search-last-regexp regexp))
	    ((not rmail-search-last-regexp)
	     (error "No previous Rmail search string")))
      (list rmail-search-last-regexp
	    (prefix-numeric-value current-prefix-arg))))
  ;; Don't use save-excursion because that prevents point from moving
  ;; properly in the summary buffer.
  (let ((buffer (current-buffer))
	(selwin (selected-window)))
    (unwind-protect
	(progn
	  (rmail-pop-to-buffer rmail-buffer)
	  (rmail-search regexp n))
      (select-window selwin)
      (set-buffer buffer))))

(defun rmail-summary-toggle-header ()
  "Show original message header if pruned header currently shown, or vice versa."
  (interactive)
  (save-window-excursion
    (set-buffer rmail-buffer)
    (rmail-toggle-header))
  ;; Inside save-excursion, some changes to point in the RMAIL buffer are lost.
  ;; Set point to point-min in the RMAIL buffer, if it is visible.
  (let ((window (get-buffer-window rmail-buffer)))
    (if window
        ;; Using save-window-excursion would lose the new value of point.
        (let ((owin (selected-window)))
          (unwind-protect
              (progn
                (select-window window)
                (goto-char (point-min)))
            (select-window owin))))))


(defun rmail-summary-add-label (label)
  "Add LABEL to labels associated with current Rmail message.
Completion is performed over known labels when reading."
  (interactive (list (with-current-buffer rmail-buffer
		       (rmail-read-label "Add label"))))
  (with-current-buffer rmail-buffer
    (rmail-add-label label)))

(defun rmail-summary-kill-label (label)
  "Remove LABEL from labels associated with current Rmail message.
Completion is performed over known labels when reading."
  (interactive (list (with-current-buffer rmail-buffer
		       (rmail-read-label "Kill label"))))
  (with-current-buffer rmail-buffer
    (rmail-set-label label nil)))

;;;; *** Rmail Summary Mailing Commands ***

(defun rmail-summary-override-mail-send-and-exit ()
  "Replace bindings to `mail-send-and-exit' with `rmail-summary-send-and-exit'."
  (use-local-map (copy-keymap (current-local-map)))
  (dolist (key (where-is-internal 'mail-send-and-exit))
    (define-key (current-local-map) key 'rmail-summary-send-and-exit)))

(defun rmail-summary-mail ()
  "Send mail in another window.
While composing the message, use \\[mail-yank-original] to yank the
original message into it."
  (interactive)
  (let ((window (get-buffer-window rmail-buffer)))
    (if window
	(select-window window)
      (set-buffer rmail-buffer)))
  (rmail-start-mail nil nil nil nil nil (current-buffer))
  (rmail-summary-override-mail-send-and-exit))

(defun rmail-summary-continue ()
  "Continue composing outgoing message previously being composed."
  (interactive)
  (let ((window (get-buffer-window rmail-buffer)))
    (if window
	(select-window window)
      (set-buffer rmail-buffer)))
  (rmail-start-mail t))

(defun rmail-summary-reply (just-sender)
  "Reply to the current message.
Normally include CC: to all other recipients of original message;
prefix argument means ignore them.  While composing the reply,
use \\[mail-yank-original] to yank the original message into it."
  (interactive "P")
  (let ((window (get-buffer-window rmail-buffer)))
    (if window
	(select-window window)
      (set-buffer rmail-buffer)))
  (rmail-reply just-sender)
  (rmail-summary-override-mail-send-and-exit))

(defun rmail-summary-retry-failure ()
  "Edit a mail message which is based on the contents of the current message.
For a message rejected by the mail system, extract the interesting headers and
the body of the original message; otherwise copy the current message."
  (interactive)
  (let ((window (get-buffer-window rmail-buffer)))
    (if window
	(select-window window)
      (set-buffer rmail-buffer)))
  (rmail-retry-failure)
  (rmail-summary-override-mail-send-and-exit))

(defun rmail-summary-send-and-exit ()
  "Send mail reply and return to summary buffer."
  (interactive)
  (mail-send-and-exit t))

(defun rmail-summary-forward (resend)
  "Forward the current message to another user.
With prefix argument, \"resend\" the message instead of forwarding it;
see the documentation of `rmail-resend'."
  (interactive "P")
  (save-excursion
    (let ((window (get-buffer-window rmail-buffer)))
      (if window
	  (select-window window)
	(set-buffer rmail-buffer)))
    (rmail-forward resend)
    (rmail-summary-override-mail-send-and-exit)))

(defun rmail-summary-resend ()
  "Resend current message using `rmail-resend'."
  (interactive)
  (save-excursion
    (let ((window (get-buffer-window rmail-buffer)))
      (if window
	  (select-window window)
	(set-buffer rmail-buffer)))
    (call-interactively 'rmail-resend)))

;; Summary output commands.

(defun rmail-summary-output (&optional file-name n)
  "Append this message to mail file FILE-NAME.
This works with both mbox format and Babyl format files,
outputting in the appropriate format for each.
The default file name comes from `rmail-default-file',
which is updated to the name you use in this command.

A prefix argument N says to output that many consecutive messages
from those in the summary, starting with the current one.
Deleted messages are skipped and don't count.
When called from Lisp code, N may be omitted and defaults to 1.

This command always outputs the complete message header,
even the header display is currently pruned."
  (interactive
   (progn (require 'rmailout)
	  (list (rmail-output-read-file-name)
		(prefix-numeric-value current-prefix-arg))))
  (let ((i 0) prev-msg)
    (while
	(and (< i n)
	     (progn (rmail-summary-goto-msg)
		    (not (eq prev-msg
			     (setq prev-msg
				   (with-current-buffer rmail-buffer
				     rmail-current-message))))))
      (setq i (1+ i))
      (with-current-buffer rmail-buffer
	(let ((rmail-delete-after-output nil))
	  (rmail-output file-name 1)))
      (if rmail-delete-after-output
	  (rmail-summary-delete-forward nil)
	(if (< i n)
	    (rmail-summary-next-msg 1))))))

(defalias 'rmail-summary-output-to-rmail-file 'rmail-summary-output)

(declare-function rmail-output-as-seen "rmailout"
		  (file-name &optional count noattribute from-gnus))

(defun rmail-summary-output-as-seen (&optional file-name n)
  "Append this message to mbox file named FILE-NAME.
A prefix argument N says to output that many consecutive messages,
from the summary, starting with the current one.
Deleted messages are skipped and don't count.
When called from Lisp code, N may be omitted and defaults to 1.

This outputs the message header as you see it (or would see it)
displayed in Rmail.

The default file name comes from `rmail-default-file',
which is updated to the name you use in this command."
  (interactive
   (progn (require 'rmailout)
	  (list (rmail-output-read-file-name)
		(prefix-numeric-value current-prefix-arg))))
  (require 'rmailout) ; for rmail-output-as-seen in non-interactive case
  (let ((i 0) prev-msg)
    (while
	(and (< i n)
	     (progn (rmail-summary-goto-msg)
		    (not (eq prev-msg
			     (setq prev-msg
				   (with-current-buffer rmail-buffer
				     rmail-current-message))))))
      (setq i (1+ i))
      (with-current-buffer rmail-buffer
	(let ((rmail-delete-after-output nil))
	  (rmail-output-as-seen file-name 1)))
      (if rmail-delete-after-output
	  (rmail-summary-delete-forward nil)
	(if (< i n)
	    (rmail-summary-next-msg 1))))))

(defun rmail-summary-output-menu ()
  "Output current message to another Rmail file, chosen with a menu.
Also set the default for subsequent \\[rmail-output-to-babyl-file] commands.
The variables `rmail-secondary-file-directory' and
`rmail-secondary-file-regexp' control which files are offered in the menu."
  (interactive)
  (with-current-buffer rmail-buffer
    (let ((rmail-delete-after-output nil))
      (call-interactively 'rmail-output-menu)))
  (if rmail-delete-after-output
      (rmail-summary-delete-forward nil)))

(defun rmail-summary-construct-io-menu ()
  (let ((files (rmail-find-all-files rmail-secondary-file-directory)))
    (if files
	(progn
	  (define-key rmail-summary-mode-map [menu-bar classify input-menu]
	    (cons "Input Rmail File"
		  (rmail-list-to-menu "Input Rmail File"
				      files
				      'rmail-summary-input)))
	  (define-key rmail-summary-mode-map [menu-bar classify output-menu]
	    (cons "Output Rmail File"
		  (rmail-list-to-menu "Output Rmail File"
				      files
				      'rmail-summary-output))))
      (define-key rmail-summary-mode-map [menu-bar classify input-menu]
	'("Input Rmail File" . rmail-disable-menu))
      (define-key rmail-summary-mode-map [menu-bar classify output-menu]
	'("Output Rmail File" . rmail-disable-menu)))))

(defun rmail-summary-output-body (&optional file-name)
  "Write this message body to the file FILE-NAME.
FILE-NAME defaults, interactively, from the Subject field of the message."
  (interactive)
  (with-current-buffer rmail-buffer
    (let ((rmail-delete-after-output nil))
      (if file-name
	  (rmail-output-body-to-file file-name)
	(call-interactively 'rmail-output-body-to-file))))
  (if rmail-delete-after-output
      (rmail-summary-delete-forward nil)))

;; Sorting messages in Rmail Summary buffer.

(defun rmail-summary-sort-by-date (reverse)
  "Sort messages of current Rmail summary by \"Date\" header.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-date) reverse))

(defun rmail-summary-sort-by-subject (reverse)
  "Sort messages of current Rmail summary by \"Subject\" header.
Ignores any \"Re: \" prefix.  If prefix argument REVERSE is
non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-subject) reverse))

(defun rmail-summary-sort-by-author (reverse)
  "Sort messages of current Rmail summary by author.
This uses either the \"From\" or \"Sender\" header, downcased.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-author) reverse))

(defun rmail-summary-sort-by-recipient (reverse)
  "Sort messages of current Rmail summary by recipient.
This uses either the \"To\" or \"Apparently-To\" header, downcased.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-recipient) reverse))

(defun rmail-summary-sort-by-correspondent (reverse)
  "Sort messages of current Rmail summary by other correspondent.
This uses either the \"From\", \"Sender\", \"To\", or
\"Apparently-To\" header, downcased.  Uses the first header not
excluded by `mail-dont-reply-to-names'.  If prefix argument
REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-correspondent) reverse))

(defun rmail-summary-sort-by-lines (reverse)
  "Sort messages of current Rmail summary by the number of lines.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P")
  (rmail-sort-from-summary (function rmail-sort-by-lines) reverse))

(defun rmail-summary-sort-by-labels (reverse labels)
  "Sort messages of current Rmail summary by labels.
LABELS is a comma-separated list of labels.
If prefix argument REVERSE is non-nil, sorts in reverse order."
  (interactive "P\nsSort by labels: ")
  (rmail-sort-from-summary
   (lambda (reverse) (rmail-sort-by-labels reverse labels))
   reverse))

(defun rmail-sort-from-summary (sortfun reverse)
  "Sort the Rmail buffer using sorting function SORTFUN.
Passes REVERSE to SORTFUN as its sole argument.  Then regenerates
the summary.  Note that the whole Rmail buffer is sorted, even if
the summary is only showing a subset of messages."
  (require 'rmailsort)
  (let ((selwin (selected-window)))
    (unwind-protect
	(progn (rmail-pop-to-buffer rmail-buffer)
	       (funcall sortfun reverse))
      (select-window selwin))))

(provide 'rmailsum)

;; Local Variables:
;; generated-autoload-file: "rmail.el"
;; End:

;;; rmailsum.el ends here
