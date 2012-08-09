;;; rmailout.el --- "RMAIL" mail reader for Emacs: output message to a file

;; Copyright (C) 1985, 1987, 1993-1994, 2001-2012
;;   Free Software Foundation, Inc.

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

;;; Code:

(require 'rmail)
(provide 'rmailout)

(defcustom rmail-output-decode-coding nil
  "If non-nil, do coding system decoding when outputting message as Babyl."
  :type 'boolean
  :group 'rmail-output)

;; FIXME risky?
(defcustom rmail-output-file-alist nil
  "Alist matching regexps to suggested output Rmail files.
This is a list of elements of the form (REGEXP . NAME-EXP).
The suggestion is taken if REGEXP matches anywhere in the message buffer.
NAME-EXP may be a string constant giving the file name to use,
or more generally it may be any kind of expression that returns
a file name as a string."
  :type '(repeat (cons regexp
		       (choice :value ""
			       (string :tag "File Name")
			       sexp)))
  :group 'rmail-output)
;;;###autoload(put 'rmail-output-file-alist 'risky-local-variable t)

(defcustom rmail-fields-not-to-output nil
  "Regexp describing fields to exclude when outputting a message to a file.
The function `rmail-delete-unwanted-fields' uses this, ignoring case."
  :type '(choice (const :tag "None" nil)
		 regexp)
  :group 'rmail-output)

(defun rmail-output-read-file-name ()
  "Read the file name to use for `rmail-output'.
Set `rmail-default-file' to this name as well as returning it."
  (let ((default-file
	  (let (answer tail)
	    (setq tail rmail-output-file-alist)
	    ;; Suggest a file based on a pattern match.
	    (while (and tail (not answer))
	      (save-excursion
		(goto-char (point-min))
		(if (re-search-forward (car (car tail)) nil t)
		    (setq answer (eval (cdr (car tail)))))
		(setq tail (cdr tail))))
	    ;; If no suggestion, use same file as last time.
	    (or answer rmail-default-file))))
    (let ((read-file
	   (expand-file-name
	    (read-file-name
	     (concat "Output message to mail file (default "
		     (file-name-nondirectory default-file)
		     "): ")
	     (file-name-directory default-file)
	     (abbreviate-file-name default-file))
	    (file-name-directory default-file))))
      (setq rmail-default-file
	    (if (file-directory-p read-file)
		(expand-file-name (file-name-nondirectory default-file)
				  read-file)
	      (expand-file-name
	       (or read-file (file-name-nondirectory default-file))
	       (file-name-directory default-file)))))))

(defun rmail-delete-unwanted-fields (preserve)
  "Delete all headers matching `rmail-fields-not-to-output'.
Retains headers matching the regexp PRESERVE.  Ignores case.
The buffer should be narrowed to just the header."
  (if rmail-fields-not-to-output
      (save-excursion
	(goto-char (point-min))
	(let ((case-fold-search t))
	  (while (re-search-forward rmail-fields-not-to-output nil t)
	    (beginning-of-line)
	    (unless (looking-at preserve)
	      (delete-region (point) (line-beginning-position 2))))))))

(defun rmail-output-as-babyl (file-name nomsg)
  "Convert the current buffer's text to Babyl and output to FILE-NAME.
Alters the current buffer's text, so it should be a temporary buffer.
If a buffer is visiting FILE-NAME, adds the text to that buffer
rather than saving the file directly.  If the buffer is an Rmail buffer,
updates it accordingly.  If no buffer is visiting FILE-NAME, appends
the text directly to FILE-NAME, and displays a \"Wrote file\" message
unless NOMSG is a symbol (neither nil nor t)."
  (let ((coding-system-for-write 'emacs-mule-unix))
    (save-restriction
      (goto-char (point-min))
      (search-forward "\n\n" nil 'move)
      (narrow-to-region (point-min) (point))
      (if rmail-fields-not-to-output
	  (rmail-delete-unwanted-fields nil)))

    ;; Convert to Babyl format.
    (rmail-convert-to-babyl-format)
    ;; Write it into the file, or its buffer.
    (let ((buf (find-buffer-visiting file-name))
	  (tembuf (current-buffer)))
      (if (null buf)
	  (write-region (point-min) (point-max) file-name t nomsg)
	(if (eq buf (current-buffer))
	    (error "Can't output message to same file it's already in"))
	;; File has been visited, in buffer BUF.
	(set-buffer buf)
	(let ((inhibit-read-only t)
	      (msg (bound-and-true-p rmail-current-message)))
	  ;; If MSG is non-nil, buffer is in RMAIL mode.
	  (if msg
	      (rmail-output-to-babyl-buffer tembuf msg)
	    ;; Output file not in rmail mode => just insert at the end.
	    (narrow-to-region (point-min) (1+ (buffer-size)))
	    (goto-char (point-max))
	    (insert-buffer-substring tembuf)))))))

;; Called only if rmail-summary-exists, which means rmailsum is loaded.
(declare-function rmail-update-summary "rmailsum" (&rest ignore))

(defun rmail-output-to-babyl-buffer (tembuf msg)
  "Copy message in TEMBUF into the current Babyl Rmail buffer.
Do what is necessary to make Rmail know about the new message, then
display message number MSG."
  ;; Turn on Auto Save mode, if it's off in this buffer but enabled by default.
  (and (not buffer-auto-save-file-name)
       auto-save-default
       (auto-save-mode t))
  (rmail-maybe-set-message-counters)
  (widen)
  (narrow-to-region (point-max) (point-max))
  (insert-buffer-substring tembuf)
  (goto-char (point-min))
  (widen)
  (search-backward "\n\^_")
  (narrow-to-region (point) (point-max))
  (rmail-count-new-messages t)
  (if (rmail-summary-exists)
      (rmail-select-summary (rmail-update-summary)))
  (rmail-show-message-1 msg))

(defun rmail-convert-to-babyl-format ()
  "Convert the mbox message in the current buffer to Babyl format."
  (let ((count 0) (start (point-min))
	(case-fold-search nil)
	(buffer-undo-list t))
    (goto-char (point-min))
    (save-restriction
      (unless (looking-at "^From ")
	(error "Invalid mbox message"))
      (insert "\^L\n0,,\n*** EOOH ***\n")
      (rmail-nuke-pinhead-header)
      ;; Decode base64 or quoted printable contents, Rmail style.
      (let* ((header-end (save-excursion
			   (and (re-search-forward "\n\n" nil t)
				(1- (point)))))
	     (case-fold-search t)
	     (quoted-printable-header-field-end
	      (save-excursion
		(re-search-forward
		 "^content-transfer-encoding:\\(\n?[\t ]\\)*quoted-printable\\(\n?[\t ]\\)*"
		 header-end t)))
	     (base64-header-field-end
	      (and
	       ;; Don't decode non-text data.
	       (save-excursion
		 (re-search-forward
		  "^content-type:\\(\n?[\t ]\\)\\(text\\|message\\)/"
		  header-end t))
	       (save-excursion
		 (re-search-forward
		  "^content-transfer-encoding:\\(\n?[\t ]\\)*base64\\(\n?[\t ]\\)*"
		  header-end t)))))

	(goto-char (point-max))
	(if quoted-printable-header-field-end
	    (save-excursion
	      (unless (mail-unquote-printable-region
		       header-end (point) nil t t)
		(message "Malformed MIME quoted-printable message"))
	      ;; Change "quoted-printable" to "8bit",
	      ;; to reflect the decoding we just did.
	      (goto-char quoted-printable-header-field-end)
	      (delete-region (point) (search-backward ":"))
	      (insert ": 8bit")))
	(if base64-header-field-end
	    (save-excursion
	      (when (condition-case nil
			(progn
			  (base64-decode-region
			   (1+ header-end)
			   (save-excursion
			     ;; Prevent base64-decode-region
			     ;; from removing newline characters.
			     (skip-chars-backward "\n\t ")
			     (point)))
			  t)
		      (error nil))
		;; Change "base64" to "8bit", to reflect the
		;; decoding we just did.
		(goto-char base64-header-field-end)
		(delete-region (point) (search-backward ":"))
		(insert ": 8bit")))))
      ;; Transform anything within the message text
      ;; that might appear to be the end of a Babyl-format message.
      (save-excursion
	(save-restriction
	  (narrow-to-region start (point))
	  (goto-char (point-min))
	  (while (search-forward "\n\^_" nil t) ; single char
	    (replace-match "\n^_"))))		; 2 chars: "^" and "_"
      ;; This is for malformed messages that don't end in newline.
      ;; There shouldn't be any, but some users say occasionally
      ;; there are some.
      (or (bolp) (newline))
      (insert ?\^_)
      (setq last-coding-system-used nil)
      ;; Decode coding system, following specs in the message header,
      ;; and record what coding system was decoded.
      (if rmail-output-decode-coding
	  (let ((mime-charset
		 (if (save-excursion
		       (goto-char start)
		       (search-forward "\n\n" nil t)
		       (let ((case-fold-search t))
			 (re-search-backward
			  rmail-mime-charset-pattern
			  start t)))
		     (intern (downcase (match-string 1))))))
	    (rmail-decode-region start (point) mime-charset)))
      (save-excursion
	(goto-char start)
	(forward-line 3)
	(insert "X-Coding-System: "
		(symbol-name last-coding-system-used)
		"\n")))))

(defun rmail-nuke-pinhead-header ()
  "Delete the \"From \" line in the current mbox message.
The variable `rmail-unix-mail-delimiter' specifies the From line format.
Replaces the From line with a \"Mail-from\" header.  Adds \"Date\" and
\"From\" headers if they are not already present."
  (save-excursion
    (save-restriction
      (let ((start (point))
  	    (end (progn
		   (condition-case ()
		       (search-forward "\n\n")
		     (error
		      (goto-char (point-max))
		      (insert "\n\n")))
		   (point)))
	    has-from has-date)
	(narrow-to-region start end)
	(let ((case-fold-search t))
	  (goto-char start)
	  (setq has-from (search-forward "\nFrom:" nil t))
	  (goto-char start)
	  (setq has-date (and (search-forward "\nDate:" nil t) (point)))
	  (goto-char start))
	(let ((case-fold-search nil))
	  (if (re-search-forward (concat "^" rmail-unix-mail-delimiter) nil t)
	      (replace-match
		(concat
		  "Mail-from: \\&"
		  ;; Keep and reformat the date if we don't
		  ;;  have a Date: field.
		  (if has-date
		      ""
		    (concat
		     "Date: \\2, \\4 \\3 \\9 \\5 "

		     ;; The timezone could be matched by group 7 or group 10.
		     ;; If neither of them matched, assume EST, since only
		     ;; Easterners would be so sloppy.
		     ;; It's a shame the substitution can't use "\\10".
		     (cond
		      ((/= (match-beginning 7) (match-end 7)) "\\7")
		      ((/= (match-beginning 10) (match-end 10))
		       (buffer-substring (match-beginning 10)
					 (match-end 10)))
		      (t "EST"))
		     "\n"))
		  ;; Keep and reformat the sender if we don't
		  ;; have a From: field.
		  (if has-from
		      ""
		    "From: \\1\n"))
		t)))))))

(autoload 'mail-mbox-from "mail-utils")

(defun rmail-output-as-mbox (file-name nomsg &optional as-seen)
  "Convert the current buffer's text to mbox and output to FILE-NAME.
Alters the current buffer's text, so it should be a temporary buffer.
If a buffer is visiting FILE-NAME, adds the text to that buffer
rather than saving the file directly.  If the buffer is an Rmail buffer,
updates it accordingly.  If no buffer is visiting FILE-NAME, appends
the text directly to FILE-NAME, and displays a \"Wrote file\" message
unless NOMSG is a symbol (neither nil nor t).
AS-SEEN is non-nil if we are copying the message \"as seen\"."
  (let ((case-fold-search t)
	from date)
    (goto-char (point-min))
    ;; Preserve the Mail-From and MIME-Version fields
    ;; even if they have been pruned.
    (search-forward "\n\n" nil 'move)
    (narrow-to-region (point-min) (point))
    (rmail-delete-unwanted-fields
     (if rmail-enable-mime "Mail-From"
       "Mail-From\\|MIME-Version\\|Content-type"))
    (goto-char (point-min))
    (or (looking-at "From ")
	(insert (mail-mbox-from)))
    (widen)
    ;; Make sure message ends with blank line.
    (goto-char (point-max))
    (rmail-ensure-blank-line)
    (goto-char (point-min))
    (let ((buf (find-buffer-visiting file-name))
	  (tembuf (current-buffer)))
      (if (null buf)
	  (let ((coding-system-for-write 'raw-text-unix))
	    ;; FIXME should ensure existing file ends with a blank line.
	    (write-region (point-min) (point-max) file-name t nomsg))
	(if (eq buf (current-buffer))
	    (error "Can't output message to same file it's already in"))
	;; File has been visited, in buffer BUF.
	(set-buffer buf)
 	(let ((inhibit-read-only t)
	      (msg (and (boundp 'rmail-current-message)
			rmail-current-message)))
	  (and msg as-seen
	       (error "Can't output \"as seen\" to a visited Rmail file"))
	  (if msg
	      (rmail-output-to-rmail-buffer tembuf msg)
	    ;; Output file not in Rmail mode => just insert at the end.
	    (narrow-to-region (point-min) (1+ (buffer-size)))
	    (goto-char (point-max))
	    (insert-buffer-substring tembuf)))))))

(defun rmail-output-to-rmail-buffer (tembuf msg)
  "Copy message in TEMBUF into the current Rmail buffer.
Do what is necessary to make Rmail know about the new message. then
display message number MSG."
  (save-excursion
    (rmail-swap-buffers-maybe)
    (rmail-modify-format)
    ;; Turn on Auto Save mode, if it's off in this buffer but enabled
    ;; by default.
    (and (not buffer-auto-save-file-name)
	 auto-save-default
	 (auto-save-mode t))
    (rmail-maybe-set-message-counters)
    ;; Insert the new message after the last old message.
    (widen)
    (unless (zerop (buffer-size))
      ;; Make sure the last old message ends with a blank line.
      (goto-char (point-max))
      (rmail-ensure-blank-line)
      ;; Insert the new message at the end.
      (narrow-to-region (point-max) (point-max)))
    (insert-buffer-substring tembuf)
    (rmail-count-new-messages t)
    ;; FIXME should re-use existing windows.
    (if (rmail-summary-exists)
	(rmail-select-summary (rmail-update-summary)))
    (rmail-show-message-1 msg)))

;;; There are functions elsewhere in Emacs that use this function;
;;; look at them before you change the calling method.
;;;###autoload
(defun rmail-output (file-name &optional count noattribute not-rmail)
  "Append this message to mail file FILE-NAME.
Writes mbox format, unless FILE-NAME exists and is Babyl format, in which
case it writes Babyl.

Interactively, the default file name comes from `rmail-default-file',
which is updated to the name you use in this command.  In all uses, if
FILE-NAME is not absolute, it is expanded with the directory part of
`rmail-default-file'.

If a buffer is visiting FILE-NAME, adds the text to that buffer
rather than saving the file directly.  If the buffer is an Rmail
buffer, updates it accordingly.

This command always outputs the complete message header, even if
the header display is currently pruned.

Optional prefix argument COUNT (default 1) says to output that
many consecutive messages, starting with the current one (ignoring
deleted messages).  If `rmail-delete-after-output' is non-nil, deletes
messages after output.

The optional third argument NOATTRIBUTE, if non-nil, says not to
set the `filed' attribute, and not to display a \"Wrote file\"
message (if writing a file directly).

Set the optional fourth argument NOT-RMAIL non-nil if you call this
from a non-Rmail buffer.  In this case, COUNT is ignored."
  (interactive
   (list (rmail-output-read-file-name)
	 (prefix-numeric-value current-prefix-arg)))
  (or count (setq count 1))
  (setq file-name
	(expand-file-name file-name
			  (and rmail-default-file
			       (file-name-directory rmail-default-file))))
  ;; Warn about creating new file.
  (or (find-buffer-visiting file-name)
      (file-exists-p file-name)
      (yes-or-no-p (concat "\"" file-name "\" does not exist, create it? "))
      (error "Output file does not exist"))
  (if noattribute (setq noattribute 'nomsg))
  (let ((babyl-format (and (file-readable-p file-name)
			   (mail-file-babyl-p file-name)))
	(cur (current-buffer))
	(buf (find-buffer-visiting file-name)))

    ;; If a babyl file is visited in a buffer, is it visited as babyl
    ;; or as mbox?
    (and babyl-format buf
	 (with-current-buffer buf
	   (save-restriction
	     (widen)
	     (save-excursion
	       (goto-char (point-min))
	       (setq babyl-format
		     (looking-at "BABYL OPTIONS:"))))))

    (if not-rmail		 ; eg via message-fcc-handler-function
	(with-temp-buffer
	  (insert-buffer-substring cur)
	  ;; Output in the appropriate format.
	  (if babyl-format
	      (progn
		(goto-char (point-min))
		;; rmail-convert-to-babyl-format errors if no From line,
		;; whereas rmail-output-as-mbox inserts one.
		(or (looking-at "From ")
		    (insert (mail-mbox-from)))
		(rmail-output-as-babyl file-name noattribute))
	    (rmail-output-as-mbox file-name noattribute)))
      ;; Called from an Rmail buffer.
      (if rmail-buffer
	  (set-buffer rmail-buffer)
	(error "There is no Rmail buffer"))
      (if (zerop rmail-total-messages)
	  (error "No messages to output"))
      (let ((orig-count count)
	    beg end)
	(while (> count 0)
	  (setq beg (rmail-msgbeg rmail-current-message)
		end (rmail-msgend rmail-current-message))
	  ;; All access to the buffer's local variables is now finished...
	  (save-excursion
	    ;; ... so it is ok to go to a different buffer.
	    (if (rmail-buffers-swapped-p) (set-buffer rmail-view-buffer))
	    (setq cur (current-buffer))
	    (save-restriction
	      (widen)
	      (with-temp-buffer
		(insert-buffer-substring cur beg end)
		(if babyl-format
		    (rmail-output-as-babyl file-name noattribute)
		  (rmail-output-as-mbox file-name noattribute)))))
	  (or noattribute		; mark message as "filed"
	      (rmail-set-attribute rmail-filed-attr-index t))
	  (setq count (1- count))
	  (let ((next-message-p
		 (if rmail-delete-after-output
		     (rmail-delete-forward)
		   (if (> count 0)
		       (rmail-next-undeleted-message 1))))
		(num-appended (- orig-count count)))
	    (if (and (> count 0) (not next-message-p))
		(error "Only %d message%s appended" num-appended
		       (if (= num-appended 1) "" "s")))))))))

;; FIXME nothing outside uses this, so NOT-RMAIL could be dropped.
;; FIXME this duplicates code from rmail-output.
;;;###autoload
(defun rmail-output-as-seen (file-name &optional count noattribute not-rmail)
  "Append this message to mbox file named FILE-NAME.
The details are as for `rmail-output', except that:
  i) the header is output as currently seen
 ii) this function cannot write to Babyl files
iii) an Rmail buffer cannot be visiting FILE-NAME

Note that if NOT-RMAIL is non-nil, there is no difference between this
function and `rmail-output'.  This argument may be removed in future,
so you should call `rmail-output' directly in that case."
  (interactive
   (list (rmail-output-read-file-name)
	 (prefix-numeric-value current-prefix-arg)))
  (if not-rmail
      (rmail-output file-name count noattribute not-rmail)
    (or count (setq count 1))
    (setq file-name
	  (expand-file-name file-name
			    (and rmail-default-file
				 (file-name-directory rmail-default-file))))
    ;; Warn about creating new file.
    (or (find-buffer-visiting file-name)
	(file-exists-p file-name)
	(yes-or-no-p (concat "\"" file-name "\" does not exist, create it? "))
	(error "Output file does not exist"))
    ;; FIXME why not?
    (if (and (file-readable-p file-name) (mail-file-babyl-p file-name))
	(error "Cannot output `as seen' to a Babyl file"))
    (if noattribute (setq noattribute 'nomsg))
    (if rmail-buffer
	(set-buffer rmail-buffer)
      (error "There is no Rmail buffer"))
    (if (zerop rmail-total-messages)
	(error "No messages to output"))
    (let ((orig-count count)
	  (cur (current-buffer)))
      (while (> count 0)
	(let (beg end)
	  ;; If operating from whole-mbox buffer, get message bounds.
	  (or (rmail-buffers-swapped-p)
	      (setq beg (rmail-msgbeg rmail-current-message)
		    end (rmail-msgend rmail-current-message)))
	  (save-restriction
	    (widen)
	    ;; If operating from the view buffer, get the bounds.
	    (or beg
		(setq beg (point-min)
		      end (point-max)))
	    (with-temp-buffer
	      (insert-buffer-substring cur beg end)
	      (rmail-output-as-mbox file-name noattribute t))))
	(or noattribute		; mark message as "filed"
	    (rmail-set-attribute rmail-filed-attr-index t))
	(setq count (1- count))
	(let ((next-message-p
	       (if rmail-delete-after-output
		   (rmail-delete-forward)
		 (if (> count 0)
		     (rmail-next-undeleted-message 1))))
	      (num-appended (- orig-count count)))
	  (if (and (> count 0) (not next-message-p))
	      (error "Only %d message%s appended" num-appended
		     (if (= num-appended 1) "" "s"))))))))


;;;###autoload
(defun rmail-output-body-to-file (file-name)
  "Write this message body to the file FILE-NAME.
Interactively, the default file name comes from either the message
\"Subject\" header, or from `rmail-default-body-file'.  Updates the value
of `rmail-default-body-file' accordingly.  In all uses, if FILE-NAME
is not absolute, it is expanded with the directory part of
`rmail-default-body-file'.

Note that this overwrites FILE-NAME (after confirmation), rather
than appending to it.  Deletes the message after writing if
`rmail-delete-after-output' is non-nil."
  (interactive
   (let ((default-file
	   (or (mail-fetch-field "Subject")
	       rmail-default-body-file)))
     (setq default-file
	   (replace-regexp-in-string ":" "-" default-file))
     (setq default-file
	   (replace-regexp-in-string " " "-" default-file))
     (list (setq rmail-default-body-file
		 (read-file-name
		  "Output message body to file: "
		  (and default-file (file-name-directory default-file))
		  default-file
		  nil default-file)))))
  (setq file-name
	(expand-file-name file-name
			  (and rmail-default-body-file
			       (file-name-directory rmail-default-body-file))))
  (if (zerop rmail-current-message)
      (error "No message to output"))
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n")
    (and (file-exists-p file-name)
	 (not (y-or-n-p (format "File %s exists; overwrite? " file-name)))
	 (error "Operation aborted"))
    (write-region (point) (point-max) file-name))
  (if rmail-delete-after-output
      (rmail-delete-forward)))

;;; rmailout.el ends here
