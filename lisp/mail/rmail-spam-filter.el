;;; rmail-spam-filter.el --- spam filter for Rmail, the Emacs mail reader

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.
;; Keywords: email, spam, filter, rmail
;; Author: Eli Tziperman <eli AT deas.harvard.edu>
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
;;; -----------

;;; Automatically recognize and delete junk email before it is
;;; displayed in rmail/rmail-summary.  Spam emails are defined by
;;; specifying one or more of the sender, subject and contents.
;;; URL: http://www.weizmann.ac.il/~eli/Downloads/rmail-spam-filter/

;;; Usage:
;;; ------

;;; put in your .emacs:

;;; (require 'rmail-spam-filter)

;;; and use customize (in rmail-spam-filter group) to:

;;; (*) turn on the variable rmail-use-spam-filter,

;;; (*) specify in variable rsf-definitions-alist what sender,
;;; subject and contents make an email be considered spam.

;;; in addition, you may:

;;; (*) Block future mail with the subject or sender of a message
;;; while reading it in RMAIL: just click on the "Spam" item on the
;;; menubar, and add the subject or sender to the list of spam
;;; definitions using the mouse and the appropriate menu item. You
;;; need to later also save the list of spam definitions using the
;;; same menu item, or alternatively, see variable
;;; `rsf-autosave-newly-added-definitions'.

;;; (*) specify if blind-cc'ed mail (no "To:" header field) is to be
;;; treated as spam (variable rsf-no-blind-cc; Thanks to Ethan
;;; Brown <ethan@gso.saic.com> for this).

;;; (*) specify if rmail-spam-filter should ignore case of spam
;;; definitions (variable rsf-ignore-case; Thanks to
;;; Ethan Brown <ethan@gso.saic.com> for the suggestion).

;;; (*) Specify a "white-list" of trusted senders. If any
;;; rsf-white-list string matches a substring of the "From"
;;; header, the message is flagged as a valid, non-spam message (Ethan
;;; Brown <ethan@gso.saic.com>).

;;; (*) rmail-spam-filter is best used with a general purpose spam
;;; filter such as the procmail-based http://www.spambouncer.org/.
;;; Spambouncer is set to only mark messages as spam/blocked/bulk/OK
;;; via special headers, and these headers may then be defined in
;;; rmail-spam-filter such that the spam is rejected by
;;; rmail-spam-filter itself.

(require 'rmail)
(require 'rmailsum)

(defgroup rmail-spam-filter nil
  "Spam filter for Rmail, the Emacs mail reader."
  :group 'rmail)

(defcustom rmail-use-spam-filter nil
  "Non-nil to activate the Rmail spam filter.
Set `rsf-definitions-alist' to define what you consider spam emails."
  :type 'boolean
  :group 'rmail-spam-filter)

(defcustom rsf-file "~/XRMAIL-SPAM"
  "Name of Rmail file for optionally saving some of the spam.
You can either just delete spam, or save it in this file for
later review.  Which action to take for each spam definition is
specified by the \"action\" element of the definition."
  :type 'string
  :group 'rmail-spam-filter)

(defcustom rsf-no-blind-cc nil
  "Non-nil means mail with no explicit To: or Cc: is spam."
  :type 'boolean
  :group 'rmail-spam-filter)

(defcustom rsf-ignore-case nil
  "Non-nil means to ignore case in `rsf-definitions-alist'."
  :type 'boolean
  :group 'rmail-spam-filter)

(defcustom rsf-beep nil
  "Non-nil means to beep if spam is found."
  :type 'boolean
  :group 'rmail-spam-filter)

(defcustom rsf-sleep-after-message 2.0
  "Seconds to wait after displaying a message that spam was found."
  :type 'number
  :group 'rmail-spam-filter)

(defcustom rsf-min-region-to-spam-list 7
  "Minimum size of region that you can add to the spam list.
The aim is to avoid adding too short a region, which could result
in false positive identification of a valid message as spam."
  :type 'integer
  :group 'rmail-spam-filter)

(defcustom rsf-autosave-newly-added-definitions nil
  "Non-nil to auto-save new spam entries.
Any time you add an entry via the \"Spam\" menu, immediately saves
the custom file."
  :type 'boolean
  :group 'rmail-spam-filter)

(defcustom rsf-white-list nil
  "List of regexps to identify valid senders.
If any element matches the \"From\" header, the message is
flagged as a valid, non-spam message.  E.g., if your domain is
\"emacs.com\" then including \"emacs\\\\.com\" in this list would
flag all mail (purporting to be) from your colleagues as valid."
  :type '(repeat string)
  :group 'rmail-spam-filter)

(defcustom rsf-definitions-alist nil
  "A list of rules (definitions) matching spam messages.
Each rule is an alist, with elements of the form (FIELD . REGEXP).
The recognized FIELDS are: from, to, subject, content-type,
x-spam-status, and contents.  The \"contents\" element refers to
the entire text of the message; all the other elements refer to
message headers of the same name.

Using an empty-string for REGEXP is the same as omitting that
element altogether.

Each rule should contain one \"action\" element, saying what to do
if the rule is matched.  This has the form (action . CHOICE), where
CHOICE may be either `output-and-delete' (save to `rsf-file', then delete),
or `delete-spam' (just delete).

A rule matches only if all the specified elements match."
  :type '(repeat
          (list :format "%v"
	   (cons :format "%v" :value (from . "")
		 (const :format ""  from)
		 (string :tag "From"  ""))
	   (cons :format "%v" :value (to . "")
		 (const :format ""  to)
		 (string :tag "To"  ""))
	   (cons :format "%v" :value (subject . "")
		 (const :format ""  subject)
		 (string :tag "Subject"  ""))
	   (cons :format "%v" :value (content-type . "")
		 (const :format ""  content-type)
		 (string :tag "Content-Type"  ""))
	   (cons :format "%v" :value (contents . "")
		 (const :format ""  contents)
		 (string :tag "Contents"  ""))
	   (cons :format "%v" :value (x-spam-status . "")
		 (const :format ""  x-spam-status)
		 (string :tag "X-Spam-Status"  ""))
	   (cons :format "%v" :value (action . output-and-delete)
		 (const :format "" action)
		 (choice :tag "Action selection"
		  (const :tag "Output and delete" output-and-delete)
		  (const :tag "Delete" delete-spam)
		  ))))
  :group 'rmail-spam-filter)

;; FIXME nothing uses this, and it could just be let-bound.
(defvar rsf-scanning-messages-now nil
  "Non-nil when `rmail-spam-filter' scans messages.")

;; the advantage over the automatic filter definitions is the AND conjunction
;; of in-one-definition-elements
(defun rsf-check-field (field-symbol message-data definition result)
  "Check if a message appears to be spam.
FIELD-SYMBOL is one of the possible keys of a `rsf-definitions-alist'
rule; e.g. from, to.  MESSAGE-DATA is a string giving the value of
FIELD-SYMBOL in the current message.  DEFINITION is the element of
`rsf-definitions-alist' currently being checked.

RESULT is a cons of the form (MAYBE-SPAM . IS-SPAM).  If the car
is nil, or if the entry for FIELD-SYMBOL in this DEFINITION is
absent or the empty string, this function does nothing.

Otherwise, if MESSAGE-DATA is non-nil and the entry matches it,
the cdr is set to t.  Else, the car is set to nil."
  (let ((definition-field (cdr (assoc field-symbol definition))))
    ;; Only in this case can maybe-spam change from t to nil.
    (if (and (car result) (> (length definition-field) 0))
        ;; If FIELD-SYMBOL field appears in the message, and also in
        ;; spam definition list, this is potentially a spam.
        (if (and message-data
                 (string-match definition-field message-data))
            ;; If we do not get a contradiction from another field, this is spam
            (setcdr result t)
          ;; The message data contradicts the specification, this is not spam.
          ;; Note that the total absence of a header specified in the
          ;; rule means this cannot be spam.
          (setcar result nil)))))

(defun rmail-spam-filter (msg)
  "Return nil if message number MSG is spam based on `rsf-definitions-alist'.
If spam, optionally output message to a file `rsf-file' and delete
it from rmail file.  Called for each new message retrieved by
`rmail-get-new-mail'."
  (let ((return-value)
	;; maybe-spam is in the car, this-is-a-spam-email in cdr.
	(maybe-spam '(nil . nil))
	message-sender message-to message-cc message-recipients
	message-subject message-content-type message-spam-status
	(num-spam-definition-elements (safe-length rsf-definitions-alist))
	(num-element 0)
	(exit-while-loop nil)
	;; Do we want to ignore case in spam definitions.
	(case-fold-search rsf-ignore-case)
	;; make sure bbdb does not create entries for messages while spam
	;; filter is scanning the rmail file:
	(bbdb/mail_auto_create_p nil)
	;; Other things may wish to know if we are running (nothing
	;; uses this at present).
	(rsf-scanning-messages-now t))
    (save-excursion
      ;; Narrow buffer to header of message and get Sender and
      ;; Subject fields to be used below:
      (save-restriction
        (goto-char (rmail-msgbeg msg))
        (narrow-to-region (point) (progn (search-forward "\n\n") (point)))
        (setq message-sender (mail-fetch-field "From"))
        (setq message-to (mail-fetch-field "To")
              message-cc (mail-fetch-field "Cc")
              message-recipients (or (and message-to message-cc
                                          (concat message-to ", " message-cc))
                                     message-to
                                     message-cc))
        (setq message-subject (mail-fetch-field "Subject"))
        (setq message-content-type (mail-fetch-field "Content-Type"))
        (setq message-spam-status (mail-fetch-field "X-Spam-Status")))
      ;; Check for blind CC condition.  Set vars such that while
      ;; loop will be bypassed and spam condition will trigger.
      (and rsf-no-blind-cc
           (null message-recipients)
           (setq exit-while-loop t
                 maybe-spam '(t . t)))
      ;; Check white list, and likewise cause while loop bypass.
      (and message-sender
           (let ((white-list rsf-white-list)
                 (found nil))
             (while (and (not found) white-list)
               (if (string-match (car white-list) message-sender)
                   (setq found t)
                 (setq white-list (cdr white-list))))
             found)
           (setq exit-while-loop t
                 maybe-spam '(nil . nil)))
      ;; Scan all elements of the list rsf-definitions-alist.
      (while (and (< num-element num-spam-definition-elements)
                  (not exit-while-loop))
        (let ((definition (nth num-element rsf-definitions-alist)))
          ;; Initialize car, which is set to t in one of two cases:
          ;; (1) unspecified definition-elements are found in
          ;; rsf-definitions-alist, (2) empty field is found in the
          ;; message being scanned (e.g. empty subject, sender,
          ;; recipients, etc).  It is set to nil if a non-empty field
          ;; of the scanned message does not match a specified field
          ;; in rsf-definitions-alist.
          ;; FIXME the car is never set to t?!

          ;; Initialize cdr to nil.  This is set to t if one of the
          ;; spam definitions matches a field in the scanned message.
          (setq maybe-spam (cons t nil))

          ;; Maybe the different fields should also be done in a
          ;; loop to make the whole thing more flexible.

          ;; If sender field is not specified in message being
          ;; scanned, AND if "from" field does not appear in spam
          ;; definitions for this element, this may still be spam due
          ;; to another element...
          (rsf-check-field 'from message-sender definition maybe-spam)
          ;; Next, if spam was not ruled out already, check recipients:
          (rsf-check-field 'to message-recipients definition maybe-spam)
          ;; Next, if spam was not ruled out already, check subject:
          (rsf-check-field 'subject message-subject definition maybe-spam)
          ;; Next, if spam was not ruled out already, check content-type:
          (rsf-check-field 'content-type message-content-type
                           definition maybe-spam)
          ;; Next, if spam was not ruled out already, check contents:
          ;; If contents field is not specified, this may still be
          ;; spam due to another element...
          (rsf-check-field 'contents
                           (buffer-substring-no-properties
                            (rmail-msgbeg msg) (rmail-msgend msg))
                           definition maybe-spam)

          ;; Finally, check the X-Spam-Status header.  You will typically
          ;; look for the "Yes" string in this header field.
          (rsf-check-field 'x-spam-status message-spam-status
                           definition maybe-spam)

          ;; If the search in rsf-definitions-alist found
          ;; that this email is spam, output the email to the spam
          ;; rmail file, mark the email for deletion, leave the
          ;; while loop and return nil so that an rmail summary line
          ;; won't be displayed for this message: (FIXME ?)
          (if (and (car maybe-spam) (cdr maybe-spam))
              (setq exit-while-loop t)
            ;; Else, spam was not yet found, proceed to next element
            ;; in rsf-definitions-alist:
            (setq num-element (1+ num-element)))))

      (if (and (car maybe-spam) (cdr maybe-spam))
          ;; Temporarily set rmail-current-message in order to output
          ;; and delete the spam msg if needed:
          (let ((rmail-current-message msg) ; FIXME does this do anything?
                (action (cdr (assq 'action
                                   (nth num-element rsf-definitions-alist))))
                (newfile (not (file-exists-p rsf-file))))
            ;; Check action item in rsf-definitions-alist and do it.
            (cond
             ((eq action 'output-and-delete)
              ;; Else the prompt to write a new file leaves the raw
              ;; mbox buffer visible.
              (and newfile
                   (rmail-show-message (rmail-first-unseen-message) t))
              (rmail-output rsf-file)
              ;; Swap back, else rmail-get-new-mail-1 gets confused.
              (when newfile
                (rmail-swap-buffers-maybe)
                (widen))
              ;; Don't delete if automatic deletion after output is on.
              (or rmail-delete-after-output (rmail-delete-message)))
             ((eq action 'delete-spam)
              (rmail-delete-message)))
            (setq return-value nil))
        (setq return-value t)))
    return-value))

(defun rmail-get-new-mail-filter-spam (nnew)
  "Check the most NNEW recent messages for spam.
This is called at the end of `rmail-get-new-mail-1' if there is new mail."
  (let* ((nold (- rmail-total-messages nnew))
	 (nspam 0)
	 (nscan (1+ nold))
	 ;; Save the original deleted state of all the messages.
	 (rdv-old rmail-deleted-vector)
	 errflag)
    ;; Set all messages undeleted so that the expunge only affects spam.
    (setq rmail-deleted-vector (make-string (1+ rmail-total-messages) ?\s))
    (while (and (not errflag) (<= nscan rmail-total-messages))
      (condition-case nil
	  (or (rmail-spam-filter nscan)
	      (setq nspam (1+ nspam)))
	(error (setq errflag nscan)))
      (setq nscan (1+ nscan)))
    (unwind-protect
	(if errflag
	    (progn
	      (setq rmail-use-spam-filter nil)
	      (if rsf-beep (ding t))
	      (message "Spam filter error for new message %d, disabled" errflag)
	      (sleep-for rsf-sleep-after-message))
	  (when (> nspam 0)
	    ;; Otherwise sleep or expunge prompt leaves raw mbox buffer showing.
	    (rmail-show-message (or (rmail-first-unseen-message) 1) t)
	    (unwind-protect
		(progn
		  (if rsf-beep (ding t))
		  (message "Rmail spam-filter detected and deleted %d spam \
message%s"
			   nspam (if (= 1 nspam) "" "s"))
		  (sleep-for rsf-sleep-after-message)
		  (if (rmail-expunge-confirmed) (rmail-only-expunge t)))
	      ;; Swap back, else get-new-mail-1 gets confused.
	      (rmail-swap-buffers-maybe)
	      (widen))))
      ;; Restore the original deleted state.  Character N refers to message N.
      (setq rmail-deleted-vector
	    (concat (substring rdv-old 0 (1+ nold))
		    ;; This still works if we deleted all the new mail.
		    (substring rmail-deleted-vector (1+ nold)))))
    ;; Return a message based on the number of spam messages found.
    (cond
     (errflag ", error in spam filter")
     ((zerop nspam) "")
     ((= 1 nnew) ", and it appears to be spam")
     ((= nspam nnew) ", and all appear to be spam")
     (t (format ", and %d appear%s to be spam" nspam
		(if (= 1 nspam) "s" ""))))))

;; define functions for interactively adding sender/subject of a
;; specific message to the spam definitions while reading it, using
;; the menubar:
(defun rsf-add-subject-to-spam-list ()
  "Add the \"Subject\" header to the spam list."
  (interactive)
  (let ((message-subject (regexp-quote (rmail-get-header "Subject"))))
    ;; Note the use of a backquote and comma on the subject line here,
    ;; to make sure message-subject is actually evaluated and its value
    ;; substituted.
    (add-to-list 'rsf-definitions-alist
		 ;; Note that an empty element is treated the same as
		 ;; an absent one, so why does it bother to add them?
		 (list '(from . "")
		       '(to . "")
		       `(subject . ,message-subject)
		       '(content-type . "")
		       '(contents . "")
		       '(action . output-and-delete))
		 t)
    (customize-mark-to-save 'rsf-definitions-alist)
    (if rsf-autosave-newly-added-definitions
	(progn
	  (custom-save-all)
          (message "Added subject `%s' to spam list, and saved it"
                   message-subject))
      (message "Added subject `%s' to spam list (remember to save it)"
               message-subject))))

(defun rsf-add-sender-to-spam-list ()
  "Add the \"From\" address to the spam list."
  (interactive)
  (let ((message-sender (regexp-quote (rmail-get-header "From"))))
    (add-to-list 'rsf-definitions-alist
		 (list `(from . ,message-sender)
		       '(to . "")
		       '(subject . "")
		       '(content-type . "")
		       '(contents . "")
		       '(action . output-and-delete))
		 t)
    (customize-mark-to-save 'rsf-definitions-alist)
    (if rsf-autosave-newly-added-definitions
	(progn
	  (custom-save-all)
          (message "Added sender `%s' to spam list, and saved it"
                   message-sender))
      (message "Added sender `%s' to spam list (remember to save it)"
               message-sender))))

(defun rsf-add-region-to-spam-list ()
  "Add the marked region in the Rmail buffer to the spam list.
Adds to spam definitions as a \"contents\" field."
  (interactive)
  (set-buffer rmail-buffer)
  ;; Check if region is inactive or has zero size.
  (if (not (and mark-active (not (= (region-beginning) (region-end)))))
      ;; If inactive, print error message.
      (message "You must highlight some text in the Rmail buffer")
    (if (< (- (region-end) (region-beginning)) rsf-min-region-to-spam-list)
        (message "Region is too small (minimum %d characters)"
                 rsf-min-region-to-spam-list)
      ;; If region active and long enough, add to list of spam definitions.
      (let ((region-to-spam-list (regexp-quote
                                  (buffer-substring-no-properties
                                   (region-beginning) (region-end)))))
        (add-to-list 'rsf-definitions-alist
                     (list '(from . "")
                           '(to . "")
                           '(subject . "")
                           '(content-type . "")
                           `(contents . ,region-to-spam-list)
                           '(action . output-and-delete))
                     t)
        (customize-mark-to-save 'rsf-definitions-alist)
        (if rsf-autosave-newly-added-definitions
            (progn
              (custom-save-all)
              (message "Added highlighted text:\n%s\n\
to the spam list, and saved it" region-to-spam-list))
          (message "Added highlighted text:\n%s\n\
to the spam list (remember to save it)" region-to-spam-list))))))

(defun rsf-customize-spam-definitions ()
  "Customize `rsf-definitions-alist'."
  (interactive)
  (customize-variable 'rsf-definitions-alist))

(defun rsf-customize-group ()
  "Customize the rmail-spam-filter group."
  (interactive)
  (customize-group 'rmail-spam-filter))

(defun rsf-custom-save-all ()
  "Interactive version of `custom-save-all'."
  (interactive)
  (custom-save-all))

;; Add menu items (and keyboard shortcuts) to both rmail and rmail-summary.
(dolist (map (list rmail-summary-mode-map rmail-mode-map))
  (easy-menu-define nil map nil
    '("Spam"
      ["Add subject to spam list" rsf-add-subject-to-spam-list]
      ["Add sender to spam list"  rsf-add-sender-to-spam-list]
      ["Add region to spam list"  rsf-add-region-to-spam-list]
      ["Save spam definitions"    rsf-custom-save-all]
      "--"
      ["Customize spam definitions" rsf-customize-spam-definitions]
      ["Browse spam customizations" rsf-customize-group]
      ))
  (define-key map "\C-cSt" 'rsf-add-subject-to-spam-list)
  (define-key map "\C-cSr" 'rsf-add-sender-to-spam-list)
  (define-key map "\C-cSn" 'rsf-add-region-to-spam-list)
  (define-key map "\C-cSa" 'rsf-custom-save-all)
  (define-key map "\C-cSd" 'rsf-customize-spam-definitions)
  (define-key map "\C-cSg" 'rsf-customize-group))

(defun rsf-add-content-type-field ()
  "Maintain backward compatibility for `rmail-spam-filter'.
The most recent version of `rmail-spam-filter' checks the content-type
field of the incoming mail to see if it is spam.  The format of
`rsf-definitions-alist' has therefore changed.  This function
checks to see if the old format is used, and updates it if necessary."
  (interactive)
  (if (and rsf-definitions-alist
           (not (assoc 'content-type (car rsf-definitions-alist))))
      (let ((result nil)
            (current nil)
            (definitions rsf-definitions-alist))
        (while definitions
          (setq current (car definitions))
          (setq definitions (cdr definitions))
          (setq result
                (append result
                        (list
                         (list (assoc 'from current)
                               (assoc 'to current)
                               (assoc 'subject current)
                               (cons 'content-type "")
                               (assoc 'contents current)
                               (assoc 'action current))))))
        (setq rsf-definitions-alist result)
        (customize-mark-to-save 'rsf-definitions-alist)
        (if rsf-autosave-newly-added-definitions
            (progn
              (custom-save-all)
              (message "Spam definitions converted to new format, and saved"))
          (message "Spam definitions converted to new format (remember to save)")))))

(provide 'rmail-spam-filter)

;;; rmail-spam-filter ends here
