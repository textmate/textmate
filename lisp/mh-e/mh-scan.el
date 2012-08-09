;;; mh-scan.el --- MH-E scan line constants and utilities

;; Copyright (C) 1993, 1995, 1997, 2000-2012  Free Software Foundation, Inc.

;; Author: Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;; This file contains constants and a few functions for interpreting
;; scan lines.

;;; Change Log:

;;; Code:

(require 'mh-e)



;;; Scan Formats

;; The following scan formats are passed to the scan program if the setting of
;; `mh-scan-format-file' is t. They are identical except the later one makes
;; use of the nmh `decode' function to decode RFC 2047 encodings. If you just
;; want to change the column of the notations, use the `mh-set-cmd-note'
;; function.

(defvar mh-scan-format-mh
  (concat
   "%4(msg)"
   "%<(cur)+%| %>"
   "%<{replied}-"
   "%?(nonnull(comp{to}))%<(mymbox{to})t%>"
   "%?(nonnull(comp{cc}))%<(mymbox{cc})c%>"
   "%?(nonnull(comp{bcc}))%<(mymbox{bcc})b%>"
   "%?(nonnull(comp{newsgroups}))n%>"
   "%<(zero) %>"
   "%02(mon{date})/%02(mday{date})%<{date} %|*%>"
   "%<(mymbox{from})%<{to}To:%14(friendly{to})%>%>"
   "%<(zero)%17(friendly{from})%>  "
   "%{subject}%<{body}<<%{body}%>")
  "*Scan format string for MH.
This string is passed to the scan program via the -format
argument.  This format is identical to the default except that
additional hints for fontification have been added to the fifth
column (remember that in Emacs, the first column is 0).

The values of the fifth column, in priority order, are: \"-\" if
the message has been replied to, t if an address on the To: line
matches one of the mailboxes of the current user, \"c\" if the Cc:
line matches, \"b\" if the Bcc: line matches, and \"n\" if a
non-empty Newsgroups: header is present.")

(defvar mh-scan-format-nmh
  (concat
   "%4(msg)"
   "%<(cur)+%| %>"
   "%<{replied}-"
   "%?(nonnull(comp{to}))%<(mymbox{to})t%>"
   "%?(nonnull(comp{cc}))%<(mymbox{cc})c%>"
   "%?(nonnull(comp{bcc}))%<(mymbox{bcc})b%>"
   "%?(nonnull(comp{newsgroups}))n%>"
   "%<(zero) %>"
   "%02(mon{date})/%02(mday{date})%<{date} %|*%>"
   "%<(mymbox{from})%<{to}To:%14(decode(friendly{to}))%>%>"
   "%<(zero)%17(decode(friendly{from}))%>  "
   "%(decode{subject})%<{body}<<%{body}%>")
  "*Scan format string for nmh.
This string is passed to the scan program via the -format arg.
This format is identical to the default except that additional
hints for fontification have been added to the fifth
column (remember that in Emacs, the first column is 0).

The values of the fifth column, in priority order, are: \"-\" if
the message has been replied to, t if an address on the To: field
matches one of the mailboxes of the current user, \"c\" if the Cc:
field matches, \"b\" if the Bcc: field matches, and \"n\" if a
non-empty Newsgroups: field is present.")



;;; Regular Expressions

;; Alphabetical.

(defvar mh-scan-body-regexp "\\(<<\\([^\n]+\\)?\\)"
  "This regular expression matches the message body fragment.

Note that the default setting of `mh-folder-font-lock-keywords'
expects this expression to contain at least one parenthesized
expression which matches the body text as in the default of
\"\\\\(<<\\\\([^\\n]+\\\\)?\\\\)\".  If this regular expression is
not correct, the body fragment will not be highlighted with the
face `mh-folder-body'.")

(defvar mh-scan-cur-msg-number-regexp "^\\( *[0-9]+\\+\\).*"
  "This regular expression matches the current message.

It must match from the beginning of the line.  Note that the
default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which
matches the message number as in the default of

  \"^\\\\( *[0-9]+\\\\+\\\\).*\".

This expression includes the leading space and current message
marker \"+\" within the parenthesis since it looks better to
highlight these items as well.  The highlighting is done with the
face `mh-folder-cur-msg-number'.  This regular expression should
be correct as it is needed by non-fontification functions.  See
also `mh-note-cur'.")

(defvar mh-scan-date-regexp "\\([0-9][0-9]/[0-9][0-9]\\)"
  "This regular expression matches a valid date.

It must not be anchored to the beginning or the end of the line.
Note that the default setting of `mh-folder-font-lock-keywords'
expects this expression to contain only one parenthesized
expression which matches the date field as in the default of
\"\\\\([0-9][0-9]/[0-9][0-9]\\\\)\"}.  If this regular expression
is not correct, the date will not be highlighted with the face
`mh-folder-date'.")

(defvar mh-scan-deleted-msg-regexp "^\\( *[0-9]+\\)D"
  "This regular expression matches deleted messages.

It must match from the beginning of the line.  Note that the
default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which
matches the message number as in the default of

  \"^\\\\( *[0-9]+\\\\)D\".

This expression includes the leading space within the parenthesis
since it looks better to highlight it as well.  The highlighting
is done with the face `mh-folder-deleted'.  This regular
expression should be correct as it is needed by non-fontification
functions.  See also `mh-note-deleted'.")

(defvar mh-scan-good-msg-regexp  "^\\( *[0-9]+\\)[^D^0-9]"
  "This regular expression matches \"good\" messages.

It must match from the beginning of the line.  Note that the
default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which
matches the message number as in the default of

  \"^\\\\( *[0-9]+\\\\)[^D^0-9]\".

This expression includes the leading space within the parenthesis
since it looks better to highlight it as well.  The highlighting
is done with the face `mh-folder-msg-number'.  This regular
expression should be correct as it is needed by non-fontification
functions.")

(defvar mh-scan-msg-format-regexp "%\\([0-9]*\\)(msg)"
  "This regular expression finds the message number width in a scan format.

Note that the message number must be placed in a parenthesized
expression as in the default of \"%\\\\([0-9]*\\\\)(msg)\".  This
variable is only consulted if `mh-scan-format-file' is set to
\"Use MH-E scan Format\".")

(defvar mh-scan-msg-format-string "%d"
  "This is a format string for width of the message number in a scan format.

Use \"0%d\" for zero-filled message numbers.  This variable is only
consulted if `mh-scan-format-file' is set to \"Use MH-E scan
Format\".")

(defvar mh-scan-msg-number-regexp "^ *\\([0-9]+\\)"
  "This regular expression extracts the message number.

It must match from the beginning of the line.  Note that the
message number must be placed in a parenthesized expression as in
the default of \"^ *\\\\([0-9]+\\\\)\".")

(defvar mh-scan-msg-overflow-regexp "^[?0-9][0-9]"
  "This regular expression matches overflowed message numbers.")

(defvar mh-scan-msg-search-regexp "^[^0-9]*%d[^0-9]"
  "This regular expression matches a particular message.

It is a format string; use \"%d\" to represent the location of the
message number within the expression as in the default of
\"^[^0-9]*%d[^0-9]\".")

(defvar mh-scan-rcpt-regexp  "\\(To:\\)\\(..............\\)"
  "This regular expression specifies the recipient in messages you sent.

Note that the default setting of `mh-folder-font-lock-keywords'
expects this expression to contain two parenthesized expressions.
The first is expected to match the \"To:\" that the default scan
format file generates.  The second is expected to match the
recipient's name as in the default of
\"\\\\(To:\\\\)\\\\(..............\\\\)\".  If this regular
expression is not correct, the \"To:\" string will not be
highlighted with the face `mh-folder-to' and the recipient will
not be highlighted with the face `mh-folder-address'")

(defvar mh-scan-refiled-msg-regexp  "^\\( *[0-9]+\\)\\^"
  "This regular expression matches refiled messages.

It must match from the beginning of the line.  Note that the
default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least one parenthesized expression which
matches the message number as in the default of

  \"^\\\\( *[0-9]+\\\\)\\\\^\".

This expression includes the leading space within the parenthesis
since it looks better to highlight it as well.  The highlighting
is done with the face `mh-folder-refiled'.  This regular
expression should be correct as it is needed by non-fontification
functions.  See also `mh-note-refiled'.")

(defvar mh-scan-sent-to-me-sender-regexp
  "^ *[0-9]+.\\([bct]\\).....[ ]*\\(..................\\)"
  "This regular expression matches messages sent to us.

Note that the default setting of `mh-folder-font-lock-keywords'
expects this expression to contain at least two parenthesized
expressions.  The first should match the fontification hint (see
`mh-scan-format-nmh') and the second should match the user name
as in the default of

  ^ *[0-9]+.\\\\([bct]\\\\).....[ ]*\\\\(..................\\\\)

If this regular expression is not correct, the notation hints
will not be highlighted with the face
`mh-mh-folder-sent-to-me-hint' and the sender will not be
highlighted with the face `mh-folder-sent-to-me-sender'.")

(defvar mh-scan-subject-regexp
  "^ *[0-9]+........[ ]*...................\\([Rr][Ee]\\(\\[[0-9]+\\]\\)?:\\s-*\\)*\\([^<\n]*\\)"
  "This regular expression matches the subject.

It must match from the beginning of the line.  Note that the
default setting of `mh-folder-font-lock-keywords' expects this
expression to contain at least three parenthesized expressions.
The first is expected to match the \"Re:\" string, if any, and is
highlighted with the face `mh-folder-followup'.  The second
matches an optional bracketed number after \"Re:\", such as in
\"Re[2]:\" (and is thus a sub-expression of the first expression)
and the third is expected to match the subject line itself which
is highlighted with the face `mh-folder-subject'.  For example,
the default (broken on multiple lines for readability) is

  ^ *[0-9]+........[ ]*...................
  \\\\([Rr][Ee]\\\\(\\\\\\=[[0-9]+\\\\]\\\\)?:\\\\s-*\\\\)*
  \\\\([^<\\n]*\\\\)

This regular expression should be correct as it is needed by
non-fontification functions.")

(defvar mh-scan-valid-regexp "^ *[0-9]"
  "This regular expression describes a valid scan line.

This is used to eliminate error messages that are occasionally
produced by \"inc\".")



;;; Widths, Offsets and Columns

(defvar mh-cmd-note 4
  "Column for notations.

This variable should be set with the function `mh-set-cmd-note'.
This variable may be updated dynamically if
`mh-adaptive-cmd-note-flag' is on.

Note that columns in Emacs start with 0.")
(make-variable-buffer-local 'mh-cmd-note)

(defvar mh-scan-cmd-note-width 1
  "Number of columns consumed by the cmd-note field in `mh-scan-format'.

This column will have one of the values: \" \", \"D\", \"^\", \"+\", where

  \" \" is the default value,
  \"D\" is the `mh-note-deleted' character,
  \"^\" is the `mh-note-refiled' character, and
  \"+\" is the `mh-note-cur' character.")

(defvar mh-scan-destination-width 1
  "Number of columns consumed by the destination field in `mh-scan-format'.

This column will have one of \" \", \"%\", \"-\", \"t\", \"c\", \"b\", or \"n\"
in it.

  \" \" blank space is the default character.
  \"%\" indicates that the message in a named MH sequence.
  \"-\" indicates that the message has been annotated with a replied field.
  \"t\" indicates that the message contains mymbox in the To: field.
  \"c\" indicates that the message contains mymbox in the Cc: field.
  \"b\" indicates that the message contains mymbox in the Bcc: field.
  \"n\" indicates that the message contains a Newsgroups: field.")

(defvar mh-scan-date-width 5
  "Number of columns consumed by the date field in `mh-scan-format'.
This column will typically be of the form mm/dd.")

(defvar mh-scan-date-flag-width 1
  "Number of columns consumed to flag (in)valid dates in `mh-scan-format'.
This column will have \" \" for valid and \"*\" for invalid or
missing dates.")

(defvar mh-scan-from-mbox-width 17
  "Number of columns consumed with the \"From:\" line in `mh-scan-format'.
This column will have a friendly name or e-mail address of the
originator, or a \"To: address\" for outgoing e-mail messages.")

(defvar mh-scan-from-mbox-sep-width 2
  "Number of columns consumed by whitespace after from-mbox in `mh-scan-format'.
This column will only ever have spaces in it.")

(defvar mh-scan-field-destination-offset
  (+ mh-scan-cmd-note-width)
  "The offset from the `mh-cmd-note' for the destination column.")

(defvar mh-scan-field-from-start-offset
  (+ mh-scan-cmd-note-width
     mh-scan-destination-width
     mh-scan-date-width
     mh-scan-date-flag-width)
  "The offset from the `mh-cmd-note' to find the start of \"From:\" address.")

(defvar mh-scan-field-from-end-offset
  (+ mh-scan-field-from-start-offset mh-scan-from-mbox-width)
  "The offset from the `mh-cmd-note' to find the end of \"From:\" address.")

(defvar mh-scan-field-subject-start-offset
  (+ mh-scan-cmd-note-width
     mh-scan-destination-width
     mh-scan-date-width
     mh-scan-date-flag-width
     mh-scan-from-mbox-width
     mh-scan-from-mbox-sep-width)
  "The offset from the `mh-cmd-note' to find the start of the subject.")



;;; Notation

;; Alphabetical.

(defvar mh-note-cur ?+
  "The current message (in MH, not in MH-E) is marked by this character.
See also `mh-scan-cur-msg-number-regexp'.")

(defvar mh-note-copied ?C
  "Messages that have been copied are marked by this character.")

(defvar mh-note-deleted ?D
  "Messages that have been deleted are marked by this character.
See also `mh-scan-deleted-msg-regexp'.")

(defvar mh-note-dist ?R
  "Messages that have been redistributed are marked by this character.")

(defvar mh-note-forw ?F
  "Messages that have been forwarded are marked by this character.")

(defvar mh-note-printed ?P
  "Messages that have been printed are marked by this character.")

(defvar mh-note-refiled ?^
  "Messages that have been refiled are marked by this character.
See also `mh-scan-refiled-msg-regexp'.")

(defvar mh-note-repl ?-
  "Messages that have been replied to are marked by this character.")

(defvar mh-note-seq ?%
  "Messages in a user-defined sequence are marked by this character.

Messages in the \"search\" sequence are marked by this character as
well.")



;;; Utilities

;;;###mh-autoload
(defun mh-scan-msg-number-regexp ()
  "Return value of variable `mh-scan-msg-number-regexp'."
  mh-scan-msg-number-regexp)

;;;###mh-autoload
(defun mh-scan-msg-search-regexp ()
  "Return value of variable `mh-scan-msg-search-regexp'."
  mh-scan-msg-search-regexp)

;;;###mh-autoload
(defun mh-set-cmd-note (column)
  "Set `mh-cmd-note' to COLUMN.
Note that columns in Emacs start with 0."
  (setq mh-cmd-note column))

;;;###mh-autoload
(defun mh-scan-format ()
  "Return the output format argument for the scan program."
  (if (equal mh-scan-format-file t)
      (list "-format" (if (mh-variant-p 'nmh 'gnu-mh)
                          (list (mh-update-scan-format
                                 mh-scan-format-nmh mh-cmd-note))
                        (list (mh-update-scan-format
                               mh-scan-format-mh mh-cmd-note))))
    (if (not (equal mh-scan-format-file nil))
        (list "-form" mh-scan-format-file))))

(defun mh-update-scan-format (fmt width)
  "Return a scan format with the (msg) width in the FMT replaced with WIDTH.

The message number width portion of the format is discovered
using `mh-scan-msg-format-regexp'.  Its replacement is controlled
with `mh-scan-msg-format-string'."
  (or (and
       (string-match mh-scan-msg-format-regexp fmt)
       (let ((begin (match-beginning 1))
             (end (match-end 1)))
         (concat (substring fmt 0 begin)
                 (format mh-scan-msg-format-string width)
                 (substring fmt end))))
      fmt))

;;;###mh-autoload
(defun mh-msg-num-width (folder)
  "Return the width of the largest message number in this FOLDER."
  (or mh-progs (mh-find-path))
  (let ((tmp-buffer (get-buffer-create mh-temp-buffer))
        (width 0))
    (with-current-buffer tmp-buffer
      (erase-buffer)
      (apply 'call-process
             (expand-file-name mh-scan-prog mh-progs) nil '(t nil) nil
             (list folder "last" "-format" "%(msg)"))
      (goto-char (point-min))
      (if (re-search-forward mh-scan-msg-number-regexp nil 0 1)
          (setq width (length (buffer-substring
                               (match-beginning 1) (match-end 1))))))
    width))

;;;###mh-autoload
(defun mh-msg-num-width-to-column (width)
  "Return the column for notations given message number WIDTH.
Note that columns in Emacs start with 0.

If `mh-scan-format-file' is set to \"Use MH-E scan Format\" this
means that either `mh-scan-format-mh' or `mh-scan-format-nmh' are
in use.  This function therefore assumes that the first column is
empty (to provide room for the cursor), the following WIDTH
columns contain the message number, and the column for notations
comes after that."
  (if (eq mh-scan-format-file t)
      (max (1+ width) 2)
    (error "%s %s" "Can't call `mh-msg-num-width-to-column' when"
           "`mh-scan-format-file' is not set to \"Use MH-E scan Format\"")))

(provide 'mh-scan)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-scan.el ends here
