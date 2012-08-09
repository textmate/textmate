;;; mh-comp.el --- MH-E functions for composing and sending messages

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

;; This file includes the functions in the MH-Folder maps that get us
;; into MH-Letter mode, as well the functions in the MH-Letter mode
;; that are used to send the mail. Other that those, functions that
;; are needed in mh-letter.el should be found there.

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'mh-gnus)                      ;needed because mh-gnus.el not compiled
(require 'mh-scan)

(require 'sendmail)

(autoload 'easy-menu-add "easymenu")
(autoload 'mml-insert-tag "mml")



;;; Site Customization

(defvar mh-send-prog "send"
  "Name of the MH send program.
Some sites need to change this because of a name conflict.")

(defvar mh-send-uses-spost-flag nil
  "Non-nil means \"send\" uses \"spost\" to submit messages.

If the value of \"postproc:\" is \"spost\", you may need to set
this variable to t to tell MH-E to avoid using features of
\"post\" that are not supported by \"spost\". You'll know that
you'll need to do this if sending mail fails with an error of
\"spost: -msgid unknown\".")

(defvar mh-redist-background nil
  "If non-nil redist will be done in background like send.
This allows transaction log to be visible if -watch, -verbose or
-snoop are used.")



;;; Variables

(defvar mh-comp-formfile "components"
  "Name of file to be used as a skeleton for composing messages.

Default is \"components\".

If not an absolute file name, the file is searched for first in the
user's MH directory, then in the system MH lib directory.")

(defvar mh-repl-formfile "replcomps"
  "Name of file to be used as a skeleton for replying to messages.

Default is \"replcomps\".

If not an absolute file name, the file is searched for first in the
user's MH directory, then in the system MH lib directory.")

(defvar mh-repl-group-formfile "replgroupcomps"
  "Name of file to be used as a skeleton for replying to messages.

Default is \"replgroupcomps\".

This file is used to form replies to the sender and all recipients of
a message. Only used if `(mh-variant-p 'nmh)' is non-nil.
If not an absolute file name, the file is searched for first in the
user's MH directory, then in the system MH lib directory.")

(defvar mh-rejected-letter-start
  (format "^%s$"
          (regexp-opt
           '("Content-Type: message/rfc822" ;MIME MDN
             "------ This is a copy of the message, including all the headers. ------";from exim
             "--- Below this line is a copy of the message."; from qmail
             "   ----- Unsent message follows -----" ;from sendmail V5
             " --------Unsent Message below:" ; from sendmail at BU
             "   ----- Original message follows -----" ;from sendmail V8
             "------- Unsent Draft"     ;from MH itself
             "----------  Original Message  ----------" ;from zmailer
             "  --- The unsent message follows ---" ;from AIX mail system
             "    Your message follows:" ;from MMDF-II
             "Content-Description: Returned Content" ;1993 KJ sendmail
             ))))

(defvar mh-new-draft-cleaned-headers
  "^Date:\\|^Received:\\|^Message-Id:\\|^From:\\|^Sender:\\|^Errors-To:\\|^Delivery-Date:\\|^Return-Path:"
  "Regexp of header lines to remove before offering a message as a new draft\\<mh-folder-mode-map>.
Used by the \\[mh-edit-again] and \\[mh-extract-rejected-mail] commands.")

(defvar mh-letter-mode-syntax-table
  (let ((syntax-table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?% "." syntax-table)
    syntax-table)
  "Syntax table used by MH-E while in MH-Letter mode.")

(defvar mh-send-args ""
  "Extra args to pass to \"send\" command.")

(defvar mh-annotate-char nil
  "Character to use to annotate `mh-sent-from-msg'.")

(defvar mh-annotate-field nil
  "Field name for message annotation.")

(defvar mh-annotate-list nil
  "Messages annotated, either a sequence name or a list of message numbers.
This variable can be used by `mh-annotate-msg-hook'.")

(defvar mh-insert-auto-fields-done-local nil
  "Buffer-local variable set when `mh-insert-auto-fields' called successfully.")
(make-variable-buffer-local 'mh-insert-auto-fields-done-local)



;;; MH-E Entry Points

;;;###autoload
(defun mh-smail ()
  "Compose a message with the MH mail system.
See `mh-send' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send))

;;;###autoload
(defun mh-smail-other-window ()
  "Compose a message with the MH mail system in other window.
See `mh-send' for more details on composing mail."
  (interactive)
  (mh-find-path)
  (call-interactively 'mh-send-other-window))

(defun mh-send-other-window (to cc subject)
  "Compose a message in another window.

See `mh-send' for more information and a description of how the
TO, CC, and SUBJECT arguments are used."
  (interactive (list
                (mh-interactive-read-address "To: ")
                (mh-interactive-read-address "Cc: ")
                (mh-interactive-read-string "Subject: ")))
  (let ((pop-up-windows t))
    (mh-send-sub to cc subject (current-window-configuration))))

(defvar mh-error-if-no-draft nil)       ;raise error over using old draft

;;;###autoload
(defun mh-smail-batch (&optional to subject other-headers &rest ignored)
  "Compose a message with the MH mail system.

This function does not prompt the user for any header fields, and
thus is suitable for use by programs that want to create a mail
buffer. Users should use \\[mh-smail] to compose mail.

Optional arguments for setting certain fields include TO,
SUBJECT, and OTHER-HEADERS. Additional arguments are IGNORED.

This function remains for Emacs 21 compatibility. New
applications should use `mh-user-agent-compose'."
  (mh-find-path)
  (let ((mh-error-if-no-draft t))
    (mh-send (or to "") "" (or subject ""))))

;;;###autoload
(define-mail-user-agent 'mh-e-user-agent
  'mh-user-agent-compose 'mh-send-letter 'mh-fully-kill-draft
  'mh-before-send-letter-hook)

;;;###autoload
(defun mh-user-agent-compose (&optional to subject other-headers continue
                                        switch-function yank-action
                                        send-actions return-action
                                        &rest ignored)
  "Set up mail composition draft with the MH mail system.
This is the `mail-user-agent' entry point to MH-E. This function
conforms to the contract specified by `define-mail-user-agent'
which means that this function should accept the same arguments
as `compose-mail'.

The optional arguments TO and SUBJECT specify recipients and the
initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional header fields.
Elements look like (HEADER . VALUE) where both HEADER and VALUE
are strings.

CONTINUE, SWITCH-FUNCTION, YANK-ACTION, SEND-ACTIONS, and
RETURN-ACTION and any additional arguments are IGNORED."
  (mh-find-path)
  (let ((mh-error-if-no-draft t))
    (mh-send to "" subject)
    (while other-headers
      (mh-insert-fields (concat (car (car other-headers)) ":")
                        (cdr (car other-headers)))
      (setq other-headers (cdr other-headers)))))

;; Shush compiler.
(mh-do-in-xemacs
  (defvar sendmail-coding-system))

;;;###autoload
(defun mh-send-letter (&optional arg)
  "Save draft and send message.

When you are all through editing a message, you send it with this
command. You can give a prefix argument ARG to monitor the first stage
of the delivery\; this output can be found in a buffer called \"*MH-E
Mail Delivery*\".

The hook `mh-before-send-letter-hook' is run at the beginning of
this command. For example, if you want to check your spelling in
your message before sending, add the function `ispell-message'.

Unless `mh-insert-auto-fields' had previously been called
manually, the function `mh-insert-auto-fields' is called to
insert fields based upon the recipients. If fields are added, you
are given a chance to see and to confirm these fields before the
message is actually sent. You can do away with this confirmation
by turning off the option `mh-auto-fields-prompt-flag'.

In case the MH \"send\" program is installed under a different name,
use `mh-send-prog' to tell MH-E the name.

The hook `mh-annotate-msg-hook' is run after annotating the
message and scan line."
  (interactive "P")
  (run-hooks 'mh-before-send-letter-hook)
  (if (and (mh-insert-auto-fields t)
           mh-auto-fields-prompt-flag
           (goto-char (point-min)))
      (if (not (y-or-n-p "Auto fields inserted, send? "))
          (error "Send aborted")))
  (cond ((mh-mh-directive-present-p)
         (mh-mh-to-mime))
        ((or (mh-mml-tag-present-p) (not (mh-ascii-buffer-p)))
         (mh-mml-to-mime)))
  (save-buffer)
  (message "Sending...")
  (let ((draft-buffer (current-buffer))
        (file-name buffer-file-name)
        (config mh-previous-window-config)
        (coding-system-for-write
         (if (and (local-variable-p 'buffer-file-coding-system
                                    (current-buffer)) ;XEmacs needs two args
                  ;; We're not sure why, but buffer-file-coding-system
                  ;; tends to get set to undecided-unix.
                  (not (memq buffer-file-coding-system
                             '(undecided undecided-unix undecided-dos))))
             buffer-file-coding-system
           (or (and (boundp 'sendmail-coding-system) sendmail-coding-system)
               (and (default-boundp 'buffer-file-coding-system)
                    (default-value 'buffer-file-coding-system))
               'iso-latin-1))))
    ;; Older versions of spost do not support -msgid and -mime.
    (unless mh-send-uses-spost-flag
      ;; Adding a Message-ID field looks good, makes it easier to search for
      ;; message in your +outbox, and best of all doesn't break threading for
      ;; the recipient if you reply to a message in your +outbox.
      (setq mh-send-args (concat "-msgid " mh-send-args))
      ;; The default BCC encapsulation will make a MIME message unreadable.
      ;; With nmh use the -mime arg to prevent this.
      (if (and (mh-variant-p 'nmh)
               (mh-goto-header-field "Bcc:")
               (mh-goto-header-field "Content-Type:"))
          (setq mh-send-args (concat "-mime " mh-send-args))))
    (cond (arg
           (pop-to-buffer mh-mail-delivery-buffer)
           (erase-buffer)
           (mh-exec-cmd-output mh-send-prog t
                               "-nodraftfolder" "-watch" "-nopush"
                               (split-string mh-send-args) file-name)
           (goto-char (point-max))      ; show the interesting part
           (recenter -1)
           (set-buffer draft-buffer))   ; for annotation below
          (t
           (mh-exec-cmd-daemon mh-send-prog nil
                               "-nodraftfolder" "-noverbose"
                               (split-string mh-send-args) file-name)))
    (if mh-annotate-char
        (mh-annotate-msg mh-sent-from-msg
                         mh-sent-from-folder
                         mh-annotate-char
                         "-component" mh-annotate-field
                         "-text" (format "\"%s %s\""
                                         (mh-get-header-field "To:")
                                         (mh-get-header-field "Cc:"))))

    (cond ((or (not arg)
               (y-or-n-p "Kill draft buffer? "))
           (kill-buffer draft-buffer)
           (if config
               (set-window-configuration config))))
    (if arg
        (message "Sending...done")
      (message "Sending...backgrounded"))))

;;;###autoload
(defun mh-fully-kill-draft ()
  "Quit editing and delete draft message.

If for some reason you are not happy with the draft, you can use
this command to kill the draft buffer and delete the draft
message. Use the command \\[kill-buffer] if you don't want to
delete the draft message."
  (interactive)
  (if (y-or-n-p "Kill draft message? ")
      (let ((config mh-previous-window-config))
        (if (file-exists-p buffer-file-name)
            (delete-file buffer-file-name))
        (set-buffer-modified-p nil)
        (kill-buffer (buffer-name))
        (message "")
        (if config
            (set-window-configuration config)))
    (error "Message not killed")))



;;; MH-Folder Commands

;; Alphabetical.

;;;###mh-autoload
(defun mh-edit-again (message)
  "Edit a MESSAGE to send it again.

If you don't complete a draft for one reason or another, and if
the draft buffer is no longer available, you can pick your draft
up again with this command. If you don't use a draft folder, your
last \"draft\" file will be used. If you use draft folders,
you'll need to visit the draft folder with \"\\[mh-visit-folder]
drafts <RET>\", use \\[mh-next-undeleted-msg] to move to the
appropriate message, and then use \\[mh-edit-again] to prepare
the message for editing.

This command can also be used to take messages that were sent to
you and to send them to more people.

Don't use this command to re-edit a message from a Mailer-Daemon
who complained that your mail wasn't posted for some reason or
another (see `mh-extract-rejected-mail').

The default message is the current message.

See also `mh-send'."
  (interactive (list (mh-get-msg-num t)))
  (let* ((from-folder mh-current-folder)
         (config (current-window-configuration))
         (draft
          (cond ((and mh-draft-folder (equal from-folder mh-draft-folder))
                 (pop-to-buffer (find-file-noselect (mh-msg-filename message))
                                t)
                 (rename-buffer (format "draft-%d" message))
                 ;; Make buffer writable...
                 (setq buffer-read-only nil)
                 ;; If buffer was being used to display the message reinsert
                 ;; from file...
                 (when (eq major-mode 'mh-show-mode)
                   (erase-buffer)
                   (insert-file-contents buffer-file-name))
                 (buffer-name))
                (t
                 (mh-read-draft "clean-up" (mh-msg-filename message) nil)))))
    (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil)
    (mh-insert-header-separator)
    (goto-char (point-min))
    (save-buffer)
    (mh-compose-and-send-mail draft "" from-folder nil nil nil nil nil nil
                              config)
    (mh-letter-mode-message)
    (mh-letter-adjust-point)))

;;;###mh-autoload
(defun mh-extract-rejected-mail (message)
  "Edit a MESSAGE that was returned by the mail system.

This command prepares the message for editing by removing the
Mailer-Daemon envelope and unneeded header fields. Fix whatever
addressing problem you had, and send the message again with
\\[mh-send-letter].

The default message is the current message.

See also `mh-send'."
  (interactive (list (mh-get-msg-num t)))
  (let ((from-folder mh-current-folder)
        (config (current-window-configuration))
        (draft (mh-read-draft "extraction" (mh-msg-filename message) nil)))
    (goto-char (point-min))
    (cond ((re-search-forward mh-rejected-letter-start nil t)
           (skip-chars-forward " \t\n")
           (delete-region (point-min) (point))
           (mh-clean-msg-header (point-min) mh-new-draft-cleaned-headers nil))
          (t
           (message "Does not appear to be a rejected letter")))
    (mh-insert-header-separator)
    (goto-char (point-min))
    (save-buffer)
    (mh-compose-and-send-mail draft "" from-folder message
                              (mh-get-header-field "To:")
                              (mh-get-header-field "From:")
                              (mh-get-header-field "Cc:")
                              nil nil config)
    (mh-letter-mode-message)))

;;;###mh-autoload
(defun mh-forward (to cc &optional range)
  "Forward message.

You are prompted for the TO and CC recipients. You are given a
draft to edit that looks like it would if you had run the MH
command \"forw\". You can then add some text.

You can forward several messages by using a RANGE. All of the
messages in the range are inserted into your draft. Check the
documentation of `mh-interactive-range' to see how RANGE is read
in interactive use.

The hook `mh-forward-hook' is called on the draft.

See also `mh-compose-forward-as-mime-flag',
`mh-forward-subject-format', and `mh-send'."
  (interactive (list (mh-interactive-read-address "To: ")
                     (mh-interactive-read-address "Cc: ")
                     (mh-interactive-range "Forward")))
  (let* ((folder mh-current-folder)
         (msgs (mh-range-to-msg-list range))
         (config (current-window-configuration))
         (fwd-msg-file (mh-msg-filename (car msgs) folder))
         ;; forw always leaves file in "draft" since it doesn't have -draft
         (draft-name (expand-file-name "draft" mh-user-path))
         (draft (cond ((or (not (file-exists-p draft-name))
                           (y-or-n-p "The file draft exists; discard it? "))
                       (mh-exec-cmd "forw" "-build"
                                    (if (and (mh-variant-p 'nmh)
                                             mh-compose-forward-as-mime-flag)
                                        "-mime")
                                    mh-current-folder
                                    (mh-coalesce-msg-list msgs))
                       (prog1
                           (mh-read-draft "" draft-name t)
                         (mh-insert-fields "To:" to "Cc:" cc)
                         (save-buffer)))
                      (t
                       (mh-read-draft "" draft-name nil)))))
    (let (orig-from
          orig-subject)
      (with-current-buffer (get-buffer-create mh-temp-buffer)
        (erase-buffer)
        (insert-file-contents fwd-msg-file)
        (setq orig-from (mh-get-header-field "From:"))
        (setq orig-subject (mh-get-header-field "Subject:")))
      (let ((forw-subject
             (mh-forwarded-letter-subject orig-from orig-subject)))
        (mh-insert-fields "Subject:" forw-subject)
        (goto-char (point-min))
        ;; If using MML, translate MH-style directive
        (if (equal mh-compose-insertion 'mml)
            (save-excursion
              (goto-char (mh-mail-header-end))
              (while
                  (re-search-forward
                   "^#forw \\[\\([^]]+\\)\\] \\(+\\S-+\\) \\(.*\\)$"
                   (point-max) t)
                (let ((description (if (equal (match-string 1)
                                              "forwarded messages")
                                       "forwarded message %d"
                                     (match-string 1)))
                      (msgs (split-string (match-string 3)))
                      (i 0))
                  (beginning-of-line)
                  (delete-region (point) (progn (forward-line 1) (point)))
                  (dolist (msg msgs)
                    (setq i (1+ i))
                    (mh-mml-forward-message (format description i)
                                            folder msg)
                    ;; Was inserted before us, move to end of file to preserve order
                    (goto-char (point-max)))))))
        ;; Position just before forwarded message.
        (if (re-search-forward "^------- Forwarded Message" nil t)
            (forward-line -1)
          (goto-char (mh-mail-header-end))
          (forward-line 1))
        (delete-other-windows)
        (mh-add-msgs-to-seq msgs 'forwarded t)
        (mh-compose-and-send-mail draft "" folder msgs
                                  to forw-subject cc
                                  mh-note-forw "Forwarded:"
                                  config)
        (mh-letter-mode-message)
        (mh-letter-adjust-point)
        (run-hooks 'mh-forward-hook)))))

(defun mh-forwarded-letter-subject (from subject)
  "Return a Subject suitable for a forwarded message.
Original message has headers FROM and SUBJECT."
  (let ((addr-start (string-match "<" from))
        (comment (string-match "(" from)))
    (cond ((and addr-start (> addr-start 0))
           ;; Full Name <luser@host>
           (setq from (substring from 0 (1- addr-start))))
          (comment
           ;; luser@host (Full Name)
           (setq from (substring from (1+ comment) (1- (length from)))))))
  (format mh-forward-subject-format from subject))

;;;###mh-autoload
(defun mh-redistribute (to cc &optional message)
  "Redistribute a message.

This command is similar in function to forwarding mail, but it
does not allow you to edit the message, nor does it add your name
to the \"From\" header field. It appears to the recipient as if
the message had come from the original sender. When you run this
command, you are prompted for the TO and CC recipients. The
default MESSAGE is the current message.

Also investigate the command \\[mh-edit-again] for another way to
redistribute messages.

See also `mh-redist-full-contents-flag'.

The hook `mh-annotate-msg-hook' is run after annotating the
message and scan line."
  (interactive (list (mh-read-address "Redist-To: ")
                     (mh-read-address "Redist-Cc: ")
                     (mh-get-msg-num t)))
  (or message
      (setq message (mh-get-msg-num t)))
  (save-window-excursion
    (let ((folder mh-current-folder)
          (draft (mh-read-draft "redistribution"
                                (if mh-redist-full-contents-flag
                                    (mh-msg-filename message)
                                  nil)
                                nil)))
      (mh-goto-header-end 0)
      (insert "Resent-To: " to "\n")
      (if (not (equal cc "")) (insert "Resent-cc: " cc "\n"))
      (mh-clean-msg-header
       (point-min)
       "^Message-Id:\\|^Received:\\|^Return-Path:\\|^Sender:\\|^Date:\\|^From:"
       nil)
      (save-buffer)
      (message "Redistributing...")
      (let ((env "mhdist=1"))
        ;; Setup environment...
        (setq env (concat env " mhaltmsg="
                          (if mh-redist-full-contents-flag
                              buffer-file-name
                            (mh-msg-filename message folder))))
        (unless mh-redist-full-contents-flag
          (setq env (concat env " mhannotate=1")))
        ;; Redistribute...
        (if mh-redist-background
            (mh-exec-cmd-env-daemon env mh-send-prog nil buffer-file-name)
          (mh-exec-cmd-error env mh-send-prog "-push" buffer-file-name))
        ;; Annotate...
        (mh-annotate-msg message folder mh-note-dist
                         "-component" "Resent:"
                         "-text" (format "\"%s %s\"" to cc)))
      (kill-buffer draft)
      (message "Redistributing...done"))))

;;;###mh-autoload
(defun mh-reply (message &optional reply-to includep)
  "Reply to a MESSAGE.

When you reply to a message, you are first prompted with \"Reply
to whom?\" (unless the optional argument REPLY-TO is provided).
You have several choices here.

     Response     Reply Goes To

     from         The person who sent the message. This is the
                  default, so <RET> is sufficient.

     to           Replies to the sender, plus all recipients in the
                  \"To:\" header field.

     all cc       Forms a reply to the addresses in the
                  \"Mail-Followup-To:\" header field if one
                  exists; otherwise forms a reply to the sender,
                  plus all recipients.

Depending on your answer, \"repl\" is given a different argument
to form your reply. Specifically, a choice of \"from\" or none at
all runs \"repl -nocc all\", and a choice of \"to\" runs \"repl
-cc to\". Finally, either \"cc\" or \"all\" runs \"repl -cc all
-nocc me\".

Two windows are then created. One window contains the message to
which you are replying in an MH-Show buffer. Your draft, in
MH-Letter mode (*note `mh-letter-mode'), is in the other window.
If the reply draft was not one that you expected, check the
things that affect the behavior of \"repl\" which include the
\"repl:\" profile component and the \"replcomps\" and
\"replgroupcomps\" files.

If you supply a prefix argument INCLUDEP, the message you are
replying to is inserted in your reply after having first been run
through \"mhl\" with the format file \"mhl.reply\".

Alternatively, you can customize the option `mh-yank-behavior'
and choose one of its \"Automatically\" variants to do the same
thing. If you do so, the prefix argument has no effect.

Another way to include the message automatically in your draft is
to use \"repl: -filter repl.filter\" in your MH profile.

If you wish to customize the header or other parts of the reply
draft, please see \"repl\" and \"mh-format\".

See also `mh-reply-show-message-flag',
`mh-reply-default-reply-to', and `mh-send'."
  (interactive (list
                (mh-get-msg-num t)
                (let ((minibuffer-help-form
                       "from => Sender only\nto => Sender and primary recipients\ncc or all => Sender and all recipients"))
                  (or mh-reply-default-reply-to
                      (completing-read "Reply to whom (default from): "
                                       '(("from") ("to") ("cc") ("all"))
                                       nil
                                       t)))
                current-prefix-arg))
  (let* ((folder mh-current-folder)
         (show-buffer mh-show-buffer)
         (config (current-window-configuration))
         (group-reply (or (equal reply-to "cc") (equal reply-to "all")))
         (form-file (cond ((and (mh-variant-p 'nmh 'gnu-mh) group-reply
                                (stringp mh-repl-group-formfile))
                           mh-repl-group-formfile)
                          ((stringp mh-repl-formfile) mh-repl-formfile)
                          (t nil))))
    (message "Composing a reply...")
    (mh-exec-cmd "repl" "-build" "-noquery" "-nodraftfolder"
                 (if form-file
                     (list "-form" form-file))
                 mh-current-folder message
                 (cond ((or (equal reply-to "from") (equal reply-to ""))
                        '("-nocc" "all"))
                       ((equal reply-to "to")
                        '("-cc" "to"))
                       (group-reply (if (mh-variant-p 'nmh 'gnu-mh)
                                        '("-group" "-nocc" "me")
                                      '("-cc" "all" "-nocc" "me"))))
                 (cond ((or (eq mh-yank-behavior 'autosupercite)
                            (eq mh-yank-behavior 'autoattrib))
                        '("-noformat"))
                       (includep '("-filter" "mhl.reply"))
                       (t '())))
    (let ((draft (mh-read-draft "reply"
                                (expand-file-name "reply" mh-user-path)
                                t)))
      (delete-other-windows)
      (save-buffer)

      (let ((to (mh-get-header-field "To:"))
            (subject (mh-get-header-field "Subject:"))
            (cc (mh-get-header-field "Cc:")))
        (goto-char (point-min))
        (mh-goto-header-end 1)
        (or includep
            (not mh-reply-show-message-flag)
            (mh-in-show-buffer (show-buffer)
              (mh-display-msg message folder)))
        (mh-add-msgs-to-seq message 'answered t)
        (message "Composing a reply...done")
        (mh-compose-and-send-mail draft "" folder message to subject cc
                                  mh-note-repl "Replied:" config))
      (when (and (or (eq 'autosupercite mh-yank-behavior)
                     (eq 'autoattrib mh-yank-behavior))
                 (eq (mh-show-buffer-message-number) mh-sent-from-msg))
        (undo-boundary)
        (mh-yank-cur-msg))
      (mh-letter-mode-message))))

;;;###mh-autoload
(defun mh-send (to cc subject)
  "Compose a message.

Your letter appears in an Emacs buffer whose mode is
MH-Letter (see `mh-letter-mode').

The arguments TO, CC, and SUBJECT can be used to prefill the
draft fields or suppress the prompts if `mh-compose-prompt-flag'
is on. They are also passed to the function set in the option
`mh-compose-letter-function'.

See also `mh-insert-x-mailer-flag' and `mh-letter-mode-hook'.

Outside of an MH-Folder buffer (`mh-folder-mode'), you must call
either \\[mh-smail] or \\[mh-smail-other-window] to compose a new
message."
  (interactive (list
                (mh-interactive-read-address "To: ")
                (mh-interactive-read-address "Cc: ")
                (mh-interactive-read-string "Subject: ")))
  (let ((config (current-window-configuration)))
    (delete-other-windows)
    (mh-send-sub to cc subject config)))



;;; Support Routines

(defun mh-interactive-read-address (prompt)
  "Read an address.
If `mh-compose-prompt-flag' is non-nil, then read an address with
PROMPT.
Otherwise return the empty string."
  (if mh-compose-prompt-flag (mh-read-address prompt) ""))

(defun mh-interactive-read-string (prompt)
  "Read a string.
If `mh-compose-prompt-flag' is non-nil, then read a string with
PROMPT.
Otherwise return the empty string."
  (if mh-compose-prompt-flag (read-string prompt) ""))

;;;###mh-autoload
(defun mh-show-buffer-message-number (&optional buffer)
  "Message number of displayed message in corresponding show buffer.

Return nil if show buffer not displayed.
If in `mh-letter-mode', don't display the message number being replied
to, but rather the message number of the show buffer associated with
our originating folder buffer.
Optional argument BUFFER can be used to specify the buffer."
  (save-excursion
    (if buffer
        (set-buffer buffer))
    (cond ((eq major-mode 'mh-show-mode)
           (let ((number-start (mh-search-from-end ?/ buffer-file-name)))
             (string-to-number (substring buffer-file-name
                                          (1+ number-start)))))
          ((and (eq major-mode 'mh-folder-mode)
                mh-show-buffer
                (get-buffer mh-show-buffer))
           (mh-show-buffer-message-number mh-show-buffer))
          ((and (eq major-mode 'mh-letter-mode)
                mh-sent-from-folder
                (get-buffer mh-sent-from-folder))
           (mh-show-buffer-message-number mh-sent-from-folder))
          (t
           nil))))

(defun mh-send-sub (to cc subject config)
  "Do the real work of composing and sending a letter.
Expects the TO, CC, and SUBJECT fields as arguments.
CONFIG is the window configuration before sending mail."
  (let ((folder mh-current-folder)
        (msg-num (mh-get-msg-num nil)))
    (message "Composing a message...")
    (let ((draft (mh-read-draft
                  "message"
                  (let (components)
                    (cond
                     ((file-exists-p
                       (setq components
                             (expand-file-name mh-comp-formfile mh-user-path)))
                      components)
                     ((file-exists-p
                       (setq components
                             (expand-file-name mh-comp-formfile mh-lib)))
                      components)
                     (t
                      (error "Can't find %s in %s or %s"
                             mh-comp-formfile mh-user-path mh-lib))))
                  nil)))
      (mh-insert-fields "To:" to "Subject:" subject "Cc:" cc)
      (goto-char (point-max))
      (mh-compose-and-send-mail draft "" folder msg-num
                                to subject cc
                                nil nil config)
      (mh-letter-mode-message)
      (mh-letter-adjust-point))))

(defun mh-read-draft (use initial-contents delete-contents-file)
  "Read draft file into a draft buffer and make that buffer the current one.

USE is a message used for prompting about the intended use of the
message.
INITIAL-CONTENTS is filename that is read into an empty buffer, or nil
if buffer should not be modified. Delete the initial-contents file if
DELETE-CONTENTS-FILE flag is set.
Returns the draft folder's name.
If the draft folder facility is enabled in ~/.mh_profile, a new buffer
is used each time and saved in the draft folder. The draft file can
then be reused."
  (cond (mh-draft-folder
         (let ((orig-default-dir default-directory)
               (draft-file-name (mh-new-draft-name)))
           (pop-to-buffer (generate-new-buffer
                           (format "draft-%s"
                                   (file-name-nondirectory draft-file-name))))
           (condition-case ()
               (insert-file-contents draft-file-name t)
             (file-error))
           (setq default-directory orig-default-dir)))
        (t
         (let ((draft-name (expand-file-name "draft" mh-user-path)))
           (pop-to-buffer "draft")      ; Create if necessary
           (if (buffer-modified-p)
               (if (y-or-n-p "Draft has been modified; kill anyway? ")
                   (set-buffer-modified-p nil)
                 (error "Draft preserved")))
           (setq buffer-file-name draft-name)
           (clear-visited-file-modtime)
           (unlock-buffer)
           (cond ((and (file-exists-p draft-name)
                       (not (equal draft-name initial-contents)))
                  (insert-file-contents draft-name)
                  (delete-file draft-name))))))
  (cond ((and initial-contents
              (or (zerop (buffer-size))
                  (if (y-or-n-p
                       (format "A draft exists.  Use for %s? " use))
                      (if mh-error-if-no-draft
                          (error "A prior draft exists"))
                    t)))
         (erase-buffer)
         (insert-file-contents initial-contents)
         (if delete-contents-file (delete-file initial-contents))))
  (auto-save-mode 1)
  (if mh-draft-folder
      (save-buffer))                    ; Do not reuse draft name
  (buffer-name))

(defun mh-new-draft-name ()
  "Return the pathname of folder for draft messages."
  (save-excursion
    (mh-exec-cmd-quiet t "mhpath" mh-draft-folder "new")
    (buffer-substring (point-min) (1- (point-max)))))

(defun mh-insert-fields (&rest name-values)
  "Insert the NAME-VALUES pairs in the current buffer.
If the field exists, append the value to it.
Do not insert any pairs whose value is the empty string."
  (let ((case-fold-search t))
    (while name-values
      (let ((field-name (car name-values))
            (value (car (cdr name-values))))
        (if (not (string-match "^.*:$" field-name))
            (setq field-name (concat field-name ":")))
        (cond ((or (null value)
                   (equal value ""))
               nil)
              ((mh-position-on-field field-name)
               (insert " " (or value "")))
              (t
               (insert field-name " " value "\n")))
        (setq name-values (cdr (cdr name-values)))))))

(defun mh-compose-and-send-mail (draft send-args
                                       sent-from-folder sent-from-msg
                                       to subject cc
                                       annotate-char annotate-field
                                       config)
  "Edit and compose a draft message in buffer DRAFT and send or save it.
SEND-ARGS is the argument passed to the send command.
SENT-FROM-FOLDER is buffer containing scan listing of current folder,
or nil if none exists.
SENT-FROM-MSG is the message number or sequence name or nil.
The TO, SUBJECT, and CC fields are passed to the
`mh-compose-letter-function'.
If ANNOTATE-CHAR is non-null, it is used to notate the scan listing of
the message. In that case, the ANNOTATE-FIELD is used to build a
string for `mh-annotate-msg'.
CONFIG is the window configuration to restore after sending the
letter."
  (pop-to-buffer draft)
  (mh-letter-mode)

  ;; Insert identity.
  (mh-insert-identity mh-identity-default t)
  (mh-identity-make-menu)
  (mh-identity-add-menu)

  ;; Cleanup possibly RFC2047 encoded subject header
  (mh-decode-message-subject)

  ;; Insert extra fields.
  (mh-insert-x-mailer)
  (mh-insert-x-face)

  (mh-letter-hide-all-skipped-fields)

  (setq mh-sent-from-folder sent-from-folder)
  (setq mh-sent-from-msg sent-from-msg)
  (setq mh-send-args send-args)
  (setq mh-annotate-char annotate-char)
  (setq mh-annotate-field annotate-field)
  (setq mh-previous-window-config config)
  (setq mode-line-buffer-identification (list "    {%b}"))
  (mh-logo-display)
  (mh-make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'mh-tidy-draft-buffer nil t)
  (run-hook-with-args 'mh-compose-letter-function to subject cc))

(defun mh-insert-x-mailer ()
  "Append an X-Mailer field to the header.
The versions of MH-E, Emacs, and MH are shown."
  ;; Lazily initialize mh-x-mailer-string.
  (when (and mh-insert-x-mailer-flag (null mh-x-mailer-string))
    (setq mh-x-mailer-string
          (format "MH-E %s; %s; %sEmacs %s"
                  mh-version mh-variant-in-use
                  (if (featurep 'xemacs) "X" "GNU ")
                  (cond ((not (featurep 'xemacs))
                         (string-match "[0-9]+\\.[0-9]+\\(\\.[0-9]+\\)?"
                                       emacs-version)
                         (match-string 0 emacs-version))
                        ((string-match "[0-9.]*\\( +\([ a-z]+[0-9]+\)\\)?"
                                       emacs-version)
                         (match-string 0 emacs-version))
                        (t (format "%s.%s" emacs-major-version
                                   emacs-minor-version))))))
  ;; Insert X-Mailer, but only if it doesn't already exist.
  (save-excursion
    (when (and mh-insert-x-mailer-flag
               (null (mh-goto-header-field "X-Mailer")))
      (mh-insert-fields "X-Mailer:" mh-x-mailer-string))))

(defun mh-insert-x-face ()
  "Append X-Face, Face or X-Image-URL field to header.
If the field already exists, this function does nothing."
  (when (and (file-exists-p mh-x-face-file)
             (file-readable-p mh-x-face-file))
    (save-excursion
      (unless (or (mh-position-on-field "X-Face")
                  (mh-position-on-field "Face")
                  (mh-position-on-field "X-Image-URL"))
        (save-excursion
          (goto-char (+ (point) (cadr (insert-file-contents mh-x-face-file))))
          (if (not (looking-at "^"))
              (insert "\n")))
        (unless (looking-at "\\(X-Face\\|Face\\|X-Image-URL\\): ")
          (insert "X-Face: "))))))

(defun mh-tidy-draft-buffer ()
  "Run when a draft buffer is destroyed."
  (let ((buffer (get-buffer mh-recipients-buffer)))
    (if buffer
        (kill-buffer buffer))))

(defun mh-letter-mode-message ()
  "Display a help message for users of `mh-letter-mode'.
This should be the last function called when composing the draft."
  (message "%s" (substitute-command-keys
                 (concat "Type \\[mh-send-letter] to send message, "
                         "\\[mh-help] for help"))))

(defun mh-letter-adjust-point ()
  "Move cursor to first header field if are using the no prompt mode."
  (unless mh-compose-prompt-flag
    (goto-char (point-max))
    (mh-letter-next-header-field)))

(defun mh-annotate-msg (msg folder note &rest args)
  "Mark MSG in FOLDER with character NOTE and annotate message with ARGS.
MSG can be a message number, a list of message numbers, or a sequence.
The hook `mh-annotate-msg-hook' is run after annotating; see its
documentation for variables it can use."
  (apply 'mh-exec-cmd "anno" folder
         (if (listp msg) (append msg args) (cons msg args)))
  (save-excursion
    (cond ((get-buffer folder)          ; Buffer may be deleted
           (set-buffer folder)
           (mh-iterate-on-range nil msg
             (mh-notate nil note
                        (+ mh-cmd-note mh-scan-field-destination-offset))))))
  (let ((mh-current-folder folder)
        ;; mh-annotate-list is a sequence name or a list of message numbers
        (mh-annotate-list (if (numberp msg) (list msg) msg)))
    (run-hooks 'mh-annotate-msg-hook)))

(defun mh-insert-header-separator ()
  "Insert `mh-mail-header-separator', if absent."
  (save-excursion
    (goto-char (point-min))
    (rfc822-goto-eoh)
    (if (looking-at "$")
        (insert mh-mail-header-separator))))

;;;###mh-autoload
(defun mh-insert-auto-fields (&optional non-interactive)
  "Insert custom fields if recipient is found in `mh-auto-fields-list'.

Once the header contains one or more recipients, you may run this
command to insert these fields manually. However, if you use this
command, the automatic insertion when the message is sent is
disabled.

In a program, set buffer-local `mh-insert-auto-fields-done-local'
if header fields were added. If NON-INTERACTIVE is non-nil,
perform actions quietly and only if
`mh-insert-auto-fields-done-local' is nil. Return t if fields
added; otherwise return nil."
  (interactive)
  (when (or (not non-interactive)
            (not mh-insert-auto-fields-done-local))
    (save-excursion
      (when (and (or (mh-goto-header-field "To:")
                     (mh-goto-header-field "cc:")))
        (let ((list mh-auto-fields-list)
              (fields-inserted nil))
          (while list
            (let ((regexp (nth 0 (car list)))
                  (entries (nth 1 (car list))))
              (when (mh-regexp-in-field-p regexp "To:" "cc:")
                (setq mh-insert-auto-fields-done-local t)
                (setq fields-inserted t)
                (if (not non-interactive)
                    (message "Fields for %s added" regexp))
                (let ((entry-list entries))
                  (while entry-list
                    (let ((field (caar entry-list))
                          (value (cdar entry-list)))
                      (cond
                       ((equal ":identity" field)
                        (when
                            ;;(and (not mh-identity-local)
                            ;; Bug 1204506.  But do we need to be able
                            ;; to set an identity manually that won't be
                            ;; overridden by mh-insert-auto-fields?
                            (assoc value mh-identity-list)
                          ;;)
                          (mh-insert-identity value)))
                       (t
                        (mh-modify-header-field field value
                                                (equal field "From")))))
                    (setq entry-list (cdr entry-list))))))
            (setq list (cdr list)))
          fields-inserted)))))

(defun mh-modify-header-field (field value &optional overwrite-flag)
  "To header FIELD add VALUE.
If OVERWRITE-FLAG is non-nil then the old value, if present, is
discarded."
  (cond ((and overwrite-flag
              (mh-goto-header-field (concat field ":")))
         (insert " " value)
         (delete-region (point) (mh-line-end-position)))
        ((and (not overwrite-flag)
              (mh-regexp-in-field-p (concat "\\b" value "\\b") field))
         ;; Already there, do nothing.
         )
        ((and (not overwrite-flag)
              (mh-goto-header-field (concat field ":")))
         (insert " " value ","))
        (t
         (mh-goto-header-end 0)
         (insert field ": " value "\n"))))

(defun mh-regexp-in-field-p (regexp &rest fields)
  "Non-nil means REGEXP was found in FIELDS."
  (save-excursion
    (let ((search-result nil)
          (field))
      (while fields
        (setq field (car fields))
        (if (and (mh-goto-header-field field)
                 (re-search-forward
                  regexp (save-excursion (mh-header-field-end)(point)) t))
            (setq fields nil
                  search-result t)
          (setq fields (cdr fields))))
      search-result)))

(defun mh-ascii-buffer-p ()
  "Check if current buffer is entirely composed of ASCII.
The function doesn't work for XEmacs since `find-charset-region'
doesn't exist there."
  (loop for charset in (mh-funcall-if-exists
                        find-charset-region (point-min) (point-max))
        unless (eq charset 'ascii) return nil
        finally return t))

(provide 'mh-comp)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-comp.el ends here
