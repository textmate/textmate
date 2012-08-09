;;; mh-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (mh-iterate-on-range mh-iterate-on-messages-in-region
;;;;;;  mh-do-at-event-location mh-in-show-buffer with-mh-folder-updating
;;;;;;  mh-defstruct mh-mark-active-p mh-make-local-hook defmacro-mh
;;;;;;  defun-mh mh-funcall-if-exists mh-do-in-xemacs mh-do-in-gnu-emacs
;;;;;;  mh-require-cl) "mh-acros" "mh-acros.el" (20352 65510))
;;; Generated autoloads from mh-acros.el

(autoload 'mh-require-cl "mh-acros" "\
Macro to load \"cl\" if needed.

Emacs coding conventions require that the \"cl\" package not be
required at runtime. However, the \"cl\" package in Emacs 21.4
and earlier left \"cl\" routines in their macro expansions. In
particular, the expansion of (setf (gethash ...) ...) used
functions in \"cl\" at run time. This macro recognizes that and
loads \"cl\" appropriately.

\(fn)" nil (quote macro))

(autoload 'mh-do-in-gnu-emacs "mh-acros" "\
Execute BODY if in GNU Emacs.

\(fn &rest BODY)" nil (quote macro))

(autoload 'mh-do-in-xemacs "mh-acros" "\
Execute BODY if in XEmacs.

\(fn &rest BODY)" nil (quote macro))

(autoload 'mh-funcall-if-exists "mh-acros" "\
Call FUNCTION with ARGS as parameters if it exists.

\(fn FUNCTION &rest ARGS)" nil (quote macro))

(autoload 'defun-mh "mh-acros" "\
Create function NAME.
If FUNCTION exists, then NAME becomes an alias for FUNCTION.
Otherwise, create function NAME with ARG-LIST and BODY.

\(fn NAME FUNCTION ARG-LIST &rest BODY)" nil (quote macro))

(autoload 'defmacro-mh "mh-acros" "\
Create macro NAME.
If MACRO exists, then NAME becomes an alias for MACRO.
Otherwise, create macro NAME with ARG-LIST and BODY.

\(fn NAME MACRO ARG-LIST &rest BODY)" nil (quote macro))

(autoload 'mh-make-local-hook "mh-acros" "\
Make HOOK local if needed.
XEmacs and versions of GNU Emacs before 21.1 require
`make-local-hook' to be called.

\(fn HOOK)" nil (quote macro))

(autoload 'mh-mark-active-p "mh-acros" "\
A macro that expands into appropriate code in XEmacs and nil in GNU Emacs.
In GNU Emacs if CHECK-TRANSIENT-MARK-MODE-FLAG is non-nil then
check if variable `transient-mark-mode' is active.

\(fn CHECK-TRANSIENT-MARK-MODE-FLAG)" nil (quote macro))

(autoload 'mh-defstruct "mh-acros" "\
Replacement for `defstruct' from the \"cl\" package.
The `defstruct' in the \"cl\" library produces compiler warnings,
and generates code that uses functions present in \"cl\" at
run-time. This is a partial replacement, that avoids these
issues.

NAME-SPEC declares the name of the structure, while FIELDS
describes the various structure fields. Lookup `defstruct' for
more details.

\(fn NAME-SPEC &rest FIELDS)" nil (quote macro))

(autoload 'with-mh-folder-updating "mh-acros" "\
Format is (with-mh-folder-updating (SAVE-MODIFICATION-FLAG) &body BODY).
Execute BODY, which can modify the folder buffer without having to
worry about file locking or the read-only flag, and return its result.
If SAVE-MODIFICATION-FLAG is non-nil, the buffer's modification flag
is unchanged, otherwise it is cleared.

\(fn SAVE-MODIFICATION-FLAG &rest BODY)" nil (quote macro))

(autoload 'mh-in-show-buffer "mh-acros" "\
Format is (mh-in-show-buffer (SHOW-BUFFER) &body BODY).
Display buffer SHOW-BUFFER in other window and execute BODY in it.
Stronger than `save-excursion', weaker than `save-window-excursion'.

\(fn SHOW-BUFFER &rest BODY)" nil (quote macro))

(autoload 'mh-do-at-event-location "mh-acros" "\
Switch to the location of EVENT and execute BODY.
After BODY has been executed return to original window. The
modification flag of the buffer in the event window is
preserved.

\(fn EVENT &rest BODY)" nil (quote macro))

(defsubst mh-seq-msgs (sequence) "\
Extract messages from the given SEQUENCE." (cdr sequence))

(autoload 'mh-iterate-on-messages-in-region "mh-acros" "\
Iterate over region.

VAR is bound to the message on the current line as we loop
starting from BEGIN till END. In each step BODY is executed.

If VAR is nil then the loop is executed without any binding.

\(fn VAR BEGIN END &rest BODY)" nil (quote macro))

(autoload 'mh-iterate-on-range "mh-acros" "\
Iterate an operation over a region or sequence.

VAR is bound to each message in turn in a loop over RANGE, which
can be a message number, a list of message numbers, a sequence, a
region in a cons cell, or a MH range (something like last:20) in
a string. In each iteration, BODY is executed.

The parameter RANGE is usually created with
`mh-interactive-range' in order to provide a uniform interface to
MH-E functions.

\(fn VAR RANGE &rest BODY)" nil (quote macro))

;;;***

;;;### (autoloads (mh-alias-grab-from-field mh-alias-for-from-p mh-alias-address-to-alias
;;;;;;  mh-alias-letter-expand-alias mh-alias-minibuffer-confirm-address
;;;;;;  mh-read-address mh-alias-expand mh-alias-reload-maybe) "mh-alias"
;;;;;;  "mh-alias.el" (20352 65510))
;;; Generated autoloads from mh-alias.el

(autoload 'mh-alias-reload-maybe "mh-alias" "\
Load new MH aliases.

\(fn)" nil nil)

(autoload 'mh-alias-expand "mh-alias" "\
Return expansion for ALIAS.
Blind aliases or users from /etc/passwd are not expanded.

\(fn ALIAS)" nil nil)

(autoload 'mh-read-address "mh-alias" "\
Read an address from the minibuffer with PROMPT.

\(fn PROMPT)" nil nil)

(autoload 'mh-alias-minibuffer-confirm-address "mh-alias" "\
Display the alias expansion if `mh-alias-flash-on-comma' is non-nil.

\(fn)" t nil)

(autoload 'mh-alias-letter-expand-alias "mh-alias" "\
Expand mail alias before point.

\(fn)" nil nil)

(autoload 'mh-alias-address-to-alias "mh-alias" "\
Return the ADDRESS alias if defined, or nil.

\(fn ADDRESS)" nil nil)

(autoload 'mh-alias-for-from-p "mh-alias" "\
Return t if sender's address has a corresponding alias.

\(fn)" nil nil)

(autoload 'mh-alias-grab-from-field "mh-alias" "\
Add alias for the sender of the current message.

\(fn)" t nil)

;;;***

;;;### (autoloads (mh-insert-auto-fields mh-show-buffer-message-number
;;;;;;  mh-send mh-reply mh-redistribute mh-forward mh-extract-rejected-mail
;;;;;;  mh-edit-again) "mh-comp" "mh-comp.el" (20352 65510))
;;; Generated autoloads from mh-comp.el

(autoload 'mh-edit-again "mh-comp" "\
Edit a MESSAGE to send it again.

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

See also `mh-send'.

\(fn MESSAGE)" t nil)

(autoload 'mh-extract-rejected-mail "mh-comp" "\
Edit a MESSAGE that was returned by the mail system.

This command prepares the message for editing by removing the
Mailer-Daemon envelope and unneeded header fields. Fix whatever
addressing problem you had, and send the message again with
\\[mh-send-letter].

The default message is the current message.

See also `mh-send'.

\(fn MESSAGE)" t nil)

(autoload 'mh-forward "mh-comp" "\
Forward message.

You are prompted for the TO and CC recipients. You are given a
draft to edit that looks like it would if you had run the MH
command \"forw\". You can then add some text.

You can forward several messages by using a RANGE. All of the
messages in the range are inserted into your draft. Check the
documentation of `mh-interactive-range' to see how RANGE is read
in interactive use.

The hook `mh-forward-hook' is called on the draft.

See also `mh-compose-forward-as-mime-flag',
`mh-forward-subject-format', and `mh-send'.

\(fn TO CC &optional RANGE)" t nil)

(autoload 'mh-redistribute "mh-comp" "\
Redistribute a message.

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
message and scan line.

\(fn TO CC &optional MESSAGE)" t nil)

(autoload 'mh-reply "mh-comp" "\
Reply to a MESSAGE.

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
`mh-reply-default-reply-to', and `mh-send'.

\(fn MESSAGE &optional REPLY-TO INCLUDEP)" t nil)

(autoload 'mh-send "mh-comp" "\
Compose a message.

Your letter appears in an Emacs buffer whose mode is
MH-Letter (see `mh-letter-mode').

The arguments TO, CC, and SUBJECT can be used to prefill the
draft fields or suppress the prompts if `mh-compose-prompt-flag'
is on. They are also passed to the function set in the option
`mh-compose-letter-function'.

See also `mh-insert-x-mailer-flag' and `mh-letter-mode-hook'.

Outside of an MH-Folder buffer (`mh-folder-mode'), you must call
either \\[mh-smail] or \\[mh-smail-other-window] to compose a new
message.

\(fn TO CC SUBJECT)" t nil)

(autoload 'mh-show-buffer-message-number "mh-comp" "\
Message number of displayed message in corresponding show buffer.

Return nil if show buffer not displayed.
If in `mh-letter-mode', don't display the message number being replied
to, but rather the message number of the show buffer associated with
our originating folder buffer.
Optional argument BUFFER can be used to specify the buffer.

\(fn &optional BUFFER)" nil nil)

(autoload 'mh-insert-auto-fields "mh-comp" "\
Insert custom fields if recipient is found in `mh-auto-fields-list'.

Once the header contains one or more recipients, you may run this
command to insert these fields manually. However, if you use this
command, the automatic insertion when the message is sent is
disabled.

In a program, set buffer-local `mh-insert-auto-fields-done-local'
if header fields were added. If NON-INTERACTIVE is non-nil,
perform actions quietly and only if
`mh-insert-auto-fields-done-local' is nil. Return t if fields
added; otherwise return nil.

\(fn &optional NON-INTERACTIVE)" t nil)

;;;***

;;;### (autoloads (mh-msg-filename mh-refile-a-msg mh-delete-a-msg
;;;;;;  mh-folder-from-address mh-prompt-for-refile-folder mh-next-msg
;;;;;;  mh-reset-threads-and-narrowing mh-regenerate-headers mh-process-or-undo-commands
;;;;;;  mh-scan-folder mh-make-folder-mode-line mh-set-folder-modified-p
;;;;;;  mh-outstanding-commands-p mh-recenter mh-goto-cur-msg mh-update-sequences
;;;;;;  mh-write-msg-to-file mh-visit-folder mh-undo mh-toggle-showing
;;;;;;  mh-rescan-folder mh-refile-or-write-again mh-refile-msg mh-quit
;;;;;;  mh-previous-unread-msg mh-previous-undeleted-msg mh-previous-page
;;;;;;  mh-prev-button mh-page-msg mh-next-unread-msg mh-next-undeleted-msg
;;;;;;  mh-next-button mh-modify mh-last-msg mh-inc-folder mh-goto-msg
;;;;;;  mh-first-msg mh-execute-commands mh-delete-msg-no-motion
;;;;;;  mh-delete-msg) "mh-folder" "mh-folder.el" (20400 62402))
;;; Generated autoloads from mh-folder.el

(autoload 'mh-delete-msg "mh-folder" "\
Delete RANGE\\<mh-folder-mode-map>.

To mark a message for deletion, use this command. A \"D\" is
placed by the message in the scan window, and the next undeleted
message is displayed. If the previous command had been
\\[mh-previous-undeleted-msg], then the next message displayed is
the first undeleted message previous to the message just deleted.
Use \\[mh-next-undeleted-msg] to force subsequent
\\[mh-delete-msg] commands to move forward to the next undeleted
message after deleting the message under the cursor.

The hook `mh-delete-msg-hook' is called after you mark a message
for deletion. For example, a past maintainer of MH-E used this
once when he kept statistics on his mail usage.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

\(fn RANGE)" t nil)

(autoload 'mh-delete-msg-no-motion "mh-folder" "\
Delete RANGE, don't move to next message.

This command marks the RANGE for deletion but leaves the cursor
at the current message in case you wish to perform other
operations on the message.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

\(fn RANGE)" t nil)

(autoload 'mh-execute-commands "mh-folder" "\
Process outstanding delete and refile requests\\<mh-folder-mode-map>.

If you've marked messages to be deleted or refiled and you want
to go ahead and delete or refile the messages, use this command.
Many MH-E commands that may affect the numbering of the
messages (such as \\[mh-rescan-folder] or \\[mh-pack-folder])
will ask if you want to process refiles or deletes first and then
either run this command for you or undo the pending refiles and
deletes.

This function runs `mh-before-commands-processed-hook' before the
commands are processed and `mh-after-commands-processed-hook'
after the commands are processed.

\(fn)" t nil)

(autoload 'mh-first-msg "mh-folder" "\
Display first message.

\(fn)" t nil)

(autoload 'mh-goto-msg "mh-folder" "\
Go to a message\\<mh-folder-mode-map>.

You can enter the message NUMBER either before or after typing
\\[mh-goto-msg]. In the latter case, Emacs prompts you.

In a program, optional non-nil second argument NO-ERROR-IF-NO-MESSAGE
means return nil instead of signaling an error if message does not
exist; in this case, the cursor is positioned near where the message
would have been. Non-nil third argument DONT-SHOW means not to show
the message.

\(fn NUMBER &optional NO-ERROR-IF-NO-MESSAGE DONT-SHOW)" t nil)

(autoload 'mh-inc-folder "mh-folder" "\
Incorporate new mail into a folder.

You can incorporate mail from any file into the current folder by
specifying a prefix argument; you'll be prompted for the name of
the FILE to use as well as the destination FOLDER

The hook `mh-inc-folder-hook' is run after incorporating new
mail.

Do not call this function from outside MH-E; use \\[mh-rmail]
instead.

\(fn &optional FILE FOLDER)" t nil)

(autoload 'mh-last-msg "mh-folder" "\
Display last message.

\(fn)" t nil)

(autoload 'mh-modify "mh-folder" "\
Edit message.

There are times when you need to edit a message. For example, you
may need to fix a broken Content-Type header field. You can do
this with this command. It displays the raw message in an
editable buffer. When you are done editing, save and kill the
buffer as you would any other.

From a program, edit MESSAGE; nil means edit current message.

\(fn &optional MESSAGE)" t nil)

(autoload 'mh-next-button "mh-folder" "\
Go to the next button.

If the end of the buffer is reached then the search wraps over to
the start of the buffer.

If an optional prefix argument BACKWARD-FLAG is given, the cursor
will move to the previous button.

\(fn &optional BACKWARD-FLAG)" t nil)

(autoload 'mh-next-undeleted-msg "mh-folder" "\
Display next message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip.

In a program, pause for a second after printing message if we are
at the last undeleted message and optional argument
WAIT-AFTER-COMPLAINING-FLAG is non-nil.

\(fn &optional COUNT WAIT-AFTER-COMPLAINING-FLAG)" t nil)

(autoload 'mh-next-unread-msg "mh-folder" "\
Display next unread message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip.

\(fn &optional COUNT)" t nil)

(autoload 'mh-page-msg "mh-folder" "\
Display next page in message.

You can give this command a prefix argument that specifies the
number of LINES to scroll. This command will also show the next
undeleted message if it is used at the bottom of a message.

\(fn &optional LINES)" t nil)

(autoload 'mh-prev-button "mh-folder" "\
Go to the previous button.

If the beginning of the buffer is reached then the search wraps
over to the end of the buffer.

\(fn)" t nil)

(autoload 'mh-previous-page "mh-folder" "\
Display next page in message.

You can give this command a prefix argument that specifies the
number of LINES to scroll.

\(fn &optional LINES)" t nil)

(autoload 'mh-previous-undeleted-msg "mh-folder" "\
Display previous message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip.

In a program, pause for a second after printing message if we are
at the last undeleted message and optional argument
WAIT-AFTER-COMPLAINING-FLAG is non-nil.

\(fn &optional COUNT WAIT-AFTER-COMPLAINING-FLAG)" t nil)

(autoload 'mh-previous-unread-msg "mh-folder" "\
Display previous unread message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip.

\(fn &optional COUNT)" t nil)

(autoload 'mh-quit "mh-folder" "\
Quit the current MH-E folder.

When you want to quit using MH-E and go back to editing, you can use
this command. This buries the buffers of the current MH-E folder and
restores the buffers that were present when you first ran
\\[mh-rmail]. It also removes any MH-E working buffers whose name
begins with \" *mh-\" or \"*MH-E \". You can later restore your MH-E
session by selecting the \"+inbox\" buffer or by running \\[mh-rmail]
again.

The two hooks `mh-before-quit-hook' and `mh-quit-hook' are called by
this function. The former one is called before the quit occurs, so you
might use it to perform any MH-E operations; you could perform some
query and abort the quit or call `mh-execute-commands', for example.
The latter is not run in an MH-E context, so you might use it to
modify the window setup.

\(fn)" t nil)

(autoload 'mh-refile-msg "mh-folder" "\
Refile (output) RANGE into FOLDER.

You are prompted for the folder name. Note that this command can also
be used to create folders. If you specify a folder that does not
exist, you will be prompted to create it.

The hook `mh-refile-msg-hook' is called after a message is marked to
be refiled.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

In a program, the variables `mh-last-destination' and
`mh-last-destination-folder' are not updated if
DONT-UPDATE-LAST-DESTINATION-FLAG is non-nil.

\(fn RANGE FOLDER &optional DONT-UPDATE-LAST-DESTINATION-FLAG)" t nil)

(autoload 'mh-refile-or-write-again "mh-folder" "\
Repeat last output command.

If you are refiling several messages into the same folder, you
can use this command to repeat the last
refile (\\[mh-refile-msg]) or write (\\[mh-write-msg-to-file]).
You can use a range.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

In a program, a non-nil INTERACTIVE-FLAG means that the function was
called interactively.

\(fn RANGE &optional INTERACTIVE-FLAG)" t nil)

(autoload 'mh-rescan-folder "mh-folder" "\
Rescan folder\\<mh-folder-mode-map>.

This command is useful to grab all messages in your \"+inbox\" after
processing your new mail for the first time. If you don't want to
rescan the entire folder, this command will accept a RANGE. Check the
documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

This command will ask if you want to process refiles or deletes first
and then either run \\[mh-execute-commands] for you or undo the
pending refiles and deletes.

In a program, the processing of outstanding commands is not performed
if DONT-EXEC-PENDING is non-nil.

\(fn &optional RANGE DONT-EXEC-PENDING)" t nil)

(autoload 'mh-toggle-showing "mh-folder" "\
Toggle between MH-Folder and MH-Folder Show modes.

This command switches between MH-Folder mode and MH-Folder Show
mode. MH-Folder mode turns off the associated show buffer so that
you can perform operations on the messages quickly without
reading them. This is an excellent way to prune out your junk
mail or to refile a group of messages to another folder for later
examination.

\(fn)" t nil)

(autoload 'mh-undo "mh-folder" "\
Undo pending deletes or refiles in RANGE.

If you've deleted a message or refiled it, but changed your mind,
you can cancel the action before you've executed it. Use this
command to undo a refile on or deletion of a single message. You
can also undo refiles and deletes for messages that are found in
a given RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

\(fn RANGE)" t nil)

(autoload 'mh-visit-folder "mh-folder" "\
Visit FOLDER.

When you want to read the messages that you have refiled into folders,
use this command to visit the folder. You are prompted for the folder
name.

The folder buffer will show just unseen messages if there are any;
otherwise, it will show all the messages in the buffer as long there
are fewer than `mh-large-folder' messages. If there are more, then you
are prompted for a range of messages to scan.

You can provide a prefix argument in order to specify a RANGE of
messages to show when you visit the folder. In this case, regions are
not used to specify the range and `mh-large-folder' is ignored. Check
the documentation of `mh-interactive-range' to see how RANGE is read
in interactive use.

Note that this command can also be used to create folders. If you
specify a folder that does not exist, you will be prompted to create
it.

Do not call this function from outside MH-E; use \\[mh-rmail] instead.

If, in a program, RANGE is nil (the default), then all messages in
FOLDER are displayed. If an index buffer is being created then
INDEX-DATA is used to initialize the index buffer specific data
structures.

\(fn FOLDER &optional RANGE INDEX-DATA)" t nil)

(autoload 'mh-write-msg-to-file "mh-folder" "\
Append MESSAGE to end of FILE\\<mh-folder-mode-map>.

You are prompted for the filename. If the file already exists,
the message is appended to it. You can also write the message to
the file without the header by specifying a prefix argument
NO-HEADER. Subsequent writes to the same file can be made with
the command \\[mh-refile-or-write-again].

\(fn MESSAGE FILE NO-HEADER)" t nil)

(autoload 'mh-update-sequences "mh-folder" "\
Flush MH-E's state out to MH.

This function updates the sequence specified by your
\"Unseen-Sequence:\" profile component, \"cur\", and the sequence
listed by the `mh-tick-seq' option which is \"tick\" by default.
The message at the cursor is used for \"cur\".

\(fn)" t nil)

(autoload 'mh-goto-cur-msg "mh-folder" "\
Position the cursor at the current message.
When optional argument MINIMAL-CHANGES-FLAG is non-nil, the
function doesn't recenter the folder buffer.

\(fn &optional MINIMAL-CHANGES-FLAG)" nil nil)

(autoload 'mh-recenter "mh-folder" "\
Like recenter but with three improvements:

- At the end of the buffer it tries to show fewer empty lines.

- operates only if the current buffer is in the selected window.
  (Commands like `save-some-buffers' can make this false.)

- nil ARG means recenter as if prefix argument had been given.

\(fn ARG)" nil nil)

(autoload 'mh-outstanding-commands-p "mh-folder" "\
Return non-nil if there are outstanding deletes or refiles.

\(fn)" nil nil)

(autoload 'mh-set-folder-modified-p "mh-folder" "\
Mark current folder as modified or unmodified according to FLAG.

\(fn FLAG)" nil nil)

(autoload 'mh-make-folder-mode-line "mh-folder" "\
Set the fields of the mode line for a folder buffer.
The optional argument is now obsolete and IGNORED. It used to be
used to pass in what is now stored in the buffer-local variable
`mh-mode-line-annotation'.

\(fn &optional IGNORED)" nil nil)

(autoload 'mh-scan-folder "mh-folder" "\
Scan FOLDER over RANGE.

After the scan is performed, switch to the buffer associated with
FOLDER.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

The processing of outstanding commands is not performed if
DONT-EXEC-PENDING is non-nil.

\(fn FOLDER RANGE &optional DONT-EXEC-PENDING)" nil nil)

(autoload 'mh-process-or-undo-commands "mh-folder" "\
If FOLDER has outstanding commands, then either process or discard them.
Called by functions like `mh-sort-folder', so also invalidate
show buffer.

\(fn FOLDER)" nil nil)

(autoload 'mh-regenerate-headers "mh-folder" "\
Scan folder over RANGE.
If UPDATE, append the scan lines, otherwise replace.

\(fn RANGE &optional UPDATE)" nil nil)

(autoload 'mh-reset-threads-and-narrowing "mh-folder" "\
Reset all variables pertaining to threads and narrowing.
Also removes all content from the folder buffer.

\(fn)" nil nil)

(autoload 'mh-next-msg "mh-folder" "\
Move backward or forward to the next undeleted message in the buffer.
If optional argument WAIT-AFTER-COMPLAINING-FLAG is non-nil and
we are at the last message, then wait for a second after telling
the user that there aren't any more unread messages.

\(fn &optional WAIT-AFTER-COMPLAINING-FLAG)" nil nil)

(autoload 'mh-prompt-for-refile-folder "mh-folder" "\
Prompt the user for a folder in which the message should be filed.
The folder is returned as a string.

The default folder name is generated by the option
`mh-default-folder-for-message-function' if it is non-nil or
`mh-folder-from-address'.

\(fn)" nil nil)

(autoload 'mh-folder-from-address "mh-folder" "\
Derive folder name from sender.

The name of the folder is derived as follows:

  a) The folder name associated with the first address found in
     the list `mh-default-folder-list' is used. Each element in
     this list contains a \"Check Recipient\" item. If this item is
     turned on, then the address is checked against the recipient
     instead of the sender. This is useful for mailing lists.

  b) An alias prefixed by `mh-default-folder-prefix'
     corresponding to the address is used. The prefix is used to
     prevent clutter in your mail directory.

Return nil if a folder name was not derived, or if the variable
`mh-default-folder-must-exist-flag' is t and the folder does not
exist.

\(fn)" nil nil)

(autoload 'mh-delete-a-msg "mh-folder" "\
Delete MESSAGE.
If MESSAGE is nil then the message at point is deleted.
The hook `mh-delete-msg-hook' is called after you mark a message
for deletion. For example, a past maintainer of MH-E used this
once when he kept statistics on his mail usage.

\(fn MESSAGE)" nil nil)

(autoload 'mh-refile-a-msg "mh-folder" "\
Refile MESSAGE in FOLDER.
If MESSAGE is nil then the message at point is refiled.
Folder is a symbol, not a string.
The hook `mh-refile-msg-hook' is called after a message is marked to
be refiled.

\(fn MESSAGE FOLDER)" nil nil)

(autoload 'mh-msg-filename "mh-folder" "\
Return the file name of MSG in FOLDER (default current folder).

\(fn MSG &optional FOLDER)" nil nil)

;;;***

;;;### (autoloads (mh-undo-folder mh-store-msg mh-sort-folder mh-pipe-msg
;;;;;;  mh-page-digest-backwards mh-page-digest mh-pack-folder mh-list-folders
;;;;;;  mh-kill-folder mh-copy-msg mh-burst-digest) "mh-funcs" "mh-funcs.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from mh-funcs.el

(autoload 'mh-burst-digest "mh-funcs" "\
Break up digest into separate messages\\<mh-folder-mode-map>.

This command uses the MH command \"burst\" to break out each
message in the digest into its own message. Using this command,
you can quickly delete unwanted messages, like this: Once the
digest is split up, toggle out of MH-Folder Show mode with
\\[mh-toggle-showing] so that the scan lines fill the screen and
messages aren't displayed. Then use \\[mh-delete-msg] to quickly
delete messages that you don't want to read (based on the
\"Subject:\" header field). You can also burst the digest to
reply directly to the people who posted the messages in the
digest. One problem you may encounter is that the \"From:\"
header fields are preceded with a \">\" so that your reply can't
create the \"To:\" field correctly. In this case, you must
correct the \"To:\" field yourself.

\(fn)" t nil)

(autoload 'mh-copy-msg "mh-funcs" "\
Copy RANGE to FOLDER\\<mh-folder-mode-map>.

If you wish to copy a message to another folder, you can use this
command (see the \"-link\" argument to \"refile\"). Like the
command \\[mh-refile-msg], this command prompts you for the name
of the target folder and you can specify a range. Note that
unlike the command \\[mh-refile-msg], the copy takes place
immediately. The original copy remains in the current folder.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

\(fn RANGE FOLDER)" t nil)

(autoload 'mh-kill-folder "mh-funcs" "\
Remove folder.

Remove all of the messages (files) within the current folder, and
then remove the folder (directory) itself.

Run the abnormal hook `mh-kill-folder-suppress-prompt-hooks'. The
hook functions are called with no arguments and should return a
non-nil value to suppress the normal prompt when you remove a
folder. This is useful for folders that are easily regenerated.

\(fn)" t nil)

(autoload 'mh-list-folders "mh-funcs" "\
List mail folders.

\(fn)" t nil)

(autoload 'mh-pack-folder "mh-funcs" "\
Pack folder\\<mh-folder-mode-map>.

This command packs the folder, removing gaps from the numbering
sequence. If you don't want to rescan the entire folder
afterward, this command will accept a RANGE. Check the
documentation of `mh-interactive-range' to see how RANGE is read
in interactive use.

This command will ask if you want to process refiles or deletes
first and then either run \\[mh-execute-commands] for you or undo
the pending refiles and deletes.

The hook `mh-pack-folder-hook' is run after the folder is packed;
see its documentation for variables it can use.

\(fn RANGE)" t nil)

(autoload 'mh-page-digest "mh-funcs" "\
Display next message in digest.

\(fn)" t nil)

(autoload 'mh-page-digest-backwards "mh-funcs" "\
Display previous message in digest.

\(fn)" t nil)

(autoload 'mh-pipe-msg "mh-funcs" "\
Pipe message through shell command COMMAND.

You are prompted for the Unix command through which you wish to
run your message. If you give a prefix argument INCLUDE-HEADER to
this command, the message header is included in the text passed
to the command.

\(fn COMMAND INCLUDE-HEADER)" t nil)

(autoload 'mh-sort-folder "mh-funcs" "\
Sort folder.

By default, messages are sorted by date. The option
`mh-sortm-args' holds extra arguments to pass on to the command
\"sortm\" when a prefix argument EXTRA-ARGS is used.

\(fn &optional EXTRA-ARGS)" t nil)

(autoload 'mh-store-msg "mh-funcs" "\
Unpack message created with \"uudecode\" or \"shar\".

The default DIRECTORY for extraction is the current directory;
however, you have a chance to specify a different extraction
directory. The next time you use this command, the default
directory is the last directory you used. If you would like to
change the initial default directory, customize the option
`mh-store-default-directory', change the value from \"Current\"
to \"Directory\", and then enter the name of the directory for
storing the content of these messages.

\(fn DIRECTORY)" t nil)

(autoload 'mh-undo-folder "mh-funcs" "\
Undo all refiles and deletes in the current folder.
Arguments are IGNORED (for `revert-buffer').

\(fn &rest IGNORED)" t nil)

;;;***

;;;### (autoloads (mh-identity-handler-bottom mh-identity-handler-top
;;;;;;  mh-identity-insert-attribution-verb mh-identity-handler-attribution-verb
;;;;;;  mh-identity-handler-signature mh-identity-handler-gpg-identity
;;;;;;  mh-insert-identity mh-identity-add-menu mh-identity-make-menu)
;;;;;;  "mh-identity" "mh-identity.el" (20352 65510))
;;; Generated autoloads from mh-identity.el

(autoload 'mh-identity-make-menu "mh-identity" "\
Build the Identity menu.
This should be called any time `mh-identity-list' or
`mh-auto-fields-list' change.
See `mh-identity-add-menu'.

\(fn)" nil nil)

(autoload 'mh-identity-add-menu "mh-identity" "\
Add the current Identity menu.
See `mh-identity-make-menu'.

\(fn)" nil nil)

(autoload 'mh-insert-identity "mh-identity" "\
Insert fields specified by given IDENTITY.

In a program, do not insert fields if MAYBE-INSERT is non-nil,
`mh-identity-default' is non-nil, and fields have already been
inserted.

See `mh-identity-list'.

\(fn IDENTITY &optional MAYBE-INSERT)" t nil)

(autoload 'mh-identity-handler-gpg-identity "mh-identity" "\
Process header FIELD \":pgg-default-user-id\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added.
The buffer-local variable `mh-identity-pgg-default-user-id' is set to
VALUE when action 'add is selected.

\(fn FIELD ACTION &optional VALUE)" nil nil)

(autoload 'mh-identity-handler-signature "mh-identity" "\
Process header FIELD \":signature\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added.

\(fn FIELD ACTION &optional VALUE)" nil nil)

(autoload 'mh-identity-handler-attribution-verb "mh-identity" "\
Process header FIELD \":attribution-verb\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added.

\(fn FIELD ACTION &optional VALUE)" nil nil)

(autoload 'mh-identity-insert-attribution-verb "mh-identity" "\
Insert VALUE as attribution verb, setting up delimiting markers.
If VALUE is nil, use `mh-extract-from-attribution-verb'.

\(fn VALUE)" nil nil)

(autoload 'mh-identity-handler-top "mh-identity" "\
Process header FIELD.
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added. If the field wasn't present, it is added to the top of the
header.

\(fn FIELD ACTION &optional VALUE)" nil nil)

(autoload 'mh-identity-handler-bottom "mh-identity" "\
Process header FIELD.
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added. If the field wasn't present, it is added to the bottom of
the header.

\(fn FIELD ACTION &optional VALUE)" nil nil)

;;;***

;;;### (autoloads (mh-inc-spool-make) "mh-inc" "mh-inc.el" (20352
;;;;;;  65510))
;;; Generated autoloads from mh-inc.el

(autoload 'mh-inc-spool-make "mh-inc" "\
Make all commands and defines keys for contents of `mh-inc-spool-list'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (mh-spamprobe-whitelist mh-spamprobe-blacklist
;;;;;;  mh-bogofilter-whitelist mh-bogofilter-blacklist mh-spamassassin-identify-spammers
;;;;;;  mh-spamassassin-whitelist mh-spamassassin-blacklist mh-junk-whitelist
;;;;;;  mh-junk-blacklist) "mh-junk" "mh-junk.el" (20352 65510))
;;; Generated autoloads from mh-junk.el

(autoload 'mh-junk-blacklist "mh-junk" "\
Blacklist RANGE as spam.

This command trains the spam program in use (see the option
`mh-junk-program') with the content of RANGE and then handles the
message(s) as specified by the option `mh-junk-disposition'.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

For more information about using your particular spam fighting
program, see:

  - `mh-spamassassin-blacklist'
  - `mh-bogofilter-blacklist'
  - `mh-spamprobe-blacklist'

\(fn RANGE)" t nil)

(autoload 'mh-junk-whitelist "mh-junk" "\
Whitelist RANGE as ham.

This command reclassifies the RANGE as ham if it were incorrectly
classified as spam (see the option `mh-junk-program'). It then
refiles the message into the \"+inbox\" folder.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

\(fn RANGE)" t nil)

(autoload 'mh-spamassassin-blacklist "mh-junk" "\
Blacklist MSG with SpamAssassin.

SpamAssassin is one of the more popular spam filtering programs.
Get it from your local distribution or from the SpamAssassin web
site at URL `http://spamassassin.org/'.

To use SpamAssassin, add the following recipes to
\".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with SpamAssassin.
    :0fw
    | spamc

    # Anything with a spam level of 10 or more is junked immediately.
    :0:
    * ^X-Spam-Level: ..........
    /dev/null

    :0:
    * ^X-Spam-Status: Yes
    spam/.

If you don't use \"spamc\", use \"spamassassin -P -a\".

Note that one of the recipes above throws away messages with a
score greater than or equal to 10. Here's how you can determine a
value that works best for you.

First, run \"spamassassin -t\" on every mail message in your
archive and use Gnumeric to verify that the average plus the
standard deviation of good mail is under 5, the SpamAssassin
default for \"spam\".

Using Gnumeric, sort the messages by score and view the messages
with the highest score. Determine the score which encompasses all
of your interesting messages and add a couple of points to be
conservative. Add that many dots to the \"X-Spam-Level:\" header
field above to send messages with that score down the drain.

In the example above, messages with a score of 5-9 are set aside
in the \"+spam\" folder for later review. The major weakness of
rules-based filters is a plethora of false positives so it is
worthwhile to check.

If SpamAssassin classifies a message incorrectly, or is unsure,
you can use the MH-E commands \\[mh-junk-blacklist] and
\\[mh-junk-whitelist].

The command \\[mh-junk-blacklist] adds a \"blacklist_from\" entry
to \"~/spamassassin/user_prefs\", deletes the message, and sends
the message to the Razor, so that others might not see this spam.
If the \"sa-learn\" command is available, the message is also
recategorized as spam.

The command \\[mh-junk-whitelist] adds a \"whitelist_from\" rule
to the \"~/.spamassassin/user_prefs\" file. If the \"sa-learn\"
command is available, the message is also recategorized as ham.

Over time, you'll observe that the same host or domain occurs
repeatedly in the \"blacklist_from\" entries, so you might think
that you could avoid future spam by blacklisting all mail from a
particular domain. The utility function
`mh-spamassassin-identify-spammers' helps you do precisely that.
This function displays a frequency count of the hosts and domains
in the \"blacklist_from\" entries from the last blank line in
\"~/.spamassassin/user_prefs\" to the end of the file. This
information can be used so that you can replace multiple
\"blacklist_from\" entries with a single wildcard entry such as:

    blacklist_from *@*amazingoffersdirect2u.com

In versions of SpamAssassin (2.50 and on) that support a Bayesian
classifier, \\[mh-junk-blacklist] uses the program \"sa-learn\"
to recategorize the message as spam. Neither MH-E, nor
SpamAssassin, rebuilds the database after adding words, so you
will need to run \"sa-learn --rebuild\" periodically. This can be
done by adding the following to your crontab:

    0 * * * *   sa-learn --rebuild > /dev/null 2>&1

\(fn MSG)" nil nil)

(autoload 'mh-spamassassin-whitelist "mh-junk" "\
Whitelist MSG with SpamAssassin.

The \\[mh-junk-whitelist] command adds a \"whitelist_from\" rule to
the \"~/.spamassassin/user_prefs\" file. If the \"sa-learn\" command
is available, the message is also recategorized as ham.

See `mh-spamassassin-blacklist' for more information.

\(fn MSG)" nil nil)

(autoload 'mh-spamassassin-identify-spammers "mh-junk" "\
Identify spammers who are repeat offenders.

This function displays a frequency count of the hosts and domains
in the \"blacklist_from\" entries from the last blank line in
\"~/.spamassassin/user_prefs\" to the end of the file. This
information can be used so that you can replace multiple
\"blacklist_from\" entries with a single wildcard entry such as:

    blacklist_from *@*amazingoffersdirect2u.com

\(fn)" t nil)

(autoload 'mh-bogofilter-blacklist "mh-junk" "\
Blacklist MSG with bogofilter.

Bogofilter is a Bayesian spam filtering program. Get it from your
local distribution or from the bogofilter web site at URL
`http://bogofilter.sourceforge.net/'.

Bogofilter is taught by running:

    bogofilter -n < good-message

on every good message, and

    bogofilter -s < spam-message

on every spam message. This is called a full training; three other
training methods are described in the FAQ that is distributed with
bogofilter. Note that most Bayesian filters need 1000 to 5000 of each
type of message to start doing a good job.

To use bogofilter, add the following recipes to \".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with bogofilter.
    :0fw
    | bogofilter -3 -e -p

    :0:
    * ^X-Bogosity: Yes, tests=bogofilter
    spam/.

    :0:
    * ^X-Bogosity: Unsure, tests=bogofilter
    spam/unsure/.

If bogofilter classifies a message incorrectly, or is unsure, you can
use the MH-E commands \\[mh-junk-blacklist] and \\[mh-junk-whitelist]
to update bogofilter's training.

The \"Bogofilter FAQ\" suggests that you run the following
occasionally to shrink the database:

    bogoutil -d wordlist.db | bogoutil -l wordlist.db.new
    mv wordlist.db wordlist.db.prv
    mv wordlist.db.new wordlist.db

The \"Bogofilter tuning HOWTO\" describes how you can fine-tune Bogofilter.

\(fn MSG)" nil nil)

(autoload 'mh-bogofilter-whitelist "mh-junk" "\
Whitelist MSG with bogofilter.

See `mh-bogofilter-blacklist' for more information.

\(fn MSG)" nil nil)

(autoload 'mh-spamprobe-blacklist "mh-junk" "\
Blacklist MSG with SpamProbe.

SpamProbe is a Bayesian spam filtering program. Get it from your
local distribution or from the SpamProbe web site at URL
`http://spamprobe.sourceforge.net'.

To use SpamProbe, add the following recipes to \".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with SpamProbe.
    :0
    SCORE=| spamprobe receive

    :0 wf
    | formail -I \"X-SpamProbe: $SCORE\"

    :0:
    *^X-SpamProbe: SPAM
    spam/.

If SpamProbe classifies a message incorrectly, you can use the
MH-E commands \\[mh-junk-blacklist] and \\[mh-junk-whitelist] to
update SpamProbe's training.

\(fn MSG)" nil nil)

(autoload 'mh-spamprobe-whitelist "mh-junk" "\
Whitelist MSG with SpamProbe.

See `mh-spamprobe-blacklist' for more information.

\(fn MSG)" nil nil)

;;;***

;;;### (autoloads (mh-letter-toggle-header-field-display-button mh-complete-word
;;;;;;  mh-position-on-field mh-letter-next-header-field mh-yank-cur-msg
;;;;;;  mh-insert-signature mh-letter-mode) "mh-letter" "mh-letter.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from mh-letter.el

(autoload 'mh-letter-mode "mh-letter" "\
Mode for composing letters in MH-E\\<mh-letter-mode-map>.

When you have finished composing, type \\[mh-send-letter] to send
the message using the MH mail handling system.

There are two types of tags used by MH-E when composing MIME
messages: MML and MH. The option `mh-compose-insertion' controls
what type of tags are inserted by MH-E commands. These tags can
be converted to MIME body parts by running \\[mh-mh-to-mime] for
MH-style directives or \\[mh-mml-to-mime] for MML tags.

Options that control this mode can be changed with
\\[customize-group]; specify the \"mh-compose\" group.

When a message is composed, the hooks `text-mode-hook',
`mail-mode-hook', and `mh-letter-mode-hook' are run (in that
order).

\\{mh-letter-mode-map}

\(fn)" t nil)

(autoload 'mh-insert-signature "mh-letter" "\
Insert signature in message.

This command inserts your signature at the current cursor location.

By default, the text of your signature is taken from the file
\"~/.signature\". You can read from other sources by changing the
option `mh-signature-file-name'.

A signature separator (\"-- \") will be added if the signature block
does not contain one and `mh-signature-separator-flag' is on.

The hook `mh-insert-signature-hook' is run after the signature is
inserted. Hook functions may access the actual name of the file or the
function used to insert the signature with `mh-signature-file-name'.

The signature can also be inserted using Identities (see
`mh-identity-list').

In a program, you can pass in a signature FILE.

\(fn &optional FILE)" t nil)

(autoload 'mh-yank-cur-msg "mh-letter" "\
Insert the current message into the draft buffer.

It is often useful to insert a snippet of text from a letter that
someone mailed to provide some context for your reply. This
command does this by adding an attribution, yanking a portion of
text from the message to which you're replying, and inserting
`mh-ins-buf-prefix' (`> ') before each line.

The attribution consists of the sender's name and email address
followed by the content of the option
`mh-extract-from-attribution-verb'.

You can also turn on the option
`mh-delete-yanked-msg-window-flag' to delete the window
containing the original message after yanking it to make more
room on your screen for your reply.

You can control how the message to which you are replying is
yanked into your reply using `mh-yank-behavior'.

If this isn't enough, you can gain full control over the
appearance of the included text by setting `mail-citation-hook'
to a function that modifies it. For example, if you set this hook
to `trivial-cite' (which is NOT part of Emacs), set
`mh-yank-behavior' to \"Body and Header\" (see URL
`http://shasta.cs.uiuc.edu/~lrclause/tc.html').

Note that if `mail-citation-hook' is set, `mh-ins-buf-prefix' is
not inserted. If the option `mh-yank-behavior' is set to one of
the supercite flavors, the hook `mail-citation-hook' is ignored
and `mh-ins-buf-prefix' is not inserted.

\(fn)" t nil)

(autoload 'mh-letter-next-header-field "mh-letter" "\
Cycle to the next header field.
If we are at the last header field go to the start of the message
body.

\(fn)" nil nil)

(autoload 'mh-position-on-field "mh-letter" "\
Move to the end of the FIELD in the header.
Move to end of entire header if FIELD not found.
Returns non-nil if FIELD was found.
The optional second arg is for pre-version 4 compatibility and is
IGNORED.

\(fn FIELD &optional IGNORED)" nil nil)

(autoload 'mh-complete-word "mh-letter" "\
Complete WORD from CHOICES.
Any match found replaces the text from BEGIN to END.

\(fn WORD CHOICES BEGIN END)" nil nil)

(autoload 'mh-letter-toggle-header-field-display-button "mh-letter" "\
Toggle header field display at location of EVENT.
This function does the same thing as
`mh-letter-toggle-header-field-display' except that it is
callable from a mouse button.

\(fn EVENT)" t nil)

;;;***

;;;### (autoloads (mh-narrow-to-to mh-narrow-to-subject mh-narrow-to-range
;;;;;;  mh-narrow-to-from mh-narrow-to-cc mh-delete-subject-or-thread
;;;;;;  mh-delete-subject) "mh-limit" "mh-limit.el" (20352 65510))
;;; Generated autoloads from mh-limit.el

(autoload 'mh-delete-subject "mh-limit" "\
Delete messages with same subject\\<mh-folder-mode-map>.

To delete messages faster, you can use this command to delete all
the messages with the same subject as the current message. This
command puts these messages in a sequence named \"subject\". You
can undo this action by using \\[mh-undo] with a prefix argument
and then specifying the \"subject\" sequence.

\(fn)" t nil)

(autoload 'mh-delete-subject-or-thread "mh-limit" "\
Delete messages with same subject or thread\\<mh-folder-mode-map>.

To delete messages faster, you can use this command to delete all
the messages with the same subject as the current message. This
command puts these messages in a sequence named \"subject\". You
can undo this action by using \\[mh-undo] with a prefix argument
and then specifying the \"subject\" sequence.

However, if the buffer is displaying a threaded view of the
folder then this command behaves like \\[mh-thread-delete].

\(fn)" t nil)

(autoload 'mh-narrow-to-cc "mh-limit" "\
Limit to messages with the same \"Cc:\" field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command.

\(fn &optional PICK-EXPR)" t nil)

(autoload 'mh-narrow-to-from "mh-limit" "\
Limit to messages with the same \"From:\" field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command.

\(fn &optional PICK-EXPR)" t nil)

(autoload 'mh-narrow-to-range "mh-limit" "\
Limit to RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command.

\(fn RANGE)" t nil)

(autoload 'mh-narrow-to-subject "mh-limit" "\
Limit to messages with same subject.
With a prefix argument, edit PICK-EXPR.
The string Re: is removed from the search.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command.

\(fn &optional PICK-EXPR)" t nil)

(autoload 'mh-narrow-to-to "mh-limit" "\
Limit to messages with the same \"To:\" field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command.

\(fn &optional PICK-EXPR)" t nil)

;;;***

;;;### (autoloads (mh-destroy-postponed-handles mh-mime-cleanup mh-have-file-command
;;;;;;  mh-file-mime-type mh-mh-directive-present-p mh-mml-tag-present-p
;;;;;;  mh-mml-unsecure-message mh-mml-to-mime mh-mml-secure-message-signencrypt
;;;;;;  mh-mml-secure-message-sign mh-mml-secure-message-encrypt
;;;;;;  mh-mh-to-mime-undo mh-mh-to-mime mh-mh-compose-external-type
;;;;;;  mh-mh-compose-external-compressed-tar mh-mh-compose-anon-ftp
;;;;;;  mh-compose-insertion mh-mml-forward-message mh-compose-forward
;;;;;;  mh-display-emphasis mh-display-smileys mh-add-missing-mime-version-header
;;;;;;  mh-goto-next-button mh-mime-display mh-decode-message-subject
;;;;;;  mh-decode-message-header mh-toggle-mime-buttons mh-toggle-mh-decode-mime-flag
;;;;;;  mh-mime-save-parts mh-folder-toggle-mime-part mh-folder-save-mime-part
;;;;;;  mh-folder-inline-mime-part mh-display-with-external-viewer
;;;;;;  mh-buffer-data) "mh-mime" "mh-mime.el" (20352 65510))
;;; Generated autoloads from mh-mime.el

(autoload 'mh-buffer-data "mh-mime" "\
Convenience macro to get the MIME data structures of the current buffer.

\(fn)" nil (quote macro))

(autoload 'mh-display-with-external-viewer "mh-mime" "\
View attachment externally.

If Emacs does not know how to view an attachment, you could save
it into a file and then run some program to open it. It is
easier, however, to launch the program directly from MH-E with
this command. While you'll most likely use this to view
spreadsheets and documents, it is also useful to use your browser
to view HTML attachments with higher fidelity than what Emacs can
provide.

This command displays the attachment associated with the button
under the cursor. If the cursor is not located over a button,
then the cursor first moves to the next button, wrapping to the
beginning of the message if necessary. You can provide a numeric
prefix argument PART-INDEX to view the attachment labeled with
that number.

This command tries to provide a reasonable default for the viewer
by calling the Emacs function `mailcap-mime-info'. This function
usually reads the file \"/etc/mailcap\".

\(fn PART-INDEX)" t nil)

(autoload 'mh-folder-inline-mime-part "mh-mime" "\
Show attachment verbatim.

You can view the raw contents of an attachment with this command.
This command displays (or hides) the contents of the attachment
associated with the button under the cursor verbatim. If the
cursor is not located over a button, then the cursor first moves
to the next button, wrapping to the beginning of the message if
necessary.

You can also provide a numeric prefix argument PART-INDEX to view
the attachment labeled with that number.

\(fn PART-INDEX)" t nil)

(autoload 'mh-folder-save-mime-part "mh-mime" "\
Save (output) attachment.

This command saves the attachment associated with the button under the
cursor. If the cursor is not located over a button, then the cursor
first moves to the next button, wrapping to the beginning of the
message if necessary.

You can also provide a numeric prefix argument PART-INDEX to save the
attachment labeled with that number.

This command prompts you for a filename and suggests a specific name
if it is available.

\(fn PART-INDEX)" t nil)

(autoload 'mh-folder-toggle-mime-part "mh-mime" "\
View attachment.

This command displays (or hides) the attachment associated with
the button under the cursor. If the cursor is not located over a
button, then the cursor first moves to the next button, wrapping
to the beginning of the message if necessary. This command has
the advantage over related commands of working from the MH-Folder
buffer.

You can also provide a numeric prefix argument PART-INDEX to view
the attachment labeled with that number. If Emacs does not know
how to display the attachment, then Emacs offers to save the
attachment in a file.

\(fn PART-INDEX)" t nil)

(autoload 'mh-mime-save-parts "mh-mime" "\
Save attachments.

You can save all of the attachments at once with this command.
The attachments are saved in the directory specified by the
option `mh-mime-save-parts-default-directory' unless you use a
prefix argument PROMPT in which case you are prompted for the
directory. These directories may be superseded by MH profile
components, since this function calls on \"mhstore\" (\"mhn\") to
do the work.

\(fn PROMPT)" t nil)

(autoload 'mh-toggle-mh-decode-mime-flag "mh-mime" "\
Toggle the value of `mh-decode-mime-flag'.

\(fn)" t nil)

(autoload 'mh-toggle-mime-buttons "mh-mime" "\
Toggle option `mh-display-buttons-for-inline-parts-flag'.

\(fn)" t nil)

(autoload 'mh-decode-message-header "mh-mime" "\
Decode RFC2047 encoded message header fields.

\(fn)" nil nil)

(autoload 'mh-decode-message-subject "mh-mime" "\
Decode RFC2047 encoded message header fields.

\(fn)" nil nil)

(autoload 'mh-mime-display "mh-mime" "\
Display (and possibly decode) MIME handles.
Optional argument, PRE-DISSECTED-HANDLES is a list of MIME
handles. If present they are displayed otherwise the buffer is
parsed and then displayed.

\(fn &optional PRE-DISSECTED-HANDLES)" nil nil)

(autoload 'mh-goto-next-button "mh-mime" "\
Search for next button satisfying criterion.

If BACKWARD-FLAG is non-nil search backward in the buffer for a mime
button.
If CRITERION is a function or a symbol which has a function binding
then that function must return non-nil at the button we stop.

\(fn BACKWARD-FLAG &optional CRITERION)" nil nil)

(autoload 'mh-add-missing-mime-version-header "mh-mime" "\
Some mail programs don't put a MIME-Version header.
I have seen this only in spam, so maybe we shouldn't fix
this ;-)

\(fn)" nil nil)

(autoload 'mh-display-smileys "mh-mime" "\
Display smileys.

\(fn)" nil nil)

(autoload 'mh-display-emphasis "mh-mime" "\
Display graphical emphasis.

\(fn)" nil nil)

(autoload 'mh-compose-forward "mh-mime" "\
Add tag to forward a message.

You are prompted for a content DESCRIPTION, the name of the
FOLDER in which the messages to forward are located, and a RANGE
of messages, which defaults to the current message in that
folder. Check the documentation of `mh-interactive-range' to see
how RANGE is read in interactive use.

The option `mh-compose-insertion' controls what type of tags are inserted.

\(fn &optional DESCRIPTION FOLDER RANGE)" t nil)

(autoload 'mh-mml-forward-message "mh-mime" "\
Forward a message as attachment.

The function will prompt the user for a DESCRIPTION, a FOLDER and
MESSAGE number.

\(fn DESCRIPTION FOLDER MESSAGE)" nil nil)

(autoload 'mh-compose-insertion "mh-mime" "\
Add tag to include a file such as an image or sound.

You are prompted for the filename containing the object, the
media type if it cannot be determined automatically, and a
content description. If you're using MH-style directives, you
will also be prompted for additional attributes.

The option `mh-compose-insertion' controls what type of tags are
inserted. Optional argument INLINE means make it an inline
attachment.

\(fn &optional INLINE)" t nil)

(autoload 'mh-mh-compose-anon-ftp "mh-mime" "\
Add tag to include anonymous ftp reference to a file.

You can have your message initiate an \"ftp\" transfer when the
recipient reads the message. You are prompted for the remote HOST
and FILENAME, the media TYPE, and the content DESCRIPTION.

See also \\[mh-mh-to-mime].

\(fn HOST FILENAME TYPE DESCRIPTION)" t nil)

(autoload 'mh-mh-compose-external-compressed-tar "mh-mime" "\
Add tag to include anonymous ftp reference to a compressed tar file.

In addition to retrieving the file via anonymous \"ftp\" as per
the command \\[mh-mh-compose-anon-ftp], the file will also be
uncompressed and untarred. You are prompted for the remote HOST
and FILENAME and the content DESCRIPTION.

See also \\[mh-mh-to-mime].

\(fn HOST FILENAME DESCRIPTION)" t nil)

(autoload 'mh-mh-compose-external-type "mh-mime" "\
Add tag to refer to a remote file.

This command is a general utility for referencing external files.
In fact, all of the other commands that insert directives to
access external files call this command. You are prompted for the
ACCESS-TYPE, remote HOST and FILENAME, and content TYPE. If you
provide a prefix argument, you are also prompted for a content
DESCRIPTION, ATTRIBUTES, PARAMETERS, and a COMMENT.

See also \\[mh-mh-to-mime].

\(fn ACCESS-TYPE HOST FILENAME TYPE &optional DESCRIPTION ATTRIBUTES PARAMETERS COMMENT)" t nil)

(autoload 'mh-mh-to-mime "mh-mime" "\
Compose MIME message from MH-style directives.

Typically, you send a message with attachments just like any other
message. However, you may take a sneak preview of the MIME encoding if
you wish by running this command.

If you wish to pass additional arguments to \"mhbuild\" (\"mhn\")
to affect how it builds your message, use the option
`mh-mh-to-mime-args'. For example, you can build a consistency
check into the message by setting `mh-mh-to-mime-args' to
\"-check\". The recipient of your message can then run \"mhbuild
-check\" on the message--\"mhbuild\" (\"mhn\") will complain if
the message has been corrupted on the way. This command only
consults this option when given a prefix argument EXTRA-ARGS.

The hook `mh-mh-to-mime-hook' is called after the message has been
formatted.

The effects of this command can be undone by running
\\[mh-mh-to-mime-undo].

\(fn &optional EXTRA-ARGS)" t nil)

(autoload 'mh-mh-to-mime-undo "mh-mime" "\
Undo effects of \\[mh-mh-to-mime].

It does this by reverting to a backup file. You are prompted to
confirm this action, but you can avoid the confirmation by adding
a prefix argument NOCONFIRM.

\(fn NOCONFIRM)" t nil)

(autoload 'mh-mml-secure-message-encrypt "mh-mime" "\
Add tag to encrypt the message.

A proper multipart message is created for you when you send the
message. Use the command \\[mh-mml-unsecure-message] to remove
this tag. Use a prefix argument METHOD to be prompted for one of
the possible security methods (see `mh-mml-method-default').

\(fn METHOD)" t nil)

(autoload 'mh-mml-secure-message-sign "mh-mime" "\
Add tag to sign the message.

A proper multipart message is created for you when you send the
message. Use the command \\[mh-mml-unsecure-message] to remove
this tag. Use a prefix argument METHOD to be prompted for one of
the possible security methods (see `mh-mml-method-default').

\(fn METHOD)" t nil)

(autoload 'mh-mml-secure-message-signencrypt "mh-mime" "\
Add tag to encrypt and sign the message.

A proper multipart message is created for you when you send the
message. Use the command \\[mh-mml-unsecure-message] to remove
this tag. Use a prefix argument METHOD to be prompted for one of
the possible security methods (see `mh-mml-method-default').

\(fn METHOD)" t nil)

(autoload 'mh-mml-to-mime "mh-mime" "\
Compose MIME message from MML tags.

Typically, you send a message with attachments just like any
other message. However, you may take a sneak preview of the MIME
encoding if you wish by running this command.

This action can be undone by running \\[undo].

\(fn)" t nil)

(autoload 'mh-mml-unsecure-message "mh-mime" "\
Remove any secure message tags.

\(fn)" t nil)

(autoload 'mh-mml-tag-present-p "mh-mime" "\
Check if the current buffer has text which may be a MML tag.

\(fn)" nil nil)

(autoload 'mh-mh-directive-present-p "mh-mime" "\
Check if the text between BEGIN and END might be a MH-style directive.
The optional argument BEGIN defaults to the beginning of the
buffer, while END defaults to the end of the buffer.

\(fn &optional BEGIN END)" nil nil)

(autoload 'mh-file-mime-type "mh-mime" "\
Return MIME type of FILENAME from file command.
Returns nil if file command not on system.

\(fn FILENAME)" nil nil)

(autoload 'mh-have-file-command "mh-mime" "\
Return t if 'file' command is on the system.
'file -i' is used to get MIME type of composition insertion.

\(fn)" nil nil)

(autoload 'mh-mime-cleanup "mh-mime" "\
Free the decoded MIME parts.

\(fn)" nil nil)

(autoload 'mh-destroy-postponed-handles "mh-mime" "\
Free MIME data for externally displayed MIME parts.

\(fn)" nil nil)

;;;***

;;;### (autoloads (mh-print-msg mh-ps-print-toggle-color mh-ps-print-toggle-faces
;;;;;;  mh-ps-print-msg-file mh-ps-print-msg) "mh-print" "mh-print.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from mh-print.el

(autoload 'mh-ps-print-msg "mh-print" "\
Print RANGE\\<mh-folder-mode-map>.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

This command will print inline text attachments but will not decrypt
messages. However, when a message is displayed in an MH-Show buffer,
then that buffer is used verbatim for printing with the caveat that
only text attachments, if opened inline, are printed. Therefore,
encrypted messages can be printed by showing and decrypting them
first.

MH-E uses the \"ps-print\" package to do the printing, so you can
customize the printing further by going to the `ps-print'
customization group. This command does not use the options
`mh-lpr-command-format' or `mh-print-background-flag'. See also the
commands \\[mh-ps-print-toggle-color] and
\\[mh-ps-print-toggle-faces].

\(fn RANGE)" t nil)

(autoload 'mh-ps-print-msg-file "mh-print" "\
Print RANGE to FILE\\<mh-folder-mode-map>.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

This command will print inline text attachments but will not decrypt
messages. However, when a message is displayed in an MH-Show buffer,
then that buffer is used verbatim for printing with the caveat that
only text attachments, if opened inline, are printed. Therefore,
encrypted messages can be printed by showing and decrypting them
first.

MH-E uses the \"ps-print\" package to do the printing, so you can
customize the printing further by going to the `ps-print'
customization group. This command does not use the options
`mh-lpr-command-format' or `mh-print-background-flag'. See also the
commands \\[mh-ps-print-toggle-color] and
\\[mh-ps-print-toggle-faces].

\(fn RANGE FILE)" t nil)

(autoload 'mh-ps-print-toggle-faces "mh-print" "\
Toggle whether printing is done with faces or not.

When faces are enabled, the printed message will look very
similar to the message in the MH-Show buffer.

\(fn)" t nil)

(autoload 'mh-ps-print-toggle-color "mh-print" "\
Toggle whether color is used in printing messages.

Colors are emulated on black-and-white printers with shades of
gray. This might produce illegible output, even if your screen
colors only use shades of gray. If this is the case, try using
this command to toggle between color, no color, and a black and
white representation of the colors and see which works best. You
change this setting permanently by customizing the option
`ps-print-color-p'.

\(fn)" t nil)

(autoload 'mh-print-msg "mh-print" "\
Print RANGE the old fashioned way\\<mh-folder-mode-map>.

The message is formatted with \"mhl\" (see option
`mh-mhl-format-file') and printed with the \"lpr\" command (see
option `mh-lpr-command-format').

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

Consider using \\[mh-ps-print-msg] instead.

\(fn RANGE)" t nil)

;;;***

;;;### (autoloads (mh-msg-num-width-to-column mh-msg-num-width mh-scan-format
;;;;;;  mh-set-cmd-note mh-scan-msg-search-regexp mh-scan-msg-number-regexp)
;;;;;;  "mh-scan" "mh-scan.el" (20352 65510))
;;; Generated autoloads from mh-scan.el

(autoload 'mh-scan-msg-number-regexp "mh-scan" "\
Return value of variable `mh-scan-msg-number-regexp'.

\(fn)" nil nil)

(autoload 'mh-scan-msg-search-regexp "mh-scan" "\
Return value of variable `mh-scan-msg-search-regexp'.

\(fn)" nil nil)

(autoload 'mh-set-cmd-note "mh-scan" "\
Set `mh-cmd-note' to COLUMN.
Note that columns in Emacs start with 0.

\(fn COLUMN)" nil nil)

(autoload 'mh-scan-format "mh-scan" "\
Return the output format argument for the scan program.

\(fn)" nil nil)

(autoload 'mh-msg-num-width "mh-scan" "\
Return the width of the largest message number in this FOLDER.

\(fn FOLDER)" nil nil)

(autoload 'mh-msg-num-width-to-column "mh-scan" "\
Return the column for notations given message number WIDTH.
Note that columns in Emacs start with 0.

If `mh-scan-format-file' is set to \"Use MH-E scan Format\" this
means that either `mh-scan-format-mh' or `mh-scan-format-nmh' are
in use.  This function therefore assumes that the first column is
empty (to provide room for the cursor), the following WIDTH
columns contain the message number, and the column for notations
comes after that.

\(fn WIDTH)" nil nil)

;;;***

;;;### (autoloads (mh-index-update-maps mh-index-read-data mh-index-delete-from-sequence
;;;;;;  mh-index-add-to-sequence mh-create-sequence-map mh-index-create-sequences
;;;;;;  mh-index-execute-commands mh-search-p mh-index-create-imenu-index
;;;;;;  mh-index-delete-folder-headers mh-index-insert-folder-headers
;;;;;;  mh-index-group-by-folder mh-index-visit-folder mh-index-previous-folder
;;;;;;  mh-index-next-folder mh-index-sequenced-messages mh-index-ticked-messages
;;;;;;  mh-index-new-messages mh-search) "mh-search" "mh-search.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from mh-search.el

(autoload 'mh-search "mh-search" "\
Search your MH mail.

This command helps you find messages in your entire corpus of
mail. You can search for messages to or from a particular person
or about a particular subject. In fact, you can also search for
messages containing selected strings in any arbitrary header
field or any string found within the messages.

Out of the box, MH-E uses \"pick\" to find messages. With a
little extra effort, you can set an indexing program which
rewards you with extremely quick results. The drawback is that
sometimes the index does not contain the words you're looking
for. You can still use \"pick\" in these situations.

You are prompted for the FOLDER to search. This can be \"all\" to
search all folders. Note that the search works recursively on the
listed folder.

Next, an MH-Search buffer appears where you can enter search
criteria SEARCH-REGEXP.

     From:
     To:
     Cc:
     Date:
     Subject:
     --------

Edit this template by entering your search criteria in an
appropriate header field that is already there, or create a new
field yourself. If the string you're looking for could be
anywhere in a message, then place the string underneath the row
of dashes.

As an example, let's say that we want to find messages from
Ginnean about horseback riding in the Kosciusko National
Park (Australia) during January, 1994. Normally we would start
with a broad search and narrow it down if necessary to produce a
manageable amount of data, but we'll cut to the chase and create
a fairly restrictive set of criteria as follows:\\<mh-search-mode-map>

     From: ginnean
     To:
     Cc:
     Date: Jan 1994
     Subject:
     --------
     horse
     kosciusko

As with MH-Letter mode, MH-Search provides commands like
\\[mh-to-field] to help you fill in the blanks.\\<mh-folder-mode-map>

If you find that you do the same thing over and over when editing
the search template, you may wish to bind some shortcuts to keys.
This can be done with the variable `mh-search-mode-hook', which is
called when \\[mh-search] is run on a new pattern.\\<mh-search-mode-map>

To perform the search, type \\[mh-index-do-search].

Sometimes you're searching for text that is either not indexed,
or hasn't been indexed yet. In this case you can override the
default method with the pick method by running the command
\\[mh-pick-do-search].

The messages that are found are put in a temporary sub-folder of
\"+mhe-index\" and are displayed in an MH-Folder buffer. This
buffer is special because it displays messages from multiple
folders; each set of messages from a given folder has a heading
with the folder name.\\<mh-folder-mode-map>

The appearance of the heading can be modified by customizing the
face `mh-search-folder'. You can jump back and forth between the
headings using the commands \\[mh-index-next-folder] and
\\[mh-index-previous-folder].

In addition, the command \\[mh-index-visit-folder] can be used to
visit the folder of the message at point. Initially, only the
messages that matched the search criteria are displayed in the
folder. While the temporary buffer has its own set of message
numbers, the actual messages numbers are shown in the visited
folder. Thus, the command \\[mh-index-visit-folder] is useful to
find the actual message number of an interesting message, or to
view surrounding messages with the command \\[mh-rescan-folder].

Because this folder is temporary, you'll probably get in the
habit of killing it when you're done with \\[mh-kill-folder].

You can regenerate the results by running this command with a
prefix argument REDO-SEARCH-FLAG.

Note: This command uses an \"X-MHE-Checksum:\" header field to
cache the MD5 checksum of a message. This means that if an
incoming message already contains an \"X-MHE-Checksum:\" field,
that message might not be found by this command. The following
\"procmail\" recipe avoids this problem by renaming the existing
header field:

     :0 wf
     | formail -R \"X-MHE-Checksum\" \"X-Old-MHE-Checksum\"

Configuring Indexed Searches

The command \\[mh-search] runs the command defined by the option
`mh-search-program'. The default value is \"Auto-detect\" which
means that MH-E will automatically choose one of \"swish++\",
\"swish-e\", \"mairix\", \"namazu\", \"pick\" and \"grep\" in
that order. If, for example, you have both \"swish++\" and
\"mairix\" installed and you want to use \"mairix\", then you can
set this option to \"mairix\".

The documentation for the following commands describe how to set
up the various indexing programs to use with MH-E.

    - `mh-swish++-execute-search'
    - `mh-swish-execute-search'
    - `mh-mairix-execute-search'
    - `mh-namazu-execute-search'
    - `mh-pick-execute-search'
    - `mh-grep-execute-search'

In a program, if FOLDER is \"+\" or nil, then mail in all folders
are searched. Optional argument WINDOW-CONFIG stores the window
configuration that will be restored after the user quits the
folder containing the index search results.

\(fn FOLDER SEARCH-REGEXP &optional REDO-SEARCH-FLAG WINDOW-CONFIG)" t nil)

(autoload 'mh-index-new-messages "mh-search" "\
Display unseen messages.

If you use a program such as \"procmail\" to use \"rcvstore\" to file
your incoming mail automatically, you can display new, unseen,
messages using this command. All messages in the \"unseen\"
sequence from the folders in `mh-new-messages-folders' are
listed.

With a prefix argument, enter a space-separated list of FOLDERS,
or nothing to search all folders.

\(fn FOLDERS)" t nil)

(autoload 'mh-index-ticked-messages "mh-search" "\
Display ticked messages.

All messages in `mh-tick-seq' from the folders in
`mh-ticked-messages-folders' are listed.

With a prefix argument, enter a space-separated list of FOLDERS,
or nothing to search all folders.

\(fn FOLDERS)" t nil)

(autoload 'mh-index-sequenced-messages "mh-search" "\
Display messages in any sequence.

All messages from the FOLDERS in `mh-new-messages-folders' in the
SEQUENCE you provide are listed. With a prefix argument, enter a
space-separated list of folders at the prompt, or nothing to
search all folders.

\(fn FOLDERS SEQUENCE)" t nil)

(autoload 'mh-index-next-folder "mh-search" "\
Jump to the next folder marker.

With non-nil optional argument BACKWARD-FLAG, jump to the previous
group of results.

\(fn &optional BACKWARD-FLAG)" t nil)

(autoload 'mh-index-previous-folder "mh-search" "\
Jump to the previous folder marker.

\(fn)" t nil)

(autoload 'mh-index-visit-folder "mh-search" "\
Visit original folder from where the message at point was found.

\(fn)" t nil)

(autoload 'mh-index-group-by-folder "mh-search" "\
Partition the messages based on source folder.
Returns an alist with the folder names in the car and the cdr
being the list of messages originally from that folder.

\(fn)" nil nil)

(autoload 'mh-index-insert-folder-headers "mh-search" "\
Annotate the search results with original folder names.

\(fn)" nil nil)

(autoload 'mh-index-delete-folder-headers "mh-search" "\
Delete the folder headers.

\(fn)" nil nil)

(autoload 'mh-index-create-imenu-index "mh-search" "\
Create alist of folder names and positions in index folder buffers.

\(fn)" nil nil)

(autoload 'mh-search-p "mh-search" "\
Non-nil means that this folder was generated by searching.

\(fn)" nil nil)

(autoload 'mh-index-execute-commands "mh-search" "\
Delete/refile the actual messages.
The copies in the searched folder are then deleted/refiled to get
the desired result. Before deleting the messages we make sure
that the message being deleted is identical to the one that the
user has marked in the index buffer.

\(fn)" nil nil)

(autoload 'mh-index-create-sequences "mh-search" "\
Mirror sequences present in source folders in index folder.

\(fn)" nil nil)

(autoload 'mh-create-sequence-map "mh-search" "\
Return a map from msg number to list of sequences in which it is present.
SEQ-LIST is an assoc list whose keys are sequence names and whose
cdr is the list of messages in that sequence.

\(fn SEQ-LIST)" nil nil)

(autoload 'mh-index-add-to-sequence "mh-search" "\
Add to SEQ the messages in the list MSGS.
This function updates the source folder sequences. Also makes an
attempt to update the source folder buffer if we have it open.

\(fn SEQ MSGS)" nil nil)

(autoload 'mh-index-delete-from-sequence "mh-search" "\
Delete from SEQ the messages in MSGS.
This function updates the source folder sequences. Also makes an
attempt to update the source folder buffer if present.

\(fn SEQ MSGS)" nil nil)

(autoload 'mh-index-read-data "mh-search" "\
Read index data from file.

\(fn)" nil nil)

(autoload 'mh-index-update-maps "mh-search" "\
Annotate all as yet unannotated messages in FOLDER with their MD5 hash.
As a side effect msg -> checksum map is updated. Optional
argument ORIGIN-MAP is a hashtable which maps each message in the
index folder to the original folder and message from whence it
was copied. If present the checksum -> (origin-folder,
origin-index) map is updated too.

\(fn FOLDER &optional ORIGIN-MAP)" nil nil)

;;;***

;;;### (autoloads (mh-remove-all-notation mh-notate-user-sequences
;;;;;;  mh-notate-deleted-and-refiled mh-remove-cur-notation mh-notate-cur
;;;;;;  mh-notate mh-read-folder-sequences mh-parse-flist-output-line
;;;;;;  mh-translate-range mh-range-to-msg-list mh-read-range mh-interactive-range
;;;;;;  mh-valid-view-change-operation-p mh-add-msgs-to-seq mh-undefine-sequence
;;;;;;  mh-define-sequence mh-seq-to-msgs mh-find-seq mh-valid-seq-p
;;;;;;  mh-read-seq-default mh-widen mh-toggle-tick mh-put-msg-in-seq
;;;;;;  mh-narrow-to-tick mh-narrow-to-seq mh-msg-is-in-seq mh-list-sequences
;;;;;;  mh-delete-seq mh-delete-msg-from-seq mh-catchup) "mh-seq"
;;;;;;  "mh-seq.el" (20352 65510))
;;; Generated autoloads from mh-seq.el

(autoload 'mh-catchup "mh-seq" "\
Delete RANGE from the \"unseen\" sequence.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

\(fn RANGE)" t nil)

(autoload 'mh-delete-msg-from-seq "mh-seq" "\
Delete RANGE from SEQUENCE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

In a program, non-nil INTERNAL-FLAG means do not inform MH of the
change.

\(fn RANGE SEQUENCE &optional INTERNAL-FLAG)" t nil)

(autoload 'mh-delete-seq "mh-seq" "\
Delete SEQUENCE.

You are prompted for the sequence to delete. Note that this
deletes only the sequence, not the messages in the sequence. If
you want to delete the messages, use \"\\[universal-argument]
\\[mh-delete-msg]\".

\(fn SEQUENCE)" t nil)

(autoload 'mh-list-sequences "mh-seq" "\
List all sequences in folder.

The list appears in a buffer named \"*MH-E Sequences*\".

\(fn)" t nil)

(autoload 'mh-msg-is-in-seq "mh-seq" "\
Display the sequences in which the current message appears.

Use a prefix argument to display the sequences in which another
MESSAGE appears.

\(fn MESSAGE)" t nil)

(autoload 'mh-narrow-to-seq "mh-seq" "\
Restrict display to messages in SEQUENCE.

You are prompted for the name of the sequence. What this command
does is show only those messages that are in the selected
sequence in the MH-Folder buffer. In addition, it limits further
MH-E searches to just those messages.

When you want to widen the view to all your messages again, use
\\[mh-widen].

\(fn SEQUENCE)" t nil)

(autoload 'mh-narrow-to-tick "mh-seq" "\
Limit to ticked messages.

What this command does is show only those messages that are in
the \"tick\" sequence (which you can customize via the
`mh-tick-seq' option) in the MH-Folder buffer. In addition, it
limits further MH-E searches to just those messages. When you
want to widen the view to all your messages again, use
\\[mh-widen].

\(fn)" t nil)

(autoload 'mh-put-msg-in-seq "mh-seq" "\
Add RANGE to SEQUENCE\\<mh-folder-mode-map>.

Give this command a RANGE and you can add all the messages in a
sequence to another sequence (for example,
\"\\[universal-argument] \\[mh-put-msg-in-seq] SourceSequence RET
DestSequence RET\"). Check the documentation of
`mh-interactive-range' to see how RANGE is read in interactive
use.

\(fn RANGE SEQUENCE)" t nil)

(autoload 'mh-toggle-tick "mh-seq" "\
Toggle tick mark of RANGE.

This command adds messages to the \"tick\" sequence (which you can customize
via the option `mh-tick-seq'). This sequence can be viewed later with the
\\[mh-index-ticked-messages] command.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

\(fn RANGE)" t nil)

(autoload 'mh-widen "mh-seq" "\
Remove last restriction.

Each limit or sequence restriction can be undone in turn with
this command. Give this command a prefix argument ALL-FLAG to
remove all limits and sequence restrictions.

\(fn &optional ALL-FLAG)" t nil)

(autoload 'mh-read-seq-default "mh-seq" "\
Read and return sequence name with default narrowed or previous sequence.
PROMPT is the prompt to use when reading. If NOT-EMPTY is non-nil
then a non-empty sequence is read.

\(fn PROMPT NOT-EMPTY)" nil nil)

(autoload 'mh-valid-seq-p "mh-seq" "\
Return non-nil if NAME is a valid MH sequence name.

\(fn NAME)" nil nil)

(autoload 'mh-find-seq "mh-seq" "\
Return sequence NAME.

\(fn NAME)" nil nil)

(autoload 'mh-seq-to-msgs "mh-seq" "\
Return a list of the messages in SEQ.

\(fn SEQ)" nil nil)

(autoload 'mh-define-sequence "mh-seq" "\
Define the SEQ to contain the list of MSGS.
Do not mark pseudo-sequences or empty sequences.
Signals an error if SEQ is an invalid name.

\(fn SEQ MSGS)" nil nil)

(autoload 'mh-undefine-sequence "mh-seq" "\
Remove from the SEQ the list of MSGS.

\(fn SEQ MSGS)" nil nil)

(autoload 'mh-add-msgs-to-seq "mh-seq" "\
Add MSGS to SEQ.

Remove duplicates and keep sequence sorted. If optional
INTERNAL-FLAG is non-nil, do not mark the message in the scan
listing or inform MH of the addition.

If DONT-ANNOTATE-FLAG is non-nil then the annotations in the
folder buffer are not updated.

\(fn MSGS SEQ &optional INTERNAL-FLAG DONT-ANNOTATE-FLAG)" nil nil)

(autoload 'mh-valid-view-change-operation-p "mh-seq" "\
Check if the view change operation can be performed.
OP is one of 'widen and 'unthread.

\(fn OP)" nil nil)

(autoload 'mh-interactive-range "mh-seq" "\
Return interactive specification for message, sequence, range or region.
By convention, the name of this argument is RANGE.

If variable `transient-mark-mode' is non-nil and the mark is active,
then this function returns a cons-cell of the region.

If optional prefix argument is provided, then prompt for message range
with RANGE-PROMPT. A list of messages in that range is returned.

If a MH range is given, say something like last:20, then a list
containing the messages in that range is returned.

If DEFAULT non-nil then it is returned.

Otherwise, the message number at point is returned.

This function is usually used with `mh-iterate-on-range' in order to
provide a uniform interface to MH-E functions.

\(fn RANGE-PROMPT &optional DEFAULT)" nil nil)

(autoload 'mh-read-range "mh-seq" "\
Read a message range with PROMPT.

If FOLDER is non-nil then a range is read from that folder, otherwise
use `mh-current-folder'.

If DEFAULT is a string then use that as default range to return. If
DEFAULT is nil then ask user with default answer a range based on the
sequences that seem relevant. Finally if DEFAULT is t, try to avoid
prompting the user. Unseen messages, if present, are returned. If the
folder has fewer than `mh-large-folder' messages then \"all\" messages
are returned. Finally as a last resort prompt the user.

If EXPAND-FLAG is non-nil then a list of message numbers corresponding
to the input is returned. If this list is empty then an error is
raised. If EXPAND-FLAG is nil just return the input string. In this
case we don't check if the range is empty.

If ASK-FLAG is non-nil, then the user is always queried for a range of
messages. If ASK-FLAG is nil, then the function checks if the unseen
sequence is non-empty. If that is the case, `mh-unseen-seq', or the
list of messages in it depending on the value of EXPAND, is returned.
Otherwise if the folder has fewer than `mh-large-folder' messages then
the list of messages corresponding to \"all\" is returned. If neither
of the above holds then as a last resort the user is queried for a
range of messages.

If NUMBER-AS-RANGE-FLAG is non-nil, then if a number, N is read as
input, it is interpreted as the range \"last:N\".

This function replaces the existing function `mh-read-msg-range'.
Calls to:

  (mh-read-msg-range folder flag)

should be replaced with:

  (mh-read-range \"Suitable prompt\" folder t nil flag
                 mh-interpret-number-as-range-flag)

\(fn PROMPT &optional FOLDER DEFAULT EXPAND-FLAG ASK-FLAG NUMBER-AS-RANGE-FLAG)" nil nil)

(autoload 'mh-range-to-msg-list "mh-seq" "\
Return a list of messages for RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

\(fn RANGE)" nil nil)

(autoload 'mh-translate-range "mh-seq" "\
In FOLDER, translate the string EXPR to a list of messages numbers.

\(fn FOLDER EXPR)" nil nil)

(autoload 'mh-parse-flist-output-line "mh-seq" "\
Parse LINE to generate folder name, unseen messages and total messages.
If CURRENT-FOLDER is non-nil then it contains the current folder
name and it is used to avoid problems in corner cases involving
folders whose names end with a '+' character.

\(fn LINE &optional CURRENT-FOLDER)" nil nil)

(autoload 'mh-read-folder-sequences "mh-seq" "\
Read and return the predefined sequences for a FOLDER.
If SAVE-REFILES is non-nil, then keep the sequences
that note messages to be refiled.

\(fn FOLDER SAVE-REFILES)" nil nil)

(autoload 'mh-notate "mh-seq" "\
Mark MSG with the character NOTATION at position OFFSET.
Null MSG means the message at cursor.
If NOTATION is nil then no change in the buffer occurs.

\(fn MSG NOTATION OFFSET)" nil nil)

(autoload 'mh-notate-cur "mh-seq" "\
Mark the MH sequence cur.
In addition to notating the current message with `mh-note-cur'
the function uses `overlay-arrow-position' to put a marker in the
fringe.

\(fn)" nil nil)

(autoload 'mh-remove-cur-notation "mh-seq" "\
Remove old cur notation.

\(fn)" nil nil)

(autoload 'mh-notate-deleted-and-refiled "mh-seq" "\
Notate messages marked for deletion or refiling.
Messages to be deleted are given by `mh-delete-list' while
messages to be refiled are present in `mh-refile-list'.

\(fn)" nil nil)

(autoload 'mh-notate-user-sequences "mh-seq" "\
Mark user-defined sequences in RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use; if nil all messages are
notated.

\(fn &optional RANGE)" nil nil)

(autoload 'mh-remove-all-notation "mh-seq" "\
Remove all notations on all scan lines that MH-E introduces.

\(fn)" nil nil)

;;;***

;;;### (autoloads (mh-gnus-article-highlight-citation mh-show-addr
;;;;;;  mh-show-mode mh-show-font-lock-keywords-with-cite mh-show-font-lock-keywords
;;;;;;  mh-invalidate-show-buffer mh-clean-msg-header mh-display-msg
;;;;;;  mh-start-of-uncleaned-message mh-maybe-show mh-show-preferred-alternative
;;;;;;  mh-header-display mh-show) "mh-show" "mh-show.el" (20352
;;;;;;  65510))
;;; Generated autoloads from mh-show.el

(autoload 'mh-show "mh-show" "\
Display message\\<mh-folder-mode-map>.

If the message under the cursor is already displayed, this command
scrolls to the beginning of the message. MH-E normally hides a lot of
the superfluous header fields that mailers add to a message, but if
you wish to see all of them, use the command \\[mh-header-display].

Two hooks can be used to control how messages are displayed. The
first hook, `mh-show-mode-hook', is called early on in the
process of the message display. It is usually used to perform
some action on the message's content. The second hook,
`mh-show-hook', is the last thing called after messages are
displayed. It's used to affect the behavior of MH-E in general or
when `mh-show-mode-hook' is too early.

From a program, optional argument MESSAGE can be used to display an
alternative message. The optional argument REDISPLAY-FLAG forces the
redisplay of the message even if the show buffer was already
displaying the correct message.

See the \"mh-show\" customization group for a litany of options that
control what displayed messages look like.

\(fn &optional MESSAGE REDISPLAY-FLAG)" t nil)

(autoload 'mh-header-display "mh-show" "\
Display message with all header fields\\<mh-folder-mode-map>.

Use the command \\[mh-show] to show the message normally again.

\(fn)" t nil)

(autoload 'mh-show-preferred-alternative "mh-show" "\
Display message with the default preferred alternative.
This is as if `mm-discouraged-alternatives' is set to nil.

Use the command \\[mh-show] to show the message normally again.

\(fn)" t nil)

(autoload 'mh-maybe-show "mh-show" "\
Display message at cursor, but only if in show mode.
If optional arg MSG is non-nil, display that message instead.

\(fn &optional MSG)" nil nil)

(autoload 'mh-start-of-uncleaned-message "mh-show" "\
Position uninteresting headers off the top of the window.

\(fn)" nil nil)

(autoload 'mh-display-msg "mh-show" "\
Display MSG-NUM of FOLDER-NAME.
Sets the current buffer to the show buffer.

\(fn MSG-NUM FOLDER-NAME)" nil nil)

(autoload 'mh-clean-msg-header "mh-show" "\
Flush extraneous lines in message header.

Header is cleaned from START to the end of the message header.
INVISIBLE-HEADERS contains a regular expression specifying lines
to delete from the header. VISIBLE-HEADERS contains a regular
expression specifying the lines to display. INVISIBLE-HEADERS is
ignored if VISIBLE-HEADERS is non-nil.

\(fn START INVISIBLE-HEADERS VISIBLE-HEADERS)" nil nil)

(autoload 'mh-invalidate-show-buffer "mh-show" "\
Invalidate the show buffer so we must update it to use it.

\(fn)" nil nil)

(autoload 'mh-show-font-lock-keywords "mh-show" "\
Return variable `mh-show-font-lock-keywords'.

\(fn)" nil nil)

(autoload 'mh-show-font-lock-keywords-with-cite "mh-show" "\
Return variable `mh-show-font-lock-keywords-with-cite'.

\(fn)" nil nil)

(autoload 'mh-show-mode "mh-show" "\
Major mode for showing messages in MH-E.\\<mh-show-mode-map>

Email addresses and URLs in the message are highlighted if the
option `goto-address-highlight-p' is on, which it is by default.
To view the web page for a highlighted URL or to send a message
using a highlighted email address, use the middle mouse button or
\\[goto-address-at-point]. See Info node `(mh-e)Sending Mail' to
see how to configure Emacs to send the message using MH-E.

The hook `mh-show-mode-hook' is called upon entry to this mode.

See also `mh-folder-mode'.

\\{mh-show-mode-map}

\(fn)" t nil)

(autoload 'mh-show-addr "mh-show" "\
Use `goto-address'.

\(fn)" nil nil)

(autoload 'mh-gnus-article-highlight-citation "mh-show" "\
Highlight cited text in current buffer using Gnus.

\(fn)" t nil)

;;;***

;;;### (autoloads (mh-speed-add-folder mh-speed-invalidate-map mh-speed-flists
;;;;;;  mh-folder-speedbar-buttons) "mh-speed" "mh-speed.el" (20352
;;;;;;  65510))
;;; Generated autoloads from mh-speed.el

(autoload 'mh-folder-speedbar-buttons "mh-speed" "\
Interface function to create MH-E speedbar buffer.
BUFFER is the MH-E buffer for which the speedbar buffer is to be
created.

\(fn BUFFER)" nil nil)

(defalias 'mh-show-speedbar-buttons 'mh-folder-speedbar-buttons)

(defalias 'mh-letter-speedbar-buttons 'mh-folder-speedbar-buttons)

(autoload 'mh-speed-flists "mh-speed" "\
Execute flists -recurse and update message counts.
If FORCE is non-nil the timer is reset.

Any number of optional FOLDERS can be specified. If specified,
flists is run only for that one folder.

\(fn FORCE &rest FOLDERS)" t nil)

(autoload 'mh-speed-invalidate-map "mh-speed" "\
Remove FOLDER from various optimization caches.

\(fn FOLDER)" t nil)

(autoload 'mh-speed-add-folder "mh-speed" "\
Add FOLDER since it is being created.
The function invalidates the latest ancestor that is present.

\(fn FOLDER)" nil nil)

;;;***

;;;### (autoloads (mh-thread-forget-message mh-thread-add-spaces
;;;;;;  mh-thread-find-msg-subject mh-thread-update-scan-line-map
;;;;;;  mh-thread-print-scan-lines mh-thread-generate mh-thread-parse-scan-line
;;;;;;  mh-thread-inc mh-toggle-threads mh-thread-refile mh-thread-previous-sibling
;;;;;;  mh-thread-next-sibling mh-thread-delete mh-thread-ancestor)
;;;;;;  "mh-thread" "mh-thread.el" (20352 65510))
;;; Generated autoloads from mh-thread.el

(autoload 'mh-thread-ancestor "mh-thread" "\
Display ancestor of current message.

If you do not care for the way a particular thread has turned,
you can move up the chain of messages with this command. This
command can also take a prefix argument THREAD-ROOT-FLAG to jump
to the message that started everything.

\(fn &optional THREAD-ROOT-FLAG)" t nil)

(autoload 'mh-thread-delete "mh-thread" "\
Delete thread.

\(fn)" t nil)

(autoload 'mh-thread-next-sibling "mh-thread" "\
Display next sibling.

With non-nil optional argument PREVIOUS-FLAG jump to the previous
sibling.

\(fn &optional PREVIOUS-FLAG)" t nil)

(autoload 'mh-thread-previous-sibling "mh-thread" "\
Display previous sibling.

\(fn)" t nil)

(autoload 'mh-thread-refile "mh-thread" "\
Refile (output) thread into FOLDER.

\(fn FOLDER)" t nil)

(autoload 'mh-toggle-threads "mh-thread" "\
Toggle threaded view of folder.

\(fn)" t nil)

(autoload 'mh-thread-inc "mh-thread" "\
Update thread tree for FOLDER.
All messages after START-POINT are added to the thread tree.

\(fn FOLDER START-POINT)" nil nil)

(autoload 'mh-thread-parse-scan-line "mh-thread" "\
Parse a scan line.
If optional argument STRING is given then that is assumed to be
the scan line. Otherwise uses the line at point as the scan line
to parse.

\(fn &optional STRING)" nil nil)

(autoload 'mh-thread-generate "mh-thread" "\
Scan FOLDER to get info for threading.
Only information about messages in MSG-LIST are added to the tree.

\(fn FOLDER MSG-LIST)" nil nil)

(autoload 'mh-thread-print-scan-lines "mh-thread" "\
Print scan lines in THREAD-TREE in threaded mode.

\(fn THREAD-TREE)" nil nil)

(autoload 'mh-thread-update-scan-line-map "mh-thread" "\
In threaded view update `mh-thread-scan-line-map'.
MSG is the message being notated with NOTATION at OFFSET.

\(fn MSG NOTATION OFFSET)" nil nil)

(autoload 'mh-thread-find-msg-subject "mh-thread" "\
Find canonicalized subject of MSG.
This function can only be used the folder is threaded.

\(fn MSG)" nil nil)

(autoload 'mh-thread-add-spaces "mh-thread" "\
Add COUNT spaces to each scan line in `mh-thread-scan-line-map'.

\(fn COUNT)" nil nil)

(autoload 'mh-thread-forget-message "mh-thread" "\
Forget the message INDEX from the threading tables.

\(fn INDEX)" nil nil)

;;;***

;;;### (autoloads (mh-signature-separator-p mh-letter-skip-leading-whitespace-in-header-field
;;;;;;  mh-letter-toggle-header-field-display mh-letter-skipped-header-field-p
;;;;;;  mh-letter-hide-all-skipped-fields mh-header-field-end mh-header-field-beginning
;;;;;;  mh-mail-header-end mh-goto-header-end mh-goto-header-field
;;;;;;  mh-get-header-field mh-extract-from-header-value mh-in-header-p
;;;;;;  mh-prompt-for-folder mh-folder-completion-function mh-speed-flists-active-p
;;;;;;  mh-expand-file-name mh-folder-name-p mh-remove-from-sub-folders-cache
;;;;;;  mh-sub-folders mh-folder-list mh-clear-sub-folders-cache
;;;;;;  mh-get-msg-num mh-lessp mh-coalesce-msg-list mh-prefix-help
;;;;;;  mh-help mh-set-help mh-ephem-message mh-find-path mh-logo-display
;;;;;;  mh-replace-string mh-quote-pick-expr mh-mapc mh-make-local-vars
;;;;;;  mh-delete-line mh-colors-in-use-p mh-colors-available-p mh-beginning-of-word
;;;;;;  mh-search-from-end) "mh-utils" "mh-utils.el" (20400 62402))
;;; Generated autoloads from mh-utils.el

(autoload 'mh-search-from-end "mh-utils" "\
Return the position of last occurrence of CHAR in STRING.
If CHAR is not present in STRING then return nil. The function is
used in lieu of `search' in the CL package.

\(fn CHAR STRING)" nil nil)

(autoload 'mh-beginning-of-word "mh-utils" "\
Return position of the N th word backwards.

\(fn &optional N)" nil nil)

(autoload 'mh-colors-available-p "mh-utils" "\
Check if colors are available in the Emacs being used.

\(fn)" nil nil)

(autoload 'mh-colors-in-use-p "mh-utils" "\
Check if colors are being used in the folder buffer.

\(fn)" nil nil)

(autoload 'mh-delete-line "mh-utils" "\
Delete the next LINES lines.

\(fn LINES)" nil nil)

(autoload 'mh-make-local-vars "mh-utils" "\
Initialize local variables according to the variable-value PAIRS.

\(fn &rest PAIRS)" nil nil)

(autoload 'mh-mapc "mh-utils" "\
Apply FUNCTION to each element of LIST for side effects only.

\(fn FUNCTION LIST)" nil nil)

(autoload 'mh-quote-pick-expr "mh-utils" "\
Quote `mh-pick-regexp-chars' in PICK-EXPR.
PICK-EXPR is a list of strings. Return nil if PICK-EXPR is nil.

\(fn PICK-EXPR)" nil nil)

(autoload 'mh-replace-string "mh-utils" "\
Replace all occurrences of OLD with NEW in the current buffer.
Ignores case when searching for OLD.

\(fn OLD NEW)" nil nil)

(autoload 'mh-logo-display "mh-utils" "\
Modify mode line to display MH-E logo.

\(fn)" nil nil)

(autoload 'mh-find-path "mh-utils" "\
Set variables from user's MH profile.

This function sets `mh-user-path' from your \"Path:\" MH profile
component (but defaults to \"Mail\" if one isn't present),
`mh-draft-folder' from \"Draft-Folder:\", `mh-unseen-seq' from
\"Unseen-Sequence:\", `mh-previous-seq' from
\"Previous-Sequence:\", and `mh-inbox' from \"Inbox:\" (defaults
to \"+inbox\").

The hook `mh-find-path-hook' is run after these variables have
been set. This hook can be used the change the value of these
variables if you need to run with different values between MH and
MH-E.

\(fn)" nil nil)

(autoload 'mh-ephem-message "mh-utils" "\
Display STRING in the minibuffer momentarily.

\(fn STRING)" nil nil)

(autoload 'mh-set-help "mh-utils" "\
Set help messages.

The MESSAGES are assumed to be an associative array. It is used
to show help for the most common commands in the current mode.
The key is a prefix char. The value is one or more strings which
are concatenated together and displayed in a help buffer if ? is
pressed after the prefix character. The special key nil is used
to display the non-prefixed commands.

The substitutions described in `substitute-command-keys' are performed as
well.

If optional argument DEFAULT is non-nil, then these messages will
be used if help is asked for an unknown mode.

\(fn MESSAGES &optional DEFAULT)" nil nil)

(autoload 'mh-help "mh-utils" "\
Display cheat sheet for the MH-E commands.
See `mh-set-help' for setting the help messages.
HELP-MESSAGES are used instead if given.
This is a list of one or more strings which are concatenated together
and displayed in a help buffer.

\(fn &optional HELP-MESSAGES)" t nil)

(autoload 'mh-prefix-help "mh-utils" "\
Display cheat sheet for the commands of the current prefix in minibuffer.

\(fn)" t nil)

(autoload 'mh-coalesce-msg-list "mh-utils" "\
Given a list of MESSAGES, return a list of message number ranges.
This is the inverse of `mh-read-msg-list', which expands ranges.
Message lists passed to MH programs should be processed by this
function to avoid exceeding system command line argument limits.

\(fn MESSAGES)" nil nil)

(autoload 'mh-lessp "mh-utils" "\
Return the lesser of two message indicators MSG1 and MSG2.
Strings are \"smaller\" than numbers.
Valid values are things like \"cur\", \"last\", 1, and 1820.

\(fn MSG1 MSG2)" nil nil)

(autoload 'mh-get-msg-num "mh-utils" "\
Return the message number of the displayed message.
If the argument ERROR-IF-NO-MESSAGE is non-nil, then complain if
the cursor is not pointing to a message.

\(fn ERROR-IF-NO-MESSAGE)" nil nil)

(autoload 'mh-clear-sub-folders-cache "mh-utils" "\
Clear `mh-sub-folders-cache'.

\(fn)" nil nil)

(autoload 'mh-folder-list "mh-utils" "\
Return FOLDER and its descendants.
FOLDER may have a + prefix. Returns a list of strings without the
+ prefix. If FOLDER is nil, then all folders are considered. For
example, if your Mail directory only contains the folders +inbox,
+outbox, +lists, and +lists/mh-e, then

  (mh-folder-list nil)
       => (\"inbox\" \"lists\" \"lists/mh-e\" \"outbox\")
  (mh-folder-list \"+lists\")
       => (\"lists\" \"lists/mh-e\")

Respects the value of `mh-recursive-folders-flag'. If this flag
is nil, and the sub-folders have not been explicitly viewed, then
they will not be returned.

\(fn FOLDER)" nil nil)

(autoload 'mh-sub-folders "mh-utils" "\
Find the subfolders of FOLDER.
The function avoids running folders unnecessarily by caching the
results of the actual folders call.

If optional argument ADD-TRAILING-SLASH-FLAG is non-nil then a
slash is added to each of the sub-folder names that may have
nested folders within them.

\(fn FOLDER &optional ADD-TRAILING-SLASH-FLAG)" nil nil)

(autoload 'mh-remove-from-sub-folders-cache "mh-utils" "\
Remove FOLDER and its parent from `mh-sub-folders-cache'.
FOLDER should be unconditionally removed from the cache. Also the
last ancestor of FOLDER present in the cache must be removed as
well.

To see why this is needed assume we have a folder +foo which has
a single sub-folder qux. Now we create the folder +foo/bar/baz.
Here we will need to invalidate the cached sub-folders of +foo,
otherwise completion on +foo won't tell us about the option
+foo/bar!

\(fn FOLDER)" nil nil)

(autoload 'mh-folder-name-p "mh-utils" "\
Return non-nil if NAME is the name of a folder.
A name (a string or symbol) can be a folder name if it begins
with \"+\".

\(fn NAME)" nil nil)

(autoload 'mh-expand-file-name "mh-utils" "\
Expand FILENAME like `expand-file-name', but also handle MH folder names.
Any filename that starts with '+' is treated as a folder name.
See `expand-file-name' for description of DEFAULT.

\(fn FILENAME &optional DEFAULT)" nil nil)

(autoload 'mh-speed-flists-active-p "mh-utils" "\
Check if speedbar is running with message counts enabled.

\(fn)" nil nil)

(autoload 'mh-folder-completion-function "mh-utils" "\
Programmable completion for folder names.
NAME is the partial folder name that has been input. PREDICATE if
non-nil is a function that is used to filter the possible
choices. FLAG is nil to indicate `try-completion', t for
`all-completions', or the symbol lambda for `test-completion'.
See Info node `(elisp) Programmed Completion' for details.

\(fn NAME PREDICATE FLAG)" nil nil)

(autoload 'mh-prompt-for-folder "mh-utils" "\
Prompt for a folder name with PROMPT.
Returns the folder's name as a string. DEFAULT is used if the
folder exists and the user types return. If the CAN-CREATE flag
is t, then a folder is created if it doesn't already exist. If
optional argument DEFAULT-STRING is non-nil, use it in the prompt
instead of DEFAULT. If ALLOW-ROOT-FOLDER-FLAG is non-nil then the
function will accept the folder +, which means all folders when
used in searching.

\(fn PROMPT DEFAULT CAN-CREATE &optional DEFAULT-STRING ALLOW-ROOT-FOLDER-FLAG)" nil nil)

(autoload 'mh-in-header-p "mh-utils" "\
Return non-nil if the point is in the header of a draft message.

\(fn)" nil nil)

(autoload 'mh-extract-from-header-value "mh-utils" "\
Extract From: string from header.

\(fn)" nil nil)

(autoload 'mh-get-header-field "mh-utils" "\
Find and return the body of FIELD in the mail header.
Returns the empty string if the field is not in the header of the
current buffer.

\(fn FIELD)" nil nil)

(autoload 'mh-goto-header-field "mh-utils" "\
Move to FIELD in the message header.
Move to the end of the FIELD name, which should end in a colon.
Returns t if found, nil if not.

\(fn FIELD)" nil nil)

(autoload 'mh-goto-header-end "mh-utils" "\
Move the cursor ARG lines after the header.

\(fn ARG)" nil nil)

(autoload 'mh-mail-header-end "mh-utils" "\
Substitute for `mail-header-end' that doesn't widen the buffer.

In MH-E we frequently need to find the end of headers in nested
messages, where the buffer has been narrowed. This function works
in this situation.

\(fn)" nil nil)

(autoload 'mh-header-field-beginning "mh-utils" "\
Move to the beginning of the current header field.
Handles RFC 822 continuation lines.

\(fn)" nil nil)

(autoload 'mh-header-field-end "mh-utils" "\
Move to the end of the current header field.
Handles RFC 822 continuation lines.

\(fn)" nil nil)

(autoload 'mh-letter-hide-all-skipped-fields "mh-utils" "\
Hide all skipped fields.

\(fn)" nil nil)

(autoload 'mh-letter-skipped-header-field-p "mh-utils" "\
Check if FIELD is to be skipped.

\(fn FIELD)" nil nil)

(autoload 'mh-letter-toggle-header-field-display "mh-utils" "\
Toggle display of header field at point.

Use this command to display truncated header fields. This command
is a toggle so entering it again will hide the field. This
command takes a prefix argument ARG: if negative then the field
is hidden, if positive then the field is displayed.

\(fn ARG)" t nil)

(autoload 'mh-letter-skip-leading-whitespace-in-header-field "mh-utils" "\
Skip leading whitespace in a header field.
If the header field doesn't have at least one space after the
colon then a space character is added.

\(fn)" nil nil)

(autoload 'mh-signature-separator-p "mh-utils" "\
Return non-nil if buffer includes \"^-- $\".

\(fn)" nil nil)

;;;***

;;;### (autoloads (mh-set-x-image-cache-directory mh-show-xface)
;;;;;;  "mh-xface" "mh-xface.el" (20352 65510))
;;; Generated autoloads from mh-xface.el

(autoload 'mh-show-xface "mh-xface" "\
Display X-Face.

\(fn)" nil nil)

(autoload 'mh-set-x-image-cache-directory "mh-xface" "\
Set the DIRECTORY where X-Image-URL images are cached.
This is only done if `mh-x-image-cache-directory' is nil.

\(fn DIRECTORY)" nil nil)

;;;***

;;;### (autoloads nil nil ("mh-buffers.el" "mh-compat.el" "mh-e.el"
;;;;;;  "mh-gnus.el" "mh-tool-bar.el") (20436 19804 855318))

;;;***

(provide 'mh-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mh-loaddefs.el ends here
