;;; mh-folder.el --- MH-Folder mode

;; Copyright (C) 2002-2003, 2005-2012  Free Software Foundation, Inc.

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

;; Mode for browsing folders

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'mh-scan)
(mh-require-cl)

;; Dynamically-created functions not found in mh-loaddefs.el.
(autoload 'mh-tool-bar-folder-buttons-init "mh-tool-bar")
(autoload 'mh-tool-bar-init "mh-tool-bar")

(require 'gnus-util)
(autoload 'message-fetch-field "message")



;;; MH-E Entry Points

;;;###autoload
(defun mh-rmail (&optional arg)
  "Incorporate new mail with MH.
Scan an MH folder if ARG is non-nil.

This function is an entry point to MH-E, the Emacs interface to
the MH mail system."
  (interactive "P")
  (mh-find-path)
  (if arg
      (call-interactively 'mh-visit-folder)
    (unless (get-buffer mh-inbox)
      (mh-visit-folder mh-inbox (symbol-name mh-unseen-seq)))
    (mh-inc-folder)))

;;;###autoload
(defun mh-nmail (&optional arg)
  "Check for new mail in inbox folder.
Scan an MH folder if ARG is non-nil.

This function is an entry point to MH-E, the Emacs interface to
the MH mail system."
  (interactive "P")
  (mh-find-path)                        ; init mh-inbox
  (if arg
      (call-interactively 'mh-visit-folder)
    (mh-visit-folder mh-inbox)))


;;; Desktop Integration

;; desktop-buffer-mode-handlers appeared in Emacs 22.
(if (boundp 'desktop-buffer-mode-handlers)
    (add-to-list 'desktop-buffer-mode-handlers
                 '(mh-folder-mode . mh-restore-desktop-buffer)))

(defun mh-restore-desktop-buffer (desktop-buffer-file-name
                                  desktop-buffer-name
                                  desktop-buffer-misc)
  "Restore an MH folder buffer specified in a desktop file.
When desktop creates a buffer, DESKTOP-BUFFER-FILE-NAME holds the
file name to visit, DESKTOP-BUFFER-NAME holds the desired buffer
name, and DESKTOP-BUFFER-MISC holds a list of miscellaneous info
used by the `desktop-buffer-handlers' functions."
  (mh-find-path)
  (mh-visit-folder desktop-buffer-name)
  (current-buffer))



;;; Variables

(defvar mh-folder-filename nil
  "Full path of directory for this folder.")

(defvar mh-partial-folder-mode-line-annotation "select"
  "Annotation when displaying part of a folder.
The string is displayed after the folder's name. nil for no
annotation.")

(defvar mh-last-destination nil
  "Destination of last refile or write command.")

(defvar mh-last-destination-folder nil
  "Destination of last refile command.")

(defvar mh-last-destination-write nil
  "Destination of last write command.")

(defvar mh-first-msg-num nil
  "Number of first message in buffer.")

(defvar mh-last-msg-num nil
  "Number of last msg in buffer.")

(defvar mh-msg-count nil
  "Number of msgs in buffer.")



;;; Sequence Menu

(easy-menu-define
  mh-folder-sequence-menu mh-folder-mode-map "Menu for MH-E folder-sequence."
  '("Sequence"
    ["Add Message to Sequence..."       mh-put-msg-in-seq (mh-get-msg-num nil)]
    ["List Sequences for Message"       mh-msg-is-in-seq (mh-get-msg-num nil)]
    ["Delete Message from Sequence..."  mh-delete-msg-from-seq
     (mh-get-msg-num nil)]
    ["List Sequences in Folder..."      mh-list-sequences t]
    ["Delete Sequence..."               mh-delete-seq t]
    ["Narrow to Sequence..."            mh-narrow-to-seq t]
    ["Widen from Sequence"              mh-widen mh-folder-view-stack]
    "--"
    ["Narrow to Subject Sequence"       mh-narrow-to-subject t]
    ["Narrow to Tick Sequence"          mh-narrow-to-tick
     (and mh-tick-seq (mh-seq-msgs (mh-find-seq mh-tick-seq)))]
    ["Delete Rest of Same Subject"      mh-delete-subject t]
    ["Toggle Tick Mark"                 mh-toggle-tick t]
    "--"
    ["Push State Out to MH"             mh-update-sequences t]))

;;; Message Menu

(easy-menu-define
  mh-folder-message-menu mh-folder-mode-map "Menu for MH-E folder-message."
  '("Message"
    ["Show Message"                     mh-show (mh-get-msg-num nil)]
    ["Show Message with Header"         mh-header-display (mh-get-msg-num nil)]
    ["Show Message with Preferred Alternative"
                                        mh-show-preferred-alternative (mh-get-msg-num nil)]
    ["Next Message"                     mh-next-undeleted-msg t]
    ["Previous Message"                 mh-previous-undeleted-msg t]
    ["Go to First Message"              mh-first-msg t]
    ["Go to Last Message"               mh-last-msg t]
    ["Go to Message by Number..."       mh-goto-msg t]
    ["Modify Message"                   mh-modify t]
    ["Delete Message"                   mh-delete-msg (mh-get-msg-num nil)]
    ["Refile Message"                   mh-refile-msg (mh-get-msg-num nil)]
    ["Undo Delete/Refile"               mh-undo (mh-outstanding-commands-p)]
    ["Execute Delete/Refile"            mh-execute-commands
     (mh-outstanding-commands-p)]
    "--"
    ["Compose a New Message"            mh-send t]
    ["Reply to Message..."              mh-reply (mh-get-msg-num nil)]
    ["Forward Message..."               mh-forward (mh-get-msg-num nil)]
    ["Redistribute Message..."          mh-redistribute (mh-get-msg-num nil)]
    ["Edit Message Again"               mh-edit-again (mh-get-msg-num nil)]
    ["Re-edit a Bounced Message"        mh-extract-rejected-mail t]
    "--"
    ["Copy Message to Folder..."        mh-copy-msg (mh-get-msg-num nil)]
    ["Print Message"                    mh-print-msg (mh-get-msg-num nil)]
    ["Write Message to File..."         mh-write-msg-to-file
     (mh-get-msg-num nil)]
    ["Pipe Message to Command..."       mh-pipe-msg (mh-get-msg-num nil)]
    ["Unpack Uuencoded Message..."      mh-store-msg (mh-get-msg-num nil)]
    ["Burst Digest Message"             mh-burst-digest (mh-get-msg-num nil)]))

;;; Folder Menu

(easy-menu-define
  mh-folder-folder-menu mh-folder-mode-map  "Menu for MH-E folder."
  '("Folder"
    ["Incorporate New Mail"             mh-inc-folder t]
    ["Toggle Show/Folder"               mh-toggle-showing t]
    ["Execute Delete/Refile"            mh-execute-commands
     (mh-outstanding-commands-p)]
    ["Rescan Folder"                    mh-rescan-folder t]
    ["Thread Folder"                    mh-toggle-threads
     (not (memq 'unthread mh-view-ops))]
    ["Pack Folder"                      mh-pack-folder t]
    ["Sort Folder"                      mh-sort-folder t]
    "--"
    ["List Folders"                     mh-list-folders t]
    ["Visit a Folder..."                mh-visit-folder t]
    ["View New Messages"                mh-index-new-messages t]
    ["Search..."                        mh-search t]
    "--"
    ["Quit MH-E"                        mh-quit t]))



;;; MH-Folder Keys

(suppress-keymap mh-folder-mode-map)

;; Use defalias to make sure the documented primary key bindings
;; appear in menu lists.
(defalias 'mh-alt-show 'mh-show)
(defalias 'mh-alt-refile-msg 'mh-refile-msg)
(defalias 'mh-alt-send 'mh-send)
(defalias 'mh-alt-visit-folder 'mh-visit-folder)

;; Save the "b" binding for a future `back'. Maybe?
(gnus-define-keys  mh-folder-mode-map
  " "           mh-page-msg
  "!"           mh-refile-or-write-again
  "'"           mh-toggle-tick
  ","           mh-header-display
  "."           mh-alt-show
  ":"           mh-show-preferred-alternative
  ";"           mh-toggle-mh-decode-mime-flag
  ">"           mh-write-msg-to-file
  "?"           mh-help
  "E"           mh-extract-rejected-mail
  "M"           mh-modify
  "\177"        mh-previous-page
  "\C-d"        mh-delete-msg-no-motion
  "\t"          mh-index-next-folder
  [backtab]     mh-index-previous-folder
  "\M-\t"       mh-index-previous-folder
  "\e<"         mh-first-msg
  "\e>"         mh-last-msg
  "\ed"         mh-redistribute
  "\r"          mh-show
  "^"           mh-alt-refile-msg
  "c"           mh-copy-msg
  "d"           mh-delete-msg
  "e"           mh-edit-again
  "f"           mh-forward
  "g"           mh-goto-msg
  "i"           mh-inc-folder
  "k"           mh-delete-subject-or-thread
  "m"           mh-alt-send
  "n"           mh-next-undeleted-msg
  "\M-n"        mh-next-unread-msg
  "o"           mh-refile-msg
  "p"           mh-previous-undeleted-msg
  "\M-p"        mh-previous-unread-msg
  "q"           mh-quit
  "r"           mh-reply
  "s"           mh-send
  "t"           mh-toggle-showing
  "u"           mh-undo
  "v"           mh-index-visit-folder
  "x"           mh-execute-commands
  "|"           mh-pipe-msg)

(gnus-define-keys (mh-folder-map "F" mh-folder-mode-map)
  "?"           mh-prefix-help
  "'"           mh-index-ticked-messages
  "S"           mh-sort-folder
  "c"           mh-catchup
  "f"           mh-alt-visit-folder
  "k"           mh-kill-folder
  "l"           mh-list-folders
  "n"           mh-index-new-messages
  "o"           mh-alt-visit-folder
  "p"           mh-pack-folder
  "q"           mh-index-sequenced-messages
  "r"           mh-rescan-folder
  "s"           mh-search
  "u"           mh-undo-folder
  "v"           mh-visit-folder)

(define-key mh-folder-mode-map "I" mh-inc-spool-map)

(gnus-define-keys (mh-junk-map "J" mh-folder-mode-map)
  "?"           mh-prefix-help
  "b"           mh-junk-blacklist
  "w"           mh-junk-whitelist)

(gnus-define-keys (mh-ps-print-map "P" mh-folder-mode-map)
  "?"           mh-prefix-help
  "C"           mh-ps-print-toggle-color
  "F"           mh-ps-print-toggle-faces
  "f"           mh-ps-print-msg-file
  "l"           mh-print-msg
  "p"           mh-ps-print-msg)

(gnus-define-keys (mh-sequence-map "S" mh-folder-mode-map)
  "'"           mh-narrow-to-tick
  "?"           mh-prefix-help
  "d"           mh-delete-msg-from-seq
  "k"           mh-delete-seq
  "l"           mh-list-sequences
  "n"           mh-narrow-to-seq
  "p"           mh-put-msg-in-seq
  "s"           mh-msg-is-in-seq
  "w"           mh-widen)

(gnus-define-keys (mh-thread-map "T" mh-folder-mode-map)
  "?"           mh-prefix-help
  "u"           mh-thread-ancestor
  "p"           mh-thread-previous-sibling
  "n"           mh-thread-next-sibling
  "t"           mh-toggle-threads
  "d"           mh-thread-delete
  "o"           mh-thread-refile)

(gnus-define-keys (mh-limit-map "/" mh-folder-mode-map)
  "'"           mh-narrow-to-tick
  "?"           mh-prefix-help
  "c"           mh-narrow-to-cc
  "g"           mh-narrow-to-range
  "m"           mh-narrow-to-from
  "s"           mh-narrow-to-subject
  "t"           mh-narrow-to-to
  "w"           mh-widen)

(gnus-define-keys (mh-extract-map "X" mh-folder-mode-map)
  "?"           mh-prefix-help
  "s"           mh-store-msg            ;shar
  "u"           mh-store-msg)           ;uuencode

(gnus-define-keys (mh-digest-map "D" mh-folder-mode-map)
  " "           mh-page-digest
  "?"           mh-prefix-help
  "\177"        mh-page-digest-backwards
  "b"           mh-burst-digest)

(gnus-define-keys (mh-mime-map "K" mh-folder-mode-map)
  "?"           mh-prefix-help
  "a"           mh-mime-save-parts
  "e"           mh-display-with-external-viewer
  "i"           mh-folder-inline-mime-part
  "o"           mh-folder-save-mime-part
  "t"           mh-toggle-mime-buttons
  "v"           mh-folder-toggle-mime-part
  "\t"          mh-next-button
  [backtab]     mh-prev-button
  "\M-\t"       mh-prev-button)

(cond
 ((featurep 'xemacs)
  (define-key mh-folder-mode-map [button2] 'mh-show-mouse))
 (t
  (define-key mh-folder-mode-map [mouse-2] 'mh-show-mouse)))

;; "C-c /" prefix is used in mh-folder-mode by pgp.el and mailcrypt



;;; MH-Folder Help Messages

;; If you add a new prefix, add appropriate text to the nil key.

;; In general, messages are grouped logically. Taking the main commands for
;; example, the first line is "ways to view messages," the second line is
;; "things you can do with messages", and the third is "composing" messages.

;; When adding a new prefix, ensure that the help message contains "what" the
;; prefix is for. For example, if the word "folder" were not present in the
;; "F" entry, it would not be clear what these commands operated upon.
(defvar mh-folder-mode-help-messages
  '((nil "[i]nc, [.]show, [,]show all, [n]ext, [p]revious,\n"
         "[d]elete, [o]refile, e[x]ecute,\n"
         "[s]end, [r]eply,\n"
         "[;]toggle MIME decoding.\n"
         "Prefix characters:\n [F]older, [S]equence, [J]unk, MIME [K]eys,"
         "\n [T]hread, [/]limit, e[X]tract, [D]igest, [I]nc spools.")

    (?F "[l]ist; [v]isit folder;\n"
        "[n]ew messages; [']ticked messages; [s]earch;\n"
        "[p]ack; [S]ort; [r]escan; [k]ill")
    (?P "[p]rint message to [f]ile; old-style [l]pr printing;\n"
        "Toggle printing of [C]olors, [F]aces")
    (?S "[p]ut message in sequence, [n]arrow, [']narrow to ticked, [w]iden,\n"
        "[s]equences, [l]ist,\n"
        "[d]elete message from sequence, [k]ill sequence")
    (?T "[t]oggle, [d]elete, [o]refile thread")
    (?/ "Limit to [c]c, ran[g]e, fro[m], [s]ubject, [t]o; [w]iden")
    (?X "un[s]har, [u]udecode message")
    (?D "[b]urst digest")
    (?K "[v]iew, [i]nline, with [e]xternal viewer; \n"
        "[o]utput/save MIME part; save [a]ll parts; \n"
        "[t]oggle buttons; [TAB] next; [SHIFT-TAB] previous")
    (?J "[b]lacklist, [w]hitelist message"))
  "Key binding cheat sheet.
See `mh-set-help'.")



;;; MH-Folder Font Lock

(defvar mh-folder-font-lock-keywords
  (list
   ;; Folders when displaying index buffer
   (list "^\\+.*"
         '(0 'mh-search-folder))
   ;; Marked for deletion
   (list (concat mh-scan-deleted-msg-regexp ".*")
         '(0 'mh-folder-deleted))
   ;; Marked for refile
   (list (concat mh-scan-refiled-msg-regexp ".*")
         '(0 'mh-folder-refiled))
   ;; After subject
   (list mh-scan-body-regexp
         '(1 'mh-folder-body nil t))
   ;; Subject
   '(mh-folder-font-lock-subject
     (1 'mh-folder-followup append t)
     (2 'mh-folder-subject append t))
   ;; Current message number
   (list mh-scan-cur-msg-number-regexp
         '(1 'mh-folder-cur-msg-number))
   ;; Message number
   (list mh-scan-good-msg-regexp
         '(1 'mh-folder-msg-number))
   ;; Date
   (list mh-scan-date-regexp
         '(1 'mh-folder-date))
   ;; Messages from me (To:)
   (list mh-scan-rcpt-regexp
         '(1 'mh-folder-to)
         '(2 'mh-folder-address))
   ;; Messages to me
   (list mh-scan-sent-to-me-sender-regexp
         '(1 'mh-folder-sent-to-me-hint)
         '(2 'mh-folder-sent-to-me-sender)))
  "Keywords (regular expressions) used to fontify the MH-Folder buffer.")

(defun mh-folder-font-lock-subject (limit)
  "Return MH-E scan subject strings to font-lock between point and LIMIT."
  (if (not (re-search-forward mh-scan-subject-regexp limit t))
      nil
    (if (match-beginning 1)
        (set-match-data (list (match-beginning 1) (match-end 3)
                              (match-beginning 1) (match-end 3) nil nil))
      (set-match-data (list (match-beginning 3) (match-end 3)
                            nil nil (match-beginning 3) (match-end 3))))
    t))

;; Fontify unseen messages in bold.

(defmacro mh-generate-sequence-font-lock (seq prefix face)
  "Generate the appropriate code to fontify messages in SEQ.
PREFIX is used to generate unique names for the variables and
functions defined by the macro. So a different prefix should be
provided for every invocation.
FACE is the font-lock face used to display the matching scan lines."
  (let ((cache (intern (format "mh-folder-%s-seq-cache" prefix)))
        (func (intern (format "mh-folder-font-lock-%s" prefix))))
    `(progn
       (defvar ,cache nil
         "Internal cache variable used for font-lock in MH-E.
Should only be non-nil through font-lock stepping, and nil once
font-lock is done highlighting.")
       (make-variable-buffer-local ',cache)

       (defun ,func (limit)
         "Return unseen message lines to font-lock between point and LIMIT."
         (if (not ,cache) (setq ,cache (mh-seq-msgs (mh-find-seq ,seq))))
         (let ((cur-msg (mh-get-msg-num nil)))
           (cond ((not ,cache)
                  nil)
                 ((>= (point) limit)              ;Presumably at end of buffer
                  (setq ,cache nil)
                  nil)
                 ((member cur-msg ,cache)
                  (let ((bpoint (progn (beginning-of-line)(point)))
                        (epoint (progn (forward-line 1)(point))))
                    (if (<= limit (point)) (setq  ,cache nil))
                    (set-match-data (list bpoint epoint bpoint epoint))
                    t))
                 (t
                  ;; move forward one line at a time, checking each message
                  (while (and (= 0 (forward-line 1))
                              (> limit (point))
                              (not (member (mh-get-msg-num nil) ,cache))))
                  ;; Examine how we must have exited the loop...
                  (let ((cur-msg (mh-get-msg-num nil)))
                    (cond ((or (<= limit (point))
                               (not (member cur-msg ,cache)))
                           (setq ,cache nil)
                           nil)
                          ((member cur-msg ,cache)
                           (let ((bpoint (progn (beginning-of-line) (point)))
                                 (epoint (progn (forward-line 1) (point))))
                             (if (<= limit (point)) (setq ,cache nil))
                             (set-match-data
                              (list bpoint epoint bpoint epoint))
                             t))))))))

       (setq mh-folder-font-lock-keywords
             (append mh-folder-font-lock-keywords
                     (list (list ',func (list 1 '',face 'prepend t))))))))

(mh-generate-sequence-font-lock mh-unseen-seq unseen bold)
(mh-generate-sequence-font-lock mh-tick-seq tick mh-folder-tick)



;;; MH-Folder Mode

(defmacro mh-remove-xemacs-horizontal-scrollbar ()
  "Get rid of the horizontal scrollbar that XEmacs insists on putting in."
  (when (featurep 'xemacs)
    `(if (and (featurep 'scrollbar)
              (fboundp 'set-specifier))
         (set-specifier horizontal-scrollbar-visible-p nil
                        (cons (current-buffer) nil)))))

;; Register mh-folder-mode as supporting which-function-mode...
(mh-require 'which-func nil t)
(when (boundp 'which-func-modes)
  (add-to-list 'which-func-modes 'mh-folder-mode))

;; Shush compiler.
(defvar desktop-save-buffer)
(defvar font-lock-auto-fontify)
(mh-do-in-xemacs
  (defvar font-lock-defaults))

;; Ensure new buffers won't get this mode if default major-mode is nil.
(put 'mh-folder-mode 'mode-class 'special)

;; Autoload cookie needed by desktop.el
;;;###autoload
(define-derived-mode mh-folder-mode fundamental-mode "MH-Folder"
  "Major MH-E mode for \"editing\" an MH folder scan listing.\\<mh-folder-mode-map>

You can show the message the cursor is pointing to, and step through
the messages. Messages can be marked for deletion or refiling into
another folder; these commands are executed all at once with a
separate command.

Options that control this mode can be changed with
\\[customize-group]; specify the \"mh\" group. In particular, please
see the `mh-scan-format-file' option if you wish to modify scan's
format.

When a folder is visited, the hook `mh-folder-mode-hook' is run.

Ranges
======
Many commands that operate on individual messages, such as
`mh-forward' or `mh-refile-msg' take a RANGE argument. This argument
can be used in several ways.

If you provide the prefix argument (\\[universal-argument]) to
these commands, then you will be prompted for the message range.
This can be any valid MH range which can include messages,
sequences, and the abbreviations (described in the mh(1) man
page):

<num1>-<num2>
    Indicates all messages in the range <num1> to <num2>, inclusive.
    The range must be nonempty.

<num>:N
<num>:+N
<num>:-N
    Up to N messages beginning with (or ending with) message num. Num
    may be any of the predefined symbols: first, prev, cur, next or
    last.

first:N
prev:N
next:N
last:N
    The first, previous, next or last messages, if they exist.

all
    All of the messages.

For example, a range that shows all of these things is `1 2 3
5-10 last:5 unseen'.

If the option `transient-mark-mode' is set to t and you set a
region in the MH-Folder buffer, then the MH-E command will
perform the operation on all messages in that region.

\\{mh-folder-mode-map}"
  (mh-do-in-gnu-emacs
    (unless mh-folder-tool-bar-map
        (mh-tool-bar-folder-buttons-init))
    (if (boundp 'tool-bar-map)
        (set (make-local-variable 'tool-bar-map) mh-folder-tool-bar-map)))
  (mh-do-in-xemacs
    (mh-tool-bar-init :folder))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(mh-folder-font-lock-keywords t))
  (make-local-variable 'desktop-save-buffer)
  (setq desktop-save-buffer t)
  (mh-make-local-vars
   'mh-colors-available-flag (mh-colors-available-p)
                                        ; Do we have colors available
   'mh-current-folder (buffer-name)     ; Name of folder, a string
   'mh-show-buffer (format "show-%s" (buffer-name)) ; Buffer that displays msgs
   'mh-folder-filename                  ; e.g. "/usr/foobar/Mail/inbox/"
   (file-name-as-directory (mh-expand-file-name (buffer-name)))
   'mh-display-buttons-for-inline-parts-flag
   mh-display-buttons-for-inline-parts-flag ; Allow for display of buttons to
                                        ; be  toggled.
   'mh-arrow-marker (make-marker)       ; Marker where arrow is displayed
   'overlay-arrow-position nil          ; Allow for simultaneous display in
   'overlay-arrow-string ">"            ;  different MH-E buffers.
   'mh-showing-mode nil                 ; Show message also?
   'mh-delete-list nil                  ; List of msgs nums to delete
   'mh-refile-list nil                  ; List of folder names in mh-seq-list
   'mh-seq-list nil                     ; Alist of (seq . msgs) nums
   'mh-seen-list nil                    ; List of displayed messages
   'mh-next-direction 'forward          ; Direction to move to next message
   'mh-view-ops ()                      ; Stack that keeps track of the order
                                        ; in which narrowing/threading has been
                                        ; carried out.
   'mh-folder-view-stack ()             ; Stack of previous views of the
                                        ; folder.
   'mh-index-data nil                   ; If the folder was created by a call
                                        ; to mh-search, this contains info
                                        ; about the search results.
   'mh-index-previous-search nil        ; folder, indexer, search-regexp
   'mh-index-msg-checksum-map nil       ; msg -> checksum map
   'mh-index-checksum-origin-map nil    ; checksum -> ( orig-folder, orig-msg )
   'mh-index-sequence-search-flag nil   ; folder resulted from sequence search
   'mh-first-msg-num nil                ; Number of first msg in buffer
   'mh-last-msg-num nil                 ; Number of last msg in buffer
   'mh-msg-count nil                    ; Number of msgs in buffer
   'mh-mode-line-annotation nil         ; Indicates message range
   'mh-sequence-notation-history (make-hash-table)
                                        ; Remember what is overwritten by
                                        ; mh-note-seq.
   'imenu-create-index-function 'mh-index-create-imenu-index
                                        ; Setup imenu support
   'mh-previous-window-config nil)      ; Previous window configuration
  (mh-remove-xemacs-horizontal-scrollbar)
  (setq truncate-lines t)
  (auto-save-mode -1)
  (setq buffer-offer-save t)
  (mh-make-local-hook (mh-write-file-functions))
  (add-hook (mh-write-file-functions) 'mh-execute-commands nil t)
  (make-local-variable 'revert-buffer-function)
  (make-local-variable 'hl-line-mode)   ; avoid pollution
  (mh-funcall-if-exists hl-line-mode 1)
  (setq revert-buffer-function 'mh-undo-folder)
  (add-to-list 'minor-mode-alist '(mh-showing-mode " Show"))
  (easy-menu-add mh-folder-sequence-menu)
  (easy-menu-add mh-folder-message-menu)
  (easy-menu-add mh-folder-folder-menu)
  (mh-inc-spool-make)
  (mh-set-help mh-folder-mode-help-messages)
  (if (and (featurep 'xemacs)
           font-lock-auto-fontify)
      (turn-on-font-lock)))             ; Force font-lock in XEmacs.



;;; MH-Folder Commands

;; Alphabetical.
;; See also mh-comp.el, mh-junk.el, mh-mime.el, mh-print.el,
;; mh-search.el, and mh-seq.el.

;;;###mh-autoload
(defun mh-delete-msg (range)
  "Delete RANGE\\<mh-folder-mode-map>.

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
RANGE is read in interactive use."
  (interactive (list (mh-interactive-range "Delete")))
  (mh-delete-msg-no-motion range)
  (if (looking-at mh-scan-deleted-msg-regexp)
      (mh-next-msg)))

;;;###mh-autoload
(defun mh-delete-msg-no-motion (range)
  "Delete RANGE, don't move to next message.

This command marks the RANGE for deletion but leaves the cursor
at the current message in case you wish to perform other
operations on the message.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use."
  (interactive (list (mh-interactive-range "Delete")))
  (mh-iterate-on-range () range
    (mh-delete-a-msg nil)))

;;;###mh-autoload
(defun mh-execute-commands ()
  "Process outstanding delete and refile requests\\<mh-folder-mode-map>.

If you've marked messages to be deleted or refiled and you want
to go ahead and delete or refile the messages, use this command.
Many MH-E commands that may affect the numbering of the
messages (such as \\[mh-rescan-folder] or \\[mh-pack-folder])
will ask if you want to process refiles or deletes first and then
either run this command for you or undo the pending refiles and
deletes.

This function runs `mh-before-commands-processed-hook' before the
commands are processed and `mh-after-commands-processed-hook'
after the commands are processed."
  (interactive)
  (if mh-folder-view-stack (mh-widen t))
  (mh-process-commands mh-current-folder)
  (mh-set-scan-mode)
  (mh-goto-cur-msg)                    ; after mh-set-scan-mode for efficiency
  (mh-make-folder-mode-line)
  t)                                    ; return t for write-file-functions

;;;###mh-autoload
(defun mh-first-msg ()
  "Display first message."
  (interactive)
  (goto-char (point-min))
  (while (and (not (eobp)) (not (looking-at mh-scan-valid-regexp)))
    (forward-line 1)))

;;;###mh-autoload
(defun mh-goto-msg (number &optional no-error-if-no-message dont-show)
  "Go to a message\\<mh-folder-mode-map>.

You can enter the message NUMBER either before or after typing
\\[mh-goto-msg]. In the latter case, Emacs prompts you.

In a program, optional non-nil second argument NO-ERROR-IF-NO-MESSAGE
means return nil instead of signaling an error if message does not
exist\; in this case, the cursor is positioned near where the message
would have been. Non-nil third argument DONT-SHOW means not to show
the message."
  (interactive "NGo to message: ")
  (setq number (prefix-numeric-value number))
  (let ((point (point))
        (return-value t))
    (goto-char (point-min))
    (unless (re-search-forward (format (mh-scan-msg-search-regexp) number)
                               nil t)
      (goto-char point)
      (unless no-error-if-no-message
        (error "No message %d" number))
      (setq return-value nil))
    (beginning-of-line)
    (or dont-show (not return-value) (mh-maybe-show number))
    return-value))

;;;###mh-autoload
(defun mh-inc-folder (&optional file folder)
  "Incorporate new mail into a folder.

You can incorporate mail from any file into the current folder by
specifying a prefix argument; you'll be prompted for the name of
the FILE to use as well as the destination FOLDER

The hook `mh-inc-folder-hook' is run after incorporating new
mail.

Do not call this function from outside MH-E; use \\[mh-rmail]
instead."
  (interactive (list (if current-prefix-arg
                         (expand-file-name
                          (read-file-name "inc mail from file: "
                                          mh-user-path)))
                     (if current-prefix-arg
                         (mh-prompt-for-folder "inc mail into" mh-inbox t))))
  (if (not folder)
      (setq folder mh-inbox))
  (let ((threading-needed-flag nil))
    (let ((config (current-window-configuration)))
      (when (and mh-show-buffer (get-buffer mh-show-buffer))
        (delete-windows-on mh-show-buffer))
      (cond ((not (get-buffer folder))
             (mh-make-folder folder)
             (setq threading-needed-flag mh-show-threads-flag)
             (setq mh-previous-window-config config))
            ((not (eq (current-buffer) (get-buffer folder)))
             (switch-to-buffer folder)
             (setq mh-previous-window-config config))))
    (mh-get-new-mail file)
    (when (and threading-needed-flag
               (save-excursion
                 (goto-char (point-min))
                 (or (null mh-large-folder)
                     (not (equal (forward-line (1+ mh-large-folder)) 0))
                     (and (message "Not threading since the number of messages exceeds `mh-large-folder'")
                          nil))))
      (mh-toggle-threads))
    (beginning-of-line)
    (if (and mh-showing-mode (looking-at mh-scan-valid-regexp)) (mh-show))
    (run-hooks 'mh-inc-folder-hook)))

;;;###mh-autoload
(defun mh-last-msg ()
  "Display last message."
  (interactive)
  (goto-char (point-max))
  (while (and (not (bobp)) (not (looking-at mh-scan-valid-regexp)))
    (forward-line -1))
  (mh-recenter nil))

;;;###mh-autoload
(defun mh-modify (&optional message)
  "Edit message.

There are times when you need to edit a message. For example, you
may need to fix a broken Content-Type header field. You can do
this with this command. It displays the raw message in an
editable buffer. When you are done editing, save and kill the
buffer as you would any other.

From a program, edit MESSAGE; nil means edit current message."
  (interactive)
  (let* ((message (or message (mh-get-msg-num t)))
         (msg-filename (mh-msg-filename message))
         edit-buffer)
    (when (not (file-exists-p msg-filename))
      (error "Message %d does not exist" message))

    ;; Invalidate the show buffer if it is showing the same message that is
    ;; to be edited.
    (when (and (buffer-live-p (get-buffer mh-show-buffer))
               (equal (with-current-buffer mh-show-buffer
                        buffer-file-name)
                      msg-filename))
      (mh-invalidate-show-buffer))

    ;; Edit message
    (find-file msg-filename)
    (setq edit-buffer (current-buffer))

    ;; Set buffer properties
    (mh-letter-mode)
    (use-local-map text-mode-map)

    ;; Just show the edit buffer...
    (delete-other-windows)
    (switch-to-buffer edit-buffer)))

;;;###mh-autoload
(defun mh-next-button (&optional backward-flag)
  "Go to the next button.

If the end of the buffer is reached then the search wraps over to
the start of the buffer.

If an optional prefix argument BACKWARD-FLAG is given, the cursor
will move to the previous button."
  (interactive (list current-prefix-arg))
  (unless mh-showing-mode
    (mh-show))
  (mh-in-show-buffer (mh-show-buffer)
    (mh-goto-next-button backward-flag)))

;;;###mh-autoload
(defun mh-next-undeleted-msg (&optional count wait-after-complaining-flag)
  "Display next message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip.

In a program, pause for a second after printing message if we are
at the last undeleted message and optional argument
WAIT-AFTER-COMPLAINING-FLAG is non-nil."
  (interactive "p")
  (setq mh-next-direction 'forward)
  (forward-line 1)
  (cond ((re-search-forward mh-scan-good-msg-regexp nil t count)
         (beginning-of-line)
         (mh-maybe-show))
        (t (forward-line -1)
           (message "No more undeleted messages")
           (if wait-after-complaining-flag (sit-for 1)))))

;;;###mh-autoload
(defun mh-next-unread-msg (&optional count)
  "Display next unread message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip."
  (interactive "p")
  (unless (> count 0)
    (error "The function `mh-next-unread-msg' expects positive argument"))
  (setq count (1- count))
  (let ((unread-sequence (reverse (cdr (assoc mh-unseen-seq mh-seq-list))))
        (cur-msg (mh-get-msg-num nil)))
    (cond ((and (not cur-msg) (not (bobp))
                ;; If we are at the end of the buffer back up one line and go
                ;; to unread message after that.
                (progn
                  (forward-line -1)
                  (setq cur-msg (mh-get-msg-num nil)))
                nil))
          ((or (null unread-sequence) (not cur-msg))
           ;; No unread message or there aren't any messages in buffer...
           (message "No more unread messages"))
          ((progn
             ;; Skip messages
             (while (and unread-sequence (>= cur-msg (car unread-sequence)))
               (setq unread-sequence (cdr unread-sequence)))
             (while (> count 0)
               (setq unread-sequence (cdr unread-sequence))
               (setq count (1- count)))
             (not (car unread-sequence)))
           (message "No more unread messages"))
          (t (loop for msg in unread-sequence
                   when (mh-goto-msg msg t) return nil
                   finally (message "No more unread messages"))))))

;;;###mh-autoload
(defun mh-page-msg (&optional lines)
  "Display next page in message.

You can give this command a prefix argument that specifies the
number of LINES to scroll. This command will also show the next
undeleted message if it is used at the bottom of a message."
  (interactive "P")
  (if mh-showing-mode
      (if mh-page-to-next-msg-flag
          (if (equal mh-next-direction 'backward)
              (mh-previous-undeleted-msg)
            (mh-next-undeleted-msg))
        (if (mh-in-show-buffer (mh-show-buffer)
              (pos-visible-in-window-p (point-max)))
            (progn
              (message
               "End of message (Type %s to read %s undeleted message)"
               (single-key-description last-input-event)
               (if (equal mh-next-direction 'backward)
                   "previous"
                 "next"))
              (setq mh-page-to-next-msg-flag t))
          (scroll-other-window lines)))
    (mh-show)))

;;;###mh-autoload
(defun mh-prev-button ()
  "Go to the previous button.

If the beginning of the buffer is reached then the search wraps
over to the end of the buffer."
  (interactive)
  (mh-next-button t))

;;;###mh-autoload
(defun mh-previous-page (&optional lines)
  "Display next page in message.

You can give this command a prefix argument that specifies the
number of LINES to scroll."
  (interactive "P")
  (mh-in-show-buffer (mh-show-buffer)
    (scroll-down lines)))

;;;###mh-autoload
(defun mh-previous-undeleted-msg (&optional count wait-after-complaining-flag)
  "Display previous message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip.

In a program, pause for a second after printing message if we are
at the last undeleted message and optional argument
WAIT-AFTER-COMPLAINING-FLAG is non-nil."
  (interactive "p")
  (setq mh-next-direction 'backward)
  (beginning-of-line)
  (cond ((re-search-backward mh-scan-good-msg-regexp nil t count)
         (mh-maybe-show))
        (t (message "No previous undeleted message")
           (if wait-after-complaining-flag (sit-for 1)))))

;;;###mh-autoload
(defun mh-previous-unread-msg (&optional count)
  "Display previous unread message.

This command can be given a prefix argument COUNT to specify how
many unread messages to skip."
  (interactive "p")
  (unless (> count 0)
    (error "The function `mh-previous-unread-msg' expects positive argument"))
  (setq count (1- count))
  (let ((unread-sequence (cdr (assoc mh-unseen-seq mh-seq-list)))
        (cur-msg (mh-get-msg-num nil)))
    (cond ((and (not cur-msg) (not (bobp))
                ;; If we are at the end of the buffer back up one line and go
                ;; to unread message after that.
                (progn
                  (forward-line -1)
                  (setq cur-msg (mh-get-msg-num nil)))
                nil))
          ((or (null unread-sequence) (not cur-msg))
           ;; No unread message or there aren't any messages in buffer...
           (message "No more unread messages"))
          ((progn
             ;; Skip count messages...
             (while (and unread-sequence (>= (car unread-sequence) cur-msg))
               (setq unread-sequence (cdr unread-sequence)))
             (while (> count 0)
               (setq unread-sequence (cdr unread-sequence))
               (setq count (1- count)))
             (not (car unread-sequence)))
           (message "No more unread messages"))
          (t (loop for msg in unread-sequence
                   when (mh-goto-msg msg t) return nil
                   finally (message "No more unread messages"))))))

;;;###mh-autoload
(defun mh-quit ()
  "Quit the current MH-E folder.

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
modify the window setup."
  (interactive)
  (run-hooks 'mh-before-quit-hook)
  (let ((show-buffer (get-buffer mh-show-buffer)))
    (when show-buffer
      (kill-buffer show-buffer)))
  (mh-update-sequences)
  (mh-destroy-postponed-handles)
  (bury-buffer (current-buffer))

  ;; Delete all MH-E temporary and working buffers.
  (dolist (buffer (buffer-list))
    (when (or (string-match "^ \\*mh-" (buffer-name buffer))
              (string-match "^\\*MH-E " (buffer-name buffer)))
      (kill-buffer buffer)))

  (if mh-previous-window-config
      (set-window-configuration mh-previous-window-config))
  (run-hooks 'mh-quit-hook))

;;;###mh-autoload
(defun mh-refile-msg (range folder &optional dont-update-last-destination-flag)
  "Refile (output) RANGE into FOLDER.

You are prompted for the folder name. Note that this command can also
be used to create folders. If you specify a folder that does not
exist, you will be prompted to create it.

The hook `mh-refile-msg-hook' is called after a message is marked to
be refiled.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

In a program, the variables `mh-last-destination' and
`mh-last-destination-folder' are not updated if
DONT-UPDATE-LAST-DESTINATION-FLAG is non-nil."
  (interactive (list (mh-interactive-range "Refile")
                     (intern (mh-prompt-for-refile-folder))))
  (unless dont-update-last-destination-flag
    (setq mh-last-destination (cons 'refile folder)
          mh-last-destination-folder mh-last-destination))
  (mh-iterate-on-range () range
    (mh-refile-a-msg nil folder))
  (when (looking-at mh-scan-refiled-msg-regexp) (mh-next-msg)))

;;;###mh-autoload
(defun mh-refile-or-write-again (range &optional interactive-flag)
  "Repeat last output command.

If you are refiling several messages into the same folder, you
can use this command to repeat the last
refile (\\[mh-refile-msg]) or write (\\[mh-write-msg-to-file]).
You can use a range.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

In a program, a non-nil INTERACTIVE-FLAG means that the function was
called interactively."
  (interactive (list (mh-interactive-range "Redo") t))
  (if (null mh-last-destination)
      (error "No previous refile or write"))
  (cond ((eq (car mh-last-destination) 'refile)
         (mh-refile-msg range (cdr mh-last-destination))
         (message "Destination folder: %s" (cdr mh-last-destination)))
        (t
         (mh-iterate-on-range msg range
           (apply 'mh-write-msg-to-file msg (cdr mh-last-destination)))
         (mh-next-msg interactive-flag))))

;;;###mh-autoload
(defun mh-rescan-folder (&optional range dont-exec-pending)
  "Rescan folder\\<mh-folder-mode-map>.

This command is useful to grab all messages in your \"+inbox\" after
processing your new mail for the first time. If you don't want to
rescan the entire folder, this command will accept a RANGE. Check the
documentation of `mh-interactive-range' to see how RANGE is read in
interactive use.

This command will ask if you want to process refiles or deletes first
and then either run \\[mh-execute-commands] for you or undo the
pending refiles and deletes.

In a program, the processing of outstanding commands is not performed
if DONT-EXEC-PENDING is non-nil."
  (interactive (list (if current-prefix-arg
                         (mh-read-range "Rescan" mh-current-folder t nil t
                                        mh-interpret-number-as-range-flag)
                       nil)))
  (setq mh-next-direction 'forward)
  (let ((threaded-flag (memq 'unthread mh-view-ops))
        (msg-num (mh-get-msg-num nil)))
    (mh-scan-folder mh-current-folder (or range "all") dont-exec-pending)
    ;; If there isn't a cur sequence, mh-scan-folder goes to the first message.
    ;; Try to stay where we were.
    (if (null (car (mh-seq-to-msgs 'cur)))
        (mh-goto-msg msg-num t t))
    (cond (threaded-flag (mh-toggle-threads))
          (mh-index-data (mh-index-insert-folder-headers)))))

(defun mh-show-mouse (event)
  "Move point to mouse EVENT and show message."
  (interactive "e")
  (mouse-set-point event)
  (mh-show))

;;;###mh-autoload
(defun mh-toggle-showing ()
  "Toggle between MH-Folder and MH-Folder Show modes.

This command switches between MH-Folder mode and MH-Folder Show
mode. MH-Folder mode turns off the associated show buffer so that
you can perform operations on the messages quickly without
reading them. This is an excellent way to prune out your junk
mail or to refile a group of messages to another folder for later
examination."
  (interactive)
  (if mh-showing-mode
      (mh-set-scan-mode)
    (mh-show)))

;;;###mh-autoload
(defun mh-undo (range)
  "Undo pending deletes or refiles in RANGE.

If you've deleted a message or refiled it, but changed your mind,
you can cancel the action before you've executed it. Use this
command to undo a refile on or deletion of a single message. You
can also undo refiles and deletes for messages that are found in
a given RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use."
  (interactive (list (mh-interactive-range "Undo")))
  (cond ((numberp range)
         (let ((original-position (point)))
           (beginning-of-line)
           (while (not (or (looking-at mh-scan-deleted-msg-regexp)
                           (looking-at mh-scan-refiled-msg-regexp)
                           (and (eq mh-next-direction 'forward) (bobp))
                           (and (eq mh-next-direction 'backward)
                                (save-excursion (forward-line) (eobp)))))
             (forward-line (if (eq mh-next-direction 'forward) -1 1)))
           (if (or (looking-at mh-scan-deleted-msg-regexp)
                   (looking-at mh-scan-refiled-msg-regexp))
               (progn
                 (mh-undo-msg (mh-get-msg-num t))
                 (mh-maybe-show))
             (goto-char original-position)
             (error "Nothing to undo"))))
        (t (mh-iterate-on-range () range
             (mh-undo-msg nil))))
  (if (not (mh-outstanding-commands-p))
      (mh-set-folder-modified-p nil)))

;;;###mh-autoload
(defun mh-visit-folder (folder &optional range index-data)
  "Visit FOLDER.

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
structures."
  (interactive (let ((folder-name (mh-prompt-for-folder "Visit" mh-inbox t)))
                 (list folder-name
                       (mh-read-range "Scan" folder-name t nil
                                      current-prefix-arg
                                      mh-interpret-number-as-range-flag))))
  (let ((config (current-window-configuration))
        (current-buffer (current-buffer))
        (threaded-view-flag mh-show-threads-flag))
    (delete-other-windows)
    (when (get-buffer folder)
      (with-current-buffer folder
        (setq threaded-view-flag (memq 'unthread mh-view-ops))))
    (when index-data
      (mh-make-folder folder)
      (setq mh-index-data (car index-data)
            mh-index-msg-checksum-map (make-hash-table :test #'equal)
            mh-index-checksum-origin-map (make-hash-table :test #'equal))
      (mh-index-update-maps folder (cadr index-data))
      (mh-index-create-sequences))
    (mh-scan-folder folder (or range "all"))
    (cond ((and threaded-view-flag
                (save-excursion
                  (goto-char (point-min))
                  (or (null mh-large-folder)
                      (not (equal (forward-line (1+ mh-large-folder)) 0))
                      (and (message "Not threading since the number of messages exceeds `mh-large-folder'")
                           nil))))
           (mh-toggle-threads))
          (mh-index-data
           (mh-index-insert-folder-headers)))
    (unless (eq current-buffer (current-buffer))
      (setq mh-previous-window-config config)))
  nil)

;;;###mh-autoload
(defun mh-write-msg-to-file (message file no-header)
  "Append MESSAGE to end of FILE\\<mh-folder-mode-map>.

You are prompted for the filename. If the file already exists,
the message is appended to it. You can also write the message to
the file without the header by specifying a prefix argument
NO-HEADER. Subsequent writes to the same file can be made with
the command \\[mh-refile-or-write-again]."
  (interactive
   (list (mh-get-msg-num t)
         (let ((default-dir (if (eq 'write (car mh-last-destination-write))
                                (file-name-directory
                                 (car (cdr mh-last-destination-write)))
                              default-directory)))
           (read-file-name (format "Save message%s in file: "
                                   (if current-prefix-arg " body" ""))
                           default-dir
                           (if (eq 'write (car mh-last-destination-write))
                               (car (cdr mh-last-destination-write))
                             (expand-file-name "mail.out" default-dir))))
         current-prefix-arg))
  (let ((msg-file-to-output (mh-msg-filename message))
        (output-file (mh-expand-file-name file)))
    (setq mh-last-destination (list 'write file (if no-header 'no-header))
          mh-last-destination-write mh-last-destination)
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (insert-file-contents msg-file-to-output)
      (goto-char (point-min))
      (if no-header (search-forward "\n\n"))
      (append-to-file (point) (point-max) output-file))))

;;;###mh-autoload
(defun mh-update-sequences ()
  "Flush MH-E's state out to MH.

This function updates the sequence specified by your
\"Unseen-Sequence:\" profile component, \"cur\", and the sequence
listed by the `mh-tick-seq' option which is \"tick\" by default.
The message at the cursor is used for \"cur\"."
  (interactive)
  ;; mh-update-sequences is the opposite of mh-read-folder-sequences,
  ;; which updates MH-E's state from MH.
  (let ((folder-set (mh-update-unseen))
        (new-cur (mh-get-msg-num nil)))
    (if new-cur
        (let ((seq-entry (mh-find-seq 'cur)))
          (mh-remove-cur-notation)
          (setcdr seq-entry
                  (list new-cur))       ;delete-seq-locally, add-msgs-to-seq
          (mh-define-sequence 'cur (list new-cur))
          (beginning-of-line)
          (if (looking-at mh-scan-good-msg-regexp)
              (mh-notate-cur)))
      (or folder-set
          (save-excursion
            ;; psg - mh-current-folder is nil if mh-summary-height < 4 !
            ;;       So I added this sanity check.
            (if (stringp mh-current-folder)
                (mh-exec-cmd-quiet t "folder" mh-current-folder "-fast")
              (mh-exec-cmd-quiet t "folder" "-fast")))))))



;;; Support Routines

(defun mh-get-new-mail (maildrop-name)
  "Read new mail from MAILDROP-NAME into the current buffer.
Return in the current buffer."
  (let ((point-before-inc (point))
        (folder mh-current-folder)
        (new-mail-flag nil))
    (with-mh-folder-updating (t)
      (if maildrop-name
          (message "inc %s -file %s..." folder maildrop-name)
        (message "inc %s..." folder))
      (setq mh-next-direction 'forward)
      (goto-char (point-max))
      (mh-remove-cur-notation)
      (let ((start-of-inc (point)))
        (if maildrop-name
            ;; I think MH 5 used "-ms-file" instead of "-file",
            ;; which would make inc'ing from maildrops fail.
            (mh-exec-cmd-output mh-inc-prog nil folder
                                (mh-scan-format)
                                "-file" (expand-file-name maildrop-name)
                                "-width" (window-width)
                                "-truncate")
          (mh-exec-cmd-output mh-inc-prog nil
                              (mh-scan-format)
                              "-width" (window-width)))
        (if maildrop-name
            (message "inc %s -file %s...done" folder maildrop-name)
          (message "inc %s...done" folder))
        (goto-char start-of-inc)
        (cond ((save-excursion
                 (re-search-forward "^inc: no mail" nil t))
               (message "No new mail%s%s" (if maildrop-name " in " "")
                        (if maildrop-name maildrop-name "")))
              ((and (when mh-folder-view-stack
                      (let ((saved-text (buffer-substring-no-properties
                                         start-of-inc (point-max))))
                        (delete-region start-of-inc (point-max))
                        (unwind-protect (mh-widen t)
                          (mh-remove-cur-notation)
                          (goto-char (point-max))
                          (setq start-of-inc (point))
                          (insert saved-text)
                          (goto-char start-of-inc))))
                    nil))
              ((re-search-forward "^inc:" nil t) ; Error messages
               (error "Error incorporating mail"))
              ((and
                (equal mh-scan-format-file t)
                mh-adaptive-cmd-note-flag
                ;; Have we reached an edge condition?
                (save-excursion
                  (re-search-forward mh-scan-msg-overflow-regexp nil 0 1))
                (setq start-of-inc (mh-generate-new-cmd-note folder))
                nil))
              (t
               (setq new-mail-flag t)))
        (keep-lines mh-scan-valid-regexp) ; Flush random scan lines
        (let* ((sequences (mh-read-folder-sequences folder t))
               (new-cur (assoc 'cur sequences))
               (new-unseen (assoc mh-unseen-seq sequences)))
          (unless (assoc 'cur mh-seq-list)
            (push (list 'cur) mh-seq-list))
          (unless (assoc mh-unseen-seq mh-seq-list)
            (push (list mh-unseen-seq) mh-seq-list))
          (setcdr (assoc 'cur mh-seq-list) (cdr new-cur))
          (setcdr (assoc mh-unseen-seq mh-seq-list) (cdr new-unseen)))
        (when (equal (point-max) start-of-inc)
          (mh-notate-cur))
        (if new-mail-flag
            (progn
              (mh-make-folder-mode-line)
              (when (mh-speed-flists-active-p)
                (mh-speed-flists t mh-current-folder))
              (when (memq 'unthread mh-view-ops)
                (mh-thread-inc folder start-of-inc))
              (mh-goto-cur-msg))
          (goto-char point-before-inc))
        (mh-notate-user-sequences (cons start-of-inc (point-max)))))))

(defun mh-generate-new-cmd-note (folder)
  "Fix the `mh-cmd-note' value for this FOLDER.

After doing an `mh-get-new-mail' operation in this FOLDER, at least
one line that looks like a truncated message number was found.

Remove the text added by the last `mh-inc' command. It should be the
messages cur-last. Call `mh-set-cmd-note', adjusting the notation
column with the width of the largest message number in FOLDER.

Reformat the message number width on each line in the buffer and trim
the line length to fit in the window.

Rescan the FOLDER in the range cur-last in order to display the
messages that were removed earlier. They should all fit in the scan
line now with no message truncation."
  (save-excursion
    (let ((maxcol (1- (window-width)))
          (old-cmd-note mh-cmd-note)
          mh-cmd-note-fmt
          msgnum)
      ;; Nuke all of the lines just added by the last inc
      (delete-char (- (point-max) (point)))
      ;; Update the current buffer to reflect the new mh-cmd-note
      ;; value needed to display messages.
      (mh-set-cmd-note (mh-msg-num-width-to-column (mh-msg-num-width folder)))
      (setq mh-cmd-note-fmt (concat "%" (format "%d" mh-cmd-note) "d"))
      ;; Cleanup the messages that are in the buffer right now
      (goto-char (point-min))
      (cond ((memq 'unthread mh-view-ops)
             (mh-thread-add-spaces (- mh-cmd-note old-cmd-note)))
            (t (while (re-search-forward (mh-scan-msg-number-regexp) nil 0 1)
                 ;; reformat the number to fix in mh-cmd-note columns
                 (setq msgnum (string-to-number
                               (buffer-substring
                                (match-beginning 1) (match-end 1))))
                 (replace-match (format mh-cmd-note-fmt msgnum))
                 ;; trim the line to fix in the window
                 (end-of-line)
                 (let ((eol (point)))
                   (move-to-column maxcol)
                   (if (<= (point) eol)
                       (delete-char (- eol (point))))))))
      ;; now re-read the lost messages
      (goto-char (point-max))
      (prog1 (point)
        (mh-regenerate-headers "cur-last" t)))))

;;;###mh-autoload
(defun mh-goto-cur-msg (&optional minimal-changes-flag)
  "Position the cursor at the current message.
When optional argument MINIMAL-CHANGES-FLAG is non-nil, the
function doesn't recenter the folder buffer."
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (cond ((and cur-msg
                (mh-goto-msg cur-msg t t))
           (unless minimal-changes-flag
             (mh-notate-cur)
             (mh-recenter 0)
             (mh-maybe-show cur-msg)))
          (t
           (setq overlay-arrow-position nil)
           (message "No current message")))))

;;;###mh-autoload
(defun mh-recenter (arg)
  "Like recenter but with three improvements:

- At the end of the buffer it tries to show fewer empty lines.

- operates only if the current buffer is in the selected window.
  (Commands like `save-some-buffers' can make this false.)

- nil ARG means recenter as if prefix argument had been given."
  (cond ((not (eq (get-buffer-window (current-buffer)) (selected-window)))
         nil)
        ((= (point-max) (save-excursion
                          (forward-line (- (/ (window-height) 2) 2))
                          (point)))
         (let ((lines-from-end 2))
           (save-excursion
             (while (> (point-max) (progn (forward-line) (point)))
               (incf lines-from-end)))
           (recenter (- lines-from-end))))
        ;; '(4) is the same as C-u prefix argument.
        (t (recenter (or arg '(4))))))

(defun mh-update-unseen ()
  "Synchronize the unseen sequence with MH.
Return non-nil if the MH folder was set.
The hook `mh-unseen-updated-hook' is called after the unseen sequence
is updated."
  (if mh-seen-list
      (let* ((unseen-seq (mh-find-seq mh-unseen-seq))
             (unseen-msgs (mh-seq-msgs unseen-seq)))
        (if unseen-msgs
            (progn
              (mh-undefine-sequence mh-unseen-seq mh-seen-list)
              (run-hooks 'mh-unseen-updated-hook)
              (while mh-seen-list
                (setq unseen-msgs (delq (car mh-seen-list) unseen-msgs))
                (setq mh-seen-list (cdr mh-seen-list)))
              (setcdr unseen-seq unseen-msgs)
              t)                        ;since we set the folder
          (setq mh-seen-list nil)))))

;;;###mh-autoload
(defun mh-outstanding-commands-p ()
  "Return non-nil if there are outstanding deletes or refiles."
  (save-excursion
    (when (eq major-mode 'mh-show-mode)
      (set-buffer mh-show-folder-buffer))
    (or mh-delete-list mh-refile-list)))

;;;###mh-autoload
(defun mh-set-folder-modified-p (flag)
  "Mark current folder as modified or unmodified according to FLAG."
  (set-buffer-modified-p flag))

(defun mh-process-commands (folder)
  "Process outstanding commands for FOLDER.

This function runs `mh-before-commands-processed-hook' before the
commands are processed and `mh-after-commands-processed-hook'
after the commands are processed."
  (message "Processing deletes and refiles for %s..." folder)
  (set-buffer folder)
  (with-mh-folder-updating (nil)
    ;; Run the before hook -- the refile and delete lists are still valid
    (run-hooks 'mh-before-commands-processed-hook)

    ;; Update the unseen sequence if it exists
    (mh-update-unseen)

    (let ((redraw-needed-flag mh-index-data)
          (folders-changed (list mh-current-folder))
          (seq-map (and mh-refile-list mh-refile-preserves-sequences-flag
                        (mh-create-sequence-map mh-seq-list)))
          (dest-map (and mh-refile-list mh-refile-preserves-sequences-flag
                         (make-hash-table))))
      ;; Remove invalid scan lines if we are in an index folder and then remove
      ;; the real messages
      (when mh-index-data
        (mh-index-delete-folder-headers)
        (setq folders-changed
              (append folders-changed (mh-index-execute-commands))))

      ;; Then refile messages
      (mh-mapc #'(lambda (folder-msg-list)
                   (let* ((dest-folder (symbol-name (car folder-msg-list)))
                          (last (car (mh-translate-range dest-folder "last")))
                          (msgs (cdr folder-msg-list)))
                     (push dest-folder folders-changed)
                     (setq redraw-needed-flag t)
                     (apply #'mh-exec-cmd
                            "refile" "-src" folder dest-folder
                            (mh-coalesce-msg-list msgs))
                     (mh-delete-scan-msgs msgs)
                     ;; Preserve sequences in destination folder...
                     (when mh-refile-preserves-sequences-flag
                       (clrhash dest-map)
                       (loop for i from (1+ (or last 0))
                             for msg in (sort (copy-sequence msgs) #'<)
                             do (loop for seq-name in (gethash msg seq-map)
                                      do (push i (gethash seq-name dest-map))))
                       (maphash
                        #'(lambda (seq msgs)
                            ;; Can't be run in the background, since the
                            ;; current folder is changed by mark this could
                            ;; lead to a race condition with the next refile.
                            (apply #'mh-exec-cmd "mark"
                                   "-sequence" (symbol-name seq) dest-folder
                                   "-add" (mapcar #'(lambda (x) (format "%s" x))
                                                  (mh-coalesce-msg-list msgs))))
                        dest-map))))
               mh-refile-list)
      (setq mh-refile-list ())

      ;; Now delete messages
      (cond (mh-delete-list
             (setq redraw-needed-flag t)
             (apply 'mh-exec-cmd "rmm" folder
                    (mh-coalesce-msg-list mh-delete-list))
             (mh-delete-scan-msgs mh-delete-list)
             (setq mh-delete-list nil)))

      ;; Don't need to remove sequences since delete and refile do so.
      ;; Mark cur message
      (if (> (buffer-size) 0)
          (mh-define-sequence 'cur (list (or (mh-get-msg-num nil) "last"))))

      ;; Redraw folder buffer if needed
      (when (and redraw-needed-flag)
        (when (mh-speed-flists-active-p)
          (apply #'mh-speed-flists t folders-changed))
        (cond ((memq 'unthread mh-view-ops) (mh-thread-inc folder (point-max)))
              (mh-index-data (mh-index-insert-folder-headers))))

      (and (buffer-file-name (get-buffer mh-show-buffer))
           (not (file-exists-p (buffer-file-name (get-buffer mh-show-buffer))))
           ;; If "inc" were to put a new msg in this file,
           ;; we would not notice, so mark it invalid now.
           (mh-invalidate-show-buffer))

      (setq mh-seq-list (mh-read-folder-sequences mh-current-folder nil))
      (mh-remove-all-notation)
      (mh-notate-user-sequences)

      ;; Run the after hook -- now folders-changed is valid,
      ;; but not the lists of specific messages.
      (let ((mh-folders-changed folders-changed))
        (run-hooks 'mh-after-commands-processed-hook)))

    (message "Processing deletes and refiles for %s...done" folder)))

(defun mh-delete-scan-msgs (msgs)
  "Delete the scan listing lines for MSGS."
  (save-excursion
    (while msgs
      (when (mh-goto-msg (car msgs) t t)
        (when (memq 'unthread mh-view-ops)
          (mh-thread-forget-message (car msgs)))
        (mh-delete-line 1))
      (setq msgs (cdr msgs)))))

(defun mh-set-scan-mode ()
  "Display the scan listing buffer, but do not show a message."
  (if (get-buffer mh-show-buffer)
      (delete-windows-on mh-show-buffer))
  (mh-showing-mode 0)
  (force-mode-line-update)
  (if mh-recenter-summary-flag
      (mh-recenter nil)))

;;;###mh-autoload
(defun mh-make-folder-mode-line (&optional ignored)
  "Set the fields of the mode line for a folder buffer.
The optional argument is now obsolete and IGNORED. It used to be
used to pass in what is now stored in the buffer-local variable
`mh-mode-line-annotation'."
  (save-excursion
    (save-window-excursion
      (mh-first-msg)
      (let ((new-first-msg-num (mh-get-msg-num nil)))
        (when (or (not (memq 'unthread mh-view-ops))
                  (null mh-first-msg-num)
                  (null new-first-msg-num)
                  (< new-first-msg-num mh-first-msg-num))
          (setq mh-first-msg-num new-first-msg-num)))
      (mh-last-msg)
      (let ((new-last-msg-num (mh-get-msg-num nil)))
        (when (or (not (memq 'unthread mh-view-ops))
                  (null mh-last-msg-num)
                  (null new-last-msg-num)
                  (> new-last-msg-num mh-last-msg-num))
          (setq mh-last-msg-num new-last-msg-num)))
      (setq mh-msg-count (if mh-first-msg-num
                             (count-lines (point-min) (point-max))
                           0))
      (setq mode-line-buffer-identification
            (list (format "    {%%b%s} %s msg%s"
                          (if mh-mode-line-annotation
                              (format "/%s" mh-mode-line-annotation)
                            "")
                          (if (zerop mh-msg-count)
                              "no"
                            (format "%d" mh-msg-count))
                          (if (zerop mh-msg-count)
                              "s"
                            (cond ((> mh-msg-count 1)
                                   (format "s (%d-%d)" mh-first-msg-num
                                           mh-last-msg-num))
                                  (mh-first-msg-num
                                   (format " (%d)" mh-first-msg-num))
                                  (""))))))
      (mh-logo-display))))

;;;###mh-autoload
(defun mh-scan-folder (folder range &optional dont-exec-pending)
  "Scan FOLDER over RANGE.

After the scan is performed, switch to the buffer associated with
FOLDER.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

The processing of outstanding commands is not performed if
DONT-EXEC-PENDING is non-nil."
  (when (stringp range)
    (setq range (delete "" (split-string range "[ \t\n]"))))
  (cond ((null (get-buffer folder))
         (mh-make-folder folder))
        (t
         (unless dont-exec-pending
           (mh-process-or-undo-commands folder)
           (mh-reset-threads-and-narrowing))
         (switch-to-buffer folder)))
  (mh-regenerate-headers range)
  (if (zerop (buffer-size))
      (if (equal range "all")
          (message "Folder %s is empty" folder)
        (message "No messages in %s, range %s" folder range))
    (mh-goto-cur-msg))
  (when (mh-outstanding-commands-p)
    (mh-notate-deleted-and-refiled)))

;;;###mh-autoload
(defun mh-process-or-undo-commands (folder)
  "If FOLDER has outstanding commands, then either process or discard them.
Called by functions like `mh-sort-folder', so also invalidate
show buffer."
  (set-buffer folder)
  (if (mh-outstanding-commands-p)
      (if (or mh-do-not-confirm-flag
              (y-or-n-p
               "Process outstanding deletes and refiles? "))
          (mh-process-commands folder)
        (set-buffer folder)
        (mh-undo-folder)))
  (mh-update-unseen)
  (mh-invalidate-show-buffer))

;;;###mh-autoload
(defun mh-regenerate-headers (range &optional update)
  "Scan folder over RANGE.
If UPDATE, append the scan lines, otherwise replace."
  (let ((folder mh-current-folder)
        (range (if (and range (atom range)) (list range) range))
        scan-start)
    (message "Scanning %s..." folder)
    (mh-remove-all-notation)
    (with-mh-folder-updating (nil)
      (if update
          (goto-char (point-max))
        (delete-region (point-min) (point-max))
        (if mh-adaptive-cmd-note-flag
            (mh-set-cmd-note (mh-msg-num-width-to-column (mh-msg-num-width
                                                          folder)))))
      (setq scan-start (point))
      (apply #'mh-exec-cmd-output
             mh-scan-prog nil
             (mh-scan-format)
             "-noclear" "-noheader"
             "-width" (window-width)
             folder range)
      (goto-char scan-start)
      (cond ((looking-at "scan: no messages in")
             (keep-lines mh-scan-valid-regexp)) ; Flush random scan lines
            ((looking-at (if (mh-variant-p 'gnu-mh)
                             "scan: message set .* does not exist"
                           "scan: bad message list "))
             (keep-lines mh-scan-valid-regexp))
            ((looking-at "scan: "))     ; Keep error messages
            (t
             (keep-lines mh-scan-valid-regexp))) ; Flush random scan lines
      (setq mh-seq-list (mh-read-folder-sequences folder nil))
      (mh-notate-user-sequences)
      (or update
          (setq mh-mode-line-annotation
                (if (equal range '("all"))
                    nil
                  mh-partial-folder-mode-line-annotation)))
      (mh-make-folder-mode-line))
    (message "Scanning %s...done" folder)))

;;;###mh-autoload
(defun mh-reset-threads-and-narrowing ()
  "Reset all variables pertaining to threads and narrowing.
Also removes all content from the folder buffer."
  (setq mh-view-ops ())
  (setq mh-folder-view-stack ())
  (setq mh-thread-scan-line-map-stack ())
  (let ((buffer-read-only nil)) (erase-buffer)))

(defun mh-make-folder (name)
  "Create a new mail folder called NAME.
Make it the current folder."
  (switch-to-buffer name)
  (setq buffer-read-only nil)
  (erase-buffer)
  (if mh-adaptive-cmd-note-flag
      (mh-set-cmd-note (mh-msg-num-width-to-column (mh-msg-num-width name))))
  (setq buffer-read-only t)
  (mh-folder-mode)
  (mh-set-folder-modified-p nil)
  (setq buffer-file-name mh-folder-filename)
  (when (and (not mh-index-data)
             (file-exists-p (concat buffer-file-name mh-index-data-file)))
    (mh-index-read-data))
  (mh-make-folder-mode-line))

;;;###mh-autoload
(defun mh-next-msg (&optional wait-after-complaining-flag)
  "Move backward or forward to the next undeleted message in the buffer.
If optional argument WAIT-AFTER-COMPLAINING-FLAG is non-nil and
we are at the last message, then wait for a second after telling
the user that there aren't any more unread messages."
  (if (eq mh-next-direction 'forward)
      (mh-next-undeleted-msg 1 wait-after-complaining-flag)
    (mh-previous-undeleted-msg 1 wait-after-complaining-flag)))

;;;###mh-autoload
(defun mh-prompt-for-refile-folder ()
  "Prompt the user for a folder in which the message should be filed.
The folder is returned as a string.

The default folder name is generated by the option
`mh-default-folder-for-message-function' if it is non-nil or
`mh-folder-from-address'."
  (mh-prompt-for-folder
   "Destination"
   (let ((refile-file (ignore-errors (mh-msg-filename (mh-get-msg-num t)))))
     (if (null refile-file) ""
       (with-current-buffer (get-buffer-create mh-temp-buffer)
         (erase-buffer)
         (insert-file-contents refile-file)
         (or (and mh-default-folder-for-message-function
                  (let ((buffer-file-name refile-file))
                    (funcall mh-default-folder-for-message-function)))
             (mh-folder-from-address)
             (and (eq 'refile (car mh-last-destination-folder))
                  (symbol-name (cdr mh-last-destination-folder)))
             ""))))
   t))

;;;###mh-autoload
(defun mh-folder-from-address ()
  "Derive folder name from sender.

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
exist."
  ;; Loop for all entries in mh-default-folder-list
  (save-restriction
    (goto-char (point-min))
    (re-search-forward "\n\n" nil 'limit)
    (narrow-to-region (point-min) (point))
    (let ((to/cc (concat (or (message-fetch-field "to") "") ", "
                         (or (message-fetch-field "cc") "")))
          (from (or (message-fetch-field "from") ""))
          folder-name)
      (setq folder-name
            (loop for list in mh-default-folder-list
                  when (string-match (nth 0 list) (if (nth 2 list) to/cc from))
                  return (nth 1 list)
                  finally return nil))

      ;; Make sure a result from `mh-default-folder-list' begins with "+"
      ;; since 'mh-expand-file-name below depends on it
      (when (and folder-name (not (eq (aref folder-name 0) ?+)))
        (setq folder-name (concat "+" folder-name)))

      ;; If not, is there an alias for the address?
      (when (not folder-name)
        (let* ((from-header (mh-extract-from-header-value))
               (address (and from-header
                             (nth 1 (mail-extract-address-components
                                     from-header))))
               (alias (and address (mh-alias-address-to-alias address))))
          (when alias
            (setq folder-name
                  (and alias (concat "+" mh-default-folder-prefix alias))))))

      ;; If mh-default-folder-must-exist-flag set, check that folder exists.
      (if (and folder-name
               (or (not mh-default-folder-must-exist-flag)
                   (file-exists-p (mh-expand-file-name folder-name))))
          folder-name))))

;;;###mh-autoload
(defun mh-delete-a-msg (message)
  "Delete MESSAGE.
If MESSAGE is nil then the message at point is deleted.
The hook `mh-delete-msg-hook' is called after you mark a message
for deletion. For example, a past maintainer of MH-E used this
once when he kept statistics on his mail usage."
  (save-excursion
    (if (numberp message)
        (mh-goto-msg message nil t)
      (beginning-of-line)
      (setq message (mh-get-msg-num t)))
    (if (looking-at mh-scan-refiled-msg-regexp)
        (error "Message %d is refiled; undo refile before deleting" message))
    (if (looking-at mh-scan-deleted-msg-regexp)
        nil
      (mh-set-folder-modified-p t)
      (setq mh-delete-list (cons message mh-delete-list))
      (mh-notate nil mh-note-deleted mh-cmd-note)
      (run-hooks 'mh-delete-msg-hook))))

;;;###mh-autoload
(defun mh-refile-a-msg (message folder)
  "Refile MESSAGE in FOLDER.
If MESSAGE is nil then the message at point is refiled.
Folder is a symbol, not a string.
The hook `mh-refile-msg-hook' is called after a message is marked to
be refiled."
  (save-excursion
    (if (numberp message)
        (mh-goto-msg message nil t)
      (beginning-of-line)
      (setq message (mh-get-msg-num t)))
    (cond ((looking-at mh-scan-deleted-msg-regexp)
           (error "Message %d is deleted; undo delete before moving" message))
          ((looking-at mh-scan-refiled-msg-regexp)
           (if (y-or-n-p
                (format "Message %d already refiled; copy to %s as well? "
                        message folder))
               (mh-exec-cmd "refile" (mh-get-msg-num t) "-link"
                            "-src" mh-current-folder
                            (symbol-name folder))
             (message "Message not copied")))
          (t
           (mh-set-folder-modified-p t)
           (cond ((null (assoc folder mh-refile-list))
                  (push (list folder message) mh-refile-list))
                 ((not (member message (cdr (assoc folder mh-refile-list))))
                  (push message (cdr (assoc folder mh-refile-list)))))
           (mh-notate nil mh-note-refiled mh-cmd-note)
           (run-hooks 'mh-refile-msg-hook)))))

(defun mh-undo-msg (msg)
  "Undo the deletion or refile of one MSG.
If MSG is nil then act on the message at point"
  (save-excursion
    (if (numberp msg)
        (mh-goto-msg msg t t)
      (beginning-of-line)
      (setq msg (mh-get-msg-num t)))
    (cond ((memq msg mh-delete-list)
           (setq mh-delete-list (delq msg mh-delete-list)))
          (t
           (dolist (folder-msg-list mh-refile-list)
             (setf (cdr folder-msg-list) (remove msg (cdr folder-msg-list))))
           (setq mh-refile-list (loop for x in mh-refile-list
                                      unless (null (cdr x)) collect x))))
    (mh-notate nil ?  mh-cmd-note)))

;;;###mh-autoload
(defun mh-msg-filename (msg &optional folder)
  "Return the file name of MSG in FOLDER (default current folder)."
  (expand-file-name (int-to-string msg)
                    (if folder
                        (mh-expand-file-name folder)
                      mh-folder-filename)))

(provide 'mh-folder)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-folder.el ends here
