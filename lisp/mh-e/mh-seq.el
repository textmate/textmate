;;; mh-seq.el --- MH-E sequences support

;; Copyright (C) 1993, 1995, 2001-2012  Free Software Foundation, Inc.

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

;; Sequences are stored in the alist `mh-seq-list' in the form:
;;     ((seq-name msgs ...) (seq-name msgs ...) ...)

;;; Change Log:

;;; Code:

(require 'mh-e)
(mh-require-cl)
(require 'mh-scan)

(require 'font-lock)

;;; Variables

(defvar mh-last-seq-used nil
  "Name of seq to which a msg was last added.")

(defvar mh-non-seq-mode-line-annotation nil
  "Saved value of `mh-mode-line-annotation' when narrowed to a seq.")
(make-variable-buffer-local 'mh-non-seq-mode-line-annotation)

(defvar mh-internal-seqs '(answered cur deleted forwarded printed))

;;; Macros

(defsubst mh-make-seq (name msgs)
  "Create sequence NAME with the given MSGS."
  (cons name msgs))

(defsubst mh-seq-name (sequence)
  "Extract sequence name from the given SEQUENCE."
  (car sequence))



;;; MH-Folder Commands

;; Alphabetical.

;;;###mh-autoload
(defun mh-catchup (range)
  "Delete RANGE from the \"unseen\" sequence.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use."
  (interactive (list (mh-interactive-range "Catchup"
                                           (cons (point-min) (point-max)))))
  (mh-delete-msg-from-seq range mh-unseen-seq))

;;;###mh-autoload
(defun mh-delete-msg-from-seq (range sequence &optional internal-flag)
  "Delete RANGE from SEQUENCE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

In a program, non-nil INTERNAL-FLAG means do not inform MH of the
change."
  (interactive (list (mh-interactive-range "Delete")
                     (mh-read-seq-default "Delete from" t)
                     nil))
  (let ((entry (mh-find-seq sequence))
        (user-sequence-flag (not (mh-internal-seq sequence)))
        (folders-changed (list mh-current-folder))
        (msg-list ()))
    (when entry
      (mh-iterate-on-range msg range
        (push msg msg-list)
        ;; Calling "mark" repeatedly takes too long. So we will pretend here
        ;; that we are just modifying an internal sequence...
        (when (memq msg (cdr entry))
          (mh-remove-sequence-notation msg (not user-sequence-flag)))
        (mh-delete-a-msg-from-seq msg sequence t))
      ;; ... and here we will "mark" all the messages at one go.
      (unless internal-flag (mh-undefine-sequence sequence msg-list))
      (when (and mh-index-data (not internal-flag))
        (setq folders-changed
              (append folders-changed
                      (mh-index-delete-from-sequence sequence msg-list))))
      (when (and (eq sequence mh-unseen-seq) (mh-speed-flists-active-p))
        (apply #'mh-speed-flists t folders-changed)))))

;;;###mh-autoload
(defun mh-delete-seq (sequence)
  "Delete SEQUENCE.

You are prompted for the sequence to delete. Note that this
deletes only the sequence, not the messages in the sequence. If
you want to delete the messages, use \"\\[universal-argument]
\\[mh-delete-msg]\"."
  (interactive (list (mh-read-seq-default "Delete" t)))
  (let ((msg-list (mh-seq-to-msgs sequence))
        (internal-flag (mh-internal-seq sequence))
        (folders-changed (list mh-current-folder)))
    (mh-iterate-on-range msg sequence
      (mh-remove-sequence-notation msg internal-flag))
    (mh-undefine-sequence sequence '("all"))
    (mh-delete-seq-locally sequence)
    (when mh-index-data
      (setq folders-changed
            (append folders-changed
                    (mh-index-delete-from-sequence sequence msg-list))))
    (when (and (eq sequence mh-unseen-seq) (mh-speed-flists-active-p))
      (apply #'mh-speed-flists t folders-changed))))

;; Shush compiler.
(defvar view-exit-action)

;;;###mh-autoload
(defun mh-list-sequences ()
  "List all sequences in folder.

The list appears in a buffer named \"*MH-E Sequences*\"."
  (interactive)
  (let ((folder mh-current-folder)
        (temp-buffer mh-sequences-buffer)
        (seq-list mh-seq-list)
        (max-len 0))
    (with-output-to-temp-buffer temp-buffer
      (with-current-buffer temp-buffer
        (erase-buffer)
        (message "Listing sequences ...")
        (insert "Sequences in folder " folder ":\n")
        (let ((seq-list seq-list))
          (while seq-list
            (setq max-len
                  (max (length (symbol-name (mh-seq-name (pop seq-list))))
                       max-len)))
          (setq max-len (+ 2 max-len)))
        (while seq-list
          (let ((name (mh-seq-name (car seq-list)))
                (sorted-seq-msgs
                 (mh-coalesce-msg-list
                  (sort (copy-sequence (mh-seq-msgs (car seq-list))) '<)))
                name-spec)
            (insert (setq name-spec (format (format "%%%ss:" max-len) name)))
            (while sorted-seq-msgs
              (let ((next-element (format " %s" (pop sorted-seq-msgs))))
                (when (>= (+ (current-column) (length next-element))
                          (window-width))
                  (insert "\n")
                  (insert (format (format "%%%ss" (length name-spec)) "")))
                (insert next-element)))
            (insert "\n"))
          (setq seq-list (cdr seq-list)))
        (goto-char (point-min))
        (mh-view-mode-enter)
        (setq view-exit-action 'kill-buffer)
        (message "Listing sequences...done")))))

;;;###mh-autoload
(defun mh-msg-is-in-seq (message)
  "Display the sequences in which the current message appears.

Use a prefix argument to display the sequences in which another
MESSAGE appears."
  (interactive "P")
  (if (not message)
      (setq message (mh-get-msg-num t)))
  (let* ((dest-folder (loop for seq in mh-refile-list
                            when (member message (cdr seq)) return (car seq)
                            finally return nil))
         (deleted-flag (unless dest-folder (member message mh-delete-list))))
    (message "Message %d%s is in sequences: %s"
             message
             (cond (dest-folder (format " (to be refiled to %s)" dest-folder))
                   (deleted-flag (format " (to be deleted)"))
                   (t ""))
             (mapconcat 'concat
                        (mh-list-to-string (mh-seq-containing-msg message t))
                        " "))))

;; Shush compiler.
(mh-do-in-xemacs
  (defvar tool-bar-mode))
(defvar tool-bar-map)

;;;###mh-autoload
(defun mh-narrow-to-seq (sequence)
  "Restrict display to messages in SEQUENCE.

You are prompted for the name of the sequence. What this command
does is show only those messages that are in the selected
sequence in the MH-Folder buffer. In addition, it limits further
MH-E searches to just those messages.

When you want to widen the view to all your messages again, use
\\[mh-widen]."
  (interactive (list (mh-read-seq "Narrow to" t)))
  (with-mh-folder-updating (t)
    (cond ((mh-seq-to-msgs sequence)
           (mh-remove-all-notation)
           (let ((eob (point-max))
                 (msg-at-cursor (mh-get-msg-num nil)))
             (push mh-thread-scan-line-map mh-thread-scan-line-map-stack)
             (setq mh-thread-scan-line-map (make-hash-table :test #'eql))
             (mh-copy-seq-to-eob sequence)
             (push (buffer-substring-no-properties (point-min) eob)
                   mh-folder-view-stack)
             (delete-region (point-min) eob)
             (mh-notate-deleted-and-refiled)
             (mh-notate-cur)
             (when msg-at-cursor (mh-goto-msg msg-at-cursor t t))
             (setq mh-non-seq-mode-line-annotation mh-mode-line-annotation)
             (setq mh-mode-line-annotation (symbol-name sequence))
             (mh-make-folder-mode-line)
             (mh-recenter nil)
             (when (and (boundp 'tool-bar-mode) tool-bar-mode)
               (set (make-local-variable 'tool-bar-map)
                    mh-folder-seq-tool-bar-map)
               (when (buffer-live-p (get-buffer mh-show-buffer))
                 (with-current-buffer mh-show-buffer
                   (set (make-local-variable 'tool-bar-map)
                        mh-show-seq-tool-bar-map))))
             (push 'widen mh-view-ops)))
          (t
           (error "No messages in sequence %s" (symbol-name sequence))))))

;;;###mh-autoload
(defun mh-narrow-to-tick ()
  "Limit to ticked messages.

What this command does is show only those messages that are in
the \"tick\" sequence (which you can customize via the
`mh-tick-seq' option) in the MH-Folder buffer. In addition, it
limits further MH-E searches to just those messages. When you
want to widen the view to all your messages again, use
\\[mh-widen]."
  (interactive)
  (cond ((not mh-tick-seq)
         (error "Enable ticking by customizing `mh-tick-seq'"))
        ((null (mh-seq-msgs (mh-find-seq mh-tick-seq)))
         (message "No messages in %s sequence" mh-tick-seq))
        (t (mh-narrow-to-seq mh-tick-seq))))

;;;###mh-autoload
(defun mh-put-msg-in-seq (range sequence)
  "Add RANGE to SEQUENCE\\<mh-folder-mode-map>.

Give this command a RANGE and you can add all the messages in a
sequence to another sequence (for example,
\"\\[universal-argument] \\[mh-put-msg-in-seq] SourceSequence RET
DestSequence RET\"). Check the documentation of
`mh-interactive-range' to see how RANGE is read in interactive
use."
  (interactive (list (mh-interactive-range "Add messages from")
                     (mh-read-seq-default "Add to" nil)))
  (unless (mh-valid-seq-p sequence)
    (error "Can't put message in invalid sequence %s" sequence))
  (let* ((internal-seq-flag (mh-internal-seq sequence))
         (original-msgs (mh-seq-msgs (mh-find-seq sequence)))
         (folders (list mh-current-folder))
         (msg-list (mh-range-to-msg-list range)))
    (mh-add-msgs-to-seq msg-list sequence nil t)
    (mh-iterate-on-range m range
      (unless (memq m original-msgs)
        (mh-add-sequence-notation m internal-seq-flag)))
    (if (not internal-seq-flag)
        (setq mh-last-seq-used sequence))
    (when mh-index-data
      (setq folders
            (append folders (mh-index-add-to-sequence sequence msg-list))))
    (when (and (eq sequence mh-unseen-seq) (mh-speed-flists-active-p))
      (apply #'mh-speed-flists t folders))))

;;;###mh-autoload
(defun mh-toggle-tick (range)
  "Toggle tick mark of RANGE.

This command adds messages to the \"tick\" sequence (which you can customize
via the option `mh-tick-seq'). This sequence can be viewed later with the
\\[mh-index-ticked-messages] command.

Check the documentation of `mh-interactive-range' to see how RANGE is read in
interactive use."
  (interactive (list (mh-interactive-range "Tick")))
  (unless mh-tick-seq
    (error "Enable ticking by customizing `mh-tick-seq'"))
  (let* ((tick-seq (mh-find-seq mh-tick-seq))
         (tick-seq-msgs (mh-seq-msgs tick-seq))
         (ticked ())
         (unticked ()))
    (mh-iterate-on-range msg range
      (cond ((member msg tick-seq-msgs)
             (push msg unticked)
             (setcdr tick-seq (delq msg (cdr tick-seq)))
             (when (null (cdr tick-seq)) (setq mh-last-seq-used nil))
             (mh-remove-sequence-notation msg (mh-colors-in-use-p)))
            (t
             (push msg ticked)
             (setq mh-last-seq-used mh-tick-seq)
             (let ((mh-seq-list (cons `(,mh-tick-seq ,msg) mh-seq-list)))
               (mh-add-sequence-notation msg (mh-colors-in-use-p))))))
    (mh-add-msgs-to-seq ticked mh-tick-seq nil t)
    (mh-undefine-sequence mh-tick-seq unticked)
    (when mh-index-data
      (mh-index-add-to-sequence mh-tick-seq ticked)
      (mh-index-delete-from-sequence mh-tick-seq unticked))))

;;;###mh-autoload
(defun mh-widen (&optional all-flag)
  "Remove last restriction.

Each limit or sequence restriction can be undone in turn with
this command. Give this command a prefix argument ALL-FLAG to
remove all limits and sequence restrictions."
  (interactive "P")
  (let ((msg (mh-get-msg-num nil)))
    (when mh-folder-view-stack
      (cond (all-flag
             (while (cdr mh-view-ops)
               (setq mh-view-ops (cdr mh-view-ops)))
             (when (eq (car mh-view-ops) 'widen)
               (setq mh-view-ops (cdr mh-view-ops))))
            ((mh-valid-view-change-operation-p 'widen) nil)
            ((memq 'widen mh-view-ops)
             (while (not (eq (car mh-view-ops) 'widen))
               (setq mh-view-ops (cdr mh-view-ops)))
             (setq mh-view-ops (cdr mh-view-ops)))
            (t (error "Widening is not applicable")))
      ;; If ALL-FLAG is non-nil then rewind stacks
      (when all-flag
        (while (cdr mh-thread-scan-line-map-stack)
          (setq mh-thread-scan-line-map-stack
                (cdr mh-thread-scan-line-map-stack)))
        (while (cdr mh-folder-view-stack)
          (setq mh-folder-view-stack (cdr mh-folder-view-stack))))
      (setq mh-thread-scan-line-map (pop mh-thread-scan-line-map-stack))
      (with-mh-folder-updating (t)
        (delete-region (point-min) (point-max))
        (insert (pop mh-folder-view-stack))
        (mh-remove-all-notation)
        (setq mh-mode-line-annotation mh-non-seq-mode-line-annotation)
        (mh-make-folder-mode-line))
      (if msg
          (mh-goto-msg msg t t))
      (mh-notate-deleted-and-refiled)
      (mh-notate-user-sequences)
      (mh-notate-cur)
      (mh-recenter nil)))
  (when (and (null mh-folder-view-stack) (boundp 'tool-bar-mode) tool-bar-mode)
    (set (make-local-variable 'tool-bar-map) mh-folder-tool-bar-map)
    (when (buffer-live-p (get-buffer mh-show-buffer))
      (with-current-buffer mh-show-buffer
        (set (make-local-variable 'tool-bar-map) mh-show-tool-bar-map)))))



;;; Support Routines

(defvar mh-sequence-history ())

;;;###mh-autoload
(defun mh-read-seq-default (prompt not-empty)
  "Read and return sequence name with default narrowed or previous sequence.
PROMPT is the prompt to use when reading. If NOT-EMPTY is non-nil
then a non-empty sequence is read."
  (mh-read-seq prompt not-empty
               (or mh-last-seq-used
                   (car (mh-seq-containing-msg (mh-get-msg-num nil) nil)))))

(defun mh-read-seq (prompt not-empty &optional default)
  "Read and return a sequence name.
Prompt with PROMPT, raise an error if the sequence is empty and
the NOT-EMPTY flag is non-nil, and supply an optional DEFAULT
sequence. A reply of '%' defaults to the first sequence
containing the current message."
  (let* ((input (completing-read (format "%s sequence%s: " prompt
                                         (if default
                                             (format " (default %s)" default)
                                           ""))
                                 (mh-seq-names mh-seq-list)
                                 nil nil nil 'mh-sequence-history))
         (seq (cond ((equal input "%")
                     (car (mh-seq-containing-msg (mh-get-msg-num t) nil)))
                    ((equal input "") default)
                    (t (intern input))))
         (msgs (mh-seq-to-msgs seq)))
    (if (and (null msgs) not-empty)
        (error "No messages in sequence %s" seq))
    seq))

(defun mh-internal-seq (name)
  "Return non-nil if NAME is the name of an internal MH-E sequence."
  (or (memq name mh-internal-seqs)
      (eq name mh-unseen-seq)
      (and (mh-colors-in-use-p) mh-tick-seq (eq name mh-tick-seq))
      (eq name mh-previous-seq)
      (mh-folder-name-p name)))

;;;###mh-autoload
(defun mh-valid-seq-p (name)
  "Return non-nil if NAME is a valid MH sequence name."
  (and (symbolp name)
       (string-match "^[a-zA-Z][a-zA-Z0-9]*$" (symbol-name name))))

;;;###mh-autoload
(defun mh-find-seq (name)
  "Return sequence NAME."
  (assoc name mh-seq-list))

;;;###mh-autoload
(defun mh-seq-to-msgs (seq)
  "Return a list of the messages in SEQ."
  (mh-seq-msgs (mh-find-seq seq)))

(defun mh-seq-containing-msg (msg &optional include-internal-flag)
  "Return a list of the sequences containing MSG.
If INCLUDE-INTERNAL-FLAG non-nil, include MH-E internal sequences
in list."
  (let ((l mh-seq-list)
        (seqs ()))
    (while l
      (and (memq msg (mh-seq-msgs (car l)))
           (or include-internal-flag
               (not (mh-internal-seq (mh-seq-name (car l)))))
           (setq seqs (cons (mh-seq-name (car l)) seqs)))
      (setq l (cdr l)))
    seqs))

;;;###mh-autoload
(defun mh-define-sequence (seq msgs)
  "Define the SEQ to contain the list of MSGS.
Do not mark pseudo-sequences or empty sequences.
Signals an error if SEQ is an invalid name."
  (if (and msgs
           (mh-valid-seq-p seq)
           (not (mh-folder-name-p seq)))
      (save-excursion
        (mh-exec-cmd-error nil "mark" mh-current-folder "-add" "-zero"
                           "-sequence" (symbol-name seq)
                           (mh-coalesce-msg-list msgs)))))

;;;###mh-autoload
(defun mh-undefine-sequence (seq msgs)
  "Remove from the SEQ the list of MSGS."
  (when (and (mh-valid-seq-p seq) msgs)
    (apply #'mh-exec-cmd "mark" mh-current-folder "-delete"
           "-sequence" (symbol-name seq) (mh-coalesce-msg-list msgs))))

;;;###mh-autoload
(defun mh-add-msgs-to-seq (msgs seq &optional internal-flag dont-annotate-flag)
  "Add MSGS to SEQ.

Remove duplicates and keep sequence sorted. If optional
INTERNAL-FLAG is non-nil, do not mark the message in the scan
listing or inform MH of the addition.

If DONT-ANNOTATE-FLAG is non-nil then the annotations in the
folder buffer are not updated."
  (let ((entry (mh-find-seq seq))
        (internal-seq-flag (mh-internal-seq seq)))
    (if (and msgs (atom msgs)) (setq msgs (list msgs)))
    (if (null entry)
        (setq mh-seq-list
              (cons (mh-make-seq seq (mh-canonicalize-sequence msgs))
                    mh-seq-list))
      (if msgs (setcdr entry (mh-canonicalize-sequence
                              (append msgs (mh-seq-msgs entry))))))
    (unless internal-flag
      (mh-add-to-sequence seq msgs)
      (when (not dont-annotate-flag)
        (mh-iterate-on-range msg msgs
          (unless (memq msg (cdr entry))
            (mh-add-sequence-notation msg internal-seq-flag)))))))

(defun mh-add-to-sequence (seq msgs)
  "The sequence SEQ is augmented with the messages in MSGS."
  ;; Add to a SEQUENCE each message the list of MSGS.
  (if (and (mh-valid-seq-p seq) (not (mh-folder-name-p seq)))
      (if msgs
          (apply 'mh-exec-cmd "mark" mh-current-folder "-add"
                 "-sequence" (symbol-name seq)
                 (mh-coalesce-msg-list msgs)))))

(defun mh-canonicalize-sequence (msgs)
  "Sort MSGS in decreasing order and remove duplicates."
  (let* ((sorted-msgs (sort (copy-sequence msgs) '>))
         (head sorted-msgs))
    (while (cdr head)
      (if (= (car head) (cadr head))
          (setcdr head (cddr head))
        (setq head (cdr head))))
    sorted-msgs))

(defun mh-delete-a-msg-from-seq (msg sequence internal-flag)
  "Delete MSG from SEQUENCE.
If INTERNAL-FLAG is non-nil, then do not inform MH of the
change."
  (let ((entry (mh-find-seq sequence)))
    (when (and entry (memq msg (mh-seq-msgs entry)))
      (if (not internal-flag)
          (mh-undefine-sequence sequence (list msg)))
      (setcdr entry (delq msg (mh-seq-msgs entry))))))

(defun mh-delete-seq-locally (seq)
  "Remove MH-E's record of SEQ."
  (let ((entry (mh-find-seq seq)))
    (setq mh-seq-list (delq entry mh-seq-list))))

(defun mh-copy-seq-to-eob (seq)
  "Copy SEQ to the end of the buffer."
  ;; It is quite involved to write something which will work at any place in
  ;; the buffer, so we will write something which works only at the end of
  ;; the buffer. If we ever need to insert sequences in the middle of the
  ;; buffer, this will need to be fixed.
  (save-excursion
    (let* ((msgs (mh-seq-to-msgs seq))
           (coalesced-msgs (mh-coalesce-msg-list msgs)))
      (goto-char (point-max))
      (save-restriction
        (narrow-to-region (point) (point))
        (mh-regenerate-headers coalesced-msgs t)
        (cond ((memq 'unthread mh-view-ops)
               ;; Populate restricted scan-line map
               (mh-remove-all-notation)
               (mh-iterate-on-range msg (cons (point-min) (point-max))
                 (setf (gethash msg mh-thread-scan-line-map)
                       (mh-thread-parse-scan-line)))
               ;; Remove scan lines and read results from pre-computed tree
               (delete-region (point-min) (point-max))
               (mh-thread-print-scan-lines
                (mh-thread-generate mh-current-folder ()))
               (mh-notate-user-sequences))
              (mh-index-data
               (mh-index-insert-folder-headers)))))))

;;;###mh-autoload
(defun mh-valid-view-change-operation-p (op)
  "Check if the view change operation can be performed.
OP is one of 'widen and 'unthread."
  (cond ((eq (car mh-view-ops) op)
         (pop mh-view-ops))
        (t nil)))



;;; Ranges

(defvar mh-range-seq-names)
(defvar mh-range-history ())
(defvar mh-range-completion-map (copy-keymap minibuffer-local-completion-map))
(define-key mh-range-completion-map " " 'self-insert-command)

;;;###mh-autoload
(defun mh-interactive-range (range-prompt &optional default)
  "Return interactive specification for message, sequence, range or region.
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
provide a uniform interface to MH-E functions."
  (cond ((mh-mark-active-p t) (cons (region-beginning) (region-end)))
        (current-prefix-arg (mh-read-range range-prompt nil nil t t))
        (default default)
        (t (mh-get-msg-num t))))

;;;###mh-autoload
(defun mh-read-range (prompt &optional folder default
                             expand-flag ask-flag number-as-range-flag)
  "Read a message range with PROMPT.

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
                 mh-interpret-number-as-range-flag)"
  (setq default (or default mh-last-seq-used
                    (car (mh-seq-containing-msg (mh-get-msg-num nil) t)))
        prompt (format "%s range" prompt))
  (let* ((folder (or folder mh-current-folder))
         (guess (eq default t))
         (counts (and guess (mh-folder-size folder)))
         (unseen (and counts (> (cadr counts) 0)))
         (large (and counts mh-large-folder (> (car counts) mh-large-folder)))
         (default (cond ((and guess large) (format "last:%s" mh-large-folder))
                        ((and guess (not large)) "all")
                        ((stringp default) default)
                        ((symbolp default) (symbol-name default))))
         (prompt (cond ((and guess large default)
                        (format "%s (folder has %s messages, default %s)"
                                prompt (car counts) default))
                       ((and guess large)
                        (format "%s (folder has %s messages)"
                                prompt (car counts)))
                       (default
                         (format "%s (default %s)" prompt default))))
         (minibuffer-local-completion-map mh-range-completion-map)
         (seq-list (if (eq folder mh-current-folder)
                       mh-seq-list
                     (mh-read-folder-sequences folder nil)))
         (mh-range-seq-names
          (append '(("first") ("last") ("all") ("prev") ("next"))
                  (mh-seq-names seq-list)))
         (input (cond ((and (not ask-flag) unseen) (symbol-name mh-unseen-seq))
                      ((and (not ask-flag) (not large)) "all")
                      (t (completing-read (format "%s: " prompt)
                                          'mh-range-completion-function nil nil
                                          nil 'mh-range-history default))))
         msg-list)
    (when (and number-as-range-flag
               (string-match "^[ \t]*\\([0-9]+\\)[ \t]*$" input))
      (setq input (concat "last:" (match-string 1 input))))
    (cond ((not expand-flag) input)
          ((assoc (intern input) seq-list)
           (cdr (assoc (intern input) seq-list)))
          ((setq msg-list (mh-translate-range folder input)) msg-list)
          (t (error "No messages in range %s" input)))))

;;;###mh-autoload
(defun mh-range-to-msg-list (range)
  "Return a list of messages for RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use."
  (let (msg-list)
    (mh-iterate-on-range msg range
      (push msg msg-list))
    (nreverse msg-list)))

;;;###mh-autoload
(defun mh-translate-range (folder expr)
  "In FOLDER, translate the string EXPR to a list of messages numbers."
  (save-excursion
    (let ((strings (delete "" (split-string expr "[ \t\n]")))
          (result ()))
      (ignore-errors
        (apply #'mh-exec-cmd-quiet nil "mhpath" folder strings)
        (set-buffer mh-temp-buffer)
        (goto-char (point-min))
        (while (re-search-forward "/\\([0-9]*\\)$" nil t)
          (push (string-to-number (match-string 1)) result))
        (nreverse result)))))

(defun mh-range-completion-function (string predicate flag)
  "Programmable completion of message ranges.
STRING is the user input that is to be completed. PREDICATE if non-nil is a
function used to filter the possible choices and FLAG determines whether the
completion is over."
  (let* ((candidates mh-range-seq-names)
         (last-char (and (not (equal string ""))
                         (aref string (1- (length string)))))
         (last-word (cond ((null last-char) "")
                          ((memq last-char '(?  ?- ?:)) "")
                          (t (car (last (split-string string "[ -:]+"))))))
         (prefix (substring string 0 (- (length string) (length last-word)))))
    (cond ((eq flag nil)
           (let ((res (try-completion last-word candidates predicate)))
             (cond ((null res) nil)
                   ((eq res t) t)
                   (t (concat prefix res)))))
          ((eq flag t)
           (all-completions last-word candidates predicate))
          ((eq flag 'lambda)
           (loop for x in candidates
                 when (equal x last-word) return t
                 finally return nil)))))

(defun mh-seq-names (seq-list)
  "Return an alist containing the names of the SEQ-LIST."
  (mapcar (lambda (entry) (list (symbol-name (mh-seq-name entry))))
          seq-list))

(defun mh-folder-size (folder)
  "Find size of FOLDER."
  (if mh-flists-present-flag
      (mh-folder-size-flist folder)
    (mh-folder-size-folder folder)))

(defun mh-folder-size-flist (folder)
  "Find size of FOLDER using \"flist\"."
  (with-temp-buffer
    (call-process (expand-file-name "flist" mh-progs) nil t nil "-showzero"
                  "-norecurse" folder "-sequence" (symbol-name mh-unseen-seq))
    (goto-char (point-min))
    (multiple-value-bind (folder unseen total)
        (values-list
         (mh-parse-flist-output-line
          (buffer-substring (point) (mh-line-end-position))))
      (list total unseen folder))))

(defun mh-folder-size-folder (folder)
  "Find size of FOLDER using \"folder\"."
  (with-temp-buffer
    (let ((u (length (cdr (assoc mh-unseen-seq
                                 (mh-read-folder-sequences folder nil))))))
      (call-process (expand-file-name "folder" mh-progs) nil t nil
                    "-norecurse" folder)
      (goto-char (point-min))
      (if (re-search-forward " has \\([0-9]+\\) " nil t)
          (list (string-to-number (match-string 1)) u folder)
        (list 0 u folder)))))

;;;###mh-autoload
(defun mh-parse-flist-output-line (line &optional current-folder)
  "Parse LINE to generate folder name, unseen messages and total messages.
If CURRENT-FOLDER is non-nil then it contains the current folder
name and it is used to avoid problems in corner cases involving
folders whose names end with a '+' character."
  (with-temp-buffer
    (insert line)
    (goto-char (point-max))
    (let (folder unseen total p)
      (when (search-backward " out of " (point-min) t)
        (setq total (string-to-number
                     (buffer-substring-no-properties
                      (match-end 0) (mh-line-end-position))))
        (when (search-backward " in sequence " (point-min) t)
          (setq p (point))
          (when (search-backward " has " (point-min) t)
            (setq unseen (string-to-number (buffer-substring-no-properties
                                            (match-end 0) p)))
            (while (eq (char-after) ? )
              (backward-char))
            (setq folder (buffer-substring-no-properties
                          (point-min) (1+ (point))))
            (when (and (equal (aref folder (1- (length folder))) ?+)
                       (equal current-folder folder))
              (setq folder (substring folder 0 (1- (length folder)))))
            (list (format "+%s" folder) unseen total)))))))

;;;###mh-autoload
(defun mh-read-folder-sequences (folder save-refiles)
  "Read and return the predefined sequences for a FOLDER.
If SAVE-REFILES is non-nil, then keep the sequences
that note messages to be refiled."
  (let ((seqs ()))
    (cond (save-refiles
           (mh-mapc (function (lambda (seq) ; Save the refiling sequences
                                (if (mh-folder-name-p (mh-seq-name seq))
                                    (setq seqs (cons seq seqs)))))
                    mh-seq-list)))
    (save-excursion
      (if (eq 0 (mh-exec-cmd-quiet nil "mark" folder "-list"))
          (progn
            ;; look for name in line of form "cur: 4" or "myseq (private): 23"
            (while (re-search-forward "^[^: ]+" nil t)
              (setq seqs (cons (mh-make-seq (intern (buffer-substring
                                                     (match-beginning 0)
                                                     (match-end 0)))
                                            (mh-read-msg-list))
                               seqs)))
            (delete-region (point-min) (point))))) ; avoid race with
                                        ; mh-process-daemon
    seqs))

(defun mh-read-msg-list ()
  "Return a list of message numbers from point to the end of the line.
Expands ranges into set of individual numbers."
  (let ((msgs ())
        (end-of-line (point-at-eol))
        num)
    (while (re-search-forward "[0-9]+" end-of-line t)
      (setq num (string-to-number (buffer-substring (match-beginning 0)
                                                    (match-end 0))))
      (cond ((looking-at "-")           ; Message range
             (forward-char 1)
             (re-search-forward "[0-9]+" end-of-line t)
             (let ((num2 (string-to-number
                          (buffer-substring (match-beginning 0)
                                            (match-end 0)))))
               (if (< num2 num)
                   (error "Bad message range: %d-%d" num num2))
               (while (<= num num2)
                 (setq msgs (cons num msgs))
                 (setq num (1+ num)))))
            ((not (zerop num))          ;"pick" outputs "0" to mean no match
             (setq msgs (cons num msgs)))))
    msgs))



;;; Notation

;;;###mh-autoload
(defun mh-notate (msg notation offset)
  "Mark MSG with the character NOTATION at position OFFSET.
Null MSG means the message at cursor.
If NOTATION is nil then no change in the buffer occurs."
  (save-excursion
    (if (or (null msg)
            (mh-goto-msg msg t t))
        (with-mh-folder-updating (t)
          (beginning-of-line)
          (forward-char offset)
          (let* ((change-stack-flag
                  (and (equal offset
                              (+ mh-cmd-note mh-scan-field-destination-offset))
                       (not (eq notation mh-note-seq))))
                 (msg (and change-stack-flag (or msg (mh-get-msg-num nil))))
                 (stack (and msg (gethash msg mh-sequence-notation-history)))
                 (notation (or notation (char-after))))
            (if stack
                ;; The presence of the stack tells us that we don't need to
                ;; notate the message, since the notation would be replaced
                ;; by a sequence notation. So we will just put the notation
                ;; at the bottom of the stack. If the sequence is deleted,
                ;; the correct notation will be shown.
                (setf (gethash msg mh-sequence-notation-history)
                      (reverse (cons notation (cdr (reverse stack)))))
              ;; Since we don't have any sequence notations in the way, just
              ;; notate the scan line.
              (delete-char 1)
              (insert notation))
            (when change-stack-flag
              (mh-thread-update-scan-line-map msg notation offset)))))))

;;;###mh-autoload
(defun mh-notate-cur ()
  "Mark the MH sequence cur.
In addition to notating the current message with `mh-note-cur'
the function uses `overlay-arrow-position' to put a marker in the
fringe."
  (let ((cur (car (mh-seq-to-msgs 'cur))))
    (when (and cur (mh-goto-msg cur t t))
      (beginning-of-line)
      (when (looking-at mh-scan-good-msg-regexp)
        (mh-notate nil mh-note-cur mh-cmd-note))
      (setq mh-arrow-marker (set-marker mh-arrow-marker (point)))
      (setq overlay-arrow-position mh-arrow-marker))))

;;;###mh-autoload
(defun mh-remove-cur-notation ()
  "Remove old cur notation."
  (let ((cur-msg (car (mh-seq-to-msgs 'cur))))
    (save-excursion
      (when (and cur-msg
                 (mh-goto-msg cur-msg t t)
                 (looking-at mh-scan-cur-msg-number-regexp))
        (mh-notate nil ?  mh-cmd-note)
        (setq overlay-arrow-position nil)))))

;; FIXME?  We may want to clear all notations and add one for current-message
;;         and process user sequences.
;;;###mh-autoload
(defun mh-notate-deleted-and-refiled ()
  "Notate messages marked for deletion or refiling.
Messages to be deleted are given by `mh-delete-list' while
messages to be refiled are present in `mh-refile-list'."
  (let ((refiled-hash (make-hash-table))
        (deleted-hash (make-hash-table)))
    (dolist (msg mh-delete-list)
      (setf (gethash msg deleted-hash) t))
    (dolist (dest-msg-list mh-refile-list)
      (dolist (msg (cdr dest-msg-list))
        (setf (gethash msg refiled-hash) t)))
    (mh-iterate-on-messages-in-region msg (point-min) (point-max)
      (cond ((gethash msg refiled-hash)
             (mh-notate nil mh-note-refiled mh-cmd-note))
            ((gethash msg deleted-hash)
             (mh-notate nil mh-note-deleted mh-cmd-note))))))

;;;###mh-autoload
(defun mh-notate-user-sequences (&optional range)
  "Mark user-defined sequences in RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use; if nil all messages are
notated."
  (unless range
    (setq range (cons (point-min) (point-max))))
  (let ((seqs mh-seq-list)
        (msg-hash (make-hash-table)))
    (dolist (seq seqs)
      (dolist (msg (mh-seq-msgs seq))
        (push (car seq) (gethash msg msg-hash))))
    (mh-iterate-on-range msg range
      (loop for seq in (gethash msg msg-hash)
            do (mh-add-sequence-notation msg (mh-internal-seq seq))))))

(defun mh-add-sequence-notation (msg internal-seq-flag)
  "Add sequence notation to the MSG on the current line.
If INTERNAL-SEQ-FLAG is non-nil, then refontify the scan line if
font-lock is turned on."
  (with-mh-folder-updating (t)
    (save-excursion
      (beginning-of-line)
      (if internal-seq-flag
          (progn
            ;; Change the buffer so that if transient-mark-mode is active
            ;; and there is an active region it will get deactivated as in
            ;; the case of user sequences.
            (mh-notate nil nil mh-cmd-note)
            (when font-lock-mode
              (font-lock-fontify-region (point) (mh-line-end-position))))
        (forward-char (+ mh-cmd-note mh-scan-field-destination-offset))
        (let ((stack (gethash msg mh-sequence-notation-history)))
          (setf (gethash msg mh-sequence-notation-history)
                (cons (char-after) stack)))
        (mh-notate nil mh-note-seq
                   (+ mh-cmd-note mh-scan-field-destination-offset))))))

(defun mh-remove-sequence-notation (msg internal-seq-flag &optional all)
  "Remove sequence notation from the MSG on the current line.
If INTERNAL-SEQ-FLAG is non-nil, then `font-lock' was used to
highlight the sequence. In that case, no notation needs to be removed.
Otherwise the effect of inserting `mh-note-seq' needs to be reversed.
If ALL is non-nil, then all sequence marks on the scan line are
removed."
  (with-mh-folder-updating (t)
    ;; This takes care of internal sequences...
    (mh-notate nil nil mh-cmd-note)
    (unless internal-seq-flag
      ;; ... and this takes care of user sequences.
      (let ((stack (gethash msg mh-sequence-notation-history)))
        (while (and all (cdr stack))
          (setq stack (cdr stack)))
        (when stack
          (save-excursion
            (beginning-of-line)
            (forward-char (+ mh-cmd-note mh-scan-field-destination-offset))
            (delete-char 1)
            (insert (car stack))))
        (setf (gethash msg mh-sequence-notation-history) (cdr stack))))))

;;;###mh-autoload
(defun mh-remove-all-notation ()
  "Remove all notations on all scan lines that MH-E introduces."
  (save-excursion
    (setq overlay-arrow-position nil)
    (goto-char (point-min))
    (mh-iterate-on-range msg (cons (point-min) (point-max))
      (mh-notate nil ?  mh-cmd-note)
      (mh-remove-sequence-notation msg nil t))
    (clrhash mh-sequence-notation-history)))



;; XXX Unused, delete, or create bind key?
(defun mh-rename-seq (sequence new-name)
  "Rename SEQUENCE to have NEW-NAME."
  (interactive (list (mh-read-seq "Old" t)
                     (intern (read-string "New sequence name: "))))
  (let ((old-seq (mh-find-seq sequence)))
    (or old-seq
        (error "Sequence %s does not exist" sequence))
    ;; Create new sequence first, since it might raise an error.
    (mh-define-sequence new-name (mh-seq-msgs old-seq))
    (mh-undefine-sequence sequence (mh-seq-msgs old-seq))
    (rplaca old-seq new-name)))

(provide 'mh-seq)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-seq.el ends here
