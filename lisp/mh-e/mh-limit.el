;;; mh-limit.el --- MH-E display limits

;; Copyright (C) 2001-2003, 2006-2012  Free Software Foundation, Inc.

;; Author: Peter S. Galbraith <psg@debian.org>
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

;; "Poor man's threading" by psg.

;;; Change Log:

;;; Code:

(require 'mh-e)
(mh-require-cl)
(require 'mh-scan)

(autoload 'message-fetch-field "message")



;;; MH-Folder Commands

;; Alphabetical.

;;;###mh-autoload
(defun mh-delete-subject ()
  "Delete messages with same subject\\<mh-folder-mode-map>.

To delete messages faster, you can use this command to delete all
the messages with the same subject as the current message. This
command puts these messages in a sequence named \"subject\". You
can undo this action by using \\[mh-undo] with a prefix argument
and then specifying the \"subject\" sequence."
  (interactive)
  (let ((count (mh-subject-to-sequence nil)))
    (cond
     ((not count)                       ; No subject line, delete msg anyway
      (mh-delete-msg (mh-get-msg-num t)))
     ((= 0 count)                       ; No other msgs, delete msg anyway.
      (message "No other messages with same Subject following this one")
      (mh-delete-msg (mh-get-msg-num t)))
     (t                                 ; We have a subject sequence.
      (message "Marked %d messages for deletion" count)
      (mh-delete-msg 'subject)))))

;;;###mh-autoload
(defun mh-delete-subject-or-thread ()
  "Delete messages with same subject or thread\\<mh-folder-mode-map>.

To delete messages faster, you can use this command to delete all
the messages with the same subject as the current message. This
command puts these messages in a sequence named \"subject\". You
can undo this action by using \\[mh-undo] with a prefix argument
and then specifying the \"subject\" sequence.

However, if the buffer is displaying a threaded view of the
folder then this command behaves like \\[mh-thread-delete]."
  (interactive)
  (if (memq 'unthread mh-view-ops)
      (mh-thread-delete)
    (mh-delete-subject)))

;;;###mh-autoload
(defun mh-narrow-to-cc (&optional pick-expr)
  "Limit to messages with the same \"Cc:\" field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr
          (mh-quote-pick-expr (mh-current-message-header-field 'cc)))))
  (mh-narrow-to-header-field 'cc pick-expr))

;;;###mh-autoload
(defun mh-narrow-to-from (&optional pick-expr)
  "Limit to messages with the same \"From:\" field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr
          (mh-quote-pick-expr (mh-current-message-header-field 'from)))))
  (mh-narrow-to-header-field 'from pick-expr))

;;;###mh-autoload
(defun mh-narrow-to-range (range)
  "Limit to RANGE.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive (list (mh-interactive-range "Narrow to")))
  (when (assoc 'range mh-seq-list) (mh-delete-seq 'range))
  (mh-add-msgs-to-seq (mh-range-to-msg-list range) 'range)
  (mh-narrow-to-seq 'range))

;;;###mh-autoload
(defun mh-narrow-to-subject (&optional pick-expr)
  "Limit to messages with same subject.
With a prefix argument, edit PICK-EXPR.
The string Re: is removed from the search.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr
          (mh-quote-pick-expr (mh-current-message-header-field 'subject)))))
  (setq pick-expr
        (let ((case-fold-search t))
          (loop for s in pick-expr
                collect (mh-replace-regexp-in-string "re: *" "" s))))
  (mh-narrow-to-header-field 'subject pick-expr))

;;;###mh-autoload
(defun mh-narrow-to-to (&optional pick-expr)
  "Limit to messages with the same \"To:\" field.
With a prefix argument, edit PICK-EXPR.

Use \\<mh-folder-mode-map>\\[mh-widen] to undo this command."
  (interactive
   (list (mh-edit-pick-expr
          (mh-quote-pick-expr (mh-current-message-header-field 'to)))))
  (mh-narrow-to-header-field 'to pick-expr))



;;; Support Routines

(defun mh-subject-to-sequence (all)
  "Put all following messages with same subject in sequence 'subject.
If arg ALL is t, move to beginning of folder buffer to collect all
messages.
If arg ALL is nil, collect only messages fron current one on forward.

Return number of messages put in the sequence:

 nil -> there was no subject line.

 0   -> there were no later messages with the same
        subject (sequence not made)

 >1  -> the total number of messages including current one."
  (if (memq 'unthread mh-view-ops)
      (mh-subject-to-sequence-threaded all)
    (mh-subject-to-sequence-unthreaded all)))

(defun mh-subject-to-sequence-threaded (all)
  "Put all messages with the same subject in the 'subject sequence.

This function works when the folder is threaded. In this
situation the subject could get truncated and so the normal
matching doesn't work.

The parameter ALL is non-nil then all the messages in the buffer
are considered, otherwise only the messages after the current one
are taken into account."
  (let* ((cur (mh-get-msg-num nil))
         (subject (mh-thread-find-msg-subject cur))
         region msgs)
    (if (null subject)
        (and (message "No subject line") nil)
      (setq region (cons (if all (point-min) (point)) (point-max)))
      (mh-iterate-on-range msg region
        (when (eq (mh-thread-find-msg-subject msg) subject)
          (push msg msgs)))
      (setq msgs (sort msgs #'mh-lessp))
      (if (null msgs)
          0
        (when (assoc 'subject mh-seq-list)
          (mh-delete-seq 'subject))
        (mh-add-msgs-to-seq msgs 'subject)
        (length msgs)))))

(defvar mh-limit-max-subject-size 41
  "Maximum size of the subject part.
It would be desirable to avoid hard-coding this.")

(defun mh-subject-to-sequence-unthreaded (all)
  "Put all following messages with same subject in sequence 'subject.

This function only works with an unthreaded folder. If arg ALL is
t, move to beginning of folder buffer to collect all messages. If
arg ALL is nil, collect only messages fron current one on
forward.

Return number of messages put in the sequence:

 nil -> there was no subject line.
 0   -> there were no later messages with the same
        subject (sequence not made)
 >1  -> the total number of messages including current one."
  (if (not (eq major-mode 'mh-folder-mode))
      (error "Not in a folder buffer"))
  (save-excursion
    (beginning-of-line)
    (if (or (not (looking-at mh-scan-subject-regexp))
            (not (match-string 3))
            (string-equal "" (match-string 3)))
        (progn (message "No subject line")
               nil)
      (let ((subject (mh-match-string-no-properties 3))
            (list))
        (if (> (length subject) mh-limit-max-subject-size)
            (setq subject (substring subject 0 mh-limit-max-subject-size)))
        (save-excursion
          (if all
              (goto-char (point-min)))
          (while (re-search-forward mh-scan-subject-regexp nil t)
            (let ((this-subject (mh-match-string-no-properties 3)))
              (if (> (length this-subject) mh-limit-max-subject-size)
                  (setq this-subject (substring this-subject
                                                0 mh-limit-max-subject-size)))
              (if (string-equal this-subject subject)
                  (setq list (cons (mh-get-msg-num t) list))))))
        (cond
         (list
          ;; If we created a new sequence, add the initial message to it too.
          (if (not (member (mh-get-msg-num t) list))
              (setq list (cons (mh-get-msg-num t) list)))
          (if (assoc 'subject mh-seq-list) (mh-delete-seq 'subject))
          ;; sort the result into a sequence
          (let ((sorted-list (sort (copy-sequence list) 'mh-lessp)))
            (while sorted-list
              (mh-add-msgs-to-seq (car sorted-list) 'subject nil)
              (setq sorted-list (cdr sorted-list)))
            (safe-length list)))
         (t
          0))))))

(defun mh-edit-pick-expr (default)
  "With prefix arg edit a pick expression.
If no prefix arg is given, then return DEFAULT."
  (let ((default-string (loop for x in default concat (format " %s" x))))
    (if (or current-prefix-arg (equal default-string ""))
        (mh-pick-args-list (read-string "Pick expression: "
                                        default-string))
      default)))

(defun mh-pick-args-list (s)
  "Form list by grouping elements in string S suitable for pick arguments.
For example, the string \"-subject a b c -from Joe User
<user@domain.com>\" is converted to (\"-subject\" \"a b c\"
\"-from\" \"Joe User <user@domain.com>\""
  (let ((full-list (split-string s))
        current-arg collection arg-list)
    (while full-list
      (setq current-arg (car full-list))
      (if (null (string-match "^-" current-arg))
          (setq collection
                (if (null collection)
                    current-arg
                  (format "%s %s" collection current-arg)))
        (when collection
          (setq arg-list (append arg-list (list collection)))
          (setq collection nil))
        (setq arg-list (append arg-list (list current-arg))))
      (setq full-list (cdr full-list)))
    (when collection
      (setq arg-list (append arg-list (list collection))))
    arg-list))

(defun mh-current-message-header-field (header-field)
  "Return a pick regexp to match HEADER-FIELD of the message at point."
  (let ((num (mh-get-msg-num nil)))
    (when num
      (let ((folder mh-current-folder))
        (with-temp-buffer
          (insert-file-contents-literally (mh-msg-filename num folder))
          (goto-char (point-min))
          (when (search-forward "\n\n" nil t)
            (narrow-to-region (point-min) (point)))
          (let* ((field (or (message-fetch-field (format "%s" header-field))
                            ""))
                 (field-option (format "-%s" header-field))
                 (patterns (loop for x in (split-string  field "[ ]*,[ ]*")
                                 unless (equal x "")
                                 collect (if (string-match "<\\(.*@.*\\)>" x)
                                             (match-string 1 x)
                                           x))))
            (when patterns
              (loop with accum = `(,field-option ,(car patterns))
                    for e in (cdr patterns)
                    do (setq accum `(,field-option ,e "-or" ,@accum))
                    finally return accum))))))))

(defun mh-narrow-to-header-field (header-field pick-expr)
  "Limit to messages whose HEADER-FIELD match PICK-EXPR.
The MH command pick is used to do the match."
  (let ((folder mh-current-folder)
        (original (mh-coalesce-msg-list
                   (mh-range-to-msg-list (cons (point-min) (point-max)))))
        (msg-list ()))
    (with-temp-buffer
      (apply #'mh-exec-cmd-output "pick" nil folder
             (append original (list "-list") pick-expr))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((num (ignore-errors
                     (string-to-number
                      (buffer-substring (point) (mh-line-end-position))))))
          (when num (push num msg-list))
          (forward-line))))
    (if (null msg-list)
        (message "No matches")
      (when (assoc 'header mh-seq-list) (mh-delete-seq 'header))
      (mh-add-msgs-to-seq msg-list 'header)
      (mh-narrow-to-seq 'header))))

(provide 'mh-limit)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-limit.el ends here
