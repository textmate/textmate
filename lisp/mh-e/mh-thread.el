;;; mh-thread.el --- MH-E threading support

;; Copyright (C) 2002-2004, 2006-2012  Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
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

;; The threading portion of this files tries to implement the
;; algorithm described at:
;;   http://www.jwz.org/doc/threading.html
;; It also begins to implement the IMAP Threading extension RFC. The
;; implementation lacks the reference and subject canonicalization of
;; the RFC.

;; In the presentation buffer, children messages are shown indented
;; with either [ ] or < > around them. Square brackets ([ ]) denote
;; that the algorithm can point out some headers which when taken
;; together implies that the unindented message is an ancestor of the
;; indented message. If no such proof exists then angles (< >) are
;; used.

;; If threading is slow on your machine, compile this file. Of all the
;; files in MH-E, this one really benefits from compilation.

;; Some issues and problems are as follows:

;;  (1) Scan truncates the fields at length 512. So longer
;;      references: headers get mutilated. The same kind of MH
;;      format string works when composing messages. Is there a way
;;      to avoid this? My scan command is as follows:
;;        scan +folder -width 10000 \
;;             -format "%(msg)\n%{message-id}\n%{references}\n%{subject}\n"
;;      I would really appreciate it if someone would help me with this.

;;  (2) Implement heuristics to recognize message identifiers in
;;      In-Reply-To: header. Right now it just assumes that the last
;;      text between angles (< and >) is the message identifier.
;;      There is the chance that this will incorrectly use an email
;;      address like a message identifier.

;;  (3) Error checking of found message identifiers should be done.

;;  (4) Since this breaks the assumption that message indices
;;      increase as one goes down the buffer, the binary search
;;      based mh-goto-msg doesn't work. I have a simpler replacement
;;      which may be less efficient.

;;  (5) Better canonicalizing for message identifier and subject
;;      strings.

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'mh-scan)

(mh-defstruct (mh-thread-message (:conc-name mh-message-)
                                 (:constructor mh-thread-make-message))
  (id nil)
  (references ())
  (subject "")
  (subject-re-p nil))

(mh-defstruct (mh-thread-container (:conc-name mh-container-)
                                   (:constructor mh-thread-make-container))
  message parent children
  (real-child-p t))

(defvar mh-thread-id-hash nil
  "Hashtable used to canonicalize message identifiers.")
(make-variable-buffer-local 'mh-thread-id-hash)

(defvar mh-thread-subject-hash nil
  "Hashtable used to canonicalize subject strings.")
(make-variable-buffer-local 'mh-thread-subject-hash)

(defvar mh-thread-id-table nil
  "Thread ID table maps from message identifiers to message containers.")
(make-variable-buffer-local 'mh-thread-id-table)

(defvar mh-thread-index-id-map nil
  "Table to look up message identifier from message index.")
(make-variable-buffer-local 'mh-thread-index-id-map)

(defvar mh-thread-id-index-map nil
  "Table to look up message index number from message identifier.")
(make-variable-buffer-local 'mh-thread-id-index-map)

(defvar mh-thread-subject-container-hash nil
  "Hashtable used to group messages by subject.")
(make-variable-buffer-local 'mh-thread-subject-container-hash)

(defvar mh-thread-duplicates nil
  "Hashtable used to associate messages with the same message identifier.")
(make-variable-buffer-local 'mh-thread-duplicates)

(defvar mh-thread-history ()
  "Variable to remember the transformations to the thread tree.
When new messages are added, these transformations are rewound,
then the links are added from the newly seen messages. Finally
the transformations are redone to get the new thread tree. This
makes incremental threading easier.")
(make-variable-buffer-local 'mh-thread-history)

(defvar mh-thread-body-width nil
  "Width of scan substring that contains subject and body of message.")



;;; MH-Folder Commands

;;;###mh-autoload
(defun mh-thread-ancestor (&optional thread-root-flag)
  "Display ancestor of current message.

If you do not care for the way a particular thread has turned,
you can move up the chain of messages with this command. This
command can also take a prefix argument THREAD-ROOT-FLAG to jump
to the message that started everything."
  (interactive "P")
  (beginning-of-line)
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point")))
  (let ((current-level (mh-thread-current-indentation-level)))
    (cond (thread-root-flag
           (while (mh-thread-immediate-ancestor))
           (mh-maybe-show))
          ((equal current-level 1)
           (message "Message has no ancestor"))
          (t (mh-thread-immediate-ancestor)
             (mh-maybe-show)))))

;;;###mh-autoload
(defun mh-thread-delete ()
  "Delete thread."
  (interactive)
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point"))
        (t (let ((region (mh-thread-find-children)))
             (mh-iterate-on-messages-in-region () (car region) (cadr region)
               (mh-delete-a-msg nil))
             (mh-next-msg)))))

;;;###mh-autoload
(defun mh-thread-next-sibling (&optional previous-flag)
  "Display next sibling.

With non-nil optional argument PREVIOUS-FLAG jump to the previous
sibling."
  (interactive)
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point")))
  (beginning-of-line)
  (let ((point (point))
        (done nil)
        (my-level (mh-thread-current-indentation-level)))
    (while (and (not done)
                (equal (forward-line (if previous-flag -1 1)) 0)
                (not (eobp)))
      (let ((level (mh-thread-current-indentation-level)))
        (cond ((equal level my-level)
               (setq done 'success))
              ((< level my-level)
               (message "No %s sibling" (if previous-flag "previous" "next"))
               (setq done 'failure)))))
    (cond ((eq done 'success) (mh-maybe-show))
          ((eq done 'failure) (goto-char point))
          (t (message "No %s sibling" (if previous-flag "previous" "next"))
             (goto-char point)))))

;;;###mh-autoload
(defun mh-thread-previous-sibling ()
  "Display previous sibling."
  (interactive)
  (mh-thread-next-sibling t))

;;;###mh-autoload
(defun mh-thread-refile (folder)
  "Refile (output) thread into FOLDER."
  (interactive (list (intern (mh-prompt-for-refile-folder))))
  (cond ((not (memq 'unthread mh-view-ops))
         (error "Folder isn't threaded"))
        ((eobp)
         (error "No message at point"))
        (t (let ((region (mh-thread-find-children)))
             (mh-iterate-on-messages-in-region () (car region) (cadr region)
               (mh-refile-a-msg nil folder))
             (mh-next-msg)))))

;;;###mh-autoload
(defun mh-toggle-threads ()
  "Toggle threaded view of folder."
  (interactive)
  (let ((msg-at-point (mh-get-msg-num nil))
        (old-buffer-modified-flag (buffer-modified-p))
        (buffer-read-only nil))
    (cond ((memq 'unthread mh-view-ops)
           (unless (mh-valid-view-change-operation-p 'unthread)
             (error "Can't unthread folder"))
           (let ((msg-list ()))
             (goto-char (point-min))
             (while (not (eobp))
               (let ((index (mh-get-msg-num nil)))
                 (when index
                   (push index msg-list)))
               (forward-line))
             (mh-scan-folder mh-current-folder
                             (mapcar #'(lambda (x) (format "%s" x))
                                     (mh-coalesce-msg-list msg-list))
                             t))
           (when mh-index-data
             (mh-index-insert-folder-headers)
             (mh-notate-cur)))
          (t (mh-thread-folder)
             (push 'unthread mh-view-ops)))
    (when msg-at-point (mh-goto-msg msg-at-point t t))
    (set-buffer-modified-p old-buffer-modified-flag)
    (mh-recenter nil)))



;;; Support Routines

(defun mh-thread-current-indentation-level ()
  "Find the number of spaces by which current message is indented."
  (save-excursion
    (let ((address-start-offset (+ mh-cmd-note mh-scan-date-flag-width
                                   mh-scan-date-width 1))
          (level 0))
      (beginning-of-line)
      (forward-char address-start-offset)
      (while (char-equal (char-after) ? )
        (incf level)
        (forward-char))
      level)))

(defun mh-thread-immediate-ancestor ()
  "Jump to immediate ancestor in thread tree."
  (beginning-of-line)
  (let ((point (point))
        (ancestor-level (- (mh-thread-current-indentation-level) 2))
        (done nil))
    (if (< ancestor-level 0)
        nil
      (while (and (not done) (equal (forward-line -1) 0))
        (when (equal ancestor-level (mh-thread-current-indentation-level))
          (setq done t)))
      (unless done
        (goto-char point))
      done)))

(defun mh-thread-find-children ()
  "Return a region containing the current message and its children.
The result is returned as a list of two elements. The first is
the point at the start of the region and the second is the point
at the end."
  (beginning-of-line)
  (if (eobp)
      nil
    (let ((address-start-offset (+ mh-cmd-note mh-scan-date-flag-width
                                   mh-scan-date-width 1))
          (level (mh-thread-current-indentation-level))
          spaces begin)
      (setq begin (point))
      (setq spaces (format (format "%%%ss" (1+ level)) ""))
      (forward-line)
      (block nil
        (while (not (eobp))
          (forward-char address-start-offset)
          (unless (equal (string-match spaces (buffer-substring-no-properties
                                               (point) (mh-line-end-position)))
                         0)
            (beginning-of-line)
            (backward-char)
            (return))
          (forward-line)))
      (list begin (point)))))



;;; Thread Creation

(defun mh-thread-folder ()
  "Generate thread view of folder."
  (message "Threading %s..." (buffer-name))
  (mh-thread-initialize)
  (goto-char (point-min))
  (mh-remove-all-notation)
  (let ((msg-list ()))
    (mh-iterate-on-range msg (cons (point-min) (point-max))
      (setf (gethash msg mh-thread-scan-line-map) (mh-thread-parse-scan-line))
      (push msg msg-list))
    (let* ((range (mh-coalesce-msg-list msg-list))
           (thread-tree (mh-thread-generate (buffer-name) range)))
      (delete-region (point-min) (point-max))
      (mh-thread-print-scan-lines thread-tree)
      (mh-notate-user-sequences)
      (mh-notate-deleted-and-refiled)
      (mh-notate-cur)
      (message "Threading %s...done" (buffer-name)))))

;;;###mh-autoload
(defun mh-thread-inc (folder start-point)
  "Update thread tree for FOLDER.
All messages after START-POINT are added to the thread tree."
  (mh-thread-rewind-pruning)
  (mh-remove-all-notation)
  (goto-char start-point)
  (let ((msg-list ()))
    (while (not (eobp))
      (let ((index (mh-get-msg-num nil)))
        (when (numberp index)
          (push index msg-list)
          (setf (gethash index mh-thread-scan-line-map)
                (mh-thread-parse-scan-line)))
        (forward-line)))
    (let ((thread-tree (mh-thread-generate folder msg-list))
          (buffer-read-only nil)
          (old-buffer-modified-flag (buffer-modified-p)))
      (delete-region (point-min) (point-max))
      (mh-thread-print-scan-lines thread-tree)
      (mh-notate-user-sequences)
      (mh-notate-deleted-and-refiled)
      (mh-notate-cur)
      (set-buffer-modified-p old-buffer-modified-flag))))

(defmacro mh-thread-initialize-hash (var test)
  "Initialize the hash table in VAR.
TEST is the test to use when creating a new hash table."
  (unless (symbolp var) (error "Expected a symbol: %s" var))
  `(if ,var (clrhash ,var) (setq ,var (make-hash-table :test ,test))))

(defun mh-thread-initialize ()
  "Make new hash tables, or clear them if already present."
  (mh-thread-initialize-hash mh-thread-id-hash #'equal)
  (mh-thread-initialize-hash mh-thread-subject-hash #'equal)
  (mh-thread-initialize-hash mh-thread-id-table #'eq)
  (mh-thread-initialize-hash mh-thread-id-index-map #'eq)
  (mh-thread-initialize-hash mh-thread-index-id-map #'eql)
  (mh-thread-initialize-hash mh-thread-scan-line-map #'eql)
  (mh-thread-initialize-hash mh-thread-subject-container-hash #'eq)
  (mh-thread-initialize-hash mh-thread-duplicates #'eq)
  (setq mh-thread-history ()))

(defsubst mh-thread-id-container (id)
  "Given ID, return the corresponding container in `mh-thread-id-table'.
If no container exists then a suitable container is created and
the id-table is updated."
  (when (not id)
    (error "1"))
  (or (gethash id mh-thread-id-table)
      (setf (gethash id mh-thread-id-table)
            (let ((message (mh-thread-make-message :id id)))
              (mh-thread-make-container :message message)))))

(defsubst mh-thread-remove-parent-link (child)
  "Remove parent link of CHILD if it exists."
  (let* ((child-container (if (mh-thread-container-p child)
                              child (mh-thread-id-container child)))
         (parent-container (mh-container-parent child-container)))
    (when parent-container
      (setf (mh-container-children parent-container)
            (loop for elem in (mh-container-children parent-container)
                  unless (eq child-container elem) collect elem))
      (setf (mh-container-parent child-container) nil))))

(defsubst mh-thread-add-link (parent child &optional at-end-p)
  "Add links so that PARENT becomes a parent of CHILD.
Doesn't make any changes if CHILD is already an ancestor of
PARENT. If optional argument AT-END-P is non-nil, the CHILD is
added to the end of the children list of PARENT."
  (let ((parent-container (cond ((null parent) nil)
                                ((mh-thread-container-p parent) parent)
                                (t (mh-thread-id-container parent))))
        (child-container (if (mh-thread-container-p child)
                             child (mh-thread-id-container child))))
    (when (and parent-container
               (not (mh-thread-ancestor-p child-container parent-container))
               (not (mh-thread-ancestor-p parent-container child-container)))
      (mh-thread-remove-parent-link child-container)
      (cond ((not at-end-p)
             (push child-container (mh-container-children parent-container)))
            ((null (mh-container-children parent-container))
             (push child-container (mh-container-children parent-container)))
            (t (let ((last-child (mh-container-children parent-container)))
                 (while (cdr last-child)
                   (setq last-child (cdr last-child)))
                 (setcdr last-child (cons child-container nil)))))
      (setf (mh-container-parent child-container) parent-container))
    (unless parent-container
      (mh-thread-remove-parent-link child-container))))

(defun mh-thread-rewind-pruning ()
  "Restore the thread tree to its state before pruning."
  (while mh-thread-history
    (let ((action (pop mh-thread-history)))
      (cond ((eq (car action) 'DROP)
             (mh-thread-remove-parent-link (cadr action))
             (mh-thread-add-link (caddr action) (cadr action)))
            ((eq (car action) 'PROMOTE)
             (let ((node (cadr action))
                   (parent (caddr action))
                   (children (cdddr action)))
               (dolist (child children)
                 (mh-thread-remove-parent-link child)
                 (mh-thread-add-link node child))
               (mh-thread-add-link parent node)))
            ((eq (car action) 'SUBJECT)
             (let ((node (cadr action)))
               (mh-thread-remove-parent-link node)
               (setf (mh-container-real-child-p node) t)))))))

(defun mh-thread-ancestor-p (ancestor successor)
  "Return t if ANCESTOR is really an ancestor of SUCCESSOR and nil otherwise.
In the limit, the function returns t if ANCESTOR and SUCCESSOR
are the same containers."
  (block nil
    (while successor
      (when (eq ancestor successor) (return t))
      (setq successor (mh-container-parent successor)))
    nil))

;; Another and may be better approach would be to generate all the info from
;; the scan which generates the threading info. For now this will have to do.
;;;###mh-autoload
(defun mh-thread-parse-scan-line (&optional string)
  "Parse a scan line.
If optional argument STRING is given then that is assumed to be
the scan line. Otherwise uses the line at point as the scan line
to parse."
  (let* ((string (or string (buffer-substring-no-properties
                             (mh-line-beginning-position)
                             (mh-line-end-position))))
         (address-start (+ mh-cmd-note mh-scan-field-from-start-offset))
         (body-start (+ mh-cmd-note mh-scan-field-from-end-offset))
         (first-string (substring string 0 address-start)))
    (list first-string
          (substring string address-start (- body-start 2))
          (substring string body-start)
          string)))

(defsubst mh-thread-canonicalize-id (id)
  "Produce canonical string representation for ID.
This allows cheap string comparison with EQ."
  (or (and (equal id "") (copy-sequence ""))
      (gethash id mh-thread-id-hash)
      (setf (gethash id mh-thread-id-hash) id)))

(defsubst mh-thread-prune-subject (subject)
  "Prune leading Re:'s, Fwd:'s etc. and trailing (fwd)'s from SUBJECT.
If the result after pruning is not the empty string then it is
canonicalized so that subjects can be tested for equality with
eq. This is done so that all the messages without a subject are
not put into a single thread."
  (let ((case-fold-search t)
        (subject-pruned-flag nil))
    ;; Prune subject leader
    (while (or (string-match "^[ \t]*\\(re\\|fwd?\\)\\(\\[[0-9]*\\]\\)?:[ \t]*"
                             subject)
               (string-match "^[ \t]*\\[[^\\]][ \t]*" subject))
      (setq subject-pruned-flag t)
      (setq subject (substring subject (match-end 0))))
    ;; Prune subject trailer
    (while (or (string-match "(fwd)$" subject)
               (string-match "[ \t]+$" subject))
      (setq subject-pruned-flag t)
      (setq subject (substring subject 0 (match-beginning 0))))
    ;; Canonicalize subject only if it is non-empty
    (cond ((equal subject "") (list subject subject-pruned-flag))
          (t (list
              (or (gethash subject mh-thread-subject-hash)
                  (setf (gethash subject mh-thread-subject-hash) subject))
              subject-pruned-flag)))))

(defsubst mh-thread-group-by-subject (roots)
  "Group the set of message containers, ROOTS based on subject.
Bug: Check for and make sure that something without Re: is made
the parent in preference to something that has it."
  (clrhash mh-thread-subject-container-hash)
  (let ((results ()))
    (dolist (root roots)
      (let* ((subject (mh-thread-container-subject root))
             (parent (gethash subject mh-thread-subject-container-hash)))
        (cond (parent (mh-thread-remove-parent-link root)
                      (mh-thread-add-link parent root t)
                      (setf (mh-container-real-child-p root) nil)
                      (push `(SUBJECT ,root) mh-thread-history))
              (t
               (setf (gethash subject mh-thread-subject-container-hash) root)
               (push root results)))))
    (nreverse results)))

(defun mh-thread-container-subject (container)
  "Return the subject of CONTAINER.
If CONTAINER is empty return the subject info of one of its
children."
  (cond ((and (mh-container-message container)
              (mh-message-id (mh-container-message container)))
         (mh-message-subject (mh-container-message container)))
        (t (block nil
             (dolist (kid (mh-container-children container))
               (when (and (mh-container-message kid)
                          (mh-message-id (mh-container-message kid)))
                 (let ((kid-message (mh-container-message kid)))
                   (return (mh-message-subject kid-message)))))
             (error "This can't happen")))))

(defsubst mh-thread-update-id-index-maps (id index)
  "Message with id, ID is the message in INDEX.
The function also checks for duplicate messages (that is multiple
messages with the same ID). These messages are put in the
`mh-thread-duplicates' hash table."
  (let ((old-index (gethash id mh-thread-id-index-map)))
    (when old-index (push old-index (gethash id mh-thread-duplicates)))
    (setf (gethash id mh-thread-id-index-map) index)
    (setf (gethash index mh-thread-index-id-map) id)))

(defsubst mh-thread-get-message-container (message)
  "Return container which has MESSAGE in it.
If there is no container present then a new container is
allocated."
  (let* ((id (mh-message-id message))
         (container (gethash id mh-thread-id-table)))
    (cond (container (setf (mh-container-message container) message)
                     container)
          (t (setf (gethash id mh-thread-id-table)
                   (mh-thread-make-container :message message))))))

(defsubst mh-thread-get-message (id subject-re-p subject refs)
  "Return appropriate message.
Otherwise update message already present to have the proper ID,
SUBJECT-RE-P, SUBJECT and REFS fields."
  (let* ((container (gethash id mh-thread-id-table))
         (message (if container (mh-container-message container) nil)))
    (cond (message
           (setf (mh-message-subject-re-p message) subject-re-p)
           (setf (mh-message-subject message) subject)
           (setf (mh-message-id message) id)
           (setf (mh-message-references message) refs)
           message)
          (container
           (setf (mh-container-message container)
                 (mh-thread-make-message :id id :references refs
                                         :subject subject
                                         :subject-re-p subject-re-p)))
          (t (let ((message (mh-thread-make-message :id id :references refs
                                                    :subject-re-p subject-re-p
                                                    :subject subject)))
               (prog1 message
                 (mh-thread-get-message-container message)))))))

(defvar mh-message-id-regexp "^<.*@.*>$"
  "Regexp to recognize whether a string is a message identifier.")

;;;###mh-autoload
(defun mh-thread-generate (folder msg-list)
  "Scan FOLDER to get info for threading.
Only information about messages in MSG-LIST are added to the tree."
  (with-temp-buffer
    (mh-thread-set-tables folder)
    (when msg-list
      (apply
       #'call-process (expand-file-name mh-scan-prog mh-progs) nil '(t nil) nil
       "-width" "10000" "-format"
       "%(msg)\n%{message-id}\n%{references}\n%{in-reply-to}\n%{subject}\n"
       folder (mapcar #'(lambda (x) (format "%s" x)) msg-list)))
    (goto-char (point-min))
    (let ((roots ())
          (case-fold-search t))
      (block nil
        (while (not (eobp))
          (block process-message
            (let* ((index-line
                    (prog1 (buffer-substring (point) (mh-line-end-position))
                      (forward-line)))
                   (index (string-to-number index-line))
                   (id (prog1 (buffer-substring (point) (mh-line-end-position))
                         (forward-line)))
                   (refs (prog1
                             (buffer-substring (point) (mh-line-end-position))
                           (forward-line)))
                   (in-reply-to (prog1 (buffer-substring (point)
                                                         (mh-line-end-position))
                                  (forward-line)))
                   (subject (prog1
                                (buffer-substring
                                 (point) (mh-line-end-position))
                              (forward-line)))
                   (subject-re-p nil))
              (unless (gethash index mh-thread-scan-line-map)
                (return-from process-message))
              (unless (integerp index) (return)) ;Error message here
              (multiple-value-setq (subject subject-re-p)
                (values-list (mh-thread-prune-subject subject)))
              (setq in-reply-to (mh-thread-process-in-reply-to in-reply-to))
              (setq refs (loop for x in (append (split-string refs) in-reply-to)
                               when (string-match mh-message-id-regexp x)
                               collect x))
              (setq id (mh-thread-canonicalize-id id))
              (mh-thread-update-id-index-maps id index)
              (setq refs (mapcar #'mh-thread-canonicalize-id refs))
              (mh-thread-get-message id subject-re-p subject refs)
              (do ((ancestors refs (cdr ancestors)))
                  ((null (cdr ancestors))
                   (when (car ancestors)
                     (mh-thread-remove-parent-link id)
                     (mh-thread-add-link (car ancestors) id)))
                (mh-thread-add-link (car ancestors) (cadr ancestors)))))))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (when (null (mh-container-parent v))
                     (push v roots)))
               mh-thread-id-table)
      (setq roots (mh-thread-prune-containers roots))
      (prog1 (setq roots (mh-thread-group-by-subject roots))
        (let ((history mh-thread-history))
          (set-buffer folder)
          (setq mh-thread-history history))))))

(defun mh-thread-set-tables (folder)
  "Use the tables of FOLDER in current buffer."
  (flet ((mh-get-table (symbol)
                       (with-current-buffer folder
                         (symbol-value symbol))))
    (setq mh-thread-id-hash (mh-get-table 'mh-thread-id-hash))
    (setq mh-thread-subject-hash (mh-get-table 'mh-thread-subject-hash))
    (setq mh-thread-id-table (mh-get-table 'mh-thread-id-table))
    (setq mh-thread-id-index-map (mh-get-table 'mh-thread-id-index-map))
    (setq mh-thread-index-id-map (mh-get-table 'mh-thread-index-id-map))
    (setq mh-thread-scan-line-map (mh-get-table 'mh-thread-scan-line-map))
    (setq mh-thread-subject-container-hash
          (mh-get-table 'mh-thread-subject-container-hash))
    (setq mh-thread-duplicates (mh-get-table 'mh-thread-duplicates))
    (setq mh-thread-history (mh-get-table 'mh-thread-history))))

(defun mh-thread-process-in-reply-to (reply-to-header)
  "Extract message id's from REPLY-TO-HEADER.
Ideally this should have some regexp which will try to guess if a
string between < and > is a message id and not an email address.
For now it will take the last string inside angles."
  (let ((end (mh-search-from-end ?> reply-to-header)))
    (when (numberp end)
      (let ((begin (mh-search-from-end ?< (substring reply-to-header 0 end))))
        (when (numberp begin)
          (list (substring reply-to-header begin (1+ end))))))))

(defun mh-thread-prune-containers (roots)
  "Prune empty containers in the containers ROOTS."
  (let ((dfs-ordered-nodes ())
        (work-list roots))
    (while work-list
      (let ((node (pop work-list)))
        (dolist (child (mh-container-children node))
          (push child work-list))
        (push node dfs-ordered-nodes)))
    (while dfs-ordered-nodes
      (let ((node (pop dfs-ordered-nodes)))
        (cond ((gethash (mh-message-id (mh-container-message node))
                        mh-thread-id-index-map)
               ;; Keep it
               (setf (mh-container-children node)
                     (mh-thread-sort-containers (mh-container-children node))))
              ((and (mh-container-children node)
                    (or (null (cdr (mh-container-children node)))
                        (mh-container-parent node)))
               ;; Promote kids
               (let ((children ()))
                 (dolist (kid (mh-container-children node))
                   (mh-thread-remove-parent-link kid)
                   (mh-thread-add-link (mh-container-parent node) kid)
                   (push kid children))
                 (push `(PROMOTE ,node ,(mh-container-parent node) ,@children)
                       mh-thread-history)
                 (mh-thread-remove-parent-link node)))
              ((mh-container-children node)
               ;; Promote the first orphan to parent and add the other kids as
               ;; his children
               (setf (mh-container-children node)
                     (mh-thread-sort-containers (mh-container-children node)))
               (let ((new-parent (car (mh-container-children node)))
                     (other-kids (cdr (mh-container-children node))))
                 (mh-thread-remove-parent-link new-parent)
                 (dolist (kid other-kids)
                   (mh-thread-remove-parent-link kid)
                   (setf (mh-container-real-child-p kid) nil)
                   (mh-thread-add-link new-parent kid t))
                 (push `(PROMOTE ,node ,(mh-container-parent node)
                                 ,new-parent ,@other-kids)
                       mh-thread-history)
                 (mh-thread-remove-parent-link node)))
              (t
               ;; Drop it
               (push `(DROP ,node ,(mh-container-parent node))
                     mh-thread-history)
               (mh-thread-remove-parent-link node)))))
    (let ((results ()))
      (maphash #'(lambda (k v)
                   (declare (ignore k))
                   (when (and (null (mh-container-parent v))
                              (gethash (mh-message-id (mh-container-message v))
                                       mh-thread-id-index-map))
                     (push v results)))
               mh-thread-id-table)
      (mh-thread-sort-containers results))))

(defun mh-thread-sort-containers (containers)
  "Sort a list of message CONTAINERS to be in ascending order wrt index."
  (sort containers
        #'(lambda (x y)
            (when (and (mh-container-message x) (mh-container-message y))
              (let* ((id-x (mh-message-id (mh-container-message x)))
                     (id-y (mh-message-id (mh-container-message y)))
                     (index-x (gethash id-x mh-thread-id-index-map))
                     (index-y (gethash id-y mh-thread-id-index-map)))
                (and (integerp index-x) (integerp index-y)
                     (< index-x index-y)))))))

(defvar mh-thread-last-ancestor)

;;;###mh-autoload
(defun mh-thread-print-scan-lines (thread-tree)
  "Print scan lines in THREAD-TREE in threaded mode."
  (let ((mh-thread-body-width (- (window-width) mh-cmd-note
                                 (1- mh-scan-field-subject-start-offset)))
        (mh-thread-last-ancestor nil))
    (if (null mh-index-data)
        (mh-thread-generate-scan-lines thread-tree -2)
      (loop for x in (mh-index-group-by-folder)
            do (let* ((old-map mh-thread-scan-line-map)
                      (mh-thread-scan-line-map (make-hash-table)))
                 (setq mh-thread-last-ancestor nil)
                 (loop for msg in (cdr x)
                       do (let ((v (gethash msg old-map)))
                            (when v
                              (setf (gethash msg mh-thread-scan-line-map) v))))
                 (when (> (hash-table-count mh-thread-scan-line-map) 0)
                   (insert (if (bobp) "" "\n") (car x) "\n")
                   (mh-thread-generate-scan-lines thread-tree -2))))
      (mh-index-create-imenu-index))))

(defun mh-thread-generate-scan-lines (tree level)
  "Generate scan lines.
TREE is the hierarchical tree of messages, SCAN-LINE-MAP maps
message indices to the corresponding scan lines and LEVEL used to
determine indentation of the message."
  (cond ((null tree) nil)
        ((mh-thread-container-p tree)
         (let* ((message (mh-container-message tree))
                (id (mh-message-id message))
                (index (gethash id mh-thread-id-index-map))
                (duplicates (gethash id mh-thread-duplicates))
                (new-level (+ level 2))
                (dupl-flag t)
                (force-angle-flag nil)
                (increment-level-flag nil))
           (dolist (scan-line (mapcar (lambda (x)
                                        (gethash x mh-thread-scan-line-map))
                                      (reverse (cons index duplicates))))
             (when scan-line
               (when (and dupl-flag (equal level 0)
                          (mh-thread-ancestor-p mh-thread-last-ancestor tree))
                 (setq level (+ level 2)
                       new-level (+ new-level 2)
                       force-angle-flag t))
               (when (equal level 0)
                 (setq mh-thread-last-ancestor tree)
                 (while (mh-container-parent mh-thread-last-ancestor)
                   (setq mh-thread-last-ancestor
                         (mh-container-parent mh-thread-last-ancestor))))
               (let* ((lev (if dupl-flag level new-level))
                      (square-flag (or (and (mh-container-real-child-p tree)
                                            (not force-angle-flag)
                                            dupl-flag)
                                       (equal lev 0))))
                 (insert (car scan-line)
                         (format (format "%%%ss" lev) "")
                         (if square-flag "[" "<")
                         (cadr scan-line)
                         (if square-flag "]" ">")
                         (truncate-string-to-width
                          (caddr scan-line) (- mh-thread-body-width lev))
                         "\n"))
               (setq increment-level-flag t)
               (setq dupl-flag nil)))
           (unless increment-level-flag (setq new-level level))
           (dolist (child (mh-container-children tree))
             (mh-thread-generate-scan-lines child new-level))))
        (t (let ((nlevel (+ level 2)))
             (dolist (ch tree)
               (mh-thread-generate-scan-lines ch nlevel))))))



;;; Additional Utilities

;;;###mh-autoload
(defun mh-thread-update-scan-line-map (msg notation offset)
  "In threaded view update `mh-thread-scan-line-map'.
MSG is the message being notated with NOTATION at OFFSET."
  (let* ((msg (or msg (mh-get-msg-num nil)))
         (cur-scan-line (and mh-thread-scan-line-map
                             (gethash msg mh-thread-scan-line-map)))
         (old-scan-lines (loop for map in mh-thread-scan-line-map-stack
                               collect (and map (gethash msg map)))))
    (when cur-scan-line
      (setf (aref (car cur-scan-line) offset) notation))
    (dolist (line old-scan-lines)
      (when line (setf (aref (car line) offset) notation)))))

;;;###mh-autoload
(defun mh-thread-find-msg-subject (msg)
  "Find canonicalized subject of MSG.
This function can only be used the folder is threaded."
  (ignore-errors
    (mh-message-subject
     (mh-container-message (gethash (gethash msg mh-thread-index-id-map)
                                    mh-thread-id-table)))))

;;;###mh-autoload
(defun mh-thread-add-spaces (count)
  "Add COUNT spaces to each scan line in `mh-thread-scan-line-map'."
  (let ((spaces (format (format "%%%ss" count) "")))
    (while (not (eobp))
      (let* ((msg-num (mh-get-msg-num nil))
             (old-line (nth 3 (gethash msg-num mh-thread-scan-line-map))))
        (when (numberp msg-num)
          (setf (gethash msg-num mh-thread-scan-line-map)
                (mh-thread-parse-scan-line (format "%s%s" spaces old-line)))))
      (forward-line 1))))

;;;###mh-autoload
(defun mh-thread-forget-message (index)
  "Forget the message INDEX from the threading tables."
  (let* ((id (gethash index mh-thread-index-id-map))
         (id-index (gethash id mh-thread-id-index-map))
         (duplicates (gethash id mh-thread-duplicates)))
    (remhash index mh-thread-index-id-map)
    (remhash index mh-thread-scan-line-map)
    (cond ((and (eql index id-index) (null duplicates))
           (remhash id mh-thread-id-index-map))
          ((eql index id-index)
           (setf (gethash id mh-thread-id-index-map) (car duplicates))
           (setf (gethash (car duplicates) mh-thread-index-id-map) id)
           (setf (gethash id mh-thread-duplicates) (cdr duplicates)))
          (t
           (setf (gethash id mh-thread-duplicates)
                 (remove index duplicates))))))

(provide 'mh-thread)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-thread.el ends here
