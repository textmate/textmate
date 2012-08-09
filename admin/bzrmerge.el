;;; bzrmerge.el --- help merge one Emacs bzr branch to another

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: 

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

;; Some usage notes are in admin/notes/bzr.

;;; Code:

(eval-when-compile
  (require 'cl))                        ; assert

(defvar bzrmerge-skip-regexp
  "back[- ]?port\\|merge\\|sync\\|re-?generate\\|bump version"
  "Regexp matching logs of revisions that might be skipped.
`bzrmerge-missing' will ask you if it should skip any matches.")

(defconst bzrmerge-buffer "*bzrmerge*"
  "Working buffer for bzrmerge.")

(defconst bzrmerge-warning-buffer "*bzrmerge warnings*"
  "Buffer where bzrmerge will display any warnings.")

(defun bzrmerge-merges ()
  "Return the list of already merged (not yet committed) revisions.
The list returned is sorted by oldest-first."
  (with-current-buffer (get-buffer-create bzrmerge-buffer)
    (erase-buffer)
    ;; We generally want to make sure we start with a clean tree, but we also
    ;; want to allow restarts (i.e. with some part of FROM already merged but
    ;; not yet committed).
    (call-process "bzr" nil t nil "status" "-v")
    (goto-char (point-min))
    (when (re-search-forward "^conflicts:\n" nil t)
      (error "You still have unresolved conflicts"))
    (let ((merges ()))
      (if (not (re-search-forward "^pending merges:\n" nil t))
          (when (save-excursion
                  (goto-char (point-min))
                  (re-search-forward "^[a-z ]*:\n" nil t))
            (error "You still have uncommitted changes"))
        ;; This is really stupid, but it seems there's no easy way to figure
        ;; out which revisions have been merged already.  The only info I can
        ;; find is the "pending merges" from "bzr status -v", which is not
        ;; very machine-friendly.
        (while (not (eobp))
          (skip-chars-forward " ")
          (push (buffer-substring (point) (line-end-position)) merges)
          (forward-line 1)))
      merges)))

(defun bzrmerge-check-match (merge)
  ;; Make sure the MERGES match the revisions on the FROM branch.
  ;; Stupidly the best form of MERGES I can find is the one from
  ;; "bzr status -v" which is very machine non-friendly, so I have
  ;; to do some fuzzy matching.
  (let ((author
         (or
          (save-excursion
            (if (re-search-forward "^author: *\\([^<]*[^ ]\\) +<.*"
                                   nil t)
                (match-string 1)))
          (save-excursion
            (if (re-search-forward
                 "^committer: *\\([^<]*[^< ]\\) +<" nil t)
                (match-string 1)))))
        (timestamp
         (save-excursion
           (if (re-search-forward
                "^timestamp:[^0-9]*\\([-0-9]+\\)" nil t)
               (match-string 1))))
        (line1
         (save-excursion
           (if (re-search-forward "^message:[ \n]*" nil t)
               (buffer-substring (point) (line-end-position))))))
    ;; The `merge' may have a truncated line1 with "...", so get
    ;; rid of any "..." and then look for a prefix match.
    (when (string-match "\\.+\\'" merge)
      (setq merge (substring merge 0 (match-beginning 0))))
    (or (string-prefix-p
         merge (concat author " " timestamp " " line1))
        (string-prefix-p
         merge (concat author " " timestamp " [merge] " line1)))))

(defun bzrmerge-missing (from merges)
  "Return the list of revisions that need to be merged.
MERGES is the revisions already merged but not yet committed.
Asks about skipping revisions with logs matching `bzrmerge-skip-regexp'.
The result is of the form (TOMERGE . TOSKIP) where TOMERGE and TOSKIP
are both lists of revnos, in oldest-first order."
  (with-current-buffer (get-buffer-create bzrmerge-buffer)
    (erase-buffer)
    (call-process "bzr" nil t nil "missing" "--theirs-only"
                  (expand-file-name from))
    (let ((revnos ()) (skipped ()))
      (pop-to-buffer (current-buffer))
      (goto-char (point-max))
      (while (re-search-backward "^------------------------------------------------------------\nrevno: \\([0-9.]+\\).*" nil t)
        (save-excursion
          (if merges
              (while (not (bzrmerge-check-match (pop merges)))
                (unless merges
                  (error "Unmatched tip of merged revisions")))
            (let ((case-fold-search t)
                  (revno (match-string 1))
                  (skip nil))
              (if (string-match "\\." revno)
                  (error "Unexpected dotted revno!")
                (setq revno (string-to-number revno)))
              (re-search-forward "^message:\n")
              (while (and (not skip)
                          (re-search-forward bzrmerge-skip-regexp nil t))
                (let ((str (buffer-substring (line-beginning-position)
                                             (line-end-position))))
                  (when (string-match "\\` *" str)
                    (setq str (substring str (match-end 0))))
                  (when (string-match "[.!;, ]+\\'" str)
                    (setq str (substring str 0 (match-beginning 0))))
                  (let ((help-form "\
Type `y' to skip this revision,
`N' to include it and go on to the next revision,
`n' to not skip, but continue to search this log entry for skip regexps,
`q' to quit merging."))
                    (case (save-excursion
                            (read-char-choice
                             (format "%s: Skip (y/n/N/q/%s)? " str
                                     (key-description (vector help-char)))
                             '(?y ?n ?N ?q)))
                      (?y (setq skip t))
                      (?q (keyboard-quit))
                      ;; A single log entry can match skip-regexp multiple
                      ;; times.  If you are sure you don't want to skip it,
                      ;; you don't want to be asked multiple times.
                      (?N (setq skip 'no))))))
              (if (eq skip t)
                  (push revno skipped)
                (push revno revnos)))))
        (delete-region (point) (point-max)))
      (and (or revnos skipped)
           (cons (nreverse revnos) (nreverse skipped))))))

(defun bzrmerge-resolve (file)
  (unless (file-exists-p file) (error "Bzrmerge-resolve: Can't find %s" file))
  (with-demoted-errors
    (let ((exists (find-buffer-visiting file)))
      (with-current-buffer (let ((enable-local-variables :safe))
                             (find-file-noselect file))
        (if (buffer-modified-p)
            (error "Unsaved changes in %s" (current-buffer)))
        (save-excursion
          (cond
           ((derived-mode-p 'change-log-mode)
            ;; Fix up dates before resolving the conflicts.
            (goto-char (point-min))
            (let ((diff-auto-refine-mode nil))
              (while (re-search-forward smerge-begin-re nil t)
                (smerge-match-conflict)
                (smerge-ensure-match 3)
                (let ((start1 (match-beginning 1))
                      (end1 (match-end 1))
                      (start3 (match-beginning 3))
                      (end3 (copy-marker (match-end 3) t)))
                  (goto-char start3)
                  (while (re-search-forward change-log-start-entry-re end3 t)
                    (let* ((str (match-string 0))
                           (newstr (save-match-data
                                     (concat (add-log-iso8601-time-string)
                                             (when (string-match " *\\'" str)
                                               (match-string 0 str))))))
                      (replace-match newstr t t)))
                  ;; change-log-resolve-conflict prefers to put match-1's
                  ;; elements first (for equal dates), whereas we want to put
                  ;; match-3's first.
                  (let ((match3 (buffer-substring start3 end3))
                        (match1 (buffer-substring start1 end1)))
                    (delete-region start3 end3)
                    (goto-char start3)
                    (insert match1)
                    (delete-region start1 end1)
                    (goto-char start1)
                    (insert match3)))))
            ;; (pop-to-buffer (current-buffer)) (debug 'before-resolve)
            ))
          ;; Try to resolve the conflicts.
          (cond
           ((member file '("configure" "lisp/ldefs-boot.el"
                           "lisp/emacs-lisp/cl-loaddefs.el"))
            ;; We are in the file's buffer, so names are relative.
            (call-process "bzr" nil t nil "revert"
                          (file-name-nondirectory file))
            (revert-buffer nil 'noconfirm))
           (t
            (goto-char (point-max))
            (while (re-search-backward smerge-begin-re nil t)
              (save-excursion
                (ignore-errors
                  (smerge-match-conflict)
                  (smerge-resolve))))
            ;; (when (derived-mode-p 'change-log-mode)
            ;;   (pop-to-buffer (current-buffer)) (debug 'after-resolve))
            (save-buffer)))
          (goto-char (point-min))
          (prog1 (re-search-forward smerge-begin-re nil t)
            (unless exists (kill-buffer))))))))

(defun bzrmerge-add-metadata (from endrevno)
  "Add the metadata for a merge of FROM upto ENDREVNO.
Does not make other difference."
  (if (with-temp-buffer
        (call-process "bzr" nil t nil "status")
        (goto-char (point-min))
        (re-search-forward "^conflicts:\n" nil t))
      (error "Don't know how to add metadata in the presence of conflicts")
    (call-process "bzr" nil t nil "shelve" "--all"
                  "-m" "Bzrmerge shelved merge during skipping")
    (call-process "bzr" nil t nil "revert")
    (call-process "bzr" nil t nil
                  "merge" "-r" (format "%s" endrevno) from)
    (call-process "bzr" nil t nil "revert" ".")
    (call-process "bzr" nil t nil "unshelve")))

(defvar bzrmerge-already-done nil)

(defun bzrmerge-apply (missing from)
  (setq from (expand-file-name from))
  (with-current-buffer (get-buffer-create bzrmerge-buffer)
    (erase-buffer)
    (when (equal (cdr bzrmerge-already-done) (list from missing))
      (setq missing (car bzrmerge-already-done)))
    (setq bzrmerge-already-done nil)
    (let ((merge (car missing))
          (skip (cdr missing))
          (unsafe nil)
          beg end)
      (when (or merge skip)
        (cond
         ((and skip (or (null merge) (< (car skip) (car merge))))
          ;; Do a "skip" (i.e. merge the meta-data only).
          (setq beg (1- (car skip)))
          (while (and skip (or (null merge) (< (car skip) (car merge))))
            (assert (> (car skip) (or end beg)))
            (setq end (pop skip)))
          (message "Skipping %s..%s" beg end)
          (bzrmerge-add-metadata from end))

         (t
          ;; Do a "normal" merge.
          (assert (or (null skip) (< (car merge) (car skip))))
          (setq beg (1- (car merge)))
          (while (and merge (or (null skip) (< (car merge) (car skip))))
            (assert (> (car merge) (or end beg)))
            (setq end (pop merge)))
          (message "Merging %s..%s" beg end)
          (if (with-temp-buffer
                (call-process "bzr" nil t nil "status")
                (zerop (buffer-size)))
              (call-process "bzr" nil t nil
                            "merge" "-r" (format "%s" end) from)
            ;; Stupidly, "bzr merge --force -r A..B" dos not maintain the
            ;; metadata properly except when the checkout is clean.
            (call-process "bzr" nil t nil "merge"
                          "--force" "-r" (format "%s..%s" beg end) from)
            ;; The merge did not update the metadata, so force the next time
            ;; around to update it (as a "skip").
            (setq unsafe t)
            (push end skip))
          (pop-to-buffer (current-buffer))
          (sit-for 1)
          ;; (debug 'after-merge)
          ;; Check the conflicts.
          ;; FIXME if using the helpful bzr changelog_merge plugin,
          ;; there are normally no conflicts in ChangeLogs.
          ;; But we still want the dates fixing, like bzrmerge-resolve does.
          (let ((conflicted nil)
                (files ()))
            (goto-char (point-min))
            (when (re-search-forward "bzr: ERROR:" nil t)
              (error "Internal Bazaar error!!"))
            (while (re-search-forward "^Text conflict in " nil t)
              (push (buffer-substring (point) (line-end-position)) files))
            (if (re-search-forward "^\\([0-9]+\\) conflicts encountered" nil t)
                (if (/= (length files) (string-to-number (match-string 1)))
                    (setq conflicted t))
              (if files (setq conflicted t)))
            (dolist (file files)
              (if (bzrmerge-resolve file)
                  (setq conflicted t)))
            (when conflicted
              (setq bzrmerge-already-done
                    (list (cons merge skip) from missing))
              (if unsafe
                  ;; FIXME: Obviously, we'd rather make it right rather
                  ;; than output such a warning.  But I don't know how to add
                  ;; the metadata to bzr's since the technique used in
                  ;; bzrmerge-add-metadata does not work when there
                  ;; are conflicts.
                  (display-warning 'bzrmerge "Resolve conflicts manually.
¡BEWARE!  Important metadata is kept in this Emacs session!
Do not commit without re-running `M-x bzrmerge' first!"
                                   :warning bzrmerge-warning-buffer))
              (error "Resolve conflicts manually")))))
        (cons merge skip)))))

(defun bzrmerge (from)
  "Merge from branch FROM into `default-directory'."
  (interactive
   (list
    (let ((def
           (with-temp-buffer
             (call-process "bzr" nil t nil "info")
             (goto-char (point-min))
             (when (re-search-forward "submit branch: *" nil t)
               (buffer-substring (point) (line-end-position))))))
      (read-file-name "From branch: " nil nil nil def))))
  ;; Eg we ran bzrmerge once, it stopped with conflicts, we fixed them
  ;; and are running it again.
  (if (get-buffer bzrmerge-warning-buffer)
      (kill-buffer bzrmerge-warning-buffer))
  (message "Merging from %s..." from)
  (require 'vc-bzr)
  (let ((default-directory (or (vc-bzr-root default-directory)
                               (error "Not in a Bzr tree"))))
    ;; First, check the status.
    (let* ((merges (bzrmerge-merges))
           ;; OK, we have the status, now check the missing data.
           (missing (bzrmerge-missing from merges)))
      (if (not missing)
          (message "Merging from %s...nothing to merge" from)
        (while missing
          (setq missing (bzrmerge-apply missing from)))
        (message "Merging from %s...done" from)))))

(provide 'bzrmerge)
;;; bzrmerge.el ends here
