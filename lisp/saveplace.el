;;; saveplace.el --- automatically save place in files

;; Copyright (C) 1993-1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Maintainer: FSF
;; Created: July, 1993
;; Keywords: bookmarks, placeholders

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

;; Automatically save place in files, so that visiting them later
;; (even during a different Emacs session) automatically moves point
;; to the saved position, when the file is first found.  Uses the
;; value of buffer-local variable save-place to determine whether to
;; save position or not.
;;
;; Thanks to Stefan Schoef, who sent a patch with the
;; `save-place-version-control' stuff in it.

;;; Code:

;; this is what I was using during testing:
;; (define-key ctl-x-map "p" 'toggle-save-place-globally)

(defgroup save-place nil
  "Automatically save place in files."
  :group 'data)


(defvar save-place-alist nil
  "Alist of saved places to go back to when revisiting files.
Each element looks like (FILENAME . POSITION);
visiting file FILENAME goes automatically to position POSITION
rather than the beginning of the buffer.
This alist is saved between Emacs sessions.")

(defcustom save-place nil
  "Non-nil means automatically save place in each file.
This means when you visit a file, point goes to the last place
where it was when you previously visited the same file.
This variable is automatically buffer-local.

If you wish your place in any file to always be automatically saved,
simply put this in your `~/.emacs' file:

\(setq-default save-place t)
\(require 'saveplace)

or else use the Custom facility to set this option."
  :type 'boolean
  :require 'saveplace
  :group 'save-place)

(make-variable-buffer-local 'save-place)

(defcustom save-place-file (convert-standard-filename "~/.emacs-places")
  "Name of the file that records `save-place-alist' value."
  :type 'file
  :group 'save-place)

(defcustom save-place-version-control nil
  "Controls whether to make numbered backups of master save-place file.
It can have four values: t, nil, `never', and `nospecial'.  The first
three have the same meaning that they do for the variable
`version-control', and the final value `nospecial' means just use the
value of `version-control'."
  :type '(radio (const :tag "Unconditionally" t)
		(const :tag "For VC Files" nil)
		(const never)
		(const :tag "Use value of `version-control'" nospecial))
  :group 'save-place)

(defvar save-place-loaded nil
  "Non-nil means that the `save-place-file' has been loaded.")

(defcustom save-place-limit 400
  "Maximum number of entries to retain in the list; nil means no limit."
  :version "24.1"                       ; nil -> 400
  :type '(choice (integer :tag "Entries" :value 1)
		 (const :tag "No Limit" nil))
  :group 'save-place)

(defcustom save-place-forget-unreadable-files t
  "Non-nil means forget place in unreadable files.

The filenames in `save-place-alist' that do not match
`save-place-skip-check-regexp' are filtered through
`file-readable-p'. if nil, their alist entries are removed.

You may do this anytime by calling the complementary function,
`save-place-forget-unreadable-files'.  When this option is turned on,
this happens automatically before saving `save-place-alist' to
`save-place-file'."
  :type 'boolean :group 'save-place)

(defcustom save-place-save-skipped t
  "If non-nil, remember files matching `save-place-skip-check-regexp'.

When filtering `save-place-alist' for unreadable files, some will not
be checked, based on said regexp, and instead saved or forgotten based
on this flag."
  :type 'boolean :group 'save-place)

(defcustom save-place-skip-check-regexp
  ;; thanks to ange-ftp-name-format
  "\\`/\\(?:cdrom\\|floppy\\|mnt\\|\\(?:[^@/:]*@\\)?[^@/:]*[^@/:.]:\\)"
  "Regexp whose file names shall not be checked for readability.

When forgetting unreadable files, file names matching this regular
expression shall not be checked for readability, but instead be
subject to `save-place-save-skipped'.

Files for which such a check may be inconvenient include those on
removable and network volumes."
  :type 'regexp :group 'save-place)

(defcustom save-place-ignore-files-regexp
  "\\(?:COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$"
  "Regexp matching files for which no position should be recorded.
Useful for temporary file such as commit message files that are
automatically created by the VCS.  If set to nil, this feature is
disabled, i.e., the position is recorded for all files."
  :version "24.1"
  :type 'regexp :group 'save-place)

(defun toggle-save-place (&optional parg)
  "Toggle whether to save your place in this file between sessions.
If this mode is enabled, point is recorded when you kill the buffer
or exit Emacs.  Visiting this file again will go to that position,
even in a later Emacs session.

If called with a prefix arg, the mode is enabled if and only if
the argument is positive.

To save places automatically in all files, put this in your `.emacs' file:

\(setq-default save-place t\)"
  (interactive "P")
  (if (not buffer-file-name)
      (message "Buffer `%s' not visiting a file" (buffer-name))
    (if (and save-place (or (not parg) (<= parg 0)))
	(progn
	  (message "No place will be saved in this file")
	  (setq save-place nil))
      (message "Place will be saved")
      (setq save-place t))))

(defun save-place-to-alist ()
  ;; put filename and point in a cons box and then cons that onto the
  ;; front of the save-place-alist, if save-place is non-nil.
  ;; Otherwise, just delete that file from the alist.
  ;; first check to make sure alist has been loaded in from the master
  ;; file.  If not, do so, then feel free to modify the alist.  It
  ;; will be saved again when Emacs is killed.
  (or save-place-loaded (load-save-place-alist-from-file))
  (when (and buffer-file-name
	     (or (not save-place-ignore-files-regexp)
		 (not (string-match save-place-ignore-files-regexp
				    buffer-file-name))))
    (let ((cell (assoc buffer-file-name save-place-alist))
	  (position (if (not (eq major-mode 'hexl-mode))
			(point)
		      (with-no-warnings
			(1+ (hexl-current-address))))))
      (if cell
	  (setq save-place-alist (delq cell save-place-alist)))
      (if (and save-place
	       (not (= position 1)))  ;; Optimize out the degenerate case.
	  (setq save-place-alist
		(cons (cons buffer-file-name position)
		      save-place-alist))))))

(defun save-place-forget-unreadable-files ()
  "Remove unreadable files from `save-place-alist'.
For each entry in the alist, if `file-readable-p' returns nil for the
filename, remove the entry.  Save the new alist \(as the first pair
may have changed\) back to `save-place-alist'."
  (interactive)
  ;; the following was adapted from an in-place filtering function,
  ;; `filter-mod', used in the original.
  (unless (null save-place-alist)	;says it better than `when'
    ;; first, check all except first
    (let ((fmprev save-place-alist) (fmcur (cdr save-place-alist)))
      (while fmcur			;not null
	;; a value is only saved when it becomes FMPREV.
	(if (if (string-match save-place-skip-check-regexp (caar fmcur))
		save-place-save-skipped
	      (file-readable-p (caar fmcur)))
	    (setq fmprev fmcur)
	  (setcdr fmprev (cdr fmcur)))
	(setq fmcur (cdr fmcur))))
    ;; test first pair, keep it if OK, otherwise 2nd element, which
    ;; may be '()
    (unless (if (string-match save-place-skip-check-regexp
			      (caar save-place-alist))
		save-place-save-skipped
	      (file-readable-p (caar save-place-alist)))
      (setq save-place-alist (cdr save-place-alist)))))

(defun save-place-alist-to-file ()
  (let ((file (expand-file-name save-place-file))
        (coding-system-for-write 'utf-8))
    (with-current-buffer (get-buffer-create " *Saved Places*")
      (delete-region (point-min) (point-max))
      (when save-place-forget-unreadable-files
	(save-place-forget-unreadable-files))
      (insert (format ";;; -*- coding: %s -*-\n"
                      (symbol-name coding-system-for-write)))
      (let ((print-length nil)
            (print-level nil))
        (pp (sort save-place-alist
                  (lambda (a b) (string< (car a) (car b))))
            (current-buffer)))
      (let ((version-control
             (cond
              ((null save-place-version-control) nil)
              ((eq 'never save-place-version-control) 'never)
              ((eq 'nospecial save-place-version-control) version-control)
              (t
               t))))
	(condition-case nil
	    ;; Don't use write-file; we don't want this buffer to visit it.
            (write-region (point-min) (point-max) file)
	  (file-error (message "Saving places: can't write %s" file)))
        (kill-buffer (current-buffer))))))

(defun load-save-place-alist-from-file ()
  (if (not save-place-loaded)
      (progn
        (setq save-place-loaded t)
        (let ((file (expand-file-name save-place-file)))
          ;; make sure that the alist does not get overwritten, and then
          ;; load it if it exists:
          (if (file-readable-p file)
              ;; don't want to use find-file because we have been
              ;; adding hooks to it.
              (with-current-buffer (get-buffer-create " *Saved Places*")
                (delete-region (point-min) (point-max))
                (insert-file-contents file)
                (goto-char (point-min))
                (setq save-place-alist
                      (car (read-from-string
                            (buffer-substring (point-min) (point-max)))))

                ;; If there is a limit, and we're over it, then we'll
                ;; have to truncate the end of the list:
                (if save-place-limit
                    (if (<= save-place-limit 0)
                        ;; Zero gets special cased.  I'm not thrilled
                        ;; with this, but the loop for >= 1 is tight.
                        (setq save-place-alist nil)
                      ;; Else the limit is >= 1, so enforce it by
                      ;; counting and then `setcdr'ing.
                      (let ((s save-place-alist)
                            (count 1))
                        (while s
                          (if (>= count save-place-limit)
                              (setcdr s nil)
                            (setq count (1+ count)))
                          (setq s (cdr s))))))

                (kill-buffer (current-buffer))))
          nil))))

(defun save-places-to-alist ()
  ;; go through buffer-list, saving places to alist if save-place is
  ;; non-nil, deleting them from alist if it is nil.
  (let ((buf-list (buffer-list)))
    (while buf-list
      ;; put this into a save-excursion in case someone is counting on
      ;; another function in kill-emacs-hook to act on the last buffer
      ;; they were in:
      (with-current-buffer (car buf-list)
	;; save-place checks buffer-file-name too, but we can avoid
	;; overhead of function call by checking here too.
	(and buffer-file-name (save-place-to-alist))
	(setq buf-list (cdr buf-list))))))

(defun save-place-find-file-hook ()
  (or save-place-loaded (load-save-place-alist-from-file))
  (let ((cell (assoc buffer-file-name save-place-alist)))
    (if cell
	(progn
	  (or revert-buffer-in-progress-p
	      (goto-char (cdr cell)))
          ;; and make sure it will be saved again for later
          (setq save-place t)))))

(defun save-place-kill-emacs-hook ()
  ;; First update the alist.  This loads the old save-place-file if nec.
  (save-places-to-alist)
  ;; Now save the alist in the file, if we have ever loaded the file
  ;; (including just now).
  (if save-place-loaded
      (save-place-alist-to-file)))

(add-hook 'find-file-hook 'save-place-find-file-hook t)

(unless noninteractive
  (add-hook 'kill-emacs-hook 'save-place-kill-emacs-hook))

(add-hook 'kill-buffer-hook 'save-place-to-alist)

(provide 'saveplace) ; why not...

;;; saveplace.el ends here
