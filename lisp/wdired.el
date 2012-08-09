;;; wdired.el --- Rename files editing their names in dired buffers

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Filename: wdired.el
;; Author: Juan León Lahoz García <juanleon1@gmail.com>
;; Version: 2.0
;; Keywords: dired, environment, files, renaming

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

;; wdired.el (the "w" is for writable) provides an alternative way of
;; renaming files.
;;
;; Have you ever wished to use C-x r t (string-rectangle), M-%
;; (query-replace), M-c (capitalize-word), etc... to change the name of
;; the files in a "dired" buffer? Now you can do this.  All the power
;; of Emacs commands are available to renaming files!
;;
;; This package provides a function that makes the filenames of a
;; dired buffer editable, by changing the buffer mode (which inhibits
;; all of the commands of dired mode). Here you can edit the names of
;; one or more files and directories, and when you press C-c C-c, the
;; renaming takes effect and you are back to dired mode.
;;
;; Another things you can do with WDired:
;;
;; - To move files to another directory (by typing their path,
;;   absolute or relative, as a part of the new filename).
;;
;; - To change the target of symbolic links.
;;
;; - To change the permission bits of the filenames (in systems with a
;;   working unix-alike `dired-chmod-program'). See and customize the
;;   variable `wdired-allow-to-change-permissions'.  To change a single
;;   char (toggling between its two more usual values) you can press
;;   the space bar over it or left-click the mouse.  To set any char to
;;   an specific value (this includes the SUID, SGID and STI bits) you
;;   can use the key labeled as the letter you want.  Please note that
;;   permissions of the links cannot be changed in that way, because
;;   the change would affect to their targets, and this would not be
;;   WYSIWYG :-).
;;
;; - To mark files for deletion, by deleting their whole filename.

;;; Usage:

;; You can edit the names of the files by typing C-x C-q or by
;; executing M-x wdired-change-to-wdired-mode.  Use C-c C-c when
;; finished or C-c C-k to abort.  While editing filenames, a new
;; submenu "WDired" is available at top level.  You can customize the
;; behavior of this package from this menu.

;;; Change Log:

;; Google is your friend (previous versions with complete changelogs
;; were posted to gnu.emacs.sources)

;;; Code:

(defvar dired-backup-overwrite) ; Only in Emacs 20.x this is a custom var

(eval-when-compile (require 'cl))
(require 'dired)
(autoload 'dired-do-create-files-regexp "dired-aux")

(defgroup wdired nil
  "Mode to rename files by editing their names in dired buffers."
  :group 'dired)

(defcustom wdired-use-interactive-rename nil
  "If non-nil, WDired requires confirmation before actually renaming files.
If nil, WDired doesn't require confirmation to change the file names,
and the variable `wdired-confirm-overwrite' controls whether it is ok
to overwrite files without asking."
  :type 'boolean
  :group 'wdired)

(defcustom wdired-confirm-overwrite t
  "If nil the renames can overwrite files without asking.
This variable has no effect at all if `wdired-use-interactive-rename'
is not nil."
  :type 'boolean
  :group 'wdired)

(defcustom wdired-use-dired-vertical-movement nil
  "If t, the \"up\" and \"down\" movement works as in Dired mode.
That is, always move the point to the beginning of the filename at line.

If `sometimes', only move to the beginning of filename if the point is
before it, and `track-eol' is non-nil.  This behavior is very handy
when editing several filenames.

If nil, \"up\" and \"down\" movement is done as in any other buffer."
  :type '(choice (const :tag "As in any other mode" nil)
		 (const :tag "Smart cursor placement" sometimes)
		 (other :tag "As in dired mode" t))
  :group 'wdired)

(defcustom wdired-allow-to-redirect-links t
  "If non-nil, the target of the symbolic links are editable.
In systems without symbolic links support, this variable has no effect
at all."
  :type 'boolean
  :group 'wdired)

(defcustom wdired-allow-to-change-permissions nil
  "If non-nil, the permissions bits of the files are editable.

If t, to change a single bit, put the cursor over it and press the
space bar, or left click over it.  You can also hit the letter you want
to set: if this value is allowed, the character in the buffer will be
changed.  Anyway, the point is advanced one position, so, for example,
you can keep the <x> key pressed to give execution permissions to
everybody to that file.

If `advanced', the bits are freely editable.  You can use
`string-rectangle', `query-replace', etc.  You can put any value (even
newlines), but if you want your changes to be useful, you better put a
intelligible value.

Anyway, the real change of the permissions is done by the external
program `dired-chmod-program', which must exist."
  :type '(choice (const :tag "Not allowed" nil)
                 (const :tag "Toggle/set bits" t)
		 (other :tag "Bits freely editable" advanced))
  :group 'wdired)

(defvar wdired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'wdired-finish-edit)
    (define-key map "\C-c\C-c" 'wdired-finish-edit)
    (define-key map "\C-c\C-k" 'wdired-abort-changes)
    (define-key map "\C-c\C-[" 'wdired-abort-changes)
    (define-key map "\C-x\C-q" 'wdired-exit)
    (define-key map "\C-m"     'ignore)
    (define-key map "\C-j"     'ignore)
    (define-key map "\C-o"     'ignore)
    (define-key map [up]       'wdired-previous-line)
    (define-key map "\C-p"     'wdired-previous-line)
    (define-key map [down]     'wdired-next-line)
    (define-key map "\C-n"     'wdired-next-line)

    (define-key map [menu-bar wdired]
      (cons "WDired" (make-sparse-keymap "WDired")))
    (define-key map [menu-bar wdired wdired-customize]
      '("Options" . wdired-customize))
    (define-key map [menu-bar wdired dashes]
      '("--"))
    (define-key map [menu-bar wdired wdired-abort-changes]
      '(menu-item "Abort Changes" wdired-abort-changes
		  :help "Abort changes and return to dired mode"))
    (define-key map [menu-bar wdired wdired-finish-edit]
      '("Commit Changes" . wdired-finish-edit))

    (define-key map [remap upcase-word] 'wdired-upcase-word)
    (define-key map [remap capitalize-word] 'wdired-capitalize-word)
    (define-key map [remap downcase-word] 'wdired-downcase-word)

    map))

(defvar wdired-mode-hook nil
  "Hooks run when changing to WDired mode.")

;; Local variables (put here to avoid compilation gripes)
(defvar wdired-col-perm) ;; Column where the permission bits start
(defvar wdired-old-content)
(defvar wdired-old-point)


(defun wdired-mode ()
  "\\<wdired-mode-map>File Names Editing mode.

Press \\[wdired-finish-edit] to make the changes to take effect
and exit.  To abort the edit, use \\[wdired-abort-changes].

In this mode you can edit the names of the files, the target of
the links and the permission bits of the files.  You can use
\\[customize-group] RET wdired to customize WDired behavior.

The only editable texts in a WDired buffer are filenames,
symbolic link targets, and filenames permission."
  (interactive)
  (error "This mode can be enabled only by `wdired-change-to-wdired-mode'"))
(put 'wdired-mode 'mode-class 'special)


;;;###autoload
(defun wdired-change-to-wdired-mode ()
  "Put a dired buffer in a mode in which filenames are editable.
\\<wdired-mode-map>
This mode allows the user to change the names of the files, and after
typing \\[wdired-finish-edit] Emacs renames the files and directories
in disk.

See `wdired-mode'."
  (interactive)
  (or (eq major-mode 'dired-mode)
      (error "Not a Dired buffer"))
  (set (make-local-variable 'wdired-old-content)
       (buffer-substring (point-min) (point-max)))
  (set (make-local-variable 'wdired-old-point) (point))
  (set (make-local-variable 'query-replace-skip-read-only) t)
  (set (make-local-variable 'isearch-filter-predicate)
       'wdired-isearch-filter-read-only)
  (use-local-map wdired-mode-map)
  (force-mode-line-update)
  (setq buffer-read-only nil)
  (dired-unadvertise default-directory)
  (add-hook 'kill-buffer-hook 'wdired-check-kill-buffer nil t)
  (setq major-mode 'wdired-mode)
  (setq mode-name "Editable Dired")
  (setq revert-buffer-function 'wdired-revert)
  ;; I temp disable undo for performance: since I'm going to clear the
  ;; undo list, it can save more than a 9% of time with big
  ;; directories because setting properties modify the undo-list.
  (buffer-disable-undo)
  (wdired-preprocess-files)
  (if wdired-allow-to-change-permissions
      (wdired-preprocess-perms))
  (if (and wdired-allow-to-redirect-links (fboundp 'make-symbolic-link))
      (wdired-preprocess-symlinks))
  (buffer-enable-undo) ; Performance hack. See above.
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (run-mode-hooks 'wdired-mode-hook)
  (message "%s" (substitute-command-keys
		 "Press \\[wdired-finish-edit] when finished \
or \\[wdired-abort-changes] to abort changes")))

(defun wdired-isearch-filter-read-only (beg end)
  "Skip matches that have a read-only property."
  (and (isearch-filter-visible beg end)
       (not (text-property-not-all (min beg end) (max beg end)
				   'read-only nil))))

;; Protect the buffer so only the filenames can be changed, and put
;; properties so filenames (old and new) can be easily found.
(defun wdired-preprocess-files ()
  (put-text-property (point-min) (1+ (point-min))'front-sticky t)
  (save-excursion
    (goto-char (point-min))
    (let ((b-protection (point))
	  filename)
      (while (not (eobp))
	(setq filename (dired-get-filename nil t))
        (when (and filename
		   (not (member (file-name-nondirectory filename) '("." ".."))))
	  (dired-move-to-filename)
	  ;; The rear-nonsticky property below shall ensure that text preceding
	  ;; the filename can't be modified.
	  (add-text-properties
	   (1- (point)) (point) `(old-name ,filename rear-nonsticky (read-only)))
	  (put-text-property b-protection (point) 'read-only t)
	  (setq b-protection (dired-move-to-end-of-filename t))
	  (put-text-property (point) (1+ (point)) 'end-name t))
        (forward-line))
      (put-text-property b-protection (point-max) 'read-only t))))

;; This code is a copy of some dired-get-filename lines.
(defsubst wdired-normalize-filename (file)
  (setq file
	;; FIXME: shouldn't we check for a `b' argument or somesuch before
	;; doing such unquoting?  --Stef
	(read (concat
	       "\"" (replace-regexp-in-string
		     "\\([^\\]\\|\\`\\)\"" "\\1\\\\\"" file)
	       "\"")))
  (and file buffer-file-coding-system
       (not file-name-coding-system)
       (not default-file-name-coding-system)
       (setq file (encode-coding-string file buffer-file-coding-system)))
  file)

(defun wdired-get-filename (&optional no-dir old)
  "Return the filename at line.
Similar to `dired-get-filename' but it doesn't rely on regexps.  It
relies on WDired buffer's properties.  Optional arg NO-DIR with value
non-nil means don't include directory.  Optional arg OLD with value
non-nil means return old filename."
  ;; FIXME: Use dired-get-filename's new properties.
  (let (beg end file)
    (save-excursion
      (setq end (line-end-position))
      (beginning-of-line)
      (setq beg (next-single-property-change (point) 'old-name nil end))
      (unless (eq beg end)
	(if old
	    (setq file (get-text-property beg 'old-name))
	  ;; In the following form changed `(1+ beg)' to `beg' so that
	  ;; the filename end is found even when the filename is empty.
	  ;; Fixes error and spurious newlines when marking files for
	  ;; deletion.
	  (setq end (next-single-property-change beg 'end-name))
	  (setq file (buffer-substring-no-properties (1+ beg) end)))
	(and file (setq file (wdired-normalize-filename file))))
      (if (or no-dir old)
	  file
	(and file (> (length file) 0)
             (concat (dired-current-directory) file))))))


(defun wdired-change-to-dired-mode ()
  "Change the mode back to dired."
  (or (eq major-mode 'wdired-mode)
      (error "Not a Wdired buffer"))
  (let ((inhibit-read-only t))
    (remove-text-properties
     (point-min) (point-max)
     '(front-sticky nil rear-nonsticky nil read-only nil keymap nil)))
  (use-local-map dired-mode-map)
  (force-mode-line-update)
  (setq buffer-read-only t)
  (setq major-mode 'dired-mode)
  (setq mode-name "Dired")
  (dired-advertise)
  (remove-hook 'kill-buffer-hook 'wdired-check-kill-buffer t)
  (set (make-local-variable 'revert-buffer-function) 'dired-revert))


(defun wdired-abort-changes ()
  "Abort changes and return to dired mode."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert wdired-old-content)
    (goto-char wdired-old-point))
  (wdired-change-to-dired-mode)
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil)
  (message "Changes aborted"))

(defun wdired-finish-edit ()
  "Actually rename files based on your editing in the Dired buffer."
  (interactive)
  (wdired-change-to-dired-mode)
  (let ((changes nil)
	(errors 0)
	files-deleted
	files-renamed
	some-file-names-unchanged
	file-old file-new tmp-value)
    (save-excursion
      (when (and wdired-allow-to-redirect-links
		 (fboundp 'make-symbolic-link))
	(setq tmp-value (wdired-do-symlink-changes))
	(setq errors (cdr tmp-value))
	(setq changes (car tmp-value)))
      (when (and wdired-allow-to-change-permissions
		 (boundp 'wdired-col-perm)) ; could have been changed
	(setq tmp-value (wdired-do-perm-changes))
	(setq errors (+ errors (cdr tmp-value)))
	(setq changes (or changes (car tmp-value))))
      (goto-char (point-max))
      (while (not (bobp))
	(setq file-old (wdired-get-filename nil t))
	(when file-old
	  (setq file-new (wdired-get-filename))
          (if (equal file-new file-old)
	      (setq some-file-names-unchanged t)
            (setq changes t)
            (if (not file-new)		;empty filename!
                (push file-old files-deleted)
              (push (cons file-old (substitute-in-file-name file-new))
                    files-renamed))))
	(forward-line -1)))
    (when files-renamed
      (setq errors (+ errors (wdired-do-renames files-renamed))))
    (if changes
	(progn
	  ;; If we are displaying a single file (rather than the
	  ;; contents of a directory), change dired-directory if that
	  ;; file was renamed.  (This ought to be generalized to
	  ;; handle the multiple files case, but that's less trivial).
	  (when (and (stringp dired-directory)
		     (not (file-directory-p dired-directory))
		     (null some-file-names-unchanged)
		     (= (length files-renamed) 1))
	    (setq dired-directory (cdr (car files-renamed))))
	  ;; Re-sort the buffer.
	  (revert-buffer))
      (let ((inhibit-read-only t))
	(remove-text-properties (point-min) (point-max)
				'(old-name nil end-name nil old-link nil
					   end-link nil end-perm nil
					   old-perm nil perm-changed nil))
	(message "(No changes to be performed)")))
    (when files-deleted
      (wdired-flag-for-deletion files-deleted))
    (when (> errors 0)
      (dired-log-summary (format "%d rename actions failed" errors) nil)))
  (set-buffer-modified-p nil)
  (setq buffer-undo-list nil))

(defun wdired-do-renames (renames)
  "Perform RENAMES in parallel."
  (let ((residue ())
        (progress nil)
        (errors 0)
        (overwrite (or (not wdired-confirm-overwrite) 1)))
    (while (or renames
               ;; We've done one round through the renames, we have found
               ;; some residue, but we also made some progress, so maybe
               ;; some of the residue were resolved: try again.
               (prog1 (setq renames residue)
                 (setq progress nil)
                 (setq residue nil)))
      (let* ((rename (pop renames))
             (file-new (cdr rename)))
        (cond
         ((rassoc file-new renames)
          (error "Trying to rename 2 files to the same name"))
         ((assoc file-new renames)
          ;; Renaming to a file name that already exists but will itself be
          ;; renamed as well.  Let's wait until that one gets renamed.
          (push rename residue))
         ((and (assoc file-new residue)
               ;; Make sure the file really exists: if it doesn't it's
               ;; not really a conflict.  It might be a temp-file generated
               ;; specifically to break a circular renaming.
               (file-exists-p file-new))
          ;; Renaming to a file name that already exists, needed to be renamed,
          ;; but whose renaming could not be performed right away.
          (if (or progress renames)
              ;; There's still a chance the conflict will be resolved.
              (push rename residue)
            ;; We have not made any progress and we've reached the end of
            ;; the renames, so we really have a circular conflict, and we
            ;; have to forcefully break the cycle.
            (message "Circular renaming: using temporary file name")
            (let ((tmp (make-temp-name file-new)))
              (push (cons (car rename) tmp) renames)
              (push (cons tmp file-new) residue))))
         (t
          (setq progress t)
          (let ((file-ori (car rename)))
            (if wdired-use-interactive-rename
                (wdired-search-and-rename file-ori file-new)
              ;; If dired-rename-file autoloads dired-aux while
              ;; dired-backup-overwrite is locally bound,
              ;; dired-backup-overwrite won't be initialized.
              ;; So we must ensure dired-aux is loaded.
              (require 'dired-aux)
              (condition-case err
                  (let ((dired-backup-overwrite nil))
                    (dired-rename-file file-ori file-new
                                       overwrite))
                (error
                 (setq errors (1+ errors))
                 (dired-log (concat "Rename `" file-ori "' to `"
                                    file-new "' failed:\n%s\n")
                            err)))))))))
    errors))


(defun wdired-exit ()
  "Exit wdired and return to dired mode.
Just return to dired mode if there are no changes.  Otherwise,
ask a yes-or-no question whether to save or cancel changes,
and proceed depending on the answer."
  (interactive)
  (if (buffer-modified-p)
      (if (y-or-n-p (format "Buffer %s modified; save changes? "
			    (current-buffer)))
	  (wdired-finish-edit)
	(wdired-abort-changes))
    (wdired-change-to-dired-mode)
    (set-buffer-modified-p nil)
    (setq buffer-undo-list nil)
    (message "(No changes need to be saved)")))

;; Rename a file, searching it in a modified dired buffer, in order
;; to be able to use `dired-do-create-files-regexp' and get its
;; "benefits".
(defun wdired-search-and-rename (filename-ori filename-new)
  (save-excursion
    (goto-char (point-max))
    (forward-line -1)
    (let ((done nil)
	  curr-filename)
      (while (and (not done) (not (bobp)))
        (setq curr-filename (wdired-get-filename nil t))
        (if (equal curr-filename filename-ori)
            (progn
              (setq done t)
              (let ((inhibit-read-only t))
                (dired-move-to-filename)
                (search-forward (wdired-get-filename t) nil t)
                (replace-match (file-name-nondirectory filename-ori) t t))
              (dired-do-create-files-regexp
               (function dired-rename-file)
               "Move" 1 ".*" filename-new nil t))
	  (forward-line -1))))))

;; marks a list of files for deletion
(defun wdired-flag-for-deletion (filenames-ori)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (member (dired-get-filename nil t) filenames-ori)
          (dired-flag-file-deletion 1)
	(forward-line)))))

(defun wdired-customize ()
  "Customize WDired options."
  (interactive)
  (customize-apropos "wdired" 'groups))

(defun wdired-revert (&optional _arg _noconfirm)
  "Discard changes in the buffer and update it based on changes on disk.
Optional arguments are ignored."
  (wdired-change-to-dired-mode)
  (revert-buffer)
  (wdired-change-to-wdired-mode))

(defun wdired-check-kill-buffer ()
  ;; FIXME: Can't we use the normal mechanism for that?  --Stef
  (if (and
       (buffer-modified-p)
       (not (y-or-n-p "Buffer changed. Discard changes and kill buffer? ")))
      (error "Error")))

(defun wdired-next-line (arg)
  "Move down lines then position at filename or the current column.
See `wdired-use-dired-vertical-movement'.  Optional prefix ARG
says how many lines to move; default is one line."
  (interactive "p")
  (with-no-warnings (next-line arg))
  (if (or (eq wdired-use-dired-vertical-movement t)
	  (and wdired-use-dired-vertical-movement
	       (< (current-column)
		  (save-excursion (dired-move-to-filename)
				  (current-column)))))
      (dired-move-to-filename)))

(defun wdired-previous-line (arg)
  "Move up lines then position at filename or the current column.
See `wdired-use-dired-vertical-movement'.  Optional prefix ARG
says how many lines to move; default is one line."
  (interactive "p")
  (with-no-warnings (previous-line arg))
  (if (or (eq wdired-use-dired-vertical-movement t)
	  (and wdired-use-dired-vertical-movement
	       (< (current-column)
		  (save-excursion (dired-move-to-filename)
				  (current-column)))))
      (dired-move-to-filename)))

;; Put the needed properties to allow the user to change links' targets
(defun wdired-preprocess-symlinks ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at dired-re-sym)
            (progn
              (re-search-forward " -> \\(.*\\)$")
	      (put-text-property (- (match-beginning 1) 2)
				 (1- (match-beginning 1)) 'old-link
				 (match-string-no-properties 1))
              (put-text-property (match-end 1) (1+ (match-end 1)) 'end-link t)
              (put-text-property (1- (match-beginning 1))
				 (match-beginning 1)
				 'rear-nonsticky '(read-only))
	      (put-text-property (match-beginning 1)
				 (match-end 1) 'read-only nil)))
        (forward-line)
	(beginning-of-line)))))


(defun wdired-get-previous-link (&optional old move)
  "Return the next symlink target.
If OLD, return the old target.  If MOVE, move point before it."
  (let (beg end target)
    (setq beg (previous-single-property-change (point) 'old-link nil))
    (if beg
	(progn
	  (if old
	      (setq target (get-text-property (1- beg) 'old-link))
	    (setq end (next-single-property-change beg 'end-link))
	    (setq target (buffer-substring-no-properties (1+ beg) end)))
	  (if move (goto-char (1- beg)))))
    (and target (wdired-normalize-filename target))))

(declare-function make-symbolic-link "fileio.c")

;; Perform the changes in the target of the changed links.
(defun wdired-do-symlink-changes ()
  (let ((changes nil)
	(errors 0)
	link-to-ori link-to-new link-from)
    (goto-char (point-max))
    (while (setq link-to-new (wdired-get-previous-link))
      (setq link-to-ori (wdired-get-previous-link t t))
      (setq link-from (wdired-get-filename nil t))
      (unless (equal link-to-new link-to-ori)
        (setq changes t)
        (if (equal link-to-new "") ;empty filename!
            (setq link-to-new "/dev/null"))
        (condition-case err
            (progn
              (delete-file link-from)
              (make-symbolic-link
               (substitute-in-file-name link-to-new) link-from))
          (error
           (setq errors (1+ errors))
           (dired-log (concat "Link `" link-from "' to `"
                              link-to-new "' failed:\n%s\n")
                      err)))))
    (cons changes errors)))

;; Perform a "case command" skipping read-only words.
(defun wdired-xcase-word (command arg)
  (if (< arg 0)
      (funcall command arg)
    (while (> arg 0)
      (condition-case nil
          (progn
            (funcall command 1)
            (setq arg (1- arg)))
        (error
         (if (forward-word)
	     ;; Skip any non-word characters to avoid triggering a read-only
	     ;; error which would cause skipping the next word characters too.
	     (skip-syntax-forward "^w")
	   (setq arg 0)))))))

(defun wdired-downcase-word (arg)
  "WDired version of `downcase-word'.
Like original function but it skips read-only words."
  (interactive "p")
  (wdired-xcase-word 'downcase-word arg))

(defun wdired-upcase-word (arg)
  "WDired version of `upcase-word'.
Like original function but it skips read-only words."
  (interactive "p")
  (wdired-xcase-word 'upcase-word arg))

(defun wdired-capitalize-word (arg)
  "WDired version of `capitalize-word'.
Like original function but it skips read-only words."
  (interactive "p")
  (wdired-xcase-word 'capitalize-word arg))


;; The following code deals with changing the access bits (or
;; permissions) of the files.

(defvar wdired-perm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'wdired-toggle-bit)
    (define-key map "r" 'wdired-set-bit)
    (define-key map "w" 'wdired-set-bit)
    (define-key map "x" 'wdired-set-bit)
    (define-key map "-" 'wdired-set-bit)
    (define-key map "S" 'wdired-set-bit)
    (define-key map "s" 'wdired-set-bit)
    (define-key map "T" 'wdired-set-bit)
    (define-key map "t" 'wdired-set-bit)
    (define-key map "s" 'wdired-set-bit)
    (define-key map "l" 'wdired-set-bit)
    (define-key map [down-mouse-1] 'wdired-mouse-toggle-bit)
    map))

;; Put a keymap property to the permission bits of the files, and store the
;; original name and permissions as a property
(defun wdired-preprocess-perms ()
  (let ((inhibit-read-only t))
    (set (make-local-variable 'wdired-col-perm) nil)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(when (and (not (looking-at dired-re-sym))
		   (wdired-get-filename)
		   (re-search-forward dired-re-perms (line-end-position) 'eol))
	  (let ((begin (match-beginning 0))
		(end (match-end 0)))
	    (unless wdired-col-perm
	      (setq wdired-col-perm (- (current-column) 9)))
	    (if (eq wdired-allow-to-change-permissions 'advanced)
		(progn
		  (put-text-property begin end 'read-only nil)
		  ;; make first permission bit writable
		  (put-text-property
		   (1- begin) begin 'rear-nonsticky '(read-only)))
	      ;; avoid that keymap applies to text following permissions
	      (add-text-properties
	       (1+ begin) end
	       `(keymap ,wdired-perm-mode-map rear-nonsticky (keymap))))
	    (put-text-property end (1+ end) 'end-perm t)
	    (put-text-property
	     begin (1+ begin) 'old-perm (match-string-no-properties 0))))
        (forward-line)
	(beginning-of-line)))))

(defun wdired-perm-allowed-in-pos (char pos)
  (cond
   ((= char ?-)          t)
   ((= char ?r)          (= (% pos 3) 0))
   ((= char ?w)          (= (% pos 3) 1))
   ((= char ?x)          (= (% pos 3) 2))
   ((memq char '(?s ?S)) (memq pos '(2 5)))
   ((memq char '(?t ?T)) (= pos 8))
   ((= char ?l)          (= pos 5))))

(defun wdired-set-bit ()
  "Set a permission bit character."
  (interactive)
  (if (wdired-perm-allowed-in-pos last-command-event
                                  (- (current-column) wdired-col-perm))
      (let ((new-bit (char-to-string last-command-event))
            (inhibit-read-only t)
	    (pos-prop (- (point) (- (current-column) wdired-col-perm))))
        (put-text-property 0 1 'keymap wdired-perm-mode-map new-bit)
        (put-text-property 0 1 'read-only t new-bit)
        (insert new-bit)
        (delete-char 1)
	(put-text-property (1- pos-prop) pos-prop 'perm-changed t)
	(put-text-property (1- (point)) (point) 'rear-nonsticky '(keymap)))
    (forward-char 1)))

(defun wdired-toggle-bit ()
  "Toggle the permission bit at point."
  (interactive)
  (let ((inhibit-read-only t)
	(new-bit "-")
	(pos-prop (- (point) (- (current-column) wdired-col-perm))))
    (if (eq (char-after (point)) ?-)
	(setq new-bit
	      (if (= (% (- (current-column) wdired-col-perm) 3) 0) "r"
		(if (= (% (- (current-column) wdired-col-perm) 3) 1) "w"
		  "x"))))
    (put-text-property 0 1 'keymap wdired-perm-mode-map new-bit)
    (put-text-property 0 1 'read-only t new-bit)
    (insert new-bit)
    (delete-char 1)
    (put-text-property (1- pos-prop) pos-prop 'perm-changed t)
    (put-text-property (1- (point)) (point) 'rear-nonsticky '(keymap))))

(defun wdired-mouse-toggle-bit (event)
  "Toggle the permission bit that was left clicked."
  (interactive "e")
  (mouse-set-point event)
  (wdired-toggle-bit))

;; Allowed chars for 4000 bit are Ss  in position 3
;; Allowed chars for 2000 bit are Ssl in position 6
;; Allowed chars for 1000 bit are Tt  in position 9
(defun wdired-perms-to-number (perms)
  (let ((nperm 0777))
    (if (= (elt perms 1) ?-) (setq nperm (- nperm 400)))
    (if (= (elt perms 2) ?-) (setq nperm (- nperm 200)))
    (let ((p-bit (elt perms 3)))
      (if (memq p-bit '(?- ?S)) (setq nperm (- nperm 100)))
      (if (memq p-bit '(?s ?S)) (setq nperm (+ nperm 4000))))
    (if (= (elt perms 4) ?-) (setq nperm (- nperm 40)))
    (if (= (elt perms 5) ?-) (setq nperm (- nperm 20)))
    (let ((p-bit (elt perms 6)))
      (if (memq p-bit '(?- ?S ?l)) (setq nperm (- nperm 10)))
      (if (memq p-bit '(?s ?S ?l)) (setq nperm (+ nperm 2000))))
    (if (= (elt perms 7) ?-) (setq nperm (- nperm 4)))
    (if (= (elt perms 8) ?-) (setq nperm (- nperm 2)))
    (let ((p-bit (elt perms 9)))
      (if (memq p-bit '(?- ?T)) (setq nperm (- nperm 1)))
      (if (memq p-bit '(?t ?T)) (setq nperm (+ nperm 1000))))
    nperm))

;; Perform the changes in the permissions of the files that have
;; changed.
(defun wdired-do-perm-changes ()
  (let ((changes nil)
	(errors 0)
	(prop-wanted (if (eq wdired-allow-to-change-permissions 'advanced)
			 'old-perm 'perm-changed))
	filename perms-ori perms-new perm-tmp)
    (goto-char (next-single-property-change (point-min) prop-wanted
					    nil (point-max)))
    (while (not (eobp))
      (setq perms-ori (get-text-property (point) 'old-perm))
      (setq perms-new (buffer-substring-no-properties
		       (point) (next-single-property-change (point) 'end-perm)))
      (unless (equal perms-ori perms-new)
        (setq changes t)
        (setq filename (wdired-get-filename nil t))
        (if (= (length perms-new) 10)
            (progn
              (setq perm-tmp
                    (int-to-string (wdired-perms-to-number perms-new)))
              (unless (equal 0 (process-file dired-chmod-program
					     nil nil nil perm-tmp filename))
                (setq errors (1+ errors))
                (dired-log (concat dired-chmod-program " " perm-tmp
                                   " `" filename "' failed\n\n"))))
          (setq errors (1+ errors))
          (dired-log (concat "Cannot parse permission `" perms-new
                             "' for file `" filename "'\n\n"))))
      (goto-char (next-single-property-change (1+ (point)) prop-wanted
					      nil (point-max))))
    (cons changes errors)))

(provide 'wdired)

;; Local Variables:
;; coding: latin-1
;; byte-compile-dynamic: t
;; End:

;;; wdired.el ends here
