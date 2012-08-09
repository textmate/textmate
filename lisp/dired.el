;;; dired.el --- directory-browsing commands -*- lexical-binding: t -*-

;; Copyright (C) 1985-1986, 1992-1997, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;; Maintainer: FSF
;; Keywords: files
;; Package: emacs

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

;; This is a major mode for directory browsing and editing.
;; It is documented in the Emacs manual.

;; Rewritten in 1990/1991 to add tree features, file marking and
;; sorting by Sebastian Kremer <sk@thp.uni-koeln.de>.
;; Finished up by rms in 1992.

;;; Code:

(eval-when-compile (require 'cl))

;;; Customizable variables

(defgroup dired nil
  "Directory editing."
  :link '(custom-manual "(emacs)Dired")
  :group 'files)

(defgroup dired-mark nil
  "Handling marks in Dired."
  :prefix "dired-"
  :group 'dired)


;;;###autoload
(defcustom dired-listing-switches (purecopy "-al")
  "Switches passed to `ls' for Dired.  MUST contain the `l' option.
May contain all other options that don't contradict `-l';
may contain even `F', `b', `i' and `s'.  See also the variable
`dired-ls-F-marks-symlinks' concerning the `F' switch.
On systems such as MS-DOS and MS-Windows, which use `ls' emulation in Lisp,
some of the `ls' switches are not supported; see the doc string of
`insert-directory' in `ls-lisp.el' for more details."
  :type 'string
  :group 'dired)

(defcustom dired-subdir-switches nil
  "If non-nil, switches passed to `ls' for inserting subdirectories.
If nil, `dired-listing-switches' is used."
   :group 'dired
   :type '(choice (const :tag "Use dired-listing-switches" nil)
                  (string :tag "Switches")))

(defcustom dired-chown-program
  (purecopy (cond ((executable-find "chown") "chown")
                  ((file-executable-p "/usr/sbin/chown") "/usr/sbin/chown")
                  ((file-executable-p "/etc/chown") "/etc/chown")
                  (t "chown")))
  "Name of chown command (usually `chown')."
  :group 'dired
  :type 'file)

(defcustom dired-use-ls-dired 'unspecified
  "Non-nil means Dired should pass the \"--dired\" option to \"ls\".
The special value of `unspecified' means to check explicitly, and
save the result in this variable.  This is performed the first
time `dired-insert-directory' is called.

Note that if you set this option to nil, either through choice or
because your \"ls\" program does not support \"--dired\", Dired
will fail to parse some \"unusual\" file names, e.g. those with leading
spaces.  You might want to install ls from GNU Coreutils, which does
support this option.  Alternatively, you might want to use Emacs's
own emulation of \"ls\", by using:
  \(setq ls-lisp-use-insert-directory-program nil)
  \(require 'ls-lisp)
This is used by default on MS Windows, which does not have an \"ls\" program.
Note that `ls-lisp' does not support as many options as GNU ls, though.
For more details, see Info node `(emacs)ls in Lisp'."
  :group 'dired
  :type '(choice (const :tag "Check for --dired support" unspecified)
                 (const :tag "Do not use --dired" nil)
                 (other :tag "Use --dired" t)))

(defcustom dired-chmod-program "chmod"
  "Name of chmod command (usually `chmod')."
  :group 'dired
  :type 'file)

(defcustom dired-touch-program "touch"
  "Name of touch command (usually `touch')."
   :group 'dired
   :type 'file)

(defcustom dired-ls-F-marks-symlinks nil
  "Informs Dired about how `ls -lF' marks symbolic links.
Set this to t if `ls' (or whatever program is specified by
`insert-directory-program') with `-lF' marks the symbolic link
itself with a trailing @ (usually the case under Ultrix).

Example: if `ln -s foo bar; ls -F bar' gives `bar -> foo', set it to
nil (the default), if it gives `bar@ -> foo', set it to t.

Dired checks if there is really a @ appended.  Thus, if you have a
marking `ls' program on one host and a non-marking on another host, and
don't care about symbolic links which really end in a @, you can
always set this variable to t."
  :type 'boolean
  :group 'dired-mark)

(defcustom dired-trivial-filenames (purecopy "^\\.\\.?$\\|^#")
  "Regexp of files to skip when finding first file of a directory.
A value of nil means move to the subdir line.
A value of t means move to first file."
  :type '(choice (const :tag "Move to subdir" nil)
		 (const :tag "Move to first" t)
		 regexp)
  :group 'dired)

(defcustom dired-keep-marker-rename t
  ;; Use t as default so that moved files "take their markers with them".
  "Controls marking of renamed files.
If t, files keep their previous marks when they are renamed.
If a character, renamed files (whether previously marked or not)
are afterward marked with that character."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark"))
  :group 'dired-mark)

(defcustom dired-keep-marker-copy ?C
  "Controls marking of copied files.
If t, copied files are marked if and as the corresponding original files were.
If a character, copied files are unconditionally marked with that character."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark"))
  :group 'dired-mark)

(defcustom dired-keep-marker-hardlink ?H
  "Controls marking of newly made hard links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark"))
  :group 'dired-mark)

(defcustom dired-keep-marker-symlink ?Y
  "Controls marking of newly made symbolic links.
If t, they are marked if and as the files linked to were marked.
If a character, new links are unconditionally marked with that character."
  :type '(choice (const :tag "Keep" t)
		 (character :tag "Mark"))
  :group 'dired-mark)

(defcustom dired-dwim-target nil
  "If non-nil, Dired tries to guess a default target directory.
This means: if there is a dired buffer displayed in the next window,
use its current subdir, instead of the current subdir of this dired buffer.

The target is used in the prompt for file copy, rename etc."
  :type 'boolean
  :group 'dired)

(defcustom dired-copy-preserve-time t
  "If non-nil, Dired preserves the last-modified time in a file copy.
\(This works on only some systems.)"
  :type 'boolean
  :group 'dired)

; These variables were deleted and the replacements are on files.el.
; We leave aliases behind for back-compatibility.
(defvaralias 'dired-free-space-program 'directory-free-space-program)
(defvaralias 'dired-free-space-args 'directory-free-space-args)

;;; Hook variables

(defcustom dired-load-hook nil
  "Run after loading Dired.
You can customize key bindings or load extensions with this."
  :group 'dired
  :type 'hook)

(defcustom dired-mode-hook nil
  "Run at the very end of `dired-mode'."
  :group 'dired
  :type 'hook)

(defcustom dired-before-readin-hook nil
  "This hook is run before a dired buffer is read in (created or reverted)."
  :group 'dired
  :type 'hook)

(defcustom dired-after-readin-hook nil
  "Hook run after each time a file or directory is read by Dired.
After each listing of a file or directory, this hook is run
with the buffer narrowed to the listing."
  :group 'dired
  :type 'hook)
;; Note this can't simply be run inside function `dired-ls' as the hook
;; functions probably depend on the dired-subdir-alist to be OK.

(defcustom dired-dnd-protocol-alist
  '(("^file:///" . dired-dnd-handle-local-file)
    ("^file://"  . dired-dnd-handle-file)
    ("^file:"    . dired-dnd-handle-local-file))
  "The functions to call when a drop in `dired-mode' is made.
See `dnd-protocol-alist' for more information.  When nil, behave
as in other buffers.  Changing this option is effective only for
new dired buffers."
  :type '(choice (repeat (cons (regexp) (function)))
		 (const :tag "Behave as in other buffers" nil))
  :version "22.1"
  :group 'dired)

;; Internal variables

(defvar dired-marker-char ?*		; the answer is 42
  ;; so that you can write things like
  ;; (let ((dired-marker-char ?X))
  ;;    ;; great code using X markers ...
  ;;    )
  ;; For example, commands operating on two sets of files, A and B.
  ;; Or marking files with digits 0-9.  This could implicate
  ;; concentric sets or an order for the marked files.
  ;; The code depends on dynamic scoping on the marker char.
  "In Dired, the current mark character.
This is what the do-commands look for, and what the mark-commands store.")

(defvar dired-del-marker ?D
  "Character used to flag files for deletion.")

(defvar dired-shrink-to-fit t
;; I see no reason ever to make this nil -- rms.
;;  (> baud-rate search-slow-speed)
  "Non-nil means Dired shrinks the display buffer to fit the marked files.")

(defvar dired-file-version-alist)

;;;###autoload
(defvar dired-directory nil
  "The directory name or wildcard spec that this dired directory lists.
Local to each dired buffer.  May be a list, in which case the car is the
directory name and the cdr is the list of files to mention.
The directory name must be absolute, but need not be fully expanded.")

;; Beware of "-l;reboot" etc.  See bug#3230.
(defun dired-safe-switches-p (switches)
  "Return non-nil if string SWITCHES does not look risky for dired."
  (or (not switches)
      (and (stringp switches)
           (< (length switches) 100)    ; arbitrary
           (string-match "\\` *-[- [:alnum:]]+\\'" switches))))

(defvar dired-actual-switches nil
  "The value of `dired-listing-switches' used to make this buffer's text.")

(put 'dired-actual-switches 'safe-local-variable 'dired-safe-switches-p)

(defvar dired-re-inode-size "[0-9 \t]*"
  "Regexp for optional initial inode and file size as made by `ls -i -s'.")

;; These regexps must be tested at beginning-of-line, but are also
;; used to search for next matches, so neither omitting "^" nor
;; replacing "^" by "\n" (to make it slightly faster) will work.

(defvar dired-re-mark "^[^ \n]")
;; "Regexp matching a marked line.
;; Important: the match ends just after the marker."
(defvar dired-re-maybe-mark "^. ")
;; The [^:] part after "d" and "l" is to avoid confusion with the
;; DOS/Windows-style drive letters in directory names, like in "d:/foo".
(defvar dired-re-dir (concat dired-re-maybe-mark dired-re-inode-size "d[^:]"))
(defvar dired-re-sym (concat dired-re-maybe-mark dired-re-inode-size "l[^:]"))
(defvar dired-re-exe;; match ls permission string of an executable file
  (mapconcat (function
	      (lambda (x)
		(concat dired-re-maybe-mark dired-re-inode-size x)))
	     '("-[-r][-w][xs][-r][-w].[-r][-w]."
	       "-[-r][-w].[-r][-w][xs][-r][-w]."
	       "-[-r][-w].[-r][-w].[-r][-w][xst]")
	     "\\|"))
(defvar dired-re-perms "[-bcdlps][-r][-w].[-r][-w].[-r][-w].")
(defvar dired-re-dot "^.* \\.\\.?/?$")

;; The subdirectory names in the next two lists are expanded.
(defvar dired-subdir-alist nil
  "Association list of subdirectories and their buffer positions.
Each subdirectory has an element: (DIRNAME . STARTMARKER).
The order of elements is the reverse of the order in the buffer.
In simple cases, this list contains one element.")

(defvar dired-switches-alist nil
  "Keeps track of which switches to use for inserted subdirectories.
This is an alist of the form (SUBDIR . SWITCHES).")
(make-variable-buffer-local 'dired-switches-alist)

(defvaralias 'dired-move-to-filename-regexp
  'directory-listing-before-filename-regexp)

(defvar dired-subdir-regexp "^. \\([^\n\r]+\\)\\(:\\)[\n\r]"
  "Regexp matching a maybe hidden subdirectory line in `ls -lR' output.
Subexpression 1 is the subdirectory proper, no trailing colon.
The match starts at the beginning of the line and ends after the end
of the line (\\n or \\r).
Subexpression 2 must end right before the \\n or \\r.")

(defgroup dired-faces nil
  "Faces used by Dired."
  :group 'dired
  :group 'faces)

(defface dired-header
  '((t (:inherit font-lock-type-face)))
  "Face used for directory headers."
  :group 'dired-faces
  :version "22.1")
(defvar dired-header-face 'dired-header
  "Face name used for directory headers.")

(defface dired-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for dired marks."
  :group 'dired-faces
  :version "22.1")
(defvar dired-mark-face 'dired-mark
  "Face name used for dired marks.")

(defface dired-marked
  '((t (:inherit warning)))
  "Face used for marked files."
  :group 'dired-faces
  :version "22.1")
(defvar dired-marked-face 'dired-marked
  "Face name used for marked files.")

(defface dired-flagged
  '((t (:inherit error)))
  "Face used for files flagged for deletion."
  :group 'dired-faces
  :version "22.1")
(defvar dired-flagged-face 'dired-flagged
  "Face name used for files flagged for deletion.")

(defface dired-warning
  ;; Inherit from font-lock-warning-face since with min-colors 8
  ;; font-lock-comment-face is not colored any more.
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight a part of a buffer that needs user attention."
  :group 'dired-faces
  :version "22.1")
(defvar dired-warning-face 'dired-warning
  "Face name used for a part of a buffer that needs user attention.")

(defface dired-perm-write
  '((((type w32 pc)) :inherit default)  ;; These default to rw-rw-rw.
    ;; Inherit from font-lock-comment-delimiter-face since with min-colors 8
    ;; font-lock-comment-face is not colored any more.
    (t (:inherit font-lock-comment-delimiter-face)))
  "Face used to highlight permissions of group- and world-writable files."
  :group 'dired-faces
  :version "22.2")
(defvar dired-perm-write-face 'dired-perm-write
  "Face name used for permissions of group- and world-writable files.")

(defface dired-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for subdirectories."
  :group 'dired-faces
  :version "22.1")
(defvar dired-directory-face 'dired-directory
  "Face name used for subdirectories.")

(defface dired-symlink
  '((t (:inherit font-lock-keyword-face)))
  "Face used for symbolic links."
  :group 'dired-faces
  :version "22.1")
(defvar dired-symlink-face 'dired-symlink
  "Face name used for symbolic links.")

(defface dired-ignored
  '((t (:inherit shadow)))
  "Face used for files suffixed with `completion-ignored-extensions'."
  :group 'dired-faces
  :version "22.1")
(defvar dired-ignored-face 'dired-ignored
  "Face name used for files suffixed with `completion-ignored-extensions'.")

(defvar dired-font-lock-keywords
  (list
   ;;
   ;; Dired marks.
   (list dired-re-mark '(0 dired-mark-face))
   ;;
   ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
   ;; file name itself.  We search for Dired defined regexps, and then use the
   ;; Dired defined function `dired-move-to-filename' before searching for the
   ;; simple regexp ".+".  It is that regexp which matches the file name.
   ;;
   ;; Marked files.
   (list (concat "^[" (char-to-string dired-marker-char) "]")
         '(".+" (dired-move-to-filename) nil (0 dired-marked-face)))
   ;;
   ;; Flagged files.
   (list (concat "^[" (char-to-string dired-del-marker) "]")
         '(".+" (dired-move-to-filename) nil (0 dired-flagged-face)))
   ;; People who are paranoid about security would consider this more
   ;; important than other things such as whether it is a directory.
   ;; But we don't want to encourage paranoia, so our default
   ;; should be what's most useful for non-paranoids. -- rms.
;;;   ;;
;;;   ;; Files that are group or world writable.
;;;   (list (concat dired-re-maybe-mark dired-re-inode-size
;;;		 "\\([-d]\\(....w....\\|.......w.\\)\\)")
;;;	 '(1 dired-warning-face)
;;;	 '(".+" (dired-move-to-filename) nil (0 dired-warning-face)))
   ;; However, we don't need to highlight the file name, only the
   ;; permissions, to win generally.  -- fx.
   ;; Fixme: we could also put text properties on the permission
   ;; fields with keymaps to frob the permissions, somewhat a la XEmacs.
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d]....\\(w\\)....")	; group writable
	 '(1 dired-perm-write-face))
   (list (concat dired-re-maybe-mark dired-re-inode-size
		 "[-d].......\\(w\\).")	; world writable
	 '(1 dired-perm-write-face))
   ;;
   ;; Subdirectories.
   (list dired-re-dir
	 '(".+" (dired-move-to-filename) nil (0 dired-directory-face)))
   ;;
   ;; Symbolic links.
   (list dired-re-sym
	 '(".+" (dired-move-to-filename) nil (0 dired-symlink-face)))
   ;;
   ;; Files suffixed with `completion-ignored-extensions'.
   '(eval .
     ;; It is quicker to first find just an extension, then go back to the
     ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
     (list (concat "\\(" (regexp-opt completion-ignored-extensions) "\\|#\\)$")
	   '(".+" (dired-move-to-filename) nil (0 dired-ignored-face))))
   ;;
   ;; Files suffixed with `completion-ignored-extensions'
   ;; plus a character put in by -F.
   '(eval .
     (list (concat "\\(" (regexp-opt completion-ignored-extensions)
		   "\\|#\\)[*=|]$")
	   '(".+" (progn
		    (end-of-line)
		    ;; If the last character is not part of the filename,
		    ;; move back to the start of the filename
		    ;; so it can be fontified.
		    ;; Otherwise, leave point at the end of the line;
		    ;; that way, nothing is fontified.
		    (unless (get-text-property (1- (point)) 'mouse-face)
		      (dired-move-to-filename)))
	     nil (0 dired-ignored-face))))
   ;;
   ;; Explicitly put the default face on file names ending in a colon to
   ;; avoid fontifying them as directory header.
   (list (concat dired-re-maybe-mark dired-re-inode-size dired-re-perms ".*:$")
	 '(".+" (dired-move-to-filename) nil (0 'default)))
   ;;
   ;; Directory headers.
   (list dired-subdir-regexp '(1 dired-header-face))
)
  "Additional expressions to highlight in Dired mode.")

(defvar dnd-protocol-alist)

;;; Macros must be defined before they are used, for the byte compiler.

(defmacro dired-mark-if (predicate msg)
  "Mark all files for which PREDICATE evals to non-nil.
PREDICATE is evaluated on each line, with point at beginning of line.
MSG is a noun phrase for the type of files being marked.
It should end with a noun that can be pluralized by adding `s'.
Return value is the number of files marked, or nil if none were marked."
  `(let ((inhibit-read-only t) count)
    (save-excursion
      (setq count 0)
      (when ,msg
	(message "%s %ss%s..."
		 (cond ((eq dired-marker-char ?\040) "Unmarking")
		       ((eq dired-del-marker dired-marker-char)
			"Flagging")
		       (t "Marking"))
		 ,msg
		 (if (eq dired-del-marker dired-marker-char)
		     " for deletion"
		   "")))
      (goto-char (point-min))
      (while (not (eobp))
        (if ,predicate
            (progn
              (delete-char 1)
              (insert dired-marker-char)
              (setq count (1+ count))))
        (forward-line 1))
      (if ,msg (message "%s %s%s %s%s."
                        count
                        ,msg
                        (dired-plural-s count)
                        (if (eq dired-marker-char ?\040) "un" "")
                        (if (eq dired-marker-char dired-del-marker)
                            "flagged" "marked"))))
    (and (> count 0) count)))

(defmacro dired-map-over-marks (body arg &optional show-progress
				     distinguish-one-marked)
  "Eval BODY with point on each marked line.  Return a list of BODY's results.
If no marked file could be found, execute BODY on the current
line.  ARG, if non-nil, specifies the files to use instead of the
marked files.

If ARG is an integer, use the next ARG (or previous -ARG, if
ARG<0) files.  In that case, point is dragged along.  This is so
that commands on the next ARG (instead of the marked) files can
be chained easily.
For any other non-nil value of ARG, use the current file.

If optional third arg SHOW-PROGRESS evaluates to non-nil,
redisplay the dired buffer after each file is processed.

No guarantee is made about the position on the marked line.  BODY
must ensure this itself if it depends on this.

Search starts at the beginning of the buffer, thus the car of the
list corresponds to the line nearest to the buffer's bottom.
This is also true for (positive and negative) integer values of
ARG.

BODY should not be too long as it is expanded four times.

If DISTINGUISH-ONE-MARKED is non-nil, then if we find just one
marked file, return (t FILENAME) instead of (FILENAME)."
  ;;
  ;;Warning: BODY must not add new lines before point - this may cause an
  ;;endless loop.
  ;;This warning should not apply any longer, sk  2-Sep-1991 14:10.
  `(prog1
       (let ((inhibit-read-only t) case-fold-search found results)
	 (if ,arg
	     (if (integerp ,arg)
		 (progn	;; no save-excursion, want to move point.
		   (dired-repeat-over-lines
		    ,arg
		    (function (lambda ()
				(if ,show-progress (sit-for 0))
				(setq results (cons ,body results)))))
		   (if (< ,arg 0)
		       (nreverse results)
		     results))
	       ;; non-nil, non-integer ARG means use current file:
	       (list ,body))
	   (let ((regexp (dired-marker-regexp)) next-position)
	     (save-excursion
	       (goto-char (point-min))
	       ;; remember position of next marked file before BODY
	       ;; can insert lines before the just found file,
	       ;; confusing us by finding the same marked file again
	       ;; and again and...
	       (setq next-position (and (re-search-forward regexp nil t)
					(point-marker))
		     found (not (null next-position)))
	       (while next-position
		 (goto-char next-position)
		 (if ,show-progress (sit-for 0))
		 (setq results (cons ,body results))
		 ;; move after last match
		 (goto-char next-position)
		 (forward-line 1)
		 (set-marker next-position nil)
		 (setq next-position (and (re-search-forward regexp nil t)
					  (point-marker)))))
	     (if (and ,distinguish-one-marked (= (length results) 1))
		 (setq results (cons t results)))
	     (if found
		 results
	       (list ,body)))))
     ;; save-excursion loses, again
     (dired-move-to-filename)))

(defun dired-get-marked-files (&optional localp arg filter distinguish-one-marked)
  "Return the marked files' names as list of strings.
The list is in the same order as the buffer, that is, the car is the
  first marked file.
Values returned are normally absolute file names.
Optional arg LOCALP as in `dired-get-filename'.
Optional second argument ARG, if non-nil, specifies files near
 point instead of marked files.  It usually comes from the prefix
 argument.
  If ARG is an integer, use the next ARG files.
  Any other non-nil value means to use the current file instead.
Optional third argument FILTER, if non-nil, is a function to select
  some of the files--those for which (funcall FILTER FILENAME) is non-nil.

If DISTINGUISH-ONE-MARKED is non-nil, then if we find just one marked file,
return (t FILENAME) instead of (FILENAME).
Don't use that together with FILTER."
  (let* ((all-of-them
	  (save-excursion
	    (dired-map-over-marks
	     (dired-get-filename localp)
	     arg nil distinguish-one-marked)))
	 result)
    (if (not filter)
	(if (and distinguish-one-marked (eq (car all-of-them) t))
	    all-of-them
	  (nreverse all-of-them))
      (dolist (file all-of-them)
	(if (funcall filter file)
	    (push file result)))
      result)))

;; The dired command

(defun dired-read-dir-and-switches (str)
  ;; For use in interactive.
  (reverse (list
	    (if current-prefix-arg
		(read-string "Dired listing switches: "
			     dired-listing-switches))
	    ;; If a dialog is used, call `read-directory-name' so the
	    ;; dialog code knows we want directories.  Some dialogs
	    ;; can only select directories or files when popped up,
	    ;; not both.  If no dialog is used, call `read-file-name'
	    ;; because the user may want completion of file names for
	    ;; use in a wildcard pattern.
	    (if (next-read-file-uses-dialog-p)
		(read-directory-name (format "Dired %s(directory): " str)
				     nil default-directory nil)
	      (read-file-name (format "Dired %s(directory): " str)
			      nil default-directory nil)))))

;; We want to switch to a more sophisticated version of
;; dired-read-dir-and-switches like the following, if there is a way
;; to make it more intuitive.  See bug#1285.

;; (defun dired-read-dir-and-switches (str)
;;   ;; For use in interactive.
;;   (reverse
;;    (list
;;     (if current-prefix-arg
;;         (read-string "Dired listing switches: "
;;                      dired-listing-switches))
;;     ;; If a dialog is about to be used, call read-directory-name so
;;     ;; the dialog code knows we want directories.  Some dialogs can
;;     ;; only select directories or files when popped up, not both.
;;     (if (next-read-file-uses-dialog-p)
;;         (read-directory-name (format "Dired %s(directory): " str)
;;                              nil default-directory nil)
;;       (let ((cie ()))
;;         (dolist (ext completion-ignored-extensions)
;;           (if (eq ?/ (aref ext (1- (length ext)))) (push ext cie)))
;;         (setq cie (concat (regexp-opt cie "\\(?:") "\\'"))
;;         (lexical-let* ((default (and buffer-file-name
;;                                      (abbreviate-file-name buffer-file-name)))
;;                        (cie cie)
;;                        (completion-table
;;                         ;; We need a mix of read-file-name and
;;                         ;; read-directory-name so that completion to directories
;;                         ;; is preferred, but if the user wants to enter a global
;;                         ;; pattern, he can still use completion on filenames to
;;                         ;; help him write the pattern.
;;                         ;; Essentially, we want to use
;;                         ;; (completion-table-with-predicate
;;                         ;;  'read-file-name-internal 'file-directory-p nil)
;;                         ;; but that doesn't work because read-file-name-internal
;;                         ;; does not obey its `predicate' argument.
;;                         (completion-table-in-turn
;;                          (lambda (str pred action)
;;                            (let ((read-file-name-predicate
;;                                   (lambda (f)
;;                                     (and (not (member f '("./" "../")))
;;                                          ;; Hack! Faster than file-directory-p!
;;                                          (eq (aref f (1- (length f))) ?/)
;;                                          (not (string-match cie f))))))
;;                              (complete-with-action
;;                               action 'read-file-name-internal str nil)))
;;                          'read-file-name-internal)))
;;           (minibuffer-with-setup-hook
;;               (lambda ()
;;                 (setq minibuffer-default default)
;;                 (setq minibuffer-completion-table completion-table))
;;             (read-file-name (format "Dired %s(directory): " str)
;;                             nil default-directory nil))))))))

(defun dired-file-name-at-point ()
  "Try to get a file name at point in the current dired buffer.
This hook is intended to be put in `file-name-at-point-functions'."
  (let ((filename (dired-get-filename nil t)))
    (when filename
      (if (file-directory-p filename)
	  (file-name-as-directory (abbreviate-file-name filename))
	(abbreviate-file-name filename)))))

;;;###autoload (define-key ctl-x-map "d" 'dired)
;;;###autoload
(defun dired (dirname &optional switches)
  "\"Edit\" directory DIRNAME--delete, rename, print, etc. some files in it.
Optional second argument SWITCHES specifies the `ls' options used.
\(Interactively, use a prefix argument to be able to specify SWITCHES.)
Dired displays a list of files in DIRNAME (which may also have
shell wildcards appended to select certain files).  If DIRNAME is a cons,
its first element is taken as the directory name and the rest as an explicit
list of files to make directory entries for.
\\<dired-mode-map>\
You can flag files for deletion with \\[dired-flag-file-deletion] and then
delete them by typing \\[dired-do-flagged-delete].
Type \\[describe-mode] after entering Dired for more info.

If DIRNAME is already in a dired buffer, that buffer is used without refresh."
  ;; Cannot use (interactive "D") because of wildcards.
  (interactive (dired-read-dir-and-switches ""))
  (switch-to-buffer (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-4-map "d" 'dired-other-window)
;;;###autoload
(defun dired-other-window (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but selects in another window."
  (interactive (dired-read-dir-and-switches "in other window "))
  (switch-to-buffer-other-window (dired-noselect dirname switches)))

;;;###autoload (define-key ctl-x-5-map "d" 'dired-other-frame)
;;;###autoload
(defun dired-other-frame (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame (dired-noselect dirname switches)))

;;;###autoload
(defun dired-noselect (dir-or-list &optional switches)
  "Like `dired' but returns the dired buffer as value, does not select it."
  (or dir-or-list (setq dir-or-list default-directory))
  ;; This loses the distinction between "/foo/*/" and "/foo/*" that
  ;; some shells make:
  (let (dirname initially-was-dirname)
    (if (consp dir-or-list)
	(setq dirname (car dir-or-list))
      (setq dirname dir-or-list))
    (setq initially-was-dirname
	  (string= (file-name-as-directory dirname) dirname))
    (setq dirname (abbreviate-file-name
		   (expand-file-name (directory-file-name dirname))))
    (if find-file-visit-truename
	(setq dirname (file-truename dirname)))
    ;; If the argument was syntactically  a directory name not a file name,
    ;; or if it happens to name a file that is a directory,
    ;; convert it syntactically to a directory name.
    ;; The reason for checking initially-was-dirname
    ;; and not just file-directory-p
    ;; is that file-directory-p is slow over ftp.
    (if (or initially-was-dirname (file-directory-p dirname))
	(setq dirname  (file-name-as-directory dirname)))
    (if (consp dir-or-list)
	(setq dir-or-list (cons dirname (cdr dir-or-list)))
      (setq dir-or-list dirname))
    (dired-internal-noselect dir-or-list switches)))

;; The following is an internal dired function.  It returns non-nil if
;; the directory visited by the current dired buffer has changed on
;; disk.  DIRNAME should be the directory name of that directory.
(defun dired-directory-changed-p (dirname)
  (not (let ((attributes (file-attributes dirname))
	     (modtime (visited-file-modtime)))
	 (or (eq modtime 0)
	     (not (eq (car attributes) t))
	     (equal (nth 5 attributes) modtime)))))

(defun dired-buffer-stale-p (&optional noconfirm)
  "Return non-nil if current dired buffer needs updating.
If NOCONFIRM is non-nil, then this function always returns nil
for a remote directory.  This feature is used by Auto Revert Mode."
  (let ((dirname
	 (if (consp dired-directory) (car dired-directory) dired-directory)))
    (and (stringp dirname)
	 (not (when noconfirm (file-remote-p dirname)))
	 (file-readable-p dirname)
	 ;; Do not auto-revert when the dired buffer can be currently
	 ;; written by the user as in `wdired-mode'.
	 buffer-read-only
	 (dired-directory-changed-p dirname))))

(defcustom dired-auto-revert-buffer nil
  "Automatically revert dired buffer on revisiting.
If t, revisiting an existing dired buffer automatically reverts it.
If its value is a function, call this function with the directory
name as single argument and revert the buffer if it returns non-nil.
Otherwise, a message offering to revert the changed dired buffer
is displayed.
Note that this is not the same as `auto-revert-mode' that
periodically reverts at specified time intervals."
  :type '(choice
          (const :tag "Don't revert" nil)
          (const :tag "Always revert visited dired buffer" t)
          (const :tag "Revert changed dired buffer" dired-directory-changed-p)
          (function :tag "Predicate function"))
  :group 'dired
  :version "23.2")

(defun dired-internal-noselect (dir-or-list &optional switches mode)
  ;; If there is an existing dired buffer for DIRNAME, just leave
  ;; buffer as it is (don't even call dired-revert).
  ;; This saves time especially for deep trees or with ange-ftp.
  ;; The user can type `g' easily, and it is more consistent with find-file.
  ;; But if SWITCHES are given they are probably different from the
  ;; buffer's old value, so call dired-sort-other, which does
  ;; revert the buffer.
  ;; A pity we can't possibly do "Directory has changed - refresh? "
  ;; like find-file does.
  ;; Optional argument MODE is passed to dired-find-buffer-nocreate,
  ;; see there.
  (let* ((old-buf (current-buffer))
	 (dirname (if (consp dir-or-list) (car dir-or-list) dir-or-list))
         ;; Look for an existing buffer.
         (buffer (dired-find-buffer-nocreate dirname mode))
	 ;; Note that buffer already is in dired-mode, if found.
	 (new-buffer-p (null buffer)))
    (or buffer
        (setq buffer (create-file-buffer (directory-file-name dirname))))
    (set-buffer buffer)
    (if (not new-buffer-p)		; existing buffer ...
	(cond (switches			; ... but new switches
	       ;; file list may have changed
	       (setq dired-directory dir-or-list)
	       ;; this calls dired-revert
	       (dired-sort-other switches))
	      ;; Always revert regardless of whether it has changed or not.
	      ((eq dired-auto-revert-buffer t)
	       (revert-buffer))
	      ;; Revert when predicate function returns non-nil.
	      ((functionp dired-auto-revert-buffer)
	       (when (funcall dired-auto-revert-buffer dirname)
		 (revert-buffer)
		 (message "Changed directory automatically updated")))
	      ;; If directory has changed on disk, offer to revert.
	      ((when (dired-directory-changed-p dirname)
		 (message "%s"
			  (substitute-command-keys
			   "Directory has changed on disk; type \\[revert-buffer] to update Dired")))))
      ;; Else a new buffer
      (setq default-directory
	    ;; We can do this unconditionally
	    ;; because dired-noselect ensures that the name
	    ;; is passed in directory name syntax
	    ;; if it was the name of a directory at all.
	    (file-name-directory dirname))
      (or switches (setq switches dired-listing-switches))
      (if mode (funcall mode)
        (dired-mode dir-or-list switches))
      ;; default-directory and dired-actual-switches are set now
      ;; (buffer-local), so we can call dired-readin:
      (let ((failed t))
	(unwind-protect
	    (progn (dired-readin)
		   (setq failed nil))
	  ;; dired-readin can fail if parent directories are inaccessible.
	  ;; Don't leave an empty buffer around in that case.
	  (if failed (kill-buffer buffer))))
      (goto-char (point-min))
      (dired-initial-position dirname))
    (set-buffer old-buf)
    buffer))

(defvar dired-buffers nil
  ;; Enlarged by dired-advertise
  ;; Queried by function dired-buffers-for-dir. When this detects a
  ;; killed buffer, it is removed from this list.
  "Alist of expanded directories and their associated dired buffers.")

(defvar dired-find-subdir)

;; FIXME add a doc-string, and document dired-x extensions.
(defun dired-find-buffer-nocreate (dirname &optional mode)
  ;; This differs from dired-buffers-for-dir in that it does not consider
  ;; subdirs of default-directory and searches for the first match only.
  ;; Also, the major mode must be MODE.
  (if (and (featurep 'dired-x)
           dired-find-subdir
           ;; Don't try to find a wildcard as a subdirectory.
	   (string-equal dirname (file-name-directory dirname)))
      (let* ((cur-buf (current-buffer))
	     (buffers (nreverse
		       (dired-buffers-for-dir (expand-file-name dirname))))
	     (cur-buf-matches (and (memq cur-buf buffers)
				   ;; Wildcards must match, too:
				   (equal dired-directory dirname))))
	;; We don't want to switch to the same buffer---
	(setq buffers (delq cur-buf buffers))
	(or (car (sort buffers #'dired-buffer-more-recently-used-p))
	    ;; ---unless it's the only possibility:
	    (and cur-buf-matches cur-buf)))
    ;; No dired-x, or dired-find-subdir nil.
    (setq dirname (expand-file-name dirname))
    (let (found (blist dired-buffers))    ; was (buffer-list)
      (or mode (setq mode 'dired-mode))
      (while blist
        (if (null (buffer-name (cdr (car blist))))
            (setq blist (cdr blist))
          (with-current-buffer (cdr (car blist))
            (if (and (eq major-mode mode)
                     dired-directory  ;; nil during find-alternate-file
                     (equal dirname
                            (expand-file-name
                             (if (consp dired-directory)
                                 (car dired-directory)
                               dired-directory))))
                (setq found (cdr (car blist))
                      blist nil)
              (setq blist (cdr blist))))))
      found)))


;; Read in a new dired buffer

(defun dired-readin ()
  "Read in a new dired buffer.
Differs from `dired-insert-subdir' in that it accepts
wildcards, erases the buffer, and builds the subdir-alist anew
\(including making it buffer-local and clearing it first)."

  ;; default-directory and dired-actual-switches must be buffer-local
  ;; and initialized by now.
  (let (dirname
	;; This makes readin much much faster.
	;; In particular, it prevents the font lock hook from running
	;; until the directory is all read in.
	(inhibit-modification-hooks t))
    (if (consp dired-directory)
	(setq dirname (car dired-directory))
      (setq dirname dired-directory))
    (setq dirname (expand-file-name dirname))
    (save-excursion
      ;; This hook which may want to modify dired-actual-switches
      ;; based on dired-directory, e.g. with ange-ftp to a SysV host
      ;; where ls won't understand -Al switches.
      (run-hooks 'dired-before-readin-hook)
      (if (consp buffer-undo-list)
	  (setq buffer-undo-list nil))
      (make-local-variable 'file-name-coding-system)
      (setq file-name-coding-system
	    (or coding-system-for-read file-name-coding-system))
      (let ((inhibit-read-only t)
	    ;; Don't make undo entries for readin.
	    (buffer-undo-list t))
	(widen)
	(erase-buffer)
	(dired-readin-insert))
      (goto-char (point-min))
      ;; Must first make alist buffer local and set it to nil because
      ;; dired-build-subdir-alist will call dired-clear-alist first
      (set (make-local-variable 'dired-subdir-alist) nil)
      (dired-build-subdir-alist)
      (let ((attributes (file-attributes dirname)))
	(if (eq (car attributes) t)
	    (set-visited-file-modtime (nth 5 attributes))))
      (set-buffer-modified-p nil)
      ;; No need to narrow since the whole buffer contains just
      ;; dired-readin's output, nothing else.  The hook can
      ;; successfully use dired functions (e.g. dired-get-filename)
      ;; as the subdir-alist has been built in dired-readin.
      (run-hooks 'dired-after-readin-hook))))

;; Subroutines of dired-readin

(defun dired-readin-insert ()
  ;; Insert listing for the specified dir (and maybe file list)
  ;; already in dired-directory, assuming a clean buffer.
  (let (dir file-list)
    (if (consp dired-directory)
	(setq dir (car dired-directory)
	      file-list (cdr dired-directory))
      (setq dir dired-directory
	    file-list nil))
    (setq dir (expand-file-name dir))
    (if (and (equal "" (file-name-nondirectory dir))
	     (not file-list))
	;; If we are reading a whole single directory...
	(dired-insert-directory dir dired-actual-switches nil nil t)
      (if (not (file-readable-p
		(directory-file-name (file-name-directory dir))))
	  (error "Directory %s inaccessible or nonexistent" dir)
	;; Else treat it as a wildcard spec
	;; unless we have an explicit list of files.
	(dired-insert-directory dir dired-actual-switches
				file-list (not file-list) t)))))

(defun dired-align-file (beg end)
  "Align the fields of a file to the ones of surrounding lines.
BEG..END is the line where the file info is located."
  ;; Some versions of ls try to adjust the size of each field so as to just
  ;; hold the largest element ("largest" in the current invocation, of
  ;; course).  So when a single line is output, the size of each field is
  ;; just big enough for that one output.  Thus when dired refreshes one
  ;; line, the alignment if this line w.r.t the rest is messed up because
  ;; the fields of that one line will generally be smaller.
  ;;
  ;; To work around this problem, we here add spaces to try and
  ;; re-align the fields as needed.  Since this is purely aesthetic,
  ;; it is of utmost importance that it doesn't mess up anything like
  ;; `dired-move-to-filename'.  To this end, we limit ourselves to
  ;; adding spaces only, and to only add them at places where there
  ;; was already at least one space.  This way, as long as
  ;; `directory-listing-before-filename-regexp' always matches spaces
  ;; with "*" or "+", we know we haven't made anything worse.  There
  ;; is one spot where the exact number of spaces is important, which
  ;; is just before the actual filename, so we refrain from adding
  ;; spaces there (and within the filename as well, of course).
  (save-excursion
    (let (file file-col other other-col)
      ;; Check that there is indeed a file, and that there is another adjacent
      ;; file with which to align, and that additional spaces are needed to
      ;; align the filenames.
      (when (and (setq file (progn (goto-char beg)
				   (dired-move-to-filename nil end)))
		 (setq file-col (current-column))
		 (setq other
		       (or (and (goto-char beg)
				(zerop (forward-line -1))
				(dired-move-to-filename))
			   (and (goto-char beg)
				(zerop (forward-line 1))
				(dired-move-to-filename))))
		 (setq other-col (current-column))
		 (/= file other)
		 ;; Make sure there is some work left to do.
		 (> other-col file-col))
	;; If we've only looked at the line above, check to see if the line
	;; below exists as well and if so, align with the shorter one.
	(when (and (< other file)
		   (goto-char beg)
		   (zerop (forward-line 1))
		   (dired-move-to-filename))
	  (let ((alt-col (current-column)))
	    (when (< alt-col other-col)
	      (setq other-col alt-col)
	      (setq other (point)))))
	;; Keep positions uptodate when we insert stuff.
	(if (> other file) (setq other (copy-marker other)))
	(setq file (copy-marker file))
	;; Main loop.
	(goto-char beg)
	(skip-chars-forward " ")	;Skip to the first field.
	(while (and (> other-col file-col)
		    ;; Don't touch anything just before (and after) the
		    ;; beginning of the filename.
		    (> file (point)))
	  ;; We're now just in front of a field, with a space behind us.
	  (let* ((curcol (current-column))
		 ;; Nums are right-aligned.
		 (num-align (looking-at "[0-9]"))
		 ;; Let's look at the other line, in the same column: we
		 ;; should be either near the end of the previous field, or
		 ;; in the space between that field and the next.
		 ;; [ Of course, it's also possible that we're already within
		 ;; the next field or even past it, but that's unlikely since
		 ;; other-col > file-col. ]
		 ;; Let's find the distance to the alignment-point (either
		 ;; the beginning or the end of the next field, depending on
		 ;; whether this field is left or right aligned).
		 (align-pt-offset
		  (save-excursion
		    (goto-char other)
		    (move-to-column curcol)
		    (when (looking-at
			   (concat
			    (if (eq (char-before) ?\s) " *" "[^ ]* *")
			    (if num-align "[0-9][^ ]*")))
		      (- (match-end 0) (match-beginning 0)))))
		 ;; Now, the number of spaces to insert is align-pt-offset
		 ;; minus the distance to the equivalent point on the
		 ;; current line.
		 (spaces
		  (if (not num-align)
		      align-pt-offset
		    (and align-pt-offset
			 (save-excursion
			   (skip-chars-forward "^ ")
			   (- align-pt-offset (- (current-column) curcol)))))))
	    (when (and spaces (> spaces 0))
	      (setq file-col (+ spaces file-col))
	      (if (> file-col other-col)
		  (setq spaces (- spaces (- file-col other-col))))
	      (insert-char ?\s spaces)
	      ;; Let's just make really sure we did not mess up.
	      (unless (save-excursion
			(eq (dired-move-to-filename) (marker-position file)))
		;; Damn!  We messed up: let's revert the change.
		(delete-char (- spaces)))))
	  ;; Now skip to next field.
	  (skip-chars-forward "^ ") (skip-chars-forward " "))
	(set-marker file nil)))))


(defvar ls-lisp-use-insert-directory-program)

(defun dired-switches-escape-p (switches)
  "Return non-nil if the string SWITCHES contains -b or --escape."
  ;; Do not match things like "--block-size" that happen to contain "b".
  (string-match "\\(\\`\\| \\)-[[:alnum:]]*b\\|--escape\\>" switches))

(defun dired-insert-directory (dir switches &optional file-list wildcard hdr)
  "Insert a directory listing of DIR, Dired style.
Use SWITCHES to make the listings.
If FILE-LIST is non-nil, list only those files.
Otherwise, if WILDCARD is non-nil, expand wildcards;
 in that case, DIR should be a file name that uses wildcards.
In other cases, DIR should be a directory name or a directory filename.
If HDR is non-nil, insert a header line with the directory name."
  (let ((opoint (point))
	(process-environment (copy-sequence process-environment))
	end)
    (if (and
	 ;; Don't try to invoke `ls' if we are on DOS/Windows where
	 ;; ls-lisp emulation is used, except if they want to use `ls'
	 ;; as indicated by `ls-lisp-use-insert-directory-program'.
	 (not (and (featurep 'ls-lisp)
		   (null ls-lisp-use-insert-directory-program)))
	 (or (if (eq dired-use-ls-dired 'unspecified)
		 ;; Check whether "ls --dired" gives exit code 0, and
		 ;; save the answer in `dired-use-ls-dired'.
		 (or (setq dired-use-ls-dired
			   (eq 0 (call-process insert-directory-program
					     nil nil nil "--dired")))
		     (progn
		       (message "ls does not support --dired; \
see `dired-use-ls-dired' for more details.")
		       nil))
	       dired-use-ls-dired)
	     (file-remote-p dir)))
	(setq switches (concat "--dired " switches)))
    ;; We used to specify the C locale here, to force English month names;
    ;; but this should not be necessary any more,
    ;; with the new value of `directory-listing-before-filename-regexp'.
    (if file-list
	(dolist (f file-list)
	  (let ((beg (point)))
	    (insert-directory f switches nil nil)
	    ;; Re-align fields, if necessary.
	    (dired-align-file beg (point))))
      (insert-directory dir switches wildcard (not wildcard)))
    ;; Quote certain characters, unless ls quoted them for us.
    (if (not (dired-switches-escape-p dired-actual-switches))
	(save-excursion
	  (setq end (point-marker))
	  (goto-char opoint)
	  (while (search-forward "\\" end t)
	    (replace-match (apply #'propertize
				  "\\\\"
				  (text-properties-at (match-beginning 0)))
			   nil t))
	  (goto-char opoint)
	  (while (search-forward "\^m" end t)
	    (replace-match (apply #'propertize
				  "\\015"
				  (text-properties-at (match-beginning 0)))
			   nil t))
	  (set-marker end nil))
      ;; Replace any newlines in DIR with literal "\n"s, for the sake
      ;; of the header line.  To disambiguate a literal "\n" in the
      ;; actual dirname, we also replace "\" with "\\".
      ;; Personally, I think this should always be done, irrespective
      ;; of the value of dired-actual-switches, because:
      ;;  i) Dired simply does not work with an unescaped newline in
      ;;  the directory name used in the header (bug=10469#28), and
      ;;  ii) "\" is always replaced with "\\" in the listing, so doing
      ;;  it in the header as well makes things consistent.
      ;; But at present it is only done if "-b" is in ls-switches,
      ;; because newlines in dirnames are uncommon, and people may
      ;; have gotten used to seeing unescaped "\" in the headers.
      ;; Note: adjust dired-build-subdir-alist if you change this.
      (setq dir (replace-regexp-in-string "\\\\" "\\\\" dir nil t)
            dir (replace-regexp-in-string "\n" "\\n" dir nil t)))
    (dired-insert-set-properties opoint (point))
    ;; If we used --dired and it worked, the lines are already indented.
    ;; Otherwise, indent them.
    (unless (save-excursion
	      (goto-char opoint)
	      (looking-at "  "))
      (let ((indent-tabs-mode nil))
	(indent-rigidly opoint (point) 2)))
    ;; Insert text at the beginning to standardize things.
    (save-excursion
      (goto-char opoint)
      (if (and (or hdr wildcard)
               (not (and (looking-at "^  \\(.*\\):$")
                         (file-name-absolute-p (match-string 1)))))
	  ;; Note that dired-build-subdir-alist will replace the name
	  ;; by its expansion, so it does not matter whether what we insert
	  ;; here is fully expanded, but it should be absolute.
	  (insert "  " (directory-file-name (file-name-directory dir)) ":\n"))
      (when wildcard
	;; Insert "wildcard" line where "total" line would be for a full dir.
	(insert "  wildcard " (file-name-nondirectory dir) "\n")))))

(defun dired-insert-set-properties (beg end)
  "Add various text properties to the lines in the region."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (condition-case nil
	  (if (dired-move-to-filename)
	      (add-text-properties
	       (point)
	       (save-excursion
		 (dired-move-to-end-of-filename)
		 (point))
	       '(mouse-face highlight
		 dired-filename t
		 help-echo "mouse-2: visit this file in other window")))
	(error nil))
      (forward-line 1))))

;; Reverting a dired buffer

(defun dired-revert (&optional _arg _noconfirm)
  "Reread the dired buffer.
Must also be called after `dired-actual-switches' have changed.
Should not fail even on completely garbaged buffers.
Preserves old cursor, marks/flags, hidden-p.

Dired sets `revert-buffer-function' to this function.  The args
ARG and NOCONFIRM, passed from `revert-buffer', are ignored."
  (widen)				; just in case user narrowed
  (let ((modflag (buffer-modified-p))
	(positions (dired-save-positions))
	(mark-alist nil)		; save marked files
	(hidden-subdirs (dired-remember-hidden))
	(old-subdir-alist (cdr (reverse dired-subdir-alist))) ; except pwd
	(case-fold-search nil)		; we check for upper case ls flags
	(inhibit-read-only t))
    (goto-char (point-min))
    (setq mark-alist;; only after dired-remember-hidden since this unhides:
	  (dired-remember-marks (point-min) (point-max)))
    ;; treat top level dir extra (it may contain wildcards)
    (if (not (consp dired-directory))
	(dired-uncache dired-directory)
      (dired-uncache (car dired-directory))
      (dolist (dir (cdr dired-directory))
	(if (file-name-absolute-p dir)
	    (dired-uncache dir))))
    ;; Run dired-after-readin-hook just once, below.
    (let ((dired-after-readin-hook nil))
      (dired-readin)
      (dired-insert-old-subdirs old-subdir-alist))
    (dired-mark-remembered mark-alist)	; mark files that were marked
    ;; ... run the hook for the whole buffer, and only after markers
    ;; have been reinserted (else omitting in dired-x would omit marked files)
    (run-hooks 'dired-after-readin-hook)	; no need to narrow
    (dired-restore-positions positions)
    (save-excursion			; hide subdirs that were hidden
      (dolist (dir hidden-subdirs)
	(if (dired-goto-subdir dir)
	    (dired-hide-subdir 1))))
    (unless modflag (restore-buffer-modified-p nil)))
  ;; outside of the let scope
;;;  Might as well not override the user if the user changed this.
;;;  (setq buffer-read-only t)
  )

;; Subroutines of dired-revert
;; Some of these are also used when inserting subdirs.

(defun dired-save-positions ()
  "Return current positions in the buffer and all windows with this directory.
The positions have the form (BUFFER-POSITION WINDOW-POSITIONS).

BUFFER-POSITION is the point position in the current dired buffer.
It has the form (BUFFER DIRED-FILENAME BUFFER-POINT).

WINDOW-POSITIONS are current positions in all windows displaying
this dired buffer.  The window positions have the form (WINDOW
DIRED-FILENAME WINDOW-POINT)."
  (list
   (list (current-buffer) (dired-get-filename nil t) (point))
   (mapcar (lambda (w)
	     (list w
		   (with-selected-window w
		     (dired-get-filename nil t))
		   (window-point w)))
	   (get-buffer-window-list nil 0 t))))

(defun dired-restore-positions (positions)
  "Restore POSITIONS saved with `dired-save-positions'."
  (let* ((buf-file-pos (nth 0 positions))
	 (buffer (nth 0 buf-file-pos)))
    (unless (and (nth 1 buf-file-pos)
		 (dired-goto-file (nth 1 buf-file-pos)))
      (goto-char (nth 2 buf-file-pos))
      (dired-move-to-filename))
    (dolist (win-file-pos (nth 1 positions))
      ;; Ensure that window still displays the original buffer.
      (when (eq (window-buffer (nth 0 win-file-pos)) buffer)
	(with-selected-window (nth 0 win-file-pos)
	  (unless (and (nth 1 win-file-pos)
		       (dired-goto-file (nth 1 win-file-pos)))
	    (goto-char (nth 2 win-file-pos))
	    (dired-move-to-filename)))))))

(defun dired-remember-marks (beg end)
  "Return alist of files and their marks, from BEG to END."
  (if selective-display			; must unhide to make this work.
      (let ((inhibit-read-only t))
	(subst-char-in-region beg end ?\r ?\n)))
  (let (fil chr alist)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward dired-re-mark end t)
	(if (setq fil (dired-get-filename nil t))
	    (setq chr (preceding-char)
		  alist (cons (cons fil chr) alist)))))
    alist))

(defun dired-mark-remembered (alist)
  "Mark all files remembered in ALIST.
Each element of ALIST looks like (FILE . MARKERCHAR)."
  (let (elt fil chr)
    (while alist
      (setq elt (car alist)
	    alist (cdr alist)
	    fil (car elt)
	    chr (cdr elt))
      (if (dired-goto-file fil)
	  (save-excursion
	    (beginning-of-line)
	    (delete-char 1)
	    (insert chr))))))

(defun dired-remember-hidden ()
  "Return a list of names of subdirs currently hidden."
  (let ((l dired-subdir-alist) dir pos result)
    (while l
      (setq dir (car (car l))
	    pos (cdr (car l))
	    l (cdr l))
      (goto-char pos)
      (skip-chars-forward "^\r\n")
      (if (eq (following-char) ?\r)
	  (setq result (cons dir result))))
    result))

(defun dired-insert-old-subdirs (old-subdir-alist)
  "Try to insert all subdirs that were displayed before.
Do so according to the former subdir alist OLD-SUBDIR-ALIST."
  (or (string-match "R" dired-actual-switches)
      (let (elt dir)
	(while old-subdir-alist
	  (setq elt (car old-subdir-alist)
		old-subdir-alist (cdr old-subdir-alist)
		dir (car elt))
	  (condition-case ()
	      (progn
		(dired-uncache dir)
		(dired-insert-subdir dir))
	    (error nil))))))

(defun dired-uncache (dir)
  "Remove directory DIR from any directory cache."
  (let ((handler (find-file-name-handler dir 'dired-uncache)))
    (if handler
	(funcall handler 'dired-uncache dir))))

;; dired mode key bindings and initialization

(defvar dired-mode-map
  ;; This looks ugly when substitute-command-keys uses C-d instead d:
  ;;  (define-key dired-mode-map "\C-d" 'dired-flag-file-deletion)
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [mouse-2] 'dired-mouse-find-file-other-window)
    (define-key map [follow-link] 'mouse-face)
    ;; Commands to mark or flag certain categories of files
    (define-key map "#" 'dired-flag-auto-save-files)
    (define-key map "." 'dired-clean-directory)
    (define-key map "~" 'dired-flag-backup-files)
    ;; Upper case keys (except !) for operating on the marked files
    (define-key map "A" 'dired-do-search)
    (define-key map "C" 'dired-do-copy)
    (define-key map "B" 'dired-do-byte-compile)
    (define-key map "D" 'dired-do-delete)
    (define-key map "G" 'dired-do-chgrp)
    (define-key map "H" 'dired-do-hardlink)
    (define-key map "L" 'dired-do-load)
    (define-key map "M" 'dired-do-chmod)
    (define-key map "O" 'dired-do-chown)
    (define-key map "P" 'dired-do-print)
    (define-key map "Q" 'dired-do-query-replace-regexp)
    (define-key map "R" 'dired-do-rename)
    (define-key map "S" 'dired-do-symlink)
    (define-key map "T" 'dired-do-touch)
    (define-key map "X" 'dired-do-shell-command)
    (define-key map "Z" 'dired-do-compress)
    (define-key map "!" 'dired-do-shell-command)
    (define-key map "&" 'dired-do-async-shell-command)
    ;; Comparison commands
    (define-key map "=" 'dired-diff)
    (define-key map "\M-=" 'dired-backup-diff)
    ;; Tree Dired commands
    (define-key map "\M-\C-?" 'dired-unmark-all-files)
    (define-key map "\M-\C-d" 'dired-tree-down)
    (define-key map "\M-\C-u" 'dired-tree-up)
    (define-key map "\M-\C-n" 'dired-next-subdir)
    (define-key map "\M-\C-p" 'dired-prev-subdir)
    ;; move to marked files
    (define-key map "\M-{" 'dired-prev-marked-file)
    (define-key map "\M-}" 'dired-next-marked-file)
    ;; Make all regexp commands share a `%' prefix:
    ;; We used to get to the submap via a symbol dired-regexp-prefix,
    ;; but that seems to serve little purpose, and copy-keymap
    ;; does a better job without it.
    (define-key map "%" nil)
    (define-key map "%u" 'dired-upcase)
    (define-key map "%l" 'dired-downcase)
    (define-key map "%d" 'dired-flag-files-regexp)
    (define-key map "%g" 'dired-mark-files-containing-regexp)
    (define-key map "%m" 'dired-mark-files-regexp)
    (define-key map "%r" 'dired-do-rename-regexp)
    (define-key map "%C" 'dired-do-copy-regexp)
    (define-key map "%H" 'dired-do-hardlink-regexp)
    (define-key map "%R" 'dired-do-rename-regexp)
    (define-key map "%S" 'dired-do-symlink-regexp)
    (define-key map "%&" 'dired-flag-garbage-files)
    ;; Commands for marking and unmarking.
    (define-key map "*" nil)
    (define-key map "**" 'dired-mark-executables)
    (define-key map "*/" 'dired-mark-directories)
    (define-key map "*@" 'dired-mark-symlinks)
    (define-key map "*%" 'dired-mark-files-regexp)
    (define-key map "*c" 'dired-change-marks)
    (define-key map "*s" 'dired-mark-subdir-files)
    (define-key map "*m" 'dired-mark)
    (define-key map "*u" 'dired-unmark)
    (define-key map "*?" 'dired-unmark-all-files)
    (define-key map "*!" 'dired-unmark-all-marks)
    (define-key map "U" 'dired-unmark-all-marks)
    (define-key map "*\177" 'dired-unmark-backward)
    (define-key map "*\C-n" 'dired-next-marked-file)
    (define-key map "*\C-p" 'dired-prev-marked-file)
    (define-key map "*t" 'dired-toggle-marks)
    ;; Lower keys for commands not operating on all the marked files
    (define-key map "a" 'dired-find-alternate-file)
    (define-key map "d" 'dired-flag-file-deletion)
    (define-key map "e" 'dired-find-file)
    (define-key map "f" 'dired-find-file)
    (define-key map "\C-m" 'dired-find-file)
    (put 'dired-find-file :advertised-binding "\C-m")
    (define-key map "g" 'revert-buffer)
    (define-key map "i" 'dired-maybe-insert-subdir)
    (define-key map "j" 'dired-goto-file)
    (define-key map "k" 'dired-do-kill-lines)
    (define-key map "l" 'dired-do-redisplay)
    (define-key map "m" 'dired-mark)
    (define-key map "n" 'dired-next-line)
    (define-key map "o" 'dired-find-file-other-window)
    (define-key map "\C-o" 'dired-display-file)
    (define-key map "p" 'dired-previous-line)
    (define-key map "s" 'dired-sort-toggle-or-edit)
    (define-key map "t" 'dired-toggle-marks)
    (define-key map "u" 'dired-unmark)
    (define-key map "v" 'dired-view-file)
    (define-key map "w" 'dired-copy-filename-as-kill)
    (define-key map "x" 'dired-do-flagged-delete)
    (define-key map "y" 'dired-show-file-type)
    (define-key map "+" 'dired-create-directory)
    ;; moving
    (define-key map "<" 'dired-prev-dirline)
    (define-key map ">" 'dired-next-dirline)
    (define-key map "^" 'dired-up-directory)
    (define-key map " "  'dired-next-line)
    (define-key map [remap next-line] 'dired-next-line)
    (define-key map [remap previous-line] 'dired-previous-line)
    ;; hiding
    (define-key map "$" 'dired-hide-subdir)
    (define-key map "\M-$" 'dired-hide-all)
    ;; isearch
    (define-key map (kbd "M-s a C-s")   'dired-do-isearch)
    (define-key map (kbd "M-s a M-C-s") 'dired-do-isearch-regexp)
    (define-key map (kbd "M-s f C-s")   'dired-isearch-filenames)
    (define-key map (kbd "M-s f M-C-s") 'dired-isearch-filenames-regexp)
    ;; misc
    (define-key map [remap toggle-read-only] 'dired-toggle-read-only)
    (define-key map "?" 'dired-summary)
    (define-key map "\177" 'dired-unmark-backward)
    (define-key map [remap undo] 'dired-undo)
    (define-key map [remap advertised-undo] 'dired-undo)
    ;; thumbnail manipulation (image-dired)
    (define-key map "\C-td" 'image-dired-display-thumbs)
    (define-key map "\C-tt" 'image-dired-tag-files)
    (define-key map "\C-tr" 'image-dired-delete-tag)
    (define-key map "\C-tj" 'image-dired-jump-thumbnail-buffer)
    (define-key map "\C-ti" 'image-dired-dired-display-image)
    (define-key map "\C-tx" 'image-dired-dired-display-external)
    (define-key map "\C-ta" 'image-dired-display-thumbs-append)
    (define-key map "\C-t." 'image-dired-display-thumb)
    (define-key map "\C-tc" 'image-dired-dired-comment-files)
    (define-key map "\C-tf" 'image-dired-mark-tagged-files)
    (define-key map "\C-t\C-t" 'image-dired-dired-toggle-marked-thumbs)
    (define-key map "\C-te" 'image-dired-dired-edit-comment-and-tags)
    ;; encryption and decryption (epa-dired)
    (define-key map ":d" 'epa-dired-do-decrypt)
    (define-key map ":v" 'epa-dired-do-verify)
    (define-key map ":s" 'epa-dired-do-sign)
    (define-key map ":e" 'epa-dired-do-encrypt)

    ;; Make menu bar items.

    ;; No need to fo this, now that top-level items are fewer.
    ;;;;
    ;; Get rid of the Edit menu bar item to save space.
    ;(define-key map [menu-bar edit] 'undefined)

    (define-key map [menu-bar subdir]
      (cons "Subdir" (make-sparse-keymap "Subdir")))

    (define-key map [menu-bar subdir hide-all]
      '(menu-item "Hide All" dired-hide-all
		  :help "Hide all subdirectories, leave only header lines"))
    (define-key map [menu-bar subdir hide-subdir]
      '(menu-item "Hide/UnHide Subdir" dired-hide-subdir
		  :help "Hide or unhide current directory listing"))
    (define-key map [menu-bar subdir tree-down]
      '(menu-item "Tree Down" dired-tree-down
		  :help "Go to first subdirectory header down the tree"))
    (define-key map [menu-bar subdir tree-up]
      '(menu-item "Tree Up" dired-tree-up
		  :help "Go to first subdirectory header up the tree"))
    (define-key map [menu-bar subdir up]
      '(menu-item "Up Directory" dired-up-directory
		  :help "Edit the parent directory"))
    (define-key map [menu-bar subdir prev-subdir]
      '(menu-item "Prev Subdir" dired-prev-subdir
		  :help "Go to previous subdirectory header line"))
    (define-key map [menu-bar subdir next-subdir]
      '(menu-item "Next Subdir" dired-next-subdir
		  :help "Go to next subdirectory header line"))
    (define-key map [menu-bar subdir prev-dirline]
      '(menu-item "Prev Dirline" dired-prev-dirline
		  :help "Move to next directory-file line"))
    (define-key map [menu-bar subdir next-dirline]
      '(menu-item "Next Dirline" dired-next-dirline
		  :help "Move to previous directory-file line"))
    (define-key map [menu-bar subdir insert]
      '(menu-item "Insert This Subdir" dired-maybe-insert-subdir
		  :help "Insert contents of subdirectory"
		  :enable (let ((f (dired-get-filename nil t)))
			    (and f (file-directory-p f)))))
    (define-key map [menu-bar immediate]
      (cons "Immediate" (make-sparse-keymap "Immediate")))

    (define-key map
      [menu-bar immediate image-dired-dired-display-external]
      '(menu-item "Display Image Externally" image-dired-dired-display-external
                  :help "Display image in external viewer"))
    (define-key map
      [menu-bar immediate image-dired-dired-display-image]
      '(menu-item "Display Image" image-dired-dired-display-image
                  :help "Display sized image in a separate window"))
    (define-key map
      [menu-bar immediate image-dired-dired-toggle-marked-thumbs]
      '(menu-item "Toggle Image Thumbnails in This Buffer" image-dired-dired-toggle-marked-thumbs
                  :help "Add or remove image thumbnails in front of marked file names"))

    (define-key map [menu-bar immediate revert-buffer]
      '(menu-item "Refresh" revert-buffer
		  :help "Update contents of shown directories"))

    (define-key map [menu-bar immediate dashes]
      '("--"))

    (define-key map [menu-bar immediate isearch-filenames-regexp]
      '(menu-item "Isearch Regexp in File Names..." dired-isearch-filenames-regexp
		  :help "Incrementally search for regexp in file names only"))
    (define-key map [menu-bar immediate isearch-filenames]
      '(menu-item "Isearch in File Names..." dired-isearch-filenames
		  :help "Incrementally search for string in file names only."))
    (define-key map [menu-bar immediate compare-directories]
      '(menu-item "Compare Directories..." dired-compare-directories
		  :help "Mark files with different attributes in two dired buffers"))
    (define-key map [menu-bar immediate backup-diff]
      '(menu-item "Compare with Backup" dired-backup-diff
		  :help "Diff file at cursor with its latest backup"))
    (define-key map [menu-bar immediate diff]
      '(menu-item "Diff..." dired-diff
		  :help "Compare file at cursor with another file"))
    (define-key map [menu-bar immediate view]
      '(menu-item "View This File" dired-view-file
		  :help "Examine file at cursor in read-only mode"))
    (define-key map [menu-bar immediate display]
      '(menu-item "Display in Other Window" dired-display-file
		  :help "Display file at cursor in other window"))
    (define-key map [menu-bar immediate find-file-other-window]
      '(menu-item "Find in Other Window" dired-find-file-other-window
		  :help "Edit file at cursor in other window"))
    (define-key map [menu-bar immediate find-file]
      '(menu-item "Find This File" dired-find-file
		  :help "Edit file at cursor"))
    (define-key map [menu-bar immediate create-directory]
      '(menu-item "Create Directory..." dired-create-directory
		  :help "Create a directory"))
    (define-key map [menu-bar immediate wdired-mode]
      '(menu-item "Edit File Names" wdired-change-to-wdired-mode
		  :help "Put a dired buffer in a mode in which filenames are editable"
		  :keys "C-x C-q"
		  :filter (lambda (x) (if (eq major-mode 'dired-mode) x))))

    (define-key map [menu-bar regexp]
      (cons "Regexp" (make-sparse-keymap "Regexp")))

    (define-key map
      [menu-bar regexp image-dired-mark-tagged-files]
      '(menu-item "Mark From Image Tag..." image-dired-mark-tagged-files
                  :help "Mark files whose image tags matches regexp"))

    (define-key map [menu-bar regexp dashes-1]
      '("--"))

    (define-key map [menu-bar regexp downcase]
      '(menu-item "Downcase" dired-downcase
		  ;; When running on plain MS-DOS, there's only one
		  ;; letter-case for file names.
		  :enable (or (not (fboundp 'msdos-long-file-names))
			      (msdos-long-file-names))
		  :help "Rename marked files to lower-case name"))
    (define-key map [menu-bar regexp upcase]
      '(menu-item "Upcase" dired-upcase
		  :enable (or (not (fboundp 'msdos-long-file-names))
			      (msdos-long-file-names))
		  :help "Rename marked files to upper-case name"))
    (define-key map [menu-bar regexp hardlink]
      '(menu-item "Hardlink..." dired-do-hardlink-regexp
		  :help "Make hard links for files matching regexp"))
    (define-key map [menu-bar regexp symlink]
      '(menu-item "Symlink..." dired-do-symlink-regexp
		  :visible (fboundp 'make-symbolic-link)
		  :help "Make symbolic links for files matching regexp"))
    (define-key map [menu-bar regexp rename]
      '(menu-item "Rename..." dired-do-rename-regexp
		  :help "Rename marked files matching regexp"))
    (define-key map [menu-bar regexp copy]
      '(menu-item "Copy..." dired-do-copy-regexp
		  :help "Copy marked files matching regexp"))
    (define-key map [menu-bar regexp flag]
      '(menu-item "Flag..." dired-flag-files-regexp
		  :help "Flag files matching regexp for deletion"))
    (define-key map [menu-bar regexp mark]
      '(menu-item "Mark..." dired-mark-files-regexp
		  :help "Mark files matching regexp for future operations"))
    (define-key map [menu-bar regexp mark-cont]
      '(menu-item "Mark Containing..." dired-mark-files-containing-regexp
		  :help "Mark files whose contents matches regexp"))

    (define-key map [menu-bar mark]
      (cons "Mark" (make-sparse-keymap "Mark")))

    (define-key map [menu-bar mark prev]
      '(menu-item "Previous Marked" dired-prev-marked-file
		  :help "Move to previous marked file"))
    (define-key map [menu-bar mark next]
      '(menu-item "Next Marked" dired-next-marked-file
		  :help "Move to next marked file"))
    (define-key map [menu-bar mark marks]
      '(menu-item "Change Marks..." dired-change-marks
		  :help "Replace marker with another character"))
    (define-key map [menu-bar mark unmark-all]
      '(menu-item "Unmark All" dired-unmark-all-marks))
    (define-key map [menu-bar mark symlinks]
      '(menu-item "Mark Symlinks" dired-mark-symlinks
		  :visible (fboundp 'make-symbolic-link)
		  :help "Mark all symbolic links"))
    (define-key map [menu-bar mark directories]
      '(menu-item "Mark Directories" dired-mark-directories
		  :help "Mark all directories except `.' and `..'"))
    (define-key map [menu-bar mark directory]
      '(menu-item "Mark Old Backups" dired-clean-directory
		  :help "Flag old numbered backups for deletion"))
    (define-key map [menu-bar mark executables]
      '(menu-item "Mark Executables" dired-mark-executables
		  :help "Mark all executable files"))
    (define-key map [menu-bar mark garbage-files]
      '(menu-item "Flag Garbage Files" dired-flag-garbage-files
		  :help "Flag unneeded files for deletion"))
    (define-key map [menu-bar mark backup-files]
      '(menu-item "Flag Backup Files" dired-flag-backup-files
		  :help "Flag all backup files for deletion"))
    (define-key map [menu-bar mark auto-save-files]
      '(menu-item "Flag Auto-save Files" dired-flag-auto-save-files
		  :help "Flag auto-save files for deletion"))
    (define-key map [menu-bar mark deletion]
      '(menu-item "Flag" dired-flag-file-deletion
		  :help "Flag current line's file for deletion"))
    (define-key map [menu-bar mark unmark]
      '(menu-item "Unmark" dired-unmark
		  :help "Unmark or unflag current line's file"))
    (define-key map [menu-bar mark mark]
      '(menu-item "Mark" dired-mark
		  :help "Mark current line's file for future operations"))
    (define-key map [menu-bar mark toggle-marks]
      '(menu-item "Toggle Marks" dired-toggle-marks
		  :help "Mark unmarked files, unmark marked ones"))

    (define-key map [menu-bar operate]
      (cons "Operate" (make-sparse-keymap "Operate")))

    (define-key map
      [menu-bar operate image-dired-delete-tag]
      '(menu-item "Delete Image Tag..." image-dired-delete-tag
                  :help "Delete image tag from current or marked files"))
    (define-key map
      [menu-bar operate image-dired-tag-files]
      '(menu-item "Add Image Tags..." image-dired-tag-files
                  :help "Add image tags to current or marked files"))
    (define-key map
      [menu-bar operate image-dired-dired-comment-files]
      '(menu-item "Add Image Comment..." image-dired-dired-comment-files
                  :help "Add image comment to current or marked files"))
    (define-key map
      [menu-bar operate image-dired-display-thumbs]
      '(menu-item "Display Image Thumbnails" image-dired-display-thumbs
                  :help "Display image thumbnails for current or marked image files"))

    (define-key map [menu-bar operate dashes-4]
      '("--"))

    (define-key map
      [menu-bar operate epa-dired-do-decrypt]
      '(menu-item "Decrypt" epa-dired-do-decrypt
		  :help "Decrypt file at cursor"))

    (define-key map
      [menu-bar operate epa-dired-do-verify]
      '(menu-item "Verify" epa-dired-do-verify
		  :help "Verify digital signature of file at cursor"))

    (define-key map
      [menu-bar operate epa-dired-do-sign]
      '(menu-item "Sign" epa-dired-do-sign
		  :help "Create digital signature of file at cursor"))

    (define-key map
      [menu-bar operate epa-dired-do-encrypt]
      '(menu-item "Encrypt" epa-dired-do-encrypt
		  :help "Encrypt file at cursor"))

    (define-key map [menu-bar operate dashes-3]
      '("--"))

    (define-key map [menu-bar operate query-replace]
      '(menu-item "Query Replace in Files..." dired-do-query-replace-regexp
		  :help "Replace regexp in marked files"))
    (define-key map [menu-bar operate search]
      '(menu-item "Search Files..." dired-do-search
		  :help "Search marked files for regexp"))
    (define-key map [menu-bar operate isearch-regexp]
      '(menu-item "Isearch Regexp Files..." dired-do-isearch-regexp
		  :help "Incrementally search marked files for regexp"))
    (define-key map [menu-bar operate isearch]
      '(menu-item "Isearch Files..." dired-do-isearch
		  :help "Incrementally search marked files for string"))
    (define-key map [menu-bar operate chown]
      '(menu-item "Change Owner..." dired-do-chown
		  :visible (not (memq system-type '(ms-dos windows-nt)))
		  :help "Change the owner of marked files"))
    (define-key map [menu-bar operate chgrp]
      '(menu-item "Change Group..." dired-do-chgrp
		  :visible (not (memq system-type '(ms-dos windows-nt)))
		  :help "Change the group of marked files"))
    (define-key map [menu-bar operate chmod]
      '(menu-item "Change Mode..." dired-do-chmod
		  :help "Change mode (attributes) of marked files"))
    (define-key map [menu-bar operate touch]
      '(menu-item "Change Timestamp..." dired-do-touch
		  :help "Change timestamp of marked files"))
    (define-key map [menu-bar operate load]
      '(menu-item "Load" dired-do-load
		  :help "Load marked Emacs Lisp files"))
    (define-key map [menu-bar operate compile]
      '(menu-item "Byte-compile" dired-do-byte-compile
		  :help "Byte-compile marked Emacs Lisp files"))
    (define-key map [menu-bar operate compress]
      '(menu-item "Compress" dired-do-compress
		  :help "Compress/uncompress marked files"))
    (define-key map [menu-bar operate print]
      '(menu-item "Print..." dired-do-print
		  :help "Ask for print command and print marked files"))
    (define-key map [menu-bar operate hardlink]
      '(menu-item "Hardlink to..." dired-do-hardlink
		  :help "Make hard links for current or marked files"))
    (define-key map [menu-bar operate symlink]
      '(menu-item "Symlink to..." dired-do-symlink
		  :visible (fboundp 'make-symbolic-link)
		  :help "Make symbolic links for current or marked files"))
    (define-key map [menu-bar operate async-command]
      '(menu-item "Asynchronous Shell Command..." dired-do-async-shell-command
		  :help "Run a shell command asynchronously on current or marked files"))
    (define-key map [menu-bar operate command]
      '(menu-item "Shell Command..." dired-do-shell-command
		  :help "Run a shell command on current or marked files"))
    (define-key map [menu-bar operate delete]
      '(menu-item "Delete" dired-do-delete
		  :help "Delete current file or all marked files"))
    (define-key map [menu-bar operate rename]
      '(menu-item "Rename to..." dired-do-rename
		  :help "Rename current file or move marked files"))
    (define-key map [menu-bar operate copy]
      '(menu-item "Copy to..." dired-do-copy
		  :help "Copy current file or all marked files"))

    map)
  "Local keymap for `dired-mode' buffers.")

;; Dired mode is suitable only for specially formatted data.
(put 'dired-mode 'mode-class 'special)

;; Autoload cookie needed by desktop.el
;;;###autoload
(defun dired-mode (&optional dirname switches)
  "\
Mode for \"editing\" directory listings.
In Dired, you are \"editing\" a list of the files in a directory and
  \(optionally) its subdirectories, in the format of `ls -lR'.
  Each directory is a page: use \\[backward-page] and \\[forward-page] to move pagewise.
\"Editing\" means that you can run shell commands on files, visit,
  compress, load or byte-compile them, change their file attributes
  and insert subdirectories into the same buffer.  You can \"mark\"
  files for later commands or \"flag\" them for deletion, either file
  by file or all files matching certain criteria.
You can move using the usual cursor motion commands.\\<dired-mode-map>
The buffer is read-only.  Digits are prefix arguments.
Type \\[dired-flag-file-deletion] to flag a file `D' for deletion.
Type \\[dired-mark] to Mark a file or subdirectory for later commands.
  Most commands operate on the marked files and use the current file
  if no files are marked.  Use a numeric prefix argument to operate on
  the next ARG (or previous -ARG if ARG<0) files, or just `1'
  to operate on the current file only.  Prefix arguments override marks.
  Mark-using commands display a list of failures afterwards.  Type \\[dired-summary]
  to see why something went wrong.
Type \\[dired-unmark] to Unmark a file or all files of an inserted subdirectory.
Type \\[dired-unmark-backward] to back up one line and unmark or unflag.
Type \\[dired-do-flagged-delete] to delete (eXecute) the files flagged `D'.
Type \\[dired-find-file] to Find the current line's file
  (or dired it in another buffer, if it is a directory).
Type \\[dired-find-file-other-window] to find file or dired directory in Other window.
Type \\[dired-maybe-insert-subdir] to Insert a subdirectory in this buffer.
Type \\[dired-do-rename] to Rename a file or move the marked files to another directory.
Type \\[dired-do-copy] to Copy files.
Type \\[dired-sort-toggle-or-edit] to toggle Sorting by name/date or change the `ls' switches.
Type \\[revert-buffer] to read all currently expanded directories aGain.
  This retains all marks and hides subdirs again that were hidden before.
Use `SPC' and `DEL' to move down and up by lines.

If Dired ever gets confused, you can either type \\[revert-buffer] \
to read the
directories again, type \\[dired-do-redisplay] \
to relist the file at point or the marked files or a
subdirectory, or type \\[dired-build-subdir-alist] to parse the buffer
again for the directory tree.

Customization variables (rename this buffer and type \\[describe-variable] on each line
for more info):

  `dired-listing-switches'
  `dired-trivial-filenames'
  `dired-shrink-to-fit'
  `dired-marker-char'
  `dired-del-marker'
  `dired-keep-marker-rename'
  `dired-keep-marker-copy'
  `dired-keep-marker-hardlink'
  `dired-keep-marker-symlink'

Hooks (use \\[describe-variable] to see their documentation):

  `dired-before-readin-hook'
  `dired-after-readin-hook'
  `dired-mode-hook'
  `dired-load-hook'

Keybindings:
\\{dired-mode-map}"
  ;; Not to be called interactively (e.g. dired-directory will be set
  ;; to default-directory, which is wrong with wildcards).
  (kill-all-local-variables)
  (use-local-map dired-mode-map)
  (dired-advertise)			; default-directory is already set
  (setq major-mode 'dired-mode
	mode-name "Dired"
	;; case-fold-search nil
	buffer-read-only t
	selective-display t		; for subdirectory hiding
	mode-line-buffer-identification
	(propertized-buffer-identification "%17b"))
  (set (make-local-variable 'revert-buffer-function)
       (function dired-revert))
  (set (make-local-variable 'buffer-stale-function)
       (function dired-buffer-stale-p))
  (set (make-local-variable 'page-delimiter)
       "\n\n")
  (set (make-local-variable 'dired-directory)
       (or dirname default-directory))
  ;; list-buffers uses this to display the dir being edited in this buffer.
  (setq list-buffers-directory
	(expand-file-name (if (listp dired-directory)
			      (car dired-directory)
			    dired-directory)))
  (set (make-local-variable 'dired-actual-switches)
       (or switches dired-listing-switches))
  (set (make-local-variable 'font-lock-defaults)
       '(dired-font-lock-keywords t nil nil beginning-of-line))
  (set (make-local-variable 'desktop-save-buffer)
       'dired-desktop-buffer-misc-data)
  (setq dired-switches-alist nil)
  (hack-dir-local-variables-non-file-buffer) ; before sorting
  (dired-sort-other dired-actual-switches t)
  (when (featurep 'dnd)
    (set (make-local-variable 'dnd-protocol-alist)
	 (append dired-dnd-protocol-alist dnd-protocol-alist)))
  (add-hook 'file-name-at-point-functions 'dired-file-name-at-point nil t)
  (add-hook 'isearch-mode-hook 'dired-isearch-filenames-setup nil t)
  (run-mode-hooks 'dired-mode-hook))

;; Idiosyncratic dired commands that don't deal with marks.

(defun dired-summary ()
  "Summarize basic Dired commands and show recent dired errors."
  (interactive)
  (dired-why)
  ;>> this should check the key-bindings and use substitute-command-keys if non-standard
  (message
   "d-elete, u-ndelete, x-punge, f-ind, o-ther window, R-ename, C-opy, h-elp"))

(defun dired-undo ()
  "Undo in a dired buffer.
This doesn't recover lost files, it just undoes changes in the buffer itself.
You can use it to recover marks, killed lines or subdirs."
  (interactive)
  (let ((inhibit-read-only t))
    (undo))
  (dired-build-subdir-alist)
  (message "Change in dired buffer undone.
Actual changes in files cannot be undone by Emacs."))

(defun dired-toggle-read-only ()
  "Edit dired buffer with Wdired, or set it read-only.
Call `wdired-change-to-wdired-mode' in dired buffers whose editing is
supported by Wdired (the major mode of the dired buffer is `dired-mode').
Otherwise, for buffers inheriting from dired-mode, call `toggle-read-only'."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (wdired-change-to-wdired-mode)
    (with-no-warnings
      (toggle-read-only))))

(defun dired-next-line (arg)
  "Move down lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line arg)
  (dired-move-to-filename))

(defun dired-previous-line (arg)
  "Move up lines then position at filename.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line (- arg))
  (dired-move-to-filename))

(defun dired-next-dirline (arg &optional opoint)
  "Goto ARG'th next directory file line."
  (interactive "p")
  (or opoint (setq opoint (point)))
  (if (if (> arg 0)
	  (re-search-forward dired-re-dir nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-dir nil t (- arg)))
      (dired-move-to-filename)		; user may type `i' or `f'
    (goto-char opoint)
    (error "No more subdirectories")))

(defun dired-prev-dirline (arg)
  "Goto ARG'th previous directory file line."
  (interactive "p")
  (dired-next-dirline (- arg)))

(defun dired-up-directory (&optional other-window)
  "Run Dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary."
  (interactive "P")
  (let* ((dir (dired-current-directory))
	 (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
	;; Only try dired-goto-subdir if buffer has more than one dir.
	(and (cdr dired-subdir-alist)
	     (dired-goto-subdir up))
	(progn
	  (if other-window
	      (dired-other-window up)
	    (dired up))
	  (dired-goto-file dir)))))

(defun dired-get-file-for-visit ()
  "Get the current line's file name, with an error if file does not exist."
  (interactive)
  ;; We pass t for second arg so that we don't get error for `.' and `..'.
  (let ((raw (dired-get-filename nil t))
	file-name)
    (if (null raw)
	(error "No file on this line"))
    (setq file-name (file-name-sans-versions raw t))
    (if (file-exists-p file-name)
	file-name
      (if (file-symlink-p file-name)
	  (error "File is a symlink to a nonexistent target")
	(error "File no longer exists; type `g' to update dired buffer")))))

;; Force C-m keybinding rather than `f' or `e' in the mode doc:
(define-obsolete-function-alias 'dired-advertised-find-file 'dired-find-file "23.2")
(defun dired-find-file ()
  "In Dired, visit the file or directory named on this line."
  (interactive)
  ;; Bind `find-file-run-dired' so that the command works on directories
  ;; too, independent of the user's setting.
  (let ((find-file-run-dired t))
    (find-file (dired-get-file-for-visit))))

(defun dired-find-alternate-file ()
  "In Dired, visit this file or directory instead of the dired buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (find-alternate-file (dired-get-file-for-visit)))
;; Don't override the setting from .emacs.
;;;###autoload (put 'dired-find-alternate-file 'disabled t)

(defun dired-mouse-find-file-other-window (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
	    pos (posn-point (event-end event)))
      (if (not (windowp window))
	  (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (progn
	      (select-window window)
	      (dired-other-window file)))
      (select-window window)
      (find-file-other-window (file-name-sans-versions file t)))))

(defun dired-view-file ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
	(or (and (cdr dired-subdir-alist)
		 (dired-goto-subdir file))
	    (dired file))
      (view-file file))))

(defun dired-find-file-other-window ()
  "In Dired, visit this file or directory in another window."
  (interactive)
  (find-file-other-window (dired-get-file-for-visit)))

(defun dired-display-file ()
  "In Dired, display this file or directory in another window."
  (interactive)
  (display-buffer (find-file-noselect (dired-get-file-for-visit))))

;;; Functions for extracting and manipulating file names in Dired buffers.

(defun dired-get-filename (&optional localp no-error-if-not-filep)
  "In Dired, return name of file mentioned on this line.
Value returned normally includes the directory name.
Optional arg LOCALP with value `no-dir' means don't include directory
name in result.  A value of `verbatim' means to return the name exactly as
it occurs in the buffer, and a value of t means construct name relative to
`default-directory', which still may contain slashes if in a subdirectory.
Optional arg NO-ERROR-IF-NOT-FILEP means treat `.' and `..' as
regular filenames and return nil if no filename on this line.
Otherwise, an error occurs in these cases."
  (let (case-fold-search file p1 p2 already-absolute)
    (save-excursion
      (if (setq p1 (dired-move-to-filename (not no-error-if-not-filep)))
	  (setq p2 (dired-move-to-end-of-filename no-error-if-not-filep))))
    ;; nil if no file on this line, but no-error-if-not-filep is t:
    (if (setq file (and p1 p2 (buffer-substring p1 p2)))
	(progn
	  ;; Get rid of the mouse-face property that file names have.
	  (set-text-properties 0 (length file) nil file)
	  ;; Unquote names quoted by ls or by dired-insert-directory.
	  ;; This code was written using `read' to unquote, because
          ;; it's faster than substituting \007 (4 chars) -> ^G (1
          ;; char) etc. in a lisp loop.  Unfortunately, this decision
          ;; has necessitated hacks such as dealing with filenames
          ;; with quotation marks in their names.
	  (while (string-match "\\(?:[^\\]\\|\\`\\)\\(\"\\)" file)
	    (setq file (replace-match "\\\"" nil t file 1)))
          ;; Unescape any spaces escaped by ls -b (bug#10469).
          ;; Other -b quotes, eg \t, \n, work transparently.
          (if (dired-switches-escape-p dired-actual-switches)
              (let ((start 0)
                    (rep "")
                    (shift -1))
                (if (eq localp 'verbatim)
                    (setq rep "\\\\"
                          shift +1))
                (while (string-match "\\(\\\\\\) " file start)
                  (setq file (replace-match rep nil t file 1)
                        start (+ shift (match-end 0))))))
	  (when (eq system-type 'windows-nt)
	    (save-match-data
	      (let ((start 0))
		(while (string-match "\\\\" file start)
		  (aset file (match-beginning 0) ?/)
		  (setq start (match-end 0))))))

          ;; Hence we don't need to worry about converting `\\' back to `\'.
          (setq file (read (concat "\"" file "\"")))
	  ;; The above `read' will return a unibyte string if FILE
	  ;; contains eight-bit-control/graphic characters.
	  (if (and enable-multibyte-characters
		   (not (multibyte-string-p file)))
	      (setq file (string-to-multibyte file)))))
    (and file (file-name-absolute-p file)
	 ;; A relative file name can start with ~.
	 ;; Don't treat it as absolute in this context.
	 (not (eq (aref file 0) ?~))
	 (setq already-absolute t))
    (cond
     ((null file)
      nil)
     ((eq localp 'verbatim)
      file)
     ((and (not no-error-if-not-filep)
	   (member file '("." "..")))
      (error "Cannot operate on `.' or `..'"))
     ((and (eq localp 'no-dir) already-absolute)
      (file-name-nondirectory file))
     (already-absolute
      (let ((handler (find-file-name-handler file nil)))
	;; check for safe-magic property so that we won't
	;; put /: for names that don't really need them.
	;; For instance, .gz files when auto-compression-mode is on.
	(if (and handler (not (get handler 'safe-magic)))
	    (concat "/:" file)
	  file)))
     ((eq localp 'no-dir)
      file)
     ((equal (dired-current-directory) "/")
      (setq file (concat (dired-current-directory localp) file))
      (let ((handler (find-file-name-handler file nil)))
	;; check for safe-magic property so that we won't
	;; put /: for names that don't really need them.
	;; For instance, .gz files when auto-compression-mode is on.
	(if (and handler (not (get handler 'safe-magic)))
	    (concat "/:" file)
	  file)))
     (t
      (concat (dired-current-directory localp) file)))))

(defun dired-string-replace-match (regexp string newtext
                                   &optional literal global)
  "Replace first match of REGEXP in STRING with NEWTEXT.
If it does not match, nil is returned instead of the new string.
Optional arg LITERAL means to take NEWTEXT literally.
Optional arg GLOBAL means to replace all matches."
  (if global
      (let ((start 0) ret)
	(while (string-match regexp string start)
	  (let ((from-end (- (length string) (match-end 0))))
	    (setq ret (setq string (replace-match newtext t literal string)))
	    (setq start (- (length string) from-end))))
	  ret)
    (if (not (string-match regexp string 0))
	nil
      (replace-match newtext t literal string))))

(defun dired-make-absolute (file &optional dir)
  ;;"Convert FILE (a file name relative to DIR) to an absolute file name."
  ;; We can't always use expand-file-name as this would get rid of `.'
  ;; or expand in / instead default-directory if DIR=="".
  ;; This should be good enough for ange-ftp.
  ;; It should be reasonably fast, though, as it is called in
  ;; dired-get-filename.
  (concat (or dir default-directory) file))

(defun dired-make-relative (file &optional dir _ignore)
  "Convert FILE (an absolute file name) to a name relative to DIR.
If this is impossible, return FILE unchanged.
DIR must be a directory name, not a file name."
  (or dir (setq dir default-directory))
  ;; This case comes into play if default-directory is set to
  ;; use ~.
  (if (and (> (length dir) 0) (= (aref dir 0) ?~))
      (setq dir (expand-file-name dir)))
  (if (string-match (concat "^" (regexp-quote dir)) file)
      (substring file (match-end 0))
;;;  (or no-error
;;;	(error "%s: not in directory tree growing at %s" file dir))
    file))

;;; Functions for finding the file name in a dired buffer line.

(defvar dired-permission-flags-regexp
  "\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)"
  "Regular expression to match the permission flags in `ls -l'.")

;; Move to first char of filename on this line.
;; Returns position (point) or nil if no filename on this line."
(defun dired-move-to-filename (&optional raise-error eol)
  "Move to the beginning of the filename on the current line.
Return the position of the beginning of the filename, or nil if none found."
  ;; This is the UNIX version.
  (or eol (setq eol (line-end-position)))
  (beginning-of-line)
  ;; First try assuming `ls --dired' was used.
  (let ((change (next-single-property-change (point) 'dired-filename nil eol)))
    (cond
     ((and change (< change eol))
      (goto-char change))
     ((re-search-forward directory-listing-before-filename-regexp eol t)
      (goto-char (match-end 0)))
     ((re-search-forward dired-permission-flags-regexp eol t)
      ;; Ha!  There *is* a file.  Our regexp-from-hell just failed to find it.
      (if raise-error
	  (error "Unrecognized line!  Check directory-listing-before-filename-regexp"))
      (beginning-of-line)
      nil)
     (raise-error
      (error "No file on this line")))))

(defun dired-move-to-end-of-filename (&optional no-error)
  ;; Assumes point is at beginning of filename,
  ;; thus the rwx bit re-search-backward below will succeed in *this*
  ;; line if at all.  So, it should be called only after
  ;; (dired-move-to-filename t).
  ;; On failure, signals an error (with non-nil NO-ERROR just returns nil).
  ;; This is the UNIX version.
  (if (get-text-property (point) 'dired-filename)
      (goto-char (next-single-property-change (point) 'dired-filename))
    (let (opoint file-type executable symlink hidden case-fold-search used-F eol)
      ;; case-fold-search is nil now, so we can test for capital F:
      (setq used-F (string-match "F" dired-actual-switches)
	    opoint (point)
	    eol (line-end-position)
	    hidden (and selective-display
			(save-excursion (search-forward "\r" eol t))))
      (if hidden
	  nil
	(save-excursion	;; Find out what kind of file this is:
	  ;; Restrict perm bits to be non-blank,
	  ;; otherwise this matches one char to early (looking backward):
	  ;; "l---------" (some systems make symlinks that way)
	  ;; "----------" (plain file with zero perms)
	  (if (re-search-backward
	       dired-permission-flags-regexp nil t)
	      (setq file-type (char-after (match-beginning 1))
		    symlink (eq file-type ?l)
		    ;; Only with -F we need to know whether it's an executable
		    executable (and
				used-F
				(string-match
				 "[xst]" ;; execute bit set anywhere?
				 (concat
				  (match-string 2)
				  (match-string 3)
				  (match-string 4)))))
	    (or no-error (error "No file on this line"))))
	;; Move point to end of name:
	(if symlink
	    (if (search-forward " -> " eol t)
		(progn
		  (forward-char -4)
		  (and used-F
		       dired-ls-F-marks-symlinks
		       (eq (preceding-char) ?@)	;; did ls really mark the link?
		       (forward-char -1))))
	  (goto-char eol) ;; else not a symbolic link
	  ;; ls -lF marks dirs, sockets, fifos and executables with exactly
	  ;; one trailing character. (Executable bits on symlinks ain't mean
	  ;; a thing, even to ls, but we know it's not a symlink.)
	  (and used-F
	       (or (memq file-type '(?d ?s ?p))
		   executable)
	       (forward-char -1))))
      (or no-error
	  (not (eq opoint (point)))
	  (error "%s" (if hidden
		     (substitute-command-keys
		      "File line is hidden, type \\[dired-hide-subdir] to unhide")
		   "No file on this line")))
      (if (eq opoint (point))
	  nil
	(point)))))


;;; COPY NAMES OF MARKED FILES INTO KILL-RING.

(defun dired-copy-filename-as-kill (&optional arg)
  "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank]."
  (interactive "P")
  (let ((string
         (or (dired-get-subdir)
             (mapconcat (function identity)
                        (if arg
                            (cond ((zerop (prefix-numeric-value arg))
                                   (dired-get-marked-files))
                                  ((consp arg)
                                   (dired-get-marked-files t))
                                  (t
                                   (dired-get-marked-files
				    'no-dir (prefix-numeric-value arg))))
                          (dired-get-marked-files 'no-dir))
                        " "))))
    (if (eq last-command 'kill-region)
	(kill-append string nil)
      (kill-new string))
    (message "%s" string)))


;; Keeping Dired buffers in sync with the filesystem and with each other

(defun dired-buffers-for-dir (dir &optional file)
;; Return a list of buffers for DIR (top level or in-situ subdir).
;; If FILE is non-nil, include only those whose wildcard pattern (if any)
;; matches FILE.
;; The list is in reverse order of buffer creation, most recent last.
;; As a side effect, killed dired buffers for DIR are removed from
;; dired-buffers.
  (setq dir (file-name-as-directory dir))
  (let (result buf)
    (dolist (elt dired-buffers)
      (setq buf (cdr elt))
      (cond
       ((null (buffer-name buf))
	;; Buffer is killed - clean up:
	(setq dired-buffers (delq elt dired-buffers)))
       ((dired-in-this-tree dir (car elt))
	(with-current-buffer buf
	  (and (assoc dir dired-subdir-alist)
	       (or (null file)
		   (if (stringp dired-directory)
		       (let ((wildcards (file-name-nondirectory
					 dired-directory)))
			 (or (= 0 (length wildcards))
			     (string-match (dired-glob-regexp wildcards)
					   file)))
		     (member (expand-file-name file dir)
			     (cdr dired-directory))))
	       (setq result (cons buf result)))))))
    result))

(defun dired-glob-regexp (pattern)
  "Convert glob-pattern PATTERN to a regular expression."
  (let ((matched-in-pattern 0)  ;; How many chars of PATTERN we've handled.
	regexp)
    (while (string-match "[[?*]" pattern matched-in-pattern)
      (let ((op-end (match-end 0))
	    (next-op (aref pattern (match-beginning 0))))
	(setq regexp (concat regexp
			     (regexp-quote
			      (substring pattern matched-in-pattern
					 (match-beginning 0)))))
	(cond ((= next-op ??)
	       (setq regexp (concat regexp "."))
	       (setq matched-in-pattern op-end))
	      ((= next-op ?\[)
	       ;; Fails to handle ^ yet ????
	       (let* ((set-start (match-beginning 0))
		      (set-cont
		       (if (= (aref pattern (1+ set-start)) ?^)
			   (+ 3 set-start)
			 (+ 2 set-start)))
		      (set-end (string-match "]" pattern set-cont))
		      (set (substring pattern set-start (1+ set-end))))
		 (setq regexp (concat regexp set))
		 (setq matched-in-pattern (1+ set-end))))
	      ((= next-op ?*)
	       (setq regexp (concat regexp ".*"))
	       (setq matched-in-pattern op-end)))))
    (concat "\\`"
	    regexp
	    (regexp-quote
	     (substring pattern matched-in-pattern))
	    "\\'")))



(defun dired-advertise ()
  ;;"Advertise in variable `dired-buffers' that we dired `default-directory'."
  ;; With wildcards we actually advertise too much.
  (let ((expanded-default (expand-file-name default-directory)))
    (if (memq (current-buffer) (dired-buffers-for-dir expanded-default))
	t				; we have already advertised ourselves
      (setq dired-buffers
	    (cons (cons expanded-default (current-buffer))
		  dired-buffers)))))

(defun dired-unadvertise (dir)
  ;; Remove DIR from the buffer alist in variable dired-buffers.
  ;; This has the effect of removing any buffer whose main directory is DIR.
  ;; It does not affect buffers in which DIR is a subdir.
  ;; Removing is also done as a side-effect in dired-buffer-for-dir.
  (setq dired-buffers
	(delq (assoc (expand-file-name dir) dired-buffers) dired-buffers)))

;; Tree Dired

;;; utility functions

(defun dired-in-this-tree (file dir)
  ;;"Is FILE part of the directory tree starting at DIR?"
  (let (case-fold-search)
    (string-match (concat "^" (regexp-quote dir)) file)))

(defun dired-normalize-subdir (dir)
  ;; Prepend default-directory to DIR if relative file name.
  ;; dired-get-filename must be able to make a valid file name from a
  ;; file and its directory DIR.
  (file-name-as-directory
   (if (file-name-absolute-p dir)
       dir
     (expand-file-name dir default-directory))))

(defun dired-get-subdir ()
  ;;"Return the subdir name on this line, or nil if not on a headerline."
  ;; Look up in the alist whether this is a headerline.
  (save-excursion
    (let ((cur-dir (dired-current-directory)))
      (beginning-of-line)		; alist stores b-o-l positions
      (and (zerop (- (point)
		     (dired-get-subdir-min (assoc cur-dir
						  dired-subdir-alist))))
	   cur-dir))))

;(defun dired-get-subdir-min (elt)
;  (cdr elt))
;; can't use macro,  must be redefinable for other alist format in dired-nstd.
(defalias 'dired-get-subdir-min 'cdr)

(defun dired-get-subdir-max (elt)
  (save-excursion
    (goto-char (dired-get-subdir-min elt))
    (dired-subdir-max)))

(defun dired-clear-alist ()
  (while dired-subdir-alist
    (set-marker (dired-get-subdir-min (car dired-subdir-alist)) nil)
    (setq dired-subdir-alist (cdr dired-subdir-alist))))

(defun dired-subdir-index (dir)
  ;; Return an index into alist for use with nth
  ;; for the sake of subdir moving commands.
  (let (found (index 0) (alist dired-subdir-alist))
    (while alist
      (if (string= dir (car (car alist)))
	  (setq alist nil found t)
	(setq alist (cdr alist) index (1+ index))))
    (if found index nil)))

(defun dired-next-subdir (arg &optional no-error-if-not-found no-skip)
  "Go to next subdirectory, regardless of level."
  ;; Use 0 arg to go to this directory's header line.
  ;; NO-SKIP prevents moving to end of header line, returning whatever
  ;; position was found in dired-subdir-alist.
  (interactive "p")
  (let ((this-dir (dired-current-directory))
	pos index)
    ;; nth with negative arg does not return nil but the first element
    (setq index (- (dired-subdir-index this-dir) arg))
    (setq pos (if (>= index 0)
		  (dired-get-subdir-min (nth index dired-subdir-alist))))
    (if pos
	(progn
	  (goto-char pos)
	  (or no-skip (skip-chars-forward "^\n\r"))
	  (point))
      (if no-error-if-not-found
	  nil				; return nil if not found
	(error "%s directory" (if (> arg 0) "Last" "First"))))))

(defun dired-build-subdir-alist (&optional switches)
  "Build `dired-subdir-alist' by parsing the buffer.
Returns the new value of the alist.
If optional arg SWITCHES is non-nil, use its value
instead of `dired-actual-switches'."
  (interactive)
  (dired-clear-alist)
  (save-excursion
    (let* ((count 0)
	   (inhibit-read-only t)
	   (buffer-undo-list t)
	   (switches (or switches dired-actual-switches))
	   new-dir-name
	   (R-ftp-base-dir-regex
	    ;; Used to expand subdirectory names correctly in recursive
	    ;; ange-ftp listings.
	    (and (string-match "R" switches)
		 (string-match "\\`/.*:\\(/.*\\)" default-directory)
		 (concat "\\`" (match-string 1 default-directory)))))
      (goto-char (point-min))
      (setq dired-subdir-alist nil)
      (while (re-search-forward dired-subdir-regexp nil t)
	;; Avoid taking a file name ending in a colon
	;; as a subdir name.
	(unless (save-excursion
		  (goto-char (match-beginning 0))
		  (beginning-of-line)
		  (forward-char 2)
		  (save-match-data (looking-at dired-re-perms)))
	  (save-excursion
	    (goto-char (match-beginning 1))
	    (setq new-dir-name
		  (buffer-substring-no-properties (point) (match-end 1))
		  new-dir-name
		  (save-match-data
		    (if (and R-ftp-base-dir-regex
			     (not (string= new-dir-name default-directory))
			     (string-match R-ftp-base-dir-regex new-dir-name))
			(concat default-directory
				(substring new-dir-name (match-end 0)))
		      (expand-file-name new-dir-name))))
	    (delete-region (point) (match-end 1))
	    (insert new-dir-name))
	  (setq count (1+ count))
	  ;; Undo any escaping of newlines and \ by dired-insert-directory.
	  ;; Convert "n" preceded by odd number of \ to newline, and \\ to \.
	  (when (and (dired-switches-escape-p switches)
		     (string-match-p "\\\\" new-dir-name))
	    (let (temp res)
	      (mapc (lambda (char)
		      (cond ((equal char ?\\)
			     (if temp
				 (setq res (concat res "\\")
				       temp nil)
			       (setq temp "\\")))
			    ((and temp (equal char ?n))
			     (setq res (concat res "\n")
				   temp nil))
			    (t
			     (setq res (concat res temp (char-to-string char))
				   temp nil))))
		    new-dir-name)
	      (setq new-dir-name res)))
	  (dired-alist-add-1 new-dir-name
           ;; Place a sub directory boundary between lines.
           (save-excursion
             (goto-char (match-beginning 0))
             (beginning-of-line)
             (point-marker)))))
      (if (and (> count 1) (called-interactively-p 'interactive))
	  (message "Buffer includes %d directories" count)))
    ;; We don't need to sort it because it is in buffer order per
    ;; constructionem.  Return new alist:
    dired-subdir-alist))

(defun dired-alist-add-1 (dir new-marker)
  ;; Add new DIR at NEW-MARKER.  Don't sort.
  (setq dired-subdir-alist
	(cons (cons (dired-normalize-subdir dir) new-marker)
	      dired-subdir-alist)))

(defun dired-goto-next-nontrivial-file ()
  ;; Position point on first nontrivial file after point.
  (dired-goto-next-file);; so there is a file to compare with
  (if (stringp dired-trivial-filenames)
      (while (and (not (eobp))
		  (string-match dired-trivial-filenames
				(file-name-nondirectory
				 (or (dired-get-filename nil t) ""))))
	(forward-line 1)
	(dired-move-to-filename))))

(defun dired-goto-next-file ()
  (let ((max (1- (dired-subdir-max))))
    (while (and (not (dired-move-to-filename)) (< (point) max))
      (forward-line 1))))

(defun dired-goto-file (file)
  "Go to line describing file FILE in this dired buffer."
  ;; Return value of point on success, else nil.
  ;; FILE must be an absolute file name.
  ;; Loses if FILE contains control chars like "\007" for which ls
  ;; either inserts "?" or "\\007" into the buffer, so we won't find
  ;; it in the buffer.
  (interactive
   (prog1				; let push-mark display its message
       (list (expand-file-name
	      (read-file-name "Goto file: "
			      (dired-current-directory))))
     (push-mark)))
  (unless (file-name-absolute-p file)
    (error "File name `%s' is not absolute" file))
  (setq file (directory-file-name file)) ; does no harm if not a directory
  (let* ((case-fold-search nil)
	 (dir (file-name-directory file))
	 (found (or
		 ;; First, look for a listing under the absolute name.
		 (save-excursion
		   (goto-char (point-min))
		   (dired-goto-file-1 file file (point-max)))
		 ;; Otherwise, look for it as a relative name.  The
		 ;; hair is to get the result of `dired-goto-subdir'
		 ;; without calling it if we don't have any subdirs.
		 (save-excursion
		   (when (if (string= dir (expand-file-name default-directory))
			     (goto-char (point-min))
			   (and (cdr dired-subdir-alist)
				(dired-goto-subdir dir)))
		     (dired-goto-file-1 (file-name-nondirectory file)
					file
					(dired-subdir-max)))))))
    ;; Return buffer position, if found.
    (if found
	(goto-char found))))

(defun dired-goto-file-1 (file full-name limit)
  "Advance to the Dired listing labeled by FILE; return its position.
Return nil if the listing is not found.  If FILE contains
characters that would not appear in a Dired buffer, search using
the quoted forms of those characters.

FULL-NAME specifies the actual file name the listing must have,
as returned by `dired-get-filename'.  LIMIT is the search limit."
  (let (str)
    (setq str (replace-regexp-in-string "\^m" "\\^m"  file nil t))
    (setq str (replace-regexp-in-string "\\\\" "\\\\" str nil t))
    (and (dired-switches-escape-p dired-actual-switches)
	 (string-match "[ \t\n]" str)
	 ;; FIXME: to fix this for embedded control characters etc, we
	 ;; should escape everything that `ls -b' does.
	 (setq str (replace-regexp-in-string " " "\\ "  str nil t)
	       str (replace-regexp-in-string "\t" "\\t" str nil t)
	       str (replace-regexp-in-string "\n" "\\n" str nil t)))
    (let ((found nil)
	  ;; filenames are preceded by SPC, this makes the search faster
	  ;; (e.g. for the filename "-").
	  (search-string (concat " " str)))
      (while (and (not found)
		  (search-forward search-string limit 'move))
	;; Check that we are in the right place.  Match could have
	;; BASE just as initial substring or in permission bits etc.
	(if (equal full-name (dired-get-filename nil t))
	    (setq found (dired-move-to-filename))
	  (forward-line 1)))
      found)))

(defvar dired-find-subdir)

;; FIXME document whatever dired-x is doing.
(defun dired-initial-position (dirname)
  "Where point should go in a new listing of DIRNAME.
Point assumed at beginning of new subdir line."
  (end-of-line)
  (and (featurep 'dired-x) dired-find-subdir
       (dired-goto-subdir dirname))
  (if dired-trivial-filenames (dired-goto-next-nontrivial-file)))

;; These are hooks which make tree dired work.
;; They are in this file because other parts of dired need to call them.
;; But they don't call the rest of tree dired unless there are subdirs loaded.

;; This function is called for each retrieved filename.
;; It could stand to be faster, though it's mostly function call
;; overhead.  Avoiding the function call seems to save about 10% in
;; dired-get-filename.  Make it a defsubst?
(defun dired-current-directory (&optional localp)
  "Return the name of the subdirectory to which this line belongs.
This returns a string with trailing slash, like `default-directory'.
Optional argument means return a file name relative to `default-directory'."
  (let ((here (point))
	(alist (or dired-subdir-alist
		   ;; probably because called in a non-dired buffer
		   (error "No subdir-alist in %s" (current-buffer))))
	elt dir)
    (while alist
      (setq elt (car alist)
	    dir (car elt)
	    ;; use `<=' (not `<') as subdir line is part of subdir
	    alist (if (<= (dired-get-subdir-min elt) here)
		      nil		; found
		    (cdr alist))))
    (if localp
	(dired-make-relative dir default-directory)
      dir)))

;; Subdirs start at the beginning of their header lines and end just
;; before the beginning of the next header line (or end of buffer).

(defun dired-subdir-max ()
  (save-excursion
    (if (or (null (cdr dired-subdir-alist)) (not (dired-next-subdir 1 t t)))
	(point-max)
      (point))))

;; Deleting files

(defcustom dired-recursive-deletes 'top
  "Decide whether recursive deletes are allowed.
A value of nil means no recursive deletes.
`always' means delete recursively without asking.  This is DANGEROUS!
`top' means ask for each directory at top level, but delete its subdirectories
without asking.
Anything else means ask for each directory."
  :type '(choice :tag "Delete non-empty directories"
		 (const :tag "Yes" always)
		 (const :tag "No--only delete empty directories" nil)
		 (const :tag "Confirm for each directory" t)
		 (const :tag "Confirm for each top directory only" top))
  :group 'dired)

;; Match anything but `.' and `..'.
(defvar dired-re-no-dot "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")

;; Delete file, possibly delete a directory and all its files.
;; This function is useful outside of dired.  One could change its name
;; to e.g. recursive-delete-file and put it somewhere else.
(defun dired-delete-file (file &optional recursive trash) "\
Delete FILE or directory (possibly recursively if optional RECURSIVE is true.)
RECURSIVE determines what to do with a non-empty directory.  If RECURSIVE is:
nil, do not delete.
`always', delete recursively without asking.
`top', ask for each directory at top level.
Anything else, ask for each sub-directory."
  ;; This test is equivalent to
  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
  ;; but more efficient
  (if (not (eq t (car (file-attributes file))))
      (delete-file file trash)
    (if (and recursive
	     (directory-files file t dired-re-no-dot) ; Not empty.
	     (or (eq recursive 'always)
		 (yes-or-no-p (format "Recursively %s %s? "
				      (if (and trash
					       delete-by-moving-to-trash)
					  "trash"
					"delete")
				      (dired-make-relative file)))))
	(if (eq recursive 'top) (setq recursive 'always)) ; Don't ask again.
      (setq recursive nil))
    (delete-directory file recursive trash)))

(defun dired-do-flagged-delete (&optional nomessage)
  "In Dired, delete the files flagged for deletion.
If NOMESSAGE is non-nil, we don't display any message
if there are no flagged files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  (interactive)
  (let* ((dired-marker-char dired-del-marker)
	 (regexp (dired-marker-regexp))
	 case-fold-search)
    (if (save-excursion (goto-char (point-min))
			(re-search-forward regexp nil t))
	(dired-internal-do-deletions
	 ;; this can't move point since ARG is nil
	 (dired-map-over-marks (cons (dired-get-filename) (point))
			       nil)
	 nil t)
      (or nomessage
	  (message "(No deletions requested)")))))

(defun dired-do-delete (&optional arg)
  "Delete all marked (or next ARG) files.
`dired-recursive-deletes' controls whether deletion of
non-empty directories is allowed."
  ;; This is more consistent with the file marking feature than
  ;; dired-do-flagged-delete.
  (interactive "P")
  (dired-internal-do-deletions
   ;; this may move point if ARG is an integer
   (dired-map-over-marks (cons (dired-get-filename) (point))
			 arg)
   arg t))

(defvar dired-deletion-confirmer 'yes-or-no-p) ; or y-or-n-p?

(defun dired-internal-do-deletions (l arg &optional trash)
  ;; L is an alist of files to delete, with their buffer positions.
  ;; ARG is the prefix arg.
  ;; Filenames are absolute.
  ;; (car L) *must* be the *last* (bottommost) file in the dired buffer.
  ;; That way as changes are made in the buffer they do not shift the
  ;; lines still to be changed, so the (point) values in L stay valid.
  ;; Also, for subdirs in natural order, a subdir's files are deleted
  ;; before the subdir itself - the other way around would not work.
  (let* ((files (mapcar (function car) l))
	 (count (length l))
	 (succ 0)
	 (trashing (and trash delete-by-moving-to-trash))
	 (progress-reporter
	  (make-progress-reporter
	   (if trashing "Trashing..." "Deleting...")
	   succ count)))
    ;; canonicalize file list for pop up
    (setq files (nreverse (mapcar (function dired-make-relative) files)))
    (if (dired-mark-pop-up
	 " *Deletions*" 'delete files dired-deletion-confirmer
	 (format "%s %s "
		 (if trashing "Trash" "Delete")
		 (dired-mark-prompt arg files)))
	(save-excursion
	  (let (failures);; files better be in reverse order for this loop!
	    (while l
	      (goto-char (cdr (car l)))
	      (let ((inhibit-read-only t))
		(condition-case err
		    (let ((fn (car (car l))))
		      (dired-delete-file fn dired-recursive-deletes trash)
		      ;; if we get here, removing worked
		      (setq succ (1+ succ))
		      (progress-reporter-update progress-reporter succ)
		      (dired-fun-in-all-buffers
		       (file-name-directory fn) (file-name-nondirectory fn)
		       (function dired-delete-entry) fn))
		  (error;; catch errors from failed deletions
		   (dired-log "%s\n" err)
		   (setq failures (cons (car (car l)) failures)))))
	      (setq l (cdr l)))
	    (if (not failures)
		(progress-reporter-done progress-reporter)
	      (dired-log-summary
	       (format "%d of %d deletion%s failed"
		       (length failures) count
		       (dired-plural-s count))
	       failures))))
      (message "(No deletions performed)")))
  (dired-move-to-filename))

(defun dired-fun-in-all-buffers (directory file fun &rest args)
  ;; In all buffers dired'ing DIRECTORY, run FUN with ARGS.
  ;; If the buffer has a wildcard pattern, check that it matches FILE.
  ;; (FILE does not include a directory component.)
  ;; FILE may be nil, in which case ignore it.
  ;; Return list of buffers where FUN succeeded (i.e., returned non-nil).
  (let (success-list)
    (dolist (buf (dired-buffers-for-dir (expand-file-name directory)
					file))
      (with-current-buffer buf
	(if (apply fun args)
	    (setq success-list (cons (buffer-name buf) success-list)))))
    success-list))

;; Delete the entry for FILE from
(defun dired-delete-entry (file)
  (save-excursion
    (and (dired-goto-file file)
	 (let ((inhibit-read-only t))
	   (delete-region (progn (beginning-of-line) (point))
			  (save-excursion (forward-line 1) (point))))))
  (dired-clean-up-after-deletion file))

(defvar dired-clean-up-buffers-too)

(defun dired-clean-up-after-deletion (fn)
  "Clean up after a deleted file or directory FN.
Removes any expanded subdirectory of deleted directory.
If `dired-x' is loaded and `dired-clean-up-buffers-too' is non-nil,
also offers to kill buffers visiting deleted files and directories."
  (save-excursion (and (cdr dired-subdir-alist)
		       (dired-goto-subdir fn)
		       (dired-kill-subdir)))
  ;; Offer to kill buffer of deleted file FN.
  (when (and (featurep 'dired-x) dired-clean-up-buffers-too)
    (let ((buf (get-file-buffer fn)))
      (and buf
           (funcall #'y-or-n-p
                    (format "Kill buffer of %s, too? "
                            (file-name-nondirectory fn)))
           (kill-buffer buf)))
    (let ((buf-list (dired-buffers-for-dir (expand-file-name fn))))
      (and buf-list
           (y-or-n-p (format "Kill dired buffer%s of %s, too? "
                             (dired-plural-s (length buf-list))
                             (file-name-nondirectory fn)))
           (dolist (buf buf-list)
             (kill-buffer buf))))))


;; Confirmation

(defun dired-marker-regexp ()
  (concat "^" (regexp-quote (char-to-string dired-marker-char))))

(defun dired-plural-s (count)
  (if (= 1 count) "" "s"))

(defun dired-mark-prompt (arg files)
  "Return a string suitable for use in a Dired prompt.
ARG is normally the prefix argument for the calling command.
FILES should be a list of file names.

The return value has a form like \"foo.txt\", \"[next 3 files]\",
or \"* [3 files]\"."
  ;; distinguish-one-marked can cause the first element to be just t.
  (if (eq (car files) t) (setq files (cdr files)))
  (let ((count (length files)))
    (if (= count 1)
	(car files)
      ;; more than 1 file:
      (if (integerp arg)
	  ;; abs(arg) = count
	  ;; Perhaps this is nicer, but it also takes more screen space:
	  ;;(format "[%s %d files]" (if (> arg 0) "next" "previous")
	  ;;                        count)
	  (format "[next %d files]" arg)
	(format "%c [%d files]" dired-marker-char count)))))

(defun dired-pop-to-buffer (buf)
  "Pop up buffer BUF in a way suitable for Dired."
  (let ((split-window-preferred-function
	 (lambda (window)
	   (or (and (let ((split-height-threshold 0))
		      (window-splittable-p (selected-window)))
		    ;; Try to split the selected window vertically if
		    ;; that's possible.  (Bug#1806)
		    (split-window-below))
	       ;; Otherwise, try to split WINDOW sensibly.
	       (split-window-sensibly window))))
	pop-up-frames)
    (pop-to-buffer (get-buffer-create buf)))
  ;; If dired-shrink-to-fit is t, make its window fit its contents.
  (when dired-shrink-to-fit
    ;; Try to not delete window when we want to display less than
    ;; `window-min-height' lines.
    (fit-window-to-buffer (get-buffer-window buf) nil 1)))

(defcustom dired-no-confirm nil
  "A list of symbols for commands Dired should not confirm, or t.
Command symbols are `byte-compile', `chgrp', `chmod', `chown', `compress',
`copy', `delete', `hardlink', `load', `move', `print', `shell', `symlink',
`touch' and `uncompress'.
If t, confirmation is never needed."
  :group 'dired
  :type '(choice (const :tag "Confirmation never needed" t)
		 (set (const byte-compile) (const chgrp)
		      (const chmod) (const chown) (const compress)
		      (const copy) (const delete) (const hardlink)
		      (const load) (const move) (const print)
		      (const shell) (const symlink) (const touch)
		      (const uncompress))))

(defun dired-mark-pop-up (bufname op-symbol files function &rest args)
  "Return FUNCTION's result on ARGS after showing which files are marked.
Displays the file names in a buffer named BUFNAME;
 nil gives \" *Marked Files*\".
This uses function `dired-pop-to-buffer' to do that.

FUNCTION should not manipulate files, just read input
 (an argument or confirmation).
The window is not shown if there is just one file or
 OP-SYMBOL is a member of the list in `dired-no-confirm'.
FILES is the list of marked files.  It can also be (t FILENAME)
in the case of one marked file, to distinguish that from using
just the current file."
  (or bufname (setq bufname  " *Marked Files*"))
  (if (or (eq dired-no-confirm t)
	  (memq op-symbol dired-no-confirm)
	  ;; If FILES defaulted to the current line's file.
	  (= (length files) 1))
      (apply function args)
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      ;; Handle (t FILE) just like (FILE), here.
      ;; That value is used (only in some cases), to mean
      ;; just one file that was marked, rather than the current line file.
      (dired-format-columns-of-files (if (eq (car files) t) (cdr files) files))
      (remove-text-properties (point-min) (point-max)
			      '(mouse-face nil help-echo nil)))
    (save-window-excursion
      (dired-pop-to-buffer bufname)
      (apply function args))))

(defun dired-format-columns-of-files (files)
  (let ((beg (point)))
    (completion--insert-strings files)
    (put-text-property beg (point) 'mouse-face nil)))

;; Commands to mark or flag file(s) at or near current line.

(defun dired-repeat-over-lines (arg function)
  ;; This version skips non-file lines.
  (let ((pos (make-marker)))
    (beginning-of-line)
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (while (and (not (eobp)) (dired-between-files)) (forward-line 1))
      (save-excursion
	(forward-line 1)
	(move-marker pos (1+ (point))))
      (save-excursion (funcall function))
      ;; Advance to the next line--actually, to the line that *was* next.
      ;; (If FUNCTION inserted some new lines in between, skip them.)
      (goto-char pos))
    (while (and (< arg 0) (not (bobp)))
      (setq arg (1+ arg))
      (forward-line -1)
      (while (and (not (bobp)) (dired-between-files)) (forward-line -1))
      (beginning-of-line)
      (save-excursion (funcall function)))
    (move-marker pos nil)
    (dired-move-to-filename)))

(defun dired-between-files ()
  ;; This used to be a regexp match of the `total ...' line output by
  ;; ls, which is slightly faster, but that is not very robust; notably,
  ;; it fails for non-english locales.
  (save-excursion (not (dired-move-to-filename))))

(defun dired-next-marked-file (arg &optional wrap opoint)
  "Move to the next marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (or opoint (setq opoint (point)));; return to where interactively started
  (if (if (> arg 0)
	  (re-search-forward dired-re-mark nil t arg)
	(beginning-of-line)
	(re-search-backward dired-re-mark nil t (- arg)))
      (dired-move-to-filename)
    (if (null wrap)
	(progn
	  (goto-char opoint)
	  (error "No next marked file"))
      (message "(Wraparound for next marked file)")
      (goto-char (if (> arg 0) (point-min) (point-max)))
      (dired-next-marked-file arg nil opoint))))

(defun dired-prev-marked-file (arg &optional wrap)
  "Move to the previous marked file, wrapping around the end of the buffer."
  (interactive "p\np")
  (dired-next-marked-file (- arg) wrap))

(defun dired-file-marker (file)
  ;; Return FILE's marker, or nil if unmarked.
  (save-excursion
    (and (dired-goto-file file)
	 (progn
	   (beginning-of-line)
	   (if (not (equal ?\040 (following-char)))
	       (following-char))))))

(defun dired-mark-files-in-region (start end)
  (let ((inhibit-read-only t))
    (if (> start end)
	(error "start > end"))
    (goto-char start)			; assumed at beginning of line
    (while (< (point) end)
      ;; Skip subdir line and following garbage like the `total' line:
      (while (and (< (point) end) (dired-between-files))
	(forward-line 1))
      (if (and (not (looking-at dired-re-dot))
	       (dired-get-filename nil t))
	  (progn
	    (delete-char 1)
	    (insert dired-marker-char)))
      (forward-line 1))))

(defun dired-mark (arg)
  "Mark the current (or next ARG) files.
If on a subdir headerline, mark all its files except `.' and `..'.

Use \\[dired-unmark-all-files] to remove all marks
and \\[dired-unmark] on a subdir to remove the marks in
this subdir."
  (interactive "P")
  (if (dired-get-subdir)
      (save-excursion (dired-mark-subdir-files))
    (let ((inhibit-read-only t))
      (dired-repeat-over-lines
       (prefix-numeric-value arg)
       (function (lambda () (delete-char 1) (insert dired-marker-char)))))))

(defun dired-unmark (arg)
  "Unmark the current (or next ARG) files.
If looking at a subdir, unmark all its files except `.' and `..'."
  (interactive "P")
  (let ((dired-marker-char ?\040))
    (dired-mark arg)))

(defun dired-flag-file-deletion (arg)
  "In Dired, flag the current line's file for deletion.
With prefix arg, repeat over several lines.

If on a subdir headerline, mark all its files except `.' and `..'."
  (interactive "P")
  (let ((dired-marker-char dired-del-marker))
    (dired-mark arg)))

(defun dired-unmark-backward (arg)
  "In Dired, move up lines and remove marks or deletion flags there.
Optional prefix ARG says how many lines to unmark/unflag; default
is one line."
  (interactive "p")
  (dired-unmark (- arg)))

(defun dired-toggle-marks ()
  "Toggle marks: marked files become unmarked, and vice versa.
Files marked with other flags (such as `D') are not affected.
`.' and `..' are never toggled.
As always, hidden subdirs are not affected."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t))
      (while (not (eobp))
        (or (dired-between-files)
            (looking-at dired-re-dot)
            ;; use subst instead of insdel because it does not move
            ;; the gap and thus should be faster and because
            ;; other characters are left alone automatically
            (apply 'subst-char-in-region
                   (point) (1+ (point))
                   (if (eq ?\040 (following-char)) ; SPC
                       (list ?\040 dired-marker-char)
                     (list dired-marker-char ?\040))))
        (forward-line 1)))))

;;; Commands to mark or flag files based on their characteristics or names.

(defvar dired-regexp-history nil
  "History list of regular expressions used in Dired commands.")

(defun dired-read-regexp (prompt)
  (read-from-minibuffer prompt nil nil nil 'dired-regexp-history))

(defun dired-mark-files-regexp (regexp &optional marker-char)
  "Mark all files matching REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked.

REGEXP is an Emacs regexp, not a shell wildcard.  Thus, use `\\.o$' for
object files--just `.o' will mark more than you might think."
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files (regexp): "))
	 (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename t t)))
	    (and fn (string-match regexp fn))))
     "matching file")))

(defun dired-mark-files-containing-regexp (regexp &optional marker-char)
  "Mark all files with contents containing REGEXP for use in later commands.
A prefix argument means to unmark them instead.
`.' and `..' are never marked."
  (interactive
   (list (dired-read-regexp (concat (if current-prefix-arg "Unmark" "Mark")
				    " files containing (regexp): "))
	 (if current-prefix-arg ?\040)))
  (let ((dired-marker-char (or marker-char dired-marker-char)))
    (dired-mark-if
     (and (not (looking-at dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename nil t)))
	    (when (and fn (file-readable-p fn)
		       (not (file-directory-p fn)))
	      (let ((prebuf (get-file-buffer fn)))
		(message "Checking %s" fn)
		;; For now we do it inside emacs
		;; Grep might be better if there are a lot of files
		(if prebuf
		    (with-current-buffer prebuf
		      (save-excursion
			(goto-char (point-min))
			(re-search-forward regexp nil t)))
		  (with-temp-buffer
		    (insert-file-contents fn)
		    (goto-char (point-min))
		    (re-search-forward regexp nil t))))
		      )))
     "matching file")))

(defun dired-flag-files-regexp (regexp)
  "In Dired, flag all files containing the specified REGEXP for deletion.
The match is against the non-directory part of the filename.  Use `^'
  and `$' to anchor matches.  Exclude subdirs by hiding them.
`.' and `..' are never flagged."
  (interactive (list (dired-read-regexp "Flag for deletion (regexp): ")))
  (dired-mark-files-regexp regexp dired-del-marker))

(defun dired-mark-symlinks (unflag-p)
  "Mark all symbolic links.
With prefix argument, unmark or unflag all those files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (looking-at dired-re-sym) "symbolic link")))

(defun dired-mark-directories (unflag-p)
  "Mark all directory file lines except `.' and `..'.
With prefix argument, unmark or unflag all those files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (and (looking-at dired-re-dir)
			(not (looking-at dired-re-dot)))
		   "directory file")))

(defun dired-mark-executables (unflag-p)
  "Mark all executable files.
With prefix argument, unmark or unflag all those files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char)))
    (dired-mark-if (looking-at dired-re-exe) "executable file")))

;; dired-x.el has a dired-mark-sexp interactive command: mark
;; files for which PREDICATE returns non-nil.

(defun dired-flag-auto-save-files (&optional unflag-p)
  "Flag for deletion files whose names suggest they are auto save files.
A prefix argument says to unmark or unflag those files instead."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\040 dired-del-marker)))
    (dired-mark-if
     ;; It is less than general to check for # here,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
                          (or
                           (eq (preceding-char) ?#)
                           ;; Handle executables in case of -F option.
                           ;; We need not worry about the other kinds
                           ;; of markings that -F makes, since they won't
                           ;; appear on real auto-save files.
                           (if (eq (preceding-char) ?*)
                               (progn
                                 (forward-char -1)
                                 (eq (preceding-char) ?#)))))
	  (not (looking-at dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (auto-save-file-name-p
		    (file-name-nondirectory fn)))))
     "auto save file")))

(defcustom dired-garbage-files-regexp
  ;; `log' here is dubious, since it's typically used for useful log
  ;; files, not just TeX stuff.  -- fx
  (concat (regexp-opt
	   '(".log" ".toc" ".dvi" ".bak" ".orig" ".rej" ".aux"))
	  "\\'")
  "Regular expression to match \"garbage\" files for `dired-flag-garbage-files'."
  :type 'regexp
  :group 'dired)

(defun dired-flag-garbage-files ()
  "Flag for deletion all files that match `dired-garbage-files-regexp'."
  (interactive)
  (dired-flag-files-regexp dired-garbage-files-regexp))

(defun dired-flag-backup-files (&optional unflag-p)
  "Flag all backup files (names ending with `~') for deletion.
With prefix argument, unmark or unflag these files."
  (interactive "P")
  (let ((dired-marker-char (if unflag-p ?\s dired-del-marker)))
    (dired-mark-if
     ;; Don't call backup-file-name-p unless the last character looks like
     ;; it might be the end of a backup file name.  This isn't very general,
     ;; but it's the only way this runs fast enough.
     (and (save-excursion (end-of-line)
			  ;; Handle executables in case of -F option.
			  ;; We need not worry about the other kinds
			  ;; of markings that -F makes, since they won't
			  ;; appear on real backup files.
			  (if (eq (preceding-char) ?*)
			      (forward-char -1))
			  (eq (preceding-char) ?~))
	  (not (looking-at dired-re-dir))
	  (let ((fn (dired-get-filename t t)))
	    (if fn (backup-file-name-p fn))))
     "backup file")))

(defun dired-change-marks (&optional old new)
  "Change all OLD marks to NEW marks.
OLD and NEW are both characters used to mark files."
  (interactive
   (let* ((cursor-in-echo-area t)
	  (old (progn (message "Change (old mark): ") (read-char)))
	  (new (progn (message  "Change %c marks to (new mark): " old)
		      (read-char))))
     (list old new)))
  (if (or (eq old ?\r) (eq new ?\r))
      (ding)
    (let ((string (format "\n%c" old))
	  (inhibit-read-only t))
      (save-excursion
	(goto-char (point-min))
	(while (search-forward string nil t)
	  (if (if (= old ?\s)
		  (save-match-data
		    (dired-get-filename 'no-dir t))
		t)
	      (subst-char-in-region (match-beginning 0)
				    (match-end 0) old new)))))))

(defun dired-unmark-all-marks ()
  "Remove all marks from all files in the dired buffer."
  (interactive)
  (dired-unmark-all-files ?\r))

(defun dired-unmark-all-files (mark &optional arg)
  "Remove a specific mark (or any mark) from every file.
After this command, type the mark character to remove,
or type RET to remove all marks.
With prefix arg, query for each marked file.
Type \\[help-command] at that time for help."
  (interactive "cRemove marks (RET means all): \nP")
  (save-excursion
    (let* ((count 0)
	   (inhibit-read-only t) case-fold-search
	   (string (format "\n%c" mark))
	   (help-form "\
Type SPC or `y' to unmark one file, DEL or `n' to skip to next,
`!' to unmark all remaining files with no more questions."))
      (goto-char (point-min))
      (while (if (eq mark ?\r)
		 (re-search-forward dired-re-mark nil t)
	       (search-forward string nil t))
	(if (or (not arg)
		(let ((file (dired-get-filename t t)))
		  (and file
		       (dired-query 'query "Unmark file `%s'? "
				    file))))
	    (progn (subst-char-in-region (1- (point)) (point)
					 (preceding-char) ?\s)
		   (setq count (1+ count)))))
      (message (if (= count 1) "1 mark removed"
		 "%d marks removed")
	       count))))

;; Logging failures operating on files, and showing the results.

(defvar dired-log-buffer "*Dired log*")

(defun dired-why ()
  "Pop up a buffer with error log output from Dired.
A group of errors from a single command ends with a formfeed.
Thus, use \\[backward-page] to find the beginning of a group of errors."
  (interactive)
  (if (get-buffer dired-log-buffer)
      (let ((owindow (selected-window))
	    (window (display-buffer (get-buffer dired-log-buffer))))
	(unwind-protect
	    (progn
	      (select-window window)
	      (goto-char (point-max))
	      (forward-line -1)
	      (backward-page 1)
	      (recenter 0))
	  (select-window owindow)))))

(defun dired-log (log &rest args)
  ;; Log a message or the contents of a buffer.
  ;; If LOG is a string and there are more args, it is formatted with
  ;; those ARGS.  Usually the LOG string ends with a \n.
  ;; End each bunch of errors with (dired-log t):
  ;; this inserts the current time and buffer at the start of the page,
  ;; and \f (formfeed) at the end.
  (let ((obuf (current-buffer)))
    (with-current-buffer (get-buffer-create dired-log-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(cond ((stringp log)
	       (insert (if args
			   (apply (function format) log args)
			 log)))
	      ((bufferp log)
	       (insert-buffer-substring log))
	      ((eq t log)
	       (backward-page 1)
	       (unless (bolp)
		 (insert "\n"))
	       (insert (current-time-string)
		       "\tBuffer `" (buffer-name obuf) "'\n")
	       (goto-char (point-max))
	       (insert "\f\n")))))))

(defun dired-log-summary (string failures)
  "State a summary of a command's failures, in echo area and log buffer.
STRING is an overall summary of the failures.
FAILURES is a list of file names that we failed to operate on,
or nil if file names are not applicable."
  (if (= (length failures) 1)
      (message "%s"
	       (with-current-buffer dired-log-buffer
		 (goto-char (point-max))
		 (backward-page 1)
		 (if (eolp) (forward-line 1))
		 (buffer-substring (point) (point-max))))
    (message (if failures "%s--type ? for details (%s)"
	       "%s--type ? for details")
	     string failures))
  ;; Log a summary describing a bunch of errors.
  (dired-log (concat "\n" string "\n"))
  (dired-log t))

;;; Sorting

;; Most ls can only sort by name or by date (with -t), nothing else.
;; GNU ls sorts on size with -S, on extension with -X, and unsorted with -U.
;; So anything that does not contain these is sort "by name".

(defvar dired-ls-sorting-switches "SXU"
  "String of `ls' switches \(single letters\) except \"t\" that influence sorting.

This indicates to Dired which option switches to watch out for because they
will change the sorting order behavior of `ls'.

To change the default sorting order \(e.g. add a `-v' option\), see the
variable `dired-listing-switches'.  To temporarily override the listing
format, use `\\[universal-argument] \\[dired]'.")

(defvar dired-sort-by-date-regexp
  (concat "\\(\\`\\| \\)-[^- ]*t"
	  ;; `dired-ls-sorting-switches' after -t overrides -t.
	  "[^ " dired-ls-sorting-switches "]*"
	  "\\(\\(\\`\\| +\\)\\(--[^ ]+\\|-[^- t"
	  dired-ls-sorting-switches "]+\\)\\)* *$")
  "Regexp recognized by Dired to set `by date' mode.")

(defvar dired-sort-by-name-regexp
  (concat "\\`\\(\\(\\`\\| +\\)\\(--[^ ]+\\|"
	  "-[^- t" dired-ls-sorting-switches "]+\\)\\)* *$")
  "Regexp recognized by Dired to set `by name' mode.")

(defvar dired-sort-inhibit nil
  "Non-nil means the Dired sort command is disabled.
The idea is to set this buffer-locally in special dired buffers.")

(defun dired-sort-set-modeline ()
  ;; Set modeline display according to dired-actual-switches.
  ;; Modeline display of "by name" or "by date" guarantees the user a
  ;; match with the corresponding regexps.  Non-matching switches are
  ;; shown literally.
  (when (eq major-mode 'dired-mode)
    (setq mode-name
	  (let (case-fold-search)
	    (cond ((string-match
		    dired-sort-by-name-regexp dired-actual-switches)
		   "Dired by name")
		  ((string-match
		    dired-sort-by-date-regexp dired-actual-switches)
		   "Dired by date")
		  (t
		   (concat "Dired " dired-actual-switches)))))
    (force-mode-line-update)))

(defun dired-sort-toggle-or-edit (&optional arg)
  "Toggle sorting by date, and refresh the Dired buffer.
With a prefix argument, edit the current listing switches instead."
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-sort-toggle)))

(defun dired-sort-toggle ()
  ;; Toggle between sort by date/name.  Reverts the buffer.
  (let ((sorting-by-date (string-match dired-sort-by-date-regexp
				       dired-actual-switches))
	;; Regexp for finding (possibly embedded) -t switches.
	(switch-regexp "\\(\\`\\| \\)-\\([a-su-zA-Z]*\\)\\(t\\)\\([^ ]*\\)")
	case-fold-search)
    ;; Remove the -t switch.
    (while (string-match switch-regexp dired-actual-switches)
      (if (and (equal (match-string 2 dired-actual-switches) "")
	       (equal (match-string 4 dired-actual-switches) ""))
	  ;; Remove a stand-alone -t switch.
	  (setq dired-actual-switches
		(replace-match "" t t dired-actual-switches))
	;; Remove a switch of the form -XtY for some X and Y.
	(setq dired-actual-switches
	      (replace-match "" t t dired-actual-switches 3))))
    ;; Now, if we weren't sorting by date before, add the -t switch.
    (unless sorting-by-date
      (setq dired-actual-switches (concat dired-actual-switches " -t"))))
  (dired-sort-set-modeline)
  (revert-buffer))

;; Some user code loads dired especially for this.
;; Don't do that--use replace-regexp-in-string instead.
(defun dired-replace-in-string (regexp newtext string)
  ;; Replace REGEXP with NEWTEXT everywhere in STRING and return result.
  ;; NEWTEXT is taken literally---no \\DIGIT escapes will be recognized.
  (let ((result "") (start 0) mb me)
    (while (string-match regexp string start)
      (setq mb (match-beginning 0)
	    me (match-end 0)
	    result (concat result (substring string start mb) newtext)
	    start me))
    (concat result (substring string start))))

(defun dired-sort-other (switches &optional no-revert)
  "Specify new `ls' SWITCHES for current dired buffer.
Values matching `dired-sort-by-date-regexp' or `dired-sort-by-name-regexp'
set the minor mode accordingly, others appear literally in the mode line.
With optional second arg NO-REVERT, don't refresh the listing afterwards."
  (dired-sort-R-check switches)
  (setq dired-actual-switches switches)
  (dired-sort-set-modeline)
  (or no-revert (revert-buffer)))

(defvar dired-subdir-alist-pre-R nil
  "Value of `dired-subdir-alist' before -R switch added.")
(make-variable-buffer-local 'dired-subdir-alist-pre-R)

(defun dired-sort-R-check (switches)
  "Additional processing of -R in ls option string SWITCHES.
Saves `dired-subdir-alist' when R is set and restores saved value
minus any directories explicitly deleted when R is cleared.
To be called first in body of `dired-sort-other', etc."
  (cond
   ((and (string-match "R" switches)
	 (not (string-match "R" dired-actual-switches)))
    ;; Adding -R to ls switches -- save `dired-subdir-alist':
    (setq dired-subdir-alist-pre-R dired-subdir-alist))
   ((and (string-match "R" dired-actual-switches)
	 (not (string-match "R" switches)))
    ;; Deleting -R from ls switches -- revert to pre-R subdirs
    ;; that are still present:
    (setq dired-subdir-alist
	  (if dired-subdir-alist-pre-R
	      (let (subdirs)
		(while dired-subdir-alist-pre-R
		  (if (assoc (caar dired-subdir-alist-pre-R)
			     dired-subdir-alist)
		      ;; subdir still present...
		      (setq subdirs
			    (cons (car dired-subdir-alist-pre-R)
				  subdirs)))
		  (setq dired-subdir-alist-pre-R
			(cdr dired-subdir-alist-pre-R)))
		(reverse subdirs))
	    ;; No pre-R subdir alist, so revert to main directory
	    ;; listing:
	    (list (car (reverse dired-subdir-alist))))))))


;;;;  Drag and drop support

(defcustom dired-recursive-copies 'top
  "Decide whether recursive copies are allowed.
A value of nil means no recursive copies.
`always' means copy recursively without asking.
`top' means ask for each directory at top level.
Anything else means ask for each directory."
  :type '(choice :tag "Copy directories"
		 (const :tag "No recursive copies" nil)
		 (const :tag "Ask for each directory" t)
		 (const :tag "Ask for each top directory only" top)
		 (const :tag "Copy directories without asking" always))
  :group 'dired)

(defun dired-dnd-popup-notice ()
  (message-box
   "Dired recursive copies are currently disabled.\nSee the variable `dired-recursive-copies'."))

(declare-function x-popup-menu "menu.c" (position menu))

(defun dired-dnd-do-ask-action (uri)
  ;; No need to get actions and descriptions from the source,
  ;; we only have three actions anyway.
  (let ((action (x-popup-menu
		 t
		 (list "What action?"
		       (cons ""
			     '(("Copy here" . copy)
			       ("Move here" . move)
			       ("Link here" . link)
			       "--"
			       ("Cancel" . nil)))))))
    (if action
	(dired-dnd-handle-local-file uri action)
      nil)))

(declare-function dired-relist-entry "dired-aux" (file))
(declare-function make-symbolic-link "fileio.c")

;; Only used when (featurep 'dnd).
(declare-function dnd-get-local-file-name "dnd" (uri &optional must-exist))
(declare-function dnd-get-local-file-uri "dnd" (uri))

(defvar dired-overwrite-confirmed)      ;Defined in dired-aux.

(defun dired-dnd-handle-local-file (uri action)
  "Copy, move or link a file to the dired directory.
URI is the file to handle, ACTION is one of copy, move, link or ask.
Ask means pop up a menu for the user to select one of copy, move or link."
  (require 'dired-aux)
  (let* ((from (dnd-get-local-file-name uri t))
	 (to (when from
	       (concat (dired-current-directory)
		       (file-name-nondirectory from)))))
    (when from
      (cond ((eq action 'ask)
	     (dired-dnd-do-ask-action uri))
	    ;; If copying a directory and dired-recursive-copies is
	    ;; nil, dired-copy-file fails.  Pop up a notice.
	    ((and (memq action '(copy private))
		  (file-directory-p from)
		  (not dired-recursive-copies))
	     (dired-dnd-popup-notice))
	    ((memq action '(copy private move link))
	     (let ((overwrite (and (file-exists-p to)
				   (y-or-n-p
				    (format "Overwrite existing file `%s'? " to))))
		   ;; Binding dired-overwrite-confirmed to nil makes
		   ;; dired-handle-overwrite a no-op.  We instead use
		   ;; y-or-n-p, which pops a graphical menu.
		   dired-overwrite-confirmed backup-file)
	       (when (and overwrite
			  ;; d-b-o is defined in dired-aux.
			  (boundp 'dired-backup-overwrite)
			  dired-backup-overwrite
			  (setq backup-file
				(car (find-backup-file-name to)))
			  (or (eq dired-backup-overwrite 'always)
			      (y-or-n-p
			       (format
				"Make backup for existing file `%s'? " to))))
		 (rename-file to backup-file 0)
		 (dired-relist-entry backup-file))
	       (cond ((memq action '(copy private))
		      (dired-copy-file from to overwrite))
		     ((eq action 'move)
		      (dired-rename-file from to overwrite))
		     ((eq action 'link)
		      (make-symbolic-link from to overwrite)))
	       (dired-relist-entry to)
	       action))))))

(defun dired-dnd-handle-file (uri action)
  "Copy, move or link a file to the dired directory if it is a local file.
URI is the file to handle.  If the hostname in the URI isn't local, do nothing.
ACTION is one of copy, move, link or ask.
Ask means pop up a menu for the user to select one of copy, move or link."
  (let ((local-file (dnd-get-local-file-uri uri)))
    (if local-file (dired-dnd-handle-local-file local-file action)
      nil)))


;;;;  Desktop support

(eval-when-compile (require 'desktop))

(defun dired-desktop-buffer-misc-data (dirname)
  "Auxiliary information to be saved in desktop file."
  (cons
   ;; Value of `dired-directory'.
   (if (consp dired-directory)
       ;; Directory name followed by list of files.
       (cons (desktop-file-name (car dired-directory) dirname)
             (cdr dired-directory))
     ;; Directory name, optionally with shell wildcard.
     (desktop-file-name dired-directory dirname))
   ;; Subdirectories in `dired-subdir-alist'.
   (cdr
     (nreverse
       (mapcar
         (function (lambda (f) (desktop-file-name (car f) dirname)))
         dired-subdir-alist)))))

(defun dired-restore-desktop-buffer (_file-name
                                     _buffer-name
                                     misc-data)
  "Restore a dired buffer specified in a desktop file."
  ;; First element of `misc-data' is the value of `dired-directory'.
  ;; This value is a directory name, optionally with shell wildcard or
  ;; a directory name followed by list of files.
  (let* ((dired-dir (car misc-data))
         (dir (if (consp dired-dir) (car dired-dir) dired-dir)))
    (if (file-directory-p (file-name-directory dir))
        (progn
          (dired dired-dir)
          ;; The following elements of `misc-data' are the keys
          ;; from `dired-subdir-alist'.
          (mapc 'dired-maybe-insert-subdir (cdr misc-data))
          (current-buffer))
      (message "Desktop: Directory %s no longer exists." dir)
      (when desktop-missing-file-warning (sit-for 1))
      nil)))

(add-to-list 'desktop-buffer-mode-handlers
	     '(dired-mode . dired-restore-desktop-buffer))


;;; Start of automatically extracted autoloads.

;;;### (autoloads (dired-show-file-type dired-do-query-replace-regexp
;;;;;;  dired-do-search dired-do-isearch-regexp dired-do-isearch
;;;;;;  dired-isearch-filenames-regexp dired-isearch-filenames dired-isearch-filenames-setup
;;;;;;  dired-hide-all dired-hide-subdir dired-tree-down dired-tree-up
;;;;;;  dired-kill-subdir dired-mark-subdir-files dired-goto-subdir
;;;;;;  dired-prev-subdir dired-insert-subdir dired-maybe-insert-subdir
;;;;;;  dired-downcase dired-upcase dired-do-symlink-regexp dired-do-hardlink-regexp
;;;;;;  dired-do-copy-regexp dired-do-rename-regexp dired-do-rename
;;;;;;  dired-do-hardlink dired-do-symlink dired-do-copy dired-create-directory
;;;;;;  dired-rename-file dired-copy-file dired-relist-file dired-remove-file
;;;;;;  dired-add-file dired-do-redisplay dired-do-load dired-do-byte-compile
;;;;;;  dired-do-compress dired-query dired-compress-file dired-do-kill-lines
;;;;;;  dired-run-shell-command dired-do-shell-command dired-do-async-shell-command
;;;;;;  dired-clean-directory dired-do-print dired-do-touch dired-do-chown
;;;;;;  dired-do-chgrp dired-do-chmod dired-compare-directories dired-backup-diff
;;;;;;  dired-diff) "dired-aux" "dired-aux.el" "58d623eb8e68e472e6164a1bcae83360")
;;; Generated autoloads from dired-aux.el

(autoload 'dired-diff "dired-aux" "\
Compare file at point with file FILE using `diff'.
FILE defaults to the file at the mark.  (That's the mark set by
\\[set-mark-command], not by Dired's \\[dired-mark] command.)
The prompted-for FILE is the first file given to `diff'.
With prefix arg, prompt for second argument SWITCHES,
which is the string of command switches for `diff'.

\(fn FILE &optional SWITCHES)" t nil)

(autoload 'dired-backup-diff "dired-aux" "\
Diff this file with its backup file or vice versa.
Uses the latest backup, if there are several numerical backups.
If this file is a backup, diff it with its original.
The backup file is the first file given to `diff'.
With prefix arg, prompt for argument SWITCHES which is options for `diff'.

\(fn &optional SWITCHES)" t nil)

(autoload 'dired-compare-directories "dired-aux" "\
Mark files with different file attributes in two dired buffers.
Compare file attributes of files in the current directory
with file attributes in directory DIR2 using PREDICATE on pairs of files
with the same name.  Mark files for which PREDICATE returns non-nil.
Mark files with different names if PREDICATE is nil (or interactively
with empty input at the predicate prompt).

PREDICATE is a Lisp expression that can refer to the following variables:

    size1, size2   - file size in bytes
    mtime1, mtime2 - last modification time in seconds, as a float
    fa1, fa2       - list of file attributes
                     returned by function `file-attributes'

    where 1 refers to attribute of file in the current dired buffer
    and 2 to attribute of file in second dired buffer.

Examples of PREDICATE:

    (> mtime1 mtime2) - mark newer files
    (not (= size1 size2)) - mark files with different sizes
    (not (string= (nth 8 fa1) (nth 8 fa2))) - mark files with different modes
    (not (and (= (nth 2 fa1) (nth 2 fa2))   - mark files with different UID
              (= (nth 3 fa1) (nth 3 fa2))))   and GID.

\(fn DIR2 PREDICATE)" t nil)

(autoload 'dired-do-chmod "dired-aux" "\
Change the mode of the marked (or next ARG) files.
Symbolic modes like `g+w' are allowed.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-chgrp "dired-aux" "\
Change the group of the marked (or next ARG) files.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-chown "dired-aux" "\
Change the owner of the marked (or next ARG) files.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-touch "dired-aux" "\
Change the timestamp of the marked (or next ARG) files.
This calls touch.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-print "dired-aux" "\
Print the marked (or next ARG) files.
Uses the shell command coming from variables `lpr-command' and
`lpr-switches' as default.

\(fn &optional ARG)" t nil)

(autoload 'dired-clean-directory "dired-aux" "\
Flag numerical backups for deletion.
Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
Positive prefix arg KEEP overrides `dired-kept-versions';
Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

To clear the flags on these files, you can use \\[dired-flag-backup-files]
with a prefix argument.

\(fn KEEP)" t nil)

(autoload 'dired-do-async-shell-command "dired-aux" "\
Run a shell command COMMAND on the marked files asynchronously.

Like `dired-do-shell-command' but if COMMAND doesn't end in ampersand,
adds `* &' surrounded by whitespace and executes the command asynchronously.
The output appears in the buffer `*Async Shell Command*'.

\(fn COMMAND &optional ARG FILE-LIST)" t nil)

(autoload 'dired-do-shell-command "dired-aux" "\
Run a shell command COMMAND on the marked files.
If no files are marked or a specific numeric prefix arg is given,
the next ARG files are used.  Just \\[universal-argument] means the current file.
The prompt mentions the file(s) or the marker, as appropriate.

If there is a `*' in COMMAND, surrounded by whitespace, this runs
COMMAND just once with the entire file list substituted there.

If there is no `*', but there is a `?' in COMMAND, surrounded by
whitespace, this runs COMMAND on each file individually with the
file name substituted for `?'.

Otherwise, this runs COMMAND on each file individually with the
file name added at the end of COMMAND (separated by a space).

`*' and `?' when not surrounded by whitespace have no special
significance for `dired-do-shell-command', and are passed through
normally to the shell, but you must confirm first.

If you want to use `*' as a shell wildcard with whitespace around
it, write `*\"\"' in place of just `*'.  This is equivalent to just
`*' in the shell, but avoids Dired's special handling.

If COMMAND produces output, it goes to a separate buffer.

This feature does not try to redisplay Dired buffers afterward, as
there's no telling what files COMMAND may have changed.
Type \\[dired-do-redisplay] to redisplay the marked files.

When COMMAND runs, its working directory is the top-level directory
of the Dired buffer, so output files usually are created there
instead of in a subdir.

In a noninteractive call (from Lisp code), you must specify
the list of file names explicitly with the FILE-LIST argument, which
can be produced by `dired-get-marked-files', for example.

\(fn COMMAND &optional ARG FILE-LIST)" t nil)

(autoload 'dired-run-shell-command "dired-aux" "\


\(fn COMMAND)" nil nil)

(autoload 'dired-do-kill-lines "dired-aux" "\
Kill all marked lines (not the files).
With a prefix argument, kill that many lines starting with the current line.
\(A negative argument kills backward.)
If you use this command with a prefix argument to kill the line
for a file that is a directory, which you have inserted in the
Dired buffer as a subdirectory, then it deletes that subdirectory
from the buffer as well.
To kill an entire subdirectory (without killing its line in the
parent directory), go to its directory header line and use this
command with a prefix argument (the value does not matter).

\(fn &optional ARG FMT)" t nil)

(autoload 'dired-compress-file "dired-aux" "\


\(fn FILE)" nil nil)

(autoload 'dired-query "dired-aux" "\
Format PROMPT with ARGS, query user, and store the result in SYM.
The return value is either nil or t.

The user may type y or SPC to accept once; n or DEL to skip once;
! to accept this and subsequent queries; or q or ESC to decline
this and subsequent queries.

If SYM is already bound to a non-nil value, this function may
return automatically without querying the user.  If SYM is !,
return t; if SYM is q or ESC, return nil.

\(fn SYM PROMPT &rest ARGS)" nil nil)

(autoload 'dired-do-compress "dired-aux" "\
Compress or uncompress marked (or next ARG) files.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-byte-compile "dired-aux" "\
Byte compile marked (or next ARG) Emacs Lisp files.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-load "dired-aux" "\
Load the marked (or next ARG) Emacs Lisp files.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-redisplay "dired-aux" "\
Redisplay all marked (or next ARG) files.
If on a subdir line, redisplay that subdirectory.  In that case,
a prefix arg lets you edit the `ls' switches used for the new listing.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
\\<dired-mode-map>\\[dired-reset-subdir-switches].
See Info node `(emacs)Subdir switches' for more details.

\(fn &optional ARG TEST-FOR-SUBDIR)" t nil)

(autoload 'dired-add-file "dired-aux" "\


\(fn FILENAME &optional MARKER-CHAR)" nil nil)

(autoload 'dired-remove-file "dired-aux" "\


\(fn FILE)" nil nil)

(autoload 'dired-relist-file "dired-aux" "\
Create or update the line for FILE in all Dired buffers it would belong in.

\(fn FILE)" nil nil)

(autoload 'dired-copy-file "dired-aux" "\


\(fn FROM TO OK-FLAG)" nil nil)

(autoload 'dired-rename-file "dired-aux" "\


\(fn FILE NEWNAME OK-IF-ALREADY-EXISTS)" nil nil)

(autoload 'dired-create-directory "dired-aux" "\
Create a directory called DIRECTORY.
If DIRECTORY already exists, signal an error.

\(fn DIRECTORY)" t nil)

(autoload 'dired-do-copy "dired-aux" "\
Copy all marked (or next ARG) files, or copy the current file.
This normally preserves the last-modified date when copying.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory,
and new copies of these files are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

This command copies symbolic links by creating new ones,
like `cp -d'.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-symlink "dired-aux" "\
Make symbolic links to current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new symbolic links are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

For relative symlinks, use \\[dired-do-relsymlink].

\(fn &optional ARG)" t nil)

(autoload 'dired-do-hardlink "dired-aux" "\
Add names (hard links) current file or all marked (or next ARG) files.
When operating on just the current file, you specify the new name.
When operating on multiple or marked files, you specify a directory
and new hard links are made in that directory
with the same names that the files currently have.  The default
suggested for the target directory depends on the value of
`dired-dwim-target', which see.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-rename "dired-aux" "\
Rename current file or all marked (or next ARG) files.
When renaming just the current file, you specify the new name.
When renaming multiple or marked files, you specify a directory.
This command also renames any buffers that are visiting the files.
The default suggested for the target directory depends on the value
of `dired-dwim-target', which see.

\(fn &optional ARG)" t nil)

(autoload 'dired-do-rename-regexp "dired-aux" "\
Rename selected files whose names match REGEXP to NEWNAME.

With non-zero prefix argument ARG, the command operates on the next ARG
files.  Otherwise, it operates on all the marked files, or the current
file if none are marked.

As each match is found, the user must type a character saying
  what to do with it.  For directions, type \\[help-command] at that time.
NEWNAME may contain \\=\\<n> or \\& as in `query-replace-regexp'.
REGEXP defaults to the last regexp used.

With a zero prefix arg, renaming by regexp affects the absolute file name.
Normally, only the non-directory part of the file name is used and changed.

\(fn REGEXP NEWNAME &optional ARG WHOLE-NAME)" t nil)

(autoload 'dired-do-copy-regexp "dired-aux" "\
Copy selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info.

\(fn REGEXP NEWNAME &optional ARG WHOLE-NAME)" t nil)

(autoload 'dired-do-hardlink-regexp "dired-aux" "\
Hardlink selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info.

\(fn REGEXP NEWNAME &optional ARG WHOLE-NAME)" t nil)

(autoload 'dired-do-symlink-regexp "dired-aux" "\
Symlink selected files whose names match REGEXP to NEWNAME.
See function `dired-do-rename-regexp' for more info.

\(fn REGEXP NEWNAME &optional ARG WHOLE-NAME)" t nil)

(autoload 'dired-upcase "dired-aux" "\
Rename all marked (or next ARG) files to upper case.

\(fn &optional ARG)" t nil)

(autoload 'dired-downcase "dired-aux" "\
Rename all marked (or next ARG) files to lower case.

\(fn &optional ARG)" t nil)

(autoload 'dired-maybe-insert-subdir "dired-aux" "\
Insert this subdirectory into the same dired buffer.
If it is already present, just move to it (type \\[dired-do-redisplay] to refresh),
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the ls switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output.

Dired remembers switches specified with a prefix arg, so that reverting
the buffer will not reset them.  However, using `dired-undo' to re-insert
or delete subdirectories can bypass this machinery.  Hence, you sometimes
may have to reset some subdirectory switches after a `dired-undo'.
You can reset all subdirectory switches to the default using
\\<dired-mode-map>\\[dired-reset-subdir-switches].
See Info node `(emacs)Subdir switches' for more details.

\(fn DIRNAME &optional SWITCHES NO-ERROR-IF-NOT-DIR-P)" t nil)

(autoload 'dired-insert-subdir "dired-aux" "\
Insert this subdirectory into the same dired buffer.
If it is already present, overwrites previous entry,
  else inserts it at its natural place (as `ls -lR' would have done).
With a prefix arg, you may edit the `ls' switches used for this listing.
  You can add `R' to the switches to expand the whole tree starting at
  this subdirectory.
This function takes some pains to conform to `ls -lR' output.

\(fn DIRNAME &optional SWITCHES NO-ERROR-IF-NOT-DIR-P)" t nil)

(autoload 'dired-prev-subdir "dired-aux" "\
Go to previous subdirectory, regardless of level.
When called interactively and not on a subdir line, go to this subdir's line.

\(fn ARG &optional NO-ERROR-IF-NOT-FOUND NO-SKIP)" t nil)

(autoload 'dired-goto-subdir "dired-aux" "\
Go to end of header line of DIR in this dired buffer.
Return value of point on success, otherwise return nil.
The next char is either \\n, or \\r if DIR is hidden.

\(fn DIR)" t nil)

(autoload 'dired-mark-subdir-files "dired-aux" "\
Mark all files except `.' and `..' in current subdirectory.
If the Dired buffer shows multiple directories, this command
marks the files listed in the subdirectory that point is in.

\(fn)" t nil)

(autoload 'dired-kill-subdir "dired-aux" "\
Remove all lines of current subdirectory.
Lower levels are unaffected.

\(fn &optional REMEMBER-MARKS)" t nil)

(autoload 'dired-tree-up "dired-aux" "\
Go up ARG levels in the dired tree.

\(fn ARG)" t nil)

(autoload 'dired-tree-down "dired-aux" "\
Go down in the dired tree.

\(fn)" t nil)

(autoload 'dired-hide-subdir "dired-aux" "\
Hide or unhide the current subdirectory and move to next directory.
Optional prefix arg is a repeat factor.
Use \\[dired-hide-all] to (un)hide all directories.

\(fn ARG)" t nil)

(autoload 'dired-hide-all "dired-aux" "\
Hide all subdirectories, leaving only their header lines.
If there is already something hidden, make everything visible again.
Use \\[dired-hide-subdir] to (un)hide a particular subdirectory.

\(fn &optional IGNORED)" t nil)

(autoload 'dired-isearch-filenames-setup "dired-aux" "\
Set up isearch to search in Dired file names.
Intended to be added to `isearch-mode-hook'.

\(fn)" nil nil)

(autoload 'dired-isearch-filenames "dired-aux" "\
Search for a string using Isearch only in file names in the Dired buffer.

\(fn)" t nil)

(autoload 'dired-isearch-filenames-regexp "dired-aux" "\
Search for a regexp using Isearch only in file names in the Dired buffer.

\(fn)" t nil)

(autoload 'dired-do-isearch "dired-aux" "\
Search for a string through all marked files using Isearch.

\(fn)" t nil)

(autoload 'dired-do-isearch-regexp "dired-aux" "\
Search for a regexp through all marked files using Isearch.

\(fn)" t nil)

(autoload 'dired-do-search "dired-aux" "\
Search through all marked files for a match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

\(fn REGEXP)" t nil)

(autoload 'dired-do-query-replace-regexp "dired-aux" "\
Do `query-replace-regexp' of FROM with TO, on all marked files.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].

\(fn FROM TO &optional DELIMITED)" t nil)

(autoload 'dired-show-file-type "dired-aux" "\
Print the type of FILE, according to the `file' command.
If you give a prefix to this command, and FILE is a symbolic
link, then the type of the file linked to by FILE is printed
instead.

\(fn FILE &optional DEREF-SYMLINKS)" t nil)

;;;***

;;;### (autoloads (dired-do-relsymlink dired-jump-other-window dired-jump)
;;;;;;  "dired-x" "dired-x.el" "2a39a8306a5541c304bc4ab602876f92")
;;; Generated autoloads from dired-x.el

(autoload 'dired-jump "dired-x" "\
Jump to dired buffer corresponding to current buffer.
If in a file, dired the current directory and move to file's line.
If in Dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
buffer and try again.
When OTHER-WINDOW is non-nil, jump to dired buffer in other window.
Interactively with prefix argument, read FILE-NAME and
move to its line in dired.

\(fn &optional OTHER-WINDOW FILE-NAME)" t nil)

(autoload 'dired-jump-other-window "dired-x" "\
Like \\[dired-jump] (`dired-jump') but in other window.

\(fn &optional FILE-NAME)" t nil)

(autoload 'dired-do-relsymlink "dired-x" "\
Relative symlink all marked (or next ARG) files into a directory.
Otherwise make a relative symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/file/name/that/may/change/any/day/bar/foo

For absolute symlinks, use \\[dired-do-symlink].

\(fn &optional ARG)" t nil)

;;;***

;;; End of automatically extracted autoloads.

(provide 'dired)

(run-hooks 'dired-load-hook)		; for your customizations

;;; dired.el ends here
