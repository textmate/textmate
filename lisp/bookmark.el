;;; bookmark.el --- set bookmarks, maybe annotate them, jump to them later

;; Copyright (C) 1993-1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Maintainer: Karl Fogel <kfogel@red-bean.com>
;; Created: July, 1993
;; Keywords: bookmarks, placeholders, annotations

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

;; This package is for setting "bookmarks" in files.  A bookmark
;; associates a string with a location in a certain file.  Thus, you
;; can navigate your way to that location by providing the string.
;; See the "User Variables" section for customizations.


;;; Code:

(require 'pp)
(eval-when-compile (require 'cl))

;;; Misc comments:
;;
;; If variable bookmark-use-annotations is non-nil, an annotation is
;; queried for when setting a bookmark.
;;
;; The bookmark list is sorted lexically by default, but you can turn
;; this off by setting bookmark-sort-flag to nil.  If it is nil, then
;; the list will be presented in the order it is recorded
;; (chronologically), which is actually fairly useful as well.

;;; User Variables

(defgroup bookmark nil
  "Setting, annotation and jumping to bookmarks."
  :group 'matching)


(defcustom bookmark-use-annotations nil
  "If non-nil, saving a bookmark queries for an annotation in a buffer."
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-save-flag t
  "Controls when Emacs saves bookmarks to a file.
--> nil means never save bookmarks, except when `bookmark-save' is
    explicitly called (\\[bookmark-save]).
--> t means save bookmarks when Emacs is killed.
--> Otherwise, it should be a number that is the frequency with which
    the bookmark list is saved (i.e.: the number of times which
    Emacs's bookmark list may be modified before it is automatically
    saved.).  If it is a number, Emacs will also automatically save
    bookmarks when it is killed.

Therefore, the way to get it to save every time you make or delete a
bookmark is to set this variable to 1 (or 0, which produces the same
behavior.)

To specify the file in which to save them, modify the variable
`bookmark-default-file', which is `~/.emacs.bmk' by default."
  :type '(choice (const nil) integer (other t))
  :group 'bookmark)


(defconst bookmark-old-default-file "~/.emacs-bkmrks"
  "The `.emacs.bmk' file used to be called this name.")


;; defvared to avoid a compilation warning:
(defvar bookmark-file nil
  "Old name for `bookmark-default-file'.")

(defcustom bookmark-default-file
  (if bookmark-file
      ;; In case user set `bookmark-file' in her .emacs:
      bookmark-file
    (locate-user-emacs-file "bookmarks" ".emacs.bmk"))
  "File in which to save bookmarks by default."
  :type 'file
  :group 'bookmark)


(defcustom bookmark-version-control 'nospecial
  "Whether or not to make numbered backups of the bookmark file.
It can have four values: t, nil, `never', and `nospecial'.
The first three have the same meaning that they do for the
variable `version-control', and the final value `nospecial' means just
use the value of `version-control'."
  :type '(choice (const nil) (const never) (const nospecial)
		 (other t))
  :group 'bookmark)


(defcustom bookmark-completion-ignore-case t
  "Non-nil means bookmark functions ignore case in completion."
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-sort-flag t
  "Non-nil means that bookmarks will be displayed sorted by bookmark name.
Otherwise they will be displayed in LIFO order (that is, most
recently set ones come first, oldest ones come last)."
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-automatically-show-annotations t
  "Non-nil means show annotations when jumping to a bookmark."
  :type 'boolean
  :group 'bookmark)


(defconst bookmark-bmenu-header-height 2
  "Number of lines used for the *Bookmark List* header.")

(defconst bookmark-bmenu-marks-width 2
  "Number of columns (chars) used for the *Bookmark List* marks column,
including the annotations column.")

(defcustom bookmark-bmenu-file-column 30
  "Column at which to display filenames in a buffer listing bookmarks.
You can toggle whether files are shown with \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-toggle-filenames]."
  :type 'integer
  :group 'bookmark)


(defcustom bookmark-bmenu-toggle-filenames t
  "Non-nil means show filenames when listing bookmarks.
This may result in truncated bookmark names.  To disable this, put the
following in your `.emacs' file:

\(setq bookmark-bmenu-toggle-filenames nil)"
  :type 'boolean
  :group 'bookmark)


(defcustom bookmark-menu-length 70
  "Maximum length of a bookmark name displayed on a popup menu."
  :type 'integer
  :group 'bookmark)

;; FIXME: Is it really worth a customization option?
(defcustom bookmark-search-delay 0.2
  "Time before `bookmark-bmenu-search' updates the display."
  :group 'bookmark
  :type  'integer)

(defface bookmark-menu-heading
  '((t (:inherit font-lock-type-face)))
  "Face used to highlight the heading in bookmark menu buffers."
  :group 'bookmark
  :version "22.1")


;;; No user-serviceable parts beyond this point.

;; Added  for lucid emacs  compatibility, db
(or (fboundp 'defalias)  (fset 'defalias 'fset))

;; suggested for lucid compatibility by david hughes:
(or (fboundp 'frame-height)  (defalias 'frame-height 'screen-height))


;;; Keymap stuff:

;; Set up these bindings dumping time *only*;
;; if the user alters them, don't override the user when loading bookmark.el.

;;;###autoload (define-key ctl-x-r-map "b" 'bookmark-jump)
;;;###autoload (define-key ctl-x-r-map "m" 'bookmark-set)
;;;###autoload (define-key ctl-x-r-map "l" 'bookmark-bmenu-list)

;;;###autoload
(defvar bookmark-map
  (let ((map (make-sparse-keymap)))
    ;; Read the help on all of these functions for details...
    (define-key map "x" 'bookmark-set)
    (define-key map "m" 'bookmark-set) ;"m"ark
    (define-key map "j" 'bookmark-jump)
    (define-key map "g" 'bookmark-jump) ;"g"o
    (define-key map "o" 'bookmark-jump-other-window)
    (define-key map "i" 'bookmark-insert)
    (define-key map "e" 'edit-bookmarks)
    (define-key map "f" 'bookmark-insert-location) ;"f"ind
    (define-key map "r" 'bookmark-rename)
    (define-key map "d" 'bookmark-delete)
    (define-key map "l" 'bookmark-load)
    (define-key map "w" 'bookmark-write)
    (define-key map "s" 'bookmark-save)
    map)
  "Keymap containing bindings to bookmark functions.
It is not bound to any key by default: to bind it
so that you have a bookmark prefix, just use `global-set-key' and bind a
key of your choice to `bookmark-map'.  All interactive bookmark
functions have a binding in this keymap.")

;;;###autoload (fset 'bookmark-map bookmark-map)


;;; Core variables and data structures:
(defvar bookmark-alist ()
  "Association list of bookmarks and their records.
Bookmark functions update the value automatically.
You probably do NOT want to change the value yourself.

The value is an alist with entries of the form

 (BOOKMARK-NAME . PARAM-ALIST)

or the deprecated form (BOOKMARK-NAME PARAM-ALIST).

 BOOKMARK-NAME is the name you gave to the bookmark when creating it.

 PARAM-ALIST is an alist of bookmark information.  The order of the
 entries in PARAM-ALIST is not important.  The possible entries are
 described below.  An entry with a key but null value means the entry
 is not used.

  (filename . FILENAME)
  (position . POS)
  (front-context-string . STR-AFTER-POS)
  (rear-context-string  . STR-BEFORE-POS)
  (handler . HANDLER)
  (annotation . ANNOTATION)

 FILENAME names the bookmarked file.
 POS is the bookmarked buffer position (position in the file).
 STR-AFTER-POS is buffer text that immediately follows POS.
 STR-BEFORE-POS is buffer text that immediately precedes POS.
 ANNOTATION is a string that describes the bookmark.
   See options `bookmark-use-annotations' and
   `bookmark-automatically-show-annotations'.
 HANDLER is a function that provides the bookmark-jump behavior for a
 specific kind of bookmark.  This is the case for Info bookmarks,
 for instance.  HANDLER must accept a bookmark as argument.")

(defvar bookmarks-already-loaded nil
  "Non-nil if and only if bookmarks have been loaded from `bookmark-default-file'.")


;; more stuff added by db.

(defvar bookmark-current-bookmark nil
  "Name of bookmark most recently used in the current file.
It is buffer local, used to make moving a bookmark forward
through a file easier.")

(make-variable-buffer-local 'bookmark-current-bookmark)


(defvar bookmark-alist-modification-count 0
  "Number of modifications to bookmark list since it was last saved.")


(defvar bookmark-search-size 16
  "Length of the context strings recorded on either side of a bookmark.")


(defvar bookmark-current-buffer nil
  "The buffer in which a bookmark is currently being set or renamed.
Functions that insert strings into the minibuffer use this to know
the source buffer for that information; see `bookmark-yank-word' and
`bookmark-insert-current-bookmark' for example.")


(defvar bookmark-yank-point 0
  "The next point from which to pull source text for `bookmark-yank-word'.
This point is in `bookmark-current-buffer'.")


(defvar bookmark-quit-flag nil
  "Non nil make `bookmark-bmenu-search' quit immediately.")

;; Helper functions and macros.

(defmacro with-buffer-modified-unmodified (&rest body)
  "Run BODY while preserving the buffer's `buffer-modified-p' state."
  (let ((was-modified (make-symbol "was-modified")))
    `(let ((,was-modified (buffer-modified-p)))
       (unwind-protect
           (progn ,@body)
         (set-buffer-modified-p ,was-modified)))))

;; Only functions below, in this page and the next one (file formats),
;; need to know anything about the format of bookmark-alist entries.
;; Everyone else should go through them.

(defun bookmark-name-from-full-record (bookmark-record)
  "Return the name of BOOKMARK-RECORD.  BOOKMARK-RECORD is, e.g.,
one element from `bookmark-alist'."
  (car bookmark-record))


(defun bookmark-all-names ()
  "Return a list of all current bookmark names."
  (bookmark-maybe-load-default-file)
  (mapcar 'bookmark-name-from-full-record bookmark-alist))


(defun bookmark-get-bookmark (bookmark-name-or-record &optional noerror)
  "Return the bookmark record corresponding to BOOKMARK-NAME-OR-RECORD.
If BOOKMARK-NAME-OR-RECORD is a string, look for the corresponding
bookmark record in `bookmark-alist'; return it if found, otherwise
error.  Else if BOOKMARK-NAME-OR-RECORD is already a bookmark record,
just return it."
  (cond
   ((consp bookmark-name-or-record) bookmark-name-or-record)
   ((stringp bookmark-name-or-record)
    (or (assoc-string bookmark-name-or-record bookmark-alist
                      bookmark-completion-ignore-case)
        (unless noerror (error "Invalid bookmark %s"
                               bookmark-name-or-record))))))


(defun bookmark-get-bookmark-record (bookmark-name-or-record)
  "Return the record portion of the entry for BOOKMARK-NAME-OR-RECORD in
`bookmark-alist' (that is, all information but the name)."
  (let ((alist (cdr (bookmark-get-bookmark bookmark-name-or-record))))
    ;; The bookmark objects can either look like (NAME ALIST) or
    ;; (NAME . ALIST), so we have to distinguish the two here.
    (if (and (null (cdr alist)) (consp (caar alist)))
        (car alist) alist)))


(defun bookmark-set-name (bookmark-name-or-record newname)
  "Set BOOKMARK-NAME-OR-RECORD's name to NEWNAME."
  (setcar (bookmark-get-bookmark bookmark-name-or-record) newname))

(defun bookmark-prop-get (bookmark-name-or-record prop)
  "Return the property PROP of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (cdr (assq prop (bookmark-get-bookmark-record bookmark-name-or-record))))

(defun bookmark-prop-set (bookmark-name-or-record prop val)
  "Set the property PROP of BOOKMARK-NAME-OR-RECORD to VAL."
  (let ((cell (assq
               prop (bookmark-get-bookmark-record bookmark-name-or-record))))
    (if cell
        (setcdr cell val)
      (nconc (bookmark-get-bookmark-record bookmark-name-or-record)
             (list (cons prop val))))))

(defun bookmark-get-annotation (bookmark-name-or-record)
  "Return the annotation of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'annotation))

(defun bookmark-set-annotation (bookmark-name-or-record ann)
  "Set the annotation of BOOKMARK-NAME-OR-RECORD to ANN."
  (bookmark-prop-set bookmark-name-or-record 'annotation ann))


(defun bookmark-get-filename (bookmark-name-or-record)
  "Return the full filename of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'filename))


(defun bookmark-set-filename (bookmark-name-or-record filename)
  "Set the full filename of BOOKMARK-NAME-OR-RECORD to FILENAME."
  (bookmark-prop-set bookmark-name-or-record 'filename filename))


(defun bookmark-get-position (bookmark-name-or-record)
  "Return the position (i.e.: point) of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'position))


(defun bookmark-set-position (bookmark-name-or-record position)
  "Set the position (i.e.: point) of BOOKMARK-NAME-OR-RECORD to POSITION."
  (bookmark-prop-set bookmark-name-or-record 'position position))


(defun bookmark-get-front-context-string (bookmark-name-or-record)
  "Return the front-context-string of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'front-context-string))


(defun bookmark-set-front-context-string (bookmark-name-or-record string)
  "Set the front-context-string of BOOKMARK-NAME-OR-RECORD to STRING."
  (bookmark-prop-set bookmark-name-or-record 'front-context-string string))


(defun bookmark-get-rear-context-string (bookmark-name-or-record)
  "Return the rear-context-string of BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'rear-context-string))


(defun bookmark-set-rear-context-string (bookmark-name-or-record string)
  "Set the rear-context-string of BOOKMARK-NAME-OR-RECORD to STRING."
  (bookmark-prop-set bookmark-name-or-record 'rear-context-string string))


(defun bookmark-get-handler (bookmark-name-or-record)
  "Return the handler function for BOOKMARK-NAME-OR-RECORD, or nil if none."
  (bookmark-prop-get bookmark-name-or-record 'handler))

(defvar bookmark-history nil
  "The history list for bookmark functions.")


(defun bookmark-completing-read (prompt &optional default)
  "Prompting with PROMPT, read a bookmark name in completion.
PROMPT will get a \": \" stuck on the end no matter what, so you
probably don't want to include one yourself.
Optional second arg DEFAULT is a string to return if the user enters
the empty string."
  (bookmark-maybe-load-default-file) ; paranoia
  (if (listp last-nonmenu-event)
      (bookmark-menu-popup-paned-menu t prompt
				      (if bookmark-sort-flag
					  (sort (bookmark-all-names)
						'string-lessp)
					(bookmark-all-names)))
    (let* ((completion-ignore-case bookmark-completion-ignore-case)
	   (default default)
	   (prompt (concat prompt (if default
                                      (format " (%s): " default)
                                    ": ")))
	   (str
	    (completing-read prompt
			     bookmark-alist
			     nil
			     0
			     nil
			     'bookmark-history)))
      (if (string-equal "" str) default str))))


(defmacro bookmark-maybe-historicize-string (string)
  "Put STRING into the bookmark prompt history, if caller non-interactive.
We need this because sometimes bookmark functions are invoked from
menus, so `completing-read' never gets a chance to set `bookmark-history'."
  `(or
    (called-interactively-p 'interactive)
    (setq bookmark-history (cons ,string bookmark-history))))

(defvar bookmark-make-record-function 'bookmark-make-record-default
  "A function that should be called to create a bookmark record.
Modes may set this variable buffer-locally to enable bookmarking of
locations that should be treated specially, such as Info nodes,
news posts, images, pdf documents, etc.

The function will be called with no arguments.
It should signal a user error if it is unable to construct a record for
the current location.

The returned record should be a cons cell of the form (NAME . ALIST)
where ALIST is as described in `bookmark-alist' and may typically contain
a special cons (handler . HANDLER-FUNC) which specifies the handler function
that should be used instead of `bookmark-default-handler' to open this
bookmark.  See the documentation for `bookmark-alist' for more.

NAME is a suggested name for the constructed bookmark.  It can be nil
in which case a default heuristic will be used.  The function can also
equivalently just return ALIST without NAME.")

(defun bookmark-make-record ()
  "Return a new bookmark record (NAME . ALIST) for the current location."
  (let ((record (funcall bookmark-make-record-function)))
    ;; Set up default name.
    (if (stringp (car record))
        ;; The function already provided a default name.
        record
      (if (car record) (push nil record))
      (setcar record (or bookmark-current-bookmark (bookmark-buffer-name)))
      record)))

(defun bookmark-store (name alist no-overwrite)
  "Store the bookmark NAME with data ALIST.
If NO-OVERWRITE is non-nil and another bookmark of the same name already
exists in `bookmark-alist', record the new bookmark without throwing away the
old one."
  (bookmark-maybe-load-default-file)
  (let ((stripped-name (copy-sequence name)))
    (or (featurep 'xemacs)
        ;; XEmacs's `set-text-properties' doesn't work on
        ;; free-standing strings, apparently.
        (set-text-properties 0 (length stripped-name) nil stripped-name))
    (if (and (not no-overwrite)
             (bookmark-get-bookmark stripped-name 'noerror))
        ;; already existing bookmark under that name and
        ;; no prefix arg means just overwrite old bookmark
        ;; Use the new (NAME . ALIST) format.
        (setcdr (bookmark-get-bookmark stripped-name) alist)

      ;; otherwise just cons it onto the front (either the bookmark
      ;; doesn't exist already, or there is no prefix arg.  In either
      ;; case, we want the new bookmark consed onto the alist...)

      (push (cons stripped-name alist) bookmark-alist))

    ;; Added by db
    (setq bookmark-current-bookmark stripped-name)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))

    (setq bookmark-current-bookmark stripped-name)
    (bookmark-bmenu-surreptitiously-rebuild-list)))

(defun bookmark-make-record-default (&optional no-file no-context posn)
  "Return the record describing the location of a new bookmark.
Point should be at the buffer in which the bookmark is being set,
and normally should be at the position where the bookmark is desired,
but see the optional arguments for other possibilities.

If NO-FILE is non-nil, then only return the subset of the
record that pertains to the location within the buffer, leaving off
the part that records the filename.

If NO-CONTEXT is non-nil, do not include the front- and rear-context
strings in the record -- the position is enough.

If POSN is non-nil, record POSN as the point instead of `(point)'."
  `(,@(unless no-file `((filename . ,(bookmark-buffer-file-name))))
    ,@(unless no-context `((front-context-string
                           . ,(if (>= (- (point-max) (point))
                                      bookmark-search-size)
                                  (buffer-substring-no-properties
                                   (point)
                                   (+ (point) bookmark-search-size))
                                  nil))))
    ,@(unless no-context `((rear-context-string
                           . ,(if (>= (- (point) (point-min))
                                      bookmark-search-size)
                                  (buffer-substring-no-properties
                                   (point)
                                   (- (point) bookmark-search-size))
                                  nil))))
    (position . ,(or posn (point)))))


;;; File format stuff

;; *IMPORTANT NOTICE* If you are thinking about modifying (redefining)
;; the bookmark file format -- please don't.  The current format
;; should be extensible enough.  If you feel the need to change it,
;; please discuss it with other Emacs developers first.
;;
;; The format of `bookmark-alist' has changed twice in its lifetime.
;; This comment describes the three formats, FIRST, SECOND, and
;; CURRENT.
;;
;; The FIRST format was used prior to Emacs 20:
;;
;;       ((BOOKMARK-NAME (FILENAME
;;                          STRING-IN-FRONT
;;                          STRING-BEHIND
;;                          POINT))
;;        ...)
;;
;; The SECOND format was introduced in Emacs 20:
;;
;;       ((BOOKMARK-NAME ((filename   . FILENAME)
;;                        (position   . POS)
;;                        (front-context-string . STR-AFTER-POS)
;;                        (rear-context-string  . STR-BEFORE-POS)
;;                        (annotation . ANNOTATION)
;;                        (whatever   . VALUE)
;;                        ...
;;                       ))
;;        ...)
;;
;; The CURRENT format was introduced in Emacs 22:
;;
;;       ((BOOKMARK-NAME (filename   . FILENAME)
;;                       (position   . POS)
;;                       (front-context-string . STR-AFTER-POS)
;;                       (rear-context-string  . STR-BEFORE-POS)
;;                       (annotation . ANNOTATION)
;;                       (whatever   . VALUE)
;;                       ...
;;                       )
;;        ...)
;;
;; Both FIRST and SECOND have the same level of nesting: the cadr of a
;; bookmark record is a list of entry information.  FIRST and SECOND
;; differ in the form of the record information: FIRST uses a list of
;; atoms, and SECOND uses an alist.  In the FIRST format, the order of
;; the list elements matters.  In the SECOND format, the order of the
;; alist elements is unimportant.  The SECOND format facilitates the
;; addition of new kinds of elements, to support new kinds of
;; bookmarks or code evolution.
;;
;; The CURRENT format removes a level of nesting wrt FIRST and SECOND,
;; saving one cons cell per bookmark: the cadr of a bookmark record is
;; no longer a cons.  Why that change was made remains a mystery --
;; just be aware of it.  (Be aware too that this explanatory comment
;; was incorrect in Emacs 22 and Emacs 23.1.)
;;
;; To deal with the change from FIRST format to SECOND, conversion
;; code was added, and it is still in use.  See
;; `bookmark-maybe-upgrade-file-format'.
;;
;; No conversion from SECOND to CURRENT is done.  Instead, the code
;; handles both formats OK.  It must continue to do so.
;;
;; See the doc string of `bookmark-alist' for information about the
;; elements that define a bookmark (e.g. `filename').


(defconst bookmark-file-format-version 1
  "The current version of the format used by bookmark files.
You should never need to change this.")


(defconst bookmark-end-of-version-stamp-marker
  "-*- End Of Bookmark File Format Version Stamp -*-\n"
  "This string marks the end of the version stamp in a bookmark file.")


(defun bookmark-alist-from-buffer ()
  "Return a `bookmark-alist' (in any format) from the current buffer.
The buffer must of course contain bookmark format information.
Does not care from where in the buffer it is called, and does not
affect point."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward bookmark-end-of-version-stamp-marker nil t)
        (read (current-buffer))
      ;; Else we're dealing with format version 0
      (if (search-forward "(" nil t)
          (progn
            (forward-char -1)
            (read (current-buffer)))
        ;; Else no hope of getting information here.
        (error "Not bookmark format")))))


(defun bookmark-upgrade-version-0-alist (old-list)
  "Upgrade a version 0 alist OLD-LIST to the current version."
  (mapcar
   (lambda (bookmark)
     (let* ((name      (car bookmark))
            (record    (car (cdr bookmark)))
            (filename  (nth 0 record))
            (front-str (nth 1 record))
            (rear-str  (nth 2 record))
            (position  (nth 3 record))
            (ann       (nth 4 record)))
       (list
        name
        `((filename             .    ,filename)
          (front-context-string .    ,(or front-str ""))
          (rear-context-string  .    ,(or rear-str  ""))
          (position             .    ,position)
          (annotation           .    ,ann)))))
   old-list))


(defun bookmark-upgrade-file-format-from-0 ()
  "Upgrade a bookmark file of format 0 (the original format) to format 1.
This expects to be called from `point-min' in a bookmark file."
  (message "Upgrading bookmark format from 0 to %d..."
           bookmark-file-format-version)
  (let* ((old-list (bookmark-alist-from-buffer))
         (new-list (bookmark-upgrade-version-0-alist old-list)))
    (delete-region (point-min) (point-max))
    (bookmark-insert-file-format-version-stamp)
    (pp new-list (current-buffer))
    (save-buffer))
  (goto-char (point-min))
  (message "Upgrading bookmark format from 0 to %d...done"
           bookmark-file-format-version)
  )


(defun bookmark-grok-file-format-version ()
  "Return an integer which is the file-format version of this bookmark file.
This expects to be called from `point-min' in a bookmark file."
  (if (looking-at "^;;;;")
      (save-excursion
        (save-match-data
          (re-search-forward "[0-9]")
          (forward-char -1)
          (read (current-buffer))))
    ;; Else this is format version 0, the original one, which didn't
    ;; even have version stamps.
    0))


(defun bookmark-maybe-upgrade-file-format ()
  "Check the file-format version of this bookmark file.
If the version is not up-to-date, upgrade it automatically.
This expects to be called from `point-min' in a bookmark file."
  (let ((version (bookmark-grok-file-format-version)))
    (cond
     ((= version bookmark-file-format-version)
      ) ; home free -- version is current
     ((= version 0)
      (bookmark-upgrade-file-format-from-0))
     (t
      (error "Bookmark file format version strangeness")))))


(defun bookmark-insert-file-format-version-stamp ()
  "Insert text indicating current version of bookmark file format."
  (insert
   (format ";;;; Emacs Bookmark Format Version %d ;;;;\n"
           bookmark-file-format-version))
  (insert ";;; This format is meant to be slightly human-readable;\n"
          ";;; nevertheless, you probably don't want to edit it.\n"
          ";;; "
          bookmark-end-of-version-stamp-marker))


;;; end file-format stuff


;;; Generic helpers.

(defun bookmark-maybe-message (fmt &rest args)
  "Apply `message' to FMT and ARGS, but only if the display is fast enough."
  (if (>= baud-rate 9600)
      (apply 'message fmt args)))


;;; Core code:

(defvar bookmark-minibuffer-read-name-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-w" 'bookmark-yank-word)
    ;; This C-u binding might not be very useful any more now that we
    ;; provide access to the default via the standard M-n binding.
    ;; Maybe we should just remove it?  --Stef-08
    (define-key map "\C-u" 'bookmark-insert-current-bookmark)
    map))

;;;###autoload
(defun bookmark-set (&optional name no-overwrite)
  "Set a bookmark named NAME at the current location.
If name is nil, then prompt the user.

With a prefix arg (non-nil NO-OVERWRITE), do not overwrite any
existing bookmark that has the same name as NAME, but instead push the
new bookmark onto the bookmark alist.  The most recently set bookmark
with name NAME is thus the one in effect at any given time, but the
others are still there, should the user decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-u inserts (at the bookmark name prompt) the name of the last
bookmark used in the document where the new bookmark is being set;
this helps you use a single bookmark name to track progress through a
large document.  If there is no prior bookmark for this document, then
C-u inserts an appropriate name based on the buffer or file.

Use \\[bookmark-delete] to remove bookmarks (you give it a name and
it removes only the first instance of a bookmark with that name from
the list of bookmarks.)"
  (interactive (list nil current-prefix-arg))
  (unwind-protect
       (let* ((record (bookmark-make-record))
              (default (car record)))

         (bookmark-maybe-load-default-file)
         ;; Don't set `bookmark-yank-point' and `bookmark-current-buffer'
         ;; if they have been already set in another buffer. (e.g gnus-art).
         (unless (and bookmark-yank-point
                      bookmark-current-buffer)
           (setq bookmark-yank-point (point))
           (setq bookmark-current-buffer (current-buffer)))

         (let ((str
                (or name
                    (read-from-minibuffer
                     (format "Set bookmark (%s): " default)
                     nil
                     bookmark-minibuffer-read-name-map
                     nil nil default))))
           (and (string-equal str "") (setq str default))
           (bookmark-store str (cdr record) no-overwrite)

           ;; Ask for an annotation buffer for this bookmark
           (when bookmark-use-annotations
             (bookmark-edit-annotation str))))
    (setq bookmark-yank-point nil)
    (setq bookmark-current-buffer nil)))


(defun bookmark-kill-line (&optional newline-too)
  "Kill from point to end of line.
If optional arg NEWLINE-TOO is non-nil, delete the newline too.
Does not affect the kill ring."
  (let ((eol (line-end-position)))
    (delete-region (point) eol)
    (if (and newline-too (looking-at "\n"))
        (delete-char 1))))


;; Defvars to avoid compilation warnings:
(defvar bookmark-annotation-name nil
  "Variable holding the name of the bookmark.
This is used in `bookmark-edit-annotation' to record the bookmark
whose annotation is being edited.")


(defun bookmark-default-annotation-text (bookmark-name)
  "Return default annotation text for BOOKMARK-NAME.
The default annotation text is simply some text explaining how to use
annotations."
  (concat "#  Type the annotation for bookmark '" bookmark-name "' here.\n"
	  "#  All lines which start with a '#' will be deleted.\n"
	  "#  Type C-c C-c when done.\n#\n"
	  "#  Author: " (user-full-name) " <" (user-login-name) "@"
	  (system-name) ">\n"
	  "#  Date:    " (current-time-string) "\n"))


(defvar bookmark-edit-annotation-text-func 'bookmark-default-annotation-text
  "Function to return default text to use for a bookmark annotation.
It takes one argument, the name of the bookmark, as a string.")
(define-obsolete-variable-alias 'bookmark-read-annotation-text-func
  'bookmark-edit-annotation-text-func "23.1")

(defvar bookmark-edit-annotation-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-c\C-c" 'bookmark-send-edited-annotation)
    map)
  "Keymap for editing an annotation of a bookmark.")


(defun bookmark-edit-annotation-mode (bookmark-name-or-record)
  "Mode for editing the annotation of bookmark BOOKMARK-NAME-OR-RECORD.
When you have finished composing, type \\[bookmark-send-annotation].

\\{bookmark-edit-annotation-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'bookmark-annotation-name)
  (setq bookmark-annotation-name bookmark-name-or-record)
  (use-local-map bookmark-edit-annotation-mode-map)
  (setq major-mode 'bookmark-edit-annotation-mode
        mode-name "Edit Bookmark Annotation")
  (insert (funcall bookmark-edit-annotation-text-func bookmark-name-or-record))
  (let ((annotation (bookmark-get-annotation bookmark-name-or-record)))
    (if (and annotation (not (string-equal annotation "")))
	(insert annotation)))
  (run-mode-hooks 'text-mode-hook))


(defun bookmark-send-edited-annotation ()
  "Use buffer contents as annotation for a bookmark.
Lines beginning with `#' are ignored."
  (interactive)
  (if (not (eq major-mode 'bookmark-edit-annotation-mode))
      (error "Not in bookmark-edit-annotation-mode"))
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (looking-at "^#")
        (bookmark-kill-line t)
      (forward-line 1)))
  ;; Take no chances with text properties.
  (let ((annotation (buffer-substring-no-properties (point-min) (point-max)))
	(bookmark-name bookmark-annotation-name))
    (bookmark-set-annotation bookmark-name annotation)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (bookmark-bmenu-surreptitiously-rebuild-list))
  (kill-buffer (current-buffer)))


(defun bookmark-edit-annotation (bookmark-name-or-record)
  "Pop up a buffer for editing bookmark BOOKMARK-NAME-OR-RECORD's annotation."
  (pop-to-buffer (generate-new-buffer-name "*Bookmark Annotation Compose*"))
  (bookmark-edit-annotation-mode bookmark-name-or-record))


(defun bookmark-insert-current-bookmark ()
  "Insert into the bookmark name currently being set the value of
`bookmark-current-bookmark' in `bookmark-current-buffer', defaulting
to the buffer's file name if `bookmark-current-bookmark' is nil."
  (interactive)
  (let ((str
	 (with-current-buffer bookmark-current-buffer
	   (or bookmark-current-bookmark
               (bookmark-buffer-name)))))
    (insert str)))


(defun bookmark-buffer-name ()
  "Return the name of the current buffer in a form usable as a bookmark name.
If the buffer is associated with a file or directory, use that name."
  (cond
   ;; Or are we a file?
   (buffer-file-name (file-name-nondirectory buffer-file-name))
   ;; Or are we a directory?
   ((and (boundp 'dired-directory) dired-directory)
    (let* ((dirname (if (stringp dired-directory)
                        dired-directory
                      (car dired-directory)))
           (idx (1- (length dirname))))
      ;; Strip the trailing slash.
      (if (= ?/ (aref dirname idx))
          (file-name-nondirectory (substring dirname 0 idx))
        ;; Else return the current-buffer
        (buffer-name (current-buffer)))))
   ;; If all else fails, use the buffer's name.
   (t
    (buffer-name (current-buffer)))))


(defun bookmark-yank-word ()
  "Get the next word from buffer `bookmark-current-buffer' and append
it to the name of the bookmark currently being set, advancing
`bookmark-yank-point' by one word."
  (interactive)
  (let ((string (with-current-buffer bookmark-current-buffer
                  (goto-char bookmark-yank-point)
                  (buffer-substring-no-properties
                   (point)
                   (progn
                     (forward-word 1)
                     (setq bookmark-yank-point (point)))))))
    (insert string)))

(defun bookmark-buffer-file-name ()
  "Return the current buffer's file in a way useful for bookmarks."
  ;; Abbreviate the path, both so it's shorter and so it's more
  ;; portable.  E.g., the user's home dir might be a different
  ;; path on different machines, but "~/" will still reach it.
  (abbreviate-file-name
   (cond
    (buffer-file-name buffer-file-name)
    ((and (boundp 'dired-directory) dired-directory)
     (if (stringp dired-directory)
         dired-directory
       (car dired-directory)))
    (t (error "Buffer not visiting a file or directory")))))


(defun bookmark-maybe-load-default-file ()
  "If bookmarks have not been loaded from the default place, load them."
  (and (not bookmarks-already-loaded)
       (null bookmark-alist)
       (prog2
           (and
            ;; Possibly the old bookmark file, "~/.emacs-bkmrks", needs
            ;; to be renamed.
            (file-exists-p bookmark-old-default-file)
            (not (file-exists-p bookmark-default-file))
            (rename-file bookmark-old-default-file
                         bookmark-default-file))
           ;; return t so the `and' will continue...
           t)

       (file-readable-p bookmark-default-file)
       (bookmark-load bookmark-default-file t t)
       (setq bookmarks-already-loaded t)))


(defun bookmark-maybe-sort-alist ()
  "Return `bookmark-alist' for display.
If `bookmark-sort-flag' is non-nil, then return a sorted copy of the alist."
  (if bookmark-sort-flag
      (sort (copy-alist bookmark-alist)
            (function
             (lambda (x y) (string-lessp (car x) (car y)))))
    bookmark-alist))


(defvar bookmark-after-jump-hook nil
  "Hook run after `bookmark-jump' jumps to a bookmark.
Useful for example to unhide text in `outline-mode'.")

(defun bookmark--jump-via (bookmark-name-or-record display-function)
  "Handle BOOKMARK-NAME-OR-RECORD, then call DISPLAY-FUNCTION with
current buffer as argument.

After calling DISPLAY-FUNCTION, set window point to the point specified
by BOOKMARK-NAME-OR-RECORD, if necessary, run `bookmark-after-jump-hook',
and then show any annotations for this bookmark."
  (bookmark-handle-bookmark bookmark-name-or-record)
  (save-current-buffer
    (funcall display-function (current-buffer)))
  (let ((win (get-buffer-window (current-buffer) 0)))
    (if win (set-window-point win (point))))
  ;; FIXME: we used to only run bookmark-after-jump-hook in
  ;; `bookmark-jump' itself, but in none of the other commands.
  (run-hooks 'bookmark-after-jump-hook)
  (if bookmark-automatically-show-annotations
      ;; if there is an annotation for this bookmark,
      ;; show it in a buffer.
      (bookmark-show-annotation bookmark-name-or-record)))


;;;###autoload
(defun bookmark-jump (bookmark &optional display-func)
  "Jump to bookmark BOOKMARK (a point in some file).
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and `bookmark-jump'
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record.

BOOKMARK is usually a bookmark name (a string).  It can also be a
bookmark record, but this is usually only done by programmatic callers.

If DISPLAY-FUNC is non-nil, it is a function to invoke to display the
bookmark.  It defaults to `switch-to-buffer'.  A typical value for
DISPLAY-FUNC would be `switch-to-buffer-other-window'."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark"
				   bookmark-current-bookmark)))
  (unless bookmark
    (error "No bookmark specified"))
  (bookmark-maybe-historicize-string bookmark)
  (bookmark--jump-via bookmark (or display-func 'switch-to-buffer)))


;;;###autoload
(defun bookmark-jump-other-window (bookmark)
  "Jump to BOOKMARK in another window.  See `bookmark-jump' for more."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark (in another window)"
                                   bookmark-current-bookmark)))
  (bookmark-jump bookmark 'switch-to-buffer-other-window))


(defun bookmark-jump-noselect (bookmark)
  "Return the location pointed to by BOOKMARK (see `bookmark-jump').
The return value has the form (BUFFER . POINT).

Note: this function is deprecated and is present for Emacs 22
compatibility only."
  (save-excursion
    (bookmark-handle-bookmark bookmark)
    (cons (current-buffer) (point))))

(make-obsolete 'bookmark-jump-noselect 'bookmark-handle-bookmark "23.1")

(defun bookmark-handle-bookmark (bookmark-name-or-record)
  "Call BOOKMARK-NAME-OR-RECORD's handler or `bookmark-default-handler'
if it has none.  This changes current buffer and point and returns nil,
or signals a `file-error'.

If BOOKMARK-NAME-OR-RECORD has no file, this is a no-op.  If
BOOKMARK-NAME-OR-RECORD has a file, but that file no longer exists,
then offer interactively to relocate BOOKMARK-NAME-OR-RECORD."
  (condition-case err
      (funcall (or (bookmark-get-handler bookmark-name-or-record)
                   'bookmark-default-handler)
               (bookmark-get-bookmark bookmark-name-or-record))
    (bookmark-error-no-filename         ;file-error
     ;; We were unable to find the marked file, so ask if user wants to
     ;; relocate the bookmark, else remind them to consider deletion.
     (when (stringp bookmark-name-or-record)
       ;; `bookmark-name-or-record' can be either a bookmark name
       ;; (from `bookmark-alist')  or a bookmark object.  If it's an
       ;; object, we assume it's a bookmark used internally by some
       ;; other package.
       (let ((file (bookmark-get-filename bookmark-name-or-record)))
         (when file        ;Don't know how to relocate if there's no `file'.
           ;; If file is not a dir, directory-file-name just returns file.
           (let ((display-name (directory-file-name file)))
             (ding)
             ;; Dialog boxes can accept a file target, but usually don't
             ;; know how to accept a directory target (at least, this
             ;; is true in Gnome on GNU/Linux, and Bug#4230 says it's
             ;; true on Windows as well).  So we suppress file dialogs
             ;; when relocating.
             (let ((use-dialog-box nil)
                   (use-file-dialog nil))
               (if (y-or-n-p (concat display-name " nonexistent.  Relocate \""
                                     bookmark-name-or-record "\"? "))
                   (progn
                     (bookmark-relocate bookmark-name-or-record)
                     ;; Try again.
                     (funcall (or (bookmark-get-handler bookmark-name-or-record)
                                  'bookmark-default-handler)
                              (bookmark-get-bookmark bookmark-name-or-record)))
                 (message
                  "Bookmark not relocated; consider removing it (%s)."
                  bookmark-name-or-record)
                 (signal (car err) (cdr err))))))))))
  ;; Added by db.
  (when (stringp bookmark-name-or-record)
    (setq bookmark-current-bookmark bookmark-name-or-record))
  nil)

(put 'bookmark-error-no-filename
     'error-conditions
     '(error bookmark-errors bookmark-error-no-filename))
(put 'bookmark-error-no-filename
     'error-message
     "Bookmark has no associated file (or directory)")

(defun bookmark-default-handler (bmk-record)
  "Default handler to jump to a particular bookmark location.
BMK-RECORD is a bookmark record, not a bookmark name (i.e., not a string).
Changes current buffer and point and returns nil, or signals a `file-error'."
  (let ((file          (bookmark-get-filename bmk-record))
	(buf           (bookmark-prop-get bmk-record 'buffer))
        (forward-str   (bookmark-get-front-context-string bmk-record))
        (behind-str    (bookmark-get-rear-context-string bmk-record))
        (place         (bookmark-get-position bmk-record)))
    (set-buffer
     (cond
      ((and file (file-readable-p file) (not (buffer-live-p buf)))
       (find-file-noselect file))
      ;; No file found.  See if buffer BUF have been created.
      ((and buf (get-buffer buf)))
      (t ;; If not, raise error.
       (signal 'bookmark-error-no-filename (list 'stringp file)))))
    (if place (goto-char place))
    ;; Go searching forward first.  Then, if forward-str exists and
    ;; was found in the file, we can search backward for behind-str.
    ;; Rationale is that if text was inserted between the two in the
    ;; file, it's better to be put before it so you can read it,
    ;; rather than after and remain perhaps unaware of the changes.
    (when (and forward-str (search-forward forward-str (point-max) t))
      (goto-char (match-beginning 0)))
    (when (and behind-str (search-backward behind-str (point-min) t))
      (goto-char (match-end 0)))
    nil))

;;;###autoload
(defun bookmark-relocate (bookmark-name)
  "Relocate BOOKMARK-NAME to another file, reading file name with minibuffer.

This makes an already existing bookmark point to that file, instead of
the one it used to point at.  Useful when a file has been renamed
after a bookmark was set in it."
  (interactive (list (bookmark-completing-read "Bookmark to relocate")))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let* ((bmrk-filename (bookmark-get-filename bookmark-name))
         (newloc (abbreviate-file-name
                  (expand-file-name
                   (read-file-name
                    (format "Relocate %s to: " bookmark-name)
                    (file-name-directory bmrk-filename))))))
    (bookmark-set-filename bookmark-name newloc)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))
    (bookmark-bmenu-surreptitiously-rebuild-list)))


;;;###autoload
(defun bookmark-insert-location (bookmark-name &optional no-history)
  "Insert the name of the file associated with BOOKMARK-NAME.

Optional second arg NO-HISTORY means don't record this in the
minibuffer history list `bookmark-history'."
  (interactive (list (bookmark-completing-read "Insert bookmark location")))
  (or no-history (bookmark-maybe-historicize-string bookmark-name))
  (let ((start (point)))
    (prog1
	(insert (bookmark-location bookmark-name))
      (if (display-mouse-p)
	  (add-text-properties
	   start
	   (save-excursion (re-search-backward
			    "[^ \t]")
                           (1+ (point)))
	   '(mouse-face highlight
	     follow-link t
	     help-echo "mouse-2: go to this bookmark in other window"))))))

;;;###autoload
(defalias 'bookmark-locate 'bookmark-insert-location)

(defun bookmark-location (bookmark-name-or-record)
  "Return a description of the location of BOOKMARK-NAME-OR-RECORD."
  (bookmark-maybe-load-default-file)
  ;; We could call the `handler' and ask for it to construct a description
  ;; dynamically: it would open up several new possibilities, but it
  ;; would have the major disadvantage of forcing to load each and
  ;; every handler when the user calls bookmark-menu.
  (or (bookmark-prop-get bookmark-name-or-record 'location)
      (bookmark-get-filename bookmark-name-or-record)
      "-- Unknown location --"))


;;;###autoload
(defun bookmark-rename (old-name &optional new-name)
  "Change the name of OLD-NAME bookmark to NEW-NAME name.
If called from keyboard, prompt for OLD-NAME and NEW-NAME.
If called from menubar, select OLD-NAME from a menu and prompt for NEW-NAME.

If called from Lisp, prompt for NEW-NAME if only OLD-NAME was passed
as an argument.  If called with two strings, then no prompting is done.
You must pass at least OLD-NAME when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consecutive words from the text of the buffer into the new bookmark
name."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (bookmark-maybe-historicize-string old-name)
  (bookmark-maybe-load-default-file)

  (setq bookmark-yank-point (point))
  (setq bookmark-current-buffer (current-buffer))
  (let ((final-new-name
         (or new-name   ; use second arg, if non-nil
             (read-from-minibuffer
              "New name: "
              nil
              (let ((now-map (copy-keymap minibuffer-local-map)))
                (define-key now-map "\C-w" 'bookmark-yank-word)
                now-map)
              nil
              'bookmark-history))))
    (bookmark-set-name old-name final-new-name)
    (setq bookmark-current-bookmark final-new-name)
    (bookmark-bmenu-surreptitiously-rebuild-list)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (bookmark-save))))


;;;###autoload
(defun bookmark-insert (bookmark-name)
  "Insert the text of the file pointed to by bookmark BOOKMARK-NAME.
BOOKMARK-NAME is a bookmark name (a string), not a bookmark record.

You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this."
  (interactive (list (bookmark-completing-read "Insert bookmark contents")))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((orig-point (point))
	(str-to-insert
	 (save-current-buffer
           (bookmark-handle-bookmark bookmark-name)
	   (buffer-string))))
    (insert str-to-insert)
    (push-mark)
    (goto-char orig-point)))


;;;###autoload
(defun bookmark-delete (bookmark-name &optional batch)
  "Delete BOOKMARK-NAME from the bookmark list.

Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark (that is, the
one most recently used in this file, if any).
Optional second arg BATCH means don't update the bookmark list buffer,
probably because we were called from there."
  (interactive
   (list (bookmark-completing-read "Delete bookmark"
				   bookmark-current-bookmark)))
  (bookmark-maybe-historicize-string bookmark-name)
  (bookmark-maybe-load-default-file)
  (let ((will-go (bookmark-get-bookmark bookmark-name 'noerror)))
    (setq bookmark-alist (delq will-go bookmark-alist))
    ;; Added by db, nil bookmark-current-bookmark if the last
    ;; occurrence has been deleted
    (or (bookmark-get-bookmark bookmark-current-bookmark 'noerror)
        (setq bookmark-current-bookmark nil)))
  (unless batch
    (bookmark-bmenu-surreptitiously-rebuild-list))
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p)
    (bookmark-save)))


(defun bookmark-time-to-save-p (&optional final-time)
  "Return t if it is time to save bookmarks to disk, nil otherwise.
Optional argument FINAL-TIME means this is being called when Emacs
is being killed, so save even if `bookmark-save-flag' is a number and
is greater than `bookmark-alist-modification-count'."
  ;; By Gregory M. Saunders <saunders{_AT_}cis.ohio-state.edu>
  (cond (final-time
	 (and (> bookmark-alist-modification-count 0)
	      bookmark-save-flag))
	((numberp bookmark-save-flag)
	 (>= bookmark-alist-modification-count bookmark-save-flag))
	(t
	 nil)))


;;;###autoload
(defun bookmark-write ()
  "Write bookmarks to a file (reading the file name with the minibuffer).
Don't use this in Lisp programs; use `bookmark-save' instead."
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-save t))


;;;###autoload
(defun bookmark-save (&optional parg file)
  "Save currently defined bookmarks.
Saves by default in the file defined by the variable
`bookmark-default-file'.  With a prefix arg, save it in file FILE
\(second argument).

If you are calling this from Lisp, the two arguments are PARG and
FILE, and if you just want it to write to the default file, then
pass no arguments.  Or pass in nil and FILE, and it will save in FILE
instead.  If you pass in one argument, and it is non-nil, then the
user will be interactively queried for a file to save in.

When you want to load in the bookmarks from a file, use
`bookmark-load', \\[bookmark-load].  That function will prompt you
for a file, defaulting to the file defined by variable
`bookmark-default-file'."
  (interactive "P")
  (bookmark-maybe-load-default-file)
  (cond
   ((and (null parg) (null file))
    ;;whether interactive or not, write to default file
    (bookmark-write-file bookmark-default-file))
   ((and (null parg) file)
    ;;whether interactive or not, write to given file
    (bookmark-write-file file))
   ((and parg (not file))
    ;;have been called interactively w/ prefix arg
    (let ((file (read-file-name "File to save bookmarks in: ")))
      (bookmark-write-file file)))
   (t ; someone called us with prefix-arg *and* a file, so just write to file
    (bookmark-write-file file)))
  ;; signal that we have synced the bookmark file by setting this to
  ;; 0.  If there was an error at any point before, it will not get
  ;; set, which is what we want.
  (setq bookmark-alist-modification-count 0))



(defun bookmark-write-file (file)
  "Write `bookmark-alist' to FILE."
  (bookmark-maybe-message "Saving bookmarks to file %s..." file)
  (with-current-buffer (get-buffer-create " *Bookmarks*")
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (let ((print-length nil)
          (print-level nil))
      (bookmark-insert-file-format-version-stamp)
      (insert "(")
      ;; Rather than a single call to `pp' we make one per bookmark.
      ;; Apparently `pp' has a poor algorithmic complexity, so this
      ;; scales a lot better.  bug#4485.
      (dolist (i bookmark-alist) (pp i (current-buffer)))
      (insert ")")
      (let ((version-control
             (cond
              ((null bookmark-version-control) nil)
              ((eq 'never bookmark-version-control) 'never)
              ((eq 'nospecial bookmark-version-control) version-control)
              (t t))))
        (condition-case nil
            (write-region (point-min) (point-max) file)
          (file-error (message "Can't write %s" file)))
        (kill-buffer (current-buffer))
        (bookmark-maybe-message
         "Saving bookmarks to file %s...done" file)))))


(defun bookmark-import-new-list (new-list)
  "Add NEW-LIST of bookmarks to `bookmark-alist'.
Rename new bookmarks as needed using suffix \"<N>\" (N=1,2,3...), when
they conflict with existing bookmark names."
  (let ((names (bookmark-all-names)))
    (dolist (full-record new-list)
      (bookmark-maybe-rename full-record names)
      (setq bookmark-alist (nconc bookmark-alist (list full-record)))
      (push (bookmark-name-from-full-record full-record) names))))


(defun bookmark-maybe-rename (full-record names)
  "Rename bookmark FULL-RECORD if its current name is already used.
This is a helper for `bookmark-import-new-list'."
  (let ((found-name (bookmark-name-from-full-record full-record)))
    (if (member found-name names)
        ;; We've got a conflict, so generate a new name
        (let ((count 2)
              (new-name found-name))
          (while (member new-name names)
            (setq new-name (concat found-name (format "<%d>" count)))
            (setq count (1+ count)))
          (bookmark-set-name full-record new-name)))))


;;;###autoload
(defun bookmark-load (file &optional overwrite no-msg)
  "Load bookmarks from FILE (which must be in bookmark format).
Appends loaded bookmarks to the front of the list of bookmarks.  If
optional second argument OVERWRITE is non-nil, existing bookmarks are
destroyed.  Optional third arg NO-MSG means don't display any messages
while loading.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs's bookmark list.  Generally, you should only load
in files that were created with the bookmark functions in the first
place.  Your own personal bookmark file, `~/.emacs.bmk', is
maintained automatically by Emacs; you shouldn't need to load it
explicitly.

If you load a file containing bookmarks with the same names as
bookmarks already present in your Emacs, the new bookmarks will get
unique numeric suffixes \"<2>\", \"<3>\", ... following the same
method buffers use to resolve name collisions."
  (interactive
   (list (read-file-name
          (format "Load bookmarks from: (%s) "
                  bookmark-default-file)
          ;;Default might not be used often,
          ;;but there's no better default, and
          ;;I guess it's better than none at all.
          "~/" bookmark-default-file 'confirm)))
  (setq file (abbreviate-file-name (expand-file-name file)))
  (if (not (file-readable-p file))
      (error "Cannot read bookmark file %s" file)
    (if (null no-msg)
        (bookmark-maybe-message "Loading bookmarks from %s..." file))
    (with-current-buffer (let ((enable-local-variables nil))
                           (find-file-noselect file))
      (goto-char (point-min))
      (bookmark-maybe-upgrade-file-format)
      (let ((blist (bookmark-alist-from-buffer)))
        (if (listp blist)
            (progn
              (if overwrite
                  (progn
                    (setq bookmark-alist blist)
                    (setq bookmark-alist-modification-count 0))
                ;; else
                (bookmark-import-new-list blist)
                (setq bookmark-alist-modification-count
                      (1+ bookmark-alist-modification-count)))
              (if (string-equal
                   (abbreviate-file-name
                    (expand-file-name bookmark-default-file))
                   file)
                  (setq bookmarks-already-loaded t))
              (bookmark-bmenu-surreptitiously-rebuild-list))
          (error "Invalid bookmark list in %s" file)))
      (kill-buffer (current-buffer)))
    (if (null no-msg)
        (bookmark-maybe-message "Loading bookmarks from %s...done" file))))



;;; Code supporting the dired-like bookmark menu.
;; Prefix is "bookmark-bmenu" for "buffer-menu":


(defvar bookmark-bmenu-hidden-bookmarks ())


(defvar bookmark-bmenu-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "v" 'bookmark-bmenu-select)
    (define-key map "w" 'bookmark-bmenu-locate)
    (define-key map "2" 'bookmark-bmenu-2-window)
    (define-key map "1" 'bookmark-bmenu-1-window)
    (define-key map "j" 'bookmark-bmenu-this-window)
    (define-key map "\C-c\C-c" 'bookmark-bmenu-this-window)
    (define-key map "f" 'bookmark-bmenu-this-window)
    (define-key map "\C-m" 'bookmark-bmenu-this-window)
    (define-key map "o" 'bookmark-bmenu-other-window)
    (define-key map "\C-o" 'bookmark-bmenu-switch-other-window)
    (define-key map "s" 'bookmark-bmenu-save)
    (define-key map "k" 'bookmark-bmenu-delete)
    (define-key map "\C-d" 'bookmark-bmenu-delete-backwards)
    (define-key map "x" 'bookmark-bmenu-execute-deletions)
    (define-key map "d" 'bookmark-bmenu-delete)
    (define-key map " " 'next-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "\177" 'bookmark-bmenu-backup-unmark)
    (define-key map "u" 'bookmark-bmenu-unmark)
    (define-key map "m" 'bookmark-bmenu-mark)
    (define-key map "l" 'bookmark-bmenu-load)
    (define-key map "r" 'bookmark-bmenu-rename)
    (define-key map "R" 'bookmark-bmenu-relocate)
    (define-key map "t" 'bookmark-bmenu-toggle-filenames)
    (define-key map "a" 'bookmark-bmenu-show-annotation)
    (define-key map "A" 'bookmark-bmenu-show-all-annotations)
    (define-key map "e" 'bookmark-bmenu-edit-annotation)
    (define-key map "/" 'bookmark-bmenu-search)
    (define-key map [mouse-2] 'bookmark-bmenu-other-window-with-mouse)
    map))

;; Bookmark Buffer Menu mode is suitable only for specially formatted
;; data.
(put 'bookmark-bmenu-mode 'mode-class 'special)


;; todo: need to display whether or not bookmark exists as a buffer in
;; flag column.

;; Format:
;; FLAGS  BOOKMARK [ LOCATION ]


(defun bookmark-bmenu-surreptitiously-rebuild-list ()
  "Rebuild the Bookmark List if it exists.
Don't affect the buffer ring order."
  (if (get-buffer "*Bookmark List*")
      (save-excursion
        (save-window-excursion
          (bookmark-bmenu-list)))))


;;;###autoload
(defun bookmark-bmenu-list ()
  "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((buf (get-buffer-create "*Bookmark List*")))
    (if (called-interactively-p 'interactive)
        (switch-to-buffer buf)
      (set-buffer buf)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "% Bookmark\n- --------\n")
    (add-text-properties (point-min) (point)
			 '(font-lock-face bookmark-menu-heading))
    (dolist (full-record (bookmark-maybe-sort-alist))
      (let ((name        (bookmark-name-from-full-record full-record))
            (annotation  (bookmark-get-annotation full-record))
            (start       (point))
            end)
        ;; if a bookmark has an annotation, prepend a "*"
        ;; in the list of bookmarks.
        (insert (if (and annotation (not (string-equal annotation "")))
                    " *" "  ")
                name)
        (setq end (point))
        (put-text-property
         (+ bookmark-bmenu-marks-width start) end 'bookmark-name-prop name)
        (when (display-mouse-p)
          (add-text-properties
           (+ bookmark-bmenu-marks-width start) end
           '(mouse-face highlight
             follow-link t
             help-echo "mouse-2: go to this bookmark in other window")))
        (insert "\n")))
    (set-buffer-modified-p (not (= bookmark-alist-modification-count 0)))
    (goto-char (point-min))
    (forward-line 2)
    (bookmark-bmenu-mode)
    (if bookmark-bmenu-toggle-filenames
        (bookmark-bmenu-toggle-filenames t))))

;;;###autoload
(defalias 'list-bookmarks 'bookmark-bmenu-list)
;;;###autoload
(defalias 'edit-bookmarks 'bookmark-bmenu-list)



(define-derived-mode bookmark-bmenu-mode special-mode "Bookmark Menu"
  "Major mode for editing a list of bookmarks.
Each line describes one of the bookmarks in Emacs.
Letters do not insert themselves; instead, they are commands.
Bookmark names preceded by a \"*\" have annotations.
\\<bookmark-bmenu-mode-map>
\\[bookmark-bmenu-mark] -- mark bookmark to be displayed.
\\[bookmark-bmenu-select] -- select bookmark of line point is on.
  Also show bookmarks marked using m in other windows.
\\[bookmark-bmenu-toggle-filenames] -- toggle displaying of filenames (they may obscure long bookmark names).
\\[bookmark-bmenu-locate] -- display (in minibuffer) location of this bookmark.
\\[bookmark-bmenu-1-window] -- select this bookmark in full-frame window.
\\[bookmark-bmenu-2-window] -- select this bookmark in one window,
  together with bookmark selected before this one in another window.
\\[bookmark-bmenu-this-window] -- select this bookmark in place of the bookmark menu buffer.
\\[bookmark-bmenu-other-window] -- select this bookmark in another window,
  so the bookmark menu bookmark remains visible in its window.
\\[bookmark-bmenu-switch-other-window] -- switch the other window to this bookmark.
\\[bookmark-bmenu-rename] -- rename this bookmark (prompts for new name).
\\[bookmark-bmenu-relocate] -- relocate this bookmark's file (prompts for new file).
\\[bookmark-bmenu-delete] -- mark this bookmark to be deleted, and move down.
\\[bookmark-bmenu-delete-backwards] -- mark this bookmark to be deleted, and move up.
\\[bookmark-bmenu-execute-deletions] -- delete bookmarks marked with `\\[bookmark-bmenu-delete]'.
\\[bookmark-bmenu-save] -- save the current bookmark list in the default file.
  With a prefix arg, prompts for a file to save in.
\\[bookmark-bmenu-load] -- load in a file of bookmarks (prompts for file.)
\\[bookmark-bmenu-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[bookmark-bmenu-backup-unmark] -- back up a line and remove marks.
\\[bookmark-bmenu-show-annotation] -- show the annotation, if it exists, for the current bookmark
  in another buffer.
\\[bookmark-bmenu-show-all-annotations] -- show the annotations of all bookmarks in another buffer.
\\[bookmark-bmenu-edit-annotation] -- edit the annotation for the current bookmark."
  (setq truncate-lines t)
  (setq buffer-read-only t))


(defun bookmark-bmenu-toggle-filenames (&optional show)
  "Toggle whether filenames are shown in the bookmark list.
Optional argument SHOW means show them unconditionally."
  (interactive)
  (cond
   (show
    (setq bookmark-bmenu-toggle-filenames nil)
    (bookmark-bmenu-show-filenames)
    (setq bookmark-bmenu-toggle-filenames t))
   (bookmark-bmenu-toggle-filenames
    (bookmark-bmenu-hide-filenames)
    (setq bookmark-bmenu-toggle-filenames nil))
   (t
    (bookmark-bmenu-show-filenames)
    (setq bookmark-bmenu-toggle-filenames t))))


(defun bookmark-bmenu-show-filenames (&optional force)
  "In an interactive bookmark list, show filenames along with bookmarks.
Non-nil FORCE forces a redisplay showing the filenames.  FORCE is used
mainly for debugging, and should not be necessary in normal use."
  (if (and (not force) bookmark-bmenu-toggle-filenames)
      nil ;already shown, so do nothing
    (with-buffer-modified-unmodified
     (save-excursion
       (save-window-excursion
         (goto-char (point-min))
         (forward-line 2)
         (setq bookmark-bmenu-hidden-bookmarks ())
         (let ((inhibit-read-only t))
           (while (< (point) (point-max))
             (let ((bmrk (bookmark-bmenu-bookmark)))
               (push bmrk bookmark-bmenu-hidden-bookmarks)
               (let ((start (line-end-position)))
                 (move-to-column bookmark-bmenu-file-column t)
                 ;; Strip off `mouse-face' from the white spaces region.
                 (if (display-mouse-p)
                     (remove-text-properties start (point)
                                             '(mouse-face nil help-echo nil))))
               (delete-region (point) (progn (end-of-line) (point)))
               (insert "  ")
               ;; Pass the NO-HISTORY arg:
               (bookmark-insert-location bmrk t)
               (forward-line 1)))))))))


(defun bookmark-bmenu-hide-filenames (&optional force)
  "In an interactive bookmark list, hide the filenames of the bookmarks.
Non-nil FORCE forces a redisplay showing the filenames.  FORCE is used
mainly for debugging, and should not be necessary in normal use."
  (when (and (not force) bookmark-bmenu-toggle-filenames)
    ;; nothing to hide if above is nil
    (with-buffer-modified-unmodified
     (save-excursion
       (goto-char (point-min))
       (forward-line 2)
       (setq bookmark-bmenu-hidden-bookmarks
             (nreverse bookmark-bmenu-hidden-bookmarks))
       (let ((inhibit-read-only t))
         (while bookmark-bmenu-hidden-bookmarks
           (move-to-column bookmark-bmenu-marks-width t)
           (bookmark-kill-line)
           (let ((name  (pop bookmark-bmenu-hidden-bookmarks))
                 (start (point)))
             (insert name)
             (put-text-property start (point) 'bookmark-name-prop name)
             (if (display-mouse-p)
                 (add-text-properties
                  start (point)
                  '(mouse-face
                    highlight follow-link t help-echo
                    "mouse-2: go to this bookmark in other window"))))
           (forward-line 1)))))))


(defun bookmark-bmenu-ensure-position ()
  "If point is not on a bookmark line, move it to one.
If before the first bookmark line, move to the first; if after the
last full line, move to the last full line.  The return value is undefined."
  (cond ((< (count-lines (point-min) (point)) bookmark-bmenu-header-height)
         (goto-char (point-min))
         (forward-line bookmark-bmenu-header-height))
        ((and (bolp) (eobp))
         (beginning-of-line 0))))


(defun bookmark-bmenu-bookmark ()
  "Return the bookmark for this line in an interactive bookmark list buffer."
  (bookmark-bmenu-ensure-position)
  (save-excursion
    (beginning-of-line)
    (forward-char bookmark-bmenu-marks-width)
    (get-text-property (point) 'bookmark-name-prop)))


(defun bookmark-show-annotation (bookmark-name-or-record)
  "Display the annotation for BOOKMARK-NAME-OR-RECORD in a buffer,
if an annotation exists."
  (let ((annotation (bookmark-get-annotation bookmark-name-or-record)))
    (when (and annotation (not (string-equal annotation "")))
      (save-excursion
        (let ((old-buf (current-buffer)))
          (pop-to-buffer (get-buffer-create "*Bookmark Annotation*") t)
          (delete-region (point-min) (point-max))
          (insert annotation)
          (goto-char (point-min))
          (switch-to-buffer-other-window old-buf))))))


(defun bookmark-show-all-annotations ()
  "Display the annotations for all bookmarks in a buffer."
  (save-selected-window
    (pop-to-buffer (get-buffer-create "*Bookmark Annotation*") t)
    (delete-region (point-min) (point-max))
    (dolist (full-record bookmark-alist)
      (let* ((name (bookmark-name-from-full-record full-record))
             (ann  (bookmark-get-annotation full-record)))
        (insert (concat name ":\n"))
        (if (and ann (not (string-equal ann "")))
            ;; insert the annotation, indented by 4 spaces.
            (progn
              (save-excursion (insert ann) (unless (bolp)
                                             (insert "\n")))
              (while (< (point) (point-max))
                (beginning-of-line)     ; paranoia
                (insert "    ")
                (forward-line)
                (end-of-line))))))
    (goto-char (point-min))))


(defun bookmark-bmenu-mark ()
  "Mark bookmark on this line to be displayed by \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-select]."
  (interactive)
  (beginning-of-line)
  (bookmark-bmenu-ensure-position)
  (with-buffer-modified-unmodified
   (let ((inhibit-read-only t))
     (delete-char 1)
     (insert ?>)
     (forward-line 1)
     (bookmark-bmenu-ensure-position))))


(defun bookmark-bmenu-select ()
  "Select this line's bookmark; also display bookmarks marked with `>'.
You can mark bookmarks with the \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-mark] command."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (menu (current-buffer))
        (others ())
        tem)
    (goto-char (point-min))
    (while (re-search-forward "^>" nil t)
      (setq tem (bookmark-bmenu-bookmark))
      (let ((inhibit-read-only t))
        (delete-char -1)
        (insert ?\s))
      (or (string-equal tem bmrk)
          (member tem others)
          (setq others (cons tem others))))
    (setq others (nreverse others)
          tem (/ (1- (frame-height)) (1+ (length others))))
    (delete-other-windows)
    (bookmark-jump bmrk)
    (bury-buffer menu)
    (if others
        (while others
          (split-window nil tem)
          (other-window 1)
          (bookmark-jump (car others))
          (setq others (cdr others)))
      (other-window 1))))


(defun bookmark-bmenu-any-marks ()
  "Return non-nil if any bookmarks are marked in the marks column."
  (save-excursion
    (goto-char (point-min))
    (bookmark-bmenu-ensure-position)
    (catch 'found-mark
      (while (not (eobp))
        (beginning-of-line)
        (if (looking-at "^\\S-")
            (throw 'found-mark t)
          (forward-line 1)))
      nil)))


(defun bookmark-bmenu-save (parg)
  "Save the current list into a bookmark file.
With a prefix arg, prompts for a file to save them in."
  (interactive "P")
  (save-excursion
    (save-window-excursion
      (bookmark-save parg)
      (set-buffer-modified-p nil))))


(defun bookmark-bmenu-load ()
  "Load the bookmark file and rebuild the bookmark menu-buffer."
  (interactive)
  (bookmark-bmenu-ensure-position)
  (save-excursion
    (save-window-excursion
      ;; This will call `bookmark-bmenu-list'
      (call-interactively 'bookmark-load))))


(defun bookmark-bmenu-1-window ()
  "Select this line's bookmark, alone, in full frame."
  (interactive)
  (bookmark-jump (bookmark-bmenu-bookmark))
  (bury-buffer (other-buffer))
  (delete-other-windows))


(defun bookmark-bmenu-2-window ()
  "Select this line's bookmark, with previous buffer in second window."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (menu (current-buffer))
        (pop-up-windows t))
    (delete-other-windows)
    (switch-to-buffer (other-buffer) nil t)
    (bookmark--jump-via bmrk 'pop-to-buffer)
    (bury-buffer menu)))


(defun bookmark-bmenu-this-window ()
  "Select this line's bookmark in this window."
  (interactive)
  (bookmark-jump (bookmark-bmenu-bookmark)))


(defun bookmark-bmenu-other-window ()
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (bookmark--jump-via bookmark 'switch-to-buffer-other-window)))


(defun bookmark-bmenu-switch-other-window ()
  "Make the other window select this line's bookmark.
The current window remains selected."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark))
        (pop-up-windows t)
        same-window-buffer-names
        same-window-regexps)
    (bookmark--jump-via bookmark 'display-buffer)))

(defun bookmark-bmenu-other-window-with-mouse (event)
  "Select bookmark at the mouse pointer in other window, leaving bookmark menu visible."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (bookmark-bmenu-other-window))))


(defun bookmark-bmenu-show-annotation ()
  "Show the annotation for the current bookmark in another window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (bookmark-show-annotation bookmark)))


(defun bookmark-bmenu-show-all-annotations ()
  "Show the annotation for all bookmarks in another window."
  (interactive)
  (bookmark-show-all-annotations))


(defun bookmark-bmenu-edit-annotation ()
  "Edit the annotation for the current bookmark in another window."
  (interactive)
  (let ((bookmark (bookmark-bmenu-bookmark)))
    (bookmark-edit-annotation bookmark)))


(defun bookmark-bmenu-unmark (&optional backup)
  "Cancel all requested operations on bookmark on this line and move down.
Optional BACKUP means move up."
  (interactive "P")
  (beginning-of-line)
  (bookmark-bmenu-ensure-position)
  (with-buffer-modified-unmodified
   (let ((inhibit-read-only t))
     (delete-char 1)
     ;; any flags to reset according to circumstances?  How about a
     ;; flag indicating whether this bookmark is being visited?
     ;; well, we don't have this now, so maybe later.
     (insert " "))
   (forward-line (if backup -1 1))
   (bookmark-bmenu-ensure-position)))


(defun bookmark-bmenu-backup-unmark ()
  "Move up and cancel all requested operations on bookmark on line above."
  (interactive)
  (forward-line -1)
  (bookmark-bmenu-ensure-position)
  (bookmark-bmenu-unmark)
  (forward-line -1)
  (bookmark-bmenu-ensure-position))


(defun bookmark-bmenu-delete ()
  "Mark bookmark on this line to be deleted.
To carry out the deletions that you've marked, use \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]."
  (interactive)
  (beginning-of-line)
  (bookmark-bmenu-ensure-position)
  (with-buffer-modified-unmodified
   (let ((inhibit-read-only t))
     (delete-char 1)
     (insert ?D)
     (forward-line 1)
     (bookmark-bmenu-ensure-position))))


(defun bookmark-bmenu-delete-backwards ()
  "Mark bookmark on this line to be deleted, then move up one line.
To carry out the deletions that you've marked, use \\<bookmark-bmenu-mode-map>\\[bookmark-bmenu-execute-deletions]."
  (interactive)
  (bookmark-bmenu-delete)
  (forward-line -2)
  (bookmark-bmenu-ensure-position)
  (forward-line 1)
  (bookmark-bmenu-ensure-position))


(defun bookmark-bmenu-execute-deletions ()
  "Delete bookmarks flagged `D'."
  (interactive)
  (message "Deleting bookmarks...")
  (let ((o-point  (point))
        (o-str    (save-excursion
                    (beginning-of-line)
                    (unless (looking-at "^D")
                      (buffer-substring
                       (point)
                       (progn (end-of-line) (point))))))
        (o-col     (current-column)))
    (goto-char (point-min))
    (forward-line 1)
    (while (re-search-forward "^D" (point-max) t)
      (bookmark-delete (bookmark-bmenu-bookmark) t)) ; pass BATCH arg
    (bookmark-bmenu-list)
    (if o-str
        (progn
          (goto-char (point-min))
          (search-forward o-str)
          (beginning-of-line)
          (forward-char o-col))
      (goto-char o-point))
    (beginning-of-line)
    (message "Deleting bookmarks...done")
    ))


(defun bookmark-bmenu-rename ()
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (thispoint (point)))
    (bookmark-rename bmrk)
    (goto-char thispoint)))


(defun bookmark-bmenu-locate ()
  "Display location of this bookmark.  Displays in the minibuffer."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark)))
    (message "%s" (bookmark-location bmrk))))

(defun bookmark-bmenu-relocate ()
  "Change the file path of the bookmark on the current line,
  prompting with completion for the new path."
  (interactive)
  (let ((bmrk (bookmark-bmenu-bookmark))
        (thispoint (point)))
    (bookmark-relocate bmrk)
    (goto-char thispoint)))

;;; Bookmark-bmenu search

;; Store keyboard input for incremental search.
(defvar bookmark-search-pattern)

(defun bookmark-read-search-input ()
  "Read each keyboard input and add it to `bookmark-search-pattern'."
  (let ((prompt       (propertize "Pattern: " 'face 'minibuffer-prompt))
        ;; (inhibit-quit t) ; inhibit-quit is evil.  Use it with extreme care!
        (tmp-list     ()))
    (while
        (let ((char (read-key (concat prompt bookmark-search-pattern))))
          (case char
            ((?\e ?\r) nil) ; RET or ESC break the search loop.
            (?\C-g (setq bookmark-quit-flag t) nil)
            (?\d (pop tmp-list) t) ; Delete last char of pattern with DEL
            (t
             (if (characterp char)
                 (push char tmp-list)
               (setq unread-command-events
                     (nconc (mapcar 'identity
                                    (this-single-command-raw-keys))
                            unread-command-events))
               nil))))
      (setq bookmark-search-pattern
            (apply 'string (reverse tmp-list))))))


(defun bookmark-bmenu-filter-alist-by-regexp (regexp)
  "Filter `bookmark-alist' with bookmarks matching REGEXP and rebuild list."
  (let ((bookmark-alist
         (loop for i in bookmark-alist
               when (string-match regexp (car i)) collect i into new
               finally return new)))
    (bookmark-bmenu-list)))


;;;###autoload
(defun bookmark-bmenu-search ()
  "Incremental search of bookmarks, hiding the non-matches as we go."
  (interactive)
  (let ((bmk (bookmark-bmenu-bookmark))
        (bookmark-search-pattern "")
        (timer (run-with-idle-timer
                bookmark-search-delay 'repeat
                #'(lambda ()
                    (bookmark-bmenu-filter-alist-by-regexp
                     bookmark-search-pattern)))))
    (unwind-protect
        (bookmark-read-search-input)
      (cancel-timer timer)
      (message nil)
      (when bookmark-quit-flag        ; C-g hit restore menu list.
        (bookmark-bmenu-list) (bookmark-bmenu-goto-bookmark bmk))
      (setq bookmark-quit-flag nil))))

(defun bookmark-bmenu-goto-bookmark (name)
  "Move point to bookmark with name NAME."
  (goto-char (point-min))
  (while (not (equal name (bookmark-bmenu-bookmark)))
    (forward-line 1))
  (forward-line 0))



;;; Menu bar stuff.  Prefix is "bookmark-menu".

(defun bookmark-menu-popup-paned-menu (event name entries)
  "Pop up multi-paned menu at EVENT, return string chosen from ENTRIES.
That is, ENTRIES is a list of strings which appear as the choices
in the menu.
The number of panes depends on the number of entries.
The visible entries are truncated to `bookmark-menu-length', but the
strings returned are not."
  (let ((f-height (/ (frame-height) 2))
	(pane-list nil)
	(iter 0))
    (while entries
      (let (lst
	    (count 0))
	(while (and (< count f-height) entries)
	  (let ((str (car entries)))
	    (push (cons
		   (if (> (length str) bookmark-menu-length)
		       (substring str 0 bookmark-menu-length)
		     str)
		   str)
		  lst)
	    (setq entries (cdr entries))
	    (setq count (1+ count))))
	(setq iter (1+ iter))
	(push (cons
	       (format "-*- %s (%d) -*-" name iter)
	       (nreverse lst))
	      pane-list)))

    ;; Popup the menu and return the string.
    (x-popup-menu event (cons (concat "-*- " name " -*-")
			      (nreverse pane-list)))))


;; Thanks to Roland McGrath for fixing menubar.el so that the
;; following works, and for explaining what to do to make it work.

;; We MUST autoload EACH form used to set up this variable's value, so
;; that the whole job is done in loaddefs.el.

;; Emacs menubar stuff.

;;;###autoload
(defvar menu-bar-bookmark-map
  (let ((map (make-sparse-keymap "Bookmark functions")))
    (define-key map [load]
      `(menu-item ,(purecopy "Load a Bookmark File...") bookmark-load
		  :help ,(purecopy "Load bookmarks from a bookmark file)")))
    (define-key map [write]
      `(menu-item ,(purecopy "Save Bookmarks As...") bookmark-write
		  :help ,(purecopy "Write bookmarks to a file (reading the file name with the minibuffer)")))
    (define-key map [save]
      `(menu-item ,(purecopy "Save Bookmarks") bookmark-save
		  :help ,(purecopy "Save currently defined bookmarks")))
    (define-key map [edit]
      `(menu-item ,(purecopy "Edit Bookmark List") bookmark-bmenu-list
		  :help ,(purecopy "Display a list of existing bookmarks")))
    (define-key map [delete]
      `(menu-item ,(purecopy "Delete Bookmark...") bookmark-delete
		  :help ,(purecopy "Delete a bookmark from the bookmark list")))
    (define-key map [rename]
      `(menu-item ,(purecopy "Rename Bookmark...") bookmark-rename
		  :help ,(purecopy "Change the name of a bookmark")))
    (define-key map [locate]
      `(menu-item ,(purecopy "Insert Location...") bookmark-locate
		  :help ,(purecopy "Insert the name of the file associated with a bookmark")))
    (define-key map [insert]
      `(menu-item ,(purecopy "Insert Contents...") bookmark-insert
		  :help ,(purecopy "Insert the text of the file pointed to by a bookmark")))
    (define-key map [set]
      `(menu-item ,(purecopy "Set Bookmark...") bookmark-set
		  :help ,(purecopy "Set a bookmark named inside a file.")))
    (define-key map [jump]
      `(menu-item ,(purecopy "Jump to Bookmark...") bookmark-jump
		  :help ,(purecopy "Jump to a bookmark (a point in some file)")))
    map))

;;;###autoload
(defalias 'menu-bar-bookmark-map menu-bar-bookmark-map)

;; make bookmarks appear toward the right side of the menu.
(if (boundp 'menu-bar-final-items)
    (if menu-bar-final-items
        (push 'bookmark menu-bar-final-items))
  (setq menu-bar-final-items '(bookmark)))

;;;; end bookmark menu stuff ;;;;


;; Load Hook
(defvar bookmark-load-hook nil
  "Hook run at the end of loading library `bookmark.el'.")

;; Exit Hook, called from kill-emacs-hook
(defvar bookmark-exit-hook nil
  "Hook run when Emacs exits.")

(define-obsolete-variable-alias 'bookmark-exit-hooks 'bookmark-exit-hook "22.1")

(defun bookmark-exit-hook-internal ()
  "Save bookmark state, if necessary, at Emacs exit time.
This also runs `bookmark-exit-hook'."
  (run-hooks 'bookmark-exit-hook)
  (and bookmark-alist
       (bookmark-time-to-save-p t)
       (bookmark-save)))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'bookmark-exit-hook-internal))

(defun bookmark-unload-function ()
  "Unload the Bookmark library."
  (when bookmark-save-flag (bookmark-save))
  ;; continue standard unloading
  nil)


(run-hooks 'bookmark-load-hook)

(provide 'bookmark)

;;; bookmark.el ends here
