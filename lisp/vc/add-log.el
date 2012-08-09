;;; add-log.el --- change log maintenance commands for Emacs

;; Copyright (C) 1985-1986, 1988, 1993-1994, 1997-1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: vc tools

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

;; This facility is documented in the Emacs Manual.

;; Todo:

;; - Find/use/create _MTN/log if there's a _MTN directory.
;; - Find/use/create ++log.* if there's an {arch} directory.
;; - Use an open *VC-Log* or *cvs-commit* buffer if it's related to the
;;   source file.
;; - Don't add TAB indents (and username?) if inserting entries in those
;;   special places.

;;; Code:

(defgroup change-log nil
  "Change log maintenance."
  :group 'tools
  :link '(custom-manual "(emacs)Change Log")
  :prefix "change-log-"
  :prefix "add-log-")


(defcustom change-log-default-name nil
  "Name of a change log file for \\[add-change-log-entry]."
  :type '(choice (const :tag "default" nil)
		 string)
  :group 'change-log)
;;;###autoload
(put 'change-log-default-name 'safe-local-variable 'string-or-null-p)

(defcustom change-log-mode-hook nil
  "Normal hook run by `change-log-mode'."
  :type 'hook
  :group 'change-log)

;; Many modes set this variable, so avoid warnings.
;;;###autoload
(defcustom add-log-current-defun-function nil
  "If non-nil, function to guess name of surrounding function.
It is used by `add-log-current-defun' in preference to built-in rules.
Returns function's name as a string, or nil if outside a function."
  :type '(choice (const nil) function)
  :group 'change-log)

;;;###autoload
(defcustom add-log-full-name nil
  "Full name of user, for inclusion in ChangeLog daily headers.
This defaults to the value returned by the function `user-full-name'."
  :type '(choice (const :tag "Default" nil)
		 string)
  :group 'change-log)

;;;###autoload
(defcustom add-log-mailing-address nil
  "Email addresses of user, for inclusion in ChangeLog headers.
This defaults to the value of `user-mail-address'.  In addition to
being a simple string, this value can also be a list.  All elements
will be recognized as referring to the same user; when creating a new
ChangeLog entry, one element will be chosen at random."
  :type '(choice (const :tag "Default" nil)
		 (string :tag "String")
		 (repeat :tag "List of Strings" string))
  :group 'change-log)

(defcustom add-log-time-format 'add-log-iso8601-time-string
  "Function that defines the time format.
For example, `add-log-iso8601-time-string', which gives the
date in international ISO 8601 format,
and `current-time-string' are two valid values."
  :type '(radio (const :tag "International ISO 8601 format"
		       add-log-iso8601-time-string)
		(const :tag "Old format, as returned by `current-time-string'"
		       current-time-string)
		(function :tag "Other"))
  :group 'change-log)

(defcustom add-log-keep-changes-together nil
  "If non-nil, normally keep day's log entries for one file together.

Log entries for a given file made with \\[add-change-log-entry] or
\\[add-change-log-entry-other-window] will only be added to others \
for that file made
today if this variable is non-nil or that file comes first in today's
entries.  Otherwise another entry for that file will be started.  An
original log:

	* foo (...): ...
	* bar (...): change 1

in the latter case, \\[add-change-log-entry-other-window] in a \
buffer visiting `bar', yields:

	* bar (...): -!-
	* foo (...): ...
	* bar (...): change 1

and in the former:

	* foo (...): ...
	* bar (...): change 1
	(...): -!-

The NEW-ENTRY arg to `add-change-log-entry' can override the effect of
this variable."
  :version "20.3"
  :type 'boolean
  :group 'change-log)

(defcustom add-log-always-start-new-record nil
  "If non-nil, `add-change-log-entry' will always start a new record."
  :version "22.1"
  :type 'boolean
  :group 'change-log)

(defcustom add-log-buffer-file-name-function nil
  "If non-nil, function to call to identify the full filename of a buffer.
This function is called with no argument.  If this is nil, the default is to
use `buffer-file-name'."
  :type '(choice (const nil) function)
  :group 'change-log)

(defcustom add-log-file-name-function nil
  "If non-nil, function to call to identify the filename for a ChangeLog entry.
This function is called with one argument, the value of variable
`buffer-file-name' in that buffer.  If this is nil, the default is to
use the file's name relative to the directory of the change log file."
  :type '(choice (const nil) function)
  :group 'change-log)


(defcustom change-log-version-info-enabled nil
  "If non-nil, enable recording version numbers with the changes."
  :version "21.1"
  :type 'boolean
  :group 'change-log)

(defcustom change-log-version-number-regexp-list
  (let ((re "\\([0-9]+\.[0-9.]+\\)"))
    (list
     ;;  (defconst ad-version "2.15"
     (concat "^(def[^ \t\n]+[ \t]+[^ \t\n][ \t]\"" re)
     ;; Revision: pcl-cvs.el,v 1.72 1999/09/05 20:21:54 monnier Exp
     (concat "^;+ *Revision: +[^ \t\n]+[ \t]+" re)))
  "List of regexps to search for version number.
The version number must be in group 1.
Note: The search is conducted only within 10%, at the beginning of the file."
  :version "21.1"
  :type '(repeat regexp)
  :group 'change-log)

(defface change-log-date
  '((t (:inherit font-lock-string-face)))
  "Face used to highlight dates in date lines."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-date-face 'change-log-date "22.1")

(defface change-log-name
  '((t (:inherit font-lock-constant-face)))
  "Face for highlighting author names."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-name-face 'change-log-name "22.1")

(defface change-log-email
  '((t (:inherit font-lock-variable-name-face)))
  "Face for highlighting author email addresses."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-email-face 'change-log-email "22.1")

(defface change-log-file
  '((t (:inherit font-lock-function-name-face)))
  "Face for highlighting file names."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-file-face 'change-log-file "22.1")

(defface change-log-list
  '((t (:inherit font-lock-keyword-face)))
  "Face for highlighting parenthesized lists of functions or variables."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-list-face 'change-log-list "22.1")

(defface change-log-conditionals
  '((t (:inherit font-lock-variable-name-face)))
  "Face for highlighting conditionals of the form `[...]'."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-conditionals-face
  'change-log-conditionals "22.1")

(defface change-log-function
  '((t (:inherit font-lock-variable-name-face)))
  "Face for highlighting items of the form `<....>'."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-function-face
  'change-log-function "22.1")

(defface change-log-acknowledgement
  '((t (:inherit font-lock-comment-face)))
  "Face for highlighting acknowledgments."
  :version "21.1"
  :group 'change-log)
(define-obsolete-face-alias 'change-log-acknowledgement-face
  'change-log-acknowledgement "22.1")

(defconst change-log-file-names-re "^\\( +\\|\t\\)\\* \\([^ ,:([\n]+\\)")
(defconst change-log-start-entry-re "^\\sw.........[0-9:+ ]*")

(defvar change-log-font-lock-keywords
  `(;;
    ;; Date lines, new (2000-01-01) and old (Sat Jan  1 00:00:00 2000) styles.
    ;; Fixme: this regexp is just an approximate one and may match
    ;; wrongly with a non-date line existing as a random note.  In
    ;; addition, using any kind of fixed setting like this doesn't
    ;; work if a user customizes add-log-time-format.
    ("^[0-9-]+ +\\|^ \\{11,\\}\\|^\t \\{3,\\}\\|^\\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\) [A-z][a-z][a-z] [0-9:+ ]+"
     (0 'change-log-date-face)
     ;; Name and e-mail; some people put e-mail in parens, not angles.
     ("\\([^<(]+?\\)[ \t]*[(<]\\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\\)[>)]" nil nil
      (1 'change-log-name)
      (2 'change-log-email)))
    ;;
    ;; File names.
    (,change-log-file-names-re
     (2 'change-log-file)
     ;; Possibly further names in a list:
     ("\\=, \\([^ ,:([\n]+\\)" nil nil (1 'change-log-file))
     ;; Possibly a parenthesized list of names:
     ("\\= (\\([^(),\n]+\\|(\\(setf\\|SETF\\) [^() ,\n]+)\\)"
      nil nil (1 'change-log-list))
     ("\\=, *\\([^(),\n]+\\|(\\(setf\\|SETF\\) [^() ,\n]+)\\)"
      nil nil (1 'change-log-list)))
    ;;
    ;; Function or variable names.
    ("^\\( +\\|\t\\)(\\([^(),\n]+\\|(\\(setf\\|SETF\\) [^() ,\n]+)\\)"
     (2 'change-log-list)
     ("\\=, *\\([^(),\n]+\\|(\\(setf\\|SETF\\) [^() ,\n]+)\\)" nil nil
      (1 'change-log-list)))
    ;;
    ;; Conditionals.
    ("\\[!?\\([^]\n]+\\)\\]\\(:\\| (\\)" (1 'change-log-conditionals))
    ;;
    ;; Function of change.
    ("<\\([^>\n]+\\)>\\(:\\| (\\)" (1 'change-log-function))
    ;;
    ;; Acknowledgements.
    ;; Don't include plain "From" because that is vague;
    ;; we want to encourage people to say something more specific.
    ;; Note that the FSF does not use "Patches by"; our convention
    ;; is to put the name of the author of the changes at the top
    ;; of the change log entry.
    ("\\(^\\( +\\|\t\\)\\|  \\)\\(Thanks to\\|Patch\\(es\\)? by\\|Report\\(ed by\\| from\\)\\|Suggest\\(ed by\\|ion from\\)\\)"
     3 'change-log-acknowledgement))
  "Additional expressions to highlight in Change Log mode.")

(defun change-log-search-file-name (where)
  "Return the file-name for the change under point."
  (save-excursion
    (goto-char where)
    (beginning-of-line 1)
    (if (looking-at change-log-start-entry-re)
	;; We are at the start of an entry, search forward for a file
	;; name.
	(progn
	  (re-search-forward change-log-file-names-re nil t)
	  (match-string-no-properties 2))
      (if (looking-at change-log-file-names-re)
	  ;; We found a file name.
	  (match-string-no-properties 2)
	;; Look backwards for either a file name or the log entry start.
	(if (re-search-backward
	     (concat "\\(" change-log-start-entry-re
		     "\\)\\|\\("
		     change-log-file-names-re "\\)") nil t)
	    (if (match-beginning 1)
		;; We got the start of the entry, look forward for a
		;; file name.
		(progn
		  (re-search-forward change-log-file-names-re nil t)
		  (match-string-no-properties 2))
	      (match-string-no-properties 4))
	  ;; We must be before any file name, look forward.
	  (re-search-forward change-log-file-names-re nil t)
	  (match-string-no-properties 2))))))

(defun change-log-find-file ()
  "Visit the file for the change under point."
  (interactive)
  (let ((file (change-log-search-file-name (point))))
    (if (and file (file-exists-p file))
	(find-file file)
      (message "No such file or directory: %s" file))))

(defun change-log-search-tag-name-1 (&optional from)
  "Search for a tag name within subexpression 1 of last match.
Optional argument FROM specifies a buffer position where the tag
name should be located.  Return value is a cons whose car is the
string representing the tag and whose cdr is the position where
the tag was found."
  (save-restriction
    (narrow-to-region (match-beginning 1) (match-end 1))
    (when from (goto-char from))
    ;; The regexp below skips any symbol near `point' (FROM) followed by
    ;; whitespace and another symbol.  This should skip, for example,
    ;; "struct" in a specification like "(struct buffer)" and move to
    ;; "buffer".  A leading paren is ignored.
    (when (looking-at
	   "[(]?\\(?:\\(?:\\sw\\|\\s_\\)+\\(?:[ \t]+\\(\\sw\\|\\s_\\)+\\)\\)")
      (goto-char (match-beginning 1)))
    (cons (find-tag-default) (point))))

(defconst change-log-tag-re
  "(\\(\\(?:\\sw\\|\\s_\\)+\\(?:[, \t]+\\(?:\\sw\\|\\s_\\)+\\)*\\))"
  "Regexp matching a tag name in change log entries.")

(defun change-log-search-tag-name (&optional at)
  "Search for a tag name near `point'.
Optional argument AT non-nil means search near buffer position AT.
Return value is a cons whose car is the string representing
the tag and whose cdr is the position where the tag was found."
  (save-excursion
    (goto-char (setq at (or at (point))))
    (save-restriction
      (widen)
      (or (condition-case nil
	      ;; Within parenthesized list?
	      (save-excursion
		(backward-up-list)
		(when (looking-at change-log-tag-re)
		  (change-log-search-tag-name-1 at)))
	    (error nil))
	  (condition-case nil
	      ;; Before parenthesized list on same line?
	      (save-excursion
		(when (and (skip-chars-forward " \t")
			   (looking-at change-log-tag-re))
		  (change-log-search-tag-name-1)))
	    (error nil))
	  (condition-case nil
	      ;; Near file name?
	      (save-excursion
		(when (and (progn
			     (beginning-of-line)
			     (looking-at change-log-file-names-re))
			   (goto-char (match-end 0))
			   (skip-syntax-forward " ")
			   (looking-at change-log-tag-re))
		  (change-log-search-tag-name-1)))
	    (error nil))
	  (condition-case nil
	      ;; Anywhere else within current entry?
	      (let ((from
		     (save-excursion
		       (end-of-line)
		       (if (re-search-backward change-log-start-entry-re nil t)
			   (match-beginning 0)
			 (point-min))))
		    (to
		     (save-excursion
		       (end-of-line)
		       (if (re-search-forward change-log-start-entry-re nil t)
			   (match-beginning 0)
			 (point-max)))))
		(when (and (< from to) (<= from at) (<= at to))
		  (save-restriction
		    ;; Narrow to current change log entry.
		    (narrow-to-region from to)
		    (cond
		     ((re-search-backward change-log-tag-re nil t)
		      (narrow-to-region (match-beginning 1) (match-end 1))
		      (goto-char (point-max))
		      (cons (find-tag-default) (point-max)))
		     ((re-search-forward change-log-tag-re nil t)
		      (narrow-to-region (match-beginning 1) (match-end 1))
		      (goto-char (point-min))
		      (cons (find-tag-default) (point-min)))))))
	    (error nil))))))

(defvar change-log-find-head nil)
(defvar change-log-find-tail nil)
(defvar change-log-find-window nil)

(defun change-log-goto-source-1 (tag regexp file buffer
				     &optional window first last)
  "Search for tag TAG in buffer BUFFER visiting file FILE.
REGEXP is a regular expression for TAG.  The remaining arguments
are optional: WINDOW denotes the window to display the results of
the search.  FIRST is a position in BUFFER denoting the first
match from previous searches for TAG.  LAST is the position in
BUFFER denoting the last match for TAG in the last search."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
	(widen)
	(if last
	    (progn
	      ;; When LAST is set make sure we continue from the next
	      ;; line end to not find the same tag again.
	      (goto-char last)
	      (end-of-line)
	      (condition-case nil
		  ;; Try to go to the end of the current defun to avoid
		  ;; false positives within the current defun's body
		  ;; since these would match `add-log-current-defun'.
		  (end-of-defun)
		;; Don't fall behind when `end-of-defun' fails.
		(error (progn (goto-char last) (end-of-line))))
	      (setq last nil))
	  ;; When LAST was not set start at beginning of BUFFER.
	  (goto-char (point-min)))
	(let (current-defun)
	  (while (and (not last) (re-search-forward regexp nil t))
	      ;; Verify that `add-log-current-defun' invoked at the end
	      ;; of the match returns TAG.  This heuristic works well
	      ;; whenever the name of the defun occurs within the first
	      ;; line of the defun.
	      (setq current-defun (add-log-current-defun))
	      (when (and current-defun (string-equal current-defun tag))
		;; Record this as last match.
		(setq last (line-beginning-position))
		;; Record this as first match when there's none.
		(unless first (setq first last)))))))
    (if (or last first)
	(with-selected-window
	    (setq change-log-find-window (or window (display-buffer buffer)))
	  (if last
	      (progn
		(when (or (< last (point-min)) (> last (point-max)))
		  ;; Widen to show TAG.
		  (widen))
		(push-mark)
		(goto-char last))
	    ;; When there are no more matches go (back) to FIRST.
	    (message "No more matches for tag `%s' in file `%s'" tag file)
	    (setq last first)
	    (goto-char first))
	  ;; Return new "tail".
	  (list (selected-window) first last))
      (message "Source location of tag `%s' not found in file `%s'" tag file)
      nil)))

(defun change-log-goto-source ()
  "Go to source location of \"change log tag\" near `point'.
A change log tag is a symbol within a parenthesized,
comma-separated list.  If no suitable tag can be found nearby,
try to visit the file for the change under `point' instead."
  (interactive)
  (if (and (eq last-command 'change-log-goto-source)
	   change-log-find-tail)
      (setq change-log-find-tail
	    (condition-case nil
		(apply 'change-log-goto-source-1
		       (append change-log-find-head change-log-find-tail))
	      (error
	       (format "Cannot find more matches for tag `%s' in file `%s'"
		       (car change-log-find-head)
		       (nth 2 change-log-find-head)))))
    (save-excursion
      (let* ((at (point))
	     (tag-at (change-log-search-tag-name))
	     (tag (car tag-at))
	     (file (when tag-at (change-log-search-file-name (cdr tag-at))))
	     (file-at (when file (match-beginning 2)))
	     ;; `file-2' is the file `change-log-search-file-name' finds
	     ;; at `point'.  We use `file-2' as a fallback when `tag' or
	     ;; `file' are not suitable for some reason.
	     (file-2 (change-log-search-file-name at))
	     (file-2-at (when file-2 (match-beginning 2))))
	(cond
	 ((and (or (not tag) (not file) (not (file-exists-p file)))
	       (or (not file-2) (not (file-exists-p file-2))))
	  (error "Cannot find tag or file near `point'"))
	 ((and file-2 (file-exists-p file-2)
	       (or (not tag) (not file) (not (file-exists-p file))
		   (and (or (and (< file-at file-2-at) (<= file-2-at at))
			    (and (<= at file-2-at) (< file-2-at file-at))))))
	  ;; We either have not found a suitable file name or `file-2'
	  ;; provides a "better" file name wrt `point'.  Go to the
	  ;; buffer of `file-2' instead.
	  (setq change-log-find-window
		(display-buffer (find-file-noselect file-2))))
	 (t
	  (setq change-log-find-head
		(list tag (concat "\\_<" (regexp-quote tag) "\\_>")
		      file (find-file-noselect file)))
	  (condition-case nil
	      (setq change-log-find-tail
		    (apply 'change-log-goto-source-1 change-log-find-head))
	    (error
	     (format "Cannot find matches for tag `%s' in file `%s'"
		     tag file)))))))))

(defun change-log-next-error (&optional argp reset)
  "Move to the Nth (default 1) next match in a ChangeLog buffer.
Compatibility function for \\[next-error] invocations."
  (interactive "p")
  (let* ((argp (or argp 0))
	 (count (abs argp))		; how many cycles
	 (down (< argp 0))		; are we going down? (is argp negative?)
	 (up (not down))
	 (search-function (if up 're-search-forward 're-search-backward)))

    ;; set the starting position
    (goto-char (cond (reset (point-min))
		     (down (line-beginning-position))
		     (up (line-end-position))
		     ((point))))

    (funcall search-function change-log-file-names-re nil t count))

  (beginning-of-line)
  ;; if we found a place to visit...
  (when (looking-at change-log-file-names-re)
    (let (change-log-find-window)
      (change-log-goto-source)
      (when change-log-find-window
	;; Select window displaying source file.
	(select-window change-log-find-window)))))

(defvar change-log-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-p] 'add-log-edit-prev-comment)
    (define-key map [?\C-c ?\C-n] 'add-log-edit-next-comment)
    (define-key map [?\C-c ?\C-f] 'change-log-find-file)
    (define-key map [?\C-c ?\C-c] 'change-log-goto-source)
    (define-key map [menu-bar changelog] (cons "ChangeLog" menu-map))
    (define-key menu-map [gs]
      '(menu-item "Go To Source" change-log-goto-source
		  :help "Go to source location of ChangeLog tag near point"))
    (define-key menu-map [ff]
      '(menu-item "Find File" change-log-find-file
		  :help "Visit the file for the change under point"))
    (define-key menu-map [sep] '("--"))
    (define-key menu-map [nx]
      '(menu-item "Next Log-Edit Comment" add-log-edit-next-comment
		  :help "Cycle forward through Log-Edit mode comment history"))
    (define-key menu-map [pr]
      '(menu-item "Previous Log-Edit Comment" add-log-edit-prev-comment
		  :help "Cycle backward through Log-Edit mode comment history"))
    map)
  "Keymap for Change Log major mode.")

;; It used to be called change-log-time-zone-rule but really should be
;; called add-log-time-zone-rule since it's only used from add-log-* code.
(defvaralias 'change-log-time-zone-rule 'add-log-time-zone-rule)
(defvar add-log-time-zone-rule nil
  "Time zone used for calculating change log time stamps.
It takes the same format as the TZ argument of `set-time-zone-rule'.
If nil, use local time.
If t, use universal time.")
(put 'add-log-time-zone-rule 'safe-local-variable
     (lambda (x) (or (booleanp x) (stringp x))))

(defun add-log-iso8601-time-zone (&optional time)
  (let* ((utc-offset (or (car (current-time-zone time)) 0))
	 (sign (if (< utc-offset 0) ?- ?+))
	 (sec (abs utc-offset))
	 (ss (% sec 60))
	 (min (/ sec 60))
	 (mm (% min 60))
	 (hh (/ min 60)))
    (format (cond ((not (zerop ss)) "%c%02d:%02d:%02d")
		  ((not (zerop mm)) "%c%02d:%02d")
		  (t "%c%02d"))
	    sign hh mm ss)))

(defvar add-log-iso8601-with-time-zone nil)

(defun add-log-iso8601-time-string ()
  (let ((time (format-time-string "%Y-%m-%d"
                                  nil (eq t add-log-time-zone-rule))))
    (if add-log-iso8601-with-time-zone
        (concat time " " (add-log-iso8601-time-zone))
      time)))

(defun change-log-name ()
  "Return (system-dependent) default name for a change log file."
  (or change-log-default-name
      "ChangeLog"))

(defun add-log-edit-prev-comment (arg)
  "Cycle backward through Log-Edit mode comment history.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (save-restriction
    (narrow-to-region (point)
		      (if (memq last-command '(add-log-edit-prev-comment
					       add-log-edit-next-comment))
			  (mark) (point)))
    (when (fboundp 'log-edit-previous-comment)
      (log-edit-previous-comment arg)
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (unless (save-restriction (widen) (bolp))
	(delete-region (point) (progn (skip-chars-forward " \t\n") (point))))
      (set-mark (point-min))
      (goto-char (point-max))
      (delete-region (point) (progn (skip-chars-backward " \t\n") (point))))))

(defun add-log-edit-next-comment (arg)
  "Cycle forward through Log-Edit mode comment history.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (add-log-edit-prev-comment (- arg)))

;;;###autoload
(defun prompt-for-change-log-name ()
  "Prompt for a change log name."
  (let* ((default (change-log-name))
	 (name (expand-file-name
		(read-file-name (format "Log file (default %s): " default)
				nil default))))
    ;; Handle something that is syntactically a directory name.
    ;; Look for ChangeLog or whatever in that directory.
    (if (string= (file-name-nondirectory name) "")
	(expand-file-name (file-name-nondirectory default)
			  name)
      ;; Handle specifying a file that is a directory.
      (if (file-directory-p name)
	  (expand-file-name (file-name-nondirectory default)
			    (file-name-as-directory name))
	name))))

(defun change-log-version-number-search ()
  "Return version number of current buffer's file.
This is the value returned by `vc-working-revision' or, if that is
nil, by matching `change-log-version-number-regexp-list'."
  (let* ((size (buffer-size))
	 (limit
	  ;; The version number can be anywhere in the file, but
	  ;; restrict search to the file beginning: 10% should be
	  ;; enough to prevent some mishits.
	  ;;
	  ;; Apply percentage only if buffer size is bigger than
	  ;; approx 100 lines.
	  (if (> size (* 100 80)) (+ (point) (/ size 10)))))
    (or (and buffer-file-name (vc-working-revision buffer-file-name))
	(save-restriction
	  (widen)
	  (let ((regexps change-log-version-number-regexp-list)
		version)
	    (while regexps
	      (save-excursion
		(goto-char (point-min))
		(when (re-search-forward (pop regexps) limit t)
		  (setq version (match-string 1)
			regexps nil))))
	    version)))))

(declare-function diff-find-source-location "diff-mode"
		  (&optional other-file reverse noprompt))

;;;###autoload
(defun find-change-log (&optional file-name buffer-file)
  "Find a change log file for \\[add-change-log-entry] and return the name.

Optional arg FILE-NAME specifies the file to use.
If FILE-NAME is nil, use the value of `change-log-default-name'.
If `change-log-default-name' is nil, behave as though it were 'ChangeLog'
\(or whatever we use on this operating system).

If `change-log-default-name' contains a leading directory component, then
simply find it in the current directory.  Otherwise, search in the current
directory and its successive parents for a file so named.

Once a file is found, `change-log-default-name' is set locally in the
current buffer to the complete file name.
Optional arg BUFFER-FILE overrides `buffer-file-name'."
  ;; If we are called from a diff, first switch to the source buffer;
  ;; in order to respect buffer-local settings of change-log-default-name, etc.
  (with-current-buffer (let ((buff (if (derived-mode-p 'diff-mode)
				       (car (ignore-errors
					     (diff-find-source-location))))))
			 (if (buffer-live-p buff) buff
			   (current-buffer)))
      ;; If user specified a file name or if this buffer knows which one to use,
      ;; just use that.
    (or file-name
	(setq file-name (and change-log-default-name
			     (file-name-directory change-log-default-name)
			     change-log-default-name))
	(progn
	  ;; Chase links in the source file
	  ;; and use the change log in the dir where it points.
	  (setq file-name (or (and (or buffer-file buffer-file-name)
				   (file-name-directory
				    (file-chase-links
				     (or buffer-file buffer-file-name))))
			      default-directory))
	  (if (file-directory-p file-name)
	      (setq file-name (expand-file-name (change-log-name) file-name)))
	  ;; Chase links before visiting the file.
	  ;; This makes it easier to use a single change log file
	  ;; for several related directories.
	  (setq file-name (file-chase-links file-name))
	  (setq file-name (expand-file-name file-name))
	  ;; Move up in the dir hierarchy till we find a change log file.
	  (let ((file1 file-name)
		parent-dir)
	    (while (and (not (or (get-file-buffer file1) (file-exists-p file1)))
			(progn (setq parent-dir
				     (file-name-directory
				      (directory-file-name
				       (file-name-directory file1))))
			       ;; Give up if we are already at the root dir.
			       (not (string= (file-name-directory file1)
					     parent-dir))))
	      ;; Move up to the parent dir and try again.
	      (setq file1 (expand-file-name
			   (file-name-nondirectory (change-log-name))
			   parent-dir)))
	    ;; If we found a change log in a parent, use that.
	    (if (or (get-file-buffer file1) (file-exists-p file1))
		(setq file-name file1)))))
    ;; Make a local variable in this buffer so we needn't search again.
    (set (make-local-variable 'change-log-default-name) file-name))
  file-name)

(defun add-log-file-name (buffer-file log-file)
  ;; Never want to add a change log entry for the ChangeLog file itself.
  (unless (or (null buffer-file) (string= buffer-file log-file))
    (if add-log-file-name-function
	(funcall add-log-file-name-function buffer-file)
      (setq buffer-file
            (let* ((dir (file-name-directory log-file))
                   (rel (file-relative-name buffer-file dir)))
              ;; Sometimes with symlinks, the two buffers may have names that
              ;; appear to belong to different directory trees.  So check the
              ;; file-truenames, to see if we get a better result.
              (if (not (string-match "\\`\\.\\./" rel))
                  rel
                (let ((new (file-relative-name (file-truename buffer-file)
                                               (file-truename dir))))
                  (if (< (length new) (length rel))
                      new rel)))))
      ;; If we have a backup file, it's presumably because we're
      ;; comparing old and new versions (e.g. for deleted
      ;; functions) and we'll want to use the original name.
      (if (backup-file-name-p buffer-file)
	  (file-name-sans-versions buffer-file)
	buffer-file))))

;;;###autoload
(defun add-change-log-entry (&optional whoami file-name other-window new-entry
				       put-new-entry-on-new-line)
  "Find change log file, and add an entry for today and an item for this file.
Optional arg WHOAMI (interactive prefix) non-nil means prompt for user
name and email (stored in `add-log-full-name' and `add-log-mailing-address').

Second arg FILE-NAME is file name of the change log.
If nil, use the value of `change-log-default-name'.

Third arg OTHER-WINDOW non-nil means visit in other window.

Fourth arg NEW-ENTRY non-nil means always create a new entry at the front;
never append to an existing entry.  Option `add-log-keep-changes-together'
otherwise affects whether a new entry is created.

Fifth arg PUT-NEW-ENTRY-ON-NEW-LINE non-nil means that if a new
entry is created, put it on a new line by itself, do not put it
after a comma on an existing line.

Option `add-log-always-start-new-record' non-nil means always create a
new record, even when the last record was made on the same date and by
the same person.

The change log file can start with a copyright notice and a copying
permission notice.  The first blank line indicates the end of these
notices.

Today's date is calculated according to `add-log-time-zone-rule' if
non-nil, otherwise in local time."
  (interactive (list current-prefix-arg
		     (prompt-for-change-log-name)))
  (let* ((defun (add-log-current-defun))
	 (version (and change-log-version-info-enabled
		       (change-log-version-number-search)))
	 (buf-file-name (if add-log-buffer-file-name-function
			    (funcall add-log-buffer-file-name-function)
			  buffer-file-name))
	 (buffer-file (if buf-file-name (expand-file-name buf-file-name)))
	 (file-name (expand-file-name (find-change-log file-name buffer-file)))
	 ;; Set ITEM to the file name to use in the new item.
	 (item (add-log-file-name buffer-file file-name)))

    (unless (equal file-name buffer-file-name)
      (cond
       ((equal file-name (buffer-file-name (window-buffer (selected-window))))
        ;; If the selected window already shows the desired buffer don't show
        ;; it again (particularly important if other-window is true).
        ;; This is important for diff-add-change-log-entries-other-window.
        (set-buffer (window-buffer (selected-window))))
       ((or other-window (window-dedicated-p (selected-window)))
        (find-file-other-window file-name))
       (t (find-file file-name))))
    (or (derived-mode-p 'change-log-mode)
	(change-log-mode))
    (undo-boundary)
    (goto-char (point-min))

    (let ((full-name (or add-log-full-name (user-full-name)))
          (mailing-address (or add-log-mailing-address user-mail-address)))

      (when whoami
        (setq full-name (read-string "Full name: " full-name))
        ;; Note that some sites have room and phone number fields in
        ;; full name which look silly when inserted.  Rather than do
        ;; anything about that here, let user give prefix argument so that
        ;; s/he can edit the full name field in prompter if s/he wants.
        (setq mailing-address
	      (read-string "Mailing address: " mailing-address)))

      ;; If file starts with a copyright and permission notice, skip them.
      ;; Assume they end at first blank line.
      (when (looking-at "Copyright")
        (search-forward "\n\n")
        (skip-chars-forward "\n"))

      ;; Advance into first entry if it is usable; else make new one.
      (let ((new-entries
             (mapcar (lambda (addr)
                       (concat
                        (if (stringp add-log-time-zone-rule)
                            (let ((tz (getenv "TZ")))
                              (unwind-protect
                                  (progn
                                    (setenv "TZ" add-log-time-zone-rule)
                                    (funcall add-log-time-format))
                                (setenv "TZ" tz)))
                          (funcall add-log-time-format))
                        "  " full-name
                        "  <" addr ">"))
                     (if (consp mailing-address)
                         mailing-address
                       (list mailing-address)))))
        (if (and (not add-log-always-start-new-record)
                 (let ((hit nil))
                   (dolist (entry new-entries hit)
                     (and (looking-at (regexp-quote entry))
			  ;; Reject multiple author entries.  (Bug#8645)
			  (save-excursion
			    (forward-line 1)
			    (not (looking-at "[ \t]+.*<.*>$")))
			  (setq hit t)))))
            (forward-line 1)
          (insert (nth (random (length new-entries))
                       new-entries)
                  (if use-hard-newlines hard-newline "\n")
                  (if use-hard-newlines hard-newline "\n"))
          (forward-line -1))))

    ;; Determine where we should stop searching for a usable
    ;; item to add to, within this entry.
    (let ((bound
           (save-excursion
             (if (looking-at "\n*[^\n* \t]")
                 (skip-chars-forward "\n")
               (if add-log-keep-changes-together
                   (forward-page)      ; page delimits entries for date
                 (forward-paragraph))) ; paragraph delimits entries for file
             (point))))

      ;; Now insert the new line for this item.
      (cond ((re-search-forward "^\\s *\\* *$" bound t)
             ;; Put this file name into the existing empty item.
             (if item
                 (insert item)))
            ((and (not new-entry)
                  (let (case-fold-search)
                    (re-search-forward
                     (concat (regexp-quote (concat "* " item))
                             ;; Don't accept `foo.bar' when
                             ;; looking for `foo':
                             "\\(\\s \\|[(),:]\\)")
                     bound t)))
             ;; Add to the existing item for the same file.
             (re-search-forward "^\\s *$\\|^\\s \\*")
             (goto-char (match-beginning 0))
             ;; Delete excess empty lines; make just 2.
             (while (and (not (eobp)) (looking-at "^\\s *$"))
               (delete-region (point) (line-beginning-position 2)))
             (insert (if use-hard-newlines hard-newline "\n")
                     (if use-hard-newlines hard-newline "\n"))
             (forward-line -2)
             (indent-relative-maybe))
            (t
             ;; Make a new item.
             (while (looking-at "\\sW")
               (forward-line 1))
             (while (and (not (eobp)) (looking-at "^\\s *$"))
               (delete-region (point) (line-beginning-position 2)))
             (insert (if use-hard-newlines hard-newline "\n")
                     (if use-hard-newlines hard-newline "\n")
                     (if use-hard-newlines hard-newline "\n"))
             (forward-line -2)
             (indent-to left-margin)
             (insert "* ")
             (if item (insert item)))))
    ;; Now insert the function name, if we have one.
    ;; Point is at the item for this file,
    ;; either at the end of the line or at the first blank line.
    (if (not defun)
	;; No function name, so put in a colon unless we have just a star.
	(unless (save-excursion
		  (beginning-of-line 1)
		  (looking-at "\\s *\\(\\* *\\)?$"))
	  (insert ": ")
	  (if version (insert version ?\s)))
      ;; Make it easy to get rid of the function name.
      (undo-boundary)
      (unless (save-excursion
		(beginning-of-line 1)
		(looking-at "\\s *$"))
	(insert ?\s))
      ;; See if the prev function name has a message yet or not.
      ;; If not, merge the two items.
      (let ((pos (point-marker)))
	(skip-syntax-backward " ")
	(skip-chars-backward "):")
	(if (and (not put-new-entry-on-new-line)
		 (looking-at "):")
		 (let ((pos (save-excursion (backward-sexp 1) (point))))
		   (when (equal (buffer-substring pos (point)) defun)
		     (delete-region pos (point)))
		   (> fill-column (+ (current-column) (length defun) 4))))
	    (progn (skip-chars-backward ", ")
		   (delete-region (point) pos)
		   (unless (memq (char-before) '(?\()) (insert ", ")))
	  (when (and (not put-new-entry-on-new-line) (looking-at "):"))
	    (delete-region (+ 1 (point)) (line-end-position)))
	  (goto-char pos)
	  (insert "("))
	(set-marker pos nil))
      (insert defun "): ")
      (if version (insert version ?\s)))))

;;;###autoload
(defun add-change-log-entry-other-window (&optional whoami file-name)
  "Find change log file in other window and add entry and item.
This is just like `add-change-log-entry' except that it displays
the change log file in another window."
  (interactive (if current-prefix-arg
		   (list current-prefix-arg
			 (prompt-for-change-log-name))))
  (add-change-log-entry whoami file-name t))


(defvar change-log-indent-text 0)

(defun change-log-fill-parenthesized-list ()
  ;; Fill parenthesized lists of names according to GNU standards.
  ;; * file-name.ext (very-long-foo, very-long-bar, very-long-foobar):
  ;; should be filled as
  ;; * file-name.ext (very-long-foo, very-long-bar)
  ;; (very-long-foobar):
  (save-excursion
    (end-of-line 0)
    (skip-chars-backward " \t")
    (when (and (equal (char-before) ?\,)
	       (> (point) (1+ (point-min))))
      (condition-case nil
	  (when (save-excursion
		  (and (prog2
			   (up-list -1)
			   (equal (char-after) ?\()
			 (skip-chars-backward " \t"))
		       (or (bolp)
			   ;; Skip everything but a whitespace or asterisk.
			   (and (not (zerop (skip-chars-backward "^ \t\n*")))
				(skip-chars-backward " \t")
				;; We want one asterisk here.
				(= (skip-chars-backward "*") -1)
				(skip-chars-backward " \t")
				(bolp)))))
	    ;; Delete the comma.
	    (delete-char -1)
	    ;; Close list on previous line.
	    (insert ")")
	    (skip-chars-forward " \t\n")
	    ;; Start list on new line.
	    (insert-before-markers "("))
	(error nil)))))

(defun change-log-indent ()
  (change-log-fill-parenthesized-list)
  (let* ((indent
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (cond
	     ((and (looking-at "\\(.*\\)  [^ \n].*[^ \n]  <.*>\\(?: +(.*)\\)? *$")
		   ;; Matching the output of add-log-time-format is difficult,
		   ;; but I'll get it has at least two adjacent digits.
		   (string-match "[[:digit:]][[:digit:]]" (match-string 1)))
	      0)
	     ((looking-at "[^*(]")
	      (+ (current-left-margin) change-log-indent-text))
	     (t (current-left-margin)))))
	 (pos (save-excursion (indent-line-to indent) (point))))
    (if (> pos (point)) (goto-char pos))))


(defvar smerge-resolve-function)
(defvar copyright-at-end-flag)

;;;###autoload
(define-derived-mode change-log-mode text-mode "Change Log"
  "Major mode for editing change logs; like Indented Text mode.
Prevents numeric backups and sets `left-margin' to 8 and `fill-column' to 74.
New log entries are usually made with \\[add-change-log-entry] or \\[add-change-log-entry-other-window].
Each entry behaves as a paragraph, and the entries for one day as a page.
Runs `change-log-mode-hook'.
\n\\{change-log-mode-map}"
  (setq left-margin 8
	fill-column 74
	indent-tabs-mode t
	tab-width 8
	show-trailing-whitespace t)
  (set (make-local-variable 'fill-forward-paragraph-function)
       'change-log-fill-forward-paragraph)
  ;; Make sure we call `change-log-indent' when filling.
  (set (make-local-variable 'fill-indent-according-to-mode) t)
  ;; Avoid that filling leaves behind a single "*" on a line.
  (add-hook 'fill-nobreak-predicate
	    (lambda ()
              (looking-back "^\\s *\\*\\s *" (line-beginning-position)))
	    nil t)
  (set (make-local-variable 'indent-line-function) 'change-log-indent)
  (set (make-local-variable 'tab-always-indent) nil)
  (set (make-local-variable 'copyright-at-end-flag) t)
  ;; We really do want "^" in paragraph-start below: it is only the
  ;; lines that begin at column 0 (despite the left-margin of 8) that
  ;; we are looking for.  Adding `* ' allows eliding the blank line
  ;; between entries for different files.
  (set (make-local-variable 'paragraph-start) "\\s *$\\|\f\\|^\\<")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  ;; Match null string on the date-line so that the date-line
  ;; is grouped with what follows.
  (set (make-local-variable 'page-delimiter) "^\\<\\|^\f")
  (set (make-local-variable 'version-control) 'never)
  (set (make-local-variable 'smerge-resolve-function)
       'change-log-resolve-conflict)
  (set (make-local-variable 'adaptive-fill-regexp) "\\s *")
  (set (make-local-variable 'font-lock-defaults)
       '(change-log-font-lock-keywords t nil nil backward-paragraph))
  (set (make-local-variable 'multi-isearch-next-buffer-function)
       'change-log-next-buffer)
  (set (make-local-variable 'beginning-of-defun-function)
       'change-log-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'change-log-end-of-defun)
  ;; next-error function glue
  (setq next-error-function 'change-log-next-error)
  (setq next-error-last-buffer (current-buffer)))

(defun change-log-next-buffer (&optional buffer wrap)
  "Return the next buffer in the series of ChangeLog file buffers.
This function is used for multiple buffers isearch.
A sequence of buffers is formed by ChangeLog files with decreasing
numeric file name suffixes in the directory of the initial ChangeLog
file were isearch was started."
  (let* ((name (change-log-name))
	 (files (cons name (sort (file-expand-wildcards
				  (concat name "[-.][0-9]*"))
				 (lambda (a b)
                                   ;; The file's extension may not have a valid
                                   ;; version form (e.g. VC backup revisions).
                                   (ignore-errors
                                     (version< (substring b (length name))
                                               (substring a (length name))))))))
	 (files (if isearch-forward files (reverse files))))
    (find-file-noselect
     (if wrap
	 (car files)
       (cadr (member (file-name-nondirectory (buffer-file-name buffer))
		     files))))))

(defun change-log-fill-forward-paragraph (n)
  "Cut paragraphs so filling preserves open parentheses at beginning of lines."
  (let (;; Add lines starting with whitespace followed by a left paren or an
	;; asterisk.
	(paragraph-start (concat paragraph-start "\\|\\s *\\(?:\\s(\\|\\*\\)")))
    (forward-paragraph n)))

(defcustom add-log-current-defun-header-regexp
  "^\\([[:upper:]][[:upper:]_ ]*[[:upper:]_]\\|[-_[:alpha:]]+\\)[ \t]*[:=]"
  "Heuristic regexp used by `add-log-current-defun' for unknown major modes.
The regexp's first submatch is placed in the ChangeLog entry, in
parentheses."
  :type 'regexp
  :group 'change-log)

;;;###autoload
(defvar add-log-lisp-like-modes
  '(emacs-lisp-mode lisp-mode scheme-mode dsssl-mode lisp-interaction-mode)
  "*Modes that look like Lisp to `add-log-current-defun'.")

;;;###autoload
(defvar add-log-c-like-modes
  '(c-mode c++-mode c++-c-mode objc-mode)
  "*Modes that look like C to `add-log-current-defun'.")

;;;###autoload
(defvar add-log-tex-like-modes
  '(TeX-mode plain-TeX-mode LaTeX-mode tex-mode)
  "*Modes that look like TeX to `add-log-current-defun'.")

(declare-function c-cpp-define-name "cc-cmds" ())
(declare-function c-defun-name      "cc-cmds" ())

;;;###autoload
(defun add-log-current-defun ()
  "Return name of function definition point is in, or nil.

Understands C, Lisp, LaTeX (\"functions\" are chapters, sections, ...),
Texinfo (@node titles) and Perl.

Other modes are handled by a heuristic that looks in the 10K before
point for uppercase headings starting in the first column or
identifiers followed by `:' or `='.  See variables
`add-log-current-defun-header-regexp' and
`add-log-current-defun-function'.

Has a preference of looking backwards."
  (condition-case nil
      (save-excursion
	(let ((location (point)))
	  (cond (add-log-current-defun-function
		 (funcall add-log-current-defun-function))
		((apply 'derived-mode-p add-log-lisp-like-modes)
		 ;; If we are now precisely at the beginning of a defun,
		 ;; make sure beginning-of-defun finds that one
		 ;; rather than the previous one.
		 (or (eobp) (forward-char 1))
		 (beginning-of-defun)
		 ;; Make sure we are really inside the defun found,
		 ;; not after it.
		 (when (and (looking-at "\\s(")
			    (progn (end-of-defun)
				   (< location (point)))
			    (progn (forward-sexp -1)
				   (>= location (point))))
		   (if (looking-at "\\s(")
		       (forward-char 1))
		   ;; Skip the defining construct name, typically "defun"
		   ;; or "defvar".
		   (forward-sexp 1)
		   ;; The second element is usually a symbol being defined.
		   ;; If it is not, use the first symbol in it.
		   (skip-chars-forward " \t\n'(")
		   (buffer-substring-no-properties (point)
						   (progn (forward-sexp 1)
							  (point)))))
		((apply 'derived-mode-p add-log-c-like-modes)
		 (or (c-cpp-define-name)
		     (c-defun-name)))
		((apply #'derived-mode-p add-log-tex-like-modes)
		 (if (re-search-backward
		      "\\\\\\(sub\\)*\\(section\\|paragraph\\|chapter\\)"
		      nil t)
		     (progn
		       (goto-char (match-beginning 0))
		       (buffer-substring-no-properties
			(1+ (point))	; without initial backslash
			(line-end-position)))))
		((derived-mode-p 'texinfo-mode)
		 (if (re-search-backward "^@node[ \t]+\\([^,\n]+\\)" nil t)
		     (match-string-no-properties 1)))
		((derived-mode-p 'perl-mode 'cperl-mode)
		 (if (re-search-backward "^sub[ \t]+\\([^({ \t\n]+\\)" nil t)
		     (match-string-no-properties 1)))
		;; Emacs's autoconf-mode installs its own
		;; `add-log-current-defun-function'.  This applies to
		;; a different mode apparently for editing .m4
		;; autoconf source.
                ((derived-mode-p 'autoconf-mode)
                 (if (re-search-backward
		      "^\\(\\(m4_\\)?define\\|A._DEFUN\\)(\\[?\\([A-Za-z0-9_]+\\)" nil t)
                     (match-string-no-properties 3)))
		(t
		 ;; If all else fails, try heuristics
		 (let (case-fold-search
		       result)
		   (end-of-line)
		   (when (re-search-backward
			  add-log-current-defun-header-regexp
			  (- (point) 10000)
			  t)
		     (setq result (or (match-string-no-properties 1)
				      (match-string-no-properties 0)))
		     ;; Strip whitespace away
		     (when (string-match "\\([^ \t\n\r\f].*[^ \t\n\r\f]\\)"
					 result)
		       (setq result (match-string-no-properties 1 result)))
		     result))))))
    (error nil)))

(defvar change-log-get-method-definition-md)

;; Subroutine used within change-log-get-method-definition.
;; Add the last match in the buffer to the end of `md',
;; followed by the string END; move to the end of that match.
(defun change-log-get-method-definition-1 (end)
  (setq change-log-get-method-definition-md
	(concat change-log-get-method-definition-md
		(match-string 1)
		end))
  (goto-char (match-end 0)))

(defun change-log-get-method-definition ()
"For Objective C, return the method name if we are in a method."
  (let ((change-log-get-method-definition-md "["))
    (save-excursion
      (if (re-search-backward "^@implementation\\s-*\\([A-Za-z_]*\\)" nil t)
	  (change-log-get-method-definition-1 " ")))
    (save-excursion
      (cond
       ((re-search-forward "^\\([-+]\\)[ \t\n\f\r]*\\(([^)]*)\\)?\\s-*" nil t)
	(change-log-get-method-definition-1 "")
	(while (not (looking-at "[{;]"))
	  (looking-at
	   "\\([A-Za-z_]*:?\\)\\s-*\\(([^)]*)\\)?[A-Za-z_]*[ \t\n\f\r]*")
	  (change-log-get-method-definition-1 ""))
	(concat change-log-get-method-definition-md "]"))))))

(autoload 'timezone-make-date-sortable "timezone")

(defun change-log-sortable-date-at ()
  "Return date of log entry in a consistent form for sorting.
Point is assumed to be at the start of the entry."
  (if (looking-at change-log-start-entry-re)
      (let ((date (match-string-no-properties 0)))
	(if date
	    (if (string-match "\\(....\\)-\\(..\\)-\\(..\\)\\s-+" date)
		(concat (match-string 1 date) (match-string 2 date)
			(match-string 3 date))
	      (ignore-errors (timezone-make-date-sortable date)))))
    (error "Bad date")))

(defun change-log-resolve-conflict ()
  "Function to be used in `smerge-resolve-function'."
  (save-excursion
    (save-restriction
      (narrow-to-region (match-beginning 0) (match-end 0))
      (let ((mb1 (match-beginning 1))
            (me1 (match-end 1))
            (mb3 (match-beginning 3))
            (me3 (match-end 3))
            (tmp1 (generate-new-buffer " *changelog-resolve-1*"))
	    (tmp2 (generate-new-buffer " *changelog-resolve-2*")))
	(unwind-protect
	    (let ((buf (current-buffer)))
	      (with-current-buffer tmp1
                (change-log-mode)
		(insert-buffer-substring buf mb1 me1))
	      (with-current-buffer tmp2
                (change-log-mode)
		(insert-buffer-substring buf mb3 me3)
                ;; Do the merge here instead of inside `buf' so as to be
                ;; more robust in case change-log-merge fails.
		(change-log-merge tmp1))
	      (goto-char (point-max))
	      (delete-region (point-min)
			     (prog1 (point)
			       (insert-buffer-substring tmp2))))
	  (kill-buffer tmp1)
	  (kill-buffer tmp2))))))

;;;###autoload
(defun change-log-merge (other-log)
  "Merge the contents of change log file OTHER-LOG with this buffer.
Both must be found in Change Log mode (since the merging depends on
the appropriate motion commands).  OTHER-LOG can be either a file name
or a buffer.

Entries are inserted in chronological order.  Both the current and
old-style time formats for entries are supported."
  (interactive "*fLog file name to merge: ")
  (if (not (derived-mode-p 'change-log-mode))
      (error "Not in Change Log mode"))
  (let ((other-buf (if (bufferp other-log) other-log
		     (find-file-noselect other-log)))
	(buf (current-buffer))
	date1 start end)
    (save-excursion
      (goto-char (point-min))
      (set-buffer other-buf)
      (goto-char (point-min))
      (if (not (derived-mode-p 'change-log-mode))
	  (error "%s not found in Change Log mode" other-log))
      ;; Loop through all the entries in OTHER-LOG.
      (while (not (eobp))
	(setq date1 (change-log-sortable-date-at))
	(setq start (point)
	      end (progn (forward-page) (point)))
	;; Look for an entry in original buffer that isn't later.
	(with-current-buffer buf
	  (while (and (not (eobp))
		      (string< date1 (change-log-sortable-date-at)))
	    (forward-page))
	  (if (not (eobp))
	      (insert-buffer-substring other-buf start end)
	    ;; At the end of the original buffer, insert a newline to
	    ;; separate entries and then the rest of the file being
	    ;; merged.
	    (unless (or (bobp)
			(and (= ?\n (char-before))
			     (or (<= (1- (point)) (point-min))
				 (= ?\n (char-before (1- (point)))))))
	      (insert (if use-hard-newlines hard-newline "\n")))
	    ;; Move to the end of it to terminate outer loop.
	    (with-current-buffer other-buf
	      (goto-char (point-max)))
	    (insert-buffer-substring other-buf start)))))))

(defun change-log-beginning-of-defun ()
  (re-search-backward change-log-start-entry-re nil 'move))

(defun change-log-end-of-defun ()
  ;; Look back and if there is no entry there it means we are before
  ;; the first ChangeLog entry, so go forward until finding one.
  (unless (save-excursion (re-search-backward change-log-start-entry-re nil t))
    (re-search-forward change-log-start-entry-re nil t))

  ;; In case we are at the end of log entry going forward a line will
  ;; make us find the next entry when searching. If we are inside of
  ;; an entry going forward a line will still keep the point inside
  ;; the same entry.
  (forward-line 1)

  ;; In case we are at the beginning of an entry, move past it.
  (when (looking-at change-log-start-entry-re)
    (goto-char (match-end 0))
    (forward-line 1))

  ;; Search for the start of the next log entry.  Go to the end of the
  ;; buffer if we could not find a next entry.
  (when (re-search-forward change-log-start-entry-re nil 'move)
    (goto-char (match-beginning 0))
    (forward-line -1)))

(provide 'add-log)

;;; add-log.el ends here
