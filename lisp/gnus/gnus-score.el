;;; gnus-score.el --- scoring code for Gnus

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Per Abrahamsen <amanda@iesd.auc.dk>
;;	Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-range)
(require 'gnus-win)
(require 'message)
(require 'score-mode)

(defcustom gnus-global-score-files nil
  "List of global score files and directories.
Set this variable if you want to use people's score files.  One entry
for each score file or each score file directory.  Gnus will decide
by itself what score files are applicable to which group.

Say you want to use the single score file
\"/ftp.gnus.org@ftp:/pub/larsi/ding/score/soc.motss.SCORE\" and all
score files in the \"/ftp.some-where:/pub/score\" directory.

 (setq gnus-global-score-files
       '(\"/ftp.gnus.org:/pub/larsi/ding/score/soc.motss.SCORE\"
	 \"/ftp.some-where:/pub/score\"))"
  :group 'gnus-score-files
  :type '(repeat file))

(defcustom gnus-score-file-single-match-alist nil
  "Alist mapping regexps to lists of score files.
Each element of this alist should be of the form
	(\"REGEXP\" [ \"SCORE-FILE-1\" ] [ \"SCORE-FILE-2\" ] ... )

If the name of a group is matched by REGEXP, the corresponding scorefiles
will be used for that group.
The first match found is used, subsequent matching entries are ignored (to
use multiple matches, see `gnus-score-file-multiple-match-alist').

These score files are loaded in addition to any files returned by
`gnus-score-find-score-files-function'."
  :group 'gnus-score-files
  :type '(repeat (cons regexp (repeat file))))

(defcustom gnus-score-file-multiple-match-alist nil
  "Alist mapping regexps to lists of score files.
Each element of this alist should be of the form
	(\"REGEXP\" [ \"SCORE-FILE-1\" ] [ \"SCORE-FILE-2\" ] ... )

If the name of a group is matched by REGEXP, the corresponding scorefiles
will be used for that group.
If multiple REGEXPs match a group, the score files corresponding to each
match will be used (for only one match to be used, see
`gnus-score-file-single-match-alist').

These score files are loaded in addition to any files returned by
`gnus-score-find-score-files-function'."
  :group 'gnus-score-files
  :type '(repeat (cons regexp (repeat file))))

(defcustom gnus-score-file-suffix "SCORE"
  "Suffix of the score files."
  :group 'gnus-score-files
  :type 'string)

(defcustom gnus-adaptive-file-suffix "ADAPT"
  "Suffix of the adaptive score files."
  :group 'gnus-score-files
  :group 'gnus-score-adapt
  :type 'string)

(defcustom gnus-score-find-score-files-function 'gnus-score-find-bnews
  "Function used to find score files.
The function will be called with the group name as the argument, and
should return a list of score files to apply to that group.  The score
files do not actually have to exist.

Predefined values are:

`gnus-score-find-single': Only apply the group's own score file.
`gnus-score-find-hierarchical': Also apply score files from parent groups.
`gnus-score-find-bnews': Apply score files whose names matches.

See the documentation to these functions for more information.

This variable can also be a list of functions to be called.  Each
function is given the group name as argument and should either return
a list of score files, or a list of score alists.

If functions other than these pre-defined functions are used,
the `a' symbolic prefix to the score commands will always use
\"all.SCORE\"."
  :group 'gnus-score-files
  :type '(radio (function-item gnus-score-find-single)
		(function-item gnus-score-find-hierarchical)
		(function-item gnus-score-find-bnews)
		(repeat :tag "List of functions"
			(choice (function :tag "Other" :value 'ignore)
				(function-item gnus-score-find-single)
				(function-item gnus-score-find-hierarchical)
				(function-item gnus-score-find-bnews)))
		(function :tag "Other" :value 'ignore)))

(defcustom gnus-score-interactive-default-score 1000
  "*Scoring commands will raise/lower the score with this number as the default."
  :group 'gnus-score-default
  :type 'integer)

(defcustom gnus-score-expiry-days 7
  "*Number of days before unused score file entries are expired.
If this variable is nil, no score file entries will be expired."
  :group 'gnus-score-expire
  :type '(choice (const :tag "never" nil)
		 number))

(defcustom gnus-update-score-entry-dates t
  "*If non-nil, update matching score entry dates.
If this variable is nil, then score entries that provide matches
will be expired along with non-matching score entries."
  :group 'gnus-score-expire
  :type 'boolean)

(defcustom gnus-decay-scores nil
  "*If non-nil, decay non-permanent scores.

If it is a regexp, only decay score files matching regexp."
  :group 'gnus-score-decay
  :type `(choice (const :tag "never" nil)
		 (const :tag "always" t)
		 (const :tag "adaptive score files"
			,(concat "\\." gnus-adaptive-file-suffix "\\'"))
		 (regexp)))

(defcustom gnus-decay-score-function 'gnus-decay-score
  "*Function called to decay a score.
It is called with one parameter -- the score to be decayed."
  :group 'gnus-score-decay
  :type '(radio (function-item gnus-decay-score)
		(function :tag "Other")))

(defcustom gnus-score-decay-constant 3
  "*Decay all \"small\" scores with this amount."
  :group 'gnus-score-decay
  :type 'integer)

(defcustom gnus-score-decay-scale .05
  "*Decay all \"big\" scores with this factor."
  :group 'gnus-score-decay
  :type 'number)

(defcustom gnus-home-score-file nil
  "Variable to control where interactive score entries are to go.
It can be:

 * A string
   This file will be used as the home score file.

 * A function
   The result of this function will be used as the home score file.
   The function will be passed the name of the group as its
   parameter.

 * A list
   The elements in this list can be:

   * `(regexp file-name ...)'
     If the `regexp' matches the group name, the first `file-name'
     will be used as the home score file.  (Multiple filenames are
     allowed so that one may use gnus-score-file-single-match-alist to
     set this variable.)

   * A function.
     If the function returns non-nil, the result will be used
     as the home score file.  The function will be passed the
     name of the group as its parameter.

   * A string.  Use the string as the home score file.

   The list will be traversed from the beginning towards the end looking
   for matches."
  :group 'gnus-score-files
  :type '(choice string
		 (repeat (choice string
				 (cons regexp (repeat file))
				 function))
		 (function-item gnus-hierarchial-home-score-file)
		 (function-item gnus-current-home-score-file)
		 function))

(defcustom gnus-home-adapt-file nil
  "Variable to control where new adaptive score entries are to go.
This variable allows the same syntax as `gnus-home-score-file'."
  :group 'gnus-score-adapt
  :group 'gnus-score-files
  :type '(choice string
		 (repeat (choice string
				 (cons regexp (repeat file))
				 function))
		 function))

(defcustom gnus-default-adaptive-score-alist
  `((gnus-kill-file-mark)
    (gnus-unread-mark)
    (gnus-read-mark
     (from , (+ 2 gnus-score-decay-constant))
     (subject , (+ 27 gnus-score-decay-constant)))
    (gnus-catchup-mark
     (subject , (+ -7 (* -1 gnus-score-decay-constant))))
    (gnus-killed-mark
     (from , (- -1 gnus-score-decay-constant))
     (subject , (+ -17 (* -1 gnus-score-decay-constant))))
    (gnus-del-mark
     (from , (- -1 gnus-score-decay-constant))
     (subject , (+ -12 (* -1 gnus-score-decay-constant)))))
  "Alist of marks and scores.
If you use score decays, you might want to set values higher than
`gnus-score-decay-constant'."
  :group 'gnus-score-adapt
  :type '(repeat (cons (symbol :tag "Mark")
		       (repeat (list (choice :tag "Header"
					     (const from)
					     (const subject)
					     (symbol :tag "other"))
				     (integer :tag "Score"))))))

(defcustom gnus-adaptive-word-length-limit nil
  "*Words of a length lesser than this limit will be ignored when doing adaptive scoring."
  :version "22.1"
  :group 'gnus-score-adapt
  :type '(radio (const :format "Unlimited " nil)
		(integer :format "Maximum length: %v")))

(defcustom gnus-ignored-adaptive-words nil
  "List of words to be ignored when doing adaptive word scoring."
  :group 'gnus-score-adapt
  :type '(repeat string))

(defcustom gnus-default-ignored-adaptive-words
  '("a" "i" "the" "to" "of" "and" "in" "is" "it" "for" "that" "if" "you"
    "this" "be" "on" "with" "not" "have" "are" "or" "as" "from" "can"
    "but" "by" "at" "an" "will" "no" "all" "was" "do" "there" "my" "one"
    "so" "we" "they" "what" "would" "any" "which" "about" "get" "your"
    "use" "some" "me" "then" "name" "like" "out" "when" "up" "time"
    "other" "more" "only" "just" "end" "also" "know" "how" "new" "should"
    "been" "than" "them" "he" "who" "make" "may" "people" "these" "now"
    "their" "here" "into" "first" "could" "way" "had" "see" "work" "well"
    "were" "two" "very" "where" "while" "us" "because" "good" "same"
    "even" "much" "most" "many" "such" "long" "his" "over" "last" "since"
    "right" "before" "our" "without" "too" "those" "why" "must" "part"
    "being" "current" "back" "still" "go" "point" "value" "each" "did"
    "both" "true" "off" "say" "another" "state" "might" "under" "start"
    "try" "re")
  "*Default list of words to be ignored when doing adaptive word scoring."
  :group 'gnus-score-adapt
  :type '(repeat string))

(defcustom gnus-default-adaptive-word-score-alist
  `((,gnus-read-mark . 30)
    (,gnus-catchup-mark . -10)
    (,gnus-killed-mark . -20)
    (,gnus-del-mark . -15))
  "*Alist of marks and scores."
  :group 'gnus-score-adapt
  :type '(repeat (cons (character :tag "Mark")
		       (integer :tag "Score"))))

(defcustom gnus-adaptive-word-minimum nil
  "If a number, this is the minimum score value that can be assigned to a word."
  :group 'gnus-score-adapt
  :type '(choice (const nil) integer))

(defcustom gnus-adaptive-word-no-group-words nil
  "If t, don't adaptively score words included in the group name."
  :group 'gnus-score-adapt
  :type 'boolean)

(defcustom gnus-score-mimic-keymap nil
  "*Have the score entry functions pretend that they are a keymap."
  :group 'gnus-score-default
  :type 'boolean)

(defcustom gnus-score-exact-adapt-limit 10
  "*Number that says how long a match has to be before using substring matching.
When doing adaptive scoring, one normally uses fuzzy or substring
matching.  However, if the header one matches is short, the possibility
for false positives is great, so if the length of the match is less
than this variable, exact matching will be used.

If this variable is nil, exact matching will always be used."
  :group 'gnus-score-adapt
  :type '(choice (const nil) integer))

(defcustom gnus-score-uncacheable-files "ADAPT$"
  "All score files that match this regexp will not be cached."
  :group 'gnus-score-adapt
  :group 'gnus-score-files
  :type 'regexp)

(defcustom gnus-adaptive-pretty-print nil
  "If non-nil, adaptive score files fill are pretty printed."
  :group 'gnus-score-files
  :group 'gnus-score-adapt
  :version "23.1" ;; No Gnus
  :type 'boolean)

(defcustom gnus-score-default-header nil
  "Default header when entering new scores.

Should be one of the following symbols.

 a: from
 s: subject
 b: body
 h: head
 i: message-id
 t: references
 x: xref
 e: `extra' (non-standard overview)
 l: lines
 d: date
 f: followup

If nil, the user will be asked for a header."
  :group 'gnus-score-default
  :type '(choice (const :tag "from" a)
		 (const :tag "subject" s)
		 (const :tag "body" b)
		 (const :tag "head" h)
		 (const :tag "message-id" i)
		 (const :tag "references" t)
		 (const :tag "xref" x)
		 (const :tag "extra" e)
		 (const :tag "lines" l)
		 (const :tag "date" d)
		 (const :tag "followup" f)
		 (const :tag "ask" nil)))

(defcustom gnus-score-default-type nil
  "Default match type when entering new scores.

Should be one of the following symbols.

 s: substring
 e: exact string
 f: fuzzy string
 r: regexp string
 b: before date
 a: after date
 n: this date
 <: less than number
 >: greater than number
 =: equal to number

If nil, the user will be asked for a match type."
  :group 'gnus-score-default
  :type '(choice (const :tag "substring" s)
		 (const :tag "exact string" e)
		 (const :tag "fuzzy string" f)
		 (const :tag "regexp string" r)
		 (const :tag "before date" b)
		 (const :tag "after date" a)
		 (const :tag "this date" n)
		 (const :tag "less than number" <)
		 (const :tag "greater than number" >)
		 (const :tag "equal than number" =)
		 (const :tag "ask" nil)))

(defcustom gnus-score-default-fold nil
  "Non-nil means use case folding for new score file entries."
  :group 'gnus-score-default
  :type 'boolean)

(defcustom gnus-score-default-duration nil
  "Default duration of effect when entering new scores.

Should be one of the following symbols.

 t: temporary
 p: permanent
 i: immediate

If nil, the user will be asked for a duration."
  :group 'gnus-score-default
  :type '(choice (const :tag "temporary" t)
		 (const :tag "permanent" p)
		 (const :tag "immediate" i)
		 (const :tag "ask" nil)))

(defcustom gnus-score-after-write-file-function nil
  "Function called with the name of the score file just written to disk."
  :group 'gnus-score-files
  :type '(choice (const nil) function))

(defcustom gnus-score-thread-simplify nil
  "If non-nil, subjects will simplified as in threading."
  :group 'gnus-score-various
  :type 'boolean)

(defcustom gnus-inhibit-slow-scoring nil
  "Inhibit slow scoring, e.g. scoring on headers or body.

If a regexp, scoring on headers or body is inhibited if the group
matches the regexp.  If it is t, scoring on headers or body is
inhibited for all groups."
  :group 'gnus-score-various
  :version "23.1" ;; No Gnus
  :type '(choice (const :tag "All" nil)
		 (const :tag "None" t)
		 regexp))



;; Internal variables.

(defvar gnus-score-use-all-scores t
  "If nil, only `gnus-score-find-score-files-function' is used.")

(defvar gnus-adaptive-word-syntax-table
  (let ((table (copy-syntax-table (standard-syntax-table)))
	(numbers '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))
    (while numbers
      (modify-syntax-entry (pop numbers) " " table))
    (modify-syntax-entry ?' "w" table)
    table)
  "Syntax table used when doing adaptive word scoring.")

(defvar gnus-scores-exclude-files nil)
(defvar gnus-internal-global-score-files nil)
(defvar gnus-score-file-list nil)

(defvar gnus-short-name-score-file-cache nil)

(defvar gnus-score-help-winconf nil)
(defvar gnus-adaptive-score-alist gnus-default-adaptive-score-alist)
(defvar gnus-adaptive-word-score-alist gnus-default-adaptive-word-score-alist)
(defvar gnus-score-trace nil)
(defvar gnus-score-edit-buffer nil)

(defvar gnus-score-alist nil
  "Alist containing score information.
The keys can be symbols or strings.  The following symbols are defined.

touched: If this alist has been modified.
mark:    Automatically mark articles below this.
expunge: Automatically expunge articles below this.
files:   List of other score files to load when loading this one.
eval:    Sexp to be evaluated when the score file is loaded.

String entries have the form (HEADER (MATCH TYPE SCORE DATE) ...)
where HEADER is the header being scored, MATCH is the string we are
looking for, TYPE is a flag indicating whether it should use regexp or
substring matching, SCORE is the score to add and DATE is the date
of the last successful match.")

(defvar gnus-score-cache nil)
(defvar gnus-scores-articles nil)
(defvar gnus-score-index nil)


(defconst gnus-header-index
  ;; Name to index alist.
  '(("number" 0 gnus-score-integer)
    ("subject" 1 gnus-score-string)
    ("from" 2 gnus-score-string)
    ("date" 3 gnus-score-date)
    ("message-id" 4 gnus-score-string)
    ("references" 5 gnus-score-string)
    ("chars" 6 gnus-score-integer)
    ("lines" 7 gnus-score-integer)
    ("xref" 8 gnus-score-string)
    ("extra" 9 gnus-score-string)
    ("head" -1 gnus-score-body)
    ("body" -1 gnus-score-body)
    ("all" -1 gnus-score-body)
    ("followup" 2 gnus-score-followup)
    ("thread" 5 gnus-score-thread)))

;;; Summary mode score maps.

(gnus-define-keys (gnus-summary-score-map "V" gnus-summary-mode-map)
  "s" gnus-summary-set-score
  "S" gnus-summary-current-score
  "c" gnus-score-change-score-file
  "C" gnus-score-customize
  "m" gnus-score-set-mark-below
  "x" gnus-score-set-expunge-below
  "R" gnus-summary-rescore
  "e" gnus-score-edit-current-scores
  "f" gnus-score-edit-file
  "F" gnus-score-flush-cache
  "t" gnus-score-find-trace
  "w" gnus-score-find-favourite-words)

;; Summary score file commands

;; Much modification of the kill (ahem, score) code and lots of the
;; functions are written by Per Abrahamsen <amanda@iesd.auc.dk>.

(defun gnus-summary-lower-score (&optional score symp)
  "Make a score entry based on the current article.
The user will be prompted for header to score on, match type,
permanence, and the string to be used.  The numerical prefix will
be used as SCORE.  A symbolic prefix of `a' (the SYMP parameter)
says to use the `all.SCORE' file for the command instead of the
current score file."
  (interactive (gnus-interactive "P\ny"))
  (gnus-summary-increase-score (- (gnus-score-delta-default score)) symp))

(defun gnus-score-kill-help-buffer ()
  (when (get-buffer "*Score Help*")
    (kill-buffer "*Score Help*")
    (when gnus-score-help-winconf
      (set-window-configuration gnus-score-help-winconf))))

(defun gnus-summary-increase-score (&optional score symp)
  "Make a score entry based on the current article.
The user will be prompted for header to score on, match type,
permanence, and the string to be used.  The numerical prefix will
be used as SCORE.  A symbolic prefix of `a' (the SYMP parameter)
says to use the `all.SCORE' file for the command instead of the
current score file."
  (interactive (gnus-interactive "P\ny"))
  (let* ((nscore (gnus-score-delta-default score))
	 (prefix (if (< nscore 0) ?L ?I))
	 (increase (> nscore 0))
	 (char-to-header
	  '((?a "from" nil nil string)
	    (?s "subject" nil nil string)
	    (?b "body" "" nil body-string)
	    (?h "head" "" nil body-string)
	    (?i "message-id" nil nil string)
	    (?r "references" "message-id" nil string)
	    (?x "xref" nil nil string)
	    (?e "extra" nil nil string)
	    (?l "lines" nil nil number)
	    (?d "date" nil nil date)
	    (?f "followup" nil nil string)
	    (?t "thread" "message-id" nil string)))
	 (char-to-type
	  '((?s s "substring" string)
	    (?e e "exact string" string)
	    (?f f "fuzzy string" string)
	    (?r r "regexp string" string)
	    (?z s "substring" body-string)
	    (?p r "regexp string" body-string)
	    (?b before "before date" date)
	    (?a after "after date" date)
	    (?n at "this date" date)
	    (?< < "less than number" number)
	    (?> > "greater than number" number)
	    (?= = "equal to number" number)))
	 (current-score-file gnus-current-score-file)
	 (char-to-perm
	  (list (list ?t (current-time-string) "temporary")
		'(?p perm "permanent") '(?i now "immediate")))
	 (mimic gnus-score-mimic-keymap)
	 (hchar (and gnus-score-default-header
		     (aref (symbol-name gnus-score-default-header) 0)))
	 (tchar (and gnus-score-default-type
		     (aref (symbol-name gnus-score-default-type) 0)))
	 (pchar (and gnus-score-default-duration
		     (aref (symbol-name gnus-score-default-duration) 0)))
	 entry temporary type match extra)

    (unwind-protect
	(progn

	  ;; First we read the header to score.
	  (while (not hchar)
	    (if mimic
		(progn
		  (sit-for 1)
		  (message "%c-" prefix))
	      (message "%s header (%s?): " (if increase "Increase" "Lower")
		       (mapconcat (lambda (s) (char-to-string (car s)))
				  char-to-header "")))
	    (setq hchar (read-char))
	    (when (or (= hchar ??) (= hchar ?\C-h))
	      (setq hchar nil)
	      (gnus-score-insert-help "Match on header" char-to-header 1)))

	  (gnus-score-kill-help-buffer)
	  (unless (setq entry (assq (downcase hchar) char-to-header))
	    (if mimic (error "%c %c" prefix hchar)
	      (error "Invalid header type")))

	  (when (/= (downcase hchar) hchar)
	    ;; This was a majuscule, so we end reading and set the defaults.
	    (if mimic (message "%c %c" prefix hchar) (message ""))
	    (setq tchar (or tchar ?s)
		  pchar (or pchar ?t)))

	  (let ((legal-types
		 (delq nil
		       (mapcar (lambda (s)
				 (if (eq (nth 4 entry)
					 (nth 3 s))
				     s nil))
			       char-to-type))))
	    ;; We continue reading - the type.
	    (while (not tchar)
	      (if mimic
		  (progn
		    (sit-for 1) (message "%c %c-" prefix hchar))
		(message "%s header '%s' with match type (%s?): "
			 (if increase "Increase" "Lower")
			 (nth 1 entry)
			 (mapconcat (lambda (s) (char-to-string (car s)))
				    legal-types "")))
	      (setq tchar (read-char))
	      (when (or (= tchar ??) (= tchar ?\C-h))
		(setq tchar nil)
		(gnus-score-insert-help "Match type" legal-types 2)))

	    (gnus-score-kill-help-buffer)
	    (unless (setq type (nth 1 (assq (downcase tchar) legal-types)))
	      (if mimic (error "%c %c" prefix hchar)
		(error "Invalid match type"))))

	  (when (/= (downcase tchar) tchar)
	    ;; It was a majuscule, so we end reading and use the default.
	    (if mimic (message "%c %c %c" prefix hchar tchar)
	      (message ""))
	    (setq pchar (or pchar ?t)))

	  ;; We continue reading.
	  (while (not pchar)
	    (if mimic
		(progn
		  (sit-for 1) (message "%c %c %c-" prefix hchar tchar))
	      (message "%s permanence (%s?): " (if increase "Increase" "Lower")
		       (mapconcat (lambda (s) (char-to-string (car s)))
				  char-to-perm "")))
	    (setq pchar (read-char))
	    (when (or (= pchar ??) (= pchar ?\C-h))
	      (setq pchar nil)
	      (gnus-score-insert-help "Match permanence" char-to-perm 2)))

	  (gnus-score-kill-help-buffer)
	  (if mimic (message "%c %c %c %c" prefix hchar tchar pchar)
	    (message ""))
	  (unless (setq temporary (cadr (assq pchar char-to-perm)))
	    ;; Deal with der(r)ided superannuated paradigms.
	    (when (and (eq (1+ prefix) 77)
		       (eq (+ hchar 12) 109)
		       (eq (1- tchar) 113)
		       (eq (- pchar 4) 111))
	      (error "You rang?"))
	    (if mimic
		(error "%c %c %c %c" prefix hchar tchar pchar)
	      (error "Invalid match duration"))))
      ;; Always kill the score help buffer.
      (gnus-score-kill-help-buffer))

    ;; If scoring an extra (non-standard overview) header,
    ;; we must find out which header is in question.
    (setq extra
	  (and gnus-extra-headers
	       (equal (nth 1 entry) "extra")
	       (intern			; need symbol
                (let ((collection (mapcar 'symbol-name gnus-extra-headers)))
                  (gnus-completing-read
                   "Score extra header"  ; prompt
                   collection            ; completion list
                   t                     ; require match
                   nil                   ; no history
                   nil                   ; no initial-input
                   (car collection)))))) ; default value
    ;; extra is now nil or a symbol.

    ;; We have all the data, so we enter this score.
    (setq match (if (string= (nth 2 entry) "") ""
		  (gnus-summary-header (or (nth 2 entry) (nth 1 entry))
				       nil extra)))

    ;; Modify the match, perhaps.
    (cond
     ((equal (nth 1 entry) "xref")
      (when (string-match "^Xref: *" match)
	(setq match (substring match (match-end 0))))
      (when (string-match "^[^:]* +" match)
	(setq match (substring match (match-end 0))))))

    (when (memq type '(r R regexp Regexp))
      (setq match (regexp-quote match)))

    ;; Change score file to the "all.SCORE" file.
    (when (eq symp 'a)
      (with-current-buffer gnus-summary-buffer
	(gnus-score-load-file
	 ;; This is a kludge; yes...
	 (cond
	  ((eq gnus-score-find-score-files-function
	       'gnus-score-find-hierarchical)
	   (gnus-score-file-name ""))
	  ((eq gnus-score-find-score-files-function 'gnus-score-find-single)
	   current-score-file)
	  (t
	   (gnus-score-file-name "all"))))))

    (gnus-summary-score-entry
     (nth 1 entry)			; Header
     match				; Match
     type				; Type
     (if (eq score 's) nil score)	; Score
     (if (eq temporary 'perm)		; Temp
	 nil
       temporary)
     (not (nth 3 entry))		; Prompt
     nil				; not silent
     extra)				; non-standard overview.

    (when (eq symp 'a)
      ;; We change the score file back to the previous one.
      (with-current-buffer gnus-summary-buffer
	(gnus-score-load-file current-score-file)))))

(defun gnus-score-insert-help (string alist idx)
  (setq gnus-score-help-winconf (current-window-configuration))
  (with-current-buffer (gnus-get-buffer-create "*Score Help*")
    (buffer-disable-undo)
    (delete-windows-on (current-buffer))
    (erase-buffer)
    (insert string ":\n\n")
    (let ((max -1)
	  (list alist)
	  (i 0)
	  n width pad format)
      ;; find the longest string to display
      (while list
	(setq n (length (nth idx (car list))))
	(unless (> max n)
	  (setq max n))
	(setq list (cdr list)))
      (setq max (+ max 4))		; %c, `:', SPACE, a SPACE at end
      (setq n (/ (1- (window-width)) max)) ; items per line
      (setq width (/ (1- (window-width)) n)) ; width of each item
      ;; insert `n' items, each in a field of width `width'
      (while alist
	(if (< i n)
	    ()
	  (setq i 0)
	  (delete-char -1)		; the `\n' takes a char
	  (insert "\n"))
	(setq pad (- width 3))
	(setq format (concat "%c: %-" (int-to-string pad) "s"))
	(insert (format format (caar alist) (nth idx (car alist))))
	(setq alist (cdr alist))
	(setq i (1+ i))))
    (goto-char (point-min))
    ;; display ourselves in a small window at the bottom
    (gnus-select-lowest-window)
    (if (< (/ (window-height) 2) window-min-height)
	(switch-to-buffer "*Score Help*")
      (split-window)
      (pop-to-buffer "*Score Help*"))
    (let ((window-min-height 1))
      (shrink-window-if-larger-than-buffer))
    (select-window (gnus-get-buffer-window gnus-summary-buffer t))))

(defun gnus-summary-header (header &optional no-err extra)
  ;; Return HEADER for current articles, or error.
  (let ((article (gnus-summary-article-number))
	headers)
    (if article
	(if (and (setq headers (gnus-summary-article-header article))
		 (vectorp headers))
	    (if extra			; `header' must be "extra"
		(or (cdr (assq extra (mail-header-extra headers))) "")
	      (aref headers (nth 1 (assoc header gnus-header-index))))
	  (if no-err
	      nil
	    (error "Pseudo-articles can't be scored")))
      (if no-err
	  (error "No article on current line")
	nil))))

(defun gnus-newsgroup-score-alist ()
  (or
   (let ((param-file (gnus-group-find-parameter
		      gnus-newsgroup-name 'score-file)))
     (when param-file
       (gnus-score-load param-file)))
   (gnus-score-load
    (gnus-score-file-name gnus-newsgroup-name)))
  gnus-score-alist)

(defsubst gnus-score-get (symbol &optional alist)
  ;; Get SYMBOL's definition in ALIST.
  (cdr (assoc symbol
	      (or alist
		  gnus-score-alist
		  (gnus-newsgroup-score-alist)))))

(defun gnus-summary-score-entry (header match type score date
					&optional prompt silent extra)
  "Enter score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is the match type: substring, regexp, exact, fuzzy.
SCORE is the score to add.
DATE is the expire date, or nil for no expire, or 'now for immediate expire.
If optional argument `PROMPT' is non-nil, allow user to edit match.
If optional argument `SILENT' is nil, show effect of score entry.
If optional argument `EXTRA' is non-nil, it's a non-standard overview header."
  ;; Regexp is the default type.
  (when (eq type t)
    (setq type 'r))
  ;; Simplify matches...
  (cond ((or (eq type 'r) (eq type 's) (eq type nil))
	 (setq match (if match (gnus-simplify-subject-re match) "")))
	((eq type 'f)
	 (setq match (gnus-simplify-subject-fuzzy match))))
  (let ((score (gnus-score-delta-default score))
	(header (downcase header))
	new)
    (set-text-properties 0 (length header) nil header)
    (when prompt
      (setq match (read-string
		   (format "Match %s on %s, %s: "
			   (cond ((eq date 'now)
				  "now")
				 ((stringp date)
				  "temp")
				 (t "permanent"))
			   header
			   (if (< score 0) "lower" "raise"))
		   (if (numberp match)
		       (int-to-string match)
		     match))))

    ;; If this is an integer comparison, we transform from string to int.
    (if (eq (nth 2 (assoc header gnus-header-index)) 'gnus-score-integer)
	(if (stringp match)
	    (setq match (string-to-number match)))
      (set-text-properties 0 (length match) nil match))

    (unless (eq date 'now)
      ;; Add the score entry to the score file.
      (when (= score gnus-score-interactive-default-score)
	(setq score nil))
      (let ((old (gnus-score-get header))
	    elem)
	(setq new
	      (cond
	       (extra
		(list match score
		      (and date (if (numberp date) date
				  (date-to-day date)))
		      type (symbol-name extra)))
	       (type
		(list match score
		      (and date (if (numberp date) date
				  (date-to-day date)))
		      type))
	       (date (list match score (date-to-day date)))
	       (score (list match score))
	       (t (list match))))
	;; We see whether we can collapse some score entries.
	;; This isn't quite correct, because there may be more elements
	;; later on with the same key that have matching elems...  Hm.
	(if (and old
		 (setq elem (assoc match old))
		 (eq (nth 3 elem) (nth 3 new))
		 (or (and (numberp (nth 2 elem)) (numberp (nth 2 new)))
		     (and (not (nth 2 elem)) (not (nth 2 new)))))
	    ;; Yup, we just add this new score to the old elem.
	    (setcar (cdr elem) (+ (or (nth 1 elem)
				      gnus-score-interactive-default-score)
				  (or (nth 1 new)
				      gnus-score-interactive-default-score)))
	  ;; Nope, we have to add a new elem.
	  (gnus-score-set header (if old (cons new old) (list new)) nil t))
	(gnus-score-set 'touched '(t))))

    ;; Score the current buffer.
    (unless silent
      (if (and (>= (nth 1 (assoc header gnus-header-index)) 0)
	       (eq (nth 2 (assoc header gnus-header-index))
		   'gnus-score-string))
	  (gnus-summary-score-effect header match type score extra)
	(gnus-summary-rescore)))

    ;; Return the new scoring rule.
    new))

(defun gnus-summary-score-effect (header match type score &optional extra)
  "Simulate the effect of a score file entry.
HEADER is the header being scored.
MATCH is the string we are looking for.
TYPE is the score type.
SCORE is the score to add.
EXTRA is the possible non-standard header."
  (interactive (list (gnus-completing-read "Header"
                                           (mapcar
                                            'car
                                            (gnus-remove-if-not
                                             (lambda (x) (fboundp (nth 2 x)))
                                             gnus-header-index))
                                           t)
		     (read-string "Match: ")
		     (if (y-or-n-p "Use regexp match? ") 'r 's)
		     (string-to-number (read-string "Score: "))))
  (save-excursion
    (unless (and (stringp match) (> (length match) 0))
      (error "No match"))
    (goto-char (point-min))
    (let ((regexp (cond ((eq type 'f)
			 (gnus-simplify-subject-fuzzy match))
			((eq type 'r)
			 match)
			((eq type 'e)
			 (concat "\\`" (regexp-quote match) "\\'"))
			(t
			 (regexp-quote match)))))
      (while (not (eobp))
	(let ((content (gnus-summary-header header 'noerr extra))
	      (case-fold-search t))
	  (and content
	       (when (if (eq type 'f)
			 (string-equal (gnus-simplify-subject-fuzzy content)
				       regexp)
		       (string-match regexp content))
		 (gnus-summary-raise-score score))))
	(beginning-of-line 2))))
  (gnus-set-mode-line 'summary))

(defun gnus-summary-score-crossposting (score date)
  ;; Enter score file entry for current crossposting.
  ;; SCORE is the score to add.
  ;; DATE is the expire date.
  (let ((xref (gnus-summary-header "xref"))
	(start 0)
	group)
    (unless xref
      (error "This article is not crossposted"))
    (while (string-match " \\([^ \t]+\\):" xref start)
      (setq start (match-end 0))
      (when (not (string=
		  (setq group
			(substring xref (match-beginning 1) (match-end 1)))
		  gnus-newsgroup-name))
	(gnus-summary-score-entry
	 "xref" (concat " " group ":") nil score date t)))))


;;;
;;; Gnus Score Files
;;;

;; All score code written by Per Abrahamsen <abraham@iesd.auc.dk>.

(defun gnus-score-set-mark-below (score)
  "Automatically mark articles with score below SCORE as read."
  (interactive
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-number (read-string "Mark below: ")))))
  (setq score (or score gnus-summary-default-score 0))
  (gnus-score-set 'mark (list score))
  (gnus-score-set 'touched '(t))
  (setq gnus-summary-mark-below score)
  (gnus-score-update-lines))

(defun gnus-score-update-lines ()
  "Update all lines in the summary buffer."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (gnus-summary-update-line)
      (forward-line 1))))

(defun gnus-score-update-all-lines ()
  "Update all lines in the summary buffer, even the hidden ones."
  (save-excursion
    (goto-char (point-min))
    (let (hidden)
      (while (not (eobp))
	(when (gnus-summary-show-thread)
	  (push (point) hidden))
	(gnus-summary-update-line)
	(forward-line 1))
      ;; Re-hide the hidden threads.
      (while hidden
	(goto-char (pop hidden))
	(gnus-summary-hide-thread)))))

(defun gnus-score-set-expunge-below (score)
  "Automatically expunge articles with score below SCORE."
  (interactive
   (list (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
	     (string-to-number (read-string "Set expunge below: ")))))
  (setq score (or score gnus-summary-default-score 0))
  (gnus-score-set 'expunge (list score))
  (gnus-score-set 'touched '(t)))

(defun gnus-score-followup-article (&optional score)
  "Add SCORE to all followups to the article in the current buffer."
  (interactive "P")
  (setq score (gnus-score-delta-default score))
  (when (gnus-buffer-live-p gnus-summary-buffer)
    (save-excursion
      (save-restriction
	(message-narrow-to-headers)
	(let ((id (mail-fetch-field "message-id")))
	  (when id
	    (set-buffer gnus-summary-buffer)
	    (gnus-summary-score-entry
	     "references" (concat id "[ \t]*$") 'r
	     score (current-time-string) nil t)))))))

(defun gnus-score-followup-thread (&optional score)
  "Add SCORE to all later articles in the thread the current buffer is part of."
  (interactive "P")
  (setq score (gnus-score-delta-default score))
  (when (gnus-buffer-live-p gnus-summary-buffer)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(let ((id (mail-fetch-field "message-id")))
	  (when id
	    (set-buffer gnus-summary-buffer)
	    (gnus-summary-score-entry
	     "references" id 's
	     score (current-time-string))))))))

(defun gnus-score-set (symbol value &optional alist warn)
  ;; Set SYMBOL to VALUE in ALIST.
  (let* ((alist
	  (or alist
	      gnus-score-alist
	      (gnus-newsgroup-score-alist)))
	 (entry (assoc symbol alist)))
    (cond ((gnus-score-get 'read-only alist)
	   ;; This is a read-only score file, so we do nothing.
	   (when warn
	     (gnus-message 4 "Note: read-only score file; entry discarded")))
	  (entry
	   (setcdr entry value))
	  ((null alist)
	   (error "Empty alist"))
	  (t
	   (setcdr alist
		   (cons (cons symbol value) (cdr alist)))))))

(defun gnus-summary-raise-score (n)
  "Raise the score of the current article by N."
  (interactive "p")
  (gnus-summary-set-score (+ (gnus-summary-article-score)
			     (or n gnus-score-interactive-default-score ))))

(defun gnus-summary-set-score (n)
  "Set the score of the current article to N."
  (interactive "p")
  (save-excursion
    (gnus-summary-show-thread)
    (let ((buffer-read-only nil))
      ;; Set score.
      (gnus-summary-update-mark
       (if (= n (or gnus-summary-default-score 0)) ?  ;Whitespace
	 (if (< n (or gnus-summary-default-score 0))
	     gnus-score-below-mark gnus-score-over-mark))
       'score))
    (let* ((article (gnus-summary-article-number))
	   (score (assq article gnus-newsgroup-scored)))
      (if score (setcdr score n)
	(push (cons article n) gnus-newsgroup-scored)))
    (gnus-summary-update-line)))

(defun gnus-summary-current-score ()
  "Return the score of the current article."
  (interactive)
  (gnus-message 1 "%s" (gnus-summary-article-score)))

(defun gnus-score-change-score-file (file)
  "Change current score alist."
  (interactive
   (list (read-file-name "Change to score file: " gnus-kill-files-directory)))
  (gnus-score-load-file file)
  (gnus-set-mode-line 'summary))

(defvar gnus-score-edit-exit-function)
(defun gnus-score-edit-current-scores (file)
  "Edit the current score alist."
  (interactive (list gnus-current-score-file))
  (if (not gnus-current-score-file)
      (error "No current score file")
    (let ((winconf (current-window-configuration)))
      (when (buffer-name gnus-summary-buffer)
	(gnus-score-save))
      (gnus-make-directory (file-name-directory file))
      (setq gnus-score-edit-buffer (find-file-noselect file))
      (gnus-configure-windows 'edit-score)
      (gnus-score-mode)
      (setq gnus-score-edit-exit-function 'gnus-score-edit-done)
      (make-local-variable 'gnus-prev-winconf)
      (setq gnus-prev-winconf winconf))
    (gnus-message
     4 "%s" (substitute-command-keys
	     "\\<gnus-score-mode-map>\\[gnus-score-edit-exit] to save edits"))))

(defun gnus-score-edit-all-score ()
  "Edit the all.SCORE file."
  (interactive)
  (find-file (gnus-score-file-name "all"))
  (gnus-score-mode)
  (setq gnus-score-edit-exit-function 'gnus-score-edit-done)
  (gnus-message
   4 (substitute-command-keys
      "\\<gnus-score-mode-map>\\[gnus-score-edit-exit] to save edits")))

(defun gnus-score-edit-file (file)
  "Edit a score file."
  (interactive
   (list (read-file-name "Edit score file: " gnus-kill-files-directory)))
  (gnus-make-directory (file-name-directory file))
  (when (buffer-name gnus-summary-buffer)
    (gnus-score-save))
  (let ((winconf (current-window-configuration)))
    (setq gnus-score-edit-buffer (find-file-noselect file))
    (gnus-configure-windows 'edit-score)
    (gnus-score-mode)
    (setq gnus-score-edit-exit-function 'gnus-score-edit-done)
    (make-local-variable 'gnus-prev-winconf)
    (setq gnus-prev-winconf winconf))
  (gnus-message
   4 "%s" (substitute-command-keys
	   "\\<gnus-score-mode-map>\\[gnus-score-edit-exit] to save edits")))

(defun gnus-score-edit-file-at-point (&optional format)
  "Edit score file at point in Score Trace buffers.
If FORMAT, also format the current score file."
  (let* ((rule (save-excursion
		 (beginning-of-line)
		 (read (current-buffer))))
	 (sep "[ \n\r\t]*")
	 ;; Must be synced with `gnus-score-find-trace':
	 (reg " -> +")
	 (file (save-excursion
		 (end-of-line)
		 (if (and (re-search-backward reg (point-at-bol) t)
			  (re-search-forward  reg (point-at-eol) t))
		     (buffer-substring (point) (point-at-eol))
		   nil))))
    (if (or (not file)
	    (string-match "\\<\\(non-file rule\\|A file\\)\\>" file)
	    ;; (see `gnus-score-find-trace' and `gnus-score-advanced')
	    (string= "" file))
	(gnus-error 3 "Can't find a score file in current line.")
      (gnus-score-edit-file file)
      (when format
	(gnus-score-pretty-print))
      (when (consp rule) ;; the rule exists
	(setq rule (mapconcat #'(lambda (obj)
				  (regexp-quote (format "%S" obj)))
			      rule
			      sep))
	(goto-char (point-min))
	(re-search-forward rule nil t)
	;; make it easy to use `kill-sexp':
	(goto-char (1- (match-beginning 0)))))))

(defun gnus-score-load-file (file)
  ;; Load score file FILE.  Returns a list a retrieved score-alists.
  (let* ((file (expand-file-name
		(or (and (string-match
			  (concat "^" (regexp-quote
				       (expand-file-name
					gnus-kill-files-directory)))
			  (expand-file-name file))
			 file)
		    (expand-file-name file gnus-kill-files-directory))))
	 (cached (assoc file gnus-score-cache))
	 (global (member file gnus-internal-global-score-files))
	 lists alist)
    (if cached
	;; The score file was already loaded.
	(setq alist (cdr cached))
      ;; We load the score file.
      (setq gnus-score-alist nil)
      (setq alist (gnus-score-load-score-alist file))
      ;; We add '(touched) to the alist to signify that it hasn't been
      ;; touched (yet).
      (unless (assq 'touched alist)
	(push (list 'touched nil) alist))
      ;; If it is a global score file, we make it read-only.
      (and global
	   (not (assq 'read-only alist))
	   (push (list 'read-only t) alist))
      (push (cons file alist) gnus-score-cache))
    (let ((a alist)
	  found)
      (while a
	;; Downcase all header names.
	(cond
	 ((stringp (caar a))
	  (setcar (car a) (downcase (caar a)))
	  (setq found t))
	 ;; Advanced scoring.
	 ((consp (caar a))
	  (setq found t)))
	(pop a))
      ;; If there are actual scores in the alist, we add it to the
      ;; return value of this function.
      (when found
	(setq lists (list alist))))
    ;; Treat the other possible atoms in the score alist.
    (let ((mark (car (gnus-score-get 'mark alist)))
	  (expunge (car (gnus-score-get 'expunge alist)))
	  (mark-and-expunge (car (gnus-score-get 'mark-and-expunge alist)))
	  (files (gnus-score-get 'files alist))
	  (exclude-files (gnus-score-get 'exclude-files alist))
	  (orphan (car (gnus-score-get 'orphan alist)))
	  (adapt (gnus-score-get 'adapt alist))
	  (thread-mark-and-expunge
	   (car (gnus-score-get 'thread-mark-and-expunge alist)))
	  (adapt-file (car (gnus-score-get 'adapt-file alist)))
	  (local (gnus-score-get 'local alist))
	  (decay (car (gnus-score-get 'decay alist)))
	  (eval (car (gnus-score-get 'eval alist))))
      ;; Perform possible decays.
      (when (and (if (stringp gnus-decay-scores)
		     (string-match gnus-decay-scores file)
		   gnus-decay-scores)
		 (or cached (file-exists-p file))
		 (or (not decay)
		     (gnus-decay-scores alist decay)))
	(gnus-score-set 'touched '(t) alist)
	(gnus-score-set 'decay (list (time-to-days (current-time))) alist))
      ;; We do not respect eval and files atoms from global score
      ;; files.
      (when (and files (not global))
	(setq lists (apply 'append lists
			   (mapcar 'gnus-score-load-file
				   (if adapt-file (cons adapt-file files)
				     files)))))
      (when (and eval (not global))
	(eval eval))
      ;; We then expand any exclude-file directives.
      (setq gnus-scores-exclude-files
	    (nconc
	     (apply
	      'nconc
	      (mapcar
	       (lambda (sfile)
		 (list
		  (expand-file-name sfile (file-name-directory file))
		  (expand-file-name sfile gnus-kill-files-directory)))
	       exclude-files))
	     gnus-scores-exclude-files))
      (when local
	(with-current-buffer gnus-summary-buffer
	  (while local
	    (and (consp (car local))
		 (symbolp (caar local))
		 (progn
		   (make-local-variable (caar local))
		   (set (caar local) (nth 1 (car local)))))
	    (setq local (cdr local)))))
      (when orphan
	(setq gnus-orphan-score orphan))
      (setq gnus-adaptive-score-alist
	    (cond ((equal adapt '(t))
		   (setq gnus-newsgroup-adaptive t)
		   gnus-default-adaptive-score-alist)
		  ((equal adapt '(ignore))
		   (setq gnus-newsgroup-adaptive nil))
		  ((consp adapt)
		   (setq gnus-newsgroup-adaptive t)
		   adapt)
		  (t
		   gnus-default-adaptive-score-alist)))
      (setq gnus-thread-expunge-below
	    (or thread-mark-and-expunge gnus-thread-expunge-below))
      (setq gnus-summary-mark-below
	    (or mark mark-and-expunge gnus-summary-mark-below))
      (setq gnus-summary-expunge-below
	    (or expunge mark-and-expunge gnus-summary-expunge-below))
      (setq gnus-newsgroup-adaptive-score-file
	    (or adapt-file gnus-newsgroup-adaptive-score-file)))
    (setq gnus-current-score-file file)
    (setq gnus-score-alist alist)
    lists))

(defun gnus-score-load (file)
  ;; Load score FILE.
  (let ((cache (assoc file gnus-score-cache)))
    (if cache
	(setq gnus-score-alist (cdr cache))
      (setq gnus-score-alist nil)
      (gnus-score-load-score-alist file)
      (unless gnus-score-alist
	(setq gnus-score-alist (copy-alist '((touched nil)))))
      (push (cons file gnus-score-alist) gnus-score-cache))))

(defun gnus-score-remove-from-cache (file)
  (setq gnus-score-cache
	(delq (assoc file gnus-score-cache) gnus-score-cache)))

(defun gnus-score-load-score-alist (file)
  "Read score FILE."
  (let (alist)
    (if (not (file-readable-p file))
	;; Couldn't read file.
	(setq gnus-score-alist nil)
      ;; Read file.
      (with-temp-buffer
	(let ((coding-system-for-read score-mode-coding-system))
	  (insert-file-contents file))
	(goto-char (point-min))
	;; Only do the loading if the score file isn't empty.
	(when (save-excursion (re-search-forward "[()0-9a-zA-Z]" nil t))
	  (setq alist
		(condition-case ()
		    (read (current-buffer))
		  (error
		   (gnus-error 3.2 "Problem with score file %s" file))))))
      (cond
       ((and alist
	     (atom alist))
	;; Bogus score file.
	(error "Invalid syntax with score file %s" file))
       ((eq (car alist) 'setq)
	;; This is an old-style score file.
	(setq gnus-score-alist (gnus-score-transform-old-to-new alist)))
       (t
	(setq gnus-score-alist alist)))
      ;; Check the syntax of the score file.
      (setq gnus-score-alist
	    (gnus-score-check-syntax gnus-score-alist file)))))

(defun gnus-score-check-syntax (alist file)
  "Check the syntax of the score ALIST."
  (cond
   ((null alist)
    nil)
   ((not (consp alist))
    (gnus-message 1 "Score file is not a list: %s" file)
    (ding)
    nil)
   (t
    (let ((a alist)
	  sr err s type)
      (while (and a (not err))
	(setq
	 err
	 (cond
	  ((not (listp (car a)))
	   (format "Invalid score element %s in %s" (car a) file))
	  ((stringp (caar a))
	   (cond
	    ((not (listp (setq sr (cdar a))))
	     (format "Invalid header match %s in %s" (nth 1 (car a)) file))
	    (t
	     (setq type (caar a))
	     (while (and sr (not err))
	       (setq s (pop sr))
	       (setq
		err
		(cond
		 ((if (member (downcase type) '("lines" "chars"))
		      (not (numberp (car s)))
		    (not (stringp (car s))))
		  (format "Invalid match %s in %s" (car s) file))
		 ((and (cadr s) (not (integerp (cadr s))))
		  (format "Non-integer score %s in %s" (cadr s) file))
		 ((and (caddr s) (not (integerp (caddr s))))
		  (format "Non-integer date %s in %s" (caddr s) file))
		 ((and (cadddr s) (not (symbolp (cadddr s))))
		  (format "Non-symbol match type %s in %s" (cadddr s) file)))))
	     err)))))
	(setq a (cdr a)))
      (if err
	  (progn
	    (ding)
	    (gnus-message 3 "%s" err)
	    (sit-for 2)
	    nil)
	alist)))))

(defun gnus-score-transform-old-to-new (alist)
  (let* ((alist (nth 2 alist))
	 out entry)
    (when (eq (car alist) 'quote)
      (setq alist (nth 1 alist)))
    (while alist
      (setq entry (car alist))
      (if (stringp (car entry))
	  (let ((scor (cdr entry)))
	    (push entry out)
	    (while scor
	      (setcar scor
		      (list (caar scor) (nth 2 (car scor))
			    (and (nth 3 (car scor))
				 (date-to-day (nth 3 (car scor))))
			    (if (nth 1 (car scor)) 'r 's)))
	      (setq scor (cdr scor))))
	(push (if (not (listp (cdr entry)))
		  (list (car entry) (cdr entry))
		entry)
	      out))
      (setq alist (cdr alist)))
    (cons (list 'touched t) (nreverse out))))

(defun gnus-score-save ()
  ;; Save all score information.
  (let ((cache gnus-score-cache)
	entry score file)
    (save-excursion
      (setq gnus-score-alist nil)
      (nnheader-set-temp-buffer " *Gnus Scores*")
      (while cache
	(current-buffer)
	(setq entry (pop cache)
	      file (nnheader-translate-file-chars (car entry) t)
	      score (cdr entry))
	(if (or (not (equal (gnus-score-get 'touched score) '(t)))
		(gnus-score-get 'read-only score)
		(and (file-exists-p file)
		     (not (file-writable-p file))))
	    ()
	  (setq score (setcdr entry (gnus-delete-alist 'touched score)))
	  (erase-buffer)
	  (let (emacs-lisp-mode-hook)
	    (if (and (not gnus-adaptive-pretty-print)
		     (string-match
		      (concat (regexp-quote gnus-adaptive-file-suffix) "$")
		      file))
		;; This is an adaptive score file, so we do not run it through
		;; `pp' unless requested.  These files can get huge, and are
		;; not meant to be edited by human hands.
		(gnus-prin1 score)
	      ;; This is a normal score file, so we print it very
	      ;; prettily.
	      (let ((lisp-mode-syntax-table score-mode-syntax-table))
		(gnus-pp score))))
	  (gnus-make-directory (file-name-directory file))
	  ;; If the score file is empty, we delete it.
	  (if (zerop (buffer-size))
	      (delete-file file)
	    ;; There are scores, so we write the file.
	    (when (file-writable-p file)
	      (let ((coding-system-for-write score-mode-coding-system))
		(gnus-write-buffer file))
	      (when gnus-score-after-write-file-function
		(funcall gnus-score-after-write-file-function file)))))
	(and gnus-score-uncacheable-files
	     (string-match gnus-score-uncacheable-files file)
	     (gnus-score-remove-from-cache file)))
      (kill-buffer (current-buffer)))))

(defun gnus-score-load-files (score-files)
  "Load all score files in SCORE-FILES."
  ;; Load the score files.
  (let (scores)
    (while score-files
      (if (stringp (car score-files))
	  ;; It is a string, which means that it's a score file name,
	  ;; so we load the score file and add the score alist to
	  ;; the list of alists.
	  (setq scores (nconc (gnus-score-load-file (car score-files)) scores))
	;; It is an alist, so we just add it to the list directly.
	(setq scores (nconc (car score-files) scores)))
      (setq score-files (cdr score-files)))
    ;; Prune the score files that are to be excluded, if any.
    (when gnus-scores-exclude-files
      (let ((s scores)
	    c)
	(while s
	  (and (setq c (rassq (car s) gnus-score-cache))
	       (member (car c) gnus-scores-exclude-files)
	       (setq scores (delq (car s) scores)))
	  (setq s (cdr s)))))
    scores))

(defun gnus-score-headers (score-files &optional trace)
  ;; Score `gnus-newsgroup-headers'.
  (let (scores news)
    ;; PLM: probably this is not the best place to clear orphan-score
    (setq gnus-orphan-score nil
	  gnus-scores-articles nil
	  gnus-scores-exclude-files nil
	  scores (gnus-score-load-files score-files))
    (setq news scores)
    ;; Do the scoring.
    (while news
      (setq scores news
	    news nil)
      (when (and gnus-summary-default-score
		 scores)
	(let* ((entries gnus-header-index)
	       (now (date-to-day (current-time-string)))
	       (expire (and gnus-score-expiry-days
			    (- now gnus-score-expiry-days)))
	       (headers gnus-newsgroup-headers)
	       (current-score-file gnus-current-score-file)
	       entry header new)
	  (gnus-message 7 "Scoring...")
	  ;; Create articles, an alist of the form `(HEADER . SCORE)'.
	  (while (setq header (pop headers))
	    ;; WARNING: The assq makes the function O(N*S) while it could
	    ;; be written as O(N+S), where N is (length gnus-newsgroup-headers)
	    ;; and S is (length gnus-newsgroup-scored).
	    (unless (assq (mail-header-number header) gnus-newsgroup-scored)
	      (setq gnus-scores-articles ;Total of 2 * N cons-cells used.
		    (cons (cons header (or gnus-summary-default-score 0))
			  gnus-scores-articles))))

	  (with-current-buffer (gnus-get-buffer-create "*Headers*")
	    (buffer-disable-undo)
	    (when (gnus-buffer-live-p gnus-summary-buffer)
	      (message-clone-locals gnus-summary-buffer))

	    ;; Set the global variant of this variable.
	    (setq gnus-current-score-file current-score-file)
	    ;; score orphans
	    (when gnus-orphan-score
	      (setq gnus-score-index
		    (nth 1 (assoc "references" gnus-header-index)))
	      (gnus-score-orphans gnus-orphan-score))
	    ;; Run each header through the score process.
	    (while entries
	      (setq entry (pop entries)
		    header (nth 0 entry)
		    gnus-score-index (nth 1 (assoc header gnus-header-index)))
	      (when (< 0 (apply 'max (mapcar
				      (lambda (score)
					(length (gnus-score-get header score)))
				      scores)))
		(when (if (and gnus-inhibit-slow-scoring
			       (or (eq gnus-inhibit-slow-scoring t)
				   (and (stringp gnus-inhibit-slow-scoring)
					;; Always true here?
					;; (stringp gnus-newsgroup-name)
					(string-match
					 gnus-inhibit-slow-scoring
					 gnus-newsgroup-name)))
			       (> 0 (nth 1 (assoc header gnus-header-index))))
			  (progn
			    (gnus-message
			     7 "Scoring on headers or body skipped.")
			    nil)
			;; Call the scoring function for this type of "header".
			(setq new (funcall (nth 2 entry) scores header
					   now expire trace)))
		  (push new news))))
	    (when (gnus-buffer-live-p gnus-summary-buffer)
	      (let ((scored gnus-newsgroup-scored))
		(with-current-buffer gnus-summary-buffer
		  (setq gnus-newsgroup-scored scored))))
	    ;; Remove the buffer.
	    (gnus-kill-buffer (current-buffer)))

	  ;; Add articles to `gnus-newsgroup-scored'.
	  (while gnus-scores-articles
	    (when (or (/= gnus-summary-default-score
			  (cdar gnus-scores-articles))
		      gnus-save-score)
	      (push (cons (mail-header-number (caar gnus-scores-articles))
			  (cdar gnus-scores-articles))
		    gnus-newsgroup-scored))
	    (setq gnus-scores-articles (cdr gnus-scores-articles)))

	  (let (score)
	    (while (setq score (pop scores))
	      (while score
		(when (consp (caar score))
		  (gnus-score-advanced (car score) trace))
		(pop score))))

	  (gnus-message 7 "Scoring...done"))))))

(defun gnus-score-lower-thread (thread score-adjust)
  "Lower the score on THREAD with SCORE-ADJUST.
THREAD is expected to contain a list of the form `(PARENT [CHILD1
CHILD2 ...])' where PARENT is a header array and each CHILD is a list
of the same form as THREAD.  The empty list nil is valid.  For each
article in the tree, the score of the corresponding entry in
`gnus-newsgroup-scored' is adjusted by SCORE-ADJUST."
  (while thread
    (let ((head (car thread)))
      (if (listp head)
	  ;; handle a child and its descendants
	  (gnus-score-lower-thread head score-adjust)
	;; handle the parent
	(let* ((article (mail-header-number head))
	       (score (assq article gnus-newsgroup-scored)))
	  (if score (setcdr score (+ (cdr score) score-adjust))
	    (push (cons article score-adjust) gnus-newsgroup-scored)))))
    (setq thread (cdr thread))))

(defun gnus-score-orphans (score)
  "Score orphans.
A root is an article with no references.  An orphan is an article
which has references, but is not connected via its references to a
root article.  This function finds all the orphans, and adjusts their
score in `gnus-newsgroup-scored' by SCORE."
  ;; gnus-make-threads produces a list, where each entry is a "thread"
  ;; as described in the gnus-score-lower-thread docs.  This function
  ;; will be called again (after limiting has been done) if the display
  ;; is threaded.  It would be nice to somehow save this info and use
  ;; it later.
  (dolist (thread (gnus-make-threads))
    (let ((id (aref (car thread) gnus-score-index)))
      ;; If the parent of the thread is not a root, lower the score of
      ;; it and its descendants.  Note that some roots seem to satisfy
      ;; (eq id nil) and some (eq id "");  not sure why.
      (when (and id
		 (not (string= id "")))
	(gnus-score-lower-thread thread score)))))

(defun gnus-score-integer (scores header now expire &optional trace)
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	entries alist)
    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))
	       (kill (car rest))
	       (match (nth 0 kill))
	       (type (or (nth 3 kill) '>))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (match-func (if (or (eq type '>) (eq type '<) (eq type '<=)
				   (eq type '>=) (eq type '=))
			       type
			     (error "Invalid match type: %s" type)))
	       (articles gnus-scores-articles))
	  ;; Instead of doing all the clever stuff that
	  ;; `gnus-score-string' does to minimize searches and stuff,
	  ;; I will assume that people generally will put so few
	  ;; matches on numbers that any cleverness will take more
	  ;; time than one would gain.
	  (while articles
	    (when (funcall match-func
			   (or (aref (caar articles) gnus-score-index) 0)
			   match)
	      (when trace
		(push (cons (car-safe (rassq alist gnus-score-cache)) kill)
		      gnus-score-trace))
	      (setq found t)
	      (setcdr (car articles) (+ score (cdar articles))))
	    (setq articles (cdr articles)))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		((and found gnus-update-score-entry-dates) ;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((and expire (< date expire)) ;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest)))))
  nil)

(defun gnus-score-date (scores header now expire &optional trace)
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	entries alist match match-func article)
    ;; Find matches.
    (while scores
      (setq alist (car scores)
	    scores (cdr scores)
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((rest (cdr entries))
	       (kill (car rest))
	       (type (or (nth 3 kill) 'before))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (found nil)
	       (articles gnus-scores-articles)
	       l)
	  (cond
	   ((eq type 'after)
	    (setq match-func 'string<
		  match (gnus-date-iso8601 (nth 0 kill))))
	   ((eq type 'before)
	    (setq match-func 'gnus-string>
		  match (gnus-date-iso8601 (nth 0 kill))))
	   ((eq type 'at)
	    (setq match-func 'string=
		  match (gnus-date-iso8601 (nth 0 kill))))
	   ((eq type 'regexp)
	    (setq match-func 'string-match
		  match (nth 0 kill)))
	   (t (error "Invalid match type: %s" type)))
	  ;; Instead of doing all the clever stuff that
	  ;; `gnus-score-string' does to minimize searches and stuff,
	  ;; I will assume that people generally will put so few
	  ;; matches on numbers that any cleverness will take more
	  ;; time than one would gain.
	  (while (setq article (pop articles))
	    (when (and
		   (setq l (aref (car article) gnus-score-index))
		   (funcall match-func match (gnus-date-iso8601 l)))
	      (when trace
		(push (cons (car-safe (rassq alist gnus-score-cache)) kill)
		      gnus-score-trace))
	      (setq found t)
	      (setcdr article (+ score (cdr article)))))
	  ;; Update expire date
	  (cond ((null date))		;Permanent entry.
		((and found gnus-update-score-entry-dates) ;Match, update date.
		 (gnus-score-set 'touched '(t) alist)
		 (setcar (nthcdr 2 kill) now))
		((and expire (< date expire)) ;Old entry, remove.
		 (gnus-score-set 'touched '(t) alist)
		 (setcdr entries (cdr rest))
		 (setq rest entries)))
	  (setq entries rest)))))
  nil)

(defun gnus-score-body (scores header now expire &optional trace)
  (if gnus-agent-fetching
      nil
    (save-excursion
      (setq gnus-scores-articles
	    (sort gnus-scores-articles
		  (lambda (a1 a2)
		    (< (mail-header-number (car a1))
		       (mail-header-number (car a2))))))
      (set-buffer nntp-server-buffer)
      (save-restriction
	(let* ((buffer-read-only nil)
	       (articles gnus-scores-articles)
	       (all-scores scores)
	       (request-func (cond ((string= "head" header)
				    'gnus-request-head)
				   ((string= "body" header)
				    'gnus-request-body)
				   (t 'gnus-request-article)))
	       entries alist ofunc article last)
	  (when articles
	    (setq last (mail-header-number (caar (last articles))))
	  ;; Not all backends support partial fetching.  In that case,
	    ;; we just fetch the entire article.
	    (unless (gnus-check-backend-function
		     (and (string-match "^gnus-" (symbol-name request-func))
			  (intern (substring (symbol-name request-func)
					     (match-end 0))))
		     gnus-newsgroup-name)
	      (setq ofunc request-func)
	      (setq request-func 'gnus-request-article))
	    (while articles
	      (setq article (mail-header-number (caar articles)))
	      (gnus-message 7 "Scoring article %s of %s..." article last)
	      (widen)
	      (when (funcall request-func article gnus-newsgroup-name)
		(goto-char (point-min))
	    ;; If just parts of the article is to be searched, but the
	    ;; backend didn't support partial fetching, we just narrow
		;; to the relevant parts.
		(when ofunc
		  (if (eq ofunc 'gnus-request-head)
		      (narrow-to-region
		       (point)
		       (or (search-forward "\n\n" nil t) (point-max)))
		    (narrow-to-region
		     (or (search-forward "\n\n" nil t) (point))
		     (point-max))))
		(setq scores all-scores)
		;; Find matches.
		(while scores
		  (setq alist (pop scores)
			entries (assoc header alist))
		  (while (cdr entries) ;First entry is the header index.
		    (let* ((rest (cdr entries))
			   (kill (car rest))
			   (match (nth 0 kill))
			   (type (or (nth 3 kill) 's))
			   (score (or (nth 1 kill)
				      gnus-score-interactive-default-score))
			   (date (nth 2 kill))
			   (found nil)
			   (case-fold-search
			    (not (or (eq type 'R) (eq type 'S)
				     (eq type 'Regexp) (eq type 'String))))
			   (search-func
			    (cond ((or (eq type 'r) (eq type 'R)
				       (eq type 'regexp) (eq type 'Regexp))
				   're-search-forward)
				  ((or (eq type 's) (eq type 'S)
				       (eq type 'string) (eq type 'String))
				   'search-forward)
				  (t
				   (error "Invalid match type: %s" type)))))
		      (goto-char (point-min))
		      (when (funcall search-func match nil t)
			;; Found a match, update scores.
			(setcdr (car articles) (+ score (cdar articles)))
			(setq found t)
			(when trace
			  (push
			   (cons (car-safe (rassq alist gnus-score-cache))
				 kill)
			   gnus-score-trace)))
		      ;; Update expire date
		      (unless trace
			(cond
			 ((null date))	;Permanent entry.
			 ((and found gnus-update-score-entry-dates)
			  ;; Match, update date.
			  (gnus-score-set 'touched '(t) alist)
			  (setcar (nthcdr 2 kill) now))
			 ((and expire (< date expire)) ;Old entry, remove.
			  (gnus-score-set 'touched '(t) alist)
			  (setcdr entries (cdr rest))
			  (setq rest entries))))
		      (setq entries rest)))))
	      (setq articles (cdr articles)))))))
    nil))

(defun gnus-score-thread (scores header now expire &optional trace)
  (gnus-score-followup scores header now expire trace t))

(defun gnus-score-followup (scores header now expire &optional trace thread)
  (if gnus-agent-fetching
      ;; FIXME: It seems doable in fetching mode.
      nil
    ;; Insert the unique article headers in the buffer.
    (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	  (current-score-file gnus-current-score-file)
	  (all-scores scores)
	  ;; gnus-score-index is used as a free variable.
	  alike last this art entries alist articles
	  new news)

      ;; Change score file to the adaptive score file.  All entries that
      ;; this function makes will be put into this file.
      (with-current-buffer gnus-summary-buffer
	(gnus-score-load-file
	 (or gnus-newsgroup-adaptive-score-file
	     (gnus-score-file-name
	      gnus-newsgroup-name gnus-adaptive-file-suffix))))

      (setq gnus-scores-articles (sort gnus-scores-articles
				       'gnus-score-string<)
	    articles gnus-scores-articles)

      (erase-buffer)
      (while articles
	(setq art (car articles)
	      this (aref (car art) gnus-score-index)
	      articles (cdr articles))
	(if (equal last this)
	    (push art alike)
	  (when last
	    (insert last ?\n)
	    (put-text-property (1- (point)) (point) 'articles alike))
	  (setq alike (list art)
		last this)))
      (when last			; Bwadr, duplicate code.
	(insert last ?\n)
	(put-text-property (1- (point)) (point) 'articles alike))

      ;; Find matches.
      (while scores
	(setq alist (car scores)
	      scores (cdr scores)
	      entries (assoc header alist))
	(while (cdr entries)		;First entry is the header index.
	  (let* ((rest (cdr entries))
		 (kill (car rest))
		 (match (nth 0 kill))
		 (type (or (nth 3 kill) 's))
		 (score (or (nth 1 kill) gnus-score-interactive-default-score))
		 (date (nth 2 kill))
		 (found nil)
		 (mt (aref (symbol-name type) 0))
		 (case-fold-search
		  (not (or (= mt ?R) (= mt ?S) (= mt ?E) (= mt ?F))))
		 (dmt (downcase mt))
		 (search-func
		  (cond ((= dmt ?r) 're-search-forward)
			((or (= dmt ?e) (= dmt ?s) (= dmt ?f)) 'search-forward)
			(t (error "Invalid match type: %s" type))))
		 arts art)
	    (goto-char (point-min))
	    (if (= dmt ?e)
		(while (funcall search-func match nil t)
		  (and (= (point-at-bol)
			  (match-beginning 0))
		       (= (progn (end-of-line) (point))
			  (match-end 0))
		       (progn
			 (setq found (setq arts (get-text-property
						 (point) 'articles)))
			 ;; Found a match, update scores.
			 (while arts
			   (setq art (car arts)
				 arts (cdr arts))
			   (gnus-score-add-followups
			    (car art) score all-scores thread))))
		  (end-of-line))
	      (while (funcall search-func match nil t)
		(end-of-line)
		(setq found (setq arts (get-text-property (point) 'articles)))
		;; Found a match, update scores.
		(while (setq art (pop arts))
		  (setcdr art (+ score (cdr art)))
		  (when trace
		    (push (cons
			   (car-safe (rassq alist gnus-score-cache))
			   kill)
			  gnus-score-trace))
		  (when (setq new (gnus-score-add-followups
				   (car art) score all-scores thread))
		    (push new news)))))
	    ;; Update expire date
	    (cond ((null date))		;Permanent entry.
		  ((and found gnus-update-score-entry-dates)
					;Match, update date.
		   (gnus-score-set 'touched '(t) alist)
		   (setcar (nthcdr 2 kill) now))
		  ((and expire (< date expire))	;Old entry, remove.
		   (gnus-score-set 'touched '(t) alist)
		   (setcdr entries (cdr rest))
		   (setq rest entries)))
	    (setq entries rest))))
      ;; We change the score file back to the previous one.
      (with-current-buffer gnus-summary-buffer
	(gnus-score-load-file current-score-file))
      (list (cons "references" news)))))

(defun gnus-score-add-followups (header score scores &optional thread)
  "Add a score entry to the adapt file."
  (with-current-buffer gnus-summary-buffer
    (let* ((id (mail-header-id header))
	   (scores (car scores))
	   entry dont)
      ;; Don't enter a score if there already is one.
      (while (setq entry (pop scores))
	(and (equal "references" (car entry))
	     (or (null (nth 3 (cadr entry)))
		 (eq 's (nth 3 (cadr entry))))
	     (assoc id entry)
	     (setq dont t)))
      (unless dont
	(gnus-summary-score-entry
	 (if thread "thread" "references")
	 id 's score (current-time-string) nil t)))))

(defun gnus-score-string (score-list header now expire &optional trace)
  ;; Score ARTICLES according to HEADER in SCORE-LIST.
  ;; Update matching entries to NOW and remove unmatched entries older
  ;; than EXPIRE.

  ;; Insert the unique article headers in the buffer.
  (let ((gnus-score-index (nth 1 (assoc header gnus-header-index)))
	;; gnus-score-index is used as a free variable.
	(simplify (and gnus-score-thread-simplify
		       (string= "subject" header)))
	alike last this art entries alist articles
	fuzzies arts words kill)

    ;; Sorting the articles costs os O(N*log N) but will allow us to
    ;; only match with each unique header.  Thus the actual matching
    ;; will be O(M*U) where M is the number of strings to match with,
    ;; and U is the number of unique headers.  It is assumed (but
    ;; untested) this will be a net win because of the large constant
    ;; factor involved with string matching.
    (setq gnus-scores-articles
	  ;; We cannot string-sort the extra headers list.  *sigh*
	  (if (= gnus-score-index 9)
	      gnus-scores-articles
	    (sort gnus-scores-articles 'gnus-score-string<))
	  articles gnus-scores-articles)

    (erase-buffer)
    (while (setq art (pop articles))
      (setq this (aref (car art) gnus-score-index))

      ;; If we're working with non-standard headers, we are stuck
      ;; with working on them as a group.  What a hassle.
      ;; Just wait 'til you see what horrors we commit against `match'...
      (if (= gnus-score-index 9)
	  (setq this (gnus-prin1-to-string this))) ; ick.

      (if simplify
	  (setq this (gnus-map-function gnus-simplify-subject-functions this)))
      (if (equal last this)
	  ;; O(N*H) cons-cells used here, where H is the number of
	  ;; headers.
	  (push art alike)
	(when last
	  ;; Insert the line, with a text property on the
	  ;; terminating newline referring to the articles with
	  ;; this line.
	  (insert last ?\n)
	  (put-text-property (1- (point)) (point) 'articles alike))
	(setq alike (list art)
	      last this)))
    (when last				; Bwadr, duplicate code.
      (insert last ?\n)
      (put-text-property (1- (point)) (point) 'articles alike))

    ;; Go through all the score alists and pick out the entries
    ;; for this header.
    (while score-list
      (setq alist (pop score-list)
	    ;; There's only one instance of this header for
	    ;; each score alist.
	    entries (assoc header alist))
      (while (cdr entries)		;First entry is the header index.
	(let* ((kill (cadr entries))
	       (type (or (nth 3 kill) 's))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (extra (nth 4 kill))	; non-standard header; string.
	       (found nil)
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search (not (memq mt '(?R ?S ?E ?F))))
	       (dmt (downcase mt))
	       ;; Assume user already simplified regexp and fuzzies
	       (match (if (and simplify (not (memq dmt '(?f ?r))))
			  (gnus-map-function
			   gnus-simplify-subject-functions
			   (nth 0 kill))
			(nth 0 kill)))
	       (search-func
		(cond ((= dmt ?r) 're-search-forward)
		      ((or (= dmt ?e) (= dmt ?s) (= dmt ?f)) 'search-forward)
		      ((= dmt ?w) nil)
		      (t (error "Invalid match type: %s" type)))))

	  ;; Evil hackery to make match usable in non-standard headers.
	  (when extra
	    (setq match (concat "[ (](" extra " \\. \"\\([^\"]*\\\\\"\\)*[^\"]*"
				(if (eq search-func 're-search-forward)
				    match
				  (regexp-quote match))
				"\\([^\"]*\\\\\"\\)*[^\"]*\")[ )]")
		  search-func 're-search-forward)) ; XXX danger?!?

	  (cond
	   ;; Fuzzy matches.  We save these for later.
	   ((= dmt ?f)
	    (push (cons entries alist) fuzzies)
	    (setq entries (cdr entries)))
	   ;; Word matches.  Save these for even later.
	   ((= dmt ?w)
	    (push (cons entries alist) words)
	    (setq entries (cdr entries)))
	   ;; Exact matches.
	   ((= dmt ?e)
	    ;; Do exact matching.
	    (goto-char (point-min))
	    (while (and (not (eobp))
			(funcall search-func match nil t))
	      ;; Is it really exact?
	      (and (eolp)
		   (= (point-at-bol) (match-beginning 0))
		   ;; Yup.
		   (progn
		     (setq found (setq arts (get-text-property
					     (point) 'articles)))
		     ;; Found a match, update scores.
		     (if trace
			 (while (setq art (pop arts))
			   (setcdr art (+ score (cdr art)))
			   (push
			    (cons
			     (car-safe (rassq alist gnus-score-cache))
			     kill)
			    gnus-score-trace))
		       (while (setq art (pop arts))
			 (setcdr art (+ score (cdr art)))))))
	      (forward-line 1))
	    ;; Update expiry date
	    (if trace
		(setq entries (cdr entries))
	      (cond
	       ;; Permanent entry.
	       ((null date)
		(setq entries (cdr entries)))
	       ;; We have a match, so we update the date.
	       ((and found gnus-update-score-entry-dates)
		(gnus-score-set 'touched '(t) alist)
		(setcar (nthcdr 2 kill) now)
		(setq entries (cdr entries)))
	       ;; This entry has expired, so we remove it.
	       ((and expire (< date expire))
		(gnus-score-set 'touched '(t) alist)
		(setcdr entries (cddr entries)))
	       ;; No match; go to next entry.
	       (t
		(setq entries (cdr entries))))))
	   ;; Regexp and substring matching.
	   (t
	    (goto-char (point-min))
	    (when (string= match "")
	      (setq match "\n"))
	    (while (and (not (eobp))
			(funcall search-func match nil t))
	      (goto-char (match-beginning 0))
	      (end-of-line)
	      (setq found (setq arts (get-text-property (point) 'articles)))
	      ;; Found a match, update scores.
	      (if trace
		  (while (setq art (pop arts))
		    (setcdr art (+ score (cdr art)))
		    (push (cons (car-safe (rassq alist gnus-score-cache)) kill)
			  gnus-score-trace))
		(while (setq art (pop arts))
		  (setcdr art (+ score (cdr art)))))
	      (forward-line 1))
	    ;; Update expiry date
	    (if trace
		(setq entries (cdr entries))
	      (cond
	       ;; Permanent entry.
	       ((null date)
		(setq entries (cdr entries)))
	       ;; We have a match, so we update the date.
	       ((and found gnus-update-score-entry-dates)
		(gnus-score-set 'touched '(t) alist)
		(setcar (nthcdr 2 kill) now)
		(setq entries (cdr entries)))
	       ;; This entry has expired, so we remove it.
	       ((and expire (< date expire))
		(gnus-score-set 'touched '(t) alist)
		(setcdr entries (cddr entries)))
	       ;; No match; go to next entry.
	       (t
		(setq entries (cdr entries))))))))))

    ;; Find fuzzy matches.
    (when fuzzies
      ;; Simplify the entire buffer for easy matching.
      (gnus-simplify-buffer-fuzzy gnus-simplify-subject-fuzzy-regexp)
      (while (setq kill (cadaar fuzzies))
	(let* ((match (nth 0 kill))
	       (type (nth 3 kill))
	       (score (or (nth 1 kill) gnus-score-interactive-default-score))
	       (date (nth 2 kill))
	       (mt (aref (symbol-name type) 0))
	       (case-fold-search (not (= mt ?F)))
	       found)
	  (goto-char (point-min))
	  (while (and (not (eobp))
		      (search-forward match nil t))
	    (when (and (= (point-at-bol) (match-beginning 0))
		       (eolp))
	      (setq found (setq arts (get-text-property (point) 'articles)))
	      (if trace
		  (while (setq art (pop arts))
		    (setcdr art (+ score (cdr art)))
		    (push (cons
			   (car-safe (rassq (cdar fuzzies) gnus-score-cache))
			   kill)
			  gnus-score-trace))
		;; Found a match, update scores.
		(while (setq art (pop arts))
		  (setcdr art (+ score (cdr art))))))
	    (forward-line 1))
	  ;; Update expiry date
	  (if (not trace)
	      (cond
	       ;; Permanent.
	       ((null date)
		;; Do nothing.
		)
	       ;; Match, update date.
	       ((and found gnus-update-score-entry-dates)
		(gnus-score-set 'touched '(t) (cdar fuzzies))
		(setcar (nthcdr 2 kill) now))
	       ;; Old entry, remove.
	       ((and expire (< date expire))
		(gnus-score-set 'touched '(t) (cdar fuzzies))
		(setcdr (caar fuzzies) (cddaar fuzzies)))))
	  (setq fuzzies (cdr fuzzies)))))

    (when words
      ;; Enter all words into the hashtb.
      (let ((hashtb (gnus-make-hashtable
		     (* 10 (count-lines (point-min) (point-max))))))
	(gnus-enter-score-words-into-hashtb hashtb)
	(while (setq kill (cadaar words))
	  (let* ((score (or (nth 1 kill) gnus-score-interactive-default-score))
		 (date (nth 2 kill))
		 found)
	    (when (setq arts (intern-soft (nth 0 kill) hashtb))
	      (setq arts (symbol-value arts))
	      (setq found t)
	      (if trace
		  (while (setq art (pop arts))
		    (setcdr art (+ score (cdr art)))
		    (push (cons
			   (car-safe (rassq (cdar words) gnus-score-cache))
			   kill)
			  gnus-score-trace))
		;; Found a match, update scores.
		(while (setq art (pop arts))
		  (setcdr art (+ score (cdr art))))))
	    ;; Update expiry date
	    (if (not trace)
		(cond
		 ;; Permanent.
		 ((null date)
		  ;; Do nothing.
		  )
		 ;; Match, update date.
		 ((and found gnus-update-score-entry-dates)
		  (gnus-score-set 'touched '(t) (cdar words))
		  (setcar (nthcdr 2 kill) now))
		 ;; Old entry, remove.
		 ((and expire (< date expire))
		  (gnus-score-set 'touched '(t) (cdar words))
		  (setcdr (caar words) (cddaar words)))))
	    (setq words (cdr words))))))
    nil))

(defun gnus-enter-score-words-into-hashtb (hashtb)
  ;; Find all the words in the buffer and enter them into
  ;; the hashtable.
  (let (word val)
    (goto-char (point-min))
    (with-syntax-table gnus-adaptive-word-syntax-table
      (while (re-search-forward "\\b\\w+\\b" nil t)
	(setq val
	      (gnus-gethash
	       (setq word (downcase (buffer-substring
				     (match-beginning 0) (match-end 0))))
	       hashtb))
	(gnus-sethash
	 word
	 (append (get-text-property (point-at-eol) 'articles) val)
	 hashtb)))
    ;; Make all the ignorable words ignored.
    (let ((ignored (append gnus-ignored-adaptive-words
			   (if gnus-adaptive-word-no-group-words
			       (message-tokenize-header
				(gnus-group-real-name gnus-newsgroup-name)
				"."))
			   gnus-default-ignored-adaptive-words)))
      (while ignored
	(gnus-sethash (pop ignored) nil hashtb)))))

(defun gnus-score-string< (a1 a2)
  ;; Compare headers in articles A2 and A2.
  ;; The header index used is the free variable `gnus-score-index'.
  (string-lessp (aref (car a1) gnus-score-index)
		(aref (car a2) gnus-score-index)))

(defun gnus-current-score-file-nondirectory (&optional score-file)
  (let ((score-file (or score-file gnus-current-score-file)))
    (if score-file
	(gnus-short-group-name (file-name-nondirectory score-file))
      "none")))

(defun gnus-score-adaptive ()
  "Create adaptive score rules for this newsgroup."
  (when gnus-newsgroup-adaptive
    ;; We change the score file to the adaptive score file.
    (with-current-buffer gnus-summary-buffer
      (gnus-score-load-file
       (or gnus-newsgroup-adaptive-score-file
	   (gnus-home-score-file gnus-newsgroup-name t)
	   (gnus-score-file-name
	    gnus-newsgroup-name gnus-adaptive-file-suffix))))
    ;; Perform ordinary line scoring.
    (when (or (not (listp gnus-newsgroup-adaptive))
	      (memq 'line gnus-newsgroup-adaptive))
      (save-excursion
	(let* ((malist (gnus-copy-sequence gnus-adaptive-score-alist))
	       (alist malist)
	       (date (current-time-string))
	       (data gnus-newsgroup-data)
	       elem headers match func)
	  ;; First we transform the adaptive rule alist into something
	  ;; that's faster to process.
	  (while malist
	    (setq elem (car malist))
	    (when (symbolp (car elem))
	      (setcar elem (symbol-value (car elem))))
	    (setq elem (cdr elem))
	    (while elem
	      (when (fboundp
		     (setq func
			   (intern
			    (concat "mail-header-"
				    (if (eq (caar elem) 'followup)
					"message-id"
				      (downcase (symbol-name (caar elem))))))))
		(setcdr (car elem)
			(cons (if (eq (caar elem) 'followup)
				  "references"
				(symbol-name (caar elem)))
			      (cdar elem)))
		(setcar (car elem)
			`(lambda (h)
			   (,func h))))
	      (setq elem (cdr elem)))
	    (setq malist (cdr malist)))
	  ;; Then we score away.
	  (while data
	    (setq elem (cdr (assq (gnus-data-mark (car data)) alist)))
	    (if (or (not elem)
		    (gnus-data-pseudo-p (car data)))
		()
	      (when (setq headers (gnus-data-header (car data)))
		(while elem
		  (setq match (funcall (caar elem) headers))
		  (gnus-summary-score-entry
		   (nth 1 (car elem)) match
		   (cond
		    ((numberp match)
		     '=)
		    ((equal (nth 1 (car elem)) "date")
		     'a)
		    (t
		     ;; Whether we use substring or exact matches is
		     ;; controlled here.
		     (if (or (not gnus-score-exact-adapt-limit)
			     (< (length match) gnus-score-exact-adapt-limit))
			 'e
		       (if (equal (nth 1 (car elem)) "subject")
			   'f 's))))
		   (nth 2 (car elem)) date nil t)
		  (setq elem (cdr elem)))))
	    (setq data (cdr data))))))

    ;; Perform adaptive word scoring.
    (when (and (listp gnus-newsgroup-adaptive)
	       (memq 'word gnus-newsgroup-adaptive))
      (with-temp-buffer
	(let* ((hashtb (gnus-make-hashtable 1000))
	       (date (date-to-day (current-time-string)))
	       (data gnus-newsgroup-data)
	       word d score val)
	  (with-syntax-table gnus-adaptive-word-syntax-table
	    ;; Go through all articles.
	    (while (setq d (pop data))
	      (when (and
		     (not (gnus-data-pseudo-p d))
		     (setq score
			   (cdr (assq
				 (gnus-data-mark d)
				 gnus-adaptive-word-score-alist))))
		;; This article has a mark that should lead to
		;; adaptive word rules, so we insert the subject
		;; and find all words in that string.
		(insert (mail-header-subject (gnus-data-header d)))
		(downcase-region (point-min) (point-max))
		(goto-char (point-min))
		(while (re-search-forward "\\b\\w+\\b" nil t)
		  ;; Put the word and score into the hashtb.
		  (setq val (gnus-gethash (setq word (match-string 0))
					  hashtb))
		  (when (or (not gnus-adaptive-word-length-limit)
			    (> (length word)
			       gnus-adaptive-word-length-limit))
		    (setq val (+ score (or val 0)))
		    (if (and gnus-adaptive-word-minimum
			     (< val gnus-adaptive-word-minimum))
			(setq val gnus-adaptive-word-minimum))
		    (gnus-sethash word val hashtb)))
		(erase-buffer))))
	  ;; Make all the ignorable words ignored.
	  (let ((ignored (append gnus-ignored-adaptive-words
				 (if gnus-adaptive-word-no-group-words
				     (message-tokenize-header
				      (gnus-group-real-name
				       gnus-newsgroup-name)
				      "."))
				 gnus-default-ignored-adaptive-words)))
	    (while ignored
	      (gnus-sethash (pop ignored) nil hashtb)))
	  ;; Now we have all the words and scores, so we
	  ;; add these rules to the ADAPT file.
	  (set-buffer gnus-summary-buffer)
	  (mapatoms
	   (lambda (word)
	     (when (symbol-value word)
	       (gnus-summary-score-entry
		"subject" (symbol-name word) 'w (symbol-value word)
		date nil t)))
	   hashtb))))))

(defun gnus-score-edit-done ()
  (let ((bufnam (buffer-file-name (current-buffer)))
	(winconf gnus-prev-winconf))
    (when winconf
      (set-window-configuration winconf))
    (gnus-score-remove-from-cache bufnam)
    (gnus-score-load-file bufnam)
    (run-hooks 'gnus-score-edit-done-hook)))

(defun gnus-score-find-trace ()
  "Find all score rules that applies to the current article."
  (interactive)
  (let ((old-scored gnus-newsgroup-scored))
    (let ((gnus-newsgroup-headers
	   (list (gnus-summary-article-header)))
	  (gnus-newsgroup-scored nil)
	  ;; Must be synced with `gnus-score-edit-file-at-point':
	  (frmt "%S [%s] -> %s\n")
	  trace
	  file)
      (save-excursion
	(nnheader-set-temp-buffer "*Score Trace*"))
      (setq gnus-score-trace nil)
      (gnus-possibly-score-headers 'trace)
      (if (not (setq trace gnus-score-trace))
	  (gnus-error
	   1 "No score rules apply to the current article (default score %d)."
	   gnus-summary-default-score)
	(set-buffer "*Score Trace*")
	;; Use a keymap instead?
	(local-set-key "q"
		       (lambda ()
			 (interactive)
			 (bury-buffer nil)
			 (gnus-summary-expand-window)))
	(local-set-key "k"
		       (lambda ()
			 (interactive)
			 (kill-buffer (current-buffer))
			 (gnus-summary-expand-window)))
	(local-set-key "e" (lambda ()
			     "Run `gnus-score-edit-file-at-point'."
			     (interactive)
			     (gnus-score-edit-file-at-point)))
	(local-set-key "f" (lambda ()
			     "Run `gnus-score-edit-file-at-point'."
			     (interactive)
			     (gnus-score-edit-file-at-point 'format)))
	(local-set-key "t" 'toggle-truncate-lines)
	(setq truncate-lines t)
	(dolist (entry trace)
	  (setq file (or (car entry)
			 ;; Must be synced with
			 ;; `gnus-score-edit-file-at-point':
			 "(non-file rule)"))
	  (insert
	   (format frmt
		   (cdr entry)
		   ;; Don't use `file-name-sans-extension' to see .SCORE and
		   ;; .ADAPT directly:
		   (file-name-nondirectory file)
		   (abbreviate-file-name file))))
	(insert
	 (format "\nTotal score: %d"
		 (apply '+ (mapcar
			    (lambda (s)
			      (or (caddr s)
				  gnus-score-interactive-default-score))
			    trace))))
	(insert
	 "\n\nQuick help:

Type `e' to edit score file corresponding to the score rule on current line,
`f' to format (pretty print) the score file and edit it,
`t' toggle to truncate long lines in this buffer,
`q' to quit, `k' to kill score trace buffer.

The first sexp on each line is the score rule, followed by the file name of
the score file and its full name, including the directory.")
	(goto-char (point-min))
	(gnus-configure-windows 'score-trace)))
    (set-buffer gnus-summary-buffer)
    (setq gnus-newsgroup-scored old-scored)))

(defun gnus-score-find-favourite-words ()
  "List words used in scoring."
  (interactive)
  (let ((alists (gnus-score-load-files (gnus-all-score-files)))
	alist rule rules kill)
    ;; Go through all the score alists for this group
    ;; and find all `w' rules.
    (while (setq alist (pop alists))
      (while (setq rule (pop alist))
	(when (and (stringp (car rule))
		   (equal "subject" (downcase (pop rule))))
	  (while (setq kill (pop rule))
	    (when (memq (nth 3 kill) '(w W word Word))
	      (push (cons (or (nth 1 kill)
			      gnus-score-interactive-default-score)
			  (car kill))
		    rules))))))
    (setq rules (sort rules (lambda (r1 r2)
			      (string-lessp (cdr r1) (cdr r2)))))
    ;; Add up words that have appeared several times.
    (let ((r rules))
      (while (cdr r)
	(if (equal (cdar r) (cdadr r))
	    (progn
	      (setcar (car r) (+ (caar r) (caadr r)))
	      (setcdr r (cddr r)))
	  (pop r))))
    ;; Insert the words.
    (nnheader-set-temp-buffer "*Score Words*")
    (if (not (setq rules (sort rules (lambda (r1 r2) (> (car r1) (car r2))))))
	(gnus-error 3 "No word score rules")
      (while rules
	(insert (format "%-5d: %s\n" (caar rules) (cdar rules)))
	(pop rules))
      (goto-char (point-min))
      (gnus-configure-windows 'score-words))))

(defun gnus-summary-rescore ()
  "Redo the entire scoring process in the current summary."
  (interactive)
  (gnus-score-save)
  (setq gnus-score-cache nil)
  (setq gnus-newsgroup-scored nil)
  (gnus-possibly-score-headers)
  (gnus-score-update-all-lines))

(defun gnus-score-flush-cache ()
  "Flush the cache of score files."
  (interactive)
  (gnus-score-save)
  (setq gnus-score-cache nil
	gnus-score-alist nil
	gnus-short-name-score-file-cache nil)
  (gnus-message 6 "The score cache is now flushed"))

(gnus-add-shutdown 'gnus-score-close 'gnus)

(defvar gnus-score-file-alist-cache nil)

(defun gnus-score-close ()
  "Clear all internal score variables."
  (setq gnus-score-cache nil
	gnus-internal-global-score-files nil
	gnus-score-file-list nil
	gnus-score-file-alist-cache nil))

;; Summary score marking commands.

(defun gnus-summary-raise-same-subject-and-select (score)
  "Raise articles which has the same subject with SCORE and select the next."
  (interactive "p")
  (let ((subject (gnus-summary-article-subject)))
    (gnus-summary-raise-score score)
    (while (gnus-summary-find-subject subject)
      (gnus-summary-raise-score score))
    (gnus-summary-next-article t)))

(defun gnus-summary-raise-same-subject (score)
  "Raise articles which has the same subject with SCORE."
  (interactive "p")
  (let ((subject (gnus-summary-article-subject)))
    (gnus-summary-raise-score score)
    (while (gnus-summary-find-subject subject)
      (gnus-summary-raise-score score))
    (gnus-summary-next-subject 1 t)))

(defun gnus-score-delta-default (level)
  (if level (prefix-numeric-value level)
    gnus-score-interactive-default-score))

(defun gnus-summary-raise-thread (&optional score)
  "Raise the score of the articles in the current thread with SCORE."
  (interactive "P")
  (setq score (gnus-score-delta-default score))
  (let (e)
    (save-excursion
      (let ((articles (gnus-summary-articles-in-thread)))
	(while articles
	  (gnus-summary-goto-subject (car articles))
	  (gnus-summary-raise-score score)
	  (setq articles (cdr articles))))
      (setq e (point)))
    (let ((gnus-summary-check-current t))
      (unless (zerop (gnus-summary-next-subject 1 t))
	(goto-char e))))
  (gnus-summary-recenter)
  (gnus-summary-position-point)
  (gnus-set-mode-line 'summary))

(defun gnus-summary-lower-same-subject-and-select (score)
  "Raise articles which has the same subject with SCORE and select the next."
  (interactive "p")
  (gnus-summary-raise-same-subject-and-select (- score)))

(defun gnus-summary-lower-same-subject (score)
  "Raise articles which has the same subject with SCORE."
  (interactive "p")
  (gnus-summary-raise-same-subject (- score)))

(defun gnus-summary-lower-thread (&optional score)
  "Lower score of articles in the current thread with SCORE."
  (interactive "P")
  (gnus-summary-raise-thread (- (gnus-score-delta-default score))))

;;; Finding score files.

(defun gnus-score-score-files (group)
  "Return a list of all possible score files."
  ;; Search and set any global score files.
  (when gnus-global-score-files
    (unless gnus-internal-global-score-files
      (gnus-score-search-global-directories gnus-global-score-files)))
  ;; Fix the kill-file dir variable.
  (setq gnus-kill-files-directory
	(file-name-as-directory gnus-kill-files-directory))
  ;; If we can't read it, there are no score files.
  (if (not (file-exists-p (expand-file-name gnus-kill-files-directory)))
      (setq gnus-score-file-list nil)
    (if (not (gnus-use-long-file-name 'not-score))
	;; We do not use long file names, so we have to do some
	;; directory traversing.
	(setq gnus-score-file-list
	      (cons nil
		    (or gnus-short-name-score-file-cache
			(prog2
			    (gnus-message 6 "Finding all score files...")
			    (setq gnus-short-name-score-file-cache
				  (gnus-score-score-files-1
				   gnus-kill-files-directory))
			  (gnus-message 6 "Finding all score files...done")))))
      ;; We want long file names.
      (when (or (not gnus-score-file-list)
		(not (car gnus-score-file-list))
		(gnus-file-newer-than gnus-kill-files-directory
				      (car gnus-score-file-list)))
	(setq gnus-score-file-list
	      (cons (nth 5 (file-attributes gnus-kill-files-directory))
		    (nreverse
		     (directory-files
		      gnus-kill-files-directory t
		      (gnus-score-file-regexp)))))))
    (cdr gnus-score-file-list)))

(defun gnus-score-score-files-1 (dir)
  "Return all possible score files under DIR."
  (let ((files (list (expand-file-name dir)))
	(regexp (gnus-score-file-regexp))
	(case-fold-search nil)
	seen out file)
    (while (setq file (pop files))
      (cond
       ;; Ignore files that start with a dot.
       ((string-match "^\\." (file-name-nondirectory file))
	nil)
       ;; Add subtrees of directory to also be searched.
       ((and (file-directory-p file)
	     (not (member (file-truename file) seen)))
	(push (file-truename file) seen)
	(setq files (nconc (directory-files file t nil t) files)))
       ;; Add files to the list of score files.
       ((string-match regexp file)
	(push file out))))
    (or out
	;; Return a dummy value.
	(list (expand-file-name "this.file.does.not.exist.SCORE"
				gnus-kill-files-directory)))))

(defun gnus-score-file-regexp ()
  "Return a regexp that match all score files."
  (concat "\\(" (regexp-quote gnus-score-file-suffix )
	  "\\|" (regexp-quote gnus-adaptive-file-suffix) "\\)\\'"))

(defun gnus-score-find-bnews (group)
  "Return a list of score files for GROUP.
The score files are those files in the ~/News/ directory which matches
GROUP using BNews sys file syntax."
  (let* ((sfiles (append (gnus-score-score-files group)
			 gnus-internal-global-score-files))
	 (kill-dir (file-name-as-directory
		    (expand-file-name gnus-kill-files-directory)))
	 (klen (length kill-dir))
	 (score-regexp (gnus-score-file-regexp))
	 (trans (cdr (assq ?: nnheader-file-name-translation-alist)))
	 (group-trans (nnheader-translate-file-chars group t))
	 ofiles not-match regexp)
    (with-current-buffer (gnus-get-buffer-create "*gnus score files*")
      (buffer-disable-undo)
      ;; Go through all score file names and create regexp with them
      ;; as the source.
      (while sfiles
	(erase-buffer)
	(insert (car sfiles))
	(goto-char (point-min))
	;; First remove the suffix itself.
	(when (re-search-forward (concat "." score-regexp) nil t)
	  (replace-match "" t t)
	  (goto-char (point-min))
	  (if (looking-at (regexp-quote kill-dir))
	      ;; If the file name was just "SCORE", `klen' is one character
	      ;; too much.
	      (delete-char (min (1- (point-max)) klen))
	    (goto-char (point-max))
	    (if (re-search-backward gnus-directory-sep-char-regexp nil t)
		(delete-region (1+ (point)) (point-min))
	      (gnus-message 1 "Can't find directory separator in %s"
			    (car sfiles))))
	  ;; If short file names were used, we have to translate slashes.
	  (goto-char (point-min))
	  (let ((regexp (concat
			 "[/:" (if trans (char-to-string trans)) "]")))
	    (while (re-search-forward regexp nil t)
	      (replace-match "." t t)))
	  ;; Kludge to get rid of "nntp+" problems.
	  (goto-char (point-min))
	  (when (looking-at "nn[a-z]+\\+")
	    (search-forward "+")
	    (forward-char -1)
	    (insert "\\")
	    (forward-char 1))
	  ;; Kludge to deal with "++".
	  (while (search-forward "+" nil t)
	    (replace-match "\\+" t t))
	  ;; Translate "all" to ".*".
	  (goto-char (point-min))
	  (while (search-forward "all" nil t)
	    (replace-match ".*" t t))
	  (goto-char (point-min))
	  ;; Deal with "not."s.
	  (if (looking-at "not.")
	      (progn
		(setq not-match t)
		(setq regexp
		      (concat "^" (buffer-substring 5 (point-max)) "$")))
	    (setq regexp (concat "^" (buffer-substring 1 (point-max)) "$"))
	    (setq not-match nil))
	  ;; Finally - if this resulting regexp matches the group name,
	  ;; we add this score file to the list of score files
	  ;; applicable to this group.
	  (when (or (and not-match
			 (ignore-errors
			   (not (string-match regexp group-trans))))
		    (and (not not-match)
			 (ignore-errors (string-match regexp group-trans))))
	    (push (car sfiles) ofiles)))
	(setq sfiles (cdr sfiles)))
      (gnus-kill-buffer (current-buffer))
      ;; Slight kludge here - the last score file returned should be
      ;; the local score file, whether it exists or not.  This is so
      ;; that any score commands the user enters will go to the right
      ;; file, and not end up in some global score file.
      (let ((localscore (gnus-score-file-name group)))
	(setq ofiles (cons localscore (delete localscore ofiles))))
      (gnus-sort-score-files (nreverse ofiles)))))

(defun gnus-score-find-single (group)
  "Return list containing the score file for GROUP."
  (list (or gnus-newsgroup-adaptive-score-file
	    (gnus-score-file-name group gnus-adaptive-file-suffix))
	(gnus-score-file-name group)))

(defun gnus-score-find-hierarchical (group)
  "Return list of score files for GROUP.
This includes the score file for the group and all its parents."
  (let* ((prefix (gnus-group-real-prefix group))
	 (all (list nil))
	 (group (gnus-group-real-name group))
	 (start 0))
    (while (string-match "\\." group (1+ start))
      (setq start (match-beginning 0))
      (push (substring group 0 start) all))
    (push group all)
    (setq all
	  (nconc
	   (mapcar (lambda (group)
		     (gnus-score-file-name group gnus-adaptive-file-suffix))
		   (setq all (nreverse all)))
	   (mapcar 'gnus-score-file-name all)))
    (if (equal prefix "")
	all
      (mapcar
       (lambda (file)
	 (nnheader-translate-file-chars
	  (concat (file-name-directory file) prefix
		  (file-name-nondirectory file))))
       all))))

(defun gnus-score-file-rank (file)
  "Return a number that says how specific score FILE is.
Destroys the current buffer."
  (if (member file gnus-internal-global-score-files)
      0
    (when (string-match
	   (concat "^" (regexp-quote
			(expand-file-name
			 (file-name-as-directory gnus-kill-files-directory))))
	   file)
      (setq file (substring file (match-end 0))))
    (insert file)
    (goto-char (point-min))
    (let ((beg (point))
	  elems)
      (while (re-search-forward "[./]" nil t)
	(push (buffer-substring beg (1- (point)))
	      elems))
      (erase-buffer)
      (setq elems (delete "all" elems))
      (length elems))))

(defun gnus-sort-score-files (files)
  "Sort FILES so that the most general files come first."
  (with-temp-buffer
    (let ((alist
	   (mapcar
	    (lambda (file)
	      (cons (inline (gnus-score-file-rank file)) file))
	    files)))
      (mapcar 'cdr (sort alist 'car-less-than-car)))))

(defun gnus-score-find-alist (group)
  "Return list of score files for GROUP.
The list is determined from the variable `gnus-score-file-alist'."
  (let ((alist gnus-score-file-multiple-match-alist)
	score-files)
    ;; if this group has been seen before, return the cached entry
    (if (setq score-files (assoc group gnus-score-file-alist-cache))
	(cdr score-files)		;ensures caching groups with no matches
      ;; handle the multiple match alist
      (while alist
	(when (string-match (caar alist) group)
	  (setq score-files (append (cdar alist) score-files)))
	(setq alist (cdr alist)))
      (setq alist gnus-score-file-single-match-alist)
      ;; handle the single match alist
      (while alist
	(when (string-match (caar alist) group)
	  ;; progn used just in case ("regexp") has no files
	  ;; and score-files is still nil.  -sj
	  ;; this can be construed as a "stop searching here" feature :>
	  ;; and used to simplify regexps in the single-alist
	  (setq score-files (append (cdar alist) score-files))
	  (setq alist nil))
	(setq alist (cdr alist)))
      ;; cache the score files
      (push (cons group score-files) gnus-score-file-alist-cache)
      score-files)))

(defun gnus-all-score-files (&optional group)
  "Return a list of all score files for the current group."
  (let ((funcs gnus-score-find-score-files-function)
	(group (or group gnus-newsgroup-name))
	score-files)
    (when group
      ;; Make sure funcs is a list.
      (and funcs
	   (not (listp funcs))
	   (setq funcs (list funcs)))
      (when gnus-score-use-all-scores
	;; Get the initial score files for this group.
	(when funcs
	  (setq score-files (copy-sequence (gnus-score-find-alist group))))
	;; Add any home adapt files.
	(let ((home (gnus-home-score-file group t)))
	  (when home
	    (push home score-files)
	    (setq gnus-newsgroup-adaptive-score-file home)))
	;; Check whether there is a `adapt-file' group parameter.
	(let ((param-file (gnus-group-find-parameter group 'adapt-file)))
	  (when param-file
	    (push param-file score-files)
	    (setq gnus-newsgroup-adaptive-score-file param-file))))
      ;; Go through all the functions for finding score files (or actual
      ;; scores) and add them to a list.
      (while funcs
	(when (functionp (car funcs))
	  (setq score-files
		(append score-files
			(nreverse (funcall (car funcs) group)))))
	(setq funcs (cdr funcs)))
      (when gnus-score-use-all-scores
	;; Add any home score files.
	(let ((home (gnus-home-score-file group)))
	  (when home
	    (push home score-files)))
	;; Check whether there is a `score-file' group parameter.
	(let ((param-file (gnus-group-find-parameter group 'score-file)))
	  (when param-file
	    (push param-file score-files))))
      ;; Expand all files names.
      (let ((files score-files))
	(while files
	  (when (stringp (car files))
	    (setcar files (expand-file-name
			   (car files) gnus-kill-files-directory)))
	  (pop files)))
      (setq score-files (nreverse score-files))
      ;; Remove any duplicate score files.
      (while (and score-files
		  (member (car score-files) (cdr score-files)))
	(pop score-files))
      (let ((files score-files))
	(while (cdr files)
	  (if (member (cadr files) (cddr files))
	      (setcdr files (cddr files))
	    (pop files))))
      ;; Do the scoring if there are any score files for this group.
      score-files)))

(defun gnus-possibly-score-headers (&optional trace)
  "Do scoring if scoring is required."
  (let ((score-files (gnus-all-score-files)))
    (when score-files
      (gnus-score-headers score-files trace))))

(defun gnus-score-file-name (newsgroup &optional suffix)
  "Return the name of a score file for NEWSGROUP."
  (let ((suffix (or suffix gnus-score-file-suffix)))
    (nnheader-translate-file-chars
     (cond
      ((or (null newsgroup)
	   (string-equal newsgroup ""))
       ;; The global score file is placed at top of the directory.
       (expand-file-name suffix gnus-kill-files-directory))
      ((gnus-use-long-file-name 'not-score)
       ;; Append ".SCORE" to newsgroup name.
       (expand-file-name (concat (gnus-newsgroup-savable-name newsgroup)
				 "." suffix)
			 gnus-kill-files-directory))
      (t
       ;; Place "SCORE" under the hierarchical directory.
       (expand-file-name (concat (gnus-newsgroup-directory-form newsgroup)
				 "/" suffix)
			 gnus-kill-files-directory))))))

(defun gnus-score-search-global-directories (files)
  "Scan all global score directories for score files."
  ;; Set the variable `gnus-internal-global-score-files' to all
  ;; available global score files.
  (interactive (list gnus-global-score-files))
  (let (out)
    (while files
      ;; #### /$ Unix-specific?
      (if (file-directory-p (car files))
	  (setq out (nconc (directory-files
			    (car files) t
			    (concat (gnus-score-file-regexp) "$"))))
	(push (car files) out))
      (setq files (cdr files)))
    (setq gnus-internal-global-score-files out)))

(defun gnus-score-default-fold-toggle ()
  "Toggle folding for new score file entries."
  (interactive)
  (setq gnus-score-default-fold (not gnus-score-default-fold))
  (if gnus-score-default-fold
      (gnus-message 1 "New score file entries will be case insensitive.")
    (gnus-message 1 "New score file entries will be case sensitive.")))

;;; Home score file.

(defun gnus-home-score-file (group &optional adapt)
  "Return the home score file for GROUP.
If ADAPT, return the home adaptive file instead."
  (let ((list (if adapt gnus-home-adapt-file gnus-home-score-file))
	elem found)
    ;; Make sure we have a list.
    (unless (listp list)
      (setq list (list list)))
    ;; Go through the list and look for matches.
    (while (and (not found)
		(setq elem (pop list)))
      (setq found
	    (cond
	     ;; Simple string.
	     ((stringp elem)
	      elem)
	     ;; Function.
	     ((functionp elem)
	      (funcall elem group))
	     ;; Regexp-file cons.
	     ((consp elem)
	      (when (string-match (gnus-globalify-regexp (car elem)) group)
		(replace-match (cadr elem) t nil group))))))
    (when found
      (setq found (nnheader-translate-file-chars found))
      (if (file-name-absolute-p found)
	  found
	(nnheader-concat gnus-kill-files-directory found)))))

(defun gnus-hierarchial-home-score-file (group)
  "Return the score file of the top-level hierarchy of GROUP."
  (if (string-match "^[^.]+\\." group)
      (concat (match-string 0 group) gnus-score-file-suffix)
    ;; Group name without any dots.
    (concat group (if (gnus-use-long-file-name 'not-score) "." "/")
	    gnus-score-file-suffix)))

(defun gnus-hierarchial-home-adapt-file (group)
  "Return the adapt file of the top-level hierarchy of GROUP."
  (if (string-match "^[^.]+\\." group)
      (concat (match-string 0 group) gnus-adaptive-file-suffix)
    ;; Group name without any dots.
    (concat group (if (gnus-use-long-file-name 'not-score) "." "/")
	    gnus-adaptive-file-suffix)))

(defun gnus-current-home-score-file (group)
  "Return the \"current\" regular score file."
  (car (gnus-score-find-alist group)))

;;;
;;; Score decays
;;;

(defun gnus-decay-score (score)
  "Decay SCORE according to `gnus-score-decay-constant' and `gnus-score-decay-scale'."
  (let ((n (- score
	      (* (if (< score 0) -1 1)
		 (min (abs score)
		      (max gnus-score-decay-constant
			   (* (abs score)
			      gnus-score-decay-scale)))))))
    (if (and (featurep 'xemacs)
	     ;; XEmacs's floor can handle only the floating point
	     ;; number below the half of the maximum integer.
	     (> (abs n) (lsh -1 -2)))
	(string-to-number
	 (car (split-string (number-to-string n) "\\.")))
      (floor n))))

(defun gnus-decay-scores (alist day)
  "Decay non-permanent scores in ALIST."
  (let ((times (- (time-to-days (current-time)) day))
	kill entry updated score n)
    (unless (zerop times)		;Done decays today already?
      (while (setq entry (pop alist))
	(when (stringp (car entry))
	  (setq entry (cdr entry))
	  (while (setq kill (pop entry))
	    (when (nth 2 kill)
	      (setq updated t)
	      (setq score (or (nth 1 kill)
			      gnus-score-interactive-default-score)
		    n times)
	      (while (natnump (decf n))
		(setq score (funcall gnus-decay-score-function score)))
	      (setcdr kill (cons score
				 (cdr (cdr kill)))))))))
    ;; Return whether this score file needs to be saved.  By Je-haysuss!
    updated))

(defun gnus-score-regexp-bad-p (regexp)
  "Test whether REGEXP is safe for Gnus scoring.
A regexp is unsafe if it matches newline or a buffer boundary.

If the regexp is good, return nil.  If the regexp is bad, return a
cons cell (SYM . STRING), where the symbol SYM is `new' or `bad'.
In the `new' case, the string is a safe replacement for REGEXP.
In the `bad' case, the string is a unsafe subexpression of REGEXP,
and we do not have a simple replacement to suggest.

See Info node `(gnus)Scoring Tips' for examples of good regular expressions."
  (let (case-fold-search)
    (and
     ;; First, try a relatively fast necessary condition.
     ;; Notice ranges (like [^:] or [\t-\r]), \s>, \Sw, \W, \', \`:
     (string-match "\n\\|\\\\[SsW`']\\|\\[\\^\\|[\0-\n]-" regexp)
     ;; Now break the regexp into tokens, and check each:
     (let ((tail regexp)		; remaining regexp to check
	   tok				; current token
	   bad				; nil, or bad subexpression
	   new				; nil, or replacement regexp so far
	   end)				; length of current token
       (while (and (not bad)
		   (string-match
		    "\\`\\(\\\\[sS]?.\\|\\[\\^?]?[^]]*]\\|[^\\]\\)"
		    tail))
	 (setq end (match-end 0)
	       tok (substring tail 0 end)
	       tail (substring tail end))
	 (if;; Is token `bad' (matching newline or buffer ends)?
	     (or (member tok '("\n" "\\W" "\\`" "\\'"))
		 ;; This next handles "[...]", "\\s.", and "\\S.":
		 (and (> end 2) (string-match tok "\n")))
	     (let ((newtok
		    ;; Try to suggest a replacement for tok ...
		    (cond ((string-equal tok "\\`") "^") ; or "\\(^\\)"
			  ((string-equal tok "\\'") "$") ; or "\\($\\)"
			  ((string-match "\\[\\^" tok) ; very common
			   (concat (substring tok 0 -1) "\n]")))))
	       (if newtok
		   (setq new
			 (concat
			  (or new
			      ;; good prefix so far:
			      (substring regexp 0 (- (+ (length tail) end))))
			  newtok))
		 ;; No replacement idea, so give up:
		 (setq bad tok)))
	   ;; tok is good, may need to extend new
	   (and new (setq new (concat new tok)))))
       ;; Now return a value:
       (cond
	(bad (cons 'bad bad))
	(new (cons 'new new))
	(t nil))))))

(provide 'gnus-score)

;;; gnus-score.el ends here
