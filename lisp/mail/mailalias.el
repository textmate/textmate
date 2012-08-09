;;; mailalias.el --- expand and complete mailing address aliases -*- lexical-binding: t -*-

;; Copyright (C) 1985, 1987, 1995-1997, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: mail

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

;; Basic functions for defining and expanding mail aliases.
;; These seal off the interface to the alias-definition parts of a
;; .mailrc file formatted for BSD's Mail or USL's mailx.

;;; Code:

(require 'sendmail)

(defgroup mailalias nil
  "Expanding mail aliases."
  :group 'mail)

(defcustom mail-passwd-files '("/etc/passwd")
  "List of files from which to determine valid user names."
  :type '(repeat string)
  :group 'mailalias)

(defcustom mail-passwd-command nil
  "Shell command to retrieve text to add to `/etc/passwd', or nil."
  :type '(choice string (const nil))
  :group 'mailalias)

(defvar mail-directory-names t
  "Alist of mail address directory entries.
When t this still needs to be initialized.")

(defvar mail-address-field-regexp
  "^\\(Resent-\\)?\\(To\\|From\\|CC\\|BCC\\|Reply-to\\):")

(defvar pattern)

(defcustom mail-complete-alist
  ;; Don't refer to mail-address-field-regexp here;
  ;; that confuses some things such as cus-dep.el.
  '(("^\\(Resent-\\)?\\(To\\|From\\|CC\\|BCC\\|Reply-to\\):"
     . (mail-get-names pattern))
    ("Newsgroups:" . (if (boundp 'gnus-active-hashtb)
                         gnus-active-hashtb
                       (if (boundp news-group-article-assoc)
                           news-group-article-assoc)))
    ("Followup-To:" . (mail-sentto-newsgroups))
    ;;("Distribution:" ???)
    )
  "Alist of header field and expression to return alist for completion.
The expression may reference the variable `pattern'
which will hold the string being completed.
If not on matching header, `mail-complete-function' gets called instead."
  :type 'alist
  :group 'mailalias)
(put 'mail-complete-alist 'risky-local-variable t)

;;;###autoload
(defcustom mail-complete-style 'angles
  "Specifies how \\[mail-complete] formats the full name when it completes.
If `nil', they contain just the return address like:
	king@grassland.com
If `parens', they look like:
	king@grassland.com (Elvis Parsley)
If `angles', they look like:
	Elvis Parsley <king@grassland.com>"
  :type '(choice (const angles) (const parens) (const nil))
  :group 'mailalias)

(defcustom mail-complete-function 'ispell-complete-word
  "Function to call when completing outside `mail-complete-alist'-header."
  :type '(choice function (const nil))
  :group 'mailalias)
(make-obsolete-variable 'mail-complete-function
                        'completion-at-point-functions "24.1")

(defcustom mail-directory-function nil
  "Function to get completions from directory service or nil for none.
See `mail-directory-requery'."
  :type '(choice function (const nil))
  :group 'mailalias)

;; This is for when the directory is huge, or changes frequently.
(defcustom mail-directory-requery nil
  "When non-nil call `mail-directory-function' for each completion.
In that case, one argument gets passed to the function, the partial string
entered so far."
  :type 'boolean
  :group 'mailalias)

(defcustom mail-directory-process nil
  "Shell command to get the list of names from a mail directory.
This value is used when the value of `mail-directory-function'
is `mail-directory-process'.  The value should be a list
of the form (COMMAND ARG ...), where each of the list elements
is evaluated.  COMMAND should evaluate to a string.  When
`mail-directory-requery' is non-nil, during evaluation of these
elements, the variable `pattern' contains the partial input being
completed.  `pattern' is nil when `mail-directory-requery' is nil.

The value might look like this:

  '(remote-shell-program \"HOST\" \"-nl\" \"USER\" \"COMMAND\")

or like this:

  '(remote-shell-program \"HOST\" \"-n\" \"COMMAND '^\" pattern \"'\")"
  :type 'sexp
  :group 'mailalias)
(put 'mail-directory-process 'risky-local-variable t)

(defcustom mail-directory-stream nil
  "List of (HOST SERVICE) for stream connection to mail directory."
  :type 'sexp
  :group 'mailalias)
(put 'mail-directory-stream 'risky-local-variable t)

(defcustom mail-directory-parser nil
  "How to interpret the output of `mail-directory-function'.
Three types of values are possible:

  - nil means to gather each line as one name
  - regexp means first \\(grouping\\) in successive matches is name
  - function called at beginning of buffer that returns an alist of names"
  :type '(choice (const nil) regexp function)
  :group 'mailalias)
(put 'mail-directory-parser 'risky-local-variable t)

;; Internal variables.

(defvar mail-names t
  "Alist of local users, aliases and directory entries as available.
Elements have the form (MAILNAME) or (MAILNAME . FULLNAME).
If the value means t, it means the real value should be calculated
for the next use.  This is used in `mail-complete'.")

(defvar mail-local-names t
  "Alist of local users.
When t this still needs to be initialized.")


;; Called from sendmail-send-it, or similar functions,
;; only if some mail aliases are defined.
;;;###autoload
(defun expand-mail-aliases (beg end &optional exclude)
  "Expand all mail aliases in suitable header fields found between BEG and END.
If interactive, expand in header fields.
Suitable header fields are `To', `From', `CC' and `BCC', `Reply-to', and
their `Resent-' variants.

Optional second arg EXCLUDE may be a regular expression defining text to be
removed from alias expansions."
  (interactive
   (save-excursion
     (list (goto-char (point-min))
	   (mail-header-end))))
  (sendmail-sync-aliases)
  (when (eq mail-aliases t)
    (setq mail-aliases nil)
    (build-mail-aliases))
  (save-excursion
    (goto-char beg)
    (setq end (set-marker (make-marker) end))
    (let ((case-fold-search nil))
      (while (let ((case-fold-search t))
	       (re-search-forward mail-address-field-regexp end t))
	(skip-chars-forward " \t")
	(let ((beg1 (point))
	      end1 pos epos seplen
	      ;; DISABLED-ALIASES records aliases temporarily disabled
	      ;; while we scan text that resulted from expanding those aliases.
	      ;; Each element is (ALIAS . TILL-WHEN), where TILL-WHEN
	      ;; is where to reenable the alias (expressed as number of chars
	      ;; counting from END1).
	      (disabled-aliases nil))
	  (re-search-forward "^[^ \t]" end 'move)
	  (beginning-of-line)
	  (skip-chars-backward " \t\n")
	  (setq end1 (point-marker))
	  (goto-char beg1)
	  (while (< (point) end1)
	    (setq pos (point))
	    ;; Reenable any aliases which were disabled for ranges
	    ;; that we have passed out of.
	    (while (and disabled-aliases
			(> pos (- end1 (cdr (car disabled-aliases)))))
	      (setq disabled-aliases (cdr disabled-aliases)))
	    ;; EPOS gets position of end of next name;
	    ;; SEPLEN gets length of whitespace&separator that follows it.
	    (if (re-search-forward "[ \t]*[\n,][ \t]*" end1 t)
		(setq epos (match-beginning 0)
		      seplen (- (point) epos))
	      (setq epos (marker-position end1) seplen 0))
	    (let ((string (buffer-substring-no-properties pos epos))
		  translation)
	      (if (and (not (assoc string disabled-aliases))
		       (setq translation (cdr (assoc string mail-aliases))))
		  (progn
		    ;; This name is an alias.  Disable it.
		    (setq disabled-aliases (cons (cons string (- end1 epos))
						 disabled-aliases))
		    ;; Replace the alias with its expansion
		    ;; then rescan the expansion for more aliases.
		    (goto-char pos)
		    (insert translation)
		    (when exclude
		      (let ((regexp (concat "\\b\\(" exclude "\\)\\b"))
			    (end (point-marker)))
			(goto-char pos)
			(while (re-search-forward regexp end t)
			  (replace-match ""))
			(goto-char end)))
		    (delete-region (point) (+ (point) (- epos pos)))
		    (goto-char pos))
		;; Name is not an alias.  Skip to start of next name.
		(goto-char epos)
		(forward-char seplen))))
	  (set-marker end1 nil)))
      (set-marker end nil))))

;; Called by mail-setup, or similar functions, only if the file specified
;; by mail-personal-alias-file (usually `~/.mailrc') exists.
(defun build-mail-aliases (&optional file)
  "Read mail aliases from personal aliases file and set `mail-aliases'.
By default, this is the file specified by `mail-personal-alias-file'."
  (interactive
   (list
    (read-file-name (format "Read mail alias file (default %s): "
			    mail-personal-alias-file)
		    nil mail-personal-alias-file t)))
  (setq file (expand-file-name (or file mail-personal-alias-file)))
  ;; In case mail-aliases is t, make sure define-mail-alias
  ;; does not recursively call build-mail-aliases.
  (setq mail-aliases nil)
  (with-temp-buffer
    (while file
      (cond ((get-file-buffer file)
             (insert (with-current-buffer (get-file-buffer file)
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
            ((file-exists-p file) (insert-file-contents file))
            ((file-exists-p (setq file (expand-file-name file "~/")))
             (insert-file-contents file))
            (t (setq file nil)))
      (goto-char (point-min))
      ;; Delete comments from the contents.
      (while (search-forward "# " nil t)
        (let ((p (- (point) 2)))
          (end-of-line)
          (delete-region p (point))))
      ;; Don't lose if no final newline.
      (goto-char (point-max))
      (or (eq (preceding-char) ?\n) (newline))
      (goto-char (point-min))
      ;; handle "\\\n" continuation lines
      (while (not (eobp))
        (end-of-line)
        (if (= (preceding-char) ?\\)
            (progn (delete-char -1) (delete-char 1) (insert ?\ ))
          (forward-char 1)))
      (goto-char (point-min))
      ;; handle `source' directives -- Eddy/1994/May/25
      (cond ((re-search-forward "^source[ \t]+" nil t)
             (re-search-forward "\\S-+")
             (setq file (buffer-substring-no-properties
                         (match-beginning 0) (match-end 0)))
             (beginning-of-line)
             (insert "# ")   ; to ensure we don't re-process this file
             (beginning-of-line))
            (t (setq file nil))))
    (goto-char (point-min))
    (while (re-search-forward
            "^\\(a\\|alias\\|g\\|group\\)[ \t]+\\([^ \t\n]+\\)" nil t)
      (let* ((name (match-string 2))
             (start (progn (skip-chars-forward " \t") (point)))
             value)
        (end-of-line)
        (setq value (buffer-substring-no-properties start (point)))
        (unless (equal value "")
          (define-mail-alias name value t))))
    mail-aliases))

;; Always autoloadable in case the user wants to define aliases
;; interactively or in .emacs.
;; define-mail-abbrev in mailabbrev.el duplicates much of this code.
;;;###autoload
(defun define-mail-alias (name definition &optional from-mailrc-file)
  "Define NAME as a mail alias that translates to DEFINITION.
This means that sending a message to NAME will actually send to DEFINITION.

Normally, the addresses in DEFINITION must be separated by commas.
If FROM-MAILRC-FILE is non-nil, then addresses in DEFINITION
can be separated by spaces; an address can contain spaces
if it is quoted with double-quotes."

  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  ;; Read the defaults first, if we have not done so.
  ;; But not if we are doing that already right now.
  (unless from-mailrc-file
    (sendmail-sync-aliases))
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p mail-personal-alias-file)
	    (build-mail-aliases))))
  ;; strip garbage from front and end
  (if (string-match "\\`[ \t\n,]+" definition)
      (setq definition (substring definition (match-end 0))))
  (if (string-match "[ \t\n,]+\\'" definition)
      (setq definition (substring definition 0 (match-beginning 0))))

  (let* ((L (length definition))
	 (start (if (> L 0) 0))
	 end this-entry result tem)
    (while start
      (cond
       (from-mailrc-file
	;; If we're reading from the mailrc file, addresses are
	;; delimited by spaces, and addresses with embedded spaces are
	;; surrounded by non-escaped double-quotes.
	(if (eq ?\" (aref definition start))
	    (setq start (1+ start)
		  end (and (string-match
			    "[^\\]\\(\\([\\][\\]\\)*\\)\"[ \t,]*"
			    definition start)
			   (match-end 1)))
	  (setq end (string-match "[ \t,]+" definition start)))
	;; Extract the address and advance the loop past it.
	(setq this-entry (substring definition start end)
	      start (and end (/= (match-end 0) L) (match-end 0)))
	;; If the full name contains a problem character, quote it.
	(and (string-match "\\(.+?\\)[ \t]*\\(<.*>\\)" this-entry)
	     (string-match "[^- !#$%&'*+/0-9=?A-Za-z^_`{|}~]"
			   (match-string 1 this-entry))
	     (setq this-entry (replace-regexp-in-string
			       "\\(.+?\\)[ \t]*\\(<.*>\\)"
			       "\"\\1\" \\2"
			       this-entry))))
       ;; When we are not reading from .mailrc, addresses are
       ;; separated by commas.  Try to accept a rfc822-like syntax.
       ;; (Todo: extend rfc822.el to do the work for us.)
       ((equal (string-match
		"[ \t,]*\\(\"\\(?:[^\"]\\|[^\\]\\(?:[\\][\\]\\)*\"\\)*\"[ \t]*\
<[-.!#$%&'*+/0-9=?A-Za-z^_`{|}~@]+>\\)[ \t,]*"
		definition start)
	       start)
	;; If an entry has a valid [ "foo bar" <foo@example.com> ]
	;; form, use it literally .  This also allows commas in the
	;; quoted string, e.g.  [ "foo bar, jr" <foo@example.com> ]
	(setq this-entry (match-string 1 definition)
	      start (and (/= (match-end 0) L) (match-end 0))))
       (t
	;; Otherwise, read the next address by looking for a comma.
	(setq end (string-match "[ \t\n,]*,[ \t\n]*" definition start))
	(setq this-entry (substring definition start end))
	;; Advance the loop past this address.
	(setq start (and end (/= (match-end 0) L) (match-end 0)))
	;; If the full name contains a problem character, quote it.
	(and (string-match "\\(.+?\\)[ \t]*\\(<.*>\\)" this-entry)
	     (string-match "[^- !#$%&'*+/0-9=?A-Za-z^_`{|}~]"
			   (match-string 1 this-entry))
	     (setq this-entry (replace-regexp-in-string
			       "\\(.+?\\)[ \t]*\\(<.*>\\)" "\"\\1\" \\2"
			       this-entry)))))
      (push this-entry result))

    (setq definition (mapconcat (function identity)
				(nreverse result) ", "))
    (setq tem (assoc name mail-aliases))
    (if tem
	(rplacd tem definition)
      (setq mail-aliases (cons (cons name definition) mail-aliases)
	    mail-names t))))

;;;###autoload
(defun mail-completion-at-point-function ()
  "Compute completion data for mail aliases.
For use on `completion-at-point-functions'."
  ;; Read the defaults first, if we have not done so.
  (sendmail-sync-aliases)
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p mail-personal-alias-file)
	    (build-mail-aliases))))
  (let ((list mail-complete-alist)
        (list-exp nil))
    (if (and (< 0 (mail-header-end))
	     (save-excursion
	       (if (re-search-backward "^[^\t ]" nil t)
		   (while list
		     (if (looking-at (car (car list)))
			 (setq list-exp (cdr (car list))
			       list ())
		       (setq list (cdr list)))))
	       list-exp))
	(let* ((end (point))
	       (beg (save-excursion
		      (skip-chars-backward "^ \t<,:")
		      (point)))
               (table (completion-table-dynamic
                       (lambda (prefix)
                         (let ((pattern prefix)) (eval list-exp))))))
          (list beg end table)))))

;;;###autoload
(defun mail-complete (arg)
  "Perform completion on header field or word preceding point.
Completable headers are according to `mail-complete-alist'.  If none matches
current header, calls `mail-complete-function' and passes prefix ARG if any."
  (interactive "P")
  ;; Read the defaults first, if we have not done so.
  (sendmail-sync-aliases)
  (if (eq mail-aliases t)
      (progn
	(setq mail-aliases nil)
	(if (file-exists-p mail-personal-alias-file)
	    (build-mail-aliases))))
  (let ((data (mail-completion-at-point-function)))
    (if data
        (apply #'completion-in-region data)
      (funcall mail-complete-function arg))))
(make-obsolete 'mail-complete 'mail-completion-at-point-function "24.1")

(defun mail-completion-expand (table)
  "Build new completion table that expands aliases.
Completes like TABLE except that if the completion is a valid alias,
it expands it to its full `mail-complete-style' form."
  (lambda (string pred action)
    (cond
     ((eq action nil)
      (let* ((comp (try-completion string table pred))
             (name (and (listp table) comp
                        (assoc (if (stringp comp) comp string) table))))
        (cond
         ((null name) comp)
         ((eq mail-complete-style 'parens)
          (concat (car name) " (" (cdr name) ")"))
         ((eq mail-complete-style 'angles)
          (concat (cdr name) " <" (car name) ">"))
         (t comp))))
     (t
      (complete-with-action action table string pred)))))

(defun mail-get-names (prefix)
  "Fetch local users and global mail addresses for completion.
Consults `/etc/passwd' and a directory service if one is set up via
`mail-directory-function'.
PREFIX is the string we want to complete."
  (if (eq mail-local-names t)
      (with-current-buffer (generate-new-buffer " passwd")
	(let ((files mail-passwd-files))
	  (while files
	    (insert-file-contents (car files) nil nil nil t)
	    (setq files (cdr files))))
	(if mail-passwd-command
	    (call-process shell-file-name nil t nil
			  shell-command-switch mail-passwd-command))
	(goto-char (point-min))
	(setq mail-local-names nil)
	(while (not (eobp))
	  ;;Recognize lines like
	  ;;  nobody:*:65534:65534::/:
	  ;;  +demo::::::/bin/csh
	  ;;  +ethanb
	  ;;while skipping
	  ;;  +@SOFTWARE
	  ;; The second \(...\) matches the user id.
	  (if (looking-at "\\+?\\([^:@\n+]+\\):[^:\n]*:\\([^\n:]*\\):")
	      (add-to-list 'mail-local-names
			   (cons (match-string 1)
				 (user-full-name
				  (string-to-number (match-string 2))))))
	  (beginning-of-line 2))
	(kill-buffer (current-buffer))))
  (if (or (eq mail-names t)
	  (eq mail-directory-names t))
      (let (directory)
	(and mail-directory-function
	     (eq mail-directory-names t)
	     (setq directory
		   (mail-directory (if mail-directory-requery prefix))))
	(or mail-directory-requery
	    (setq mail-directory-names directory))
	(if (or directory
		(eq mail-names t))
	    (setq mail-names
		  (sort (append (if (consp mail-aliases)
				    (mapcar
				     (function (lambda (a) (list (car a))))
				     mail-aliases))
				(if (consp mail-local-names)
				    mail-local-names)
				(or directory 
				    (when (consp mail-directory-names)
				      mail-directory-names)))
			(lambda (a b)
			  ;; Should cache downcased strings.
			  (string< (downcase (car a))
				   (downcase (car b)))))))))
  (mail-completion-expand mail-names))


(defun mail-directory (prefix)
  "Use mail-directory facility to get user names matching PREFIX.
If PREFIX is nil, get all the defined user names.
This function calls `mail-directory-function' to query the directory,
then uses `mail-directory-parser' to parse the output it returns."
  (message "Querying directory...")
  (with-current-buffer (generate-new-buffer " *mail-directory*")
    (funcall mail-directory-function prefix)
    (goto-char (point-min))
    (let (directory)
      (if (stringp mail-directory-parser)
	  (while (re-search-forward mail-directory-parser nil t)
	    (push (match-string 1) directory))
	(if mail-directory-parser
	    (setq directory (funcall mail-directory-parser))
	  (while (not (eobp))
	    (push (buffer-substring (point)
                                    (progn
                                      (forward-line)
                                      (if (bolp)
                                          (1- (point))
                                        (point))))
                  directory))))
      (kill-buffer (current-buffer))
      (message "Querying directory...done")
      directory)))

(defvar mailalias-done)

(defun mail-directory-process (prefix)
  "Run a shell command to output names in directory.
See `mail-directory-process'."
  (when (consp mail-directory-process)
    (let ((pattern prefix))             ;Dynbind!
      (apply 'call-process (eval (car mail-directory-process)) nil t nil
             (mapcar 'eval (cdr mail-directory-process))))))

;; This should handle a dialog.  Currently expects port to spit out names.
(defun mail-directory-stream (prefix)
  "Open a stream to retrieve names in directory.
See `mail-directory-stream'."
  (let ((mailalias-done nil)
        (pattern prefix))               ;Dynbind!
    (set-process-sentinel
     (apply 'open-network-stream "mailalias" (current-buffer)
	    mail-directory-stream)
     (lambda (_x _y)
       (setq mailalias-done t)))
    (while (not mailalias-done)
      (sit-for .1))))

(defun mail-sentto-newsgroups ()
  "Return all entries from Newsgroups: header as completion alist."
  (save-excursion
    (if (mail-position-on-field "newsgroups" t)
	(let ((point (point))
	      list)
	  (while (< (skip-chars-backward "^:, \t\n") 0)
	    (setq list `((,(buffer-substring (point) point))
			 ,@list))
	    (skip-chars-backward ", \t\n")
	    (setq point (point)))
	  list))))

(provide 'mailalias)

;;; mailalias.el ends here
