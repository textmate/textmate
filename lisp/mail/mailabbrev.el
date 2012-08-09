;;; mailabbrev.el --- abbrev-expansion of mail aliases

;; Copyright (C) 1985-1987, 1992-1993, 1996-1997, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com; now jwz@jwz.org>
;; Maintainer: FSF
;; Created: 19 Oct 90
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

;; This file ensures that, when the point is in a To:, CC:, BCC:, or From:
;; field, word-abbrevs are defined for each of your mail aliases.  These
;; aliases will be defined from your .mailrc file (or the file specified by
;; `mail-personal-alias-file') if it exists.  Your mail aliases will
;; expand any time you type a word-delimiter at the end of an abbreviation.
;;
;; What you see is what you get: if mailabbrev is in use when you type
;; a name, and the name does not expand, you know it is not an abbreviation.
;; However, if you yank abbreviations into the headers
;; in a way that bypasses the check for abbreviations,
;; they are expanded (but not visibly) when you send the message.
;;
;; Your mail alias abbrevs will be in effect only when the point is in an
;; appropriate header field.  When in the body of the message, or other
;; header fields, the mail aliases will not expand.  Rather, the normal
;; mode-specific abbrev table will be used if
;; defined.  So if you use mail-mode specific abbrevs, this code will not
;; adversely affect you.  You can control which header fields the abbrevs
;; are used in by changing the variable mail-abbrev-mode-regexp.
;;
;; If auto-fill mode is on, abbrevs will wrap at commas instead of at word
;; boundaries; also, header continuation-lines will be properly indented.
;;
;; You can also insert a mail alias with mail-abbrev-insert-alias
;; (bound to C-c C-a), which prompts you for an alias (with completion)
;; and inserts its expansion at point.
;;
;; This file fixes a bug in the old system which prohibited your .mailrc
;; file from having lines like
;;
;;     alias someone "John Doe <doe@quux.com>"
;;
;; That is, if you want an address to have embedded spaces, simply surround it
;; with double-quotes.  This is necessary because the format of the .mailrc
;; file bogusly uses spaces as address delimiters.  The following line defines
;; an alias which expands to three addresses:
;;
;;     alias foobar addr-1 addr-2 "address three <addr-3>"
;;
;; (This is bogus because mail-delivery programs want commas, not spaces,
;; but that's what the file format is, so we have to live with it.)
;;
;; If you like, you can call the function define-mail-abbrev to define your
;; mail aliases instead of using a .mailrc file.  When you call it in this
;; way, addresses are separated by commas.
;;
;; CAVEAT: This works on most Sun systems; I have been told that some versions
;; of /bin/mail do not understand double-quotes in the .mailrc file.  So you
;; should make sure your version does before including verbose addresses like
;; this.  One solution to this, if you are on a system whose /bin/mail doesn't
;; work that way, (and you still want to be able to /bin/mail to send mail in
;; addition to emacs) is to define minimal aliases (without full names) in
;; your .mailrc file, and use define-mail-abbrev to redefine them when sending
;; mail from emacs; this way, mail sent from /bin/mail will work, and mail
;; sent from emacs will be pretty.
;;
;; Aliases in the mailrc file may be nested.  If you define aliases like
;;     alias group1 fred ethel
;;     alias group2 larry curly moe
;;     alias everybody group1 group2
;; Then when you type "everybody" on the To: line, it will be expanded to
;;     fred, ethyl, larry, curly, moe
;;
;; Aliases may also contain forward references; the alias of "everybody" can
;; precede the aliases of "group1" and "group2".
;;
;; This code also understands the "source" .mailrc command, for reading
;; aliases from some other file as well.
;;
;; Aliases may contain hyphens, as in "alias foo-bar foo@bar"; word-abbrevs
;; normally cannot contain hyphens, but this code works around that for the
;; specific case of mail-alias word-abbrevs.
;;
;; To read in the contents of another .mailrc-type file from emacs, use the
;; command Meta-X merge-mail-abbrevs.  The rebuild-mail-abbrevs command is
;; similar, but will delete existing aliases first.
;;
;; If you would like your aliases to be expanded when you type M-> or ^N to
;; move out of the mail-header into the message body (instead of having to
;; type SPC at the end of the abbrev before moving away) then you can do
;;
;;  (add-hook
;;   'mail-mode-hook
;;   (lambda ()
;;      (define-key mail-mode-map [remap next-line] 'mail-abbrev-next-line)
;;      (define-key mail-mode-map [remap end-of-buffer] 'mail-abbrev-end-of-buffer)))
;;
;; If you want multiple addresses separated by a string other than ", " then
;; you can set the variable mail-alias-separator-string to it.  This has to
;; be a comma bracketed by whitespace if you want any kind of reasonable
;; behavior.
;;
;; Thanks to Harald Hanche-Olsen, Michael Ernst, David Loeffler, and
;; Noah Friedman for suggestions and bug reports.

;; To use this package, do (add-hook 'mail-mode-hook 'mail-abbrevs-setup).

;;; Code:

(eval-when-compile
  (require 'sendmail))

(defgroup mail-abbrev nil
  "Expand mail aliases as abbrevs, in certain mail headers."
  :group 'abbrev-mode)

;;;###autoload
(define-minor-mode mail-abbrevs-mode
  "Toggle abbrev expansion of mail aliases (Mail Abbrevs mode).
With a prefix argument ARG, enable Mail Abbrevs mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Mail Abbrevs mode is a global minor mode.  When enabled,
abbrev-like expansion is performed when editing certain mail
headers (those specified by `mail-abbrev-mode-regexp'), based on
the entries in your `mail-personal-alias-file'."
  :global t
  :group 'mail-abbrev
  :version "20.3"
  (if mail-abbrevs-mode (mail-abbrevs-enable) (mail-abbrevs-disable)))

(defcustom mail-abbrevs-only nil
  "Non-nil means only mail abbrevs should expand automatically.
Other abbrevs expand only when you explicitly use `expand-abbrev'."
  :type 'boolean
  :group 'mail-abbrev)

;; originally defined in sendmail.el - used to be an alist, now is a table.
(defvar mail-abbrevs nil
  "Word-abbrev table of mail address aliases.
If this is nil, it means the aliases have not yet been initialized and
should be read from the .mailrc file.  (This is distinct from there being
no aliases, which is represented by this being a table with no entries.)")

(defvar mail-abbrev-modtime nil
  "The modification time of your mail alias file when it was last examined.")

(defun mail-abbrevs-sync-aliases ()
  (when mail-personal-alias-file
    (if (file-exists-p mail-personal-alias-file)
	(let ((modtime (nth 5 (file-attributes mail-personal-alias-file))))
	  (if (not (equal mail-abbrev-modtime modtime))
	      (progn
		(setq mail-abbrev-modtime modtime)
		(build-mail-abbrevs)))))))

;;;###autoload
(defun mail-abbrevs-setup ()
  "Initialize use of the `mailabbrev' package."
  (if (and (not (vectorp mail-abbrevs))
	   (file-exists-p mail-personal-alias-file))
      (progn
	(setq mail-abbrev-modtime
	      (nth 5 (file-attributes mail-personal-alias-file)))
	(build-mail-abbrevs)))
  (mail-abbrevs-sync-aliases)
  (add-hook 'abbrev-expand-functions 'mail-abbrev-expand-wrapper nil t)
  (abbrev-mode 1))

(defun mail-abbrevs-enable ()
  (add-hook 'mail-mode-hook 'mail-abbrevs-setup))

(defun mail-abbrevs-disable ()
  "Turn off use of the `mailabbrev' package."
  (remove-hook 'mail-mode-hook 'mail-abbrevs-setup)
  (abbrev-mode (if (default-value 'abbrev-mode) 1 -1)))

;;;###autoload
(defun build-mail-abbrevs (&optional file recursivep)
  "Read mail aliases from personal mail alias file and set `mail-abbrevs'.
By default this is the file specified by `mail-personal-alias-file'."
  (setq file (expand-file-name (or file mail-personal-alias-file)))
  (if (vectorp mail-abbrevs)
      nil
    (setq mail-abbrevs nil)
    (define-abbrev-table 'mail-abbrevs '()))
  (message "Parsing %s..." file)
  (with-temp-buffer
    (buffer-disable-undo)
    (cond ((get-file-buffer file)
           (insert (with-current-buffer (get-file-buffer file)
                     (buffer-substring (point-min) (point-max)))))
          ((not (file-exists-p file)))
          (t (insert-file-contents file)))
    ;; Don't lose if no final newline.
    (goto-char (point-max))
    (or (eq (preceding-char) ?\n) (newline))
    (goto-char (point-min))
    ;; Delete comments from the file
    (while (search-forward "# " nil t)
      (let ((p (- (point) 2)))
        (end-of-line)
        (delete-region p (point))))
    (goto-char (point-min))
    ;; handle "\\\n" continuation lines
    (while (not (eobp))
      (end-of-line)
      (if (= (preceding-char) ?\\)
          (progn (delete-char -1) (delete-char 1) (insert ?\ ))
        (forward-char 1)))
    (goto-char (point-min))
    (while (re-search-forward
            "^\\(a\\(lias\\)?\\|g\\(roup\\)?\\|source\\)[ \t]+" nil t)
      (beginning-of-line)
      (if (looking-at "source[ \t]+\\([^ \t\n]+\\)")
          (progn
            (end-of-line)
            (build-mail-abbrevs
             (substitute-in-file-name
              (buffer-substring (match-beginning 1) (match-end 1)))
             t))
        (re-search-forward "[ \t]+\\([^ \t\n]+\\)")
        (let* ((name (buffer-substring
                      (match-beginning 1) (match-end 1)))
               (start (progn (skip-chars-forward " \t") (point))))
          (end-of-line)
          ;; (message "** %s \"%s\"" name (buffer-substring start (point)))(sit-for 1)
          (define-mail-abbrev
            name
            (buffer-substring start (point))
            t))))
    ;; Resolve forward references in .mailrc file.
    ;; This would happen automatically before the first abbrev was
    ;; expanded, but why not do it now.
    (or recursivep (mail-resolve-all-aliases))
    mail-abbrevs)
  (message "Parsing %s... done" file))

(defvar mail-alias-separator-string ", "
  "*A string inserted between addresses in multi-address mail aliases.
This has to contain a comma, so \", \" is a reasonable value.  You might
also want something like \",\\n    \" to get each address on its own line.")

;; define-mail-abbrev sets this flag, which causes mail-resolve-all-aliases
;; to be called before expanding abbrevs if it's necessary.
(defvar mail-abbrev-aliases-need-to-be-resolved t)

;; originally defined in mailalias.el ; build-mail-abbrevs calls this with
;; stuff parsed from the .mailrc file.
;;
;;;###autoload
(defun define-mail-abbrev (name definition &optional from-mailrc-file)
  "Define NAME as a mail alias abbrev that translates to DEFINITION.
If DEFINITION contains multiple addresses, separate them with commas.

Optional argument FROM-MAILRC-FILE means that DEFINITION comes
from a mailrc file.  In that case, addresses are separated with
spaces and addresses with embedded spaces are surrounded by
double-quotes."
  ;; When this is called from build-mail-abbrevs, the third argument is
  ;; true, and we do some evil space->comma hacking like /bin/mail does.
  (interactive "sDefine mail alias: \nsDefine %s as mail alias for: ")
  ;; Read the defaults first, if we have not done so.
  (unless (vectorp mail-abbrevs) (build-mail-abbrevs))
  ;; strip garbage from front and end
  (if (string-match "\\`[ \t\n,]+" definition)
      (setq definition (substring definition (match-end 0))))
  (if (string-match "[ \t\n,]+\\'" definition)
      (setq definition (substring definition 0 (match-beginning 0))))
  (let* ((L (length definition))
	 (start (if (> L 0) 0))
	 end this-entry result)
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
			       this-entry)))
	(push this-entry result))
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
	(push (match-string 1 definition) result)
	(setq start (and (/= (match-end 0) L) (match-end 0))))
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
			       this-entry)))
	(push this-entry result))))

    (setq definition (mapconcat (function identity)
				(nreverse result)
				mail-alias-separator-string)))
  (setq mail-abbrev-aliases-need-to-be-resolved t)
  (setq name (downcase name))
  ;; use an abbrev table instead of an alist for mail-abbrevs.
  (let ((abbrevs-changed abbrevs-changed))  ; protect this from being changed.
    (define-abbrev mail-abbrevs name definition 'mail-abbrev-expand-hook 0 t)))


(defun mail-resolve-all-aliases ()
  "Resolve all forward references in the mail aliases table."
  (if mail-abbrev-aliases-need-to-be-resolved
      (progn
;;	(message "Resolving mail aliases...")
	(if (vectorp mail-abbrevs)
	    (mapatoms (function mail-resolve-all-aliases-1) mail-abbrevs))
	(setq mail-abbrev-aliases-need-to-be-resolved nil)
;;	(message "Resolving mail aliases... done.")
	)))

(defun mail-resolve-all-aliases-1 (sym &optional so-far)
  (if (memq sym so-far)
      (error "mail alias loop detected: %s"
	     (mapconcat 'symbol-name (cons sym so-far) " <- ")))
  (let ((definition (and (boundp sym) (symbol-value sym))))
    (if definition
	(let ((result '())
	      (start 0))
	  (while start
	    (let ((end (string-match "[ \t\n]*,[, \t\n]*" definition start)))
	      (setq result (cons (substring definition start end) result)
		    start (and end (match-end 0)))))
	  (setq definition
		(mapconcat (function (lambda (x)
			     (or (mail-resolve-all-aliases-1
				   (intern-soft (downcase x) mail-abbrevs)
				   (cons sym so-far))
				 x)))
			   (nreverse result)
			   mail-alias-separator-string))
	  (set sym definition))))
  (symbol-value sym))


(defun mail-abbrev-expand-hook ()
  "For use as the fourth arg to `define-abbrev'.
After expanding a mail-abbrev, if Auto Fill mode is on and we're past the
fill-column, break the line at the previous comma, and indent the next line."
  ;; Disable abbrev mode to avoid recursion in indent-relative expanding
  ;; part of the abbrev expansion as an abbrev itself.
  (let ((abbrev-mode nil))
    (save-excursion
      (let ((p (point))
	    bol comma fp)
	(beginning-of-line)
	(setq bol (point))
	(goto-char p)
	(while (and auto-fill-function
		    (>= (current-column) fill-column)
		    (search-backward "," bol t))
	  (setq comma (point))
	  (forward-char 1)		; Now we are just past the comma.
	  (insert "\n")
	  (delete-horizontal-space)
	  (setq p (point))
	  (indent-relative)
	  (setq fp (buffer-substring p (point)))
	  ;; Go to the end of the new line.
	  (end-of-line)
	  (if (> (current-column) fill-column)
	      ;; It's still too long; do normal auto-fill.
	      (let ((fill-prefix (or fp "\t")))
		(do-auto-fill)))
	  ;; Resume the search.
	  (goto-char comma)
	  )))))

;;; Syntax tables and abbrev-expansion

(defvar mail-abbrev-mode-regexp
  "^\\(Resent-\\)?\\(To\\|From\\|CC\\|BCC\\|Reply-to\\):"
  "*Regexp to select mail-headers in which mail abbrevs should be expanded.
This string will be handed to `looking-at' with point at the beginning
of the current line; if it matches, abbrev mode will be turned on, otherwise
it will be turned off.  (You don't need to worry about continuation lines.)
This should be set to match those mail fields in which you want abbreviations
turned on.")

(defvar mail-abbrev-syntax-table nil
  "The syntax-table used for abbrev-expansion purposes.
This is not actually made the current syntax table of the buffer, but
simply controls the set of characters which may be a part of the name
of a mail alias.  The value is set up, buffer-local, when first needed.")

(defun mail-abbrev-make-syntax-table ()
  (make-local-variable 'mail-abbrev-syntax-table)
  (unless mail-abbrev-syntax-table
    (let ((tab (copy-syntax-table (syntax-table)))
	  (_ (aref (standard-syntax-table) ?_))
	  (w (aref (standard-syntax-table) ?w)))
      (map-char-table
       (function (lambda (key value)
		   (if (null value)
		       ;; Fetch the inherited value
		       (setq value (aref tab key)))
		   (if (equal value _)
		       (set-char-table-range tab key w))))
       tab)
      (modify-syntax-entry ?@ "w" tab)
      (modify-syntax-entry ?% "w" tab)
      (modify-syntax-entry ?! "w" tab)
      (modify-syntax-entry ?. "w" tab)
      (modify-syntax-entry ?_ "w" tab)
      (modify-syntax-entry ?- "w" tab)
      (setq mail-abbrev-syntax-table tab))))

(defun mail-abbrev-in-expansion-header-p ()
  "Whether point is in a mail-address header field."
  (let ((case-fold-search t))
    (and ;;
         ;; we are on an appropriate header line...
     (save-excursion
       (unless (eobp) (forward-char 1))
       (re-search-backward "^[^ \t]" nil 'move)
       ;; are we at the front of an appropriate header line?
       (looking-at mail-abbrev-mode-regexp))
     ;;
     ;; ...and are we in the headers?
     (< (point)
	(save-restriction
	  (widen)
	  (save-excursion
	    (rfc822-goto-eoh)
	    (point)))))))

(defun mail-abbrev-expand-wrapper (expand)
  (if (and mail-abbrevs (not (eq mail-abbrevs t)))
      (if (mail-abbrev-in-expansion-header-p)

          ;; We are in a To: (or CC:, or whatever) header, and
          ;; should use word-abbrevs to expand mail aliases.
          (let ((local-abbrev-table mail-abbrevs))

            ;; Before anything else, resolve aliases if they need it.
            (and mail-abbrev-aliases-need-to-be-resolved
                 (mail-resolve-all-aliases))

            ;; Now proceed with the abbrev section.
            ;;   -  We already installed mail-abbrevs as the abbrev table.
            ;;   -  Then install the mail-abbrev-syntax-table, which
            ;;      temporarily marks all of the
            ;;      non-alphanumeric-atom-characters (the "_"
            ;;      syntax ones) as being normal word-syntax.  We do this
            ;;      because the C code for expand-abbrev only works on words,
            ;;      and we want these characters to be considered words for
            ;;      the purpose of abbrev expansion.
            ;;   -  Then we call the expand function, to do
            ;;      the abbrev expansion with the above syntax table.

            (mail-abbrev-make-syntax-table)

            ;; If the character just typed was non-alpha-symbol-syntax,
            ;; then don't expand the abbrev now (that is, don't expand
            ;; when the user types -.)  Check the character's syntax in
            ;; the usual syntax table.

            (or (and (integerp last-command-event)
                     ;; Some commands such as M-> may want to expand first.
                     (equal this-command 'self-insert-command)
                     (or (eq (char-syntax last-command-event) ?_)
                         ;; Don't expand on @.
                         (memq last-command-event '(?@ ?. ?% ?! ?_ ?-))))
                ;; Use this table so that abbrevs can have hyphens in them.
                (with-syntax-table mail-abbrev-syntax-table
                  (funcall expand))))

        (if (or (not mail-abbrevs-only)
                (eq this-command 'expand-abbrev))
            ;; We're not in a mail header where mail aliases should
            ;; be expanded, then use the normal mail-mode abbrev table
            ;; (if any) and the normal mail-mode syntax table.
            (funcall expand)
          ;; This is not a mail abbrev, and we should not expand it.
          ;; Don't expand anything.
          nil))
    ;; No mail-abbrevs at all, do the normal thing.
    (funcall expand)))

;;; utilities

(defun merge-mail-abbrevs (file)
  "Merge mail aliases from the given file with existing ones."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def mail-personal-alias-file))
		  (read-file-name
		   (format "Read additional aliases from file (default %s): "
			    def)
		    default-directory
		    (expand-file-name def default-directory)
		    t))))
  (build-mail-abbrevs file))

(defun rebuild-mail-abbrevs (&optional file)
  "Rebuild all the mail aliases from the given file."
  (interactive (list
		(let ((insert-default-directory t)
		      (default-directory (expand-file-name "~/"))
		      (def mail-personal-alias-file))
		  (read-file-name
		   (format "Read mail aliases from file (default %s): " def)
		   default-directory
		   (expand-file-name def default-directory)
		   t))))
  (if (null file)
      (setq file buffer-file-name))
  (setq mail-abbrevs nil)
  (build-mail-abbrevs file))

(defun mail-abbrev-insert-alias (&optional alias)
  "Prompt for and insert a mail alias."
  (interactive (progn
		(if (not (vectorp mail-abbrevs)) (mail-abbrevs-setup))
		(list (completing-read "Expand alias: " mail-abbrevs nil t))))
  (if (not (vectorp mail-abbrevs)) (mail-abbrevs-setup))
  (insert (or (and alias (symbol-value (intern-soft alias mail-abbrevs))) ""))
  (mail-abbrev-expand-hook))

(defun mail-abbrev-complete-alias ()
  "Perform completion on alias preceding point."
  (interactive)
  (mail-abbrev-make-syntax-table)
  (let ((end (point))
        (beg (with-syntax-table mail-abbrev-syntax-table
               (save-excursion
                 (backward-word 1)
                 (point)))))
    (completion-in-region beg end mail-abbrevs)))

(defun mail-abbrev-next-line (&optional arg)
  "Expand a mail abbrev before point, then move vertically down ARG lines.
This only expands an abbrev (if one is present) if called with
point at the end of a line, or on whitespace before the end of a line.

In terms of line motion, this behaves like `next-line', which see."
  (interactive "p")
  (if (looking-at "[ \t]*\n") (expand-abbrev))
  (setq this-command 'next-line)
  (with-no-warnings (next-line arg)))

(defun mail-abbrev-end-of-buffer (&optional arg)
  "Expand a mail abbrev before point, then move to the end of the buffer.
This only expands an abbrev (if one is present) if called with
point at the end of a line, or on whitespace before the end of a line.

In other respects, this behaves like `end-of-buffer', which see."
  (interactive "P")
  (if (looking-at "[ \t]*\n") (expand-abbrev))
  (setq this-command 'end-of-buffer)
  (with-no-warnings (end-of-buffer arg)))

(eval-after-load "sendmail"
  '(progn
     (define-key mail-mode-map "\C-c\C-a" 'mail-abbrev-insert-alias)
     (define-key mail-mode-map "\e\t"	; like lisp-complete-symbol
       'mail-abbrev-complete-alias)))

;;(define-key mail-mode-map "\C-n" 'mail-abbrev-next-line)
;;(define-key mail-mode-map "\M->" 'mail-abbrev-end-of-buffer)

(provide 'mailabbrev)

;;; mailabbrev.el ends here
