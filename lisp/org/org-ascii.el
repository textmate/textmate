;;; org-ascii.el --- ASCII export for Org-mode

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;;; Code:

(require 'org-exp)

(eval-when-compile
  (require 'cl))

(defgroup org-export-ascii nil
  "Options specific for ASCII export of Org-mode files."
  :tag "Org Export ASCII"
  :group 'org-export)

(defcustom org-export-ascii-underline '(?\- ?\= ?\~ ?^ ?\# ?\$)
  "Characters for underlining headings in ASCII export.
In the given sequence, these characters will be used for level 1, 2, ..."
  :group 'org-export-ascii
  :type '(repeat character))

(defcustom org-export-ascii-bullets '(?* ?+ ?-)
  "Bullet characters for headlines converted to lists in ASCII export.
The first character is used for the first lest level generated in this
way, and so on.  If there are more levels than characters given here,
the list will be repeated.
Note that plain lists will keep the same bullets as the have in the
Org-mode file."
  :group 'org-export-ascii
  :type '(repeat character))

(defcustom org-export-ascii-links-to-notes t
  "Non-nil means convert links to notes before the next headline.
When nil, the link will be exported in place.  If the line becomes long
in this way, it will be wrapped."
  :group 'org-export-ascii
  :type 'boolean)

(defcustom org-export-ascii-table-keep-all-vertical-lines nil
  "Non-nil means keep all vertical lines in ASCII tables.
When nil, vertical lines will be removed except for those needed
for column grouping."
  :group 'org-export-ascii
  :type 'boolean)

(defcustom org-export-ascii-table-widen-columns t
  "Non-nil means widen narrowed columns for export.
When nil, narrowed columns will look in ASCII export just like in org-mode,
i.e. with \"=>\" as ellipsis."
  :group 'org-export-ascii
  :type 'boolean)

(defvar org-export-ascii-entities 'ascii
  "The ascii representation to be used during ascii export.
Possible values are:

ascii     Only use plain ASCII characters
latin1    Include Latin-1 character
utf8      Use all UTF-8 characters")

;;; Hooks

(defvar org-export-ascii-final-hook nil
  "Hook run at the end of ASCII export, in the new buffer.")

;;; ASCII export

(defvar org-ascii-current-indentation nil) ; For communication

;;;###autoload
(defun org-export-as-latin1 (&rest args)
  "Like `org-export-as-ascii', use latin1 encoding for special symbols."
  (interactive)
  (org-export-as-encoding 'org-export-as-ascii (org-called-interactively-p 'any)
			  'latin1 args))

;;;###autoload
(defun org-export-as-latin1-to-buffer (&rest args)
  "Like `org-export-as-ascii-to-buffer', use latin1 encoding for symbols."
  (interactive)
  (org-export-as-encoding 'org-export-as-ascii-to-buffer
			  (org-called-interactively-p 'any) 'latin1 args))

;;;###autoload
(defun org-export-as-utf8 (&rest args)
  "Like `org-export-as-ascii', use encoding for special symbols."
  (interactive)
  (org-export-as-encoding 'org-export-as-ascii
			  (org-called-interactively-p 'any)
			  'utf8 args))

;;;###autoload
(defun org-export-as-utf8-to-buffer (&rest args)
  "Like `org-export-as-ascii-to-buffer', use utf8 encoding for symbols."
  (interactive)
  (org-export-as-encoding 'org-export-as-ascii-to-buffer
			  (org-called-interactively-p 'any) 'utf8 args))

(defun org-export-as-encoding (command interactivep encoding &rest args)
  (let ((org-export-ascii-entities encoding))
    (if interactivep
	(call-interactively command)
      (apply command args))))


;;;###autoload
(defun org-export-as-ascii-to-buffer (arg)
  "Call `org-export-as-ascii` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-ascii'."
  (interactive "P")
  (org-export-as-ascii arg nil nil "*Org ASCII Export*")
  (when org-export-show-temporary-export-buffer
    (switch-to-buffer-other-window "*Org ASCII Export*")))

;;;###autoload
(defun org-replace-region-by-ascii (beg end)
  "Assume the current region has org-mode syntax, and convert it to plain ASCII.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in a Mail buffer and then use this
command to convert it."
  (interactive "r")
  (let (reg ascii buf pop-up-frames)
    (save-window-excursion
      (if (eq major-mode 'org-mode)
	  (setq ascii (org-export-region-as-ascii
		      beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq ascii (org-export-region-as-ascii
		      (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert ascii)))

;;;###autoload
(defun org-export-region-as-ascii (beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to plain ASCII.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted ASCII.  If BUFFER is the symbol `string', return the
produced ASCII as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq ascii (org-export-region-as-ascii beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (when (org-called-interactively-p 'any)
    (setq buffer "*Org ASCII Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
	ext-plist rtn)
    (setq ext-plist (plist-put ext-plist :ignore-subtree-p t))
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-ascii
	       nil nil ext-plist
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (org-called-interactively-p 'any) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

;;;###autoload
(defun org-export-as-ascii (arg &optional hidden ext-plist
			       to-buffer body-only pub-dir)
  "Export the outline as a pretty ASCII file.
If there is an active region, export only the region.
The prefix ARG specifies how many levels of the outline should become
underlined headlines, default is 3.    Lower levels will become bulleted
lists.  When HIDDEN is non-nil, don't display the ASCII buffer.
EXT-PLIST is a property list with external parameters overriding
org-mode's default settings, but still inferior to file-local
settings.  When TO-BUFFER is non-nil, create a buffer with that
name and export to that buffer.  If TO-BUFFER is the symbol
`string', don't leave any buffer behind but just return the
resulting ASCII as a string.  When BODY-ONLY is set, don't produce
the file header and footer.  When PUB-DIR is set, use this as the
publishing directory."
  (interactive "P")
  (run-hooks 'org-export-first-hook)
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (let* ((opt-plist (org-combine-plists (org-default-export-plist)
					ext-plist
					(org-infile-export-plist)))
	 (region-p (org-region-active-p))
	 (rbeg (and region-p (region-beginning)))
	 (rend (and region-p (region-end)))
	 (subtree-p
	  (if (plist-get opt-plist :ignore-subtree-p)
	      nil
	    (when region-p
	      (save-excursion
		(goto-char rbeg)
		(and (org-at-heading-p)
		     (>= (org-end-of-subtree t t) rend))))))
	 (level-offset (if subtree-p
			   (save-excursion
			     (goto-char rbeg)
			     (+ (funcall outline-level)
				(if org-odd-levels-only 1 0)))
			 0))
	 (opt-plist (setq org-export-opt-plist
			  (if subtree-p
			      (org-export-add-subtree-options opt-plist rbeg)
			    opt-plist)))
	 ;; The following two are dynamically scoped into other
	 ;; routines below.
	 (org-current-export-dir
	  (or pub-dir (org-export-directory :html opt-plist)))
	 (org-current-export-file buffer-file-name)
	 (custom-times org-display-custom-times)
	 (org-ascii-current-indentation '(0 . 0))
	 (level 0) line txt
	 (umax nil)
	 (umax-toc nil)
	 (case-fold-search nil)
	 (bfname (buffer-file-name (or (buffer-base-buffer) (current-buffer))))
	 (filename (if to-buffer
		       nil
		     (concat (file-name-as-directory
			      (or pub-dir
				  (org-export-directory :ascii opt-plist)))
			     (file-name-sans-extension
			      (or (and subtree-p
				       (org-entry-get (region-beginning)
						      "EXPORT_FILE_NAME" t))
				  (file-name-nondirectory bfname)))
			     ".txt")))
	 (filename (and filename
			(if (equal (file-truename filename)
				   (file-truename bfname))
			    (concat filename ".txt")
			  filename)))
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string)
		       (get-buffer-create "*Org ASCII Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
	 (org-levels-open (make-vector org-level-max nil))
	 (odd org-odd-levels-only)
	 (date  (plist-get opt-plist :date))
	 (author (plist-get opt-plist :author))
	 (title (or (and subtree-p (org-export-get-title-from-subtree))
		    (plist-get opt-plist :title)
		    (and (not
			  (plist-get opt-plist :skip-before-1st-heading))
			 (org-export-grab-title-from-buffer))
		    (and (buffer-file-name)
			 (file-name-sans-extension
			  (file-name-nondirectory bfname)))
		    "UNTITLED"))
	 (email (plist-get opt-plist :email))
	 (language (plist-get opt-plist :language))
	 (quote-re0 (concat "^\\(" org-quote-string "\\)\\( +\\|[ \t]*$\\)"))
	 (todo nil)
	 (lang-words nil)
	 (region
	  (buffer-substring
	   (if (org-region-active-p) (region-beginning) (point-min))
	   (if (org-region-active-p) (region-end) (point-max))))
	 (org-export-footnotes-seen nil)
	 (org-export-footnotes-data (org-footnote-all-labels 'with-defs))
	 (lines (org-split-string
		 (org-export-preprocess-string
		  region
		  :for-backend 'ascii
		  :skip-before-1st-heading
		  (plist-get opt-plist :skip-before-1st-heading)
		  :drawers (plist-get opt-plist :drawers)
		  :tags (plist-get opt-plist :tags)
		  :priority (plist-get opt-plist :priority)
		  :footnotes (plist-get opt-plist :footnotes)
		  :timestamps (plist-get opt-plist :timestamps)
		  :todo-keywords (plist-get opt-plist :todo-keywords)
		  :tasks (plist-get opt-plist :tasks)
		  :verbatim-multiline t
		  :select-tags (plist-get opt-plist :select-tags)
		  :exclude-tags (plist-get opt-plist :exclude-tags)
		  :archived-trees
		  (plist-get opt-plist :archived-trees)
		  :add-text (plist-get opt-plist :text))
		 "\n"))
	 thetoc have-headings first-heading-pos
	 table-open table-buffer link-buffer link type path desc desc0 rpl wrap fnc)
    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (setq org-min-level (org-get-min-level lines level-offset))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)
    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
    (set-buffer buffer)
    (erase-buffer)
    (fundamental-mode)
    (org-install-letbind)
    ;; create local variables for all options, to make sure all called
    ;; functions get the correct information
    (mapc (lambda (x)
	    (set (make-local-variable (nth 2 x))
		 (plist-get opt-plist (car x))))
	  org-export-plist-vars)
    (org-set-local 'org-odd-levels-only odd)
    (setq umax (if arg (prefix-numeric-value arg)
		 org-export-headline-levels))
    (setq umax-toc (if (integerp org-export-with-toc)
		       (min org-export-with-toc umax)
		     umax))

    ;; File header
    (unless body-only
      (when (and title (not (string= "" title)))
	(org-insert-centered title ?=)
	(insert "\n"))

      (if (and (or author email)
	       org-export-author-info)
	  (insert (concat (nth 1 lang-words) ": " (or author "")
			  (if (and org-export-email-info
				   email (string-match "\\S-" email))
			      (concat " <" email ">") "")
			  "\n")))

      (cond
       ((and date (string-match "%" date))
	(setq date (format-time-string date)))
       (date)
       (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

      (if (and date org-export-time-stamp-file)
	  (insert (concat (nth 2 lang-words) ": " date"\n")))

      (unless (= (point) (point-min))
	(insert "\n\n")))

    (if (and org-export-with-toc (not body-only))
	(progn
	  (push (concat (nth 3 lang-words) "\n") thetoc)
	  (push (concat (make-string (string-width (nth 3 lang-words)) ?=)
			"\n") thetoc)
	  (mapc #'(lambda (line)
		   (if (string-match org-todo-line-regexp
				     line)
		       ;; This is a headline
		       (progn
			 (setq have-headings t)
			 (setq level (- (match-end 1) (match-beginning 1)
					level-offset)
			       level (org-tr-level level)
			       txt (match-string 3 line)
			       todo
			       (or (and org-export-mark-todo-in-toc
					(match-beginning 2)
					(not (member (match-string 2 line)
						     org-done-keywords)))
					; TODO, not DONE
				   (and org-export-mark-todo-in-toc
					(= level umax-toc)
					(org-search-todo-below
					 line lines level))))
			 (setq txt (org-html-expand-for-ascii txt))

			 (while (string-match org-bracket-link-regexp txt)
			   (setq txt
				 (replace-match
				  (match-string (if (match-end 2) 3 1) txt)
				  t t txt)))

			 (if (and (memq org-export-with-tags '(not-in-toc nil))
				  (string-match
				   (org-re "[ \t]+:[[:alnum:]_@#%:]+:[ \t]*$")
				   txt))
			     (setq txt (replace-match "" t t txt)))
			 (if (string-match quote-re0 txt)
			     (setq txt (replace-match "" t t txt 1)))

			 (if org-export-with-section-numbers
			     (setq txt (concat (org-section-number level)
					       " " txt)))
			 (if (<= level umax-toc)
			     (progn
			       (push
				(concat
				 (make-string
				  (* (max 0 (- level org-min-level)) 4) ?\ )
				 (format (if todo "%s (*)\n" "%s\n") txt))
				thetoc)
			       (setq org-last-level level))
			   ))))
		lines)
	  (setq thetoc (if have-headings (nreverse thetoc) nil))))

    (org-init-section-numbers)
    (while (setq line (pop lines))
      (when (and link-buffer (string-match org-outline-regexp-bol line))
	(org-export-ascii-push-links (nreverse link-buffer))
	(setq link-buffer nil))
      (setq wrap nil)
      ;; Remove the quoted HTML tags.
      (setq line (org-html-expand-for-ascii line))
      ;; Replace links with the description when possible
      (while (string-match org-bracket-link-analytic-regexp++ line)
	(setq path (match-string 3 line)
	      link (concat (match-string 1 line) path)
	      type (match-string 2 line)
	      desc0 (match-string 5 line)
	      desc0 (replace-regexp-in-string "\\\\_" "_" desc0)
	      desc (or desc0 link)
	      desc (replace-regexp-in-string "\\\\_" "_" desc))
	(if (and (> (length link) 8)
		 (equal (substring link 0 8) "coderef:"))
	    (setq line (replace-match
			(format (org-export-get-coderef-format (substring link 8) desc)
				(cdr (assoc
				      (substring link 8)
				      org-export-code-refs)))
			t t line))
	  (setq rpl (concat "[" desc "]"))
	  (if (functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
	      (setq rpl (or (save-match-data
			      (funcall fnc (org-link-unescape path)
				       desc0 'ascii))
			    rpl))
	    (when (and desc0 (not (equal desc0 link)))
	      (if org-export-ascii-links-to-notes
		  (push (cons desc0 link) link-buffer)
		(setq rpl (concat rpl " (" link ")")
		      wrap (+ (length line) (- (length (match-string 0 line)))
			      (length desc))))))
	  (setq line (replace-match rpl t t line))))
      (when custom-times
	(setq line (org-translate-time line)))
      (cond
       ((string-match "^\\(\\*+\\)[ \t]+\\(.*\\)" line)
	;; a Headline
	(setq first-heading-pos (or first-heading-pos (point)))
	(setq level (org-tr-level (- (match-end 1) (match-beginning 1)
				     level-offset))
	      txt (match-string 2 line))
	(org-ascii-level-start level txt umax lines))

       ((and org-export-with-tables
	     (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	(if (not table-open)
	    ;; New table starts
	    (setq table-open t table-buffer nil))
	;; Accumulate lines
	(setq table-buffer (cons line table-buffer))
	(when (or (not lines)
		  (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
				     (car lines))))
	  (setq table-open nil
		table-buffer (nreverse table-buffer))
	  (insert (mapconcat
		   (lambda (x)
		     (org-fix-indentation x org-ascii-current-indentation))
		   (org-format-table-ascii table-buffer)
		   "\n") "\n")))
       (t
	(if (string-match "^\\([ \t]*\\)\\([-+*][ \t]+\\)\\(.*?\\)\\( ::\\)"
			  line)
	    (setq line (replace-match "\\1\\3:" t nil line)))
	(setq line (org-fix-indentation line org-ascii-current-indentation))
	;; Remove forced line breaks
	(if (string-match "\\\\\\\\[ \t]*$" line)
	    (setq line (replace-match "" t t line)))
	(if (and org-export-with-fixed-width
		 (string-match "^\\([ \t]*\\)\\(:\\( \\|$\\)\\)" line))
	    (setq line (replace-match "\\1" nil nil line))
	  (if wrap (setq line (org-export-ascii-wrap line wrap))))
	(insert line "\n"))))

    (org-export-ascii-push-links (nreverse link-buffer))

    (normal-mode)

    ;; insert the table of contents
    (when thetoc
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*\\[TABLE-OF-CONTENTS\\][ \t]*$" nil t)
	  (progn
	    (goto-char (match-beginning 0))
	    (replace-match ""))
	(goto-char first-heading-pos))
      (mapc 'insert thetoc)
      (or (looking-at "[ \t]*\n[ \t]*\n")
	  (insert "\n\n")))

    ;; Convert whitespace place holders
    (goto-char (point-min))
    (let (beg end)
      (while (setq beg (next-single-property-change (point) 'org-whitespace))
	(setq end (next-single-property-change beg 'org-whitespace))
	(goto-char beg)
	(delete-region beg end)
	(insert (make-string (- end beg) ?\ ))))

    ;; remove display and invisible chars
    (let (beg end)
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'display))
	(setq end (next-single-property-change beg 'display))
	(delete-region beg end)
	(goto-char beg)
	(insert "=>"))
      (goto-char (point-min))
      (while (setq beg (next-single-property-change (point) 'org-cwidth))
	(setq end (next-single-property-change beg 'org-cwidth))
	(delete-region beg end)
	(goto-char beg)))
    (run-hooks 'org-export-ascii-final-hook)
    (or to-buffer (save-buffer))
    (goto-char (point-min))
    (or (org-export-push-to-kill-ring "ASCII")
	(message "Exporting... done"))
    ;; Return the buffer or a string, according to how this function was called
    (if (eq to-buffer 'string)
	(prog1 (buffer-substring (point-min) (point-max))
	  (kill-buffer (current-buffer)))
      (current-buffer))))

(defun org-export-ascii-preprocess (parameters)
  "Do extra work for ASCII export."
  ;;
  ;; Realign tables to get rid of narrowing
  (when org-export-ascii-table-widen-columns
    (let ((org-table-do-narrow nil))
      (goto-char (point-min))
      (org-ascii-replace-entities)
      (goto-char (point-min))
      (org-table-map-tables
       (lambda () (org-if-unprotected (org-table-align)))
       'quietly)))
  ;; Put quotes around verbatim text
  (goto-char (point-min))
  (while (re-search-forward org-verbatim-re nil t)
    (org-if-unprotected-at (match-beginning 4)
      (goto-char (match-end 2))
      (backward-delete-char 1) (insert "'")
      (goto-char (match-beginning 2))
      (delete-char 1) (insert "`")
      (goto-char (match-end 2))))
  ;; Remove target markers
  (goto-char (point-min))
  (while (re-search-forward  "<<<?\\([^<>]*\\)>>>?\\([ \t]*\\)" nil t)
    (org-if-unprotected-at (match-beginning 1)
      (replace-match "\\1\\2")))
  ;; Remove list start counters
  (goto-char (point-min))
  (while (org-list-search-forward
	  "\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*" nil t)
    (replace-match ""))
  (remove-text-properties
   (point-min) (point-max)
   '(face nil font-lock-fontified nil font-lock-multiline nil line-prefix nil wrap-prefix nil)))

(defun org-html-expand-for-ascii (line)
  "Handle quoted HTML for ASCII export."
  (if org-export-html-expand
      (while (string-match "@<[^<>\n]*>" line)
	;; We just remove the tags for now.
	(setq line (replace-match "" nil nil line))))
  line)

(defun org-ascii-replace-entities ()
  "Replace entities with the ASCII representation."
  (let (e)
    (while (re-search-forward "\\\\\\([a-zA-Z]+[0-9]*\\)\\({}\\)?" nil t)
      (org-if-unprotected-at (match-beginning 1)
	(setq e (org-entity-get-representation (match-string 1)
					       org-export-ascii-entities))
	(and e (replace-match e t t))))))

(defun org-export-ascii-wrap (line where)
  "Wrap LINE at or before WHERE."
  (let ((ind (org-get-indentation line))
	pos)
    (catch 'found
      (loop for i from where downto (/ where 2) do
	    (and (equal (aref line i) ?\ )
		 (setq pos i)
		 (throw 'found t))))
    (if pos
	(concat (substring line 0 pos) "\n"
		(make-string ind ?\ )
		(substring line (1+ pos)))
      line)))

(defun org-export-ascii-push-links (link-buffer)
  "Push out links in the buffer."
  (when link-buffer
    ;; We still have links to push out.
    (insert "\n")
    (let ((ind ""))
      (save-match-data
	(if (save-excursion
	      (re-search-backward
	       (concat "^\\(\\([ \t]*\\)\\|\\("
		       org-outline-regexp
		       "\\)\\)[^ \t\n]") nil t))
	    (setq ind (or (match-string 2)
			  (make-string (length (match-string 3)) ?\ )))))
      (mapc (lambda (x) (insert ind "[" (car x) "]: " (cdr x) "\n"))
	    link-buffer))
    (insert "\n")))

(defun org-ascii-level-start (level title umax &optional lines)
  "Insert a new level in ASCII export."
  (let (char (n (- level umax 1)) (ind 0))
    (if (> level umax)
	(progn
	  (insert (make-string (* 2 n) ?\ )
		  (char-to-string (nth (% n (length org-export-ascii-bullets))
				       org-export-ascii-bullets))
		  " " title "\n")
	  ;; find the indentation of the next non-empty line
	  (catch 'stop
	    (while lines
	      (if (string-match "^\\* " (car lines)) (throw 'stop nil))
	      (if (string-match "^\\([ \t]*\\)\\S-" (car lines))
		  (throw 'stop (setq ind (org-get-indentation (car lines)))))
	      (pop lines)))
	  (setq org-ascii-current-indentation (cons (* 2 (1+ n)) ind)))
      (if (or (not (equal (char-before) ?\n))
	      (not (equal (char-before (1- (point))) ?\n)))
	  (insert "\n"))
      (setq char (or (nth (1- level) org-export-ascii-underline)
      		     (car (last org-export-ascii-underline))))
      (unless org-export-with-tags
	(if (string-match (org-re "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$") title)
	    (setq title (replace-match "" t t title))))
      (if org-export-with-section-numbers
	  (setq title (concat (org-section-number level) " " title)))
      (insert title "\n" (make-string (string-width title) char) "\n")
      (setq org-ascii-current-indentation '(0 . 0)))))

(defun org-insert-centered (s &optional underline)
  "Insert the string S centered and underline it with character UNDERLINE."
  (let ((ind (max (/ (- fill-column (string-width s)) 2) 0)))
    (insert (make-string ind ?\ ) s "\n")
    (if underline
	(insert (make-string ind ?\ )
		(make-string (string-width s) underline)
		"\n"))))

(defvar org-table-colgroup-info nil)
(defun org-format-table-ascii (lines)
  "Format a table for ascii export."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (not (string-match "^[ \t]*|" (car lines)))
      ;; Table made by table.el - test for spanning
      lines

    ;; A normal org table
    ;; Get rid of hlines at beginning and end
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
    (setq lines (nreverse lines))
    (when org-export-table-remove-special-lines
      ;; Check if the table has a marking column.  If yes remove the
      ;; column and the special lines
      (setq lines (org-table-clean-before-export lines)))
    ;; Get rid of the vertical lines except for grouping
    (if org-export-ascii-table-keep-all-vertical-lines
	lines
      (let ((vl (org-colgroup-info-to-vline-list org-table-colgroup-info))
	    rtn line vl1 start)
	(while (setq line (pop lines))
	  (if (string-match org-table-hline-regexp line)
	      (and (string-match "|\\(.*\\)|" line)
		   (setq line (replace-match " \\1" t nil line)))
	    (setq start 0 vl1 vl)
	    (while (string-match "|" line start)
	      (setq start (match-end 0))
	      (or (pop vl1) (setq line (replace-match " " t t line)))))
	  (push line rtn))
	(nreverse rtn)))))

(defun org-colgroup-info-to-vline-list (info)
  (let (vl new last)
    (while info
      (setq last new new (pop info))
      (if (or (memq last '(:end :startend))
	      (memq new  '(:start :startend)))
	  (push t vl)
	(push nil vl)))
    (setq vl (nreverse vl))
    (and vl (setcar vl nil))
    vl))

(provide 'org-ascii)

;;; org-ascii.el ends here
