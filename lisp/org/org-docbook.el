;;; org-docbook.el --- DocBook exporter for org-mode
;;
;; Copyright (C) 2007-2012  Free Software Foundation, Inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-docbook.el
;; Author: Baoqiu Cui <cbaoqiu AT yahoo DOT com>
;; Maintainer: Baoqiu Cui <cbaoqiu AT yahoo DOT com>
;; Keywords: org, wp, docbook
;; Description: Converts an org-mode buffer into DocBook
;; URL:

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
;;
;; This library implements a DocBook exporter for org-mode.  The basic
;; idea and design is very similar to what `org-export-as-html' has.
;; Code prototype was also started with `org-export-as-html'.
;;
;; Put this file into your load-path and the following line into your
;; ~/.emacs:
;;
;;   (require 'org-docbook)
;;
;; The interactive functions are similar to those of the HTML and LaTeX
;; exporters:
;;
;; M-x `org-export-as-docbook'
;; M-x `org-export-as-docbook-pdf'
;; M-x `org-export-as-docbook-pdf-and-open'
;; M-x `org-export-as-docbook-batch'
;; M-x `org-export-as-docbook-to-buffer'
;; M-x `org-export-region-as-docbook'
;; M-x `org-replace-region-by-docbook'
;;
;; Note that, in order to generate PDF files using the DocBook XML files
;; created by DocBook exporter, the following two variables have to be
;; set based on what DocBook tools you use for XSLT processor and XSL-FO
;; processor:
;;
;;   org-export-docbook-xslt-proc-command
;;   org-export-docbook-xsl-fo-proc-command
;;
;; Check the document of these two variables to see examples of how they
;; can be set.
;;
;; If the Org file to be exported contains special characters written in
;; TeX-like syntax, like \alpha and \beta, you need to include the right
;; entity file(s) in the DOCTYPE declaration for the DocBook XML file.
;; This is required to make the DocBook XML file valid.  The DOCTYPE
;; declaration string can be set using the following variable:
;;
;;   org-export-docbook-doctype
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'footnote)
(require 'org)
(require 'org-exp)
(require 'org-html)
(require 'format-spec)

;;; Variables:

(defvar org-docbook-para-open nil)
(defvar org-export-docbook-inline-images t)
(defvar org-export-docbook-link-org-files-as-docbook nil)

(declare-function org-id-find-id-file "org-id" (id))

;;; User variables:

(defgroup org-export-docbook nil
  "Options for exporting Org-mode files to DocBook."
  :tag "Org Export DocBook"
  :group 'org-export)

(defcustom org-export-docbook-extension ".xml"
  "Extension of DocBook XML files."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-header "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
  "Header of DocBook XML files."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-doctype nil
  "DOCTYPE declaration string for DocBook XML files.
This can be used to include entities that are needed to handle
special characters in Org files.

For example, if the Org file to be exported contains XHTML
entities, you can set this variable to:

\"<!DOCTYPE article [
<!ENTITY % xhtml1-symbol PUBLIC
\"-//W3C//ENTITIES Symbol for HTML//EN//XML\"
\"http://www.w3.org/2003/entities/2007/xhtml1-symbol.ent\"
>
%xhtml1-symbol;
]>
\"

If you want to process DocBook documents without an Internet
connection, it is suggested that you download the required entity
file(s) and use system identifier(s) (external files) in the
DOCTYPE declaration."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-article-header "<article xmlns=\"http://docbook.org/ns/docbook\"
         xmlns:xlink=\"http://www.w3.org/1999/xlink\" version=\"5.0\" xml:lang=\"en\">"
  "Article header of DocBook XML files."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-section-id-prefix "sec-"
  "Prefix of section IDs used during exporting.
This can be set before exporting to avoid same set of section IDs
being used again and again, which can be a problem when multiple
people work on the same document."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-footnote-id-prefix "fn-"
  "The prefix of footnote IDs used during exporting.
Like `org-export-docbook-section-id-prefix', this variable can help
avoid same set of footnote IDs being used multiple times."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-footnote-separator "<superscript>, </superscript>"
  "Text used to separate footnotes."
  :group 'org-export-docbook
  :version "24.1"
  :type 'string)

(defcustom org-export-docbook-emphasis-alist
  `(("*" "<emphasis role=\"bold\">" "</emphasis>")
    ("/" "<emphasis>" "</emphasis>")
    ("_" "<emphasis role=\"underline\">" "</emphasis>")
    ("=" "<code>" "</code>")
    ("~" "<literal>" "</literal>")
    ("+" "<emphasis role=\"strikethrough\">" "</emphasis>"))
  "A list of DocBook expressions to convert emphasis fontifiers.
Each element of the list is a list of three elements.
The first element is the character used as a marker for fontification.
The second element is a formatting string to wrap fontified text with.
The third element decides whether to protect converted text from other
conversions."
  :group 'org-export-docbook
  :type 'alist)

(defcustom org-export-docbook-default-image-attributes
  `(("align" . "\"center\"")
    ("valign". "\"middle\""))
  "Alist of default DocBook image attributes.
These attributes will be inserted into element <imagedata> by
default, but users can override them using `#+ATTR_DocBook:'."
  :group 'org-export-docbook
  :type 'alist)

(defcustom org-export-docbook-inline-image-extensions
  '("jpeg" "jpg" "png" "gif" "svg")
  "Extensions of image files that can be inlined into DocBook."
  :group 'org-export-docbook
  :type '(repeat (string :tag "Extension")))

(defcustom org-export-docbook-coding-system nil
  "Coding system for DocBook XML files."
  :group 'org-export-docbook
  :type 'coding-system)

(defcustom org-export-docbook-xslt-stylesheet nil
  "File name of the XSLT stylesheet used by DocBook exporter.
This XSLT stylesheet is used by
`org-export-docbook-xslt-proc-command' to generate the Formatting
Object (FO) files.  You can use either `fo/docbook.xsl' that
comes with DocBook, or any customization layer you may have."
  :group 'org-export-docbook
  :version "24.1"
  :type 'string)

(defcustom org-export-docbook-xslt-proc-command nil
  "Format of XSLT processor command used by DocBook exporter.
This command is used to process a DocBook XML file to generate
the Formatting Object (FO) file.

The value of this variable should be a format control string that
includes three arguments: `%i', `%o', and `%s'.  During exporting
time, `%i' is replaced by the input DocBook XML file name, `%o'
is replaced by the output FO file name, and `%s' is replaced by
`org-export-docbook-xslt-stylesheet' (or the #+XSLT option if it
is specified in the Org file).

For example, if you use Saxon as the XSLT processor, you may want
to set the variable to

  \"java com.icl.saxon.StyleSheet -o %o %i %s\"

If you use Xalan, you can set it to

  \"java org.apache.xalan.xslt.Process -out %o -in %i -xsl %s\"

For xsltproc, the following string should work:

  \"xsltproc --output %o %s %i\"

You can include additional stylesheet parameters in this command.
Just make sure that they meet the syntax requirement of each
processor."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-xsl-fo-proc-command nil
  "Format of XSL-FO processor command used by DocBook exporter.
This command is used to process a Formatting Object (FO) file to
generate the PDF file.

The value of this variable should be a format control string that
includes two arguments: `%i' and `%o'.  During exporting time,
`%i' is replaced by the input FO file name, and `%o' is replaced
by the output PDF file name.

For example, if you use FOP as the XSL-FO processor, you can set
the variable to

  \"fop %i %o\""
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-keywords-markup "<literal>%s</literal>"
  "A printf format string to be applied to keywords by DocBook exporter."
  :group 'org-export-docbook
  :type 'string)

(defcustom org-export-docbook-timestamp-markup "<emphasis>%s</emphasis>"
  "A printf format string to be applied to time stamps by DocBook exporter."
  :group 'org-export-docbook
  :type 'string)

;;; Hooks

(defvar org-export-docbook-final-hook nil
  "Hook run at the end of DocBook export, in the new buffer.")

;;; Autoload functions:

;;;###autoload
(defun org-export-as-docbook-batch ()
  "Call `org-export-as-docbook' in batch style.
This function can be used in batch processing.

For example:

$ emacs --batch
        --load=$HOME/lib/emacs/org.el
        --visit=MyOrgFile.org --funcall org-export-as-docbook-batch"
  (org-export-as-docbook 'hidden))

;;;###autoload
(defun org-export-as-docbook-to-buffer ()
  "Call `org-export-as-docbook' with output to a temporary buffer.
No file is created."
  (interactive)
  (org-export-as-docbook nil nil "*Org DocBook Export*")
  (when org-export-show-temporary-export-buffer
    (switch-to-buffer-other-window "*Org DocBook Export*")))

;;;###autoload
(defun org-replace-region-by-docbook (beg end)
  "Replace the region from BEG to END with its DocBook export.
It assumes the region has `org-mode' syntax, and then convert it to
DocBook.  This can be used in any buffer.  For example, you could
write an itemized list in `org-mode' syntax in an DocBook buffer and
then use this command to convert it."
  (interactive "r")
  (let (reg docbook buf)
    (save-window-excursion
      (if (eq major-mode 'org-mode)
	  (setq docbook (org-export-region-as-docbook
			 beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq docbook (org-export-region-as-docbook
			 (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert docbook)))

;;;###autoload
(defun org-export-region-as-docbook (beg end &optional body-only buffer)
  "Convert region from BEG to END in `org-mode' buffer to DocBook.
If prefix arg BODY-ONLY is set, omit file header and footer and
only produce the region of converted text, useful for
cut-and-paste operations.  If BUFFER is a buffer or a string,
use/create that buffer as a target of the converted DocBook.  If
BUFFER is the symbol `string', return the produced DocBook as a
string and leave not buffer behind.  For example, a Lisp program
could call this function in the following way:

  (setq docbook (org-export-region-as-docbook beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (interactive "r\nP")
  (when (org-called-interactively-p 'any)
    (setq buffer "*Org DocBook Export*"))
  (let ((transient-mark-mode t)
	(zmacs-regions t)
	rtn)
    (goto-char end)
    (set-mark (point)) ;; To activate the region
    (goto-char beg)
    (setq rtn (org-export-as-docbook
	       nil nil
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (org-called-interactively-p 'any) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

;;;###autoload
(defun org-export-as-docbook-pdf (&optional hidden ext-plist
					    to-buffer body-only pub-dir)
  "Export as DocBook XML file, and generate PDF file."
  (interactive "P")
  (if (or (not org-export-docbook-xslt-proc-command)
	  (not (string-match "%[ios].+%[ios].+%[ios]" org-export-docbook-xslt-proc-command)))
      (error "XSLT processor command is not set correctly"))
  (if (or (not org-export-docbook-xsl-fo-proc-command)
	  (not (string-match "%[io].+%[io]" org-export-docbook-xsl-fo-proc-command)))
      (error "XSL-FO processor command is not set correctly"))
  (message "Exporting to PDF...")
  (let* ((wconfig (current-window-configuration))
	 (opt-plist
	  (org-export-process-option-filters
	   (org-combine-plists (org-default-export-plist)
			       ext-plist
			       (org-infile-export-plist))))
	 (docbook-buf (org-export-as-docbook hidden ext-plist
					     to-buffer body-only pub-dir))
	 (filename (buffer-file-name docbook-buf))
	 (base (file-name-sans-extension filename))
	 (fofile (concat base ".fo"))
	 (pdffile (concat base ".pdf")))
    (and (file-exists-p pdffile) (delete-file pdffile))
    (message "Processing DocBook XML file...")
    (shell-command (format-spec org-export-docbook-xslt-proc-command
				(format-spec-make
				 ?i (shell-quote-argument filename)
				 ?o (shell-quote-argument fofile)
				 ?s (shell-quote-argument
				     (or (plist-get opt-plist :xslt)
					 org-export-docbook-xslt-stylesheet)))))
    (shell-command (format-spec org-export-docbook-xsl-fo-proc-command
				(format-spec-make
				 ?i (shell-quote-argument fofile)
				 ?o (shell-quote-argument pdffile))))
    (message "Processing DocBook file...done")
    (if (not (file-exists-p pdffile))
	(error "PDF file was not produced")
      (set-window-configuration wconfig)
      (message "Exporting to PDF...done")
      pdffile)))

;;;###autoload
(defun org-export-as-docbook-pdf-and-open ()
  "Export as DocBook XML file, generate PDF file, and open it."
  (interactive)
  (let ((pdffile (org-export-as-docbook-pdf)))
    (if pdffile
	(org-open-file pdffile)
      (error "PDF file was not produced"))))

(defvar org-heading-keyword-regexp-format) ; defined in org.el

;;;###autoload
(defun org-export-as-docbook (&optional hidden ext-plist
					to-buffer body-only pub-dir)
  "Export the current buffer as a DocBook file.
If there is an active region, export only the region.  When
HIDDEN is obsolete and does nothing.  EXT-PLIST is a
property list with external parameters overriding org-mode's
default settings, but still inferior to file-local settings.
When TO-BUFFER is non-nil, create a buffer with that name and
export to that buffer.  If TO-BUFFER is the symbol `string',
don't leave any buffer behind but just return the resulting HTML
as a string.  When BODY-ONLY is set, don't produce the file
header and footer, simply return the content of the document (all
top-level sections).  When PUB-DIR is set, use this as the
publishing directory."
  (interactive "P")
  (run-hooks 'org-export-first-hook)

  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting...")
  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-keywords org-done-keywords)
  (setq-default org-maybe-keyword-time-regexp org-maybe-keyword-time-regexp)
  (let* ((opt-plist
	  (org-export-process-option-filters
	   (org-combine-plists (org-default-export-plist)
			       ext-plist
			       (org-infile-export-plist))))
	 (link-validate (plist-get opt-plist :link-validation-function))
	 valid
	 (odd org-odd-levels-only)
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
	  (or pub-dir (org-export-directory :docbook opt-plist)))
	 (org-current-export-file buffer-file-name)
	 (level 0) (line "") (origline "") txt todo
	 (filename (if to-buffer nil
		     (expand-file-name
		      (concat
		       (file-name-sans-extension
			(or (and subtree-p
				 (org-entry-get (region-beginning)
						"EXPORT_FILE_NAME" t))
			    (file-name-nondirectory buffer-file-name)))
		       org-export-docbook-extension)
		      (file-name-as-directory
		       (or pub-dir (org-export-directory :docbook opt-plist))))))
	 (current-dir (if buffer-file-name
			  (file-name-directory buffer-file-name)
			default-directory))
	 (auto-insert nil); Avoid any auto-insert stuff for the new file
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string)
		       (get-buffer-create "*Org DocBook Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
	 ;; org-levels-open is a global variable
	 (org-levels-open (make-vector org-level-max nil))
	 (date (plist-get opt-plist :date))
	 (author (or (plist-get opt-plist :author)
		     user-full-name))
	 (email (plist-get opt-plist :email))
	 firstname othername surname
	 (title (or (and subtree-p (org-export-get-title-from-subtree))
		    (plist-get opt-plist :title)
		    (and (not
			  (plist-get opt-plist :skip-before-1st-heading))
			 (org-export-grab-title-from-buffer))
		    (and buffer-file-name
			 (file-name-sans-extension
			  (file-name-nondirectory buffer-file-name)))
		    "UNTITLED"))
	 ;; We will use HTML table formatter to export tables to DocBook
	 ;; format, so need to set html-table-tag here.
	 (html-table-tag (plist-get opt-plist :html-table-tag))
	 (quote-re0   (concat "^ *" org-quote-string "\\( +\\|[ \t]*$\\)"))
	 (quote-re    (format org-heading-keyword-regexp-format
			      org-quote-string))
	 (inquote     nil)
	 (infixed     nil)
	 (inverse     nil)
	 (llt org-plain-list-ordered-item-terminator)
	 (email (plist-get opt-plist :email))
	 (language (plist-get opt-plist :language))
	 (lang-words  nil)
	 cnt
	 (start 0)
	 (coding-system (and (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (coding-system-for-write (or org-export-docbook-coding-system
				      coding-system))
	 (save-buffer-coding-system (or org-export-docbook-coding-system
					coding-system))
	 (charset (and coding-system-for-write
		       (fboundp 'coding-system-get)
		       (coding-system-get coding-system-for-write
					  'mime-charset)))
	 (region
	  (buffer-substring
	   (if region-p (region-beginning) (point-min))
	   (if region-p (region-end) (point-max))))
	 (org-export-footnotes-seen nil)
	 (org-export-footnotes-data (org-footnote-all-labels 'with-defs))
	 (lines
	  (org-split-string
	   (org-export-preprocess-string
	    region
	    :emph-multiline t
	    :for-backend 'docbook
	    :skip-before-1st-heading
	    (plist-get opt-plist :skip-before-1st-heading)
	    :drawers (plist-get opt-plist :drawers)
	    :todo-keywords (plist-get opt-plist :todo-keywords)
	    :tasks (plist-get opt-plist :tasks)
	    :tags (plist-get opt-plist :tags)
	    :priority (plist-get opt-plist :priority)
	    :footnotes (plist-get opt-plist :footnotes)
	    :timestamps (plist-get opt-plist :timestamps)
	    :archived-trees
	    (plist-get opt-plist :archived-trees)
	    :select-tags (plist-get opt-plist :select-tags)
	    :exclude-tags (plist-get opt-plist :exclude-tags)
	    :add-text
	    (plist-get opt-plist :text)
	    :LaTeX-fragments
	    (plist-get opt-plist :LaTeX-fragments))
	   "[\r\n]"))
	 ;; Use literal output to show check boxes.
	 (checkbox-start
	  (nth 1 (assoc "=" org-export-docbook-emphasis-alist)))
	 (checkbox-end
	  (nth 2 (assoc "=" org-export-docbook-emphasis-alist)))
	 table-open type
	 table-buffer table-orig-buffer
	 ind item-type starter
	 rpl path attr caption label desc descp desc1 desc2 link
	 fnc item-tag item-number
	 footref-seen footnote-list
	 id-file
	 )

    ;; Fine detailed info about author name.
    (if (string-match "\\([^ ]+\\) \\(.+ \\)?\\([^ ]+\\)" author)
	(progn
	  (setq firstname (match-string 1 author)
		othername (or (match-string 2 author) "")
		surname (match-string 3 author))))

    ;; Get all footnote text.
    (setq footnote-list
	  (org-export-docbook-get-footnotes lines))

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (setq org-min-level (org-get-min-level lines level-offset))
    (setq org-last-level org-min-level)
    (org-init-section-numbers)

    ;; Get and save the date.
    (cond
     ((and date (string-match "%" date))
      (setq date (format-time-string date)))
     (date)
     (t (setq date (format-time-string "%Y-%m-%d %T %Z"))))

    ;; Get the language-dependent settings
    (setq lang-words (or (assoc language org-export-language-setup)
			 (assoc "en" org-export-language-setup)))

    ;; Switch to the output buffer.  Use fundamental-mode for now.  We
    ;; could turn on nXML mode later and do some indentation.
    (set-buffer buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    ;; The main body...
    (let ((case-fold-search nil)
	  (org-odd-levels-only odd))

      ;; Create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapc (lambda (x)
	      (set (make-local-variable (nth 2 x))
		   (plist-get opt-plist (car x))))
	    org-export-plist-vars)

      ;; Insert DocBook file header, title, and author info.
      (unless body-only
	(insert org-export-docbook-header)
	(if org-export-docbook-doctype
	    (insert org-export-docbook-doctype))
	(insert "<!-- Date: " date " -->\n")
	(insert (format "<!-- DocBook XML file generated by Org-mode %s Emacs %s -->\n"
			org-version emacs-major-version))
	(insert org-export-docbook-article-header)
	(insert (format
		 "\n  <title>%s</title>
  <info>
    <author>
      <personname>
        <firstname>%s</firstname> <othername>%s</othername> <surname>%s</surname>
      </personname>
      %s
    </author>
  </info>\n"
		 (org-docbook-expand title)
		 firstname othername surname
		 (if (and org-export-email-info
			  email (string-match "\\S-" email))
		     (concat "<email>" email "</email>") "")
		 )))

      (org-init-section-numbers)

      (org-export-docbook-open-para)

      ;; Loop over all the lines...
      (while (setq line (pop lines) origline line)
	(catch 'nextline

	  ;; End of quote section?
	  (when (and inquote (string-match org-outline-regexp-bol line))
	    (insert "]]></programlisting>\n")
	    (org-export-docbook-open-para)
	    (setq inquote nil))
	  ;; Inside a quote section?
	  (when inquote
	    (insert (org-docbook-protect line) "\n")
	    (throw 'nextline nil))

	  ;; Fixed-width, verbatim lines (examples)
	  (when (and org-export-with-fixed-width
		     (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)" line))
	    (when (not infixed)
	      (setq infixed t)
	      (org-export-docbook-close-para-maybe)
	      (insert "<programlisting><![CDATA["))
	    (insert (match-string 3 line) "\n")
	    (when (or (not lines)
		      (not (string-match "^[ \t]*\\(:.*\\)"
					 (car lines))))
	      (setq infixed nil)
	      (insert "]]></programlisting>\n")
	      (org-export-docbook-open-para))
	    (throw 'nextline nil))

	  ;; Protected HTML
	  (when (get-text-property 0 'org-protected line)
	    (let (par (ind (get-text-property 0 'original-indentation line)))
	      (when (re-search-backward
		     "\\(<para>\\)\\([ \t\r\n]*\\)\\=" (- (point) 100) t)
		(setq par (match-string 1))
		(replace-match "\\2\n"))
	      (insert line "\n")
	      (while (and lines
			  (or (= (length (car lines)) 0)
			      (not ind)
			      (equal ind (get-text-property 0 'original-indentation (car lines))))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-protected (car lines))))
		(insert (pop lines) "\n"))
	      (and par (insert "<para>\n")))
	    (throw 'nextline nil))

	  ;; Start of block quotes and verses
	  (when (or (equal "ORG-BLOCKQUOTE-START" line)
		    (and (equal "ORG-VERSE-START" line)
			 (setq inverse t)))
	    (org-export-docbook-close-para-maybe)
	    (insert "<blockquote>")
	    ;; Check whether attribution for this blockquote exists.
	    (let (tmp1
		  attribution
		  (end (if inverse "ORG-VERSE-END" "ORG-BLOCKQUOTE-END"))
		  (quote-lines nil))
	      (while (and (setq tmp1 (pop lines))
			  (not (equal end tmp1)))
		(push tmp1 quote-lines))
	      (push tmp1 lines)		; Put back quote end mark
	      ;; Check the last line in the quote to see if it contains
	      ;; the attribution.
	      (setq tmp1 (pop quote-lines))
	      (if (string-match "\\(^.*\\)\\(--[ \t]+\\)\\(.+\\)$" tmp1)
		  (progn
		    (setq attribution (match-string 3 tmp1))
		    (when (save-match-data
			    (string-match "[^ \t]" (match-string 1 tmp1)))
		      (push (match-string 1 tmp1) lines)))
		(push tmp1 lines))
	      (while (setq tmp1 (pop quote-lines))
		(push tmp1 lines))
	      (when attribution
		(insert "<attribution>" attribution "</attribution>")))
	    ;; Insert <literallayout> for verse.
	    (if inverse
		(insert "\n<literallayout>")
	      (org-export-docbook-open-para))
	    (throw 'nextline nil))

	  ;; End of block quotes
	  (when (equal "ORG-BLOCKQUOTE-END" line)
	    (org-export-docbook-close-para-maybe)
	    (insert "</blockquote>\n")
	    (org-export-docbook-open-para)
	    (throw 'nextline nil))

	  ;; End of verses
	  (when (equal "ORG-VERSE-END" line)
	    (insert "</literallayout>\n</blockquote>\n")
	    (org-export-docbook-open-para)
	    (setq inverse nil)
	    (throw 'nextline nil))

	  ;; Text centering.  Element <para role="centered"> does not
	  ;; seem to work with FOP, so for now we use <informaltable> to
	  ;; center the text, which can contain multiple paragraphs.
	  (when (equal "ORG-CENTER-START" line)
	    (org-export-docbook-close-para-maybe)
	    (insert "<informaltable frame=\"none\" colsep=\"0\" rowsep=\"0\">\n"
		    "<tgroup align=\"center\" cols=\"1\">\n"
		    "<tbody><row><entry>\n")
	    (org-export-docbook-open-para)
	    (throw 'nextline nil))

	  (when (equal "ORG-CENTER-END" line)
	    (org-export-docbook-close-para-maybe)
	    (insert "</entry></row></tbody>\n"
		    "</tgroup>\n</informaltable>\n")
	    (org-export-docbook-open-para)
	    (throw 'nextline nil))

	  ;; Make targets to anchors.  Note that currently FOP does not
	  ;; seem to support <anchor> tags when generating PDF output,
	  ;; but this can be used in DocBook --> HTML conversion.
	  (setq start 0)
	  (while (string-match
		  "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line start)
	    (cond
	     ((get-text-property (match-beginning 1) 'org-protected line)
	      (setq start (match-end 1)))
	     ((match-end 2)
	      (setq line (replace-match
			  (format "@<anchor xml:id=\"%s\"/>"
				  (org-solidify-link-text (match-string 1 line)))
			  t t line)))
	     (t
	      (setq line (replace-match
			  (format "@<anchor xml:id=\"%s\"/>"
				  (org-solidify-link-text (match-string 1 line)))
			  t t line)))))

	  ;; Put time stamps and related keywords into special mark-up
	  ;; elements.
	  (setq line (org-export-docbook-handle-time-stamps line))

	  ;; Replace "&", "<" and ">" by "&amp;", "&lt;" and "&gt;".
	  ;; Handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>").
	  ;; Also handle sub_superscripts and check boxes.
	  (or (string-match org-table-hline-regexp line)
	      (setq line (org-docbook-expand line)))

	  ;; Format the links
	  (setq start 0)
	  (while (string-match org-bracket-link-analytic-regexp++ line start)
	    (setq start (match-beginning 0))
	    (setq path (save-match-data (org-link-unescape
					 (match-string 3 line))))
	    (setq type (cond
			((match-end 2) (match-string 2 line))
			((save-match-data
			   (or (file-name-absolute-p path)
			       (string-match "^\\.\\.?/" path)))
			 "file")
			(t "internal")))
	    (setq path (org-extract-attributes (org-link-unescape path)))
	    (setq attr (get-text-property 0 'org-attributes path)
		  caption (get-text-property 0 'org-caption path)
		  label (get-text-property 0 'org-label path))
	    (setq desc1 (if (match-end 5) (match-string 5 line))
		  desc2 (if (match-end 2) (concat type ":" path) path)
		  descp (and desc1 (not (equal desc1 desc2)))
		  desc (or desc1 desc2))
	    ;; Make an image out of the description if that is so wanted
	    (when (and descp (org-file-image-p
			      desc org-export-docbook-inline-image-extensions))
	      (save-match-data
		(if (string-match "^file:" desc)
		    (setq desc (substring desc (match-end 0))))))
	    ;; FIXME: do we need to unescape here somewhere?
	    (cond
	     ((equal type "internal")
	      (setq rpl (format "<link linkend=\"%s\">%s</link>"
				(org-solidify-link-text
				 (save-match-data (org-link-unescape path)) nil)
				(org-export-docbook-format-desc desc))))
	     ((and (equal type "id")
		   (setq id-file (org-id-find-id-file path)))
	      ;; This is an id: link to another file (if it was the same file,
	      ;; it would have become an internal link...)
	      (save-match-data
		(setq id-file (file-relative-name
			       id-file (file-name-directory org-current-export-file)))
		(setq id-file (concat (file-name-sans-extension id-file)
				      org-export-docbook-extension))
		(setq rpl (format "<link xlink:href=\"%s#%s\">%s</link>"
				  id-file path (org-export-docbook-format-desc desc)))))
	     ((member type '("http" "https"))
	      ;; Standard URL, just check if we need to inline an image
	      (if (and (or (eq t org-export-docbook-inline-images)
			   (and org-export-docbook-inline-images (not descp)))
		       (org-file-image-p
			path org-export-docbook-inline-image-extensions))
		  (setq rpl (org-export-docbook-format-image
			     (concat type ":" path)))
		(setq link (concat type ":" path))
		(setq rpl (format "<link xlink:href=\"%s\">%s</link>"
				  (org-export-html-format-href link)
				  (org-export-docbook-format-desc desc)))
		))
	     ((member type '("ftp" "mailto" "news"))
	      ;; Standard URL
	      (setq link (concat type ":" path))
	      (setq rpl (format "<link xlink:href=\"%s\">%s</link>"
				(org-export-html-format-href link)
				(org-export-docbook-format-desc desc))))
	     ((string= type "coderef")
	      (setq rpl (format (org-export-get-coderef-format path (and descp desc))
				(cdr (assoc path org-export-code-refs)))))
	     ((functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
	      ;; The link protocol has a function for format the link
	      (setq rpl
		    (save-match-data
		      (funcall fnc (org-link-unescape path) desc1 'html))))

	     ((string= type "file")
	      ;; FILE link
	      (let* ((filename path)
		     (abs-p (file-name-absolute-p filename))
		     thefile file-is-image-p search)
		(save-match-data
		  (if (string-match "::\\(.*\\)" filename)
		      (setq search (match-string 1 filename)
			    filename (replace-match "" t nil filename)))
		  (setq valid
			(if (functionp link-validate)
			    (funcall link-validate filename current-dir)
			  t))
		  (setq file-is-image-p
			(org-file-image-p
			 filename org-export-docbook-inline-image-extensions))
		  (setq thefile (if abs-p (expand-file-name filename) filename))
		  ;; Carry over the properties (expand-file-name will
		  ;; discard the properties of filename)
		  (add-text-properties 0 (1- (length thefile))
				       (list 'org-caption caption
					     'org-attributes attr
					     'org-label label)
				       thefile)
		  (when (and org-export-docbook-link-org-files-as-docbook
			     (string-match "\\.org$" thefile))
		    (setq thefile (concat (substring thefile 0
						     (match-beginning 0))
					  org-export-docbook-extension))
		    (if (and search
			     ;; make sure this is can be used as target search
			     (not (string-match "^[0-9]*$" search))
			     (not (string-match "^\\*" search))
			     (not (string-match "^/.*/$" search)))
			(setq thefile (concat thefile "#"
					      (org-solidify-link-text
					       (org-link-unescape search)))))
		    (when (string-match "^file:" desc)
		      (setq desc (replace-match "" t t desc))
		      (if (string-match "\\.org$" desc)
			  (setq desc (replace-match "" t t desc))))))
		(setq rpl (if (and file-is-image-p
				   (or (eq t org-export-docbook-inline-images)
				       (and org-export-docbook-inline-images
					    (not descp))))
			      (progn
				(message "image %s %s" thefile org-docbook-para-open)
				(org-export-docbook-format-image thefile))
			    (format "<link xlink:href=\"%s\">%s</link>"
				    thefile (org-export-docbook-format-desc desc))))
		(if (not valid) (setq rpl desc))))

	     (t
	      ;; Just publish the path, as default
	      (setq rpl (concat "&lt;" type ":"
				(save-match-data (org-link-unescape path))
				"&gt;"))))
	    (setq line (replace-match rpl t t line)
		  start (+ start (length rpl))))

	  ;; TODO items: can we do something better?!
	  (if (and (string-match org-todo-line-regexp line)
		   (match-beginning 2))
	      (setq line
		    (concat (substring line 0 (match-beginning 2))
			    "[" (match-string 2 line) "]"
			    (substring line (match-end 2)))))

	  ;; Does this contain a reference to a footnote?
	  (when org-export-with-footnotes
	    (setq start 0)
	    (while (string-match "\\([^* \t].*?\\)\\[\\([0-9]+\\)\\]" line start)
	      ;; Discard protected matches not clearly identified as
	      ;; footnote markers.
	      (if (or (get-text-property (match-beginning 2) 'org-protected line)
		      (not (get-text-property (match-beginning 2) 'org-footnote line)))
		  (setq start (match-end 2))
		(let* ((num (match-string 2 line))
		       (footnote-def (assoc num footnote-list)))
		  (if (assoc num footref-seen)
		      (setq line (replace-match
				  (format "%s<footnoteref linkend=\"%s%s\"/>"
					  (match-string 1 line)
					  org-export-docbook-footnote-id-prefix num)
				  t t line))
		    (setq line (replace-match
				(concat
				 (format "%s<footnote xml:id=\"%s%s\"><para>%s</para></footnote>"
					 (match-string 1 line)
					 org-export-docbook-footnote-id-prefix
					 num
					 (if footnote-def
					     (save-match-data
					       (org-docbook-expand (cdr footnote-def)))
					   (format "FOOTNOTE DEFINITION NOT FOUND: %s" num)))
				 ;; If another footnote is following the
				 ;; current one, add a separator.
				 (if (save-match-data
				       (string-match "\\`\\[[0-9]+\\]"
						     (substring line (match-end 0))))
				     org-export-docbook-footnote-separator
				   ""))
				t t line))
		    (push (cons num 1) footref-seen))))))

	  (cond
	   ((string-match "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$" line)
	    ;; This is a headline
	    (setq level (org-tr-level (- (match-end 1) (match-beginning 1)
					 level-offset))
		  txt (match-string 2 line))
	    (if (string-match quote-re0 txt)
		(setq txt (replace-match "" t t txt)))
	    (org-export-docbook-level-start level txt)
	    ;; QUOTES
	    (when (string-match quote-re line)
	      (org-export-docbook-close-para-maybe)
	      (insert "<programlisting><![CDATA[")
	      (setq inquote t)))

	   ;; Tables: since version 4.3 of DocBook DTD, HTML tables are
	   ;; supported.  We can use existing HTML table exporter code
	   ;; here.
	   ((and org-export-with-tables
		 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	    (if (not table-open)
		;; New table starts
		(setq table-open t
		      table-buffer nil
		      table-orig-buffer nil))
	    ;; Accumulate lines
	    (setq table-buffer (cons line table-buffer)
		  table-orig-buffer (cons origline table-orig-buffer))
	    (when (or (not lines)
		      (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
					 (car lines))))
	      (setq table-open nil
		    table-buffer (nreverse table-buffer)
		    table-orig-buffer (nreverse table-orig-buffer))
	      (org-export-docbook-close-para-maybe)
	      (insert (org-export-docbook-finalize-table
		       (org-format-table-html table-buffer table-orig-buffer
					      'no-css)))))

	   ;; Normal lines
	   (t
	    ;; This line either is list item or end a list.
	    (when (when (get-text-property 0 'list-item line)
	   	      (setq line (org-export-docbook-list-line
			  line
			  (get-text-property 0 'list-item line)
			  (get-text-property 0 'list-struct line)
			  (get-text-property 0 'list-prevs line)))))

	    ;; Empty lines start a new paragraph.  If hand-formatted lists
	    ;; are not fully interpreted, lines starting with "-", "+", "*"
	    ;; also start a new paragraph.
	    (if (and (string-match "^ [-+*]-\\|^[ \t]*$" line)
		     (not inverse))
		(org-export-docbook-open-para))

	    ;; Is this the start of a footnote?
	    (when org-export-with-footnotes
	      (when (and (boundp 'footnote-section-tag-regexp)
			 (string-match (concat "^" footnote-section-tag-regexp)
				       line))
		;; ignore this line
		(throw 'nextline nil))
	      ;; These footnote lines have been read and saved before,
	      ;; ignore them at this time.
	      (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
		(org-export-docbook-close-para-maybe)
		(throw 'nextline nil)))

	    ;; FIXME: It might be a good idea to add an option to
	    ;; support line break processing instruction <?linebreak?>.
	    ;; Org-mode supports line break "\\" in HTML exporter, and
	    ;; some DocBook users may also want to force line breaks
	    ;; even though DocBook only supports that in
	    ;; <literallayout>.

	    (insert line "\n")))))

      ;; Properly close all local lists and other lists
      (when inquote
	(insert "]]></programlisting>\n")
	(org-export-docbook-open-para))

      ;; Close all open sections.
      (org-export-docbook-level-start 1 nil)

      (unless (plist-get opt-plist :buffer-will-be-killed)
	(normal-mode)
	(if (eq major-mode (default-value 'major-mode))
	    (nxml-mode)))

      ;; Remove empty paragraphs. Replace them with a newline.
      (goto-char (point-min))
      (while (re-search-forward
	      "[ \r\n\t]*\\(<para>\\)[ \r\n\t]*</para>[ \r\n\t]*" nil t)
	(when (not (get-text-property (match-beginning 1) 'org-protected))
	  (replace-match "\n")
	  (backward-char 1)))
      ;; Fill empty sections with <para></para>.  This is to make sure
      ;; that the DocBook document generated is valid and well-formed.
      (goto-char (point-min))
      (while (re-search-forward
	      "</title>\\([ \r\n\t]*\\)</section>" nil t)
	(when (not (get-text-property (match-beginning 0) 'org-protected))
	  (replace-match "\n<para></para>\n" nil nil nil 1)))
      ;; Insert the last closing tag.
      (goto-char (point-max))
      (unless body-only
	(insert "</article>"))
      (run-hooks 'org-export-docbook-final-hook)
      (or to-buffer (save-buffer))
      (goto-char (point-min))
      (or (org-export-push-to-kill-ring "DocBook")
	  (message "Exporting... done"))
      (if (eq to-buffer 'string)
	  (prog1 (buffer-substring (point-min) (point-max))
	    (kill-buffer (current-buffer)))
	(current-buffer)))))

(defun org-export-docbook-open-para ()
  "Insert <para>, but first close previous paragraph if any."
  (org-export-docbook-close-para-maybe)
  (insert "\n<para>")
  (setq org-docbook-para-open t))

(defun org-export-docbook-close-para-maybe ()
  "Close DocBook paragraph if there is one open."
  (when org-docbook-para-open
    (insert "</para>\n")
    (setq org-docbook-para-open nil)))

(defun org-export-docbook-close-li (&optional type)
  "Close list if necessary."
  (org-export-docbook-close-para-maybe)
  (if (equal type "d")
      (insert "</listitem></varlistentry>\n")
    (insert "</listitem>\n")))

(defun org-export-docbook-level-start (level title)
  "Insert a new level in DocBook export.
When TITLE is nil, just close all open levels."
  (org-export-docbook-close-para-maybe)
  (let* ((target (and title (org-get-text-property-any 0 'target title)))
	 (l org-level-max)
	 section-number)
    (while (>= l level)
      (if (aref org-levels-open (1- l))
	  (progn
	    (insert "</section>\n")
	    (aset org-levels-open (1- l) nil)))
      (setq l (1- l)))
    (when title
      ;; If title is nil, this means this function is called to close
      ;; all levels, so the rest is done only if title is given.
      ;;
      ;; Format tags: put them into a superscript like format.
      (when (string-match (org-re "\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$") title)
	(setq title
	      (replace-match
	       (if org-export-with-tags
		   (save-match-data
		     (concat
		      "<superscript>"
		      (match-string 1 title)
		      "</superscript>"))
		 "")
	       t t title)))
      (aset org-levels-open (1- level) t)
      (setq section-number (org-section-number level))
      (insert (format "\n<section xml:id=\"%s%s\">\n<title>%s</title>"
		      org-export-docbook-section-id-prefix
		      (replace-regexp-in-string "\\." "_" section-number)
		      title))
      (org-export-docbook-open-para))))

(defun org-docbook-expand (string)
  "Prepare STRING for DocBook export.
Applies all active conversions.  If there are links in the
string, don't modify these."
  (let* ((re (concat org-bracket-link-regexp "\\|"
		     (org-re "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")))
	 m s l res)
    (while (setq m (string-match re string))
      (setq s (substring string 0 m)
	    l (match-string 0 string)
	    string (substring string (match-end 0)))
      (push (org-docbook-do-expand s) res)
      (push l res))
    (push (org-docbook-do-expand string) res)
    (apply 'concat (nreverse res))))

(defun org-docbook-do-expand (s)
  "Apply all active conversions to translate special ASCII to DocBook."
  (setq s (org-html-protect s))
  (while (string-match "@&lt;\\([^&]*\\)&gt;" s)
    (setq s (replace-match "<\\1>" t nil s)))
  (if org-export-with-emphasize
      (setq s (org-export-docbook-convert-emphasize s)))
  (if org-export-with-special-strings
      (setq s (org-export-docbook-convert-special-strings s)))
  (if org-export-with-sub-superscripts
      (setq s (org-export-docbook-convert-sub-super s)))
  (if org-export-with-TeX-macros
      (let ((start 0) wd rep)
	(while (setq start (string-match "\\\\\\([a-zA-Z]+\\)\\({}\\)?"
					 s start))
	  (if (get-text-property (match-beginning 0) 'org-protected s)
	      (setq start (match-end 0))
	    (setq wd (match-string 1 s))
	    (if (setq rep (org-entity-get-representation wd 'html))
		(setq s (replace-match rep t t s))
	      (setq start (+ start (length wd))))))))
  s)

(defun org-export-docbook-format-desc (desc)
  "Make sure DESC is valid as a description in a link."
  (save-match-data
    (org-docbook-do-expand desc)))

(defun org-export-docbook-convert-emphasize (string)
  "Apply emphasis for DocBook exporting."
  (let ((s 0) rpl)
    (while (string-match org-emph-re string s)
      (if (not (equal
		(substring string (match-beginning 3) (1+ (match-beginning 3)))
		(substring string (match-beginning 4) (1+ (match-beginning 4)))))
	  (setq s (match-beginning 0)
		rpl
		(concat
		 (match-string 1 string)
		 (nth 1 (assoc (match-string 3 string)
			       org-export-docbook-emphasis-alist))
		 (match-string 4 string)
		 (nth 2 (assoc (match-string 3 string)
			       org-export-docbook-emphasis-alist))
		 (match-string 5 string))
		string (replace-match rpl t t string)
		s (+ s (- (length rpl) 2)))
	(setq s (1+ s))))
    string))

(defun org-docbook-protect (string)
  (org-html-protect string))

;; For now, simply return string as it is.
(defun org-export-docbook-convert-special-strings (string)
  "Convert special characters in STRING to DocBook."
  string)

(defun org-export-docbook-get-footnotes (lines)
  "Given a list of LINES, return a list of alist footnotes."
  (let ((list nil) line)
    (while (setq line (pop lines))
      (if (string-match "^[ \t]*\\[\\([0-9]+\\)\\] \\(.+\\)" line)
	  (push (cons (match-string 1 line) (match-string 2 line))
		list)))
    list))

(defun org-export-docbook-format-image (src)
  "Create image element in DocBook."
  (save-match-data
    (let* ((caption (org-find-text-property-in-string 'org-caption src))
	   (attr (or (org-find-text-property-in-string 'org-attributes src)
		     ""))
	   (label (org-find-text-property-in-string 'org-label src))
	   (default-attr org-export-docbook-default-image-attributes)
	   tmp)
      (setq caption (and caption (org-html-do-expand caption)))
      (while (setq tmp (pop default-attr))
	(if (not (string-match (concat (car tmp) "=") attr))
	    (setq attr (concat attr " " (car tmp) "=" (cdr tmp)))))
      (format "<mediaobject%s>
<imageobject>\n<imagedata fileref=\"%s\" %s/>\n</imageobject>
%s</mediaobject>"
	      (if label (concat " xml:id=\"" label "\"") "")
	      src attr
	      (if caption
		  (concat "<caption>\n<para>"
			  caption
			  "</para>\n</caption>\n")
		"")
	      ))))

(defun org-export-docbook-preprocess (parameters)
  "Extra preprocessing work for DocBook export."
  ;; Merge lines starting with "\par" to one line.  Such lines are
  ;; regarded as the continuation of a long footnote.
  (goto-char (point-min))
  (while (re-search-forward "\n\\(\\\\par\\>\\)" nil t)
    (if (not (get-text-property (match-beginning 1) 'org-protected))
	(replace-match ""))))

(defun org-export-docbook-finalize-table (table)
  "Clean up TABLE and turn it into DocBook format.
This function adds a label to the table if it is available, and
also changes TABLE to informaltable if caption does not exist.
TABLE is a string containing the HTML code generated by
`org-format-table-html' for a table in Org-mode buffer."
  (let (table-with-label)
    ;; Get the label if it exists, and move it into the <table> element.
    (setq table-with-label
	  (if (string-match
	       "^<table \\(\\(.\\|\n\\)+\\)<a name=\"\\(.+\\)\" id=\".+\"></a>\n\\(\\(.\\|\n\\)+\\)</table>"
	       table)
	      (replace-match (concat "<table xml:id=\"" (match-string 3 table) "\" "
				     (match-string 1 table)
				     (match-string 4 table)
				     "</table>")
			     nil t table)
	    table))
    ;; Change <table> into <informaltable> if caption does not exist.
    (if (string-match
	 "^<table \\(\\(.\\|\n\\)+\\)<caption></caption>\n\\(\\(.\\|\n\\)+\\)</table>"
	 table-with-label)
	(replace-match (concat "<informaltable "
			       (match-string 1 table-with-label)
			       (match-string 3 table-with-label)
			       "</informaltable>")
		       nil t table-with-label)
      table-with-label)))

;; Note: This function is very similar to
;; org-export-html-convert-sub-super.  They can be merged in the future.
(defun org-export-docbook-convert-sub-super (string)
  "Convert sub- and superscripts in STRING for DocBook."
  (let (key c (s 0) (requireb (eq org-export-with-sub-superscripts '{})))
    (while (string-match org-match-substring-regexp string s)
      (cond
       ((and requireb (match-end 8)) (setq s (match-end 2)))
       ((get-text-property  (match-beginning 2) 'org-protected string)
	(setq s (match-end 2)))
       (t
	(setq s (match-end 1)
	      key (if (string= (match-string 2 string) "_")
		      "subscript"
		    "superscript")
	      c (or (match-string 8 string)
		    (match-string 6 string)
		    (match-string 5 string))
	      string (replace-match
		      (concat (match-string 1 string)
			      "<" key ">" c "</" key ">")
		      t t string)))))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string)))
    string))

(defun org-export-docbook-protect-tags (string)
  "Change ``<...>'' in string STRING into ``@<...>''.
This is normally needed when STRING contains DocBook elements
that need to be preserved in later phase of DocBook exporting."
  (let ((start 0))
    (while (string-match "<\\([^>]*\\)>" string start)
      (setq string (replace-match
		    "@<\\1>" t nil string)
	    start (match-end 0)))
    string))

(defun org-export-docbook-handle-time-stamps (line)
  "Format time stamps in string LINE."
  (let (replaced
	(kw-markup (org-export-docbook-protect-tags
		    org-export-docbook-keywords-markup))
	(ts-markup (org-export-docbook-protect-tags
		    org-export-docbook-timestamp-markup)))
    (while (string-match org-maybe-keyword-time-regexp line)
      (setq replaced
	    (concat replaced
		    (substring line 0 (match-beginning 0))
		    (if (match-end 1)
			(format kw-markup
				(match-string 1 line)))
		    " "
		    (format ts-markup
			    (substring (org-translate-time
					(match-string 3 line)) 1 -1)))
	    line (substring line (match-end 0))))
    (concat replaced line)))

(defun org-export-docbook-list-line (line pos struct prevs)
  "Insert list syntax in export buffer. Return LINE, maybe modified.

POS is the item position or line position the line had before
modifications to buffer. STRUCT is the list structure. PREVS is
the alist of previous items."
  (let* ((get-type
	  (function
	   ;; Translate type of list containing POS to "ordered",
	   ;; "variable" or "itemized".
	   (lambda (pos struct prevs)
	     (let ((type (org-list-get-list-type pos struct prevs)))
	       (cond
		((eq 'ordered type) "ordered")
		((eq 'descriptive type) "variable")
		(t "itemized"))))))
	 (get-closings
	  (function
	   ;; Return list of all items and sublists ending at POS, in
	   ;; reverse order.
	   (lambda (pos)
	     (let (out)
	       (catch 'exit
		 (mapc (lambda (e)
			 (let ((end (nth 6 e))
			       (item (car e)))
			   (cond
			    ((= end pos) (push item out))
			    ((>= item pos) (throw 'exit nil)))))
		       struct))
	       out)))))
    ;; First close any previous item, or list, ending at POS.
    (mapc (lambda (e)
	    (let* ((lastp (= (org-list-get-last-item e struct prevs) e))
		   (first-item (org-list-get-list-begin e struct prevs))
		   (type (funcall get-type first-item struct prevs)))
	      ;; Ending for every item
	      (org-export-docbook-close-para-maybe)
	      (insert (if (equal type "variable")
			  "</listitem></varlistentry>\n"
			"</listitem>\n"))
	      ;; We're ending last item of the list: end list.
	      (when lastp
		(insert (format "</%slist>\n" type))
		(org-export-docbook-open-para))))
	  (funcall get-closings pos))
    (cond
     ;; At an item: insert appropriate tags in export buffer.
     ((assq pos struct)
      (string-match (concat "[ \t]*\\(\\S-+[ \t]*\\)"
			    "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[a-zA-Z]\\)\\][ \t]*\\)?"
			    "\\(?:\\(\\[[ X-]\\]\\)[ \t]+\\)?"
			    "\\(?:\\(.*\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?"
			    "\\(.*\\)")
		    line)
      (let* ((checkbox (match-string 3 line))
	     (desc-tag (or (match-string 4 line) "???"))
	     (body (match-string 5 line))
	     (list-beg (org-list-get-list-begin pos struct prevs))
	     (firstp (= list-beg pos))
	     ;; Always refer to first item to determine list type, in
	     ;; case list is ill-formed.
	     (type (funcall get-type list-beg struct prevs))
	     ;; Special variables for ordered lists.
	     (counter (let ((count-tmp (org-list-get-counter pos struct)))
			(cond
			 ((not count-tmp) nil)
			 ((string-match "[A-Za-z]" count-tmp)
			  (- (string-to-char (upcase count-tmp)) 64))
			 ((string-match "[0-9]+" count-tmp)
			  count-tmp)))))
	;; When FIRSTP, a new list or sub-list is starting.
	(when firstp
	  (org-export-docbook-close-para-maybe)
	  (insert (format "<%slist>\n" type)))
	(insert (cond
		 ((equal type "variable")
		  (format "<varlistentry><term>%s</term><listitem>" desc-tag))
		 ((and (equal type "ordered") counter)
		  (format "<listitem override=\"%s\">" counter))
		 (t "<listitem>")))
	;; For DocBook, we need to open a para right after tag
	;; <listitem>.
	(org-export-docbook-open-para)
	;; If line had a checkbox, some additional modification is required.
	(when checkbox (setq body (concat checkbox " " body)))
	;; Return modified line
	body))
     ;; At a list ender:  normal text follows: need <para>.
     ((equal "ORG-LIST-END-MARKER" line)
      (org-export-docbook-open-para)
      (throw 'nextline nil))
     ;; Not at an item: return line unchanged (side-effects only).
     (t line))))

(provide 'org-docbook)

;;; org-docbook.el ends here
