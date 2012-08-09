;;; org-lparse.el --- Line-oriented parser-exporter for Org-mode

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Jambunathan K <kjambunathan at gmail dot com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org

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

;; `org-lparse' is the entry point for the generic line-oriented
;; exporter.  `org-do-lparse' is the genericized version of the
;; original `org-export-as-html' routine.

;; `org-lparse-native-backends' is a good starting point for
;; exploring the generic exporter.

;; Following new interactive commands are provided by this library.
;; `org-lparse', `org-lparse-and-open', `org-lparse-to-buffer'
;; `org-replace-region-by', `org-lparse-region'.

;; Note that the above routines correspond to the following routines
;; in the html exporter `org-export-as-html',
;; `org-export-as-html-and-open', `org-export-as-html-to-buffer',
;; `org-replace-region-by-html' and `org-export-region-as-html'.

;; The new interactive command `org-lparse-convert' can be used to
;; convert documents between various formats.  Use this to command,
;; for example, to convert odt file to doc or pdf format.

;;; Code:
(eval-when-compile
  (require 'cl))
(require 'org-exp)
(require 'org-list)
(require 'format-spec)

;;;###autoload
(defun org-lparse-and-open (target-backend native-backend arg
					   &optional file-or-buf)
  "Export outline to TARGET-BACKEND via NATIVE-BACKEND and open exported file.
If there is an active region, export only the region.  The prefix
ARG specifies how many levels of the outline should become
headlines.  The default is 3.  Lower levels will become bulleted
lists."
  (let (f (file-or-buf (or file-or-buf
			   (org-lparse target-backend native-backend
				       arg 'hidden))))
    (when file-or-buf
      (setq f (cond
	       ((bufferp file-or-buf) buffer-file-name)
	       ((file-exists-p file-or-buf) file-or-buf)
	       (t (error "org-lparse-and-open: This shouldn't happen"))))
      (message "Opening file %s" f)
      (org-open-file f)
      (when org-export-kill-product-buffer-when-displayed
	(kill-buffer (current-buffer))))))

;;;###autoload
(defun org-lparse-batch (target-backend &optional native-backend)
  "Call the function `org-lparse'.
This function can be used in batch processing as:
emacs   --batch
        --load=$HOME/lib/emacs/org.el
        --eval \"(setq org-export-headline-levels 2)\"
        --visit=MyFile --funcall org-lparse-batch"
  (setq native-backend (or native-backend target-backend))
  (org-lparse target-backend native-backend
	      org-export-headline-levels 'hidden))

;;;###autoload
(defun org-lparse-to-buffer (backend arg)
  "Call `org-lparse' with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to
`org-lparse'."
  (let ((tempbuf (format "*Org %s Export*" (upcase backend))))
      (org-lparse backend backend arg nil nil tempbuf)
      (when org-export-show-temporary-export-buffer
	(switch-to-buffer-other-window tempbuf))))

;;;###autoload
(defun org-replace-region-by (backend beg end)
  "Assume the current region has org-mode syntax, and convert it to HTML.
This can be used in any buffer.  For example, you could write an
itemized list in org-mode syntax in an HTML buffer and then use
this command to convert it."
  (let (reg backend-string buf pop-up-frames)
    (save-window-excursion
      (if (eq major-mode 'org-mode)
	  (setq backend-string (org-lparse-region backend beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq backend-string (org-lparse-region backend (point-min)
						  (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert backend-string)))

;;;###autoload
(defun org-lparse-region (backend beg end &optional body-only buffer)
  "Convert region from BEG to END in org-mode buffer to HTML.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted HTML.  If BUFFER is the symbol `string', return the
produced HTML as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq html (org-lparse-region \"html\" beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only return the buffer."
  (let ((transient-mark-mode t) (zmacs-regions t)
	ext-plist rtn)
    (setq ext-plist (plist-put ext-plist :ignore-subtree-p t))
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-lparse backend backend nil nil ext-plist buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (org-called-interactively-p 'any) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

(defvar org-lparse-par-open nil)

(defun org-lparse-should-inline-p (filename descp)
   "Return non-nil if link FILENAME should be inlined.
The decision to inline the FILENAME link is based on the current
settings.  DESCP is the boolean of whether there was a link
description.  See variables `org-export-html-inline-images' and
`org-export-html-inline-image-extensions'."
   (let ((inline-images (org-lparse-get 'INLINE-IMAGES))
	 (inline-image-extensions
	  (org-lparse-get 'INLINE-IMAGE-EXTENSIONS)))
        (and (or (eq t inline-images) (and inline-images (not descp)))
	     (org-file-image-p filename inline-image-extensions))))

(defun org-lparse-format-org-link (line opt-plist)
  "Return LINE with markup of Org mode links.
OPT-PLIST is the export options list."
  (let ((start 0)
	(current-dir (if buffer-file-name
			 (file-name-directory buffer-file-name)
		       default-directory))
	(link-validate (plist-get opt-plist :link-validation-function))
	type id-file fnc
	rpl path attr desc descp desc1 desc2 link
	org-lparse-link-description-is-image)
    (while (string-match org-bracket-link-analytic-regexp++ line start)
      (setq org-lparse-link-description-is-image nil)
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
      (setq path (org-extract-attributes path))
      (setq attr (get-text-property 0 'org-attributes path))
      (setq desc1 (if (match-end 5) (match-string 5 line))
	    desc2 (if (match-end 2) (concat type ":" path) path)
	    descp (and desc1 (not (equal desc1 desc2)))
	    desc (or desc1 desc2))
      ;; Make an image out of the description if that is so wanted
      (when (and descp (org-file-image-p
			desc (org-lparse-get 'INLINE-IMAGE-EXTENSIONS)))
	(setq org-lparse-link-description-is-image t)
	(save-match-data
	  (if (string-match "^file:" desc)
	      (setq desc (substring desc (match-end 0)))))
	(save-match-data
	  (setq desc (org-add-props
			 (org-lparse-format 'INLINE-IMAGE desc)
			 '(org-protected t)))))
      (cond
       ((equal type "internal")
	(let
	    ((frag-0
	      (if (= (string-to-char path) ?#)
		  (substring path 1)
		path)))
	  (setq rpl
		(org-lparse-format
		 'ORG-LINK opt-plist "" "" (org-solidify-link-text
					    (save-match-data
					      (org-link-unescape frag-0))
					    nil) desc attr descp))))
       ((and (equal type "id")
	     (setq id-file (org-id-find-id-file path)))
	;; This is an id: link to another file (if it was the same file,
	;; it would have become an internal link...)
	(save-match-data
	  (setq id-file (file-relative-name
			 id-file
			 (file-name-directory org-current-export-file)))
	  (setq rpl
		(org-lparse-format
		 'ORG-LINK opt-plist type id-file
		 (concat (if (org-uuidgen-p path) "ID-") path)
		 desc attr descp))))
       ((member type '("http" "https"))
	;; standard URL, can inline as image
	(setq rpl
	      (org-lparse-format
	       'ORG-LINK opt-plist type path nil desc attr descp)))
       ((member type '("ftp" "mailto" "news"))
	;; standard URL, can't inline as image
	(setq rpl
	      (org-lparse-format
	       'ORG-LINK opt-plist type path nil desc attr descp)))

       ((string= type "coderef")
	(setq rpl (org-lparse-format
		   'ORG-LINK opt-plist type "" path desc nil descp)))

       ((functionp (setq fnc (nth 2 (assoc type org-link-protocols))))
	;; The link protocol has a function for format the link
	(setq rpl (save-match-data
		    (funcall fnc (org-link-unescape path)
			     desc1 (and (boundp 'org-lparse-backend)
					(case org-lparse-backend
					  (xhtml 'html)
					  (t org-lparse-backend)))))))
       ((string= type "file")
	;; FILE link
	(save-match-data
	  (let*
	      ((components
		(if
		    (string-match "::\\(.*\\)" path)
		    (list
		     (replace-match "" t nil path)
		     (match-string 1 path))
		  (list path nil)))

	       ;;The proper path, without a fragment
	       (path-1
		(first components))

	       ;;The raw fragment
	       (fragment-0
		(second components))

	       ;;Check the fragment.  If it can't be used as
	       ;;target fragment we'll pass nil instead.
	       (fragment-1
		(if
		    (and fragment-0
			 (not (string-match "^[0-9]*$" fragment-0))
			 (not (string-match "^\\*" fragment-0))
			 (not (string-match "^/.*/$" fragment-0)))
		    (org-solidify-link-text
		     (org-link-unescape fragment-0))
		  nil))
	       (desc-2
		;;Description minus "file:" and ".org"
		(if (string-match "^file:" desc)
		    (let
			((desc-1 (replace-match "" t t desc)))
		      (if (string-match "\\.org$" desc-1)
			  (replace-match "" t t desc-1)
			desc-1))
		  desc)))

	    (setq rpl
		  (if
		      (and
		       (functionp link-validate)
		       (not (funcall link-validate path-1 current-dir)))
		      desc
		    (org-lparse-format
		     'ORG-LINK opt-plist "file" path-1 fragment-1
		     desc-2 attr descp))))))

       (t
	;; just publish the path, as default
	(setq rpl (concat "<i>&lt;" type ":"
			  (save-match-data (org-link-unescape path))
			  "&gt;</i>"))))
      (setq line (replace-match rpl t t line)
	    start (+ start (length rpl))))
    line))

(defvar org-lparse-par-open-stashed)	; bound during `org-do-lparse'
(defun org-lparse-stash-save-paragraph-state ()
  (assert (zerop org-lparse-par-open-stashed))
  (setq org-lparse-par-open-stashed org-lparse-par-open)
  (setq org-lparse-par-open nil))

(defun org-lparse-stash-pop-paragraph-state ()
  (setq org-lparse-par-open org-lparse-par-open-stashed)
  (setq org-lparse-par-open-stashed 0))

(defmacro with-org-lparse-preserve-paragraph-state (&rest body)
  `(let ((org-lparse-do-open-par org-lparse-par-open))
     (org-lparse-end-paragraph)
     ,@body
     (when org-lparse-do-open-par
       (org-lparse-begin-paragraph))))
(def-edebug-spec with-org-lparse-preserve-paragraph-state (body))

(defvar org-lparse-native-backends nil
  "List of native backends registered with `org-lparse'.
A backend can use `org-lparse-register-backend' to add itself to
this list.

All native backends must implement a get routine and a mandatory
set of callback routines.

The get routine must be named as org-<backend>-get where backend
is the name of the backend.  The exporter uses `org-lparse-get'
and retrieves the backend-specific callback by querying for
ENTITY-CONTROL and ENTITY-FORMAT variables.

For the sake of illustration, the html backend implements
`org-xhtml-get'.  It returns
`org-xhtml-entity-control-callbacks-alist' and
`org-xhtml-entity-format-callbacks-alist' as the values of
ENTITY-CONTROL and ENTITY-FORMAT settings.")

(defun org-lparse-register-backend (backend)
  "Make BACKEND known to `org-lparse' library.
Add BACKEND to `org-lparse-native-backends'."
  (when backend
    (setq backend (cond
		   ((symbolp backend) (symbol-name backend))
		   ((stringp backend) backend)
		   (t (error "Error while registering backend: %S" backend))))
    (add-to-list 'org-lparse-native-backends backend)))

(defun org-lparse-unregister-backend (backend)
  (setq org-lparse-native-backends
	(remove (cond
		 ((symbolp backend) (symbol-name backend))
		 ((stringp backend) backend))
		org-lparse-native-backends))
  (message "Unregistered backend %S" backend))

(defun org-lparse-do-reachable-formats (in-fmt)
  "Return verbose info about formats to which IN-FMT can be converted.
Return a list where each element is of the
form (CONVERTER-PROCESS . OUTPUT-FMT-ALIST).  See
`org-export-odt-convert-processes' for CONVERTER-PROCESS and see
`org-export-odt-convert-capabilities' for OUTPUT-FMT-ALIST."
  (let (reachable-formats)
    (dolist (backend org-lparse-native-backends reachable-formats)
      (let* ((converter (org-lparse-backend-get
			 backend 'CONVERT-METHOD))
	     (capabilities (org-lparse-backend-get
			    backend 'CONVERT-CAPABILITIES)))
	(when converter
	  (dolist (c capabilities)
	    (when (member in-fmt (nth 1 c))
	      (push (cons converter (nth 2 c)) reachable-formats))))))))

(defun org-lparse-reachable-formats (in-fmt)
  "Return list of formats to which IN-FMT can be converted.
The list of the form (OUTPUT-FMT-1 OUTPUT-FMT-2 ...)."
  (let (l)
    (mapc (lambda (e) (add-to-list 'l e))
	  (apply 'append (mapcar
			  (lambda (e) (mapcar 'car (cdr e)))
			  (org-lparse-do-reachable-formats in-fmt))))
    l))

(defun org-lparse-reachable-p (in-fmt out-fmt)
  "Return non-nil if IN-FMT can be converted to OUT-FMT."
  (catch 'done
    (let ((reachable-formats (org-lparse-do-reachable-formats in-fmt)))
      (dolist (e reachable-formats)
	(let ((out-fmt-spec (assoc out-fmt (cdr e))))
	  (when out-fmt-spec
	    (throw 'done (cons (car e) out-fmt-spec))))))))

(defun org-lparse-backend-is-native-p (backend)
  (member backend org-lparse-native-backends))

(defun org-lparse (target-backend native-backend arg
				  &optional hidden ext-plist
				  to-buffer body-only pub-dir)
  "Export the outline to various formats.
If there is an active region, export only the region.  The
outline is first exported to NATIVE-BACKEND and optionally
converted to TARGET-BACKEND.  See `org-lparse-native-backends'
for list of known native backends.  Each native backend can
specify a converter and list of target backends it exports to
using the CONVERT-PROCESS and OTHER-BACKENDS settings of it's get
method.  See `org-xhtml-get' for an illustrative example.

ARG is a prefix argument that specifies how many levels of
outline should become headlines.  The default is 3.  Lower levels
will become bulleted lists.

HIDDEN is obsolete and does nothing.

EXT-PLIST is a property list that controls various aspects of
export.  The settings here override org-mode's default settings
and but are inferior to file-local settings.

TO-BUFFER dumps the exported lines to a buffer or a string
instead of a file.  If TO-BUFFER is the symbol `string' return the
exported lines as a string.  If TO-BUFFER is non-nil, create a
buffer with that name and export to that buffer.

BODY-ONLY controls the presence of header and footer lines in
exported text.  If BODY-ONLY is non-nil, don't produce the file
header and footer, simply return the content of <body>...</body>,
without even the body tags themselves.

PUB-DIR specifies the publishing directory."
  (let* ((org-lparse-backend (intern native-backend))
	 (org-lparse-other-backend (and target-backend
					(intern target-backend))))
    (unless (org-lparse-backend-is-native-p native-backend)
      (error "Don't know how to export natively to backend %s" native-backend))

    (unless (or (equal native-backend target-backend)
		(org-lparse-reachable-p native-backend target-backend))
      (error "Don't know how to export to backend %s %s" target-backend
	     (format "via %s" native-backend)))
    (run-hooks 'org-export-first-hook)
    (org-do-lparse arg hidden ext-plist to-buffer body-only pub-dir)))

(defcustom org-lparse-use-flashy-warning nil
  "Control flashing of messages logged with `org-lparse-warn'.
When non-nil, messages are fontified with warning face and the
exporter lingers for a while to catch user's attention."
  :type 'boolean
  :group 'org-lparse)

(defun org-lparse-convert-read-params ()
  "Return IN-FILE and OUT-FMT params for `org-lparse-do-convert'.
This is a helper routine for interactive use."
  (let* ((input (if (featurep 'ido) 'ido-completing-read 'completing-read))
	 (in-file (read-file-name "File to be converted: "
				  nil buffer-file-name t))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt-choices (org-lparse-reachable-formats in-fmt))
	 (out-fmt
	  (or (and out-fmt-choices
		   (funcall input "Output format:  "
			    out-fmt-choices nil nil nil))
	      (error
	       "No known converter or no known output formats for %s files"
	       in-fmt))))
    (list in-file out-fmt)))

(eval-when-compile
  (require 'browse-url))

(defun org-lparse-do-convert (in-file out-fmt &optional prefix-arg)
  "Workhorse routine for `org-export-odt-convert'."
  (require 'browse-url)
  (let* ((in-file (expand-file-name (or in-file buffer-file-name)))
	 (dummy (or (file-readable-p in-file)
		    (error "Cannot read %s" in-file)))
	 (in-fmt (file-name-extension in-file))
	 (out-fmt (or out-fmt (error "Output format unspecified")))
	 (how (or (org-lparse-reachable-p in-fmt out-fmt)
		  (error "Cannot convert from %s format to %s format?"
			 in-fmt out-fmt)))
	 (convert-process (car how))
	 (out-file (concat (file-name-sans-extension in-file) "."
			   (nth 1 (or (cdr how) out-fmt))))
	 (extra-options (or (nth 2 (cdr how)) ""))
	 (out-dir (file-name-directory in-file))
	 (cmd (format-spec convert-process
			   `((?i . ,(shell-quote-argument in-file))
			     (?I . ,(browse-url-file-url in-file))
			     (?f . ,out-fmt)
			     (?o . ,out-file)
			     (?O . ,(browse-url-file-url out-file))
			     (?d . , (shell-quote-argument out-dir))
			     (?D . ,(browse-url-file-url out-dir))
			     (?x . ,extra-options)))))
    (when (file-exists-p out-file)
      (delete-file out-file))

    (message "Executing %s" cmd)
    (let ((cmd-output (shell-command-to-string cmd)))
      (message "%s" cmd-output))

    (cond
     ((file-exists-p out-file)
      (message "Exported to %s" out-file)
      (when prefix-arg
	(message "Opening %s..."  out-file)
	(org-open-file out-file))
      out-file)
     (t
      (message "Export to %s failed" out-file)
      nil))))

(defvar org-lparse-insert-tag-with-newlines 'both)

;; Following variables are let-bound during `org-lparse'
(defvar org-lparse-dyn-first-heading-pos)
(defvar org-lparse-toc)
(defvar org-lparse-entity-control-callbacks-alist)
(defvar org-lparse-entity-format-callbacks-alist)
(defvar org-lparse-backend nil
  "The native backend to which the document is currently exported.
This variable is let bound during `org-lparse'.  Valid values are
one of the symbols corresponding to `org-lparse-native-backends'.

Compare this variable with `org-export-current-backend' which is
bound only during `org-export-preprocess-string' stage of the
export process.

See also `org-lparse-other-backend'.")

(defvar org-lparse-other-backend nil
  "The target backend to which the document is currently exported.
This variable is let bound during `org-lparse'.  This variable is
set to either `org-lparse-backend' or one of the symbols
corresponding to OTHER-BACKENDS specification of the
org-lparse-backend.

For example, if a document is exported to \"odt\" then both
org-lparse-backend and org-lparse-other-backend are bound to
'odt.  On the other hand, if a document is exported to \"odt\"
and then converted to \"doc\" then org-lparse-backend is set to
'odt and org-lparse-other-backend is set to 'doc.")

(defvar org-lparse-body-only nil
  "Bind this to BODY-ONLY arg of `org-lparse'.")

(defvar org-lparse-to-buffer nil
  "Bind this to TO-BUFFER arg of `org-lparse'.")

(defun org-lparse-get-block-params (params)
  (save-match-data
    (when params
      (setq params (org-trim params))
      (unless (string-match "\\`(.*)\\'" params)
	(setq params (format "(%s)" params)))
      (ignore-errors (read params)))))

(defvar org-heading-keyword-regexp-format) ; defined in org.el
(defvar org-lparse-special-blocks '("list-table" "annotation"))
(defun org-do-lparse (arg &optional hidden ext-plist
			  to-buffer body-only pub-dir)
  "Export the outline to various formats.
See `org-lparse' for more information. This function is a
html-agnostic version of the `org-export-as-html' function in 7.5
version."
  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

  (org-lparse-warn
   (format "Exporting to %s using org-lparse..."
	   (upcase (symbol-name
		    (or org-lparse-backend org-lparse-other-backend)))))

  (setq-default org-todo-line-regexp org-todo-line-regexp)
  (setq-default org-deadline-line-regexp org-deadline-line-regexp)
  (setq-default org-done-keywords org-done-keywords)
  (setq-default org-maybe-keyword-time-regexp org-maybe-keyword-time-regexp)
  (let* (hfy-user-sheet-assoc		; let `htmlfontify' know that
					; we are interested in
					; collecting styles
	 org-lparse-encode-pending
	 org-lparse-par-open
	 (org-lparse-par-open-stashed 0)

	 ;; list related vars
	 (org-lparse-list-stack '())

	 ;; list-table related vars
	 org-lparse-list-table-p
	 org-lparse-list-table:table-cell-open
	 org-lparse-list-table:table-row
	 org-lparse-list-table:lines

	 org-lparse-outline-text-open
	 (org-lparse-latex-fragment-fallback ; currently used only by
					; odt exporter
	  (or (ignore-errors (org-lparse-get 'LATEX-FRAGMENT-FALLBACK))
	      (if (and (org-check-external-command "latex" "" t)
		       (org-check-external-command "dvipng" "" t))
		  'dvipng
		'verbatim)))
	 (org-lparse-insert-tag-with-newlines 'both)
	 (org-lparse-to-buffer to-buffer)
	 (org-lparse-body-only body-only)
	 (org-lparse-entity-control-callbacks-alist
	  (org-lparse-get 'ENTITY-CONTROL))
	 (org-lparse-entity-format-callbacks-alist
	  (org-lparse-get 'ENTITY-FORMAT))
	 (opt-plist
	  (org-export-process-option-filters
	   (org-combine-plists (org-default-export-plist)
			       ext-plist
			       (org-infile-export-plist))))
	 (body-only (or body-only (plist-get opt-plist :body-only)))
	 valid org-lparse-dyn-first-heading-pos
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
	  (or pub-dir (org-lparse-get 'EXPORT-DIR opt-plist)))
	 (org-current-export-file buffer-file-name)
	 (level 0) (line "") (origline "") txt todo
	 (umax nil)
	 (umax-toc nil)
	 (filename (if to-buffer nil
		     (expand-file-name
		      (concat
		       (file-name-sans-extension
			(or (and subtree-p
				 (org-entry-get (region-beginning)
						"EXPORT_FILE_NAME" t))
			    (file-name-nondirectory buffer-file-name)))
		       "." (org-lparse-get 'FILE-NAME-EXTENSION opt-plist))
		      (file-name-as-directory
		       (or pub-dir (org-lparse-get 'EXPORT-DIR opt-plist))))))
	 (current-dir (if buffer-file-name
			  (file-name-directory buffer-file-name)
			default-directory))
	 (auto-insert nil) ; Avoid any auto-insert stuff for the new file
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string)
		       (get-buffer-create (org-lparse-get 'EXPORT-BUFFER-NAME)))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect
		    (or (let ((f (org-lparse-get 'INIT-METHOD)))
			  (and f (functionp f) (funcall f filename)))
			filename))))
	 (org-levels-open (make-vector org-level-max nil))
	 (dummy (mapc
		 (lambda(p)
		   (let* ((val (plist-get opt-plist p))
			  (val (org-xml-encode-org-text-skip-links val)))
		     (setq opt-plist (plist-put opt-plist p val))))
		 '(:date :author :keywords :description)))
	 (date (plist-get opt-plist :date))
	 (date (cond
		((and date (string-match "%" date))
		 (format-time-string date))
		(date date)
		(t (format-time-string "%Y-%m-%d %T %Z"))))
	 (dummy (setq opt-plist (plist-put opt-plist :effective-date date)))
	 (title       (org-xml-encode-org-text-skip-links
		       (or (and subtree-p (org-export-get-title-from-subtree))
			   (plist-get opt-plist :title)
			   (and (not body-only)
				(not
				 (plist-get opt-plist :skip-before-1st-heading))
				(org-export-grab-title-from-buffer))
			   (and buffer-file-name
				(file-name-sans-extension
				 (file-name-nondirectory buffer-file-name)))
			   "UNTITLED")))
	 (dummy (setq opt-plist (plist-put opt-plist :title title)))
	 (html-table-tag (plist-get opt-plist :html-table-tag))
	 (quote-re0 (concat "^ *" org-quote-string "\\( +\\|[ \t]*$\\)"))
	 (quote-re (format org-heading-keyword-regexp-format
			   org-quote-string))
	 (org-lparse-dyn-current-environment nil)
	 ;; Get the language-dependent settings
	 (lang-words (or (assoc (plist-get opt-plist :language)
				org-export-language-setup)
			 (assoc "en" org-export-language-setup)))
	 (dummy (setq opt-plist (plist-put opt-plist :lang-words lang-words)))
	 (head-count  0) cnt
	 (start       0)
	 (coding-system-for-write
	  (or (ignore-errors (org-lparse-get 'CODING-SYSTEM-FOR-WRITE))
	      (and (boundp 'buffer-file-coding-system)
		   buffer-file-coding-system)))
	 (save-buffer-coding-system
	  (or (ignore-errors (org-lparse-get 'CODING-SYSTEM-FOR-SAVE))
	      (and (boundp 'buffer-file-coding-system)
		   buffer-file-coding-system)))
	 (region
	  (buffer-substring
	   (if region-p (region-beginning) (point-min))
	   (if region-p (region-end) (point-max))))
	 (org-export-have-math nil)
	 (org-export-footnotes-seen nil)
	 (org-export-footnotes-data (org-footnote-all-labels 'with-defs))
	 (org-footnote-insert-pos-for-preprocessor 'point-min)
	 (org-lparse-opt-plist opt-plist)
	 (lines
	  (org-split-string
	   (org-export-preprocess-string
	    region
	    :emph-multiline t
	    :for-backend (if (equal org-lparse-backend 'xhtml) ; hack
			     'html
			   org-lparse-backend)
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
	 table-open
	 table-buffer table-orig-buffer
	 ind
	 rpl path attr desc descp desc1 desc2 link
	 snumber fnc
	 footnotes footref-seen
	 org-lparse-output-buffer
	 org-lparse-footnote-definitions
	 org-lparse-footnote-number
	 ;; collection
	 org-lparse-collect-buffer
	 (org-lparse-collect-count 0)	; things will get haywire if
					; collections are chained. Use
					; this variable to assert this
					; pre-requisite
	 org-lparse-toc
	 href
	 )

    (let ((inhibit-read-only t))
      (org-unmodified
       (remove-text-properties (point-min) (point-max)
			       '(:org-license-to-kill t))))

    (message "Exporting...")
    (org-init-section-numbers)

    ;; Switch to the output buffer
    (setq org-lparse-output-buffer buffer)
    (set-buffer org-lparse-output-buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (fundamental-mode)
    (org-install-letbind)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    (let ((case-fold-search nil)
	  (org-odd-levels-only odd))
      ;; create local variables for all options, to make sure all called
      ;; functions get the correct information
      (mapc (lambda (x)
	      (set (make-local-variable (nth 2 x))
		   (plist-get opt-plist (car x))))
	    org-export-plist-vars)
      (setq umax (if arg (prefix-numeric-value arg)
		   org-export-headline-levels))
      (setq umax-toc (if (integerp org-export-with-toc)
			 (min org-export-with-toc umax)
		       umax))
      (setq org-lparse-opt-plist
	    (plist-put org-lparse-opt-plist :headline-levels  umax))

      (when (and org-export-with-toc (not body-only))
	(setq lines (org-lparse-prepare-toc
		     lines level-offset opt-plist umax-toc)))

      (unless body-only
	(org-lparse-begin 'DOCUMENT-CONTENT opt-plist)
	(org-lparse-begin 'DOCUMENT-BODY opt-plist))

      (setq head-count 0)
      (org-init-section-numbers)

      (org-lparse-begin-paragraph)

      (while (setq line (pop lines) origline line)
	(catch 'nextline
	  (when (and (org-lparse-current-environment-p 'quote)
		     (string-match org-outline-regexp-bol line))
	    (org-lparse-end-environment 'quote))

	  (when (org-lparse-current-environment-p 'quote)
	    (org-lparse-insert 'LINE line)
	    (throw 'nextline nil))

	  ;; Fixed-width, verbatim lines (examples)
	  (when (and org-export-with-fixed-width
		     (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)" line))
	    (when (not (org-lparse-current-environment-p 'fixedwidth))
	      (org-lparse-begin-environment 'fixedwidth))
	    (org-lparse-insert 'LINE (match-string 3 line))
	    (when (or (not lines)
		      (not (string-match "^[ \t]*:\\(\\([ \t]\\|$\\)\\(.*\\)\\)"
					 (car lines))))
	      (org-lparse-end-environment 'fixedwidth))
	    (throw 'nextline nil))

	  ;; Native Text
	  (when (and (get-text-property 0 'org-native-text line)
		     ;; Make sure it is the entire line that is protected
		     (not (< (or (next-single-property-change
				  0 'org-native-text line) 10000)
			     (length line))))
	    (let ((ind (get-text-property 0 'original-indentation line)))
	      (org-lparse-begin-environment 'native)
	      (org-lparse-insert 'LINE line)
	      (while (and lines
			  (or (= (length (car lines)) 0)
			      (not ind)
			      (equal ind (get-text-property
					  0 'original-indentation (car lines))))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-native-text (car lines))))
		(org-lparse-insert 'LINE (pop lines)))
	      (org-lparse-end-environment 'native))
	    (throw 'nextline nil))

	  ;; Protected HTML
	  (when (and (get-text-property 0 'org-protected line)
		     ;; Make sure it is the entire line that is protected
		     (not (< (or (next-single-property-change
				  0 'org-protected line) 10000)
			     (length line))))
	    (let ((ind (get-text-property 0 'original-indentation line)))
	      (org-lparse-insert 'LINE line)
	      (while (and lines
			  (or (= (length (car lines)) 0)
			      (not ind)
			      (equal ind (get-text-property
					  0 'original-indentation (car lines))))
			  (or (= (length (car lines)) 0)
			      (get-text-property 0 'org-protected (car lines))))
		(org-lparse-insert 'LINE (pop lines))))
	    (throw 'nextline nil))

	  ;; Blockquotes, verse, and center
	  (when (string-match
		 "^ORG-\\(.+\\)-\\(START\\|END\\)\\([ \t]+.*\\)?$" line)
	    (let* ((style (intern (downcase (match-string 1 line))))
		   (env-options-plist (org-lparse-get-block-params
				       (match-string 3 line)))
		   (f (cdr (assoc (match-string 2 line)
				  '(("START" . org-lparse-begin-environment)
				    ("END" . org-lparse-end-environment))))))
	      (when (memq style
			  (append
			   '(blockquote verse center)
			   (mapcar 'intern org-lparse-special-blocks)))
		(funcall f style env-options-plist)
		(throw 'nextline nil))))

	  (run-hooks 'org-export-html-after-blockquotes-hook)
	  (when (org-lparse-current-environment-p 'verse)
	    (let ((i (org-get-string-indentation line)))
	      (if (> i 0)
		  (setq line (concat
			      (let ((org-lparse-encode-pending t))
				(org-lparse-format 'SPACES (* 2 i)))
			      " " (org-trim line))))
	      (unless (string-match "\\\\\\\\[ \t]*$" line)
		(setq line (concat line "\\\\")))))

	  ;; make targets to anchors
	  (setq start 0)
	  (while (string-match
		  "<<<?\\([^<>]*\\)>>>?\\((INVISIBLE)\\)?[ \t]*\n?" line start)
	    (cond
	     ((get-text-property (match-beginning 1) 'org-protected line)
	      (setq start (match-end 1)))
	     ((match-end 2)
	      (setq line (replace-match
			  (let ((org-lparse-encode-pending t))
			    (org-lparse-format
			     'ANCHOR "" (org-solidify-link-text
					 (match-string 1 line))))
			  t t line)))
	     ((and org-export-with-toc (equal (string-to-char line) ?*))
	      ;; FIXME: NOT DEPENDENT on TOC?????????????????????
	      (setq line (replace-match
			  (let ((org-lparse-encode-pending t))
			    (org-lparse-format
			     'FONTIFY (match-string 1 line) "target"))
			  ;; (concat "@<i>" (match-string 1 line) "@</i> ")
			  t t line)))
	     (t
	      (setq line (replace-match
			  (concat
			   (let ((org-lparse-encode-pending t))
			     (org-lparse-format
			      'ANCHOR (match-string 1 line)
			      (org-solidify-link-text (match-string 1 line))
			      "target")) " ")
			  t t line)))))

	  (let ((org-lparse-encode-pending t))
	    (setq line (org-lparse-handle-time-stamps line)))

	  ;; replace "&" by "&amp;", "<" and ">" by "&lt;" and "&gt;"
	  ;; handle @<..> HTML tags (replace "@&gt;..&lt;" by "<..>")
	  ;; Also handle sub_superscripts and checkboxes
	  (or (string-match org-table-hline-regexp line)
	      (string-match "^[ \t]*\\([+]-\\||[ ]\\)[-+ |]*[+|][ \t]*$" line)
	      (setq line (org-xml-encode-org-text-skip-links line)))

	  (setq line (org-lparse-format-org-link line opt-plist))

	  ;; TODO items
	  (if (and org-todo-line-regexp
		   (string-match org-todo-line-regexp line)
		   (match-beginning 2))
	      (setq line (concat
			  (substring line 0 (match-beginning 2))
			  (org-lparse-format 'TODO (match-string 2 line))
			  (substring line (match-end 2)))))

	  ;; Does this contain a reference to a footnote?
	  (when org-export-with-footnotes
	    (setq start 0)
	    (while (string-match "\\([^* \t].*?\\)[ \t]*\\[\\([0-9]+\\)\\]" line start)
	      ;; Discard protected matches not clearly identified as
	      ;; footnote markers.
	      (if (or (get-text-property (match-beginning 2) 'org-protected line)
		      (not (get-text-property (match-beginning 2) 'org-footnote line)))
		  (setq start (match-end 2))
		(let ((n (match-string 2 line)) refcnt a)
		  (if (setq a (assoc n footref-seen))
		      (progn
			(setcdr a (1+ (cdr a)))
			(setq refcnt (cdr a)))
		    (setq refcnt 1)
		    (push (cons n 1) footref-seen))
		  (setq line
			(replace-match
			 (concat
			  (or (match-string 1 line) "")
			  (org-lparse-format
			   'FOOTNOTE-REFERENCE
			   n (cdr (assoc n org-lparse-footnote-definitions))
			   refcnt)
			  ;; If another footnote is following the
			  ;; current one, add a separator.
			  (if (save-match-data
				(string-match "\\`\\[[0-9]+\\]"
					      (substring line (match-end 0))))
			      (ignore-errors
				(org-lparse-get 'FOOTNOTE-SEPARATOR))
			    ""))
			 t t line))))))

	  (cond
	   ((string-match "^\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*$" line)
	    ;; This is a headline
	    (setq level (org-tr-level (- (match-end 1) (match-beginning 1)
					 level-offset))
		  txt (match-string 2 line))
	    (if (string-match quote-re0 txt)
		(setq txt (replace-match "" t t txt)))
	    (if (<= level (max umax umax-toc))
		(setq head-count (+ head-count 1)))
	    (unless org-lparse-dyn-first-heading-pos
	      (setq org-lparse-dyn-first-heading-pos (point)))
	    (org-lparse-begin-level level txt umax head-count)

	    ;; QUOTES
	    (when (string-match quote-re line)
	      (org-lparse-begin-environment 'quote)))

	   ((and org-export-with-tables
		 (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)" line))
	    (when (not table-open)
	      ;; New table starts
	      (setq table-open t table-buffer nil table-orig-buffer nil))

	    ;; Accumulate lines
	    (setq table-buffer (cons line table-buffer)
		  table-orig-buffer (cons origline table-orig-buffer))
	    (when (or (not lines)
		      (not (string-match "^\\([ \t]*\\)\\(|\\|\\+-+\\+\\)"
					 (car lines))))
	      (setq table-open nil
		    table-buffer (nreverse table-buffer)
		    table-orig-buffer (nreverse table-orig-buffer))
	      (org-lparse-end-paragraph)
	      (when org-lparse-list-table-p
		(error "Regular tables are not allowed in a list-table block"))
	      (org-lparse-insert 'TABLE table-buffer table-orig-buffer)))

	   ;; Normal lines
	   (t
	    ;; This line either is list item or end a list.
	    (when (get-text-property 0 'list-item line)
	      (setq line (org-lparse-export-list-line
			  line
			  (get-text-property 0 'list-item line)
			  (get-text-property 0 'list-struct line)
			  (get-text-property 0 'list-prevs line))))

	    ;; Horizontal line
	    (when (string-match "^[ \t]*-\\{5,\\}[ \t]*$" line)
	      (with-org-lparse-preserve-paragraph-state
	       (org-lparse-insert 'HORIZONTAL-LINE))
	      (throw 'nextline nil))

	    ;; Empty lines start a new paragraph.  If hand-formatted lists
	    ;; are not fully interpreted, lines starting with "-", "+", "*"
	    ;; also start a new paragraph.
	    (when (string-match "^ [-+*]-\\|^[ \t]*$" line)
	      (when org-lparse-footnote-number
		(org-lparse-end-footnote-definition org-lparse-footnote-number)
		(setq org-lparse-footnote-number nil))
	      (org-lparse-begin-paragraph))

	    ;; Is this the start of a footnote?
	    (when org-export-with-footnotes
	      (when (and (boundp 'footnote-section-tag-regexp)
			 (string-match (concat "^" footnote-section-tag-regexp)
				       line))
		;; ignore this line
		(throw 'nextline nil))
	      (when (string-match "^[ \t]*\\[\\([0-9]+\\)\\]" line)
		(org-lparse-end-paragraph)
		(setq org-lparse-footnote-number (match-string 1 line))
		(setq line (replace-match "" t t line))
		(org-lparse-begin-footnote-definition org-lparse-footnote-number)))
	    ;; Check if the line break needs to be conserved
	    (cond
	     ((string-match "\\\\\\\\[ \t]*$" line)
	      (setq line (replace-match
			  (org-lparse-format 'LINE-BREAK)
			  t t line)))
	     (org-export-preserve-breaks
	      (setq line (concat line (org-lparse-format 'LINE-BREAK)))))

	    ;; Check if a paragraph should be started
	    (let ((start 0))
	      (while (and org-lparse-par-open
			  (string-match "\\\\par\\>" line start))
		(error "FIXME")
		;; Leave a space in the </p> so that the footnote matcher
		;; does not see this.
		(if (not (get-text-property (match-beginning 0)
					    'org-protected line))
		    (setq line (replace-match "</p ><p >" t t line)))
		(setq start (match-end 0))))

	    (org-lparse-insert 'LINE line)))))

      ;; Properly close all local lists and other lists
      (when (org-lparse-current-environment-p 'quote)
	(org-lparse-end-environment 'quote))

      (org-lparse-end-level 1 umax)

      ;; the </div> to close the last text-... div.
      (when (and (> umax 0) org-lparse-dyn-first-heading-pos)
	(org-lparse-end-outline-text-or-outline))

      (org-lparse-end 'DOCUMENT-BODY opt-plist)
      (unless body-only
	(org-lparse-end 'DOCUMENT-CONTENT))

      (org-lparse-end 'EXPORT)

      ;; kill collection buffer
      (when org-lparse-collect-buffer
	(kill-buffer org-lparse-collect-buffer))

      (goto-char (point-min))
      (or (org-export-push-to-kill-ring
	   (upcase (symbol-name org-lparse-backend)))
	  (message "Exporting... done"))

      (cond
       ((not to-buffer)
	(let ((f (org-lparse-get 'SAVE-METHOD)))
	  (or (and f (functionp f) (funcall f filename opt-plist))
	      (save-buffer)))
	(or (and (boundp 'org-lparse-other-backend)
		 org-lparse-other-backend
		 (not (equal org-lparse-backend org-lparse-other-backend))
		 (org-lparse-do-convert
		  buffer-file-name (symbol-name org-lparse-other-backend)))
	    (current-buffer)))
       ((eq to-buffer 'string)
	(prog1 (buffer-substring (point-min) (point-max))
	  (kill-buffer (current-buffer))))
       (t (current-buffer))))))

(defun org-lparse-format-table (lines olines)
  "Returns backend-specific code for org-type and table-type tables."
  (if (stringp lines)
      (setq lines (org-split-string lines "\n")))
  (if (string-match "^[ \t]*|" (car lines))
      ;; A normal org table
      (org-lparse-format-org-table lines nil)
    ;; Table made by table.el
    (or (org-lparse-format-table-table-using-table-generate-source
	 ;; FIXME: Need to take care of this during merge
	 (if (eq org-lparse-backend 'xhtml) 'html org-lparse-backend)
	 olines
	 (not org-export-prefer-native-exporter-for-tables))
	;; We are here only when table.el table has NO col or row
	;; spanning and the user prefers using org's own converter for
	;; exporting of such simple table.el tables.
	(org-lparse-format-table-table lines))))

(defun org-lparse-table-get-colalign-info (lines)
  (let ((col-cookies (org-find-text-property-in-string
			'org-col-cookies (car lines))))
    (when (and col-cookies org-table-clean-did-remove-column)
      (setq col-cookies
	    (mapcar (lambda (x) (cons (1- (car x)) (cdr x))) col-cookies)))
    col-cookies))

(defvar org-lparse-table-style)
(defvar org-lparse-table-ncols)
(defvar org-lparse-table-rownum)
(defvar org-lparse-table-is-styled)
(defvar org-lparse-table-begin-marker)
(defvar org-lparse-table-num-numeric-items-per-column)
(defvar org-lparse-table-colalign-info)
(defvar org-lparse-table-colalign-vector)

;; Following variables are defined in org-table.el
(defvar org-table-number-fraction)
(defvar org-table-number-regexp)
(defun org-lparse-org-table-to-list-table (lines &optional splice)
  "Convert org-table to list-table.
LINES is a list of the form (ROW1 ROW2 ROW3 ...) where each
element is a `string' representing a single row of org-table.
Thus each ROW has vertical separators \"|\" separating the table
fields.  A ROW could also be a row-group separator of the form
\"|---...|\".  Return a list of the form (ROW1 ROW2 ROW3
...). ROW could either be symbol `:hrule' or a list of the
form (FIELD1 FIELD2 FIELD3 ...) as appropriate."
  (let (line lines-1)
    (cond
     (splice
      (while (setq line (pop lines))
	(unless (string-match "^[ \t]*|-" line)
	  (push (org-split-string line "[ \t]*|[ \t]*") lines-1))))
     (t
      (while (setq line (pop lines))
	(cond
	 ((string-match "^[ \t]*|-" line)
	  (when lines
	    (push :hrule lines-1)))
	 (t
	  (push (org-split-string line "[ \t]*|[ \t]*") lines-1))))))
    (nreverse lines-1)))

(defun org-lparse-insert-org-table (lines &optional splice)
  "Format a org-type table into backend-specific code.
LINES is a list of lines.  Optional argument SPLICE means, do not
insert header and surrounding <table> tags, just format the lines.
Optional argument NO-CSS means use XHTML attributes instead of CSS
for formatting.  This is required for the DocBook exporter."
  (require 'org-table)
  ;; Get rid of hlines at beginning and end
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (if (string-match "^[ \t]*|-" (car lines)) (setq lines (cdr lines)))
  (setq lines (nreverse lines))
  (when org-export-table-remove-special-lines
    ;; Check if the table has a marking column.  If yes remove the
    ;; column and the special lines
    (setq lines (org-table-clean-before-export lines)))
  (let* ((caption (org-find-text-property-in-string 'org-caption (car lines)))
	 (caption (and caption (org-xml-encode-org-text caption)))
	 (label (org-find-text-property-in-string 'org-label (car lines)))
	 (org-lparse-table-colalign-info (org-lparse-table-get-colalign-info lines))
	 (attributes (org-find-text-property-in-string 'org-attributes
						       (car lines)))
	 (head (and org-export-highlight-first-table-line
		    (delq nil (mapcar
			       (lambda (x) (string-match "^[ \t]*|-" x))
			       (cdr lines))))))
    (setq lines (org-lparse-org-table-to-list-table lines splice))
    (org-lparse-insert-list-table
     lines splice caption label attributes head org-lparse-table-colalign-info)))

(defun org-lparse-insert-list-table (lines &optional splice
					      caption label attributes head
					      org-lparse-table-colalign-info)
  (or (featurep 'org-table)		; required for
      (require 'org-table))		; `org-table-number-regexp'
  (let* ((org-lparse-table-rownum -1) org-lparse-table-ncols i (cnt 0)
	 tbopen fields line
	 org-lparse-table-cur-rowgrp-is-hdr
	 org-lparse-table-rowgrp-open
	 org-lparse-table-num-numeric-items-per-column
	 org-lparse-table-colalign-vector n
	 org-lparse-table-rowgrp-info
	 org-lparse-table-begin-marker
	 (org-lparse-table-style 'org-table)
	 org-lparse-table-is-styled)
    (cond
     (splice
      (setq org-lparse-table-is-styled nil)
      (while (setq line (pop lines))
	(insert (org-lparse-format-table-row line) "\n")))
     (t
      (setq org-lparse-table-is-styled t)
      (org-lparse-begin 'TABLE caption label attributes)
      (setq org-lparse-table-begin-marker (point))
      (org-lparse-begin-table-rowgroup head)
      (while (setq line (pop lines))
	(cond
	 ((equal line :hrule)
	  (org-lparse-begin-table-rowgroup))
	 (t
	  (insert (org-lparse-format-table-row line) "\n"))))
      (org-lparse-end 'TABLE-ROWGROUP)
      (org-lparse-end-table)))))

(defun org-lparse-format-org-table (lines &optional splice)
  (with-temp-buffer
    (org-lparse-insert-org-table lines splice)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-lparse-format-list-table (lines &optional splice)
  (with-temp-buffer
    (org-lparse-insert-list-table lines splice)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-lparse-insert-table-table (lines)
  "Format a table generated by table.el into backend-specific code.
This conversion does *not* use `table-generate-source' from table.el.
This has the advantage that Org-mode's HTML conversions can be used.
But it has the disadvantage, that no cell- or row-spanning is allowed."
  (let (line field-buffer
	     (org-lparse-table-cur-rowgrp-is-hdr
	      org-export-highlight-first-table-line)
	     (caption nil)
	     (attributes nil)
	     (label nil)
	     (org-lparse-table-style 'table-table)
	     (org-lparse-table-is-styled nil)
	     fields org-lparse-table-ncols i (org-lparse-table-rownum -1)
	     (empty (org-lparse-format 'SPACES 1)))
    (org-lparse-begin 'TABLE caption label attributes)
    (while (setq line (pop lines))
      (cond
       ((string-match "^[ \t]*\\+-" line)
	(when field-buffer
	  (let ((org-export-table-row-tags '("<tr>" . "</tr>"))
		;; (org-export-html-table-use-header-tags-for-first-column nil)
		)
	    (insert (org-lparse-format-table-row field-buffer empty)))
	  (setq org-lparse-table-cur-rowgrp-is-hdr nil)
	  (setq field-buffer nil)))
       (t
	;; Break the line into fields and store the fields
	(setq fields (org-split-string line "[ \t]*|[ \t]*"))
	(if field-buffer
	    (setq field-buffer (mapcar
				(lambda (x)
				  (concat x (org-lparse-format 'LINE-BREAK)
					  (pop fields)))
				field-buffer))
	  (setq field-buffer fields)))))
    (org-lparse-end-table)))

(defun org-lparse-format-table-table (lines)
  (with-temp-buffer
    (org-lparse-insert-table-table lines)
    (buffer-substring-no-properties (point-min) (point-max))))

(defvar table-source-languages)		; defined in table.el
(defun org-lparse-format-table-table-using-table-generate-source (backend
								 lines
								 &optional
								 spanned-only)
  "Format a table into BACKEND, using `table-generate-source' from table.el.
Use SPANNED-ONLY to suppress exporting of simple table.el tables.

When SPANNED-ONLY is nil, all table.el tables are exported.  When
SPANNED-ONLY is non-nil, only tables with either row or column
spans are exported.

This routine returns the generated source or nil as appropriate.

Refer docstring of `org-export-prefer-native-exporter-for-tables'
for further information."
  (require 'table)
  (with-current-buffer (get-buffer-create " org-tmp1 ")
    (erase-buffer)
    (insert (mapconcat 'identity lines "\n"))
    (goto-char (point-min))
    (if (not (re-search-forward "|[^+]" nil t))
	(error "Error processing table"))
    (table-recognize-table)
    (when (or (not spanned-only)
	      (let* ((dim (table-query-dimension))
		     (c (nth 4 dim)) (r (nth 5 dim)) (cells (nth 6 dim)))
		(not (= (* c r) cells))))
      (with-current-buffer (get-buffer-create " org-tmp2 ") (erase-buffer))
      (cond
       ((member backend table-source-languages)
	(table-generate-source backend " org-tmp2 ")
	(set-buffer " org-tmp2 ")
	(buffer-substring (point-min) (point-max)))
       (t
	;; table.el doesn't support the given backend. Currently this
	;; happens in case of odt export.  Strip the table from the
	;; generated document. A better alternative would be to embed
	;; the table as ascii text in the output document.
	(org-lparse-warn
	 (concat
	  "Found table.el-type table in the source org file. "
	  (format "table.el doesn't support %s backend. "
		  (upcase (symbol-name backend)))
	  "Skipping ahead ..."))
	"")))))

(defun org-lparse-handle-time-stamps (s)
  "Format time stamps in string S, or remove them."
  (catch 'exit
    (let (r b)
      (when org-maybe-keyword-time-regexp
	(while (string-match org-maybe-keyword-time-regexp s)
	  (or b (setq b (substring s 0 (match-beginning 0))))
	  (setq r (concat
		   r (substring s 0 (match-beginning 0)) " "
		   (org-lparse-format
		    'FONTIFY
		    (concat
		     (if (match-end 1)
			 (org-lparse-format
			  'FONTIFY
			  (match-string 1 s) "timestamp-kwd"))
		     " "
		     (org-lparse-format
		      'FONTIFY
		      (substring (org-translate-time (match-string 3 s)) 1 -1)
		      "timestamp"))
		    "timestamp-wrapper"))
		s (substring s (match-end 0)))))

      ;; Line break if line started and ended with time stamp stuff
      (if (not r)
	  s
	(setq r (concat r s))
	(unless (string-match "\\S-" (concat b s))
	  (setq r (concat r (org-lparse-format 'LINE-BREAK))))
	r))))

(defun org-xml-encode-plain-text (s)
  "Convert plain text characters to HTML equivalent.
Possible conversions are set in `org-export-html-protect-char-alist'."
  (let ((cl (org-lparse-get 'PLAIN-TEXT-MAP)) c)
    (while (setq c (pop cl))
      (let ((start 0))
	(while (string-match (car c) s start)
	  (setq s (replace-match (cdr c) t t s)
		start (1+ (match-beginning 0))))))
    s))

(defun org-xml-encode-org-text-skip-links (string)
  "Prepare STRING for HTML export.  Apply all active conversions.
If there are links in the string, don't modify these.  If STRING
is nil, return nil."
  (when string
    (let* ((re (concat org-bracket-link-regexp "\\|"
		       (org-re "[ \t]+\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$")))
	   m s l res)
      (while (setq m (string-match re string))
	(setq s (substring string 0 m)
	      l (match-string 0 string)
	      string (substring string (match-end 0)))
	(push (org-xml-encode-org-text s) res)
	(push l res))
      (push (org-xml-encode-org-text string) res)
      (apply 'concat (nreverse res)))))

(defun org-xml-encode-org-text (s)
  "Apply all active conversions to translate special ASCII to HTML."
  (setq s (org-xml-encode-plain-text s))
  (if org-export-html-expand
      (while (string-match "@&lt;\\([^&]*\\)&gt;" s)
	(setq s (replace-match "<\\1>" t nil s))))
  (if org-export-with-emphasize
      (setq s (org-lparse-apply-char-styles s)))
  (if org-export-with-special-strings
      (setq s (org-lparse-convert-special-strings s)))
  (if org-export-with-sub-superscripts
      (setq s (org-lparse-apply-sub-superscript-styles s)))
  (if org-export-with-TeX-macros
      (let ((start 0) wd rep)
	(while (setq start (string-match "\\\\\\([a-zA-Z]+[0-9]*\\)\\({}\\)?"
					 s start))
	  (if (get-text-property (match-beginning 0) 'org-protected s)
	      (setq start (match-end 0))
	    (setq wd (match-string 1 s))
	    (if (setq rep (org-lparse-format 'ORG-ENTITY wd))
		(setq s (replace-match rep t t s))
	      (setq start (+ start (length wd))))))))
  s)

(defun org-lparse-convert-special-strings (string)
  "Convert special characters in STRING to HTML."
  (let ((all (org-lparse-get 'SPECIAL-STRING-REGEXPS))
	e a re rpl start)
    (while (setq a (pop all))
      (setq re (car a) rpl (cdr a) start 0)
      (while (string-match re string start)
	(if (get-text-property (match-beginning 0) 'org-protected string)
	    (setq start (match-end 0))
	  (setq string (replace-match rpl t nil string)))))
    string))

(defun org-lparse-apply-sub-superscript-styles (string)
  "Apply subscript and superscript styles to STRING.
Use `org-export-with-sub-superscripts' to control application of
sub and superscript styles."
  (let (key c (s 0) (requireb (eq org-export-with-sub-superscripts '{})))
    (while (string-match org-match-substring-regexp string s)
      (cond
       ((and requireb (match-end 8)) (setq s (match-end 2)))
       ((get-text-property  (match-beginning 2) 'org-protected string)
	(setq s (match-end 2)))
       (t
	(setq s (match-end 1)
	      key (if (string= (match-string 2 string) "_")
		      'subscript 'superscript)
	      c (or (match-string 8 string)
		    (match-string 6 string)
		    (match-string 5 string))
	      string (replace-match
		      (concat (match-string 1 string)
			      (org-lparse-format 'FONTIFY c key))
		      t t string)))))
    (while (string-match "\\\\\\([_^]\\)" string)
      (setq string (replace-match (match-string 1 string) t t string)))
    string))

(defvar org-lparse-char-styles
  `(("*" bold)
    ("/" emphasis)
    ("_" underline)
    ("=" code)
    ("~" verbatim)
    ("+" strike))
  "Map Org emphasis markers to char styles.
This is an alist where each element is of the
form (ORG-EMPHASIS-CHAR . CHAR-STYLE).")

(defun org-lparse-apply-char-styles (string)
  "Apply char styles to STRING.
The variable `org-lparse-char-styles' controls how the Org
emphasis markers are interpreted."
  (let ((s 0) rpl)
    (while (string-match org-emph-re string s)
      (if (not (equal
		(substring string (match-beginning 3) (1+ (match-beginning 3)))
		(substring string (match-beginning 4) (1+ (match-beginning 4)))))
	  (setq s (match-beginning 0)
		rpl
		(concat
		 (match-string 1 string)
		 (org-lparse-format
		  'FONTIFY (match-string 4 string)
		  (nth 1 (assoc (match-string 3 string)
				org-lparse-char-styles)))
		 (match-string 5 string))
		string (replace-match rpl t t string)
		s (+ s (- (length rpl) 2)))
	(setq s (1+ s))))
    string))

(defun org-lparse-export-list-line (line pos struct prevs)
  "Insert list syntax in export buffer.  Return LINE, maybe modified.

POS is the item position or line position the line had before
modifications to buffer.  STRUCT is the list structure.  PREVS is
the alist of previous items."
  (let* ((get-type
	  (function
	   ;; Translate type of list containing POS to "d", "o" or
	   ;; "u".
	   (lambda (pos struct prevs)
	     (let ((type (org-list-get-list-type pos struct prevs)))
	       (cond
		((eq 'ordered type) "o")
		((eq 'descriptive type) "d")
		(t "u"))))))
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
	      (org-lparse-end-paragraph)
	      ;; Ending for every item
	      (org-lparse-end-list-item-1 type)
	      ;; We're ending last item of the list: end list.
	      (when lastp
		(org-lparse-end-list type)
		(org-lparse-begin-paragraph))))
	  (funcall get-closings pos))
    (cond
     ;; At an item: insert appropriate tags in export buffer.
     ((assq pos struct)
      (string-match
       (concat "[ \t]*\\(\\S-+[ \t]*\\)"
	       "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ \t]*\\)?"
	       "\\(?:\\(\\[[ X-]\\]\\)[ \t]+\\)?"
	       "\\(?:\\(.*\\)[ \t]+::\\(?:[ \t]+\\|$\\)\\)?"
	       "\\(.*\\)") line)
      (let* ((checkbox (match-string 3 line))
	     (desc-tag (or (match-string 4 line) "???"))
	     (body (or (match-string 5 line) ""))
	     (list-beg (org-list-get-list-begin pos struct prevs))
	     (firstp (= list-beg pos))
	     ;; Always refer to first item to determine list type, in
	     ;; case list is ill-formed.
	     (type (funcall get-type list-beg struct prevs))
	     (counter (let ((count-tmp (org-list-get-counter pos struct)))
			(cond
			 ((not count-tmp) nil)
			 ((string-match "[A-Za-z]" count-tmp)
			  (- (string-to-char (upcase count-tmp)) 64))
			 ((string-match "[0-9]+" count-tmp)
			  count-tmp)))))
	(when firstp
	  (org-lparse-end-paragraph)
	  (org-lparse-begin-list type))

	(let ((arg (cond ((equal type "d") desc-tag)
			 ((equal type "o") counter))))
	  (org-lparse-begin-list-item type arg))

	;; If line had a checkbox, some additional modification is required.
	(when checkbox
	  (setq body
		(concat
		 (org-lparse-format
		  'FONTIFY (concat
			    "["
			    (cond
			     ((string-match "X" checkbox) "X")
			     ((string-match " " checkbox)
			      (org-lparse-format 'SPACES 1))
			     (t "-"))
			    "]")
		  'code)
		 " "
		 body)))
	;; Return modified line
	body))
     ;; At a list ender: go to next line (side-effects only).
     ((equal "ORG-LIST-END-MARKER" line) (throw 'nextline nil))
     ;; Not at an item: return line unchanged (side-effects only).
     (t line))))

(defun org-lparse-bind-local-variables (opt-plist)
  (mapc (lambda (x)
	  (set (make-local-variable (nth 2 x))
	       (plist-get opt-plist (car x))))
	org-export-plist-vars))

(defvar org-lparse-table-rowgrp-open)
(defvar org-lparse-table-cur-rowgrp-is-hdr)
(defvar org-lparse-footnote-number)
(defvar org-lparse-footnote-definitions)
(defvar org-lparse-output-buffer nil
  "Buffer to which `org-do-lparse' writes to.
This buffer contains the contents of the to-be-created exported
document.")

(defcustom org-lparse-debug nil
  "Enable or Disable logging of `org-lparse' callbacks.
The parameters passed to the backend-registered ENTITY-CONTROL
and ENTITY-FORMAT callbacks are logged as comment strings in the
exported buffer.  (org-lparse-format 'COMMENT fmt args) is used
for logging.  Customize this variable only if you are an expert
user.  Valid values of this variable are:
nil     : Disable logging
control : Log all invocations of `org-lparse-begin' and
          `org-lparse-end' callbacks.
format  : Log invocations of `org-lparse-format' callbacks.
t       : Log all invocations of `org-lparse-begin', `org-lparse-end'
          and `org-lparse-format' callbacks,"
  :group 'org-lparse
  :type '(choice
	  (const :tag "Disable" nil)
	  (const :tag "Format callbacks" format)
	  (const :tag "Control callbacks" control)
	  (const :tag "Format and Control callbacks" t)))

(defun org-lparse-begin (entity &rest args)
  "Begin ENTITY in current buffer.  ARGS is entity specific.
ENTITY can be one of PARAGRAPH, LIST, LIST-ITEM etc.

Use (org-lparse-begin 'LIST \"o\") to begin a list in current
buffer.

See `org-xhtml-entity-control-callbacks-alist' for more
information."
  (when (and (member org-lparse-debug '(t control))
	     (not (eq entity 'DOCUMENT-CONTENT)))
    (insert (org-lparse-format 'COMMENT "%s BEGIN %S" entity args)))

  (let ((f (cadr (assoc entity org-lparse-entity-control-callbacks-alist))))
    (unless f (error "Unknown entity: %s" entity))
    (apply f args)))

(defun org-lparse-end (entity &rest args)
  "Close ENTITY in current buffer.  ARGS is entity specific.
ENTITY can be one of PARAGRAPH, LIST, LIST-ITEM
etc.

Use (org-lparse-end 'LIST \"o\") to close a list in current
buffer.

See `org-xhtml-entity-control-callbacks-alist' for more
information."
  (when (and (member org-lparse-debug '(t control))
	     (not (eq entity 'DOCUMENT-CONTENT)))
    (insert (org-lparse-format 'COMMENT "%s END %S" entity args)))

  (let ((f (caddr (assoc entity org-lparse-entity-control-callbacks-alist))))
    (unless f (error "Unknown entity: %s" entity))
    (apply f args)))

(defun org-lparse-begin-paragraph (&optional style)
  "Insert <p>, but first close previous paragraph if any."
  (org-lparse-end-paragraph)
  (org-lparse-begin 'PARAGRAPH style)
  (setq org-lparse-par-open t))

(defun org-lparse-end-paragraph ()
  "Close paragraph if there is one open."
  (when org-lparse-par-open
    (org-lparse-end 'PARAGRAPH)
    (setq org-lparse-par-open nil)))

(defun org-lparse-end-list-item-1 (&optional type)
  "Close <li> if necessary."
  (org-lparse-end-paragraph)
  (org-lparse-end-list-item (or type "u")))

(defun org-lparse-preprocess-after-blockquote-hook ()
  "Treat `org-lparse-special-blocks' specially."
  (goto-char (point-min))
  (while (re-search-forward
	  "^[ \t]*#\\+\\(begin\\|end\\)_\\(\\S-+\\)[ \t]*\\(.*\\)$" nil t)
    (when (member (downcase (match-string 2)) org-lparse-special-blocks)
      (replace-match
       (if (equal (downcase (match-string 1)) "begin")
	   (format "ORG-%s-START %s" (upcase (match-string 2))
		   (match-string 3))
	 (format "ORG-%s-END %s" (upcase (match-string 2))
		 (match-string 3))) t t))))

(add-hook 'org-export-preprocess-after-blockquote-hook
	  'org-lparse-preprocess-after-blockquote-hook)

(defun org-lparse-strip-experimental-blocks-maybe-hook ()
  "Strip \"list-table\" and \"annotation\" blocks.
Stripping happens only when the exported backend is not one of
\"odt\" or \"xhtml\"."
  (when (not org-lparse-backend)
    (message "Stripping following blocks - %S" org-lparse-special-blocks)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while
	  (re-search-forward
	   "^[ \t]*#\\+begin_\\(\\S-+\\)\\([ \t]+.*\\)?\n\\([^\000]*?\\)\n[ \t]*#\\+end_\\1\\>.*"
	   nil t)
	(when (member (match-string 1) org-lparse-special-blocks)
	  (replace-match "" t t))))))

(add-hook 'org-export-preprocess-hook
	  'org-lparse-strip-experimental-blocks-maybe-hook)

(defvar org-lparse-list-table-p nil
  "Non-nil if `org-do-lparse' is within a list-table.")

(defvar org-lparse-dyn-current-environment nil)
(defun org-lparse-begin-environment (style &optional env-options-plist)
  (case style
    (list-table
     (setq org-lparse-list-table-p t))
    (t (setq org-lparse-dyn-current-environment style)
       (org-lparse-begin 'ENVIRONMENT  style env-options-plist))))

(defun org-lparse-end-environment (style &optional env-options-plist)
  (case style
    (list-table
     (setq org-lparse-list-table-p nil))
    (t (org-lparse-end 'ENVIRONMENT style env-options-plist)
       (setq org-lparse-dyn-current-environment nil))))

(defun org-lparse-current-environment-p (style)
  (eq org-lparse-dyn-current-environment style))

(defun org-lparse-begin-footnote-definition (n)
  (org-lparse-begin-collect)
  (setq org-lparse-insert-tag-with-newlines nil)
  (org-lparse-begin 'FOOTNOTE-DEFINITION n))

(defun org-lparse-end-footnote-definition (n)
  (org-lparse-end 'FOOTNOTE-DEFINITION n)
  (setq org-lparse-insert-tag-with-newlines 'both)
  (let ((footnote-def (org-lparse-end-collect)))
    ;; Cleanup newlines in footnote definition.  This ensures that a
    ;; transcoded line is never (wrongly) broken in to multiple lines.
    (let ((pos 0))
      (while (string-match "[\r\n]+" footnote-def pos)
	(setq pos (1+ (match-beginning 0)))
	(setq footnote-def (replace-match " " t t footnote-def))))
    (push (cons n footnote-def) org-lparse-footnote-definitions)))

(defvar org-lparse-collect-buffer nil
  "An auxiliary buffer named \"*Org Lparse Collect*\".
`org-do-lparse' uses this as output buffer while collecting
footnote definitions and table-cell contents of list-tables.  See
`org-lparse-begin-collect' and `org-lparse-end-collect'.")

(defvar org-lparse-collect-count nil
  "Count number of calls to `org-lparse-begin-collect'.
Use this counter to catch chained collections if they ever
happen.")

(defun org-lparse-begin-collect ()
  "Temporarily switch to `org-lparse-collect-buffer'.
Also erase it's contents."
  (unless (zerop org-lparse-collect-count)
    (error "FIXME (org-lparse.el): Encountered chained collections"))
  (incf org-lparse-collect-count)
  (unless org-lparse-collect-buffer
    (setq org-lparse-collect-buffer
	  (get-buffer-create "*Org Lparse Collect*")))
  (set-buffer org-lparse-collect-buffer)
  (erase-buffer))

(defun org-lparse-end-collect ()
  "Switch to `org-lparse-output-buffer'.
Return contents of `org-lparse-collect-buffer' as a `string'."
  (assert (> org-lparse-collect-count 0))
  (decf org-lparse-collect-count)
  (prog1 (buffer-string)
    (erase-buffer)
    (set-buffer org-lparse-output-buffer)))

(defun org-lparse-format (entity &rest args)
  "Format ENTITY in backend-specific way and return it.
ARGS is specific to entity being formatted.

Use (org-lparse-format 'HEADING \"text\" 1) to format text as
level 1 heading.

See `org-xhtml-entity-format-callbacks-alist' for more information."
  (when (and (member org-lparse-debug '(t format))
	     (not (equal entity 'COMMENT)))
    (insert (org-lparse-format 'COMMENT "%s: %S" entity args)))
  (cond
   ((consp entity)
    (let ((text (pop args)))
      (apply 'org-lparse-format 'TAGS entity text args)))
   (t
    (let ((f (cdr (assoc entity org-lparse-entity-format-callbacks-alist))))
      (unless f (error "Unknown entity: %s" entity))
      (apply f args)))))

(defun org-lparse-insert (entity &rest args)
  (insert (apply 'org-lparse-format entity args)))

(defun org-lparse-prepare-toc (lines level-offset opt-plist umax-toc)
  (let* ((quote-re0 (concat "^[ \t]*" org-quote-string "\\>"))
	 (org-min-level (org-get-min-level lines level-offset))
	 (org-last-level org-min-level)
	 level)
    (with-temp-buffer
      (org-lparse-bind-local-variables opt-plist)
      (erase-buffer)
      (org-lparse-begin 'TOC (nth 3 (plist-get opt-plist :lang-words)) umax-toc)
      (setq
       lines
       (mapcar
	#'(lambda (line)
	    (when (and (string-match org-todo-line-regexp line)
		       (not (get-text-property 0 'org-protected line))
		       (<= (setq level (org-tr-level
					(- (match-end 1) (match-beginning 1)
					   level-offset)))
			   umax-toc))
	      (let ((txt (save-match-data
			   (org-xml-encode-org-text-skip-links
			    (org-export-cleanup-toc-line
			     (match-string 3 line)))))
		    (todo (and
			   org-export-mark-todo-in-toc
			   (or (and (match-beginning 2)
				    (not (member (match-string 2 line)
						 org-done-keywords)))
			       (and (= level umax-toc)
				    (org-search-todo-below
				     line lines level)))))
		    tags)
		;; Check for targets
		(while (string-match org-any-target-regexp line)
		  (setq line
			(replace-match
			 (let ((org-lparse-encode-pending t))
			   (org-lparse-format 'FONTIFY
					     (match-string 1 line) "target"))
			 t t line)))
		(when (string-match
		       (org-re "[ \t]+:\\([[:alnum:]_@:]+\\):[ \t]*$") txt)
		  (setq tags (match-string 1 txt)
			txt (replace-match "" t nil txt)))
		(when (string-match quote-re0 txt)
		  (setq txt (replace-match "" t t txt)))
		(while (string-match "&lt;\\(&lt;\\)+\\|&gt;\\(&gt;\\)+" txt)
		  (setq txt (replace-match "" t t txt)))
		(org-lparse-format
		 'TOC-ITEM
		 (let* ((snumber (org-section-number level))
			(href (replace-regexp-in-string
			       "\\." "-" (format "sec-%s" snumber)))
			(href
			 (or
			  (cdr (assoc
				href org-export-preferred-target-alist))
			  href))
			(href (org-solidify-link-text href)))
		   (org-lparse-format 'TOC-ENTRY snumber todo txt tags href))
		 level org-last-level)
		(setq org-last-level level)))
	    line)
	lines))
      (org-lparse-end 'TOC)
      (setq org-lparse-toc (buffer-string))))
  lines)

(defun org-lparse-format-table-row (fields &optional text-for-empty-fields)
  (if org-lparse-table-ncols
      ;; second and subsequent rows of the table
      (when (and org-lparse-list-table-p
		 (> (length fields) org-lparse-table-ncols))
	(error "Table row has %d columns but header row claims %d columns"
	       (length fields) org-lparse-table-ncols))
    ;; first row of the table
    (setq org-lparse-table-ncols (length fields))
    (when org-lparse-table-is-styled
      (setq org-lparse-table-num-numeric-items-per-column
	    (make-vector org-lparse-table-ncols 0))
      (setq org-lparse-table-colalign-vector
	    (make-vector org-lparse-table-ncols nil))
      (let ((c -1))
	(while  (< (incf c) org-lparse-table-ncols)
	  (let* ((col-cookie (cdr (assoc (1+ c) org-lparse-table-colalign-info)))
		 (align (nth 0 col-cookie)))
	    (setf (aref org-lparse-table-colalign-vector c)
		  (cond
		   ((string= align "l") "left")
		   ((string= align "r") "right")
		   ((string= align "c") "center")
		   (t nil))))))))
  (incf org-lparse-table-rownum)
  (let ((i -1))
    (org-lparse-format
     'TABLE-ROW
     (mapconcat
      (lambda (x)
	(when (and (string= x "") text-for-empty-fields)
	  (setq x text-for-empty-fields))
	(incf i)
	(let (col-cookie horiz-span)
	  (when org-lparse-table-is-styled
	    (when (and (< i org-lparse-table-ncols)
		       (string-match org-table-number-regexp x))
	      (incf (aref org-lparse-table-num-numeric-items-per-column i)))
	    (setq col-cookie (cdr (assoc (1+ i) org-lparse-table-colalign-info))
		  horiz-span (nth 1 col-cookie)))
	  (org-lparse-format
	   'TABLE-CELL x org-lparse-table-rownum i (or horiz-span 0))))
      fields "\n"))))

(defun org-lparse-get (what &optional opt-plist)
  "Query for value of WHAT for the current backend `org-lparse-backend'.
See also `org-lparse-backend-get'."
  (if (boundp 'org-lparse-backend)
      (org-lparse-backend-get (symbol-name org-lparse-backend) what opt-plist)
    (error "org-lparse-backend is not bound yet")))

(defun org-lparse-backend-get (backend what &optional opt-plist)
  "Query BACKEND for value of WHAT.
Dispatch the call to `org-<backend>-user-get'.  If that throws an
error, dispatch the call to `org-<backend>-get'.  See
`org-xhtml-get' for all known settings queried for by
`org-lparse' during the course of export."
  (assert (stringp backend) t)
  (unless (org-lparse-backend-is-native-p backend)
    (error "Unknown native backend %s" backend))
  (let ((backend-get-method (intern (format "org-%s-get" backend)))
	(backend-user-get-method (intern (format "org-%s-user-get" backend))))
    (cond
     ((functionp backend-get-method)
      (condition-case nil
	  (funcall backend-user-get-method what opt-plist)
	(error (funcall backend-get-method what opt-plist))))
     (t
      (error "Native backend %s doesn't define %s" backend backend-get-method)))))

(defun org-lparse-insert-tag (tag &rest args)
  (when (member org-lparse-insert-tag-with-newlines '(lead both))
    (insert  "\n"))
  (insert (apply 'format tag args))
  (when (member org-lparse-insert-tag-with-newlines '(trail both))
    (insert  "\n")))

(defun org-lparse-get-targets-from-title (title)
  (let* ((target (org-get-text-property-any 0 'target title))
	 (extra-targets (assoc target org-export-target-aliases))
	 (target (or (cdr (assoc target org-export-preferred-target-alist))
		     target)))
    (cons target (remove target extra-targets))))

(defun org-lparse-suffix-from-snumber (snumber)
  (let* ((snu (replace-regexp-in-string "\\." "-" snumber))
	 (href (cdr (assoc (concat "sec-" snu)
			   org-export-preferred-target-alist))))
    (org-solidify-link-text (or href snu))))

(defun org-lparse-begin-level (level title umax head-count)
  "Insert a new LEVEL in HTML export.
When TITLE is nil, just close all open levels."
  (org-lparse-end-level level umax)
  (unless title (error "Why is heading nil"))
  (let* ((targets (org-lparse-get-targets-from-title title))
	 (target (car targets)) (extra-targets (cdr targets))
	 (target (and target (org-solidify-link-text target)))
	 (extra-class (org-get-text-property-any 0 'html-container-class title))
	 snumber tags level1 class)
    (when (string-match (org-re "\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$") title)
      (setq tags (and org-export-with-tags (match-string 1 title)))
      (setq title (replace-match "" t t title)))
    (if (> level umax)
	(progn
	  (if (aref org-levels-open (1- level))
	      (org-lparse-end-list-item-1)
	    (aset org-levels-open (1- level) t)
	    (org-lparse-end-paragraph)
	    (org-lparse-begin-list 'unordered))
	  (org-lparse-begin-list-item
	   'unordered target (org-lparse-format
			      'HEADLINE title extra-targets tags)))
      (aset org-levels-open (1- level) t)
      (setq snumber (org-section-number level))
      (setq level1 (+ level (or (org-lparse-get 'TOPLEVEL-HLEVEL) 1) -1))
      (unless (= head-count 1)
	(org-lparse-end-outline-text-or-outline))
      (org-lparse-begin-outline-and-outline-text
       level1 snumber title tags target extra-targets extra-class)
      (org-lparse-begin-paragraph))))

(defun org-lparse-end-level (level umax)
  (org-lparse-end-paragraph)
  (loop for l from org-level-max downto level
	do (when (aref org-levels-open (1- l))
	     ;; Terminate one level in HTML export
	     (if (<= l umax)
		 (org-lparse-end-outline-text-or-outline)
	       (org-lparse-end-list-item-1)
	       (org-lparse-end-list 'unordered))
	     (aset org-levels-open (1- l) nil))))

(defvar org-lparse-outline-text-open)
(defun org-lparse-begin-outline-and-outline-text (level1 snumber title tags
							target extra-targets
							extra-class)
  (org-lparse-begin
   'OUTLINE level1 snumber title tags target extra-targets extra-class)
  (org-lparse-begin-outline-text level1 snumber extra-class))

(defun org-lparse-end-outline-text-or-outline ()
  (cond
   (org-lparse-outline-text-open
    (org-lparse-end 'OUTLINE-TEXT)
    (setq org-lparse-outline-text-open nil))
   (t (org-lparse-end 'OUTLINE))))

(defun org-lparse-begin-outline-text (level1 snumber extra-class)
  (assert (not org-lparse-outline-text-open) t)
  (setq org-lparse-outline-text-open t)
  (org-lparse-begin 'OUTLINE-TEXT level1 snumber extra-class))

(defun org-lparse-html-list-type-to-canonical-list-type (ltype)
  (cdr (assoc ltype '(("o" . ordered)
		      ("u" . unordered)
		      ("d" . description)))))

;; following vars are bound during `org-do-lparse'
(defvar org-lparse-list-stack)
(defvar org-lparse-list-table:table-row)
(defvar org-lparse-list-table:lines)

;; Notes on LIST-TABLES
;; ====================
;; Lists withing "list-table" blocks (as shown below)
;;
;; #+begin_list-table
;; - Row 1
;;   - 1.1
;;   - 1.2
;;   - 1.3
;; - Row 2
;;   - 2.1
;;   - 2.2
;;   - 2.3
;; #+end_list-table
;;
;; will be exported as though it were a table as shown below.
;;
;; | Row 1 | 1.1 | 1.2 | 1.3 |
;; | Row 2 | 2.1 | 2.2 | 2.3 |
;;
;; Note that org-tables are NOT multi-line and each line is mapped to
;; a unique row in the exported document.  So if an exported table
;; needs to contain a single paragraph (with copious text) it needs to
;; be typed up in a single line. Editing such long lines using the
;; table editor will be a cumbersome task.  Furthermore inclusion of
;; multi-paragraph text in a table cell is well-nigh impossible.
;;
;; LIST-TABLEs are meant to circumvent the above problems with
;; org-tables.
;;
;; Note that in the example above the list items could be paragraphs
;; themselves and the list can be arbitrarily deep.
;;
;; Inspired by following thread:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-03/msg01101.html

(defun org-lparse-begin-list (ltype)
  (push ltype org-lparse-list-stack)
  (let ((list-level (length org-lparse-list-stack)))
    (cond
     ((not org-lparse-list-table-p)
      (org-lparse-begin 'LIST ltype))
     ;; process LIST-TABLE
     ((= 1 list-level)
      ;; begin LIST-TABLE
      (setq org-lparse-list-table:lines nil)
      (setq org-lparse-list-table:table-row nil))
     ((= 2 list-level)
      (ignore))
     (t
      (org-lparse-begin 'LIST ltype)))))

(defun org-lparse-end-list (ltype)
  (pop org-lparse-list-stack)
  (let ((list-level (length org-lparse-list-stack)))
    (cond
     ((not org-lparse-list-table-p)
      (org-lparse-end 'LIST ltype))
     ;; process LIST-TABLE
     ((= 0 list-level)
      ;; end LIST-TABLE
      (insert (org-lparse-format-list-table
	       (nreverse org-lparse-list-table:lines))))
     ((= 1 list-level)
      (ignore))
     (t
      (org-lparse-end 'LIST ltype)))))

(defun org-lparse-begin-list-item (ltype &optional arg headline)
  (let ((list-level (length org-lparse-list-stack)))
    (cond
     ((not org-lparse-list-table-p)
      (org-lparse-begin 'LIST-ITEM ltype arg headline))
     ;; process LIST-TABLE
     ((= 1 list-level)
      ;; begin TABLE-ROW for LIST-TABLE
      (setq org-lparse-list-table:table-row nil)
      (org-lparse-begin-list-table:table-cell))
     ((= 2 list-level)
      ;; begin TABLE-CELL for LIST-TABLE
      (org-lparse-begin-list-table:table-cell))
     (t
      (org-lparse-begin 'LIST-ITEM ltype arg headline)))))

(defun org-lparse-end-list-item (ltype)
  (let ((list-level (length org-lparse-list-stack)))
    (cond
     ((not org-lparse-list-table-p)
      (org-lparse-end 'LIST-ITEM ltype))
     ;; process LIST-TABLE
     ((= 1 list-level)
      ;; end TABLE-ROW for LIST-TABLE
      (org-lparse-end-list-table:table-cell)
      (push (nreverse org-lparse-list-table:table-row)
	    org-lparse-list-table:lines))
     ((= 2 list-level)
      ;; end TABLE-CELL for LIST-TABLE
      (org-lparse-end-list-table:table-cell))
     (t
      (org-lparse-end 'LIST-ITEM ltype)))))

(defvar org-lparse-list-table:table-cell-open)
(defun org-lparse-begin-list-table:table-cell ()
  (org-lparse-end-list-table:table-cell)
  (setq org-lparse-list-table:table-cell-open t)
  (org-lparse-begin-collect)
  (org-lparse-begin-paragraph))

(defun org-lparse-end-list-table:table-cell ()
  (when org-lparse-list-table:table-cell-open
    (setq org-lparse-list-table:table-cell-open nil)
    (org-lparse-end-paragraph)
    (push (org-lparse-end-collect)
	  org-lparse-list-table:table-row)))

(defvar org-lparse-table-rowgrp-info)
(defun org-lparse-begin-table-rowgroup (&optional is-header-row)
  (push (cons (1+ org-lparse-table-rownum) :start) org-lparse-table-rowgrp-info)
  (org-lparse-begin 'TABLE-ROWGROUP is-header-row))

(defun org-lparse-end-table ()
  (when org-lparse-table-is-styled
    ;; column groups
    (unless (car org-table-colgroup-info)
      (setq org-table-colgroup-info
	    (cons :start (cdr org-table-colgroup-info))))

    ;; column alignment
    (let ((c -1))
      (mapc
       (lambda (x)
	 (incf c)
	 (setf (aref org-lparse-table-colalign-vector c)
	       (or (aref org-lparse-table-colalign-vector c)
		   (if (> (/ (float x) (1+ org-lparse-table-rownum))
			  org-table-number-fraction)
		       "right" "left"))))
       org-lparse-table-num-numeric-items-per-column)))
  (org-lparse-end 'TABLE))

(defvar org-lparse-encode-pending nil)

(defun org-lparse-format-tags (tag text prefix suffix &rest args)
  (cond
   ((consp tag)
    (concat prefix (apply 'format (car tag) args) text suffix
	    (format (cdr tag))))
   ((stringp tag)			; singleton tag
    (concat prefix (apply 'format tag args) text))))

(defun org-xml-fix-class-name (kwd) 	; audit callers of this function
  "Turn todo keyword into a valid class name.
Replaces invalid characters with \"_\"."
  (save-match-data
    (while (string-match "[^a-zA-Z0-9_]" kwd)
      (setq kwd (replace-match "_" t t kwd))))
  kwd)

(defun org-lparse-format-todo (todo)
  (org-lparse-format 'FONTIFY
		     (concat
		      (ignore-errors (org-lparse-get 'TODO-KWD-CLASS-PREFIX))
		      (org-xml-fix-class-name todo))
		     (list (if (member todo org-done-keywords) "done" "todo")
			   todo)))

(defun org-lparse-format-extra-targets (extra-targets)
  (if (not extra-targets) ""
      (mapconcat (lambda (x)
	       (setq x (org-solidify-link-text
			(if (org-uuidgen-p x) (concat "ID-" x) x)))
	       (org-lparse-format 'ANCHOR "" x))
	     extra-targets "")))

(defun org-lparse-format-org-tags (tags)
  (if (not tags) ""
    (org-lparse-format
     'FONTIFY (mapconcat
	       (lambda (x)
		 (org-lparse-format
		  'FONTIFY x
		  (concat
		   (ignore-errors (org-lparse-get 'TAG-CLASS-PREFIX))
		   (org-xml-fix-class-name x))))
	       (org-split-string tags ":")
	       (org-lparse-format 'SPACES 1)) "tag")))

(defun org-lparse-format-section-number (&optional snumber level)
  (and org-export-with-section-numbers
       (not org-lparse-body-only) snumber level
       (org-lparse-format 'FONTIFY snumber (format "section-number-%d" level))))

(defun org-lparse-warn (msg)
  (if (not org-lparse-use-flashy-warning)
      (message msg)
    (put-text-property 0 (length msg) 'face 'font-lock-warning-face msg)
    (message msg)
    (sleep-for 3)))

(defun org-xml-format-href (s)
  "Make sure the S is valid as a href reference in an XHTML document."
  (save-match-data
    (let ((start 0))
      (while (string-match "&" s start)
	(setq start (+ (match-beginning 0) 3)
	      s (replace-match "&amp;" t t s)))))
  s)

(defun org-xml-format-desc (s)
  "Make sure the S is valid as a description in a link."
  (if (and s (not (get-text-property 1 'org-protected s)))
      (save-match-data
	(org-xml-encode-org-text s))
    s))

(provide 'org-lparse)

;;; org-lparse.el ends here
