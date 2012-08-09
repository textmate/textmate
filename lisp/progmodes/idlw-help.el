;;; idlw-help.el --- HTML Help code for IDLWAVE

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.
;;
;; Authors: J.D. Smith <jdsmith@as.arizona.edu>
;;          Carsten Dominik <dominik@science.uva.nl>
;; Maintainer: J.D. Smith <jdsmith@as.arizona.edu>
;; Version: 6.1.22
;; Package: idlwave

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

;; The help link information for IDLWAVE's online help feature for
;; system routines is extracted automatically from the IDL
;; documentation, and is available, along with general routine
;; information, in the file idlw-rinfo.el.  The HTML help file
;; themselves are not distributable with Emacs, but are available,
;; along with new versions of IDLWAVE, documentation, and more
;; information, at:
;;
;;           http://idlwave.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; Code:
(defvar idlwave-help-browse-url-available t
  "Whether browse-url is available.")

(require 'browse-url)

(defgroup idlwave-online-help nil
  "Online Help options for IDLWAVE mode."
  :group 'idlwave)

(defcustom idlwave-html-help-pre-v6 nil
  "Whether pre or post-v6.0 IDL help documents are being used."
  :group 'idlwave-online-help
  :type 'boolean)

(defvar idlwave-html-link-sep
  (if idlwave-html-help-pre-v6 "#" "#wp"))

(defcustom idlwave-html-system-help-location   "help/online_help/"
  "The directory, relative to `idlwave-system-directory', where the IDL
HTML help files live, for IDL 6.2 and later.  This location, if found,
is used in preference to the old `idlwave-html-help-location'."
  :group 'idlwave-online-help
  :type 'directory)

(defcustom idlwave-html-help-location
   (if (memq system-type '(ms-dos windows-nt))
      nil
    "/usr/local/etc/")
  "The directory where the idl_html_help/ dir lives.  Obsolete for IDL
6.2 or later (see `idlwave-html-system-help-location')."
  :group 'idlwave-online-help
  :type 'directory)

(defvar idlwave-help-use-hh nil
  "Obsolete variable.")

(defcustom idlwave-help-use-assistant t
  "Whether to use the IDL Assistant as the help browser."
  :group 'idlwave-online-help
  :type 'boolean)

(defcustom idlwave-help-browser-function browse-url-browser-function
  "Function to use to display HTML help.
Defaults to `browse-url-browser-function', which see."
  :group 'idlwave-online-help
  :type 'function)

(defcustom idlwave-help-browser-generic-program browse-url-generic-program
  "Program to run if using `browse-url-generic-program'."
  :group 'idlwave-online-help
  :type 'string)

(defvar browse-url-generic-args)

(defcustom idlwave-help-browser-generic-args
  (if (boundp 'browse-url-generic-args)
      browse-url-generic-args "")
  "Program args to use if using `browse-url-generic-program'."
  :group 'idlwave-online-help
  :type 'string)

(defcustom idlwave-help-browser-is-local nil
  "Whether the browser will display locally in an Emacs window.
Several browsers run and/or display inside Emacs windows, but most are
external programs.  If the browser name contains \"-w3\", it is
assumed to be local to Emacs.  For other local browsers, this variable
must be explicitly set non-nil in order for the variable
`idlwave-help-use-dedicated-frame' to function."
  :group 'idlwave-online-help
  :type 'boolean)

(defvar idlwave-help-directory ""
  "Obsolete variable.  See `idlwave-html-help-location'.")

(defcustom idlwave-help-use-dedicated-frame t
  "*Non-nil means, use a separate frame for Online Help if possible."
  :group 'idlwave-online-help
  :type 'boolean)

(defcustom idlwave-help-frame-parameters
  '((height . 32) (unsplittable . t))
  "The frame parameters for the special Online Help frame.
See also `idlwave-help-use-dedicated-frame'.
If you do not set the frame width here, the value specified in
`idlw-help.el' will be used."
  :group 'idlwave-online-help
  :type '(repeat
	  (cons symbol sexp)))

(defcustom idlwave-max-popup-menu-items 20
  "Maximum number of items per pane in popup menus.
Currently only used for class selection during completion help."
  :group 'idlwave-online-help
  :type 'integer)

(defcustom idlwave-extra-help-function 'idlwave-help-with-source
  "The function to call for online help if the normal help fails.
Online help works only for system routines which are described in the
IDL manuals.  A function may be specified to access help from other sources.

The function must accept four arguments: NAME, TYPE, CLASS, KEYWORD.
The Help buffer is current when this function is called, and the help
text should be loaded into this buffer.  If help is found, the
function should return the buffer position which should be used as
`window-start' in the help window.  Also, the variable
`idlwave-help-mode-line-indicator' should be set to a useful string,
which will be displayed in the mode line of the help window.  If
should also set the variable `idlwave-help-min-frame-width' to a
positive integer.  IDLWAVE will ensure that the help frame is at least
that many columns wide.  Failure to find help should be indicated by
throwing an error.

When this variable is non-nil, IDLWAVE will allow the mouse-3 help click
for every routine and keyword, even though the item may not be highlighted
in blue (indicating the availability of system documentation).

The default value for this function is `idlwave-help-with-source' which
loads the routine source file into the help buffer.  If you try to write
a different function which accesses a special help file or so, it is
probably a good idea to still call this function as a fallback."
  :group 'idlwave-online-help
  :type 'symbol)

(defcustom idlwave-help-fontify-source-code nil
  "*Non-nil means, fontify source code displayed as help like normal code."
  :group 'idlwave-online-help
  :type 'boolean)

(defcustom idlwave-help-source-try-header t
  "*Non-nil means, try to find help in routine header when displaying source.
Routines which are not documented in the system manual use their source as
help text.  When this variable is non-nil, we try to find a description of
the help item in the first routine doclib header above the routine definition.
If the variable is nil, or if we cannot find/parse the header, the routine
definition is displayed instead."
  :group 'idlwave-online-help
  :type 'boolean)


(defcustom idlwave-help-doclib-name "name"
  "*A regexp for the heading word to search for in doclib headers
which specifies the `name' section.  Can be used for localization
support."
  :group 'idlwave-online-help
  :type 'string)

(defcustom idlwave-help-doclib-keyword "KEYWORD"
  "*A regexp for the heading word to search for in doclib headers
which specifies the `keywords' section.  Can be used for localization
support."
  :group 'idlwave-online-help
  :type 'string)

(defface idlwave-help-link
  '((t :inherit link))
  "Face for highlighting links into IDLWAVE online help."
  :group 'idlwave-online-help)

(defvar idlwave-help-activate-links-aggressively nil
  "Obsolete variable.")

(defvar idlwave-completion-help-info)

(defvar idlwave-help-frame nil
  "The frame for display of IDL online help.")
(defvar idlwave-help-frame-width 102
  "The default width of the help frame.")

(defvar idlwave-html-help-is-available nil
  "Is the system online help text available?")

(defvar idlwave-help-mode-line-indicator ""
  "Used for the special mode line in the `idlwave-help-mode'.")

(defvar idlwave-help-window-configuration nil)
(defvar idlwave-help-special-topic-words nil) ; defined by get_rinfo

;; Define the key bindings for the Help application

(defvar idlwave-help-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'idlwave-help-quit)
    (define-key map "w" 'widen)
    (define-key map "\C-m" (lambda (arg)
                             (interactive "p")
                             (scroll-up arg)))
    (define-key map " " 'scroll-up-command)
    (define-key map [delete] 'scroll-down-command)
    (define-key map "h" 'idlwave-help-find-header)
    (define-key map "H" 'idlwave-help-find-first-header)
    (define-key map "." 'idlwave-help-toggle-header-match-and-def)
    (define-key map "F" 'idlwave-help-fontify)
    (define-key map "\M-?" 'idlwave-help-return-to-calling-frame)
    (define-key map "x" 'idlwave-help-return-to-calling-frame)
    map)
  "The keymap used in `idlwave-help-mode'.")

;; Define the menu for the Help application

(easy-menu-define
  idlwave-help-menu idlwave-help-mode-map
  "Menu for Help IDLWAVE system"
  '("IDLHelp"
    ["Definition <-> Help Text" idlwave-help-toggle-header-match-and-def t]
    ["Find DocLib Header" idlwave-help-find-header t]
    ["Find First DocLib Header" idlwave-help-find-first-header t]
    ["Fontify help buffer" idlwave-help-fontify t]
    "--"
    ["Quit" idlwave-help-quit t]))

(defvar idlwave-help-def-pos)
(defvar idlwave-help-args)
(defvar idlwave-help-in-header)
(declare-function idlwave-prepare-structure-tag-completion "idlw-complete-structtag")
(declare-function idlwave-all-method-classes "idlwave")
(declare-function idlwave-all-method-keyword-classes "idlwave")
(declare-function idlwave-beginning-of-statement "idlwave")
(declare-function idlwave-best-rinfo-assoc "idlwave")
(declare-function idlwave-class-found-in "idlwave")
(declare-function idlwave-class-or-superclass-with-tag "idlwave")
(declare-function idlwave-completing-read "idlwave")
(declare-function idlwave-current-routine "idlwave")
(declare-function idlwave-downcase-safe "idlwave")
(declare-function idlwave-entry-find-keyword "idlwave")
(declare-function idlwave-expand-keyword "idlwave")
(declare-function idlwave-find-class-definition "idlwave")
(declare-function idlwave-find-inherited-class "idlwave")
(declare-function idlwave-find-struct-tag "idlwave")
(declare-function idlwave-get-buffer-visiting "idlwave")
(declare-function idlwave-in-quote "idlwave")
(declare-function idlwave-make-full-name "idlwave")
(declare-function idlwave-members-only "idlwave")
(declare-function idlwave-popup-select "idlwave")
(declare-function idlwave-routine-source-file "idlwave")
(declare-function idlwave-routines "idlwave")
(declare-function idlwave-sintern-class "idlwave")
(declare-function idlwave-sintern-keyword "idlwave")
(declare-function idlwave-sintern-method "idlwave")
(declare-function idlwave-sintern-routine-or-method "idlwave")
(declare-function idlwave-sintern-sysvar "idlwave" t t);idlwave-new-sintern-type
(declare-function idlwave-sintern-sysvartag "idlwave" t t)
(declare-function idlwave-substitute-link-target "idlwave")
(declare-function idlwave-sys-dir "idlwave")
(declare-function idlwave-this-word "idlwave")
(declare-function idlwave-what-module-find-class "idlwave")
(declare-function idlwave-where "idlwave")

(define-derived-mode idlwave-help-mode special-mode "IDLWAVE Help"
  "Major mode for displaying IDL Help.

This is a VIEW mode for the ASCII version of IDL Help files,
with some extras.  Its main purpose is speed - so don't
expect a fully hyper-linked help.

Scrolling:          SPC  DEL  RET
Text Searches:      Inside Topic: Use Emacs search functions
Exit:               [q]uit or mouse button 3 will kill the frame

When the hep text is a source file, the following commands are available

Fontification:      [F]ontify the buffer like source code
Jump:               [h] to function doclib header
                    [H] to file doclib header
                    [.] back and forth between header and definition

Here are all keybindings.
\\{idlwave-help-mode-map}"
  (buffer-disable-undo)
  (easy-menu-add idlwave-help-menu idlwave-help-mode-map)
  (setq truncate-lines t)
  (setq case-fold-search t)
  (setq mode-line-format
	(list ""
	      'mode-line-modified
	      'mode-line-buffer-identification
	      ":  " 'idlwave-help-mode-line-indicator
	      " -%-"))
  (setq buffer-read-only t)
  (set (make-local-variable 'idlwave-help-def-pos) nil)
  (set (make-local-variable 'idlwave-help-args) nil)
  (set (make-local-variable 'idlwave-help-in-header) nil))

(defun idlwave-html-help-location ()
  "Return the help directory where HTML files are, or nil if that is unknown."
  (let ((syshelp-dir (expand-file-name
		      idlwave-html-system-help-location (idlwave-sys-dir)))
	(help-dir (or (and (stringp idlwave-html-help-location)
			   (> (length idlwave-html-help-location) 0)
			   idlwave-html-help-location)
		      (getenv "IDLWAVE_HELP_LOCATION"))))
    (if (and syshelp-dir (file-directory-p syshelp-dir))
	syshelp-dir
      (if help-dir
	  (progn
	    (setq help-dir (expand-file-name "idl_html_help" help-dir))
	    (if (file-directory-p help-dir) help-dir))))))

(defvar idlwave-help-assistant-available nil)

(defun idlwave-help-check-locations ()
  ;; Check help locations and assistant.
  (let ((sys-dir (idlwave-sys-dir))
	(help-loc (idlwave-html-help-location)))
    (if (or (not (file-directory-p sys-dir))
	    (not help-loc)
	    (not (file-directory-p help-loc)))
	(message
	 "HTML help location not found: try setting `idlwave-system-directory' and/or `idlwave-html-help-location'."))
    ;; see if we have the assistant
    (when (and idlwave-help-use-assistant
	       (not (eq (idlwave-help-assistant-available) t)))
      (message "Cannot locate IDL Assistant, enabling default browser.")
      (setq idlwave-help-use-assistant nil)
      (unless idlwave-help-browse-url-available
	(error "browse-url is not available; install it or IDL Assistant to use HTML help")))))


(defvar idlwave-current-obj_new-class)
(defvar idlwave-help-diagnostics)
(defvar idlwave-experimental)
(defvar idlwave-last-context-help-pos)
(defun idlwave-do-context-help (&optional arg)
  "Wrapper around the call to `idlwave-do-context-help1'.
It collects and prints the diagnostics messages."
  (let ((marker (list (current-buffer) (point)))
	(idlwave-help-diagnostics nil))
    ;; Check for frame switching.  When the command is invoked twice
    ;; at the same position, we try to switch to the help frame
    ;; FIXME:  Frame switching works only on XEmacs
    (if (and idlwave-experimental
	     (equal last-command this-command)
	     (equal idlwave-last-context-help-pos marker))
	(idlwave-help-select-help-frame)
      ;; Do the real thing.
      (setq idlwave-last-context-help-pos marker)
      (idlwave-do-context-help1 arg)
      (if idlwave-help-diagnostics
	  (message "%s" (mapconcat 'identity
				   (nreverse idlwave-help-diagnostics)
				   "; "))))))

(defvar idlwave-help-do-class-struct-tag nil)
(defvar idlwave-structtag-struct-location)
(defvar idlwave-help-do-struct-tag nil)
(defvar idlwave-system-variables-alist)
(defvar idlwave-executive-commands-alist)
(defvar idlwave-system-class-info)
(defun idlwave-do-context-help1 (&optional arg)
  "The work-horse version of `idlwave-context-help', which see."
  (save-excursion
    (if (equal (char-after) ?/)
	(forward-char 1)
      (if (equal (char-before) ?=)
	  (backward-char 1)))
    (let* ((idlwave-query-class nil)
	   (idlwave-force-class-query (equal arg '(4)))
	   (chars "a-zA-Z0-9_$.!")
	   (beg (save-excursion (skip-chars-backward chars) (point)))
	   (end (save-excursion (skip-chars-forward chars) (point)))
	   (this-word (buffer-substring-no-properties beg end))
	   (st-ass (assoc-string this-word
				 idlwave-help-special-topic-words t))
	   (classtag (and (string-match "self\\." this-word)
			  (< beg (- end 4))))
	   (structtag (and (fboundp 'idlwave-complete-structure-tag)
			   (string-match "\\`\\([^.]+\\)\\." this-word)
			   (< beg (- end 4))))
	   module keyword cw mod1 mod2 mod3)
      (if (or arg
	      (and (not classtag)
		   (not structtag)
		   (not (member (string-to-char this-word) '(?! ?.)))))
	  ;; Need the module information
	  (progn
	    ;; MODULE is (name type class), for this or any inheriting class
	    (setq module (idlwave-what-module-find-class)
		  cw (nth 2 (idlwave-where))) ;what would we complete here?
	    ;; Correct for OBJ_NEW, we may need an INIT method instead.
	    (if (equal (idlwave-downcase-safe (car module)) "obj_new")
		(let* ((bos (save-excursion (idlwave-beginning-of-statement)
					    (point)))
		       (str (buffer-substring bos (point))))
		  (if (string-match "OBJ_NEW([ \t]*['\"]\\([a-zA-Z][a-zA-Z0-9$_]+\\)['\"]" str)
		      (setq module (list "init" 'fun (match-string 1 str))
			    idlwave-current-obj_new-class (match-string 1 str))
		    )))))
      (cond
       (arg (setq mod1 module))

       ;; A special topic -- only system help
       ((and st-ass (not (memq cw '(function-keyword procedure-keyword))))
	(setq mod1 (list (cdr st-ass))))

       ;; A system variable -- only system help
       ((string-match
	 "\\`!\\([a-zA-Z0-9_]+\\)\\(\.\\([A-Za-z0-9_]+\\)\\)?"
	 this-word)
	(let* ((word  (match-string-no-properties 1 this-word))
	       (entry (assq (idlwave-sintern-sysvar word)
			    idlwave-system-variables-alist))
	       (tag (match-string-no-properties 3 this-word))
	       (tag-target (if tag
			       (cdr
				(assq (idlwave-sintern-sysvartag tag)
				      (cdr (assq 'tags entry))))))
	       (link (nth 1 (assq 'link entry))))
	  (if tag-target
	      (setq link (idlwave-substitute-link-target link
							 tag-target)))
	  (setq mod1 (list link))))

       ;; An executive command -- only system help
       ((string-match "^\\.\\([A-Z_]+\\)" this-word)
	(let* ((word  (match-string 1 this-word))
	       (link  (cdr (assoc-string
			    word
			    idlwave-executive-commands-alist t))))
	  (setq mod1 (list link))))

       ;; A class -- system OR in-text help (via class__define).
       ((and (eq cw 'class)
	     (or (idlwave-in-quote)  ; e.g. obj_new
		 (re-search-backward "\\<inherits[ \t]+[A-Za-z0-9_]*\\="
				     (max (point-min) (- (point) 40)) t)))
	;; Class completion inside string delimiters must be
	;; the class inside OBJ_NEW.
	(let* ((entry  (assq
			(idlwave-sintern-class this-word)
			idlwave-system-class-info))
	       (name   (concat (downcase this-word) "__define"))
	       (link   (nth 1 (assq 'link entry))))
	  (setq mod1 (list link name 'pro))))

       ;; A class structure tag (self.BLAH) -- only in-text help available
       (classtag
	(let ((tag (substring this-word (match-end 0)))
	      class-with found-in)
	  (when (setq class-with
		      (idlwave-class-or-superclass-with-tag
		       (nth 2 (idlwave-current-routine))
		       tag))
	    (setq found-in (idlwave-class-found-in class-with))
	    (if (assq (idlwave-sintern-class class-with)
		      idlwave-system-class-info)
		(error "No help available for system class tags"))
	    (setq idlwave-help-do-class-struct-tag t)
	    (setq mod1 (list nil
			     (if found-in
				 (cons (concat found-in "__define") class-with)
			       (concat class-with "__define"))
			     'pro
			     nil ; no class.... it's a procedure!
			     tag)))))

       ;; A regular structure tag -- only in text, and if
       ;; optional `complete-structtag' loaded.
       (structtag
	(let ((var (match-string 1 this-word))
	      (tag (substring this-word (match-end 0))))
	  ;; Check if we need to update the "current" structure
	  (idlwave-prepare-structure-tag-completion var)
	  (setq idlwave-help-do-struct-tag
		idlwave-structtag-struct-location
		mod1 (list nil nil nil nil tag))))

       ;; A routine keyword -- in text or system help
       ((and (memq cw '(function-keyword procedure-keyword))
	     (stringp this-word)
	     (string-match "\\S-" this-word)
	     (not (string-match "!" this-word)))
	(cond ((or (= (char-before beg) ?/)
		   (save-excursion (goto-char end)
				   (looking-at "[ \t]*=")))
	       ;; Certainly a keyword. Check for abbreviation etc.
	       (setq keyword (idlwave-expand-keyword this-word module))
	       (cond
		((null keyword)
		 (idlwave-help-diagnostics
		  (format "%s does not accept `%s' kwd"
			  (idlwave-make-full-name (nth 2 module)
						  (car module))
			  (upcase this-word))
		  'ding))
		((consp keyword)
		 (idlwave-help-diagnostics
		  (format "%d matches for kwd abbrev `%s'"
			  (length keyword) this-word)
		  'ding)
		 ;; We continue anyway with the first match...
		 (setq keyword (car keyword))))
	       ;; Keyword, or just module
	       (setq mod1 (append (list t) module (list keyword)))
	       (setq mod2 (append (list t) module)))
	      ((equal (char-after end) ?\()
	       ;; A function - what-module will have caught this
	       (setq mod1 (append (list t) module)))
	      (t
	       ;; undecided - try function, keyword, then enclosing mod.
	       ;; Check for keyword abbreviations, but do not report
	       ;; errors, because it might be something else.
	       ;; FIXME: is this a good way to handle this?
	       (setq keyword (idlwave-expand-keyword this-word module))
	       (if (consp keyword) (setq keyword (car keyword)))
	       (setq mod1 (append (list t) module (list keyword))
		     mod2 (list t this-word 'fun nil)
		     mod3 (append (list t) module)))))

       ;; Everything else
       (t
	(setq mod1 (append (list t) module))))
      (if mod3
	  (condition-case nil
	      (apply 'idlwave-online-help mod1)
	    (error (condition-case nil
		       (apply 'idlwave-online-help mod2)
		     (error (apply 'idlwave-online-help mod3)))))
	(if mod2
	    (condition-case nil
		(apply 'idlwave-online-help mod1)
	      (error (apply 'idlwave-online-help mod2)))
	  (if mod1
	      (apply 'idlwave-online-help mod1)
	    (error "Don't know which item to show help for")))))))

(defun idlwave-do-mouse-completion-help (ev)
  "Display online help on an item in the *Completions* buffer.
Needs additional info stored in global `idlwave-completion-help-info'."
  (let* ((cw (selected-window))
	 (info idlwave-completion-help-info) ; global passed in
	 (what (nth 0 info))
	 (idlw-help-name (nth 1 info))
	 (type (nth 2 info))
	 (class (nth 3 info))
	 (need-class class)
	 (idlw-help-kwd (nth 4 info))
	 (sclasses (nth 5 info))
	 word idlw-help-link)
    (mouse-set-point ev)


    ;; See if we can also find help somewhere, e.g. for multiple classes
    (setq word (idlwave-this-word))
    (if (string= word "")
	(error "No help item selected"))
    (setq idlw-help-link (get-text-property 0 'link word))
    (select-window cw)
    (cond
     ;; Routine name
     ((memq what '(procedure function routine))
      (setq idlw-help-name word)
      (if (or (eq class t)
	      (and (stringp class) sclasses))
	  (let* ((classes (idlwave-all-method-classes
			   (idlwave-sintern-method idlw-help-name)
			   type)))
	    (setq idlw-help-link t)		; No specific link valid yet
	    (if sclasses
		(setq classes (idlwave-members-only
			       classes (cons class sclasses))))
	    (setq class (idlwave-popup-select ev classes
					      "Select Class" 'sort))))

      ;; XXX is this necessary, given all-method-classes?
      (if (stringp class)
	  (setq class (idlwave-find-inherited-class
		       (idlwave-sintern-routine-or-method idlw-help-name class)
		       type (idlwave-sintern-class class)))))

     ;; Keyword
     ((eq what 'keyword)
      (setq idlw-help-kwd word)
      (if (or (eq class t)
	      (and (stringp class) sclasses))
	  (let ((classes  (idlwave-all-method-keyword-classes
			   (idlwave-sintern-method idlw-help-name)
			   (idlwave-sintern-keyword idlw-help-kwd)
			   type)))
	    (setq idlw-help-link t) ; Link can't be correct yet
	    (if sclasses
		(setq classes (idlwave-members-only
			       classes (cons class sclasses))))
	    (setq class (idlwave-popup-select ev classes
					      "Select Class" 'sort))
	    ;; XXX is this necessary, given all-method-keyword-classes?
	    (if (stringp class)
		(setq class (idlwave-find-inherited-class
			     (idlwave-sintern-routine-or-method
			      idlw-help-name class)
			     type (idlwave-sintern-class class)))))
	(if (string= (downcase idlw-help-name) "obj_new")
	    (setq class idlwave-current-obj_new-class
		  idlw-help-name "Init"))))

     ;; Class name
     ((eq what 'class)
      (setq class word
	    word nil))

     ;; A special named function to call which sets some of our variables
     ((and (symbolp what)
	   (fboundp what))
      (funcall what 'set word))

     (t (error "Cannot help with this item")))
    (if (and need-class (not class)
	     (not (and idlw-help-link (not (eq idlw-help-link t)))))
	(error "Cannot help with this item"))
    (idlwave-online-help idlw-help-link (or idlw-help-name word)
			 type class idlw-help-kwd)))

(defvar idlwave-highlight-help-links-in-completion)
(defvar idlwave-completion-help-links)
(defun idlwave-highlight-linked-completions ()
  "Highlight all completions for which help is available and attach link.
Those words in `idlwave-completion-help-links' have links.  The
`idlwave-help-link' face is used for this."
  (if idlwave-highlight-help-links-in-completion
      (with-current-buffer (get-buffer "*Completions*")
	(save-excursion
	  (let* ((case-fold-search t)
		 (props (list 'face 'idlwave-help-link))
		 (info idlwave-completion-help-info) ; global passed in
		 (what (nth 0 info))  ; what was completed, or a func
		 (class (nth 3 info)) ; any class
		 word beg end doit)
	    (goto-char (point-min))
	    (re-search-forward "possible completions are:" nil t)
	    (while (re-search-forward "\\s-\\([A-Za-z0-9_.]+\\)\\(\\s-\\|\\'\\)"
				      nil t)
	      (setq beg (match-beginning 1) end (match-end 1)
		    word (match-string 1) doit nil)
	      ;; Call special completion function test
	      (if (and (symbolp what)
		       (fboundp what))
		  (setq doit (funcall what 'test word))
		;; Look for special link property passed in help-links
		(if idlwave-completion-help-links
		    (setq doit (assoc-string
				word idlwave-completion-help-links t))))
	      (when doit
		(if (consp doit)
		    (setq props (append props `(link ,(cdr doit)))))
		(let ((buffer-read-only nil))
		  (add-text-properties beg end props)))
	      (goto-char end)))))))

;; Arrange for this function to be called after completion
(add-hook 'idlwave-completion-setup-hook
	  'idlwave-highlight-linked-completions)

(defvar idlwave-help-return-frame nil
  "The frame to return to from the help frame.")

(defun idlwave-help-quit ()
  "Exit IDLWAVE Help buffer.  Kill the dedicated frame if any."
  (interactive)
  (cond ((and idlwave-help-use-dedicated-frame
	      (eq (selected-frame) idlwave-help-frame))
	 (if (and idlwave-experimental
		  (frame-live-p idlwave-help-return-frame))
	     ;; Try to select the return frame.
	     ;; This can crash on slow network connections, obviously when
	     ;; we kill the help frame before the return-frame is selected.
	     ;; To protect the workings, we wait for up to one second
	     ;; and check if the return-frame *is* now selected.
	     ;; This is marked "experimental" since we are not sure when
	     ;; it's OK.
	     (let ((maxtime 1.0) (time 0.) (step 0.1))
	       (select-frame idlwave-help-return-frame)
	       (while (and (sit-for step)
			   (not (eq (selected-frame)
				    idlwave-help-return-frame))
			   (< (setq time (+ time step)) maxtime)))))
	 (delete-frame idlwave-help-frame))
	((window-configuration-p idlwave-help-window-configuration)
	 (set-window-configuration idlwave-help-window-configuration)
	 (select-window (previous-window)))
	(t (kill-buffer (idlwave-help-get-help-buffer)))))


(defvar default-toolbar-visible-p)

(defun idlwave-help-display-help-window (&optional pos-or-func)
  "Display the help window.
Move window start to POS-OR-FUNC, if passed as a position, or call it
if passed as a function.  See `idlwave-help-use-dedicated-frame'."
  (let ((cw (selected-window))
	(buf (idlwave-help-get-help-buffer)))
    (if (and window-system idlwave-help-use-dedicated-frame)
	(progn
	  (idlwave-help-show-help-frame)
	  (switch-to-buffer buf))
      ;; Do it in this frame and save the window configuration
      (if (not (get-buffer-window buf nil))
	  (setq idlwave-help-window-configuration
		(current-window-configuration)))
      (display-buffer buf nil (selected-frame))
      (select-window (get-buffer-window buf)))
    (raise-frame)
    (if pos-or-func
	(if (functionp pos-or-func)
	    (funcall pos-or-func)
	  (goto-char pos-or-func)
	  (recenter 0)))
    (select-window cw)))

(defun idlwave-help-select-help-frame ()
  "Select the help frame."
  (if (and (frame-live-p idlwave-help-frame)
	   (not (eq (selected-frame) idlwave-help-frame)))
      (progn
	(setq idlwave-help-return-frame (selected-frame))
	(select-frame idlwave-help-frame))))

(defun idlwave-help-return-to-calling-frame ()
  "Select the frame from which the help frame was selected."
  (interactive)
  (if (and (frame-live-p idlwave-help-return-frame)
	   (not (eq (selected-frame) idlwave-help-return-frame)))
      (select-frame idlwave-help-return-frame)))

(defun idlwave-online-help (link &optional name type class keyword)
  "Display HTML or other special help on a certain topic.
Either loads an HTML link, if LINK is non-nil, or gets special-help on
the optional arguments, if any special help is defined.  If LINK is
t, first look up the optional arguments in the routine info list to
see if a link is set for it.  Try extra help functions if necessary."
  ;; Lookup link
  (if (eq link t)
      (let ((entry (idlwave-best-rinfo-assoc name type class
					     (idlwave-routines) nil t)))
	(if entry
	    (cond
	     ;; Try keyword link
	     ((and keyword
		   (setq link (cdr
			       (idlwave-entry-find-keyword entry keyword)))))
	     ;; Default, regular entry link
	     (t (setq link (idlwave-entry-has-help entry))))
	  (if (and
	       class
	       ;; Check for system class help
	       (setq entry (assq (idlwave-sintern-class class)
				 idlwave-system-class-info)
		     link (nth 1 (assq 'link entry))))
	      (message
	       (concat "No routine info for %s"
		       ", falling back on class help.")
	       (idlwave-make-full-name class name))))))

  (cond
   ;; An explicit link
   ((stringp link)
    (idlwave-help-html-link link))

   ;; Any extra help
   (idlwave-extra-help-function
    (idlwave-help-get-special-help name type class keyword))

   ;; Nothing worked
   (t (idlwave-help-error name type class keyword))))


(defun idlwave-help-get-special-help (name type class keyword)
  "Call the function given by `idlwave-extra-help-function'."
  (let* ((cw (selected-window))
	 (help-pos (with-current-buffer (idlwave-help-get-help-buffer)
		     (let ((buffer-read-only nil))
		       (funcall idlwave-extra-help-function
				name type class keyword)))))
    (if help-pos
	(idlwave-help-display-help-window help-pos)
      (idlwave-help-error name type class keyword))
    (select-window cw)))

(defun idlwave-help-html-link (link)
  "Get HTML help on a given LINK."
  (let ((browse-url-browser-function idlwave-help-browser-function)
	(help-loc (idlwave-html-help-location))
	(browse-url-generic-program idlwave-help-browser-generic-program)
	;(browse-url-generic-args idlwave-help-browser-generic-args)
	full-link)

    ;; Just a regular file name (+ anchor name)
    (unless (and (stringp help-loc)
		 (file-directory-p help-loc))
      (error "Invalid help location"))
    (setq full-link (browse-url-file-url (expand-file-name link help-loc)))

    ;; Select the browser
    (cond
     (idlwave-help-use-assistant
      (idlwave-help-assistant-open-link link))

     ((or idlwave-help-browser-is-local
	  (string-match "w3" (symbol-name idlwave-help-browser-function)))
      (idlwave-help-display-help-window (lambda () (browse-url full-link))))

     (t (browse-url full-link)))))

;; A special help routine for source-level syntax help in files.
(defvar idlwave-help-fontify-source-code)
(defvar idlwave-help-source-try-header)
(defvar idlwave-current-tags-buffer)
(defvar idlwave-current-tags-class)
(defun idlwave-help-with-source (name type class keyword)
  "Provide help for routines not documented in the IDL manuals.
Works by loading the routine source file into the help buffer.
Depending on the value of `idlwave-help-source-try-header', it
attempts to show the routine definition or the header description.
If `idlwave-help-do-class-struct-tag' is non-nil, keyword is a tag
to show help on from the class definition structure.
If `idlwave-help-do-struct-tag' is non-nil, show help from the
matching structure tag definition.

This function can be used as `idlwave-extra-help-function'."
  (let* ((class-struct-tag idlwave-help-do-class-struct-tag)
	 (struct-tag idlwave-help-do-struct-tag)
	 (case-fold-search t)
	 (real-class (if (consp name) (cdr name)))
	 (name (if (consp name) (car name) name))
	 (class-only (and (stringp class) (not (stringp name))))
	 file header-pos def-pos in-buf)
    (if class-only   ;Help with class?  Using "Init" as source.
	(setq name "Init"
	      type 'fun))
    (if (not struct-tag)
	(setq file
	      (idlwave-routine-source-file
	       (nth 3 (idlwave-best-rinfo-assoc
		       name (or type t) class (idlwave-routines))))))
    (setq idlwave-help-def-pos nil
	  idlwave-help-args (list name type class keyword)
	  idlwave-help-in-header nil
	  idlwave-help-do-struct-tag nil
	  idlwave-help-do-class-struct-tag nil)
    (if (or struct-tag (stringp file))
	(progn
	  (setq in-buf ; structure-tag completion is always in current buffer
		(if struct-tag
		    idlwave-current-tags-buffer
		  (idlwave-get-buffer-visiting file)))
	  ;; see if file is in a visited buffer, insert those contents
	  (if in-buf
	      (progn
		(setq file (buffer-file-name in-buf))
		(erase-buffer)
		(insert-buffer-substring in-buf))
	    (if (file-exists-p file) ;; otherwise just load the file
		(progn
		  (erase-buffer)
		  (insert-file-contents file nil nil nil 'replace))
	      (idlwave-help-error name type class keyword)))
	  (goto-char (point-min))
	  (if (and idlwave-help-fontify-source-code (not in-buf))
	      (idlwave-help-fontify)))
      (idlwave-help-error name type class keyword))
    (setq idlwave-help-mode-line-indicator file)

    ;; Try to find a good place to display
    (setq def-pos
	  ;; Find the class structure tag if that's what we're after
	  (cond
	   ;; Class structure tags: find the class or named structure
	   ;; definition
	   (class-struct-tag
	    (save-excursion
	      (setq class
		    (if (string-match "[a-zA-Z0-9]\\(__\\)" name)
			(substring name 0 (match-beginning 1))
		      idlwave-current-tags-class))
	      (and
	       (idlwave-find-class-definition class nil real-class)
	       (idlwave-find-struct-tag keyword))))

	   ;; Generic structure tags: the structure definition
	   ;; location within the file has been recorded in
	   ;; `struct-tag'
	   (struct-tag
	    (save-excursion
	      (and
	       (integerp struct-tag)
	       (goto-char struct-tag)
	       (idlwave-find-struct-tag keyword))))

	   ;; Just find the routine definition
	   (t
	    (if class-only (point-min)
	      (idlwave-help-find-routine-definition name type class keyword))))
	  idlwave-help-def-pos def-pos)

    (if (and idlwave-help-source-try-header
	     (not (or struct-tag class-struct-tag)))
	;; Check if we can find the header
	(save-excursion
	  (goto-char (or def-pos (point-max)))
	  (setq header-pos (idlwave-help-find-in-doc-header
			    name type class keyword 'exact)
		idlwave-help-in-header header-pos)))

    (if (or header-pos def-pos)
	(progn
	  (if (boundp 'idlwave-help-min-frame-width)
	      (setq idlwave-help-min-frame-width 80))
	  (goto-char (or header-pos def-pos)))
      (idlwave-help-error name type class keyword))

    (point)))


(defun idlwave-help-find-routine-definition (name type class keyword)
  "Find the definition of routine CLASS::NAME in current buffer.
Returns the point of match if successful, nil otherwise.
KEYWORD is ignored."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
	 (concat "^[ \t]*"
		 (if (eq type 'pro) "pro"
		   (if (eq type 'fun) "function"
		     "\\(pro\\|function\\)"))
		 "[ \t]+"
		 (regexp-quote (downcase (idlwave-make-full-name class name)))
		 "[, \t\r\n]")
	 nil t)
	(match-beginning 0)
      nil)))

(defvar idlwave-doclib-start)
(defvar idlwave-doclib-end)
(defun idlwave-help-find-in-doc-header (name type class keyword
					     &optional exact)
  "Find the requested help in the doc-header above point.

First checks if there is a doc-lib header which describes the correct
routine.  Then tries to find the KEYWORDS section and the KEYWORD, if
given.  Returns the point which should be window start of the help
window.  If EXACT is non-nil, the full help position must be found -
down to the keyword requested.  This setting is for context help, if
the exact spot is needed.

If EXACT is nil, the position of the header is returned if it
describes the correct routine - even if the keyword description cannot
be found.  TYPE is ignored.

This function expects a more or less standard routine header.  In
particular it looks for the `NAME:' tag, either with a colon, or alone
on a line.  Then `NAME:' must be followed by the routine name on the
same or the next line.  When KEYWORD is non-nil, looks first for a
`KEYWORDS' section.  It is amazing how inconsistent this is through
some IDL libraries I have seen.  We settle for a line containing an
upper case \"KEYWORD\" string.  If this line is not found we search
for the keyword anyway to increase the hit-rate

When one of these sections exists we check for a line starting with any of

  /KEYWORD  KEYWORD-  KEYWORD=  KEYWORD

with spaces allowed between the keyword and the following dash or equal sign.
If there is a match, we assume it is the keyword description."
  (let* ((case-fold-search t)
	 (rname (if (stringp class)
		    (concat
		     "\\("
		     ;; Traditional name or class::name
		     "\\("
		     "\\(" (regexp-quote (downcase class)) "::\\)?"
		     (regexp-quote (downcase name))
		     "\\>\\)"
		     (concat
		      "\\|"
		      ;; class__define or just class
		      (regexp-quote (downcase class)) "\\(__define\\)?")
		     "\\)")
		  (regexp-quote (downcase name))))

	 ;; NAME tag plus the routine name.  The new version is from JD.
	 (name-re (concat
		   "\\(^;+\\*?[ \t]*"
		   idlwave-help-doclib-name
		   "\\([ \t]*:\\|[ \t]*$\\)[ \t]*\\(\n;+[ \t]*\\)*"
		   rname
		   "\\|"
		   "^;+[ \t]*"
		   rname
		   ":[ \t]*$\\)"))

	 ;; Header start plus name
	 (header-re (concat "\\(" idlwave-doclib-start "\\).*\n"
			    "\\(^;+.*\n\\)*"
			    "\\(" name-re "\\)"))
	 ;; A keywords section
	 (kwds-re (concat		                    ; forgiving
		   "^;+\\*?[ \t]*"
		   "\\([-A-Z_ ]*"
		   idlwave-help-doclib-keyword
		   "[-A-Z_ ]*\\)"
		   "\\(:\\|[ \t]*\n\\)"))

	 ;; The individual keyword description line.
	 (kwd-re (if keyword                                ; hard (well...)
		     (concat
		      "^;+[ \t]+"
		      "\\(/" (regexp-quote (upcase keyword))
		      "\\|"  (regexp-quote (upcase keyword)) "[ \t]*[-=:\n]"
		      "\\)")))
	 (kwd-re2 (if keyword                               ; forgiving
		      (concat
		       "^;+[ \t]+"
		       (regexp-quote (upcase keyword))
		      "\\>")))
	 dstart dend name-pos kwds-pos kwd-pos)
    (catch 'exit
      (save-excursion
	(goto-char (point-min))
	(while (and (setq dstart (re-search-forward idlwave-doclib-start nil t))
		    (setq dend (re-search-forward idlwave-doclib-end nil t)))
	  ;; found a routine header
	  (goto-char dstart)
	  (if (setq name-pos (re-search-forward name-re dend t))
	      (progn
		(if keyword
		    ;; We do need a keyword
		    (progn
		      ;; Try to find a keyword section, but don't force it.
		      (goto-char name-pos)
		      (if (let ((case-fold-search nil))
			    (re-search-forward kwds-re dend t))
			  (setq kwds-pos (match-beginning 0)))
		      ;; Find the keyword description
		      (if (or (let ((case-fold-search nil))
				(re-search-forward kwd-re dend t))
			      (re-search-forward kwd-re dend t)
			      (let ((case-fold-search nil))
				(re-search-forward kwd-re2 dend t))
			      (re-search-forward kwd-re2 dend t))
			  (setq kwd-pos (match-beginning 0))
			(if exact
			    (progn
			      (idlwave-help-diagnostics
			       (format "Could not find description of kwd %s"
				       (upcase keyword)))
			      (throw 'exit nil))))))
		;; Return the best position we got
		(throw 'exit (or kwd-pos kwds-pos name-pos dstart)))
	    (goto-char dend))))
      (idlwave-help-diagnostics "Could not find doclib header")
      (throw 'exit nil))))

(defun idlwave-help-diagnostics (string &optional ding)
  "Add a diagnostics string to the list.
When DING is non-nil, ring the bell as well."
  (if (boundp 'idlwave-help-diagnostics)
      (progn
	(setq idlwave-help-diagnostics
	      (cons string idlwave-help-diagnostics))
	(if ding (ding)))))

(defun idlwave-help-toggle-header-top-and-def (arg)
  (interactive "P")
  (let (pos)
    (if idlwave-help-in-header
	;; Header was the last thing displayed
	(progn
	  (setq idlwave-help-in-header nil)
	  (setq pos idlwave-help-def-pos))
      ;; Try to display header
      (setq pos (idlwave-help-find-in-doc-header
		 (nth 0 idlwave-help-args)
		 (nth 1 idlwave-help-args)
		 (nth 2 idlwave-help-args)
		 nil))
      (if pos
	  (setq idlwave-help-in-header t)
	(error "Cannot find doclib header for routine %s"
	       (idlwave-make-full-name (nth 2 idlwave-help-args)
				       (nth 0 idlwave-help-args)))))
    (if pos
	(progn
	  (goto-char pos)
	  (recenter 0)))))

(defun idlwave-help-find-first-header (arg)
  (interactive "P")
  (let (pos)
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward idlwave-doclib-start nil t)
	  (setq pos (match-beginning 0))))
    (if pos
	(progn
	  (goto-char pos)
	  (recenter 0))
      (error "No DocLib Header in current file"))))

(defun idlwave-help-find-header (arg)
  "Jump to the DocLib Header."
  (interactive "P")
  (if arg
      (idlwave-help-find-first-header nil)
    (setq idlwave-help-in-header nil)
    (idlwave-help-toggle-header-match-and-def arg 'top)))

(defun idlwave-help-toggle-header-match-and-def (arg &optional top)
  (interactive "P")
  (let ((args idlwave-help-args)
	pos)
    (if idlwave-help-in-header
	;; Header was the last thing displayed
	(progn
	  (setq idlwave-help-in-header nil)
	  (setq pos idlwave-help-def-pos))
      ;; Try to display header
      (setq pos (apply 'idlwave-help-find-in-doc-header
		       (if top
			   (list (car args) (nth 1 args) (nth 2 args) nil)
			 args)))
      (if pos
	  (setq idlwave-help-in-header t)
	(error "Cannot find doclib header for routine %s"
	       (idlwave-make-full-name (nth 2 idlwave-help-args)
				       (nth 0 idlwave-help-args)))))
    (if pos
	(progn
	  (goto-char pos)
	  (recenter 0)))))

(defvar font-lock-verbose)
(defvar idlwave-mode-syntax-table)
(defvar idlwave-font-lock-defaults)
(defun idlwave-help-fontify ()
  "Fontify the Help buffer as source code.
Useful when source code is displayed as help.  See the option
`idlwave-help-fontify-source-code'."
  (interactive)
  (if (featurep 'font-lock)
      (let ((major-mode 'idlwave-mode)
	    (font-lock-verbose
	     (if (interactive-p) font-lock-verbose nil))
	    (syntax-table (syntax-table)))
	(unwind-protect
	    (progn
	      (set-syntax-table idlwave-mode-syntax-table)
	      (set (make-local-variable 'font-lock-defaults)
		   idlwave-font-lock-defaults)
	      (font-lock-fontify-buffer))
	  (set-syntax-table syntax-table)))))


(defun idlwave-help-error (name type class keyword)
  (error "Can't find help on %s%s %s"
	 (or (and (or class name) (idlwave-make-full-name class name))
	     "<unknown>")
	 (if keyword (format ", keyword %s" (upcase keyword)) "")
	 (if idlwave-html-help-location
	     ""
	   "(help location unknown)")))

(defun idlwave-help-show-help-frame ()
  "Show the help frame, creating it if necessary."
  ;; Use a special frame for this
  (unless (frame-live-p idlwave-help-frame)
    (setq idlwave-help-frame
	  (make-frame idlwave-help-frame-parameters))
    ;; Strip menubar (?) and toolbar from the Help frame.
    (if (fboundp 'set-specifier)
	(progn
	  ;; XEmacs
	  (let ((sval (cons idlwave-help-frame nil)))
	    ;; (set-specifier menubar-visible-p sval)
	    (set-specifier default-toolbar-visible-p sval)))
      ;; Emacs
      (modify-frame-parameters idlwave-help-frame
			       '(;;(menu-bar-lines . 0)
				 (tool-bar-lines . 0)))))
  (select-frame idlwave-help-frame))

(defun idlwave-help-get-help-buffer ()
  "Return the IDLWAVE Help buffer.  Make it first if necessary."
  (let ((buf (get-buffer "*IDLWAVE Help*")))
    (if buf
	nil
      (setq buf (get-buffer-create "*IDLWAVE Help*"))
      (with-current-buffer buf
	(idlwave-help-mode)))
    buf))

(defun idlwave-grep (regexp list)
  (let (rtn)
    (while list
      (if (string-match regexp (car list))
	  (setq rtn (cons (car list) rtn)))
      (setq list (cdr list)))
    (nreverse rtn)))

(defun idlwave-entry-has-help (entry)
  (and entry (car (nth 5 entry))))

(defun idlwave-has-help (name type class)
  "Does this have help associated with it?"
  (let ((entry (idlwave-best-rinfo-assoc name type class (idlwave-routines))))
    (idlwave-entry-has-help entry)))

;;----- Control the IDL Assistant, which shipped with IDL v6.2
(defvar idlwave-help-assistant-process nil)
(defvar idlwave-help-assistant-socket nil)

;; The Windows version does not have a !DIR/bin/* set of front-end
;; scripts, but instead only links directly to bin.x86.  As a result,
;; we must pass the -profile argument as well.
(defvar idlwave-help-assistant-command
  (if (memq system-type '(ms-dos windows-nt))
      "bin/bin.x86/idl_assistant.exe"
    "bin/idl_assistant")
  "The command, rooted at `idlwave-system-directory', which invokes the
IDL assistant.")

(defun idlwave-help-assistant-available ()
  (if idlwave-help-assistant-available
      (eq idlwave-help-assistant-available t)
    (setq idlwave-help-assistant-available
	  (if (file-executable-p (idlwave-help-assistant-command))
	      t
	    'not-available))))

(defun idlwave-help-assistant-command ()
  (expand-file-name idlwave-help-assistant-command (idlwave-sys-dir)))

(defun idlwave-help-assistant-start (&optional full-link)
  "Start the IDL Assistant, loading link FULL-LINK, if passed."
  (when (or (not idlwave-help-assistant-socket)
	    (not (eq (process-status idlwave-help-assistant-socket) 'open)))
    (let* ((help-loc (idlwave-html-help-location))
	   (command (idlwave-help-assistant-command))
	   (extra-args
	    (nconc
	     (if (memq system-type '(ms-dos windows-nt))
		 `("-profile" ,(expand-file-name "idl.adp" help-loc)))
	     (if full-link `("-file" ,full-link))))
	   port)
      (if idlwave-help-assistant-socket
	  (delete-process idlwave-help-assistant-socket))

      (setq idlwave-help-assistant-process
	    (apply 'start-process
		   "IDL_ASSISTANT_PROC" nil command "-server" extra-args))

      (set-process-filter idlwave-help-assistant-process
			  (lambda (proc string)
			    (setq port (string-to-number string))))
      (unless (accept-process-output idlwave-help-assistant-process 15)
	(error "Failed binding IDL_ASSISTANT socket"))
      (if (not port)
	  (error "Unable to open IDL_ASSISTANT")
	(set-process-filter idlwave-help-assistant-process nil)
	(setq idlwave-help-assistant-socket
	      (open-network-stream "IDL_ASSISTANT_SOCK"
				   nil "localhost" port))
	(if (eq (process-status idlwave-help-assistant-socket) 'open)
	    (progn
	      (process-send-string  idlwave-help-assistant-socket
				    (concat "setHelpPath " help-loc "\n"))
	      t)
	  (idlwave-help-assistant-close)
	  (error "Cannot communicate with IDL_ASSISTANT"))))))

(defun idlwave-help-assistant-raise ()
  (idlwave-help-assistant-start)
  (process-send-string idlwave-help-assistant-socket "raise\n"))

(defun idlwave-help-assistant-open-link (&optional link)
  ;; Open a link (file name with anchor, no leading path) in the assistant.
  (let ((help-loc (idlwave-html-help-location))
	topic anchor file just-started exists full-link)

    (if (string-match "\.html" link)
	(setq topic (substring link 0 (match-beginning 0))
	      anchor (substring link (match-end 0)))
      (error "Malformed help link"))

    (setq file (expand-file-name (concat topic ".html") help-loc))
    (if (file-exists-p file)
	(setq exists t)
      (setq file (expand-file-name
		  (concat (upcase topic) ".html") help-loc))
      (setq exists (file-exists-p file)))

    (setq full-link    (concat file anchor)
	  just-started (idlwave-help-assistant-start (if exists full-link)))
    (if exists
	(progn
	  (if (not just-started)
	      (process-send-string idlwave-help-assistant-socket
				   (concat "openLink " full-link "\n")))
	  (process-send-string idlwave-help-assistant-socket
			       (concat "searchIndexNoOpen " topic "\n")))
      (process-send-string idlwave-help-assistant-socket
			   (concat "searchIndexAndOpen " topic "\n"))))
  (idlwave-help-assistant-raise))

(defvar idlwave-help-assistant-help-with-topic-history nil
  "The history of help topics selected with the minibuffer.")

(defvar idlwave-system-routines)

(defun idlwave-help-assistant-help-with-topic (&optional topic)
  "Prompt for and provide help with TOPIC."
  (interactive)
  (let (list)
    (unless topic
      (idlwave-routines)
      (setq list (append (mapcar (lambda (x)
				   (concat (nth 2 x) (car x)))
				 idlwave-system-routines)
			 (mapcar (lambda (x)
				   (concat "." (car x)))
				 idlwave-executive-commands-alist)
			 idlwave-system-class-info))
      (setq topic
	    (idlwave-completing-read
	     "Help Topic: " list
	     nil nil nil
	     'idlwave-help-assistant-help-with-topic-history)))
    (if (and topic (not (string= topic "")))
	(idlwave-help-assistant-open-link (concat topic ".html")))))

(defun idlwave-help-assistant-close ()
  (when (and idlwave-help-assistant-process
	     (eq (process-status idlwave-help-assistant-process) 'run))
    (when idlwave-help-assistant-socket
      (process-send-string idlwave-help-assistant-socket "quit\n")
      (delete-process idlwave-help-assistant-socket))
    (stop-process idlwave-help-assistant-process)
    (delete-process idlwave-help-assistant-process)
    (setq idlwave-help-assistant-socket nil
	  idlwave-help-assistant-process nil)))


(provide 'idlw-help)
(provide 'idlwave-help)

;;; idlw-help.el ends here
