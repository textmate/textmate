;;; cc-styles.el --- support for styles in CC Mode

;; Copyright (C) 1985, 1987, 1992-2012  Free Software Foundation, Inc.

;; Authors:    2004- Alan Mackenzie
;;             1998- Martin Stjernholm
;;             1992-1999 Barry A. Warsaw
;;             1987 Dave Detlefs
;;             1987 Stewart Clamen
;;             1985 Richard M. Stallman
;; Maintainer: bug-cc-mode@gnu.org
;; Created:    22-Apr-1997 (split from cc-mode.el)
;; Keywords:   c languages
;; Package:    cc-mode

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

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-bytecomp" nil t)))

(cc-require 'cc-defs)
(cc-require 'cc-vars)
(cc-require 'cc-align)
;; cc-align is only indirectly required: Styles added with
;; `c-add-style' often contains references to functions defined there.

;; Silence the compiler.
(cc-bytecomp-defvar adaptive-fill-first-line-regexp) ; Emacs


(defvar c-style-alist
  '(("gnu"
     (c-basic-offset . 2)
     (c-comment-only-line-offset . (0 . 0))
     (c-hanging-braces-alist     . ((substatement-open before after)
				    (arglist-cont-nonempty)))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 5)
			 (substatement-open . +)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-case-open . +)
			 (statement-cont . +)
			 (arglist-intro . c-lineup-arglist-intro-after-paren)
			 (arglist-close . c-lineup-arglist)
			 (inline-open . 0)
			 (brace-list-open . +)
			 (topmost-intro-cont
			  . (first c-lineup-topmost-intro-cont
				   c-lineup-gnu-DEFUN-intro-cont))))
     (c-special-indent-hook . c-gnu-impose-minimum)
     (c-block-comment-prefix . ""))

    ("k&r"
     (c-basic-offset . 5)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . 0)
			 (substatement-open . 0)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-cont . +))))

    ("bsd"
     (c-basic-offset . 8)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro . +)
			 (substatement-open . 0)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-cont . +)
			 (inline-open . 0)
			 (inexpr-class . 0))))

    ("stroustrup"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-offsets-alist . ((statement-block-intro . +)
			 (substatement-open . 0)
			 (substatement-label . 0)
			 (label . 0)
			 (statement-cont . +))))

    ("whitesmith"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     ;; It's obvious that the CC Mode way of choosing anchor positions
     ;; doesn't fit this style at all. :P
     (c-offsets-alist . ((defun-open . +)
			 (defun-close . c-lineup-whitesmith-in-block)
			 (defun-block-intro . (add c-lineup-whitesmith-in-block
						   c-indent-multi-line-block))
			 (class-open . +)
			 (class-close . +)
			 (inline-open . +)
			 (inline-close . c-lineup-whitesmith-in-block)
			 (knr-argdecl-intro . +)
			 (block-open . 0) ; Get indentation from `statement' instead.
			 (block-close . c-lineup-whitesmith-in-block)
			 (brace-list-open . +)
			 (brace-list-close . c-lineup-whitesmith-in-block)
			 (brace-list-intro . (add c-lineup-whitesmith-in-block
						  c-indent-multi-line-block))
			 (brace-list-entry . (add c-lineup-after-whitesmith-blocks
						  c-indent-multi-line-block))
			 (brace-entry-open . (add c-lineup-after-whitesmith-blocks
						  c-indent-multi-line-block))
			 (statement . (add c-lineup-after-whitesmith-blocks
					   c-indent-multi-line-block))
			 (statement-block-intro . (add c-lineup-whitesmith-in-block
						       c-indent-multi-line-block))
			 (substatement-open . +)
			 (substatement-label . +)
			 (label . 0)
			 (arglist-intro . (add c-lineup-whitesmith-in-block
					       c-indent-multi-line-block))
			 (arglist-cont . (add c-lineup-after-whitesmith-blocks
					      c-indent-multi-line-block))
			 (arglist-cont-nonempty . (add c-lineup-whitesmith-in-block
						       c-indent-multi-line-block))
			 (arglist-close . c-lineup-whitesmith-in-block)
			 (inclass . c-lineup-whitesmith-in-block)
			 (extern-lang-open . +)
			 (namespace-open . +)
			 (module-open . +)
			 (composition-open . +)
			 (extern-lang-close . +)
			 (namespace-close . +)
			 (module-close . +)
			 (composition-close . +)
			 (inextern-lang . c-lineup-whitesmith-in-block)
			 (innamespace . c-lineup-whitesmith-in-block)
			 (inmodule . c-lineup-whitesmith-in-block)
			 (incomposition . c-lineup-whitesmith-in-block)
			 (inexpr-class . 0))))

    ("ellemtel"
     (c-basic-offset . 3)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist     . ((substatement-open before after)
				    (arglist-cont-nonempty)))
     (c-offsets-alist . ((topmost-intro        . 0)
			 (substatement         . +)
			 (substatement-open    . 0)
                         (case-label           . +)
                         (access-label         . -)
			 (inclass              . +)
			 (inline-open          . 0))))
    ("linux"
     (c-basic-offset  . 8)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)
				(arglist-cont-nonempty)))
     (c-cleanup-list . (brace-else-brace))
     (c-offsets-alist . ((statement-block-intro . +)
			 (knr-argdecl-intro     . 0)
			 (substatement-open     . 0)
			 (substatement-label    . 0)
			 (label                 . 0)
			 (statement-cont        . +))))

    ("python"
     (indent-tabs-mode . t)
     (fill-column      . 78)
     (c-basic-offset   . 8)
     (c-offsets-alist  . ((substatement-open . 0)
			  (inextern-lang . 0)
			  (arglist-intro . +)
			  (knr-argdecl-intro . +)))
     (c-hanging-braces-alist . ((brace-list-open)
				(brace-list-intro)
				(brace-list-close)
				(brace-entry-open)
				(substatement-open after)
				(block-close . c-snug-do-while)
				(arglist-cont-nonempty)))
     (c-block-comment-prefix . ""))

    ("java"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . (0 . 0))
     ;; the following preserves Javadoc starter lines
     (c-offsets-alist . ((inline-open . 0)
			 (topmost-intro-cont    . +)
			 (statement-block-intro . +)
 			 (knr-argdecl-intro     . 5)
			 (substatement-open     . +)
			 (substatement-label    . +)
 			 (label                 . +)
 			 (statement-case-open   . +)
 			 (statement-cont        . +)
 			 (arglist-intro  . c-lineup-arglist-intro-after-paren)
 			 (arglist-close  . c-lineup-arglist)
 			 (access-label   . 0)
			 (inher-cont     . c-lineup-java-inher)
			 (func-decl-cont . c-lineup-java-throws))))

    ;; awk style exists primarily for auto-newline settings.  Otherwise it's
    ;; pretty much like k&r.
    ("awk"
     (c-basic-offset . 4)
     (c-comment-only-line-offset . 0)
     (c-hanging-braces-alist . ((defun-open after)
				(defun-close . c-snug-1line-defun-close)
				(substatement-open after)
				(block-close . c-snug-do-while)
				(arglist-cont-nonempty)))
     (c-hanging-semi&comma-criteria . nil)
     (c-cleanup-list . nil)		; You might want one-liner-defun here.
     (c-offsets-alist . ((statement-block-intro . +)
			 (substatement-open . 0)
			 (statement-cont . +))))

    )
  "Styles of indentation.
Elements of this alist are of the form:

  (STYLE-STRING [BASE-STYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

where STYLE-STRING is a short descriptive string used to select a
style, VARIABLE is any Emacs variable, and VALUE is the intended value
for that variable when using the selected style.

Optional BASE-STYLE if present, is a string and must follow
STYLE-STRING.  BASE-STYLE names a style that this style inherits from.
By default, all styles inherit from the \"user\" style, which is
computed at run time.  Style loops generate errors.

Two variables are treated specially.  When VARIABLE is
`c-offsets-alist', the VALUE is a list containing elements of the
form:

  (SYNTACTIC-SYMBOL . OFFSET)

as described in `c-offsets-alist'.  These are passed directly to
`c-set-offset' so there is no need to set every syntactic symbol in
your style, only those that are different from the default.

When VARIABLE is `c-special-indent-hook', its VALUE is added to
`c-special-indent-hook' using `add-hook'.  If VALUE is a list, each
element of the list is added with `add-hook'.

Do not change this variable directly.  Use the function `c-add-style'
to add new styles or modify existing styles (it is not a good idea to
modify existing styles -- you should create a new style that inherits
the existing style).")


;; Functions that manipulate styles
(defun c-set-style-1 (conscell dont-override)
  ;; Set the style for one variable
  (let ((attr (car conscell))
	(val  (cdr conscell)))
    (cond
     ;; first special variable
     ((eq attr 'c-offsets-alist)
      (let ((offsets (cond ((eq dont-override t)
			    c-offsets-alist)
			   (dont-override
			    (default-value 'c-offsets-alist)))))
	(mapcar (lambda (langentry)
		  (let ((langelem (car langentry))
			(offset (cdr langentry)))
		    (unless (assq langelem offsets)
		      (c-set-offset langelem offset))))
		val)))
     ;; second special variable
     ((eq attr 'c-special-indent-hook)
      ;; Maybe we should ignore dont-override here and always add new
      ;; hooks?
      (unless (cond ((eq dont-override t)
		     c-special-indent-hook)
		    (dont-override
		     (default-value 'c-special-indent-hook)))
	(if (listp val)
	    (mapcar (lambda (func)
		      (add-hook 'c-special-indent-hook func t t))
		    val)
	  (add-hook 'c-special-indent-hook val t t))))
     ;; all other variables
     (t (when (or (not dont-override)
		  (not (memq attr c-style-variables))
		  (eq (if (eq dont-override t)
			  (symbol-value attr)
			(default-value attr))
		      'set-from-style))
	  (set attr val)
	  ;; Must update a number of other variables if
	  ;; c-comment-prefix-regexp is set.
	  (if (eq attr 'c-comment-prefix-regexp)
	      (c-setup-paragraph-variables)))))))

(defun c-get-style-variables (style basestyles)
  ;; Return all variables in a style by resolving inheritances.
  (if (not style)
      (copy-alist c-fallback-style)
    (let ((vars (cdr (or (assoc (downcase style) c-style-alist)
			 (assoc (upcase style) c-style-alist)
			 (assoc style c-style-alist)
			 (progn
			   (c-benign-error "Undefined style: %s" style)
			   nil)))))
      (let ((base (and (stringp (car-safe vars))
		       (prog1
			   (downcase (car vars))
			 (setq vars (cdr vars))))))
	(if (memq base basestyles)
	    (c-benign-error "Style loop detected: %s in %s" base basestyles)
	  (nconc (c-get-style-variables base (cons base basestyles))
		 (copy-alist vars)))))))

(defvar c-set-style-history nil)

;;;###autoload
(defun c-set-style (stylename &optional dont-override)
  "Set the current buffer to use the style STYLENAME.
STYLENAME, a string, must be an existing CC Mode style - These are contained
in the variable `c-style-alist'.

The variable `c-indentation-style' will get set to STYLENAME.

\"Setting the style\" is done by setting CC Mode's \"style variables\" to the
values indicated by the pertinent entry in `c-style-alist'.  Other variables
might get set too.

If DONT-OVERRIDE is neither nil nor t, style variables whose default values
have been set (more precisely, whose default values are not the symbol
`set-from-style') will not be changed.  This avoids overriding global settings
done in ~/.emacs.  It is useful to call c-set-style from a mode hook in this
way.

If DONT-OVERRIDE is t, style variables that already have values (i.e., whose
values are not the symbol `set-from-style') will not be overridden.  CC Mode
calls c-set-style internally in this way whilst initializing a buffer; if
cc-set-style is called like this from anywhere else, it will usually behave as
a null operation."
  (interactive
   (list (let ((completion-ignore-case t)
	       (prompt (format "Which %s indentation style? "
			       mode-name)))
	   (completing-read prompt c-style-alist nil t nil
			    'c-set-style-history
			    c-indentation-style))))
  (or c-buffer-is-cc-mode
      (error "Buffer %s is not a CC Mode buffer (c-set-style)" (buffer-name)))
  (or (stringp stylename)
      (error "Argument to c-set-style was not a string"))
  (c-initialize-builtin-style)
  (let ((vars (c-get-style-variables stylename nil)))
    (unless dont-override
      ;; Since we always add to c-special-indent-hook we must reset it
      ;; first, or else the hooks from the preceding style will
      ;; remain.  This is not necessary for c-offsets-alist, since
      ;; c-get-style-variables contains every valid offset type in the
      ;; fallback entry.
      (setq c-special-indent-hook
	    (default-value 'c-special-indent-hook)))
    (mapc (lambda (elem)
	    (c-set-style-1 elem dont-override))
	  ;; Need to go through the variables backwards when we
	  ;; don't override any settings.
	  (if (eq dont-override t) (nreverse vars) vars)))
  (setq c-indentation-style stylename)
  (c-keep-region-active))

;;;###autoload
(defun c-add-style (style description &optional set-p)
  "Adds a style to `c-style-alist', or updates an existing one.
STYLE is a string identifying the style to add or update.  DESCRIPTION
is an association list describing the style and must be of the form:

  ([BASESTYLE] (VARIABLE . VALUE) [(VARIABLE . VALUE) ...])

See the variable `c-style-alist' for the semantics of BASESTYLE,
VARIABLE and VALUE.  This function also sets the current style to
STYLE using `c-set-style' if the optional SET-P flag is non-nil."
  (interactive
   (let ((stylename (completing-read "Style to add: " c-style-alist
				     nil nil nil 'c-set-style-history))
         (descr (eval-minibuffer "Style description: ")))
     (list stylename descr
	   (y-or-n-p "Set the style too? "))))
  (setq style (downcase style))
  (let ((s (assoc style c-style-alist)))
    (if s
        (setcdr s (copy-alist description)) ; replace
      (setq c-style-alist (cons (cons style description) c-style-alist))))
  (and set-p (c-set-style style)))


(defvar c-read-offset-history nil)

(defun c-read-offset (langelem)
  ;; read new offset value for LANGELEM from minibuffer. return a
  ;; valid value only
  (let* ((oldoff  (cdr-safe (or (assq langelem c-offsets-alist)
				(assq langelem (get 'c-offsets-alist
						    'c-stylevar-fallback)))))
	 (symname (symbol-name langelem))
	 (defstr  (format "(default %s): " oldoff))
	 (errmsg  (concat "Offset must be int, func, var, vector, list, "
			  "or [+,-,++,--,*,/] "
			  defstr))
	 (prompt (concat symname " offset " defstr))
	 (keymap (make-sparse-keymap))
	 (minibuffer-completion-table obarray)
	 (minibuffer-completion-predicate 'fboundp)
	 offset input)
    ;; In principle completing-read is used here, but SPC is unbound
    ;; to make it less annoying to enter lists.
    (set-keymap-parent keymap minibuffer-local-completion-map)
    (define-key keymap " " 'self-insert-command)
    (while (not offset)
      (setq input (read-from-minibuffer prompt nil keymap t
					'c-read-offset-history
					(format "%s" oldoff)))
      (if (c-valid-offset input)
	  (setq offset input)
	;; error, but don't signal one, keep trying
	;; to read an input value
	(ding)
	(setq prompt errmsg)))
    offset))

;;;###autoload
(defun c-set-offset (symbol offset &optional ignored)
  "Change the value of a syntactic element symbol in `c-offsets-alist'.
SYMBOL is the syntactic element symbol to change and OFFSET is the new
offset for that syntactic element.  The optional argument is not used
and exists only for compatibility reasons."
  (interactive
   (let* ((langelem
	   (intern (completing-read
		    (concat "Syntactic symbol to change"
			    (if current-prefix-arg " or add" "")
			    ": ")
		    (mapcar
		     #'(lambda (langelem)
			 (cons (format "%s" (car langelem)) nil))
		     (get 'c-offsets-alist 'c-stylevar-fallback))
		    nil (not current-prefix-arg)
		    ;; initial contents tries to be the last element
		    ;; on the syntactic analysis list for the current
		    ;; line
		    (and c-buffer-is-cc-mode
			 (c-save-buffer-state
			     ((syntax (c-guess-basic-syntax))
			      (len (length syntax))
			      (ic (format "%s" (car (nth (1- len) syntax)))))
			   (cons ic 0)))
		    )))
	  (offset (c-read-offset langelem)))
     (list langelem offset current-prefix-arg)))
  ;; sanity check offset
  (if (c-valid-offset offset)
      (let ((entry (assq symbol c-offsets-alist)))
	(if entry
	    (setcdr entry offset)
	  (if (assq symbol (get 'c-offsets-alist 'c-stylevar-fallback))
	      (setq c-offsets-alist (cons (cons symbol offset)
					  c-offsets-alist))
	    (c-benign-error "%s is not a valid syntactic symbol" symbol))))
    (c-benign-error "Invalid indentation setting for symbol %s: %S"
		    symbol offset))
  (c-keep-region-active))


(defun c-setup-paragraph-variables ()
  "Fix things up for paragraph recognition and filling inside comments and
strings by incorporating the values of `c-comment-prefix-regexp',
`sentence-end', `paragraph-start' and `paragraph-separate' in the relevant
variables."

  (interactive)
  (or c-buffer-is-cc-mode
      (error "Buffer %s is not a CC Mode buffer (c-setup-paragraph-variables)"
	     (buffer-name)))
  ;; Set up the values for use in comments.
  (setq c-current-comment-prefix
	(if (listp c-comment-prefix-regexp)
	    (cdr-safe (or (assoc major-mode c-comment-prefix-regexp)
			  (assoc 'other c-comment-prefix-regexp)))
	  c-comment-prefix-regexp))

  (let* ((empty-is-prefix (string-match c-current-comment-prefix ""))
	 (nonws-comment-line-prefix
	  (concat "\\(" c-current-comment-prefix "\\)[ \t]*"))
	 (comment-line-prefix (concat "[ \t]*" nonws-comment-line-prefix))
	 (blank-or-comment-line-prefix
	  (concat "[ \t]*"
		  (if empty-is-prefix "" "\\(")
		  nonws-comment-line-prefix
		  (if empty-is-prefix "" "\\)?"))))

    (setq paragraph-start (concat blank-or-comment-line-prefix
				  c-paragraph-start
				  "\\|"
				  page-delimiter)
	  paragraph-separate (concat blank-or-comment-line-prefix
				     c-paragraph-separate
				     "\\|"
				     page-delimiter)
	  paragraph-ignore-fill-prefix t
	  adaptive-fill-mode t
	  adaptive-fill-regexp
	  (concat comment-line-prefix
		  (if (default-value 'adaptive-fill-regexp)
		      (concat "\\("
			      (default-value 'adaptive-fill-regexp)
			      "\\)")
		    "")))

    (when (boundp 'adaptive-fill-first-line-regexp)
      ;; XEmacs adaptive fill mode doesn't have this.
      (set (make-local-variable 'adaptive-fill-first-line-regexp)
           (concat "\\`" comment-line-prefix
                   ;; Maybe we should incorporate the old value here,
                   ;; but then we have to do all sorts of kludges to
                   ;; deal with the \` and \' it probably contains.
                   "\\'"))))

  ;; Set up the values for use in strings.  These are the default
  ;; paragraph-start/separate values, enhanced to accept escaped EOLs as
  ;; whitespace.  Used in c-beginning/end-of-sentence-in-string in cc-cmds.
  (setq c-string-par-start
	;;(concat "\\(" (default-value 'paragraph-start) "\\)\\|[ \t]*\\\\$"))
	"\f\\|[ \t]*\\\\?$")
  (setq c-string-par-separate
	;;(concat "\\(" (default-value 'paragraph-separate) "\\)\\|[ \t]*\\\\$"))
	"[ \t\f]*\\\\?$")
  (setq c-sentence-end-with-esc-eol
	(concat "\\(\\(" (c-default-value-sentence-end) "\\)"
		;; N.B.:  "$" would be invalid when not enclosed like "\\($\\)".
		"\\|" "[.?!][]\"')}]* ?\\\\\\($\\)[ \t\n]*"
		"\\)")))


;; Helper for setting up Filladapt mode.  It's not used by CC Mode itself.

(cc-bytecomp-defvar filladapt-token-table)
(cc-bytecomp-defvar filladapt-token-match-table)
(cc-bytecomp-defvar filladapt-token-conversion-table)

(defun c-setup-filladapt ()
  "Convenience function to configure Kyle E. Jones' Filladapt mode for
CC Mode by making sure the proper entries are present on
`filladapt-token-table', `filladapt-token-match-table', and
`filladapt-token-conversion-table'.  This is intended to be used on
`c-mode-common-hook' or similar."
  ;; This function is intended to be used explicitly by the end user
  ;; only.

  ;; The default configuration already handles C++ comments, but we
  ;; need to add handling of C block comments.  A new filladapt token
  ;; `c-comment' is added for that.
  (let (p)
    (setq p filladapt-token-table)
    (while (and p (not (eq (car-safe (cdr-safe (car-safe p))) 'c-comment)))
      (setq p (cdr-safe p)))
    (if p
	(setcar (car p) c-current-comment-prefix)
      (setq filladapt-token-table
	    (append (list (car filladapt-token-table)
			  (list c-current-comment-prefix 'c-comment))
		    (cdr filladapt-token-table)))))
  (unless (assq 'c-comment filladapt-token-match-table)
    (setq filladapt-token-match-table
	  (append '((c-comment c-comment))
		  filladapt-token-match-table)))
  (unless (assq 'c-comment filladapt-token-conversion-table)
    (setq filladapt-token-conversion-table
	  (append '((c-comment . exact))
		  filladapt-token-conversion-table))))


(defun c-initialize-builtin-style ()
  ;; Dynamically append the default value of most variables. This is
  ;; crucial because future c-set-style calls will always reset the
  ;; variables first to the `cc-mode' style before instituting the new
  ;; style.  Only do this once!
  (unless (get 'c-initialize-builtin-style 'is-run)
    (put 'c-initialize-builtin-style 'is-run t)
    ;;(c-initialize-cc-mode)
    (unless (assoc "user" c-style-alist)
      (let ((vars c-style-variables) var val uservars)
	(while vars
	  (setq var (car vars)
		val (symbol-value var)
		vars (cdr vars))
	  (cond ((eq var 'c-offsets-alist)
		 (or (null val)
		     (setq uservars (cons (cons 'c-offsets-alist val)
					  uservars))))
		((not (eq val 'set-from-style))
		 (setq uservars (cons (cons var val)
				      uservars)))))
	(c-add-style "user" uservars)))
    (unless (assoc "cc-mode" c-style-alist)
      (c-add-style "cc-mode" '("user")))
    (if c-style-variables-are-local-p
	(c-make-styles-buffer-local))))

(defun c-make-styles-buffer-local (&optional this-buf-only-p)
  "Make all CC Mode style variables buffer local.
If `this-buf-only-p' is non-nil, the style variables will be made
buffer local only in the current buffer.  Otherwise they'll be made
permanently buffer local in any buffer that changes their values.

The buffer localness of the style variables are normally controlled
with the variable `c-style-variables-are-local-p', so there's seldom
any reason to call this function directly."

  ;; style variables
  (let ((func (if this-buf-only-p
		  'make-local-variable
		'make-variable-buffer-local))
	(varsyms (cons 'c-indentation-style (copy-alist c-style-variables))))
    (delq 'c-special-indent-hook varsyms)
    (mapc func varsyms)
    ;; Hooks must be handled specially
    (if this-buf-only-p
	(if (featurep 'xemacs) (make-local-hook 'c-special-indent-hook))
      (with-no-warnings (make-variable-buffer-local 'c-special-indent-hook))
      (setq c-style-variables-are-local-p t))
    ))

(defun cc-choose-style-for-mode (mode default-style)
  "Return suitable style for MODE from DEFAULT-STYLE.
DEFAULT-STYLE has the same format as `c-default-style'."
  (if (stringp default-style)
      default-style
    (or (cdr (assq mode default-style))
	(cdr (assq 'other default-style))
	"gnu")))



(cc-provide 'cc-styles)

;;; cc-styles.el ends here
