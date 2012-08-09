;;; help-mode.el --- `help-mode' used by *Help* buffers

;; Copyright (C) 1985-1986, 1993-1994, 1998-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help, internal
;; Package: emacs

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

;; Defines `help-mode', which is the mode used by *Help* buffers, and
;; associated support machinery, such as adding hyperlinks, etc.,

;;; Code:

(require 'button)
(require 'view)
(eval-when-compile (require 'easymenu))

(defvar help-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
    (define-key map [mouse-2] 'help-follow-mouse)
    (define-key map "\C-c\C-b" 'help-go-back)
    (define-key map "\C-c\C-f" 'help-go-forward)
    (define-key map "\C-c\C-c" 'help-follow-symbol)
    (define-key map "\r" 'help-follow)
    map)
  "Keymap for help mode.")

(easy-menu-define help-mode-menu help-mode-map
  "Menu for Help Mode."
  '("Help-Mode"
    ["Show Help for Symbol" help-follow-symbol
     :help "Show the docs for the symbol at point"]
    ["Previous Topic" help-go-back
     :help "Go back to previous topic in this help buffer"]
    ["Next Topic" help-go-forward
     :help "Go back to next topic in this help buffer"]
    ["Move to Previous Button" backward-button
     :help "Move to the Next Button in the help buffer"]
    ["Move to Next Button" forward-button
      :help "Move to the Next Button in the help buffer"]))

(defvar help-xref-stack nil
  "A stack of ways by which to return to help buffers after following xrefs.
Used by `help-follow' and `help-xref-go-back'.
An element looks like (POSITION FUNCTION ARGS...).
To use the element, do (apply FUNCTION ARGS) then goto the point.")
(put 'help-xref-stack 'permanent-local t)
(make-variable-buffer-local 'help-xref-stack)

(defvar help-xref-forward-stack nil
  "A stack used to navigate help forwards after using the back button.
Used by `help-follow' and `help-xref-go-forward'.
An element looks like (POSITION FUNCTION ARGS...).
To use the element, do (apply FUNCTION ARGS) then goto the point.")
(put 'help-xref-forward-stack 'permanent-local t)
(make-variable-buffer-local 'help-xref-forward-stack)

(defvar help-xref-stack-item nil
  "An item for `help-follow' in this buffer to push onto `help-xref-stack'.
The format is (FUNCTION ARGS...).")
(put 'help-xref-stack-item 'permanent-local t)
(make-variable-buffer-local 'help-xref-stack-item)

(defvar help-xref-stack-forward-item nil
  "An item for `help-go-back' to push onto `help-xref-forward-stack'.
The format is (FUNCTION ARGS...).")
(put 'help-xref-stack-forward-item 'permanent-local t)
(make-variable-buffer-local 'help-xref-stack-forward-item)

(setq-default help-xref-stack nil help-xref-stack-item nil)
(setq-default help-xref-forward-stack nil help-xref-forward-stack-item nil)

(defcustom help-mode-hook nil
  "Hook run by `help-mode'."
  :type 'hook
  :group 'help)

;; Button types used by help

(define-button-type 'help-xref
  'follow-link t
  'action #'help-button-action)

(defun help-button-action (button)
  "Call BUTTON's help function."
  (help-do-xref (button-start button)
		(button-get button 'help-function)
		(button-get button 'help-args)))

;; These 6 calls to define-button-type were generated in a dolist
;; loop, but that is bad because it means these button types don't
;; have an easily found definition.

(define-button-type 'help-function
  :supertype 'help-xref
  'help-function 'describe-function
  'help-echo (purecopy "mouse-2, RET: describe this function"))

(define-button-type 'help-variable
  :supertype 'help-xref
  'help-function 'describe-variable
  'help-echo (purecopy "mouse-2, RET: describe this variable"))

(define-button-type 'help-face
  :supertype 'help-xref
  'help-function 'describe-face
  'help-echo (purecopy "mouse-2, RET: describe this face"))

(define-button-type 'help-coding-system
  :supertype 'help-xref
  'help-function 'describe-coding-system
  'help-echo (purecopy "mouse-2, RET: describe this coding system"))

(define-button-type 'help-input-method
  :supertype 'help-xref
  'help-function 'describe-input-method
  'help-echo (purecopy "mouse-2, RET: describe this input method"))

(define-button-type 'help-character-set
  :supertype 'help-xref
  'help-function 'describe-character-set
  'help-echo (purecopy "mouse-2, RET: describe this character set"))

;; Make some more idiosyncratic button types.

(define-button-type 'help-symbol
  :supertype 'help-xref
  'help-function #'help-xref-interned
  'help-echo (purecopy "mouse-2, RET: describe this symbol"))

(define-button-type 'help-back
  :supertype 'help-xref
  'help-function #'help-xref-go-back
  'help-echo (purecopy "mouse-2, RET: go back to previous help buffer"))

(define-button-type 'help-forward
  :supertype 'help-xref
  'help-function #'help-xref-go-forward
  'help-echo (purecopy "mouse-2, RET: move forward to next help buffer"))

(define-button-type 'help-info-variable
  :supertype 'help-xref
  ;; the name of the variable is put before the argument to Info
  'help-function (lambda (_a v) (info v))
  'help-echo (purecopy "mouse-2, RET: read this Info node"))

(define-button-type 'help-info
  :supertype 'help-xref
  'help-function #'info
  'help-echo (purecopy "mouse-2, RET: read this Info node"))

(define-button-type 'help-url
  :supertype 'help-xref
  'help-function #'browse-url
  'help-echo (purecopy "mouse-2, RET: view this URL in a browser"))

(define-button-type 'help-customize-variable
  :supertype 'help-xref
  'help-function (lambda (v)
		   (customize-variable v))
  'help-echo (purecopy "mouse-2, RET: customize variable"))

(define-button-type 'help-customize-face
  :supertype 'help-xref
  'help-function (lambda (v)
		   (customize-face v))
  'help-echo (purecopy "mouse-2, RET: customize face"))

(define-button-type 'help-function-def
  :supertype 'help-xref
  'help-function (lambda (fun file)
		   (require 'find-func)
		   (when (eq file 'C-source)
		     (setq file
			   (help-C-file-name (indirect-function fun) 'fun)))
		   ;; Don't use find-function-noselect because it follows
		   ;; aliases (which fails for built-in functions).
		   (let ((location
			  (find-function-search-for-symbol fun nil file)))
		     (pop-to-buffer (car location))
		     (if (cdr location)
			 (goto-char (cdr location))
		       (message "Unable to find location in file"))))
  'help-echo (purecopy "mouse-2, RET: find function's definition"))

(define-button-type 'help-function-cmacro
  :supertype 'help-xref
  'help-function (lambda (fun file)
		   (setq file (locate-library file t))
		   (if (and file (file-readable-p file))
		       (progn
			 (pop-to-buffer (find-file-noselect file))
			 (goto-char (point-min))
			 (if (re-search-forward
			      (format "^[ \t]*(define-compiler-macro[ \t]+%s"
				      (regexp-quote (symbol-name fun))) nil t)
			     (forward-line 0)
			   (message "Unable to find location in file")))
		     (message "Unable to find file")))
  'help-echo (purecopy "mouse-2, RET: find function's compiler macro"))

(define-button-type 'help-variable-def
  :supertype 'help-xref
  'help-function (lambda (var &optional file)
		   (when (eq file 'C-source)
		     (setq file (help-C-file-name var 'var)))
		   (let ((location (find-variable-noselect var file)))
		     (pop-to-buffer (car location))
		     (if (cdr location)
		       (goto-char (cdr location))
		       (message "Unable to find location in file"))))
  'help-echo (purecopy "mouse-2, RET: find variable's definition"))

(define-button-type 'help-face-def
  :supertype 'help-xref
  'help-function (lambda (fun file)
		   (require 'find-func)
		   ;; Don't use find-function-noselect because it follows
		   ;; aliases (which fails for built-in functions).
		   (let ((location
			  (find-function-search-for-symbol fun 'defface file)))
		     (pop-to-buffer (car location))
		     (if (cdr location)
			 (goto-char (cdr location))
		       (message "Unable to find location in file"))))
  'help-echo (purecopy "mouse-2, RET: find face's definition"))

(define-button-type 'help-package
  :supertype 'help-xref
  'help-function 'describe-package
  'help-echo (purecopy "mouse-2, RET: Describe package"))

(define-button-type 'help-package-def
  :supertype 'help-xref
  'help-function (lambda (file) (dired file))
  'help-echo (purecopy "mouse-2, RET: visit package directory"))

(define-button-type 'help-theme-def
  :supertype 'help-xref
  'help-function 'find-file
  'help-echo (purecopy "mouse-2, RET: visit theme file"))

(define-button-type 'help-theme-edit
  :supertype 'help-xref
  'help-function 'customize-create-theme
  'help-echo (purecopy "mouse-2, RET: edit this theme file"))

;;;###autoload
(define-derived-mode help-mode special-mode "Help"
  "Major mode for viewing help text and navigating references in it.
Entry to this mode runs the normal hook `help-mode-hook'.
Commands:
\\{help-mode-map}"
  (set (make-local-variable 'revert-buffer-function)
       'help-mode-revert-buffer))

;;;###autoload
(defun help-mode-setup ()
  (help-mode)
  (setq buffer-read-only nil))

;;;###autoload
(defun help-mode-finish ()
  (when (eq major-mode 'help-mode)
    ;; View mode's read-only status of existing *Help* buffer is lost
    ;; by with-output-to-temp-buffer.
    (toggle-read-only 1)

    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-read-only t))
	(when (re-search-forward "^This [^[:space:]]+ is advised.$" nil t)
	  (put-text-property (match-beginning 0)
			     (match-end 0)
			     'face 'font-lock-warning-face))))

    (help-make-xrefs (current-buffer))))

;; Grokking cross-reference information in doc strings and
;; hyperlinking it.

;; This may have some scope for extension and the same or something
;; similar should be done for widget doc strings, which currently use
;; another mechanism.

(defvar help-back-label (purecopy "[back]")
  "Label to use by `help-make-xrefs' for the go-back reference.")

(defvar help-forward-label (purecopy "[forward]")
  "Label to use by `help-make-xrefs' for the go-forward reference.")

(defconst help-xref-symbol-regexp
  (purecopy (concat "\\(\\<\\(\\(variable\\|option\\)\\|"  ; Link to var
 		    "\\(function\\|command\\)\\|"          ; Link to function
 		    "\\(face\\)\\|"			   ; Link to face
 		    "\\(symbol\\|program\\|property\\)\\|" ; Don't link
		    "\\(source \\(?:code \\)?\\(?:of\\|for\\)\\)\\)"
		    "[ \t\n]+\\)?"
		    ;; Note starting with word-syntax character:
		    "`\\(\\sw\\(\\sw\\|\\s_\\)+\\)'"))
  "Regexp matching doc string references to symbols.

The words preceding the quoted symbol can be used in doc strings to
distinguish references to variables, functions and symbols.")

(defvar help-xref-mule-regexp nil
  "Regexp matching doc string references to MULE-related keywords.

It is usually nil, and is temporarily bound to an appropriate regexp
when help commands related to multilingual environment (e.g.,
`describe-coding-system') are invoked.")


(defconst help-xref-info-regexp
  (purecopy "\\<[Ii]nfo[ \t\n]+\\(node\\|anchor\\)[ \t\n]+`\\([^']+\\)'")
  "Regexp matching doc string references to an Info node.")

(defconst help-xref-url-regexp
  (purecopy "\\<[Uu][Rr][Ll][ \t\n]+`\\([^']+\\)'")
  "Regexp matching doc string references to a URL.")

;;;###autoload
(defun help-setup-xref (item interactive-p)
  "Invoked from commands using the \"*Help*\" buffer to install some xref info.

ITEM is a (FUNCTION . ARGS) pair appropriate for recreating the help
buffer after following a reference.  INTERACTIVE-P is non-nil if the
calling command was invoked interactively.  In this case the stack of
items for help buffer \"back\" buttons is cleared.

This should be called very early, before the output buffer is cleared,
because we want to record the \"previous\" position of point so we can
restore it properly when going back."
  (with-current-buffer (help-buffer)
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when interactive-p
      (let ((tail (nthcdr 10 help-xref-stack)))
	;; Truncate the stack.
	(if tail (setcdr tail nil))))
    (setq help-xref-stack-item item)))

(defvar help-xref-following nil
  "Non-nil when following a help cross-reference.")

;;;###autoload
(defun help-buffer ()
  "Return the name of a buffer for inserting help.
If `help-xref-following' is non-nil, this is the name of the
current buffer.  Signal an error if this buffer is not derived
from `help-mode'.
Otherwise, return \"*Help*\", creating a buffer with that name if
it does not already exist."
  (buffer-name				;for with-output-to-temp-buffer
   (if (not help-xref-following)
       (get-buffer-create "*Help*")
     (unless (derived-mode-p 'help-mode)
       (error "Current buffer is not in Help mode"))
     (current-buffer))))

;;;###autoload
(defun help-make-xrefs (&optional buffer)
  "Parse and hyperlink documentation cross-references in the given BUFFER.

Find cross-reference information in a buffer and activate such cross
references for selection with `help-follow'.  Cross-references have
the canonical form `...'  and the type of reference may be
disambiguated by the preceding word(s) used in
`help-xref-symbol-regexp'.  Faces only get cross-referenced if
preceded or followed by the word `face'.  Variables without
variable documentation do not get cross-referenced, unless
preceded by the word `variable' or `option'.

If the variable `help-xref-mule-regexp' is non-nil, find also
cross-reference information related to multilingual environment
\(e.g., coding-systems).  This variable is also used to disambiguate
the type of reference as the same way as `help-xref-symbol-regexp'.

A special reference `back' is made to return back through a stack of
help buffers.  Variable `help-back-label' specifies the text for
that."
  (interactive "b")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      ;; Skip the header-type info, though it might be useful to parse
      ;; it at some stage (e.g. "function in `library'").
      (forward-paragraph)
      (let ((old-modified (buffer-modified-p)))
        (let ((stab (syntax-table))
              (case-fold-search t)
              (inhibit-read-only t))
          (set-syntax-table emacs-lisp-mode-syntax-table)
          ;; The following should probably be abstracted out.
          (unwind-protect
              (progn
                ;; Info references
                (save-excursion
                  (while (re-search-forward help-xref-info-regexp nil t)
                    (let ((data (match-string 2)))
                      (save-match-data
                        (unless (string-match "^([^)]+)" data)
                          (setq data (concat "(emacs)" data)))
			(setq data ;; possible newlines if para filled
			      (replace-regexp-in-string "[ \t\n]+" " " data t t)))
                      (help-xref-button 2 'help-info data))))
                ;; URLs
                (save-excursion
                  (while (re-search-forward help-xref-url-regexp nil t)
                    (let ((data (match-string 1)))
                      (help-xref-button 1 'help-url data))))
                ;; Mule related keywords.  Do this before trying
                ;; `help-xref-symbol-regexp' because some of Mule
                ;; keywords have variable or function definitions.
                (if help-xref-mule-regexp
                    (save-excursion
                      (while (re-search-forward help-xref-mule-regexp nil t)
                        (let* ((data (match-string 7))
                               (sym (intern-soft data)))
                          (cond
                           ((match-string 3) ; coding system
                            (and sym (coding-system-p sym)
                                 (help-xref-button 6 'help-coding-system sym)))
                           ((match-string 4) ; input method
                            (and (assoc data input-method-alist)
                                 (help-xref-button 7 'help-input-method data)))
                           ((or (match-string 5) (match-string 6)) ; charset
                            (and sym (charsetp sym)
                                 (help-xref-button 7 'help-character-set sym)))
                           ((assoc data input-method-alist)
                            (help-xref-button 7 'help-character-set data))
                           ((and sym (coding-system-p sym))
                            (help-xref-button 7 'help-coding-system sym))
                           ((and sym (charsetp sym))
                            (help-xref-button 7 'help-character-set sym)))))))
                ;; Quoted symbols
                (save-excursion
                  (while (re-search-forward help-xref-symbol-regexp nil t)
                    (let* ((data (match-string 8))
                           (sym (intern-soft data)))
                      (if sym
                          (cond
                           ((match-string 3) ; `variable' &c
                            (and (or (boundp sym) ; `variable' doesn't ensure
                                        ; it's actually bound
                                     (get sym 'variable-documentation))
                                 (help-xref-button 8 'help-variable sym)))
                           ((match-string 4) ; `function' &c
                            (and (fboundp sym) ; similarly
                                 (help-xref-button 8 'help-function sym)))
                           ((match-string 5) ; `face'
                            (and (facep sym)
                                 (help-xref-button 8 'help-face sym)))
                           ((match-string 6)) ; nothing for `symbol'
                           ((match-string 7)
                            ;; this used:
                            ;; #'(lambda (arg)
                            ;;     (let ((location
                            ;;            (find-function-noselect arg)))
                            ;;       (pop-to-buffer (car location))
                            ;; 	(goto-char (cdr location))))
                            (help-xref-button 8 'help-function-def sym))
                           ((and
                             (facep sym)
                             (save-match-data (looking-at "[ \t\n]+face\\W")))
                            (help-xref-button 8 'help-face sym))
                           ((and (or (boundp sym)
                                     (get sym 'variable-documentation))
                                 (fboundp sym))
                            ;; We can't intuit whether to use the
                            ;; variable or function doc -- supply both.
                            (help-xref-button 8 'help-symbol sym))
                           ((and
                             (or (boundp sym)
                                 (get sym 'variable-documentation))
                             (or
                              (documentation-property
                               sym 'variable-documentation)
                              (condition-case nil
                                  (documentation-property
                                   (indirect-variable sym)
                                   'variable-documentation)
                                (cyclic-variable-indirection nil))))
                            (help-xref-button 8 'help-variable sym))
                           ((fboundp sym)
                            (help-xref-button 8 'help-function sym)))))))
                ;; An obvious case of a key substitution:
                (save-excursion
                  (while (re-search-forward
                          ;; Assume command name is only word and symbol
                          ;; characters to get things like `use M-x foo->bar'.
                          ;; Command required to end with word constituent
                          ;; to avoid `.' at end of a sentence.
                          "\\<M-x\\s-+\\(\\sw\\(\\sw\\|\\s_\\)*\\sw\\)" nil t)
                    (let ((sym (intern-soft (match-string 1))))
                      (if (fboundp sym)
                          (help-xref-button 1 'help-function sym)))))
                ;; Look for commands in whole keymap substitutions:
                (save-excursion
                  ;; Make sure to find the first keymap.
                  (goto-char (point-min))
                  ;; Find a header and the column at which the command
                  ;; name will be found.

                  ;; If the keymap substitution isn't the last thing in
                  ;; the doc string, and if there is anything on the same
                  ;; line after it, this code won't recognize the end of it.
                  (while (re-search-forward "^key +binding\n\\(-+ +\\)-+\n\n"
                                            nil t)
                    (let ((col (- (match-end 1) (match-beginning 1))))
                      (while
                          (and (not (eobp))
                               ;; Stop at a pair of blank lines.
                               (not (looking-at "\n\\s-*\n")))
                        ;; Skip a single blank line.
                        (and (eolp) (forward-line))
                        (end-of-line)
                        (skip-chars-backward "^ \t\n")
                        (if (and (>= (current-column) col)
                                 (looking-at "\\(\\sw\\|\\s_\\)+$"))
                            (let ((sym (intern-soft (match-string 0))))
                              (if (fboundp sym)
                                  (help-xref-button 0 'help-function sym))))
                        (forward-line))))))
            (set-syntax-table stab))
          ;; Delete extraneous newlines at the end of the docstring
          (goto-char (point-max))
          (while (and (not (bobp)) (bolp))
            (delete-char -1))
          (insert "\n")
          (when (or help-xref-stack help-xref-forward-stack)
            (insert "\n"))
          ;; Make a back-reference in this buffer if appropriate.
          (when help-xref-stack
            (help-insert-xref-button help-back-label 'help-back
                                     (current-buffer)))
          ;; Make a forward-reference in this buffer if appropriate.
          (when help-xref-forward-stack
            (when help-xref-stack
              (insert "\t"))
            (help-insert-xref-button help-forward-label 'help-forward
                                     (current-buffer)))
          (when (or help-xref-stack help-xref-forward-stack)
            (insert "\n")))
        (set-buffer-modified-p old-modified)))))

;;;###autoload
(defun help-xref-button (match-number type &rest args)
  "Make a hyperlink for cross-reference text previously matched.
MATCH-NUMBER is the subexpression of interest in the last matched
regexp.  TYPE is the type of button to use.  Any remaining arguments are
passed to the button's help-function when it is invoked.
See `help-make-xrefs'."
  ;; Don't mung properties we've added specially in some instances.
  (unless (button-at (match-beginning match-number))
    (make-text-button (match-beginning match-number)
		      (match-end match-number)
		      'type type 'help-args args)))

;;;###autoload
(defun help-insert-xref-button (string type &rest args)
  "Insert STRING and make a hyperlink from cross-reference text on it.
TYPE is the type of button to use.  Any remaining arguments are passed
to the button's help-function when it is invoked.
See `help-make-xrefs'."
  (unless (button-at (point))
    (insert-text-button string 'type type 'help-args args)))

;;;###autoload
(defun help-xref-on-pp (from to)
  "Add xrefs for symbols in `pp's output between FROM and TO."
  (if (> (- to from) 5000) nil
    (with-syntax-table emacs-lisp-mode-syntax-table
      (save-excursion
	(save-restriction
	  (narrow-to-region from to)
	  (goto-char (point-min))
	  (condition-case nil
	      (while (not (eobp))
		(cond
		 ((looking-at "\"") (forward-sexp 1))
		 ((looking-at "#<") (search-forward ">" nil 'move))
		 ((looking-at "\\(\\(\\sw\\|\\s_\\)+\\)")
		  (let* ((sym (intern-soft (match-string 1)))
			 (type (cond ((fboundp sym) 'help-function)
				     ((or (memq sym '(t nil))
					  (keywordp sym))
				      nil)
				     ((and sym
					   (or (boundp sym)
					       (get sym
						    'variable-documentation)))
				      'help-variable))))
		    (when type (help-xref-button 1 type sym)))
		  (goto-char (match-end 1)))
		 (t (forward-char 1))))
	    (error nil)))))))


;; Additional functions for (re-)creating types of help buffers.
(defun help-xref-interned (symbol)
  "Follow a hyperlink which appeared to be an arbitrary interned SYMBOL.
Both variable, function and face documentation are extracted into a single
help buffer."
  (with-current-buffer (help-buffer)
    ;; Push the previous item on the stack before clobbering the output buffer.
    (help-setup-xref nil nil)
    (let ((facedoc (when (facep symbol)
		     ;; Don't record the current entry in the stack.
		     (setq help-xref-stack-item nil)
		     (describe-face symbol)))
	  (fdoc (when (fboundp symbol)
		  ;; Don't record the current entry in the stack.
		  (setq help-xref-stack-item nil)
		  (describe-function symbol)))
	  (sdoc (when (or (boundp symbol)
			  (get symbol 'variable-documentation))
		  ;; Don't record the current entry in the stack.
		  (setq help-xref-stack-item nil)
		  (describe-variable symbol))))
      (cond
       (sdoc
	;; We now have a help buffer on the variable.
	;; Insert the function and face text before it.
	(when (or fdoc facedoc)
	  (goto-char (point-min))
	  (let ((inhibit-read-only t))
	    (when fdoc
	      (insert fdoc "\n\n")
	      (when facedoc
		(insert (make-string 30 ?-) "\n\n" (symbol-name symbol)
			" is also a " "face." "\n\n")))
	    (when facedoc
	      (insert facedoc "\n\n"))
	    (insert (make-string 30 ?-) "\n\n" (symbol-name symbol)
		    " is also a " "variable." "\n\n"))
	  ;; Don't record the `describe-variable' item in the stack.
	  (setq help-xref-stack-item nil)
	  (help-setup-xref (list #'help-xref-interned symbol) nil)))
       (fdoc
	;; We now have a help buffer on the function.
	;; Insert face text before it.
	(when facedoc
	  (goto-char (point-max))
	  (let ((inhibit-read-only t))
	    (insert "\n\n" (make-string 30 ?-) "\n\n" (symbol-name symbol)
		    " is also a " "face." "\n\n" facedoc))
	  ;; Don't record the `describe-function' item in the stack.
	  (setq help-xref-stack-item nil)
	  (help-setup-xref (list #'help-xref-interned symbol) nil)))))))


;; Navigation/hyperlinking with xrefs

(defun help-xref-go-back (buffer)
  "From BUFFER, go back to previous help buffer text using `help-xref-stack'."
  (let (item position method args)
    (with-current-buffer buffer
      (push (cons (point) help-xref-stack-item) help-xref-forward-stack)
      (when help-xref-stack
	(setq item (pop help-xref-stack)
	      ;; Clear the current item so that it won't get pushed
	      ;; by the function we're about to call.  TODO: We could also
	      ;; push it onto a "forward" stack and add a `forw' button.
	      help-xref-stack-item nil
	      position (car item)
	      method (cadr item)
	      args (cddr item))))
    (apply method args)
    (with-current-buffer buffer
      (if (get-buffer-window buffer)
	  (set-window-point (get-buffer-window buffer) position)
	(goto-char position)))))

(defun help-xref-go-forward (buffer)
  "From BUFFER, go forward to next help buffer."
  (let (item position method args)
    (with-current-buffer buffer
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (when help-xref-forward-stack
	(setq item (pop help-xref-forward-stack)
	      ;; Clear the current item so that it won't get pushed
	      ;; by the function we're about to call.  TODO: We could also
	      ;; push it onto a "forward" stack and add a `forw' button.
	      help-xref-stack-item nil
	      position (car item)
	      method (cadr item)
	      args (cddr item))))
    (apply method args)
    (with-current-buffer buffer
      (if (get-buffer-window buffer)
	  (set-window-point (get-buffer-window buffer) position)
	(goto-char position)))))

(defun help-go-back ()
  "Go back to previous topic in this help buffer."
  (interactive)
  (if help-xref-stack
      (help-xref-go-back (current-buffer))
    (error "No previous help buffer")))

(defun help-go-forward ()
  "Go back to next topic in this help buffer."
  (interactive)
  (if help-xref-forward-stack
      (help-xref-go-forward (current-buffer))
    (error "No next help buffer")))

(defun help-do-xref (_pos function args)
  "Call the help cross-reference function FUNCTION with args ARGS.
Things are set up properly so that the resulting help-buffer has
a proper [back] button."
  ;; There is a reference at point.  Follow it.
  (let ((help-xref-following t))
    (apply function args)))

;; The doc string is meant to explain what buttons do.
(defun help-follow-mouse ()
  "Follow the cross-reference that you click on."
  (interactive)
  (error "No cross-reference here"))

;; The doc string is meant to explain what buttons do.
(defun help-follow ()
  "Follow cross-reference at point.

For the cross-reference format, see `help-make-xrefs'."
  (interactive)
  (error "No cross-reference here"))

(defun help-follow-symbol (&optional pos)
  "In help buffer, show docs for symbol at POS, defaulting to point.
Show all docs for that symbol as either a variable, function or face."
  (interactive "d")
  (unless pos
    (setq pos (point)))
  ;; check if the symbol under point is a function, variable or face
  (let ((sym
	 (intern
	  (save-excursion
	    (goto-char pos) (skip-syntax-backward "w_")
	    (buffer-substring (point)
			      (progn (skip-syntax-forward "w_")
				     (point)))))))
    (when (or (boundp sym)
	      (get sym 'variable-documentation)
	      (fboundp sym) (facep sym))
      (help-do-xref pos #'help-xref-interned (list sym)))))

(defun help-mode-revert-buffer (_ignore-auto noconfirm)
  (when (or noconfirm (yes-or-no-p "Revert help buffer? "))
    (let ((pos (point))
	  (item help-xref-stack-item)
	  ;; Pretend there is no current item to add to the history.
	  (help-xref-stack-item nil)
	  ;; Use the current buffer.
	  (help-xref-following t))
      (apply (car item) (cdr item))
      (goto-char pos))))

(defun help-insert-string (string)
  "Insert STRING to the help buffer and install xref info for it.
This function can be used to restore the old contents of the help buffer
when going back to the previous topic in the xref stack.  It is needed
in case when it is impossible to recompute the old contents of the
help buffer by other means."
  (setq help-xref-stack-item (list #'help-insert-string string))
  (with-output-to-temp-buffer (help-buffer)
    (insert string)))

(provide 'help-mode)

;;; help-mode.el ends here
