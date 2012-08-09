;;; sgml-mode.el --- SGML- and HTML-editing modes -*- coding: utf-8 -*-

;; Copyright (C) 1992, 1995-1996, 1998, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: James Clark <jjc@jclark.com>
;; Maintainer: FSF
;; Adapted-By: ESR, Daniel Pfeiffer <occitan@esperanto.org>,
;;             F.Potorti@cnuce.cnr.it
;; Keywords: wp, hypermedia, comm, languages

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

;; Configurable major mode for editing document in the SGML standard general
;; markup language.  As an example contains a mode for editing the derived
;; HTML hypertext markup language.

;;; Code:

(eval-when-compile
  (require 'skeleton)
  (require 'outline)
  (require 'cl))

(defgroup sgml nil
  "SGML editing mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom sgml-basic-offset 2
  "Specifies the basic indentation level for `sgml-indent-line'."
  :type 'integer
  :group 'sgml)

(defcustom sgml-transformation-function 'identity
  "Default value for `skeleton-transformation-function' in SGML mode."
  :type 'function
  :group 'sgml)

(put 'sgml-transformation-function 'variable-interactive
     "aTransformation function: ")
(defvaralias 'sgml-transformation 'sgml-transformation-function)

(defcustom sgml-mode-hook nil
  "Hook run by command `sgml-mode'.
`text-mode-hook' is run first."
  :group 'sgml
  :type 'hook)

;; As long as Emacs's syntax can't be complemented with predicates to context
;; sensitively confirm the syntax of characters, we have to live with this
;; kludgy kind of tradeoff.
(defvar sgml-specials '(?\")
  "List of characters that have a special meaning for SGML mode.
This list is used when first loading the `sgml-mode' library.
The supported characters and potential disadvantages are:

  ?\\\"	Makes \" in text start a string.
  ?'	Makes ' in text start a string.
  ?-	Makes -- in text start a comment.

When only one of ?\\\" or ?' are included, \"'\" or '\"', as can be found in
DTDs, start a string.  To partially avoid this problem this also makes these
self insert as named entities depending on `sgml-quick-keys'.

Including ?- has the problem of affecting dashes that have nothing to do
with comments, so we normally turn it off.")

(defvar sgml-quick-keys nil
  "Use <, >, &, /, SPC and `sgml-specials' keys \"electrically\" when non-nil.
This takes effect when first loading the `sgml-mode' library.")

(defvar sgml-mode-map
  (let ((map (make-keymap))	;`sparse' doesn't allow binding to charsets.
	(menu-map (make-sparse-keymap "SGML")))
    (define-key map "\C-c\C-i" 'sgml-tags-invisible)
    (define-key map "/" 'sgml-slash)
    (define-key map "\C-c\C-n" 'sgml-name-char)
    (define-key map "\C-c\C-t" 'sgml-tag)
    (define-key map "\C-c\C-a" 'sgml-attributes)
    (define-key map "\C-c\C-b" 'sgml-skip-tag-backward)
    (define-key map [?\C-c left] 'sgml-skip-tag-backward)
    (define-key map "\C-c\C-f" 'sgml-skip-tag-forward)
    (define-key map [?\C-c right] 'sgml-skip-tag-forward)
    (define-key map "\C-c\C-d" 'sgml-delete-tag)
    (define-key map "\C-c\^?" 'sgml-delete-tag)
    (define-key map "\C-c?" 'sgml-tag-help)
    (define-key map "\C-c]" 'sgml-close-tag)
    (define-key map "\C-c/" 'sgml-close-tag)

    ;; Redundant keybindings, for consistency with TeX mode.
    (define-key map "\C-c\C-o" 'sgml-tag)
    (define-key map "\C-c\C-e" 'sgml-close-tag)

    (define-key map "\C-c8" 'sgml-name-8bit-mode)
    (define-key map "\C-c\C-v" 'sgml-validate)
    (when sgml-quick-keys
      (define-key map "&" 'sgml-name-char)
      (define-key map "<" 'sgml-tag)
      (define-key map " " 'sgml-auto-attributes)
      (define-key map ">" 'sgml-maybe-end-tag)
      (when (memq ?\" sgml-specials)
        (define-key map "\"" 'sgml-name-self))
      (when (memq ?' sgml-specials)
        (define-key map "'" 'sgml-name-self)))
    (let ((c 127)
	  (map (nth 1 map)))
      (while (< (setq c (1+ c)) 256)
	(aset map c 'sgml-maybe-name-self)))
    (define-key map [menu-bar sgml] (cons "SGML" menu-map))
    (define-key menu-map [sgml-validate] '("Validate" . sgml-validate))
    (define-key menu-map [sgml-name-8bit-mode]
      '("Toggle 8 Bit Insertion" . sgml-name-8bit-mode))
    (define-key menu-map [sgml-tags-invisible]
      '("Toggle Tag Visibility" . sgml-tags-invisible))
    (define-key menu-map [sgml-tag-help]
      '("Describe Tag" . sgml-tag-help))
    (define-key menu-map [sgml-delete-tag]
      '("Delete Tag" . sgml-delete-tag))
    (define-key menu-map [sgml-skip-tag-forward]
      '("Forward Tag" . sgml-skip-tag-forward))
    (define-key menu-map [sgml-skip-tag-backward]
      '("Backward Tag" . sgml-skip-tag-backward))
    (define-key menu-map [sgml-attributes]
      '("Insert Attributes" . sgml-attributes))
    (define-key menu-map [sgml-tag] '("Insert Tag" . sgml-tag))
    map)
  "Keymap for SGML mode.  See also `sgml-specials'.")

(defun sgml-make-syntax-table (specials)
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    (modify-syntax-entry ?: "_" table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?. "_" table)
    (if (memq ?- specials)
	(modify-syntax-entry ?- "_ 1234" table))
    (if (memq ?\" specials)
	(modify-syntax-entry ?\" "\"\"" table))
    (if (memq ?' specials)
	(modify-syntax-entry ?\' "\"'" table))
    table))

(defvar sgml-mode-syntax-table (sgml-make-syntax-table sgml-specials)
  "Syntax table used in SGML mode.  See also `sgml-specials'.")

(defconst sgml-tag-syntax-table
  (let ((table (sgml-make-syntax-table sgml-specials)))
    (dolist (char '(?\( ?\) ?\{ ?\} ?\[ ?\] ?$ ?% ?& ?* ?+ ?/))
      (modify-syntax-entry char "." table))
    (unless (memq ?' sgml-specials)
      ;; Avoid that skipping a tag backwards skips any "'" prefixing it.
      (modify-syntax-entry ?' "w" table))
    table)
  "Syntax table used to parse SGML tags.")

(defcustom sgml-name-8bit-mode nil
  "When non-nil, insert non-ASCII characters as named entities."
  :type 'boolean
  :group 'sgml)

(defvar sgml-char-names
  [nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   "nbsp" "excl" "quot" "num" "dollar" "percnt" "amp" "apos"
   "lpar" "rpar" "ast" "plus" "comma" "hyphen" "period" "sol"
   nil nil nil nil nil nil nil nil
   nil nil "colon" "semi" "lt" "eq" "gt" "quest"
   "commat" nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil "lsqb" nil "rsqb" "uarr" "lowbar"
   "lsquo" nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil "lcub" "verbar" "rcub" "tilde" nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   nil nil nil nil nil nil nil nil
   "nbsp" "iexcl" "cent" "pound" "curren" "yen" "brvbar" "sect"
   "uml" "copy" "ordf" "laquo" "not" "shy" "reg" "macr"
   "ring" "plusmn" "sup2" "sup3" "acute" "micro" "para" "middot"
   "cedil" "sup1" "ordm" "raquo" "frac14" "frac12" "frac34" "iquest"
   "Agrave" "Aacute" "Acirc" "Atilde" "Auml" "Aring" "AElig" "Ccedil"
   "Egrave" "Eacute" "Ecirc" "Euml" "Igrave" "Iacute" "Icirc" "Iuml"
   "ETH" "Ntilde" "Ograve" "Oacute" "Ocirc" "Otilde" "Ouml" nil
   "Oslash" "Ugrave" "Uacute" "Ucirc" "Uuml" "Yacute" "THORN" "szlig"
   "agrave" "aacute" "acirc" "atilde" "auml" "aring" "aelig" "ccedil"
   "egrave" "eacute" "ecirc" "euml" "igrave" "iacute" "icirc" "iuml"
   "eth" "ntilde" "ograve" "oacute" "ocirc" "otilde" "ouml" "divide"
   "oslash" "ugrave" "uacute" "ucirc" "uuml" "yacute" "thorn" "yuml"]
  "Vector of symbolic character names without `&' and `;'.")

(put 'sgml-table 'char-table-extra-slots 0)

(defvar sgml-char-names-table
  (let ((table (make-char-table 'sgml-table))
	(i 32)
	elt)
    (while (< i 128)
      (setq elt (aref sgml-char-names i))
      (if elt (aset table (make-char 'latin-iso8859-1 i) elt))
      (setq i (1+ i)))
    table)
  "A table for mapping non-ASCII characters into SGML entity names.
Currently, only Latin-1 characters are supported.")

;; nsgmls is a free SGML parser in the SP suite available from
;; ftp.jclark.com and otherwise packaged for GNU systems.
;; Its error messages can be parsed by next-error.
;; The -s option suppresses output.

(defcustom sgml-validate-command "nsgmls -s" ; replaced old `sgmls'
  "The command to validate an SGML document.
The file name of current buffer file name will be appended to this,
separated by a space."
  :type 'string
  :version "21.1"
  :group 'sgml)

(defvar sgml-saved-validate-command nil
  "The command last used to validate in this buffer.")

;; I doubt that null end tags are used much for large elements,
;; so use a small distance here.
(defcustom sgml-slash-distance 1000
  "If non-nil, is the maximum distance to search for matching `/'."
  :type '(choice (const nil) integer)
  :group 'sgml)

(defconst sgml-namespace-re "[_[:alpha:]][-_.[:alnum:]]*")
(defconst sgml-name-re "[_:[:alpha:]][-_.:[:alnum:]]*")
(defconst sgml-tag-name-re (concat "<\\([!/?]?" sgml-name-re "\\)"))
(defconst sgml-attrs-re "\\(?:[^\"'/><]\\|\"[^\"]*\"\\|'[^']*'\\)*")
(defconst sgml-start-tag-regex (concat "<" sgml-name-re sgml-attrs-re)
  "Regular expression that matches a non-empty start tag.
Any terminating `>' or `/' is not matched.")

(defface sgml-namespace
  '((t (:inherit font-lock-builtin-face)))
  "`sgml-mode' face used to highlight the namespace part of identifiers."
  :group 'sgml)
(defvar sgml-namespace-face 'sgml-namespace)

;; internal
(defconst sgml-font-lock-keywords-1
  `((,(concat "<\\([!?]" sgml-name-re "\\)") 1 font-lock-keyword-face)
    ;; We could use the simpler "\\(" sgml-namespace-re ":\\)?" instead,
    ;; but it would cause a bit more backtracking in the re-matcher.
    (,(concat "</?\\(" sgml-namespace-re "\\)\\(?::\\(" sgml-name-re "\\)\\)?")
     (1 (if (match-end 2) sgml-namespace-face font-lock-function-name-face))
     (2 font-lock-function-name-face nil t))
    ;; FIXME: this doesn't cover the variables using a default value.
    ;; The first shy-group is an important anchor: it prevents an O(n^2)
    ;; pathological case where we otherwise keep retrying a failing match
    ;; against a very long word at every possible position within the word.
    (,(concat "\\(?:^\\|[ \t]\\)\\(" sgml-namespace-re "\\)\\(?::\\("
	      sgml-name-re "\\)\\)?=[\"']")
     (1 (if (match-end 2) sgml-namespace-face font-lock-variable-name-face))
     (2 font-lock-variable-name-face nil t))
    (,(concat "[&%]" sgml-name-re ";?") . font-lock-variable-name-face)))

(defconst sgml-font-lock-keywords-2
  (append
   sgml-font-lock-keywords-1
   '((eval
      . (cons (concat "<"
		      (regexp-opt (mapcar 'car sgml-tag-face-alist) t)
		      "\\([ \t][^>]*\\)?>\\([^<]+\\)</\\1>")
	      '(3 (cdr (assoc-string (match-string 1) sgml-tag-face-alist t))
		prepend))))))

;; for font-lock, but must be defvar'ed after
;; sgml-font-lock-keywords-1 and sgml-font-lock-keywords-2 above
(defvar sgml-font-lock-keywords sgml-font-lock-keywords-1
  "*Rules for highlighting SGML code.  See also `sgml-tag-face-alist'.")

(defconst sgml-syntax-propertize-function
  (syntax-propertize-rules
  ;; Use the `b' style of comments to avoid interference with the -- ... --
  ;; comments recognized when `sgml-specials' includes ?-.
  ;; FIXME: beware of <!--> blabla <!--> !!
   ("\\(<\\)!--" (1 "< b"))
    ("--[ \t\n]*\\(>\\)" (1 "> b"))
    ;; Double quotes outside of tags should not introduce strings.
    ;; Be careful to call `syntax-ppss' on a position before the one we're
    ;; going to change, so as not to need to flush the data we just computed.
    ("\"" (0 (if (prog1 (zerop (car (syntax-ppss (match-beginning 0))))
                   (goto-char (match-end 0)))
                 "."))))
  "Syntactic keywords for `sgml-mode'.")

;; internal
(defvar sgml-face-tag-alist ()
  "Alist of face and tag name for facemenu.")

(defvar sgml-tag-face-alist ()
  "Tag names and face or list of faces to fontify with when invisible.
When `font-lock-maximum-decoration' is 1 this is always used for fontifying.
When more these are fontified together with `sgml-font-lock-keywords'.")

(defvar sgml-display-text ()
  "Tag names as lowercase symbols, and display string when invisible.")

;; internal
(defvar sgml-tags-invisible nil)

(defcustom sgml-tag-alist
  '(("![" ("ignore" t) ("include" t))
    ("!attlist")
    ("!doctype")
    ("!element")
    ("!entity"))
  "Alist of tag names for completing read and insertion rules.
This alist is made up as

  ((\"tag\" . TAGRULE)
   ...)

TAGRULE is a list of optionally t (no endtag) or `\\n' (separate endtag by
newlines) or a skeleton with nil, t or `\\n' in place of the interactor
followed by an ATTRIBUTERULE (for an always present attribute) or an
attribute alist.

The attribute alist is made up as

  ((\"attribute\" . ATTRIBUTERULE)
   ...)

ATTRIBUTERULE is a list of optionally t (no value when no input) followed by
an optional alist of possible values."
  :type '(repeat (cons (string :tag "Tag Name")
		       (repeat :tag "Tag Rule" sexp)))
  :group 'sgml)
(put 'sgml-tag-alist 'risky-local-variable t)

(defcustom sgml-tag-help
  '(("!" . "Empty declaration for comment")
    ("![" . "Embed declarations with parser directive")
    ("!attlist" . "Tag attributes declaration")
    ("!doctype" . "Document type (DTD) declaration")
    ("!element" . "Tag declaration")
    ("!entity" . "Entity (macro) declaration"))
  "Alist of tag name and short description."
  :type '(repeat (cons (string :tag "Tag Name")
		       (string :tag "Description")))
  :group 'sgml)

(defcustom sgml-xml-mode nil
  "When non-nil, tag insertion functions will be XML-compliant.
It is set to be buffer-local when the file has
a DOCTYPE or an XML declaration."
  :type 'boolean
  :version "22.1"
  :group 'sgml)

(defvar sgml-empty-tags nil
  "List of tags whose !ELEMENT definition says EMPTY.")

(defvar sgml-unclosed-tags nil
  "List of tags whose !ELEMENT definition says the end-tag is optional.")

(defun sgml-xml-guess ()
  "Guess whether the current buffer is XML.  Return non-nil if so."
  (save-excursion
    (goto-char (point-min))
    (or (string= "xml" (file-name-extension (or buffer-file-name "")))
        ;; Maybe the buffer-size check isn't needed, I don't know.
        (and (zerop (buffer-size))
             (string= "xhtml" (file-name-extension (or buffer-file-name ""))))
	(looking-at "\\s-*<\\?xml")
	(when (re-search-forward
	       (eval-when-compile
		 (mapconcat 'identity
			    '("<!DOCTYPE" "\\(\\w+\\)" "\\(\\w+\\)"
			      "\"\\([^\"]+\\)\"" "\"\\([^\"]+\\)\"")
			    "\\s-+"))
	       nil t)
	  (string-match "X\\(HT\\)?ML" (match-string 3))))))

(defvar v2)				; free for skeleton

(defun sgml-comment-indent-new-line (&optional soft)
  (let ((comment-start "-- ")
	(comment-start-skip "\\(<!\\)?--[ \t]*")
	(comment-end " --")
	(comment-style 'plain))
    (comment-indent-new-line soft)))

(defun sgml-mode-facemenu-add-face-function (face end)
  (let ((tag-face (cdr (assq face sgml-face-tag-alist))))
    (cond (tag-face
	   (setq tag-face (funcall skeleton-transformation-function tag-face))
	   (setq facemenu-end-add-face (concat "</" tag-face ">"))
	   (concat "<" tag-face ">"))
	  ((and (consp face)
		(consp (car face))
		(null  (cdr face))
		(memq (caar face) '(:foreground :background)))
	   (setq facemenu-end-add-face "</span>")
	   (format "<span style=\"%s:%s\">"
		   (if (eq (caar face) :foreground)
		       "color"
		     "background-color")
		   (cadr (car face))))
	  (t
	   (error "Face not configured for %s mode"
		  (format-mode-line mode-name))))))

(defun sgml-fill-nobreak ()
  "Don't break between a tag name and its first argument.
This function is designed for use in `fill-nobreak-predicate'.

    <a href=\"some://where\" type=\"text/plain\">
      ^                   ^
      | no break here     | but still allowed here"
  (save-excursion
    (skip-chars-backward " \t")
    (and (not (zerop (skip-syntax-backward "w_")))
	 (skip-chars-backward "/?!")
	 (eq (char-before) ?<))))

;;;###autoload
(define-derived-mode sgml-mode text-mode '(sgml-xml-mode "XML" "SGML")
  "Major mode for editing SGML documents.
Makes > match <.
Keys <, &, SPC within <>, \", / and ' can be electric depending on
`sgml-quick-keys'.

An argument of N to a tag-inserting command means to wrap it around
the next N words.  In Transient Mark mode, when the mark is active,
N defaults to -1, which means to wrap it around the current region.

If you like upcased tags, put (setq sgml-transformation-function 'upcase)
in your `.emacs' file.

Use \\[sgml-validate] to validate your document with an SGML parser.

Do \\[describe-variable] sgml- SPC to see available variables.
Do \\[describe-key] on the following bindings to discover what they do.
\\{sgml-mode-map}"
  (make-local-variable 'sgml-saved-validate-command)
  (make-local-variable 'facemenu-end-add-face)
  ;;(make-local-variable 'facemenu-remove-face-function)
  ;; A start or end tag by itself on a line separates a paragraph.
  ;; This is desirable because SGML discards a newline that appears
  ;; immediately after a start tag or immediately before an end tag.
  (set (make-local-variable 'paragraph-start) (concat "[ \t]*$\\|\
\[ \t]*</?\\(" sgml-name-re sgml-attrs-re "\\)?>"))
  (set (make-local-variable 'paragraph-separate)
       (concat paragraph-start "$"))
  (set (make-local-variable 'adaptive-fill-regexp) "[ \t]*")
  (add-hook 'fill-nobreak-predicate 'sgml-fill-nobreak nil t)
  (set (make-local-variable 'indent-line-function) 'sgml-indent-line)
  (set (make-local-variable 'comment-start) "<!-- ")
  (set (make-local-variable 'comment-end) " -->")
  (set (make-local-variable 'comment-indent-function) 'sgml-comment-indent)
  (set (make-local-variable 'comment-line-break-function)
       'sgml-comment-indent-new-line)
  (set (make-local-variable 'skeleton-further-elements)
       '((completion-ignore-case t)))
  (set (make-local-variable 'skeleton-end-hook)
       (lambda ()
         (or (eolp)
             (not (or (eq v2 '\n) (eq (car-safe v2) '\n)))
             (newline-and-indent))))
  (set (make-local-variable 'font-lock-defaults)
       '((sgml-font-lock-keywords
          sgml-font-lock-keywords-1
          sgml-font-lock-keywords-2)
         nil t))
  (set (make-local-variable 'syntax-propertize-function)
       sgml-syntax-propertize-function)
  (set (make-local-variable 'facemenu-add-face-function)
       'sgml-mode-facemenu-add-face-function)
  (set (make-local-variable 'sgml-xml-mode) (sgml-xml-guess))
  (if sgml-xml-mode
      ()
    (set (make-local-variable 'skeleton-transformation-function)
         sgml-transformation-function))
  ;; This will allow existing comments within declarations to be
  ;; recognized.
  ;; I can't find a clear description of SGML/XML comments, but it seems that
  ;; the only reliable ones are <!-- ... --> although it's not clear what
  ;; "..." can contain.  It used to accept -- ... -- as well, but that was
  ;; apparently a mistake.
  (set (make-local-variable 'comment-start-skip) "<!--[ \t]*")
  (set (make-local-variable 'comment-end-skip) "[ \t]*--[ \t\n]*>")
  ;; This definition has an HTML leaning but probably fits well for other modes.
  (setq imenu-generic-expression
	`((nil
	   ,(concat "<!\\(element\\|entity\\)[ \t\n]+%?[ \t\n]*\\("
		    sgml-name-re "\\)")
	   2)
	  ("Id"
	   ,(concat "<[^>]+[ \t\n]+[Ii][Dd]=\\(['\"]"
		    (if sgml-xml-mode "" "?")
		    "\\)\\(" sgml-name-re "\\)\\1")
	   2)
	  ("Name"
	   ,(concat "<[^>]+[ \t\n]+[Nn][Aa][Mm][Ee]=\\(['\"]"
		    (if sgml-xml-mode "" "?")
		    "\\)\\(" sgml-name-re "\\)\\1")
	   2))))

(defun sgml-comment-indent ()
  (if (looking-at "--") comment-column 0))

(defun sgml-slash (arg)
  "Insert ARG slash characters.
Behaves electrically if `sgml-quick-keys' is non-nil."
  (interactive "p")
  (cond
   ((not (and (eq (char-before) ?<) (= arg 1)))
    (sgml-slash-matching arg))
   ((eq sgml-quick-keys 'indent)
    (insert-char ?/ 1)
    (indent-according-to-mode))
   ((eq sgml-quick-keys 'close)
    (delete-char -1)
    (sgml-close-tag))
   (t
    (sgml-slash-matching arg))))

(defun sgml-slash-matching (arg)
  "Insert `/' and display any previous matching `/'.
Two `/'s are treated as matching if the first `/' ends a net-enabling
start tag, and the second `/' is the corresponding null end tag."
  (interactive "p")
  (insert-char ?/ arg)
  (if (> arg 0)
      (let ((oldpos (point))
	    (blinkpos)
	    (level 0))
	(save-excursion
	  (save-restriction
	    (if sgml-slash-distance
		(narrow-to-region (max (point-min)
				       (- (point) sgml-slash-distance))
				  oldpos))
	    (if (and (re-search-backward sgml-start-tag-regex (point-min) t)
		     (eq (match-end 0) (1- oldpos)))
		()
	      (goto-char (1- oldpos))
	      (while (and (not blinkpos)
			  (search-backward "/" (point-min) t))
		(let ((tagend (save-excursion
				(if (re-search-backward sgml-start-tag-regex
							(point-min) t)
				    (match-end 0)
				  nil))))
		  (if (eq tagend (point))
		      (if (eq level 0)
			  (setq blinkpos (point))
			(setq level (1- level)))
		    (setq level (1+ level)))))))
	  (when blinkpos
            (goto-char blinkpos)
            (if (pos-visible-in-window-p)
                (sit-for 1)
              (message "Matches %s"
                       (buffer-substring (line-beginning-position)
                                         (1+ blinkpos)))))))))

;; Why doesn't this use the iso-cvt table or, preferably, generate the
;; inverse of the extensive table in the SGML Quail input method?  -- fx
;; I guess that's moot since it only works with Latin-1 anyhow.
(defun sgml-name-char (&optional char)
  "Insert a symbolic character name according to `sgml-char-names'.
Non-ASCII chars may be inserted either with the meta key, as in M-SPC for
no-break space or M-- for a soft hyphen; or via an input method or
encoded keyboard operation."
  (interactive "*")
  (insert ?&)
  (or char
      (setq char (read-quoted-char "Enter char or octal number")))
  (delete-char -1)
  (insert char)
  (undo-boundary)
  (sgml-namify-char))

(defun sgml-namify-char ()
  "Change the char before point into its `&name;' equivalent.
Uses `sgml-char-names'."
  (interactive)
  (let* ((char (char-before))
	 (name
	  (cond
	   ((null char) (error "No char before point"))
	   ((< char 256) (or (aref sgml-char-names char) char))
	   ((aref sgml-char-names-table char))
	   ((encode-char char 'ucs)))))
    (if (not name)
	(error "Don't know the name of `%c'" char)
      (delete-char -1)
      (insert (format (if (numberp name) "&#%d;" "&%s;") name)))))

(defun sgml-name-self ()
  "Insert a symbolic character name according to `sgml-char-names'."
  (interactive "*")
  (sgml-name-char last-command-event))

(defun sgml-maybe-name-self ()
  "Insert a symbolic character name according to `sgml-char-names'."
  (interactive "*")
  (if sgml-name-8bit-mode
      (sgml-name-char last-command-event)
    (self-insert-command 1)))

(defun sgml-name-8bit-mode ()
  "Toggle whether to insert named entities instead of non-ASCII characters.
This only works for Latin-1 input."
  (interactive)
  (setq sgml-name-8bit-mode (not sgml-name-8bit-mode))
  (message "sgml name entity mode is now %s"
	   (if sgml-name-8bit-mode "ON" "OFF")))

;; When an element of a skeleton is a string "str", it is passed
;; through `skeleton-transformation-function' and inserted.
;; If "str" is to be inserted literally, one should obtain it as
;; the return value of a function, e.g. (identity "str").

(defvar sgml-tag-last nil)
(defvar sgml-tag-history nil)
(define-skeleton sgml-tag
  "Prompt for a tag and insert it, optionally with attributes.
Completion and configuration are done according to `sgml-tag-alist'.
If you like tags and attributes in uppercase do \\[set-variable]
`skeleton-transformation-function' RET `upcase' RET, or put this
in your `.emacs':
  (setq sgml-transformation-function 'upcase)"
  (funcall (or skeleton-transformation-function 'identity)
           (setq sgml-tag-last
		 (completing-read
		  (if (> (length sgml-tag-last) 0)
		      (format "Tag (default %s): " sgml-tag-last)
		    "Tag: ")
		  sgml-tag-alist nil nil nil 'sgml-tag-history sgml-tag-last)))
  ?< str |
  (("") -1 '(undo-boundary) (identity "&lt;")) |	; see comment above
  `(("") '(setq v2 (sgml-attributes ,str t)) ?>
    (cond
     ((string= "![" ,str)
      (backward-char)
      '(("") " [ " _ " ]]"))
     ((and (eq v2 t) sgml-xml-mode (member ,str sgml-empty-tags))
      '(("") -1 " />"))
     ((or (and (eq v2 t) (not sgml-xml-mode)) (string-match "^[/!?]" ,str))
      nil)
     ((symbolp v2)
      ;; Make sure we don't fall into an infinite loop.
      ;; For xhtml's `tr' tag, we should maybe use \n instead.
      (if (eq v2 t) (setq v2 nil))
      ;; We use `identity' to prevent skeleton from passing
      ;; `str' through `skeleton-transformation-function' a second time.
      '(("") v2 _ v2 "</" (identity ',str) ?>))
     ((eq (car v2) t)
      (cons '("") (cdr v2)))
     (t
      (append '(("") (car v2))
	      (cdr v2)
	      '(resume: (car v2) _ "</" (identity ',str) ?>))))))

(autoload 'skeleton-read "skeleton")

(defun sgml-attributes (tag &optional quiet)
  "When at top level of a tag, interactively insert attributes.

Completion and configuration of TAG are done according to `sgml-tag-alist'.
If QUIET, do not print a message when there are no attributes for TAG."
  (interactive (list (save-excursion (sgml-beginning-of-tag t))))
  (or (stringp tag) (error "Wrong context for adding attribute"))
  (if tag
      (let ((completion-ignore-case t)
	    (alist (cdr (assoc (downcase tag) sgml-tag-alist)))
	    car attribute i)
	(if (or (symbolp (car alist))
		(symbolp (car (car alist))))
	    (setq car (car alist)
		  alist (cdr alist)))
	(or quiet
	    (message "No attributes configured."))
	(if (stringp (car alist))
	    (progn
	      (insert (if (eq (preceding-char) ?\s) "" ?\s)
		      (funcall skeleton-transformation-function (car alist)))
	      (sgml-value alist))
	  (setq i (length alist))
	  (while (> i 0)
	    (insert ?\s)
	    (insert (funcall skeleton-transformation-function
			     (setq attribute
				   (skeleton-read '(completing-read
						    "Attribute: "
						    alist)))))
	    (if (string= "" attribute)
		(setq i 0)
	      (sgml-value (assoc (downcase attribute) alist))
	      (setq i (1- i))))
	  (if (eq (preceding-char) ?\s)
	      (delete-char -1)))
	car)))

(defun sgml-auto-attributes (arg)
  "Self insert the character typed; at top level of tag, prompt for attributes.
With prefix argument, only self insert."
  (interactive "*P")
  (let ((point (point))
	tag)
    (if (or arg
	    (not sgml-tag-alist)	; no message when nothing configured
	    (symbolp (setq tag (save-excursion (sgml-beginning-of-tag t))))
	    (eq (aref tag 0) ?/))
	(self-insert-command (prefix-numeric-value arg))
      (sgml-attributes tag)
      (setq last-command-event ?\s)
      (or (> (point) point)
	  (self-insert-command 1)))))

(defun sgml-tag-help (&optional tag)
  "Display description of tag TAG.  If TAG is omitted, use the tag at point."
  (interactive
   (list (let ((def (save-excursion
		      (if (eq (following-char) ?<) (forward-char))
		      (sgml-beginning-of-tag))))
	   (completing-read (if def
				(format "Tag (default %s): " def)
			      "Tag: ")
			    sgml-tag-alist nil nil nil
			    'sgml-tag-history def))))
  (or (and tag (> (length tag) 0))
      (save-excursion
	(if (eq (following-char) ?<)
	    (forward-char))
	(setq tag (sgml-beginning-of-tag))))
  (or (stringp tag)
      (error "No tag selected"))
  (setq tag (downcase tag))
  (message "%s"
	   (or (cdr (assoc (downcase tag) sgml-tag-help))
	       (and (eq (aref tag 0) ?/)
		    (cdr (assoc (downcase (substring tag 1)) sgml-tag-help)))
	       "No description available")))

(defun sgml-maybe-end-tag (&optional arg)
  "Name self unless in position to end a tag or a prefix ARG is given."
  (interactive "P")
  (if (or arg (eq (car (sgml-lexical-context)) 'tag))
      (self-insert-command (prefix-numeric-value arg))
    (sgml-name-self)))

(defun sgml-skip-tag-backward (arg)
  "Skip to beginning of tag or matching opening tag if present.
With prefix argument ARG, repeat this ARG times.
Return non-nil if we skipped over matched tags."
  (interactive "p")
  ;; FIXME: use sgml-get-context or something similar.
  (let ((return t))
    (while (>= arg 1)
      (search-backward "<" nil t)
      (if (looking-at "</\\([^ \n\t>]+\\)")
          ;; end tag, skip any nested pairs
          (let ((case-fold-search t)
                (re (concat "</?" (regexp-quote (match-string 1))
                            ;; Ignore empty tags like <foo/>.
                            "\\([^>]*[^/>]\\)?>")))
            (while (and (re-search-backward re nil t)
                        (eq (char-after (1+ (point))) ?/))
              (forward-char 1)
              (sgml-skip-tag-backward 1)))
        (setq return nil))
      (setq arg (1- arg)))
    return))

(defvar sgml-electric-tag-pair-overlays nil)
(defvar sgml-electric-tag-pair-timer nil)

(defun sgml-electric-tag-pair-before-change-function (beg end)
  (condition-case err
  (save-excursion
    (goto-char end)
    (skip-chars-backward "[:alnum:]-_.:")
    (if (and ;; (<= (point) beg) ; This poses problems for downcase-word.
             (or (eq (char-before) ?<)
                 (and (eq (char-before) ?/)
                      (eq (char-before (1- (point))) ?<)))
             (null (get-char-property (point) 'text-clones)))
        (let* ((endp (eq (char-before) ?/))
               (cl-start (point))
               (cl-end (progn (skip-chars-forward "[:alnum:]-_.:") (point)))
               (match
                (if endp
                    (when (sgml-skip-tag-backward 1) (forward-char 1) t)
                  (with-syntax-table sgml-tag-syntax-table
                    (up-list -1)
                    (when (sgml-skip-tag-forward 1)
                      (backward-sexp 1)
                      (forward-char 2)
                      t))))
               (clones (get-char-property (point) 'text-clones)))
          (when (and match
                     (/= cl-end cl-start)
                     (equal (buffer-substring cl-start cl-end)
                            (buffer-substring (point)
                                              (save-excursion
                                                (skip-chars-forward "[:alnum:]-_.:")
                                                (point))))
                     (or (not endp) (eq (char-after cl-end) ?>)))
            (when clones
              (message "sgml-electric-tag-pair-before-change-function: deleting old OLs")
              (mapc 'delete-overlay clones))
            (message "sgml-electric-tag-pair-before-change-function: new clone")
            (text-clone-create cl-start cl-end 'spread "[[:alnum:]-_.:]+")
            (setq sgml-electric-tag-pair-overlays
                  (append (get-char-property (point) 'text-clones)
                          sgml-electric-tag-pair-overlays))))))
  (scan-error nil)
  (error (message "Error in sgml-electric-pair-mode: %s" err))))

(defun sgml-electric-tag-pair-flush-overlays ()
  (while sgml-electric-tag-pair-overlays
    (delete-overlay (pop sgml-electric-tag-pair-overlays))))

(define-minor-mode sgml-electric-tag-pair-mode
  "Toggle SGML Electric Tag Pair mode.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

SGML Electric Tag Pair mode is a buffer-local minor mode for use
with `sgml-mode' and related major modes.  When enabled, editing
an opening markup tag automatically updates the closing tag."
  :lighter "/e"
  (if sgml-electric-tag-pair-mode
      (progn
        (add-hook 'before-change-functions
                  'sgml-electric-tag-pair-before-change-function
                  nil t)
        (unless sgml-electric-tag-pair-timer
          (setq sgml-electric-tag-pair-timer
                (run-with-idle-timer 5 'repeat 'sgml-electric-tag-pair-flush-overlays))))
    (remove-hook 'before-change-functions
                 'sgml-electric-tag-pair-before-change-function
                 t)
    ;; We leave the timer running for other buffers.
    ))


(defun sgml-skip-tag-forward (arg)
  "Skip to end of tag or matching closing tag if present.
With prefix argument ARG, repeat this ARG times.
Return t if after a closing tag."
  (interactive "p")
  ;; FIXME: Use sgml-get-context or something similar.
  ;; It currently might jump to an unrelated </P> if the <P>
  ;; we're skipping has no matching </P>.
  (let ((return t))
    (with-syntax-table sgml-tag-syntax-table
      (while (>= arg 1)
	(skip-chars-forward "^<>")
	(if (eq (following-char) ?>)
	    (up-list -1))
	(if (looking-at "<\\([^/ \n\t>]+\\)\\([^>]*[^/>]\\)?>")
	    ;; start tag, skip any nested same pairs _and_ closing tag
	    (let ((case-fold-search t)
		  (re (concat "</?" (regexp-quote (match-string 1))
			      ;; Ignore empty tags like <foo/>.
			      "\\([^>]*[^/>]\\)?>"))
		  point close)
	      (forward-list 1)
	      (setq point (point))
	      ;; FIXME: This re-search-forward will mistakenly match
	      ;; tag-like text inside attributes.
	      (while (and (re-search-forward re nil t)
			  (not (setq close
				     (eq (char-after (1+ (match-beginning 0))) ?/)))
			  (goto-char (match-beginning 0))
			  (sgml-skip-tag-forward 1))
		(setq close nil))
	      (unless close
		(goto-char point)
		(setq return nil)))
	  (forward-list 1))
	(setq arg (1- arg)))
      return)))

(defsubst sgml-looking-back-at (str)
  "Return t if the test before point matches STR."
  (let ((start (- (point) (length str))))
    (and (>= start (point-min))
         (equal str (buffer-substring-no-properties start (point))))))

(defun sgml-delete-tag (arg)
  ;; FIXME: Should be called sgml-kill-tag or should not touch the kill-ring.
  "Delete tag on or after cursor, and matching closing or opening tag.
With prefix argument ARG, repeat this ARG times."
  (interactive "p")
  (while (>= arg 1)
    (save-excursion
      (let* (close open)
	(if (looking-at "[ \t\n]*<")
	    ;; just before tag
	    (if (eq (char-after (match-end 0)) ?/)
		;; closing tag
		(progn
		  (setq close (point))
		  (goto-char (match-end 0))))
	  ;; on tag?
	  (or (save-excursion (setq close (sgml-beginning-of-tag)
				    close (and (stringp close)
					       (eq (aref close 0) ?/)
					       (point))))
	      ;; not on closing tag
	      (let ((point (point)))
		(sgml-skip-tag-backward 1)
		(if (or (not (eq (following-char) ?<))
			(save-excursion
			  (forward-list 1)
			  (<= (point) point)))
		    (error "Not on or before tag")))))
	(if close
	    (progn
	      (sgml-skip-tag-backward 1)
	      (setq open (point))
	      (goto-char close)
	      (kill-sexp 1))
	  (setq open (point))
	  (when (and (sgml-skip-tag-forward 1)
		     (not (sgml-looking-back-at "/>")))
	    (kill-sexp -1)))
	;; Delete any resulting empty line.  If we didn't kill-sexp,
	;; this *should* do nothing, because we're right after the tag.
	(if (progn (forward-line 0) (looking-at "\\(?:[ \t]*$\\)\n?"))
	    (delete-region (match-beginning 0) (match-end 0)))
	(goto-char open)
	(kill-sexp 1)
	(if (progn (forward-line 0) (looking-at "\\(?:[ \t]*$\\)\n?"))
	    (delete-region (match-beginning 0) (match-end 0)))))
    (setq arg (1- arg))))


;; Put read-only last to enable setting this even when read-only enabled.
(or (get 'sgml-tag 'invisible)
    (setplist 'sgml-tag
	      (append '(invisible t
			point-entered sgml-point-entered
			rear-nonsticky t
			read-only t)
		      (symbol-plist 'sgml-tag))))

(defun sgml-tags-invisible (arg)
  "Toggle visibility of existing tags."
  (interactive "P")
  (let ((modified (buffer-modified-p))
	(inhibit-read-only t)
	(inhibit-modification-hooks t)
	;; Avoid spurious the `file-locked' checks.
	(buffer-file-name nil)
	;; This is needed in case font lock gets called,
	;; since it moves point and might call sgml-point-entered.
	;; How could it get called?  -stef
	(inhibit-point-motion-hooks t)
	string)
    (unwind-protect
	(save-excursion
	  (goto-char (point-min))
	  (if (set (make-local-variable 'sgml-tags-invisible)
		   (if arg
		       (>= (prefix-numeric-value arg) 0)
		     (not sgml-tags-invisible)))
	      (while (re-search-forward sgml-tag-name-re nil t)
		(setq string
		      (cdr (assq (intern-soft (downcase (match-string 1)))
				 sgml-display-text)))
		(goto-char (match-beginning 0))
		(and (stringp string)
		     (not (overlays-at (point)))
		     (let ((ol (make-overlay (point) (match-beginning 1))))
		       (overlay-put ol 'before-string string)
		       (overlay-put ol 'sgml-tag t)))
		(put-text-property (point)
				   (progn (forward-list) (point))
				   'category 'sgml-tag))
	    (let ((pos (point-min)))
	      (while (< (setq pos (next-overlay-change pos)) (point-max))
		(dolist (ol (overlays-at pos))
		  (if (overlay-get ol 'sgml-tag)
		      (delete-overlay ol)))))
	    (remove-text-properties (point-min) (point-max) '(category nil))))
      (restore-buffer-modified-p modified))
    (run-hooks 'sgml-tags-invisible-hook)
    (message "")))

(defun sgml-point-entered (x y)
  ;; Show preceding or following hidden tag, depending of cursor direction.
  (let ((inhibit-point-motion-hooks t))
    (save-excursion
      (condition-case nil
	  (message "Invisible tag: %s"
		   ;; Strip properties, otherwise, the text is invisible.
		   (buffer-substring-no-properties
		    (point)
		    (if (or (and (> x y)
				 (not (eq (following-char) ?<)))
			    (and (< x y)
				 (eq (preceding-char) ?>)))
			(backward-list)
		      (forward-list))))
	(error nil)))))



(defun sgml-validate (command)
  "Validate an SGML document.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.
You can then use the command \\[next-error] to find the next error message
and move to the line in the SGML document that caused it."
  (interactive
   (list (read-string "Validate command: "
		      (or sgml-saved-validate-command
			  (concat sgml-validate-command
				  " "
				  (shell-quote-argument
				   (let ((name (buffer-file-name)))
				     (and name
					  (file-name-nondirectory name)))))))))
  (setq sgml-saved-validate-command command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compilation-start command))

(defsubst sgml-at-indentation-p ()
  "Return true if point is at the first non-whitespace character on the line."
  (save-excursion
    (skip-chars-backward " \t")
    (bolp)))

(defun sgml-lexical-context (&optional limit)
  "Return the lexical context at point as (TYPE . START).
START is the location of the start of the lexical element.
TYPE is one of `string', `comment', `tag', `cdata', `pi', or `text'.

Optional argument LIMIT is the position to start parsing from.
If nil, start from a preceding tag at indentation."
  (save-excursion
    (let ((pos (point))
	  text-start state)
      (if limit
          (goto-char limit)
        ;; Skip tags backwards until we find one at indentation
        (while (and (ignore-errors (sgml-parse-tag-backward))
                    (not (sgml-at-indentation-p)))))
      (with-syntax-table sgml-tag-syntax-table
	(while (< (point) pos)
	  ;; When entering this loop we're inside text.
	  (setq text-start (point))
	  (skip-chars-forward "^<" pos)
          (setq state
                (cond
                 ((= (point) pos)
                  ;; We got to the end without seeing a tag.
                  nil)
                 ((looking-at "<!\\[[A-Z]+\\[")
                  ;; We've found a CDATA section or similar.
                  (let ((cdata-start (point)))
                    (unless (search-forward "]]>" pos 'move)
                      (list 0 nil nil 'cdata nil nil nil nil cdata-start))))
		 ((looking-at comment-start-skip)
		  ;; parse-partial-sexp doesn't handle <!-- comments -->,
		  ;; or only if ?- is in sgml-specials, so match explicitly
		  (let ((start (point)))
		    (unless (re-search-forward comment-end-skip pos 'move)
		      (list 0 nil nil nil t nil nil nil start))))
                 ((and sgml-xml-mode (looking-at "<\\?"))
                  ;; Processing Instructions.
                  ;; In SGML, it's basically a normal tag of the form
                  ;; <?NAME ...> but in XML, it takes the form <? ... ?>.
                  (let ((pi-start (point)))
                    (unless (search-forward "?>" pos 'move)
                      (list 0 nil nil 'pi nil nil nil nil pi-start))))
                 (t
                  ;; We've reached a tag.  Parse it.
                  ;; FIXME: Handle net-enabling start-tags
                  (parse-partial-sexp (point) pos 0))))))
      (cond
       ((memq (nth 3 state) '(cdata pi)) (cons (nth 3 state) (nth 8 state)))
       ((nth 3 state) (cons 'string (nth 8 state)))
       ((nth 4 state) (cons 'comment (nth 8 state)))
       ((and state (> (nth 0 state) 0)) (cons 'tag (nth 1 state)))
       (t (cons 'text text-start))))))

(defun sgml-beginning-of-tag (&optional top-level)
  "Skip to beginning of tag and return its name.
If this can't be done, return nil."
  (let ((context (sgml-lexical-context)))
    (if (eq (car context) 'tag)
	(progn
	  (goto-char (cdr context))
	  (when (looking-at sgml-tag-name-re)
	    (match-string-no-properties 1)))
      (if top-level nil
	(when (not (eq (car context) 'text))
	  (goto-char (cdr context))
	  (sgml-beginning-of-tag t))))))

(defun sgml-value (alist)
  "Interactively insert value taken from attribute-rule ALIST.
See `sgml-tag-alist' for info about attribute rules."
  (setq alist (cdr alist))
  (if (stringp (car alist))
      (insert "=\"" (car alist) ?\")
    (if (and (eq (car alist) t) (not sgml-xml-mode))
	(when (cdr alist)
	  (insert "=\"")
	  (setq alist (skeleton-read '(completing-read "Value: " (cdr alist))))
	  (if (string< "" alist)
	      (insert alist ?\")
	    (delete-char -2)))
      (insert "=\"")
      (if (cdr alist)
          (insert (skeleton-read '(completing-read "Value: " alist)))
        (when (null alist)
          (insert (skeleton-read '(read-string "Value: ")))))
      (insert ?\"))))

(defun sgml-quote (start end &optional unquotep)
  "Quote SGML text in region START ... END.
Only &, < and > are quoted, the rest is left untouched.
With prefix argument UNQUOTEP, unquote the region."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (if unquotep
	;; FIXME: We should unquote other named character references as well.
	(while (re-search-forward
		"\\(&\\(amp\\|\\(l\\|\\(g\\)\\)t\\)\\)[][<>&;\n\t \"%!'(),/=?]"
		nil t)
	  (replace-match (if (match-end 4) ">" (if (match-end 3) "<" "&")) t t
			 nil (if (eq (char-before (match-end 0)) ?\;) 0 1)))
      (while (re-search-forward "[&<>]" nil t)
	(replace-match (cdr (assq (char-before) '((?& . "&amp;")
						  (?< . "&lt;")
						  (?> . "&gt;"))))
		       t t)))))

(defun sgml-pretty-print (beg end)
  "Simple-minded pretty printer for SGML.
Re-indents the code and inserts newlines between BEG and END.
You might want to turn on `auto-fill-mode' to get better results."
  ;; TODO:
  ;; - insert newline between some start-tag and text.
  ;; - don't insert newline in front of some end-tags.
  (interactive "r")
  (save-excursion
    (if (< beg end)
	(goto-char beg)
      (goto-char end)
      (setq end beg)
      (setq beg (point)))
    ;; Don't use narrowing because it screws up auto-indent.
    (setq end (copy-marker end t))
    (with-syntax-table sgml-tag-syntax-table
      (while (re-search-forward "<" end t)
	(goto-char (match-beginning 0))
	(unless (or ;;(looking-at "</")
		    (progn (skip-chars-backward " \t") (bolp)))
	  (reindent-then-newline-and-indent))
	(forward-sexp 1)))
    ;; (indent-region beg end)
    ))


;; Parsing

(defstruct (sgml-tag
            (:constructor sgml-make-tag (type start end name)))
  type start end name)

(defsubst sgml-parse-tag-name ()
  "Skip past a tag-name, and return the name."
  (buffer-substring-no-properties
   (point) (progn (skip-syntax-forward "w_") (point))))

(defun sgml-tag-text-p (start end)
  "Return non-nil if text between START and END is a tag.
Checks among other things that the tag does not contain spurious
unquoted < or > chars inside, which would indicate that it
really isn't a tag after all."
  (save-excursion
    (with-syntax-table sgml-tag-syntax-table
      (let ((pps (parse-partial-sexp start end 2)))
	(and (= (nth 0 pps) 0))))))

(defun sgml-parse-tag-backward (&optional limit)
  "Parse an SGML tag backward, and return information about the tag.
Assume that parsing starts from within a textual context.
Leave point at the beginning of the tag."
  (catch 'found
    (let (tag-type tag-start tag-end name)
      (or (re-search-backward "[<>]" limit 'move)
	  (error "No tag found"))
      (when (eq (char-after) ?<)
	;; Oops!! Looks like we were not in a textual context after all!.
	;; Let's try to recover.
        ;; Remember the tag-start so we don't need to look for it later.
	;; This is not just an optimization but also makes sure we don't get
	;; stuck in infloops in cases where "looking back for <" would not go
	;; back far enough.
        (setq tag-start (point))
	(with-syntax-table sgml-tag-syntax-table
	  (let ((pos (point)))
	    (condition-case nil
                ;; FIXME: This does not correctly skip over PI an CDATA tags.
		(forward-sexp)
	      (scan-error
	       ;; This < seems to be just a spurious one, let's ignore it.
	       (goto-char pos)
	       (throw 'found (sgml-parse-tag-backward limit))))
	    ;; Check it is really a tag, without any extra < or > inside.
	    (unless (sgml-tag-text-p pos (point))
	      (goto-char pos)
	      (throw 'found (sgml-parse-tag-backward limit)))
	    (forward-char -1))))
      (setq tag-end (1+ (point)))
      (cond
       ((sgml-looking-back-at "--")	; comment
	(setq tag-type 'comment
	      tag-start (or tag-start (search-backward "<!--" nil t))))
       ((sgml-looking-back-at "]]")	; cdata
	(setq tag-type 'cdata
	      tag-start (or tag-start
                            (re-search-backward "<!\\[[A-Z]+\\[" nil t))))
       ((sgml-looking-back-at "?")      ; XML processing-instruction
        (setq tag-type 'pi
              ;; IIUC: SGML processing instructions take the form <?foo ...>
              ;; i.e. a "normal" tag, handled below.  In XML this is changed
              ;; to <?foo ... ?> where "..." can contain < and > and even <?
              ;; but not ?>.  This means that when parsing backward, there's
              ;; no easy way to make sure that we find the real beginning of
              ;; the PI.
	      tag-start (or tag-start (search-backward "<?" nil t))))
       (t
        (unless tag-start
          (setq tag-start
                (with-syntax-table sgml-tag-syntax-table
                  (goto-char tag-end)
                  (condition-case nil
                      (backward-sexp)
                    (scan-error
                     ;; This > isn't really the end of a tag. Skip it.
                     (goto-char (1- tag-end))
                     (throw 'found (sgml-parse-tag-backward limit))))
                  (point))))
	(goto-char (1+ tag-start))
	(case (char-after)
	  (?! (setq tag-type 'decl))    ; declaration
	  (?? (setq tag-type 'pi))      ; processing-instruction
	  (?% (setq tag-type 'jsp))	; JSP tags
	  (?/				; close-tag
	   (forward-char 1)
	   (setq tag-type 'close
		 name (sgml-parse-tag-name)))
	  (t				; open or empty tag
	   (setq tag-type 'open
		 name (sgml-parse-tag-name))
	   (if (or (eq ?/ (char-before (- tag-end 1)))
		   (sgml-empty-tag-p name))
	       (setq tag-type 'empty))))))
      (goto-char tag-start)
      (sgml-make-tag tag-type tag-start tag-end name))))

(defun sgml-get-context (&optional until)
  "Determine the context of the current position.
By default, parse until we find a start-tag as the first thing on a line.
If UNTIL is `empty', return even if the context is empty (i.e.
we just skipped over some element and got to a beginning of line).

The context is a list of tag-info structures.  The last one is the tag
immediately enclosing the current position.

Point is assumed to be outside of any tag.  If we discover that it's
not the case, the first tag returned is the one inside which we are."
  (let ((here (point))
	(stack nil)
	(ignore nil)
	(context nil)
	tag-info)
    ;; CONTEXT keeps track of the tag-stack
    ;; STACK keeps track of the end tags we've seen (and thus the start-tags
    ;;   we'll have to ignore) when skipping over matching open..close pairs.
    ;; IGNORE is a list of tags that can be ignored because they have been
    ;;   closed implicitly.
    (skip-chars-backward " \t\n")      ; Make sure we're not at indentation.
    (while
	(and (not (eq until 'now))
	     (or stack
		 (not (if until (eq until 'empty) context))
		 (not (sgml-at-indentation-p))
		 (and context
		      (/= (point) (sgml-tag-start (car context)))
		      (sgml-unclosed-tag-p (sgml-tag-name (car context)))))
	     (setq tag-info (ignore-errors (sgml-parse-tag-backward))))

      ;; This tag may enclose things we thought were tags.  If so,
      ;; discard them.
      (while (and context
                  (> (sgml-tag-end tag-info)
                     (sgml-tag-end (car context))))
        (setq context (cdr context)))

      (cond
       ((> (sgml-tag-end tag-info) here)
	;; Oops!!  Looks like we were not outside of any tag, after all.
	(push tag-info context)
	(setq until 'now))

       ;; start-tag
       ((eq (sgml-tag-type tag-info) 'open)
	(cond
	 ((null stack)
	  (if (assoc-string (sgml-tag-name tag-info) ignore t)
	      ;; There was an implicit end-tag.
	      nil
	    (push tag-info context)
	    ;; We're changing context so the tags implicitly closed inside
	    ;; the previous context aren't implicitly closed here any more.
	    ;; [ Well, actually it depends, but we don't have the info about
	    ;; when it doesn't and when it does.   --Stef ]
	    (setq ignore nil)))
	 ((eq t (compare-strings (sgml-tag-name tag-info) nil nil
				 (car stack) nil nil t))
	  (setq stack (cdr stack)))
	 (t
	  ;; The open and close tags don't match.
	  (if (not sgml-xml-mode)
	      (unless (sgml-unclosed-tag-p (sgml-tag-name tag-info))
		(message "Unclosed tag <%s>" (sgml-tag-name tag-info))
		(let ((tmp stack))
		  ;; We could just assume that the tag is simply not closed
		  ;; but it's a bad assumption when tags *are* closed but
		  ;; not properly nested.
		  (while (and (cdr tmp)
			      (not (eq t (compare-strings
					  (sgml-tag-name tag-info) nil nil
					  (cadr tmp) nil nil t))))
		    (setq tmp (cdr tmp)))
		  (if (cdr tmp) (setcdr tmp (cddr tmp)))))
	    (message "Unmatched tags <%s> and </%s>"
		     (sgml-tag-name tag-info) (pop stack)))))

	(if (and (null stack) (sgml-unclosed-tag-p (sgml-tag-name tag-info)))
	    ;; This is a top-level open of an implicitly closed tag, so any
	    ;; occurrence of such an open tag at the same level can be ignored
	    ;; because it's been implicitly closed.
	    (push (sgml-tag-name tag-info) ignore)))

       ;; end-tag
       ((eq (sgml-tag-type tag-info) 'close)
	(if (sgml-empty-tag-p (sgml-tag-name tag-info))
	    (message "Spurious </%s>: empty tag" (sgml-tag-name tag-info))
	  (push (sgml-tag-name tag-info) stack)))
       ))

    ;; return context
    context))

(defun sgml-show-context (&optional full)
  "Display the current context.
If FULL is non-nil, parse back to the beginning of the buffer."
  (interactive "P")
  (with-output-to-temp-buffer "*XML Context*"
    (save-excursion
      (let ((context (sgml-get-context)))
	(when full
	  (let ((more nil))
	    (while (setq more (sgml-get-context))
	      (setq context (nconc more context)))))
	(pp context)))))


;; Editing shortcuts

(defun sgml-close-tag ()
  "Close current element.
Depending on context, inserts a matching close-tag, or closes
the current start-tag or the current comment or the current cdata, ..."
  (interactive)
  (case (car (sgml-lexical-context))
    (comment 	(insert " -->"))
    (cdata 	(insert "]]>"))
    (pi 	(insert " ?>"))
    (jsp 	(insert " %>"))
    (tag 	(insert " />"))
    (text
     (let ((context (save-excursion (sgml-get-context))))
       (if context
           (progn
             (insert "</" (sgml-tag-name (car (last context))) ">")
             (indent-according-to-mode)))))
    (otherwise
     (error "Nothing to close"))))

(defun sgml-empty-tag-p (tag-name)
  "Return non-nil if TAG-NAME is an implicitly empty tag."
  (and (not sgml-xml-mode)
       (assoc-string tag-name sgml-empty-tags 'ignore-case)))

(defun sgml-unclosed-tag-p (tag-name)
  "Return non-nil if TAG-NAME is a tag for which an end-tag is optional."
  (and (not sgml-xml-mode)
       (assoc-string tag-name sgml-unclosed-tags 'ignore-case)))


(defun sgml-calculate-indent (&optional lcon)
  "Calculate the column to which this line should be indented.
LCON is the lexical context, if any."
  (unless lcon (setq lcon (sgml-lexical-context)))

  ;; Indent comment-start markers inside <!-- just like comment-end markers.
  (if (and (eq (car lcon) 'tag)
	   (looking-at "--")
	   (save-excursion (goto-char (cdr lcon)) (looking-at "<!--")))
      (setq lcon (cons 'comment (+ (cdr lcon) 2))))

  (case (car lcon)

    (string
     ;; Go back to previous non-empty line.
     (while (and (> (point) (cdr lcon))
		 (zerop (forward-line -1))
		 (looking-at "[ \t]*$")))
     (if (> (point) (cdr lcon))
	 ;; Previous line is inside the string.
	 (current-indentation)
       (goto-char (cdr lcon))
       (1+ (current-column))))

    (comment
     (let ((mark (looking-at "--")))
       ;; Go back to previous non-empty line.
       (while (and (> (point) (cdr lcon))
		   (zerop (forward-line -1))
		   (or (looking-at "[ \t]*$")
		       (if mark (not (looking-at "[ \t]*--"))))))
       (if (> (point) (cdr lcon))
	   ;; Previous line is inside the comment.
	   (skip-chars-forward " \t")
	 (goto-char (cdr lcon))
	 ;; Skip `<!' to get to the `--' with which we want to align.
	 (search-forward "--")
	 (goto-char (match-beginning 0)))
       (when (and (not mark) (looking-at "--"))
	 (forward-char 2) (skip-chars-forward " \t"))
       (current-column)))

    ;; We don't know how to indent it.  Let's be honest about it.
    (cdata nil)
    ;; We don't know how to indent it.  Let's be honest about it.
    (pi nil)

    (tag
     (goto-char (1+ (cdr lcon)))
     (skip-chars-forward "^ \t\n")	;Skip tag name.
     (skip-chars-forward " \t")
     (if (not (eolp))
	 (current-column)
       ;; This is the first attribute: indent.
       (goto-char (1+ (cdr lcon)))
       (+ (current-column) sgml-basic-offset)))

    (text
     (while (looking-at "</")
       (forward-sexp 1)
       (skip-chars-forward " \t"))
     (let* ((here (point))
	    (unclosed (and ;; (not sgml-xml-mode)
		       (looking-at sgml-tag-name-re)
		       (assoc-string (match-string 1)
				     sgml-unclosed-tags 'ignore-case)
		       (match-string 1)))
	    (context
	     ;; If possible, align on the previous non-empty text line.
	     ;; Otherwise, do a more serious parsing to find the
	     ;; tag(s) relative to which we should be indenting.
	     (if (and (not unclosed) (skip-chars-backward " \t")
		      (< (skip-chars-backward " \t\n") 0)
		      (back-to-indentation)
		      (> (point) (cdr lcon)))
		 nil
	       (goto-char here)
	       (nreverse (sgml-get-context (if unclosed nil 'empty)))))
	    (there (point)))
       ;; Ignore previous unclosed start-tag in context.
       (while (and context unclosed
		   (eq t (compare-strings
			  (sgml-tag-name (car context)) nil nil
			  unclosed nil nil t)))
	 (setq context (cdr context)))
       ;; Indent to reflect nesting.
       (cond
	;; If we were not in a text context after all, let's try again.
	((and context (> (sgml-tag-end (car context)) here))
	 (goto-char here)
	 (sgml-calculate-indent
	  (cons (if (memq (sgml-tag-type (car context)) '(comment cdata))
		    (sgml-tag-type (car context)) 'tag)
		(sgml-tag-start (car context)))))
	;; Align on the first element after the nearest open-tag, if any.
	((and context
	      (goto-char (sgml-tag-end (car context)))
	      (skip-chars-forward " \t\n")
	      (< (point) here) (sgml-at-indentation-p))
	 (current-column))
	(t
	 (goto-char there)
	 (+ (current-column)
	    (* sgml-basic-offset (length context)))))))

    (otherwise
     (error "Unrecognized context %s" (car lcon)))

    ))

(defun sgml-indent-line ()
  "Indent the current line as SGML."
  (interactive)
  (let* ((savep (point))
	 (indent-col
	  (save-excursion
	    (back-to-indentation)
	    (if (>= (point) savep) (setq savep nil))
	    (sgml-calculate-indent))))
    (if (null indent-col)
	'noindent
      (if savep
	  (save-excursion (indent-line-to indent-col))
	(indent-line-to indent-col)))))

(defun sgml-guess-indent ()
  "Guess an appropriate value for `sgml-basic-offset'.
Base the guessed indentation level on the first indented tag in the buffer.
Add this to `sgml-mode-hook' for convenience."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\([ \t]+\\)<" 500 'noerror)
        (progn
          (set (make-local-variable 'sgml-basic-offset)
               (1- (current-column)))
          (message "Guessed sgml-basic-offset = %d"
                   sgml-basic-offset)
          ))))

(defun sgml-parse-dtd ()
  "Simplistic parse of the current buffer as a DTD.
Currently just returns (EMPTY-TAGS UNCLOSED-TAGS)."
  (goto-char (point-min))
  (let ((empty nil)
	(unclosed nil))
    (while (re-search-forward "<!ELEMENT[ \t\n]+\\([^ \t\n]+\\)[ \t\n]+[-O][ \t\n]+\\([-O]\\)[ \t\n]+\\([^ \t\n]+\\)" nil t)
      (cond
       ((string= (match-string 3) "EMPTY")
	(push (match-string-no-properties 1) empty))
       ((string= (match-string 2) "O")
	(push (match-string-no-properties 1) unclosed))))
    (setq empty (sort (mapcar 'downcase empty) 'string<))
    (setq unclosed (sort (mapcar 'downcase unclosed) 'string<))
    (list empty unclosed)))

;;; HTML mode

(defcustom html-mode-hook nil
  "Hook run by command `html-mode'.
`text-mode-hook' and `sgml-mode-hook' are run first."
  :group 'sgml
  :type 'hook
  :options '(html-autoview-mode))

(defvar html-quick-keys sgml-quick-keys
  "Use C-c X combinations for quick insertion of frequent tags when non-nil.
This defaults to `sgml-quick-keys'.
This takes effect when first loading the library.")

(defvar html-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap "HTML")))
    (set-keymap-parent map  sgml-mode-map)
    (define-key map "\C-c6" 'html-headline-6)
    (define-key map "\C-c5" 'html-headline-5)
    (define-key map "\C-c4" 'html-headline-4)
    (define-key map "\C-c3" 'html-headline-3)
    (define-key map "\C-c2" 'html-headline-2)
    (define-key map "\C-c1" 'html-headline-1)
    (define-key map "\C-c\r" 'html-paragraph)
    (define-key map "\C-c\n" 'html-line)
    (define-key map "\C-c\C-c-" 'html-horizontal-rule)
    (define-key map "\C-c\C-co" 'html-ordered-list)
    (define-key map "\C-c\C-cu" 'html-unordered-list)
    (define-key map "\C-c\C-cr" 'html-radio-buttons)
    (define-key map "\C-c\C-cc" 'html-checkboxes)
    (define-key map "\C-c\C-cl" 'html-list-item)
    (define-key map "\C-c\C-ch" 'html-href-anchor)
    (define-key map "\C-c\C-cn" 'html-name-anchor)
    (define-key map "\C-c\C-ci" 'html-image)
    (when html-quick-keys
      (define-key map "\C-c-" 'html-horizontal-rule)
      (define-key map "\C-co" 'html-ordered-list)
      (define-key map "\C-cu" 'html-unordered-list)
      (define-key map "\C-cr" 'html-radio-buttons)
      (define-key map "\C-cc" 'html-checkboxes)
      (define-key map "\C-cl" 'html-list-item)
      (define-key map "\C-ch" 'html-href-anchor)
      (define-key map "\C-cn" 'html-name-anchor)
      (define-key map "\C-ci" 'html-image))
    (define-key map "\C-c\C-s" 'html-autoview-mode)
    (define-key map "\C-c\C-v" 'browse-url-of-buffer)
    (define-key map [menu-bar html] (cons "HTML" menu-map))
    (define-key menu-map [html-autoview-mode]
      '("Toggle Autoviewing" . html-autoview-mode))
    (define-key menu-map [browse-url-of-buffer]
      '("View Buffer Contents" . browse-url-of-buffer))
    (define-key menu-map [nil] '("--"))
    ;;(define-key menu-map "6" '("Heading 6" . html-headline-6))
    ;;(define-key menu-map "5" '("Heading 5" . html-headline-5))
    ;;(define-key menu-map "4" '("Heading 4" . html-headline-4))
    (define-key menu-map "3" '("Heading 3" . html-headline-3))
    (define-key menu-map "2" '("Heading 2" . html-headline-2))
    (define-key menu-map "1" '("Heading 1" . html-headline-1))
    (define-key menu-map "l" '("Radio Buttons" . html-radio-buttons))
    (define-key menu-map "c" '("Checkboxes" . html-checkboxes))
    (define-key menu-map "l" '("List Item" . html-list-item))
    (define-key menu-map "u" '("Unordered List" . html-unordered-list))
    (define-key menu-map "o" '("Ordered List" . html-ordered-list))
    (define-key menu-map "-" '("Horizontal Rule" . html-horizontal-rule))
    (define-key menu-map "\n" '("Line Break" . html-line))
    (define-key menu-map "\r" '("Paragraph" . html-paragraph))
    (define-key menu-map "i" '("Image" . html-image))
    (define-key menu-map "h" '("Href Anchor" . html-href-anchor))
    (define-key menu-map "n" '("Name Anchor" . html-name-anchor))
    map)
  "Keymap for commands for use in HTML mode.")

(defvar html-face-tag-alist
  '((bold . "b")
    (italic . "i")
    (underline . "u")
    (modeline . "rev"))
  "Value of `sgml-face-tag-alist' for HTML mode.")

(defvar html-tag-face-alist
  '(("b" . bold)
    ("big" . bold)
    ("blink" . highlight)
    ("cite" . italic)
    ("em" . italic)
    ("h1" bold underline)
    ("h2" bold-italic underline)
    ("h3" italic underline)
    ("h4" . underline)
    ("h5" . underline)
    ("h6" . underline)
    ("i" . italic)
    ("rev"  . modeline)
    ("s" . underline)
    ("small" . default)
    ("strong" . bold)
    ("title" bold underline)
    ("tt" . default)
    ("u" . underline)
    ("var" . italic))
  "Value of `sgml-tag-face-alist' for HTML mode.")

(defvar html-display-text
  '((img . "[/]")
    (hr . "----------")
    (li . "o "))
  "Value of `sgml-display-text' for HTML mode.")


;; should code exactly HTML 3 here when that is finished
(defvar html-tag-alist
  (let* ((1-7 '(("1") ("2") ("3") ("4") ("5") ("6") ("7")))
	 (1-9 `(,@1-7 ("8") ("9")))
	 (align '(("align" ("left") ("center") ("right"))))
	 (valign '(("top") ("middle") ("bottom") ("baseline")))
	 (rel '(("next") ("previous") ("parent") ("subdocument") ("made")))
	 (href '("href" ("ftp:") ("file:") ("finger:") ("gopher:") ("http:")
		 ("mailto:") ("news:") ("rlogin:") ("telnet:") ("tn3270:")
		 ("wais:") ("/cgi-bin/")))
	 (name '("name"))
	 (link `(,href
		 ("rel" ,@rel)
		 ("rev" ,@rel)
		 ("title")))
	 (list '((nil \n ("List item: " "<li>" str
                          (if sgml-xml-mode "</li>") \n))))
	 (cell `(t
		 ,@align
		 ("valign" ,@valign)
		 ("colspan" ,@1-9)
		 ("rowspan" ,@1-9)
		 ("nowrap" t))))
    ;; put ,-expressions first, else byte-compile chokes (as of V19.29)
    ;; and like this it's more efficient anyway
    `(("a" ,name ,@link)
      ("base" t ,@href)
      ("dir" ,@list)
      ("font" nil "size" ("-1") ("+1") ("-2") ("+2") ,@1-7)
      ("form" (\n _ \n "<input type=\"submit\" value=\"\""
	       (if sgml-xml-mode " />" ">"))
       ("action" ,@(cdr href)) ("method" ("get") ("post")))
      ("h1" ,@align)
      ("h2" ,@align)
      ("h3" ,@align)
      ("h4" ,@align)
      ("h5" ,@align)
      ("h6" ,@align)
      ("hr" t ("size" ,@1-9) ("width") ("noshade" t) ,@align)
      ("img" t ("align" ,@valign ("texttop") ("absmiddle") ("absbottom"))
       ("src") ("alt") ("width" "1") ("height" "1")
       ("border" "1") ("vspace" "1") ("hspace" "1") ("ismap" t))
      ("input" t ("size" ,@1-9) ("maxlength" ,@1-9) ("checked" t) ,name
       ("type" ("text") ("password") ("checkbox") ("radio")
	("submit") ("reset"))
       ("value"))
      ("link" t ,@link)
      ("menu" ,@list)
      ("ol" ,@list ("type" ("A") ("a") ("I") ("i") ("1")))
      ("p" t ,@align)
      ("select" (nil \n
		     ("Text: "
		      "<option>" str (if sgml-xml-mode "</option>") \n))
       ,name ("size" ,@1-9) ("multiple" t))
      ("table" (nil \n
		    ((completing-read "Cell kind: " '(("td") ("th"))
				      nil t "t")
		     "<tr><" str ?> _
		     (if sgml-xml-mode (concat "<" str "></tr>")) \n))
       ("border" t ,@1-9) ("width" "10") ("cellpadding"))
      ("td" ,@cell)
      ("textarea" ,name ("rows" ,@1-9) ("cols" ,@1-9))
      ("th" ,@cell)
      ("ul" ,@list ("type" ("disc") ("circle") ("square")))

      ,@sgml-tag-alist

      ("abbrev")
      ("acronym")
      ("address")
      ("array" (nil \n
		    ("Item: " "<item>" str (if sgml-xml-mode "</item>") \n))
       "align")
      ("au")
      ("b")
      ("big")
      ("blink")
      ("blockquote" \n)
      ("body" \n ("background" ".gif") ("bgcolor" "#") ("text" "#")
       ("link" "#") ("alink" "#") ("vlink" "#"))
      ("box" (nil _ "<over>" _ (if sgml-xml-mode "</over>")))
      ("br" t ("clear" ("left") ("right")))
      ("caption" ("valign" ("top") ("bottom")))
      ("center" \n)
      ("cite")
      ("code" \n)
      ("dd" ,(not sgml-xml-mode))
      ("del")
      ("dfn")
      ("div")
      ("dl" (nil \n
		 ( "Term: "
		   "<dt>" str (if sgml-xml-mode "</dt>")
                   "<dd>" _ (if sgml-xml-mode "</dd>") \n)))
      ("dt" (t _ (if sgml-xml-mode "</dt>")
             "<dd>" (if sgml-xml-mode "</dd>") \n))
      ("em")
      ("fn" "id" "fn")  ;; Footnotes were deprecated in HTML 3.2
      ("head" \n)
      ("html" (\n
	       "<head>\n"
	       "<title>" (setq str (read-input "Title: ")) "</title>\n"
	       "</head>\n"
	       "<body>\n<h1>" str "</h1>\n" _
	       "\n<address>\n<a href=\"mailto:"
	       user-mail-address
	       "\">" (user-full-name) "</a>\n</address>\n"
	       "</body>"
		))
      ("i")
      ("ins")
      ("isindex" t ("action") ("prompt"))
      ("kbd")
      ("lang")
      ("li" ,(not sgml-xml-mode))
      ("math" \n)
      ("nobr")
      ("option" t ("value") ("label") ("selected" t))
      ("over" t)
      ("person") ;; Tag for person's name tag deprecated in HTML 3.2
      ("pre" \n)
      ("q")
      ("rev")
      ("s")
      ("samp")
      ("small")
      ("span" nil
	("class"
	 ("builtin")
	 ("comment")
	 ("constant")
	 ("function-name")
	 ("keyword")
	 ("string")
	 ("type")
	 ("variable-name")
	 ("warning")))
      ("strong")
      ("sub")
      ("sup")
      ("title")
      ("tr" t)
      ("tt")
      ("u")
      ("var")
      ("wbr" t)))
  "*Value of `sgml-tag-alist' for HTML mode.")

(defvar html-tag-help
  `(,@sgml-tag-help
    ("a" . "Anchor of point or link elsewhere")
    ("abbrev" . "Abbreviation")
    ("acronym" . "Acronym")
    ("address" . "Formatted mail address")
    ("array" . "Math array")
    ("au" . "Author")
    ("b" . "Bold face")
    ("base" . "Base address for URLs")
    ("big" . "Font size")
    ("blink" . "Blinking text")
    ("blockquote" . "Indented quotation")
    ("body" . "Document body")
    ("box" . "Math fraction")
    ("br" . "Line break")
    ("caption" . "Table caption")
    ("center" . "Centered text")
    ("changed" . "Change bars")
    ("cite" . "Citation of a document")
    ("code" . "Formatted source code")
    ("dd" . "Definition of term")
    ("del" . "Deleted text")
    ("dfn" . "Defining instance of a term")
    ("dir" . "Directory list (obsolete)")
    ("div" . "Generic block-level container")
    ("dl" . "Definition list")
    ("dt" . "Term to be defined")
    ("em" . "Emphasized")
    ("embed" . "Embedded data in foreign format")
    ("fig" . "Figure")
    ("figa" . "Figure anchor")
    ("figd" . "Figure description")
    ("figt" . "Figure text")
    ("fn" . "Footnote")  ;; No one supports special footnote rendering.
    ("font" . "Font size")
    ("form" . "Form with input fields")
    ("group" . "Document grouping")
    ("h1" . "Most important section headline")
    ("h2" . "Important section headline")
    ("h3" . "Section headline")
    ("h4" . "Minor section headline")
    ("h5" . "Unimportant section headline")
    ("h6" . "Least important section headline")
    ("head" . "Document header")
    ("hr" . "Horizontal rule")
    ("html" . "HTML Document")
    ("i" . "Italic face")
    ("img" . "Graphic image")
    ("input" . "Form input field")
    ("ins" . "Inserted text")
    ("isindex" . "Input field for index search")
    ("kbd" . "Keyboard example face")
    ("lang" . "Natural language")
    ("li" . "List item")
    ("link" . "Link relationship")
    ("math" . "Math formula")
    ("menu" . "Menu list (obsolete)")
    ("mh" . "Form mail header")
    ("nextid" . "Allocate new id")
    ("nobr" . "Text without line break")
    ("ol" . "Ordered list")
    ("option" . "Selection list item")
    ("over" . "Math fraction rule")
    ("p" . "Paragraph start")
    ("panel" . "Floating panel")
    ("person" . "Person's name")
    ("pre" . "Preformatted fixed width text")
    ("q" . "Quotation")
    ("rev" . "Reverse video")
    ("s" . "Strikeout")
    ("samp" . "Sample text")
    ("select" . "Selection list")
    ("small" . "Font size")
    ("sp" . "Nobreak space")
    ("span" . "Generic inline container")
    ("strong" . "Standout text")
    ("sub" . "Subscript")
    ("sup" . "Superscript")
    ("table" . "Table with rows and columns")
    ("tb" . "Table vertical break")
    ("td" . "Table data cell")
    ("textarea" . "Form multiline edit area")
    ("th" . "Table header cell")
    ("title" . "Document title")
    ("tr" . "Table row separator")
    ("tt" . "Typewriter face")
    ("u" . "Underlined text")
    ("ul" . "Unordered list")
    ("var" . "Math variable face")
    ("wbr" . "Enable <br> within <nobr>"))
  "*Value of `sgml-tag-help' for HTML mode.")


;;;###autoload
(define-derived-mode html-mode sgml-mode '(sgml-xml-mode "XHTML" "HTML")
  "Major mode based on SGML mode for editing HTML documents.
This allows inserting skeleton constructs used in hypertext documents with
completion.  See below for an introduction to HTML.  Use
\\[browse-url-of-buffer] to see how this comes out.  See also `sgml-mode' on
which this is based.

Do \\[describe-variable] html- SPC and \\[describe-variable] sgml- SPC to see available variables.

To write fairly well formatted pages you only need to know few things.  Most
browsers have a function to read the source code of the page being seen, so
you can imitate various tricks.  Here's a very short HTML primer which you
can also view with a browser to see what happens:

<title>A Title Describing Contents</title> should be on every page.  Pages can
have <h1>Very Major Headlines</h1> through <h6>Very Minor Headlines</h6>
<hr> Parts can be separated with horizontal rules.

<p>Paragraphs only need an opening tag.  Line breaks and multiple spaces are
ignored unless the text is <pre>preformatted.</pre>  Text can be marked as
<b>bold</b>, <i>italic</i> or <u>underlined</u> using the normal M-o or
Edit/Text Properties/Face commands.

Pages can have <a name=\"SOMENAME\">named points</a> and can link other points
to them with <a href=\"#SOMENAME\">see also somename</a>.  In the same way <a
href=\"URL\">see also URL</a> where URL is a filename relative to current
directory, or absolute as in `http://www.cs.indiana.edu/elisp/w3/docs.html'.

Images in many formats can be inlined with <img src=\"URL\">.

If you mainly create your own documents, `sgml-specials' might be
interesting.  But note that some HTML 2 browsers can't handle `&apos;'.
To work around that, do:
   (eval-after-load \"sgml-mode\" '(aset sgml-char-names ?' nil))

\\{html-mode-map}"
  (set (make-local-variable 'sgml-display-text) html-display-text)
  (set (make-local-variable 'sgml-tag-face-alist) html-tag-face-alist)
  (make-local-variable 'sgml-tag-alist)
  (make-local-variable 'sgml-face-tag-alist)
  (make-local-variable 'sgml-tag-help)
  (make-local-variable 'outline-regexp)
  (make-local-variable 'outline-heading-end-regexp)
  (make-local-variable 'outline-level)
  (make-local-variable 'sentence-end-base)
  (setq sentence-end-base "[.?!][]\"'”)}]*\\(<[^>]*>\\)*"
	sgml-tag-alist html-tag-alist
	sgml-face-tag-alist html-face-tag-alist
	sgml-tag-help html-tag-help
	outline-regexp "^.*<[Hh][1-6]\\>"
	outline-heading-end-regexp "</[Hh][1-6]>"
	outline-level (lambda ()
			(char-before (match-end 0))))
  (setq imenu-create-index-function 'html-imenu-index)
  (set (make-local-variable 'sgml-empty-tags)
       ;; From HTML-4.01's loose.dtd, parsed with `sgml-parse-dtd',
       ;; plus manual addition of "wbr".
       '("area" "base" "basefont" "br" "col" "frame" "hr" "img" "input"
	 "isindex" "link" "meta" "param" "wbr"))
  (set (make-local-variable 'sgml-unclosed-tags)
       ;; From HTML-4.01's loose.dtd, parsed with `sgml-parse-dtd'.
       '("body" "colgroup" "dd" "dt" "head" "html" "li" "option"
	 "p" "tbody" "td" "tfoot" "th" "thead" "tr"))
  ;; It's for the user to decide if it defeats it or not  -stef
  ;; (make-local-variable 'imenu-sort-function)
  ;; (setq imenu-sort-function nil) ; sorting the menu defeats the purpose
  )

(defvar html-imenu-regexp
  "\\s-*<h\\([1-9]\\)[^\n<>]*>\\(<[^\n<>]*>\\)*\\s-*\\([^\n<>]*\\)"
  "*A regular expression matching a head line to be added to the menu.
The first `match-string' should be a number from 1-9.
The second `match-string' matches extra tags and is ignored.
The third `match-string' will be the used in the menu.")

(defun html-imenu-index ()
  "Return a table of contents for an HTML buffer for use with Imenu."
  (let (toc-index)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward html-imenu-regexp nil t)
	(setq toc-index
	      (cons (cons (concat (make-string
				   (* 2 (1- (string-to-number (match-string 1))))
				   ?\s)
				  (match-string 3))
			  (line-beginning-position))
		    toc-index))))
    (nreverse toc-index)))

(define-minor-mode html-autoview-mode
  "Toggle viewing of HTML files on save (HTML Autoview mode).
With a prefix argument ARG, enable HTML Autoview mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

HTML Autoview mode is a buffer-local minor mode for use with
`html-mode'.  If enabled, saving the file automatically runs
`browse-url-of-buffer' to view it."
  nil nil nil
  :group 'sgml
  (if html-autoview-mode
      (add-hook 'after-save-hook 'browse-url-of-buffer nil t)
    (remove-hook 'after-save-hook 'browse-url-of-buffer t)))


(define-skeleton html-href-anchor
  "HTML anchor tag with href attribute."
  "URL: "
  ;; '(setq input "http:")
  "<a href=\"" str "\">" _ "</a>")

(define-skeleton html-name-anchor
  "HTML anchor tag with name attribute."
  "Name: "
  "<a name=\"" str "\""
  (if sgml-xml-mode (concat " id=\"" str "\""))
  ">" _ "</a>")

(define-skeleton html-headline-1
  "HTML level 1 headline tags."
  nil
  "<h1>" _ "</h1>")

(define-skeleton html-headline-2
  "HTML level 2 headline tags."
  nil
  "<h2>" _ "</h2>")

(define-skeleton html-headline-3
  "HTML level 3 headline tags."
  nil
  "<h3>" _ "</h3>")

(define-skeleton html-headline-4
  "HTML level 4 headline tags."
  nil
  "<h4>" _ "</h4>")

(define-skeleton html-headline-5
  "HTML level 5 headline tags."
  nil
  "<h5>" _ "</h5>")

(define-skeleton html-headline-6
  "HTML level 6 headline tags."
  nil
  "<h6>" _ "</h6>")

(define-skeleton html-horizontal-rule
  "HTML horizontal rule tag."
  nil
  (if sgml-xml-mode "<hr />" "<hr>") \n)

(define-skeleton html-image
  "HTML image tag."
  "Image URL: "
  "<img src=\"" str "\" alt=\"" _ "\""
  (if sgml-xml-mode " />" ">"))

(define-skeleton html-line
  "HTML line break tag."
  nil
  (if sgml-xml-mode "<br />" "<br>") \n)

(define-skeleton html-ordered-list
  "HTML ordered list tags."
  nil
  "<ol>" \n
  "<li>" _ (if sgml-xml-mode "</li>") \n
  "</ol>")

(define-skeleton html-unordered-list
  "HTML unordered list tags."
  nil
  "<ul>" \n
  "<li>" _ (if sgml-xml-mode "</li>") \n
  "</ul>")

(define-skeleton html-list-item
  "HTML list item tag."
  nil
  (if (bolp) nil '\n)
  "<li>" _ (if sgml-xml-mode "</li>"))

(define-skeleton html-paragraph
  "HTML paragraph tag."
  nil
  (if (bolp) nil ?\n)
  "<p>" _ (if sgml-xml-mode "</p>"))

(define-skeleton html-checkboxes
  "Group of connected checkbox inputs."
  nil
  '(setq v1 nil
	 v2 nil)
  ("Value: "
   "<input type=\"" (identity "checkbox") ; see comment above about identity
   "\" name=\"" (or v1 (setq v1 (skeleton-read "Name: ")))
   "\" value=\"" str ?\"
   (when (y-or-n-p "Set \"checked\" attribute? ")
     (funcall skeleton-transformation-function
	      (if sgml-xml-mode " checked=\"checked\"" " checked")))
   (if sgml-xml-mode " />" ">")
   (skeleton-read "Text: " (capitalize str))
   (or v2 (setq v2 (if (y-or-n-p "Newline after text? ")
		       (funcall skeleton-transformation-function
                                (if sgml-xml-mode "<br />" "<br>"))
		     "")))
   \n))

(define-skeleton html-radio-buttons
  "Group of connected radio button inputs."
  nil
  '(setq v1 nil
	 v2 (cons nil nil))
  ("Value: "
   "<input type=\"" (identity "radio") ; see comment above about identity
   "\" name=\"" (or (car v2) (setcar v2 (skeleton-read "Name: ")))
   "\" value=\"" str ?\"
   (when (and (not v1) (setq v1 (y-or-n-p "Set \"checked\" attribute? ")))
     (funcall skeleton-transformation-function
	      (if sgml-xml-mode " checked=\"checked\"" " checked")))
   (if sgml-xml-mode " />" ">")
   (skeleton-read "Text: " (capitalize str))
   (or (cdr v2) (setcdr v2 (if (y-or-n-p "Newline after text? ")
			       (funcall skeleton-transformation-function
                                        (if sgml-xml-mode "<br />" "<br>"))
			     "")))
   \n))

(provide 'sgml-mode)

;;; sgml-mode.el ends here
