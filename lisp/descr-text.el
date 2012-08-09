;;; descr-text.el --- describe text mode

;; Copyright (C) 1994-1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
;; Maintainer: FSF
;; Keywords: faces, i18n, Unicode, multilingual

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

;;; Describe-Text Mode.

;;; Code:

(eval-when-compile (require 'quail))
(require 'help-mode)

;;; Describe-Text Utilities.

(defun describe-text-widget (widget)
  "Insert text to describe WIDGET in the current buffer."
  (insert-text-button
   (symbol-name (if (symbolp widget) widget (car widget)))
   'action `(lambda (&rest ignore)
	      (widget-browse ',widget))
   'help-echo "mouse-2, RET: browse this widget")
  (insert " ")
  (insert-text-button
   "(widget)Top" 'type 'help-info 'help-args '("(widget)Top")))

(defun describe-text-sexp (sexp)
  "Insert a short description of SEXP in the current buffer."
  (let ((pp (condition-case signal
		(pp-to-string sexp)
	      (error (prin1-to-string signal)))))
    (when (string-match-p "\n\\'" pp)
      (setq pp (substring pp 0 (1- (length pp)))))

    (if (and (not (string-match-p "\n" pp))
    	     (<= (length pp) (- (window-width) (current-column))))
	(insert pp)
      (insert-text-button
       "[Show]" 'action `(lambda (&rest ignore)
			   (with-output-to-temp-buffer
			       "*Pp Eval Output*"
			     (princ ',pp)))
       'help-echo "mouse-2, RET: pretty print value in another buffer"))))

(defun describe-property-list (properties)
  "Insert a description of PROPERTIES in the current buffer.
PROPERTIES should be a list of overlay or text properties.
The `category', `face' and `font-lock-face' properties are made
into help buttons that call `describe-text-category' or
`describe-face' when pushed."
  ;; Sort the properties by the size of their value.
  (dolist (elt (sort (let (ret)
		       (while properties
			 (push (list (pop properties) (pop properties)) ret))
		       ret)
		     (lambda (a b) (string< (prin1-to-string (nth 0 a) t)
					    (prin1-to-string (nth 0 b) t)))))
    (let ((key (nth 0 elt))
	  (value (nth 1 elt)))
      (insert (propertize (format "  %-20s " key)
			  'face 'help-argument-name))
      (cond ((eq key 'category)
	     (insert-text-button
	      (symbol-name value)
	      'action `(lambda (&rest ignore)
			 (describe-text-category ',value))
	      'follow-link t
	      'help-echo "mouse-2, RET: describe this category"))
            ((memq key '(face font-lock-face mouse-face))
	     (insert-text-button
	      (format "%S" value)
	      'type 'help-face 'help-args (list value)))
            ((widgetp value)
	     (describe-text-widget value))
	    (t
	     (describe-text-sexp value))))
    (insert "\n")))

;;; Describe-Text Commands.

(defun describe-text-category (category)
  "Describe a text property category."
  (interactive "SCategory: ")
  (help-setup-xref (list #'describe-text-category category)
		   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (insert "Category " (format "%S" category) ":\n\n")
      (describe-property-list (symbol-plist category))
      (goto-char (point-min)))))

;;;###autoload
(defun describe-text-properties (pos &optional output-buffer buffer)
  "Describe widgets, buttons, overlays, and text properties at POS.
POS is taken to be in BUFFER or in current buffer if nil.
Interactively, describe them for the character after point.
If optional second argument OUTPUT-BUFFER is non-nil,
insert the output into that buffer, and don't initialize or clear it
otherwise."
  (interactive "d")
  (let ((src-buf (current-buffer)))
    (if buffer (set-buffer buffer) (setq buffer (current-buffer)))
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (if output-buffer
      (describe-text-properties-1 pos output-buffer)
    (if (not (or (text-properties-at pos) (overlays-at pos)))
	(message "This is plain text.")
        (with-temp-buffer
	    (setq output-buffer (current-buffer))
	    (insert "Text content at position " (format "%d" pos) ":\n\n")
          (set-buffer buffer)
          (describe-text-properties-1 pos output-buffer)
          (set-buffer src-buf)
          (help-setup-xref (list 'describe-text-properties pos nil buffer)
                           (called-interactively-p 'interactive))
          (with-help-window (help-buffer)
            (with-current-buffer standard-output
              (buffer-swap-text output-buffer)
              (goto-char (point-min)))))))))

(defun describe-text-properties-1 (pos output-buffer)
  (let* ((properties (text-properties-at pos))
	 (overlays (overlays-at pos))
	 (wid-field (get-char-property pos 'field))
	 (wid-button (get-char-property pos 'button))
	 (wid-doc (get-char-property pos 'widget-doc))
	 ;; If button.el is not loaded, we have no buttons in the text.
	 (button (and (fboundp 'button-at) (button-at pos)))
	 (button-type (and button (button-type button)))
	 (button-label (and button (button-label button)))
	 (widget (or wid-field wid-button wid-doc)))
    (with-current-buffer output-buffer
      ;; Widgets
      (when (widgetp widget)
	(newline)
	(insert (cond (wid-field "This is an editable text area")
		      (wid-button "This is an active area")
		      (wid-doc "This is documentation text")))
	(insert " of a ")
	(describe-text-widget widget)
	(insert ".\n\n"))
      ;; Buttons
      (when (and button (not (widgetp wid-button)))
	(newline)
	(insert "Here is a `" (format "%S" button-type)
		"' button labeled `" button-label "'.\n\n"))
      ;; Overlays
      (when overlays
	(newline)
	(if (eq (length overlays) 1)
	    (insert "There is an overlay here:\n")
	  (insert "There are " (format "%d" (length overlays))
			 " overlays here:\n"))
	(dolist (overlay overlays)
	  (insert " From " (format "%d" (overlay-start overlay))
			 " to " (format "%d" (overlay-end overlay)) "\n")
	  (describe-property-list (overlay-properties overlay)))
	(insert "\n"))
      ;; Text properties
      (when properties
	(newline)
	(insert "There are text properties here:\n")
	(describe-property-list properties)))))

(defcustom describe-char-unidata-list
  '(name old-name general-category decomposition)
  "List of Unicode-based character property names shown by `describe-char'."
  :group 'mule
  :version "23.1"
  :type '(choice (const :tag "All properties" t)
          (set
           (const :tag "Unicode name" name)
           (const :tag "Unicode old name" old-name)
           (const :tag "Unicode general category " general-category)
           (const :tag "Unicode canonical combining class"
                  canonical-combining-class)
           (const :tag "Unicode bidi class" bidi-class)
           (const :tag "Unicode decomposition mapping" decomposition)
           (const :tag "Unicode decimal digit value" decimal-digit-value)
           (const :tag "Unicode digit value" digit-value)
           (const :tag "Unicode numeric value" numeric-value)
           (const :tag "Unicode mirrored" mirrored)
           (const :tag "Unicode ISO 10646 comment" iso-10646-comment)
           (const :tag "Unicode simple uppercase mapping" uppercase)
           (const :tag "Unicode simple lowercase mapping" lowercase)
           (const :tag "Unicode simple titlecase mapping" titlecase))))

(defcustom describe-char-unicodedata-file nil
  "Location of Unicode data file.
This is the UnicodeData.txt file from the Unicode Consortium, used for
diagnostics.  If it is non-nil `describe-char' will print data
looked up from it.  This facility is mostly of use to people doing
multilingual development.

This is a fairly large file, not typically present on GNU systems.
At the time of writing it is at the URL
`http://www.unicode.org/Public/UNIDATA/UnicodeData.txt'."
  :group 'mule
  :version "22.1"
  :type '(choice (const :tag "None" nil)
		 file))

(defun describe-char-unicode-data (char)
  "Return a list of Unicode data for Unicode CHAR.
Each element is a list of a property description and the property value.
The list is null if CHAR isn't found in `describe-char-unicodedata-file'.
This function is semi-obsolete.  Use `get-char-code-property'."
  (when describe-char-unicodedata-file
    (unless (file-exists-p describe-char-unicodedata-file)
      (error "`unicodedata-file' %s not found" describe-char-unicodedata-file))
    (with-current-buffer (get-buffer-create " *Unicode Data*")
      (when (zerop (buffer-size))
	;; Don't use -literally in case of DOS line endings.
	(insert-file-contents describe-char-unicodedata-file))
      (goto-char (point-min))
      (let ((hex (format "%04X" char))
	    found first last)
	(if (re-search-forward (concat "^" hex) nil t)
	    (setq found t)
	  ;; It's not listed explicitly.  Look for ranges, e.g. CJK
	  ;; ideographs, and check whether it's in one of them.
	  (while (and (re-search-forward "^\\([^;]+\\);[^;]+First>;" nil t)
		      (>= char (setq first
				     (string-to-number (match-string 1) 16)))
		      (progn
			(forward-line 1)
			(looking-at "^\\([^;]+\\);[^;]+Last>;")
			(> char
			   (setq last
				 (string-to-number (match-string 1) 16))))))
	  (if (and (>= char first)
		   (<= char last))
	      (setq found t)))
	(if found
	    (let ((fields (mapcar (lambda (elt)
				    (if (> (length elt) 0)
					elt))
				  (cdr (split-string
					(buffer-substring
					 (line-beginning-position)
					 (line-end-position))
					";")))))
	      ;; The length depends on whether the last field was empty.
	      (unless (or (= 13 (length fields))
			  (= 14 (length fields)))
		(error "Invalid contents in %s" describe-char-unicodedata-file))
	      ;; The field names and values lists are slightly
	      ;; modified from Mule-UCS unidata.el.
	      (list
	       (list "Name" (let ((name (nth 0 fields)))
			      ;; Check for <..., First>, <..., Last>
			      (if (string-match "\\`\\(<[^,]+\\)," name)
				  (concat (match-string 1 name) ">")
				name)))
	       (list "Category"
		     (let ((val (nth 1 fields)))
		       (or (char-code-property-description
			    'general-category (intern val))
			   val)))
	       (list "Combining class"
		     (let ((val (nth 1 fields)))
		       (or (char-code-property-description
			    'canonical-combining-class (intern val))
			   val)))
	       (list "Bidi category"
		     (let ((val (nth 1 fields)))
		       (or (char-code-property-description
			    'bidi-class (intern val))
			   val)))
	       (list
		"Decomposition"
		(if (nth 4 fields)
		    (let* ((parts (split-string (nth 4 fields)))
			   (info (car parts)))
		      (if (string-match "\\`<\\(.+\\)>\\'" info)
			  (setq info (match-string 1 info))
			(setq info nil))
		      (if info (setq parts (cdr parts)))
		      (setq parts (mapconcat
				   (lambda (arg)
				     (string (string-to-number arg 16)))
				   parts " "))
		      (concat info (if info " ") parts))))
	       (list "Decimal digit value"
		     (nth 5 fields))
	       (list "Digit value"
		     (nth 6 fields))
	       (list "Numeric value"
		     (nth 7 fields))
	       (list "Mirrored"
		     (if (equal "Y" (nth 8 fields))
			 "yes"))
	       (list "Old name" (nth 9 fields))
	       (list "ISO 10646 comment" (nth 10 fields))
	       (list "Uppercase" (and (nth 11 fields)
				      (string (string-to-number
					       (nth 11 fields) 16))))
	       (list "Lowercase" (and (nth 12 fields)
				      (string (string-to-number
					       (nth 12 fields) 16))))
	       (list "Titlecase" (and (nth 13 fields)
				      (string (string-to-number
					       (nth 13 fields) 16)))))))))))

;; Not defined on builds without X, but behind display-graphic-p.
(declare-function internal-char-font "fontset.c" (position &optional ch))

;; Return information about how CHAR is displayed at the buffer
;; position POS.  If the selected frame is on a graphic display,
;; return a string "FONT-DRIVER:FONT-NAME (GLYPH-CODE)" where:
;;   FONT-DRIVER is the font-driver name,
;;   FONT-NAME is the font name,
;;   GLYPH-CODE is a hexadigit string representing the glyph-ID.
;; Otherwise, return a string describing the terminal codes for the
;; character.
(defun describe-char-display (pos char)
  (if (display-graphic-p (selected-frame))
      (let ((char-font-info (internal-char-font pos char)))
	(if char-font-info
	    (let ((type (font-get (car char-font-info) :type))
		  (name (font-xlfd-name (car char-font-info)))
		  (code (cdr char-font-info)))
	       (if (integerp code)
		   (format "%s:%s (#x%02X)" type name code)
		 (format "%s:%s (#x%04X%04X)"
			 type name (car code) (cdr code))))))
    (let* ((charset (get-text-property pos 'charset))
	   (coding (or (terminal-coding-system) 'us-ascii))
	   (encoded (encode-coding-char char coding charset)))
      (if encoded
	  (encoded-string-description encoded coding)))))


;; Return a string of CH with composition for padding on both sides.
;; It is displayed without overlapping with the left/right columns.
(defsubst describe-char-padded-string (ch)
  (if (internal-char-font nil ch)
      (compose-string (string ch) 0 1 (format "\t%c\t" ch))
    (string ch)))

;; Return a nicely formatted list of categories; extended category
;; description is added to the category name as a tooltip
(defsubst describe-char-categories (category-set)
  (let ((mnemonics (category-set-mnemonics category-set)))
    (unless (eq mnemonics "")
      (list (mapconcat
	     (lambda (x)
	       (let* ((c (category-docstring x))
		      (doc (if (string-match "\\`\\(.*?\\)\n" c)
			       (propertize (match-string 1 c)
                                           'help-echo
                                           (substring c (1+ (match-end 1))))
			     c)))
		 (format "%c:%s" x doc)))
	     mnemonics ", ")))))

;;;###autoload
(defun describe-char (pos &optional buffer)
  "Describe position POS (interactively, point) and the char after POS.
POS is taken to be in BUFFER, or the current buffer if BUFFER is nil.
The information is displayed in buffer `*Help*'.

The position information includes POS; the total size of BUFFER; the
region limits, if narrowed; the column number; and the horizontal
scroll amount, if the buffer is horizontally scrolled.

The character information includes the character code; charset and
code points in it; syntax; category; how the character is encoded in
BUFFER and in BUFFER's file; character composition information (if
relevant); the font and font glyphs used to display the character;
the character's canonical name and other properties defined by the
Unicode Data Base; and widgets, buttons, overlays, and text properties
relevant to POS."
  (interactive "d")
  (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
  (let ((src-buf (current-buffer)))
    (set-buffer buffer)
    (if (>= pos (point-max))
        (error "No character follows specified position"))
    (let* ((char (char-after pos))
           (eight-bit-p (and (not enable-multibyte-characters) (>= char 128)))
           (charset (if eight-bit-p 'eight-bit
                      (or (get-text-property pos 'charset)
                          (char-charset char))))
           (composition (find-composition pos nil nil t))
           (component-chars nil)
           (display-table (or (window-display-table)
                              buffer-display-table
                              standard-display-table))
           (disp-vector (and display-table (aref display-table char)))
           (multibyte-p enable-multibyte-characters)
           (overlays (mapcar (lambda (o) (overlay-properties o))
                             (overlays-at pos)))
           (char-description (if (not multibyte-p)
                                 (single-key-description char)
                               (if (< char 128)
                                   (single-key-description char)
                                 (string-to-multibyte
                                  (char-to-string char)))))
           (text-props-desc
            (let ((tmp-buf (generate-new-buffer " *text-props*")))
              (unwind-protect
                  (progn
                    (describe-text-properties pos tmp-buf)
                    (with-current-buffer tmp-buf (buffer-string)))
                (kill-buffer tmp-buf))))
           item-list max-width code)

      (if multibyte-p
          (or (setq code (encode-char char charset))
              (setq charset (char-charset char)
                    code (encode-char char charset)))
        (setq code char))
      (cond
       ;; Append a PDF character to directional embeddings and
       ;; overrides, to prevent potential messup of the following
       ;; text.
       ((memq char '(?\x202a ?\x202b ?\x202d ?\x202e))
	(setq char-description
	      (concat char-description
		      (propertize (string ?\x202c) 'invisible t))))
       ;; Append a LRM character to any strong character to avoid
       ;; messing up the numerical codepoint.
       ((memq (get-char-code-property char 'bidi-class) '(R AL))
	(setq char-description
	      (concat char-description
		      (propertize (string ?\x200e) 'invisible t)))))
      (when composition
        ;; When the composition is trivial (i.e. composed only with the
        ;; current character itself without any alternate characters),
        ;; we don't show the composition information.  Otherwise, store
        ;; two descriptive strings in the first two elements of
        ;; COMPOSITION.
        (or (catch 'tag
              (let ((from (car composition))
                    (to (nth 1 composition))
                    (components (nth 2 composition))
                    ch)
                (if (and (vectorp components) (vectorp (aref components 0)))
                    (let ((idx (- pos from))
                          (nglyphs (lgstring-glyph-len components))
                          (i 0) j glyph glyph-from)
                      ;; COMPONENTS is a gstring.  Find a grapheme
                      ;; cluster containing the current character.
                      (while (and (< i nglyphs)
                                  (setq glyph (lgstring-glyph components i))
                                  (< (lglyph-to glyph) idx))
                        (setq i (1+ i)))
                      (if (or (not glyph) (= i nglyphs))
                          ;; The composition is broken.
                          (throw 'tag nil))
                      (setq glyph-from (lglyph-from glyph)
                            to (+ from (lglyph-to glyph) 1)
                            from (+ from glyph-from)
                            j i)
                      (while (and (< j nglyphs)
                                  (setq glyph (lgstring-glyph components j))
                                  (= (lglyph-from glyph) glyph-from))
                        (setq j (1+ j)))
                      (if (and (= to (1+ from))
                               (= i (1- j))
                               (setq glyph (lgstring-glyph components i))
                               (= char (lglyph-char glyph)))
                          ;; The composition is trivial.
                          (throw 'tag nil))
                      (nconc composition (list i (1- j))))
                  (dotimes (i (length components))
                    (if (integerp (setq ch (aref components i)))
                        (push (cons ch (describe-char-display pos ch))
                              component-chars)))
                  (setq component-chars (nreverse component-chars)))
                (if (< from pos)
                    (if (< (1+ pos) to)
                        (setcar composition
                                (concat
                                 " with the surrounding characters \""
                                 (mapconcat 'describe-char-padded-string
                                            (buffer-substring from pos) "")
                                 "\" and \""
                                 (mapconcat 'describe-char-padded-string
                                            (buffer-substring (1+ pos) to) "")
                                 "\""))
                      (setcar composition
                              (concat
                               " with the preceding character(s) \""
                               (mapconcat 'describe-char-padded-string
                                          (buffer-substring from pos) "")
                               "\"")))
                  (if (< (1+ pos) to)
                      (setcar composition
                              (concat
                               " with the following character(s) \""
                               (mapconcat 'describe-char-padded-string
                                          (buffer-substring (1+ pos) to) "")
                               "\""))
                    (setcar composition nil)))
                (setcar (cdr composition)
                        (format "composed to form \"%s\" (see below)"
                                (buffer-substring from to)))))
            (setq composition nil)))

      (setq item-list
            `(("position"
               ,(let* ((beg      (point-min))
                       (end      (point-max))
                       (total    (buffer-size))
                       (percent  (if (> total 50000) ; Avoid overflow multiplying by 100
                                     (/ (+ (/ total 200) (1- pos))  (max (/ total 100) 1))
                                   (/ (+ (/ total 2) (* 100 (1- pos)))  (max total 1))))
                       (hscroll  (if (= (window-hscroll) 0)
                                     ""
                                   (format ", Hscroll: %d" (window-hscroll))))
                       (col      (current-column)))
                  (if (or (/= beg 1)  (/= end (1+ total)))
                      (format "%d of %d (%d%%), restriction: <%d-%d>, column: %d%s"
                              pos total percent col beg end hscroll)
                    (if (= pos end)
                        (format "%d of %d (EOB), column: %d%s" pos total col hscroll)
                      (format "%d of %d (%d%%), column: %d%s"
                              pos total percent col hscroll)))))
              ("character"
               ,(format "%s (displayed as %s) (codepoint %d, #o%o, #x%x)"
			char-description
                        (apply 'propertize char-description
                               (text-properties-at pos))
                        char char char))
              ("preferred charset"
               ,`(insert-text-button
                  ,(symbol-name charset)
                  'type 'help-character-set 'help-args '(,charset))
               ,(format "(%s)" (charset-description charset)))
              ("code point in charset"
               ,(let ((str (if (integerp code)
                               (format (if (< code 256) "0x%02X" "0x%04X")
                                       code)
                             (format "0x%04X%04X" (car code) (cdr code)))))
                  (if (<= (charset-dimension charset) 2)
                      `(insert-text-button
                        ,str
                        'action (lambda (&rest ignore)
                                  (list-charset-chars ',charset)
                                  (with-selected-window
                                      (get-buffer-window "*Character List*" 0)
                                    (goto-char (point-min))
                                    (forward-line 2) ;Skip the header.
                                    (let ((case-fold-search nil))
                                      (if (search-forward
                                           ,(char-to-string char) nil t)
                                          (goto-char (match-beginning 0))))))
                        'follow-link t
                        'help-echo
                        "mouse-2, RET: show this character in its character set")
                    str)))
              ("syntax"
               ,(let ((syntax (syntax-after pos)))
                  (with-temp-buffer
                    (internal-describe-syntax-value syntax)
                    (buffer-string))))
              ("category"
               ,@(if (not eight-bit-p)
                     (let ((category-set (char-category-set char)))
                       (if category-set
                           (describe-char-categories category-set)
                         '("-- none --")))))
              ("to input"
               ,@(if (not eight-bit-p)
                     (let ((key-list (and (eq input-method-function
                                              'quail-input-method)
                                          (quail-find-key char))))
                       (if (consp key-list)
                           (list "type"
                                 (concat "\""
                                         (mapconcat 'identity
                                                    key-list "\" or \"")
                                         "\"")
                                 "with"
                                 `(insert-text-button
                                   ,current-input-method
                                   'type 'help-input-method
                                   'help-args '(,current-input-method)))))))
              ("buffer code"
               ,(if multibyte-p
                    (encoded-string-description
                     (string-as-unibyte (char-to-string char)) nil)
                  (format "#x%02X" char)))
              ("file code"
               ,@(if multibyte-p
                     (let* ((coding buffer-file-coding-system)
                            (encoded (encode-coding-char char coding charset)))
                       (if encoded
                           (list (encoded-string-description encoded coding)
                                 (format "(encoded by coding system %S)"
                                         coding))
                         (list "not encodable by coding system"
                               (symbol-name coding))))
                   (list (format "#x%02X" char))))
              ("display"
               ,(cond
                 (disp-vector
                  (setq disp-vector (copy-sequence disp-vector))
                  (dotimes (i (length disp-vector))
                    (aset disp-vector i
                          (cons (aref disp-vector i)
                                (describe-char-display
                                 pos (glyph-char (aref disp-vector i))))))
                  (format "by display table entry [%s] (see below)"
                          (mapconcat
                           (lambda (x)
                             (format "?%c" (glyph-char (car x))))
                           disp-vector " ")))
                 (composition
                  (cadr composition))
                 (t
                  (let ((display (describe-char-display pos char)))
                    (if (display-graphic-p (selected-frame))
                        (if display
                            (concat "by this font (glyph code)\n    " display)
                          "no font available")
                      (if display
                          (format "terminal code %s" display)
                        "not encodable for terminal"))))))
              ,@(let ((face
                       (if (not (or disp-vector composition))
                           (cond
                            ((and show-trailing-whitespace
                                  (save-excursion (goto-char pos)
                                                  (looking-at-p "[ \t]+$")))
                             'trailing-whitespace)
                            ((and nobreak-char-display char (eq char '#xa0))
                             'nobreak-space)
                            ((and nobreak-char-display char
				  (memq char '(#xad #x2010 #x2011)))
                             'escape-glyph)
                            ((and (< char 32) (not (memq char '(9 10))))
                             'escape-glyph)))))
                  (if face (list (list "hardcoded face"
                                       `(insert-text-button
                                         ,(symbol-name face)
                                         'type 'help-face
                                         'help-args '(,face))))))
              ,@(if (not eight-bit-p)
                    (let ((unicodedata (describe-char-unicode-data char)))
                      (if unicodedata
                          (cons (list "Unicode data" "") unicodedata))))))
      (setq max-width (apply 'max (mapcar (lambda (x)
                                            (if (cadr x) (length (car x)) 0))
                                          item-list)))
      (set-buffer src-buf)
      (help-setup-xref (list 'describe-char pos buffer)
                       (called-interactively-p 'interactive))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (set-buffer-multibyte multibyte-p)
          (let ((formatter (format "%%%ds:" max-width)))
            (dolist (elt item-list)
              (when (cadr elt)
                (insert (format formatter (car elt)))
                (dolist (clm (cdr elt))
                  (if (eq (car-safe clm) 'insert-text-button)
                      (progn (insert " ") (eval clm))
                    (when (>= (+ (current-column)
                                 (or (string-match-p "\n" clm)
                                     (string-width clm))
                                 1)
                              (window-width))
                      (insert "\n")
                      (indent-to (1+ max-width)))
                    (unless (zerop (length clm))
                      (insert " " clm))))
                (insert "\n"))))

          (when overlays
            (save-excursion
              (goto-char (point-min))
              (re-search-forward "character:[ \t\n]+")
              (let ((end (+ (point) (length char-description))))
                (mapc (lambda (props)
                        (let ((o (make-overlay (point) end)))
                          (while props
                            (overlay-put o (car props) (nth 1 props))
                            (setq props (cddr props)))))
                      overlays))))

          (when disp-vector
            (insert
             "\nThe display table entry is displayed by ")
            (if (display-graphic-p (selected-frame))
                (progn
                  (insert "these fonts (glyph codes):\n")
                  (dotimes (i (length disp-vector))
                    (insert (glyph-char (car (aref disp-vector i))) ?:
                            (propertize " " 'display '(space :align-to 5))
                            (or (cdr (aref disp-vector i)) "-- no font --")
                            "\n")
                    (let ((face (glyph-face (car (aref disp-vector i)))))
                      (when face
                        (insert (propertize " " 'display '(space :align-to 5))
                                "face: ")
                        (insert (concat "`" (symbol-name face) "'"))
                        (insert "\n")))))
              (insert "these terminal codes:\n")
              (dotimes (i (length disp-vector))
                (insert (car (aref disp-vector i))
                        (propertize " " 'display '(space :align-to 5))
                        (or (cdr (aref disp-vector i)) "-- not encodable --")
                        "\n"))))

          (when composition
            (insert "\nComposed")
            (if (car composition)
                (insert (car composition)))
            (if (and (vectorp (nth 2 composition))
                     (vectorp (aref (nth 2 composition) 0)))
                (let* ((gstring (nth 2 composition))
                       (font (lgstring-font gstring))
                       (from (nth 3 composition))
                       (to (nth 4 composition))
                       glyph)
                  (if (fontp font)
                      (progn
                        (insert " using this font:\n  "
                                (symbol-name (font-get font :type))
                                ?:
                                (aref (query-font font) 0)
                                "\nby these glyphs:\n")
                        (while (and (<= from to)
                                    (setq glyph (lgstring-glyph gstring from)))
                          (insert (format "  %S\n" glyph))
                          (setq from (1+ from))))
                    (insert " by these characters:\n")
                    (while (and (<= from to)
                                (setq glyph (lgstring-glyph gstring from)))
                      (insert (format " %c (#x%d)\n"
                                      (lglyph-char glyph) (lglyph-char glyph)))
                      (setq from (1+ from)))))
              (insert " by the rule:\n\t(")
              (let ((first t))
                (mapc (lambda (x)
                        (if first (setq first nil)
                          (insert " "))
                        (if (consp x) (insert (format "%S" x))
                          (if (= x ?\t) (insert (single-key-description x))
                            (insert ??)
                            (insert (describe-char-padded-string x)))))
                      (nth 2 composition)))
              (insert  ")\nThe component character(s) are displayed by ")
              (if (display-graphic-p (selected-frame))
                  (progn
                    (insert "these fonts (glyph codes):")
                    (dolist (elt component-chars)
                      (if (/= (car elt) ?\t)
                          (insert "\n "
                                  (describe-char-padded-string (car elt))
                                  ?:
                                  (propertize " "
                                              'display '(space :align-to 5))
                                  (or (cdr elt) "-- no font --")))))
                (insert "these terminal codes:")
                (dolist (elt component-chars)
                  (insert "\n  " (car elt) ":"
                          (propertize " " 'display '(space :align-to 4))
                          (or (cdr elt) "-- not encodable --"))))
              (insert "\nSee the variable `reference-point-alist' for "
                      "the meaning of the rule.\n")))

          (unless eight-bit-p
            (insert (if (not describe-char-unidata-list)
                        "\nCharacter code properties are not shown: "
                      "\nCharacter code properties: "))
            (insert-text-button
             "customize what to show"
             'action (lambda (&rest _ignore)
                       (customize-variable
                        'describe-char-unidata-list))
             'follow-link t)
            (insert "\n")
            (dolist (elt (if (eq describe-char-unidata-list t)
                             (nreverse (mapcar 'car char-code-property-alist))
                           describe-char-unidata-list))
              (let ((val (get-char-code-property char elt))
                    description)
                (when val
                  (setq description (char-code-property-description elt val))
                  (insert (if description
                              (format "  %s: %s (%s)\n" elt val description)
                            (format "  %s: %s\n" elt val)))))))

          (if text-props-desc (insert text-props-desc))
          (toggle-read-only 1))))))

(define-obsolete-function-alias 'describe-char-after 'describe-char "22.1")

(provide 'descr-text)

;;; descr-text.el ends here
