;;; enriched.el --- read and save files in text/enriched format

;; Copyright (C) 1994-1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Boris Goldowsky <boris@gnu.org>
;; Keywords: wp, faces

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

;; This file implements reading, editing, and saving files with
;; text-properties such as faces, levels of indentation, and true line
;; breaks distinguished from newlines just used to fit text into the window.

;; The file format used is the MIME text/enriched format, which is a
;; standard format defined in internet RFC 1563.  All standard annotations
;; are supported except for <smaller> and <bigger>, which are currently not
;; possible to display.

;; A separate file, enriched.doc, contains further documentation and other
;; important information about this code.  It also serves as an example
;; file in text/enriched format.  It should be in the etc directory of your
;; emacs distribution.

;;; Code:

(provide 'enriched)

;;;
;;; Variables controlling the display
;;;

(defgroup enriched nil
  "Read and save files in text/enriched format."
  :group 'wp)

(defcustom enriched-verbose t
  "If non-nil, give status messages when reading and writing files."
  :type 'boolean
  :group 'enriched)

;;;
;;; Set up faces & display table
;;;

;; Emacs doesn't have a "fixed" face by default, since all faces currently
;; have to be fixed-width.  So we just pick one that looks different from the
;; default.
(defface fixed
  '((t (:weight bold)))
  "Face used for text that must be shown in fixed width.
Currently, Emacs can only display fixed-width fonts, but this may change.
This face is used for text specifically marked as fixed-width, for example
in text/enriched files."
  :group 'enriched)

(defface excerpt
  '((t (:slant italic)))
  "Face used for text that is an excerpt from another document.
This is used in Enriched mode for text explicitly marked as an excerpt."
  :group 'enriched)

(defconst enriched-display-table (or (copy-sequence standard-display-table)
				     (make-display-table)))
(aset enriched-display-table ?\f (make-vector (1- (frame-width)) ?-))

(defconst enriched-par-props '(left-margin right-margin justification)
  "Text-properties that usually apply to whole paragraphs.
These are set front-sticky everywhere except at hard newlines.")

;;;
;;; Variables controlling the file format
;;;   (bidirectional)

(defconst enriched-initial-annotation
  (lambda ()
    (format "Content-Type: text/enriched\nText-Width: %d\n\n"
	    fill-column))
  "What to insert at the start of a text/enriched file.
If this is a string, it is inserted.  If it is a list, it should be a lambda
expression, which is evaluated to get the string to insert.")

(defconst enriched-annotation-format "<%s%s>"
  "General format of enriched-text annotations.")

(defconst enriched-annotation-regexp "<\\(/\\)?\\([-A-Za-z0-9]+\\)>"
  "Regular expression matching enriched-text annotations.")

(defvar enriched-translations
  '((face          (bold-italic "bold" "italic")
		   (bold        "bold")
		   (italic      "italic")
		   (underline   "underline")
		   (fixed       "fixed")
		   (excerpt     "excerpt")
		   (default     )
		   (nil         enriched-encode-other-face))
    (left-margin   (4           "indent"))
    (right-margin  (4           "indentright"))
    (justification (none        "nofill")
		   (right       "flushright")
		   (left        "flushleft")
		   (full        "flushboth")
		   (center      "center"))
    (PARAMETER     (t           "param")) ; Argument of preceding annotation
    ;; The following are not part of the standard:
    (FUNCTION      (enriched-decode-foreground "x-color")
		   (enriched-decode-background "x-bg-color")
		   (enriched-decode-display-prop "x-display"))
    (read-only     (t           "x-read-only"))
    (display	   (nil		enriched-handle-display-prop))
    (unknown       (nil         format-annotate-value))
;   (font-size     (2           "bigger")       ; unimplemented
;		   (-2          "smaller"))
)
  "List of definitions of text/enriched annotations.
See `format-annotate-region' and `format-deannotate-region' for the definition
of this structure.")

(defconst enriched-ignore
  '(front-sticky rear-nonsticky hard)
  "Properties that are OK to ignore when saving text/enriched files.
Any property that is neither on this list nor dealt with by
`enriched-translations' will generate a warning.")

;;; Internal variables

(defcustom enriched-mode-hook nil
  "Hook run after entering/leaving Enriched mode.
If you set variables in this hook, you should arrange for them to be restored
to their old values if you leave Enriched mode.  One way to do this is to add
them and their old values to `enriched-old-bindings'."
  :type 'hook
  :group 'enriched)

(defvar enriched-old-bindings nil
  "Store old variable values that we change when entering mode.
The value is a list of \(VAR VALUE VAR VALUE...).")
(make-variable-buffer-local 'enriched-old-bindings)

;; The next variable is buffer local if and only if Enriched mode is
;; enabled.  The buffer local value records whether
;; `default-text-properties' should remain buffer local when disabling
;; Enriched mode.  For technical reasons, the default value should be t.
(defvar enriched-default-text-properties-local-flag t)

;; Technical internal variable.  Bound to t if `enriched-mode' is
;; being rerun by a major mode to allow it to restore buffer-local
;; variables and to correctly update `enriched-old-bindings'.
(defvar enriched-rerun-flag nil)

;;;
;;; Keybindings
;;;

(defvar enriched-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap move-beginning-of-line] 'beginning-of-line-text)
    (define-key map "\C-m" 'reindent-then-newline-and-indent)
    (define-key map
      [remap newline-and-indent] 'reindent-then-newline-and-indent)
    (define-key map "\M-j" 'facemenu-justification-menu)
    (define-key map "\M-S" 'set-justification-center)
    (define-key map "\C-x\t" 'increase-left-margin)
    (define-key map "\C-c[" 'set-left-margin)
    (define-key map "\C-c]" 'set-right-margin)
    map)
  "Keymap for Enriched mode.")

;;;
;;; Define the mode
;;;

(put 'enriched-mode 'permanent-local t)
;;;###autoload
(define-minor-mode enriched-mode
  "Minor mode for editing text/enriched files.
These are files with embedded formatting information in the MIME standard
text/enriched format.

With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.

Turning the mode on or off runs `enriched-mode-hook'.

More information about Enriched mode is available in the file
etc/enriched.doc in the Emacs distribution directory.

Commands:

\\{enriched-mode-map}"
  :group 'enriched :lighter " Enriched"
  (cond ((null enriched-mode)
	 ;; Turn mode off
         (remove-hook 'change-major-mode-hook
                      'enriched-before-change-major-mode 'local)
	 (setq buffer-file-format (delq 'text/enriched buffer-file-format))
	 ;; restore old variable values
	 (while enriched-old-bindings
	   (set (pop enriched-old-bindings) (pop enriched-old-bindings)))
	 (unless enriched-default-text-properties-local-flag
	   (kill-local-variable 'default-text-properties))
	 (kill-local-variable 'enriched-default-text-properties-local-flag)
	 (unless use-hard-newlines (use-hard-newlines 0)))

	((and (memq 'text/enriched buffer-file-format)
	      (not enriched-rerun-flag))
	 ;; Mode already on; do nothing.
	 nil)

	(t				; Turn mode on
         (add-hook 'change-major-mode-hook
                   'enriched-before-change-major-mode nil 'local)
	 (add-to-list 'buffer-file-format 'text/enriched)
	 ;; Save old variable values before we change them.
	 ;; These will be restored if we exit Enriched mode.
	 (setq enriched-old-bindings
	       (list 'buffer-display-table buffer-display-table
		     'default-text-properties default-text-properties
		     'use-hard-newlines use-hard-newlines))
	 (make-local-variable 'enriched-default-text-properties-local-flag)
	 (setq enriched-default-text-properties-local-flag
	       (local-variable-p 'default-text-properties))
	 (make-local-variable 'default-text-properties)
	 (setq buffer-display-table  enriched-display-table)
	 (use-hard-newlines 1 (if enriched-rerun-flag 'never nil))
	 (let ((sticky (plist-get default-text-properties 'front-sticky))
	       (p enriched-par-props))
	   (dolist (x p)
	     (add-to-list 'sticky x))
	   (if sticky
	       (setq default-text-properties
		     (plist-put default-text-properties
				'front-sticky sticky)))))))

(defun enriched-before-change-major-mode ()
  (when enriched-mode
    (while enriched-old-bindings
      (set (pop enriched-old-bindings) (pop enriched-old-bindings)))))

(defun enriched-after-change-major-mode ()
  (when enriched-mode
    (let ((enriched-rerun-flag t))
      (enriched-mode 1))))

(add-hook 'after-change-major-mode-hook 'enriched-after-change-major-mode)


(fset 'enriched-mode-map enriched-mode-map)

;;;
;;; Some functions dealing with text-properties, especially indentation
;;;

(defun enriched-map-property-regions (prop func &optional from to)
  "Apply a function to regions of the buffer based on a text property.
For each contiguous region of the buffer for which the value of PROPERTY is
eq, the FUNCTION will be called.  Optional arguments FROM and TO specify the
region over which to scan.

The specified function receives three arguments: the VALUE of the property in
the region, and the START and END of each region."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (let ((begin (point))
	    end
	    (marker (make-marker))
	    (val (get-text-property (point) prop)))
	(while (setq end (text-property-not-all begin (point-max) prop val))
	  (move-marker marker end)
	  (funcall func val begin (marker-position marker))
	  (setq begin (marker-position marker)
		val (get-text-property marker prop)))
	(if (< begin (point-max))
	    (funcall func val begin (point-max)))))))

(put 'enriched-map-property-regions 'lisp-indent-hook 1)

(defun enriched-insert-indentation (&optional from to)
  "Indent and justify each line in the region."
  (save-excursion
    (save-restriction
      (if to (narrow-to-region (point-min) to))
      (goto-char (or from (point-min)))
      (if (not (bolp)) (forward-line 1))
      (while (not (eobp))
	(if (eolp)
	    nil ; skip blank lines
	  (indent-to (current-left-margin))
	  (justify-current-line t nil t))
	(forward-line 1)))))

;;;
;;; Encoding Files
;;;

;;;###autoload
(defun enriched-encode (from to orig-buf)
  (if enriched-verbose (message "Enriched: encoding document..."))
  (let ((inhibit-read-only t))
    (save-restriction
      (narrow-to-region from to)
      (delete-to-left-margin)
      (unjustify-region)
      (goto-char from)
      (format-replace-strings '(("<" . "<<")))
      (format-insert-annotations
       (format-annotate-region from (point-max) enriched-translations
			       'enriched-make-annotation enriched-ignore))
      (goto-char from)
      (insert (if (stringp enriched-initial-annotation)
		  enriched-initial-annotation
		(save-excursion
		  ;; Eval this in the buffer we are annotating.  This
		  ;; fixes a bug which was saving incorrect File-Width
		  ;; information, since we were looking at local
		  ;; variables in the wrong buffer.
		  (if orig-buf (set-buffer orig-buf))
		  (funcall enriched-initial-annotation))))
      (enriched-map-property-regions 'hard
	(lambda (v b e)
	  (if (and v (= ?\n (char-after b)))
	      (progn (goto-char b) (insert "\n"))))
	(point) nil)
      (if enriched-verbose (message nil))
      ;; Return new end.
      (point-max))))

(defun enriched-make-annotation (internal-ann positive)
  "Format an annotation INTERNAL-ANN.
INTERNAL-ANN may be a string, for a flag, or a list of the form (PARAM VALUE).
If POSITIVE is non-nil, this is the opening annotation;
if nil, the matching close."
  (cond ((stringp internal-ann)
	 (format enriched-annotation-format (if positive "" "/") internal-ann))
	;; Otherwise it is an annotation with parameters, represented as a list
	(positive
	 (let ((item (car internal-ann))
	       (params (cdr internal-ann)))
	   (concat (format enriched-annotation-format "" item)
		   (mapconcat (lambda (i) (concat "<param>" i "</param>"))
			      params ""))))
	(t (format enriched-annotation-format "/" (car internal-ann)))))

(defun enriched-encode-other-face (old new)
  "Generate annotations for random face change.
One annotation each for foreground color, background color, italic, etc."
  (cons (and old (enriched-face-ans old))
	(and new (enriched-face-ans new))))

(defun enriched-face-ans (face)
  "Return annotations specifying FACE.
FACE may be a list of faces instead of a single face;
it can also be anything allowed as an element of a list
which can be the value of the `face' text property."
  (cond ((and (consp face) (eq (car face) 'foreground-color))
	 (list (list "x-color" (cdr face))))
	((and (consp face) (eq (car face) 'background-color))
	 (list (list "x-bg-color" (cdr face))))
	((and (listp face) (eq (car face) :foreground))
	 (list (list "x-color" (cadr face))))
	((and (listp face) (eq (car face) :background))
	 (list (list "x-bg-color" (cadr face))))
	((listp face)
	 (apply 'append (mapcar 'enriched-face-ans face)))
	((let* ((fg (face-attribute face :foreground))
		(bg (face-attribute face :background))
		(props (face-font face t))
		(ans (cdr (format-annotate-single-property-change
			   'face nil props enriched-translations))))
	   (unless (eq fg 'unspecified)
	     (setq ans (cons (list "x-color" fg) ans)))
	   (unless (eq bg 'unspecified)
	     (setq ans (cons (list "x-bg-color" bg) ans)))
	   ans))))

;;;
;;; Decoding files
;;;

;;;###autoload
(defun enriched-decode (from to)
  (if enriched-verbose (message "Enriched: decoding document..."))
  (use-hard-newlines 1 'never)
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)

      ;; Deal with header
      (let ((file-width (enriched-get-file-width)))
	(enriched-remove-header)

	;; Deal with newlines
	(while (search-forward-regexp "\n\n+" nil t)
	  (if (current-justification)
	      (delete-char -1))
	  (set-hard-newline-properties (match-beginning 0) (point)))

	;; Translate annotations
	(format-deannotate-region from (point-max) enriched-translations
				  'enriched-next-annotation)

	;; Indent or fill the buffer
	(cond (file-width		; File was filled to this width
	       (setq fill-column file-width)
	       (if enriched-verbose (message "Indenting..."))
	       (enriched-insert-indentation))
	      (t			; File was not filled.
	       (if enriched-verbose (message "Filling paragraphs..."))
	       (fill-region (point-min) (point-max))))
	(if enriched-verbose (message nil)))
      (point-max))))

(defun enriched-next-annotation ()
  "Find and return next text/enriched annotation.
Any \"<<\" strings encountered are converted to \"<\".
Return value is \(begin end name positive-p), or nil if none was found."
  (while (and (search-forward "<" nil 1)
	      (progn (goto-char (match-beginning 0))
		     (not (looking-at enriched-annotation-regexp))))
    (forward-char 1)
    (if (= ?< (char-after (point)))
	(delete-char 1)
      ;; A single < that does not start an annotation is an error,
      ;; which we note and then ignore.
      (message "Warning: malformed annotation in file at %s"
	       (1- (point)))))
  (if (not (eobp))
      (let* ((beg (match-beginning 0))
	     (end (match-end 0))
	     (name (downcase (buffer-substring
			      (match-beginning 2) (match-end 2))))
	     (pos (not (match-beginning 1))))
	(list beg end name pos))))

(defun enriched-get-file-width ()
  "Look for file width information on this line."
  (save-excursion
    (if (search-forward "Text-Width: " (+ (point) 1000) t)
	(read (current-buffer)))))

(defun enriched-remove-header ()
  "Remove file-format header at point."
  (while (looking-at "^[-A-Za-z]+: .*\n")
    (delete-region (point) (match-end 0)))
  (if (looking-at "^\n")
      (delete-char 1)))

(defun enriched-decode-foreground (from to &optional color)
  (if color
      (list from to 'face (list ':foreground color))
    (message "Warning: no color specified for <x-color>")
    nil))

(defun enriched-decode-background (from to &optional color)
  (if color
      (list from to 'face (list ':background color))
    (message "Warning: no color specified for <x-bg-color>")
    nil))

;;; Handling the `display' property.


(defun enriched-handle-display-prop (old new)
  "Return a list of annotations for a change in the `display' property.
OLD is the old value of the property, NEW is the new value.  Value
is a list `(CLOSE OPEN)', where CLOSE is a list of annotations to
close and OPEN a list of annotations to open.  Each of these lists
has the form `(ANNOTATION PARAM ...)'."
  (let ((annotation "x-display")
	(param (prin1-to-string (or old new))))
    (if (null old)
        (cons nil (list (list annotation param)))
      (cons (list (list annotation param)) nil))))

(defun enriched-decode-display-prop (start end &optional param)
  "Decode a `display' property for text between START and END.
PARAM is a `<param>' found for the property.
Value is a list `(START END SYMBOL VALUE)' with START and END denoting
the range of text to assign text property SYMBOL with value VALUE."
  (let ((prop (when (stringp param)
		(condition-case ()
		    (car (read-from-string param))
		  (error nil)))))
    (unless prop
      (message "Warning: invalid <x-display> parameter %s" param))
    (list start end 'display prop)))

;;; enriched.el ends here
