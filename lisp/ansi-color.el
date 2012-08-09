;;; ansi-color.el --- translate ANSI escape sequences into faces

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 3.4.2
;; Keywords: comm processes terminals services

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

;; This file provides a function that takes a string or a region
;; containing Select Graphic Rendition (SGR) control sequences (formerly
;; known as ANSI escape sequences) and tries to translate these into
;; faces.
;;
;; This allows you to run ls --color=yes in shell-mode.  It is now
;; enabled by default; to disable it, set ansi-color-for-comint-mode
;; to nil.
;;
;; Note that starting your shell from within Emacs might set the TERM
;; environment variable.  The new setting might disable the output of
;; SGR control sequences.  Using ls --color=yes forces ls to produce
;; these.
;;
;; SGR control sequences are defined in section 3.8.117 of the ECMA-48
;; standard (identical to ISO/IEC 6429), which is freely available as a
;; PDF file <URL:http://www.ecma.ch/ecma1/STAND/ECMA-048.HTM>.  The
;; "Graphic Rendition Combination Mode (GRCM)" implemented is
;; "cumulative mode" as defined in section 7.2.8.  Cumulative mode means
;; that whenever possible, SGR control sequences are combined (ie. blue
;; and bold).

;; The basic functions are:
;;
;; `ansi-color-apply' to colorize a string containing SGR control
;; sequences.
;;
;; `ansi-color-filter-apply' to filter SGR control sequences from a
;; string.
;;
;; `ansi-color-apply-on-region' to colorize a region containing SGR
;; control sequences.
;;
;; `ansi-color-filter-region' to filter SGR control sequences from a
;; region.

;;; Thanks

;; Georges Brun-Cottan <gbruncot@emc.com> for improving ansi-color.el
;; substantially by adding the code needed to cope with arbitrary chunks
;; of output and the filter functions.
;;
;; Markus Kuhn <Markus.Kuhn@cl.cam.ac.uk> for pointing me to ECMA-48.
;;
;; Stefan Monnier <foo@acm.com> for explaining obscure font-lock stuff and for
;; code suggestions.



;;; Code:

(defvar comint-last-output-start)

;; Customization

(defgroup ansi-colors nil
  "Translating SGR control sequences to faces.
This translation effectively colorizes strings and regions based upon
SGR control sequences embedded in the text.  SGR (Select Graphic
Rendition) control sequences are defined in section 3.8.117 of the
ECMA-48 standard \(identical to ISO/IEC 6429), which is freely available
as a PDF file <URL:http://www.ecma.ch/ecma1/STAND/ECMA-048.HTM>."
  :version "21.1"
  :group 'processes)

(defcustom ansi-color-faces-vector
  [default bold default italic underline bold bold-italic modeline]
  "Faces used for SGR control sequences determining a face.
This vector holds the faces used for SGR control sequence parameters 0
to 7.

Parameter  Description        Face used by default
  0        default            default
  1        bold               bold
  2        faint              default
  3        italic             italic
  4        underlined         underline
  5        slowly blinking    bold
  6        rapidly blinking   bold-italic
  7        negative image     modeline

Note that the symbol `default' is special: It will not be combined
with the current face.

This vector is used by `ansi-color-make-color-map' to create a color
map.  This color map is stored in the variable `ansi-color-map'."
  :type '(vector face face face face face face face face)
  :set 'ansi-color-map-update
  :initialize 'custom-initialize-default
  :group 'ansi-colors)

(defcustom ansi-color-names-vector
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Colors used for SGR control sequences determining a color.
This vector holds the colors used for SGR control sequences parameters
30 to 37 \(foreground colors) and 40 to 47 (background colors).

Parameter  Color
  30  40   black
  31  41   red
  32  42   green
  33  43   yellow
  34  44   blue
  35  45   magenta
  36  46   cyan
  37  47   white

This vector is used by `ansi-color-make-color-map' to create a color
map.  This color map is stored in the variable `ansi-color-map'.

Each element may also be a cons cell where the car and cdr specify the
foreground and background colors, respectively."
  :type '(vector (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color))
                 (choice color (cons color color)))
  :set 'ansi-color-map-update
  :initialize 'custom-initialize-default
  :group 'ansi-colors)

(defconst ansi-color-regexp "\033\\[\\([0-9;]*m\\)"
  "Regexp that matches SGR control sequences.")

(defconst ansi-color-drop-regexp
  "\033\\[\\([ABCDsuK]\\|2J\\|=[0-9]+[hI]\\|[0-9;]*[Hf]\\)"
  "Regexp that matches ANSI control sequences to silently drop.")

(defconst ansi-color-parameter-regexp "\\([0-9]*\\)[m;]"
  "Regexp that matches SGR control sequence parameters.")


;; Convenience functions for comint modes (eg. shell-mode)


(defcustom ansi-color-for-comint-mode t
  "Determines what to do with comint output.
If nil, do nothing.
If the symbol `filter', then filter all SGR control sequences.
If anything else (such as t), then translate SGR control sequences
into text properties.

In order for this to have any effect, `ansi-color-process-output' must
be in `comint-output-filter-functions'.

This can be used to enable colorized ls --color=yes output
in shell buffers.  You set this variable by calling one of:
\\[ansi-color-for-comint-mode-on]
\\[ansi-color-for-comint-mode-off]
\\[ansi-color-for-comint-mode-filter]"
  :type '(choice (const :tag "Do nothing" nil)
		 (const :tag "Filter" filter)
		 (const :tag "Translate" t))
  :group 'ansi-colors
  :version "23.2")

(defvar ansi-color-apply-face-function 'ansi-color-apply-overlay-face
  "Function for applying an Ansi Color face to text in a buffer.
This function should accept three arguments: BEG, END, and FACE,
and it should apply face FACE to the text between BEG and END.")

;;;###autoload
(defun ansi-color-for-comint-mode-on ()
  "Set `ansi-color-for-comint-mode' to t."
  (interactive)
  (setq ansi-color-for-comint-mode t))

(defun ansi-color-for-comint-mode-off ()
  "Set `ansi-color-for-comint-mode' to nil."
  (interactive)
  (setq ansi-color-for-comint-mode nil))

(defun ansi-color-for-comint-mode-filter ()
  "Set `ansi-color-for-comint-mode' to symbol `filter'."
  (interactive)
  (setq ansi-color-for-comint-mode 'filter))

;;;###autoload
(defun ansi-color-process-output (ignored)
  "Maybe translate SGR control sequences of comint output into text properties.

Depending on variable `ansi-color-for-comint-mode' the comint output is
either not processed, SGR control sequences are filtered using
`ansi-color-filter-region', or SGR control sequences are translated into
text properties using `ansi-color-apply-on-region'.

The comint output is assumed to lie between the marker
`comint-last-output-start' and the process-mark.

This is a good function to put in `comint-output-filter-functions'."
  (let ((start-marker (if (and (markerp comint-last-output-start)
			       (eq (marker-buffer comint-last-output-start)
				   (current-buffer))
			       (marker-position comint-last-output-start))
			  comint-last-output-start
			(point-min-marker)))
	(end-marker (process-mark (get-buffer-process (current-buffer)))))
    (cond ((eq ansi-color-for-comint-mode nil))
	  ((eq ansi-color-for-comint-mode 'filter)
	   (ansi-color-filter-region start-marker end-marker))
	  (t
	   (ansi-color-apply-on-region start-marker end-marker)))))

(defalias 'ansi-color-unfontify-region 'font-lock-default-unfontify-region)
(make-obsolete 'ansi-color-unfontify-region "not needed any more" "24.1")

;; Working with strings
(defvar ansi-color-context nil
  "Context saved between two calls to `ansi-color-apply'.
This is a list of the form (FACES FRAGMENT) or nil.  FACES is a list of
faces the last call to `ansi-color-apply' ended with, and FRAGMENT is a
string starting with an escape sequence, possibly the start of a new
escape sequence.")
(make-variable-buffer-local 'ansi-color-context)

(defun ansi-color-filter-apply (string)
  "Filter out all ANSI control sequences from STRING.

Every call to this function will set and use the buffer-local variable
`ansi-color-context' to save partial escape sequences.  This information
will be used for the next call to `ansi-color-apply'.  Set
`ansi-color-context' to nil if you don't want this.

This function can be added to `comint-preoutput-filter-functions'."
  (let ((start 0) end result)
    ;; if context was saved and is a string, prepend it
    (if (cadr ansi-color-context)
        (setq string (concat (cadr ansi-color-context) string)
              ansi-color-context nil))
    ;; find the next escape sequence
    (while (setq end (string-match ansi-color-regexp string start))
      (setq result (concat result (substring string start end))
	    start (match-end 0)))
    ;; save context, add the remainder of the string to the result
    (let (fragment)
      (if (string-match "\033" string start)
	  (let ((pos (match-beginning 0)))
	    (setq fragment (substring string pos)
		  result (concat result (substring string start pos))))
	(setq result (concat result (substring string start))))
      (setq ansi-color-context (if fragment (list nil fragment))))
    result))

(defun ansi-color-apply (string)
  "Translates SGR control sequences into text properties.
Delete all other control sequences without processing them.

Applies SGR control sequences setting foreground and background colors
to STRING using text properties and returns the result.  The colors used
are given in `ansi-color-faces-vector' and `ansi-color-names-vector'.
See function `ansi-color-apply-sequence' for details.

Every call to this function will set and use the buffer-local variable
`ansi-color-context' to save partial escape sequences and current face.
This information will be used for the next call to `ansi-color-apply'.
Set `ansi-color-context' to nil if you don't want this.

This function can be added to `comint-preoutput-filter-functions'."
  (let ((face (car ansi-color-context))
	(start 0) end escape-sequence result
	colorized-substring)
    ;; If context was saved and is a string, prepend it.
    (if (cadr ansi-color-context)
        (setq string (concat (cadr ansi-color-context) string)
              ansi-color-context nil))
    ;; Find the next escape sequence.
    (while (setq end (string-match ansi-color-regexp string start))
      (setq escape-sequence (match-string 1 string))
      ;; Colorize the old block from start to end using old face.
      (when face
	(put-text-property start end 'font-lock-face face string))
      (setq colorized-substring (substring string start end)
	    start (match-end 0))
      ;; Eliminate unrecognized ANSI sequences.
      (while (string-match ansi-color-drop-regexp colorized-substring)
	(setq colorized-substring
	      (replace-match "" nil nil colorized-substring)))
      (push colorized-substring result)
      ;; Create new face, by applying escape sequence parameters.
      (setq face (ansi-color-apply-sequence escape-sequence face)))
    ;; if the rest of the string should have a face, put it there
    (when face
      (put-text-property start (length string) 'font-lock-face face string))
    ;; save context, add the remainder of the string to the result
    (let (fragment)
      (if (string-match "\033" string start)
	  (let ((pos (match-beginning 0)))
	    (setq fragment (substring string pos))
	    (push (substring string start pos) result))
	(push (substring string start) result))
      (setq ansi-color-context (if (or face fragment) (list face fragment))))
    (apply 'concat (nreverse result))))

;; Working with regions

(defvar ansi-color-context-region nil
  "Context saved between two calls to `ansi-color-apply-on-region'.
This is a list of the form (FACES MARKER) or nil.  FACES is a list of
faces the last call to `ansi-color-apply-on-region' ended with, and
MARKER is a buffer position within an escape sequence or the last
position processed.")
(make-variable-buffer-local 'ansi-color-context-region)

(defun ansi-color-filter-region (begin end)
  "Filter out all ANSI control sequences from region BEGIN to END.

Every call to this function will set and use the buffer-local variable
`ansi-color-context-region' to save position.  This information will be
used for the next call to `ansi-color-apply-on-region'.  Specifically,
it will override BEGIN, the start of the region.  Set
`ansi-color-context-region' to nil if you don't want this."
  (let ((end-marker (copy-marker end))
	(start (or (cadr ansi-color-context-region) begin)))
    (save-excursion
      (goto-char start)
      ;; Delete unrecognized escape sequences.
      (while (re-search-forward ansi-color-drop-regexp end-marker t)
        (replace-match ""))
      (goto-char start)
      ;; Delete SGR escape sequences.
      (while (re-search-forward ansi-color-regexp end-marker t)
        (replace-match ""))
      ;; save context, add the remainder of the string to the result
      (if (re-search-forward "\033" end-marker t)
	  (setq ansi-color-context-region (list nil (match-beginning 0)))
	(setq ansi-color-context-region nil)))))

(defun ansi-color-apply-on-region (begin end)
  "Translates SGR control sequences into overlays or extents.
Delete all other control sequences without processing them.

SGR control sequences are applied by setting foreground and
background colors to the text between BEGIN and END using
overlays.  The colors used are given in `ansi-color-faces-vector'
and `ansi-color-names-vector'.  See `ansi-color-apply-sequence'
for details.

Every call to this function will set and use the buffer-local variable
`ansi-color-context-region' to save position and current face.  This
information will be used for the next call to
`ansi-color-apply-on-region'.  Specifically, it will override BEGIN, the
start of the region and set the face with which to start.  Set
`ansi-color-context-region' to nil if you don't want this."
  (let ((face (car ansi-color-context-region))
	(start-marker (or (cadr ansi-color-context-region)
			  (copy-marker begin)))
	(end-marker (copy-marker end))
	escape-sequence)
    ;; First, eliminate unrecognized ANSI control sequences.
    (save-excursion
      (goto-char start-marker)
      (while (re-search-forward ansi-color-drop-regexp end-marker t)
	(replace-match "")))
    (save-excursion
      (goto-char start-marker)
      ;; Find the next SGR sequence.
      (while (re-search-forward ansi-color-regexp end-marker t)
	;; Colorize the old block from start to end using old face.
	(funcall ansi-color-apply-face-function
		 start-marker (match-beginning 0)
		 face)
        ;; store escape sequence and new start position
        (setq escape-sequence (match-string 1)
	      start-marker (copy-marker (match-end 0)))
	;; delete the escape sequence
	(replace-match "")
	;; create new face by applying all the parameters in the escape
	;; sequence
	(setq face (ansi-color-apply-sequence escape-sequence face)))
      ;; search for the possible start of a new escape sequence
      (if (re-search-forward "\033" end-marker t)
	  (progn
	    ;; if the rest of the region should have a face, put it there
	    (funcall ansi-color-apply-face-function
		     start-marker (point) face)
	    ;; save face and point
	    (setq ansi-color-context-region
		  (list face (copy-marker (match-beginning 0)))))
	;; if the rest of the region should have a face, put it there
	(funcall ansi-color-apply-face-function
		 start-marker end-marker face)
	(setq ansi-color-context-region (if face (list face)))))))

(defun ansi-color-apply-overlay-face (beg end face)
  "Make an overlay from BEG to END, and apply face FACE.
If FACE is nil, do nothing."
  (when face
    (ansi-color-set-extent-face
     (ansi-color-make-extent beg end)
     face)))

;; This function helps you look for overlapping overlays.  This is
;; useful in comint-buffers.  Overlapping overlays should not happen!
;; A possible cause for bugs are the markers.  If you create an overlay
;; up to the end of the region, then that end might coincide with the
;; process-mark.  As text is added BEFORE the process-mark, the overlay
;; will keep growing.  Therefore, as more overlays are created later on,
;; there will be TWO OR MORE overlays covering the buffer at that point.
;; This function helps you check your buffer for these situations.
; (defun ansi-color-debug-overlays ()
;   (interactive)
;   (let ((pos (point-min)))
;     (while (< pos (point-max))
;       (if (<= 2 (length (overlays-at pos)))
; 	  (progn
; 	    (goto-char pos)
; 	    (error "%d overlays at %d" (length (overlays-at pos)) pos))
; 	(let (message-log-max)
; 	  (message  "Reached %d." pos)))
;       (setq pos (next-overlay-change pos)))))

;; Emacs/XEmacs compatibility layer

(defun ansi-color-make-face (property color)
  "Return a face with PROPERTY set to COLOR.
PROPERTY can be either symbol `foreground' or symbol `background'.

For Emacs, we just return the cons cell \(PROPERTY . COLOR).
For XEmacs, we create a temporary face and return it."
  (if (featurep 'xemacs)
      (let ((face (make-face (intern (concat color "-" (symbol-name property)))
			     "Temporary face created by ansi-color."
			     t)))
	(set-face-property face property color)
	face)
    (cond ((eq property 'foreground)
	   (cons 'foreground-color color))
	  ((eq property 'background)
	   (cons 'background-color color))
	  (t
	   (cons property color)))))

(defun ansi-color-make-extent (from to &optional object)
  "Make an extent for the range [FROM, TO) in OBJECT.

OBJECT defaults to the current buffer.  XEmacs uses `make-extent', Emacs
uses `make-overlay'.  XEmacs can use a buffer or a string for OBJECT,
Emacs requires OBJECT to be a buffer."
  (if (fboundp 'make-extent)
      (make-extent from to object)
    ;; In Emacs, the overlay might end at the process-mark in comint
    ;; buffers.  In that case, new text will be inserted before the
    ;; process-mark, ie. inside the overlay (using insert-before-marks).
    ;; In order to avoid this, we use the `insert-behind-hooks' overlay
    ;; property to make sure it works.
    (let ((overlay (make-overlay from to object)))
      (overlay-put overlay 'modification-hooks '(ansi-color-freeze-overlay))
      overlay)))

(defun ansi-color-freeze-overlay (overlay is-after begin end &optional len)
  "Prevent OVERLAY from being extended.
This function can be used for the `modification-hooks' overlay
property."
  ;; if stuff was inserted at the end of the overlay
  (when (and is-after
	     (= 0 len)
	     (= end (overlay-end overlay)))
    ;; reset the end of the overlay
    (move-overlay overlay (overlay-start overlay) begin)))

(defun ansi-color-set-extent-face (extent face)
  "Set the `face' property of EXTENT to FACE.
XEmacs uses `set-extent-face', Emacs  uses `overlay-put'."
  (if (featurep 'xemacs)
      (set-extent-face extent face)
    (overlay-put extent 'face face)))

;; Helper functions

(defun ansi-color-apply-sequence (escape-sequence faces)
  "Apply ESCAPE-SEQ to FACES and return the new list of faces.

ESCAPE-SEQ is an escape sequences parsed by `ansi-color-get-face'.

If the new faces start with the symbol `default', then the new
faces are returned.  If the faces start with something else,
they are appended to the front of the FACES list, and the new
list of faces is returned.

If `ansi-color-get-face' returns nil, then we either got a
null-sequence, or we stumbled upon some garbage.  In either
case we return nil."
  (let ((new-faces (ansi-color-get-face escape-sequence)))
    (cond ((null new-faces)
	   nil)
	  ((eq (car new-faces) 'default)
	   (cdr new-faces))
	  (t
	   ;; Like (append NEW-FACES FACES)
	   ;; but delete duplicates in FACES.
	   (let ((modified-faces (copy-sequence faces)))
	     (dolist (face (nreverse new-faces))
	       (setq modified-faces (delete face modified-faces))
	       (push face modified-faces))
	     modified-faces)))))

(defun ansi-color-make-color-map ()
  "Creates a vector of face definitions and returns it.

The index into the vector is an ANSI code.  See the documentation of
`ansi-color-map' for an example.

The face definitions are based upon the variables
`ansi-color-faces-vector' and `ansi-color-names-vector'."
  (let ((ansi-color-map (make-vector 50 nil))
        (index 0))
    ;; miscellaneous attributes
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index e)
                 (setq index (1+ index)) ))
     ansi-color-faces-vector)
    ;; foreground attributes
    (setq index 30)
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index
		       (ansi-color-make-face 'foreground
                                             (if (consp e) (car e) e)))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ;; background attributes
    (setq index 40)
    (mapc
     (function (lambda (e)
                 (aset ansi-color-map index
		       (ansi-color-make-face 'background
                                             (if (consp e) (cdr e) e)))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ansi-color-map))

(defvar ansi-color-map (ansi-color-make-color-map)
  "A brand new color map suitable for `ansi-color-get-face'.

The value of this variable is usually constructed by
`ansi-color-make-color-map'.  The values in the array are such that the
numbers included in an SGR control sequences point to the correct
foreground or background colors.

Example: The sequence \033[34m specifies a blue foreground.  Therefore:
     (aref ansi-color-map 34)
          => \(foreground-color . \"blue\")")

(defun ansi-color-map-update (symbol value)
  "Update `ansi-color-map'.

Whenever the vectors used to construct `ansi-color-map' are changed,
this function is called.  Therefore this function is listed as the :set
property of `ansi-color-faces-vector' and `ansi-color-names-vector'."
  (set-default symbol value)
  (setq ansi-color-map (ansi-color-make-color-map)))

(defun ansi-color-get-face-1 (ansi-code)
  "Get face definition from `ansi-color-map'.
ANSI-CODE is used as an index into the vector."
  (condition-case nil
      (aref ansi-color-map ansi-code)
    (args-out-of-range nil)))

(defun ansi-color-get-face (escape-seq)
  "Create a new face by applying all the parameters in ESCAPE-SEQ.

Should any of the parameters result in the default face (usually this is
the parameter 0), then the effect of all previous parameters is canceled.

ESCAPE-SEQ is a SGR control sequences such as \\033[34m.  The parameter
34 is used by `ansi-color-get-face-1' to return a face definition."
  (let ((i 0)
        f val)
    (while (string-match ansi-color-parameter-regexp escape-seq i)
      (setq i (match-end 0)
	    val (ansi-color-get-face-1
		 (string-to-number (match-string 1 escape-seq) 10)))
      (cond ((not val))
	    ((eq val 'default)
	     (setq f (list val)))
	    (t
	     (unless (member val f)
	       (push val f)))))
    f))

(provide 'ansi-color)

;;; ansi-color.el ends here
