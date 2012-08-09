;;; org-faces.el --- Face definitions for Org-mode.

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

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

;; This file contains the face definitions for Org.

;;; Code:

(require 'org-macs)
(require 'org-compat)

(defun org-copy-face (old-face new-face docstring &rest attributes)
  (unless (facep new-face)
    (if (fboundp 'set-face-attribute)
	(progn
	  (make-face new-face)
	  (set-face-attribute new-face nil :inherit old-face)
	  (apply 'set-face-attribute new-face nil attributes)
	  (set-face-doc-string new-face docstring))
      (copy-face old-face new-face)
      (if (fboundp 'set-face-doc-string)
	  (set-face-doc-string new-face docstring)))))
(put 'org-copy-face 'lisp-indent-function 2)

(defgroup org-faces nil
  "Faces in Org-mode."
  :tag "Org Faces"
  :group 'org-appearance)

(defface org-default
  (org-compatible-face 'default nil)
  "Face used for default text."
  :group 'org-faces)

(defface org-hide
  '((((background light)) (:foreground "white"))
    (((background dark)) (:foreground "black")))
  "Face used to hide leading stars in headlines.
The foreground color of this face should be equal to the background
color of the frame."
  :group 'org-faces)

(defface org-level-1 ;; originally copied from font-lock-function-name-face
  (org-compatible-face 'outline-1
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used for level 1 headlines."
  :group 'org-faces)

(defface org-level-2 ;; originally copied from font-lock-variable-name-face
  (org-compatible-face 'outline-2
    '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
      (((class color) (min-colors 16) (background dark))  (:foreground "LightGoldenrod"))
      (((class color) (min-colors 8)  (background light)) (:foreground "yellow"))
      (((class color) (min-colors 8)  (background dark))  (:foreground "yellow" :bold t))
      (t (:bold t))))
  "Face used for level 2 headlines."
  :group 'org-faces)

(defface org-level-3 ;; originally copied from font-lock-keyword-face
  (org-compatible-face 'outline-3
    '((((class color) (min-colors 88) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 88) (background dark))  (:foreground "Cyan1"))
      (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 16) (background dark))  (:foreground "Cyan"))
      (((class color) (min-colors 8)  (background light)) (:foreground "purple" :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "cyan" :bold t))
      (t (:bold t))))
  "Face used for level 3 headlines."
  :group 'org-faces)

(defface org-level-4   ;; originally copied from font-lock-comment-face
  (org-compatible-face 'outline-4
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark))  (:foreground "chocolate1"))
      (((class color) (min-colors 16) (background light)) (:foreground "red"))
      (((class color) (min-colors 16) (background dark))  (:foreground "red1"))
      (((class color) (min-colors 8) (background light))  (:foreground "red" :bold t))
      (((class color) (min-colors 8) (background dark))   (:foreground "red" :bold t))
      (t (:bold t))))
  "Face used for level 4 headlines."
  :group 'org-faces)

(defface org-level-5 ;; originally copied from font-lock-type-face
  (org-compatible-face 'outline-5
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 5 headlines."
  :group 'org-faces)

(defface org-level-6 ;; originally copied from font-lock-constant-face
  (org-compatible-face 'outline-6
    '((((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
      (((class color) (min-colors 8)) (:foreground "magenta"))))
  "Face used for level 6 headlines."
  :group 'org-faces)

(defface org-level-7 ;; originally copied from font-lock-builtin-face
  (org-compatible-face 'outline-7
    '((((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
      (((class color) (min-colors 8)) (:foreground "blue"))))
  "Face used for level 7 headlines."
  :group 'org-faces)

(defface org-level-8 ;; originally copied from font-lock-string-face
  (org-compatible-face 'outline-8
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 8 headlines."
  :group 'org-faces)

(defface org-special-keyword ;; originally copied from font-lock-string-face
  (org-compatible-face 'font-lock-keyword-face
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (t (:italic t))))
  "Face used for special keywords."
  :group 'org-faces)

(defface org-drawer ;; originally copied from font-lock-function-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used for drawers."
  :group 'org-faces)

(defface org-property-value nil
  "Face used for the value of a property."
  :group 'org-faces)

(defface org-column
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light))
       (:background "grey90" :weight normal :slant normal :strike-through nil
		    :underline nil))
      (((class color) (min-colors 16) (background dark))
       (:background "grey30" :weight normal :slant normal :strike-through nil
		    :underline nil))
      (((class color) (min-colors 8))
       (:background "cyan" :foreground "black"
		    :weight normal :slant normal :strike-through nil
		    :underline nil))
      (t (:inverse-video t))))
  "Face for column display of entry properties.
This is actually only part of the face definition for the text in column view.
The following faces apply, with this priority.

1. The color of the reference face.  This is normally the level fact that
   is used in the outline.  In agenda-mode, it will be the face of the
   first character in the line.  The color is explicitly retained to
   make sure that the column line still looks a bit like the structure
   line it is masking.

2. The `org-column' face.

3. The remaining properties of the reference face.

Since column view works by putting overlays with a display property
over individual characters in the buffer, the face of the underlining
character (this might for example be the a TODO keyword) might still
shine through in some properties.  So when your column view looks
funny, with \"random\" colors, weight, strike-through, try to explicitly
set the properties in the `org-column' face.  For example, set
:underline to nil, or the :slant to `normal'.

Under XEmacs, the rules are simpler, because the XEmacs version of
column view defines special faces for each outline level.  See the file
`org-colview-xemacs.el' for details."
  :group 'org-faces)

(defface org-column-title
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light))
       (:background "grey90" :underline t :weight bold))
      (((class color) (min-colors 16) (background dark))
       (:background "grey30" :underline t :weight bold))
      (((class color) (min-colors 8))
       (:background "cyan" :foreground "black" :underline t :weight bold))
      (t (:inverse-video t))))
  "Face for column display of entry properties."
  :group 'org-faces)

(when (fboundp 'set-face-attribute)
  ;; Make sure that a fixed-width face is used when we have a column table.
  (set-face-attribute 'org-column nil
		      :height (face-attribute 'default :height)
		      :family (face-attribute 'default :family)))

(defface org-agenda-column-dateline
  (org-compatible-face 'org-column
    '((t nil)))
  "Face used in agenda column view for datelines with summaries."
  :group 'org-faces)

(defface org-warning
  (org-compatible-face 'font-lock-warning-face
    '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t))
      (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t))
      (t (:bold t))))
  "Face for deadlines and TODO keywords."
  :group 'org-faces)

(defface org-archived    ; similar to shadow
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for headline with the ARCHIVE tag."
  :group 'org-faces)

(defface org-link
  (org-compatible-face 'link
    '((((class color) (background light)) (:foreground "Purple" :underline t))
      (((class color) (background dark)) (:foreground "Cyan" :underline t))
      (t (:underline t))))
  "Face for links."
  :group 'org-faces)

(defface org-footnote
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for links."
  :group 'org-faces)

(defface org-ellipsis
  '((((class color) (background light)) (:foreground "DarkGoldenrod" :underline t))
    (((class color) (background dark)) (:foreground "LightGoldenrod" :underline t))
    (t (:strike-through t)))
  "Face for the ellipsis in folded text."
  :group 'org-faces)

(defface org-target
  '((((class color) (background light)) (:underline t))
    (((class color) (background dark)) (:underline t))
    (t (:underline t)))
  "Face for link targets."
  :group 'org-faces)

(defface org-date
  '((((class color) (background light)) (:foreground "Purple" :underline t))
    (((class color) (background dark)) (:foreground "Cyan" :underline t))
    (t (:underline t)))
  "Face for date/time stamps."
  :group 'org-faces)

(defface org-date-selected
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold nil))
      (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold nil))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold nil))
      (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold nil))
      (t (:inverse-video t))))
  "Face for highlighting the calendar day when using `org-read-date'."
  :group 'org-faces)

(defface org-sexp-date
  '((((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:underline t)))
  "Face for diary-like sexp date specifications."
  :group 'org-faces)

(defface org-tag
  '((t (:bold t)))
  "Default face for tags.
Note that the variable `org-tag-faces' can be used to overrule this face for
specific tags."
  :group 'org-faces)

(defface org-todo ; font-lock-warning-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "Red1" :bold t))
      (((class color) (min-colors 16) (background dark))  (:foreground "Pink" :bold t))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"  :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "red"  :bold t))
      (t (:inverse-video t :bold t))))
  "Face for TODO keywords."
  :group 'org-faces)

(defface org-done ;; originally copied from font-lock-type-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen" :bold t))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen" :bold t))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold t))))
  "Face used for todo keywords that indicate DONE items."
  :group 'org-faces)

(defface org-agenda-done ;; originally copied from font-lock-type-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold nil))))
  "Face used in agenda, to indicate lines switched to DONE.
This face is used to de-emphasize items that where brightly colored in the
agenda because they were things to do, or overdue.  The DONE state itself
is of course immediately visible, but for example a passed deadline is
\(by default) very bright read.  This face could be simply the default face
of the frame, for example."
  :group 'org-faces)

(defface org-headline-done ;; originally copied from font-lock-string-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (((class color) (min-colors 8)  (background light)) (:bold nil))))
  "Face used to indicate that a headline is DONE.
This face is only used if `org-fontify-done-headline' is set.  If applies
to the part of the headline after the DONE keyword."
  :group 'org-faces)

(defcustom org-faces-easy-properties
  '((todo . :foreground) (tag . :foreground) (priority . :foreground))
  "The property changes by easy faces.
This is an alist, the keys show the area of application, the values
can be `:foreground' or `:background'.  A color string for special
keywords will then be interpreted as either foreground or background
color."
  :group 'org-faces
  :group 'org-todo
  :version "24.1"
  :type '(repeat
	  (cons (choice (const todo) (const tag) (const priority))
		(choice (const :foreground) (const :background)))))

(defcustom org-todo-keyword-faces nil
  "Faces for specific TODO keywords.
This is a list of cons cells, with TODO keywords in the car
and faces in the cdr.  The face can be a symbol, a color
as a string (in which case the rest is inherited from the `org-todo' face),
or a property list of attributes, like
   (:foreground \"blue\" :weight bold :underline t).
If it is a color string, the variable `org-faces-easy-properties'
determines if it is a foreground or a background color."
  :group 'org-faces
  :group 'org-todo
  :type '(repeat
	  (cons
	   (string :tag "Keyword")
	   (choice :tag "Face   "
	    (string :tag "Color")
	    (sexp :tag "Face")))))

(defcustom org-priority-faces nil
  "Faces for specific Priorities.
This is a list of cons cells, with priority character in the car
and faces in the cdr.  The face can be a symbol, a color as
as a string, or a property list of attributes, like
    (:foreground \"blue\" :weight bold :underline t).
If it is a color string, the variable `org-faces-easy-properties'
determines if it is a foreground or a background color."
  :group 'org-faces
  :group 'org-todo
  :type '(repeat
	  (cons
	   (character :tag "Priority")
	   (choice    :tag "Face    "
	    (string :tag "Color")
	    (sexp :tag "Face")))))

(defvar org-tags-special-faces-re nil)
(defun org-set-tag-faces (var value)
  (set var value)
  (if (not value)
      (setq org-tags-special-faces-re nil)
    (setq org-tags-special-faces-re
	  (concat ":\\(" (mapconcat 'car value "\\|") "\\):"))))

(defface org-checkbox
  (org-compatible-face 'bold
    '((t (:bold t))))
  "Face for checkboxes"
  :group 'org-faces)


(org-copy-face 'org-todo 'org-checkbox-statistics-todo
  "Face used for unfinished checkbox statistics.")

(org-copy-face 'org-done 'org-checkbox-statistics-done
  "Face used for finished checkbox statistics.")

(defcustom org-tag-faces nil
  "Faces for specific tags.
This is a list of cons cells, with tags in the car and faces in the cdr.
The face can be a symbol, a foreground color (in which case the rest is
inherited from the `org-tag' face) or a property list of attributes,
like (:foreground \"blue\" :weight bold :underline t).
If you set this variable through customize, it will immediately be effective
in new buffers and in modified lines.
If you set it with Lisp, a restart of Emacs is required to activate the
changes."
  :group 'org-faces
  :group 'org-tags
  :set 'org-set-tag-faces
  :type '(repeat
	  (cons
	   (string :tag "Tag ")
	   (choice :tag "Face"
	    (string :tag "Foreground color")
	    (sexp :tag "Face")))))

(defface org-table ;; originally copied from font-lock-function-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)  (background light)) (:foreground "blue"))
      (((class color) (min-colors 8)  (background dark)))))
  "Face used for tables."
  :group 'org-faces)

(defface org-formula
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"))
      (((class color) (min-colors 8)  (background dark)) (:foreground "red"))
      (t (:bold t :italic t))))
  "Face for formulas."
  :group 'org-faces)

(defface org-code
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for fixed-width text like code snippets."
  :group 'org-faces
  :version "22.1")

(defface org-meta-line
  (org-compatible-face 'font-lock-comment-face nil)
  "Face for meta lines startin with \"#+\"."
  :group 'org-faces
  :version "22.1")

(defface org-document-title
  '((((class color) (background light)) (:foreground "midnight blue" :weight bold :height 1.44))
    (((class color) (background dark)) (:foreground "pale turquoise" :weight bold :height 1.44))
    (t (:weight bold :height 1.44)))
  "Face for document title, i.e. that which follows the #+TITLE: keyword."
  :group 'org-faces)

(defface org-document-info
  '((((class color) (background light)) (:foreground "midnight blue"))
    (((class color) (background dark)) (:foreground "pale turquoise"))
    (t nil))
  "Face for document date, author and email; i.e. that which
follows a #+DATE:, #+AUTHOR: or #+EMAIL: keyword."
  :group 'org-faces)

(defface org-document-info-keyword
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face for #+TITLE:, #+AUTHOR:, #+EMAIL: and #+DATE: keywords."
  :group 'org-faces)

(defface org-block
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50"))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70"))
      (((class color) (min-colors 8) (background light))
       (:foreground "green"))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow"))))
  "Face text in #+begin ... #+end blocks."
  :group 'org-faces
  :version "22.1")

(defface org-block-background '((t ()))
  "Face used for the source block background.")

(org-copy-face 'org-meta-line 'org-block-begin-line
  "Face used for the line delimiting the begin of source blocks.")

(org-copy-face 'org-meta-line 'org-block-end-line
  "Face used for the line delimiting the end of source blocks.")

(defface org-verbatim
  (org-compatible-face 'shadow
    '((((class color grayscale) (min-colors 88) (background light))
       (:foreground "grey50" :underline t))
      (((class color grayscale) (min-colors 88) (background dark))
       (:foreground "grey70" :underline t))
      (((class color) (min-colors 8) (background light))
       (:foreground "green" :underline t))
      (((class color) (min-colors 8) (background dark))
       (:foreground "yellow" :underline t))))
  "Face for fixed-with text like code snippets."
  :group 'org-faces
  :version "22.1")

(org-copy-face 'org-block 'org-quote
   "Face for #+BEGIN_QUOTE ... #+END_QUOTE blocks.")
(org-copy-face 'org-block 'org-verse
   "Face for #+BEGIN_VERSE ... #+END_VERSE blocks.")

(defcustom org-fontify-quote-and-verse-blocks nil
  "Non-nil means, add a special face to #+begin_quote and #+begin_verse block.
When nil, format these as normal Org.  This is the default, because the
content of these blocks will still be treated as Org syntax."
  :group 'org-faces
  :version "24.1"
  :type 'boolean)

(defface org-clock-overlay ;; copied from secondary-selection
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light))
       (:background "yellow1"))
      (((class color) (min-colors 88) (background dark))
       (:background "SkyBlue4"))
      (((class color) (min-colors 16) (background light))
       (:background "yellow"))
      (((class color) (min-colors 16) (background dark))
       (:background "SkyBlue4"))
      (((class color) (min-colors 8))
       (:background "cyan" :foreground "black"))
      (t (:inverse-video t))))
    "Basic face for displaying the secondary selection."
    :group 'org-faces)

(defface org-agenda-structure ;; originally copied from font-lock-function-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used in agenda for captions and dates."
  :group 'org-faces)

(org-copy-face 'org-agenda-structure 'org-agenda-date
  "Face used in agenda for normal days.")

(org-copy-face 'org-agenda-date 'org-agenda-date-today
  "Face used in agenda for today."
  :weight 'bold :italic 't)

(org-copy-face 'secondary-selection 'org-agenda-clocking
  "Face marking the current clock item in the agenda.")

(org-copy-face 'org-agenda-date 'org-agenda-date-weekend
  "Face used in agenda for weekend days.
See the variable `org-agenda-weekend-days' for a definition of which days
belong to the weekend."
	       :weight 'bold)

(defface org-scheduled
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen"))
      (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold t :italic t))))
  "Face for items scheduled for a certain day."
  :group 'org-faces)

(defface org-scheduled-today
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "DarkGreen"))
      (((class color) (min-colors 88) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))
      (t (:bold t :italic t))))
  "Face for items scheduled for a certain day."
  :group 'org-faces)

(defface org-agenda-dimmed-todo-face
  '((((background light)) (:foreground "grey50"))
    (((background dark)) (:foreground "grey50")))
  "Face used to dim blocked tasks in the agenda."
  :group 'org-faces)

(defface org-scheduled-previously
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"))
      (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
      (t (:bold t))))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defface org-upcoming-deadline
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark)) (:foreground "chocolate1"))
      (((class color) (min-colors 8)  (background light)) (:foreground "red"))
      (((class color) (min-colors 8)  (background dark)) (:foreground "red" :bold t))
      (t (:bold t))))
  "Face for items scheduled previously, and not yet done."
  :group 'org-faces)

(defcustom org-agenda-deadline-faces
  '((1.0 . org-warning)
    (0.5 . org-upcoming-deadline)
    (0.0 . default))
  "Faces for showing deadlines in the agenda.
This is a list of cons cells.  The cdr of each cell is a face to be used,
and it can also just be like '(:foreground \"yellow\").
Each car is a fraction of the head-warning time that must have passed for
this the face in the cdr to be used for display.  The numbers must be
given in descending order.  The head-warning time is normally taken
from `org-deadline-warning-days', but can also be specified in the deadline
timestamp itself, like this:

   DEADLINE: <2007-08-13 Mon -8d>

You may use d for days, w for weeks, m for months and y for years.  Months
and years will only be treated in an approximate fashion (30.4 days for a
month and 365.24 days for a year)."
  :group 'org-faces
  :group 'org-agenda-daily/weekly
  :type '(repeat
	  (cons
	   (number :tag "Fraction of head-warning time passed")
	   (sexp :tag "Face"))))

(defface org-agenda-restriction-lock
  (org-compatible-face nil
    '((((class color) (min-colors 88) (background light)) (:background "yellow1"))
      (((class color) (min-colors 88) (background dark))  (:background "skyblue4"))
      (((class color) (min-colors 16) (background light)) (:background "yellow1"))
      (((class color) (min-colors 16) (background dark))  (:background "skyblue4"))
      (((class color) (min-colors 8)) (:background "cyan" :foreground "black"))
      (t (:inverse-video t))))
  "Face for showing the agenda restriction lock."
  :group 'org-faces)

(defface org-agenda-filter-tags
  (org-compatible-face 'modeline
    nil)
  "Face for tag(s) in the mode-line when filtering the agenda."
  :group 'org-faces)

(defface org-agenda-filter-category
  (org-compatible-face 'modeline
    nil)
  "Face for tag(s) in the mode-line when filtering the agenda."
  :group 'org-faces)

(defface org-time-grid ;; originally copied from font-lock-variable-name-face
  (org-compatible-face nil
    '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightGoldenrod"))
      (((class color) (min-colors 8)) (:foreground "yellow" :weight light))))
  "Face used for time grids."
  :group 'org-faces)

(org-copy-face 'org-time-grid 'org-agenda-current-time
  "Face used to show the current time in the time grid.")

(defface org-agenda-diary
  (org-compatible-face 'default
    nil)
  "Face used for agenda entries that come from the Emacs diary."
  :group 'org-faces)

(defface org-agenda-calendar-event
  (org-compatible-face 'default
    nil)
  "Face used to show events and appointments in the agenda."
  :group 'org-faces)

(defface org-agenda-calendar-sexp
  (org-compatible-face 'default
    nil)
  "Face used to show events computed from a S-expression."
  :group 'org-faces)

(defconst org-level-faces
  '(org-level-1 org-level-2 org-level-3 org-level-4
    org-level-5 org-level-6 org-level-7 org-level-8
    ))

(defcustom org-n-level-faces (length org-level-faces)
  "The number of different faces to be used for headlines.
Org-mode defines 8 different headline faces, so this can be at most 8.
If it is less than 8, the level-1 face gets re-used for level N+1 etc."
  :type 'integer
  :group 'org-faces)

(defcustom org-cycle-level-faces t
 "Non-nil means level styles cycle after level `org-n-level-faces'.
Then so level org-n-level-faces+1 is styled like level 1.
If nil, then all levels >=org-n-level-faces are styled like
level org-n-level-faces"
 :group 'org-appearance
 :group 'org-faces
 :version "24.1"
 :type 'boolean)

(defface org-latex-and-export-specials
  (let ((font (cond ((assq :inherit custom-face-attributes)
		     '(:inherit underline))
		    (t '(:underline t)))))
    `((((class grayscale) (background light))
       (:foreground "DimGray" ,@font))
      (((class grayscale) (background dark))
       (:foreground "LightGray" ,@font))
      (((class color) (background light))
       (:foreground "SaddleBrown"))
      (((class color) (background dark))
       (:foreground "burlywood"))
      (t (,@font))))
  "Face used to highlight math latex and other special exporter stuff."
  :group 'org-faces)

(org-copy-face 'modeline 'org-mode-line-clock
  "Face used for clock display in mode line.")
(org-copy-face 'modeline 'org-mode-line-clock-overrun
  "Face used for clock display for overrun tasks in mode line."
  :background "red")

(provide 'org-faces)

;;; org-faces.el ends here
