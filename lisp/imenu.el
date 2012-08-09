;;; imenu.el --- framework for mode-specific buffer indexes

;; Copyright (C) 1994-1998, 2001-2012 Free Software Foundation, Inc.

;; Author: Ake Stenhoff <etxaksf@aom.ericsson.se>
;;         Lars Lindberg <lli@sypro.cap.se>
;; Maintainer: FSF
;; Created: 8 Feb 1994
;; Keywords: tools convenience

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

;; Purpose of this package:
;;   To present a framework for mode-specific buffer indexes.
;;   A buffer index is an alist of names and buffer positions.
;;   For instance all functions in a C-file and their positions.
;;
;;   It is documented in the Emacs Lisp manual.
;;
;; How it works:

;;   A mode-specific function is called to generate the index.  It is
;;   then presented to the user, who can choose from this index.
;;
;;   The package comes with a set of example functions for how to
;;   utilize this package.

;;   There are *examples* for index gathering functions/regular
;;   expressions for C/C++ and Lisp/Emacs Lisp but it is easy to
;;   customize for other modes.  A function for jumping to the chosen
;;   index position is also supplied.

;;; History:
;;  Thanks go to
;;  [simon] - Simon Leinen simon@lia.di.epfl.ch
;;  [dean] - Dean Andrews ada@unison.com
;;  [alon] - Alon Albert al@mercury.co.il
;;  [greg] - Greg Thompson gregt@porsche.visix.COM
;;  [wolfgang] - Wolfgang Bangerth zcg51122@rpool1.rus.uni-stuttgart.de
;;  [kai] - Kai Grossjohann grossjoh@linus.informatik.uni-dortmund.de
;;  [david] - David M. Smith dsmith@stats.adelaide.edu.au
;;  [christian] - Christian Egli Christian.Egli@hcsd.hac.com
;;  [karl] - Karl Fogel kfogel@floss.life.uiuc.edu

;;; Code:

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup imenu nil
  "Mode-specific buffer indexes."
  :group 'matching
  :group 'frames
  :group 'convenience
  :link '(custom-manual "(elisp)Imenu"))

(defcustom imenu-use-markers t
  "Non-nil means use markers instead of integers for Imenu buffer positions.

Setting this to nil makes Imenu work a little faster but editing the
buffer will make the generated index positions wrong.

This might not yet be honored by all index-building functions."
  :type 'boolean
  :group 'imenu)


(defcustom imenu-max-item-length 60
  "If a number, truncate Imenu entries to that length."
  :type '(choice integer
		 (const :tag "Unlimited"))
  :group 'imenu)

(defcustom imenu-auto-rescan nil
  "Non-nil means Imenu should always rescan the buffers."
  :type 'boolean
  :group 'imenu)

(defcustom imenu-auto-rescan-maxout 60000
  "Imenu auto-rescan is disabled in buffers larger than this size (in bytes).
This variable is buffer-local."
  :type 'integer
  :group 'imenu)

(defvar imenu-always-use-completion-buffer-p nil)
(make-obsolete-variable 'imenu-always-use-completion-buffer-p
			'imenu-use-popup-menu "22.1")

(defcustom imenu-use-popup-menu
  (if imenu-always-use-completion-buffer-p
      (not (eq imenu-always-use-completion-buffer-p 'never))
    'on-mouse)
  "Use a popup menu rather than a minibuffer prompt.
If nil, always use a minibuffer prompt.
If t, always use a popup menu,
If `on-mouse' use a popup menu when `imenu' was invoked with the mouse."
  :type '(choice (const :tag "On Mouse" on-mouse)
		 (const :tag "Never" nil)
		 (other :tag "Always" t))
  :group 'imenu)

(defcustom imenu-eager-completion-buffer
  (not (eq imenu-always-use-completion-buffer-p 'never))
  "If non-nil, eagerly popup the completion buffer."
  :type 'boolean
  :group 'imenu
  :version "22.1")

(defcustom imenu-after-jump-hook nil
  "Hooks called after jumping to a place in the buffer.

Useful things to use here include `reposition-window', `recenter', and
\(lambda () (recenter 0)) to show at top of screen."
  :type 'hook
  :group 'imenu)

;;;###autoload
(defcustom imenu-sort-function nil
  "The function to use for sorting the index mouse-menu.

Affects only the mouse index menu.

Set this to nil if you don't want any sorting (faster).
The items in the menu are then presented in the order they were found
in the buffer.

Set it to `imenu--sort-by-name' if you want alphabetic sorting.

The function should take two arguments and return t if the first
element should come before the second.  The arguments are cons cells;
\(NAME . POSITION).  Look at `imenu--sort-by-name' for an example."
  :type '(choice (const :tag "No sorting" nil)
		 (const :tag "Sort by name" imenu--sort-by-name)
		 (function :tag "Another function"))
  :group 'imenu)

(defcustom imenu-max-items 25
  "Maximum number of elements in a mouse menu for Imenu."
  :type 'integer
  :group 'imenu)

;; No longer used.  KFS 2004-10-27
;; (defcustom imenu-scanning-message "Scanning buffer for index (%3d%%)"
;;   "Progress message during the index scanning of the buffer.
;; If non-nil, user gets a message during the scanning of the buffer.
;;
;; Relevant only if the mode-specific function that creates the buffer
;; index use `imenu-progress-message', and not useful if that is fast, in
;; which case you might as well set this to nil."
;;   :type '(choice string
;;  		 (const :tag "None" nil))
;;   :group 'imenu)

(defcustom imenu-space-replacement "."
  "The replacement string for spaces in index names.
Used when presenting the index in a completion buffer to make the
names work as tokens."
  :type '(choice string (const nil))
  :group 'imenu)

(defcustom imenu-level-separator ":"
  "The separator between index names of different levels.
Used for making mouse-menu titles and for flattening nested indexes
with name concatenation."
  :type 'string
  :group 'imenu)

;;;###autoload
(defvar imenu-generic-expression nil
  "The regex pattern to use for creating a buffer index.

If non-nil this pattern is passed to `imenu--generic-function' to
create a buffer index.  Look there for the documentation of this
pattern's structure.

For example, see the value of `fortran-imenu-generic-expression' used by
`fortran-mode' with `imenu-syntax-alist' set locally to give the
characters which normally have \"symbol\" syntax \"word\" syntax
during matching.")
;;;###autoload(put 'imenu-generic-expression 'risky-local-variable t)

;;;###autoload
(make-variable-buffer-local 'imenu-generic-expression)

;;;; Hooks

;;;###autoload
(defvar imenu-create-index-function 'imenu-default-create-index-function
  "The function to use for creating an index alist of the current buffer.

It should be a function that takes no arguments and returns
an index alist of the current buffer.  The function is
called within a `save-excursion'.

See `imenu--index-alist' for the format of the buffer index alist.")
;;;###autoload
(make-variable-buffer-local 'imenu-create-index-function)

;;;###autoload
(defvar imenu-prev-index-position-function 'beginning-of-defun
  "Function for finding the next index position.

If `imenu-create-index-function' is set to
`imenu-default-create-index-function', then you must set this variable
to a function that will find the next index, looking backwards in the
file.

The function should leave point at the place to be connected to the
index and it should return nil when it doesn't find another index.")
;;;###autoload
(make-variable-buffer-local 'imenu-prev-index-position-function)

;;;###autoload
(defvar imenu-extract-index-name-function nil
  "Function for extracting the index item name, given a position.

This function is called after `imenu-prev-index-position-function'
finds a position for an index item, with point at that position.
It should return the name for that index item.")
;;;###autoload
(make-variable-buffer-local 'imenu-extract-index-name-function)

;;;###autoload
(defvar imenu-name-lookup-function nil
  "Function to compare string with index item.

This function will be called with two strings, and should return
non-nil if they match.

If nil, comparison is done with `string='.
Set this to some other function for more advanced comparisons,
such as \"begins with\" or \"name matches and number of
arguments match\".")
;;;###autoload
(make-variable-buffer-local 'imenu-name-lookup-function)

;;;###autoload
(defvar imenu-default-goto-function 'imenu-default-goto-function
  "The default function called when selecting an Imenu item.
The function in this variable is called when selecting a normal index-item.")
;;;###autoload
(make-variable-buffer-local 'imenu-default-goto-function)


(defun imenu--subalist-p (item)
  (and (consp (cdr item)) (listp (cadr item))
       (not (eq (car (cadr item)) 'lambda))))

;; Macro to display a progress message.
;; RELPOS is the relative position to display.
;; If RELPOS is nil, then the relative position in the buffer
;; is calculated.
;; PREVPOS is the variable in which we store the last position displayed.
(defmacro imenu-progress-message (prevpos &optional relpos reverse)

;; Made obsolete/empty, as computers are now faster than the eye, and
;; it had problems updating the messages correctly, and could shadow
;; more important messages/prompts in the minibuffer.  KFS 2004-10-27.

;;  `(and
;;    imenu-scanning-message
;;    (let ((pos ,(if relpos
;; 		    relpos
;; 		  `(imenu--relative-position ,reverse))))
;;      (if ,(if relpos t
;; 	     `(> pos (+ 5 ,prevpos)))
;; 	  (progn
;; 	    (message imenu-scanning-message pos)
;; 	    (setq ,prevpos pos)))))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Some examples of functions utilizing the framework of this
;;;; package.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: This was the only imenu-example-* definition actually used,
;; by cperl-mode.el.  Now cperl-mode has its own copy, so these can
;; all be removed.
(defun imenu-example--name-and-position ()
  "Return the current/previous sexp and its (beginning) location.
Don't move point."
  (save-excursion
    (forward-sexp -1)
    ;; [ydi] modified for imenu-use-markers
    (let ((beg (if imenu-use-markers (point-marker) (point)))
	  (end (progn (forward-sexp) (point))))
      (cons (buffer-substring beg end)
	    beg))))
(make-obsolete 'imenu-example--name-and-position
	       "use your own function instead." "23.2")

;;;
;;; Lisp
;;;

(defun imenu-example--lisp-extract-index-name ()
  ;; Example of a candidate for `imenu-extract-index-name-function'.
  ;; This will generate a flat index of definitions in a lisp file.
  (save-match-data
    (and (looking-at "(def")
	 (condition-case nil
	     (progn
	       (down-list 1)
	       (forward-sexp 2)
	       (let ((beg (point))
		     (end (progn (forward-sexp -1) (point))))
		 (buffer-substring beg end)))
	   (error nil)))))
(make-obsolete 'imenu-example--lisp-extract-index-name "your own" "23.2")

(defun imenu-example--create-lisp-index ()
  ;; Example of a candidate for `imenu-create-index-function'.
  ;; It will generate a nested index of definitions.
  (let ((index-alist '())
	(index-var-alist '())
	(index-type-alist '())
	(index-unknown-alist '())
	prev-pos)
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (while (beginning-of-defun)
      (imenu-progress-message prev-pos nil t)
	  (save-match-data
	    (and (looking-at "(def")
		 (save-excursion
	       (down-list 1)
		   (cond
		((looking-at "def\\(var\\|const\\)")
		     (forward-sexp 2)
		     (push (imenu-example--name-and-position)
			   index-var-alist))
		((looking-at "def\\(un\\|subst\\|macro\\|advice\\)")
		     (forward-sexp 2)
		     (push (imenu-example--name-and-position)
			   index-alist))
		((looking-at "def\\(type\\|struct\\|class\\|ine-condition\\)")
		     (forward-sexp 2)
 		 (if (= (char-after (1- (point))) ?\))
			 (progn
 		       (forward-sexp -1)
			   (down-list 1)
 		       (forward-sexp 1)))
		     (push (imenu-example--name-and-position)
			   index-type-alist))
		    (t
		     (forward-sexp 2)
		     (push (imenu-example--name-and-position)
		       index-unknown-alist)))))))
    (imenu-progress-message prev-pos 100)
    (and index-var-alist
	 (push (cons "Variables" index-var-alist)
	       index-alist))
    (and index-type-alist
 	 (push (cons "Types" index-type-alist)
  	       index-alist))
    (and index-unknown-alist
	 (push (cons "Syntax-unknown" index-unknown-alist)
	       index-alist))
    index-alist))
(make-obsolete 'imenu-example--create-lisp-index "your own" "23.2")

;; Regular expression to find C functions
(defvar imenu-example--function-name-regexp-c
  (concat
   "^[a-zA-Z0-9]+[ \t]?"		; type specs; there can be no
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"	; more than 3 tokens, right?
   "\\([a-zA-Z0-9_*]+[ \t]+\\)?"
   "\\([*&]+[ \t]*\\)?"			; pointer
   "\\([a-zA-Z0-9_*]+\\)[ \t]*("	; name
   ))

(defun imenu-example--create-c-index (&optional regexp)
  (let ((index-alist '())
	prev-pos char)
    (goto-char (point-min))
    (imenu-progress-message prev-pos 0)
    ;; Search for the function
    (save-match-data
      (while (re-search-forward
	      (or regexp imenu-example--function-name-regexp-c)
	      nil t)
	(imenu-progress-message prev-pos)
	(backward-up-list 1)
	(save-excursion
	  (goto-char (scan-sexps (point) 1))
	  (setq char (following-char)))
	;; Skip this function name if it is a prototype declaration.
	(if (not (eq char ?\;))
	    (push (imenu-example--name-and-position) index-alist))))
    (imenu-progress-message prev-pos 100)
    (nreverse index-alist)))
(make-obsolete 'imenu-example--create-c-index "your own" "23.2")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The item to use in the index for rescanning the buffer.
(defconst imenu--rescan-item '("*Rescan*" . -99))

;; The latest buffer index.
;; Buffer local.
(defvar imenu--index-alist nil
  "The buffer index alist computed for this buffer in Imenu.

Simple elements in the alist look like (INDEX-NAME . POSITION).
POSITION is the buffer position of the item; to go to the item
is simply to move point to that position.

Special elements look like (INDEX-NAME POSITION FUNCTION ARGUMENTS...).
To \"go to\" a special element means applying FUNCTION
to INDEX-NAME, POSITION, and the ARGUMENTS.

A nested sub-alist element looks like (INDEX-NAME SUB-ALIST).
The function `imenu--subalist-p' tests an element and returns t
if it is a sub-alist.

There is one simple element with negative POSITION; selecting that
element recalculates the buffer's index alist.")
;;;###autoload(put 'imenu--index-alist 'risky-local-variable t)

(make-variable-buffer-local 'imenu--index-alist)

(defvar imenu--last-menubar-index-alist nil
  "The latest buffer index alist used to update the menu bar menu.")

(make-variable-buffer-local 'imenu--last-menubar-index-alist)

;; History list for 'jump-to-function-in-buffer'.
;; Making this buffer local caused it not to work!
(defvar imenu--history-list nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal support functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Sort function
;;; Sorts the items depending on their index name.
;;; An item looks like (NAME . POSITION).
;;;
(defun imenu--sort-by-name (item1 item2)
  (string-lessp (car item1) (car item2)))

(defun imenu--sort-by-position (item1 item2)
  (< (cdr item1) (cdr item2)))

(defun imenu--relative-position (&optional reverse)
  ;; Support function to calculate relative position in buffer
  ;; Beginning of buffer is 0 and end of buffer is 100
  ;; If REVERSE is non-nil then the beginning is 100 and the end is 0.
  (let ((pos (point))
	(total (buffer-size)))
    (and reverse (setq pos (- total pos)))
    (if (> total 50000)
	;; Avoid overflow from multiplying by 100!
	(/ (1- pos) (max (/ total 100) 1))
      (/ (* 100 (1- pos)) (max total 1)))))

;; Split LIST into sublists of max length N.
;; Example (imenu--split '(1 2 3 4 5 6 7 8) 3)-> '((1 2 3) (4 5 6) (7 8))
;;
;; The returned list DOES NOT share structure with LIST.
(defun imenu--split (list n)
  (let ((remain list)
	(result '())
	(sublist '())
	(i 0))
    (while remain
      (push (pop remain) sublist)
      (incf i)
      (and (= i n)
	   ;; We have finished a sublist
	   (progn (push (nreverse sublist) result)
		  (setq i 0)
		  (setq sublist '()))))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (and sublist
	 (push (nreverse sublist) result))
    (nreverse result)))

;;; Split the alist MENULIST into a nested alist, if it is long enough.
;;; In any case, add TITLE to the front of the alist.
;;; If IMENU--RESCAN-ITEM is present in MENULIST, it is moved to the
;;; beginning of the returned alist.
;;;
;;; The returned alist DOES NOT share structure with MENULIST.
(defun imenu--split-menu (menulist title)
  (let ((menulist (copy-sequence menulist))
        keep-at-top tail)
    (if (memq imenu--rescan-item menulist)
	(setq keep-at-top (list imenu--rescan-item)
	      menulist (delq imenu--rescan-item menulist)))
    (setq tail menulist)
    (dolist (item tail)
      (when (imenu--subalist-p item)
	(push item keep-at-top)
	(setq menulist (delq item menulist))))
    (if imenu-sort-function
	(setq menulist (sort menulist imenu-sort-function)))
    (if (> (length menulist) imenu-max-items)
	(setq menulist
	      (mapcar
	       (lambda (menu)
		 (cons (format "From: %s" (caar menu)) menu))
	       (imenu--split menulist imenu-max-items))))
    (cons title
	  (nconc (nreverse keep-at-top) menulist))))

;;; Split up each long alist that are nested within ALIST
;;; into nested alists.
;;;
;;; Return a split and sorted copy of ALIST. The returned alist DOES
;;; NOT share structure with ALIST.
(defun imenu--split-submenus (alist)
  (mapcar (function
	   (lambda (elt)
	     (if (and (consp elt)
		      (stringp (car elt))
		      (listp (cdr elt)))
		 (imenu--split-menu (cdr elt) (car elt))
	       elt)))
	  alist))

;;; Truncate all strings in MENULIST to imenu-max-item-length
(defun imenu--truncate-items (menulist)
  (mapcar (function
	   (lambda (item)
	     (cond
	      ((consp (cdr item))
	       (imenu--truncate-items (cdr item)))
	      ;; truncate if necessary
	      ((and (numberp imenu-max-item-length)
		    (> (length (car item)) imenu-max-item-length))
	       (setcar item (substring (car item) 0 imenu-max-item-length))))))
	  menulist))


(defun imenu--make-index-alist (&optional noerror)
  "Create an index alist for the definitions in the current buffer.
This works by using the hook function `imenu-create-index-function'.
Report an error if the list is empty unless NOERROR is supplied and
non-nil.

See `imenu--index-alist' for the format of the index alist."
  (or (and imenu--index-alist
	   (or (not imenu-auto-rescan)
	       (and imenu-auto-rescan
		    (> (buffer-size)  imenu-auto-rescan-maxout))))
      ;; Get the index; truncate if necessary
      (progn
	(setq imenu--index-alist
	      (save-excursion
		(save-restriction
		  (widen)
		  (funcall imenu-create-index-function))))
	(imenu--truncate-items imenu--index-alist)))
  (or imenu--index-alist noerror
      (error "No items suitable for an index found in this buffer"))
  (or imenu--index-alist
      (setq imenu--index-alist (list nil)))
  ;; Add a rescan option to the index.
  (cons imenu--rescan-item imenu--index-alist))

;;; Find all markers in alist and makes
;;; them point nowhere.
;;; The top-level call uses nil as the argument;
;;; non-nil arguments are in recursive calls.
(defvar imenu--cleanup-seen)

(defun imenu--cleanup (&optional alist)
  ;; If alist is provided use that list.
  ;; If not, empty the table of lists already seen
  ;; and use imenu--index-alist.
  (if alist
      (setq imenu--cleanup-seen (cons alist imenu--cleanup-seen))
    (setq alist imenu--index-alist imenu--cleanup-seen (list alist)))

  (and alist
       (mapc
	(lambda (item)
	  (cond
	   ((markerp (cdr item))
	    (set-marker (cdr item) nil))
	   ;; Don't process one alist twice.
	   ((memq (cdr item) imenu--cleanup-seen))
	   ((imenu--subalist-p item)
	    (imenu--cleanup (cdr item)))))
	alist)
       t))

(defun imenu--create-keymap (title alist &optional cmd)
  (list* 'keymap title
	 (mapcar
	  (lambda (item)
	    (list* (car item) (car item)
		   (cond
		    ((imenu--subalist-p item)
		     (imenu--create-keymap (car item) (cdr item) cmd))
		    (t
		     `(lambda () (interactive)
			,(if cmd `(,cmd ',item) (list 'quote item)))))))
	  alist)))

(defun imenu--in-alist (str alist)
  "Check whether the string STR is contained in multi-level ALIST."
  (let (elt head tail res)
    (setq res nil)
    (while alist
      (setq elt (car alist)
	    tail (cdr elt)
	    alist (cdr alist)
	    head (car elt))
      ;; A nested ALIST element looks like
      ;;   (INDEX-NAME (INDEX-NAME . INDEX-POSITION) ...)
      ;; while a bottom-level element looks like
      ;;   (INDEX-NAME . INDEX-POSITION)
      ;; We are only interested in the bottom-level elements, so we need to
      ;; recurse if TAIL is a list.
      (cond ((listp tail)
	     (if (setq res (imenu--in-alist str tail))
		 (setq alist nil)))
	    ((if imenu-name-lookup-function
                 (funcall imenu-name-lookup-function str head)
               (string= str head))
	     (setq alist nil res elt))))
    res))

(defvar imenu-syntax-alist nil
  "Alist of syntax table modifiers to use while in `imenu--generic-function'.

The car of the assocs may be either a character or a string and the
cdr is a syntax description appropriate for `modify-syntax-entry'.  For
a string, all the characters in the string get the specified syntax.

This is typically used to give word syntax to characters which
normally have symbol syntax to simplify `imenu-expression'
and speed-up matching.")
;;;###autoload
(make-variable-buffer-local 'imenu-syntax-alist)

(defun imenu-default-create-index-function ()
  "Default function to create an index alist of the current buffer.

The most general method is to move point to end of buffer, then repeatedly call
`imenu-prev-index-position-function' and `imenu-extract-index-name-function'.
All the results returned by the latter are gathered into an index alist.
This method is used if those two variables are non-nil.

The alternate method, which is the one most often used, is to call
`imenu--generic-function' with `imenu-generic-expression' as argument."
  ;; These should really be done by setting imenu-create-index-function
  ;; in these major modes.  But save that change for later.
  (cond ((and imenu-prev-index-position-function
	      imenu-extract-index-name-function)
	 (let ((index-alist '()) (pos (point))
	       prev-pos name)
	   (goto-char (point-max))
	   (imenu-progress-message prev-pos 0 t)
	   ;; Search for the function
	   (while (funcall imenu-prev-index-position-function)
             (when (= pos (point))
               (error "Infinite loop at %s:%d: imenu-prev-index-position-function does not move point" (buffer-name) pos))
             (setq pos (point))
	     (imenu-progress-message prev-pos nil t)
	     (save-excursion
	       (setq name (funcall imenu-extract-index-name-function)))
	     (and (stringp name)
 		  ;; [ydi] updated for imenu-use-markers
		  (push (cons name (if imenu-use-markers (point-marker) (point)))
			index-alist)))
	   (imenu-progress-message prev-pos 100 t)
	   index-alist))
	;; Use generic expression if possible.
	((and imenu-generic-expression)
	 (imenu--generic-function imenu-generic-expression))
	(t
	 (error "This buffer cannot use `imenu-default-create-index-function'"))))

;;;
;;; Generic index gathering function.
;;;

(defvar imenu-case-fold-search t
  "Defines whether `imenu--generic-function' should fold case when matching.

This variable should be set (only) by initialization code
for modes which use `imenu--generic-function'.  If it is not set, but
`font-lock-defaults' is set, then font-lock's setting is used.")
;;;###autoload
(make-variable-buffer-local 'imenu-case-fold-search)

;; This function can be called with quitting disabled,
;; so it needs to be careful never to loop!
(defun imenu--generic-function (patterns)
  "Return an index alist of the current buffer based on PATTERNS.

PATTERNS is an alist with elements that look like this:
 (MENU-TITLE REGEXP INDEX)
or like this:
 (MENU-TITLE REGEXP INDEX FUNCTION ARGUMENTS...)
with zero or more ARGUMENTS.  The former format creates a simple
element in the index alist when it matches; the latter creates a
special element of the form (INDEX-NAME POSITION-MARKER FUNCTION
ARGUMENTS...) with FUNCTION and ARGUMENTS copied from PATTERNS.

MENU-TITLE is a string used as the title for the submenu or nil
if the entries are not nested.

REGEXP is a regexp that should match a construct in the buffer
that is to be displayed in the menu; i.e., function or variable
definitions, etc.  It contains a substring which is the name to
appear in the menu.  See the info section on Regexps for more
information.  REGEXP may also be a function, called without
arguments.  It is expected to search backwards.  It shall return
true and set `match-data' if it finds another element.

INDEX points to the substring in REGEXP that contains the
name (of the function, variable or type) that is to appear in the
menu.

The variable `imenu-case-fold-search' determines whether or not the
regexp matches are case sensitive, and `imenu-syntax-alist' can be
used to alter the syntax table for the search.

See `lisp-imenu-generic-expression' for an example of PATTERNS.

Returns an index of the current buffer as an alist.  The elements in
the alist look like:
 (INDEX-NAME . INDEX-POSITION)
or like:
 (INDEX-NAME INDEX-POSITION FUNCTION ARGUMENTS...)
They may also be nested index alists like:
 (INDEX-NAME . INDEX-ALIST)
depending on PATTERNS."

  (let ((index-alist (list 'dummy))
	prev-pos
        (case-fold-search (if (or (local-variable-p 'imenu-case-fold-search)
				  (not (local-variable-p 'font-lock-defaults)))
			      imenu-case-fold-search
			    (nth 2 font-lock-defaults)))
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    ;; Modify the syntax table used while matching regexps.
    (dolist (syn slist)
      ;; The character(s) to modify may be a single char or a string.
      (if (numberp (car syn))
	  (modify-syntax-entry (car syn) (cdr syn) table)
        (mapc (lambda (c)
                (modify-syntax-entry c (cdr syn) table))
              (car syn))))
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0 t)
    (unwind-protect			; for syntax table
	(save-match-data
	  (set-syntax-table table)

	  ;; map over the elements of imenu-generic-expression
	  ;; (typically functions, variables ...)
	  (dolist (pat patterns)
	    (let ((menu-title (car pat))
		  (regexp (nth 1 pat))
		  (index (nth 2 pat))
		  (function (nth 3 pat))
		  (rest (nthcdr 4 pat))
		  start beg)
	      ;; Go backwards for convenience of adding items in order.
	      (goto-char (point-max))
	      (while (and (if (functionp regexp)
			      (funcall regexp)
			    (re-search-backward regexp nil t))
			  ;; Exit the loop if we get an empty match,
			  ;; because it means a bad regexp was specified.
			  (not (= (match-beginning 0) (match-end 0))))
		(setq start (point))
		;; Record the start of the line in which the match starts.
		;; That's the official position of this definition.
		(goto-char (match-beginning index))
		(beginning-of-line)
		(setq beg (point))
		(imenu-progress-message prev-pos nil t)
		;; Add this sort of submenu only when we've found an
		;; item for it, avoiding empty, duff menus.
		(unless (assoc menu-title index-alist)
		  (push (list menu-title) index-alist))
		(if imenu-use-markers
		    (setq beg (copy-marker beg)))
		(let ((item
		       (if function
			   (nconc (list (match-string-no-properties index)
					beg function)
				  rest)
			 (cons (match-string-no-properties index)
			       beg)))
		      ;; This is the desired submenu,
		      ;; starting with its title (or nil).
		      (menu (assoc menu-title index-alist)))
		  ;; Insert the item unless it is already present.
		  (unless (member item (cdr menu))
		    (setcdr menu
			    (cons item (cdr menu)))))
		;; Go to the start of the match, to make sure we
		;; keep making progress backwards.
		(goto-char start))))
	  (set-syntax-table old-table)))
    (imenu-progress-message prev-pos 100 t)
    ;; Sort each submenu by position.
    ;; This is in case one submenu gets items from two different regexps.
    (dolist (item index-alist)
      (when (listp item)
	(setcdr item (sort (cdr item) 'imenu--sort-by-position))))
    (let ((main-element (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist))
	     (cdr main-element)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main functions for this package!
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See also info-lookup-find-item
(defun imenu-find-default (guess completions)
  "Fuzzily find an item based on GUESS inside the alist COMPLETIONS."
  (catch 'found
    (let ((case-fold-search t))
      (if (assoc guess completions) guess
	(dolist (re (list (concat "\\`" (regexp-quote guess) "\\'")
			  (concat "\\`" (regexp-quote guess))
			  (concat (regexp-quote guess) "\\'")
			  (regexp-quote guess)))
	  (dolist (x completions)
	    (if (string-match re (car x)) (throw 'found (car x)))))))))

(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Return one of the entries in index-alist or nil."
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
	choice
	(prepared-index-alist
	 (if (not imenu-space-replacement) index-alist
	   (mapcar
	    (lambda (item)
	      (cons (subst-char-in-string ?\s (aref imenu-space-replacement 0)
					  (car item))
		    (cdr item)))
	    index-alist))))
    (when (stringp name)
      (setq name (or (imenu-find-default name prepared-index-alist) name)))
    (cond (prompt)
	  ((and name (imenu--in-alist name prepared-index-alist))
	   (setq prompt (format "Index item (default %s): " name)))
	  (t (setq prompt "Index item: ")))
    (let ((minibuffer-setup-hook minibuffer-setup-hook))
      ;; Display the completion buffer.
      (if (not imenu-eager-completion-buffer)
	  (add-hook 'minibuffer-setup-hook 'minibuffer-completion-help))
      (setq name (completing-read prompt
				  prepared-index-alist
				  nil t nil 'imenu--history-list name)))

    (when (stringp name)
      (setq choice (assoc name prepared-index-alist))
      (if (imenu--subalist-p choice)
	  (imenu--completion-buffer (cdr choice) prompt)
	choice))))

(defun imenu--mouse-menu (index-alist event &optional title)
  "Let the user select from a buffer index from a mouse menu.

INDEX-ALIST is the buffer index and EVENT is a mouse event.

Returns t for rescan and otherwise an element or subelement of INDEX-ALIST."
  (setq index-alist (imenu--split-submenus index-alist))
  (let* ((menu (imenu--split-menu index-alist (or title (buffer-name))))
	 (map (imenu--create-keymap (car menu)
				    (cdr (if (< 1 (length (cdr menu)))
					     menu
					   (car (cdr menu)))))))
    (popup-menu map event)))

(defun imenu-choose-buffer-index (&optional prompt alist)
  "Let the user select from a buffer index and return the chosen index.

If the user originally activated this function with the mouse, a mouse
menu is used.  Otherwise a completion buffer is used and the user is
prompted with PROMPT.

If you call this function with index alist ALIST, then it lets the user
select from ALIST.

With no index alist ALIST, it calls `imenu--make-index-alist' to
create the index alist.

If `imenu-use-popup-menu' is nil, then the completion buffer
is always used, no matter if the mouse was used or not.

The returned value is of the form (INDEX-NAME . INDEX-POSITION)."
  (let (index-alist
	(mouse-triggered (listp last-nonmenu-event))
	(result t))
    ;; If selected by mouse, see to that the window where the mouse is
    ;; really is selected.
    (and mouse-triggered
	 (not (equal last-nonmenu-event '(menu-bar)))
	 (let ((window (posn-window (event-start last-nonmenu-event))))
	   (or (framep window) (null window) (select-window window))))
    ;; Create a list for this buffer only when needed.
    (while (eq result t)
      (setq index-alist (if alist alist (imenu--make-index-alist)))
      (setq result
	    (if (and imenu-use-popup-menu
		     (or (eq imenu-use-popup-menu t) mouse-triggered))
		(imenu--mouse-menu index-alist last-nonmenu-event)
	      (imenu--completion-buffer index-alist prompt)))
      (and (equal result imenu--rescan-item)
	   (imenu--cleanup)
	   (setq result t imenu--index-alist nil)))
    result))

;;;###autoload
(defun imenu-add-to-menubar (name)
  "Add an `imenu' entry to the menu bar for the current buffer.
NAME is a string used to name the menu bar item.
See the command `imenu' for more information."
  (interactive "sImenu menu item name: ")
  (if (or (and imenu-prev-index-position-function
	       imenu-extract-index-name-function)
	  imenu-generic-expression
	  (not (eq imenu-create-index-function
		   'imenu-default-create-index-function)))
      (unless (and (current-local-map)
                   (keymapp (lookup-key (current-local-map) [menu-bar index])))
	(let ((newmap (make-sparse-keymap)))
	  (set-keymap-parent newmap (current-local-map))
	  (setq imenu--last-menubar-index-alist nil)
	  (define-key newmap [menu-bar index]
	    `(menu-item ,name ,(make-sparse-keymap "Imenu")))
	  (use-local-map newmap)
	  (add-hook 'menu-bar-update-hook 'imenu-update-menubar)))
    (error "The mode `%s' does not support Imenu"
           (format-mode-line mode-name))))

;;;###autoload
(defun imenu-add-menubar-index ()
  "Add an Imenu \"Index\" entry on the menu bar for the current buffer.

A trivial interface to `imenu-add-to-menubar' suitable for use in a hook."
  (interactive)
  (imenu-add-to-menubar "Index"))

(defvar imenu-buffer-menubar nil)

(defvar imenu-menubar-modified-tick 0
  "The value of (buffer-chars-modified-tick) as of the last call
to `imenu-update-menubar'.")
(make-variable-buffer-local 'imenu-menubar-modified-tick)

(defun imenu-update-menubar ()
  (when (and (current-local-map)
	     (keymapp (lookup-key (current-local-map) [menu-bar index]))
	     (/= (buffer-chars-modified-tick) imenu-menubar-modified-tick))
    (setq imenu-menubar-modified-tick (buffer-chars-modified-tick))
    (let ((index-alist (imenu--make-index-alist t)))
      ;; Don't bother updating if the index-alist has not changed
      ;; since the last time we did it.
      (unless (equal index-alist imenu--last-menubar-index-alist)
	(let (menu menu1 old)
	  (setq imenu--last-menubar-index-alist index-alist)
	  (setq index-alist (imenu--split-submenus index-alist))
	  (setq menu (imenu--split-menu index-alist
					(buffer-name)))
	  (setq menu1 (imenu--create-keymap (car menu)
					    (cdr (if (< 1 (length (cdr menu)))
						     menu
						   (car (cdr menu))))
					    'imenu--menubar-select))
	  (setq old (lookup-key (current-local-map) [menu-bar index]))
	  ;; This should never happen, but in some odd cases, potentially,
	  ;; lookup-key may return a dynamically composed keymap.
	  (if (keymapp (cadr old)) (setq old (cadr old)))
	  (setcdr old (cdr menu1)))))))

(defun imenu--menubar-select (item)
  "Use Imenu to select the function or variable named in this menu ITEM."
  (if (equal item imenu--rescan-item)
      (progn
	(imenu--cleanup)
	;; Make sure imenu-update-menubar redoes everything.
	(setq imenu-menubar-modified-tick -1)
	(setq imenu--index-alist nil)
	(setq imenu--last-menubar-index-alist nil)
	(imenu-update-menubar)
	t)
    (imenu item)
    nil))

(defun imenu-default-goto-function (name position &optional rest)
  "Move to the given position.

NAME is ignored.  POSITION is where to move.  REST is also ignored.
The ignored args just make this function have the same interface as a
function placed in a special index-item."
  (if (or (< position (point-min))
	  (> position (point-max)))
      ;; widen if outside narrowing
      (widen))
  (goto-char position))

;;;###autoload
(defun imenu (index-item)
  "Jump to a place in the buffer chosen using a buffer menu or mouse menu.
INDEX-ITEM specifies the position.  See `imenu-choose-buffer-index'
for more information."
  (interactive (list (imenu-choose-buffer-index)))
  ;; Convert a string to an alist element.
  (if (stringp index-item)
      (setq index-item (assoc index-item (imenu--make-index-alist))))
  (when index-item
    (push-mark)
    (let* ((is-special-item (listp (cdr index-item)))
	   (function
	    (if is-special-item
		(nth 2 index-item) imenu-default-goto-function))
	   (position (if is-special-item
			 (cadr index-item) (cdr index-item)))
	   (rest (if is-special-item (cddr index-item))))
      (apply function (car index-item) position rest))
    (run-hooks 'imenu-after-jump-hook)))

(dolist (mess
	 '("^No items suitable for an index found in this buffer$"
	   "^This buffer cannot use `imenu-default-create-index-function'$"
	   "^The mode `.*' does not support Imenu$"))
  (add-to-list 'debug-ignored-errors mess))

(provide 'imenu)

;;; imenu.el ends here
