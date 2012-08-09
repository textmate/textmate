;;; two-column.el --- minor mode for editing of two-column text

;; Copyright (C) 1992-1995, 2001-2012 Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
;; Adapted-By: ESR, Daniel Pfeiffer
;; Keywords: wp

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

;;; Komentario:				 Commentary:

;; Tiu  programaro ebligas  vin redakti	 This package gives you the ability
;; dukolumnan tekston.			 to edit text in a two-column format.


;; Vi  havas  tri eblecojn por eki tiun	 You have three ways to start up this
;; mal^cefan modalon.  ^Ciu donas al vi	 minor mode.  Each gives you a
;; horizontale disigatan fenestron, si-	 horizontally split window similar to
;; milan al fina apareco de via teksto:	 the final outcome of your text:


;; f2 2	    asocias  novan  bufron  nomatan  associates a new  buffer called
;; C-x 6 2  same, sed kun 2C/ anta^u.	     the   same,    but   with   2C/
;;					     prepended.

;; f2 b	    asocias alian bufron.  Vi povas  associates    another   buffer.
;; C-x 6 b  anka^u asocii  dataron,   se vi  This can be used to associate a
;;	    ^jus anta^ue faris C-x C-f.	     file if you just did C-x C-f.

;; f2 s	    disigas  jam dukolumnan tekston  splits a  two-column  text into
;; C-x 6 s  en  du   bufroj  ekde  la  nuna  two  buffers from  the  current
;;	    linio,  kaj je la nuna kolumno.  line and at the current column.
;;	    La    anta^uaj   signoj   (ofte  The preceding characters (often
;;	    tabeligilo  a^u  |)  estas   la  tab   or  |)  are   the  column
;;	    kolumna disiganto.  Linioj kiuj  separator.   Lines  that  don't
;;	    ne   enhavas   ilin   ne  estas  have them  won't  be separated.
;;	    disigitaj.   Kiel  la kvara kaj  Like the  fourth and fifth line
;;	    la   kvina  linio se vi disigas  if  you split this  file from
;;	    ^ci dataron ekde la unua  angla  the first english word.
;;	    vorto.

;; Se  vi  volas  meti  longajn liniojn	 If you include long lines, i.e which
;; (ekz. programerojn) en la  kunigotan	 will span both columns  (eg.  source
;; tekston,   ili  devas  esti  en   la	 code), they should  be  in what will
;; estonte unua kolumno.  La alia devas	 be the    first column,    with  the
;; havi vakajn linion apud ili.		 associated buffer having empty lines
;;					 next to them.

;; Averto: en Emacs kiam vi ^san^gas la	 Attention:  in Emacs when you change
;; ^cefan modalon, la mal^cefaj modaloj	 the major mode,  the minor modes are
;; estas  anka^u  elmemorigitaj.   Tiu-	 also  purged  from  memory.  In that
;; okaze  vi devas religi la du bufrojn	 case you   must  reassociate the two
;; per iu  C-x 6-ordono,  ekz. C-x 6 b.	 buffers with any C-x 6-command, e.g.
;;					 C-x 6 b.

;; Kiam   vi   estos  kontenta   de  la	 When you have edited both buffers to
;; rezulto, vi kunmetos la du kolumnojn	 your  content,  you merge them  with
;; per  C-x 6 1.   Se  vi  poste  vidas	 C-x 6 1.  If you then see a problem,
;; problemon, vi  neniigu   la kunmeton	 you undo the  merge with  C-x u  and
;; per C-x u  kaj  plue  modifu  la  du	 continue   to  edit the two buffers.
;; bufrojn.  Kiam vi ne plu volas tajpi	 When you  no longer  want to edit in
;; dukolumne,  vi  eliru el la mal^cefa	 two  columns, you turn off the minor
;; modalo per C-x 6 d.			 mode with C-x 6 d.


;; Aldone al dukolumna  redaktado,  ek-	 In addition to two-column editing of
;; zemple por  skribi dulingvan tekston	 text, for example for writing a
;; flank-al-flanke kiel ^ci tiu,  aliaj	 bilingual text side-by-side as shown
;; interesaj uzoj trovitas por tiu mal-	 here, other interesting uses have
;; ^cefa modalo:			 been found for this minor mode:

;; Vi povas  disigi la  kolumnojn per {+} You can separate the columns with
;; ajna   pla^ca   ^ceno   starigante {+} any string that pleases you, by
;; `2C-separator'.   Ekzemple  "{+} " {+} setting `2C-separator'.  For example
;; por  amuzi^gi.  f2 s  a^u  C-x 6 s {+} "{+} " if you'd like to have fun.
;; traktas   tiujn    kun    prefiksa {+} f2 s or C-x 6 s handles these with a
;; argumento  kiu  signifas la longon {+} prefix argument that means the
;; de tia ^ceno.		      {+} desired length of such a string.


;; Programistoj eble ^satus la  eblecon  Programmers might like the ability
;; forspliti la komentarian kolumnon de  to split off the comment column of a
;; dosiero  kiel la sekvanta.  Vi povas  file that looks like the following.
;; rearan^gigi  la paragrafon.  La pro-  You can fill-paragraph the comment.
;; blemo  estas  ke  koda^jo tuj   lar-  The problem is, code quickly gets
;; ^gi^gas,  tiel  ke vi  bezonas   pli  rather wide, so you need to use a
;; mallar^gan   komentarian   kolumnon.  narrower comment column.  Code lines
;; Koda^jaj linioj tra `comment-column'  that reach beyond `comment-column'
;; ne problemas,  krom  ke vi ne  vidos  are no problem, except that you
;; iliajn finojn dum redaktado.		 won't see their end during editing.


;; BEGIN				-- This is just some meaningless
;;     FOR i IN 1..10 LOOP		-- code in Ada, that runs foobar
;;	   foobar( i );			-- once for each argument from one
;;     END LOOP;			-- to ten, and then we're already
;; END;					-- through with it.

;; Pli bone ankora^u, vi povas  pozici-	 Better yet, you can put the point
;; i^gi anta^u "This",  tajpi  M-3 f2 s	 before "This", type  M-3 f2 s
;; kiu  igas "-- " la separigilon inter	 which makes "-- " the separator
;; senkomentaria  Ada  bufro  kaj  nur-	 between a no-comments Ada buffer,
;; teksta  komentaria  bufro.   Kiam vi	 and a plain text comment buffer.
;; denove  kuni^gos ilin,  ^ciu  nevaka	 When you put them back together,
;; linio  de  l'  dua  kolumno   denove	 every non-empty line of the 2nd
;; anta^uhavos "-- ".			 column will again be preceded by
;;					 "-- ".


;;; Code:


;; Lucid patch
(or (fboundp 'frame-width)
    (fset 'frame-width 'screen-width))


;;;;; Set up keymap ;;;;;

(defvar 2C-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "2" '2C-two-columns)
    (define-key map [f2] '2C-two-columns)
    (define-key map "b" '2C-associate-buffer)
    (define-key map "s" '2C-split)
    map)
  "Keymap for commands for setting up two-column mode.")



;;;###autoload (autoload '2C-command "two-column" () t 'keymap)
(fset '2C-command 2C-mode-map)

;; This one is for historical reasons and simple keyboards, it is not
;; at all mnemonic.  All usual sequences containing 2 were used, and
;; f2 could not be set up in a standard way under Emacs 18.
;;;###autoload (global-set-key "\C-x6" '2C-command)

;;;###autoload (global-set-key [f2] '2C-command)


(defvar 2C-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" '2C-merge)
    (define-key map "d" '2C-dissociate)
    (define-key map "o" '2C-associated-buffer)
    (define-key map "\^m" '2C-newline)
    (define-key map "|" '2C-toggle-autoscroll)
    (define-key map "{" '2C-shrink-window-horizontally)
    (define-key map "}" '2C-enlarge-window-horizontally)
    map)
  "Keymap for commands for use in two-column mode.")


(setq minor-mode-map-alist
      (cons (cons '2C-mode
		  (let ((map (make-sparse-keymap)))
		    (substitute-key-definition '2C-command 2C-minor-mode-map
					       map (current-global-map))
		    (substitute-key-definition 'enlarge-window-horizontally
					       '2C-enlarge-window-horizontally
					       map (current-global-map))
		    (substitute-key-definition 'shrink-window-horizontally
					       '2C-shrink-window-horizontally
					       map (current-global-map))
		    map))
	    minor-mode-map-alist))

;;;;; variable declarations ;;;;;

(defgroup two-column nil
  "Minor mode for editing of two-column text."
  :prefix "2C-"
  :group 'frames)


;; Markers seem to be the only buffer-id not affected by renaming a buffer.
;; This nevertheless loses when a buffer is killed.  The variable-name is
;; required by `describe-mode'.
(defvar 2C-mode nil
  "Marker to the associated buffer, if non-nil.")
(make-variable-buffer-local '2C-mode)
(put '2C-mode 'permanent-local t)



(setq minor-mode-alist (cons '(2C-mode " 2C") minor-mode-alist))



;; rearranged, so that the pertinent info will show in 40 columns
(defcustom 2C-mode-line-format
	'("-%*- %15b --"  (-3 . "%p")  "--%[("  mode-name
	  minor-mode-alist  "%n"  mode-line-process  ")%]%-")
  "Value of `mode-line-format' for a buffer in two-column minor mode."
  :type 'sexp
  :group 'two-column)


(defcustom 2C-other-buffer-hook 'text-mode
  "Hook run in new buffer when it is associated with current one."
  :type 'function
  :group 'two-column)


(defcustom 2C-separator ""
  "A string inserted between the two columns when merging.
This gets set locally by \\[2C-split]."
  :type 'string
  :group 'two-column)
(put '2C-separator 'permanent-local t)



(defcustom 2C-window-width 40
  "The width of the first column.  (Must be at least `window-min-width')
This value is local for every buffer that sets it."
  :type 'integer
  :group 'two-column)
(make-variable-buffer-local '2C-window-width)
(put '2C-window-width 'permanent-local t)



(defcustom 2C-beyond-fill-column 4
  "Base for calculating `fill-column' for a buffer in two-column minor mode.
The value of `fill-column' becomes `2C-window-width' for this buffer
minus this value."
  :type 'integer
  :group 'two-column)



(defcustom 2C-autoscroll t
  "If non-nil, Emacs attempts to keep the two column's buffers aligned."
  :type 'boolean
  :group 'two-column)



(defvar 2C-autoscroll-start nil)
(make-variable-buffer-local '2C-autoscroll-start)

;;;;; base functions ;;;;;

;; The access method for the other buffer.  This tries to remedy against
;; lost local variables and lost buffers.
(defun 2C-other (&optional req)
  (or (if 2C-mode
	  (or (prog1
		  (marker-buffer 2C-mode)
		(setq mode-line-format 2C-mode-line-format))
	      ;; The associated buffer somehow got killed.
	      (progn
		;; The other variables may later be useful if the user
		;; reestablishes the association.
		(kill-local-variable '2C-mode)
		(kill-local-variable 'mode-line-format)
		nil)))
      (if req (error "You must first set two-column minor mode"))))



;; function for setting up two-column minor mode in a buffer associated
;; with the buffer pointed to by the marker other.
(defun 2C-mode (other)
  "Minor mode for independently editing two columns.
This is set up for two associated buffers by the three commands bound
to  \\[2C-two-columns] ,  \\[2C-associate-buffer]  and  \\[2C-split].
Turning on two-column mode calls the value of the variable `2C-mode-hook',
if that value is non-nil.

These buffers can be edited separately, for example with `fill-paragraph'.
If you want to disable parallel scrolling temporarily, use  \\[2C-toggle-autoscroll] .

If you include long lines, i.e which will span both columns (eg.
source code), they should be in what will be the first column, with
the associated buffer having empty lines next to them.

Potential uses are writing bilingual texts, or editing the comments of a
source code.  See the file lisp/two-column.el for detailed examples.

You have the following commands at your disposal:

\\[2C-two-columns]   Rearrange screen with current buffer first
\\[2C-associate-buffer]   Reassociate buffer after changing major mode
\\[shrink-window-horizontally], \\[enlarge-window-horizontally]   Shrink, enlarge current column
\\[2C-associated-buffer]   Switch to associated buffer at same point
\\[2C-newline] Insert newline(s) in both buffers at same point
\\[2C-merge]   Merge both buffers
\\[2C-dissociate]   Dissociate the two buffers

These keybindings can be customized in your ~/.emacs by `2C-mode-map',
`2C-minor-mode-map' and by binding `2C-command' to some prefix.

The appearance of the screen can be customized by the variables
`2C-window-width', `2C-beyond-fill-column', `2C-mode-line-format' and
`truncate-partial-width-windows'."
  (add-hook 'post-command-hook '2C-autoscroll nil t)
  (setq fill-column (- 2C-window-width
		       2C-beyond-fill-column)
	mode-line-format 2C-mode-line-format
	2C-mode other)
  (run-hooks '2C-mode-hook))



;;;###autoload
(defun 2C-two-columns (&optional buffer)
  "Split current window vertically for two-column editing.
\\<global-map>When called the first time, associates a buffer with the current
buffer in two-column minor mode (use \\[describe-mode] once in the mode,
for details.).  It runs `2C-other-buffer-hook' in the new buffer.
When called again, restores the screen layout with the current buffer
first and the associated buffer to its right."
  (interactive "P")
  ;; first go to full width, so that we can certainly split into two windows
  (unless (window-full-width-p)
    (enlarge-window 99999 t))
  (split-window-right
   (max window-min-width (min 2C-window-width
			      (- (frame-width) window-min-width))))
  (if (2C-other)
      (progn
	(other-window 1)
	(switch-to-buffer (2C-other))
	(other-window -1)
	(if 2C-autoscroll
	    (2C-toggle-autoscroll t)))

    (2C-mode (prog1 (point-marker)
	       (other-window 1)
	       (switch-to-buffer
		(or buffer
		    (generate-new-buffer (concat "2C/" (buffer-name)))))
	       (or buffer
		   (run-hooks '2C-other-buffer-hook))))

    (2C-mode (prog1 (point-marker)
	       (other-window -1)))))



;;;###autoload
(defun 2C-associate-buffer ()
  "Associate another buffer with this one in two-column minor mode.
Can also be used to associate a just previously visited file, by
accepting the proposed default buffer.

\(See  \\[describe-mode] .)"
  (interactive)
  (let ((b1 (current-buffer))
	(b2 (or (2C-other)
		(read-buffer "Associate buffer: " (other-buffer)))))
    (save-excursion
      (setq 2C-mode nil)
      (set-buffer b2)
      (and (2C-other)
	   (not (eq b1 (2C-other)))
	   (error "Buffer already associated with buffer `%s'"
		  (buffer-name (2C-other))))
      (setq b1 (and (assq '2C-window-width (buffer-local-variables))
		    2C-window-width)))
    ; if other buffer has a local width, adjust here too
    (if b1 (setq 2C-window-width (- (frame-width) b1)))
    (2C-two-columns b2)))



;;;###autoload
(defun 2C-split (arg)
  "Split a two-column text at point, into two buffers in two-column minor mode.
Point becomes the local value of `2C-window-width'.  Only lines that
have the ARG same preceding characters at that column get split.  The
ARG preceding characters without any leading whitespace become the local
value for `2C-separator'.  This way lines that continue across both
columns remain untouched in the first buffer.

This function can be used with a prototype line, to set up things.  You
write the first line of each column and then split that line.  E.g.:

First column's text    sSs  Second column's text
		       \\___/\\
			/    \\
   5 character Separator      You type  M-5 \\[2C-split]  with the point here.

\(See  \\[describe-mode] .)"
  (interactive "*p")
  (and (2C-other)
       (if (y-or-n-p (concat "Overwrite associated buffer `"
			     (buffer-name (2C-other))
			     "'? "))
	   (with-current-buffer (2C-other)
	     (erase-buffer))
	 (signal 'quit nil)))
  (let ((point (point))
	;; make next-line always come back to same column
	(column (current-column))
	;; a counter for empty lines in other buffer
	(n (1- (count-lines (point-min) (point))))
	chars other)
    (save-excursion
      (backward-char arg)
      (setq chars (buffer-substring (point) point))
      (skip-chars-forward " \t" point)
      (make-local-variable '2C-separator)
      (setq 2C-separator (buffer-substring (point) point)
	    2C-window-width (+ (fringe-columns 'left)
			       (fringe-columns 'right)
			       (scroll-bar-columns 'left)
			       (scroll-bar-columns 'right)
			       (current-column))))
    (2C-two-columns)
    (setq other (2C-other))
    ;; now we're ready to actually split
    (save-excursion
      (while (not (eobp))
	(if (not (and (= (current-column) column)
		      (string= chars
			       (buffer-substring (point)
						 (save-excursion
						   (backward-char arg)
						   (point))))))
	    (setq n (1+ n))
	  (setq point (point))
	  (backward-char arg)
	  (skip-chars-backward " \t")
	  (delete-region point (point))
	  (setq point (point))
	  (insert-char ?\n n)
	  (append-to-buffer other point (progn (end-of-line)
					       (if (eobp)
						   (point)
						 (1+ (point)))))
	  (delete-region point (point))
	  (setq n 0))
	(forward-line 1)
	(move-to-column column)))))




(defun 2C-dissociate ()
  "Turn off two-column minor mode in current and associated buffer.
If the associated buffer is unmodified and empty, it is killed."
  (interactive)
  (let ((buffer (current-buffer)))
    (save-excursion
      (and (2C-other)
	   (set-buffer (2C-other))
	   (or (not (2C-other))
	       (eq buffer (2C-other)))
	   (if (and (not (buffer-modified-p))
		    (eobp) (bobp))
	       (kill-buffer nil)
	     (kill-local-variable '2C-mode)
	     (kill-local-variable '2C-window-width)
	     (kill-local-variable '2C-separator)
	     (kill-local-variable 'mode-line-format)
	     (kill-local-variable 'fill-column))))
    (kill-local-variable '2C-mode)
    (kill-local-variable '2C-window-width)
    (kill-local-variable '2C-separator)
    (kill-local-variable 'mode-line-format)
    (kill-local-variable 'fill-column)))



;; this doesn't use yank-rectangle, so that the first column can
;; contain long lines
(defun 2C-merge ()
  "Merges the associated buffer with the current buffer.
They get merged at the column, which is the value of `2C-window-width',
i.e. usually at the vertical window separator.  This separator gets
replaced with white space.  Beyond that the value of `2C-separator' gets
inserted on merged lines.  The two columns are thus pasted side by side,
in a single text.  If the other buffer is not displayed to the left of
this one, then this one becomes the left column.

If you want `2C-separator' on empty lines in the second column,
you should put just one space in them.  In the final result, you can strip
off trailing spaces with \\[delete-trailing-whitespace]."
  (interactive)
  (and (> (car (window-edges)) 0)	; not touching left edge of screen
       (eq (window-buffer (previous-window))
	   (2C-other t))
       (other-window -1))
  (save-excursion
    (let ((b1 (current-buffer))
	  (b2 (2C-other t))
	  string)
      (goto-char (point-min))
      (set-buffer b2)
      (goto-char (point-min))
      (while (not (eobp))
	(setq string (buffer-substring (point)
				       (progn (end-of-line) (point))))
	(or (eobp)
	    (forward-char))		; next line
	(set-buffer b1)
	(if (string= string "")
	    ()
	  (end-of-line)
	  (indent-to-column 2C-window-width)
	  (insert 2C-separator string))
	(forward-line 1)		; add one if necessary
	(set-buffer b2))))
  (unless (window-full-width-p)
    (enlarge-window 99999 t)))

;;;;; utility functions ;;;;;

(defun 2C-associated-buffer ()
  "Switch to associated buffer."
  (interactive)
  (let ((line (+ (count-lines (point-min) (point))
		 (if (bolp) 1 0)))
	(col (if (eolp) (if (bolp) 0) (current-column))))
    (if (get-buffer-window (2C-other t))
	(select-window (get-buffer-window (2C-other)))
      (switch-to-buffer (2C-other)))
    (goto-char (point-min))
    (newline (forward-line (1- line)))
    (if col
	(move-to-column col)
      (end-of-line 1))))

(defun 2C-newline (arg)
  "Insert ARG newlines in both buffers."
  (interactive "P")
  (save-window-excursion
    (2C-associated-buffer)
    (newline arg))
  (newline arg))

(defun 2C-toggle-autoscroll (arg)
  "Toggle autoscrolling.
With prefix argument ARG, turn on autoscrolling if ARG is
positive, otherwise turn it off.  When autoscrolling is turned
on, this also realigns the two buffers."
  (interactive "P")
  ;(sit-for 0)
  (setq 2C-autoscroll-start (window-start))
  (if (setq 2C-autoscroll (if arg
			      (>= (prefix-numeric-value arg) 0)
			    (not 2C-autoscroll)))
      (select-window
       (prog1 (selected-window)
	 (message "Autoscrolling is on.")
	 (setq arg (count-lines (point-min) (window-start)))
	 (if (get-buffer-window (2C-other t))
	     (progn
	       (select-window (get-buffer-window (2C-other)))
	       (setq arg (- arg (count-lines (point-min) (window-start))))
	       ;; make sure that other buffer has enough lines
	       (save-excursion
		 (insert-char ?\n
			      (- arg (count-lines (window-start)
						  (goto-char (point-max)))
				 -1)))
	       (scroll-up arg)))))
    (message "Autoscrolling is off.")))



(defun 2C-autoscroll ()
  (if 2C-autoscroll
      ;; catch a mouse scroll on non-selected scrollbar
      (select-window
       (prog1 (selected-window)
	 (and (consp last-command-event)
	      (not (eq (selected-window)
		       (car (car (cdr last-command-event)))))
	      (select-window (car (car (cdr last-command-event)))))
	 ;; In some cases scrolling causes an error, but post-command-hook
	 ;; shouldn't, and should always stay in the original window
	 (condition-case ()
	     (and (or 2C-autoscroll-start (2C-toggle-autoscroll t) nil)
		  (/= (window-start) 2C-autoscroll-start)
		  (2C-other)
		  (get-buffer-window (2C-other))
		  (let ((lines (count-lines (window-start)
					    2C-autoscroll-start)))
		    (if (< (window-start) 2C-autoscroll-start)
			(setq lines (- lines)))
		    (setq 2C-autoscroll-start (window-start))
		    (select-window (get-buffer-window (2C-other)))
		    ;; make sure that other buffer has enough lines
		    (save-excursion
		      (insert-char
		       ?\n (- lines (count-lines (window-start)
						 (goto-char (point-max)))
			      -1)))
		    (scroll-up lines)
		    (setq 2C-autoscroll-start (window-start))))
	   (error))))))



(defun 2C-enlarge-window-horizontally (arg)
  "Make current window ARG columns wider."
  (interactive "p")
  (enlarge-window arg t)
  (and (2C-other)
       (setq 2C-window-width (+ 2C-window-width arg))
       (set-buffer (2C-other))
       (setq 2C-window-width (- 2C-window-width arg))))

(defun 2C-shrink-window-horizontally (arg)
  "Make current window ARG columns narrower."
  (interactive "p")
  (2C-enlarge-window-horizontally (- arg)))



(provide 'two-column)

;;; two-column.el ends here
