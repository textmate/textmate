;;; ediff-help.el --- Code related to the contents of Ediff help buffers

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Michael Kifer <kifer@cs.stonybrook.edu>
;; Package: ediff

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


;; Compiler pacifier start
(defvar ediff-multiframe)
;; end pacifier

(require 'ediff-init)

;; Help messages

(defconst ediff-long-help-message-head
  "    Move around      |      Toggle features      |        Manipulate
=====================|===========================|============================="
  "The head of the full help message.")
(defconst ediff-long-help-message-tail
  "=====================|===========================|=============================
    R -show registry |     = -compare regions    |  M   -show session group
    D -diff output   |     E -browse Ediff manual|  G   -send bug report
    i -status info   |     ? -help off           |  z/q -suspend/quit
-------------------------------------------------------------------------------
For help on a specific command:  Click Button 2 over it; or
              			 Put the cursor over it and type RET."
  "The tail of the full-help message.")

(defconst ediff-long-help-message-compare3
  "
p,DEL -previous diff |     | -vert/horiz split   | xy -copy buf X's region to Y
n,SPC -next diff     |     h -highlighting       | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      |    #c -ignore case        |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -rotate buffers|     m -wide display       |
"
  "Help message usually used for 3-way comparison.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-compare2
  "
p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A
n,SPC -next diff     |     h -highlighting       | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      |    #c -ignore case        |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -swap variants |     m -wide display       |
"
  "Help message usually used for 2-way comparison.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-narrow2
  "
p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A
n,SPC -next diff     |     h -highlighting       | rx -restore buf X's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      |    #c -ignore case        |  % -narrow/widen buffs
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -swap variants |     m -wide display       |
"
  "Help message when comparing windows or regions line-by-line.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-word-mode
  "
p,DEL -previous diff |     | -vert/horiz split   | xy -copy buf X's region to Y
n,SPC -next diff     |     h -highlighting       | rx -restore buf X's old diff
    j -jump to diff  |                           |
   gx -goto X's point|    % -narrow/widen buffs  |  ! -recompute diffs
  C-l -recenter      |    #c -ignore case        |
  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X
  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output
    ~ -swap variants |     m -wide display       |
"
  "Help message when comparing windows or regions word-by-word.
Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-long-help-message-merge
  "
p,DEL -previous diff |     | -vert/horiz split   |  x -copy buf X's region to C
n,SPC -next diff     |     h -highlighting       |  r -restore buf C's old diff
    j -jump to diff  |     @ -auto-refinement    |  * -refine current region
   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions
  C-l -recenter      | #f/#h -focus/hide regions |  + -combine diff regions
  v/V -scroll up/dn  |     X -read-only in buf X | wx -save buf X
  </> -scroll lt/rt  |     m -wide display       | wd -save diff output
    ~ -swap variants |     s -shrink window C    |  / -show ancestor buff
                     |  $$ -show clashes only    |  & -merge w/new default
                     |  $* -skip changed regions |
"
  "Help message for merge sessions.
Normally, not a user option.  See `ediff-help-message' for details.")

;; The actual long help message.
(ediff-defvar-local ediff-long-help-message ""
  "Normally, not a user option.  See `ediff-help-message' for details.")

(defconst ediff-brief-message-string
  " Type ? for help"
  "Contents of the brief help message.")
;; The actual brief help message
(ediff-defvar-local ediff-brief-help-message ""
  "Normally, not a user option.  See `ediff-help-message' for details.")

(ediff-defvar-local ediff-brief-help-message-function nil
  "The brief help message that the user can customize.
If the user sets this to a parameter-less function, Ediff will use it to
produce the brief help message.  This function must return a string.")
(ediff-defvar-local ediff-long-help-message-function nil
  "The long help message that the user can customize.
See `ediff-brief-help-message-function' for more.")

(defcustom ediff-use-long-help-message nil
  "If t, Ediff displays a long help message.  Short help message otherwise."
  :type 'boolean
  :group 'ediff-window)

;; The actual help message.
(ediff-defvar-local ediff-help-message ""
  "The actual help message.
Normally, the user shouldn't touch this.  However, if you want Ediff to
start up with different help messages for different jobs, you can change
the value of this variable and the variables `ediff-help-message-*' in
`ediff-startup-hook'.")


;; the keymap that defines clicks over the quick help regions
(defvar ediff-help-region-map (make-sparse-keymap))

(define-key
  ediff-help-region-map
  (if (featurep 'emacs) [mouse-2] [button2])
  'ediff-help-for-quick-help)

;; runs in the control buffer
(defun ediff-set-help-overlays ()
  (goto-char (point-min))
  (let (overl beg end cmd)
    (while (re-search-forward " *\\([^ \t\n|]+\\||\\) +-[^|\n]+" nil 'noerror)
      (setq beg (match-beginning 0)
	    end (match-end 0)
	    cmd (buffer-substring (match-beginning 1) (match-end 1)))
      (setq overl (ediff-make-overlay beg end))
      (if (featurep 'emacs)
	  (ediff-overlay-put overl 'mouse-face 'highlight)
	(ediff-overlay-put overl 'highlight t))
      (ediff-overlay-put overl 'ediff-help-info cmd))))


(defun ediff-help-for-quick-help ()
  "Explain Ediff commands in more detail."
  (interactive)
  (ediff-barf-if-not-control-buffer)
  (let ((pos (ediff-event-point last-command-event))
	overl cmd)

    (if (featurep 'xemacs)
	(setq overl (extent-at pos (current-buffer) 'ediff-help-info)
	      cmd   (ediff-overlay-get overl 'ediff-help-info))
      (setq cmd (car (mapcar (lambda (elt)
			       (overlay-get elt 'ediff-help-info))
			     (overlays-at pos)))))

    (if (not (stringp cmd))
	(error "Hmm...  I don't see an Ediff command around here..."))

    (ediff-documentation "Quick Help Commands")

    (let (case-fold-search)
      (cond ((string= cmd "?") (re-search-forward "^`\\?'"))
	    ((string= cmd "G") (re-search-forward "^`G'"))
	    ((string= cmd "E") (re-search-forward "^`E'"))
	    ((string= cmd "wd") (re-search-forward "^`wd'"))
	    ((string= cmd "wx") (re-search-forward "^`wa'"))
	    ((string= cmd "a/b") (re-search-forward "^`a'"))
	    ((string= cmd "x") (re-search-forward "^`a'"))
	    ((string= cmd "xy") (re-search-forward "^`ab'"))
	    ((string= cmd "p,DEL") (re-search-forward "^`p'"))
	    ((string= cmd "n,SPC") (re-search-forward "^`n'"))
	    ((string= cmd "j") (re-search-forward "^`j'"))
	    ((string= cmd "gx") (re-search-forward "^`ga'"))
	    ((string= cmd "!") (re-search-forward "^`!'"))
	    ((string= cmd "*") (re-search-forward "^`\\*'"))
	    ((string= cmd "m") (re-search-forward "^`m'"))
	    ((string= cmd "|") (re-search-forward "^`|'"))
	    ((string= cmd "@") (re-search-forward "^`@'"))
	    ((string= cmd "h") (re-search-forward "^`h'"))
	    ((string= cmd "r") (re-search-forward "^`r'"))
	    ((string= cmd "rx") (re-search-forward "^`ra'"))
	    ((string= cmd "##") (re-search-forward "^`##'"))
	    ((string= cmd "#c") (re-search-forward "^`#c'"))
	    ((string= cmd "#f/#h") (re-search-forward "^`#f'"))
	    ((string= cmd "X") (re-search-forward "^`A'"))
	    ((string= cmd "v/V") (re-search-forward "^`v'"))
	    ((string= cmd "</>") (re-search-forward "^`<'"))
	    ((string= cmd "~") (re-search-forward "^`~'"))
	    ((string= cmd "i") (re-search-forward "^`i'"))
	    ((string= cmd "D") (re-search-forward "^`D'"))
	    ((string= cmd "R") (re-search-forward "^`R'"))
	    ((string= cmd "M") (re-search-forward "^`M'"))
	    ((string= cmd "z/q") (re-search-forward "^`z'"))
	    ((string= cmd "%") (re-search-forward "^`%'"))
	    ((string= cmd "C-l") (re-search-forward "^`C-l'"))
	    ((string= cmd "$$") (re-search-forward "^`\\$\\$'"))
	    ((string= cmd "$*") (re-search-forward "^`\\$\\*'"))
	    ((string= cmd "/") (re-search-forward "^`/'"))
	    ((string= cmd "&") (re-search-forward "^`&'"))
	    ((string= cmd "s") (re-search-forward "^`s'"))
	    ((string= cmd "+") (re-search-forward "^`\\+'"))
	    ((string= cmd "=") (re-search-forward "^`='"))
	    (t (error "Undocumented command! Type `G' in Ediff Control Panel to drop a note to the Ediff maintainer")))
      ) ; let case-fold-search
    ))


;; assuming we are in control window, calculate length of the first line in
;; help message
(defun ediff-help-message-line-length ()
  (save-excursion
    (goto-char (point-min))
    (if ediff-use-long-help-message
	(forward-line 1))
    (end-of-line)
    (current-column)))


(defun ediff-indent-help-message ()
  (let* ((shift (/ (max 0 (- (window-width (selected-window))
			     (ediff-help-message-line-length)))
		   2))
	 (str (make-string shift ?\ )))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(insert str)
	(beginning-of-line)
	(forward-line 1)))))


;; compose the help message as a string
(defun ediff-set-help-message ()
  (setq ediff-long-help-message
	(cond ((and ediff-long-help-message-function
		    (or (symbolp ediff-long-help-message-function)
			(consp ediff-long-help-message-function)))
	       (funcall ediff-long-help-message-function))
	      (ediff-word-mode
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-word-mode
		       ediff-long-help-message-tail))
	      (ediff-narrow-job
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-narrow2
		       ediff-long-help-message-tail))
	      (ediff-merge-job
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-merge
		       ediff-long-help-message-tail))
	      (ediff-diff3-job
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-compare3
		       ediff-long-help-message-tail))
	      (t
	       (concat ediff-long-help-message-head
		       ediff-long-help-message-compare2
		       ediff-long-help-message-tail))))
  (setq ediff-brief-help-message
	(cond ((and ediff-brief-help-message-function
		    (or (symbolp ediff-brief-help-message-function)
			(consp ediff-brief-help-message-function)))
	       (funcall ediff-brief-help-message-function))
	      ((stringp ediff-brief-help-message-function)
	       ediff-brief-help-message-function)
	      ((ediff-multiframe-setup-p) ediff-brief-message-string)
	      (t ; long brief msg, not multiframe --- put in the middle
	       ediff-brief-message-string)
	      ))
  (setq ediff-help-message (if ediff-use-long-help-message
			       ediff-long-help-message
			     ediff-brief-help-message))
  (run-hooks 'ediff-display-help-hook))

;;;###autoload
(defun ediff-customize ()
  (interactive)
  (customize-group "ediff"))


(provide 'ediff-help)


;;; ediff-help.el ends here
