;;; wyse50.el --- terminal support code for Wyse 50 -*- no-byte-compile: t -*-

;; Copyright (C) 1989, 1993-1994, 2001-2012 Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>,
;;	Jim Blandy <jimb@occs.cs.oberlin.edu>
;; Keywords: terminals

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

;; Uses the Emacs 19 terminal initialization features --- won't work with 18.
;; Rewritten for Emacs 19 by jimb,  January 1992
;; Cleaned up for new terminal package conventions by esr, March 1993
;; Should work well for Televideo TVI 925 although it's overkill.
;;
;; The Wyse50 is ergonomically wonderful, but its escape-sequence design sucks
;; rocks.  The left-arrow key emits a backspace (!) and the down-arrow a line
;; feed (!!).  Thus, you have to unbind some commonly-used Emacs keys to
;; enable the arrows.

;;; Code:

(defvar wyse50-terminal-map
  (let ((map (make-sparse-keymap)))
    (dolist (key-definition
             '( ;; These might be set up by termcap and terminfo
               ("\C-k"	[up])
               ("\C-j"	[down])
               ("\C-l"	[right])
               ("\C-h"	[left])
               ("\^a@\^m"	[f1])
               ("\^aA\^m"	[f2])
               ("\^aB\^m"	[f3])
               ("\^aC\^m"	[f4])
               ("\^aD\^m"	[f5])
               ("\^aE\^m"	[f6])
               ("\^aF\^m"	[f7])
               ("\^aG\^m"	[f8])
               ("\^aH\^m"	[f9])

               ;; These might be set up by terminfo
               ("\eK"	[next])
               ("\eT"	[clearline])
               ("\^^"	[home])
               ("\e\^^"	[end])
               ("\eQ"	[insert])
               ("\eE"	[insertline])
               ("\eR"	[deleteline])
               ("\eP"	[print])
               ("\er"	[replace])
               ("\^aI\^m"	[f10])
               ("\^aJ\^m"	[f11])
               ("\^aK\^m"	[f12])
               ("\^aL\^m"	[f13])
               ("\^aM\^m"	[f14])
               ("\^aN\^m"	[f15])
               ("\^aO\^m"	[f16])
               ("\^a`\^m"	[f17])
               ("\^aa\^m"	[f18])
               ("\^ab\^m"	[f19])
               ("\^ac\^m"	[f20])
               ("\^ad\^m"	[f21])
               ("\^ae\^m"	[f22])
               ("\^af\^m"	[f23])
               ("\^ag\^m"	[f24])
               ("\^ah\^m"	[f25])
               ("\^ai\^m"	[f26])
               ("\^aj\^m"	[f27])
               ("\^ak\^m"	[f28])
               ("\^al\^m"	[f29])
               ("\^am\^m"	[f30])
               ("\^an\^m"	[f31])
               ("\^ao\^m"	[f32])

               ;; Terminfo may know about these, but X won't
               ("\eI"	[key-stab])  ;; Not an X keysym
               ("\eJ"	[key-snext]) ;; Not an X keysym
               ("\eY"	[key-clear]) ;; Not an X keysym

               ;; These are totally strange :-)
               ("\eW"	[?\C-?])		;; Not an X keysym
               ("\^a\^k\^m"	[funct-up])	;; Not an X keysym
               ("\^a\^j\^m"	[funct-down])	;; Not an X keysym
               ("\^a\^l\^m"	[funct-right])	;; Not an X keysym
               ("\^a\^h\^m"	[funct-left])	;; Not an X keysym
               ("\^a\^m\^m"	[funct-return])	;; Not an X keysym
               ("\^a\^i\^m"	[funct-tab])	;; Not an X keysym
               ))
      (define-key map
        (car key-definition) (nth 1 key-definition)))
    map))

(defun terminal-init-wyse50 ()
  "Terminal initialization function for wyse50."
  ;; Use inheritance to let the main keymap override these defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (let ((m (copy-keymap wyse50-terminal-map)))
    (set-keymap-parent m (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map m))

  ;; Miscellaneous hacks

  ;; This is an ugly hack for a nasty problem:
  ;; Wyse 50 takes one character cell to store video attributes (which seems to
  ;; explain width 79 rather than 80, column 1 is not used!!!).
  ;; On killing (C-x C-c) the end inverse code (on column 1 of line 24)
  ;; of the mode line is overwritten AFTER all the y-or-n questions.
  ;; This causes the attribute to remain in effect until the mode line has
  ;; scrolled of the screen.  Suspending (C-z) does not cause this problem.
  ;; On such terminals, Emacs should sacrifice the first and last character of
  ;; each mode line, rather than a whole screen column!
  (add-hook 'kill-emacs-hook
	    (function (lambda () (interactive)
			(send-string-to-terminal
			 (concat "\ea23R" (1+ (frame-width)) "C\eG0"))))))

(defun enable-arrow-keys ()
  "To be called by `term-setup-hook'. Overrides 6 Emacs standard keys
whose functions are then typed as follows:
C-a	Funct Left-arrow
C-h	M-?
LFD	Funct Return, some modes override down-arrow via LFD
C-k	CLR Line
C-l	Scrn CLR
M-r	M-x move-to-window-line, Funct up-arrow or down-arrow are similar"
  (interactive)
  ;; Not needed any more now that we use input-decode-map.
  ;; (dolist (key-definition 
  ;;         ;; By unsetting C-a and then binding it to a prefix, we
  ;;         ;; allow the rest of the function keys which start with C-a
  ;;         ;; to be recognized.
  ;;         '(("\C-a"	nil)
  ;;           ("\C-k"	nil)
  ;;           ("\C-j"	nil)
  ;;           ("\C-l"	nil)
  ;;           ("\C-h"	nil)
  ;;           ("\er"	nil)))
  ;;   (global-set-key (car key-definition)
  ;;                   (nth 1 key-definition)))
  (fset 'enable-arrow-keys nil))

;;; wyse50.el ends here
