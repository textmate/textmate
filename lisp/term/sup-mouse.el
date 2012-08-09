;;; sup-mouse.el --- supdup mouse support for lisp machines

;; Copyright (C) 1985-1986, 2001-2012 Free Software Foundation, Inc.

;; Author: Wolfgang Rupprecht
;; Maintainer: FSF
;; Created: 21 Nov 1986
;; Keywords: hardware

;;     (from code originally written by John Robinson@bbn for the bitgraph)

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

;;;  User customization option:

(defcustom sup-mouse-fast-select-window nil
  "Non-nil means mouse hits select new window, then execute.
Otherwise just select."
  :type 'boolean
  :group 'mouse)

(defconst mouse-left 0)
(defconst mouse-center 1)
(defconst mouse-right 2)

(defconst mouse-2left 4)
(defconst mouse-2center 5)
(defconst mouse-2right 6)

(defconst mouse-3left 8)
(defconst mouse-3center 9)
(defconst mouse-3right 10)

;;;  Defuns:

(defun sup-mouse-report ()
  "This function is called directly by the mouse, it parses and
executes the mouse commands.

 L move point          *  |---- These apply for mouse click in a window.
2L delete word            |
3L copy word		  | If sup-mouse-fast-select-window is nil,
 C move point and yank *  | just selects that window.
2C yank pop		  |
 R set mark            *  |
2R delete region	  |
3R copy region		  |

on modeline		    on \"scroll bar\"	in minibuffer
 L scroll-up		    line to top		execute-extended-command
 C proportional goto-char   line to middle	mouse-help
 R scroll-down		    line to bottom	eval-expression"

  (interactive)
  (let*
;; expect a string of <esc>:<buttons>;<x-pos>;<y-pos>c
      ((buttons (sup-get-tty-num ?\;))
       (x (sup-get-tty-num ?\;))
       (y (sup-get-tty-num ?c))
       (window (sup-pos-to-window x y))
       (edges (window-edges window))
       (old-window (selected-window))
       (in-minibuf-p (eq y (1- (frame-height))))
       (same-window-p (and (not in-minibuf-p) (eq window old-window)))
       (in-modeline-p (eq y (1- (nth 3 edges))))
       (in-scrollbar-p (>= x (1- (nth 2 edges)))))
    (setq x (- x (nth 0 edges)))
    (setq y (- y (nth 1 edges)))

;    (error "mouse-hit %d %d %d" buttons x y) ;;;; debug

    (cond (in-modeline-p
	   (select-window window)
	   (cond ((= buttons mouse-left)
		  (scroll-up))
		 ((= buttons mouse-right)
		  (scroll-down))
		 ((= buttons mouse-center)
		  (goto-char (/ (* x
				   (- (point-max) (point-min)))
				(1- (window-width))))
		  (beginning-of-line)
		  (what-cursor-position)))
	   (select-window old-window))
	  (in-scrollbar-p
	   (select-window window)
	   (scroll-up
	    (cond ((= buttons mouse-left)
		   y)
		  ((= buttons mouse-right)
		   (+ y (- 2 (window-height))))
		  ((= buttons mouse-center)
		   (/ (+ 2 y y (- (window-height))) 2))
		  (t
		   0)))
	   (select-window old-window))
	  (same-window-p
	   (cond ((= buttons mouse-left)
		  (sup-move-point-to-x-y x y))
		 ((= buttons mouse-2left)
		  (sup-move-point-to-x-y x y)
		  (kill-word 1))
		 ((= buttons mouse-3left)
		  (sup-move-point-to-x-y x y)
		  (save-excursion
		    (copy-region-as-kill
		     (point) (progn (forward-word 1) (point))))
		  (setq this-command 'yank)
		  )
		 ((= buttons mouse-right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (exchange-point-and-mark))
		 ((= buttons mouse-2right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (kill-region (mark) (point)))
		 ((= buttons mouse-3right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (copy-region-as-kill (mark) (point))
		  (setq this-command 'yank))
		 ((= buttons mouse-center)
		  (sup-move-point-to-x-y x y)
		  (setq this-command 'yank)
		  (yank))
		 ((= buttons mouse-2center)
		  (yank-pop 1))
		 )
	   )
	  (in-minibuf-p
	   (cond ((= buttons mouse-right)
		  (call-interactively 'eval-expression))
		 ((= buttons mouse-left)
		  (call-interactively 'execute-extended-command))
		 ((= buttons mouse-center)
		  (describe-function 'sup-mouse-report)); silly self help
		 ))
	  (t				;in another window
	   (select-window window)
	   (cond ((not sup-mouse-fast-select-window))
		 ((= buttons mouse-left)
		  (sup-move-point-to-x-y x y))
		 ((= buttons mouse-right)
		  (push-mark)
		  (sup-move-point-to-x-y x y)
		  (exchange-point-and-mark))
		 ((= buttons mouse-center)
		  (sup-move-point-to-x-y x y)
		  (setq this-command 'yank)
		  (yank))
		 ))
	  )))


(defun sup-get-tty-num (term-char)
  "Read from terminal until TERM-CHAR is read, and return intervening number.
Upon non-numeric not matching TERM-CHAR signal an error."
  (let
      ((num 0)
       (char (read-char)))
    (while (and (>= char ?0)
		(<= char ?9))
      (setq num (+ (* num 10) (- char ?0)))
      (setq char (read-char)))
    (or (eq term-char char)
	(error "Invalid data format in mouse command"))
    num))

(defun sup-move-point-to-x-y (x y)
  "Position cursor in window coordinates.
X and Y are 0-based character positions in the window."
  (move-to-window-line y)
  (move-to-column x)
  )

(defun sup-pos-to-window (x y)
  "Find window corresponding to frame coordinates.
X and Y are 0-based character positions on the frame."
  (get-window-with-predicate (lambda (w)
			       (coordinates-in-window-p (cons x y) w))))

;;; sup-mouse.el ends here
