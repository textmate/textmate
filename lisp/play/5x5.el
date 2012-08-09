;;; 5x5.el --- simple little puzzle game -*- coding: utf-8 -*-

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: Dave Pearson <davep@davep.org>
;; Maintainer: Dave Pearson <davep@davep.org>
;; Created: 1998-10-03
;; Keywords: games puzzles

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

;; The aim of 5x5 is to fill in all the squares.  If you need any more of an
;; explanation you probably shouldn't play the game.

;;; TODO:

;; o The code for updating the grid needs to be re-done.  At the moment it
;;   simply re-draws the grid every time a move is made.
;;
;; o Look into tarting up the display with color.  gamegrid.el looks
;;   interesting, perhaps that is the way to go?

;;; Thanks:

;; Ralf Fassel <ralf@akutech.de> for his help and introduction to writing an
;; emacs mode.
;;
;; Pascal Q. Porcupine <joshagam@cs.nmsu.edu> for inspiring the animated
;; cracker.
;;
;; Vincent Belaïche <vincentb1@users.sourceforge.net> & Jay P. Belanger
;; <jay.p.belanger@gmail.com> for the math solver.

;;; Code:

;; Things we need.

(eval-when-compile
  (require 'cl))

;; Customize options.

(defgroup 5x5 nil
  "5x5 - Silly little puzzle game."
  :group  'games
  :prefix "5x5-")

(defcustom 5x5-grid-size 5
  "Size of the playing area."
  :type  'integer
  :group '5x5)

(defcustom 5x5-x-scale 4
  "X scaling factor for drawing the grid."
  :type  'integer
  :group '5x5)

(defcustom 5x5-y-scale 3
  "Y scaling factor for drawing the grid."
  :type  'integer
  :group '5x5)

(defcustom 5x5-animate-delay .01
  "Delay in seconds when animating a solution crack."
  :type  'number
  :group '5x5)

(defcustom 5x5-hassle-me t
  "Should 5x5 ask you when you want to do a destructive operation?"
  :type  'boolean
  :group '5x5)

(defcustom 5x5-mode-hook nil
  "Hook run on starting 5x5."
  :type  'hook
  :group '5x5)

;; Non-customize variables.

(defmacro 5x5-defvar-local (var value doc)
  "Define VAR to VALUE with documentation DOC and make it buffer local."
  `(progn
     (defvar ,var ,value ,doc)
     (make-variable-buffer-local (quote ,var))))

(5x5-defvar-local 5x5-grid nil
  "5x5 grid contents.")

(5x5-defvar-local 5x5-x-pos 2
  "X position of cursor.")

(5x5-defvar-local 5x5-y-pos 2
  "Y position of cursor.")

(5x5-defvar-local 5x5-moves 0
  "Moves made.")

(5x5-defvar-local 5x5-cracking nil
  "Are we in cracking mode?")

(defvar 5x5-buffer-name "*5x5*"
  "Name of the 5x5 play buffer.")

(defvar 5x5-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?"                       #'describe-mode)
    (define-key map "\r"                      #'5x5-flip-current)
    (define-key map " "                       #'5x5-flip-current)
    (define-key map [up]                      #'5x5-up)
    (define-key map [down]                    #'5x5-down)
    (define-key map [left]                    #'5x5-left)
    (define-key map [tab]                     #'5x5-right)
    (define-key map [right]                   #'5x5-right)
    (define-key map [(control a)]             #'5x5-bol)
    (define-key map [(control e)]             #'5x5-eol)
    (define-key map [(control p)]             #'5x5-up)
    (define-key map [(control n)]             #'5x5-down)
    (define-key map [(control b)]             #'5x5-left)
    (define-key map [(control f)]             #'5x5-right)
    (define-key map [home]                    #'5x5-bol)
    (define-key map [end]                     #'5x5-eol)
    (define-key map [prior]                   #'5x5-first)
    (define-key map [next]                    #'5x5-last)
    (define-key map "r"                       #'5x5-randomize)
    (define-key map [(control c) (control r)] #'5x5-crack-randomly)
    (define-key map [(control c) (control c)] #'5x5-crack-mutating-current)
    (define-key map [(control c) (control b)] #'5x5-crack-mutating-best)
    (define-key map [(control c) (control x)] #'5x5-crack-xor-mutate)
    (define-key map "n"                       #'5x5-new-game)
    (define-key map "s"                       #'5x5-solve-suggest)
    (define-key map "<"                       #'5x5-solve-rotate-left)
    (define-key map ">"                       #'5x5-solve-rotate-right)
    (define-key map "q"                       #'5x5-quit-game)
    map)
  "Local keymap for the 5x5 game.")

(5x5-defvar-local 5x5-solver-output nil
  "List that is the output of an arithmetic solver.

This list L is such that

L = (M S_1 S_2 ... S_N)

M is the move count when the solve output was stored.

S_1 ... S_N are all the solutions ordered from least to greatest
number of strokes.  S_1 is the solution to be displayed.

Each solution S_1, ..., S_N is a list (STROKE-COUNT GRID) where
STROKE-COUNT is the number of strokes to achieve the solution and
GRID is the grid of positions to click.")


;; Menu definition.

(easy-menu-define 5x5-mode-menu 5x5-mode-map "5x5 menu."
  '("5x5"
    ["New game"               5x5-new-game  t]
    ["Random game"            5x5-randomize t]
    ["Quit game"              5x5-quit-game t]
    "---"
    ["Use Calc solver"        5x5-solve-suggest         t]
    ["Rotate left list of Calc solutions"        5x5-solve-rotate-left     t]
    ["Rotate right list of Calc solutions"       5x5-solve-rotate-right    t]
    "---"
    ["Crack randomly"         5x5-crack-randomly         t]
    ["Crack mutating current" 5x5-crack-mutating-current t]
    ["Crack mutating best"    5x5-crack-mutating-best    t]
    ["Crack with xor mutate"  5x5-crack-xor-mutate       t]))

;; Gameplay functions.

(put '5x5-mode 'mode-class 'special)

(defun 5x5-mode ()
  "A mode for playing `5x5'.

The key bindings for `5x5-mode' are:

\\{5x5-mode-map}"
  (kill-all-local-variables)
  (use-local-map 5x5-mode-map)
  (setq major-mode '5x5-mode
        mode-name  "5x5")
  (run-mode-hooks '5x5-mode-hook)
  (setq buffer-read-only t
        truncate-lines   t)
  (buffer-disable-undo))

;;;###autoload
(defun 5x5 (&optional size)
  "Play 5x5.

The object of 5x5 is very simple, by moving around the grid and flipping
squares you must fill the grid.

5x5 keyboard bindings are:
\\<5x5-mode-map>
Flip                        \\[5x5-flip-current]
Move up                     \\[5x5-up]
Move down                   \\[5x5-down]
Move left                   \\[5x5-left]
Move right                  \\[5x5-right]
Start new game              \\[5x5-new-game]
New game with random grid   \\[5x5-randomize]
Random cracker              \\[5x5-crack-randomly]
Mutate current cracker      \\[5x5-crack-mutating-current]
Mutate best cracker         \\[5x5-crack-mutating-best]
Mutate xor cracker          \\[5x5-crack-xor-mutate]
Solve with Calc             \\[5x5-solve-suggest]
Rotate left Calc Solutions  \\[5x5-solve-rotate-left]
Rotate right Calc Solutions \\[5x5-solve-rotate-right]
Quit current game           \\[5x5-quit-game]"

  (interactive "P")
  (setq 5x5-cracking nil)
  (switch-to-buffer 5x5-buffer-name)
  (5x5-mode)
  (when (natnump size)
      (setq 5x5-grid-size size))
  (if (or (not 5x5-grid) (not (= 5x5-grid-size (length (aref 5x5-grid 0)))))
      (5x5-new-game))
  (5x5-draw-grid (list 5x5-grid))
  (5x5-position-cursor))

(defun 5x5-new-game ()
  "Start a new game of `5x5'."
  (interactive)
  (when (if (called-interactively-p 'interactive)
	    (5x5-y-or-n-p "Start a new game? ") t)
    (setq 5x5-x-pos (/ 5x5-grid-size 2)
          5x5-y-pos (/ 5x5-grid-size 2)
          5x5-moves 0
          5x5-grid  (5x5-make-move (5x5-make-new-grid) 5x5-y-pos 5x5-x-pos)
	  5x5-solver-output nil)
    (5x5-draw-grid (list 5x5-grid))
    (5x5-position-cursor)))

(defun 5x5-quit-game ()
  "Quit the current game of `5x5'."
  (interactive)
  (kill-buffer 5x5-buffer-name))

(defun 5x5-make-new-grid ()
  "Create and return a new `5x5' grid structure."
  (let ((grid (make-vector 5x5-grid-size nil)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (aset grid y (make-vector 5x5-grid-size nil)))
    grid))

(defun 5x5-cell (grid y x)
  "Return the value of the cell in GRID at location X,Y."
  (aref (aref grid y) x))

(defun 5x5-set-cell (grid y x value)
  "Set the value of cell X,Y in GRID to VALUE."
  (aset (aref grid y) x value))

(defun 5x5-flip-cell (grid y x)
  "Flip the value of cell X,Y in GRID."
  (5x5-set-cell grid y x (not (5x5-cell grid y x))))

(defun 5x5-copy-grid (grid)
  "Make a new copy of GRID."
  (let ((copy (5x5-make-new-grid)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (5x5-set-cell copy y x (5x5-cell grid y x))))
    copy))

(defun 5x5-make-move (grid row col)
  "Make a move on GRID at row ROW and column COL."
  (5x5-flip-cell grid row col)
  (if (> row 0)
      (5x5-flip-cell grid (1- row) col))
  (if (< row (- 5x5-grid-size 1))
      (5x5-flip-cell grid (1+ row) col))
  (if (> col 0)
      (5x5-flip-cell grid row (1- col)))
  (if (< col (- 5x5-grid-size 1))
      (5x5-flip-cell grid row (1+ col)))
  grid)

(defun 5x5-row-value (row)
  "Get the \"on-value\" for grid row ROW."
  (loop for y from 0 to (1- 5x5-grid-size) sum (if (aref row y) 1 0)))

(defun 5x5-grid-value (grid)
  "Get the \"on-value\" for grid GRID."
  (loop for y from 0 to (1- 5x5-grid-size) sum (5x5-row-value (aref grid y))))

(defun 5x5-draw-grid-end ()
  "Draw the top/bottom of the grid."
  (insert "+")
  (loop for x from 0 to (1- 5x5-grid-size) do
        (insert "-" (make-string 5x5-x-scale ?-)))
  (insert "-+ "))

(defun 5x5-draw-grid (grids)
  "Draw the grids GRIDS into the current buffer."
  (let ((inhibit-read-only t) grid-org)
    (erase-buffer)
    (loop for grid in grids do (5x5-draw-grid-end))
    (insert "\n")
    (setq grid-org (point))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for lines from 0 to (1- 5x5-y-scale) do
                (loop for grid in grids do
                      (loop for x from 0 to (1- 5x5-grid-size) do
                            (insert (if (zerop x) "| " " ")
                                    (make-string 5x5-x-scale
                                                 (if (5x5-cell grid y x) ?# ?.))))
                      (insert " | "))
                (insert "\n")))
    (when 5x5-solver-output
      (if (= (car 5x5-solver-output) 5x5-moves)
	  (save-excursion
	    (goto-char grid-org)
	    (beginning-of-line (+ 1 (/ 5x5-y-scale 2)))
	    (let ((solution-grid (cdadr 5x5-solver-output)))
	      (dotimes (y  5x5-grid-size)
		(save-excursion
		  (forward-char  (+ 1 (/ (1+ 5x5-x-scale) 2)))
		  (dotimes (x   5x5-grid-size)
		    (when (5x5-cell solution-grid y x)
		      (if (= 0 (mod 5x5-x-scale 2))
			  (progn
			    (insert "()")
			    (delete-region (point) (+ (point) 2))
			    (backward-char 2))
			(insert-char ?O 1)
			(delete-char 1)
			(backward-char)))
		    (forward-char  (1+ 5x5-x-scale))))
		(forward-line  5x5-y-scale))))
	(setq 5x5-solver-output nil)))
    (loop for grid in grids do (5x5-draw-grid-end))
    (insert "\n")
    (insert (format "On: %d  Moves: %d" (5x5-grid-value (car grids)) 5x5-moves))))

(defun 5x5-position-cursor ()
  "Position the cursor on the grid."
  (goto-char (point-min))
  (forward-line (1+ (* 5x5-y-pos 5x5-y-scale)))
  (goto-char (+ (point) (* 5x5-x-pos 5x5-x-scale) (+ 5x5-x-pos 1) 1)))

(defun 5x5-made-move ()
  "Keep track of how many moves have been made."
  (incf 5x5-moves))

(defun 5x5-make-random-grid (&optional move)
  "Make a random grid."
  (setq move (or move (symbol-function '5x5-flip-cell)))
  (let ((grid (5x5-make-new-grid)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (if (zerop (random 2))
                    (funcall move grid y x))))
    grid))

;; Cracker functions.

;;;###autoload
(defun 5x5-crack-randomly ()
  "Attempt to crack 5x5 using random solutions."
  (interactive)
  (5x5-crack #'5x5-make-random-solution))

;;;###autoload
(defun 5x5-crack-mutating-current ()
  "Attempt to crack 5x5 by mutating the current solution."
  (interactive)
  (5x5-crack #'5x5-make-mutate-current))

;;;###autoload
(defun 5x5-crack-mutating-best ()
  "Attempt to crack 5x5 by mutating the best solution."
  (interactive)
  (5x5-crack #'5x5-make-mutate-best))

;;;###autoload
(defun 5x5-crack-xor-mutate ()
  "Attempt to crack 5x5 by xoring the current and best solution.
Mutate the result."
  (interactive)
  (5x5-crack #'5x5-make-xor-with-mutation))

;;;###autoload
(defun 5x5-crack (breeder)
  "Attempt to find a solution for 5x5.

5x5-crack takes the argument BREEDER which should be a function that takes
two parameters, the first will be a grid vector array that is the current
solution and the second will be the best solution so far.  The function
should return a grid vector array that is the new solution."

  (interactive "aBreeder function: ")
  (5x5)
  (setq 5x5-cracking t)
  (let* ((best-solution    (5x5-make-random-grid))
         (current-solution best-solution)
         (best-result      (5x5-make-new-grid))
         (current-result   (5x5-make-new-grid))
         (target           (* 5x5-grid-size 5x5-grid-size)))
    (while (and (< (5x5-grid-value best-result) target)
                (not (input-pending-p)))
      (setq current-result (5x5-play-solution current-solution best-solution))
      (if (> (5x5-grid-value current-result) (5x5-grid-value best-result))
          (setq best-solution current-solution
                best-result   current-result))
      (setq current-solution (funcall breeder
                                      (5x5-copy-grid current-solution)
                                      (5x5-copy-grid best-solution)))))
  (setq 5x5-cracking nil))

(defun 5x5-make-random-solution (&rest _ignore)
  "Make a random solution."
  (5x5-make-random-grid))

(defun 5x5-make-mutate-current (current _best)
  "Mutate the current solution."
  (5x5-mutate-solution current))

(defun 5x5-make-mutate-best (_current best)
  "Mutate the best solution."
  (5x5-mutate-solution best))

(defun 5x5-make-xor-with-mutation (current best)
  "Xor current and best solution then mutate the result."
  (let ((xored (5x5-make-new-grid)))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (5x5-set-cell xored y x
                              (5x5-xor (5x5-cell current y x)
                                       (5x5-cell best    y x)))))
    (5x5-mutate-solution xored)))

(defun 5x5-mutate-solution (solution)
  "Randomly flip bits in the solution."
  (loop for y from 0 to (1- 5x5-grid-size) do
        (loop for x from 0 to (1- 5x5-grid-size) do
              (if (= (random (/ (* 5x5-grid-size 5x5-grid-size) 2))
                     (/ (/ (* 5x5-grid-size 5x5-grid-size) 2) 2))
                  (5x5-flip-cell solution y x))))
  solution)

(defun 5x5-play-solution (solution best)
  "Play a solution on an empty grid.  This destroys the current game
in progress because it is an animated attempt."
  (5x5-new-game)
  (let ((inhibit-quit t))
    (loop for y from 0 to (1- 5x5-grid-size) do
          (loop for x from 0 to (1- 5x5-grid-size) do
                (setq 5x5-y-pos y
                      5x5-x-pos x)
                (if (5x5-cell solution y x)
                    (5x5-flip-current))
                (5x5-draw-grid (list 5x5-grid solution best))
                (5x5-position-cursor)
                (sit-for 5x5-animate-delay))))
  5x5-grid)

;; Arithmetic solver
;;===========================================================================
(defun 5x5-grid-to-vec (grid)
  "Convert GRID to an equivalent Calc matrix of (mod X 2) forms
where X is 1 for setting a position, and 0 for unsetting a
position."
  (cons 'vec
	(mapcar (lambda (y)
		  (cons 'vec
			(mapcar (lambda (x)
				  (if x '(mod 1 2) '(mod 0 2)))
				y)))
		grid)))

(defun 5x5-vec-to-grid (grid-matrix)
  "Convert a grid matrix GRID-MATRIX in Calc format to a grid in
5x5 format.  See function `5x5-grid-to-vec'."
  (apply
   'vector
   (mapcar
    (lambda (x)
      (apply
       'vector
       (mapcar
	(lambda (y) (/= (cadr y) 0))
	(cdr x))))
    (cdr grid-matrix))))

(eval-and-compile
(if nil; set to t to enable solver logging
    ;; Note these logging facilities were not cleaned out as the arithmetic
    ;; solver is not yet complete --- it works only for grid size = 5.
    ;; So they may be useful again to design a more generic solution.
    (progn
      (defvar 5x5-log-buffer nil)
      (defun 5x5-log-init ()
	(if (buffer-live-p 5x5-log-buffer)
	    (with-current-buffer 5x5-log-buffer (erase-buffer))
	  (setq 5x5-log-buffer (get-buffer-create "*5x5 LOG*"))))

      (defun 5x5-log (name value)
	"Debug purposes only.

Log a matrix VALUE of (mod B 2) forms, only B is output and
Scilab matrix notation is used.  VALUE is returned so that it is
easy to log a value with minimal rewrite of code."
	(when (buffer-live-p 5x5-log-buffer)
	  (let* ((unpacked-value
		  (math-map-vec
		   (lambda (row) (math-map-vec 'cadr row))
		   value))
		 (calc-vector-commas "")
		 (calc-matrix-brackets '(C O))
		 (value-to-log (math-format-value unpacked-value)))
	    (with-current-buffer 5x5-log-buffer
	      (insert name ?= value-to-log ?\n))))
	value))
  (defsubst 5x5-log-init ())
  (defsubst 5x5-log (name value) value)))

(declare-function math-map-vec "calc-vec" (f a))
(declare-function math-sub "calc" (a b))
(declare-function math-mul "calc" (a b))
(declare-function math-make-intv "calc-forms" (mask lo hi))
(declare-function math-reduce-vec "calc-vec" (a b))
(declare-function math-format-number "calc" (a &optional prec))
(declare-function math-pow "calc-misc" (a b))
(declare-function calcFunc-arrange "calc-vec" (vec cols))
(declare-function calcFunc-cvec "calc-vec" (obj &rest dims))
(declare-function calcFunc-diag "calc-vec" (a &optional n))
(declare-function calcFunc-trn "calc-vec" (mat))
(declare-function calcFunc-inv "calc-misc" (m))
(declare-function calcFunc-mrow "calc-vec" (mat n))
(declare-function calcFunc-mcol "calc-vec" (mat n))
(declare-function calcFunc-vconcat "calc-vec" (a b))
(declare-function calcFunc-index "calc-vec" (n &optional start incr))

(defun 5x5-solver (grid)
  "Return a list of solutions for GRID.

Given some grid GRID, the returned a list of solution LIST is
sorted from least Hamming weight to greatest one.

   LIST = (SOLUTION-1 ... SOLUTION-N)

Each solution SOLUTION-I is a cons cell (HW . G) where HW is the
Hamming weight of the solution --- ie the number of strokes to
achieve it --- and G is the grid of positions to click in order
to complete the 5x5.

Solutions are sorted from least to greatest Hamming weight."
  (require 'calc-ext)
  (flet ((5x5-mat-mode-2
	  (a)
	  (math-map-vec
	   (lambda (y)
	     (math-map-vec
	      (lambda (x) `(mod ,x 2))
	      y))
	   a)))
    (let* (calc-command-flags
	   (grid-size-squared (* 5x5-grid-size 5x5-grid-size))

	   ;; targetv is the vector the origin of which is org="current
	   ;; grid" and the end of which is dest="all ones".
	   (targetv
	    (5x5-log
	     "b"
	     (let (
		   ;; org point is the current grid
		   (org (calcFunc-arrange (5x5-grid-to-vec grid)
					  1))

		   ;; end point of game is the all ones matrix
		   (dest (calcFunc-cvec '(mod 1 2) grid-size-squared 1)))
	       (math-sub dest org))))

	   ;; transferm is the transfer matrix, ie it is the 25x25
	   ;; matrix applied everytime a flip is carried out where a
	   ;; flip is defined by a 25x1 Dirac vector --- ie all zeros
	   ;; but 1 in the position that is flipped.
	   (transferm
	    (5x5-log
	     "a"
	     ;; transfer-grid is not a play grid, but this is the
	     ;; transfer matrix in the format of a vector of vectors, we
	     ;; do it this way because random access in vectors is
	     ;; faster.  The motivation is just speed as we build it
	     ;; element by element, but that could have been created
	     ;; using only Calc primitives.  Probably that would be a
	     ;; better idea to use Calc with some vector manipulation
	     ;; rather than going this way...
	     (5x5-grid-to-vec (let ((transfer-grid
				     (let ((5x5-grid-size grid-size-squared))
				       (5x5-make-new-grid))))
				(dotimes (i 5x5-grid-size)
				  (dotimes (j 5x5-grid-size)
				    ;; k0 = flattened flip position corresponding
				    ;;      to (i, j) on the grid.
				    (let* ((k0 (+ (* 5 i) j)))
				      ;; cross center
				      (5x5-set-cell transfer-grid k0 k0 t)
				      ;; Cross top.
				      (and
				       (> i 0)
				       (5x5-set-cell transfer-grid
						     (- k0 5x5-grid-size) k0 t))
				      ;; Cross bottom.
				      (and
				       (< (1+ i) 5x5-grid-size)
				       (5x5-set-cell transfer-grid
						     (+ k0 5x5-grid-size) k0 t))
				      ;; Cross left.
				      (and
				       (> j 0)
				       (5x5-set-cell transfer-grid (1- k0) k0 t))
				      ;; Cross right.
				      (and
				       (< (1+ j)  5x5-grid-size)
				       (5x5-set-cell transfer-grid
						     (1+ k0) k0 t)))))
				transfer-grid))))
	   ;; TODO: this is hard-coded for grid-size = 5, make it generic.
	   (transferm-kernel-size
	    (if (= 5x5-grid-size 5) 2
	      (error "Transfer matrix rank not known for grid-size != 5")))

	   ;; TODO: this is hard-coded for grid-size = 5, make it generic.
	   ;;
	   ;; base-change is a 25x25 matrix, where topleft submatrix
	   ;; 23x25 is a diagonal of 1, and the two last columns are a
	   ;; base of kernel of transferm.
	   ;;
	   ;; base-change must be by construction invertible.
	   (base-change
	    (5x5-log
	     "p"
	     (let ((id (5x5-mat-mode-2 (calcFunc-diag 1 grid-size-squared))))
	       (setcdr (last id (1+ transferm-kernel-size))
		       (cdr (5x5-mat-mode-2
			     '(vec (vec 0 1 1 1 0 1 0 1 0 1 1 1 0 1
					1 1 0 1 0 1 0 1 1 1 0)
				   (vec 1 1 0 1 1 0 0 0 0 0 1 1 0 1
					1 0 0 0 0 0 1 1 0 1 1)))))
	       (calcFunc-trn id))))

	   (inv-base-change
	    (5x5-log "invp"
		     (calcFunc-inv base-change)))

	   ;; B:= targetv
	   ;; A:= transferm
	   ;; P:= base-change
	   ;; P^-1 := inv-base-change
	   ;; X := solution

	   ;; B = A * X
	   ;; P^-1 * B = P^-1 * A * P * P^-1 * X
	   ;; CX = P^-1 * X
	   ;; CA = P^-1 * A * P
	   ;; CB = P^-1 * B
	   ;; CB = CA * CX
	   ;; CX = CA^-1 * CB
	   ;; X = P * CX
	   (ctransferm
	    (5x5-log
	     "ca"
	     (math-mul
	      inv-base-change
	      (math-mul transferm base-change)))); CA
	   (ctarget
	    (5x5-log
	     "cb"
	     (math-mul inv-base-change targetv))); CB
	   (row-1  (math-make-intv 3  1 transferm-kernel-size)) ; 1..2
	   (row-2   (math-make-intv 1 transferm-kernel-size
				    grid-size-squared)); 3..25
	   (col-1 (math-make-intv 3 1  (- grid-size-squared
					  transferm-kernel-size))); 1..23
	   (col-2 (math-make-intv 1 (- grid-size-squared
				       transferm-kernel-size)
				  grid-size-squared)); 24..25
	   (ctransferm-1-: (calcFunc-mrow ctransferm row-1))
	   (ctransferm-1-1 (calcFunc-mcol ctransferm-1-: col-1))

	   ;; By construction ctransferm-:-2 = 0, so ctransferm-1-2 = 0
	   ;; and ctransferm-2-2 = 0.

	   ;;(ctransferm-1-2 (calcFunc-mcol ctransferm-1-: col-2))
	   (ctransferm-2-: (calcFunc-mrow ctransferm row-2))
	   (ctransferm-2-1
	    (5x5-log
	     "ca_2_1"
	     (calcFunc-mcol ctransferm-2-: col-1)))

	   ;; By construction ctransferm-2-2 = 0.
	   ;;
	   ;;(ctransferm-2-2 (calcFunc-mcol ctransferm-2-: col-2))

	   (ctarget-1 (calcFunc-mrow ctarget row-1))
	   (ctarget-2 (calcFunc-mrow ctarget row-2))

	   ;;   ctarget-1(2x1)  =   ctransferm-1-1(2x23) *cx-1(23x1)
	   ;;                     + ctransferm-1-2(2x2) *cx-2(2x1);
	   ;;   ctarget-2(23x1) =   ctransferm-2-1(23x23)*cx-1(23x1)
	   ;;                     + ctransferm-2-2(23x2)*cx-2(2x1);
	   ;;   By construction:
	   ;;
	   ;;   ctransferm-1-2 == zeros(2,2) and ctransferm-2-2 == zeros(23,2)
	   ;;
	   ;;   So:
	   ;;
	   ;;   ctarget-2 = ctransferm-2-1*cx-1
	   ;;
	   ;;   So:
	   ;;
	   ;;   cx-1 = inv-ctransferm-2-1 * ctarget-2
	   (cx-1 (math-mul (calcFunc-inv ctransferm-2-1) ctarget-2))

	   ;; Any cx-2 can do, so there are 2^{transferm-kernel-size} solutions.
	   (solution-list
	    ;; Within solution-list each element is a cons cell:
	    ;;
	    ;; (HW . SOL)
	    ;;
	    ;; where HW is the Hamming weight of solution, and SOL is
	    ;; the solution in the form of a grid.
	    (sort
	     (cdr
	      (math-map-vec
	       (lambda (cx-2)
		 ;; Compute `solution' in the form of a 25x1 matrix of
		 ;; (mod B 2) forms --- with B = 0 or 1 --- and
		 ;; return (HW . SOL) where HW is the Hamming weight
		 ;; of solution and SOL a grid.
		 (let ((solution (math-mul
				  base-change
				  (calcFunc-vconcat cx-1 cx-2)))); X = P * CX
		   (cons
		    ;; The Hamming Weight is computed by matrix reduction
		    ;; with an ad-hoc operator.
		    (math-reduce-vec
		     ;; (cadadr '(vec (mod x 2))) => x
		     (lambda (r x) (+ (if (integerp r) r (cadadr r))
				      (cadadr x)))
		     solution); car
		    (5x5-vec-to-grid
		     (calcFunc-arrange solution 5x5-grid-size));cdr
		    )))
	       ;; A (2^K) x K matrix, where K is the dimension of kernel
	       ;; of transfer matrix --- i.e. K=2 in if the grid is 5x5
	       ;; --- for I from 0 to K-1, each row rI correspond to the
	       ;; binary representation of number I, that is to say row
	       ;; rI is a 1xK vector:
	       ;;    [ n{I,0} n{I,1} ... n{I,K-1} ]
	       ;; such that:
	       ;;    I = sum for J=0..K-1 of 2^(n{I,J})
	       (let ((calc-number-radix 2)
		     (calc-leading-zeros t)
		     (calc-word-size transferm-kernel-size))
		 (math-map-vec
		  (lambda (x)
		    (cons 'vec
			  (mapcar (lambda (x) `(vec (mod ,(logand x 1) 2)))
				  (substring (math-format-number x)
					     (- transferm-kernel-size)))))
		  (calcFunc-index (math-pow 2 transferm-kernel-size) 0))) ))
	     ;; Sort solutions according to respective Hamming weight.
	     (lambda (x y) (< (car x) (car y)))
	     )))
      (message "5x5 Solution computation done.")
      solution-list)))

(defun 5x5-solve-suggest (&optional n)
  "Suggest to the user where to click.

Argument N is ignored."
  ;; For the time being n is ignored, the idea was to use some numeric
  ;; argument to show a limited amount of positions.
  (interactive "P")
  (5x5-log-init)
  (let ((solutions (5x5-solver 5x5-grid)))
    (setq 5x5-solver-output
	  (cons 5x5-moves solutions)))
  (5x5-draw-grid (list 5x5-grid))
  (5x5-position-cursor))

(defun 5x5-solve-rotate-left (&optional n)
  "Rotate left by N the list of solutions in 5x5-solver-output.

If N is not supplied rotate by 1, that is to say put the last
element first in the list.

The 5x5 game has in general several solutions.  For grid size=5,
there are 4 possible solutions.  When function
`5x5-solve-suggest' (press `\\[5x5-solve-suggest]') is called the
solution that is presented is the one that needs least number of
strokes --- other solutions can be viewed by rotating through the
list. The list of solution is ordered by number of strokes, so
rotating left just after calling `5x5-solve-suggest' will show
the solution with second least number of strokes, while rotating
right will show the solution with greatest number of strokes."
  (interactive "P")
  (let ((len  (length 5x5-solver-output)))
    (when (>= len 3)
      (setq n (if (integerp n) n 1)
	    n (mod n (1- len)))
      (unless (eq n 0)
	(setq n  (- len n 1))
	(let* ((p-tail (last 5x5-solver-output (1+ n)))
	       (tail (cdr p-tail))
	       (l-tail (last tail)))
	  ;;
	  ;;  For n = 2:
	  ;;
	  ;;  +--+--+   +--+--+   +--+--+   +--+--+   +--+--+
	  ;;  |M | ---->|S1| ---->|S2| ---->|S3| ---->|S4| ----> nil
	  ;;  +--+--+   +--+--+   +--+--+   +--+--+   +--+--+
	  ;;    ^ 		    ^         ^         ^
	  ;;    | 		    |         |         |
	  ;;    + 5x5-solver-output |         |         + l-tail
	  ;;			    + p-tail  |
	  ;;			              + tail
	  ;;
	  (setcdr l-tail (cdr 5x5-solver-output))
	  (setcdr 5x5-solver-output tail)
	  (unless (eq p-tail 5x5-solver-output)
	    (setcdr p-tail nil)))
	(5x5-draw-grid (list 5x5-grid))
	(5x5-position-cursor)))))

(defun 5x5-solve-rotate-right (&optional n)
  "Rotate right by N the list of solutions in 5x5-solver-output.
If N is not supplied, rotate by 1.  Similar to function
`5x5-solve-rotate-left' except that rotation is right instead of
lest."
  (interactive "P")
  (setq n
	(if (integerp n) (- n)
	  -1))
  (5x5-solve-rotate-left n))



;; Keyboard response functions.

(defun 5x5-flip-current ()
  "Make a move on the current cursor location."
  (interactive)
  (setq 5x5-grid (5x5-make-move 5x5-grid 5x5-y-pos 5x5-x-pos))
  (5x5-made-move)
  (unless 5x5-cracking
    (5x5-draw-grid (list 5x5-grid)))
  (5x5-position-cursor)
  (when (= (5x5-grid-value 5x5-grid) (* 5x5-grid-size 5x5-grid-size))
    (beep)
    (message "You win!")))

(defun 5x5-up ()
  "Move up."
  (interactive)
  (unless (zerop 5x5-y-pos)
    (decf 5x5-y-pos)
    (5x5-position-cursor)))

(defun 5x5-down ()
  "Move down."
  (interactive)
  (unless (= 5x5-y-pos (1- 5x5-grid-size))
    (incf 5x5-y-pos)
    (5x5-position-cursor)))

(defun 5x5-left ()
  "Move left."
  (interactive)
  (unless (zerop 5x5-x-pos)
    (decf 5x5-x-pos)
    (5x5-position-cursor)))

(defun 5x5-right ()
  "Move right."
  (interactive)
  (unless (= 5x5-x-pos (1- 5x5-grid-size))
    (incf 5x5-x-pos)
    (5x5-position-cursor)))

(defun 5x5-bol ()
  "Move to beginning of line."
  (interactive)
  (setq 5x5-x-pos 0)
  (5x5-position-cursor))

(defun 5x5-eol ()
  "Move to end of line."
  (interactive)
  (setq 5x5-x-pos (1- 5x5-grid-size))
  (5x5-position-cursor))

(defun 5x5-first ()
  "Move to the first cell."
  (interactive)
  (setq 5x5-x-pos 0
        5x5-y-pos 0)
  (5x5-position-cursor))

(defun 5x5-last ()
  "Move to the last cell."
  (interactive)
  (setq 5x5-x-pos (1- 5x5-grid-size)
        5x5-y-pos (1- 5x5-grid-size))
  (5x5-position-cursor))

(defun 5x5-randomize ()
  "Randomize the grid."
  (interactive)
  (when (5x5-y-or-n-p "Start a new game with a random grid? ")
    (setq 5x5-x-pos (/ 5x5-grid-size 2)
          5x5-y-pos (/ 5x5-grid-size 2)
          5x5-moves 0
          5x5-grid  (5x5-make-random-grid (symbol-function '5x5-make-move))
	  5x5-solver-output nil)
    (unless 5x5-cracking
      (5x5-draw-grid (list 5x5-grid)))
    (5x5-position-cursor)))

;; Support functions

(defun 5x5-xor (x y)
  "Boolean exclusive-or of X and Y."
  (and (or x y) (not (and x y))))

(defun 5x5-y-or-n-p (prompt)
  "5x5 wrapper for `y-or-n-p' which respects the `5x5-hassle-me' setting."
  (if 5x5-hassle-me
      (y-or-n-p prompt)
    t))

(random t)

(provide '5x5)

;;; 5x5.el ends here
