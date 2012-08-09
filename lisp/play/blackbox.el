;;; blackbox.el --- blackbox game in Emacs Lisp

;; Copyright (C) 1985-1987, 1992, 2001-2012 Free Software Foundation, Inc.

;; Author: F. Thomas May <uw-nsr!uw-warp!tom@beaver.cs.washington.edu>
;; Adapted-By: ESR
;; Keywords: games

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

;; by F. Thomas May <uw-nsr!uw-warp!tom@beaver.cs.washington.edu>
;; doc comment by Root Boy Jim <rbj@dsys.icst.nbs.gov>, 27 Apr 89
;; interface improvements by ESR, Dec 5 1991.

;; The object of the game is to find four hidden balls by shooting rays
;; into the black box.  There are four possibilities: 1) the ray will
;; pass thru the box undisturbed, 2) it will hit a ball and be absorbed,
;; 3) it will be deflected and exit the box, or 4) be deflected immediately,
;; not even being allowed entry into the box.
;;
;; The strange part is the method of deflection.  It seems that rays will
;; not pass next to a ball, and change direction at right angles to avoid it.
;;
;; 		             R   3
;; 		 1 - - - - - - - - 1
;; 		   - - - - - - - -
;; 		   - O - - - - - - 3
;; 		 2 - - - - O - O -
;; 		 4 - - - - - - - -
;; 		 5 - - - - - - - - 5
;; 		   - - - - - - - - R
;; 		 H - - - - - - - O
;; 		   2   H 4       H
;;
;; Rays which enter and exit are numbered.  You can see that rays 1 & 5 pass
;; thru the box undisturbed.  Ray 2 is deflected by the northwesternmost
;; ball.  Likewise rays 3 and 4. Rays which hit balls and are absorbed are
;; marked with H.  The bottom of the left and the right of the bottom hit
;; the southeastern ball directly.  Rays may also hit balls after being
;; reflected.  Consider the H on the bottom next to the 4.  It bounces off
;; the NW-ern most ball and hits the central ball.  A ray shot from above
;; the right side 5 would hit the SE-ern most ball.  The R beneath the 5
;; is because the ball is returned instantly.  It is not allowed into
;; the box if it would reflect immediately.  The R on the top is a more
;; leisurely return.  Both central balls would tend to deflect it east
;; or west, but it cannot go either way, so it just retreats.
;;
;; At the end of the game, if you've placed guesses for as many balls as
;; there are in the box, the true board position will be revealed.  Each
;; `x' is an incorrect guess of yours; `o' is the true location of a ball.

;;; Code:

(defvar bb-board nil
  "Blackbox board.")

(defvar bb-x -1
  "Current x-position.")

(defvar bb-y -1
  "Current y-position.")

(defvar bb-score 0
  "Current score.")

(defvar bb-detour-count 0
  "Number of detours.")

(defvar bb-balls-placed nil
  "List of already placed balls.")

;; This is used below to remap existing bindings for cursor motion to
;; blackbox-specific bindings in blackbox-mode-map.  This is so that
;; users who prefer non-default key bindings for cursor motion don't
;; lose that when they play Blackbox.
(defun blackbox-redefine-key (map oldfun newfun)
  "Redefine keys that run the function OLDFUN to run NEWFUN instead."
  (define-key map (vector 'remap oldfun) newfun))


(defvar blackbox-mode-map 
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (blackbox-redefine-key map 'backward-char 'bb-left)
    (blackbox-redefine-key map 'forward-char 'bb-right)
    (blackbox-redefine-key map 'previous-line 'bb-up)
    (blackbox-redefine-key map 'next-line 'bb-down)
    (blackbox-redefine-key map 'move-end-of-line 'bb-eol)
    (blackbox-redefine-key map 'move-beginning-of-line 'bb-bol)
    (define-key map " " 'bb-romp)
    (define-key map "q" 'bury-buffer)
    (define-key map [insert] 'bb-romp)
    (define-key map [return] 'bb-done)
    (blackbox-redefine-key map 'newline 'bb-done)
    map))

;; Blackbox mode is suitable only for specially formatted data.
(put 'blackbox-mode 'mode-class 'special)

(defun blackbox-mode ()
  "Major mode for playing blackbox.
To learn how to play blackbox, see the documentation for function `blackbox'.

The usual mnemonic keys move the cursor around the box.
\\<blackbox-mode-map>\\[bb-bol] and \\[bb-eol] move to the beginning and end of line, respectively.

\\[bb-romp] -- send in a ray from point, or toggle a ball at point
\\[bb-done] -- end game and get score"
  (interactive)
  (kill-all-local-variables)
  (use-local-map blackbox-mode-map)
  (setq truncate-lines t)
  (setq major-mode 'blackbox-mode)
  (setq mode-name "Blackbox")
  (run-mode-hooks 'blackbox-mode-hook))

;;;###autoload
(defun blackbox (num)
  "Play blackbox.
Optional prefix argument is the number of balls; the default is 4.

What is blackbox?

Blackbox is a game of hide and seek played on an 8 by 8 grid (the
Blackbox).  Your opponent (Emacs, in this case) has hidden several
balls (usually 4) within this box.  By shooting rays into the box and
observing where they emerge it is possible to deduce the positions of
the hidden balls.  The fewer rays you use to find the balls, the lower
your score.

Overview of play:

\\<blackbox-mode-map>\
To play blackbox, type \\[blackbox].  An optional prefix argument
specifies the number of balls to be hidden in the box; the default is
four.

The cursor can be moved around the box with the standard cursor
movement keys.

To shoot a ray, move the cursor to the edge of the box and press SPC.
The result will be determined and the playfield updated.

You may place or remove balls in the box by moving the cursor into the
box and pressing \\[bb-romp].

When you think the configuration of balls you have placed is correct,
press \\[bb-done].  You will be informed whether you are correct or
not, and be given your score.  Your score is the number of letters and
numbers around the outside of the box plus five for each incorrectly
placed ball.  If you placed any balls incorrectly, they will be
indicated with `x', and their actual positions indicated with `o'.

Details:

There are three possible outcomes for each ray you send into the box:

	Detour: the ray is deflected and emerges somewhere other than
		where you sent it in.  On the playfield, detours are
		denoted by matching pairs of numbers -- one where the
		ray went in, and the other where it came out.

	Reflection: the ray is reflected and emerges in the same place
		it was sent in.  On the playfield, reflections are
		denoted by the letter `R'.

	Hit:	the ray strikes a ball directly and is absorbed.  It does
		not emerge from the box.  On the playfield, hits are
		denoted by the letter `H'.

The rules for how balls deflect rays are simple and are best shown by
example.

As a ray approaches a ball it is deflected ninety degrees.  Rays can
be deflected multiple times.  In the diagrams below, the dashes
represent empty box locations and the letter `O' represents a ball.
The entrance and exit points of each ray are marked with numbers as
described under \"Detour\" above.  Note that the entrance and exit
points are always interchangeable.  `*' denotes the path taken by the
ray.

Note carefully the relative positions of the ball and the ninety
degree deflection it causes.

    1
  - * - - - - - -         - - - - - - - -         - - - - - - - -
  - * - - - - - -         - - - - - - - -         - - - - - - - -
1 * * - - - - - -         - - - - - - - -         - O - - - - O -
  - - O - - - - -         - - O - - - - -         - - * * * * - -
  - - - - - - - -         - - - * * * * * 2     3 * * * - - * - -
  - - - - - - - -         - - - * - - - -         - - - O - * - -
  - - - - - - - -         - - - * - - - -         - - - - * * - -
  - - - - - - - -         - - - * - - - -         - - - - * - O -
                                2                         3

As mentioned above, a reflection occurs when a ray emerges from the same point
it was sent in.  This can happen in several ways:


  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - O - - -         - - O - O - - -          - - - - - - - -
R * * * * - - - -         - - - * - - - -          O - - - - - - -
  - - - - O - - -         - - - * - - - -        R - - - - - - - -
  - - - - - - - -         - - - * - - - -          - - - - - - - -
  - - - - - - - -         - - - * - - - -          - - - - - - - -
  - - - - - - - -       R * * * * - - - -          - - - - - - - -
  - - - - - - - -         - - - - O - - -          - - - - - - - -

In the first example, the ray is deflected downwards by the upper
ball, then left by the lower ball, and finally retraces its path to
its point of origin.  The second example is similar.  The third
example is a bit anomalous but can be rationalized by realizing the
ray never gets a chance to get into the box.  Alternatively, the ray
can be thought of as being deflected downwards and immediately
emerging from the box.

A hit occurs when a ray runs straight into a ball:

  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - O - - -
  - - - - - - - -         - - - - O - - -        H * * * * - - - -
  - - - - - - - -       H * * * * O - - -          - - - * - - - -
  - - - - - - - -         - - - - O - - -          - - - O - - - -
H * * * O - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - - - - -

Be sure to compare the second example of a hit with the first example of
a reflection."
  (interactive "P")
  (switch-to-buffer "*Blackbox*")
  (blackbox-mode)
  (setq buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (setq bb-board (bb-init-board (or num 4)))
  (setq bb-balls-placed nil)
  (setq bb-x -1)
  (setq bb-y -1)
  (setq bb-score 0)
  (setq bb-detour-count 0)
  (bb-insert-board)
  (bb-goto (cons bb-x bb-y)))

(defun bb-init-board (num-balls)
  (random t)
  (let (board pos)
    (while (>= (setq num-balls (1- num-balls)) 0)
      (while
	  (progn
	    (setq pos (cons (random 8) (random 8)))
	    (member pos board)))
      (setq board (cons pos board)))
    board))

(defun bb-insert-board ()
  (let (i (buffer-read-only nil))
    (erase-buffer)
    (insert "                     \n")
    (setq i 8)
    (while (>= (setq i (1- i)) 0)
      (insert "   - - - - - - - -   \n"))
    (insert "                     \n")
    (insert (format "\nThere are %d balls in the box" (length bb-board)))
    ))

(defun bb-right (count)
  (interactive "p")
  (while (and (> count 0) (< bb-x 8))
    (forward-char 2)
    (setq bb-x (1+ bb-x))
    (setq count (1- count))))

(defun bb-left (count)
  (interactive "p")
  (while (and (> count 0) (> bb-x -1))
    (backward-char 2)
    (setq bb-x (1- bb-x))
    (setq count (1- count))))

(defun bb-up (count)
  (interactive "p")
  (while (and (> count 0) (> bb-y -1))
    (with-no-warnings (previous-line))
    (setq bb-y (1- bb-y))
    (setq count (1- count))))

(defun bb-down (count)
  (interactive "p")
  (while (and (> count 0) (< bb-y 8))
    (with-no-warnings (next-line))
    (setq bb-y (1+ bb-y))
    (setq count (1- count))))

(defun bb-eol ()
  (interactive)
  (setq bb-x 8)
  (bb-goto (cons bb-x bb-y)))

(defun bb-bol ()
  (interactive)
  (setq bb-x -1)
  (bb-goto (cons bb-x bb-y)))

(defun bb-romp ()
  (interactive)
  (cond
   ((and
     (or (= bb-x -1) (= bb-x 8))
     (or (= bb-y -1) (= bb-y 8))))
   ((bb-outside-box bb-x bb-y)
    (bb-trace-ray bb-x bb-y))
   (t
    (bb-place-ball bb-x bb-y))))

(defun bb-place-ball (x y)
  (let ((coord (cons x y)))
    (cond
     ((member coord bb-balls-placed)
      (setq bb-balls-placed (delete coord bb-balls-placed))
      (bb-update-board "-"))
     (t
      (setq bb-balls-placed (cons coord bb-balls-placed))
      (bb-update-board (propertize "O" 'help-echo "Placed ball"))))))

(defun bb-trace-ray (x y)
  (when (= (following-char) 32)
    (let ((result (bb-trace-ray-2
                   t
                   x
                   (cond
                    ((= x -1) 1)
                    ((= x 8) -1)
                    (t 0))
                   y
                   (cond
                    ((= y -1) 1)
                    ((= y 8) -1)
                    (t 0)))))
      (cond
       ((eq result 'hit)
        (bb-update-board (propertize "H" 'help-echo "Hit"))
        (setq bb-score (1+ bb-score)))
       ((equal result (cons x y))
        (bb-update-board (propertize "R" 'help-echo "Reflection"))
        (setq bb-score (1+ bb-score)))
       (t
        (setq bb-detour-count (1+ bb-detour-count))
        (bb-update-board (propertize (format "%d" bb-detour-count)
                                     'help-echo "Detour"))
        (save-excursion
          (bb-goto result)
          (bb-update-board (propertize (format "%d" bb-detour-count)
                                       'help-echo "Detour")))
        (setq bb-score (+ bb-score 2)))))))

(defun bb-trace-ray-2 (first x dx y dy)
  (cond
   ((and (not first)
	 (bb-outside-box x y))
    (cons x y))
   ((member (cons (+ x dx) (+ y dy)) bb-board)
    'hit)
   ((member (cons (+ x dx dy) (+ y dy dx)) bb-board)
    (bb-trace-ray-2 nil x (- dy) y (- dx)))
   ((member (cons (+ x dx (- dy)) (+ y dy (- dx))) bb-board)
    (bb-trace-ray-2 nil x dy y dx))
   (t
    (bb-trace-ray-2 nil (+ x dx) dx (+ y dy) dy))))

(defun bb-done ()
  "Finish the game and report score."
  (interactive)
  (let (bogus-balls)
    (cond
     ((not (= (length bb-balls-placed) (length bb-board)))
      (message "There %s %d hidden ball%s; you have placed %d."
	       (if (= (length bb-board) 1) "is" "are")
	       (length bb-board)
	       (if (= (length bb-board) 1) "" "s")
	       (length bb-balls-placed)))
     (t
      (setq bogus-balls (bb-show-bogus-balls bb-balls-placed bb-board))
      (if (= bogus-balls 0)
	  (message "Right!  Your score is %d." bb-score)
	(message "Oops!  You missed %d ball%s.  Your score is %d."
		 bogus-balls
		 (if (= bogus-balls 1) "" "s")
		 (+ bb-score (* 5 bogus-balls))))
      (bb-goto '(-1 . -1))))))

(defun bb-show-bogus-balls (balls-placed board)
  (bb-show-bogus-balls-2 balls-placed board "x")
  (bb-show-bogus-balls-2 board balls-placed "o"))

(defun bb-show-bogus-balls-2 (list-1 list-2 c)
  (cond
   ((null list-1)
    0)
   ((member (car list-1) list-2)
    (bb-show-bogus-balls-2 (cdr list-1) list-2 c))
   (t
    (bb-goto (car list-1))
    (bb-update-board c)
    (1+ (bb-show-bogus-balls-2 (cdr list-1) list-2 c)))))

(defun bb-outside-box (x y)
  (or (= x -1) (= x 8) (= y -1) (= y 8)))

(defun bb-goto (pos)
  (goto-char (+ (* (car pos) 2) (* (cdr pos) 22) 26)))

(defun bb-update-board (c)
  (let ((buffer-read-only nil))
    (backward-char (1- (length c)))
    (delete-char (length c))
    (insert c)
    (backward-char 1)))

(provide 'blackbox)

;;; blackbox.el ends here
