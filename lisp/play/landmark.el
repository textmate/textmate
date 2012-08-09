;;; landmark.el --- neural-network robot that learns landmarks

;; Copyright (C) 1996-1997, 2000-2012 Free Software Foundation, Inc.

;; Author: Terrence Brannon (was: <brannon@rana.usc.edu>)
;; Created: December 16, 1996 - first release to usenet
;; Keywords: games, gomoku, neural network, adaptive search, chemotaxis

;;;_* Usage
;;; Just type
;;;   M-x eval-buffer
;;;   M-x landmark-test-run


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
;; Landmark is a relatively non-participatory game in which a robot
;; attempts to maneuver towards a tree at the center of the window
;; based on unique olfactory cues from each of the 4 directions. If
;; the smell of the tree increases, then the weights in the robot's
;; brain are adjusted to encourage this odor-driven behavior in the
;; future. If the smell of the tree decreases, the robots weights are
;; adjusted to discourage a correct move.

;; In laymen's terms, the search space is initially flat. The point
;; of training is to "turn up the edges of the search space" so that
;; the robot rolls toward the center.

;; Further, do not become alarmed if the robot appears to oscillate
;; back and forth between two or a few positions. This simply means
;; it is currently caught in a local minimum and is doing its best to
;; work its way out.

;; The version of this program as described has a small problem. a
;; move in a net direction can produce gross credit assignment. for
;; example, if moving south will produce positive payoff, then, if in
;; a single move, one moves east,west and south, then both east and
;; west will be improved when they shouldn't

;; Many thanks to Yuri Pryadkin <yuri@rana.usc.edu> for this
;; concise problem description.

;;;_* Require
(eval-when-compile (require 'cl))

;;;_* From Gomoku

;;; Code:

(defgroup landmark nil
  "Neural-network robot that learns landmarks."
  :prefix "landmark-"
  :group 'games)

;;;_ +  THE BOARD.

;; The board is a rectangular grid. We code empty squares with 0, X's with 1
;; and O's with 6. The rectangle is recorded in a one dimensional vector
;; containing padding squares (coded with -1). These squares allow us to
;; detect when we are trying to move out of the board.  We denote a square by
;; its (X,Y) coords, or by the INDEX corresponding to them in the vector.  The
;; leftmost topmost square has coords (1,1) and index landmark-board-width + 2.
;; Similarly, vectors between squares may be given by two DX, DY coords or by
;; one DEPL (the difference between indexes).

(defvar landmark-board-width nil
  "Number of columns on the Landmark board.")
(defvar landmark-board-height nil
  "Number of lines on the Landmark board.")

(defvar landmark-board nil
  "Vector recording the actual state of the Landmark board.")

(defvar landmark-vector-length nil
  "Length of landmark-board vector.")

(defvar landmark-draw-limit nil
  ;; This is usually set to 70% of the number of squares.
  "After how many moves will Emacs offer a draw?")

(defvar landmark-cx 0
  "This is the x coordinate of the center of the board.")

(defvar landmark-cy 0
  "This is the y coordinate of the center of the board.")

(defvar landmark-m 0
  "This is the x dimension of the playing board.")

(defvar landmark-n 0
  "This is the y dimension of the playing board.")


(defun landmark-xy-to-index (x y)
  "Translate X, Y cartesian coords into the corresponding board index."
  (+ (* y landmark-board-width) x y))

(defun landmark-index-to-x (index)
  "Return corresponding x-coord of board INDEX."
  (% index (1+ landmark-board-width)))

(defun landmark-index-to-y (index)
  "Return corresponding y-coord of board INDEX."
  (/ index (1+ landmark-board-width)))

(defun landmark-init-board ()
  "Create the landmark-board vector and fill it with initial values."
  (setq landmark-board (make-vector landmark-vector-length 0))
  ;; Every square is 0 (i.e. empty) except padding squares:
  (let ((i 0) (ii (1- landmark-vector-length)))
    (while (<= i landmark-board-width)	; The squares in [0..width] and in
      (aset landmark-board i  -1)		;    [length - width - 1..length - 1]
      (aset landmark-board ii -1)		;    are padding squares.
      (setq i  (1+ i)
	    ii (1- ii))))
  (let ((i 0))
    (while (< i landmark-vector-length)
      (aset landmark-board i -1)		; and also all k*(width+1)
      (setq i (+ i landmark-board-width 1)))))

;;;_ +  DISPLAYING THE BOARD.

;; You may change these values if you have a small screen or if the squares
;; look rectangular, but spacings SHOULD be at least 2 (MUST BE at least 1).

(defconst landmark-square-width 2
  "*Horizontal spacing between squares on the Landmark board.")

(defconst landmark-square-height 1
  "*Vertical spacing between squares on the Landmark board.")

(defconst landmark-x-offset 3
  "*Number of columns between the Landmark board and the side of the window.")

(defconst landmark-y-offset 1
  "*Number of lines between the Landmark board and the top of the window.")


;;;_ +  LANDMARK MODE AND KEYMAP.

(defcustom landmark-mode-hook nil
  "If non-nil, its value is called on entry to Landmark mode."
  :type 'hook
  :group 'landmark)

(defvar landmark-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Key bindings for cursor motion.
    (define-key map "y" 'landmark-move-nw)		; y
    (define-key map "u" 'landmark-move-ne)		; u
    (define-key map "b" 'landmark-move-sw)		; b
    (define-key map "n" 'landmark-move-se)		; n
    (define-key map "h" 'backward-char)		; h
    (define-key map "l" 'forward-char)		; l
    (define-key map "j" 'landmark-move-down)		; j
    (define-key map "k" 'landmark-move-up)		; k

    (define-key map [kp-7] 'landmark-move-nw)
    (define-key map [kp-9] 'landmark-move-ne)
    (define-key map [kp-1] 'landmark-move-sw)
    (define-key map [kp-3] 'landmark-move-se)
    (define-key map [kp-4] 'backward-char)
    (define-key map [kp-6] 'forward-char)
    (define-key map [kp-2] 'landmark-move-down)
    (define-key map [kp-8] 'landmark-move-up)

    (define-key map "\C-n" 'landmark-move-down)		; C-n
    (define-key map "\C-p" 'landmark-move-up)		; C-p

    ;; Key bindings for entering Human moves.
    (define-key map "X" 'landmark-human-plays)		; X
    (define-key map "x" 'landmark-human-plays)		; x

    (define-key map " " 'landmark-start-robot)		; SPC
    (define-key map [down-mouse-1] 'landmark-start-robot)
    (define-key map [drag-mouse-1] 'landmark-click)
    (define-key map [mouse-1] 'landmark-click)
    (define-key map [down-mouse-2] 'landmark-click)
    (define-key map [mouse-2] 'landmark-mouse-play)
    (define-key map [drag-mouse-2] 'landmark-mouse-play)

    (define-key map [remap previous-line] 'landmark-move-up)
    (define-key map [remap next-line] 'landmark-move-down)
    (define-key map [remap beginning-of-line] 'landmark-beginning-of-line)
    (define-key map [remap end-of-line] 'landmark-end-of-line)
    (define-key map [remap undo] 'landmark-human-takes-back)
    (define-key map [remap advertised-undo] 'landmark-human-takes-back)
    map)
  "Local keymap to use in Landmark mode.")



(defvar landmark-emacs-won ()
  "*For making font-lock use the winner's face for the line.")

(defface landmark-font-lock-face-O '((((class color)) :foreground "red")
			       (t :weight bold))
  "Face to use for Emacs's O."
  :version "22.1"
  :group 'landmark)

(defface landmark-font-lock-face-X '((((class color)) :foreground "green")
			       (t :weight bold))
  "Face to use for your X."
  :version "22.1"
  :group 'landmark)

(defvar landmark-font-lock-keywords
  '(("O" . 'landmark-font-lock-face-O)
    ("X" . 'landmark-font-lock-face-X)
    ("[-|/\\]" 0 (if landmark-emacs-won
		     'landmark-font-lock-face-O
		   'landmark-font-lock-face-X)))
  "*Font lock rules for Landmark.")

(put 'landmark-mode 'front-sticky
     (put 'landmark-mode 'rear-nonsticky '(intangible)))
(put 'landmark-mode 'intangible 1)
;; This one is for when they set view-read-only to t: Landmark cannot
;; allow View Mode to be activated in its buffer.
(put 'landmark-mode 'mode-class 'special)

(defun landmark-mode ()
  "Major mode for playing Landmark against Emacs.
You and Emacs play in turn by marking a free square.  You mark it with X
and Emacs marks it with O.  The winner is the first to get five contiguous
marks horizontally, vertically or in diagonal.

You play by moving the cursor over the square you choose and hitting \\[landmark-human-plays].

Other useful commands:
\\{landmark-mode-map}
Entry to this mode calls the value of `landmark-mode-hook' if that value
is non-nil.  One interesting value is `turn-on-font-lock'."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'landmark-mode
	mode-name "Landmark")
  (landmark-display-statistics)
  (use-local-map landmark-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(landmark-font-lock-keywords t)
	buffer-read-only t)
  (run-mode-hooks 'landmark-mode-hook))


;;;_ +  THE SCORE TABLE.


;; Every (free) square has a score associated to it, recorded in the
;; LANDMARK-SCORE-TABLE vector. The program always plays in the square having
;; the highest score.

(defvar landmark-score-table nil
  "Vector recording the actual score of the free squares.")


;; The key point point about the algorithm is that, rather than considering
;; the board as just a set of squares, we prefer to see it as a "space" of
;; internested 5-tuples of contiguous squares (called qtuples).
;;
;; The aim of the program is to fill one qtuple with its O's while preventing
;; you from filling another one with your X's. To that effect, it computes a
;; score for every qtuple, with better qtuples having better scores. Of
;; course, the score of a qtuple (taken in isolation) is just determined by
;; its contents as a set, i.e. not considering the order of its elements. The
;; highest score is given to the "OOOO" qtuples because playing in such a
;; qtuple is winning the game. Just after this comes the "XXXX" qtuple because
;; not playing in it is just losing the game, and so on. Note that a
;; "polluted" qtuple, i.e. one containing at least one X and at least one O,
;; has score zero because there is no more any point in playing in it, from
;; both an attacking and a defending point of view.
;;
;; Given the score of every qtuple, the score of a given free square on the
;; board is just the sum of the scores of all the qtuples to which it belongs,
;; because playing in that square is playing in all its containing qtuples at
;; once. And it is that function which takes into account the internesting of
;; the qtuples.
;;
;; This algorithm is rather simple but anyway it gives a not so dumb level of
;; play. It easily extends to "n-dimensional Landmark", where a win should not
;; be obtained with as few as 5 contiguous marks: 6 or 7 (depending on n !)
;; should be preferred.


;; Here are the scores of the nine "non-polluted" configurations.  Tuning
;; these values will change (hopefully improve) the strength of the program
;; and may change its style (rather aggressive here).

(defconst landmark-nil-score	  7  "Score of an empty qtuple.")

(defconst landmark-score-trans-table
  (let ((Xscore		15)  ; Score of a qtuple containing one X.
        (XXscore       400)  ; Score of a qtuple containing two X's.
        (XXXscore     1800)  ; Score of a qtuple containing three X's.
        (XXXXscore  100000)  ; Score of a qtuple containing four X's.
        (Oscore		35)  ; Score of a qtuple containing one O.
        (OOscore       800)  ; Score of a qtuple containing two O's.
        (OOOscore    15000)  ; Score of a qtuple containing three O's.
        (OOOOscore  800000)) ; Score of a qtuple containing four O's.

    ;; These values are not just random: if, given the following situation:
    ;;
    ;;			  . . . . . . . O .
    ;;			  . X X a . . . X .
    ;;			  . . . X . . . X .
    ;;			  . . . X . . . X .
    ;;			  . . . . . . . b .
    ;;
    ;; you want Emacs to play in "a" and not in "b", then the parameters must
    ;; satisfy the inequality:
    ;;
    ;;		   6 * XXscore > XXXscore + XXscore
    ;;
    ;; because "a" mainly belongs to six "XX" qtuples (the others are less
    ;; important) while "b" belongs to one "XXX" and one "XX" qtuples.
    ;; Other conditions are required to obtain sensible moves, but the
    ;; previous example should illustrate the point.  If you manage to
    ;; improve on these values, please send me a note.  Thanks.


    ;; As we chose values 0, 1 and 6 to denote empty, X and O squares,
    ;; the contents of a qtuple are uniquely determined by the sum of
    ;; its elements and we just have to set up a translation table.
    (vector landmark-nil-score Xscore XXscore XXXscore XXXXscore 0
            Oscore       0	0	0	 0	   0
            OOscore      0	0	0	 0	   0
            OOOscore     0	0	0	 0	   0
            OOOOscore    0	0	0	 0	   0
            0))
  "Vector associating qtuple contents to their score.")


;; If you do not modify drastically the previous constants, the only way for a
;; square to have a score higher than OOOOscore is to belong to a "OOOO"
;; qtuple, thus to be a winning move. Similarly, the only way for a square to
;; have a score between XXXXscore and OOOOscore is to belong to a "XXXX"
;; qtuple. We may use these considerations to detect when a given move is
;; winning or losing.

(defconst landmark-winning-threshold
  (aref landmark-score-trans-table (+ 6 6 6 6)) ;; OOOOscore
  "Threshold score beyond which an Emacs move is winning.")

(defconst landmark-losing-threshold
  (aref landmark-score-trans-table (+ 1 1 1 1)) ;; XXXXscore
  "Threshold score beyond which a human move is winning.")


(defun landmark-strongest-square ()
  "Compute index of free square with highest score, or nil if none."
  ;; We just have to loop other all squares. However there are two problems:
  ;; 1/ The SCORE-TABLE only gives correct scores to free squares. To speed
  ;;	up future searches, we set the score of padding or occupied squares
  ;;	to -1 whenever we meet them.
  ;; 2/ We want to choose randomly between equally good moves.
  (let ((score-max 0)
	(count	   0)			; Number of equally good moves
	(square	   (landmark-xy-to-index 1 1)) ; First square
	(end	   (landmark-xy-to-index landmark-board-width landmark-board-height))
	best-square score)
    (while (<= square end)
      (cond
       ;; If score is lower (i.e. most of the time), skip to next:
       ((< (aref landmark-score-table square) score-max))
       ;; If score is better, beware of non free squares:
       ((> (setq score (aref landmark-score-table square)) score-max)
	(if (zerop (aref landmark-board square)) ; is it free ?
	    (setq count 1		       ; yes: take it !
		  best-square square
		  score-max   score)
	    (aset landmark-score-table square -1))) ; no: kill it !
       ;; If score is equally good, choose randomly. But first check freedom:
       ((not (zerop (aref landmark-board square)))
	(aset landmark-score-table square -1))
       ((zerop (random (setq count (1+ count))))
	(setq best-square square
	      score-max	  score)))
      (setq square (1+ square)))	; try next square
    best-square))

;;;_  -  INITIALIZING THE SCORE TABLE.

;; At initialization the board is empty so that every qtuple amounts for
;; nil-score. Therefore, the score of any square is nil-score times the number
;; of qtuples that pass through it. This number is 3 in a corner and 20 if you
;; are sufficiently far from the sides. As computing the number is time
;; consuming, we initialize every square with 20*nil-score and then only
;; consider squares at less than 5 squares from one side. We speed this up by
;; taking symmetry into account.
;; Also, as it is likely that successive games will be played on a board with
;; same size, it is a good idea to save the initial SCORE-TABLE configuration.

(defvar landmark-saved-score-table nil
  "Recorded initial value of previous score table.")

(defvar landmark-saved-board-width nil
  "Recorded value of previous board width.")

(defvar landmark-saved-board-height nil
  "Recorded value of previous board height.")


(defun landmark-init-score-table ()
  "Create the score table vector and fill it with initial values."
  (if (and landmark-saved-score-table	; Has it been stored last time ?
	   (= landmark-board-width  landmark-saved-board-width)
	   (= landmark-board-height landmark-saved-board-height))
      (setq landmark-score-table (copy-sequence landmark-saved-score-table))
      ;; No, compute it:
      (setq landmark-score-table
	    (make-vector landmark-vector-length (* 20 landmark-nil-score)))
      (let (i j maxi maxj maxi2 maxj2)
	(setq maxi  (/ (1+ landmark-board-width) 2)
	      maxj  (/ (1+ landmark-board-height) 2)
	      maxi2 (min 4 maxi)
	      maxj2 (min 4 maxj))
	;; We took symmetry into account and could use it more if the board
	;; would have been square and not rectangular !
	;; In our case we deal with all (i,j) in the set [1..maxi2]*[1..maxj] U
	;; [maxi2+1..maxi]*[1..maxj2]. Maxi2 and maxj2 are used because the
	;; board may well be less than 8 by 8 !
	(setq i 1)
	(while (<= i maxi2)
	  (setq j 1)
	  (while (<= j maxj)
	    (landmark-init-square-score i j)
	    (setq j (1+ j)))
	  (setq i (1+ i)))
	(while (<= i maxi)
	  (setq j 1)
	  (while (<= j maxj2)
	    (landmark-init-square-score i j)
	    (setq j (1+ j)))
	  (setq i (1+ i))))
      (setq landmark-saved-score-table  (copy-sequence landmark-score-table)
	    landmark-saved-board-width  landmark-board-width
	    landmark-saved-board-height landmark-board-height)))

(defun landmark-nb-qtuples (i j)
  "Return the number of qtuples containing square I,J."
  ;; This function is complicated because we have to deal
  ;; with ugly cases like 3 by 6 boards, but it works.
  ;; If you have a simpler (and correct) solution, send it to me. Thanks !
  (let ((left  (min 4 (1- i)))
	(right (min 4 (- landmark-board-width i)))
	(up    (min 4 (1- j)))
	(down  (min 4 (- landmark-board-height j))))
    (+ -12
       (min (max (+ left right) 3) 8)
       (min (max (+ up down) 3) 8)
       (min (max (+ (min left up) (min right down)) 3) 8)
       (min (max (+ (min right up) (min left down)) 3) 8))))

(defun landmark-init-square-score (i j)
  "Give initial score to square I,J and to its mirror images."
  (let ((ii (1+ (- landmark-board-width i)))
	(jj (1+ (- landmark-board-height j)))
	(sc (* (landmark-nb-qtuples i j) (aref landmark-score-trans-table 0))))
    (aset landmark-score-table (landmark-xy-to-index i  j)	sc)
    (aset landmark-score-table (landmark-xy-to-index ii j)	sc)
    (aset landmark-score-table (landmark-xy-to-index i  jj) sc)
    (aset landmark-score-table (landmark-xy-to-index ii jj) sc)))
;;;_  - MAINTAINING THE SCORE TABLE.


;; We do not provide functions for computing the SCORE-TABLE given the
;; contents of the BOARD. This would involve heavy nested loops, with time
;; proportional to the size of the board. It is better to update the
;; SCORE-TABLE after each move. Updating needs not modify more than 36
;; squares: it is done in constant time.

(defun landmark-update-score-table (square dval)
  "Update score table after SQUARE received a DVAL increment."
  ;; The board has already been updated when this function is called.
  ;; Updating scores is done by looking for qtuples boundaries in all four
  ;; directions and then calling update-score-in-direction.
  ;; Finally all squares received the right increment, and then are up to
  ;; date, except possibly for SQUARE itself if we are taking a move back for
  ;; its score had been set to -1 at the time.
  (let* ((x    (landmark-index-to-x square))
	 (y    (landmark-index-to-y square))
	 (imin (max -4 (- 1 x)))
	 (jmin (max -4 (- 1 y)))
	 (imax (min 0 (- landmark-board-width x 4)))
	 (jmax (min 0 (- landmark-board-height y 4))))
    (landmark-update-score-in-direction imin imax
				      square 1 0 dval)
    (landmark-update-score-in-direction jmin jmax
				      square 0 1 dval)
    (landmark-update-score-in-direction (max imin jmin) (min imax jmax)
				      square 1 1 dval)
    (landmark-update-score-in-direction (max (- 1 y) -4
					   (- x landmark-board-width))
				      (min 0 (- x 5)
					   (- landmark-board-height y 4))
				      square -1 1 dval)))

(defun landmark-update-score-in-direction (left right square dx dy dval)
  "Update scores for all squares in the qtuples in range.
That is, those between the LEFTth square and the RIGHTth after SQUARE,
along the DX, DY direction, considering that DVAL has been added on SQUARE."
  ;; We always have LEFT <= 0, RIGHT <= 0 and DEPL > 0 but we may very well
  ;; have LEFT > RIGHT, indicating that no qtuple contains SQUARE along that
  ;; DX,DY direction.
  (cond
   ((> left right))			; Quit
   (t					; Else ..
    (let (depl square0 square1 square2 count delta)
      (setq depl    (landmark-xy-to-index dx dy)
	    square0 (+ square (* left depl))
	    square1 (+ square (* right depl))
	    square2 (+ square0 (* 4 depl)))
      ;; Compute the contents of the first qtuple:
      (setq square square0
	    count  0)
      (while (<= square square2)
	(setq count  (+ count (aref landmark-board square))
	      square (+ square depl)))
      (while (<= square0 square1)
	;; Update the squares of the qtuple beginning in SQUARE0 and ending
	;; in SQUARE2.
	(setq delta (- (aref landmark-score-trans-table count)
		       (aref landmark-score-trans-table (- count dval))))
	(cond ((not (zerop delta))	; or else nothing to update
	       (setq square square0)
	       (while (<= square square2)
		 (if (zerop (aref landmark-board square)) ; only for free squares
		     (aset landmark-score-table square
			   (+ (aref landmark-score-table square) delta)))
		 (setq square (+ square depl)))))
	;; Then shift the qtuple one square along DEPL, this only requires
	;; modifying SQUARE0 and SQUARE2.
	(setq square2 (+ square2 depl)
	      count   (+ count (- (aref landmark-board square0))
			 (aref landmark-board square2))
	      square0 (+ square0 depl)))))))

;;;
;;; GAME CONTROL.
;;;

;; Several variables are used to monitor a game, including a GAME-HISTORY (the
;; list of all (SQUARE . PREVSCORE) played) that allows to take moves back
;; (anti-updating the score table) and to compute the table from scratch in
;; case of an interruption.

(defvar landmark-game-in-progress nil
  "Non-nil if a game is in progress.")

(defvar landmark-game-history nil
  "A record of all moves that have been played during current game.")

(defvar landmark-number-of-moves nil
  "Number of moves already played in current game.")

(defvar landmark-number-of-human-moves nil
  "Number of moves already played by human in current game.")

(defvar landmark-emacs-played-first nil
  "Non-nil if Emacs played first.")

(defvar landmark-human-took-back nil
  "Non-nil if Human took back a move during the game.")

(defvar landmark-human-refused-draw nil
  "Non-nil if Human refused Emacs offer of a draw.")

(defvar landmark-emacs-is-computing nil
  ;; This is used to detect interruptions. Hopefully, it should not be needed.
  "Non-nil if Emacs is in the middle of a computation.")


(defun landmark-start-game (n m)
  "Initialize a new game on an N by M board."
  (setq landmark-emacs-is-computing t)	; Raise flag
  (setq landmark-game-in-progress t)
  (setq landmark-board-width   n
	landmark-board-height  m
	landmark-vector-length (1+ (* (+ m 2) (1+ n)))
	landmark-draw-limit    (/ (* 7 n m) 10))
  (setq landmark-emacs-won	         nil
	landmark-game-history	         nil
	landmark-number-of-moves	 0
	landmark-number-of-human-moves 0
	landmark-emacs-played-first    nil
	landmark-human-took-back	 nil
	landmark-human-refused-draw    nil)
  (landmark-init-display n m)		; Display first: the rest takes time
  (landmark-init-score-table)		; INIT-BOARD requires that the score
  (landmark-init-board)			;   table be already created.
  (setq landmark-emacs-is-computing nil))

(defun landmark-play-move (square val &optional dont-update-score)
  "Go to SQUARE, play VAL and update everything."
  (setq landmark-emacs-is-computing t)	; Raise flag
  (cond ((= 1 val)			; a Human move
	 (setq landmark-number-of-human-moves (1+ landmark-number-of-human-moves)))
	((zerop landmark-number-of-moves)	; an Emacs move. Is it first ?
	 (setq landmark-emacs-played-first t)))
  (setq landmark-game-history
	(cons (cons square (aref landmark-score-table square))
	      landmark-game-history)
	landmark-number-of-moves (1+ landmark-number-of-moves))
  (landmark-plot-square square val)
  (aset landmark-board square val)	; *BEFORE* UPDATE-SCORE !
  (if dont-update-score nil
      (landmark-update-score-table square val) ; previous val was 0: dval = val
      (aset landmark-score-table square -1))
  (setq landmark-emacs-is-computing nil))

(defun landmark-take-back ()
  "Take back last move and update everything."
  (setq landmark-emacs-is-computing t)
  (let* ((last-move (car landmark-game-history))
	 (square (car last-move))
	 (oldval (aref landmark-board square)))
    (if (= 1 oldval)
	(setq landmark-number-of-human-moves (1- landmark-number-of-human-moves)))
    (setq landmark-game-history	 (cdr landmark-game-history)
	  landmark-number-of-moves (1- landmark-number-of-moves))
    (landmark-plot-square square 0)
    (aset landmark-board square 0)	; *BEFORE* UPDATE-SCORE !
    (landmark-update-score-table square (- oldval))
    (aset landmark-score-table square (cdr last-move)))
  (setq landmark-emacs-is-computing nil))


;;;_ +  SESSION CONTROL.

(defvar landmark-number-of-trials 0
  "The number of times that landmark has been run.")

(defvar landmark-sum-of-moves 0
  "The total number of moves made in all games.")

(defvar landmark-number-of-emacs-wins 0
  "Number of games Emacs won in this session.")

(defvar landmark-number-of-human-wins 0
  "Number of games you won in this session.")

(defvar landmark-number-of-draws 0
  "Number of games already drawn in this session.")


(defun landmark-terminate-game (result)
  "Terminate the current game with RESULT."
  (setq landmark-number-of-trials (1+ landmark-number-of-trials))
  (setq landmark-sum-of-moves (+ landmark-sum-of-moves landmark-number-of-moves))
  (if (eq result 'crash-game)
      (message
       "Sorry, I have been interrupted and cannot resume that game..."))
  (landmark-display-statistics)
  ;;(ding)
  (setq landmark-game-in-progress nil))

(defun landmark-crash-game ()
  "What to do when Emacs detects it has been interrupted."
  (setq landmark-emacs-is-computing nil)
  (landmark-terminate-game 'crash-game)
  (sit-for 4)				; Let's see the message
  (landmark-prompt-for-other-game))


;;;_ +  INTERACTIVE COMMANDS.

(defun landmark-emacs-plays ()
  "Compute Emacs next move and play it."
  (interactive)
  (landmark-switch-to-window)
  (cond
   (landmark-emacs-is-computing
    (landmark-crash-game))
   ((not landmark-game-in-progress)
    (landmark-prompt-for-other-game))
   (t
    (message "Let me think...")
    (let (square score)
      (setq square (landmark-strongest-square))
      (cond ((null square)
	     (landmark-terminate-game 'nobody-won))
	    (t
	     (setq score (aref landmark-score-table square))
	     (landmark-play-move square 6)
	     (cond ((>= score landmark-winning-threshold)
		    (setq landmark-emacs-won t) ; for font-lock
		    (landmark-find-filled-qtuple square 6)
		    (landmark-terminate-game 'emacs-won))
		   ((zerop score)
		    (landmark-terminate-game 'nobody-won))
		   ((and (> landmark-number-of-moves landmark-draw-limit)
			 (not landmark-human-refused-draw)
			 (landmark-offer-a-draw))
		    (landmark-terminate-game 'draw-agreed))
		   (t
		    (landmark-prompt-for-move)))))))))

;; For small square dimensions this is approximate, since though measured in
;; pixels, event's (X . Y) is a character's top-left corner.
(defun landmark-click (click)
  "Position at the square where you click."
  (interactive "e")
  (and (windowp (posn-window (setq click (event-end click))))
       (numberp (posn-point click))
       (select-window (posn-window click))
       (setq click (posn-col-row click))
       (landmark-goto-xy
	(min (max (/ (+ (- (car click)
			   landmark-x-offset
			   1)
			(window-hscroll)
			landmark-square-width
			(% landmark-square-width 2)
			(/ landmark-square-width 2))
		     landmark-square-width)
		  1)
	     landmark-board-width)
	(min (max (/ (+ (- (cdr click)
			   landmark-y-offset
			   1)
			(let ((inhibit-point-motion-hooks t))
			  (count-lines 1 (window-start)))
			landmark-square-height
			(% landmark-square-height 2)
			(/ landmark-square-height 2))
		     landmark-square-height)
		  1)
	     landmark-board-height))))

(defun landmark-mouse-play (click)
  "Play at the square where you click."
  (interactive "e")
  (if (landmark-click click)
      (landmark-human-plays)))

(defun landmark-human-plays ()
  "Signal to the Landmark program that you have played.
You must have put the cursor on the square where you want to play.
If the game is finished, this command requests for another game."
  (interactive)
  (landmark-switch-to-window)
  (cond
   (landmark-emacs-is-computing
    (landmark-crash-game))
   ((not landmark-game-in-progress)
    (landmark-prompt-for-other-game))
   (t
    (let (square score)
      (setq square (landmark-point-square))
      (cond ((null square)
	     (error "Your point is not on a square. Retry!"))
	    ((not (zerop (aref landmark-board square)))
	     (error "Your point is not on a free square. Retry!"))
	    (t
	     (setq score (aref landmark-score-table square))
	     (landmark-play-move square 1)
	     (cond ((and (>= score landmark-losing-threshold)
			 ;; Just testing SCORE > THRESHOLD is not enough for
			 ;; detecting wins, it just gives an indication that
			 ;; we confirm with LANDMARK-FIND-FILLED-QTUPLE.
			 (landmark-find-filled-qtuple square 1))
		    (landmark-terminate-game 'human-won))
		   (t
		    (landmark-emacs-plays)))))))))

(defun landmark-human-takes-back ()
  "Signal to the Landmark program that you wish to take back your last move."
  (interactive)
  (landmark-switch-to-window)
  (cond
   (landmark-emacs-is-computing
    (landmark-crash-game))
   ((not landmark-game-in-progress)
    (message "Too late for taking back...")
    (sit-for 4)
    (landmark-prompt-for-other-game))
   ((zerop landmark-number-of-human-moves)
    (message "You have not played yet... Your move?"))
   (t
    (message "One moment, please...")
    ;; It is possible for the user to let Emacs play several consecutive
    ;; moves, so that the best way to know when to stop taking back moves is
    ;; to count the number of human moves:
    (setq landmark-human-took-back t)
    (let ((number landmark-number-of-human-moves))
      (while (= number landmark-number-of-human-moves)
	(landmark-take-back)))
    (landmark-prompt-for-move))))

(defun landmark-human-resigns ()
  "Signal to the Landmark program that you may want to resign."
  (interactive)
  (landmark-switch-to-window)
  (cond
   (landmark-emacs-is-computing
    (landmark-crash-game))
   ((not landmark-game-in-progress)
    (message "There is no game in progress"))
   ((y-or-n-p "You mean, you resign? ")
    (landmark-terminate-game 'human-resigned))
   ((y-or-n-p "You mean, we continue? ")
    (landmark-prompt-for-move))
   (t
    (landmark-terminate-game 'human-resigned)))) ; OK. Accept it

;;;_ + PROMPTING THE HUMAN PLAYER.

(defun landmark-prompt-for-move ()
  "Display a message asking for Human's move."
  (message (if (zerop landmark-number-of-human-moves)
	       "Your move? (move to a free square and hit X, RET ...)"
	       "Your move?")))

(defun landmark-prompt-for-other-game ()
  "Ask for another game, and start it."
  (if (y-or-n-p "Another game? ")
      (if (y-or-n-p "Retain learned weights ")
	  (landmark 2)
	(landmark 1))
    (message "Chicken!")))

(defun landmark-offer-a-draw ()
  "Offer a draw and return t if Human accepted it."
  (or (y-or-n-p "I offer you a draw. Do you accept it? ")
      (not (setq landmark-human-refused-draw t))))


(defun landmark-max-width ()
  "Largest possible board width for the current window."
  (1+ (/ (- (window-width (selected-window))
	    landmark-x-offset landmark-x-offset 1)
	 landmark-square-width)))

(defun landmark-max-height ()
  "Largest possible board height for the current window."
  (1+ (/ (- (window-height (selected-window))
	    landmark-y-offset landmark-y-offset 2)
	 ;; 2 instead of 1 because WINDOW-HEIGHT includes the mode line !
	 landmark-square-height)))

(defun landmark-point-y ()
  "Return the board row where point is."
  (let ((inhibit-point-motion-hooks t))
    (1+ (/ (- (count-lines 1 (point)) landmark-y-offset (if (bolp) 0 1))
	   landmark-square-height))))

(defun landmark-point-square ()
  "Return the index of the square point is on."
  (let ((inhibit-point-motion-hooks t))
    (landmark-xy-to-index (1+ (/ (- (current-column) landmark-x-offset)
			       landmark-square-width))
			(landmark-point-y))))

(defun landmark-goto-square (index)
  "Move point to square number INDEX."
  (landmark-goto-xy (landmark-index-to-x index) (landmark-index-to-y index)))

(defun landmark-goto-xy (x y)
  "Move point to square at X, Y coords."
  (let ((inhibit-point-motion-hooks t))
    (goto-char (point-min))
    (forward-line (+ landmark-y-offset (* landmark-square-height (1- y)))))
  (move-to-column (+ landmark-x-offset (* landmark-square-width (1- x)))))

(defun landmark-plot-square (square value)
  "Draw 'X', 'O' or '.' on SQUARE depending on VALUE, leave point there."
  (or (= value 1)
      (landmark-goto-square square))
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (insert-and-inherit (cond ((= value 1) ?.)
			      ((= value 2) ?N)
			      ((= value 3) ?S)
			      ((= value 4) ?E)
			      ((= value 5) ?W)
			      ((= value 6) ?^)))

    (and (zerop value)
	 (add-text-properties (1- (point)) (point)
			      '(mouse-face highlight
				help-echo "\
mouse-1: get robot moving, mouse-2: play on this square")))
    (delete-char 1)
    (backward-char 1))
  (sit-for 0))	; Display NOW

(defun landmark-init-display (n m)
  "Display an N by M Landmark board."
  (buffer-disable-undo (current-buffer))
  (let ((inhibit-read-only t)
	(point 1) opoint
	(intangible t)
	(i m) j x)
    ;; Try to minimize number of chars (because of text properties)
    (setq tab-width
	  (if (zerop (% landmark-x-offset landmark-square-width))
	      landmark-square-width
	    (max (/ (+ (% landmark-x-offset landmark-square-width)
		       landmark-square-width 1) 2) 2)))
    (erase-buffer)
    (newline landmark-y-offset)
    (while (progn
	     (setq j n
		   x (- landmark-x-offset landmark-square-width))
	     (while (>= (setq j (1- j)) 0)
	       (insert-char ?\t (/ (- (setq x (+ x landmark-square-width))
				      (current-column))
				   tab-width))
	       (insert-char ?  (- x (current-column)))
	       (if (setq intangible (not intangible))
		   (put-text-property point (point) 'intangible 2))
	       (and (zerop j)
		    (= i (- m 2))
		    (progn
		      (while (>= i 3)
			(append-to-buffer (current-buffer) opoint (point))
			(setq i (- i 2)))
		      (goto-char (point-max))))
	       (setq point (point))
	       (insert ?=)
	       (add-text-properties point (point)
				    '(mouse-face highlight help-echo "\
mouse-1: get robot moving, mouse-2: play on this square")))
	     (> (setq i (1- i)) 0))
      (if (= i (1- m))
	  (setq opoint point))
      (insert-char ?\n landmark-square-height))
    (or (eq (char-after 1) ?.)
	(put-text-property 1 2 'point-entered
			   (lambda (_x _y) (if (bobp) (forward-char)))))
    (or intangible
	(put-text-property point (point) 'intangible 2))
    (put-text-property point (point) 'point-entered
		       (lambda (_x _y) (if (eobp) (backward-char))))
    (put-text-property (point-min) (point) 'category 'landmark-mode))
  (landmark-goto-xy (/ (1+ n) 2) (/ (1+ m) 2)) ; center of the board
  (sit-for 0))				; Display NOW

(defun landmark-display-statistics ()
  "Obnoxiously display some statistics about previous games in mode line."
  ;; We store this string in the mode-line-process local variable.
  ;; This is certainly not the cleanest way out ...
  (setq mode-line-process
	(format ": Trials: %d, Avg#Moves: %d"
		landmark-number-of-trials
		(if (zerop landmark-number-of-trials)
		    0
		  (/ landmark-sum-of-moves landmark-number-of-trials))))
  (force-mode-line-update))

(defun landmark-switch-to-window ()
  "Find or create the Landmark buffer, and display it."
  (interactive)
  (let ((buff (get-buffer "*Landmark*")))
    (if buff				; Buffer exists:
	(switch-to-buffer buff)		;   no problem.
      (if landmark-game-in-progress
	  (landmark-crash-game))		;   buffer has been killed or something
      (switch-to-buffer "*Landmark*")	; Anyway, start anew.
      (landmark-mode))))


;;;_ + CROSSING WINNING QTUPLES.

;; When someone succeeds in filling a qtuple, we draw a line over the five
;; corresponding squares. One problem is that the program does not know which
;; squares ! It only knows the square where the last move has been played and
;; who won. The solution is to scan the board along all four directions.

(defun landmark-find-filled-qtuple (square value)
  "Return t if SQUARE belongs to a qtuple filled with VALUEs."
  (or (landmark-check-filled-qtuple square value 1 0)
      (landmark-check-filled-qtuple square value 0 1)
      (landmark-check-filled-qtuple square value 1 1)
      (landmark-check-filled-qtuple square value -1 1)))

(defun landmark-check-filled-qtuple (square value dx dy)
  "Return t if SQUARE belongs to a qtuple filled with VALUEs along DX, DY."
  (let ((a 0) (b 0)
	(left square) (right square)
	(depl (landmark-xy-to-index dx dy)))
    (while (and (> a -4)		; stretch tuple left
		(= value (aref landmark-board (setq left (- left depl)))))
      (setq a (1- a)))
    (while (and (< b (+ a 4))		; stretch tuple right
		(= value (aref landmark-board (setq right (+ right depl)))))
      (setq b (1+ b)))
    (cond ((= b (+ a 4))		; tuple length = 5 ?
	   (landmark-cross-qtuple (+ square (* a depl)) (+ square (* b depl))
				dx dy)
	   t))))

(defun landmark-cross-qtuple (square1 square2 dx dy)
  "Cross every square between SQUARE1 and SQUARE2 in the DX, DY direction."
  (save-excursion			; Not moving point from last square
    (let ((depl (landmark-xy-to-index dx dy))
	  (inhibit-read-only t)
	  (inhibit-point-motion-hooks t))
      ;; WARNING: this function assumes DEPL > 0 and SQUARE2 > SQUARE1
      (while (/= square1 square2)
	(landmark-goto-square square1)
	(setq square1 (+ square1 depl))
	(cond
	  ((= dy 0)			; Horizontal
	   (forward-char 1)
	   (insert-char ?- (1- landmark-square-width) t)
	   (delete-region (point) (progn
				    (skip-chars-forward " \t")
				    (point))))
	  ((= dx 0)			; Vertical
	   (let ((landmark-n 1)
		 (column (current-column)))
	     (while (< landmark-n landmark-square-height)
	       (setq landmark-n (1+ landmark-n))
	       (forward-line 1)
	       (indent-to column)
	       (insert-and-inherit ?|))))
	  ((= dx -1)			; 1st Diagonal
	   (indent-to (prog1 (- (current-column) (/ landmark-square-width 2))
			(forward-line (/ landmark-square-height 2))))
	   (insert-and-inherit ?/))
	  (t				; 2nd Diagonal
	   (indent-to (prog1 (+ (current-column) (/ landmark-square-width 2))
			(forward-line (/ landmark-square-height 2))))
	   (insert-and-inherit ?\\))))))
  (sit-for 0))				; Display NOW


;;;_ + CURSOR MOTION.

;; previous-line and next-line don't work right with intangible newlines
(defun landmark-move-down ()
  "Move point down one row on the Landmark board."
  (interactive)
  (if (< (landmark-point-y) landmark-board-height)
      (forward-line 1)));;; landmark-square-height)))

(defun landmark-move-up ()
  "Move point up one row on the Landmark board."
  (interactive)
  (if (> (landmark-point-y) 1)
      (forward-line (- landmark-square-height))))

(defun landmark-move-ne ()
  "Move point North East on the Landmark board."
  (interactive)
  (landmark-move-up)
  (forward-char))

(defun landmark-move-se ()
  "Move point South East on the Landmark board."
  (interactive)
  (landmark-move-down)
  (forward-char))

(defun landmark-move-nw ()
  "Move point North West on the Landmark board."
  (interactive)
  (landmark-move-up)
  (backward-char))

(defun landmark-move-sw ()
  "Move point South West on the Landmark board."
  (interactive)
  (landmark-move-down)
  (backward-char))

(defun landmark-beginning-of-line ()
  "Move point to first square on the Landmark board row."
  (interactive)
  (move-to-column landmark-x-offset))

(defun landmark-end-of-line ()
  "Move point to last square on the Landmark board row."
  (interactive)
  (move-to-column (+ landmark-x-offset
		     (* landmark-square-width (1- landmark-board-width)))))


;;;_ + Simulation variables

;;;_  - landmark-nvar
(defvar landmark-nvar 0.0075
  "Not used.
Affects a noise generator which was used in an earlier incarnation of
this program to add a random element to the way moves were made.")
;;;_  - lists of cardinal directions
;;;_   :
(defvar landmark-ns '(landmark-n landmark-s)
  "Used when doing something relative to the north and south axes.")
(defvar landmark-ew '(landmark-e landmark-w)
  "Used when doing something relative to the east and west axes.")
(defvar landmark-directions '(landmark-n landmark-s landmark-e landmark-w)
  "The cardinal directions.")
(defvar landmark-8-directions
  '((landmark-n) (landmark-n landmark-w) (landmark-w) (landmark-s landmark-w)
    (landmark-s) (landmark-s landmark-e) (landmark-e) (landmark-n landmark-e))
  "The full 8 possible directions.")

(defvar landmark-number-of-moves
  "The number of moves made by the robot so far.")


;;;_* Terry's mods to create lm.el

;;;(setq landmark-debug nil)
(defvar landmark-debug nil
  "If non-nil, debugging is printed.")
(defcustom landmark-one-moment-please nil
  "If non-nil, print \"One moment please\" when a new board is generated.
The drawback of this is you don't see how many moves the last run took
because it is overwritten by \"One moment please\"."
  :type 'boolean
  :group 'landmark)
(defcustom landmark-output-moves t
  "If non-nil, output number of moves so far on a move-by-move basis."
  :type 'boolean
  :group 'landmark)


(defun landmark-weights-debug ()
  (if landmark-debug
      (progn (landmark-print-wts) (landmark-blackbox) (landmark-print-y-s-noise)
	     (landmark-print-smell))))

;;;_  - Printing various things
(defun landmark-print-distance-int (direction)
  (interactive)
  (insert (format "%S %S " direction  (get direction 'distance))))


(defun landmark-print-distance ()
  (insert (format "tree: %S \n" (calc-distance-of-robot-from 'landmark-tree)))
  (mapc 'landmark-print-distance-int landmark-directions))


;;(setq direction 'landmark-n)
;;(get 'landmark-n 'landmark-s)
(defun landmark-nslify-wts-int (direction)
  (mapcar (lambda (target-direction)
	     (get direction target-direction))
	  landmark-directions))


(defun landmark-nslify-wts ()
  (interactive)
  (let ((l (apply 'append (mapcar 'landmark-nslify-wts-int landmark-directions))))
    (insert (format "set data_value WTS \n %s \n" l))
    (insert (format "/* max: %S min: %S */"
		  (eval (cons 'max l)) (eval (cons 'min l))))))

(defun landmark-print-wts-int (direction)
  (mapc (lambda (target-direction)
	     (insert (format "%S %S %S "
			      direction
			      target-direction
			     (get direction target-direction))))
	  landmark-directions)
  (insert "\n"))

(defun landmark-print-wts ()
  (interactive)
  (with-current-buffer "*landmark-wts*"
    (insert "==============================\n")
    (mapc 'landmark-print-wts-int landmark-directions)))

(defun landmark-print-moves (moves)
  (interactive)
  (with-current-buffer "*landmark-moves*"
    (insert (format "%S\n" moves))))


(defun landmark-print-y-s-noise-int (direction)
  (insert (format "%S:landmark-y %S, s %S, noise %S \n"
		    (symbol-name direction)
		    (get direction 'y_t)
		    (get direction 's)
		    (get direction 'noise)
		    )))

(defun landmark-print-y-s-noise ()
  (interactive)
  (with-current-buffer "*landmark-y,s,noise*"
    (insert "==============================\n")
    (mapc 'landmark-print-y-s-noise-int landmark-directions)))

(defun landmark-print-smell-int (direction)
  (insert (format "%S: smell: %S \n"
		    (symbol-name direction)
		    (get direction 'smell))))

(defun landmark-print-smell ()
  (interactive)
  (with-current-buffer "*landmark-smell*"
    (insert "==============================\n")
    (insert (format "tree: %S \n" (get 'z 't)))
    (mapc 'landmark-print-smell-int landmark-directions)))

(defun landmark-print-w0-int (direction)
  (insert (format "%S: w0: %S \n"
		    (symbol-name direction)
		    (get direction 'w0))))

(defun landmark-print-w0 ()
  (interactive)
  (with-current-buffer "*landmark-w0*"
    (insert "==============================\n")
    (mapc 'landmark-print-w0-int landmark-directions)))

(defun landmark-blackbox ()
  (with-current-buffer "*landmark-blackbox*"
    (insert "==============================\n")
    (insert "I smell: ")
    (mapc (lambda (direction)
	       (if (> (get direction 'smell) 0)
		   (insert (format "%S " direction))))
	    landmark-directions)
    (insert "\n")

    (insert "I move: ")
    (mapc (lambda (direction)
	       (if (> (get direction 'y_t) 0)
		   (insert (format "%S " direction))))
	    landmark-directions)
    (insert "\n")
    (landmark-print-wts-blackbox)
    (insert (format "z_t-z_t-1: %S" (- (get 'z 't) (get 'z 't-1))))
    (landmark-print-distance)
    (insert "\n")))

(defun landmark-print-wts-blackbox ()
  (interactive)
  (mapc 'landmark-print-wts-int landmark-directions))

;;;_  - learning parameters
(defcustom landmark-bound 0.005
  "The maximum that w0j may be."
  :type 'number
  :group 'landmark)
(defcustom landmark-c 1.0
  "A factor applied to modulate the increase in wij.
Used in the function landmark-update-normal-weights."
  :type 'number
  :group 'landmark)
(defcustom landmark-c-naught 0.5
  "A factor applied to modulate the increase in w0j.
Used in the function landmark-update-naught-weights."
  :type 'number
  :group 'landmark)
(defvar landmark-initial-w0 0.0)
(defvar landmark-initial-wij 0.0)
(defcustom landmark-no-payoff 0
  "The amount of simulation cycles that have occurred with no movement.
Used to move the robot when he is stuck in a rut for some reason."
  :type 'integer
  :group 'landmark)
(defcustom landmark-max-stall-time 2
  "The maximum number of cycles that the robot can remain stuck in a place.
After this limit is reached, landmark-random-move is called to push him out of it."
  :type 'integer
  :group 'landmark)


;;;_ + Randomizing functions
;;;_  - landmark-flip-a-coin ()
(defun landmark-flip-a-coin ()
  (if (> (random 5000) 2500)
      -1
    1))
;;;_   : landmark-very-small-random-number ()
;(defun landmark-very-small-random-number ()
;  (/
;   (* (/ (random 900000) 900000.0) .0001)))
;;;_   : landmark-randomize-weights-for (direction)
(defun landmark-randomize-weights-for (direction)
  (mapc (lambda (target-direction)
	     (put direction
		  target-direction
		  (* (landmark-flip-a-coin) (/  (random 10000) 10000.0))))
	  landmark-directions))
;;;_   : landmark-noise ()
(defun landmark-noise ()
  (* (- (/ (random 30001) 15000.0) 1) landmark-nvar))

;;;_   : landmark-fix-weights-for (direction)
(defun landmark-fix-weights-for (direction)
  (mapc (lambda (target-direction)
	     (put direction
		  target-direction
		  landmark-initial-wij))
	  landmark-directions))


;;;_ + Plotting functions
;;;_  - landmark-plot-internal (sym)
(defun landmark-plot-internal (sym)
  (landmark-plot-square (landmark-xy-to-index
		   (get sym 'x)
		   (get sym 'y))
		   (get sym 'sym)))
;;;_  - landmark-plot-landmarks ()
(defun landmark-plot-landmarks ()
  (setq landmark-cx (/ landmark-board-width  2))
  (setq landmark-cy (/ landmark-board-height 2))

  (put 'landmark-n    'x landmark-cx)
  (put 'landmark-n    'y 1)
  (put 'landmark-n    'sym 2)

  (put 'landmark-tree 'x landmark-cx)
  (put 'landmark-tree 'y landmark-cy)
  (put 'landmark-tree 'sym 6)

  (put 'landmark-s    'x landmark-cx)
  (put 'landmark-s    'y landmark-board-height)
  (put 'landmark-s    'sym 3)

  (put 'landmark-w    'x 1)
  (put 'landmark-w    'y (/ landmark-board-height 2))
  (put 'landmark-w    'sym 5)

  (put 'landmark-e    'x landmark-board-width)
  (put 'landmark-e    'y (/ landmark-board-height 2))
  (put 'landmark-e    'sym 4)

  (mapc 'landmark-plot-internal '(landmark-n landmark-s landmark-e landmark-w landmark-tree)))



;;;_ + Distance-calculation functions
;;;_  - square (a)
(defun square (a)
  (* a a))

;;;_  - distance (x x0 y y0)
(defun distance (x x0 y y0)
  (sqrt (+ (square (- x x0)) (square (- y y0)))))

;;;_  - calc-distance-of-robot-from (direction)
(defun calc-distance-of-robot-from (direction)
  (put direction 'distance
       (distance (get direction 'x)
		 (landmark-index-to-x (landmark-point-square))
		 (get direction 'y)
		 (landmark-index-to-y (landmark-point-square)))))

;;;_  - calc-smell-internal (sym)
(defun calc-smell-internal (sym)
  (let ((r (get sym 'r))
	(d (calc-distance-of-robot-from sym)))
    (if (> (* 0.5 (- 1 (/ d r))) 0)
	(* 0.5 (- 1 (/ d r)))
      0)))


;;;_ + Learning (neural) functions
(defun landmark-f (x)
  (cond
   ((> x landmark-bound) landmark-bound)
   ((< x 0.0) 0.0)
   (t x)))

(defun landmark-y (direction)
  (put direction 'noise (landmark-noise))
  (put direction 'y_t
       (if (> (get direction 's) 0.0)
           1.0
         0.0)))

(defun landmark-update-normal-weights (direction)
  (mapc (lambda (target-direction)
	     (put direction target-direction
		  (+
		   (get direction target-direction)
		   (* landmark-c
		      (- (get 'z 't) (get 'z 't-1))
		      (get target-direction 'y_t)
		      (get direction 'smell)))))
	  landmark-directions))

(defun landmark-update-naught-weights (direction)
  (mapc (lambda (_target-direction)
	     (put direction 'w0
		  (landmark-f
		   (+
		    (get direction 'w0)
		    (* landmark-c-naught
		       (- (get 'z 't) (get 'z 't-1))
		       (get direction 'y_t))))))
	  landmark-directions))


;;;_ + Statistics gathering and creating functions

(defun landmark-calc-current-smells ()
  (mapc (lambda (direction)
	     (put direction 'smell (calc-smell-internal direction)))
	  landmark-directions))

(defun landmark-calc-payoff ()
  (put 'z 't-1 (get 'z 't))
  (put 'z 't (calc-smell-internal 'landmark-tree))
  (if (= (- (get 'z 't) (get 'z 't-1)) 0.0)
      (incf landmark-no-payoff)
    (setf landmark-no-payoff 0)))

(defun landmark-store-old-y_t ()
  (mapc (lambda (direction)
	     (put direction 'y_t-1 (get direction 'y_t)))
	  landmark-directions))


;;;_ + Functions to move robot

(defun landmark-confidence-for (target-direction)
  (apply '+
	 (get target-direction 'w0)
	 (mapcar (lambda (direction)
		   (*
		    (get direction target-direction)
		    (get direction 'smell)))
		 landmark-directions)))


(defun landmark-calc-confidences ()
  (mapc (lambda (direction)
	     (put direction 's (landmark-confidence-for direction)))
	     landmark-directions))

(defun landmark-move ()
  (if (and (= (get 'landmark-n 'y_t) 1.0) (= (get 'landmark-s 'y_t) 1.0))
      (progn
	(mapc (lambda (dir) (put dir 'y_t 0)) landmark-ns)
	(if landmark-debug
	    (message "n-s normalization."))))
  (if (and (= (get 'landmark-w 'y_t) 1.0) (= (get 'landmark-e 'y_t) 1.0))
      (progn
	(mapc (lambda (dir) (put dir 'y_t 0)) landmark-ew)
	(if landmark-debug
	    (message "e-w normalization"))))

  (mapc (lambda (pair)
	     (if (> (get (car pair) 'y_t) 0)
		 (funcall (car (cdr pair)))))
	  '(
	    (landmark-n landmark-move-up)
	    (landmark-s landmark-move-down)
	    (landmark-e forward-char)
	    (landmark-w backward-char)))
  (landmark-plot-square (landmark-point-square) 1)
  (incf landmark-number-of-moves)
  (if landmark-output-moves
      (message "Moves made: %d" landmark-number-of-moves)))


(defun landmark-random-move ()
  (mapc
   (lambda (direction) (put direction 'y_t 0))
   landmark-directions)
  (dolist (direction (nth (random 8) landmark-8-directions))
    (put direction 'y_t 1.0))
  (landmark-move))

(defun landmark-amble-robot ()
  (interactive)
  (while (> (calc-distance-of-robot-from 'landmark-tree) 0)

    (landmark-store-old-y_t)
    (landmark-calc-current-smells)

    (if (> landmark-no-payoff landmark-max-stall-time)
	(landmark-random-move)
      (progn
	(landmark-calc-confidences)
	(mapc 'landmark-y landmark-directions)
	(landmark-move)))

    (landmark-calc-payoff)

    (mapc 'landmark-update-normal-weights landmark-directions)
    (mapc 'landmark-update-naught-weights landmark-directions)
    (if landmark-debug
	(landmark-weights-debug)))
  (landmark-terminate-game nil))


;;;_  - landmark-start-robot ()
(defun landmark-start-robot ()
  "Signal to the Landmark program that you have played.
You must have put the cursor on the square where you want to play.
If the game is finished, this command requests for another game."
  (interactive)
  (landmark-switch-to-window)
  (cond
   (landmark-emacs-is-computing
    (landmark-crash-game))
   ((not landmark-game-in-progress)
    (landmark-prompt-for-other-game))
   (t
    (let (square)
      (setq square (landmark-point-square))
      (cond ((null square)
	     (error "Your point is not on a square. Retry!"))
	    ((not (zerop (aref landmark-board square)))
	     (error "Your point is not on a free square. Retry!"))
	    (t
	     (progn
	       (landmark-plot-square square 1)

	       (landmark-store-old-y_t)
	       (landmark-calc-current-smells)
	       (put 'z 't (calc-smell-internal 'landmark-tree))

	       (landmark-random-move)

	       (landmark-calc-payoff)

	       (mapc 'landmark-update-normal-weights landmark-directions)
	       (mapc 'landmark-update-naught-weights landmark-directions)
	       (landmark-amble-robot)
	       )))))))


;;;_ + Misc functions
;;;_  - landmark-init (auto-start save-weights)
(defvar landmark-tree-r "")

(defun landmark-init (auto-start save-weights)

  (setq landmark-number-of-moves 0)

  (landmark-plot-landmarks)

  (if landmark-debug
      (save-current-buffer
        (set-buffer (get-buffer-create "*landmark-w0*"))
        (erase-buffer)
        (set-buffer (get-buffer-create "*landmark-moves*"))
        (set-buffer (get-buffer-create "*landmark-wts*"))
        (erase-buffer)
        (set-buffer (get-buffer-create "*landmark-y,s,noise*"))
        (erase-buffer)
        (set-buffer (get-buffer-create "*landmark-smell*"))
        (erase-buffer)
        (set-buffer (get-buffer-create "*landmark-blackbox*"))
        (erase-buffer)
        (set-buffer (get-buffer-create "*landmark-distance*"))
        (erase-buffer)))


  (landmark-set-landmark-signal-strengths)

  (dolist (direction landmark-directions)
    (put direction 'y_t 0.0))

  (if (not save-weights)
      (progn
	(mapc 'landmark-fix-weights-for landmark-directions)
	(dolist (direction landmark-directions)
          (put direction 'w0 landmark-initial-w0)))
    (message "Weights preserved for this run."))

  (if auto-start
      (progn
	(landmark-goto-xy (1+ (random landmark-board-width)) (1+ (random landmark-board-height)))
	(landmark-start-robot))))


;;;_  - something which doesn't work
; no-a-worka!!
;(defun landmark-sum-list (list)
;  (if (> (length list) 0)
;      (+ (car list) (landmark-sum-list (cdr list)))
;    0))
; this a worka!
; (eval  (cons '+ list))
;;;_  - landmark-set-landmark-signal-strengths ()
;;; on a screen higher than wide, I noticed that the robot would amble
;;; left and right and not move forward. examining *landmark-blackbox*
;;; revealed that there was no scent from the north and south
;;; landmarks, hence, they need less factoring down of the effect of
;;; distance on scent.

(defun landmark-set-landmark-signal-strengths ()
  (setq landmark-tree-r (* (sqrt (+ (square landmark-cx) (square landmark-cy))) 1.5))
  (mapc (lambda (direction)
	     (put direction 'r (* landmark-cx 1.1)))
	landmark-ew)
  (mapc (lambda (direction)
	     (put direction 'r (* landmark-cy 1.1)))
	landmark-ns)
  (put 'landmark-tree 'r landmark-tree-r))


;;;_ + landmark-test-run ()

;;;###autoload
(defalias 'landmark-repeat 'landmark-test-run)
;;;###autoload
(defun landmark-test-run ()
  "Run 100 Landmark games, each time saving the weights from the previous game."
  (interactive)
  (landmark 1)
  (dotimes (scratch-var 100)
    (landmark 2)))

;;;###autoload
(defun landmark (parg)
  "Start or resume an Landmark game.
If a game is in progress, this command allows you to resume it.
Here is the relation between prefix args and game options:

prefix arg | robot is auto-started | weights are saved from last game
---------------------------------------------------------------------
none / 1   | yes                   | no
       2   | yes                   | yes
       3   | no                    | yes
       4   | no                    | no

You start by moving to a square and typing \\[landmark-start-robot],
if you did not use a prefix arg to ask for automatic start.
Use \\[describe-mode] for more info."
  (interactive "p")

  (setf landmark-n nil landmark-m nil)
  (landmark-switch-to-window)
  (cond
   (landmark-emacs-is-computing
    (landmark-crash-game))
   ((or (not landmark-game-in-progress)
	(<= landmark-number-of-moves 2))
    (let ((max-width (landmark-max-width))
	  (max-height (landmark-max-height)))
      (or landmark-n (setq landmark-n max-width))
      (or landmark-m (setq landmark-m max-height))
      (cond ((< landmark-n 1)
	     (error "I need at least 1 column"))
	    ((< landmark-m 1)
	     (error "I need at least 1 row"))
	    ((> landmark-n max-width)
	     (error "I cannot display %d columns in that window" landmark-n)))
      (if (and (> landmark-m max-height)
	       (not (eq landmark-m landmark-saved-board-height))
	       ;; Use EQ because SAVED-BOARD-HEIGHT may be nil
	       (not (y-or-n-p (format "Do you really want %d rows? " landmark-m))))
	  (setq landmark-m max-height)))
    (if landmark-one-moment-please
	(message "One moment, please..."))
    (landmark-start-game landmark-n landmark-m)
    (eval (cons 'landmark-init
		(cond
		 ((= parg 1)  '(t nil))
		 ((= parg 2)  '(t t))
		 ((= parg 3)  '(nil t))
		 ((= parg 4)  '(nil nil))
		 (t '(nil t))))))))


;;;_ + Local variables

;;; The following `allout-layout' local variable setting:
;;;  - closes all topics from the first topic to just before the third-to-last,
;;;  - shows the children of the third to last (config vars)
;;;  - and the second to last (code section),
;;;  - and closes the last topic (this local-variables section).
;;;Local variables:
;;;allout-layout: (0 : -1 -1 0)
;;;End:

(random t)

(provide 'landmark)

;;; landmark.el ends here
