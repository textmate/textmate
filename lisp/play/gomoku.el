;;; gomoku.el --- Gomoku game between you and Emacs

;; Copyright (C) 1988, 1994, 1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Philippe Schnoebelen <phs@lsv.ens-cachan.fr>
;; Maintainer: FSF
;; Adapted-By: ESR, Daniel Pfeiffer <occitan@esperanto.org>
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

;; RULES:
;;
;; Gomoku is a game played between two players on a rectangular board.  Each
;; player, in turn, marks a free square of its choice. The winner is the first
;; one to mark five contiguous squares in any direction (horizontally,
;; vertically or diagonally).
;;
;; I have been told that, in "The TRUE Gomoku", some restrictions are made
;; about the squares where one may play, or else there is a known forced win
;; for the first player. This program has no such restriction, but it does not
;; know about the forced win, nor do I.
;; See http://renju.nu/r1rulhis.htm for more information.


;; There are two main places where you may want to customize the program: key
;; bindings and board display. These features are commented in the code. Go
;; and see.


;; HOW TO USE:
;;
;; The command "M-x gomoku" displays a
;; board, the size of which depends on the size of the current window. The
;; size of the board is easily modified by giving numeric arguments to the
;; gomoku command and/or by customizing the displaying parameters.
;;
;; Emacs plays when it is its turn. When it is your turn, just put the cursor
;; on the square where you want to play and hit RET, or X, or whatever key you
;; bind to the command gomoku-human-plays. When it is your turn, Emacs is
;; idle: you may switch buffers, read your mail, ... Just come back to the
;; *Gomoku* buffer and resume play.


;; ALGORITHM:
;;
;; The algorithm is briefly described in section "THE SCORE TABLE". Some
;; parameters may be modified if you want to change the style exhibited by the
;; program.

;;; Code:

(defgroup gomoku nil
  "Gomoku game between you and Emacs."
  :prefix "gomoku-"
  :group 'games)
;;;
;;; GOMOKU MODE AND KEYMAP.
;;;
(defcustom gomoku-mode-hook nil
  "If non-nil, its value is called on entry to Gomoku mode.
One useful value to include is `turn-on-font-lock' to highlight the pieces."
  :type 'hook
  :group 'gomoku)

;;;
;;; CONSTANTS FOR BOARD
;;;

(defconst gomoku-buffer-name "*Gomoku*"
  "Name of the Gomoku buffer.")

;; You may change these values if you have a small screen or if the squares
;; look rectangular, but spacings SHOULD be at least 2 (MUST BE at least 1).

(defconst gomoku-square-width 4
  "*Horizontal spacing between squares on the Gomoku board.")

(defconst gomoku-square-height 2
  "*Vertical spacing between squares on the Gomoku board.")

(defconst gomoku-x-offset 3
  "*Number of columns between the Gomoku board and the side of the window.")

(defconst gomoku-y-offset 1
  "*Number of lines between the Gomoku board and the top of the window.")


(defvar gomoku-mode-map
  (let ((map (make-sparse-keymap)))

    ;; Key bindings for cursor motion.
    (define-key map "y" 'gomoku-move-nw)		    ; y
    (define-key map "u" 'gomoku-move-ne)		    ; u
    (define-key map "b" 'gomoku-move-sw)		    ; b
    (define-key map "n" 'gomoku-move-se)		    ; n
    (define-key map "h" 'backward-char)			    ; h
    (define-key map "l" 'forward-char)			    ; l
    (define-key map "j" 'gomoku-move-down)		    ; j
    (define-key map "k" 'gomoku-move-up)		    ; k

    (define-key map [kp-7] 'gomoku-move-nw)
    (define-key map [kp-9] 'gomoku-move-ne)
    (define-key map [kp-1] 'gomoku-move-sw)
    (define-key map [kp-3] 'gomoku-move-se)
    (define-key map [kp-4] 'backward-char)
    (define-key map [kp-6] 'forward-char)
    (define-key map [kp-2] 'gomoku-move-down)
    (define-key map [kp-8] 'gomoku-move-up)

    (define-key map "\C-n" 'gomoku-move-down)		    ; C-n
    (define-key map "\C-p" 'gomoku-move-up)		    ; C-p

    ;; Key bindings for entering Human moves.
    (define-key map "X" 'gomoku-human-plays)		    ; X
    (define-key map "x" 'gomoku-human-plays)		    ; x
    (define-key map " " 'gomoku-human-plays)		    ; SPC
    (define-key map "\C-m" 'gomoku-human-plays)		    ; RET
    (define-key map "\C-c\C-p" 'gomoku-human-plays)	    ; C-c C-p
    (define-key map "\C-c\C-b" 'gomoku-human-takes-back)    ; C-c C-b
    (define-key map "\C-c\C-r" 'gomoku-human-resigns)	    ; C-c C-r
    (define-key map "\C-c\C-e" 'gomoku-emacs-plays)	    ; C-c C-e

    (define-key map [kp-enter] 'gomoku-human-plays)
    (define-key map [insert] 'gomoku-human-plays)
    (define-key map [down-mouse-1] 'gomoku-click)
    (define-key map [drag-mouse-1] 'gomoku-click)
    (define-key map [mouse-1] 'gomoku-click)
    (define-key map [down-mouse-2] 'gomoku-click)
    (define-key map [mouse-2] 'gomoku-mouse-play)
    (define-key map [drag-mouse-2] 'gomoku-mouse-play)

    (define-key map [remap previous-line] 'gomoku-move-up)
    (define-key map [remap next-line] 'gomoku-move-down)
    (define-key map [remap move-beginning-of-line] 'gomoku-beginning-of-line)
    (define-key map [remap move-end-of-line] 'gomoku-end-of-line)
    (define-key map [remap undo] 'gomoku-human-takes-back)
    (define-key map [remap advertised-undo] 'gomoku-human-takes-back)
    map)

  "Local keymap to use in Gomoku mode.")


(defvar gomoku-emacs-won ()
  "For making font-lock use the winner's face for the line.")

(defface gomoku-O
    '((((class color)) (:foreground "red" :weight bold)))
  "Face to use for Emacs's O."
  :group 'gomoku)

(defface gomoku-X
    '((((class color)) (:foreground "green" :weight bold)))
  "Face to use for your X."
  :group 'gomoku)

(defvar gomoku-font-lock-keywords
  '(("O" . 'gomoku-O)
    ("X" . 'gomoku-X)
    ("[-|/\\]" 0 (if gomoku-emacs-won 'gomoku-O 'gomoku-X)))
  "*Font lock rules for Gomoku.")

(put 'gomoku-mode 'front-sticky
     (put 'gomoku-mode 'rear-nonsticky '(intangible)))
(put 'gomoku-mode 'intangible 1)
;; This one is for when they set view-read-only to t: Gomoku cannot
;; allow View Mode to be activated in its buffer.
(put 'gomoku-mode 'mode-class 'special)

(define-derived-mode gomoku-mode nil "Gomoku"
  "Major mode for playing Gomoku against Emacs.
You and Emacs play in turn by marking a free square.  You mark it with X
and Emacs marks it with O.  The winner is the first to get five contiguous
marks horizontally, vertically or in diagonal.
\\<gomoku-mode-map>
You play by moving the cursor over the square you choose and hitting \\[gomoku-human-plays].

Other useful commands:\n
\\{gomoku-mode-map}"
  (gomoku-display-statistics)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gomoku-font-lock-keywords t)
	buffer-read-only t))

;;;
;;; THE BOARD.
;;;

;; The board is a rectangular grid. We code empty squares with 0, X's with 1
;; and O's with 6.  The rectangle is recorded in a one dimensional vector
;; containing padding squares (coded with -1).  These squares allow us to
;; detect when we are trying to move out of the board.  We denote a square by
;; its (X,Y) coords, or by the INDEX corresponding to them in the vector.  The
;; leftmost topmost square has coords (1,1) and index gomoku-board-width + 2.
;; Similarly, vectors between squares may be given by two DX, DY coords or by
;; one DEPL (the difference between indexes).

(defvar gomoku-board-width nil
  "Number of columns on the Gomoku board.")

(defvar gomoku-board-height nil
  "Number of lines on the Gomoku board.")

(defvar gomoku-board nil
  "Vector recording the actual state of the Gomoku board.")

(defvar gomoku-vector-length nil
  "Length of `gomoku-board' vector.")

(defvar gomoku-draw-limit nil
  ;; This is usually set to 70% of the number of squares.
  "After how many moves will Emacs offer a draw?")


(defun gomoku-xy-to-index (x y)
  "Translate X, Y cartesian coords into the corresponding board index."
  (+ (* y gomoku-board-width) x y))

(defun gomoku-index-to-x (index)
  "Return corresponding x-coord of board INDEX."
  (% index (1+ gomoku-board-width)))

(defun gomoku-index-to-y (index)
  "Return corresponding y-coord of board INDEX."
  (/ index (1+ gomoku-board-width)))

(defun gomoku-init-board ()
  "Create the `gomoku-board' vector and fill it with initial values."
  (setq gomoku-board (make-vector gomoku-vector-length 0))
  ;; Every square is 0 (i.e. empty) except padding squares:
  (let ((i 0) (ii (1- gomoku-vector-length)))
    (while (<= i gomoku-board-width)	; The squares in [0..width] and in
      (aset gomoku-board i  -1)		;    [length - width - 1..length - 1]
      (aset gomoku-board ii -1)		;    are padding squares.
      (setq i  (1+ i)
	    ii (1- ii))))
  (let ((i 0))
    (while (< i gomoku-vector-length)
      (aset gomoku-board i -1)		; and also all k*(width+1)
      (setq i (+ i gomoku-board-width 1)))))

;;;
;;; THE SCORE TABLE.
;;;

;; Every (free) square has a score associated to it, recorded in the
;; GOMOKU-SCORE-TABLE vector. The program always plays in the square having
;; the highest score.

(defvar gomoku-score-table nil
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
;; play. It easily extends to "n-dimensional Gomoku", where a win should not
;; be obtained with as few as 5 contiguous marks: 6 or 7 (depending on n !)
;; should be preferred.


;; Here are the scores of the nine "non-polluted" configurations.  Tuning
;; these values will change (hopefully improve) the strength of the program
;; and may change its style (rather aggressive here).

(defconst gomoku-nil-score	  7  "Score of an empty qtuple.")
(defconst gomoku-Xscore	 15  "Score of a qtuple containing one X.")
(defconst gomoku-XXscore	400  "Score of a qtuple containing two X's.")
(defconst gomoku-XXXscore     1800  "Score of a qtuple containing three X's.")
(defconst gomoku-XXXXscore  100000  "Score of a qtuple containing four X's.")
(defconst gomoku-Oscore	 35  "Score of a qtuple containing one O.")
(defconst gomoku-OOscore	800  "Score of a qtuple containing two O's.")
(defconst gomoku-OOOscore    15000  "Score of a qtuple containing three O's.")
(defconst gomoku-OOOOscore  800000  "Score of a qtuple containing four O's.")

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
;;		   6 * gomoku-XXscore > gomoku-XXXscore + gomoku-XXscore
;;
;; because "a" mainly belongs to six "XX" qtuples (the others are less
;; important) while "b" belongs to one "XXX" and one "XX" qtuples.  Other
;; conditions are required to obtain sensible moves, but the previous example
;; should illustrate the point. If you manage to improve on these values,
;; please send me a note. Thanks.


;; As we chose values 0, 1 and 6 to denote empty, X and O squares, the
;; contents of a qtuple are uniquely determined by the sum of its elements and
;; we just have to set up a translation table.

(defconst gomoku-score-trans-table
  (vector gomoku-nil-score gomoku-Xscore gomoku-XXscore gomoku-XXXscore gomoku-XXXXscore 0
	  gomoku-Oscore    0	   0	   0	    0	      0
	  gomoku-OOscore   0	   0	   0	    0	      0
	  gomoku-OOOscore  0	   0	   0	    0	      0
	  gomoku-OOOOscore 0	   0	   0	    0	      0
	  0)
  "Vector associating qtuple contents to their score.")


;; If you do not modify drastically the previous constants, the only way for a
;; square to have a score higher than gomoku-OOOOscore is to belong to a "OOOO"
;; qtuple, thus to be a winning move. Similarly, the only way for a square to
;; have a score between gomoku-XXXXscore and gomoku-OOOOscore is to belong to a "XXXX"
;; qtuple. We may use these considerations to detect when a given move is
;; winning or losing.

(defconst gomoku-winning-threshold gomoku-OOOOscore
  "Threshold score beyond which an Emacs move is winning.")

(defconst gomoku-losing-threshold gomoku-XXXXscore
  "Threshold score beyond which a human move is winning.")


(defun gomoku-strongest-square ()
  "Compute index of free square with highest score, or nil if none."
  ;; We just have to loop other all squares. However there are two problems:
  ;; 1/ The SCORE-TABLE only gives correct scores to free squares. To speed
  ;;	up future searches, we set the score of padding or occupied squares
  ;;	to -1 whenever we meet them.
  ;; 2/ We want to choose randomly between equally good moves.
  (let ((score-max 0)
	(count	   0)			; Number of equally good moves
	(square	   (gomoku-xy-to-index 1 1)) ; First square
	(end	   (gomoku-xy-to-index gomoku-board-width gomoku-board-height))
	best-square score)
    (while (<= square end)
      (cond
       ;; If score is lower (i.e. most of the time), skip to next:
       ((< (aref gomoku-score-table square) score-max))
       ;; If score is better, beware of non free squares:
       ((> (setq score (aref gomoku-score-table square)) score-max)
	(if (zerop (aref gomoku-board square)) ; is it free ?
	    (setq count 1		       ; yes: take it !
		  best-square square
		  score-max   score)
	    (aset gomoku-score-table square -1))) ; no: kill it !
       ;; If score is equally good, choose randomly. But first check freedom:
       ((not (zerop (aref gomoku-board square)))
	(aset gomoku-score-table square -1))
       ((zerop (random (setq count (1+ count))))
	(setq best-square square
	      score-max	  score)))
      (setq square (1+ square)))	; try next square
    best-square))

;;;
;;; INITIALIZING THE SCORE TABLE.
;;;

;; At initialization the board is empty so that every qtuple amounts for
;; gomoku-nil-score. Therefore, the score of any square is gomoku-nil-score times the number
;; of qtuples that pass through it. This number is 3 in a corner and 20 if you
;; are sufficiently far from the sides. As computing the number is time
;; consuming, we initialize every square with 20*gomoku-nil-score and then only
;; consider squares at less than 5 squares from one side. We speed this up by
;; taking symmetry into account.
;; Also, as it is likely that successive games will be played on a board with
;; same size, it is a good idea to save the initial SCORE-TABLE configuration.

(defvar gomoku-saved-score-table nil
  "Recorded initial value of previous score table.")

(defvar gomoku-saved-board-width nil
  "Recorded value of previous board width.")

(defvar gomoku-saved-board-height nil
  "Recorded value of previous board height.")


(defun gomoku-init-score-table ()
  "Create the score table vector and fill it with initial values."
  (if (and gomoku-saved-score-table	; Has it been stored last time ?
	   (= gomoku-board-width  gomoku-saved-board-width)
	   (= gomoku-board-height gomoku-saved-board-height))
      (setq gomoku-score-table (copy-sequence gomoku-saved-score-table))
      ;; No, compute it:
      (setq gomoku-score-table
	    (make-vector gomoku-vector-length (* 20 gomoku-nil-score)))
      (let (i j maxi maxj maxi2 maxj2)
	(setq maxi  (/ (1+ gomoku-board-width) 2)
	      maxj  (/ (1+ gomoku-board-height) 2)
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
	    (gomoku-init-square-score i j)
	    (setq j (1+ j)))
	  (setq i (1+ i)))
	(while (<= i maxi)
	  (setq j 1)
	  (while (<= j maxj2)
	    (gomoku-init-square-score i j)
	    (setq j (1+ j)))
	  (setq i (1+ i))))
      (setq gomoku-saved-score-table  (copy-sequence gomoku-score-table)
	    gomoku-saved-board-width  gomoku-board-width
	    gomoku-saved-board-height gomoku-board-height)))

(defun gomoku-nb-qtuples (i j)
  "Return the number of qtuples containing square I,J."
  ;; This function is complicated because we have to deal
  ;; with ugly cases like 3 by 6 boards, but it works.
  ;; If you have a simpler (and correct) solution, send it to me. Thanks !
  (let ((left  (min 4 (1- i)))
	(right (min 4 (- gomoku-board-width i)))
	(up    (min 4 (1- j)))
	(down  (min 4 (- gomoku-board-height j))))
    (+ -12
       (min (max (+ left right) 3) 8)
       (min (max (+ up down) 3) 8)
       (min (max (+ (min left up) (min right down)) 3) 8)
       (min (max (+ (min right up) (min left down)) 3) 8))))

(defun gomoku-init-square-score (i j)
  "Give initial score to square I,J and to its mirror images."
  (let ((ii (1+ (- gomoku-board-width i)))
	(jj (1+ (- gomoku-board-height j)))
	(sc (* (gomoku-nb-qtuples i j) (aref gomoku-score-trans-table 0))))
    (aset gomoku-score-table (gomoku-xy-to-index i  j)	sc)
    (aset gomoku-score-table (gomoku-xy-to-index ii j)	sc)
    (aset gomoku-score-table (gomoku-xy-to-index i  jj) sc)
    (aset gomoku-score-table (gomoku-xy-to-index ii jj) sc)))

;;;
;;; MAINTAINING THE SCORE TABLE.
;;;

;; We do not provide functions for computing the SCORE-TABLE given the
;; contents of the BOARD. This would involve heavy nested loops, with time
;; proportional to the size of the board. It is better to update the
;; SCORE-TABLE after each move. Updating needs not modify more than 36
;; squares: it is done in constant time.

(defun gomoku-update-score-table (square dval)
  "Update score table after SQUARE received a DVAL increment."
  ;; The board has already been updated when this function is called.
  ;; Updating scores is done by looking for qtuples boundaries in all four
  ;; directions and then calling update-score-in-direction.
  ;; Finally all squares received the right increment, and then are up to
  ;; date, except possibly for SQUARE itself if we are taking a move back for
  ;; its score had been set to -1 at the time.
  (let* ((x    (gomoku-index-to-x square))
	 (y    (gomoku-index-to-y square))
	 (imin (max -4 (- 1 x)))
	 (jmin (max -4 (- 1 y)))
	 (imax (min 0 (- gomoku-board-width x 4)))
	 (jmax (min 0 (- gomoku-board-height y 4))))
    (gomoku-update-score-in-direction imin imax
				      square 1 0 dval)
    (gomoku-update-score-in-direction jmin jmax
				      square 0 1 dval)
    (gomoku-update-score-in-direction (max imin jmin) (min imax jmax)
				      square 1 1 dval)
    (gomoku-update-score-in-direction (max (- 1 y) -4
					   (- x gomoku-board-width))
				      (min 0 (- x 5)
					   (- gomoku-board-height y 4))
				      square -1 1 dval)))

(defun gomoku-update-score-in-direction (left right square dx dy dval)
  "Update scores for all squares in the qtuples starting between the LEFTth
square and the RIGHTth after SQUARE, along the DX, DY direction, considering
that DVAL has been added on SQUARE."
  ;; We always have LEFT <= 0, RIGHT <= 0 and DEPL > 0 but we may very well
  ;; have LEFT > RIGHT, indicating that no qtuple contains SQUARE along that
  ;; DX,DY direction.
  (cond
   ((> left right))			; Quit
   (t					; Else ..
    (let (depl square0 square1 square2 count delta)
      (setq depl    (gomoku-xy-to-index dx dy)
	    square0 (+ square (* left depl))
	    square1 (+ square (* right depl))
	    square2 (+ square0 (* 4 depl)))
      ;; Compute the contents of the first qtuple:
      (setq square square0
	    count  0)
      (while (<= square square2)
	(setq count  (+ count (aref gomoku-board square))
	      square (+ square depl)))
      (while (<= square0 square1)
	;; Update the squares of the qtuple beginning in SQUARE0 and ending
	;; in SQUARE2.
	(setq delta (- (aref gomoku-score-trans-table count)
		       (aref gomoku-score-trans-table (- count dval))))
	(cond ((not (zerop delta))	; or else nothing to update
	       (setq square square0)
	       (while (<= square square2)
		 (if (zerop (aref gomoku-board square)) ; only for free squares
		     (aset gomoku-score-table square
			   (+ (aref gomoku-score-table square) delta)))
		 (setq square (+ square depl)))))
	;; Then shift the qtuple one square along DEPL, this only requires
	;; modifying SQUARE0 and SQUARE2.
	(setq square2 (+ square2 depl)
	      count   (+ count (- (aref gomoku-board square0))
			 (aref gomoku-board square2))
	      square0 (+ square0 depl)))))))

;;;
;;; GAME CONTROL.
;;;

;; Several variables are used to monitor a game, including a GAME-HISTORY (the
;; list of all (SQUARE . PREVSCORE) played) that allows to take moves back
;; (anti-updating the score table) and to compute the table from scratch in
;; case of an interruption.

(defvar gomoku-game-in-progress nil
  "Non-nil if a game is in progress.")

(defvar gomoku-game-history nil
  "A record of all moves that have been played during current game.")

(defvar gomoku-number-of-moves nil
  "Number of moves already played in current game.")

(defvar gomoku-number-of-human-moves nil
  "Number of moves already played by human in current game.")

(defvar gomoku-emacs-played-first nil
  "Non-nil if Emacs played first.")

(defvar gomoku-human-took-back nil
  "Non-nil if Human took back a move during the game.")

(defvar gomoku-human-refused-draw nil
  "Non-nil if Human refused Emacs offer of a draw.")

(defvar gomoku-emacs-is-computing nil
  ;; This is used to detect interruptions. Hopefully, it should not be needed.
  "Non-nil if Emacs is in the middle of a computation.")


(defun gomoku-start-game (n m)
  "Initialize a new game on an N by M board."
  (setq gomoku-emacs-is-computing t)	; Raise flag
  (setq gomoku-game-in-progress t)
  (setq gomoku-board-width   n
	gomoku-board-height  m
	gomoku-vector-length (1+ (* (+ m 2) (1+ n)))
	gomoku-draw-limit    (/ (* 7 n m) 10))
  (setq gomoku-emacs-won	     nil
	gomoku-game-history	     nil
	gomoku-number-of-moves	     0
	gomoku-number-of-human-moves 0
	gomoku-emacs-played-first    nil
	gomoku-human-took-back	     nil
	gomoku-human-refused-draw    nil)
  (gomoku-init-display n m)		; Display first: the rest takes time
  (gomoku-init-score-table)		; INIT-BOARD requires that the score
  (gomoku-init-board)			;   table be already created.
  (setq gomoku-emacs-is-computing nil))

(defun gomoku-play-move (square val &optional dont-update-score)
  "Go to SQUARE, play VAL and update everything."
  (setq gomoku-emacs-is-computing t)	; Raise flag
  (cond ((= 1 val)			; a Human move
	 (setq gomoku-number-of-human-moves (1+ gomoku-number-of-human-moves)))
	((zerop gomoku-number-of-moves)	; an Emacs move. Is it first ?
	 (setq gomoku-emacs-played-first t)))
  (setq gomoku-game-history
	(cons (cons square (aref gomoku-score-table square))
	      gomoku-game-history)
	gomoku-number-of-moves (1+ gomoku-number-of-moves))
  (gomoku-plot-square square val)
  (aset gomoku-board square val)	; *BEFORE* UPDATE-SCORE !
  (if dont-update-score nil
      (gomoku-update-score-table square val) ; previous val was 0: dval = val
      (aset gomoku-score-table square -1))
  (setq gomoku-emacs-is-computing nil))

(defun gomoku-take-back ()
  "Take back last move and update everything."
  (setq gomoku-emacs-is-computing t)
  (let* ((last-move (car gomoku-game-history))
	 (square (car last-move))
	 (oldval (aref gomoku-board square)))
    (if (= 1 oldval)
	(setq gomoku-number-of-human-moves (1- gomoku-number-of-human-moves)))
    (setq gomoku-game-history	 (cdr gomoku-game-history)
	  gomoku-number-of-moves (1- gomoku-number-of-moves))
    (gomoku-plot-square square 0)
    (aset gomoku-board square 0)	; *BEFORE* UPDATE-SCORE !
    (gomoku-update-score-table square (- oldval))
    (aset gomoku-score-table square (cdr last-move)))
  (setq gomoku-emacs-is-computing nil))

;;;
;;; SESSION CONTROL.
;;;

(defvar gomoku-number-of-emacs-wins 0
  "Number of games Emacs won in this session.")

(defvar gomoku-number-of-human-wins 0
  "Number of games you won in this session.")

(defvar gomoku-number-of-draws 0
  "Number of games already drawn in this session.")


(defun gomoku-terminate-game (result)
  "Terminate the current game with RESULT."
  (message
   (cond
    ((eq result 'emacs-won)
     (setq gomoku-number-of-emacs-wins (1+ gomoku-number-of-emacs-wins))
     (cond ((< gomoku-number-of-moves 20)
	    "This was a REALLY QUICK win.")
	   (gomoku-human-refused-draw
	    "I won...  Too bad you refused my offer of a draw!")
	   (gomoku-human-took-back
	    "I won...  Taking moves back will not help you!")
	   ((not gomoku-emacs-played-first)
	    "I won...  Playing first did not help you much!")
	   ((and (zerop gomoku-number-of-human-wins)
		 (zerop gomoku-number-of-draws)
		 (> gomoku-number-of-emacs-wins 1))
	    "I'm becoming tired of winning...")
	   ("I won.")))
    ((eq result 'human-won)
     (setq gomoku-number-of-human-wins (1+ gomoku-number-of-human-wins))
     (concat "OK, you won this one."
	     (cond
	      (gomoku-human-took-back
	       "  I, for one, never take my moves back...")
	      (gomoku-emacs-played-first
	       ".. so what?")
	      ("  Now, let me play first just once."))))
    ((eq result 'human-resigned)
     (setq gomoku-number-of-emacs-wins (1+ gomoku-number-of-emacs-wins))
     "So you resign.  That's just one more win for me.")
    ((eq result 'nobody-won)
     (setq gomoku-number-of-draws (1+ gomoku-number-of-draws))
     (concat "This is a draw.  "
	     (cond
	      (gomoku-human-took-back
	       "I, for one, never take my moves back...")
	      (gomoku-emacs-played-first
	       "Just chance, I guess.")
	      ("Now, let me play first just once."))))
    ((eq result 'draw-agreed)
     (setq gomoku-number-of-draws (1+ gomoku-number-of-draws))
     (concat "Draw agreed.  "
	     (cond
	      (gomoku-human-took-back
	       "I, for one, never take my moves back...")
	      (gomoku-emacs-played-first
	       "You were lucky.")
	      ("Now, let me play first just once."))))
    ((eq result 'crash-game)
     "Sorry, I have been interrupted and cannot resume that game...")))
  (gomoku-display-statistics)
  ;;(ding)
  (setq gomoku-game-in-progress nil))

(defun gomoku-crash-game ()
  "What to do when Emacs detects it has been interrupted."
  (setq gomoku-emacs-is-computing nil)
  (gomoku-terminate-game 'crash-game)
  (sit-for 4)				; Let's see the message
  (gomoku-prompt-for-other-game))

;;;
;;; INTERACTIVE COMMANDS.
;;;

;;;###autoload
(defun gomoku (&optional n m)
  "Start a Gomoku game between you and Emacs.

If a game is in progress, this command allows you to resume it.
If optional arguments N and M are given, an N by M board is used.
If prefix arg is given for N, M is prompted for.

You and Emacs play in turn by marking a free square.  You mark it with X
and Emacs marks it with O.  The winner is the first to get five contiguous
marks horizontally, vertically or in diagonal.

You play by moving the cursor over the square you choose and hitting
\\<gomoku-mode-map>\\[gomoku-human-plays].

This program actually plays a simplified or archaic version of the
Gomoku game, and ought to be upgraded to use the full modern rules.

Use \\[describe-mode] for more info."
  (interactive (if current-prefix-arg
		   (list (prefix-numeric-value current-prefix-arg)
			 (eval (read-minibuffer "Height: ")))))
  ;; gomoku-switch-to-window, but without the potential call to gomoku
  ;; from gomoku-prompt-for-other-game.
  (if (get-buffer gomoku-buffer-name)
      (switch-to-buffer gomoku-buffer-name)
    (when gomoku-game-in-progress
      (setq gomoku-emacs-is-computing nil)
      (gomoku-terminate-game 'crash-game)
      (sit-for 4)
      (or (y-or-n-p "Another game? ") (error "Chicken!")))
    (switch-to-buffer gomoku-buffer-name)
    (gomoku-mode))
  (cond
   (gomoku-emacs-is-computing
    (gomoku-crash-game))
   ((or (not gomoku-game-in-progress)
	(<= gomoku-number-of-moves 2))
    (let ((max-width (gomoku-max-width))
	  (max-height (gomoku-max-height)))
      (or n (setq n max-width))
      (or m (setq m max-height))
      (cond ((< n 1)
	     (error "I need at least 1 column"))
	    ((< m 1)
	     (error "I need at least 1 row"))
	    ((> n max-width)
	     (error "I cannot display %d columns in that window" n)))
      (if (and (> m max-height)
	       (not (eq m gomoku-saved-board-height))
	       ;; Use EQ because SAVED-BOARD-HEIGHT may be nil
	       (not (y-or-n-p (format "Do you really want %d rows? " m))))
	  (setq m max-height)))
    (message "One moment, please...")
    (gomoku-start-game n m)
    (if (y-or-n-p "Do you allow me to play first? ")
	(gomoku-emacs-plays)
	(gomoku-prompt-for-move)))
   ((y-or-n-p "Shall we continue our game? ")
    (gomoku-prompt-for-move))
   (t
    (gomoku-human-resigns))))

(defun gomoku-emacs-plays ()
  "Compute Emacs next move and play it."
  (interactive)
  (gomoku-switch-to-window)
  (cond
   (gomoku-emacs-is-computing
    (gomoku-crash-game))
   ((not gomoku-game-in-progress)
    (gomoku-prompt-for-other-game))
   (t
    (message "Let me think...")
    (let (square score)
      (setq square (gomoku-strongest-square))
      (cond ((null square)
	     (gomoku-terminate-game 'nobody-won))
	    (t
	     (setq score (aref gomoku-score-table square))
	     (gomoku-play-move square 6)
	     (cond ((>= score gomoku-winning-threshold)
		    (setq gomoku-emacs-won t) ; for font-lock
		    (gomoku-find-filled-qtuple square 6)
		    (gomoku-terminate-game 'emacs-won))
		   ((zerop score)
		    (gomoku-terminate-game 'nobody-won))
		   ((and (> gomoku-number-of-moves gomoku-draw-limit)
			 (not gomoku-human-refused-draw)
			 (gomoku-offer-a-draw))
		    (gomoku-terminate-game 'draw-agreed))
		   (t
		    (gomoku-prompt-for-move)))))))))

;; For small square dimensions this is approximate, since though measured in
;; pixels, event's (X . Y) is a character's top-left corner.
(defun gomoku-click (click)
  "Position at the square where you click."
  (interactive "e")
  (and (windowp (posn-window (setq click (event-end click))))
       (numberp (posn-point click))
       (select-window (posn-window click))
       (setq click (posn-col-row click))
       (gomoku-goto-xy
	(min (max (/ (+ (- (car click)
			   gomoku-x-offset
			   1)
			(window-hscroll)
			gomoku-square-width
			(% gomoku-square-width 2)
			(/ gomoku-square-width 2))
		     gomoku-square-width)
		  1)
	     gomoku-board-width)
	(min (max (/ (+ (- (cdr click)
			   gomoku-y-offset
			   1)
			(let ((inhibit-point-motion-hooks t))
			  (count-lines 1 (window-start)))
			gomoku-square-height
			(% gomoku-square-height 2)
			(/ gomoku-square-height 2))
		     gomoku-square-height)
		  1)
	     gomoku-board-height))))

(defun gomoku-mouse-play (click)
  "Play at the square where you click."
  (interactive "e")
  (if (gomoku-click click)
      (gomoku-human-plays)))

(defun gomoku-human-plays ()
  "Signal to the Gomoku program that you have played.
You must have put the cursor on the square where you want to play.
If the game is finished, this command requests for another game."
  (interactive)
  (gomoku-switch-to-window)
  (cond
   (gomoku-emacs-is-computing
    (gomoku-crash-game))
   ((not gomoku-game-in-progress)
    (gomoku-prompt-for-other-game))
   (t
    (let (square score)
      (setq square (gomoku-point-square))
      (cond ((null square)
	     (error "Your point is not on a square.  Retry!"))
	    ((not (zerop (aref gomoku-board square)))
	     (error "Your point is not on a free square.  Retry!"))
	    (t
	     (setq score (aref gomoku-score-table square))
	     (gomoku-play-move square 1)
	     (cond ((and (>= score gomoku-losing-threshold)
			 ;; Just testing SCORE > THRESHOLD is not enough for
			 ;; detecting wins, it just gives an indication that
			 ;; we confirm with GOMOKU-FIND-FILLED-QTUPLE.
			 (gomoku-find-filled-qtuple square 1))
		    (gomoku-terminate-game 'human-won))
		   (t
		    (gomoku-emacs-plays)))))))))

(defun gomoku-human-takes-back ()
  "Signal to the Gomoku program that you wish to take back your last move."
  (interactive)
  (gomoku-switch-to-window)
  (cond
   (gomoku-emacs-is-computing
    (gomoku-crash-game))
   ((not gomoku-game-in-progress)
    (message "Too late for taking back...")
    (sit-for 4)
    (gomoku-prompt-for-other-game))
   ((zerop gomoku-number-of-human-moves)
    (message "You have not played yet...  Your move?"))
   (t
    (message "One moment, please...")
    ;; It is possible for the user to let Emacs play several consecutive
    ;; moves, so that the best way to know when to stop taking back moves is
    ;; to count the number of human moves:
    (setq gomoku-human-took-back t)
    (let ((number gomoku-number-of-human-moves))
      (while (= number gomoku-number-of-human-moves)
	(gomoku-take-back)))
    (gomoku-prompt-for-move))))

(defun gomoku-human-resigns ()
  "Signal to the Gomoku program that you may want to resign."
  (interactive)
  (gomoku-switch-to-window)
  (cond
   (gomoku-emacs-is-computing
    (gomoku-crash-game))
   ((not gomoku-game-in-progress)
    (message "There is no game in progress"))
   ((y-or-n-p "You mean, you resign? ")
    (gomoku-terminate-game 'human-resigned))
   ((y-or-n-p "You mean, we continue? ")
    (gomoku-prompt-for-move))
   (t
    (gomoku-terminate-game 'human-resigned)))) ; OK. Accept it

;;;
;;; PROMPTING THE HUMAN PLAYER.
;;;

(defun gomoku-prompt-for-move ()
  "Display a message asking for Human's move."
  (message (if (zerop gomoku-number-of-human-moves)
	       "Your move?  (Move to a free square and hit X, RET ...)"
	       "Your move?")))

(defun gomoku-prompt-for-other-game ()
  "Ask for another game, and start it."
  (if (y-or-n-p "Another game? ")
      (gomoku gomoku-board-width gomoku-board-height)
    (error "Chicken!")))

(defun gomoku-offer-a-draw ()
  "Offer a draw and return t if Human accepted it."
  (or (y-or-n-p "I offer you a draw.  Do you accept it? ")
      (not (setq gomoku-human-refused-draw t))))

;;;
;;; DISPLAYING THE BOARD.
;;;

(defun gomoku-max-width ()
  "Largest possible board width for the current window."
  (1+ (/ (- (window-width (selected-window))
	    gomoku-x-offset gomoku-x-offset 1)
	 gomoku-square-width)))

(defun gomoku-max-height ()
  "Largest possible board height for the current window."
  (1+ (/ (- (window-height (selected-window))
	    gomoku-y-offset gomoku-y-offset 2)
	 ;; 2 instead of 1 because WINDOW-HEIGHT includes the mode line !
	 gomoku-square-height)))

(defun gomoku-point-y ()
  "Return the board row where point is."
  (let ((inhibit-point-motion-hooks t))
    (1+ (/ (- (count-lines 1 (point)) gomoku-y-offset (if (bolp) 0 1))
	   gomoku-square-height))))

(defun gomoku-point-square ()
  "Return the index of the square point is on."
  (let ((inhibit-point-motion-hooks t))
    (gomoku-xy-to-index (1+ (/ (- (current-column) gomoku-x-offset)
			       gomoku-square-width))
			(gomoku-point-y))))

(defun gomoku-goto-square (index)
  "Move point to square number INDEX."
  (gomoku-goto-xy (gomoku-index-to-x index) (gomoku-index-to-y index)))

(defun gomoku-goto-xy (x y)
  "Move point to square at X, Y coords."
  (let ((inhibit-point-motion-hooks t))
    (goto-char (point-min))
    (forward-line (+ gomoku-y-offset (* gomoku-square-height (1- y)))))
  (move-to-column (+ gomoku-x-offset (* gomoku-square-width (1- x)))))

(defun gomoku-plot-square (square value)
  "Draw 'X', 'O' or '.' on SQUARE depending on VALUE, leave point there."
  (or (= value 1)
      (gomoku-goto-square square))
  (let ((inhibit-read-only t)
	(inhibit-point-motion-hooks t))
    (insert-and-inherit (cond ((= value 1) ?X)
			      ((= value 6) ?O)
			      (?.)))
    (and (zerop value)
	 (add-text-properties
	  (1- (point)) (point)
	  '(mouse-face highlight help-echo "mouse-2: play at this square")))
    (delete-char 1)
    (backward-char 1))
  (sit-for 0))	; Display NOW

(defun gomoku-init-display (n m)
  "Display an N by M Gomoku board."
  (buffer-disable-undo (current-buffer))
  (let ((inhibit-read-only t)
	(point 1) opoint
	(intangible t)
	(i m) j x)
    ;; Try to minimize number of chars (because of text properties)
    (setq tab-width
	  (if (zerop (% gomoku-x-offset gomoku-square-width))
	      gomoku-square-width
	    (max (/ (+ (% gomoku-x-offset gomoku-square-width)
		       gomoku-square-width 1) 2) 2)))
    (erase-buffer)
    (newline gomoku-y-offset)
    (while (progn
	     (setq j n
		   x (- gomoku-x-offset gomoku-square-width))
	     (while (>= (setq j (1- j)) 0)
	       (insert-char ?\t (/ (- (setq x (+ x gomoku-square-width))
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
	       (insert ?.)
	       (add-text-properties
		point (point)
		'(mouse-face highlight
		  help-echo "mouse-2: play at this square")))
	     (> (setq i (1- i)) 0))
      (if (= i (1- m))
	  (setq opoint point))
      (insert-char ?\n gomoku-square-height))
    (or (eq (char-after 1) ?.)
	(put-text-property 1 2 'point-entered
			   (lambda (_x _y) (if (bobp) (forward-char)))))
    (or intangible
	(put-text-property point (point) 'intangible 2))
    (put-text-property point (point) 'point-entered
		       (lambda (_x _y) (if (eobp) (backward-char))))
    (put-text-property (point-min) (point) 'category 'gomoku-mode))
  (gomoku-goto-xy (/ (1+ n) 2) (/ (1+ m) 2)) ; center of the board
  (sit-for 0))				; Display NOW

(defun gomoku-display-statistics ()
  "Obnoxiously display some statistics about previous games in mode line."
  ;; We store this string in the mode-line-process local variable.
  ;; This is certainly not the cleanest way out ...
  (setq mode-line-process
	(format ": Won %d, lost %d%s"
		gomoku-number-of-human-wins
		gomoku-number-of-emacs-wins
		(if (zerop gomoku-number-of-draws)
		    ""
		  (format ", drew %d" gomoku-number-of-draws))))
  (force-mode-line-update))

(defun gomoku-switch-to-window ()
  "Find or create the Gomoku buffer, and display it."
  (interactive)
  (if (get-buffer gomoku-buffer-name)       ; Buffer exists:
      (switch-to-buffer gomoku-buffer-name) ;   no problem.
    (if gomoku-game-in-progress
        (gomoku-crash-game))            ;   buffer has been killed or something
    (switch-to-buffer gomoku-buffer-name)   ; Anyway, start anew.
    (gomoku-mode)))

;;;
;;; CROSSING WINNING QTUPLES.
;;;

;; When someone succeeds in filling a qtuple, we draw a line over the five
;; corresponding squares. One problem is that the program does not know which
;; squares ! It only knows the square where the last move has been played and
;; who won. The solution is to scan the board along all four directions.

(defun gomoku-find-filled-qtuple (square value)
  "Return t if SQUARE belongs to a qtuple filled with VALUEs."
  (or (gomoku-check-filled-qtuple square value 1 0)
      (gomoku-check-filled-qtuple square value 0 1)
      (gomoku-check-filled-qtuple square value 1 1)
      (gomoku-check-filled-qtuple square value -1 1)))

(defun gomoku-check-filled-qtuple (square value dx dy)
  "Return t if SQUARE belongs to a qtuple filled  with VALUEs along DX, DY."
  (let ((a 0) (b 0)
	(left square) (right square)
	(depl (gomoku-xy-to-index dx dy)))
    (while (and (> a -4)		; stretch tuple left
		(= value (aref gomoku-board (setq left (- left depl)))))
      (setq a (1- a)))
    (while (and (< b (+ a 4))		; stretch tuple right
		(= value (aref gomoku-board (setq right (+ right depl)))))
      (setq b (1+ b)))
    (cond ((= b (+ a 4))		; tuple length = 5 ?
	   (gomoku-cross-qtuple (+ square (* a depl)) (+ square (* b depl))
				dx dy)
	   t))))

(defun gomoku-cross-qtuple (square1 square2 dx dy)
  "Cross every square between SQUARE1 and SQUARE2 in the DX, DY direction."
  (save-excursion			; Not moving point from last square
    (let ((depl (gomoku-xy-to-index dx dy))
	  (inhibit-read-only t)
	  (inhibit-point-motion-hooks t))
      ;; WARNING: this function assumes DEPL > 0 and SQUARE2 > SQUARE1
      (while (/= square1 square2)
	(gomoku-goto-square square1)
	(setq square1 (+ square1 depl))
	(cond
	  ((= dy 0)			; Horizontal
	   (forward-char 1)
	   (insert-char ?- (1- gomoku-square-width) t)
	   (delete-region (point) (progn
				    (skip-chars-forward " \t")
				    (point))))
	  ((= dx 0)			; Vertical
	   (let ((n 1)
		 (column (current-column)))
	     (while (< n gomoku-square-height)
	       (setq n (1+ n))
	       (forward-line 1)
	       (indent-to column)
	       (insert-and-inherit ?|))))
	  ((= dx -1)			; 1st Diagonal
	   (indent-to (prog1 (- (current-column) (/ gomoku-square-width 2))
			(forward-line (/ gomoku-square-height 2))))
	   (insert-and-inherit ?/))
	  (t				; 2nd Diagonal
	   (indent-to (prog1 (+ (current-column) (/ gomoku-square-width 2))
			(forward-line (/ gomoku-square-height 2))))
	   (insert-and-inherit ?\\))))))
  (sit-for 0))				; Display NOW

;;;
;;; CURSOR MOTION.
;;;
;; previous-line and next-line don't work right with intangible newlines
(defun gomoku-move-down ()
  "Move point down one row on the Gomoku board."
  (interactive)
  (if (< (gomoku-point-y) gomoku-board-height)
      (let ((column (current-column)))
	(forward-line gomoku-square-height)
	(move-to-column column))))

(defun gomoku-move-up ()
  "Move point up one row on the Gomoku board."
  (interactive)
  (if (> (gomoku-point-y) 1)
      (let ((column (current-column)))
	(forward-line (- 1 gomoku-square-height))
	(move-to-column column))))

(defun gomoku-move-ne ()
  "Move point North East on the Gomoku board."
  (interactive)
  (gomoku-move-up)
  (forward-char))

(defun gomoku-move-se ()
  "Move point South East on the Gomoku board."
  (interactive)
  (gomoku-move-down)
  (forward-char))

(defun gomoku-move-nw ()
  "Move point North West on the Gomoku board."
  (interactive)
  (gomoku-move-up)
  (backward-char))

(defun gomoku-move-sw ()
  "Move point South West on the Gomoku board."
  (interactive)
  (gomoku-move-down)
  (backward-char))

(defun gomoku-beginning-of-line ()
  "Move point to first square on the Gomoku board row."
  (interactive)
  (move-to-column gomoku-x-offset))

(defun gomoku-end-of-line ()
  "Move point to last square on the Gomoku board row."
  (interactive)
  (move-to-column (+ gomoku-x-offset
		     (* gomoku-square-width (1- gomoku-board-width)))))

(random t)

(provide 'gomoku)

;;; gomoku.el ends here
