;;; mpuz.el --- multiplication puzzle for GNU Emacs

;; Copyright (C) 1990, 2001-2012  Free Software Foundation, Inc.

;; Author: Philippe Schnoebelen <phs@lsv.ens-cachan.fr>
;; Overhauled: Daniel Pfeiffer <occitan@esperanto.org>
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

;; `M-x mpuz' generates a random multiplication puzzle.  This is a
;; multiplication example in which each digit has been consistently replaced
;; with some letter.  Your job is to reconstruct the original digits.  Type
;; `?' while the mode is active for detailed help.

;;; Code:

(defgroup mpuz nil
  "Multiplication puzzle."
  :prefix "mpuz-"
  :group 'games)

(random t)				; randomize

(defcustom mpuz-silent 'error
  "Set this to nil if you want dings on inputs.
The value t means never ding, and `error' means only ding on wrong input."
  :type '(choice (const :tag "No" nil)
		 (const :tag "Yes" t)
		 (const :tag "If correct" error))
  :group 'mpuz)

(defcustom mpuz-solve-when-trivial t
  "Solve any row that can be trivially calculated from what you've found."
  :type 'boolean
  :group 'mpuz)

(defcustom mpuz-allow-double-multiplicator nil
  "Allow 2nd factors like 33 or 77."
  :type 'boolean
  :group 'mpuz)

(defface mpuz-unsolved
  '((((class color)) (:foreground "red1" :bold t))
    (t (:bold t)))
  "Face to use for letters to be solved."
  :group 'mpuz)

(defface mpuz-solved
  '((((class color)) (:foreground "green1" :bold t))
    (t (:bold t)))
  "Face to use for solved digits."
  :group 'mpuz)

(defface mpuz-trivial
  '((((class color)) (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face to use for trivial digits solved for you."
  :group 'mpuz)

(defface mpuz-text
  '((t (:inherit variable-pitch)))
  "Face to use for text on right."
  :group 'mpuz)


;; Mpuz mode and keymaps
;;----------------------
(defcustom mpuz-mode-hook nil
  "Hook to run upon entry to mpuz."
  :type 'hook
  :group 'mpuz)

(defvar mpuz-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (ch)
            (define-key map (char-to-string ch) 'mpuz-try-letter))
          "abcdefghijABCDEFGHIJ")
    (define-key map "\C-g" 'mpuz-offer-abort)
    (define-key map "?" 'describe-mode)
    map)
  "Local keymap to use in Mult Puzzle.")

(defun mpuz-mode ()
  "Multiplication puzzle mode.

You have to guess which letters stand for which digits in the
multiplication displayed inside the `*Mult Puzzle*' buffer.

You may enter a guess for a letter's value by typing first the letter,
then the digit.  Thus, to guess that A=3, type `A 3'.

To leave the game to do other editing work, just switch buffers.
Then you may resume the game with M-x mpuz.
You may abort a game by typing \\<mpuz-mode-map>\\[mpuz-offer-abort]."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'mpuz-mode
	mode-name  "Mult Puzzle"
	tab-width 30)
  (use-local-map mpuz-mode-map)
  (run-mode-hooks 'mpuz-mode-hook))


;; Some variables for statistics
;;------------------------------
(defvar mpuz-nb-errors 0
  "Number of errors made in current game.")

(defvar mpuz-nb-completed-games 0
  "Number of games completed.")

(defvar mpuz-nb-cumulated-errors 0
  "Number of errors made in previous games.")


;; Some variables for game tracking
;;---------------------------------
(defvar mpuz-in-progress nil
  "True if a game is currently in progress.")

(defvar mpuz-found-digits (make-bool-vector 10 nil)
  "A vector recording which digits have been decrypted.")

(defvar mpuz-trivial-digits (make-bool-vector 10 nil)
  "A vector recording which digits have been solved for you.")

(defmacro mpuz-digit-solved-p (digit)
  `(or (aref mpuz-found-digits ,digit)
       (aref mpuz-trivial-digits ,digit)))


;; A puzzle uses a permutation of [0..9] into itself.
;; We use both the permutation and its inverse.
;;---------------------------------------------------
(defvar mpuz-digit-to-letter (make-vector 10 0)
  "A permutation from [0..9] to [0..9].")

(defvar mpuz-letter-to-digit (make-vector 10 0)
  "The inverse of `mpuz-digit-to-letter'.")

(defmacro mpuz-to-digit (letter)
  (list 'aref 'mpuz-letter-to-digit letter))

(defmacro mpuz-to-letter (digit)
  (list 'aref 'mpuz-digit-to-letter digit))

(defun mpuz-build-random-perm ()
  "Initialize puzzle coding with a random permutation."
  (let ((letters (list 0 1 2 3 4 5 6 7 8 9)) ; new cons cells, because of delq
	(index 10)
	elem)
    (while letters
      (setq elem    (nth (random index) letters)
	    letters (delq elem letters)
	    index   (1- index))
      (aset mpuz-digit-to-letter index elem)
      (aset mpuz-letter-to-digit elem index))))


;; A puzzle also uses a board displaying a multiplication.
;; Every digit appears in the board, crypted or not.
;;------------------------------------------------------
(defvar mpuz-board (make-vector 10 nil)
  "The board associates to any digit the list of squares where it appears.")

(defun mpuz-put-number-on-board (number row &rest columns)
  "Put (last digit of) NUMBER on ROW and COLUMNS of the puzzle board."
  (let (digit)
    (dolist (column columns)
      (setq digit (% number 10)
            number (/ number 10))
      (aset mpuz-board digit `((,row . ,column) ,@(aref mpuz-board digit))))))

(defun mpuz-check-all-solved (&optional row col)
  "Check whether all digits have been solved.  Return t if yes."
  (catch 'solved
    (let (A B1 B2 C D E squares)
      (and mpuz-solve-when-trivial
	   (not row)
	   (while
	       (cond ((or (and (setq B1 (or B1 (mpuz-check-all-solved 4 7))
				     B2 (or B2 (mpuz-check-all-solved 4 9))
				     E (or E (mpuz-check-all-solved 10))
				     A (or A (mpuz-check-all-solved 2)))
			       B1 B2)
			  (and E (or A (and B1 B2))))
		      (mpuz-solve)
		      (mpuz-paint-board)
		      (throw 'solved t))
		     ((and (setq D (or D (mpuz-check-all-solved 8))
				 C (or C (mpuz-check-all-solved 6)))
			   D (not E))
		      (mpuz-solve 10))
		     ((and E (not (eq C D)))
		      (mpuz-solve (if D 6 8)))
		     ((and A (not (eq B2 C)))
		      (mpuz-solve (if C 4 6) (if C 9)))
		     ((and A (not (eq B1 D)))
		      (mpuz-solve (if D 4 8) (if D 7)))
		     ((and (not A) (or (and B2 C) (and B1 D)))
		      (mpuz-solve 2)))))
      (mpuz-paint-board)
      (mapc (lambda (digit)
	      (and (not (mpuz-digit-solved-p digit)) ; unsolved
		   (setq squares (aref mpuz-board digit))
		   (if row
		       (if col
			   (member (cons row col) squares)
			 (assq row squares))
		     squares) ; and appearing in the puzzle!
		   (throw 'solved nil)))
	    [0 1 2 3 4 5 6 7 8 9]))
    t))


;; To build a puzzle, we take two random numbers and multiply them.
;; We also take a random permutation for encryption.
;; The random numbers are only use to see which digit appears in which square
;; of the board. Everything is stored in individual squares.
;;---------------------------------------------------------------------------
(defun mpuz-random-puzzle ()
  "Draw random values to be multiplied in a puzzle."
  (mpuz-build-random-perm)
  (fillarray mpuz-board nil)		; erase the board
  ;; A,B,C,D & E, are the five rows of our multiplication.
  ;; Choose random values, discarding cases with leading zeros in C or D.
  (let* ((A (if mpuz-allow-double-multiplicator (+ 112 (random 888))
	      (+ 125 (random 875))))
	 (min (1+ (/ 999 A)))
	 (B1 (+ min (random (- 10 min))))
	 B2 C D E)
    (while (if (= B1 (setq B2 (+ min (random (- 10 min)))))
	       (not mpuz-allow-double-multiplicator)))
    (setq C (* A B2)
	  D (* A B1)
	  E (+ C (* D 10)))
    ;; Individual digits are now put on their respective squares.
    ;; [NB: A square is a pair (row . column) of the screen.]
    (mpuz-put-number-on-board A	2		 9 7 5)
    (mpuz-put-number-on-board (+ (* B1 10) B2) 4 9 7)
    (mpuz-put-number-on-board C	6		 9 7 5 3)
    (mpuz-put-number-on-board D 8		   7 5 3 1)
    (mpuz-put-number-on-board E	10		 9 7 5 3 1)))

;; Display
;;--------
(defconst mpuz-framework
  "
     . . .
	Number of errors (this game):	0
    x  . .
   -------
   . . . .
	Number of completed games:	0
 . . . .
 ---------	Average number of errors:	0.00
 . . . . ."
  "The general picture of the puzzle screen, as a string.")

(defun mpuz-create-buffer ()
  "Create (or recreate) the puzzle buffer.  Return it."
  (let ((buf (get-buffer-create "*Mult Puzzle*"))
	(face '(face mpuz-text))
	buffer-read-only)
    (with-current-buffer buf
      (erase-buffer)
      (insert mpuz-framework)
      (set-text-properties 13 42 face)
      (set-text-properties 79 105 face)
      (set-text-properties 128 153 face)
      (mpuz-paint-board)
      (mpuz-paint-errors)
      (mpuz-paint-statistics))
    buf))

(defun mpuz-paint-number (n &optional eol words)
  (end-of-line eol)
  (let (buffer-read-only)
    (delete-region (point)
		   (progn (backward-word (or words 1)) (point)))
    (insert n)))

(defun mpuz-paint-errors ()
  "Paint error count on the puzzle screen."
  (mpuz-switch-to-window)
  (goto-char (point-min))
  (forward-line 2)
  (mpuz-paint-number (prin1-to-string mpuz-nb-errors)))

(defun mpuz-paint-statistics ()
  "Paint statistics about previous games on the puzzle screen."
  (goto-char (point-min))
  (forward-line 6)
  (mpuz-paint-number (prin1-to-string mpuz-nb-completed-games))
  (mpuz-paint-number
   (format "%.2f"
	   (if (zerop mpuz-nb-completed-games)
	       0
	     (/ (+ 0.0 mpuz-nb-cumulated-errors)
		mpuz-nb-completed-games)))
   3 2))

(defun mpuz-paint-board ()
  "Paint board situation on the puzzle screen."
  (mpuz-switch-to-window)
  (mapc 'mpuz-paint-digit [0 1 2 3 4 5 6 7 8 9])
  (goto-char (point-min)))

(defun mpuz-paint-digit (digit)
  "Paint all occurrences of DIGIT on the puzzle board."
  (let ((char (if (mpuz-digit-solved-p digit)
		  (+ digit ?0)
		(+ (mpuz-to-letter digit) ?A)))
	(face `(face
		,(cond ((aref mpuz-trivial-digits digit) 'mpuz-trivial)
		       ((aref mpuz-found-digits digit) 'mpuz-solved)
		       ('mpuz-unsolved))))
	buffer-read-only)
    (mapc (lambda (square)
	    (goto-char (point-min))
	    (forward-line (1- (car square)))	; line before column!
	    (move-to-column (cdr square))
	    (insert char)
	    (set-text-properties (1- (point)) (point) face)
	    (delete-char 1))
	  (aref mpuz-board digit))))

(defun mpuz-get-buffer ()
  "Get the puzzle buffer if it exists."
  (get-buffer "*Mult Puzzle*"))

(defun mpuz-switch-to-window ()
  "Find or create the Mult-Puzzle buffer, and display it."
  (let ((buf (mpuz-get-buffer)))
    (or buf (setq buf (mpuz-create-buffer)))
    (switch-to-buffer buf)
    (setq buffer-read-only t)
    (mpuz-mode)))


;; Game control
;;-------------
(defun mpuz-start-new-game ()
  "Start a new puzzle."
  (message "Here we go...")
  (setq mpuz-nb-errors 0
	mpuz-in-progress t)
  (fillarray mpuz-found-digits nil)	; initialize mpuz-found-digits
  (fillarray mpuz-trivial-digits nil)
  (mpuz-random-puzzle)
  (mpuz-switch-to-window)
  (mpuz-paint-board)
  (mpuz-paint-errors)
  (mpuz-ask-for-try))

;;;###autoload
(defun mpuz ()
  "Multiplication puzzle with GNU Emacs."
  ;; Main entry point
  (interactive)
  (mpuz-switch-to-window)
  (if mpuz-in-progress
      (mpuz-offer-abort)
    (mpuz-start-new-game)))

(defun mpuz-offer-abort ()
  "Ask if user wants to abort current puzzle."
  (interactive)
  (if (y-or-n-p "Abort game? ")
      (let ((buf (mpuz-get-buffer)))
	(message "Mult Puzzle aborted.")
	(setq mpuz-in-progress nil
	      mpuz-nb-errors 0)
	(fillarray mpuz-board nil)
	(if buf (kill-buffer buf)))
    (mpuz-ask-for-try)))

(defun mpuz-ask-for-try ()
  "Ask for user proposal in puzzle."
  (message "Your try?"))

(defun mpuz-ding (error)
  "Dings, unless global variable `mpuz-silent' forbids it."
  (cond ((eq mpuz-silent t))
	((not mpuz-silent) (ding t))
	(error (ding t))))

(defun mpuz-try-letter ()
  "Propose a digit for a letter in puzzle."
  (interactive)
  (if mpuz-in-progress
      (let (letter-char digit digit-char)
	(setq letter-char (upcase last-command-event)
	      digit (mpuz-to-digit (- letter-char ?A)))
	(cond ((mpuz-digit-solved-p digit)
	       (message "%c already solved." letter-char)
	       (mpuz-ding t))
	      ((null (aref mpuz-board digit))
	       (message "%c does not appear." letter-char)
	       (mpuz-ding t))
	      ((progn (message "%c = " letter-char)
		      ;; <char> has been entered.
		      ;; Print "<char> =" and
		      ;; read <num> or = <num>
		      (setq digit-char (read-char))
		      (if (eq digit-char ?=)
			  (setq digit-char (read-char)))
		      (or (> digit-char ?9) (< digit-char ?0))) ; bad input
	       (message "%c = %c" letter-char digit-char)
	       (mpuz-ding t))
	      (t
	       (mpuz-try-proposal letter-char digit-char))))
    (if (y-or-n-p "Start a new game? ")
	(mpuz-start-new-game)
      (message "OK. I won't."))))

(defun mpuz-try-proposal (letter-char digit-char)
  "Propose LETTER-CHAR as code for DIGIT-CHAR."
  (let* ((letter (- letter-char ?A))
	 (digit (- digit-char ?0))
	 (correct-digit (mpuz-to-digit letter)))
    (cond ((mpuz-digit-solved-p correct-digit)
	   (message "%c has already been found." (+ correct-digit ?0)))
	  ((mpuz-digit-solved-p digit)
	   (message "%c has already been placed." digit-char))
	  ((= digit correct-digit)
	   (message "%c = %c correct!" letter-char digit-char)
	   (mpuz-ding nil)
	   (aset mpuz-found-digits digit t)	; Mark digit as solved
	   (and (mpuz-check-all-solved)
		(mpuz-close-game)))
	  (t ;;; incorrect guess
	   (message "%c = %c incorrect!" letter-char digit-char)
	   (mpuz-ding t)
	   (setq mpuz-nb-errors (1+ mpuz-nb-errors))
	   (mpuz-paint-errors)))))

(defun mpuz-close-game ()
  "Housecleaning when puzzle has been solved."
  (setq mpuz-in-progress nil
	mpuz-nb-cumulated-errors (+ mpuz-nb-cumulated-errors mpuz-nb-errors)
	mpuz-nb-completed-games (1+ mpuz-nb-completed-games))
  (mpuz-paint-statistics)
  (let ((message (format "Puzzle solved with %d error%s. That's %s"
			 mpuz-nb-errors
			 (if (= mpuz-nb-errors 1) "" "s")
			 (cond ((= mpuz-nb-errors 0)	"perfect!")
			       ((= mpuz-nb-errors 1)	"very good!")
			       ((= mpuz-nb-errors 2)	"good.")
			       ((= mpuz-nb-errors 3)	"not bad.")
			       ((= mpuz-nb-errors 4)	"not too bad...")
			       ((< mpuz-nb-errors 10)	"bad!")
			       ((< mpuz-nb-errors 15)	"awful.")
			       (t			"not serious.")))))
    (message "%s" message)
    (sit-for 4)
    (if (y-or-n-p (concat message "  Start a new game? "))
	(mpuz-start-new-game)
      (message "Good Bye!"))))

(defun mpuz-solve (&optional row col)
  "Find solution for autosolving."
  (mapc (lambda (digit)
	  (or (mpuz-digit-solved-p digit)
	      (if row
		  (not (if col
			   (member (cons row col) (aref mpuz-board digit))
			 (assq row (aref mpuz-board digit)))))
	      (aset mpuz-trivial-digits digit t)))
	[0 1 2 3 4 5 6 7 8 9])
  t)

(defun mpuz-show-solution (row)
  "Display solution for debugging purposes."
  (interactive "P")
  (mpuz-switch-to-window)
  (mpuz-solve (if row (* 2 (prefix-numeric-value row))))
  (mpuz-paint-board)
  (if (mpuz-check-all-solved)
      (mpuz-close-game)))

(provide 'mpuz)

;;; mpuz.el ends here
