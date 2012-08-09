;;; hanoi.el --- towers of hanoi in Emacs

;; Author: Damon Anton Permezel
;; Maintainer: FSF
;; Keywords: games

; Author (a) 1985, Damon Anton Permezel
; This is in the public domain
; since he distributed it in 1985 without copyright notice.
;; This file is part of GNU Emacs.
;
; Support for horizontal poles, large numbers of rings, real-time,
; faces, defcustom, and Towers of Unix added in 1999 by Alakazam
; Petrofsky <Alakazam@Petrofsky.Berkeley.CA.US>.

;;; Commentary:

;; Solves the Towers of Hanoi puzzle while-U-wait.
;;
;; The puzzle: Start with N rings, decreasing in sizes from bottom to
;; top, stacked around a post.  There are two other posts.  Your mission,
;; should you choose to accept it, is to shift the pile, stacked in its
;; original order, to another post.
;;
;; The challenge is to do it in the fewest possible moves.  Each move
;; shifts one ring to a different post.  But there's a rule; you can
;; only stack a ring on top of a larger one.
;;
;; The simplest nontrivial version of this puzzle is N = 3.  Solution
;; time rises as 2**N, and programs to solve it have long been considered
;; classic introductory exercises in the use of recursion.
;;
;; The puzzle is called `Towers of Hanoi' because an early popular
;; presentation wove a fanciful legend around it.  According to this
;; myth (uttered long before the Vietnam War), there is a Buddhist
;; monastery at Hanoi which contains a large room with three time-worn
;; posts in it surrounded by 21 golden discs.  Monks, acting out the
;; command of an ancient prophecy, have been moving these disks, in
;; accordance with the rules of the puzzle, once every day since the
;; monastery was founded over a thousand years ago.  They are said to
;; believe that when the last move of the puzzle is completed, the
;; world will end in a clap of thunder.  Fortunately, they are nowhere
;; even close to being done...
;;
;; 1999 addition: The `Towers of Unix' command (hanoi-unix) stems from
;; the never-disproven legend of a Eunuch monastery at Princeton that
;; contains a large air-conditioned room with three time-worn posts in
;; it surrounded by 32 silicon discs.  Nimble monks, acting out the
;; command of an ancient prophecy, have been moving these disks, in
;; accordance with the rules of the puzzle, once every second since
;; the monastery was founded almost a billion seconds ago.  They are
;; said to believe that when the last move of the puzzle is completed,
;; the world will reboot in a clap of thunder.  Actually, because the
;; bottom disc is blocked by the "Do not feed the monks" sign, it is
;; believed the End will come at the time that disc is to be moved...

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; dynamic bondage:
  (defvar baseward-step)
  (defvar fly-step)
  (defvar fly-row-start)
  (defvar pole-width)
  (defvar pole-char)
  (defvar line-offset))

(defgroup hanoi nil
  "The Towers of Hanoi."
  :group 'games)

(defcustom hanoi-horizontal-flag nil
  "If non-nil, hanoi poles are oriented horizontally."
  :group 'hanoi :type 'boolean)

(defcustom hanoi-move-period 1.0
  "Time, in seconds, for each pole-to-pole move of a ring.
If nil, move rings as fast as possible while displaying all
intermediate positions."
  :group 'hanoi :type '(restricted-sexp :match-alternatives (numberp 'nil)))

(defcustom hanoi-use-faces nil
  "If nil, all hanoi-*-face variables are ignored."
  :group 'hanoi :type 'boolean)

(defcustom hanoi-pole-face 'highlight
  "Face for poles.  Ignored if hanoi-use-faces is nil."
  :group 'hanoi :type 'face)

(defcustom hanoi-base-face 'highlight
  "Face for base.  Ignored if hanoi-use-faces is nil."
  :group 'hanoi :type 'face)

(defcustom hanoi-even-ring-face 'region
  "Face for even-numbered rings.  Ignored if hanoi-use-faces is nil."
  :group 'hanoi :type 'face)

(defcustom hanoi-odd-ring-face 'secondary-selection
  "Face for odd-numbered rings.  Ignored if hanoi-use-faces is nil."
  :group 'hanoi :type 'face)


;;;
;;; hanoi - user callable Towers of Hanoi
;;;
;;;###autoload
(defun hanoi (nrings)
  "Towers of Hanoi diversion.  Use NRINGS rings."
  (interactive
   (list (if (null current-prefix-arg)
	     3
	     (prefix-numeric-value current-prefix-arg))))
  (if (< nrings 0)
      (error "Negative number of rings"))
  (hanoi-internal nrings (make-list nrings 0) (float-time)))

;;;###autoload
(defun hanoi-unix ()
  "Towers of Hanoi, UNIX doomsday version.
Displays 32-ring towers that have been progressing at one move per
second since 1970-01-01 00:00:00 GMT.

Repent before ring 31 moves."
  (interactive)
  (let* ((start (ftruncate (float-time)))
	 (bits (loop repeat 32
		     for x = (/ start (expt 2.0 31)) then (* x 2.0)
		     collect (truncate (mod x 2.0))))
	 (hanoi-move-period 1.0))
    (hanoi-internal 32 bits start)))

;;;###autoload
(defun hanoi-unix-64 ()
  "Like hanoi-unix, but pretend to have a 64-bit clock.
This is, necessarily (as of Emacs 20.3), a crock.  When the
current-time interface is made s2G-compliant, hanoi.el will need
to be updated."
  (interactive)
  (let* ((start (ftruncate (float-time)))
	 (bits (loop repeat 64
		     for x = (/ start (expt 2.0 63)) then (* x 2.0)
		     collect (truncate (mod x 2.0))))
	 (hanoi-move-period 1.0))
    (hanoi-internal 64 bits start)))

(defun hanoi-internal (nrings bits start-time)
  "Towers of Hanoi internal interface.  Use NRINGS rings.
Start after n steps, where BITS is a big-endian list of the bits of n.
BITS must be of length nrings.  Start at START-TIME."
  (switch-to-buffer "*Hanoi*")
  (buffer-disable-undo (current-buffer))
  (setq show-trailing-whitespace nil)
  (unwind-protect
      (let*
	  (;; These lines can cause Emacs to crash if you ask for too
	   ;; many rings.  If you uncomment them, on most systems you
	   ;; can get 10,000+ rings.
	   ;;(max-specpdl-size (max max-specpdl-size (* nrings 15)))
	   ;;(max-lisp-eval-depth (max max-lisp-eval-depth (+ nrings 20)))
	   (vert (not hanoi-horizontal-flag))
	   (pole-width (length (format "%d" (max 0 (1- nrings)))))
	   (pole-char (if vert ?\| ?\-))
	   (base-char (if vert ?\= ?\|))
	   (base-len (max (+ 8 (* pole-width 3))
			  (1- (if vert (window-width) (window-height)))))
	   (max-ring-diameter (/ (- base-len 2) 3))
	   (pole1-coord (/ max-ring-diameter 2))
	   (pole2-coord (/ base-len 2))
	   (pole3-coord (- base-len (/ (1+ max-ring-diameter) 2)))
	   (pole-coords (list pole1-coord pole2-coord pole3-coord))
	   ;; Number of lines displayed below the bottom-most rings.
	   (base-lines
	    (min 3 (max 0 (- (1- (if vert (window-height) (window-width)))
			     (+ 2 nrings)))))

	   ;; These variables will be set according to hanoi-horizontal-flag:

	   ;; line-offset is the number of characters per line in the buffer.
	   line-offset
	   ;; fly-row-start is the buffer position of the leftmost or
	   ;; uppermost position in the fly row.
	   fly-row-start
	   ;; Adding fly-step to a buffer position moves you one step
	   ;; along the fly row in the direction from pole1 to pole2.
	   fly-step
	   ;; Adding baseward-step to a buffer position moves you one step
	   ;; toward the base.
	   baseward-step
	   )
	(setq buffer-read-only nil)
	(erase-buffer)
	(setq truncate-lines t)
	(if hanoi-horizontal-flag
	    (progn
	      (setq line-offset (+ base-lines nrings 3))
	      (setq fly-row-start (1- line-offset))
	      (setq fly-step line-offset)
	      (setq baseward-step -1)
	      (loop repeat base-len do
		    (unless (zerop base-lines)
		      (insert-char ?\  (1- base-lines))
		      (insert base-char)
		      (hanoi-put-face (1- (point)) (point) hanoi-base-face))
		    (insert-char ?\  (+ 2 nrings))
		    (insert ?\n))
	      (delete-char -1)
	      (loop for coord in pole-coords do
		    (loop for row from (- coord (/ pole-width 2))
			  for start = (+ (* row line-offset) base-lines 1)
			  repeat pole-width do
			  (subst-char-in-region start (+ start nrings 1)
						?\  pole-char)
			  (hanoi-put-face start (+ start nrings 1)
					  hanoi-pole-face))))
	  ;; vertical
	  (setq line-offset (1+ base-len))
	  (setq fly-step 1)
	  (setq baseward-step line-offset)
	  (let ((extra-lines (- (1- (window-height)) (+ nrings 2) base-lines)))
	    (insert-char ?\n (max 0 extra-lines))
	    (setq fly-row-start (point))
	    (insert-char ?\  base-len)
	    (insert ?\n)
	    (loop repeat (1+ nrings)
		  with pole-line =
		  (loop with line = (make-string base-len ?\ )
			for coord in pole-coords
			for start = (- coord (/ pole-width 2))
			for end = (+ start pole-width) do
			(hanoi-put-face start end hanoi-pole-face line)
			(loop for i from start below end do
			      (aset line i pole-char))
			finally return line)
		  do (insert pole-line ?\n))
	    (insert-char base-char base-len)
	    (hanoi-put-face (- (point) base-len) (point) hanoi-base-face)
	    (set-window-start (selected-window)
			      (1+ (* baseward-step
				     (max 0 (- extra-lines)))))))

	(let
	    (;; each pole is a pair of buffer positions:
	     ;; the car is the position of the top ring currently on the pole,
	     ;;   (or the base of the pole if it is empty).
	     ;; the cdr is in the fly-row just above the pole.
	     (poles (loop for coord in pole-coords
			  for fly-pos = (+ fly-row-start (* fly-step coord))
			  for base = (+ fly-pos (* baseward-step (+ 2 nrings)))
			  collect (cons base fly-pos)))
	     ;; compute the string for each ring and make the list of
	     ;; ring pairs.  Each ring pair is initially (str . diameter).
	     ;; Once placed in buffer it is changed to (center-pos . diameter).
	     (rings
	      (loop
		;; radii are measured from the edge of the pole out.
		;; So diameter = 2 * radius + pole-width.  When
		;; there's room, we make each ring's radius =
		;; pole-number + 1.  If there isn't room, we step
		;; evenly from the max radius down to 1.
		with max-radius = (min nrings
				       (/ (- max-ring-diameter pole-width) 2))
		for n from (1- nrings) downto 0
		for radius =  (1+ (/ (* n max-radius) nrings))
		for diameter = (+ pole-width (* 2 radius))
		with format-str = (format "%%0%dd" pole-width)
		for str = (concat (if vert "<" "^")
				  (make-string (1- radius) (if vert ?\- ?\|))
				  (format format-str n)
				  (make-string (1- radius) (if vert ?\- ?\|))
				  (if vert ">" "v"))
		for face =
		  (if (eq (logand n 1) 1) ; oddp would require cl at runtime
		      hanoi-odd-ring-face hanoi-even-ring-face)
		do (hanoi-put-face 0 (length str) face str)
		collect (cons str diameter)))
	     ;; Disable display of line and column numbers, for speed.
	     (line-number-mode nil) (column-number-mode nil))
	  ;; do it!
	  (hanoi-n bits rings (car poles) (cadr poles) (caddr poles)
		   start-time))
	(message "Done"))
    (setq buffer-read-only t)
    (force-mode-line-update)))

(defun hanoi-put-face (start end value &optional object)
  "If hanoi-use-faces is non-nil, call put-text-property for face property."
  (if hanoi-use-faces
      (put-text-property start end 'face value object)))


;;; Functions with a start-time argument (hanoi-0, hanoi-n, and
;;; hanoi-move-ring) start working at start-time and return the ending
;;; time.  If hanoi-move-period is nil, start-time is ignored and the
;;; return value is junk.

;;;
;;; hanoi-0 - work horse of hanoi
(defun hanoi-0 (rings from to work start-time)
  (if (null rings)
      start-time
    (hanoi-0 (cdr rings) work to from
	     (hanoi-move-ring (car rings) from to
			      (hanoi-0 (cdr rings) from work to start-time)))))

;; start after n moves, where BITS is a big-endian list of the bits of n.
;; BITS must be of same length as rings.
(defun hanoi-n (bits rings from to work start-time)
  (cond ((null rings)
	 ;; All rings have been placed in starting positions.  Update display.
	 (hanoi-sit-for 0)
	 start-time)
	((zerop (car bits))
	 (hanoi-insert-ring (car rings) from)
	 (hanoi-0 (cdr rings) work to from
		  (hanoi-move-ring (car rings) from to
				   (hanoi-n (cdr bits) (cdr rings) from work to
					    start-time))))
	(t
	 (hanoi-insert-ring (car rings) to)
	 (hanoi-n (cdr bits) (cdr rings) work to from start-time))))

;; put never-before-placed RING on POLE and update their cars.
(defun hanoi-insert-ring (ring pole)
  (decf (car pole) baseward-step)
  (let ((str (car ring))
	(start (- (car pole) (* (/ (cdr ring) 2) fly-step))))
    (setcar ring (car pole))
    (loop for pos upfrom start by fly-step
	      for i below (cdr ring) do
	      (subst-char-in-region pos (1+ pos) (char-after pos) (aref str i))
	      (set-text-properties pos (1+ pos) (text-properties-at i str)))
    (hanoi-goto-char (car pole))))

;; like goto-char, but if position is outside the window, then move to
;; corresponding position in the first row displayed.
(defun hanoi-goto-char (pos)
  (goto-char (if (or hanoi-horizontal-flag (<= (window-start) pos))
		 pos
	       (+ (window-start) (% (- pos fly-row-start) baseward-step)))))

;; do one pole-to-pole move and update the ring and pole pairs.
(defun hanoi-move-ring (ring from to start-time)
  (incf (car from) baseward-step)
  (decf (car to) baseward-step)
  (let* ;; We move flywards-steps steps up the pole to the fly row,
	;; then fly fly-steps steps across the fly row, then go
	;; baseward-steps steps down the new pole.
	((flyward-steps (/ (- (car ring) (cdr from)) baseward-step))
	 (fly-steps (abs (/ (- (cdr to) (cdr from)) fly-step)))
	 (directed-fly-step (/ (- (cdr to) (cdr from)) fly-steps))
	 (baseward-steps (/ (- (car to) (cdr to)) baseward-step))
	 ;; A step is a character cell.  A tick is a time-unit.  To
	 ;; make horizontal and vertical motion appear roughly the
	 ;; same speed, we allow one tick per horizontal step and two
	 ;; ticks per vertical step.
	 (ticks-per-pole-step (if hanoi-horizontal-flag 1 2))
	 (ticks-per-fly-step (if hanoi-horizontal-flag 2 1))
	 (flyward-ticks (* ticks-per-pole-step flyward-steps))
	 (fly-ticks (* ticks-per-fly-step fly-steps))
	 (baseward-ticks (* ticks-per-pole-step baseward-steps))
	 (total-ticks (+ flyward-ticks fly-ticks baseward-ticks))
	 (tick-to-pos
	  ;; Return the buffer position of the ring after TICK ticks.
	  (lambda (tick)
	    (cond
	     ((<= tick flyward-ticks)
	      (+ (cdr from)
		 (* baseward-step
		    (- flyward-steps (/ tick ticks-per-pole-step)))))
	     ((<= tick (+ flyward-ticks fly-ticks))
	      (+ (cdr from)
		 (* directed-fly-step
		    (/ (- tick flyward-ticks) ticks-per-fly-step))))
	     (t
	      (+ (cdr to)
		 (* baseward-step
		    (/ (- tick flyward-ticks fly-ticks)
		       ticks-per-pole-step))))))))
    (if hanoi-move-period
	(loop for elapsed = (- (float-time) start-time)
	      while (< elapsed hanoi-move-period)
	      with tick-period = (/ (float hanoi-move-period) total-ticks)
	      for tick = (ceiling (/ elapsed tick-period)) do
	      (hanoi-ring-to-pos ring (funcall tick-to-pos tick))
	      (hanoi-sit-for (- (* tick tick-period) elapsed)))
      (loop for tick from 1 to total-ticks by 2 do
	    (hanoi-ring-to-pos ring (funcall tick-to-pos tick))
	    (hanoi-sit-for 0)))
    ;; Always make last move to keep pole and ring data consistent
    (hanoi-ring-to-pos ring (car to))
    (if hanoi-move-period (+ start-time hanoi-move-period))))

;; update display and pause, quitting with a pithy comment if the user
;; hits a key.
(defun hanoi-sit-for (seconds)
  (unless (sit-for seconds)
    (signal 'quit '("I can tell you've had enough"))))

;; move ring to a given buffer position and update ring's car.
(defun hanoi-ring-to-pos (ring pos)
  (unless (= (car ring) pos)
    (let* ((start (- (car ring) (* (/ (cdr ring) 2) fly-step)))
	   (new-start (- pos (- (car ring) start))))
      (if hanoi-horizontal-flag
	  (loop for i below (cdr ring)
		for j = (if (< new-start start) i (- (cdr ring) i 1))
		for old-pos = (+ start (* j fly-step))
		for new-pos = (+ new-start (* j fly-step)) do
		(transpose-regions old-pos (1+ old-pos) new-pos (1+ new-pos)))
	(let ((end (+ start (cdr ring)))
	      (new-end (+ new-start (cdr ring))))
	  (if (< (abs (- new-start start)) (- end start))
	      ;; Overlap.  Adjust bounds
	      (if (< start new-start)
		  (setq new-start end)
		(setq new-end start)))
	  (transpose-regions start end new-start new-end t))))
    ;; If moved on or off a pole, redraw pole chars.
    (unless (eq (hanoi-pos-on-tower-p (car ring)) (hanoi-pos-on-tower-p pos))
      (let* ((pole-start (- (car ring) (* fly-step (/ pole-width 2))))
	     (pole-end (+ pole-start (* fly-step pole-width)))
	     (on-pole (hanoi-pos-on-tower-p (car ring)))
	     (new-char (if on-pole pole-char ?\ ))
	     (curr-char (if on-pole ?\  pole-char))
	     (face (if on-pole hanoi-pole-face nil)))
	(if hanoi-horizontal-flag
	    (loop for pos from pole-start below pole-end by line-offset do
		  (subst-char-in-region pos (1+ pos) curr-char new-char)
		  (hanoi-put-face pos (1+ pos) face))
	  (subst-char-in-region pole-start pole-end curr-char new-char)
	  (hanoi-put-face pole-start pole-end face))))
    (setcar ring pos))
  (hanoi-goto-char pos))

;; Check if a buffer position lies on a tower (vis. in the fly row).
(defun hanoi-pos-on-tower-p (pos)
  (if hanoi-horizontal-flag
      (/= (% pos fly-step) fly-row-start)
    (>= pos (+ fly-row-start baseward-step))))

(provide 'hanoi)

;;; hanoi.el ends here
