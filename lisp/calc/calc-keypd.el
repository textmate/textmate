;;; calc-keypd.el --- mouse-capable keypad input for Calc

;; Copyright (C) 1990-1993, 2001-2012  Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

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

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

(defvar calc-keypad-buffer nil)
(defvar calc-keypad-menu 0)
(defvar calc-keypad-full-layout nil)
(defvar calc-keypad-input nil)
(defvar calc-keypad-prev-input nil)
(defvar calc-keypad-said-hello nil)

;;; |----+----+----+----+----+----|
;;; |  ENTER  |+/- |EEX |UNDO| <- |
;;; |-----+---+-+--+--+-+---++----|
;;; | INV |  7  |  8  |  9  |  /  |
;;; |-----+-----+-----+-----+-----|
;;; | HYP |  4  |  5  |  6  |  *  |
;;; |-----+-----+-----+-----+-----|
;;; |EXEC |  1  |  2  |  3  |  -  |
;;; |-----+-----+-----+-----+-----|
;;; | OFF |  0  |  .  | PI  |  +  |
;;; |-----+-----+-----+-----+-----|
(defvar calc-keypad-layout
  '( ( ( "ENTER" calc-enter calc-roll-down calc-roll-up calc-over )
       ( "ENTER" calc-enter calc-roll-down calc-roll-up calc-over )
       ( "+/-"	 calc-change-sign calc-inv (progn -4 calc-pack) )
       ( "EEX"	 ("e") (progn calc-num-prefix calc-pack-interval)
                 (progn -5 calc-pack)  )
       ( "UNDO"	 calc-undo calc-redo calc-last-args )
       ( "<-"	 calc-pop (progn 0 calc-pop)
	         (progn calc-num-prefix calc-pop) ) )
     ( ( "INV"   calc-inverse )
       ( "7"	 ("7") calc-round )
       ( "8"	 ("8") (progn 2 calc-clean-num) )
       ( "9"	 ("9") calc-float )
       ( "/"	 calc-divide (progn calc-inverse calc-power) ) )
     ( ( "HYP"   calc-hyperbolic )
       ( "4"	 ("4") calc-ln calc-log10 )
       ( "5"	 ("5") calc-exp calc-exp10 )
       ( "6"	 ("6") calc-abs )
       ( "*"	 calc-times calc-power ) )
     ( ( "EXEC"	 calc-keypad-execute )
       ( "1"	 ("1") calc-arcsin calc-sin )
       ( "2"	 ("2") calc-arccos calc-cos )
       ( "3"	 ("3") calc-arctan calc-tan )
       ( "-"	 calc-minus calc-conj ) )
     ( ( "OFF"   calc-keypad-off )
       ( "0"	 ("0") calc-imaginary )
       ( "."	 (".") calc-precision )
       ( "PI"	 calc-pi )
       ( "+"	 calc-plus calc-sqrt ) ) ))

(defvar calc-keypad-menus '( calc-keypad-math-menu
			     calc-keypad-funcs-menu
			     calc-keypad-binary-menu
			     calc-keypad-vector-menu
			     calc-keypad-modes-menu
			     calc-keypad-user-menu ) )

;;; |----+----+----+----+----+----|
;;; |FLR |CEIL|RND |TRNC|CLN2|FLT |
;;; |----+----+----+----+----+----|
;;; | LN |EXP |    |ABS |IDIV|MOD |
;;; |----+----+----+----+----+----|
;;; |SIN |COS |TAN |SQRT|y^x |1/x |

(defvar calc-keypad-math-menu
  '( ( ( "FLR"   calc-floor )
       ( "CEIL"  calc-ceiling )
       ( "RND"   calc-round )
       ( "TRNC"  calc-trunc )
       ( "CLN2"  (progn 2 calc-clean-num) )
       ( "FLT"   calc-float ) )
     ( ( "LN"    calc-ln )
       ( "EXP"   calc-exp )
       ( ""	 nil )
       ( "ABS"   calc-abs )
       ( "IDIV"  calc-idiv )
       ( "MOD"   calc-mod ) )
     ( ( "SIN"   calc-sin )
       ( "COS"   calc-cos )
       ( "TAN"   calc-tan )
       ( "SQRT"  calc-sqrt )
       ( "y^x"   calc-power )
       ( "1/x"   calc-inv ) ) ))

;;; |----+----+----+----+----+----|
;;; |IGAM|BETA|IBET|ERF |BESJ|BESY|
;;; |----+----+----+----+----+----|
;;; |IMAG|CONJ| RE |ATN2|RAND|RAGN|
;;; |----+----+----+----+----+----|
;;; |GCD |FACT|DFCT|BNOM|PERM|NXTP|

(defvar calc-keypad-funcs-menu
  '( ( ( "IGAM"  calc-inc-gamma )
       ( "BETA"  calc-beta )
       ( "IBET"  calc-inc-beta )
       ( "ERF"   calc-erf )
       ( "BESJ"  calc-bessel-J )
       ( "BESY"  calc-bessel-Y ) )
     ( ( "IMAG"  calc-imaginary )
       ( "CONJ"  calc-conj )
       ( "RE"	 calc-re calc-im )
       ( "ATN2"  calc-arctan2 )
       ( "RAND"  calc-random )
       ( "RAGN"  calc-random-again ) )
     ( ( "GCD"   calc-gcd calc-lcm )
       ( "FACT"  calc-factorial calc-gamma )
       ( "DFCT"  calc-double-factorial )
       ( "BNOM"  calc-choose )
       ( "PERM"  calc-perm )
       ( "NXTP"	 calc-next-prime calc-prev-prime ) ) ))

;;; |----+----+----+----+----+----|
;;; |AND | OR |XOR |NOT |LSH |RSH |
;;; |----+----+----+----+----+----|
;;; |DEC |HEX |OCT |BIN |WSIZ|ARSH|
;;; |----+----+----+----+----+----|
;;; | A  | B  | C  | D  | E  | F  |

(defvar calc-keypad-binary-menu
  '( ( ( "AND"   calc-and calc-diff )
       ( "OR"    calc-or )
       ( "XOR"   calc-xor )
       ( "NOT"   calc-not calc-clip )
       ( "LSH"   calc-lshift-binary calc-rotate-binary )
       ( "RSH"   calc-rshift-binary ) )
     ( ( "DEC"   calc-decimal-radix )
       ( "HEX"   calc-hex-radix )
       ( "OCT"   calc-octal-radix )
       ( "BIN"   calc-binary-radix )
       ( "WSIZ"  calc-word-size )
       ( "ARSH"  calc-rshift-arith ) )
     ( ( "A"     ("A") )
       ( "B"     ("B") )
       ( "C"     ("C") )
       ( "D"     ("D") )
       ( "E"     ("E") )
       ( "F"     ("F") ) ) ))

;;; |----+----+----+----+----+----|
;;; |SUM |PROD|MAX |MAP*|MAP^|MAP$|
;;; |----+----+----+----+----+----|
;;; |INV |DET |TRN |IDNT|CRSS|"x" |
;;; |----+----+----+----+----+----|
;;; |PACK|UNPK|INDX|BLD |LEN |... |

(defvar calc-keypad-vector-menu
  '( ( ( "SUM"   calc-vector-sum calc-vector-alt-sum calc-vector-mean )
       ( "PROD"  calc-vector-product nil calc-vector-sdev )
       ( "MAX"   calc-vector-max calc-vector-min calc-vector-median )
       ( "MAP*"  (lambda () (interactive)
		   (calc-map '(2 calcFunc-mul "*"))) )
       ( "MAP^"  (lambda () (interactive)
		   (calc-map '(2 calcFunc-pow "^"))) )
       ( "MAP$"  calc-map-stack ) )
     ( ( "MINV"  calc-inv )
       ( "MDET"  calc-mdet )
       ( "MTRN"  calc-transpose calc-conj-transpose )
       ( "IDNT"  (progn calc-num-prefix calc-ident) )
       ( "CRSS"  calc-cross )
       ( "\"x\"" "\excalc-algebraic-entry\rx\r"
	         "\excalc-algebraic-entry\ry\r"
		 "\excalc-algebraic-entry\rz\r"
		 "\excalc-algebraic-entry\rt\r") )
     ( ( "PACK"  calc-pack )
       ( "UNPK"  calc-unpack )
       ( "INDX"  (progn calc-num-prefix calc-index) "\C-u\excalc-index\r" )
       ( "BLD"   (progn calc-num-prefix calc-build-vector) )
       ( "LEN"   calc-vlength )
       ( "..."   calc-full-vectors ) ) ))

;;; |----+----+----+----+----+----|
;;; |FLT |FIX |SCI |ENG |GRP |    |
;;; |----+----+----+----+----+----|
;;; |RAD |DEG |FRAC|POLR|SYMB|PREC|
;;; |----+----+----+----+----+----|
;;; |SWAP|RLL3|RLL4|OVER|STO |RCL |

(defvar calc-keypad-modes-menu
  '( ( ( "FLT"   calc-normal-notation
	         (progn calc-num-prefix calc-normal-notation) )
       ( "FIX"   (progn 2 calc-fix-notation)
	         (progn calc-num-prefix calc-fix-notation) )
       ( "SCI"   calc-sci-notation
	         (progn calc-num-prefix calc-sci-notation) )
       ( "ENG"   calc-eng-notation
	         (progn calc-num-prefix calc-eng-notation) )
       ( "GRP"   calc-group-digits "\C-u-3\excalc-group-digits\r" )
       ( ""	 nil ) )
     ( ( "RAD"   calc-radians-mode )
       ( "DEG"   calc-degrees-mode )
       ( "FRAC"  calc-frac-mode )
       ( "POLR"  calc-polar-mode )
       ( "SYMB"	 calc-symbolic-mode )
       ( "PREC"  calc-precision ) )
     ( ( "SWAP"  calc-roll-down )
       ( "RLL3"  (progn 3 calc-roll-up) (progn 3 calc-roll-down) )
       ( "RLL4"  (progn 4 calc-roll-up) (progn 4 calc-roll-down) )
       ( "OVER"  calc-over )
       ( "STO"   calc-keypad-store )
       ( "RCL"   calc-keypad-recall ) ) ))

(define-derived-mode calc-keypad-mode fundamental-mode "Calculator"
  "Major mode for Calc keypad input."
  (define-key calc-keypad-mode-map " " 'calc-keypad-press)
  (define-key calc-keypad-mode-map (kbd "RET") 'calc-keypad-press)
  (define-key calc-keypad-mode-map (kbd "TAB") 'calc-keypad-menu)
  (define-key calc-keypad-mode-map "q" 'calc-keypad-off)
  (define-key calc-keypad-mode-map [down-mouse-1] 'ignore)
  (define-key calc-keypad-mode-map [drag-mouse-1] 'ignore)
  (define-key calc-keypad-mode-map [double-mouse-1] 'ignore)
  (define-key calc-keypad-mode-map [triple-mouse-1] 'ignore)
  (define-key calc-keypad-mode-map [down-mouse-2] 'ignore)
  (define-key calc-keypad-mode-map [drag-mouse-2] 'ignore)
  (define-key calc-keypad-mode-map [double-mouse-2] 'ignore)
  (define-key calc-keypad-mode-map [triple-mouse-2] 'ignore)
  (define-key calc-keypad-mode-map [down-mouse-3] 'ignore)
  (define-key calc-keypad-mode-map [drag-mouse-3] 'ignore)
  (define-key calc-keypad-mode-map [double-mouse-3] 'ignore)
  (define-key calc-keypad-mode-map [triple-mouse-3] 'ignore)
  (define-key calc-keypad-mode-map [mouse-3] 'calc-keypad-right-click)
  (define-key calc-keypad-mode-map [mouse-2] 'calc-keypad-middle-click)
  (define-key calc-keypad-mode-map [mouse-1] 'calc-keypad-left-click)
  (put 'calc-keypad-mode 'mode-class 'special)
  (make-local-variable 'calc-main-buffer))

(defun calc-do-keypad (&optional full-display interactive)
  (calc-create-buffer)
  (let ((calcbuf (current-buffer)))
    (unless (bufferp calc-keypad-buffer)
      (set-buffer (setq calc-keypad-buffer (get-buffer-create "*Calc Keypad*")))
      (calc-keypad-mode)
      (setq calc-main-buffer calcbuf)
      (calc-keypad-redraw)
      (calc-trail-buffer))
    (let ((width 29)
	  (height 17)
	  win old-win)
      (if (setq win (get-buffer-window "*Calculator*"))
	  (delete-window win))
      (if (setq win (get-buffer-window "*Calc Trail*"))
	  (if (one-window-p)
	      (switch-to-buffer (other-buffer))
	    (delete-window win)))
      (if (setq win (get-buffer-window calc-keypad-buffer))
	  (progn
	    (bury-buffer "*Calculator*")
	    (bury-buffer "*Calc Trail*")
	    (bury-buffer calc-keypad-buffer)
	    (if (one-window-p)
		(switch-to-buffer (other-buffer))
	      (delete-window win)))
	(setq calc-was-keypad-mode t
	      old-win (get-largest-window))
	(if (or (< (window-height old-win) (+ height 6))
		(< (window-width old-win) (+ width 15))
		full-display)
	    (delete-other-windows old-win))
	(if (< (window-height old-win) (+ height 4))
	    (error "Screen is not tall enough for this mode"))
	(if full-display
	    (progn
	      (setq win (split-window old-win (- (window-height old-win)
						 height 1)))
	      (set-window-buffer old-win (calc-trail-buffer))
	      (set-window-buffer win calc-keypad-buffer)
	      (set-window-start win 1)
	      (setq win (split-window win (+ width 7) t))
	      (set-window-buffer win calcbuf))
	  (if (or t  ; left-side keypad not yet fully implemented
		  (< (with-current-buffer (window-buffer old-win)
		       (current-column))
		     (/ (window-width) 2)))
	      (setq win (split-window old-win (- (window-width old-win)
						 width 2)
				      t))
	    (setq old-win (split-window old-win (+ width 2) t)))
	  (set-window-buffer win calc-keypad-buffer)
	  (set-window-start win 1)
	  (split-window win (- (window-height win) height 1))
	  (set-window-buffer win calcbuf))
	(select-window old-win)
	(message "Welcome to GNU Emacs Calc!  Use the left and right mouse buttons")
	(run-hooks 'calc-keypad-start-hook)
	(and calc-keypad-said-hello interactive
	     (progn
	       (sit-for 2)
	       (message "")))
	(setq calc-keypad-said-hello t)))
    (setq calc-keypad-input nil)))

(defun calc-keypad-off ()
  (interactive)
  (if calc-standalone-flag
      (save-buffers-kill-emacs nil)
    (calc-keypad)))

(defun calc-keypad-redraw ()
  (set-buffer calc-keypad-buffer)
  (setq buffer-read-only t)
  (setq calc-keypad-full-layout (append (symbol-value (nth calc-keypad-menu
							   calc-keypad-menus))
					calc-keypad-layout))
  (let ((buffer-read-only nil)
	(row calc-keypad-full-layout)
	(y 0))
    (erase-buffer)
    (insert "\n")
    (while row
      (let ((col (car row)))
	(while col
	  (let* ((key (car col))
		 (cwid (if (>= y 4)
			   5
			 (if (and (= y 3) (eq col (car row)))
			     (progn (setq col (cdr col)) 9)
			   4)))
		 (name (if (and calc-standalone-flag
				(eq (nth 1 key) 'calc-keypad-off))
			   "EXIT"
			 (if (> (length (car key)) cwid)
			     (substring (car key) 0 cwid)
			   (car key))))
		 (wid (length name))
		 (pad (- cwid (/ wid 2))))
	    (insert (make-string (/ (- cwid wid) 2) 32)
		    name
		    (make-string (/ (- cwid wid -1) 2) 32)
		    (if (equal name "MENU")
			(int-to-string (1+ calc-keypad-menu))
		      "|")))
	  (or (setq col (cdr col))
	      (insert "\n")))
	(insert (if (>= y 4)
		    "-----+-----+-----+-----+-----"
		  (if (= y 3)
		      "-----+---+-+--+--+-+---++----"
		    "----+----+----+----+----+----"))
		(if (= y 7) "+\n" "|\n"))
	(setq y (1+ y)
	      row (cdr row)))))
  (setq calc-keypad-prev-input t)
  (calc-keypad-show-input)
  (goto-char (point-min)))

(defun calc-keypad-show-input ()
  (or (equal calc-keypad-input calc-keypad-prev-input)
      (let ((buffer-read-only nil))
	(save-excursion
	  (goto-char (point-min))
	  (forward-line 1)
	  (delete-region (point-min) (point))
	  (if calc-keypad-input
	      (insert "Calc: " calc-keypad-input "\n")
	    (insert "----+----+--Calc---+----+----"
		    (int-to-string (1+ calc-keypad-menu))
		    "\n")))))
  (setq calc-keypad-prev-input calc-keypad-input))

(defun calc-keypad-press ()
  (interactive)
  (unless (eq major-mode 'calc-keypad-mode)
    (error "Must be in *Calc Keypad* buffer for this command"))
  (let* ((row (count-lines (point-min) (point-at-bol)))
	 (y (/ row 2))
	 (x (/ (current-column) (if (>= y 4) 6 5)))
	 radix frac inv
	 (hyp (with-current-buffer calc-main-buffer
		(setq radix calc-number-radix
		      frac calc-prefer-frac
		      inv calc-inverse-flag)
		calc-hyperbolic-flag))
	 (invhyp t)
	 (menu (symbol-value (nth calc-keypad-menu calc-keypad-menus)))
	 (input calc-keypad-input)
	 (iexpon (and input
		      (or (string-match "\\*[0-9]+\\.\\^" input)
			  (and (<= radix 14) (string-match "e" input)))
		      (match-end 0)))
	 (key (nth x (nth y calc-keypad-full-layout)))
	 (cmd (or (nth (if inv (if hyp 4 2) (if hyp 3 99)) key)
		  (setq invhyp nil)
		  (nth 1 key)))
	 (isstring (and (consp cmd) (stringp (car cmd))))
	 (calc-is-keypad-press t))
    (if invhyp (calc-wrapper))  ; clear Inv and Hyp flags
    (unwind-protect
	(cond ((or (null cmd)
		   (= (% row 2) 0))
	       (beep))
	      ((and (> (minibuffer-depth) 0))
	       (cond (isstring
		      (push (aref (car cmd) 0) unread-command-events))
		     ((eq cmd 'calc-pop)
		      (push ?\177 unread-command-events))
		     ((eq cmd 'calc-enter)
		      (push 13 unread-command-events))
		     ((eq cmd 'calc-undo)
		      (push 7 unread-command-events))
		     (t
		      (beep))))
	      ((and input (string-match "STO\\|RCL" input))
	       (cond ((and isstring (string-match "[0-9]" (car cmd)))
		      (setq calc-keypad-input nil)
		      (let ((var (intern (concat "var-q" (car cmd)))))
			(cond ((equal input "STO+") (calc-store-plus var))
			      ((equal input "STO-") (calc-store-minus var))
			      ((equal input "STO*") (calc-store-times var))
			      ((equal input "STO/") (calc-store-div var))
			      ((equal input "STO^") (calc-store-power var))
			      ((equal input "STOn") (calc-store-neg 1 var))
			      ((equal input "STO&") (calc-store-inv 1 var))
			      ((equal input "STO") (calc-store-into var))
			      (t (calc-recall var)))))
		     ((memq cmd '(calc-pop calc-undo))
		      (setq calc-keypad-input nil))
		     ((and (equal input "STO")
			   (setq frac (assq cmd '( ( calc-plus . "+" )
						   ( calc-minus . "-" )
						   ( calc-times . "*" )
						   ( calc-divide . "/" )
						   ( calc-power . "^")
						   ( calc-change-sign . "n")
						   ( calc-inv . "&") ))))
		      (setq calc-keypad-input (concat input (cdr frac))))
		     (t
		      (beep))))
	      (isstring
	       (setq cmd (car cmd))
	       (if (or (and (equal cmd ".")
			    input
			    (string-match "[.:e^]" input))
		       (and (equal cmd "e")
			    input
			    (or (and (<= radix 14) (string-match "e" input))
				(string-match "\\^\\|[-.:]\\'" input)))
		       (and (not (equal cmd "."))
			    (let ((case-fold-search nil))
			      (string-match cmd "0123456789ABCDEF"
					    (if (string-match
						 "[e^]" (or input ""))
						10 radix)))))
		   (beep)
		 (setq calc-keypad-input (concat
					  (and (/= radix 10)
					       (or (not input)
						   (equal input "-"))
					       (format "%d#" radix))
					  (and (or (not input)
						   (equal input "-"))
					       (or (and (equal cmd "e") "1")
						   (and (equal cmd ".")
							(if frac "1" "0"))))
					  input
					  (if (and (equal cmd ".") frac)
					      ":"
					    (if (and (equal cmd "e")
						     (or (not input)
							 (string-match
							  "#" input))
						     (> radix 14))
						(format "*%d.^" radix)
					      cmd))))))
	      ((and (eq cmd 'calc-change-sign)
		    input)
	       (let* ((epos (or iexpon 0))
		      (suffix (substring input epos)))
		 (setq calc-keypad-input (concat
					  (substring input 0 epos)
					  (if (string-match "\\`-" suffix)
					      (substring suffix 1)
					    (concat "-" suffix))))))
	      ((and (eq cmd 'calc-pop)
		    input)
	       (if (equal input "")
		   (beep)
		 (setq calc-keypad-input (substring input 0
						    (or (string-match
							 "\\*[0-9]+\\.\\^\\'"
							 input)
							-1)))))
	      ((and (eq cmd 'calc-undo)
		    input)
	       (setq calc-keypad-input nil))
	      (t
	       (if input
		   (let ((val (math-read-number input)))
		     (setq calc-keypad-input nil)
		     (if val
			 (calc-wrapper
			  (calc-push-list (list (calc-record
						 (calc-normalize val)))))
		       (or (equal input "")
			   (beep))
		       (setq cmd nil))
		     (if (eq cmd 'calc-enter) (setq cmd nil))))
	       (setq prefix-arg current-prefix-arg)
	       (if cmd
		   (if (and (consp cmd) (eq (car cmd) 'progn))
		       (while (setq cmd (cdr cmd))
			 (if (integerp (car cmd))
			     (setq prefix-arg (car cmd))
			   (command-execute (car cmd))))
		     (command-execute cmd)))))
      (set-buffer calc-keypad-buffer)
      (calc-keypad-show-input))))

(defun calc-keypad-left-click (event)
  "Handle a left-button mouse click in Calc Keypad window."
  (interactive "e")
  (with-current-buffer calc-keypad-buffer
    (goto-char (posn-point (event-start event)))
    (calc-keypad-press)))

(defun calc-keypad-right-click (event)
  "Handle a right-button mouse click in Calc Keypad window."
  (interactive "e")
  (with-current-buffer calc-keypad-buffer
    (calc-keypad-menu)))

(defun calc-keypad-middle-click (event)
  "Handle a middle-button mouse click in Calc Keypad window."
  (interactive "e")
  (with-current-buffer calc-keypad-buffer
    (calc-keypad-menu-back)))

(defun calc-keypad-menu ()
  (interactive)
  (unless (eq major-mode 'calc-keypad-mode)
    (error "Must be in *Calc Keypad* buffer for this command"))
  (while (progn (setq calc-keypad-menu (% (1+ calc-keypad-menu)
					  (length calc-keypad-menus)))
		(not (symbol-value (nth calc-keypad-menu calc-keypad-menus)))))
  (calc-keypad-redraw))

(defun calc-keypad-menu-back ()
  (interactive)
  (or (eq major-mode 'calc-keypad-mode)
      (error "Must be in *Calc Keypad* buffer for this command"))
  (while (progn (setq calc-keypad-menu (% (1- (+ calc-keypad-menu
						 (length calc-keypad-menus)))
					  (length calc-keypad-menus)))
		(not (symbol-value (nth calc-keypad-menu calc-keypad-menus)))))
  (calc-keypad-redraw))

(defun calc-keypad-store ()
  (interactive)
  (setq calc-keypad-input "STO"))

(defun calc-keypad-recall ()
  (interactive)
  (setq calc-keypad-input "RCL"))

(defun calc-pack-interval (mode)
  (interactive "p")
  (if (or (< mode 0) (> mode 3))
      (error "Open/close code should be in the range from 0 to 3"))
  (calc-pack (- -6 mode)))

(defun calc-keypad-execute ()
  (interactive)
  (let* ((prompt "Calc keystrokes: ")
	 (flush 'x-flush-mouse-queue)
	 (prefix nil)
	 keys cmd)
    (save-excursion
      (calc-select-buffer)
      (while (progn
	       (setq keys (read-key-sequence prompt))
	       (setq cmd (key-binding keys))
	       (if (or (memq cmd '(calc-inverse
				   calc-hyperbolic
				   universal-argument
				   digit-argument
				   negative-argument))
		       (and prefix (string-match "\\`\e?[-0-9]\\'" keys)))
		   (progn
		     (setq last-command-event (aref keys (1- (length keys))))
		     (command-execute cmd)
		     (setq flush 'not-any-more
			   prefix t
			   prompt (concat prompt (key-description keys) " ")))
		 (eq cmd flush)))))  ; skip mouse-up event
    (message "")
    (if (commandp cmd)
	(command-execute cmd)
      (error "Not a Calc command: %s" (key-description keys)))))

(provide 'calc-keypd)

;;; calc-keypd.el ends here
