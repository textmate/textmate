;;; calc-misc.el --- miscellaneous functions for Calc

;; Copyright (C) 1990-1993, 2001-2012 Free Software Foundation, Inc.

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

;; This file is autoloaded from calc.el.

(require 'calc)
(require 'calc-macs)

;; Declare functions which are defined elsewhere.
(declare-function calc-do-keypad "calc-keypd" (&optional full-display interactive))
(declare-function calc-inv-hyp-prefix-help "calc-help" ())
(declare-function calc-inverse-prefix-help "calc-help" ())
(declare-function calc-hyperbolic-prefix-help "calc-help" ())
(declare-function calc-option-prefix-help "calc-help" ())
(declare-function calc-explain-why "calc-stuff" (why &optional more))
(declare-function calc-clear-command-flag "calc-ext" (f))
(declare-function calc-roll-down-with-selections "calc-sel" (n m))
(declare-function calc-roll-up-with-selections "calc-sel" (n m))
(declare-function calc-last-args "calc-undo" (n))
(declare-function calc-is-inverse "calc-ext" ())
(declare-function calc-do-prefix-help "calc-ext" (msgs group key))
(declare-function math-objvecp "calc-ext" (a))
(declare-function math-known-scalarp "calc-arith" (a &optional assume-scalar))
(declare-function math-vectorp "calc-ext" (a))
(declare-function math-matrixp "calc-ext" (a))
(declare-function math-trunc-special "calc-arith" (a prec))
(declare-function math-trunc-fancy "calc-arith" (a))
(declare-function math-floor-special "calc-arith" (a prec))
(declare-function math-floor-fancy "calc-arith" (a))
(declare-function math-square-matrixp "calc-ext" (a))
(declare-function math-matrix-inv-raw "calc-mtx" (m))
(declare-function math-known-matrixp "calc-arith" (a))
(declare-function math-mod-fancy "calc-arith" (a b))
(declare-function math-pow-of-zero "calc-arith" (a b))
(declare-function math-pow-zero "calc-arith" (a b))
(declare-function math-pow-fancy "calc-arith" (a b))
(declare-function calc-locate-cursor-element "calc-yank" (pt))

;;;###autoload
(defun calc-dispatch-help (arg)
  "C-x* is a prefix key sequence; follow it with one of these letters:

For turning Calc on and off:
  C  calc.  Start the Calculator in a window at the bottom of the screen.
  O  calc-other-window.  Start the Calculator but don't select its window.
  B  calc-big-or-small.  Control whether to use the full Emacs screen for Calc.
  Q  quick-calc.  Use the Calculator in the minibuffer.
  K  calc-keypad.  Start the Calculator in keypad mode (X window system only).
  E  calc-embedded.  Use the Calculator on a formula in this editing buffer.
  J  calc-embedded-select.  Like E, but select appropriate half of => or :=.
  W  calc-embedded-word.  Like E, but activate a single word, i.e., a number.
  Z  calc-user-invocation.  Invoke Calc in the way you defined with `Z I' cmd.
  X  calc-quit.  Turn Calc off.

For moving data into and out of Calc:
  G  calc-grab-region.  Grab the region defined by mark and point into Calc.
  R  calc-grab-rectangle.  Grab the rectangle defined by mark, point into Calc.
  :  calc-grab-sum-down.  Grab a rectangle and sum the columns.
  _  calc-grab-sum-across.  Grab a rectangle and sum the rows.
  Y  calc-copy-to-buffer.  Copy a value from the stack into the editing buffer.

For use with Embedded mode:
  A  calc-embedded-activate.  Find and activate all :='s and =>'s in buffer.
  D  calc-embedded-duplicate.  Make a copy of this formula and select it.
  F  calc-embedded-new-formula.  Insert a new formula at current point.
  N  calc-embedded-next.  Advance cursor to next known formula in buffer.
  P  calc-embedded-previous.  Advance cursor to previous known formula.
  U  calc-embedded-update-formula.  Re-evaluate formula at point.
  `  calc-embedded-edit.  Use calc-edit to edit formula at point.

Documentation:
  I  calc-info.  Read the Calculator manual in the Emacs Info system.
  T  calc-tutorial.  Run the Calculator Tutorial using the Emacs Info system.
  S  calc-summary.  Read the Summary from the Calculator manual in Info.

Miscellaneous:
  L  calc-load-everything.  Load all parts of the Calculator into memory.
  M  read-kbd-macro.  Read a region of keystroke names as a keyboard macro.
  0  (zero) calc-reset.  Reset Calc stack and modes to default state.

Press `*' twice (`C-x * *') to turn Calc on or off using the same
Calc user interface as before (either C-x * C or C-x * K; initially C-x * C).
"
  (interactive "P")
  (calc-check-defines)
  (if calc-dispatch-help
      (progn
	(save-window-excursion
	  (describe-function 'calc-dispatch-help)
	  (let ((win (get-buffer-window "*Help*")))
	    (if win
		(let (key)
		  (select-window win)
		  (while (progn
			   (message "Calc options: Calc, Keypad, ...  %s"
				    "press SPC, DEL to scroll, C-g to cancel")
			   (memq (car (setq key (calc-read-key t)))
				 '(?  ?\C-h ?\C-? ?\C-v ?\M-v)))
		    (condition-case err
			(if (memq (car key) '(?  ?\C-v))
			    (scroll-up)
			  (scroll-down))
		      (error (beep))))
		      (calc-unread-command (cdr key))))))
	(calc-do-dispatch nil))
    (let ((calc-dispatch-help t))
      (calc-do-dispatch arg))))


;;;###autoload
(defun calc-big-or-small (arg)
  "Toggle Calc between full-screen and regular mode."
  (interactive "P")
  (let ((cwin (get-buffer-window "*Calculator*"))
	(twin (get-buffer-window "*Calc Trail*"))
	(kwin (get-buffer-window "*Calc Keypad*")))
    (if cwin
	(setq calc-full-mode
	      (if kwin
		  (and twin (window-full-width-p twin))
		(window-full-height-p cwin))))
    (setq calc-full-mode (if arg
			     (> (prefix-numeric-value arg) 0)
			   (not calc-full-mode)))
    (if kwin
	(progn
	  (calc-quit)
	  (calc-do-keypad calc-full-mode nil))
      (if cwin
	  (progn
	    (calc-quit)
	    (calc nil calc-full-mode nil))))
    (message (if calc-full-mode
		 "Now using full screen for Calc"
	       "Now using partial screen for Calc"))))

;;;###autoload
(defun calc-other-window (&optional interactive)
  "Invoke the Calculator in another window."
  (interactive "p")
  (if (memq major-mode '(calc-mode calc-trail-mode))
      (progn
	(other-window 1)
	(if (memq major-mode '(calc-mode calc-trail-mode))
	    (other-window 1)))
    (if (get-buffer-window "*Calculator*")
	(calc-quit)
      (let ((win (selected-window)))
	(calc nil win interactive)))))

;;;###autoload
(defun another-calc ()
  "Create another, independent Calculator buffer."
  (interactive)
  (if (eq major-mode 'calc-mode)
      (mapc (function
	     (lambda (v)
	      (set-default v (symbol-value v)))) calc-local-var-list))
  (set-buffer (generate-new-buffer "*Calculator*"))
  (pop-to-buffer (current-buffer))
  (calc-mode))

;;;###autoload
(defun calc-info ()
  "Run the Emacs Info system on the Calculator documentation."
  (interactive)
  (select-window (get-largest-window))
  (info "Calc"))

;;;###autoload
(defun calc-info-goto-node (node)
  "Go to a node in the Calculator info documentation."
  (interactive)
  (select-window (get-largest-window))
  (info (concat "(Calc)" node)))

;;;###autoload
(defun calc-tutorial ()
  "Run the Emacs Info system on the Calculator Tutorial."
  (interactive)
  (if (get-buffer-window "*Calculator*")
      (calc-quit))
  (calc-info-goto-node "Interactive Tutorial")
  (calc-other-window)
  (message "Welcome to the Calc Tutorial!"))

;;;###autoload
(defun calc-info-summary ()
  "Run the Emacs Info system on the Calculator Summary."
  (interactive)
  (calc-info-goto-node "Summary"))

;;;###autoload
(defun calc-help ()
  (interactive)
  (let ((msgs
	 '("Press `h' for complete help; press `?' repeatedly for a summary"
	   "Letter keys: Negate; Precision; Yank; Why; Xtended cmd; Quit"
	   "Letter keys: SHIFT + Undo, reDo; Inverse, Hyperbolic, Option"
	   "Letter keys: SHIFT + sQrt; Sin, Cos, Tan; Exp, Ln, logB"
	   "Letter keys: SHIFT + Floor, Round; Abs, conJ, arG; Pi"
	   "Letter keys: SHIFT + Num-eval; More-recn; eXec-kbd-macro; Keep-args"
	   "Other keys: +, -, *, /, ^, \\ (int div), : (frac div)"
	   "Other keys: & (1/x), | (concat), % (modulo), ! (factorial)"
	   "Other keys: ' (alg-entry), = (eval), ` (edit); M-RET (last-args)"
	   "Other keys: SPC/RET (enter/dup), LFD (over); < > (scroll horiz)"
	   "Other keys: DEL (drop), M-DEL (drop-above); { } (scroll vert)"
	   "Other keys: TAB (swap/roll-dn), M-TAB (roll-up)"
	   "Other keys: [ , ; ] (vector), ( , ) (complex), ( ; ) (polar)"
	   "Prefix keys: Algebra, Binary/business, Convert, Display"
	   "Prefix keys: Functions, Graphics, Help, J (select)"
	   "Prefix keys: Kombinatorics/statistics, Modes, Store/recall"
	   "Prefix keys: Trail/time, Units/statistics, Vector/matrix"
	   "Prefix keys: Z (user), SHIFT + Z (define)"
	   "Prefix keys: prefix + ? gives further help for that prefix"
           "  Calc by Dave Gillespie, daveg@synaptics.com")))
    (if calc-full-help-flag
	msgs
      (if (or calc-inverse-flag calc-hyperbolic-flag)
	  (if calc-inverse-flag
	      (if calc-hyperbolic-flag
		  (calc-inv-hyp-prefix-help)
		(calc-inverse-prefix-help))
	    (calc-hyperbolic-prefix-help))
        (if calc-option-flag
            (calc-option-prefix-help)
          (setq calc-help-phase
                (if (eq this-command last-command)
                    (% (1+ calc-help-phase) (1+ (length msgs)))
                  0))
          (let ((msg (nth calc-help-phase msgs)))
            (message "%s" (if msg
                              (concat msg ":"
                                      (make-string (- (apply 'max
                                                             (mapcar 'length
                                                                     msgs))
                                                      (length msg)) 32)
                                      "  [?=MORE]")
                            ""))))))))




;;;; Stack and buffer management.

;; The variable calc-last-why-command is set in calc-do-handle-whys
;; and used in calc-why (in calc-stuff.el).
(defvar calc-last-why-command)

;;;###autoload
(defun calc-do-handle-whys ()
  (setq calc-why (sort calc-next-why
		       (function
			(lambda (x y)
			  (and (eq (car x) '*) (not (eq (car y) '*))))))
	calc-next-why nil)
  (if (and calc-why (or (eq calc-auto-why t)
			(and (eq (car (car calc-why)) '*)
			     calc-auto-why)))
      (progn
	(require 'calc-ext)
	(calc-explain-why (car calc-why)
			  (if (eq calc-auto-why t)
			      (cdr calc-why)
			    (if calc-auto-why
				(eq (car (nth 1 calc-why)) '*))))
	(setq calc-last-why-command this-command)
	(calc-clear-command-flag 'clear-message))))

;;;###autoload
(defun calc-record-why (&rest stuff)
  (if (eq (car stuff) 'quiet)
      (setq stuff (cdr stuff))
    (if (and (symbolp (car stuff))
	     (cdr stuff)
	     (or (Math-objectp (nth 1 stuff))
		 (and (Math-vectorp (nth 1 stuff))
		      (math-constp (nth 1 stuff)))
		 (math-infinitep (nth 1 stuff))))
	(setq stuff (cons '* stuff))
      (if (and (stringp (car stuff))
	       (string-match "\\`\\*" (car stuff)))
	  (setq stuff (cons '* (cons (substring (car stuff) 1)
				     (cdr stuff)))))))
  (setq calc-next-why (cons stuff calc-next-why))
  nil)

;; True if A is a constant or vector of constants.  [P x] [Public]
;;;###autoload
(defun math-constp (a)
  (or (Math-scalarp a)
      (and (memq (car a) '(sdev intv mod vec))
	   (progn
	     (while (and (setq a (cdr a))
			 (or (Math-scalarp (car a))  ; optimization
			     (math-constp (car a)))))
	     (null a)))))


;;;###autoload
(defun calc-roll-down-stack (n &optional m)
  (if (< n 0)
      (calc-roll-up-stack (- n) m)
    (if (or (= n 0) (> n (calc-stack-size))) (setq n (calc-stack-size)))
    (or m (setq m 1))
    (and (> n 1)
	 (< m n)
	 (if (and calc-any-selections
		  (not calc-use-selections))
	     (calc-roll-down-with-selections n m)
	   (calc-pop-push-list n
			       (append (calc-top-list m 1)
				       (calc-top-list (- n m) (1+ m))))))))

;;;###autoload
(defun calc-roll-up-stack (n &optional m)
  (if (< n 0)
      (calc-roll-down-stack (- n) m)
    (if (or (= n 0) (> n (calc-stack-size))) (setq n (calc-stack-size)))
    (or m (setq m 1))
    (and (> n 1)
	 (< m n)
	 (if (and calc-any-selections
		  (not calc-use-selections))
	     (calc-roll-up-with-selections n m)
	   (calc-pop-push-list n
			       (append (calc-top-list (- n m) 1)
				       (calc-top-list m (- n m -1))))))))


;;;###autoload
(defun calc-do-refresh ()
  (if calc-hyperbolic-flag
      (progn
	(setq calc-display-dirty t)
	nil)
    (calc-refresh)
    t))


;;;###autoload
(defun calc-record-list (vals &optional prefix)
  (while vals
    (or (eq (car vals) 'top-of-stack)
	(progn
	  (calc-record (car vals) prefix)
	  (setq prefix "...")))
    (setq vals (cdr vals))))


;;;###autoload
(defun calc-last-args-stub (arg)
  (interactive "p")
  (require 'calc-ext)
  (calc-last-args arg))


;;;###autoload
(defun calc-power (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (and (featurep 'calc-ext)
	    (calc-is-inverse))
       (calc-binary-op "root" 'calcFunc-nroot arg nil nil)
     (calc-binary-op "^" 'calcFunc-pow arg nil nil '^))))

;;;###autoload
(defun calc-mod (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-binary-op "%" 'calcFunc-mod arg nil nil '%)))

;;;###autoload
(defun calc-inv (arg)
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "inv" 'calcFunc-inv arg)))

;;;###autoload
(defun calc-percent ()
  (interactive)
  (calc-slow-wrapper
   (calc-pop-push-record-list
    1 "%" (list (list 'calcFunc-percent (calc-top-n 1))))))


;;;###autoload
(defun calc-over (n)
  (interactive "P")
  (if n
      (calc-enter (- (prefix-numeric-value n)))
    (calc-enter -2)))


;;;###autoload
(defun calc-pop-above (n)
  (interactive "P")
  (if n
      (calc-pop (- (prefix-numeric-value n)))
    (calc-pop -2)))

;;;###autoload
(defun calc-roll-down (n)
  (interactive "P")
  (calc-wrapper
   (let ((nn (prefix-numeric-value n)))
     (cond ((null n)
	    (calc-roll-down-stack 2))
	   ((> nn 0)
	    (calc-roll-down-stack nn))
	   ((= nn 0)
	    (calc-pop-push-list (calc-stack-size)
				(reverse
				 (calc-top-list (calc-stack-size)))))
	   (t
	    (calc-roll-down-stack (calc-stack-size) (- nn)))))))

;;;###autoload
(defun calc-roll-up (n)
  (interactive "P")
  (calc-wrapper
   (let ((nn (prefix-numeric-value n)))
     (cond ((null n)
	    (calc-roll-up-stack 3))
	   ((> nn 0)
	    (calc-roll-up-stack nn))
	   ((= nn 0)
	    (calc-pop-push-list (calc-stack-size)
				(reverse
				 (calc-top-list (calc-stack-size)))))
	   (t
	    (calc-roll-up-stack (calc-stack-size) (- nn)))))))

;;;###autoload
(defun calc-transpose-lines (&optional arg)
  "Transpose previous line and current line.
With argument ARG, move previous line past ARG lines.
With argument 0, switch line point is in with line mark is in."
  (interactive "p")
  (setq arg (or arg 1))
  (let (bot-line mid-line end-line
                 old-top-list new-top-list
                 bot-cell mid-cell
                 prev-mid-cell post-mid-cell post-bot-cell)
    (calc-wrapper
     (when (eq major-mode 'calc-mode)
       (cond
        ;; exchange point and mark
        ((= 0 arg)
         (setq bot-line  (calc-locate-cursor-element (point))
               mid-line  (mark))
         (if mid-line
             (setq mid-line  (calc-locate-cursor-element mid-line)
                   end-line  (1+ mid-line))
           (error "No mark set"))
         (if (< bot-line mid-line)
             (let ((temp mid-line))
               (setq mid-line bot-line
                     bot-line temp))))
        ;; move bot-line to mid-line that is above bot-line on stack (that is
        ;; to say mid-line displayed below bot-line in *Calculator* buffer)
        ((> arg 0)
         (setq bot-line (1+ (calc-locate-cursor-element (point)))
               mid-line (- bot-line arg)
               end-line mid-line))
        ;; move bot-line to mid-line that is above bot-line on stack (that is
        ;; to say mid-line displayed below bot-line in *Calculator* buffer)
        ((< arg 0)
         (setq mid-line (1+ (calc-locate-cursor-element (point)))
               bot-line (- mid-line arg)
               end-line bot-line)))
       (calc-check-stack bot-line)
       (if (= 0 mid-line)
           (error "Can't transpose beyond top"))
       (setq old-top-list (nreverse (calc-top-list bot-line)))
       ;; example: (arg = 2)
       ;; old-top-list =
       ;; 1 <-- top of stack (bottom of *Calculator* buffer)
       ;; 2
       ;; 3 <-- mid-line = 3
       ;; 4 <-- point
       ;; 5 <-- bot-line = 5
       (dotimes (i mid-line)
         (setq mid-cell old-top-list
               old-top-list (cdr old-top-list))
         (setcdr mid-cell new-top-list)
         (setq new-top-list  mid-cell))
       ;; example follow-up:
       ;; old-top-list =
       ;; 4
       ;; 5
       ;; new-top-list =
       ;; 3 <-- mid-cell
       ;; 2
       ;; 1
       (setq  prev-mid-cell old-top-list)
       (dotimes (i (- bot-line mid-line))
         (setq bot-cell old-top-list
               old-top-list (cdr old-top-list))
         (setcdr bot-cell new-top-list)
         (setq new-top-list  bot-cell))
       (setq post-mid-cell (cdr mid-cell)
             post-bot-cell  (cdr bot-cell))
       ;; example follow-up:
       ;; new-top-list =
       ;; 5 <-- bot-cell
       ;; 4 <-- prev-mid-cell & post-bot-cell
       ;; 3 <-- mid-cell
       ;; 2 <-- post-mid-cell
       ;; 1
       (cond
        ((= 0 arg); swap bot and mid
         (setcdr mid-cell            post-bot-cell)
         (setcdr bot-cell            post-mid-cell)
         (setcdr prev-mid-cell       bot-cell)
         ;; example follow-up:
         ;; 3 <-- mid-cell
         ;; 4 <-- post-bot-cell & prev-mid-cell
         ;; 5 <-- bot-cell
         ;; 2 <-- post-mid-cell
         ;; 1
         (setq new-top-list mid-cell))
        ((< 0 arg) ; move bot just after mid
         (setcdr mid-cell       bot-cell)
         (setcdr bot-cell       post-mid-cell)
         ;; example follow-up:
         ;; new-top-list =
         ;; 4 <-- post-bot-cell
         ;; 3 <-- mid-cell
         ;; 5 <-- bot-cell
         ;; 2 <-- post-mid-cell
         ;; 1
         (setq new-top-list post-bot-cell))
        ((> 0 arg) ; move mid just before bot
         (setcdr mid-cell       bot-cell)
         (setcdr prev-mid-cell  post-mid-cell)
         ;; example follow-up:
         ;; new-top-list =
         ;; 3 <-- mid-cell
         ;; 5 <-- bot-cell
         ;; 4 <-- prev-mid-cell
         ;; 2 <-- post-mid-cell
         ;; 1
         (setq new-top-list mid-cell)))
       (calc-pop-push-list bot-line new-top-list)))
    (calc-cursor-stack-index (1- end-line))))



;;; Other commands.

;;;###autoload
(defun calc-num-prefix-name (n)
  (cond ((eq n '-) "- ")
	((equal n '(4)) "C-u ")
	((consp n) (format "%d " (car n)))
	((integerp n) (format "%d " n))
	(t "")))

;;;###autoload
(defun calc-missing-key (n)
  "This is a placeholder for a command which needs to be loaded from calc-ext.
When this key is used, calc-ext (the Calculator extensions module) will be
loaded and the keystroke automatically re-typed."
  (interactive "P")
  (require 'calc-ext)
  (if (keymapp (key-binding (char-to-string last-command-event)))
      (message "%s%c-" (calc-num-prefix-name n) last-command-event))
  (calc-unread-command)
  (setq prefix-arg n))

;;;###autoload
(defun calc-shift-Y-prefix-help ()
  (interactive)
  (require 'calc-ext)
  (calc-do-prefix-help calc-Y-help-msgs "other" ?Y))




;;;###autoload
(defun calcDigit-letter ()
  (interactive)
  (if (calc-minibuffer-contains "[-+]?\\(1[1-9]\\|[2-9][0-9]\\)#.*")
      (progn
	(setq last-command-event (upcase last-command-event))
	(calcDigit-key))
    (calcDigit-nondigit)))


;; A Lisp version of temp_minibuffer_message from minibuf.c.
;;;###autoload
(defun calc-temp-minibuffer-message (m)
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((okay nil))
      (unwind-protect
	  (progn
	    (sit-for 2)
	    (identity 1)   ; this forces a call to QUIT; in bytecode.c.
	    (setq okay t))
	(progn
	  (delete-region savemax (point-max))
	  (or okay (abort-recursive-edit)))))))


(put 'math-with-extra-prec 'lisp-indent-hook 1)


;; Concatenate two vectors, or a vector and an object.  [V O O] [Public]
;;;###autoload
(defun math-concat (v1 v2)
  (if (stringp v1)
      (concat v1 v2)
    (require 'calc-ext)
    (if (and (or (math-objvecp v1) (math-known-scalarp v1))
	     (or (math-objvecp v2) (math-known-scalarp v2)))
	(append (if (and (math-vectorp v1)
			 (or (math-matrixp v1)
			     (not (math-matrixp v2))))
		    v1
		  (list 'vec v1))
		(if (and (math-vectorp v2)
			 (or (math-matrixp v2)
			     (not (math-matrixp v1))))
		    (cdr v2)
		  (list v2)))
      (list '| v1 v2))))


;; True if A is zero.  Works for un-normalized values.  [P n] [Public]
;;;###autoload
(defun math-zerop (a)
  (if (consp a)
      (cond ((memq (car a) '(bigpos bigneg))
	     (while (eq (car (setq a (cdr a))) 0))
	     (null a))
	    ((memq (car a) '(frac float polar mod))
	     (math-zerop (nth 1 a)))
	    ((eq (car a) 'cplx)
	     (and (math-zerop (nth 1 a)) (math-zerop (nth 2 a))))
	    ((eq (car a) 'hms)
	     (and (math-zerop (nth 1 a))
		  (math-zerop (nth 2 a))
		  (math-zerop (nth 3 a)))))
    (eq a 0)))


;; True if A is real and negative.  [P n] [Public]

;;;###autoload
(defun math-negp (a)
  (if (consp a)
      (cond ((eq (car a) 'bigpos) nil)
	    ((eq (car a) 'bigneg) (cdr a))
	    ((memq (car a) '(float frac))
	     (Math-integer-negp (nth 1 a)))
	    ((eq (car a) 'hms)
	     (if (math-zerop (nth 1 a))
		 (if (math-zerop (nth 2 a))
		     (math-negp (nth 3 a))
		   (math-negp (nth 2 a)))
	       (math-negp (nth 1 a))))
	    ((eq (car a) 'date)
	     (math-negp (nth 1 a)))
	    ((eq (car a) 'intv)
	     (or (math-negp (nth 3 a))
		 (and (math-zerop (nth 3 a))
		      (memq (nth 1 a) '(0 2)))))
	    ((equal a '(neg (var inf var-inf))) t))
    (< a 0)))

;; True if A is a negative number or an expression the starts with '-'.
;;;###autoload
(defun math-looks-negp (a)   ; [P x] [Public]
  (or (Math-negp a)
      (eq (car-safe a) 'neg)
      (and (memq (car-safe a) '(* /))
	   (or (math-looks-negp (nth 1 a))
	       (math-looks-negp (nth 2 a))))
      (and (eq (car-safe a) '-)
	   (math-looks-negp (nth 1 a)))))


;; True if A is real and positive.  [P n] [Public]
;;;###autoload
(defun math-posp (a)
  (if (consp a)
      (cond ((eq (car a) 'bigpos) (cdr a))
	    ((eq (car a) 'bigneg) nil)
	    ((memq (car a) '(float frac))
	     (Math-integer-posp (nth 1 a)))
	    ((eq (car a) 'hms)
	     (if (math-zerop (nth 1 a))
		 (if (math-zerop (nth 2 a))
		     (math-posp (nth 3 a))
		   (math-posp (nth 2 a)))
	       (math-posp (nth 1 a))))
	    ((eq (car a) 'date)
	     (math-posp (nth 1 a)))
	    ((eq (car a) 'mod)
	     (not (math-zerop (nth 1 a))))
	    ((eq (car a) 'intv)
	     (or (math-posp (nth 2 a))
		 (and (math-zerop (nth 2 a))
		      (memq (nth 1 a) '(0 1)))))
	    ((equal a '(var inf var-inf)) t))
    (> a 0)))

;;;###autoload
(defalias 'math-fixnump 'integerp)
;;;###autoload
(defalias 'math-fixnatnump 'natnump)


;; True if A is an even integer.  [P R R] [Public]
;;;###autoload
(defun math-evenp (a)
  (if (consp a)
      (and (memq (car a) '(bigpos bigneg))
	   (= (% (nth 1 a) 2) 0))
    (= (% a 2) 0)))

;; Compute A / 2, for small or big integer A.  [I i]
;; If A is negative, type of truncation is undefined.
;;;###autoload
(defun math-div2 (a)
  (if (consp a)
      (if (cdr a)
	  (math-normalize (cons (car a) (math-div2-bignum (cdr a))))
	0)
    (/ a 2)))

;;;###autoload
(defun math-div2-bignum (a)   ; [l l]
  (if (cdr a)
      (cons (+ (/ (car a) 2) (* (% (nth 1 a) 2) (/ math-bignum-digit-size 2)))
	    (math-div2-bignum (cdr a)))
    (list (/ (car a) 2))))


;; Reject an argument to a calculator function.  [Public]
;;;###autoload
(defun math-reject-arg (&optional a p option)
  (if option
      (calc-record-why option p a)
    (if p
	(calc-record-why p a)))
  (signal 'wrong-type-argument (and a (if p (list p a) (list a)))))


;; Coerce A to be an integer (by truncation toward zero).  [I N] [Public]

;; The variable math-trunc-prec is local to math-trunc, but used by
;; math-trunc-fancy in calc-arith.el, which is called by math-trunc.

;;;###autoload
(defun math-trunc (a &optional math-trunc-prec)
  (cond (math-trunc-prec
	 (require 'calc-ext)
	 (math-trunc-special a math-trunc-prec))
	((Math-integerp a) a)
	((Math-looks-negp a)
	 (math-neg (math-trunc (math-neg a))))
	((eq (car a) 'float)
	 (math-scale-int (nth 1 a) (nth 2 a)))
	(t (require 'calc-ext)
	   (math-trunc-fancy a))))
;;;###autoload
(defalias 'calcFunc-trunc 'math-trunc)

;; Coerce A to be an integer (by truncation toward minus infinity).  [I N]

;; The variable math-floor-prec is local to math-floor, but used by
;; math-floor-fancy in calc-arith.el, which is called by math-floor.

;;;###autoload
(defun math-floor (a &optional math-floor-prec)    ;  [Public]
  (cond (math-floor-prec
	 (require 'calc-ext)
	 (math-floor-special a math-floor-prec))
	((Math-integerp a) a)
	((Math-messy-integerp a) (math-trunc a))
	((Math-realp a)
	 (if (Math-negp a)
	     (math-add (math-trunc a) -1)
	   (math-trunc a)))
	(t (require 'calc-ext)
	   (math-floor-fancy a))))
;;;###autoload
(defalias 'calcFunc-floor 'math-floor)


;;;###autoload
(defun math-imod (a b)   ; [I I I] [Public]
  (if (and (not (consp a)) (not (consp b)))
      (if (= b 0)
	  (math-reject-arg a "*Division by zero")
	(% a b))
    (cdr (math-idivmod a b))))


;;;###autoload
(defun calcFunc-inv (m)
  (if (Math-vectorp m)
      (progn
	(require 'calc-ext)
	(if (math-square-matrixp m)
	    (or (math-with-extra-prec 2 (math-matrix-inv-raw m))
		(math-reject-arg m "*Singular matrix"))
	  (math-reject-arg m 'square-matrixp)))
    (if (and
         (require 'calc-arith)
         (math-known-matrixp m))
        (math-pow m -1)
      (math-div 1 m))))

;;;###autoload
(defun math-do-working (msg arg)
  (or executing-kbd-macro
      (progn
	(calc-set-command-flag 'clear-message)
	(if math-working-step
	    (if math-working-step-2
		(setq msg (format "[%d/%d] %s"
				  math-working-step math-working-step-2 msg))
	      (setq msg (format "[%d] %s" math-working-step msg))))
	(message "Working... %s = %s" msg
		 (math-showing-full-precision (math-format-number arg))))))


;; Compute A modulo B, defined in terms of truncation toward minus infinity.
;;;###autoload
(defun math-mod (a b)   ; [R R R] [Public]
  (cond ((and (Math-zerop a) (not (eq (car-safe a) 'mod))) a)
	((Math-zerop b)
	 (math-reject-arg a "*Division by zero"))
	((and (Math-natnump a) (Math-natnump b))
	 (math-imod a b))
	((and (Math-anglep a) (Math-anglep b))
	 (math-sub a (math-mul (math-floor (math-div a b)) b)))
	(t (require 'calc-ext)
	   (math-mod-fancy a b))))



;;; General exponentiation.

;;;###autoload
(defun math-pow (a b)   ; [O O N] [Public]
  (cond ((equal b '(var nan var-nan))
	 b)
	((Math-zerop a)
	 (if (and (Math-scalarp b) (Math-posp b))
	     (if (math-floatp b) (math-float a) a)
	   (require 'calc-ext)
	   (math-pow-of-zero a b)))
	((or (eq a 1) (eq b 1)) a)
	((or (equal a '(float 1 0)) (equal b '(float 1 0))) a)
	((Math-zerop b)
	 (if (Math-scalarp a)
	     (if (or (math-floatp a) (math-floatp b))
		 '(float 1 0) 1)
	   (require 'calc-ext)
	   (math-pow-zero a b)))
	((and (Math-integerp b) (or (Math-numberp a) (Math-vectorp a)))
	 (if (and (equal a '(float 1 1)) (integerp b))
	     (math-make-float 1 b)
	   (math-with-extra-prec 2
	     (math-ipow a b))))
	(t
	 (require 'calc-ext)
	 (math-pow-fancy a b))))

;;;###autoload
(defun math-ipow (a n)   ; [O O I] [Public]
  (cond ((Math-integer-negp n)
	 (math-ipow (math-div 1 a) (Math-integer-neg n)))
	((not (consp n))
	 (if (and (Math-ratp a) (> n 20))
	     (math-iipow-show a n)
	   (math-iipow a n)))
	((math-evenp n)
	 (math-ipow (math-mul a a) (math-div2 n)))
	(t
	 (math-mul a (math-ipow (math-mul a a)
				(math-div2 (math-add n -1)))))))

(defun math-iipow (a n)   ; [O O S]
  (cond ((= n 0) 1)
	((= n 1) a)
	((= (% n 2) 0) (math-iipow (math-mul a a) (/ n 2)))
	(t (math-mul a (math-iipow (math-mul a a) (/ n 2))))))

(defun math-iipow-show (a n)   ; [O O S]
  (math-working "pow" a)
  (let ((val (cond
	      ((= n 0) 1)
	      ((= n 1) a)
	      ((= (% n 2) 0) (math-iipow-show (math-mul a a) (/ n 2)))
	      (t (math-mul a (math-iipow-show (math-mul a a) (/ n 2)))))))
    (math-working "pow" val)
    val))


;;;###autoload
(defun math-read-radix-digit (dig)   ; [D S; Z S]
  (if (> dig ?9)
      (if (< dig ?A)
	  nil
	(- dig 55))
    (if (>= dig ?0)
	(- dig ?0)
      nil)))


;;; Bug reporting

;;;###autoload
(defun report-calc-bug ()
  "Report a bug in Calc, the GNU Emacs calculator.
Prompts for bug subject.  Leaves you in a mail buffer."
  (interactive)
  (let ((reporter-prompt-for-summary-p t))
    (reporter-submit-bug-report calc-bug-address "Calc"
				nil nil nil
				"Please describe exactly what actions triggered the bug and the
precise symptoms of the bug.  If possible, include a backtrace by
doing 'M-x toggle-debug-on-error', then reproducing the bug.
" )))
;;;###autoload
(defalias 'calc-report-bug 'report-calc-bug)

(provide 'calc-misc)

;; Local variables:
;; generated-autoload-file: "calc-loaddefs.el"
;; End:

;;; calc-misc.el ends here
