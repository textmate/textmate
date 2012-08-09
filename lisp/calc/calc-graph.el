;;; calc-graph.el --- graph output functions for Calc

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

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; Graphics

;; The following three variables are customizable and defined in calc.el.
(defvar calc-gnuplot-name)
(defvar calc-gnuplot-plot-command)
(defvar calc-gnuplot-print-command)

(defvar calc-gnuplot-tempfile "calc")

(defvar calc-gnuplot-default-device)
(defvar calc-gnuplot-default-output)
(defvar calc-gnuplot-print-device)
(defvar calc-gnuplot-print-output)
(defvar calc-gnuplot-keep-outfile nil)
(defvar calc-gnuplot-version nil)

(defvar calc-gnuplot-display (getenv "DISPLAY"))
(defvar calc-gnuplot-geometry)

(defvar calc-graph-default-resolution)
(defvar calc-graph-default-resolution-3d)
(defvar calc-graph-default-precision 5)

(defvar calc-gnuplot-buffer nil)
(defvar calc-gnuplot-input nil)

(defvar calc-gnuplot-last-error-pos 1)
(defvar calc-graph-last-device nil)
(defvar calc-graph-last-output nil)
(defvar calc-graph-file-cache nil)
(defvar calc-graph-var-cache nil)
(defvar calc-graph-data-cache nil)
(defvar calc-graph-data-cache-limit 10)
(defvar calc-graph-no-auto-view nil)
(defvar calc-graph-no-wait nil)
(defvar calc-gnuplot-trail-mark)

(defun calc-graph-fast (many)
  (interactive "P")
  (let ((calc-graph-no-auto-view t))
    (calc-graph-delete t)
    (calc-graph-add many)
    (calc-graph-plot nil)))

(defun calc-graph-fast-3d (many)
  (interactive "P")
  (let ((calc-graph-no-auto-view t))
    (calc-graph-delete t)
    (calc-graph-add-3d many)
    (calc-graph-plot nil)))

(defun calc-graph-delete (all)
  (interactive "P")
  (calc-wrapper
   (calc-graph-init)
   (with-current-buffer calc-gnuplot-input
     (and (calc-graph-find-plot t all)
	  (progn
	    (if (looking-at "s?plot")
		(progn
		  (setq calc-graph-var-cache nil)
		  (delete-region (point) (point-max)))
	      (delete-region (point) (1- (point-max)))))))
   (calc-graph-view-commands)))

(defun calc-graph-find-plot (&optional before all)
  (goto-char (point-min))
  (and (re-search-forward "^s?plot[ \t]+" nil t)
       (let ((beg (point)))
	 (goto-char (point-max))
	 (if (or all
		 (not (search-backward "," nil t))
		 (< (point) beg))
	     (progn
	       (goto-char beg)
	       (if before
		   (beginning-of-line)))
	   (or before
	       (re-search-forward ",[ \t]+")))
	 t)))

(defun calc-graph-add (many)
  (interactive "P")
  (calc-wrapper
   (calc-graph-init)
   (cond ((null many)
	  (calc-graph-add-curve (calc-graph-lookup (calc-top-n 2))
				(calc-graph-lookup (calc-top-n 1))))
	 ((or (consp many) (eq many 0))
	  (let ((xdata (calc-graph-lookup (calc-top-n 2)))
		(ylist (calc-top-n 1)))
	    (or (eq (car-safe ylist) 'vec)
		(error "Y argument must be a vector"))
	    (while (setq ylist (cdr ylist))
	      (calc-graph-add-curve xdata (calc-graph-lookup (car ylist))))))
	 ((> (setq many (prefix-numeric-value many)) 0)
	  (let ((xdata (calc-graph-lookup (calc-top-n (1+ many)))))
	    (while (> many 0)
	      (calc-graph-add-curve xdata
				    (calc-graph-lookup (calc-top-n many)))
	      (setq many (1- many)))))
	 (t
	  (let (pair)
	    (setq many (- many))
	    (while (> many 0)
	      (setq pair (calc-top-n many))
	      (or (and (eq (car-safe pair) 'vec)
		       (= (length pair) 3))
		  (error "Argument must be an [x,y] vector"))
	      (calc-graph-add-curve (calc-graph-lookup (nth 1 pair))
				    (calc-graph-lookup (nth 2 pair)))
	      (setq many (1- many))))))
   (calc-graph-view-commands)))

(defun calc-graph-add-3d (many)
  (interactive "P")
  (calc-wrapper
   (calc-graph-init)
   (cond ((null many)
	  (calc-graph-add-curve (calc-graph-lookup (calc-top-n 3))
				(calc-graph-lookup (calc-top-n 2))
				(calc-graph-lookup (calc-top-n 1))))
	 ((or (consp many) (eq many 0))
	  (let ((xdata (calc-graph-lookup (calc-top-n 3)))
		(ydata (calc-graph-lookup (calc-top-n 2)))
		(zlist (calc-top-n 1)))
	    (or (eq (car-safe zlist) 'vec)
		(error "Z argument must be a vector"))
	    (while (setq zlist (cdr zlist))
	      (calc-graph-add-curve xdata ydata
				    (calc-graph-lookup (car zlist))))))
	 ((> (setq many (prefix-numeric-value many)) 0)
	  (let ((xdata (calc-graph-lookup (calc-top-n (+ many 2))))
		(ydata (calc-graph-lookup (calc-top-n (+ many 1)))))
	    (while (> many 0)
	      (calc-graph-add-curve xdata ydata
				    (calc-graph-lookup (calc-top-n many)))
	      (setq many (1- many)))))
	 (t
	  (let (curve)
	    (setq many (- many))
	    (while (> many 0)
	      (setq curve (calc-top-n many))
	      (or (and (eq (car-safe curve) 'vec)
		       (= (length curve) 4))
		  (error "Argument must be an [x,y,z] vector"))
	      (calc-graph-add-curve (calc-graph-lookup (nth 1 curve))
				    (calc-graph-lookup (nth 2 curve))
				    (calc-graph-lookup (nth 3 curve)))
	      (setq many (1- many))))))
   (calc-graph-view-commands)))

(defun calc-graph-add-curve (xdata ydata &optional zdata)
  (let ((num (calc-graph-count-curves))
	(pstyle (calc-var-value 'var-PointStyles))
	(lstyle (calc-var-value 'var-LineStyles)))
    (with-current-buffer calc-gnuplot-input
      (goto-char (point-min))
      (if (re-search-forward (if zdata "^plot[ \t]" "^splot[ \t]")
			     nil t)
	  (error "Can't mix 2d and 3d curves on one graph"))
      (if (re-search-forward "^s?plot[ \t]" nil t)
	  (progn
	    (end-of-line)
	    (insert ", "))
	(goto-char (point-max))
	(or (eq (preceding-char) ?\n)
	    (insert "\n"))
	(insert (if zdata "splot" "plot") " \n")
	(forward-char -1))
      (insert "{" (symbol-name (nth 1 xdata))
	      ":" (symbol-name (nth 1 ydata)))
      (if zdata
	  (insert ":" (symbol-name (nth 1 zdata))))
      (insert "} "
	      "title \"" (symbol-name (nth 1 ydata)) "\" "
	      "with dots")
      (setq pstyle (and (eq (car-safe pstyle) 'vec) (nth (1+ num) pstyle)))
      (setq lstyle (and (eq (car-safe lstyle) 'vec) (nth (1+ num) lstyle))))
    (calc-graph-set-styles
     (or (and (Math-num-integerp lstyle) (math-trunc lstyle))
         0)
     (or (and (Math-num-integerp pstyle) (math-trunc pstyle))
         (if (eq (car-safe (calc-var-value (nth 2 ydata))) 'vec)
             0 -1))
     (math-contains-sdev-p (eval (nth 2 ydata))))))

(defun calc-graph-lookup (thing)
  (if (and (eq (car-safe thing) 'var)
	   (calc-var-value (nth 2 thing)))
      thing
    (let ((found (assoc thing calc-graph-var-cache)))
      (or found
	  (let ((varname (concat "PlotData"
                                 (int-to-string
                                  (1+ (length calc-graph-var-cache)))))
		var)
            (setq var (list 'var (intern varname)
			    (intern (concat "var-" varname)))
		  found (cons thing var)
		  calc-graph-var-cache (cons found calc-graph-var-cache))
	    (set (nth 2 var) thing)))
      (cdr found))))

(defun calc-graph-juggle (arg)
  (interactive "p")
  (calc-graph-init)
  (with-current-buffer calc-gnuplot-input
    (if (< arg 0)
	(let ((num (calc-graph-count-curves)))
	  (if (> num 0)
	      (while (< arg 0)
		(setq arg (+ arg num))))))
    (while (>= (setq arg (1- arg)) 0)
      (calc-graph-do-juggle))))

(defun calc-graph-count-curves ()
  (with-current-buffer calc-gnuplot-input
    (if (re-search-forward "^s?plot[ \t]" nil t)
	(let ((num 1))
	  (goto-char (point-min))
	  (while (search-forward "," nil t)
	    (setq num (1+ num)))
	  num)
      0)))

(defun calc-graph-do-juggle ()
  (let (base)
    (and (calc-graph-find-plot t t)
	 (progn
	   (setq base (point))
	   (calc-graph-find-plot t nil)
	   (or (eq base (point))
	       (let ((str (buffer-substring (+ (point) 2) (1- (point-max)))))
		 (delete-region (point) (1- (point-max)))
		 (goto-char (+ base 5))
		 (insert str ", ")))))))

(defun calc-graph-print (flag)
  (interactive "P")
  (calc-graph-plot flag t))

(defvar var-DUMMY)
(defvar var-DUMMY2)
(defvar var-PlotRejects)

;; The following variables are local to calc-graph-plot, but are
;; used in the functions calc-graph-compute-2d, calc-graph-refine-2d,
;; calc-graph-recompute-2d, calc-graph-compute-3d and
;; calc-graph-format-data, which are called by calc-graph-plot.
(defvar calc-graph-yvalue)
(defvar calc-graph-yvec)
(defvar calc-graph-numsteps)
(defvar calc-graph-numsteps3)
(defvar calc-graph-xvalue)
(defvar calc-graph-xvec)
(defvar calc-graph-xname)
(defvar calc-graph-yname)
(defvar calc-graph-xstep)
(defvar calc-graph-ycache)
(defvar calc-graph-ycacheptr)
(defvar calc-graph-refine)
(defvar calc-graph-keep-file)
(defvar calc-graph-xval)
(defvar calc-graph-xlow)
(defvar calc-graph-xhigh)
(defvar calc-graph-yval)
(defvar calc-graph-yp)
(defvar calc-graph-xp)
(defvar calc-graph-zp)
(defvar calc-graph-yvector)
(defvar calc-graph-resolution)
(defvar calc-graph-y3value)
(defvar calc-graph-y3name)
(defvar calc-graph-y3step)
(defvar calc-graph-zval)
(defvar calc-graph-stepcount)
(defvar calc-graph-is-splot)
(defvar calc-graph-surprise-splot)
(defvar calc-graph-blank)
(defvar calc-graph-non-blank)
(defvar calc-graph-curve-num)

(defun calc-graph-plot (flag &optional printing)
  (interactive "P")
  (calc-slow-wrapper
   (let ((calcbuf (current-buffer))
	 (tempbuf (get-buffer-create "*Gnuplot Temp-2*"))
	 (tempbuftop 1)
	 (tempoutfile nil)
	 (calc-graph-curve-num 0)
	 (calc-graph-refine (and flag (> (prefix-numeric-value flag) 0)))
	 (recompute (and flag (< (prefix-numeric-value flag) 0)))
	 (calc-graph-surprise-splot nil)
	 (tty-output nil)
	 cache-env calc-graph-is-splot device output calc-graph-resolution precision samples-pos)
     (add-hook 'kill-emacs-hook 'calc-graph-kill-hook)
     (save-excursion
       (calc-graph-init)
       (set-buffer tempbuf)
       (erase-buffer)
       (set-buffer calc-gnuplot-input)
       (goto-char (point-min))
       (setq calc-graph-is-splot (re-search-forward "^splot[ \t]" nil t))
       (let ((str (buffer-string))
	     (ver calc-gnuplot-version))
	 (set-buffer (get-buffer-create "*Gnuplot Temp*"))
	 (erase-buffer)
	 (insert "# (Note: This is a temporary copy---do not edit!)\n")
	 (if (>= ver 2)
	     (insert "set noarrow\nset nolabel\n"
		     "set autoscale xy\nset nologscale xy\n"
		     "set xlabel\nset ylabel\nset title\n"
		     "set noclip points\nset clip one\nset clip two\n"
		     "set format \"%g\"\nset tics\nset xtics\nset ytics\n"
		     "set style data linespoints\n"
		     "set nogrid\nset nokey\nset nopolar\n"))
	 (if (>= ver 3)
	     (insert "set surface\nset nocontour\n"
		     "set " (if calc-graph-is-splot "" "no") "parametric\n"
		     "set notime\nset border\nset ztics\nset zeroaxis\n"
		     "set view 60,30,1,1\nset offsets 0,0,0,0\n"))
	 (setq samples-pos (point))
	 (insert "\n\n" str))
       (goto-char (point-min))
       (if calc-graph-is-splot
	   (if calc-graph-refine
	       (error "This option works only for 2d plots")
	     (setq recompute t)))
       (let ((calc-gnuplot-input (current-buffer))
	     (calc-graph-no-auto-view t))
	 (if printing
	     (setq device calc-gnuplot-print-device
		   output calc-gnuplot-print-output)
	   (setq device (calc-graph-find-command "terminal")
		 output (calc-graph-find-command "output"))
	   (or device
	       (setq device calc-gnuplot-default-device))
	   (if output
	       (setq output (car (read-from-string output)))
	     (setq output calc-gnuplot-default-output)))
	 (if (or (equal device "") (equal device "default"))
	     (setq device
		   (cond
		    (printing "postscript")
		    ;; Check MS-Windows before X, in case they have
		    ;; $DISPLAY set for some reason (e.g., Cygwin or
		    ;; whatever)
		    ((string= calc-gnuplot-name "pgnuplot")
		     "windows")
		    ((or (eq window-system 'x) (getenv "DISPLAY"))
		     "x11")
		    ((>= calc-gnuplot-version 3)
		     "dumb")
		    (t "postscript"))))
	 (if (equal device "dumb")
	     (setq device (format "dumb %d %d"
				  (1- (frame-width)) (1- (frame-height)))))
	 (if (equal device "big")
	     (setq device (format "dumb %d %d"
				  (* 4 (- (frame-width) 3))
				  (* 4 (- (frame-height) 3)))))
	 (if (stringp output)
	     (if (or (equal output "auto")
		     (and (equal output "tty") (setq tty-output t)))
		 (setq tempoutfile (calc-temp-file-name -1)
		       output tempoutfile))
	   (setq output (eval output)))
	 (or (equal device calc-graph-last-device)
	     (progn
	       (setq calc-graph-last-device device)
	       (calc-gnuplot-command "set terminal" device)))
	 (or (equal output calc-graph-last-output)
	     (progn
	       (setq calc-graph-last-output output)
	       (calc-gnuplot-command "set output"
				     (if (equal output "STDOUT")
					 ""
				       (prin1-to-string output)))))
	 (setq calc-graph-resolution (calc-graph-find-command "samples"))
	 (if calc-graph-resolution
	     (setq calc-graph-resolution (string-to-number calc-graph-resolution))
	   (setq calc-graph-resolution (if calc-graph-is-splot
				calc-graph-default-resolution-3d
			      calc-graph-default-resolution)))
	 (setq precision (calc-graph-find-command "precision"))
	 (if precision
	     (setq precision (string-to-number precision))
	   (setq precision calc-graph-default-precision))
	 (calc-graph-set-command "terminal")
	 (calc-graph-set-command "output")
	 (calc-graph-set-command "samples")
	 (calc-graph-set-command "precision"))
       (goto-char samples-pos)
       (insert "set samples " (int-to-string (max (if calc-graph-is-splot 20 200)
						  (+ 5 calc-graph-resolution))) "\n")
       (while (re-search-forward "{\\*[^}]+}[^,\n]*" nil t)
	 (delete-region (match-beginning 0) (match-end 0))
	 (if (looking-at ",")
	     (delete-char 1)
	   (while (memq (preceding-char) '(?\s ?\t))
	     (forward-char -1))
	   (if (eq (preceding-char) ?\,)
	       (delete-char -1))))
       (with-current-buffer calcbuf
	 (setq cache-env (list calc-angle-mode
			       calc-complex-mode
			       calc-simplify-mode
			       calc-infinite-mode
			       calc-word-size
			       precision calc-graph-is-splot))
	 (if (and (not recompute)
		  (equal (cdr (car calc-graph-data-cache)) cache-env))
	     (while (> (length calc-graph-data-cache)
		       calc-graph-data-cache-limit)
	       (setcdr calc-graph-data-cache
		       (cdr (cdr calc-graph-data-cache))))
	   (setq calc-graph-data-cache (list (cons nil cache-env)))))
       (calc-graph-find-plot t t)
       (while (re-search-forward
	       (if calc-graph-is-splot
		   "{\\([^{}:\n]+\\):\\([^{}:\n]+\\):\\([^{}:\n]+\\)}"
		 "{\\([^{}:\n]+\\)\\(:\\)\\([^{}:\n]+\\)}")
	       nil t)
	 (setq calc-graph-curve-num (1+ calc-graph-curve-num))
	 (let* ((calc-graph-xname (buffer-substring (match-beginning 1) (match-end 1)))
		(xvar (intern (concat "var-" calc-graph-xname)))
		(calc-graph-xvalue (math-evaluate-expr (calc-var-value xvar)))
		(calc-graph-y3name (and calc-graph-is-splot
			     (buffer-substring (match-beginning 2)
					       (match-end 2))))
		(y3var (and calc-graph-is-splot (intern (concat "var-" calc-graph-y3name))))
		(calc-graph-y3value (and calc-graph-is-splot (calc-var-value y3var)))
		(calc-graph-yname (buffer-substring (match-beginning 3) (match-end 3)))
		(yvar (intern (concat "var-" calc-graph-yname)))
		(calc-graph-yvalue (calc-var-value yvar))
		filename)
	   (delete-region (match-beginning 0) (match-end 0))
	   (setq filename (calc-temp-file-name calc-graph-curve-num))
	   (with-current-buffer calcbuf
	     (let (tempbuftop
		   (calc-graph-xp calc-graph-xvalue)
		   (calc-graph-yp calc-graph-yvalue)
		   (calc-graph-zp nil)
		   (calc-graph-xlow nil) (calc-graph-xhigh nil) (y3low nil) (y3high nil)
		   calc-graph-xvec calc-graph-xval calc-graph-xstep var-DUMMY
		   y3val calc-graph-y3step var-DUMMY2 (calc-graph-zval nil)
		   calc-graph-yvec calc-graph-yval calc-graph-ycache calc-graph-ycacheptr calc-graph-yvector
		   calc-graph-numsteps calc-graph-numsteps3
		   (calc-graph-keep-file (and (not calc-graph-is-splot) (file-exists-p filename)))
		   (calc-graph-stepcount 0)
		   (calc-symbolic-mode nil)
		   (calc-prefer-frac nil)
		   (calc-internal-prec (max 3 precision))
		   (calc-simplify-mode (and (not (memq calc-simplify-mode
						       '(none num)))
					    calc-simplify-mode))
		   (calc-graph-blank t)
		   (calc-graph-non-blank nil)
		   (math-working-step 0)
		   (math-working-step-2 nil))
	       (save-excursion
		 (if calc-graph-is-splot
		     (calc-graph-compute-3d)
		   (calc-graph-compute-2d))
		 (set-buffer tempbuf)
		 (goto-char (point-max))
		 (insert "\n" calc-graph-xname)
		 (if calc-graph-is-splot
		     (insert ":" calc-graph-y3name))
		 (insert ":" calc-graph-yname "\n\n")
		 (setq tempbuftop (point))
		 (let ((calc-group-digits nil)
		       (calc-leading-zeros nil)
		       (calc-number-radix 10)
                       (calc-twos-complement-mode nil)
		       (entry (and (not calc-graph-is-splot)
				   (list calc-graph-xp calc-graph-yp calc-graph-xhigh calc-graph-numsteps))))
		   (or (equal entry
			      (nth 1 (nth (1+ calc-graph-curve-num)
					  calc-graph-file-cache)))
		       (setq calc-graph-keep-file nil))
		   (setcar (cdr (nth (1+ calc-graph-curve-num) calc-graph-file-cache))
			   entry)
		   (or calc-graph-keep-file
		       (calc-graph-format-data)))
		 (or calc-graph-keep-file
		     (progn
		       (or calc-graph-non-blank
			   (error "No valid data points for %s:%s"
				  calc-graph-xname calc-graph-yname))
		       (write-region tempbuftop (point-max) filename
				     nil 'quiet))))))
	   (insert (prin1-to-string filename))))
       (if calc-graph-surprise-splot
	   (setcdr cache-env nil))
       (if (= calc-graph-curve-num 0)
	   (progn
	     (calc-gnuplot-command "clear")
	     (calc-clear-command-flag 'clear-message)
	     (message "No data to plot!"))
	 (setq calc-graph-data-cache-limit (max calc-graph-curve-num
						calc-graph-data-cache-limit))
	 (let ((filename (calc-temp-file-name 0)))
	   (write-region (point-min) (point-max) filename nil 'quiet)
	   (calc-gnuplot-command "load" (prin1-to-string filename)))
	 (or (equal output "STDOUT")
	     calc-gnuplot-keep-outfile
	     (progn   ; need to close the output file before printing/plotting
	       (setq calc-graph-last-output "STDOUT")
	       (calc-gnuplot-command "set output")))
	 (let ((command (if printing
			    calc-gnuplot-print-command
			  (or calc-gnuplot-plot-command
			      (and (string-match "^dumb" device)
				   'calc-graph-show-dumb)
			      (and tty-output
				   'calc-graph-show-tty)))))
	   (if command
	       (if (stringp command)
		   (calc-gnuplot-command
		    "!" (format command
				(or tempoutfile
				    calc-gnuplot-print-output)))
		 (if (symbolp command)
		     (funcall command output)
		   (eval command))))))))))

(defun calc-graph-compute-2d ()
  (if (setq calc-graph-yvec (eq (car-safe calc-graph-yvalue) 'vec))
      (if (= (setq calc-graph-numsteps (1- (length calc-graph-yvalue))) 0)
	  (error "Can't plot an empty vector")
	(if (setq calc-graph-xvec (eq (car-safe calc-graph-xvalue) 'vec))
	    (or (= (1- (length calc-graph-xvalue)) calc-graph-numsteps)
		(error "%s and %s have different lengths" calc-graph-xname calc-graph-yname))
	  (if (and (eq (car-safe calc-graph-xvalue) 'intv)
		   (math-constp calc-graph-xvalue))
	      (setq calc-graph-xstep (math-div (math-sub (nth 3 calc-graph-xvalue)
					      (nth 2 calc-graph-xvalue))
				    (1- calc-graph-numsteps))
		    calc-graph-xvalue (nth 2 calc-graph-xvalue))
	    (if (math-realp calc-graph-xvalue)
		(setq calc-graph-xstep 1)
	      (error "%s is not a suitable basis for %s" calc-graph-xname calc-graph-yname)))))
    (or (math-realp calc-graph-yvalue)
	(let ((math-arglist nil))
	  (setq calc-graph-yvalue (math-evaluate-expr calc-graph-yvalue))
	  (calc-default-formula-arglist calc-graph-yvalue)
	  (or math-arglist
	      (error "%s does not contain any unassigned variables" calc-graph-yname))
	  (and (cdr math-arglist)
	       (error "%s contains more than one variable: %s"
		      calc-graph-yname math-arglist))
	  (setq calc-graph-yvalue (math-expr-subst calc-graph-yvalue
					(math-build-var-name (car math-arglist))
					'(var DUMMY var-DUMMY)))))
    (setq calc-graph-ycache (assoc calc-graph-yvalue calc-graph-data-cache))
    (delq calc-graph-ycache calc-graph-data-cache)
    (nconc calc-graph-data-cache
	   (list (or calc-graph-ycache (setq calc-graph-ycache (list calc-graph-yvalue)))))
    (if (and (not (setq calc-graph-xvec (eq (car-safe calc-graph-xvalue) 'vec)))
	     calc-graph-refine (cdr (cdr calc-graph-ycache)))
	(calc-graph-refine-2d)
      (calc-graph-recompute-2d))))

(defun calc-graph-refine-2d ()
  (setq calc-graph-keep-file nil
	calc-graph-ycacheptr (cdr calc-graph-ycache))
  (if (and (setq calc-graph-xval (calc-graph-find-command "xrange"))
	   (string-match "\\`\\[\\([0-9.eE+-]*\\):\\([0-9.eE+-]*\\)\\]\\'"
			 calc-graph-xval))
      (let ((b2 (match-beginning 2))
	    (e2 (match-end 2)))
	(setq calc-graph-xlow (math-read-number (substring calc-graph-xval
						(match-beginning 1)
						(match-end 1)))
	      calc-graph-xhigh (math-read-number (substring calc-graph-xval b2 e2))))
    (if calc-graph-xlow
	(while (and (cdr calc-graph-ycacheptr)
		    (Math-lessp (car (nth 1 calc-graph-ycacheptr)) calc-graph-xlow))
	  (setq calc-graph-ycacheptr (cdr calc-graph-ycacheptr)))))
  (setq math-working-step-2 (1- (length calc-graph-ycacheptr)))
  (while (and (cdr calc-graph-ycacheptr)
	      (or (not calc-graph-xhigh)
		  (Math-lessp (car (car calc-graph-ycacheptr)) calc-graph-xhigh)))
    (setq var-DUMMY (math-div (math-add (car (car calc-graph-ycacheptr))
					(car (nth 1 calc-graph-ycacheptr)))
			      2)
	  math-working-step (1+ math-working-step)
	  calc-graph-yval (math-evaluate-expr calc-graph-yvalue))
    (setcdr calc-graph-ycacheptr (cons (cons var-DUMMY calc-graph-yval)
			    (cdr calc-graph-ycacheptr)))
    (setq calc-graph-ycacheptr (cdr (cdr calc-graph-ycacheptr))))
  (setq calc-graph-yp calc-graph-ycache
	calc-graph-numsteps 1000000))

(defun calc-graph-recompute-2d ()
  (setq calc-graph-ycacheptr calc-graph-ycache)
  (if calc-graph-xvec
      (setq calc-graph-numsteps (1- (length calc-graph-xvalue))
	    calc-graph-yvector nil)
    (if (and (eq (car-safe calc-graph-xvalue) 'intv)
	     (math-constp calc-graph-xvalue))
	(setq calc-graph-numsteps calc-graph-resolution
	      calc-graph-yp nil
	      calc-graph-xlow (nth 2 calc-graph-xvalue)
	      calc-graph-xhigh (nth 3 calc-graph-xvalue)
	      calc-graph-xstep (math-div (math-sub calc-graph-xhigh calc-graph-xlow)
			      (1- calc-graph-numsteps))
	      calc-graph-xvalue (nth 2 calc-graph-xvalue))
      (error "%s is not a suitable basis for %s"
	     calc-graph-xname calc-graph-yname)))
  (setq math-working-step-2 calc-graph-numsteps)
  (while (>= (setq calc-graph-numsteps (1- calc-graph-numsteps)) 0)
    (setq math-working-step (1+ math-working-step))
    (if calc-graph-xvec
	(progn
	  (setq calc-graph-xp (cdr calc-graph-xp)
		calc-graph-xval (car calc-graph-xp))
	  (and (not (eq calc-graph-ycacheptr calc-graph-ycache))
	       (consp (car calc-graph-ycacheptr))
	       (not (Math-lessp (car (car calc-graph-ycacheptr)) calc-graph-xval))
	       (setq calc-graph-ycacheptr calc-graph-ycache)))
      (if (= calc-graph-numsteps 0)
	  (setq calc-graph-xval calc-graph-xhigh)   ; avoid cumulative roundoff
	(setq calc-graph-xval calc-graph-xvalue
	      calc-graph-xvalue (math-add calc-graph-xvalue calc-graph-xstep))))
    (while (and (cdr calc-graph-ycacheptr)
		(Math-lessp (car (nth 1 calc-graph-ycacheptr)) calc-graph-xval))
      (setq calc-graph-ycacheptr (cdr calc-graph-ycacheptr)))
    (or (and (cdr calc-graph-ycacheptr)
	     (Math-equal (car (nth 1 calc-graph-ycacheptr)) calc-graph-xval))
	(progn
	  (setq calc-graph-keep-file nil
		var-DUMMY calc-graph-xval)
	  (setcdr calc-graph-ycacheptr (cons (cons calc-graph-xval (math-evaluate-expr calc-graph-yvalue))
				  (cdr calc-graph-ycacheptr)))))
    (setq calc-graph-ycacheptr (cdr calc-graph-ycacheptr))
    (if calc-graph-xvec
	(setq calc-graph-yvector (cons (cdr (car calc-graph-ycacheptr)) calc-graph-yvector))
      (or calc-graph-yp (setq calc-graph-yp calc-graph-ycacheptr))))
  (if calc-graph-xvec
      (setq calc-graph-xp calc-graph-xvalue
	    calc-graph-yvec t
	    calc-graph-yp (cons 'vec (nreverse calc-graph-yvector))
	    calc-graph-numsteps (1- (length calc-graph-xp)))
    (setq calc-graph-numsteps 1000000)))

(defun calc-graph-compute-3d ()
  (if (setq calc-graph-yvec (eq (car-safe calc-graph-yvalue) 'vec))
      (if (math-matrixp calc-graph-yvalue)
	  (progn
	    (setq calc-graph-numsteps (1- (length calc-graph-yvalue))
		  calc-graph-numsteps3 (1- (length (nth 1 calc-graph-yvalue))))
	    (if (eq (car-safe calc-graph-xvalue) 'vec)
		(or (= (1- (length calc-graph-xvalue)) calc-graph-numsteps)
		    (error "%s has wrong length" calc-graph-xname))
	      (if (and (eq (car-safe calc-graph-xvalue) 'intv)
		       (math-constp calc-graph-xvalue))
		  (setq calc-graph-xvalue (calcFunc-index calc-graph-numsteps
					       (nth 2 calc-graph-xvalue)
					       (math-div
						(math-sub (nth 3 calc-graph-xvalue)
							  (nth 2 calc-graph-xvalue))
						(1- calc-graph-numsteps))))
		(if (math-realp calc-graph-xvalue)
		    (setq calc-graph-xvalue (calcFunc-index calc-graph-numsteps calc-graph-xvalue 1))
		  (error "%s is not a suitable basis for %s" calc-graph-xname calc-graph-yname))))
	    (if (eq (car-safe calc-graph-y3value) 'vec)
		(or (= (1- (length calc-graph-y3value)) calc-graph-numsteps3)
		    (error "%s has wrong length" calc-graph-y3name))
	      (if (and (eq (car-safe calc-graph-y3value) 'intv)
		       (math-constp calc-graph-y3value))
		  (setq calc-graph-y3value (calcFunc-index calc-graph-numsteps3
						(nth 2 calc-graph-y3value)
						(math-div
						 (math-sub (nth 3 calc-graph-y3value)
							   (nth 2 calc-graph-y3value))
						 (1- calc-graph-numsteps3))))
		(if (math-realp calc-graph-y3value)
		    (setq calc-graph-y3value (calcFunc-index calc-graph-numsteps3 calc-graph-y3value 1))
		  (error "%s is not a suitable basis for %s" calc-graph-y3name calc-graph-yname))))
	    (setq calc-graph-xp nil
		  calc-graph-yp nil
		  calc-graph-zp nil
		  calc-graph-xvec t)
	    (while (setq calc-graph-xvalue (cdr calc-graph-xvalue) calc-graph-yvalue (cdr calc-graph-yvalue))
	      (setq calc-graph-xp (nconc calc-graph-xp (make-list (1+ calc-graph-numsteps3) (car calc-graph-xvalue)))
		    calc-graph-yp (nconc calc-graph-yp (cons 0 (copy-sequence (cdr calc-graph-y3value))))
		    calc-graph-zp (nconc calc-graph-zp (cons '(skip)
				       (copy-sequence (cdr (car calc-graph-yvalue)))))))
	    (setq calc-graph-numsteps (1- (* calc-graph-numsteps
                                             (1+ calc-graph-numsteps3)))))
	(if (= (setq calc-graph-numsteps (1- (length calc-graph-yvalue))) 0)
	    (error "Can't plot an empty vector"))
	(or (and (eq (car-safe calc-graph-xvalue) 'vec)
		 (= (1- (length calc-graph-xvalue)) calc-graph-numsteps))
	    (error "%s is not a suitable basis for %s" calc-graph-xname calc-graph-yname))
	(or (and (eq (car-safe calc-graph-y3value) 'vec)
		 (= (1- (length calc-graph-y3value)) calc-graph-numsteps))
	    (error "%s is not a suitable basis for %s" calc-graph-y3name calc-graph-yname))
	(setq calc-graph-xp calc-graph-xvalue
	      calc-graph-yp calc-graph-y3value
	      calc-graph-zp calc-graph-yvalue
	      calc-graph-xvec t))
    (or (math-realp calc-graph-yvalue)
	(let ((math-arglist nil))
	  (setq calc-graph-yvalue (math-evaluate-expr calc-graph-yvalue))
	  (calc-default-formula-arglist calc-graph-yvalue)
	  (setq math-arglist (sort math-arglist 'string-lessp))
	  (or (cdr math-arglist)
	      (error "%s does not contain enough unassigned variables" calc-graph-yname))
	  (and (cdr (cdr math-arglist))
	       (error "%s contains too many variables: %s" calc-graph-yname math-arglist))
	  (setq calc-graph-yvalue (math-multi-subst calc-graph-yvalue
					 (mapcar 'math-build-var-name
						 math-arglist)
					 '((var DUMMY var-DUMMY)
					   (var DUMMY2 var-DUMMY2))))))
    (if (setq calc-graph-xvec (eq (car-safe calc-graph-xvalue) 'vec))
	(setq calc-graph-numsteps (1- (length calc-graph-xvalue)))
      (if (and (eq (car-safe calc-graph-xvalue) 'intv)
	       (math-constp calc-graph-xvalue))
	  (setq calc-graph-numsteps calc-graph-resolution
		calc-graph-xvalue (calcFunc-index calc-graph-numsteps
				       (nth 2 calc-graph-xvalue)
				       (math-div (math-sub (nth 3 calc-graph-xvalue)
							   (nth 2 calc-graph-xvalue))
						 (1- calc-graph-numsteps))))
	(error "%s is not a suitable basis for %s"
	       calc-graph-xname calc-graph-yname)))
    (if (eq (car-safe calc-graph-y3value) 'vec)
	(setq calc-graph-numsteps3 (1- (length calc-graph-y3value)))
      (if (and (eq (car-safe calc-graph-y3value) 'intv)
	       (math-constp calc-graph-y3value))
	  (setq calc-graph-numsteps3 calc-graph-resolution
		calc-graph-y3value (calcFunc-index calc-graph-numsteps3
					(nth 2 calc-graph-y3value)
					(math-div (math-sub (nth 3 calc-graph-y3value)
							    (nth 2 calc-graph-y3value))
						  (1- calc-graph-numsteps3))))
	(error "%s is not a suitable basis for %s"
	       calc-graph-y3name calc-graph-yname)))
    (setq calc-graph-xp nil
	  calc-graph-yp nil
	  calc-graph-zp nil
	  calc-graph-xvec t)
    (setq math-working-step 0)
    (while (setq calc-graph-xvalue (cdr calc-graph-xvalue))
      (setq calc-graph-xp (nconc calc-graph-xp (make-list (1+ calc-graph-numsteps3) (car calc-graph-xvalue)))
	    calc-graph-yp (nconc calc-graph-yp (cons 0 (copy-sequence (cdr calc-graph-y3value))))
	    calc-graph-zp (cons '(skip) calc-graph-zp)
	    calc-graph-y3step calc-graph-y3value
	    var-DUMMY (car calc-graph-xvalue)
	    math-working-step-2 0
	    math-working-step (1+ math-working-step))
      (while (setq calc-graph-y3step (cdr calc-graph-y3step))
	(setq math-working-step-2 (1+ math-working-step-2)
	      var-DUMMY2 (car calc-graph-y3step)
	      calc-graph-zp (cons (math-evaluate-expr calc-graph-yvalue) calc-graph-zp))))
    (setq calc-graph-zp (nreverse calc-graph-zp)
	  calc-graph-numsteps (1- (* calc-graph-numsteps (1+ calc-graph-numsteps3))))))

(defun calc-graph-format-data ()
  (if (math-contains-sdev-p calc-graph-yp)
      (let ((yp calc-graph-yp))
        (setq calc-graph-yp (cons 'vec (mapcar 'math-get-value (cdr yp))))
        (setq calc-graph-zp (cons 'vec (mapcar 'math-get-sdev (cdr yp))))))
  (while (<= (setq calc-graph-stepcount (1+ calc-graph-stepcount)) calc-graph-numsteps)
    (if calc-graph-xvec
	(setq calc-graph-xp (cdr calc-graph-xp)
	      calc-graph-xval (car calc-graph-xp)
	      calc-graph-yp (cdr calc-graph-yp)
	      calc-graph-yval (car calc-graph-yp)
	      calc-graph-zp (cdr calc-graph-zp)
	      calc-graph-zval (car calc-graph-zp))
      (if calc-graph-yvec
	  (setq calc-graph-xval calc-graph-xvalue
		calc-graph-xvalue (math-add calc-graph-xvalue calc-graph-xstep)
		calc-graph-yp (cdr calc-graph-yp)
		calc-graph-yval (car calc-graph-yp))
	(setq calc-graph-xval (car (car calc-graph-yp))
	      calc-graph-yval (cdr (car calc-graph-yp))
	      calc-graph-yp (cdr calc-graph-yp))
	(if (or (not calc-graph-yp)
		(and calc-graph-xhigh (equal calc-graph-xval calc-graph-xhigh)))
	    (setq calc-graph-numsteps 0))))
    (if calc-graph-is-splot
	(if (and (eq (car-safe calc-graph-zval) 'calcFunc-xyz)
		 (= (length calc-graph-zval) 4))
	    (setq calc-graph-xval (nth 1 calc-graph-zval)
		  calc-graph-yval (nth 2 calc-graph-zval)
		  calc-graph-zval (nth 3 calc-graph-zval)))
      (if (and (eq (car-safe calc-graph-yval) 'calcFunc-xyz)
	       (= (length calc-graph-yval) 4))
	  (progn
	    (or calc-graph-surprise-splot
		(with-current-buffer (get-buffer-create "*Gnuplot Temp*")
		  (save-excursion
		    (goto-char (point-max))
		    (re-search-backward "^plot[ \t]")
		    (insert "set parametric\ns")
		    (setq calc-graph-surprise-splot t))))
	    (setq calc-graph-xval (nth 1 calc-graph-yval)
		  calc-graph-zval (nth 3 calc-graph-yval)
		  calc-graph-yval (nth 2 calc-graph-yval)))
	(if (and (eq (car-safe calc-graph-yval) 'calcFunc-xy)
		 (= (length calc-graph-yval) 3))
	    (setq calc-graph-xval (nth 1 calc-graph-yval)
		  calc-graph-yval (nth 2 calc-graph-yval)))))
    (if (and (Math-realp calc-graph-xval)
	     (Math-realp calc-graph-yval)
	     (or (not calc-graph-zval) (Math-realp calc-graph-zval)))
	(progn
	  (setq calc-graph-blank nil
		calc-graph-non-blank t)
	  (if (Math-integerp calc-graph-xval)
	      (insert (math-format-number calc-graph-xval))
	    (if (eq (car calc-graph-xval) 'frac)
		(setq calc-graph-xval (math-float calc-graph-xval)))
	    (insert (math-format-number (nth 1 calc-graph-xval))
		    "e" (int-to-string (nth 2 calc-graph-xval))))
	  (insert " ")
	  (if (Math-integerp calc-graph-yval)
	      (insert (math-format-number calc-graph-yval))
	    (if (eq (car calc-graph-yval) 'frac)
		(setq calc-graph-yval (math-float calc-graph-yval)))
	    (insert (math-format-number (nth 1 calc-graph-yval))
		    "e" (int-to-string (nth 2 calc-graph-yval))))
	  (if calc-graph-zval
	      (progn
		(insert " ")
		(if (Math-integerp calc-graph-zval)
		    (insert (math-format-number calc-graph-zval))
		  (if (eq (car calc-graph-zval) 'frac)
		      (setq calc-graph-zval (math-float calc-graph-zval)))
		  (insert (math-format-number (nth 1 calc-graph-zval))
			  "e" (int-to-string (nth 2 calc-graph-zval))))))
	  (insert "\n"))
      (and (not (equal calc-graph-zval '(skip)))
           (boundp 'var-PlotRejects)
	   (eq (car-safe var-PlotRejects) 'vec)
	   (nconc var-PlotRejects
		  (list (list 'vec
			      calc-graph-curve-num
			      calc-graph-stepcount
			      calc-graph-xval calc-graph-yval)))
	   (calc-refresh-evaltos 'var-PlotRejects))
      (or calc-graph-blank
	  (progn
	    (insert "\n")
	    (setq calc-graph-blank t))))))

(defun calc-temp-file-name (num)
  (while (<= (length calc-graph-file-cache) (1+ num))
    (setq calc-graph-file-cache (nconc calc-graph-file-cache (list nil))))
  (car (or (nth (1+ num) calc-graph-file-cache)
	   (setcar (nthcdr (1+ num) calc-graph-file-cache)
		   (list (make-temp-file
			  (concat calc-gnuplot-tempfile
				  (if (<= num 0)
				      (char-to-string (- ?A num))
				    (int-to-string num))))
			 nil)))))

(defun calc-graph-delete-temps ()
  (while calc-graph-file-cache
    (and (car calc-graph-file-cache)
	 (file-exists-p (car (car calc-graph-file-cache)))
	 (condition-case err
	     (delete-file (car (car calc-graph-file-cache)))
	   (error nil)))
    (setq calc-graph-file-cache (cdr calc-graph-file-cache))))

(defun calc-graph-kill-hook ()
  (calc-graph-delete-temps))

(defun calc-graph-show-tty (output)
  "Default calc-gnuplot-plot-command for \"tty\" output mode.
This is useful for tek40xx and other graphics-terminal types."
  (call-process-region 1 1 shell-file-name
		       nil calc-gnuplot-buffer nil
		       "-c" (format "cat %s >/dev/tty; rm %s" output output)))

(defvar calc-dumb-map nil
  "The keymap for the \"dumb\" terminal plot.")

(defun calc-graph-show-dumb (&optional output)
  "Default calc-gnuplot-plot-command for Pinard's \"dumb\" terminal type.
This \"dumb\" driver will be present in Gnuplot 3.0."
  (interactive)
  (save-window-excursion
    (switch-to-buffer calc-gnuplot-buffer)
    (delete-other-windows)
    (goto-char calc-gnuplot-trail-mark)
    (or (search-forward "\f" nil t)
	(sleep-for 1))
    (goto-char (point-max))
    (re-search-backward "\f\\|^[ \t]+\\^$\\|G N U P L O T")
    (if (looking-at "\f")
	(progn
	  (forward-char 1)
	  (if (eolp) (forward-line 1))
	  (or (calc-graph-find-command "time")
	      (calc-graph-find-command "title")
	      (calc-graph-find-command "ylabel")
	      (let ((pt (point)))
		(insert-before-markers (format "(%s)" (current-time-string)))
		(goto-char pt)))
	  (set-window-start (selected-window) (point))
	  (goto-char (point-max)))
      (end-of-line)
      (backward-char 1)
      (recenter '(4)))
    (or calc-dumb-map
	(progn
	  (setq calc-dumb-map (make-sparse-keymap))
	  (define-key calc-dumb-map "\n" 'scroll-up-command)
	  (define-key calc-dumb-map " " 'scroll-up-command)
	  (define-key calc-dumb-map "\177" 'scroll-down-command)
	  (define-key calc-dumb-map "<" 'scroll-left)
	  (define-key calc-dumb-map ">" 'scroll-right)
	  (define-key calc-dumb-map "{" 'scroll-down-command)
	  (define-key calc-dumb-map "}" 'scroll-up-command)
	  (define-key calc-dumb-map "q" 'exit-recursive-edit)
	  (define-key calc-dumb-map "\C-c\C-c" 'exit-recursive-edit)))
    (use-local-map calc-dumb-map)
    (setq truncate-lines t)
    (message "Type `q' or `C-c C-c' to return to Calc")
    (recursive-edit)
    (bury-buffer "*Gnuplot Trail*")))

(defun calc-graph-clear ()
  (interactive)
  (if calc-graph-last-device
      (if (or (equal calc-graph-last-device "x11")
	      (equal calc-graph-last-device "X11"))
	  (calc-gnuplot-command "set output"
				(if (equal calc-graph-last-output "STDOUT")
				    ""
				  (prin1-to-string calc-graph-last-output)))
	(calc-gnuplot-command "clear"))))

(defun calc-graph-title-x (title)
  (interactive "sX axis title: ")
  (calc-graph-set-command "xlabel" (if (not (equal title ""))
				       (prin1-to-string title))))

(defun calc-graph-title-y (title)
  (interactive "sY axis title: ")
  (calc-graph-set-command "ylabel" (if (not (equal title ""))
				       (prin1-to-string title))))

(defun calc-graph-title-z (title)
  (interactive "sZ axis title: ")
  (calc-graph-set-command "zlabel" (if (not (equal title ""))
				       (prin1-to-string title))))

(defun calc-graph-range-x (range)
  (interactive "sX axis range: ")
  (calc-graph-set-range "xrange" range))

(defun calc-graph-range-y (range)
  (interactive "sY axis range: ")
  (calc-graph-set-range "yrange" range))

(defun calc-graph-range-z (range)
  (interactive "sZ axis range: ")
  (calc-graph-set-range "zrange" range))

(defun calc-graph-set-range (cmd range)
  (if (equal range "$")
      (calc-wrapper
       (let ((val (calc-top-n 1)))
	 (if (and (eq (car-safe val) 'intv) (math-constp val))
	     (setq range (concat
			  (math-format-number (math-float (nth 2 val))) ":"
			  (math-format-number (math-float (nth 3 val)))))
	   (if (and (eq (car-safe val) 'vec)
		    (= (length val) 3))
	       (setq range (concat
			    (math-format-number (math-float (nth 1 val))) ":"
			    (math-format-number (math-float (nth 2 val)))))
	     (error "Range specification must be an interval or 2-vector")))
	 (calc-pop-stack 1))))
  (if (string-match "\\[.+\\]" range)
      (setq range (substring range 1 -1)))
  (if (and (not (string-match ":" range))
	   (or (string-match "," range)
	       (string-match " " range)))
      (aset range (match-beginning 0) ?\:))
  (calc-graph-set-command cmd (if (not (equal range ""))
				  (concat "[" range "]"))))

(defun calc-graph-log-x (flag)
  (interactive "P")
  (calc-graph-set-log flag 0 0))

(defun calc-graph-log-y (flag)
  (interactive "P")
  (calc-graph-set-log 0 flag 0))

(defun calc-graph-log-z (flag)
  (interactive "P")
  (calc-graph-set-log 0 0 flag))

(defun calc-graph-set-log (xflag yflag zflag)
  (let* ((old (or (calc-graph-find-command "logscale") ""))
	 (xold (string-match "x" old))
	 (yold (string-match "y" old))
	 (zold (string-match "z" old))
	 str)
    (setq str (concat (if (if xflag
			      (if (eq xflag 0) xold
				(> (prefix-numeric-value xflag) 0))
			    (not xold)) "x" "")
		      (if (if yflag
			      (if (eq yflag 0) yold
				(> (prefix-numeric-value yflag) 0))
			    (not yold)) "y" "")
		      (if (if zflag
			      (if (eq zflag 0) zold
				(> (prefix-numeric-value zflag) 0))
			    (not zold)) "z" "")))
    (calc-graph-set-command "logscale" (if (not (equal str "")) str))))

(defun calc-graph-line-style (style)
  (interactive "P")
  (calc-graph-set-styles (and style (prefix-numeric-value style)) t))

(defun calc-graph-point-style (style)
  (interactive "P")
  (calc-graph-set-styles t (and style (prefix-numeric-value style))))

(defun calc-graph-set-styles (lines points &optional yerr)
  (calc-graph-init)
  (with-current-buffer calc-gnuplot-input
    (or (calc-graph-find-plot nil nil)
	(error "No data points have been set!"))
    (let ((base (point))
	  (mode nil) (lstyle nil) (pstyle nil)
	  start end lenbl penbl errform)
      (re-search-forward "[,\n]")
      (forward-char -1)
      (setq end (point) start end)
      (goto-char base)
      (if (looking-at "[^,\n]*[^,\n \t]\\([ \t]+with\\)")
	  (progn
	    (setq start (match-beginning 1))
	    (goto-char (match-end 0))
	    (if (looking-at "[ \t]+\\([a-z]+\\)")
		(setq mode (buffer-substring (match-beginning 1)
					     (match-end 1))))
	    (if (looking-at "[ \ta-z]+\\([0-9]+\\)")
		(setq lstyle (string-to-number
			      (buffer-substring (match-beginning 1)
						(match-end 1)))))
	    (if (looking-at "[ \ta-z]+[0-9]+[ \t]+\\([0-9]+\\)")
		(setq pstyle (string-to-number
			      (buffer-substring (match-beginning 1)
						(match-end 1)))))))
      (unless yerr
        (setq lenbl (or (equal mode "lines")
                        (equal mode "linespoints"))
              penbl (or (equal mode "points")
                        (equal mode "linespoints")))
        (if lines
            (or (eq lines t)
                (setq lstyle lines
                      lenbl (>= lines 0)))
          (setq lenbl (not lenbl)))
        (if points
            (or (eq points t)
                (setq pstyle points
                      penbl (>= points 0)))
          (setq penbl (not penbl))))
        (delete-region start end)
      (goto-char start)
      (setq errform
            (condition-case nil
                (math-contains-sdev-p
                 (eval (intern
                        (concat "var-"
                                (save-excursion
                                  (re-search-backward ":\\(.*\\)\\}")
                                  (match-string 1))))))
              (error nil)))
      (if yerr
          (insert " with yerrorbars")
        (insert " with "
                (if (and errform
                         (equal mode "dots")
                         (eq lines t))
                    "yerrorbars"
                  (if lenbl
                      (if penbl "linespoints" "lines")
                    (if penbl "points" "dots"))))
        (if (and pstyle (> pstyle 0))
            (insert " "
                    (if (and lstyle (> lstyle 0)) (int-to-string lstyle) "1")
                    " " (int-to-string pstyle))
          (if (and lstyle (> lstyle 0))
              (insert " " (int-to-string lstyle)))))))
  (calc-graph-view-commands))

(defun calc-graph-zero-x (flag)
  (interactive "P")
  (calc-graph-set-command "noxzeroaxis"
			  (and (if flag
				   (<= (prefix-numeric-value flag) 0)
				 (not (calc-graph-find-command "noxzeroaxis")))
			       " ")))

(defun calc-graph-zero-y (flag)
  (interactive "P")
  (calc-graph-set-command "noyzeroaxis"
			  (and (if flag
				   (<= (prefix-numeric-value flag) 0)
				 (not (calc-graph-find-command "noyzeroaxis")))
			       " ")))

(defun calc-graph-name (name)
  (interactive "sTitle for current curve: ")
  (calc-graph-init)
  (with-current-buffer calc-gnuplot-input
    (or (calc-graph-find-plot nil nil)
	(error "No data points have been set!"))
    (let ((base (point))
	  start
          end)
      (re-search-forward "[,\n]\\|[ \t]+with")
      (setq end (match-beginning 0))
      (goto-char base)
      (if (looking-at "[^,\n]*[^,\n \t]\\([ \t]+title\\)")
	  (progn
	    (goto-char (match-beginning 1))
	    (delete-region (point) end))
	(goto-char end))
      (insert " title " (prin1-to-string name))))
  (calc-graph-view-commands))

(defun calc-graph-hide (flag)
  (interactive "P")
  (calc-graph-init)
  (and (calc-graph-find-plot nil nil)
       (progn
	 (or (looking-at "{")
	     (error "Can't hide this curve (wrong format)"))
	 (forward-char 1)
	 (if (looking-at "*")
	     (if (or (null flag) (<= (prefix-numeric-value flag) 0))
		 (delete-char 1))
	   (if (or (null flag) (> (prefix-numeric-value flag) 0))
	       (insert "*"))))))

(defun calc-graph-header (title)
  (interactive "sTitle for entire graph: ")
  (calc-graph-set-command "title" (if (not (equal title ""))
				      (prin1-to-string title))))

(defun calc-graph-border (flag)
  (interactive "P")
  (calc-graph-set-command "noborder"
			  (and (if flag
				   (<= (prefix-numeric-value flag) 0)
				 (not (calc-graph-find-command "noborder")))
			       " ")))

(defun calc-graph-grid (flag)
  (interactive "P")
  (calc-graph-set-command "grid" (and (if flag
					  (> (prefix-numeric-value flag) 0)
					(not (calc-graph-find-command "grid")))
				      " ")))

(defun calc-graph-key (flag)
  (interactive "P")
  (calc-graph-set-command "key" (and (if flag
					 (> (prefix-numeric-value flag) 0)
				       (not (calc-graph-find-command "key")))
				     " ")))

(defun calc-graph-num-points (res flag)
  (interactive "sNumber of data points: \nP")
  (if flag
      (if (> (prefix-numeric-value flag) 0)
	  (if (equal res "")
	      (message "Default resolution is %d"
		       calc-graph-default-resolution)
	    (setq calc-graph-default-resolution (string-to-number res)))
	(if (equal res "")
	    (message "Default 3D resolution is %d"
		     calc-graph-default-resolution-3d)
	  (setq calc-graph-default-resolution-3d (string-to-number res))))
    (calc-graph-set-command "samples" (if (not (equal res "")) res))))

(defun calc-graph-device (name flag)
  (interactive "sDevice name: \nP")
  (if (equal name "?")
      (progn
	(calc-gnuplot-command "set terminal")
	(calc-graph-view-trail))
    (if flag
	(if (> (prefix-numeric-value flag) 0)
	    (if (equal name "")
		(message "Default GNUPLOT device is \"%s\""
			 calc-gnuplot-default-device)
	      (setq calc-gnuplot-default-device name))
	  (if (equal name "")
	      (message "GNUPLOT device for Print command is \"%s\""
		       calc-gnuplot-print-device)
	    (setq calc-gnuplot-print-device name)))
      (calc-graph-set-command "terminal" (if (not (equal name ""))
					     name)))))

(defun calc-graph-output (name flag)
  (interactive "FOutput file name: \np")
  (cond ((string-match "\\<[aA][uU][tT][oO]$" name)
	 (setq name "auto"))
	((string-match "\\<[tT][tT][yY]$" name)
	 (setq name "tty"))
	((string-match "\\<[sS][tT][dD][oO][uU][tT]$" name)
	 (setq name "STDOUT"))
	((equal (file-name-nondirectory name) "")
	 (setq name ""))
	(t (setq name (expand-file-name name))))
  (if flag
      (if (> (prefix-numeric-value flag) 0)
	  (if (equal name "")
	      (message "Default GNUPLOT output file is \"%s\""
		       calc-gnuplot-default-output)
	    (setq calc-gnuplot-default-output name))
	(if (equal name "")
	    (message "GNUPLOT output file for Print command is \"%s\""
		     calc-gnuplot-print-output)
	  (setq calc-gnuplot-print-output name)))
    (calc-graph-set-command "output" (if (not (equal name ""))
					 (prin1-to-string name)))))

(defun calc-graph-display (name)
  (interactive "sX display name: ")
  (if (equal name "")
      (message "Current X display is \"%s\""
	       (or calc-gnuplot-display "<none>"))
    (setq calc-gnuplot-display name)
    (if (calc-gnuplot-alive)
	(calc-gnuplot-command "exit"))))

(defun calc-graph-geometry (name)
  (interactive "sX geometry spec (or \"default\"): ")
  (if (equal name "")
      (message "Current X geometry is \"%s\""
	       (or calc-gnuplot-geometry "default"))
    (setq calc-gnuplot-geometry (and (not (equal name "default")) name))
    (if (calc-gnuplot-alive)
	(calc-gnuplot-command "exit"))))

(defun calc-graph-find-command (cmd)
  (calc-graph-init)
  (with-current-buffer calc-gnuplot-input
    (goto-char (point-min))
    (if (re-search-forward (concat "^set[ \t]+" cmd "[ \t]*\\(.*\\)$") nil t)
	(buffer-substring (match-beginning 1) (match-end 1)))))

(defun calc-graph-set-command (cmd &rest args)
  (calc-graph-init)
  (with-current-buffer calc-gnuplot-input
    (goto-char (point-min))
    (if (re-search-forward (concat "^set[ \t]+" cmd "[ \t\n]") nil t)
	(progn
	  (forward-char -1)
	  (end-of-line)
	  (let ((end (point)))
	    (beginning-of-line)
	    (delete-region (point) (1+ end))))
      (if (calc-graph-find-plot t t)
	  (if (eq (preceding-char) ?\n)
	      (forward-char -1))
	(goto-char (1- (point-max)))))
    (if (and args (car args))
	(progn
	  (or (bolp)
	      (insert "\n"))
	  (insert "set " (mapconcat 'identity (cons cmd args) " ") "\n"))))
  (calc-graph-view-commands))

(defun calc-graph-command (cmd)
  (interactive "sGNUPLOT command: ")
  (calc-wrapper
   (calc-graph-init)
   (calc-graph-view-trail)
   (calc-gnuplot-command cmd)
   (or (string= calc-gnuplot-name "pgnuplot")
       (progn
	 (accept-process-output)
	 (calc-graph-view-trail)))))

(defun calc-graph-kill (&optional no-view)
  (interactive)
  (calc-graph-delete-temps)
  (if (calc-gnuplot-alive)
      (calc-wrapper
       (or no-view (calc-graph-view-trail))
       (let ((calc-graph-no-wait t))
	 (calc-gnuplot-command "exit"))
       (sit-for 1)
       (if (process-status calc-gnuplot-process)
	   (delete-process calc-gnuplot-process))
       (setq calc-gnuplot-process nil))))

(defun calc-graph-quit ()
  (interactive)
  (if (get-buffer-window calc-gnuplot-input)
      (calc-graph-view-commands t))
  (if (get-buffer-window calc-gnuplot-buffer)
      (calc-graph-view-trail t))
  (calc-graph-kill t))

(defun calc-graph-view-commands (&optional no-need)
  (interactive "p")
  (or calc-graph-no-auto-view (calc-graph-init-buffers))
  (calc-graph-view calc-gnuplot-input calc-gnuplot-buffer (null no-need)))

(defun calc-graph-view-trail (&optional no-need)
  (interactive "p")
  (or calc-graph-no-auto-view (calc-graph-init-buffers))
  (calc-graph-view calc-gnuplot-buffer calc-gnuplot-input (null no-need)))

(defun calc-graph-view (buf other-buf need)
  (let (win)
    (or calc-graph-no-auto-view
	(if (setq win (get-buffer-window buf))
	    (or need
		(and (eq buf calc-gnuplot-buffer)
		     (with-current-buffer buf
		       (not (pos-visible-in-window-p (point-max) win))))
		(progn
		  (bury-buffer buf)
		  (bury-buffer other-buf)
		  (let ((curwin (selected-window)))
		    (select-window win)
		    (switch-to-buffer nil)
		    (select-window curwin))))
	  (if (setq win (get-buffer-window other-buf))
	      (set-window-buffer win buf)
	    (if (eq major-mode 'calc-mode)
		(if (or need
			(not (window-full-height-p)))
		    (display-buffer buf))
	      (switch-to-buffer buf)))))
    (with-current-buffer buf
      (if (and (eq buf calc-gnuplot-buffer)
	       (setq win (get-buffer-window buf))
	       (not (pos-visible-in-window-p (point-max) win)))
	  (progn
	    (goto-char (point-max))
	    (vertical-motion (- 6 (window-height win)))
	    (set-window-start win (point))
	    (goto-char (point-max)))))
    (or calc-graph-no-auto-view (sit-for 0))))

(defun calc-gnuplot-check-for-errors ()
  (if (save-excursion
	(prog2
	 (progn
	   (set-buffer calc-gnuplot-buffer)
	   (goto-char calc-gnuplot-last-error-pos))
	 (re-search-forward "^[ \t]+\\^$" nil t)
	 (goto-char (point-max))
	 (setq calc-gnuplot-last-error-pos (point-max))))
      (calc-graph-view-trail)))

(defun calc-gnuplot-command (&rest args)
  (calc-graph-init)
  (let ((cmd (concat (mapconcat 'identity args " ") "\n")))
    (or (string= calc-gnuplot-name "pgnuplot")
	(accept-process-output))
    (with-current-buffer calc-gnuplot-buffer
      (calc-gnuplot-check-for-errors)
      (goto-char (point-max))
      (setq calc-gnuplot-trail-mark (point))
      (or (>= calc-gnuplot-version 3)
	  (insert cmd))
      (set-marker (process-mark calc-gnuplot-process) (point))
      (process-send-string calc-gnuplot-process cmd)
      (if (get-buffer-window calc-gnuplot-buffer)
	  (calc-graph-view-trail))
      (or (string= calc-gnuplot-name "pgnuplot")
	  (accept-process-output (and (not calc-graph-no-wait)
				      calc-gnuplot-process)))
      (calc-gnuplot-check-for-errors)
      (if (get-buffer-window calc-gnuplot-buffer)
	  (calc-graph-view-trail)))))

(defun calc-graph-init-buffers ()
  (or (and calc-gnuplot-buffer
	   (buffer-name calc-gnuplot-buffer))
      (setq calc-gnuplot-buffer (get-buffer-create "*Gnuplot Trail*")))
  (or (and calc-gnuplot-input
	   (buffer-name calc-gnuplot-input))
      (setq calc-gnuplot-input (get-buffer-create "*Gnuplot Commands*"))))

(defun calc-graph-init ()
  (or (calc-gnuplot-alive)
      (let ((process-connection-type t)
	    origin)
	(if calc-gnuplot-process
	    (progn
	      (delete-process calc-gnuplot-process)
	      (setq calc-gnuplot-process nil)))
	(calc-graph-init-buffers)
	(with-current-buffer calc-gnuplot-buffer
	  (insert "\nStarting gnuplot...\n")
	  (setq origin (point)))
	(setq calc-graph-last-device nil)
	(setq calc-graph-last-output nil)
	(if (string= calc-gnuplot-name "pgnuplot")
	    (let ((version-str (shell-command-to-string "pgnuplot -V")))
	      (if (string-match "gnuplot \\([0-9]+\\)\\." version-str)
		  (setq calc-gnuplot-version (string-to-number
					      (substring version-str
							 (match-beginning 1)
							 (match-end 1))))
		(setq calc-gnuplot-version 1))))
	(condition-case err
	    (let ((args (append (and calc-gnuplot-display
				     (not (equal calc-gnuplot-display
						 (getenv "DISPLAY")))
				     (not (string= calc-gnuplot-name "pgnuplot"))
				     (list "-display"
					   calc-gnuplot-display))
				(and calc-gnuplot-geometry
				     (not (string= calc-gnuplot-name "pgnuplot"))
				     (list "-geometry"
					   calc-gnuplot-geometry)))))
	      (setq calc-gnuplot-process
		    (apply 'start-process
			   "gnuplot"
			   calc-gnuplot-buffer
			   calc-gnuplot-name
			   args))
	      (set-process-query-on-exit-flag calc-gnuplot-process nil))
	  (file-error
	   (error "Sorry, can't find \"%s\" on your system"
		  calc-gnuplot-name)))
	(with-current-buffer calc-gnuplot-buffer
	  (while (and (not (string= calc-gnuplot-name "pgnuplot"))
		      (not (save-excursion
			     (goto-char origin)
			     (search-forward "gnuplot> " nil t)))
		      (memq (process-status calc-gnuplot-process) '(run stop)))
	    (accept-process-output calc-gnuplot-process))
	  (or (memq (process-status calc-gnuplot-process) '(run stop))
	      (error "Unable to start GNUPLOT process"))
	  (if (not (string= calc-gnuplot-name "pgnuplot"))
	      (if (save-excursion
		    (goto-char origin)
		    (re-search-forward
		     "G N U P L O T.*\n.*version \\([0-9]+\\)\\." nil t))
		  (setq calc-gnuplot-version
			(string-to-number (buffer-substring
					   (match-beginning 1)
					   (match-end 1))))
		(setq calc-gnuplot-version 1)))
	  (goto-char (point-max)))))
  (with-current-buffer calc-gnuplot-input
    (if (= (buffer-size) 0)
	(insert "# Commands for running gnuplot\n\n\n")
      (or calc-graph-no-auto-view
	  (eq (char-after (1- (point-max))) ?\n)
	  (progn
	    (goto-char (point-max))
	    (insert "\n"))))))

(provide 'calc-graph)

;;; calc-graph.el ends here
