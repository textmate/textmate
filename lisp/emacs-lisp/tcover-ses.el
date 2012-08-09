;;;; testcover-ses.el -- Example use of `testcover' to test "SES"

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Jonathan Yavner <jyavner@engineer.com>
;; Maintainer: Jonathan Yavner <jyavner@engineer.com>
;; Keywords: spreadsheet lisp utility
;; Package: testcover

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

(require 'testcover)

(defvar ses-initial-global-parameters)
(defvar ses-mode-map)

(declare-function ses-set-curcell "ses")
(declare-function ses-update-cells "ses")
(declare-function ses-load "ses")
(declare-function ses-vector-delete "ses")
(declare-function ses-create-header-string "ses")
(declare-function ses-read-cell "ses")
(declare-function ses-read-symbol "ses")
(declare-function ses-command-hook "ses")
(declare-function ses-jump "ses")


;;;Here are some macros that exercise SES.  Set `pause' to t if you want the
;;;macros to pause after each step.
(let* ((pause nil)
       (x (if pause "q" ""))
       (y "ses-test.ses\r<"))
  ;;Fiddle with the existing spreadsheet
  (fset 'ses-exercise-example
	(concat   "" data-directory "ses-example.ses\r<"
		x "10"
		x ""
		x ""
		x "pses-center\r"
		x "p\r"
		x "\t\t"
		x "\r A9 B9\r"
		x ""
		x "\r2\r"
		x ""
		x "50\r"
		x "4"
		x ""
		x ""
		x "(+ o\0"
		x "-1o \r"
		x ""
		x))
  ;;Create a new spreadsheet
  (fset 'ses-exercise-new
	(concat y
		x "\"%.8g\"\r"
		x "2\r"
		x ""
		x ""
		x "2"
		x "\"Header\r"
		x "(sqrt 1\r"
		x "pses-center\r"
		x "\t"
		x "(+ A2 A3\r"
		x "(* B2 A3\r"
		x "2"
		x "\rB3\r"
		x ""
		x))
  ;;Basic cell display
  (fset 'ses-exercise-display
	(concat y ":(revert-buffer t t)\r"
		x ""
		x "\"Very long\r"
		x "w3\r"
		x "w3\r"
		x "(/ 1 0\r"
		x "234567\r"
		x "5w"
		x "\t1\r"
		x ""
		x "234567\r"
		x "\t"
		x ""
		x "345678\r"
		x "3w"
		x "\0>"
		x ""
		x ""
		x ""
		x ""
		x ""
		x ""
		x ""
		x "1\r"
		x ""
		x ""
		x "\"1234567-1234567-1234567\r"
		x "123\r"
		x "2"
		x "\"1234567-1234567-1234567\r"
		x "123\r"
		x "w8\r"
		x "\"1234567\r"
		x "w5\r"
		x))
  ;;Cell formulas
  (fset 'ses-exercise-formulas
	(concat y ":(revert-buffer t t)\r"
		x "\t\t"
		x "\t"
		x "(* B1 B2 D1\r"
		x "(* B2 B3\r"
		x "(apply '+ (ses-range B1 B3)\r"
		x "(apply 'ses+ (ses-range B1 B3)\r"
		x "(apply 'ses+ (ses-range A2 A3)\r"
		x "(mapconcat'number-to-string(ses-range B2 B4) \"-\"\r"
		x "(apply 'concat (reverse (ses-range A3 D3))\r"
		x "(* (+ A2 A3) (ses+ B2 B3)\r"
		x ""
		x "2"
		x "5\t"
		x "(apply 'ses+ (ses-range E1 E2)\r"
		x "(apply 'ses+ (ses-range A5 B5)\r"
		x "(apply 'ses+ (ses-range E1 F1)\r"
		x "(apply 'ses+ (ses-range D1 E1)\r"
		x "\t"
		x "(ses-average (ses-range A2 A5)\r"
		x "(apply 'ses+ (ses-range A5 A6)\r"
		x "k"
		x ""
		x ""
		x "2"
		x "3"
		x "o"
		x "2o"
		x "3k"
		x "(ses-average (ses-range B3 E3)\r"
		x "k"
		x "12345678\r"
		x))
  ;;Recalculating and reconstructing
  (fset 'ses-exercise-recalc
	(concat y ":(revert-buffer t t)\r"
		x ""
		x "\t\t"
		x ""
		x "(/ 1 0\r"
		x ""
		x "\n"
		x ""
		x "\"%.6g\"\r"
		x ""
		x ">nw"
		x "\0>xdelete-region\r"
		x ""
		x "8"
		x "\0>xdelete-region\r"
		x ""
		x ""
		x "k"
		x ""
		x "\"Very long\r"
		x ""
		x "\r\r"
		x ""
		x "o"
		x ""
		x "\"Very long2\r"
		x "o"
		x ""
		x "\rC3\r"
		x "\rC2\r"
		x "\0"
		x "\rC4\r"
		x "\rC2\r"
		x "\0"
		x ""
		x "xses-mode\r"
		x "<"
		x "2k"
		x))
  ;;Header line
  (fset 'ses-exercise-header-row
	(concat y ":(revert-buffer t t)\r"
		x "<"
		x ">"
		x "6<"
		x ">"
		x "7<"
		x ">"
		x "8<"
		x "2<"
		x ">"
		x "3w"
		x "10<"
		x ">"
		x "2"
		x))
  ;;Detecting unsafe formulas and printers
  (fset 'ses-exercise-unsafe
	(concat y ":(revert-buffer t t)\r"
		x "p(lambda (x) (delete-file x))\rn"
		x "p(lambda (x) (delete-file \"ses-nothing\"))\ry"
		x "\0n"
		x "(delete-file \"x\"\rn"
		x "(delete-file \"ses-nothing\"\ry"
		x "\0n"
		x "(open-network-stream \"x\" nil \"localhost\" \"smtp\"\ry"
		x "\0n"
		x))
  ;;Inserting and deleting rows
  (fset 'ses-exercise-rows
	(concat y ":(revert-buffer t t)\r"
		x ""
		x "\"%s=\"\r"
		x "20"
		x "p\"%s+\"\r"
		x ""
		x "123456789\r"
		x "\021"
		x ""
		x ""
		x "(not B25\r"
		x "k"
		x "jA3\r"
		x "19"
		x ""
		x "100"  ;Make this approx your CPU speed in MHz
		x))
  ;;Inserting and deleting columns
  (fset 'ses-exercise-columns
	(concat y ":(revert-buffer t t)\r"
		x "\"%s@\"\r"
		x "o"
		x ""
		x "o"
		x ""
		x "k"
		x "w8\r"
		x "p\"%.7s*\"\r"
		x "o"
		x ""
		x "2o"
		x "3k"
		x "\"%.6g\"\r"
		x "26o"
		x "\026\t"
		x "26o"
		x "0\r"
		x "26\t"
		x "400"
		x "50k"
		x "\0D"
		x))
  (fset 'ses-exercise-editing
	(concat y ":(revert-buffer t t)\r"
		x "1\r"
		x "('x\r"
		x ""
		x ""
		x "\r\r"
		x "w9\r"
		x "\r.5\r"
		x "\r 10\r"
		x "w12\r"
		x "\r'\r"
		x "\r\r"
		x "jA4\r"
		x "(+ A2 100\r"
		x "3\r"
		x "jB1\r"
		x "(not A1\r"
		x "\"Very long\r"
		x ""
		x "h"
		x "H"
		x ""
		x ">\t"
		x ""
		x ""
		x "2"
		x ""
		x "o"
		x "h"
		x "\0"
		x "\"Also very long\r"
		x "H"
		x "\0'\r"
		x "'Trial\r"
		x "'qwerty\r"
		x "(concat o<\0"
		x "-1o\r"
		x "(apply '+ o<\0-1o\r"
		x "2"
		x "-2"
		x "-2"
		x "2"
		x ""
		x "H"
		x "\0"
		x "\"Another long one\r"
		x "H"
		x ""
		x "<"
		x ""
		x ">"
		x "\0"
		x))
  ;;Sorting of columns
  (fset 'ses-exercise-sort-column
	(concat y ":(revert-buffer t t)\r"
		x "\"Very long\r"
		x "99\r"
		x "o13\r"
		x "(+ A3 B3\r"
		x "7\r8\r(* A4 B4\r"
		x "\0A\r"
		x "\0B\r"
		x "\0C\r"
		x "o"
		x "\0C\r"
		x))
  ;;Simple cell printers
  (fset 'ses-exercise-cell-printers
	(concat y ":(revert-buffer t t)\r"
		x "\"4\t76\r"
		x "\"4\n7\r"
		x "p\"{%S}\"\r"
		x "p(\"[%s]\")\r"
		x "p(\"<%s>\")\r"
		x "\0"
		x "p\r"
		x "pnil\r"
		x "pses-dashfill\r"
		x "48\r"
		x "\t"
		x "\0p\r"
		x "p\r"
		x "pses-dashfill\r"
		x "\0pnil\r"
		x "5\r"
		x "pses-center\r"
		x "\"%s\"\r"
		x "w8\r"
		x "p\r"
		x "p\"%.7g@\"\r"
		x "\r"
		x "\"%.6g#\"\r"
		x "\"%.6g.\"\r"
		x "\"%.6g.\"\r"
		x "pidentity\r"
		x "6\r"
		x "\"UPCASE\r"
		x "pdowncase\r"
		x "(* 3 4\r"
		x "p(lambda (x) '(\"Hi\"))\r"
		x "p(lambda (x) '(\"Bye\"))\r"
		x))
  ;;Spanning cell printers
  (fset 'ses-exercise-spanning-printers
	(concat y ":(revert-buffer t t)\r"
		x "p\"%.6g*\"\r"
		x "pses-dashfill-span\r"
		x "5\r"
		x "pses-tildefill-span\r"
		x "\"4\r"
		x "p\"$%s\"\r"
		x "p(\"$%s\")\r"
		x "8\r"
		x "p(\"!%s!\")\r"
		x "\t\"12345678\r"
		x "pses-dashfill-span\r"
		x "\"23456789\r"
		x "\t"
		x "(not t\r"
		x "w6\r"
		x "\"5\r"
		x "o"
		x "k"
		x "k"
		x "\t"
		x ""
		x "o"
		x "2k"
		x "k"
		x))
  ;;Cut/copy/paste - within same buffer
  (fset 'ses-exercise-paste-1buf
	(concat y ":(revert-buffer t t)\r"
		x "\0w"
		x ""
		x "o"
		x "\"middle\r"
		x "\0"
		x "w"
		x "\0"
		x "w"
		x ""
		x ""
		x "2y"
		x "y"
		x "y"
		x ">"
		x "y"
		x ">y"
		x "<"
		x "p\"<%s>\"\r"
		x "pses-dashfill\r"
		x "\0"
		x ""
		x ""
		x "y"
		x "\r\0w"
		x "\r"
		x "3(+ G2 H1\r"
		x "\0w"
		x ">"
		x ""
		x "8(ses-average (ses-range G2 H2)\r"
		x "\0k"
		x "7"
		x ""
		x "(ses-average (ses-range E7 E9)\r"
		x "\0"
		x ""
		x "(ses-average (ses-range E7 F7)\r"
		x "\0k"
		x ""
		x "(ses-average (ses-range D6 E6)\r"
		x "\0k"
		x ""
		x "2"
		x "\"Line A\r"
		x "pses-tildefill-span\r"
		x "\"Subline A(1)\r"
		x "pses-dashfill-span\r"
		x "\0w"
		x ""
		x ""
		x "\0w"
		x ""
		x))
  ;;Cut/copy/paste - between two buffers
  (fset 'ses-exercise-paste-2buf
	(concat y ":(revert-buffer t t)\r"
		x "o\"middle\r\0"
		x ""
		x "4bses-test.txt\r"
		x " "
		x "\"xxx\0"
		x "wo"
		x ""
		x ""
		x "o\"\0"
		x "wo"
		x "o123.45\0"
		x "o"
		x "o1 \0"
		x "o"
		x ">y"
		x "o symb\0"
		x "oy2y"
		x "o1\t\0"
		x "o"
		x "w9\np\"<%s>\"\n"
		x "o\n2\t\"3\nxxx\t5\n\0"
		x "oy"
		x))
  ;;Export text, import it back
  (fset 'ses-exercise-import-export
	(concat y ":(revert-buffer t t)\r"
		x "\0xt"
		x "4bses-test.txt\r"
		x "\n-1o"
		x "xTo-1o"
		x "'crunch\r"
		x "pses-center-span\r"
		x "\0xT"
		x "o\n-1o"
		x "\0y"
		x "\0xt"
		x "\0y"
		x "12345678\r"
		x "'bunch\r"
		x "\0xtxT"
		x)))

(defun ses-exercise-macros ()
  "Executes all SES coverage-test macros."
  (dolist (x '(ses-exercise-example
	       ses-exercise-new
	       ses-exercise-display
	       ses-exercise-formulas
	       ses-exercise-recalc
	       ses-exercise-header-row
	       ses-exercise-unsafe
	       ses-exercise-rows
	       ses-exercise-columns
	       ses-exercise-editing
	       ses-exercise-sort-column
	       ses-exercise-cell-printers
	       ses-exercise-spanning-printers
	       ses-exercise-paste-1buf
	       ses-exercise-paste-2buf
	       ses-exercise-import-export))
    (message "<Testing %s>" x)
    (execute-kbd-macro x)))

(defun ses-exercise-signals ()
  "Exercise code paths that lead to error signals, other than those for
spreadsheet files with invalid formatting."
  (message "<Checking for expected errors>")
  (switch-to-buffer "ses-test.ses")
  (deactivate-mark)
  (ses-jump 'A1)
  (ses-set-curcell)
  (dolist (x '((ses-column-widths 14)
	       (ses-column-printers "%s")
	       (ses-column-printers ["%s" "%s" "%s"]) ;Should be two
	       (ses-column-widths [14])
	       (ses-delete-column -99)
	       (ses-delete-column 2)
	       (ses-delete-row -1)
	       (ses-goto-data 'hogwash)
	       (ses-header-row -56)
	       (ses-header-row 99)
	       (ses-insert-column -14)
	       (ses-insert-row 0)
	       (ses-jump 'B8) ;Covered by preceding cell
	       (ses-printer-validate '("%s" t))
	       (ses-printer-validate '([47]))
	       (ses-read-header-row -1)
	       (ses-read-header-row 32767)
	       (ses-relocate-all 0 0 -1 1)
	       (ses-relocate-all 0 0 1 -1)
	       (ses-select (ses-range A1 A2) 'x (ses-range B1 B1))
	       (ses-set-cell 0 0 'hogwash nil)
	       (ses-set-column-width 0 0)
	       (ses-yank-cells #("a\nb"
				 0 1 (ses (A1 nil nil))
				 2 3 (ses (A3 nil nil)))
			       nil)
	       (ses-yank-cells #("ab"
				 0 1 (ses (A1 nil nil))
				 1 2 (ses (A2 nil nil)))
			       nil)
	       (ses-yank-pop nil)
	       (ses-yank-tsf "1\t2\n3" nil)
	       (let ((curcell nil)) (ses-check-curcell))
	       (let ((curcell 'A1)) (ses-check-curcell 'needrange))
	       (let ((curcell '(A1 . A2))) (ses-check-curcell 'end))
	       (let ((curcell '(A1 . A2))) (ses-sort-column "B"))
	       (let ((curcell '(C1 . D2))) (ses-sort-column "B"))
	       (execute-kbd-macro "jB10\n2")
	       (execute-kbd-macro [?j ?B ?9 ?\n ?\C-@ ?\C-f ?\C-f cut])
	       (progn (kill-new "x") (execute-kbd-macro ">n"))
	       (execute-kbd-macro "\0w")))
    (condition-case nil
	(progn
	  (eval x)
	  (signal 'singularity-error nil)) ;Shouldn't get here
      (singularity-error (error "No error from %s?" x))
      (error nil)))
  ;;Test quit-handling in ses-update-cells.  Cant' use `eval' here.
  (let ((inhibit-quit t))
    (setq quit-flag t)
    (condition-case nil
	(progn
	  (ses-update-cells '(A1))
	  (signal 'singularity-error nil))
      (singularity-error (error "Quit failure in ses-update-cells"))
      (error nil))
    (setq quit-flag nil)))

(defun ses-exercise-invalid-spreadsheets ()
  "Execute code paths that detect invalid spreadsheet files."
  ;;Detect invalid spreadsheets
  (let ((p&d "\n\n\n(ses-cell A1 nil nil nil nil)\n\n")
	(cw  "(ses-column-widths [7])\n")
	(cp  "(ses-column-printers [ses-center])\n")
	(dp  "(ses-default-printer \"%.7g\")\n")
	(hr  "(ses-header-row 0)\n")
	(p11 "(2 1 1)")
	(igp ses-initial-global-parameters))
    (dolist (x (list "(1)"
		     "(x 2 3)"
		     "(1 x 3)"
		     "(1 -1 0)"
		     "(1 2 x)"
		     "(1 2 -1)"
		     "(3 1 1)"
		     "\n\n(2 1 1)"
		     "\n\n\n(ses-cell)(2 1 1)"
		     "\n\n\n(x)\n(2 1 1)"
		     "\n\n\n\n(ses-cell A2)\n(2 2 2)"
		     "\n\n\n\n(ses-cell B1)\n(2 2 2)"
		     "\n\n\n(ses-cell A1 nil nil nil nil)\n(2 1 1)"
		     (concat p&d "(x)\n(x)\n(x)\n(x)\n" p11)
		     (concat p&d "(ses-column-widths)(x)\n(x)\n(x)\n" p11)
		     (concat p&d cw "(x)\n(x)\n(x)\n(2 1 1)")
		     (concat p&d cw "(ses-column-printers)(x)\n(x)\n" p11)
		     (concat p&d cw cp "(x)\n(x)\n" p11)
		     (concat p&d cw cp "(ses-default-printer)(x)\n" p11)
		     (concat p&d cw cp dp "(x)\n" p11)
		     (concat p&d cw cp dp "(ses-header-row)" p11)
		     (concat p&d cw cp dp hr p11)
		     (concat p&d cw cp dp "\n" hr igp)))
      (condition-case nil
	  (with-temp-buffer
	    (insert x)
	    (ses-load)
	    (signal 'singularity-error nil)) ;Shouldn't get here
	(singularity-error (error "%S is an invalid spreadsheet!" x))
	(error nil)))))

(defun ses-exercise-startup ()
  "Prepare for coverage tests"
  ;;Clean up from any previous runs
  (condition-case nil (kill-buffer "ses-example.ses") (error nil))
  (condition-case nil (kill-buffer "ses-test.ses") (error nil))
  (condition-case nil (delete-file "ses-test.ses") (file-error nil))
  (delete-other-windows) ;Needed for "\C-xo" in ses-exercise-editing
  (setq ses-mode-map nil) ;Force rebuild
  (testcover-unmark-all "ses.el")
  ;;Enable
  (let ((testcover-1value-functions
	 ;;forward-line always returns 0, for us.
	 ;;remove-text-properties always returns t for us.
	 ;;ses-recalculate-cell returns the same " " any time curcell is a cons
	 ;;Macros ses-dorange and ses-dotimes-msg generate code that always
	 ;;  returns nil
	 (append '(forward-line remove-text-properties ses-recalculate-cell
		   ses-dorange ses-dotimes-msg)
		 testcover-1value-functions))
	(testcover-constants
	 ;;These maps get initialized, then never changed again
	 (append '(ses-mode-map ses-mode-print-map ses-mode-edit-map)
		 testcover-constants)))
    (testcover-start "ses.el" t))
  (require 'unsafep)) ;In case user has safe-functions = t!


;;;#########################################################################
(defun ses-exercise ()
  "Executes all SES coverage tests and displays the results."
  (interactive)
  (ses-exercise-startup)
  ;;Run the keyboard-macro tests
  (let ((safe-functions nil)
	(ses-initial-size '(1 . 1))
	(ses-initial-column-width 7)
	(ses-initial-default-printer "%.7g")
	(ses-after-entry-functions '(forward-char))
	(ses-mode-hook nil))
    (ses-exercise-macros)
    (ses-exercise-signals)
    (ses-exercise-invalid-spreadsheets)
    ;;Upgrade of old-style spreadsheet
    (with-temp-buffer
      (insert "       \n\n\n(ses-cell A1 nil nil nil nil)\n\n(ses-column-widths [7])\n(ses-column-printers [nil])\n(ses-default-printer \"%.7g\")\n\n( ;Global parameters (these are read first)\n 1 ;SES file-format\n 1 ;numrows\n 1 ;numcols\n)\n\n")
      (ses-load))
    ;;ses-vector-delete is always called from buffer-undo-list with the same
    ;;symbol as argument.  We'll give it a different one here.
    (let ((x [1 2 3]))
      (ses-vector-delete 'x 0 0))
    ;;ses-create-header-string behaves differently in a non-window environment
    ;;but we always test under windows.
    (let ((window-system (not window-system)))
      (scroll-left 7)
      (ses-create-header-string))
    ;;Test for nonstandard after-entry functions
    (let ((ses-after-entry-functions '(forward-line))
	  ses-mode-hook)
      (ses-read-cell 0 0 1)
      (ses-read-symbol 0 0 t)))
  ;;Tests with unsafep disabled
  (let ((safe-functions t)
	ses-mode-hook)
    (message "<Checking safe-functions = t>")
    (kill-buffer "ses-example.ses")
    (find-file "ses-example.ses"))
  ;;Checks for nonstandard default values for new spreadsheets
  (let (ses-mode-hook)
    (dolist (x '(("%.6g" 8 (2 . 2))
		 ("%.8g" 6 (3 . 3))))
      (let ((ses-initial-size            (nth 2 x))
	    (ses-initial-column-width    (nth 1 x))
	    (ses-initial-default-printer (nth 0 x)))
	(with-temp-buffer
	  (set-buffer-modified-p t)
	  (ses-mode)))))
  ;;Test error-handling in command hook, outside a macro.
  ;;This will ring the bell.
  (let (curcell-overlay)
    (ses-command-hook))
  ;;Due to use of run-with-timer, ses-command-hook sometimes gets called
  ;;after we switch to another buffer.
  (switch-to-buffer "*scratch*")
  (ses-command-hook)
  ;;Print results
  (message "<Marking source code>")
  (testcover-mark-all "ses.el")
  (testcover-next-mark)
  ;;Cleanup
  (delete-other-windows)
  (kill-buffer "ses-test.txt")
  ;;Could do this here: (testcover-end "ses.el")
  (message "Done"))

;; testcover-ses.el ends here.
