;;; find-dired.el --- run a `find' command and dired the output

;; Copyright (C) 1992, 1994-1995, 2000-2012 Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.org>,
;;	   Sebastian Kremer <sk@thp.uni-koeln.de>
;; Maintainer: FSF
;; Keywords: unix

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

(require 'dired)

(defgroup find-dired nil
  "Run a `find' command and dired the output."
  :group 'dired
  :prefix "find-")

;; FIXME this option does not really belong in this file, it's more general.
;; Eg cf some tests in grep.el.
(defcustom find-exec-terminator
  (if (eq 0
	  (ignore-errors
	    (process-file find-program nil nil nil
			  null-device "-exec" "echo" "{}" "+")))
      "+"
    (shell-quote-argument ";"))
  "String that terminates \"find -exec COMMAND {} \".
The value should include any needed quoting for the shell.
Common values are \"+\" and \"\\\\;\", with the former more efficient
than the latter."
  :version "24.1"
  :group 'find-dired
  :type 'string)

;; find's -ls corresponds to these switches.
;; Note -b, at least GNU find quotes spaces etc. in filenames
(defcustom find-ls-option
  (if (eq 0
	  (ignore-errors
	    (process-file find-program nil nil nil null-device "-ls")))
      (cons "-ls"
	    (if (eq system-type 'berkeley-unix)
		"-gilsb"
	      "-dilsb"))
    (cons
     (format "-exec ls -ld {} %s" find-exec-terminator)
     "-ld"))
  "A pair of options to produce and parse an `ls -l'-type list from `find'.
This is a cons of two strings (FIND-OPTION . LS-SWITCHES).
FIND-OPTION is the option (or options) passed to `find' to produce
a file listing in the desired format.  LS-SWITCHES is a set of
`ls' switches that tell dired how to parse the output of `find'.

The two options must be set to compatible values.
For example, to use human-readable file sizes with GNU ls:
   \(\"-exec ls -ldh {} +\" . \"-ldh\")

To use GNU find's inbuilt \"-ls\" option to list files:
   \(\"-ls\" . \"-dilsb\")
since GNU find's output has the same format as using GNU ls with
the options \"-dilsb\"."
  :version "24.1"	       ; add tests for -ls and -exec + support
  :type '(cons (string :tag "Find Option")
	       (string :tag "Ls Switches"))
  :group 'find-dired)

(defcustom find-ls-subdir-switches
  (if (string-match "-[a-z]*b" (cdr find-ls-option))
      "-alb"
    "-al")
  "`ls' switches for inserting subdirectories in `*Find*' buffers.
This should contain the \"-l\" switch.
Use the \"-F\" or \"-b\" switches if and only if you also use
them for `find-ls-option'."
  :version "24.1"			; add -b test
  :type 'string
  :group 'find-dired)

(defcustom find-grep-options
  (if (or (eq system-type 'berkeley-unix)
	  (string-match "solaris2\\|irix" system-configuration))
      "-s" "-q")
  "Option to grep to be as silent as possible.
On Berkeley systems, this is `-s'; on Posix, and with GNU grep, `-q' does it.
On other systems, the closest you can come is to use `-l'."
  :type 'string
  :group 'find-dired)

;; This used to be autoloaded (see bug#4387).
(defcustom find-name-arg
  (if read-file-name-completion-ignore-case
      "-iname"
    "-name")
  "Argument used to specify file name pattern.
If `read-file-name-completion-ignore-case' is non-nil, -iname is used so that
find also ignores case.  Otherwise, -name is used."
  :type 'string
  :group 'find-dired
  :version "22.2")

(defvar find-args nil
  "Last arguments given to `find' by \\[find-dired].")

;; History of find-args values entered in the minibuffer.
(defvar find-args-history nil)

(defvar dired-sort-inhibit)

;;;###autoload
(defun find-dired (dir args)
  "Run `find' and go into Dired mode on a buffer of the output.
The command run (after changing into DIR) is essentially

    find . \\( ARGS \\) -ls

except that the car of the variable `find-ls-option' specifies what to
use in place of \"-ls\" as the final argument."
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		     (read-string "Run find (with args): " find-args
				  '(find-args-history . 1))))
  (let ((dired-buffers dired-buffers))
    ;; Expand DIR ("" means default-directory), and make sure it has a
    ;; trailing slash.
    (setq dir (file-name-as-directory (expand-file-name dir)))
    ;; Check that it's really a directory.
    (or (file-directory-p dir)
	(error "find-dired needs a directory: %s" dir))
    (switch-to-buffer (get-buffer-create "*Find*"))

    ;; See if there's still a `find' running, and offer to kill
    ;; it first, if it is.
    (let ((find (get-buffer-process (current-buffer))))
      (when find
	(if (or (not (eq (process-status find) 'run))
		(yes-or-no-p "A `find' process is running; kill it? "))
	    (condition-case nil
		(progn
		  (interrupt-process find)
		  (sit-for 1)
		  (delete-process find))
	      (error nil))
	  (error "Cannot have two processes in `%s' at once" (buffer-name)))))

    (widen)
    (kill-all-local-variables)
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq default-directory dir
	  find-args args	      ; save for next interactive call
	  args (concat find-program " . "
		       (if (string= args "")
			   ""
			 (concat
			  (shell-quote-argument "(")
			  " " args " "
			  (shell-quote-argument ")")
			  " "))
		       (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
					 (car find-ls-option))
			   (format "%s %s %s"
				   (match-string 1 (car find-ls-option))
				   (shell-quote-argument "{}")
				   find-exec-terminator)
			 (car find-ls-option))))
    ;; Start the find process.
    (shell-command (concat args "&") (current-buffer))
    ;; The next statement will bomb in classic dired (no optional arg allowed)
    (dired-mode dir (cdr find-ls-option))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map "\C-c\C-k" 'kill-find)
      (use-local-map map))
    (make-local-variable 'dired-sort-inhibit)
    (setq dired-sort-inhibit t)
    (set (make-local-variable 'revert-buffer-function)
	 `(lambda (ignore-auto noconfirm)
	    (find-dired ,dir ,find-args)))
    ;; Set subdir-alist so that Tree Dired will work:
    (if (fboundp 'dired-simple-subdir-alist)
	;; will work even with nested dired format (dired-nstd.el,v 1.15
	;; and later)
	(dired-simple-subdir-alist)
      ;; else we have an ancient tree dired (or classic dired, where
      ;; this does no harm)
      (set (make-local-variable 'dired-subdir-alist)
	   (list (cons default-directory (point-min-marker)))))
    (set (make-local-variable 'dired-subdir-switches) find-ls-subdir-switches)
    (setq buffer-read-only nil)
    ;; Subdir headlerline must come first because the first marker in
    ;; subdir-alist points there.
    (insert "  " dir ":\n")
    ;; Make second line a ``find'' line in analogy to the ``total'' or
    ;; ``wildcard'' line.
    (insert "  " args "\n")
    (setq buffer-read-only t)
    (let ((proc (get-buffer-process (current-buffer))))
      (set-process-filter proc (function find-dired-filter))
      (set-process-sentinel proc (function find-dired-sentinel))
      ;; Initialize the process marker; it is used by the filter.
      (move-marker (process-mark proc) 1 (current-buffer)))
    (setq mode-line-process '(":%s"))))

(defun kill-find ()
  "Kill the `find' process running in the current buffer."
  (interactive)
  (let ((find (get-buffer-process (current-buffer))))
    (and find (eq (process-status find) 'run)
	 (eq (process-filter find) (function find-dired-filter))
	 (condition-case nil
	     (delete-process find)
	   (error nil)))))

;;;###autoload
(defun find-name-dired (dir pattern)
  "Search DIR recursively for files matching the globbing pattern PATTERN,
and run dired on those files.
PATTERN is a shell wildcard (not an Emacs regexp) and need not be quoted.
The command run (after changing into DIR) is

    find . -name 'PATTERN' -ls"
  (interactive
   "DFind-name (directory): \nsFind-name (filename wildcard): ")
  (find-dired dir (concat find-name-arg " " (shell-quote-argument pattern))))

;; This functionality suggested by
;; From: oblanc@watcgl.waterloo.edu (Olivier Blanc)
;; Subject: find-dired, lookfor-dired
;; Date: 10 May 91 17:50:00 GMT
;; Organization: University of Waterloo

(defalias 'lookfor-dired 'find-grep-dired)
;;;###autoload
(defun find-grep-dired (dir regexp)
  "Find files in DIR containing a regexp REGEXP and start Dired on output.
The command run (after changing into DIR) is

  find . \\( -type f -exec `grep-program' `find-grep-options' \\
    -e REGEXP {} \\; \\) -ls

where the car of the variable `find-ls-option' specifies what to
use in place of \"-ls\" as the final argument."
  ;; Doc used to say "Thus ARG can also contain additional grep options."
  ;; i) Presumably ARG == REGEXP?
  ;; ii) No it can't have options, since it gets shell-quoted.
  (interactive "DFind-grep (directory): \nsFind-grep (grep regexp): ")
  ;; find -exec doesn't allow shell i/o redirections in the command,
  ;; or we could use `grep -l >/dev/null'
  ;; We use -type f, not ! -type d, to avoid getting screwed
  ;; by FIFOs and devices.  I'm not sure what's best to do
  ;; about symlinks, so as far as I know this is not wrong.
  (find-dired dir
	      (concat "-type f -exec " grep-program " " find-grep-options " -e "
		      (shell-quote-argument regexp)
		      " "
		      (shell-quote-argument "{}")
		      " "
		      ;; Doesn't work with "+".
		      (shell-quote-argument ";"))))

(defun find-dired-filter (proc string)
  ;; Filter for \\[find-dired] processes.
  (let ((buf (process-buffer proc))
	(inhibit-read-only t))
    (if (buffer-name buf)
	(with-current-buffer buf
	  (save-excursion
	    (save-restriction
	      (widen)
	      (let ((buffer-read-only nil)
		    (beg (point-max))
		    (l-opt (and (consp find-ls-option)
				(string-match "l" (cdr find-ls-option))))
		    (ls-regexp (concat "^ +[^ \t\r\n]+\\( +[^ \t\r\n]+\\) +"
				       "[^ \t\r\n]+ +[^ \t\r\n]+\\( +[0-9]+\\)")))
		(goto-char beg)
		(insert string)
		(goto-char beg)
		(or (looking-at "^")
		    (forward-line 1))
		(while (looking-at "^")
		  (insert "  ")
		  (forward-line 1))
		;; Convert ` ./FILE' to ` FILE'
		;; This would lose if the current chunk of output
		;; starts or ends within the ` ./', so back up a bit:
		(goto-char (- beg 3))	; no error if < 0
		(while (search-forward " ./" nil t)
		  (delete-region (point) (- (point) 2)))
		;; Pad the number of links and file size.  This is a
		;; quick and dirty way of getting the columns to line up
		;; most of the time, but it's not foolproof.
		(when l-opt
		  (goto-char beg)
		  (goto-char (line-beginning-position))
		  (while (re-search-forward ls-regexp nil t)
		    (replace-match (format "%4s" (match-string 1))
				   nil nil nil 1)
		    (replace-match (format "%9s" (match-string 2))
				   nil nil nil 2)
		    (forward-line 1)))
		;; Find all the complete lines in the unprocessed
		;; output and process it to add text properties.
		(goto-char (point-max))
		(if (search-backward "\n" (process-mark proc) t)
		    (progn
		      (dired-insert-set-properties (process-mark proc)
						   (1+ (point)))
		      (move-marker (process-mark proc) (1+ (point)))))))))
      ;; The buffer has been killed.
      (delete-process proc))))

(defun find-dired-sentinel (proc state)
  ;; Sentinel for \\[find-dired] processes.
  (let ((buf (process-buffer proc))
	(inhibit-read-only t))
    (if (buffer-name buf)
	(with-current-buffer buf
	  (let ((buffer-read-only nil))
	    (save-excursion
	      (goto-char (point-max))
	      (insert "\n  find " state)
	      (forward-char -1)		;Back up before \n at end of STATE.
	      (insert " at " (substring (current-time-string) 0 19))
	      (forward-char 1)
	      (setq mode-line-process
		    (concat ":"
			    (symbol-name (process-status proc))))
	      ;; Since the buffer and mode line will show that the
	      ;; process is dead, we can delete it now.  Otherwise it
	      ;; will stay around until M-x list-processes.
	      (delete-process proc)
	      (force-mode-line-update)))
	  (message "find-dired %s finished." (current-buffer))))))


(provide 'find-dired)

;;; find-dired.el ends here
