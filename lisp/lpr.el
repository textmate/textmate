;;; lpr.el --- print Emacs buffer on line printer

;; Copyright (C) 1985, 1988, 1992, 1994, 2001-2012
;;   Free Software Foundation, Inc.

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

;; Commands to send the region or a buffer to your printer.  Entry points
;; are `lpr-buffer', `print-buffer', `lpr-region', or `print-region'; option
;; variables include `printer-name', `lpr-switches' and `lpr-command'.

;;; Code:

(eval-when-compile (require 'cl))

;;;###autoload
(defvar lpr-windows-system
  (memq system-type '(ms-dos windows-nt))
  "Non-nil if running on MS-DOS or MS Windows.")

;;;###autoload
(defvar lpr-lp-system
  (memq system-type '(usg-unix-v hpux irix))
  "Non-nil if running on a system type that uses the \"lp\" command.")


(defgroup lpr nil
  "Print Emacs buffer on line printer."
  :group 'wp)


;;;###autoload
(defcustom printer-name
  (and (eq system-type 'ms-dos) "PRN")
  "The name of a local printer to which data is sent for printing.
\(Note that PostScript files are sent to `ps-printer-name', which see.\)

On Unix-like systems, a string value should be a name understood by
lpr's -P option; otherwise the value should be nil.

On MS-DOS and MS-Windows systems, a string value is taken as the name of
a printer device or port, provided `lpr-command' is set to \"\".
Typical non-default settings would be \"LPT1\" to \"LPT3\" for parallel
printers, or \"COM1\" to \"COM4\" or \"AUX\" for serial printers, or
\"//hostname/printer\" for a shared network printer.  You can also set
it to the name of a file, in which case the output gets appended to that
file.  If you want to discard the printed output, set this to \"NUL\"."
  :type '(choice :menu-tag "Printer Name"
		 :tag "Printer Name"
		 (const :tag "Default" nil)
		 ;; could use string but then we lose completion for files.
		 (file :tag "Name"))
  :group 'lpr)

;;;###autoload
(defcustom lpr-switches nil
  "List of strings to pass as extra options for the printer program.
It is recommended to set `printer-name' instead of including an explicit
switch on this list.
See `lpr-command'."
  :type '(repeat (string :tag "Argument"))
  :group 'lpr)

(defcustom lpr-add-switches (memq system-type '(berkeley-unix gnu/linux))
  "Non-nil means construct `-T' and `-J' options for the printer program.
These are made assuming that the program is `lpr';
if you are using some other incompatible printer program,
this variable should be nil."
  :type 'boolean
  :group 'lpr)

(defcustom lpr-printer-switch
  (if lpr-lp-system
      "-d "
    "-P")
  "Printer switch, that is, something like \"-P\", \"-d \", \"/D:\", etc.
This switch is used in conjunction with `printer-name'."
  :type '(choice :menu-tag "Printer Name Switch"
		 :tag "Printer Name Switch"
		 (const :tag "None" nil)
		 (string :tag "Printer Switch"))
  :group 'lpr)

;;;###autoload
(defcustom lpr-command
 (purecopy
  (cond
   (lpr-windows-system
    "")
   (lpr-lp-system
    "lp")
   (t
    "lpr")))
  "Name of program for printing a file.

On MS-DOS and MS-Windows systems, if the value is an empty string then
Emacs will write directly to the printer port named by `printer-name'.
The programs `print' and `nprint' (the standard print programs on
Windows NT and Novell Netware respectively) are handled specially, using
`printer-name' as the destination for output; any other program is
treated like `lpr' except that an explicit filename is given as the last
argument."
  :type 'string
  :group 'lpr)

;; Default is nil, because that enables us to use pr -f
;; which is more reliable than pr with no args, which is what lpr -p does.
(defcustom lpr-headers-switches nil
  "List of strings of options to request page headings in the printer program.
If nil, we run `lpr-page-header-program' to make page headings
and print the result."
  :type '(repeat (string :tag "Argument"))
  :group 'lpr)

(defcustom print-region-function nil
  "Function to call to print the region on a printer.
See definition of `print-region-1' for calling conventions."
  :type '(choice (const nil) function)
  :group 'lpr)

(defcustom lpr-page-header-program "pr"
  "Name of program for adding page headers to a file."
  :type 'string
  :group 'lpr)

;; Berkeley systems support -F, and GNU pr supports both -f and -F,
;; So it looks like -F is a better default.
(defcustom lpr-page-header-switches '("-h" "%s" "-F")
  "List of strings to use as options for the page-header-generating program.
If `%s' appears in any of the strings, it is substituted by the page title.
Note that for correct quoting, `%s' should normally be a separate element.
The variable `lpr-page-header-program' specifies the program to use."
  :type '(repeat string)
  :group 'lpr)

;;;###autoload
(defun lpr-buffer ()
  "Print buffer contents without pagination or page headers.
See the variables `lpr-switches' and `lpr-command'
for customization of the printer command."
  (interactive
   (unless (y-or-n-p "Send current buffer to default printer? ")
     (error "Cancelled")))
  (print-region-1 (point-min) (point-max) lpr-switches nil))

;;;###autoload
(defun print-buffer ()
  "Paginate and print buffer contents.

The variable `lpr-headers-switches' controls how to paginate.
If it is nil (the default), we run the `pr' program (or whatever program
`lpr-page-header-program' specifies) to paginate.
`lpr-page-header-switches' specifies the switches for that program.

Otherwise, the switches in `lpr-headers-switches' are used
in the print command itself; we expect them to request pagination.

See the variables `lpr-switches' and `lpr-command'
for further customization of the printer command."
  (interactive
   (unless (y-or-n-p "Send current buffer to default printer? ")
     (error "Cancelled")))
  (print-region-1 (point-min) (point-max) lpr-switches t))

;;;###autoload
(defun lpr-region (start end)
  "Print region contents without pagination or page headers.
See the variables `lpr-switches' and `lpr-command'
for customization of the printer command."
  (interactive
   (if (y-or-n-p "Send selected text to default printer? ")
       (list (region-beginning) (region-end))
     (error "Cancelled")))
  (print-region-1 start end lpr-switches nil))

;;;###autoload
(defun print-region (start end)
  "Paginate and print the region contents.

The variable `lpr-headers-switches' controls how to paginate.
If it is nil (the default), we run the `pr' program (or whatever program
`lpr-page-header-program' specifies) to paginate.
`lpr-page-header-switches' specifies the switches for that program.

Otherwise, the switches in `lpr-headers-switches' are used
in the print command itself; we expect them to request pagination.

See the variables `lpr-switches' and `lpr-command'
for further customization of the printer command."
  (interactive
   (if (y-or-n-p "Send selected text to default printer? ")
       (list (region-beginning) (region-end))
     (error "Cancelled")))
  (print-region-1 start end lpr-switches t))

(defun print-region-1 (start end switches page-headers)
  ;; On some MIPS system, having a space in the job name
  ;; crashes the printer demon.  But using dashes looks ugly
  ;; and it seems to annoying to do for that MIPS system.
  (let ((name  (concat (buffer-name) " Emacs buffer"))
	(title (concat (buffer-name) " Emacs buffer"))
	;; Make pipes use the same coding system as
	;; writing the buffer to a file would.
	(coding-system-for-write (or coding-system-for-write
				     buffer-file-coding-system))
	(coding-system-for-read  (or coding-system-for-read
				     buffer-file-coding-system))
	(width tab-width)
	nswitches
	switch-string)
    (save-excursion
      (and page-headers lpr-headers-switches
	   ;; It's possible to use an lpr option to get page headers.
	   (setq switches (append (if (stringp lpr-headers-switches)
				      (list lpr-headers-switches)
				    lpr-headers-switches)
				  switches)))
      (setq nswitches     (lpr-flatten-list
			   (mapcar 'lpr-eval-switch ; Dynamic evaluation
				   switches))
	    switch-string (if switches
			      (concat " with options "
				      (mapconcat 'identity switches " "))
			    ""))
      (message "Spooling%s..." switch-string)
      (if (/= tab-width 8)
	  (let ((new-coords (print-region-new-buffer start end)))
	    (setq start     (car new-coords)
		  end       (cdr new-coords)
		  tab-width width)
	    (save-excursion
	      (goto-char end)
	      (setq end (point-marker)))
	    (untabify (point-min) (point-max))))
      (if page-headers
	  (if lpr-headers-switches
	      ;; We handled this above by modifying SWITCHES.
	      nil
	    ;; Run a separate program to get page headers.
	    (let ((new-coords (print-region-new-buffer start end)))
	      (apply 'call-process-region (car new-coords) (cdr new-coords)
		     lpr-page-header-program t t nil
		     (mapcar (lambda (e) (format e title))
			     lpr-page-header-switches)))
	    (setq start (point-min)
		  end   (point-max))))
      (let ((buf (current-buffer)))
        (with-temp-buffer
          (let ((tempbuf (current-buffer)))
            (with-current-buffer buf
              (apply (or print-region-function 'call-process-region)
                     (nconc (list start end lpr-command
                                  nil tempbuf nil)
                            (and lpr-add-switches
                                 (list "-J" name))
                            ;; These belong in pr if we are using that.
                            (and lpr-add-switches lpr-headers-switches
                                 (list "-T" title))
                            (and (stringp printer-name)
                                 (list (concat lpr-printer-switch
                                               printer-name)))
                            nswitches))))
          (if (markerp end)
              (set-marker end nil))
          (message "Spooling%s...done%s%s" switch-string
                   (case (count-lines (point-min) (point-max))
                     (0 "")
                     (1 ": ")
                     (t ":\n"))
                   (buffer-string)))))))

;; This function copies the text between start and end
;; into a new buffer, makes that buffer current.
;; It returns the new range to print from the new current buffer
;; as (START . END).

(defun print-region-new-buffer (ostart oend)
  (if (string= (buffer-name) " *spool temp*")
      (cons ostart oend)
    (let ((oldbuf (current-buffer)))
      (set-buffer (get-buffer-create " *spool temp*"))
      (widen)
      (erase-buffer)
      (insert-buffer-substring oldbuf ostart oend)
      (cons (point-min) (point-max)))))

(defun printify-region (begin end)
  "Replace nonprinting characters in region with printable representations.
The printable representations use ^ (for ASCII control characters) or hex.
The characters tab, linefeed, space, return and formfeed are not affected."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (let (c)
	(while (re-search-forward "[\^@-\^h\^k\^n-\^_\177-\377]" nil t)
	  (setq c (preceding-char))
	  (delete-char -1)
	  (insert (if (< c ?\s)
		      (format "\\^%c" (+ c ?@))
		    (format "\\%02x" c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions hacked from `ps-print' package.

;; Dynamic evaluation
(defun lpr-eval-switch (arg)
  (cond ((stringp arg) arg)
	((functionp arg) (apply arg nil))
	((symbolp arg) (symbol-value arg))
	((consp arg) (apply (car arg) (cdr arg)))
	(t nil)))

;; `lpr-flatten-list' is defined here (copied from "message.el" and
;; enhanced to handle dotted pairs as well) until we can get some
;; sensible autoloads, or `flatten-list' gets put somewhere decent.

;; (lpr-flatten-list '((a . b) c (d . e) (f g h) i . j))
;; => (a b c d e f g h i j)

(defun lpr-flatten-list (&rest list)
  (lpr-flatten-list-1 list))

(defun lpr-flatten-list-1 (list)
  (cond
   ((null list) (list))
   ((consp list)
    (append (lpr-flatten-list-1 (car list))
	    (lpr-flatten-list-1 (cdr list))))
   (t (list list))))

(provide 'lpr)

;;; lpr.el ends here
