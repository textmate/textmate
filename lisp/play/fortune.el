;;; fortune.el --- use fortune to create signatures

;; Copyright (C) 1999, 2001-2012  Free Software Foundation, Inc.

;; Author: Holger Schauer <Holger.Schauer@gmx.de>
;; Keywords: games utils mail

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
;; This utility allows you to automatically cut regions to a fortune
;; file.  In case that the region stems from an article buffer (mail or
;; news), it will try to automatically determine the author of the
;; fortune.  It will also allow you to compile your fortune-database
;; as well as providing a function to extract a fortune for use as your
;; signature.
;; Of course, it can simply display a fortune, too.
;; Use prefix arguments to specify different fortune databases.

;;; Installation:

;; Please check the customize settings -- you will at least have to
;; modify the values of `fortune-dir' and `fortune-file'.

;; I then use this in my .gnus:
;;(message "Making new signature: %s" (fortune-to-signature "~/fortunes/"))
;; This automagically creates a new signature when starting up Gnus.
;; Note that the call to fortune-to-signature specifies a directory in which
;; several fortune-files and their databases are stored.

;; If you like to get a new signature for every message, you can also hook
;; it into message-mode:
;; (add-hook 'message-setup-hook 'fortune-to-signature)
;; This time no fortune-file is specified, so fortune-to-signature would use
;; the default-file as specified by fortune-file.

;; I have also this in my .gnus:
;;(add-hook 'gnus-article-mode-hook
;;	  (lambda ()
;;	     (define-key gnus-article-mode-map "i" 'fortune-from-region)))
;; which allows marking a region and then pressing "i" so that the marked
;; region will be automatically added to my favorite fortune-file.

;;; Code:

;;; **************
;;; Customizable Settings
(defgroup fortune nil
  "Settings for fortune."
  :link '(emacs-commentary-link "fortune.el")
  :version "21.1"
  :group 'games)
(defgroup fortune-signature nil
  "Settings for use of fortune for signatures."
  :group 'fortune
  :group 'mail)

(defcustom fortune-dir "~/docs/ascii/misc/fortunes/"
  "The directory to look in for local fortune cookies files."
  :type 'directory
  :group 'fortune)
(defcustom fortune-file
  (expand-file-name "usenet" fortune-dir)
  "The file in which local fortune cookies will be stored."
  :type 'file
  :group 'fortune)
(defcustom fortune-database-extension  ".dat"
  "The extension of the corresponding fortune database.
Normally you won't have a reason to change it."
  :type 'string
  :group 'fortune)
(defcustom fortune-program "fortune"
  "Program to select a fortune cookie."
  :type 'string
  :group 'fortune)
(defcustom fortune-program-options ()
  "List of options to pass to the fortune program."
  :type '(choice (repeat (string :tag "Option"))
                 (string :tag "Obsolete string of options"))
  :version "23.1"
  :group 'fortune)
(defcustom fortune-strfile "strfile"
  "Program to compute a new fortune database."
  :type 'string
  :group 'fortune)
(defcustom fortune-strfile-options ""
  "Options to pass to the strfile program (a string)."
  :type 'string
  :group 'fortune)
(defcustom fortune-quiet-strfile-options "> /dev/null"
  "Text added to the command for running `strfile'.
By default it discards the output produced by `strfile'.
Set this to \"\" if you would like to see the output."
  :type 'string
  :group 'fortune)

(defcustom fortune-always-compile t
  "Non-nil means automatically compile fortune files.
If nil, you must invoke `fortune-compile' manually to do that."
  :type 'boolean
  :group 'fortune)
(defcustom fortune-author-line-prefix "                  -- "
  "Prefix to put before the author name of a fortunate."
  :type 'string
  :group 'fortune-signature)
(defcustom fortune-fill-column fill-column
  "Fill column for fortune files."
  :type 'integer
  :group 'fortune-signature)
(defcustom fortune-from-mail "private e-mail"
  "String to use to characterize that the fortune comes from an e-mail.
No need to add an `in'."
  :type 'string
  :group 'fortune-signature)
(defcustom fortune-sigstart ""
  "Some text to insert before the fortune cookie, in a mail signature."
  :type 'string
  :group 'fortune-signature)
(defcustom fortune-sigend ""
  "Some text to insert after the fortune cookie, in a mail signature."
  :type 'string
  :group 'fortune-signature)


;; not customizable settings
(defvar fortune-buffer-name "*fortune*")
(defconst fortune-end-sep "\n%\n")


;;; **************
;;; Inserting a new fortune
(defun fortune-append (string &optional interactive file)
  "Appends STRING to the fortune FILE.

If INTERACTIVE is non-nil, don't compile the fortune file afterwards."
  (setq file (expand-file-name
	      (substitute-in-file-name (or file fortune-file))))
  (if (file-directory-p file)
      (error "Cannot append fortune to directory %s" file))
  (if interactive ; switch to file and return buffer
      (find-file-other-frame file)
    (find-file-noselect file))
  (let ((fortune-buffer (get-file-buffer file)))

    (set-buffer fortune-buffer)
    (goto-char (point-max))
    (setq fill-column fortune-fill-column)
    (setq auto-fill-inhibit-regexp "^%")
    (turn-on-auto-fill)
    (insert string fortune-end-sep)
    (unless interactive
      (save-buffer)
      (if fortune-always-compile
	  (fortune-compile file)))))

(defun fortune-ask-file ()
  "Asks the user for a file-name."
  (expand-file-name
   (read-file-name
    "Fortune file to use: "
    fortune-dir nil nil "")))

;;;###autoload
(defun fortune-add-fortune (string file)
  "Add STRING to a fortune file FILE.

Interactively, if called with a prefix argument,
read the file name to use.  Otherwise use the value of `fortune-file'."
  (interactive
   (list (read-string "Fortune: ")
	 (if current-prefix-arg (fortune-ask-file))))
  (fortune-append string t file))

;;;###autoload
(defun fortune-from-region (beg end file)
  "Append the current region to a local fortune-like data file.

Interactively, if called with a prefix argument,
read the file name to use.  Otherwise use the value of `fortune-file'."
  (interactive
   (list (region-beginning) (region-end)
	 (if current-prefix-arg (fortune-ask-file))))
  (let ((string (buffer-substring beg end))
	author newsgroup help-point)
    ;; try to determine author ...
    (save-excursion
      (goto-char (point-min))
      (setq help-point
	    (search-forward-regexp
	     "^From: \\(.*\\)$"
	     (point-max) t))
      (if help-point
	  (setq author (buffer-substring (match-beginning 1) help-point))
	(setq author "An unknown author")))
    ;; ... and newsgroup
    (save-excursion
      (goto-char (point-min))
      (setq help-point
	    (search-forward-regexp
	     "^Newsgroups: \\(.*\\)$"
	     (point-max) t))
      (if help-point
	  (setq newsgroup (buffer-substring (match-beginning 1) help-point))
	(setq newsgroup (if (or (eq major-mode 'gnus-article-mode)
				(eq major-mode 'vm-mode)
				(eq major-mode 'rmail-mode))
			    fortune-from-mail
			  "unknown"))))

    ;; append entry to end of fortune file, and display result
    (setq string (concat "\"" string "\""
			 "\n"
			 fortune-author-line-prefix
			 author " in " newsgroup))
    (fortune-append string t file)))


;;; **************
;;; Compile new database with strfile
;;;###autoload
(defun fortune-compile (&optional file)
  "Compile fortune file.

If called with a prefix asks for the FILE to compile, otherwise uses
the value of `fortune-file'.  This currently cannot handle directories."
  (interactive
    (list
     (if current-prefix-arg
	 (fortune-ask-file)
       fortune-file)))
  (let* ((fortune-file (expand-file-name (substitute-in-file-name file)))
	 (fortune-dat (expand-file-name
		       (substitute-in-file-name
			(concat fortune-file fortune-database-extension)))))
  (cond ((file-exists-p fortune-file)
	 (if (file-exists-p fortune-dat)
	     (cond ((file-newer-than-file-p fortune-file fortune-dat)
		    (message "Compiling new fortune database %s" fortune-dat)
		    (shell-command
		     (concat fortune-strfile fortune-strfile-options
			     " " fortune-file fortune-quiet-strfile-options))))))
	(t (error "Can't compile fortune file %s" fortune-file)))))


;;; **************
;;; Use fortune for signature
;;;###autoload
(defun fortune-to-signature (&optional file)
  "Create signature from output of the fortune program.

If called with a prefix asks for the FILE to choose the fortune from,
otherwise uses the value of `fortune-file'.  If you want to have fortune
choose from a set of files in a directory, call interactively with prefix
and choose the directory as the fortune-file."
  (interactive
    (list
     (if current-prefix-arg
	 (fortune-ask-file)
       fortune-file)))
   (save-excursion
    (fortune-in-buffer t file)
    (set-buffer fortune-buffer-name)
    (let* ((fortune (buffer-string))
	   (signature (concat fortune-sigstart fortune fortune-sigend)))
      (setq mail-signature signature)
      (if (boundp 'message-signature)
	  (setq message-signature signature)))))


;;; **************
;;; Display fortune
(defun fortune-in-buffer (_interactive &optional file)
  "Put a fortune cookie in the *fortune* buffer.
INTERACTIVE is ignored.  Optional argument FILE, when supplied,
specifies the file to choose the fortune from."
  (let ((fortune-buffer (or (get-buffer fortune-buffer-name)
			    (generate-new-buffer fortune-buffer-name)))
	(fort-file (expand-file-name
		    (substitute-in-file-name
		     (or file fortune-file)))))
    (with-current-buffer fortune-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if fortune-always-compile
            (fortune-compile fort-file))
        (apply 'call-process
               fortune-program            ; program to call
               nil fortune-buffer nil     ; INFILE BUFFER DISPLAY
               (append (if (stringp fortune-program-options)
                           (split-string fortune-program-options)
                         fortune-program-options) (list fort-file)))))))

;;;###autoload
(defun fortune (&optional file)
  "Display a fortune cookie.
If called with a prefix asks for the FILE to choose the fortune from,
otherwise uses the value of `fortune-file'.  If you want to have fortune
choose from a set of files in a directory, call interactively with prefix
and choose the directory as the fortune-file."
  (interactive (list (if current-prefix-arg
                         (fortune-ask-file)
                       fortune-file)))
  (fortune-in-buffer t file)
  (switch-to-buffer (get-buffer fortune-buffer-name))
  (setq buffer-read-only t))


;;; Provide ourselves.
(provide 'fortune)

;;; fortune.el ends here
