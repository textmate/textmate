;;; executable.el --- base functionality for executable interpreter scripts -*- byte-compile-dynamic: t -*-

;; Copyright (C) 1994-1996, 2000-2012  Free Software Foundation, Inc.

;; Author: Daniel Pfeiffer <occitan@esperanto.org>
;; Keywords: languages, unix

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

;; executable.el is used by certain major modes to insert a suitable
;; #! line at the beginning of the file, if the file does not already
;; have one.

;; Unless it has a magic number, a Unix file with executable mode is passed to
;; a new instance of the running shell (or to a Bourne shell if a csh is
;; running and the file starts with `:').  Only a shell can start such a file,
;; exec() cannot, which is why it is important to have a magic number in every
;; executable script.  Such a magic number is made up by the characters `#!'
;; the filename of an interpreter (in COFF, ELF or somesuch format) and one
;; optional argument.

;; This library is for certain major modes like sh-, awk-, perl-, tcl- or
;; makefile-mode to insert or update a suitable #! line at the beginning of
;; the file, if the file does not already have one and the file is not a
;; default file of that interpreter (like .profile or makefile).  It also
;; makes the file executable if it wasn't, as soon as it's saved.

;; It also allows debugging scripts, with an adaptation of compile, as far
;; as interpreters give out meaningful error messages.

;; Modes that use this should nconc `executable-map' to the end of their own
;; keymap and `executable-font-lock-keywords' to the end of their own font
;; lock keywords.  Their mode-setting commands should call
;; `executable-set-magic'.

;;; Code:

(defgroup executable nil
  "Base functionality for executable interpreter scripts."
  :group 'processes)

;; This used to default to `other', but that doesn't seem to have any
;; significance.  fx 2000-02-11.
(defcustom executable-insert t		; 'other
  "*Non-nil means offer to add a magic number to a file.
This takes effect when you switch to certain major modes,
including Shell-script mode (`sh-mode').
When you type \\[executable-set-magic], it always offers to add or
update the magic number."
;;;   :type '(choice (const :tag "off" nil)
;;; 		 (const :tag "on" t)
;;; 		 symbol)
  :type 'boolean
  :group 'executable)


(defcustom executable-query 'function
  "*If non-nil, ask user before changing an existing magic number.
When this is `function', only ask when called non-interactively."
  :type '(choice (const :tag "Don't Ask" nil)
		 (const :tag "Ask when non-interactive" function)
		 (other :tag "Ask" t))
  :group 'executable)


(defcustom executable-magicless-file-regexp "/[Mm]akefile$\\|/\\.\\(z?profile\\|bash_profile\\|z?login\\|bash_login\\|z?logout\\|bash_logout\\|.+shrc\\|esrc\\|rcrc\\|[kz]shenv\\)$"
  "*On files with this kind of name no magic is inserted or changed."
  :type 'regexp
  :group 'executable)


(defcustom executable-prefix "#! "
  "*Interpreter magic number prefix inserted when there was no magic number."
  :type 'string
  :group 'executable)


(defcustom executable-chmod 73
  "*After saving, if the file is not executable, set this mode.
This mode passed to `set-file-modes' is taken absolutely when negative, or
relative to the files existing modes.  Do nothing if this is nil.
Typical values are 73 (+x) or -493 (rwxr-xr-x)."
  :type '(choice integer
		 (const nil))
  :group 'executable)


(defvar executable-command nil)

(defcustom executable-self-display "tail"
  "*Command you use with argument `+2' to make text files self-display.
Note that the like of `more' doesn't work too well under Emacs \\[shell]."
  :type 'string
  :group 'executable)


(defvar executable-font-lock-keywords
  '(("\\`#!.*/\\([^ \t\n]+\\)" 1 font-lock-keyword-face t))
  "*Rules for highlighting executable scripts' magic number.
This can be included in `font-lock-keywords' by modes that call `executable'.")


(defvar executable-error-regexp-alist
  '(;; /bin/xyz: syntax error at line 14: `(' unexpected
    ;; /bin/xyz[5]: syntax error at line 8 : ``' unmatched
    ("^\\(.*[^[/]\\)\\(\\[[0-9]+\\]\\)?: .* error .* line \\([0-9]+\\)" 1 3)
    ;; /bin/xyz[27]: ehco:  not found
    ("^\\(.*[^/]\\)\\[\\([0-9]+\\)\\]: .*: " 1 2)
    ;; /bin/xyz: syntax error near unexpected token `)'
    ;; /bin/xyz: /bin/xyz: line 2: `)'
    ("^\\(.*[^/]\\): [^0-9\n]+\n\\1: \\1: line \\([0-9]+\\):" 1 2)
    ;; /usr/bin/awk: syntax error at line 5 of file /bin/xyz
    (" error .* line \\([0-9]+\\) of file \\(.+\\)$" 2 1)
    ;; /usr/bin/awk: calling undefined function toto
    ;;  input record number 3, file awktestdata
    ;;  source line 4 of file /bin/xyz
    ("^[^ ].+\n\\( .+\n\\)* line \\([0-9]+\\) of file \\(.+\\)$" 3 2)
    ;; makefile:1: *** target pattern contains no `%'.  Stop.
    ("^\\(.+\\):\\([0-9]+\\): " 1 2))
  "Alist of regexps used to match script errors.
See `compilation-error-regexp-alist'.")

;; The C function openp slightly modified would do the trick fine
(defvaralias 'executable-binary-suffixes 'exec-suffixes)

;;;###autoload
(defun executable-command-find-posix-p (&optional program)
  "Check if PROGRAM handles arguments Posix-style.
If PROGRAM is non-nil, use that instead of \"find\"."
  ;;  Pick file to search from location we know
  (let* ((dir (file-truename data-directory))
         (file (car (directory-files dir nil "^[^.]"))))
    (with-temp-buffer
      (call-process (or program "find")
                    nil
                    (current-buffer)
                    nil
                    dir
                    "-name"
                    file
                    "-maxdepth"
                    "1")
        (goto-char (point-min))
        (if (search-forward file nil t)
            t))))

(defun executable-chmod ()
  "This gets called after saving a file to assure that it be executable.
You can set the absolute or relative mode in variable `executable-chmod' for
non-executable files."
  (and executable-chmod
       buffer-file-name
       (or (file-executable-p buffer-file-name)
	   (set-file-modes buffer-file-name
			   (if (< executable-chmod 0)
			       (- executable-chmod)
			     (logior executable-chmod
				     (file-modes buffer-file-name)))))))


(defvar compilation-error-regexp-alist) ; from compile.el

;;;###autoload
(defun executable-interpret (command)
  "Run script with user-specified args, and collect output in a buffer.
While script runs asynchronously, you can use the \\[next-error]
command to find the next error.  The buffer is also in `comint-mode' and
`compilation-shell-minor-mode', so that you can answer any prompts."
  (interactive (list (read-string "Run script: "
				  (or executable-command
				      buffer-file-name))))
  (require 'compile)
  (save-some-buffers (not compilation-ask-about-save))
  (set (make-local-variable 'executable-command) command)
  (let ((compilation-error-regexp-alist executable-error-regexp-alist))
    (compilation-start command t (lambda (_x) "*interpretation*"))))



;;;###autoload
(defun executable-set-magic (interpreter &optional argument
					 no-query-flag insert-flag)
  "Set this buffer's interpreter to INTERPRETER with optional ARGUMENT.
The variables `executable-magicless-file-regexp', `executable-prefix',
`executable-insert', `executable-query' and `executable-chmod' control
when and how magic numbers are inserted or replaced and scripts made
executable."
  (interactive
   (let* ((name (read-string "Name or file name of interpreter: "))
	  (arg (read-string (format "Argument for %s: " name))))
     (list name arg (eq executable-query 'function) t)))

  (setq interpreter (if (file-name-absolute-p interpreter)
			interpreter
		      (or (executable-find interpreter)
			  (error "Interpreter %s not recognized"
				 interpreter))))

  (setq argument (concat (if (string-match "\\`/:" interpreter)
			     (replace-match "" nil nil interpreter)
			   interpreter)
			 (and argument (string< "" argument) " ")
			 argument))

  (or buffer-read-only
      (if buffer-file-name
	  (string-match executable-magicless-file-regexp
			buffer-file-name))
      (not (or insert-flag executable-insert))
      (> (point-min) 1)
      (save-excursion
	(goto-char (point-min))
	(add-hook 'after-save-hook 'executable-chmod nil t)
	(if (looking-at "#![ \t]*\\(.*\\)$")
	    (and (goto-char (match-beginning 1))
		 ;; If the line ends in a space,
		 ;; don't offer to change it.
		 (not (= (char-after (1- (match-end 1))) ?\s))
		 (not (string= argument
			       (buffer-substring (point) (match-end 1))))
		 (if (or (not executable-query) no-query-flag
			 (save-window-excursion
			   ;; Make buffer visible before question.
			   (switch-to-buffer (current-buffer))
			   (y-or-n-p (concat "Replace magic number by `"
					     executable-prefix argument "'? "))))
		     (progn
		       (replace-match argument t t nil 1)
		       (message "Magic number changed to `%s'"
				(concat executable-prefix argument)))))
	  (insert executable-prefix argument ?\n)
	  (message "Magic number changed to `%s'"
		   (concat executable-prefix argument)))))
    interpreter)



;;;###autoload
(defun executable-self-display ()
  "Turn a text file into a self-displaying Un*x command.
The magic number of such a command displays all lines but itself."
  (interactive)
  (if (eq this-command 'executable-self-display)
      (setq this-command 'executable-set-magic))
  (executable-set-magic executable-self-display "+2"))

;;;###autoload
(defun executable-make-buffer-file-executable-if-script-p ()
  "Make file executable according to umask if not already executable.
If file already has any execute bits set at all, do not change existing
file modes."
  (and (>= (buffer-size) 2)
       (save-restriction
	 (widen)
	 (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
       (condition-case nil
           (let* ((current-mode (file-modes (buffer-file-name)))
                  (add-mode (logand ?\111 (default-file-modes))))
             (or (/= (logand ?\111 current-mode) 0)
                 (zerop add-mode)
                 (set-file-modes (buffer-file-name)
                                 (logior current-mode add-mode))))
         ;; Eg file-modes can return nil (bug#9879).  It should not,
         ;; in this context, but we should handle it all the same.
         (error (message "Unable to make file executable")))))

(provide 'executable)

;;; executable.el ends here
