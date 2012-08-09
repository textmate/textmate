;;; novice.el --- handling of disabled commands ("novice mode") for Emacs

;; Copyright (C) 1985-1987, 1994, 2001-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal, help

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

;; This mode provides a hook which is, by default, attached to various
;; putatively dangerous commands in a (probably futile) attempt to
;; prevent lusers from shooting themselves in the feet.

;;; Code:

;; This function is called (by autoloading)
;; to handle any disabled command.
;; The command is found in this-command
;; and the keys are returned by (this-command-keys).

(eval-when-compile (require 'cl))

;;;###autoload
(defvar disabled-command-function 'disabled-command-function
  "Function to call to handle disabled commands.
If nil, the feature is disabled, i.e., all commands work normally.")

;;;###autoload
(define-obsolete-variable-alias 'disabled-command-hook 'disabled-command-function "22.1")

;; It is ok here to assume that this-command is a symbol
;; because we won't get called otherwise.
;;;###autoload
(defun disabled-command-function (&optional cmd keys)
  (unless cmd (setq cmd this-command))
  (unless keys (setq keys (this-command-keys)))
  (let (char)
    (save-window-excursion
      (help-setup-xref (list 'disabled-command-function cmd keys) nil)
      (with-output-to-temp-buffer "*Disabled Command*" ;; (help-buffer)
	 (if (or (eq (aref keys 0)
		     (if (stringp keys)
			 (aref "\M-x" 0)
		       ?\M-x))
		 (and (>= (length keys) 2)
		      (eq (aref keys 0) meta-prefix-char)
		      (eq (aref keys 1) ?x)))
	    (princ (format "You have invoked the disabled command %s.\n" cmd))
	   (princ (format "You have typed %s, invoking disabled command %s.\n"
			 (key-description keys) cmd)))
       ;; Print any special message saying why the command is disabled.
	(if (stringp (get cmd 'disabled))
	    (princ (get cmd 'disabled))
	 (princ "It is disabled because new users often find it confusing.\n")
	 (princ "Here's the first part of its description:\n\n")
	 ;; Keep only the first paragraph of the documentation.
          (with-current-buffer "*Disabled Command*" ;; standard-output
	   (goto-char (point-max))
	   (let ((start (point)))
	     (save-excursion
	       (princ (or (condition-case ()
			       (documentation cmd)
			    (error nil))
			  "<< not documented >>")))
	     (if (search-forward "\n\n" nil t)
		 (delete-region (match-beginning 0) (point-max)))
	     (goto-char (point-max))
	     (indent-rigidly start (point) 3))))
       (princ "\n\nDo you want to use this command anyway?\n\n")
       (princ "You can now type
y   to try it and enable it (no questions if you use it again).
n   to cancel--don't try the command, and it remains disabled.
SPC to try the command just this once, but leave it disabled.
!   to try it, and enable all disabled commands for this session only.")
        ;; Redundant since with-output-to-temp-buffer will do it anyway.
        ;; (with-current-buffer standard-output
        ;;   (help-mode))
        )
     (fit-window-to-buffer (get-buffer-window "*Disabled Command*"))
     (message "Type y, n, ! or SPC (the space bar): ")
     (let ((cursor-in-echo-area t))
       (while (progn (setq char (read-event))
		     (or (not (numberp char))
			 (not (memq (downcase char)
				    '(?! ?y ?n ?\s ?\C-g)))))
	 (ding)
	 (message "Please type y, n, ! or SPC (the space bar): "))))
    (setq char (downcase char))
    (case char
     (?\C-g (setq quit-flag t))
     (?! (setq disabled-command-function nil))
     (?y
	(if (and user-init-file
		 (not (string= "" user-init-file))
		 (y-or-n-p "Enable command for future editing sessions also? "))
	  (enable-command cmd)
	(put cmd 'disabled nil))))
    (or (char-equal char ?n)
        (call-interactively cmd))))

(defun en/disable-command (command disable)
  (unless (commandp command)
    (error "Invalid command name `%s'" command))
  (put command 'disabled disable)
  (let ((init-file user-init-file)
	(default-init-file
	  (if (eq system-type 'ms-dos) "~/_emacs" "~/.emacs")))
    (unless init-file
      (if (or (file-exists-p default-init-file)
	      (and (eq system-type 'windows-nt)
		   (file-exists-p "~/_emacs")))
	  ;; Started with -q, i.e. the file containing
	  ;; enabled/disabled commands hasn't been read.  Saving
	  ;; settings there would overwrite other settings.
	  (error "Saving settings from \"emacs -q\" would overwrite existing customizations"))
      (setq init-file default-init-file)
      (if (and (not (file-exists-p init-file))
	       (eq system-type 'windows-nt)
	       (file-exists-p "~/_emacs"))
	  (setq init-file "~/_emacs")))
    (with-current-buffer (find-file-noselect
                          (substitute-in-file-name init-file))
      (goto-char (point-min))
      (if (search-forward (concat "(put '" (symbol-name command) " ") nil t)
	  (delete-region
	   (progn (beginning-of-line) (point))
	   (progn (forward-line 1) (point))))
      ;; Explicitly enable, in case this command is disabled by default
      ;; or in case the code we deleted was actually a comment.
      (goto-char (point-max))
      (unless (bolp) (newline))
      (insert "(put '" (symbol-name command) " 'disabled "
	      (symbol-name disable) ")\n")
      (save-buffer))))

;;;###autoload
(defun enable-command (command)
  "Allow COMMAND to be executed without special confirmation from now on.
COMMAND must be a symbol.
This command alters the user's .emacs file so that this will apply
to future sessions."
  (interactive "CEnable command: ")
  (en/disable-command command nil))

;;;###autoload
(defun disable-command (command)
  "Require special confirmation to execute COMMAND from now on.
COMMAND must be a symbol.
This command alters the user's .emacs file so that this will apply
to future sessions."
  (interactive "CDisable command: ")
  (en/disable-command command t))

(provide 'novice)

;;; novice.el ends here
