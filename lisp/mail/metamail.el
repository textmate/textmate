;;; metamail.el --- Metamail interface for GNU Emacs

;; Copyright (C) 1993, 1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Masanobu UMEDA <umerin@mse.kyutech.ac.jp>
;; Keywords: mail, news, mime, multimedia

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

;; Note: Metamail does not have all the options which are compatible with
;; the environment variables.  For that reason, metamail.el has to
;; hack the environment variables.  In addition, there is no way to
;; display all header fields without extra informative body messages
;; which are suppressed by the "-q" option.

;; The idea of using metamail to process MIME messages is from
;; gnus-mime.el by Spike <Spike@world.std.com>.

;;; Code:

(defvar rmail-current-message)
(defvar rmail-message-vector)

(defgroup metamail nil
  "Metamail interface for Emacs."
  :group 'mail
  :group 'processes)

(defcustom metamail-program-name "metamail"
  "*Metamail program name."
  :type 'string
  :group 'metamail)

(defcustom metamail-mailer-name "emacs"
  "*Mailer name set to MM_MAILER environment variable."
  :type 'string
  :group 'metamail)

(defvar metamail-environment '("KEYHEADS=*" "MM_QUIET=1")
  "*Environment variables passed to `metamail'.
It must be a list of strings that have the format ENVVARNAME=VALUE.
It is not expected to be altered globally by `set' or `setq'.
Instead, change its value temporary using `let' or `let*' form.")

(defcustom metamail-switches '("-x" "-d" "-z")
  "*Switches for `metamail' program.
`-z' is required to remove zap file.
It is not expected to be altered globally by `set' or `setq'.
Instead, change its value temporary using `let' or `let*' form.
`-m MAILER' argument is automatically generated from the
`metamail-mailer-name' variable."
  :type '(repeat (string :tag "Switch"))
  :group 'metamail)

;;;###autoload
(defun metamail-interpret-header ()
  "Interpret a header part of a MIME message in current buffer.
Its body part is not interpreted at all."
  (interactive)
  (save-excursion
    (let* ((buffer-read-only nil)
	   (metamail-switches		;Inhibit processing an empty body.
	    (append metamail-switches '("-c" "text/plain" "-E" "7bit")))
	   (end (progn
		  (goto-char (point-min))
		  (search-forward "\n\n" nil 'move)
		  ;; An extra newline is inserted by metamail if there
		  ;; is no body part.  So, insert a dummy body by
		  ;; itself.
		  (insert "\n")
		  (point))))
      (metamail-region (point-min) end nil nil 'nodisplay)
      ;; Remove an extra newline inserted by myself.
      (goto-char (point-min))
      (if (search-forward "\n\n\n" nil t)
	  (delete-char -1))
      )))

;;;###autoload
(defun metamail-interpret-body (&optional viewmode nodisplay)
  "Interpret a body part of a MIME message in current buffer.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted.
Its header part is not interpreted at all."
  (interactive "p")
  (save-excursion
    (let ((contype nil)
	  (encoding nil)
	  (end (progn
		 (goto-char (point-min))
		 (search-forward "\n\n" nil t)
		 (point))))
      ;; Find Content-Type and Content-Transfer-Encoding from the header.
      (save-restriction
	(narrow-to-region (point-min) end)
	(setq contype
	      (or (mail-fetch-field "Content-Type") "text/plain"))
	(setq encoding
	      (or (mail-fetch-field "Content-Transfer-Encoding") "7bit")))
      ;; Interpret the body part only.
      (let ((metamail-switches		;Process body part only.
	     (append metamail-switches
		     (list "-b" "-c" contype "-E" encoding))))
	(metamail-region end (point-max) viewmode nil nodisplay))
      ;; This mode specific hack is no longer appropriate in mbox Rmail.
      ;; Pre-mbox, we have just modified the actual folder, so we
      ;; update the message-vector with the new end position of the
      ;; current message.  In mbox Rmail, all we have done is modify a
      ;; display copy of the message.  Note also that point-max is a
      ;; marker in the wrong buffer: the message-vector contains
      ;; markers in rmail-view-buffer (which is not in rmail-mode).
      ;; So this hack actually breaks the message-vector.
      ;; If you're calling this on the actual rmail-view-buffer (or a
      ;; non-swapped rmail-buffer), you would still need this hack.
      ;; But you're not going to do that.
;;;      (cond ((eq major-mode 'rmail-mode)
;;;	     ;; Adjust the marker of this message if in Rmail mode buffer.
;;;	     (set-marker (aref rmail-message-vector (1+ rmail-current-message))
;;;			 (point-max))))
      )))

;;;###autoload
(defun metamail-buffer (&optional viewmode buffer nodisplay)
  "Process current buffer through `metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted."
  (interactive "p")
  (metamail-region (point-min) (point-max) viewmode buffer nodisplay))

;;;###autoload
(defun metamail-region (beg end &optional viewmode buffer nodisplay)
  "Process current region through 'metamail'.
Optional argument VIEWMODE specifies the value of the
EMACS_VIEW_MODE environment variable (defaulted to 1).
Optional argument BUFFER specifies a buffer to be filled (nil
means current).
Optional argument NODISPLAY non-nil means buffer is not
redisplayed as output is inserted."
  (interactive "r\np")
  (let ((curbuf (current-buffer))
	(buffer-read-only nil)
	(metafile (make-temp-file "metamail"))
	(option-environment
	 (list (format "EMACS_VIEW_MODE=%d"
		       (if (numberp viewmode) viewmode 1)))))
    (save-excursion
      ;; Gee!  Metamail does not output to stdout if input comes from
      ;; stdin.
      (let ((selective-display nil))	;Disable ^M to nl translation.
	(write-region beg end metafile nil 'nomessage))
      (if buffer
	  (set-buffer buffer))
      (setq buffer-read-only nil)
      ;; Clear destination buffer.
      (if (eq curbuf (current-buffer))
	  (delete-region beg end)
	(delete-region (point-min) (point-max)))
      ;; We have to pass the environment variable KEYHEADS to display
      ;; all header fields.  Metamail should have an optional argument
      ;; to pass such information directly.
      (let ((process-environment
	     (append process-environment
		     metamail-environment option-environment))
	    (coding-system-for-read 'undecided))
	(apply (function call-process)
	       metamail-program-name
	       nil
	       t			;Output to current buffer
	       (not nodisplay)		;Force redisplay
	       (append metamail-switches
		       (list "-m" (or metamail-mailer-name "emacs"))
		       (list metafile))))
      ;; `metamail' may not delete the temporary file!
      (condition-case error
	  (delete-file metafile)
	(error nil))
      )))

(provide 'metamail)

;;; metamail.el ends here
