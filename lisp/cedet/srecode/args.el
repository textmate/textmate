;;; srecode/args.el --- Provide some simple template arguments

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;;
;; Srecode templates can accept arguments.  These arguments represent
;; sets of dictionary words that need to be derived.  This file contains
;; a set of simple arguments for srecode templates.

(require 'srecode/dictionary)

;;; Code:

;;; :blank
;;
;; Using :blank means that the template should force blank lines
;; before and after the template, regardless of where the insertion
;; is occurring.
(defun srecode-semantic-handle-:blank (dict)
  "Add macros into the dictionary DICT specifying blank line spacing.
The wrapgap means make sure the first and last lines of the macro
do not contain any text from preceding or following text."
  ;; This won't actually get used, but it might be nice
  ;; to know about it.
  (srecode-dictionary-set-value dict "BLANK" t)
  )

;;; :indent ARGUMENT HANDLING
;;
;; When a :indent argument is required, the default is to indent
;; for the current major mode.
(defun srecode-semantic-handle-:indent (dict)
  "Add macros into the dictionary DICT for indentation."
  (srecode-dictionary-set-value dict "INDENT" t)
  )

;;; :region ARGUMENT HANDLING
;;
;; When a :region argument is required, provide macros that
;; deal with that active region.
;;
;; Regions allow a macro to wrap the region text within the
;; template bounds.
;;
(defvar srecode-handle-region-when-non-active-flag nil
  "Non-nil means do region handling w/out the region being active.")

(defun srecode-semantic-handle-:region (dict)
  "Add macros into the dictionary DICT based on the current :region."
  ;; Only enable the region section if we can clearly show that
  ;; the user is intending to do something with the region.
  (when (or srecode-handle-region-when-non-active-flag
	    (eq last-command 'mouse-drag-region)
	    (and transient-mark-mode mark-active))
    ;; Show the region section
    (srecode-dictionary-show-section dict "REGION")
    (srecode-dictionary-set-value
     dict "REGIONTEXT" (buffer-substring-no-properties (point) (mark)))
    ;; Only whack the region if our template output
    ;; is also destined for the current buffer.
    (when (eq standard-output (current-buffer))
      (kill-region (point) (mark))))
  )

;;; :user ARGUMENT HANDLING
;;
;; When a :user argument is required, fill the dictionary with
;; information about the current Emacs user.
(defun srecode-semantic-handle-:user (dict)
  "Add macros into the dictionary DICT based on the current :user."
  (srecode-dictionary-set-value dict "AUTHOR" (user-full-name))
  (srecode-dictionary-set-value dict "LOGIN" (user-login-name))
  (srecode-dictionary-set-value dict "EMAIL" user-mail-address)
  (srecode-dictionary-set-value dict "EMACSINITFILE" user-init-file)
  (srecode-dictionary-set-value dict "UID" (user-uid))
  )

;;; :time ARGUMENT HANDLING
;;
;; When a :time argument is required, fill the dictionary with
;; information about the current Emacs time.
(defun srecode-semantic-handle-:time (dict)
  "Add macros into the dictionary DICT based on the current :time."
  ;; DATE Values
  (srecode-dictionary-set-value
   dict "YEAR" (format-time-string "%Y" (current-time)))
  (srecode-dictionary-set-value
   dict "MONTHNAME" (format-time-string "%B" (current-time)))
  (srecode-dictionary-set-value
   dict "MONTH" (format-time-string "%m" (current-time)))
  (srecode-dictionary-set-value
   dict "DAY" (format-time-string "%d" (current-time)))
  (srecode-dictionary-set-value
   dict "WEEKDAY" (format-time-string "%a" (current-time)))
  ;; Time Values
  (srecode-dictionary-set-value
   dict "HOUR" (format-time-string "%H" (current-time)))
  (srecode-dictionary-set-value
   dict "HOUR12" (format-time-string "%l" (current-time)))
  (srecode-dictionary-set-value
   dict "AMPM" (format-time-string "%p" (current-time)))
  (srecode-dictionary-set-value
   dict "MINUTE" (format-time-string "%M" (current-time)))
  (srecode-dictionary-set-value
   dict "SECOND" (format-time-string "%S" (current-time)))
  (srecode-dictionary-set-value
   dict "TIMEZONE" (format-time-string "%Z" (current-time)))
  ;; Convenience pre-packed date/time
  (srecode-dictionary-set-value
   dict "DATE" (format-time-string "%D" (current-time)))
  (srecode-dictionary-set-value
   dict "TIME" (format-time-string "%X" (current-time)))
  )

;;; :file ARGUMENT HANDLING
;;
;; When a :file argument is required, fill the dictionary with
;; information about the file Emacs is editing at the time of
;; insertion.
(defun srecode-semantic-handle-:file (dict)
  "Add macros into the dictionary DICT based on the current :file."
  (let* ((bfn (buffer-file-name))
	 (file (file-name-nondirectory bfn))
	 (dir (file-name-directory bfn)))
    (srecode-dictionary-set-value dict "FILENAME" file)
    (srecode-dictionary-set-value dict "FILE" (file-name-sans-extension file))
    (srecode-dictionary-set-value dict "EXTENSION" (file-name-extension file))
    (srecode-dictionary-set-value dict "DIRECTORY" dir)
    (srecode-dictionary-set-value dict "MODE" (symbol-name major-mode))
    (srecode-dictionary-set-value
     dict "SHORTMODE"
     (let* ((mode-name  (symbol-name major-mode))
	    (match (string-match "-mode" mode-name)))
       (if match
	   (substring mode-name 0 match)
	 mode-name)))
    (if (or (file-exists-p "CVS")
	    (file-exists-p "RCS"))
	(srecode-dictionary-show-section dict "RCS")
      )))

;;; :system ARGUMENT HANDLING
;;
;; When a :system argument is required, fill the dictionary with
;; information about the computer Emacs is running on.
(defun srecode-semantic-handle-:system (dict)
  "Add macros into the dictionary DICT based on the current :system."
    (srecode-dictionary-set-value dict "SYSTEMCONF" system-configuration)
    (srecode-dictionary-set-value dict "SYSTEMTYPE" system-type)
    (srecode-dictionary-set-value dict "SYSTEMNAME" (system-name))
    (srecode-dictionary-set-value dict "MAILHOST" (or mail-host-address
						      (system-name)))
  )

;;; :kill ARGUMENT HANDLING
;;
;; When a :kill argument is required, fill the dictionary with
;; information about the current kill ring.
(defun srecode-semantic-handle-:kill (dict)
  "Add macros into the dictionary DICT based on the kill ring."
  (srecode-dictionary-set-value dict "KILL" (car kill-ring))
  (srecode-dictionary-set-value dict "KILL2" (nth 1 kill-ring))
  (srecode-dictionary-set-value dict "KILL3" (nth 2 kill-ring))
  (srecode-dictionary-set-value dict "KILL4" (nth 3 kill-ring))
  )

(provide 'srecode/args)

;;; srecode/args.el ends here
