;;; srecode/srt.el --- argument handlers for SRT files

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

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
;; Filters for SRT files, the Semantic Recoder template files.

;;; Code:

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'srecode/dictionary)
(require 'srecode/insert)

(defvar srecode-read-variable-name-history nil
  "History for `srecode-read-variable-name'.")

(defun srecode-read-variable-name (prompt &optional initial hist default)
  "Read in the name of a declared variable in the current SRT file.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit."
  (let* ((newdict (srecode-create-dictionary))
	 (currfcn (semantic-current-tag))
	 )
    (srecode-resolve-argument-list
     (mapcar 'read
	     (semantic-tag-get-attribute currfcn :arguments))
     newdict)

    (with-slots (namehash) newdict
      (completing-read prompt namehash nil nil initial
		       (or hist 'srecode-read-variable-name-history)
		       default))
    ))

(defvar srecode-read-major-mode-history nil
  "History for `srecode-read-variable-name'.")

(defun srecode-read-major-mode-name (prompt &optional initial hist default)
  "Read in the name of a desired `major-mode'.
PROMPT is the prompt to use.
INITIAL is the initial string.
HIST is the history value, otherwise `srecode-read-variable-name-history'
     is used.
DEFAULT is the default if RET is hit."
  (completing-read prompt obarray
		   (lambda (s) (string-match "-mode$" (symbol-name s)))
		   nil initial (or hist 'srecode-read-major-mode-history))
  )

(defun srecode-semantic-handle-:srt (dict)
  "Add macros into the dictionary DICT based on the current SRT file.
Adds the following:
ESCAPE_START - This files value of escape_start
ESCAPE_END - This files value of escape_end
MODE - The mode of this buffer.  If not declared yet, guess."
  (let* ((es (semantic-find-first-tag-by-name "escape_start" (current-buffer)))
	 (ee (semantic-find-first-tag-by-name "escape_end" (current-buffer)))
	 (mode-var (semantic-find-first-tag-by-name "mode" (current-buffer)))
	 (mode (if mode-var
		   (semantic-tag-variable-default mode-var)
		 nil))
	 )
    (srecode-dictionary-set-value dict "ESCAPE_START"
				  (if es
				      (car (semantic-tag-variable-default es))
				    "{{"))
    (srecode-dictionary-set-value dict "ESCAPE_END"
				  (if ee
				      (car (semantic-tag-variable-default ee))
				    "}}"))
    (when (not mode)
      (let* ((fname (file-name-nondirectory
		     (buffer-file-name (current-buffer))))
	     )
	(when (string-match "-\\(\\w+\\)\\.srt" fname)
	  (setq mode (concat (match-string 1 fname) "-mode")))))

    (when mode
      (srecode-dictionary-set-value dict "MAJORMODE" mode))

    ))

(provide 'srecode/srt)

;;; srecode/srt.el ends here
