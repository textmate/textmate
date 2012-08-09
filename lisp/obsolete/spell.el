;;; spell.el --- spelling correction interface for Emacs

;; Copyright (C) 1985, 2001-2012  Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp, unix
;; Obsolete-since: 23.1
;;   (not in obsolete/ directory then, but all functions marked obsolete)

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

;; This mode provides an Emacs interface to the UNIX spell(1) program.
;; Entry points are `spell-buffer', `spell-word', `spell-region' and
;; `spell-string'.

;; See also ispell.el for an interface to the ispell program.

;;; Code:

(defgroup spell nil
  "Interface to the UNIX spell(1) program."
  :prefix "spell-"
  :group 'applications)

(defcustom spell-command "spell"
  "Command to run the spell program."
  :type 'string
  :group 'spell)

(defcustom spell-filter nil
  "Filter function to process text before passing it to spell program.
This function might remove text-processor commands.
nil means don't alter the text before checking it."
  :type '(choice (const nil) function)
  :group 'spell)

;;;###autoload
(put 'spell-filter 'risky-local-variable t)

;;;###autoload
(defun spell-buffer ()
  "Check spelling of every word in the buffer.
For each incorrect word, you are asked for the correct spelling
and then put into a query-replace to fix some or all occurrences.
If you do not want to change a word, just give the same word
as its \"correct\" spelling; then the query replace is skipped."
  (interactive)
  ;; Don't warn about spell-region being obsolete.
  (with-no-warnings
    (spell-region (point-min) (point-max) "buffer")))
;;;###autoload
(make-obsolete 'spell-buffer 'ispell-buffer "23.1")

;;;###autoload
(defun spell-word ()
  "Check spelling of word at or before point.
If it is not correct, ask user for the correct spelling
and `query-replace' the entire buffer to substitute it."
  (interactive)
  (let (beg end spell-filter)
    (save-excursion
     (if (not (looking-at "\\<"))
	 (forward-word -1))
     (setq beg (point))
     (forward-word 1)
     (setq end (point)))
    ;; Don't warn about spell-region being obsolete.
    (with-no-warnings
      (spell-region beg end (buffer-substring beg end)))))
;;;###autoload
(make-obsolete 'spell-word 'ispell-word "23.1")

;;;###autoload
(defun spell-region (start end &optional description)
  "Like `spell-buffer' but applies only to region.
Used in a program, applies from START to END.
DESCRIPTION is an optional string naming the unit being checked:
for example, \"word\"."
  (interactive "r")
  (let ((filter spell-filter)
	(buf (get-buffer-create " *temp*")))
    (with-current-buffer buf
     (widen)
     (erase-buffer))
    (message "Checking spelling of %s..." (or description "region"))
    (if (and (null filter) (= ?\n (char-after (1- end))))
	(if (string= "spell" spell-command)
	    (call-process-region start end "spell" nil buf)
	  (call-process-region start end shell-file-name
			       nil buf nil "-c" spell-command))
      (let ((oldbuf (current-buffer)))
	(with-current-buffer buf
          (insert-buffer-substring oldbuf start end)
          (or (bolp) (insert ?\n))
          (if filter (funcall filter))
          (if (string= "spell" spell-command)
              (call-process-region (point-min) (point-max) "spell" t buf)
            (call-process-region (point-min) (point-max) shell-file-name
                                 t buf nil "-c" spell-command)))))
    (message "Checking spelling of %s...%s"
	     (or description "region")
	     (if (with-current-buffer buf
                   (> (buffer-size) 0))
		 "not correct"
	       "correct"))
    (let (word newword
	  (case-fold-search t)
	  (case-replace t))
      (while (with-current-buffer buf
               (> (buffer-size) 0))
	(with-current-buffer buf
          (goto-char (point-min))
          (setq word (downcase
                      (buffer-substring (point)
                                        (progn (end-of-line) (point)))))
          (forward-char 1)
          (delete-region (point-min) (point))
          (setq newword
                (read-string (concat "`" word
                                     "' not recognized; edit a replacement: ")
                             word))
          (flush-lines (concat "^" (regexp-quote word) "$")))
	(if (not (equal word newword))
	    (progn
	     (goto-char (point-min))
	     (query-replace-regexp (concat "\\b" (regexp-quote word) "\\b")
				   newword)))))))
;;;###autoload
(make-obsolete 'spell-region 'ispell-region "23.1")

;;;###autoload
(defun spell-string (string)
  "Check spelling of string supplied as argument."
  (interactive "sSpell string: ")
  (with-temp-buffer
    (widen)
    (erase-buffer)
    (insert string "\n")
    (if (string= "spell" spell-command)
        (call-process-region (point-min) (point-max) "spell"
                             t t)
      (call-process-region (point-min) (point-max) shell-file-name
                           t t nil "-c" spell-command))
    (if (= 0 (buffer-size))
        (message "%s is correct" string)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (replace-match " "))
      (message "%sincorrect" (buffer-substring 1 (point-max))))))
;;;###autoload
(make-obsolete 'spell-string "The `spell' package is obsolete - use `ispell'."
               "23.1")

(provide 'spell)

;;; spell.el ends here
