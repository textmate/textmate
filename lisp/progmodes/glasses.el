;;; glasses.el --- make cantReadThis readable

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Milan Zamazal <pdm@zamazal.org>
;; Maintainer: Milan Zamazal <pdm@zamazal.org>
;; Keywords: tools

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

;; This file defines a minor mode for making unreadableIdentifiersLikeThis
;; readable.  In some environments, for instance Java, it is common to use such
;; unreadable identifiers.  It is not good to use underscores in identifiers of
;; your own project in such an environment to make your sources more readable,
;; since it introduces undesirable confusion, which is worse than the
;; unreadability.  Fortunately, you use Emacs for the subproject, so the
;; problem can be solved some way.
;;
;; This file defines the `glasses-mode' minor mode, which displays underscores
;; between all the pairs of lower and upper English letters.  (This only
;; displays underscores, the text is not changed actually.)  Alternatively, you
;; can say you want the capitals in some given face (e.g. bold).
;;
;; The mode does something usable, though not perfect.  Improvement suggestions
;; from Emacs experts are welcome.
;;
;; If you like in-identifier separators different from underscores, change the
;; value of the variable `glasses-separator' appropriately.  See also the
;; variables `glasses-face' and `glasses-convert-on-write-p'.  You can also use
;; the command `M-x customize-group RET glasses RET'.
;;
;; If you set any of the variables `glasses-separator' or `glasses-face' after
;; glasses.el is loaded in a different way than through customize, you
;; should call the function `glasses-set-overlay-properties' afterwards.

;;; Code:


(eval-when-compile
  (require 'cl))


;;; User variables


(defgroup glasses nil
  "Make unreadable code likeThis(one) readable."
  :version "21.1"
  :group 'tools)


(defcustom glasses-separator "_"
  "String to be displayed as a visual separator in identifiers.
It is used both for adding missing separators and for replacing separators
defined by `glasses-original-separator'.  If you don't want to add missing
separators, set `glasses-separator' to an empty string.  If you don't want to
replace existent separators, set `glasses-original-separator' to an empty
string."
  :group 'glasses
  :type 'string
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-original-separator "_"
  "*String to be displayed as `glasses-separator' in separator positions.
For instance, if you set it to \"_\" and set `glasses-separator' to \"-\",
underscore separators are displayed as hyphens.
If `glasses-original-separator' is an empty string, no such display change is
performed."
  :group 'glasses
  :type 'string
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default
  :version "22.1")


(defcustom glasses-face nil
  "Face to be put on capitals of an identifier looked through glasses.
If it is nil, no face is placed at the capitalized letter.

For example, you can set `glasses-separator' to an empty string and
`glasses-face' to `bold'.  Then unreadable identifiers will have no separators,
but will have their capitals in bold."
  :group 'glasses
  :type '(choice (const :tag "None" nil) face)
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-separate-parentheses-p t
  "If non-nil, ensure space between an identifier and an opening parenthesis."
  :group 'glasses
  :type 'boolean)

(defcustom glasses-separate-parentheses-exceptions
  '("^#[\t ]*define[\t ]*[A-Za-z0-9_-]* ?($")
  "List of regexp that are exceptions for `glasses-separate-parentheses-p'.
They are matched to the current line truncated to the point where the
parenthesis expression starts."
  :group 'glasses
  :type '(repeat regexp))

(defcustom glasses-separate-capital-groups t
  "If non-nil, try to separate groups of capital letters.
When the value is non-nil, HTMLSomething and IPv6 are displayed
as HTML_Something and I_Pv6 respectively.  Set the value to nil
if you prefer to display them unchanged."
  :group 'glasses
  :type 'boolean
  :version "24.1")

(defcustom glasses-uncapitalize-p nil
  "If non-nil, downcase embedded capital letters in identifiers.
Only identifiers starting with lower case letters are affected, letters inside
other identifiers are unchanged."
  :group 'glasses
  :type 'boolean
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-uncapitalize-regexp "[a-z]"
  "Regexp matching beginnings of words to be uncapitalized.
Only words starting with this regexp are uncapitalized.
The regexp is case sensitive.
It has any effect only when `glasses-uncapitalize-p' is non-nil."
  :group 'glasses
  :type 'regexp
  :set 'glasses-custom-set
  :initialize 'custom-initialize-default)


(defcustom glasses-convert-on-write-p nil
  "If non-nil, remove separators when writing glasses buffer to a file.
If you are confused by glasses so much, that you write the separators into code
during coding, set this variable to t.  The separators will be removed on each
file write then.

Note the removal action does not try to be much clever, so it can remove real
separators too."
  :group 'glasses
  :type 'boolean)


(defun glasses-custom-set (symbol value)
  "Set value of the variable SYMBOL to VALUE and update overlay categories.
Used in :set parameter of some customized glasses variables."
  (set-default symbol value)
  (glasses-set-overlay-properties))


;;; Utility functions

(defun glasses-parenthesis-exception-p (beg end)
  "Tell if (BEG, END) is an exception to `glasses-separate-parentheses-p'.
See `glasses-separate-parentheses-exceptions'."
  (save-match-data
    (let ((str (buffer-substring beg end)))
      (catch 'match
	(dolist (re glasses-separate-parentheses-exceptions)
	  (and (string-match re str) (throw 'match t)))))))

(defun glasses-set-overlay-properties ()
  "Set properties of glasses overlays.
Consider current setting of user variables."
  ;; In-identifier overlay
  (put 'glasses 'evaporate t)
  (put 'glasses 'before-string glasses-separator)
  (put 'glasses 'face glasses-face)
  ;; Beg-identifier overlay
  (put 'glasses-init 'evaporate t)
  (put 'glasses-init 'face glasses-face)
  ;; Parenthesis overlay
  (put 'glasses-parenthesis 'evaporate t)
  (put 'glasses-parenthesis 'before-string " "))

(glasses-set-overlay-properties)


(defun glasses-overlay-p (overlay)
  "Return whether OVERLAY is an overlay of glasses mode."
  (memq (overlay-get overlay 'category)
	'(glasses glasses-init glasses-parenthesis)))


(defun glasses-make-overlay (beg end &optional category)
  "Create and return readability overlay over the region from BEG to END.
CATEGORY is the overlay category.  If it is nil, use the `glasses' category."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'category (or category 'glasses))
    overlay))


(defun glasses-make-readable (beg end)
  "Make identifiers in the region from BEG to END readable."
  (let ((case-fold-search nil))
    (save-excursion
      (save-match-data
	;; Face only
	(goto-char beg)
	(while (re-search-forward
		"\\<\\([A-Z]\\)[a-zA-Z]*\\([a-z][A-Z]\\|[A-Z][a-z]\\)"
		end t)
	  (glasses-make-overlay (match-beginning 1) (match-end 1)
				'glasses-init))
	;; Face + separator
	(goto-char beg)
	(while (re-search-forward
                (if glasses-separate-capital-groups
                    "[a-z]\\([A-Z]\\)\\|[A-Z]\\([A-Z]\\)[a-z]"
                  "[a-z]\\([A-Z]\\)")
                end t)
	  (let* ((n (if (match-string 1) 1 2))
		 (o (glasses-make-overlay (match-beginning n) (match-end n))))
	    (goto-char (match-beginning n))
	    (when (and glasses-uncapitalize-p
		       (save-match-data
			 (looking-at "[A-Z]\\($\\|[^A-Z]\\)"))
		       (save-excursion
			 (save-match-data
			   (re-search-backward "\\<.")
			   (looking-at glasses-uncapitalize-regexp))))
	      (overlay-put o 'invisible t)
	      (overlay-put o 'after-string (downcase (match-string n))))))
        ;; Separator change
	(when (and (not (string= glasses-original-separator glasses-separator))
		   (not (string= glasses-original-separator "")))
          (goto-char beg)
	  (let ((original-regexp (regexp-quote glasses-original-separator)))
	    (while (re-search-forward
		    (format "[a-zA-Z0-9]\\(\\(%s\\)+\\)[a-zA-Z0-9]"
			    original-regexp)
		    end t)
	      (goto-char (match-beginning 1))
	      (while (looking-at original-regexp)
		(let ((o (glasses-make-overlay (point) (1+ (point)))))
		  ;; `concat' ensures the character properties won't merge
		  (overlay-put o 'display (concat glasses-separator)))
		(goto-char (match-end 0))))))
	;; Parentheses
	(when glasses-separate-parentheses-p
	  (goto-char beg)
	  (while (re-search-forward "[a-zA-Z]_*\\(\(\\)" end t)
	    (unless (glasses-parenthesis-exception-p (point-at-bol) (match-end 1))
	      (glasses-make-overlay (match-beginning 1) (match-end 1)
				    'glasses-parenthesis))))))))


(defun glasses-make-unreadable (beg end)
  "Return identifiers in the region from BEG to END to their unreadable state."
  (dolist (o (overlays-in beg end))
    (when (glasses-overlay-p o)
      (delete-overlay o))))


(defun glasses-convert-to-unreadable ()
  "Convert current buffer to unreadable identifiers and return nil.
This function modifies buffer contents, it removes all the separators,
recognized according to the current value of the variable `glasses-separator'."
  (when glasses-convert-on-write-p
    (let ((case-fold-search nil)
	  (separator (regexp-quote glasses-separator)))
      (save-excursion
	(unless (string= glasses-separator "")
	  (goto-char (point-min))
	  (while (re-search-forward
		  (format "[a-z]\\(%s\\)[A-Z]\\|[A-Z]\\(%s\\)[A-Z][a-z]"
			  separator separator)
		  nil t)
	    (let ((n (if (match-string 1) 1 2)))
	      (replace-match "" t nil nil n)
	      (goto-char (match-end n))))
	  (unless (string= glasses-separator glasses-original-separator)
	    (goto-char (point-min))
	    (while (re-search-forward (format "[a-zA-Z0-9]\\(%s+\\)[a-zA-Z0-9]"
					      separator)
				      nil t)
	      (replace-match glasses-original-separator nil nil nil 1)
	      (goto-char (match-beginning 1)))))
	(when glasses-separate-parentheses-p
	  (goto-char (point-min))
	  (while (re-search-forward "[a-zA-Z]_*\\( \\)\(" nil t)
	    (unless (glasses-parenthesis-exception-p (point-at-bol) (1+ (match-end 1)))
	      (replace-match "" t nil nil 1)))))))
  ;; nil must be returned to allow use in write file hooks
  nil)


(defun glasses-change (beg end &optional _old-len)
  "After-change function updating glass overlays."
  (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
	(end-line (save-excursion (goto-char end) (line-end-position))))
    (glasses-make-unreadable beg-line end-line)
    (glasses-make-readable beg-line end-line)))


;;; Minor mode definition


;;;###autoload
(define-minor-mode glasses-mode
  "Minor mode for making identifiers likeThis readable.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.  When this mode is active, it tries to
add virtual separators (like underscores) at places they belong to."
  :group 'glasses :lighter " o^o"
  (save-excursion
    (save-restriction
      (widen)
      ;; We erase all the overlays anyway, to avoid dual sight in some
      ;; circumstances
      (glasses-make-unreadable (point-min) (point-max))
      (if glasses-mode
	  (progn
	    (jit-lock-register 'glasses-change)
	    (add-hook 'local-write-file-hooks
		      'glasses-convert-to-unreadable nil t))
	(jit-lock-unregister 'glasses-change)
	(remove-hook 'local-write-file-hooks
		     'glasses-convert-to-unreadable t)))))


;;; Announce

(provide 'glasses)


;;; glasses.el ends here
