;;; semantic/doc.el --- Routines for documentation strings

;; Copyright (C) 1999-2003, 2005, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

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
;; It is good practice to write documentation for your functions and
;; variables.  These core routines deal with these documentation
;; comments or strings.  They can exist either as a tag property
;; (:documentation) or as a comment just before the symbol, or after
;; the symbol on the same line.

(require 'semantic/tag)

;;; Code:

;;;###autoload
(define-overloadable-function semantic-documentation-for-tag (&optional tag nosnarf)
  "Find documentation from TAG and return it as a clean string.
TAG might have DOCUMENTATION set in it already.  If not, there may be
some documentation in a comment preceding TAG's definition which we
can look for.  When appropriate, this can be overridden by a language specific
enhancement.
Optional argument NOSNARF means to only return the lexical analyzer token for it.
If nosnarf if 'lex, then only return the lex token."
  (if (not tag) (setq tag (semantic-current-tag)))
  (save-excursion
    (when (semantic-tag-with-position-p tag)
      (set-buffer (semantic-tag-buffer tag)))
    (:override
     ;; No override.  Try something simple to find documentation nearby
     (save-excursion
       (semantic-go-to-tag tag)
       (let ((doctmp (semantic-tag-docstring tag (current-buffer))))
	 (or
	  ;; Is there doc in the tag???
	  doctmp
	  ;; Check just before the definition.
	  (when (semantic-tag-with-position-p tag)
	    (semantic-documentation-comment-preceeding-tag tag nosnarf))
	  ;;  Let's look for comments either after the definition, but before code:
	  ;; Not sure yet.  Fill in something clever later....
	  nil))))))

;; FIXME this is not how you spell "preceding".
(defun semantic-documentation-comment-preceeding-tag (&optional tag nosnarf)
  "Find a comment preceding TAG.
If TAG is nil.  use the tag under point.
Searches the space between TAG and the preceding tag for a comment,
and converts the comment into clean documentation.
Optional argument NOSNARF with a value of 'lex means to return
just the lexical token and not the string."
  (if (not tag) (setq tag (semantic-current-tag)))
  (save-excursion
    ;; Find this tag.
    (semantic-go-to-tag tag)
    (let* ((starttag (semantic-find-tag-by-overlay-prev
		      (semantic-tag-start tag)))
	   (start (if starttag
		      (semantic-tag-end starttag)
		    (point-min))))
      (when (and comment-start-skip
		 (re-search-backward comment-start-skip start t))
	;; We found a comment that doesn't belong to the body
	;; of a function.
	(semantic-doc-snarf-comment-for-tag nosnarf)))
    ))

(defun semantic-doc-snarf-comment-for-tag (nosnarf)
  "Snarf up the comment at POINT for `semantic-documentation-for-tag'.
Attempt to strip out comment syntactic sugar.
Argument NOSNARF means don't modify the found text.
If NOSNARF is 'lex, then return the lex token."
  (let* ((semantic-ignore-comments nil)
	 (semantic-lex-analyzer #'semantic-comment-lexer))
    (if (memq nosnarf '(lex flex)) ;; keep `flex' for compatibility
	(car (semantic-lex (point) (1+ (point))))
      (let ((ct (semantic-lex-token-text
		 (car (semantic-lex (point) (1+ (point)))))))
	(if nosnarf
	    nil
	  ;; ok, try to clean the text up.
	  ;; Comment start thingy
	  (while (string-match (concat "^\\s-*" comment-start-skip) ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; Arbitrary punctuation at the beginning of each line.
	  (while (string-match "^\\s-*\\s.+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0)))))
	  ;; End of a block comment.
	  (if (and (boundp 'block-comment-end)
		   block-comment-end
		   (string-match block-comment-end ct))
	      (setq ct (concat (substring ct 0 (match-beginning 0))
			       (substring ct (match-end 0)))))
	  ;; In case it's a real string, STRIPIT.
	  (while (string-match "\\s-*\\s\"+\\s-*" ct)
	    (setq ct (concat (substring ct 0 (match-beginning 0))
			     (substring ct (match-end 0))))))
	;; Now return the text.
	ct))))

(provide 'semantic/doc)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/doc"
;; End:

;;; semantic/doc.el ends here
