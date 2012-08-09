;;; gnus-cite.el --- parse citations in articles for Gnus

;; Copyright (C) 1995-2012 Free Software Foundation, Inc.

;; Author: Per Abhiddenware

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

;;; Code:

(eval-when-compile (require 'cl))
(eval-when-compile
  (when (featurep 'xemacs)
    (require 'easy-mmode))) ; for `define-minor-mode'

(require 'gnus)
(require 'gnus-range)
(require 'gnus-art)
(require 'message)	; for message-cite-prefix-regexp

;;; Customization:

(defgroup gnus-cite nil
  "Citation."
  :prefix "gnus-cite-"
  :link '(custom-manual "(gnus)Article Highlighting")
  :group 'gnus-article)

(defcustom gnus-cited-opened-text-button-line-format "%(%{[-]%}%)\n"
  "Format of opened cited text buttons."
  :group 'gnus-cite
  :type 'string)

(defcustom gnus-cited-closed-text-button-line-format "%(%{[+]%}%)\n"
  "Format of closed cited text buttons."
  :group 'gnus-cite
  :type 'string)

(defcustom gnus-cited-lines-visible nil
  "The number of lines of hidden cited text to remain visible.
Or a pair (cons) of numbers which are the number of lines at the top
and bottom of the text, respectively, to remain visible."
  :group 'gnus-cite
  :type '(choice (const :tag "none" nil)
		 integer
		 (cons :tag "Top and Bottom" integer integer)))

(defcustom gnus-cite-parse-max-size 25000
  "Maximum article size (in bytes) where parsing citations is allowed.
Set it to nil to parse all articles."
  :group 'gnus-cite
  :type '(choice (const :tag "all" nil)
		 integer))

(defcustom gnus-cite-max-prefix 20
  "Maximum possible length for a citation prefix."
  :group 'gnus-cite
  :type 'integer)

(defcustom gnus-supercite-regexp
  (concat "^\\(" message-cite-prefix-regexp "\\)? *"
	  ">>>>> +\"\\([^\"\n]+\\)\" +==")
  "*Regexp matching normal Supercite attribution lines.
The first grouping must match prefixes added by other packages."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-supercite-secondary-regexp "^.*\"\\([^\"\n]+\\)\" +=="
  "Regexp matching mangled Supercite attribution lines.
The first regexp group should match the Supercite attribution."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-cite-minimum-match-count 2
  "Minimum number of identical prefixes before we believe it's a citation."
  :group 'gnus-cite
  :type 'integer)

;; Some Microsoft products put in a citation that extends to the
;; remainder of the message:
;;
;;     -----Original Message-----
;;     From: ...
;;     To: ...
;;     Sent: ...   [date, in non-RFC-2822 format]
;;     Subject: ...
;;
;;     Cited message, with no prefixes
;;
;; The four headers are always the same.  But note they are prone to
;; folding without additional indentation.
;;
;; Others use "----- Original Message -----" instead, and properly quote
;; the body using "> ".  This style is handled without special cases.

(defcustom gnus-cite-attribution-prefix
  "In article\\|in <\\|On \\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\),\\|----- ?Original Message ?-----"
  "*Regexp matching the beginning of an attribution line."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-cite-attribution-suffix
  "\\(\\(wrote\\|writes\\|said\\|says\\|>\\)\\(:\\|\\.\\.\\.\\)\\|----- ?Original Message ?-----\\)[ \t]*$"
  "*Regexp matching the end of an attribution line.
The text matching the first grouping will be used as a button."
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-cite-unsightly-citation-regexp
  "^-----Original Message-----\nFrom: \\(.+\n\\)+\n"
  "Regexp matching Microsoft-type rest-of-message citations."
  :version "22.1"
  :group 'gnus-cite
  :type 'regexp)

(defcustom gnus-cite-ignore-quoted-from t
  "Non-nil means don't regard lines beginning with \">From \" as cited text.
Those lines may have been quoted by MTAs in order not to mix up with
the envelope From line."
  :version "22.1"
  :group 'gnus-cite
  :type 'boolean)

(defface gnus-cite-attribution '((t (:italic t)))
  "Face used for attribution lines."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-attribution-face 'face-alias 'gnus-cite-attribution)
(put 'gnus-cite-attribution-face 'obsolete-face "22.1")

(defcustom gnus-cite-attribution-face 'gnus-cite-attribution
  "Face used for attribution lines.
It is merged with the face for the cited text belonging to the attribution."
  :version "22.1"
  :group 'gnus-cite
  :type 'face)

(defface gnus-cite-1 '((((class color)
			 (background dark))
			(:foreground "light blue"))
		       (((class color)
			 (background light))
			(:foreground "MidnightBlue"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-1 'face-alias 'gnus-cite-1)
(put 'gnus-cite-face-1 'obsolete-face "22.1")

(defface gnus-cite-2 '((((class color)
			 (background dark))
			(:foreground "light cyan"))
		       (((class color)
			 (background light))
			(:foreground "firebrick"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-2 'face-alias 'gnus-cite-2)
(put 'gnus-cite-face-2 'obsolete-face "22.1")

(defface gnus-cite-3 '((((class color)
			 (background dark))
			(:foreground "light yellow"))
		       (((class color)
			 (background light))
			(:foreground "dark green"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-3 'face-alias 'gnus-cite-3)
(put 'gnus-cite-face-3 'obsolete-face "22.1")

(defface gnus-cite-4 '((((class color)
			 (background dark))
			(:foreground "light pink"))
		       (((class color)
			 (background light))
			(:foreground "OrangeRed"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-4 'face-alias 'gnus-cite-4)
(put 'gnus-cite-face-4 'obsolete-face "22.1")

(defface gnus-cite-5 '((((class color)
			 (background dark))
			(:foreground "pale green"))
		       (((class color)
			 (background light))
			(:foreground "dark khaki"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-5 'face-alias 'gnus-cite-5)
(put 'gnus-cite-face-5 'obsolete-face "22.1")

(defface gnus-cite-6 '((((class color)
			 (background dark))
			(:foreground "beige"))
		       (((class color)
			 (background light))
			(:foreground "dark violet"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-6 'face-alias 'gnus-cite-6)
(put 'gnus-cite-face-6 'obsolete-face "22.1")

(defface gnus-cite-7 '((((class color)
			 (background dark))
			(:foreground "orange"))
		       (((class color)
			 (background light))
			(:foreground "SteelBlue4"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-7 'face-alias 'gnus-cite-7)
(put 'gnus-cite-face-7 'obsolete-face "22.1")

(defface gnus-cite-8 '((((class color)
			 (background dark))
			(:foreground "magenta"))
		       (((class color)
			 (background light))
			(:foreground "magenta"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-8 'face-alias 'gnus-cite-8)
(put 'gnus-cite-face-8 'obsolete-face "22.1")

(defface gnus-cite-9 '((((class color)
			 (background dark))
			(:foreground "violet"))
		       (((class color)
			 (background light))
			(:foreground "violet"))
		       (t
			(:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-9 'face-alias 'gnus-cite-9)
(put 'gnus-cite-face-9 'obsolete-face "22.1")

(defface gnus-cite-10 '((((class color)
			  (background dark))
			 (:foreground "plum1"))
			(((class color)
			  (background light))
			 (:foreground "medium purple"))
			(t
			 (:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-10 'face-alias 'gnus-cite-10)
(put 'gnus-cite-face-10 'obsolete-face "22.1")

(defface gnus-cite-11 '((((class color)
			  (background dark))
			 (:foreground "turquoise"))
			(((class color)
			  (background light))
			 (:foreground "turquoise"))
			(t
			 (:italic t)))
  "Citation face."
  :group 'gnus-cite)
;; backward-compatibility alias
(put 'gnus-cite-face-11 'face-alias 'gnus-cite-11)
(put 'gnus-cite-face-11 'obsolete-face "22.1")

(defcustom gnus-cite-face-list
  '(gnus-cite-1 gnus-cite-2 gnus-cite-3 gnus-cite-4 gnus-cite-5 gnus-cite-6
		gnus-cite-7 gnus-cite-8 gnus-cite-9 gnus-cite-10 gnus-cite-11)
  "*List of faces used for highlighting citations.

When there are citations from multiple articles in the same message,
Gnus will try to give each citation from each article its own face.
This should make it easier to see who wrote what."
  :group 'gnus-cite
  :type '(repeat face)
  :set (lambda (symbol value)
	 (prog1
	     (custom-set-default symbol value)
	   (if (boundp 'gnus-message-max-citation-depth)
	       (setq gnus-message-max-citation-depth (length value)))
	   (if (boundp 'gnus-message-citation-keywords)
	       (setq gnus-message-citation-keywords
		     `((gnus-message-search-citation-line
			,@(let ((list nil)
				(count 1))
			    (dolist (face value (nreverse list))
			      (push (list count (list 'quote face) 'prepend t)
				    list)
			      (setq count (1+ count)))))))))))

(defcustom gnus-cite-hide-percentage 50
  "Only hide excess citation if above this percentage of the body."
  :group 'gnus-cite
  :type 'number)

(defcustom gnus-cite-hide-absolute 10
  "Only hide excess citation if above this number of lines in the body."
  :group 'gnus-cite
  :type 'integer)

(defcustom gnus-cite-blank-line-after-header t
  "If non-nil, put a blank line between the citation header and the button."
  :group 'gnus-cite
  :type 'boolean)

;; This has to go here because its default value depends on
;; gnus-cite-face-list.
(defcustom gnus-article-boring-faces (cons 'gnus-signature gnus-cite-face-list)
  "List of faces that are not worth reading.
If an article has more pages below the one you are looking at, but
nothing on those pages is a word of at least three letters that is not
in a boring face, then the pages will be skipped."
  :type '(repeat face)
  :group 'gnus-article-hiding)

;;; Internal Variables:

(defvar gnus-cite-article nil)
(defvar gnus-cite-overlay-list nil)

(defvar gnus-cite-prefix-alist nil)
;; Alist of citation prefixes.
;; The cdr is a list of lines with that prefix.

(defvar gnus-cite-attribution-alist nil)
;; Alist of attribution lines.
;; The car is a line number.
;; The cdr is the prefix for the citation started by that line.

(defvar gnus-cite-loose-prefix-alist nil)
;; Alist of citation prefixes that have no matching attribution.
;; The cdr is a list of lines with that prefix.

(defvar gnus-cite-loose-attribution-alist nil)
;; Alist of attribution lines that have no matching citation.
;; Each member has the form (WROTE IN PREFIX TAG), where
;; WROTE: is the attribution line number
;; IN: is the line number of the previous line if part of the same attribution,
;; PREFIX: Is the citation prefix of the attribution line(s), and
;; TAG: Is a Supercite tag, if any.

(defvar gnus-cited-opened-text-button-line-format-alist
  `((?b (marker-position beg) ?d)
    (?e (marker-position end) ?d)
    (?n (count-lines beg end) ?d)
    (?l (- end beg) ?d)))
(defvar gnus-cited-opened-text-button-line-format-spec nil)
(defvar gnus-cited-closed-text-button-line-format-alist
  gnus-cited-opened-text-button-line-format-alist)
(defvar gnus-cited-closed-text-button-line-format-spec nil)


;;; Commands:

(defun gnus-article-highlight-citation (&optional force same-buffer)
  "Highlight cited text.
Each citation in the article will be highlighted with a different face.
The faces are taken from `gnus-cite-face-list'.
Attribution lines are highlighted with the same face as the
corresponding citation merged with the face `gnus-cite-attribution'.

Text is considered cited if at least `gnus-cite-minimum-match-count'
lines matches `message-cite-prefix-regexp' with the same prefix.

Lines matching `gnus-cite-attribution-suffix' and perhaps
`gnus-cite-attribution-prefix' are considered attribution lines."
  (interactive (list 'force))
  (with-current-buffer (if same-buffer (current-buffer) gnus-article-buffer)
    (gnus-cite-parse-maybe force)
    (let ((buffer-read-only nil)
	  (alist gnus-cite-prefix-alist)
	  (faces gnus-cite-face-list)
	  (inhibit-point-motion-hooks t)
	  face entry prefix skip numbers number face-alist)
      ;; Loop through citation prefixes.
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      prefix (car entry)
	      numbers (cdr entry)
	      face (car faces)
	      faces (or (cdr faces) gnus-cite-face-list)
	      face-alist (cons (cons prefix face) face-alist))
	(while numbers
	  (setq number (car numbers)
		numbers (cdr numbers))
	  (and (not (assq number gnus-cite-attribution-alist))
	       (not (assq number gnus-cite-loose-attribution-alist))
	       (gnus-cite-add-face number prefix face))))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      prefix (cdr entry)
	      skip (gnus-cite-find-prefix number)
	      face (cdr (assoc prefix face-alist)))
	;; Add attribution button.
	(goto-char (point-min))
	(forward-line (1- number))
	(when (re-search-forward gnus-cite-attribution-suffix
				 (point-at-eol)
				 t)
	  (gnus-article-add-button (match-beginning 1) (match-end 1)
				   'gnus-cite-toggle prefix))
	;; Highlight attribution line.
	(gnus-cite-add-face number skip face)
	(gnus-cite-add-face number skip gnus-cite-attribution-face))
      ;; Loop through attribution lines.
      (setq alist gnus-cite-loose-attribution-alist)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist)
	      number (car entry)
	      skip (gnus-cite-find-prefix number))
	(gnus-cite-add-face number skip gnus-cite-attribution-face)))))

(defun gnus-dissect-cited-text ()
  "Dissect the article buffer looking for cited text."
  (with-current-buffer gnus-article-buffer
    (gnus-cite-parse-maybe nil t)
    (let ((alist gnus-cite-prefix-alist)
	  prefix numbers number marks m)
      ;; Loop through citation prefixes.
      (while alist
	(setq numbers (pop alist)
	      prefix (pop numbers))
	(while numbers
	  (setq number (pop numbers))
	  (goto-char (point-min))
	  (forward-line number)
	  (push (cons (point-marker) "") marks)
	  (while (and numbers
		      (= (1- number) (car numbers)))
	    (setq number (pop numbers)))
	  (goto-char (point-min))
	  (forward-line (1- number))
	  (push (cons (point-marker) prefix) marks)))
      ;; Skip to the beginning of the body.
      (article-goto-body)
      (push (cons (point-marker) "") marks)
      ;; Find the end of the body.
      (goto-char (point-max))
      (gnus-article-search-signature)
      (push (cons (point-marker) "") marks)
      ;; Sort the marks.
      (setq marks (sort marks 'car-less-than-car))
      (let ((omarks marks))
	(setq marks nil)
	(while (cdr omarks)
	  (if (= (caar omarks) (caadr omarks))
	      (progn
		(unless (equal (cdar omarks) "")
		  (push (car omarks) marks))
		(unless (equal (cdadr omarks) "")
		  (push (cadr omarks) marks))
		(unless (and (equal (cdar omarks) "")
			     (equal (cdadr omarks) "")
			     (not (cddr omarks)))
		  (setq omarks (cdr omarks))))
	    (push (car omarks) marks))
	  (setq omarks (cdr omarks)))
	(when (car omarks)
	  (push (car omarks) marks))
	(setq marks (setq m (nreverse marks)))
	(while (cddr m)
	  (if (and (equal (cdadr m) "")
		   (equal (cdar m) (cdaddr m))
		   (goto-char (caadr m))
		   (forward-line 1)
		   (= (point) (caaddr m)))
	      (setcdr m (cdddr m))
	    (setq m (cdr m))))
	marks))))

(defun gnus-article-fill-cited-long-lines ()
  (gnus-article-fill-cited-article nil t))

(defun gnus-article-fill-cited-article (&optional width long-lines)
  "Do word wrapping in the current article.
If WIDTH (the numerical prefix), use that text width when
filling.  If LONG-LINES, only fill sections that have lines
longer than the frame width."
  (interactive "P")
  (with-current-buffer gnus-article-buffer
    (let ((buffer-read-only nil)
	  (inhibit-point-motion-hooks t)
	  (marks (gnus-dissect-cited-text))
	  (adaptive-fill-mode nil)
	  (filladapt-mode nil)
	  (fill-column (if width (prefix-numeric-value width) fill-column)))
      (save-restriction
	(while (cdr marks)
	  (narrow-to-region (caar marks) (caadr marks))
	  (let ((adaptive-fill-regexp
		 (concat "^" (regexp-quote (cdar marks)) " *"))
		(fill-prefix
		 (if (string= (cdar marks) "") ""
		   (concat (cdar marks) " ")))
		(do-fill (not long-lines))
		use-hard-newlines)
	    (unless do-fill
	      (setq do-fill (gnus-article-foldable-buffer (cdar marks))))
	    ;; Note: the XEmacs version of `fill-region' inserts a newline
	    ;; unless the region ends with a newline.
	    (when do-fill
	      (if (not long-lines)
		  (fill-region (point-min) (point-max))
		(goto-char (point-min))
		(while (not (eobp))
		  (end-of-line)
		  (when (prog1
			    (> (current-column) (window-width))
			  (forward-line 1))
		    (save-restriction
		      (narrow-to-region (line-beginning-position 0) (point))
		      (fill-region (point-min) (point-max))))))))
	  (set-marker (caar marks) nil)
	  (setq marks (cdr marks)))
	(when marks
	  (set-marker (caar marks) nil))
	;; All this information is now incorrect.
	(setq gnus-cite-prefix-alist nil
	      gnus-cite-attribution-alist nil
	      gnus-cite-loose-prefix-alist nil
	      gnus-cite-loose-attribution-alist nil
	      gnus-cite-article nil)))))

(defun gnus-article-foldable-buffer (prefix)
  (let ((do-fill nil)
	columns)
    (goto-char (point-min))
    (while (not (eobp))
      (unless (> (length prefix) (- (point-max) (point)))
	(forward-char (length prefix)))
      (skip-chars-forward " \t")
      (unless (eolp)
	(let ((elem (assq (current-column) columns)))
	  (unless elem
	    (setq elem (cons (current-column) 0))
	    (push elem columns))
	  (setcdr elem (1+ (cdr elem)))))
      (end-of-line)
      (when (> (current-column) (window-width))
	(setq do-fill t))
      (forward-line 1))
    (and do-fill
	 ;; We know know that there are long lines here, but does this look
	 ;; like code?  Check for ragged edges on the left.
	 (< (length columns) 3))))

(defun gnus-article-hide-citation (&optional arg force)
  "Toggle hiding of all cited text except attribution lines.
See the documentation for `gnus-article-highlight-citation'.
If given a negative prefix, always show; if given a positive prefix,
always hide."
  (interactive (append (gnus-article-hidden-arg) (list 'force)))
  (gnus-set-format 'cited-opened-text-button t)
  (gnus-set-format 'cited-closed-text-button t)
  (with-current-buffer gnus-article-buffer
    (let ((buffer-read-only nil)
          marks
          (inhibit-point-motion-hooks t)
          (props (nconc (list 'article-type 'cite)
                        gnus-hidden-properties))
          (point (point-min))
          found beg end start)
      (while (setq point
                   (text-property-any point (point-max)
                                      'gnus-callback
                                      'gnus-article-toggle-cited-text))
        (setq found t)
        (goto-char point)
        (gnus-article-toggle-cited-text
         (get-text-property point 'gnus-data) arg)
        (forward-line 1)
        (setq point (point)))
      (unless found
        (setq marks (gnus-dissect-cited-text))
        (while marks
          (setq beg nil
                end nil)
          (while (and marks (string= (cdar marks) ""))
            (setq marks (cdr marks)))
          (when marks
            (setq beg (caar marks)))
          (while (and marks (not (string= (cdar marks) "")))
            (setq marks (cdr marks)))
          (when marks
	    (setq end (caar marks)))
          ;; Skip past lines we want to leave visible.
          (when (and beg end gnus-cited-lines-visible)
            (goto-char beg)
            (forward-line (if (consp gnus-cited-lines-visible)
                              (car gnus-cited-lines-visible)
                            gnus-cited-lines-visible))
            (if (>= (point) end)
                (setq beg nil)
              (setq beg (point-marker))
              (when (consp gnus-cited-lines-visible)
                (goto-char end)
                (forward-line (- (cdr gnus-cited-lines-visible)))
                (if (<= (point) beg)
                    (setq beg nil)
		  (setq end (point-marker))))))
          (when (and beg end)
            (gnus-add-wash-type 'cite)
            ;; We use markers for the end-points to facilitate later
            ;; wrapping and mangling of text.
            (setq beg (set-marker (make-marker) beg)
                  end (set-marker (make-marker) end))
            (gnus-add-text-properties-when 'article-type nil beg end props)
            (goto-char beg)
            (when (and gnus-cite-blank-line-after-header
                       (not (save-excursion (search-backward "\n\n" nil t))))
              (insert "\n"))
            (put-text-property
             (setq start (point-marker))
             (progn
	       (gnus-article-add-button
		(point)
		(progn (eval gnus-cited-closed-text-button-line-format-spec)
		       (point))
		`gnus-article-toggle-cited-text
		(list (cons beg end) start))
	       (point))
             'article-type 'annotation)
            (set-marker beg (point))))))))

(defun gnus-article-toggle-cited-text (args &optional arg)
  "Toggle hiding the text in REGION.
ARG can be nil or a number.  Positive means hide, negative
means show, nil means toggle."
  (let* ((region (car args))
	 (beg (car region))
	 (end (cdr region))
	 (start (cadr args))
	 (hidden
	  (text-property-any beg (1- end) 'article-type 'cite))
	 (inhibit-point-motion-hooks t)
	 buffer-read-only)
    (when (or (null arg)
	      (zerop arg)
	      (and (> arg 0) (not hidden))
	      (and (< arg 0) hidden))
      (if hidden
	  (progn
	    ;; Can't remove 'cite from g-a-wash-types here because
	    ;; multiple citations may be hidden -jas
	    (gnus-remove-text-properties-when
	     'article-type 'cite beg end
	     (cons 'article-type (cons 'cite
				       gnus-hidden-properties))))
	(gnus-add-wash-type 'cite)
	(gnus-add-text-properties-when
	 'article-type nil beg end
	 (cons 'article-type (cons 'cite
				   gnus-hidden-properties))))
      (let ((gnus-article-mime-handle-alist-1 gnus-article-mime-handle-alist))
	(gnus-set-mode-line 'article))
      (save-excursion
	(goto-char start)
	(gnus-delete-line)
	(put-text-property
	 (point)
	 (progn
	   (gnus-article-add-button
	    (point)
	    (progn (eval
		    (if hidden
			gnus-cited-opened-text-button-line-format-spec
		      gnus-cited-closed-text-button-line-format-spec))
		   (point))
	    `gnus-article-toggle-cited-text
	    args)
	   (point))
	 'article-type 'annotation)))))

(defun gnus-article-hide-citation-maybe (&optional arg force)
  "Toggle hiding of cited text that has an attribution line.
If given a negative prefix, always show; if given a positive prefix,
always hide.
This will do nothing unless at least `gnus-cite-hide-percentage'
percent and at least `gnus-cite-hide-absolute' lines of the body is
cited text with attributions.  When called interactively, these two
variables are ignored.
See also the documentation for `gnus-article-highlight-citation'."
  (interactive (append (gnus-article-hidden-arg) '(force)))
  (with-current-buffer gnus-article-buffer
    (gnus-delete-wash-type 'cite)
    (unless (gnus-article-check-hidden-text 'cite arg)
      (save-excursion
	(gnus-cite-parse-maybe force)
	(article-goto-body)
	(let ((start (point))
	      (atts gnus-cite-attribution-alist)
	      (buffer-read-only nil)
	      (inhibit-point-motion-hooks t)
	      (hidden 0)
	      total)
	  (goto-char (point-max))
	  (gnus-article-search-signature)
	  (setq total (count-lines start (point)))
	  (while atts
	    (setq hidden (+ hidden (length (cdr (assoc (cdar atts)
						       gnus-cite-prefix-alist))))
		  atts (cdr atts)))
	  (when (or force
		    (and (> (* 100 hidden) (* gnus-cite-hide-percentage total))
			 (> hidden gnus-cite-hide-absolute)))
	    (gnus-add-wash-type 'cite)
	    (setq atts gnus-cite-attribution-alist)
	    (while atts
	      (setq total (cdr (assoc (cdar atts) gnus-cite-prefix-alist))
		    atts (cdr atts))
	      (while total
		(setq hidden (car total)
		      total (cdr total))
		(goto-char (point-min))
		(forward-line (1- hidden))
		(unless (assq hidden gnus-cite-attribution-alist)
		  (gnus-add-text-properties
		   (point) (progn (forward-line 1) (point))
		   (nconc (list 'article-type 'cite)
			  gnus-hidden-properties)))))))))
    (gnus-set-mode-line 'article)))

(defun gnus-article-hide-citation-in-followups ()
  "Hide cited text in non-root articles."
  (interactive)
  (with-current-buffer gnus-article-buffer
    (let ((article (cdr gnus-article-current)))
      (unless (with-current-buffer gnus-summary-buffer
		(gnus-article-displayed-root-p article))
	(gnus-article-hide-citation)))))

;;; Internal functions:

(defun gnus-cite-parse-maybe (&optional force no-overlay)
  "Always parse the buffer."
  (gnus-cite-localize)
  ;;Reset parser information.
  (setq gnus-cite-prefix-alist nil
	gnus-cite-attribution-alist nil
	gnus-cite-loose-prefix-alist nil
	gnus-cite-loose-attribution-alist nil)
  (unless no-overlay
    (gnus-cite-delete-overlays))
  ;; Parse if not too large.
  (if (and gnus-cite-parse-max-size
	   (> (buffer-size) gnus-cite-parse-max-size))
      ()
    (setq gnus-cite-article (cons (car gnus-article-current)
				  (cdr gnus-article-current)))
    (gnus-cite-parse-wrapper)))

(defun gnus-cite-delete-overlays ()
  (dolist (overlay gnus-cite-overlay-list)
    (ignore-errors
      (when (or (not (gnus-overlay-end overlay))
		(and (>= (gnus-overlay-end overlay) (point-min))
		     (<= (gnus-overlay-end overlay) (point-max))))
	(setq gnus-cite-overlay-list (delete overlay gnus-cite-overlay-list))
	(ignore-errors
	  (gnus-delete-overlay overlay))))))

(defun gnus-cite-parse-wrapper ()
  ;; Wrap chopped gnus-cite-parse.
  (article-goto-body)
  (let ((inhibit-point-motion-hooks t))
    (save-excursion
      (gnus-cite-parse-attributions))
    (save-excursion
      (gnus-cite-parse))
    (save-excursion
      (gnus-cite-connect-attributions))))

(defun gnus-cite-parse ()
  ;; Parse and connect citation prefixes and attribution lines.

  ;; Parse current buffer searching for citation prefixes.
  (let ((line (1+ (count-lines (point-min) (point))))
	(case-fold-search t)
	(max (save-excursion
	       (goto-char (point-max))
	       (gnus-article-search-signature)
	       (point)))
	(prefix-regexp (concat "^\\(" message-cite-prefix-regexp "\\)"))
	alist entry start begin end numbers prefix guess-limit)
    ;; Get all potential prefixes in `alist'.
    (while (< (point) max)
      ;; Each line.
      (setq begin (point)
	    guess-limit (progn (skip-chars-forward "^> \t\r\n") (point))
	    end (point-at-bol 2)
	    start end)
      (goto-char begin)
      ;; Ignore standard Supercite attribution prefix.
      (when (and (< guess-limit (+ begin gnus-cite-max-prefix))
		 (looking-at gnus-supercite-regexp))
	(if (match-end 1)
	    (setq end (1+ (match-end 1)))
	  (setq end (1+ begin))))
      ;; Ignore very long prefixes.
      (when (> end (+ begin gnus-cite-max-prefix))
	(setq end (+ begin gnus-cite-max-prefix)))
      ;; Ignore quoted envelope From_.
      (when (and gnus-cite-ignore-quoted-from
		 (prog2
		     (setq case-fold-search nil)
		     (looking-at ">From ")
		   (setq case-fold-search t)))
	(setq end (1+ begin)))
      (while (re-search-forward prefix-regexp (1- end) t)
	;; Each prefix.
	(setq end (match-end 0)
	      prefix (buffer-substring begin end))
	(set-text-properties 0 (length prefix) nil prefix)
	(setq entry (assoc prefix alist))
	(if entry
	    (setcdr entry (cons line (cdr entry)))
	  (push (list prefix line) alist))
	(goto-char begin))
      (goto-char start)
      (setq line (1+ line)))
    ;; Horrible special case for some Microsoft mailers.
    (goto-char (point-min))
    (setq start t begin nil entry nil)
    (while start
      ;; Assume this search ends up at the beginning of a line.
      (if (re-search-forward gnus-cite-unsightly-citation-regexp max t)
	  (progn
	    (when (number-or-marker-p start)
	      (setq begin (count-lines (point-min) start)
		    end (count-lines (point-min) (match-beginning 0))))
	    (setq start (match-end 0)))
	(when (number-or-marker-p start)
	  (setq begin (count-lines (point-min) start)
		end (count-lines (point-min) max)))
	(setq start nil))
      (when begin
	(while (< begin end)
	  ;; Need to do 1+ because we're in the bol.
	  (push (setq begin (1+ begin)) entry))))
    (when entry
      (push (cons "" entry) alist))
    ;; We got all the potential prefixes.  Now create
    ;; `gnus-cite-prefix-alist' containing the oldest prefix for each
    ;; line that appears at least `gnus-cite-minimum-match-count'
    ;; times.  First sort them by length.  Longer is older.
    (setq alist (sort alist (lambda (a b)
			      (> (length (car a)) (length (car b))))))
    (while alist
      (setq entry (car alist)
	    prefix (car entry)
	    numbers (cdr entry)
	    alist (cdr alist))
      (cond ((null numbers)
	     ;; No lines with this prefix that wasn't also part of
	     ;; a longer prefix.
	     )
	    ((< (length numbers) gnus-cite-minimum-match-count)
	     ;; Too few lines with this prefix.  We keep it a bit
	     ;; longer in case it is an exact match for an attribution
	     ;; line, but we don't remove the line from other
	     ;; prefixes.
	     (push entry gnus-cite-prefix-alist))
	    (t
	     (push entry
		   gnus-cite-prefix-alist)
	     ;; Remove articles from other prefixes.
	     (let ((loop alist)
		   current)
	       (while loop
		 (setq current (car loop)
		       loop (cdr loop))
		 (setcdr current
			 (gnus-set-difference (cdr current) numbers)))))))))

(defun gnus-cite-parse-attributions ()
  (let (al-alist)
    ;; Parse attributions
    (while (re-search-forward gnus-cite-attribution-suffix (point-max) t)
      (let* ((start (match-beginning 0))
	     (end (match-end 0))
	     (wrote (count-lines (point-min) end))
	     (prefix (gnus-cite-find-prefix wrote))
	     ;; Check previous line for an attribution leader.
	     (tag (progn
		    (beginning-of-line 1)
		    (when (looking-at gnus-supercite-secondary-regexp)
		      (buffer-substring (match-beginning 1)
					(match-end 1)))))
	     (in (progn
		   (goto-char start)
		   (and (re-search-backward gnus-cite-attribution-prefix
					    (save-excursion
					      (beginning-of-line 0)
					      (point))
					    t)
			(not (re-search-forward gnus-cite-attribution-suffix
						start t))
			(count-lines (point-min) (1+ (point)))))))
	(when (eq wrote in)
	  (setq in nil))
	(goto-char end)
	;; don't add duplicates
	(let ((al (buffer-substring (save-excursion (beginning-of-line 0)
						    (1+ (point)))
				    end)))
	  (when (not (assoc al al-alist))
	    (push (list wrote in prefix tag)
		  gnus-cite-loose-attribution-alist)
	    (push (cons al t) al-alist)))))))

(defun gnus-cite-connect-attributions ()
  ;; Connect attributions to citations

  ;; No citations have been connected to attribution lines yet.
  (setq gnus-cite-loose-prefix-alist (append gnus-cite-prefix-alist nil))

  ;; Parse current buffer searching for attribution lines.
  ;; Find exact supercite citations.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (when tag
				    (concat "\\`"
					    (regexp-quote prefix) "[ \t]*"
					    (regexp-quote tag) ">"))))
  ;; Find loose supercite citations after attributions.
  (gnus-cite-match-attributions 'small t
				(lambda (prefix tag)
				  (when tag
				    (concat "\\<"
					    (regexp-quote tag)
					    "\\>"))))
  ;; Find loose supercite citations anywhere.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (when tag
				    (concat "\\<"
					    (regexp-quote tag)
					    "\\>"))))
  ;; Find nested citations after attributions.
  (gnus-cite-match-attributions 'small-if-unique t
				(lambda (prefix tag)
				  (concat "\\`" (regexp-quote prefix) ".+")))
  ;; Find nested citations anywhere.
  (gnus-cite-match-attributions 'small nil
				(lambda (prefix tag)
				  (concat "\\`" (regexp-quote prefix) ".+")))
  ;; Remove loose prefixes with too few lines.
  (let ((alist gnus-cite-loose-prefix-alist)
	entry)
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (when (< (length (cdr entry)) gnus-cite-minimum-match-count)
	(setq gnus-cite-prefix-alist
	      (delq entry gnus-cite-prefix-alist)
	      gnus-cite-loose-prefix-alist
	      (delq entry gnus-cite-loose-prefix-alist)))))
  ;; Find flat attributions.
  (gnus-cite-match-attributions 'first t nil)
  ;; Find any attributions (are we getting desperate yet?).
  (gnus-cite-match-attributions 'first nil nil))

(defun gnus-cite-match-attributions (sort after fun)
  ;; Match all loose attributions and citations (SORT AFTER FUN) .
  ;;
  ;; If SORT is `small', the citation with the shortest prefix will be
  ;; used, if it is `first' the first prefix will be used, if it is
  ;; `small-if-unique' the shortest prefix will be used if the
  ;; attribution line does not share its own prefix with other
  ;; loose attribution lines, otherwise the first prefix will be used.
  ;;
  ;; If AFTER is non-nil, only citations after the attribution line
  ;; will be considered.
  ;;
  ;; If FUN is non-nil, it will be called with the arguments (WROTE
  ;; PREFIX TAG) and expected to return a regular expression.  Only
  ;; citations whose prefix matches the regular expression will be
  ;; considered.
  ;;
  ;; WROTE is the attribution line number.
  ;; PREFIX is the attribution line prefix.
  ;; TAG is the Supercite tag on the attribution line.
  (let ((atts gnus-cite-loose-attribution-alist)
	(case-fold-search t)
	att wrote in prefix tag regexp limit smallest best size)
    (while atts
      (setq att (car atts)
	    atts (cdr atts)
	    wrote (nth 0 att)
	    in (nth 1 att)
	    prefix (nth 2 att)
	    tag (nth 3 att)
	    regexp (if fun (funcall fun prefix tag) "")
	    size (cond ((eq sort 'small) t)
		       ((eq sort 'first) nil)
		       (t (< (length (gnus-cite-find-loose prefix)) 2)))
	    limit (if after wrote -1)
	    smallest 1000000
	    best nil)
      (let ((cites gnus-cite-loose-prefix-alist)
	    cite candidate numbers first compare)
	(while cites
	  (setq cite (car cites)
		cites (cdr cites)
		candidate (car cite)
		numbers (cdr cite)
		first (apply 'min numbers)
		compare (if size (length candidate) first))
	  (and (> first limit)
	       regexp
	       (string-match regexp candidate)
	       (< compare smallest)
	       (setq best cite
		     smallest compare))))
      (if (null best)
	  ()
	(setq gnus-cite-loose-attribution-alist
	      (delq att gnus-cite-loose-attribution-alist))
	(push (cons wrote (car best)) gnus-cite-attribution-alist)
	(when in
	  (push (cons in (car best)) gnus-cite-attribution-alist))
	(when (memq best gnus-cite-loose-prefix-alist)
	  (let ((loop gnus-cite-prefix-alist)
		(numbers (cdr best))
		current)
	    (setq gnus-cite-loose-prefix-alist
		  (delq best gnus-cite-loose-prefix-alist))
	    (while loop
	      (setq current (car loop)
		    loop (cdr loop))
	      (if (eq current best)
		  ()
		(setcdr current (gnus-set-difference (cdr current) numbers))
		(when (null (cdr current))
		  (setq gnus-cite-loose-prefix-alist
			(delq current gnus-cite-loose-prefix-alist)
			atts (delq current atts)))))))))))

(defun gnus-cite-find-loose (prefix)
  ;; Return a list of loose attribution lines prefixed by PREFIX.
  (let* ((atts gnus-cite-loose-attribution-alist)
	 att line lines)
    (while atts
      (setq att (car atts)
	    line (car att)
	    atts (cdr atts))
      (when (string-equal (gnus-cite-find-prefix line) prefix)
	(push line lines)))
    lines))

(defun gnus-cite-add-face (number prefix face)
  ;; At line NUMBER, ignore PREFIX and add FACE to the rest of the line.
  (when face
    (let ((inhibit-point-motion-hooks t)
	  from to overlay)
      (goto-char (point-min))
      (when (zerop (forward-line (1- number)))
	(forward-char (length prefix))
	(skip-chars-forward " \t")
	(setq from (point))
	(end-of-line 1)
	(skip-chars-backward " \t")
	(setq to (point))
	(when (< from to)
	  (push (setq overlay (gnus-make-overlay from to))
		gnus-cite-overlay-list)
	  (gnus-overlay-put overlay 'evaporate t)
	  (gnus-overlay-put overlay 'face face))))))

(defun gnus-cite-toggle (prefix)
  (with-current-buffer gnus-article-buffer
    (gnus-cite-parse-maybe nil t)
    (let ((buffer-read-only nil)
	  (numbers (cdr (assoc prefix gnus-cite-prefix-alist)))
	  (inhibit-point-motion-hooks t)
	  number)
      (while numbers
	(setq number (car numbers)
	      numbers (cdr numbers))
	(goto-char (point-min))
	(forward-line (1- number))
	(cond ((get-text-property (point) 'invisible)
	       ;; Can't remove 'cite from g-a-wash-types here because
	       ;; multiple citations may be hidden -jas
	       (remove-text-properties (point) (progn (forward-line 1) (point))
				       gnus-hidden-properties))
	      ((assq number gnus-cite-attribution-alist))
	      (t
	       (gnus-add-wash-type 'cite)
	       (gnus-add-text-properties
		(point) (progn (forward-line 1) (point))
		(nconc (list 'article-type 'cite)
		       gnus-hidden-properties))))
	(let ((gnus-article-mime-handle-alist-1
	       gnus-article-mime-handle-alist))
	  (gnus-set-mode-line 'article))))))

(defun gnus-cite-find-prefix (line)
  ;; Return citation prefix for LINE.
  (let ((alist gnus-cite-prefix-alist)
	(prefix "")
	entry)
    (while alist
      (setq entry (car alist)
	    alist (cdr alist))
      (when (memq line (cdr entry))
	(setq prefix (car entry))))
    prefix))

(defun gnus-cite-localize ()
  "Make the citation variables local to the article buffer."
  (let ((vars '(gnus-cite-article
		gnus-cite-overlay-list gnus-cite-prefix-alist
		gnus-cite-attribution-alist gnus-cite-loose-prefix-alist
		gnus-cite-loose-attribution-alist)))
    (while vars
      (make-local-variable (pop vars)))))

(defun gnus-cited-line-p ()
  "Say whether the current line is a cited line."
  (save-excursion
    (beginning-of-line)
    (let ((found nil))
      (dolist (prefix (mapcar 'car gnus-cite-prefix-alist))
	(when (string= (buffer-substring (point) (+ (length prefix) (point)))
		       prefix)
	  (setq found t)))
      found)))


;; Highlighting of different citation levels in message-mode.
;; - message-cite-prefix will be overridden if this is enabled.

(defvar gnus-message-max-citation-depth
  (length gnus-cite-face-list)
  "Maximum supported level of citation.")

(defvar gnus-message-cite-prefix-regexp
  (concat "^\\(?:" message-cite-prefix-regexp "\\)"))

(defun gnus-message-search-citation-line (limit)
  "Search for a cited line and set match data accordingly.
Returns nil if there is no such line before LIMIT, t otherwise."
  (when (re-search-forward gnus-message-cite-prefix-regexp limit t)
    (let ((cdepth (min (length (apply 'concat
				      (split-string
				       (match-string-no-properties 0)
				       "[ \t [:alnum:]]+")))
		       gnus-message-max-citation-depth))
	  (mlist (make-list (* (1+ gnus-message-max-citation-depth) 2) nil))
	  (start (point-at-bol))
	  (end (point-at-eol)))
      (setcar mlist start)
      (setcar (cdr mlist) end)
      (setcar (nthcdr (* cdepth 2) mlist) start)
      (setcar (nthcdr (1+ (* cdepth 2)) mlist) end)
      (set-match-data mlist))
    t))

(defvar gnus-message-citation-keywords
  ;; eval-when-compile ;; This breaks in XEmacs
  `((gnus-message-search-citation-line
     ,@(let ((list nil)
	     (count 1))
	 ;; (require 'gnus-cite)
	 (dolist (face gnus-cite-face-list (nreverse list))
	   (push (list count (list 'quote face) 'prepend t) list)
	   (setq count (1+ count)))))) ;;
  "Keywords for highlighting different levels of message citations.")

(defvar font-lock-defaults-computed)
(defvar font-lock-keywords)
(defvar font-lock-set-defaults)

(eval-and-compile
  (unless (featurep 'xemacs)
    (autoload 'font-lock-set-defaults "font-lock")))

(define-minor-mode gnus-message-citation-mode
  "Minor mode providing more font-lock support for nested citations.
When enabled, it automatically turns on `font-lock-mode'."
  nil ;; init-value
  "" ;; lighter
  nil ;; keymap
  (when (eq major-mode 'message-mode)
    (let ((defaults (car (if (featurep 'xemacs)
			     (get 'message-mode 'font-lock-defaults)
			   font-lock-defaults)))
	  default keywords)
      (while defaults
	(setq default (if (consp defaults)
			  (pop defaults)
			(prog1
			    defaults
			  (setq defaults nil))))
	(if gnus-message-citation-mode
	    ;; `gnus-message-citation-keywords' should be the last
	    ;; elements of the keywords because the others are unlikely
	    ;; to have the OVERRIDE flags -- XEmacs applies a keyword
	    ;; having no OVERRIDE flag to matched text even if it has
	    ;; already other faces, while Emacs doesn't.
	    (set (make-local-variable default)
		 (append (default-value default)
			 gnus-message-citation-keywords))
	  (kill-local-variable default))))
    ;; Force `font-lock-set-defaults' to update `font-lock-keywords'.
    (if (featurep 'xemacs)
	(progn
	  (require 'font-lock)
	  (setq font-lock-defaults-computed nil
		font-lock-keywords nil))
      (setq font-lock-set-defaults nil))
    (font-lock-set-defaults)
    (cond ((symbol-value 'font-lock-mode)
	   (font-lock-fontify-buffer))
	  (gnus-message-citation-mode
	   (font-lock-mode 1)))))

(defun turn-on-gnus-message-citation-mode ()
  "Turn on `gnus-message-citation-mode'."
  (gnus-message-citation-mode 1))
(defun turn-off-gnus-message-citation-mode ()
  "Turn off `gnus-message-citation-mode'."
  (gnus-message-citation-mode -1))

(gnus-ems-redefine)

(provide 'gnus-cite)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; gnus-cite.el ends here
