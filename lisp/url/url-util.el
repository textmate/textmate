;;; url-util.el --- Miscellaneous helper routines for URL library

;; Copyright (C) 1996-1999, 2001, 2004-2012  Free Software Foundation, Inc.

;; Author: Bill Perry <wmperry@gnu.org>
;; Keywords: comm, data, processes

;; This file is part of GNU Emacs.
;;
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

(require 'url-parse)
(require 'url-vars)
(eval-when-compile (require 'cl))
(autoload 'timezone-parse-date "timezone")
(autoload 'timezone-make-date-arpa-standard "timezone")
(autoload 'mail-header-extract "mailheader")

(defvar url-parse-args-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "A syntax table for parsing sgml attributes.")

(modify-syntax-entry ?' "\"" url-parse-args-syntax-table)
(modify-syntax-entry ?` "\"" url-parse-args-syntax-table)
(modify-syntax-entry ?{ "(" url-parse-args-syntax-table)
(modify-syntax-entry ?} ")" url-parse-args-syntax-table)

;;;###autoload
(defcustom url-debug nil
  "What types of debug messages from the URL library to show.
Debug messages are logged to the *URL-DEBUG* buffer.

If t, all messages will be logged.
If a number, all messages will be logged, as well shown via `message'.
If a list, it is a list of the types of messages to be logged."
  :type '(choice (const :tag "none" nil)
		 (const :tag "all" t)
		 (checklist :tag "custom"
			    (const :tag "HTTP" :value http)
			    (const :tag "DAV" :value dav)
			    (const :tag "General" :value retrieval)
			    (const :tag "Filename handlers" :value handlers)
			    (symbol :tag "Other")))
  :group 'url-hairy)

;;;###autoload
(defun url-debug (tag &rest args)
  (if quit-flag
      (error "Interrupted!"))
  (if (or (eq url-debug t)
	  (numberp url-debug)
	  (and (listp url-debug) (memq tag url-debug)))
      (with-current-buffer (get-buffer-create "*URL-DEBUG*")
	(goto-char (point-max))
	(insert (symbol-name tag) " -> " (apply 'format args) "\n")
	(if (numberp url-debug)
	    (apply 'message args)))))

;;;###autoload
(defun url-parse-args (str &optional nodowncase)
  ;; Return an assoc list of attribute/value pairs from an RFC822-type string
  (let (
	name				; From name=
	value				; its value
	results				; Assoc list of results
	name-pos			; Start of XXXX= position
	val-pos				; Start of value position
	st
	nd
	)
    (save-excursion
      (save-restriction
	(set-buffer (get-buffer-create " *urlparse-temp*"))
	(set-syntax-table url-parse-args-syntax-table)
	(erase-buffer)
	(insert str)
	(setq st (point-min)
	      nd (point-max))
	(set-syntax-table url-parse-args-syntax-table)
	(narrow-to-region st nd)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward "; \n\t")
	  (setq name-pos (point))
	  (skip-chars-forward "^ \n\t=;")
	  (if (not nodowncase)
	      (downcase-region name-pos (point)))
	  (setq name (buffer-substring name-pos (point)))
	  (skip-chars-forward " \t\n")
	  (if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	      (setq value nil)
	    (skip-chars-forward " \t\n=")
	    (setq val-pos (point)
		  value
		  (cond
		   ((or (= (or (char-after val-pos) 0) ?\")
			(= (or (char-after val-pos) 0) ?'))
		    (buffer-substring (1+ val-pos)
				      (condition-case ()
					  (prog2
					      (forward-sexp 1)
					      (1- (point))
					    (skip-chars-forward "\""))
					(error
					 (skip-chars-forward "^ \t\n")
					 (point)))))
		   (t
		    (buffer-substring val-pos
				      (progn
					(skip-chars-forward "^;")
					(skip-chars-backward " \t")
					(point)))))))
	  (setq results (cons (cons name value) results))
	  (skip-chars-forward "; \n\t"))
	results))))

;;;###autoload
(defun url-insert-entities-in-string (string)
  "Convert HTML markup-start characters to entity references in STRING.
Also replaces the \" character, so that the result may be safely used as
  an attribute value in a tag.  Returns a new string with the result of the
  conversion.  Replaces these characters as follows:
    &  ==>  &amp;
    <  ==>  &lt;
    >  ==>  &gt;
    \"  ==>  &quot;"
  (if (string-match "[&<>\"]" string)
      (with-current-buffer (get-buffer-create " *entity*")
	(erase-buffer)
	(buffer-disable-undo (current-buffer))
	(insert string)
	(goto-char (point-min))
	(while (progn
		 (skip-chars-forward "^&<>\"")
		 (not (eobp)))
	  (insert (cdr (assq (char-after (point))
			     '((?\" . "&quot;")
			       (?& . "&amp;")
			       (?< . "&lt;")
			       (?> . "&gt;")))))
	  (delete-char 1))
	(buffer-string))
    string))

;;;###autoload
(defun url-normalize-url (url)
  "Return a 'normalized' version of URL.
Strips out default port numbers, etc."
  (let (type data retval)
    (setq data (url-generic-parse-url url)
	  type (url-type data))
    (if (member type '("www" "about" "mailto" "info"))
	(setq retval url)
      ;; FIXME all this does, and all this function seems to do in
      ;; most cases, is remove any trailing "#anchor" part of a url.
      (setf (url-target data) nil)
      (setq retval (url-recreate-url data)))
    retval))

;;;###autoload
(defun url-lazy-message (&rest args)
  "Just like `message', but is a no-op if called more than once a second.
Will not do anything if `url-show-status' is nil."
  (if (or (and url-current-object
	       (url-silent url-current-object))
	  (null url-show-status)
	  (active-minibuffer-window)
	  (= url-lazy-message-time
	     (setq url-lazy-message-time (nth 1 (current-time)))))
      nil
    (apply 'message args)))

;;;###autoload
(defun url-get-normalized-date (&optional specified-time)
 "Return a 'real' date string that most HTTP servers can understand."
 (let ((system-time-locale "C"))
  (format-time-string "%a, %d %b %Y %T GMT"
   (or specified-time (current-time)) t)))

;;;###autoload
(defun url-eat-trailing-space (x)
  "Remove spaces/tabs at the end of a string."
  (let ((y (1- (length x)))
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (>= y 0) (memq (aref x y) skip-chars))
      (setq y (1- y)))
    (substring x 0 (1+ y))))

;;;###autoload
(defun url-strip-leading-spaces (x)
  "Remove spaces at the front of a string."
  (let ((y (1- (length x)))
	(z 0)
	(skip-chars (list ?  ?\t ?\n)))
    (while (and (<= z y) (memq (aref x z) skip-chars))
      (setq z (1+ z)))
    (substring x z nil)))

;;;###autoload
(defun url-pretty-length (n)
  (cond
   ((< n 1024)
    (format "%d bytes" n))
   ((< n (* 1024 1024))
    (format "%dk" (/ n 1024.0)))
   (t
    (format "%2.2fM" (/ n (* 1024 1024.0))))))

;;;###autoload
(defun url-display-percentage (fmt perc &rest args)
  (when (and url-show-status
	     (or (null url-current-object)
		 (not (url-silent url-current-object))))
    (if (null fmt)
	(if (fboundp 'clear-progress-display)
	    (clear-progress-display))
      (if (and (fboundp 'progress-display) perc)
	  (apply 'progress-display fmt perc args)
	(apply 'message fmt args)))))

;;;###autoload
(defun url-percentage (x y)
  (if (fboundp 'float)
      (round (* 100 (/ x (float y))))
    (/ (* x 100) y)))

;;;###autoload
(defalias 'url-basepath 'url-file-directory)

;;;###autoload
(defun url-file-directory (file)
  "Return the directory part of FILE, for a URL."
  (cond
   ((null file) "")
   ((string-match "\\?" file)
    (file-name-directory (substring file 0 (match-beginning 0))))
   (t (file-name-directory file))))

;;;###autoload
(defun url-file-nondirectory (file)
  "Return the nondirectory part of FILE, for a URL."
  (cond
   ((null file) "")
   ((string-match "\\?" file)
    (file-name-nondirectory (substring file 0 (match-beginning 0))))
   (t (file-name-nondirectory file))))

;;;###autoload
(defun url-parse-query-string (query &optional downcase allow-newlines)
  (let (retval pairs cur key val)
    (setq pairs (split-string query "&"))
    (while pairs
      (setq cur (car pairs)
	    pairs (cdr pairs))
      (if (not (string-match "=" cur))
	  nil				; Grace
	(setq key (url-unhex-string (substring cur 0 (match-beginning 0))
				    allow-newlines))
	(setq val (url-unhex-string (substring cur (match-end 0) nil)
				    allow-newlines))
	(if downcase
	    (setq key (downcase key)))
	(setq cur (assoc key retval))
	(if cur
	    (setcdr cur (cons val (cdr cur)))
	  (setq retval (cons (list key val) retval)))))
    retval))

(defun url-unhex (x)
  (if (> x ?9)
      (if (>= x ?a)
	  (+ 10 (- x ?a))
	(+ 10 (- x ?A)))
    (- x ?0)))

;; Fixme: Is this definition better, and does it ever matter?

;; (defun url-unhex-string (str &optional allow-newlines)
;;   "Remove %XX, embedded spaces, etc in a url.
;; If optional second argument ALLOW-NEWLINES is non-nil, then allow the
;; decoding of carriage returns and line feeds in the string, which is normally
;; forbidden in URL encoding."
;;   (setq str (or str ""))
;;   (setq str (replace-regexp-in-string "%[[:xdigit:]]\\{2\\}"
;; 				      (lambda (match)
;; 					(string (string-to-number
;; 						 (substring match 1) 16)))
;; 				      str t t))
;;   (if allow-newlines
;;       (replace-regexp-in-string "[\n\r]" (lambda (match)
;; 					   (format "%%%.2X" (aref match 0)))
;; 				str t t)
;;     str))

;;;###autoload
(defun url-unhex-string (str &optional allow-newlines)
  "Remove %XX embedded spaces, etc in a URL.
If optional second argument ALLOW-NEWLINES is non-nil, then allow the
decoding of carriage returns and line feeds in the string, which is normally
forbidden in URL encoding."
  (setq str (or str ""))
  (let ((tmp "")
	(case-fold-search t))
    (while (string-match "%[0-9a-f][0-9a-f]" str)
      (let* ((start (match-beginning 0))
	     (ch1 (url-unhex (elt str (+ start 1))))
	     (code (+ (* 16 ch1)
		      (url-unhex (elt str (+ start 2))))))
	(setq tmp (concat
		   tmp (substring str 0 start)
		   (cond
		    (allow-newlines
		     (byte-to-string code))
		    ((or (= code ?\n) (= code ?\r))
		     " ")
		    (t (byte-to-string code))))
	      str (substring str (match-end 0)))))
    (setq tmp (concat tmp str))
    tmp))

(defconst url-unreserved-chars
  '(
    ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
    ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
    ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
    ?- ?_ ?. ?! ?~ ?* ?' ?\( ?\))
  "A list of characters that are _NOT_ reserved in the URL spec.
This is taken from RFC 2396.")

;;;###autoload
(defun url-hexify-string (string)
  "Return a new string that is STRING URI-encoded.
First, STRING is converted to utf-8, if necessary.  Then, for each
character in the utf-8 string, those found in `url-unreserved-chars'
are left as-is, all others are represented as a three-character
string: \"%\" followed by two lowercase hex digits."
  ;; To go faster and avoid a lot of consing, we could do:
  ;;
  ;; (defconst url-hexify-table
  ;;   (let ((map (make-vector 256 nil)))
  ;;     (dotimes (byte 256) (aset map byte
  ;;                               (if (memq byte url-unreserved-chars)
  ;;                                   (char-to-string byte)
  ;;                                 (format "%%%02x" byte))))
  ;;     map))
  ;;
  ;; (mapconcat (curry 'aref url-hexify-table) ...)
  (mapconcat (lambda (byte)
               (if (memq byte url-unreserved-chars)
                   (char-to-string byte)
                 (format "%%%02x" byte)))
             (if (multibyte-string-p string)
                 (encode-coding-string string 'utf-8)
               string)
             ""))

;;;###autoload
(defun url-file-extension (fname &optional x)
  "Return the filename extension of FNAME.
If optional argument X is t, then return the basename
of the file with the extension stripped off."
  (if (and fname
	   (setq fname (url-file-nondirectory fname))
	   (string-match "\\.[^./]+$" fname))
      (if x (substring fname 0 (match-beginning 0))
	(substring fname (match-beginning 0) nil))
    ;;
    ;; If fname has no extension, and x then return fname itself instead of
    ;; nothing. When caching it allows the correct .hdr file to be produced
    ;; for filenames without extension.
    ;;
    (if x
 	fname
      "")))

;;;###autoload
(defun url-truncate-url-for-viewing (url &optional width)
  "Return a shortened version of URL that is WIDTH characters wide or less.
WIDTH defaults to the current frame width."
  (let* ((fr-width (or width (frame-width)))
	 (str-width (length url))
	 (fname nil)
	 (modified 0)
	 (urlobj nil))
    ;; The first thing that can go are the search strings
    (if (and (>= str-width fr-width)
	     (string-match "?" url))
	(setq url (concat (substring url 0 (match-beginning 0)) "?...")
	      str-width (length url)))
    (if (< str-width fr-width)
	nil				; Hey, we are done!
      (setq urlobj (url-generic-parse-url url)
	    fname (url-filename urlobj)
	    fr-width (- fr-width 4))
      (while (and (>= str-width fr-width)
		  (string-match "/" fname))
	(setq fname (substring fname (match-end 0) nil)
	      modified (1+ modified))
	(setf (url-filename urlobj) fname)
	(setq url (url-recreate-url urlobj)
	      str-width (length url)))
      (if (> modified 1)
	  (setq fname (concat "/.../" fname))
	(setq fname (concat "/" fname)))
      (setf (url-filename urlobj) fname)
      (setq url (url-recreate-url urlobj)))
    url))

;;;###autoload
(defun url-view-url (&optional no-show)
  "View the current document's URL.
Optional argument NO-SHOW means just return the URL, don't show it in
the minibuffer.

This uses `url-current-object', set locally to the buffer."
  (interactive)
  (if (not url-current-object)
      nil
    (if no-show
	(url-recreate-url url-current-object)
      (message "%s" (url-recreate-url url-current-object)))))

(defvar url-get-url-filename-chars "-%.?@a-zA-Z0-9()_/:~=&"
  "Valid characters in a URL.")

(defun url-get-url-at-point (&optional pt)
  "Get the URL closest to point, but don't change position.
Has a preference for looking backward when not directly on a symbol."
  ;; Not at all perfect - point must be right in the name.
  (save-excursion
    (if pt (goto-char pt))
    (let (start url)
      (save-excursion
	;; first see if you're just past a filename
	(if (not (eobp))
	    (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
		(progn
		  (skip-chars-backward " \n\t\r({[]})")
		  (if (not (bobp))
		      (backward-char 1)))))
	(if (and (char-after (point))
		 (string-match (concat "[" url-get-url-filename-chars "]")
			       (char-to-string (char-after (point)))))
	    (progn
	      (skip-chars-backward url-get-url-filename-chars)
	      (setq start (point))
	      (skip-chars-forward url-get-url-filename-chars))
	  (setq start (point)))
	(setq url (buffer-substring-no-properties start (point))))
      (if (and url (string-match "^(.*)\\.?$" url))
	  (setq url (match-string 1 url)))
      (if (and url (string-match "^URL:" url))
	  (setq url (substring url 4 nil)))
      (if (and url (string-match "\\.$" url))
	  (setq url (substring url 0 -1)))
      (if (and url (string-match "^www\\." url))
	  (setq url (concat "http://" url)))
      (if (and url (not (string-match url-nonrelative-link url)))
	  (setq url nil))
      url)))

(defun url-generate-unique-filename (&optional fmt)
  "Generate a unique filename in `url-temporary-directory'."
  ;; This variable is obsolete, but so is this function.
  (let ((tempdir (with-no-warnings url-temporary-directory)))
    (if (not fmt)
	(let ((base (format "url-tmp.%d" (user-real-uid)))
	      (fname "")
	      (x 0))
	  (setq fname (format "%s%d" base x))
	  (while (file-exists-p
		  (expand-file-name fname tempdir))
	    (setq x (1+ x)
		  fname (concat base (int-to-string x))))
	  (expand-file-name fname tempdir))
      (let ((base (concat "url" (int-to-string (user-real-uid))))
	    (fname "")
	    (x 0))
	(setq fname (format fmt (concat base (int-to-string x))))
	(while (file-exists-p
		(expand-file-name fname tempdir))
	  (setq x (1+ x)
		fname (format fmt (concat base (int-to-string x)))))
	(expand-file-name fname tempdir)))))
(make-obsolete 'url-generate-unique-filename 'make-temp-file "23.1")

(defun url-extract-mime-headers ()
  "Set `url-current-mime-headers' in current buffer."
  (save-excursion
    (goto-char (point-min))
    (unless url-current-mime-headers
      (set (make-local-variable 'url-current-mime-headers)
	   (mail-header-extract)))))

(defun url-make-private-file (file)
  "Make FILE only readable and writable by the current user.
Creates FILE and its parent directories if they do not exist."
  (let ((dir (file-name-directory file)))
    (when dir
      ;; For historical reasons.
      (make-directory dir t)))
  ;; Based on doc-view-make-safe-dir.
  (condition-case nil
      (let ((umask (default-file-modes)))
        (unwind-protect
            (progn
              (set-default-file-modes #o0600)
              (with-temp-buffer
                (write-region (point-min) (point-max)
                              file nil 'silent nil 'excl)))
          (set-default-file-modes umask)))
    (file-already-exists
     (if (file-symlink-p file)
         (error "Danger: `%s' is a symbolic link" file))
     (set-file-modes file #o0600))))

(provide 'url-util)

;;; url-util.el ends here
