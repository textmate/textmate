;;; nnweb.el --- retrieving articles via web search engines

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

;; Note: You need to have `w3' installed for some functions to work.

;;; Code:

(eval-when-compile (require 'cl))

(require 'nnoo)
(require 'message)
(require 'gnus-util)
(require 'gnus)
(require 'nnmail)
(require 'mm-util)
(require 'mm-url)
(eval-and-compile
  (ignore-errors
    (require 'url)))
(autoload 'w3-parse-buffer "w3-parse")

(nnoo-declare nnweb)

(defvoo nnweb-directory (nnheader-concat gnus-directory "nnweb/")
  "Where nnweb will save its files.")

(defvoo nnweb-type 'google
  "What search engine type is being used.
Valid types include `google', `dejanews', and `gmane'.")

(defvar nnweb-type-definition
  '((google
     (id . "http://www.google.com/groups?as_umsgid=%s&hl=en&dmode=source")
     (result . "http://groups.google.com/group/%s/msg/%s?dmode=source")
     (article . nnweb-google-wash-article)
     (reference . identity)
     (map . nnweb-google-create-mapping)
     (search . nnweb-google-search)
     (address . "http://groups.google.com/groups")
     (base    . "http://groups.google.com")
     (identifier . nnweb-google-identity))
    (dejanews ;; alias of google
     (id . "http://www.google.com/groups?as_umsgid=%s&hl=en&dmode=source")
     (result . "http://groups.google.com/group/%s/msg/%s?dmode=source")
     (article . nnweb-google-wash-article)
     (reference . identity)
     (map . nnweb-google-create-mapping)
     (search . nnweb-google-search)
     (address . "http://groups.google.com/groups")
     (base    . "http://groups.google.com")
     (identifier . nnweb-google-identity))
    (gmane
     (article . nnweb-gmane-wash-article)
     (id . "http://gmane.org/view.php?group=%s")
     (reference . identity)
     (map . nnweb-gmane-create-mapping)
     (search . nnweb-gmane-search)
     (address . "http://search.gmane.org/nov.php")
     (identifier . nnweb-gmane-identity)))
  "Type-definition alist.")

(defvoo nnweb-search nil
  "Search string to feed to Google.")

(defvoo nnweb-max-hits 999
  "Maximum number of hits to display.")

(defvoo nnweb-ephemeral-p nil
  "Whether this nnweb server is ephemeral.")

;;; Internal variables

(defvoo nnweb-articles nil)
(defvoo nnweb-buffer nil)
(defvoo nnweb-group-alist nil)
(defvoo nnweb-group nil)
(defvoo nnweb-hashtb nil)

;;; Interface functions

(nnoo-define-basics nnweb)

(deffoo nnweb-retrieve-headers (articles &optional group server fetch-old)
  (nnweb-possibly-change-server group server)
  (with-current-buffer nntp-server-buffer
    (erase-buffer)
    (let (article header)
      (mm-with-unibyte-current-buffer
	(while (setq article (pop articles))
	  (when (setq header (cadr (assq article nnweb-articles)))
	    (nnheader-insert-nov header))))
      'nov)))

(deffoo nnweb-request-scan (&optional group server)
  (nnweb-possibly-change-server group server)
  (if nnweb-ephemeral-p
      (setq nnweb-hashtb (gnus-make-hashtable 4095))
    (unless nnweb-articles
      (nnweb-read-overview group)))
  (funcall (nnweb-definition 'map))
  (unless nnweb-ephemeral-p
    (nnweb-write-active)
    (nnweb-write-overview group)))

(deffoo nnweb-request-group (group &optional server dont-check info)
  (nnweb-possibly-change-server group server)
  (unless (or nnweb-ephemeral-p
	      dont-check
	      nnweb-articles)
    (nnweb-read-overview group))
  (cond
   ((not nnweb-articles)
    (nnheader-report 'nnweb "No matching articles"))
   (t
    (let ((active (if nnweb-ephemeral-p
		      (cons (caar nnweb-articles)
			    (caar (last nnweb-articles)))
		    (cadr (assoc group nnweb-group-alist)))))
      (nnheader-report 'nnweb "Opened group %s" group)
      (nnheader-insert
       "211 %d %d %d %s\n" (length nnweb-articles)
       (car active) (cdr active) group)))))

(deffoo nnweb-close-group (group &optional server)
  (nnweb-possibly-change-server group server)
  (when (gnus-buffer-live-p nnweb-buffer)
    (with-current-buffer nnweb-buffer
      (set-buffer-modified-p nil)
      (kill-buffer nnweb-buffer)))
  t)

(deffoo nnweb-request-article (article &optional group server buffer)
  (nnweb-possibly-change-server group server)
  (with-current-buffer (or buffer nntp-server-buffer)
    (let* ((header (cadr (assq article nnweb-articles)))
	   (url (and header (mail-header-xref header))))
      (when (or (and url
		     (mm-with-unibyte-current-buffer
		       (mm-url-insert url)))
		(and (stringp article)
		     (nnweb-definition 'id t)
		     (let ((fetch (nnweb-definition 'id))
			   art active)
		       (when (string-match "^<\\(.*\\)>$" article)
			 (setq art (match-string 1 article)))
		       (when (and fetch art)
			 (setq url (format fetch
					   (mm-url-form-encode-xwfu art)))
			 (mm-with-unibyte-current-buffer
			   (mm-url-insert url))
			 (if (nnweb-definition 'reference t)
			     (setq article
				   (funcall (nnweb-definition
					     'reference) article)))))))
	(unless nnheader-callback-function
	  (funcall (nnweb-definition 'article)))
	(nnheader-report 'nnweb "Fetched article %s" article)
	(cons group (and (numberp article) article))))))

(deffoo nnweb-close-server (&optional server)
  (when (and (nnweb-server-opened server)
	     (gnus-buffer-live-p nnweb-buffer))
    (with-current-buffer nnweb-buffer
      (set-buffer-modified-p nil)
      (kill-buffer nnweb-buffer)))
  (nnoo-close-server 'nnweb server))

(deffoo nnweb-request-list (&optional server)
  (nnweb-possibly-change-server nil server)
  (with-current-buffer nntp-server-buffer
    (nnmail-generate-active (list (assoc server nnweb-group-alist)))
    t))

(deffoo nnweb-request-update-info (group info &optional server))

(deffoo nnweb-asynchronous-p ()
  nil)

(deffoo nnweb-request-create-group (group &optional server args)
  (nnweb-possibly-change-server nil server)
  (nnweb-request-delete-group group)
  (push `(,group ,(cons 1 0)) nnweb-group-alist)
  (nnweb-write-active)
  t)

(deffoo nnweb-request-delete-group (group &optional force server)
  (nnweb-possibly-change-server group server)
  (gnus-alist-pull group nnweb-group-alist t)
  (nnweb-write-active)
  (gnus-delete-file (nnweb-overview-file group))
  t)

(nnoo-define-skeleton nnweb)

;;; Internal functions

(defun nnweb-read-overview (group)
  "Read the overview of GROUP and build the map."
  (when (file-exists-p (nnweb-overview-file group))
    (mm-with-unibyte-buffer
      (nnheader-insert-file-contents (nnweb-overview-file group))
      (goto-char (point-min))
      (let (header)
	(while (not (eobp))
	  (setq header (nnheader-parse-nov))
	  (forward-line 1)
	  (push (list (mail-header-number header)
		      header (mail-header-xref header))
		nnweb-articles)
	  (nnweb-set-hashtb header (car nnweb-articles)))))))

(defun nnweb-write-overview (group)
  "Write the overview file for GROUP."
  (with-temp-file (nnweb-overview-file group)
    (let ((articles nnweb-articles))
      (while articles
	(nnheader-insert-nov (cadr (pop articles)))))))

(defun nnweb-set-hashtb (header data)
  (gnus-sethash (nnweb-identifier (mail-header-xref header))
		data nnweb-hashtb))

(defun nnweb-get-hashtb (url)
  (gnus-gethash (nnweb-identifier url) nnweb-hashtb))

(defun nnweb-identifier (ident)
  (funcall (nnweb-definition 'identifier) ident))

(defun nnweb-overview-file (group)
  "Return the name of the overview file of GROUP."
  (nnheader-concat nnweb-directory group ".overview"))

(defun nnweb-write-active ()
  "Save the active file."
  (gnus-make-directory nnweb-directory)
  (with-temp-file (nnheader-concat nnweb-directory "active")
    (prin1 `(setq nnweb-group-alist ',nnweb-group-alist) (current-buffer))))

(defun nnweb-read-active ()
  "Read the active file."
  (load (nnheader-concat nnweb-directory "active") t t t))

(defun nnweb-definition (type &optional noerror)
  "Return the definition of TYPE."
  (let ((def (cdr (assq type (assq nnweb-type nnweb-type-definition)))))
    (when (and (not def)
	       (not noerror))
      (error "Undefined definition %s" type))
    def))

(defun nnweb-possibly-change-server (&optional group server)
  (when server
    (unless (nnweb-server-opened server)
      (nnweb-open-server server))
    (nnweb-init server))
  (unless nnweb-group-alist
    (nnweb-read-active))
  (unless nnweb-hashtb
    (setq nnweb-hashtb (gnus-make-hashtable 4095)))
  (when group
    (setq nnweb-group group)))

(defun nnweb-init (server)
  "Initialize buffers and such."
  (unless (gnus-buffer-live-p nnweb-buffer)
    (setq nnweb-buffer
	  (save-current-buffer
            (nnheader-set-temp-buffer
             (format " *nnweb %s %s %s*"
                     nnweb-type nnweb-search server))
            (mm-disable-multibyte)
            (current-buffer)))))

;;;
;;; groups.google.com
;;;

(defun nnweb-google-wash-article ()
  ;; We have Google's masked e-mail addresses here.  :-/
  (let ((case-fold-search t)
	(start-re "<pre>[\r\n ]*")
	(end-re "[\r\n ]*</pre>"))
    (goto-char (point-min))
    (if (save-excursion
	  (or (re-search-forward "The requested message.*could not be found."
				 nil t)
	      (not (and (re-search-forward start-re nil t)
			(re-search-forward end-re nil t)))))
	;; FIXME: Don't know how to indicate "not found".
	;; Should this function throw an error?  --rsteib
	(progn
	  (gnus-message 3 "Requested article not found")
	  (erase-buffer))
      (delete-region (point-min)
		     (re-search-forward start-re))
      (goto-char (point-min))
      (delete-region (progn
		       (re-search-forward end-re)
		       (match-beginning 0))
		     (point-max))
      (mm-url-decode-entities))))

(defun nnweb-google-parse-1 (&optional Message-ID)
  "Parse search result in current buffer."
  (let ((i 0)
	(case-fold-search t)
	(active (cadr (assoc nnweb-group nnweb-group-alist)))
	Subject Score Date Newsgroups From
	map url mid)
    (unless active
      (push (list nnweb-group (setq active (cons 1 0)))
	    nnweb-group-alist))
    ;; Go through all the article hits on this page.
    (goto-char (point-min))
    (while
	(re-search-forward
	 "a +href=\"/group/\\([^>\"]+\\)/browse_thread/[^>]+#\\([0-9a-f]+\\)"
	 nil t)
      (setq Newsgroups (match-string-no-properties 1)
	    ;; Note: Starting with Google Groups 2, `mid' is a Google-internal
	    ;; ID, not a proper Message-ID.
	    mid (match-string-no-properties 2)
	    url (format
		 (nnweb-definition 'result) Newsgroups mid))
      (narrow-to-region (search-forward ">" nil t)
			(search-forward "</a>" nil t))
      (mm-url-remove-markup)
      (mm-url-decode-entities)
      (setq Subject (buffer-string))
      (goto-char (point-max))
      (widen)
      (narrow-to-region (point)
			(search-forward "</table" nil t))

      (mm-url-remove-markup)
      (mm-url-decode-entities)
      (goto-char (point-max))
      (when
	  (re-search-backward
 	   "^\\(?:\\(\\w+\\) \\([0-9]+\\)\\|\\S-+\\)\\(?: \\([0-9]\\{4\\}\\)\\)? by ?\\(.*\\)"
	   nil t)
	(setq Date (if (match-string 1)
		       (format "%s %s 00:00:00 %s"
			       (match-string 1)
			       (match-string 2)
			       (or (match-string 3)
				   (substring (current-time-string) -4)))
		     (current-time-string)))
	(setq From (match-string 4)))
      (widen)
      (incf i)
      (unless (nnweb-get-hashtb url)
	(push
	 (list
	  (incf (cdr active))
	  (make-full-mail-header
	   (cdr active) (if Newsgroups
			    (concat  "(" Newsgroups ") " Subject)
			  Subject)
	   From Date (or Message-ID mid)
	   nil 0 0 url))
	 map)
	(nnweb-set-hashtb (cadar map) (car map))))
    map))

(defun nnweb-google-reference (id)
  (let ((map (nnweb-google-parse-1 id)) header)
    (setq nnweb-articles
	  (nconc nnweb-articles map))
    (when (setq header (cadar map))
      (mm-with-unibyte-current-buffer
	(mm-url-insert (mail-header-xref header)))
      (caar map))))

(defun nnweb-google-create-mapping ()
  "Perform the search and create a number-to-url alist."
  (with-current-buffer nnweb-buffer
    (erase-buffer)
    (nnheader-message 7 "Searching google...")
    (when (funcall (nnweb-definition 'search) nnweb-search)
	(let ((more t)
	      (i 0))
	  (while more
	    (setq nnweb-articles
		  (nconc nnweb-articles (nnweb-google-parse-1)))
	    ;; Check if there are more articles to fetch
	    (goto-char (point-min))
	    (incf i 100)
	    (if (or (not (re-search-forward
			  "<a [^>]+href=\"\n?\\([^>\" \n\t]+\\)[^<]*<img[^>]+src=[^>]+next"
			  nil t))
		    (>= i nnweb-max-hits))
		(setq more nil)
	      ;; Yup, there are more articles
	      (setq more (concat (nnweb-definition 'base) (match-string 1)))
	    (when more
	      (erase-buffer)
	      (nnheader-message 7 "Searching google...(%d)" i)
	      (mm-url-insert more))))
	  ;; Return the articles in the right order.
	  (nnheader-message 7 "Searching google...done")
	  (setq nnweb-articles
		(sort nnweb-articles 'car-less-than-car))))))

(defun nnweb-google-search (search)
  (mm-url-insert
   (concat
    (nnweb-definition 'address)
    "?"
    (mm-url-encode-www-form-urlencoded
     `(("q" . ,search)
       ("num" . ,(number-to-string
		  (min 100 nnweb-max-hits)))
       ("hq" . "")
       ("hl" . "en")
       ("lr" . "")
       ("safe" . "off")
       ("sites" . "groups")
       ("filter" . "0")))))
  t)

(defun nnweb-google-identity (url)
  "Return an unique identifier based on URL."
  (if (string-match "selm=\\([^ &>]+\\)" url)
      (match-string 1 url)
    url))

;;;
;;; gmane.org
;;;
(defun nnweb-gmane-create-mapping ()
  "Perform the search and create a number-to-url alist."
  (with-current-buffer nnweb-buffer
    (let ((case-fold-search t)
	  (active (or (cadr (assoc nnweb-group nnweb-group-alist))
		      (cons 1 0)))
	  map)
      (erase-buffer)
      (nnheader-message 7 "Searching Gmane..." )
      (when (funcall (nnweb-definition 'search) nnweb-search)
	(goto-char (point-min))
	;; Skip the status line
	(forward-line 1)
	;; Thanks to Olly Betts we now have NOV lines in our buffer!
	(while (not (eobp))
	  (unless (or (eolp) (looking-at "\x0d"))
	    (let ((header (nnheader-parse-nov)))
	      (let ((xref (mail-header-xref header))
		    (from (mail-header-from header))
		    (subject (mail-header-subject header))
		    (rfc2047-encoding-type 'mime))
		(when (string-match " \\([^:]+\\)[:/]\\([0-9]+\\)" xref)
		  (mail-header-set-xref
		   header
		   (format "http://article.gmane.org/%s/%s/raw"
			   (match-string 1 xref)
			   (match-string 2 xref))))

		;; Add host part to gmane-encrypted addresses
		(when (string-match "@$" from)
		  (mail-header-set-from header
					(concat from "public.gmane.org")))

		(mail-header-set-subject header
					 (rfc2047-encode-string subject))

		(unless (nnweb-get-hashtb (mail-header-xref header))
		  (mail-header-set-number header (incf (cdr active)))
		  (push (list (mail-header-number header) header) map)
		  (nnweb-set-hashtb (cadar map) (car map))))))
	  (forward-line 1)))
      (nnheader-message 7 "Searching Gmane...done")
      (setq nnweb-articles
	    (sort (nconc nnweb-articles map) 'car-less-than-car)))))

(defun nnweb-gmane-wash-article ()
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (search-forward "<!--X-Head-of-Message-->" nil t)
      (delete-region (point-min) (point))
      (goto-char (point-min))
      (while (looking-at "^<li><em>\\([^ ]+\\)</em>.*</li>")
	(replace-match "\\1\\2" t)
	(forward-line 1))
      (mm-url-remove-markup))))

(defun nnweb-gmane-search (search)
  (mm-url-insert
   (concat
    (nnweb-definition 'address)
    "?"
    (mm-url-encode-www-form-urlencoded
     `(("query" . ,search)
       ("HITSPERPAGE" . ,(number-to-string nnweb-max-hits))
       ;;("TOPDOC" . "1000")
       ))))
  (setq buffer-file-name nil)
  (unless (featurep 'xemacs) (set-buffer-multibyte t))
  (mm-decode-coding-region (point-min) (point-max) 'utf-8)
  t)

(defun nnweb-gmane-identity (url)
  "Return a unique identifier based on URL."
  (if (string-match "group=\\(.+\\)" url)
      (match-string 1 url)
    url))

;;;
;;; General web/w3 interface utility functions
;;;

(defun nnweb-insert-html (parse)
  "Insert HTML based on a w3 parse tree."
  (if (stringp parse)
      ;; We used to call nnheader-string-as-multibyte here, but it cannot
      ;; be right, so I removed it.  If a bug shows up because of this change,
      ;; please do not blindly revert the change, but help me find the real
      ;; cause of the bug instead.  --Stef
      (insert parse)
    (insert "<" (symbol-name (car parse)) " ")
    (insert (mapconcat
	     (lambda (param)
	       (concat (symbol-name (car param)) "="
		       (prin1-to-string
			(if (consp (cdr param))
			    (cadr param)
			  (cdr param)))))
	     (nth 1 parse)
	     " "))
    (insert ">\n")
    (mapc 'nnweb-insert-html (nth 2 parse))
    (insert "</" (symbol-name (car parse)) ">\n")))

(defun nnweb-parse-find (type parse &optional maxdepth)
  "Find the element of TYPE in PARSE."
  (catch 'found
    (nnweb-parse-find-1 type parse maxdepth)))

(defun nnweb-parse-find-1 (type contents maxdepth)
  (when (or (null maxdepth)
	    (not (zerop maxdepth)))
    (when (consp contents)
      (when (eq (car contents) type)
	(throw 'found contents))
      (when (listp (cdr contents))
	(dolist (element contents)
	  (when (consp element)
	    (nnweb-parse-find-1 type element
				(and maxdepth (1- maxdepth)))))))))

(defun nnweb-parse-find-all (type parse)
  "Find all elements of TYPE in PARSE."
  (catch 'found
    (nnweb-parse-find-all-1 type parse)))

(defun nnweb-parse-find-all-1 (type contents)
  (let (result)
    (when (consp contents)
      (if (eq (car contents) type)
	  (push contents result)
	(when (listp (cdr contents))
	  (dolist (element contents)
	    (when (consp element)
	      (setq result
		    (nconc result (nnweb-parse-find-all-1 type element))))))))
    result))

(defvar nnweb-text)
(defun nnweb-text (parse)
  "Return a list of text contents in PARSE."
  (let ((nnweb-text nil))
    (nnweb-text-1 parse)
    (nreverse nnweb-text)))

(defun nnweb-text-1 (contents)
  (dolist (element contents)
    (if (stringp element)
	(push element nnweb-text)
      (when (and (consp element)
		 (listp (cdr element)))
	(nnweb-text-1 element)))))

(provide 'nnweb)

;;; nnweb.el ends here
