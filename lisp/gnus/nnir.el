;;; nnir.el --- search mail with various search engines -*- coding: iso-8859-1 -*-

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Kai Groﬂjohann <grossjohann@ls6.cs.uni-dortmund.de>
;; Swish-e and Swish++ backends by:
;;   Christoph Conrad <christoph.conrad@gmx.de>.
;; IMAP backend by: Simon Josefsson <jas@pdc.kth.se>.
;; IMAP search by: Torsten Hilbrich <torsten.hilbrich <at> gmx.net>
;; IMAP search improved by Daniel Pittman  <daniel@rimspace.net>.
;; nnmaildir support for Swish++ and Namazu backends by:
;;   Justus Piater <Justus <at> Piater.name>
;; Keywords: news mail searching ir

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

;; TODO: Documentation in the Gnus manual

;; Where in the existing gnus manual would this fit best?

;; What does it do?  Well, it allows you to search your mail using
;; some search engine (imap, namazu, swish-e, gmane and others -- see
;; later) by typing `G G' in the Group buffer.  You will then get a
;; buffer which shows all articles matching the query, sorted by
;; Retrieval Status Value (score).

;; When looking at the retrieval result (in the Summary buffer) you
;; can type `A W' (aka M-x gnus-warp-to-article RET) on an article.  You
;; will be warped into the group this article came from. Typing `A T'
;; (aka M-x gnus-summary-refer-thread RET) will warp to the group and
;; also show the thread this article is part of.

;; The Lisp setup may involve setting a few variables and setting up the
;; search engine. You can define the variables in the server definition
;; like this :
;;   (setq gnus-secondary-select-methods '(
;;       (nnimap "" (nnimap-address "localhost")
;;                  (nnir-search-engine namazu)
;;       )))
;; The main variable to set is `nnir-search-engine'.  Choose one of
;; the engines listed in `nnir-engines'.  (Actually `nnir-engines' is
;; an alist, type `C-h v nnir-engines RET' for more information; this
;; includes examples for setting `nnir-search-engine', too.)

;; If you use one of the local indices (namazu, find-grep, swish) you
;; must also set up a search engine backend.

;; 1. Namazu
;;
;; The Namazu backend requires you to have one directory containing all
;; index files, this is controlled by the `nnir-namazu-index-directory'
;; variable.  To function the `nnir-namazu-remove-prefix' variable must
;; also be correct, see the documentation for `nnir-namazu-remove-prefix'
;; above.
;;
;; It is particularly important not to pass any any switches to namazu
;; that will change the output format.  Good switches to use include
;; `--sort', `--ascending', `--early' and `--late'.  Refer to the Namazu
;; documentation for further information on valid switches.
;;
;; To index my mail with the `mknmz' program I use the following
;; configuration file:
;;
;; ,----
;; | package conf;  # Don't remove this line!
;; |
;; | # Paths which will not be indexed. Don't use `^' or `$' anchors.
;; | $EXCLUDE_PATH = "spam|sent";
;; |
;; | # Header fields which should be searchable. case-insensitive
;; | $REMAIN_HEADER = "from|date|message-id|subject";
;; |
;; | # Searchable fields. case-insensitive
;; | $SEARCH_FIELD = "from|date|message-id|subject";
;; |
;; | # The max length of a word.
;; | $WORD_LENG_MAX = 128;
;; |
;; | # The max length of a field.
;; | $MAX_FIELD_LENGTH = 256;
;; `----
;;
;; My mail is stored in the directories ~/Mail/mail/, ~/Mail/lists/ and
;; ~/Mail/archive/, so to index them I go to the directory set in
;; `nnir-namazu-index-directory' and issue the following command.
;;
;;      mknmz --mailnews ~/Mail/archive/ ~/Mail/mail/ ~/Mail/lists/
;;
;; For maximum searching efficiency I have a cron job set to run this
;; command every four hours.

;; 2. find-grep
;;
;; The find-grep engine simply runs find(1) to locate eligible
;; articles and searches them with grep(1).  This, of course, is much
;; slower than using a proper search engine but OTOH doesn't require
;; maintenance of an index and is still faster than using any built-in
;; means for searching.  The method specification of the server to
;; search must include a directory for this engine to work (E.g.,
;; `nnml-directory').  The tools must be POSIX compliant.  GNU Find
;; prior to version 4.2.12 (4.2.26 on Linux due to incorrect ARG_MAX
;; handling) does not work.
;; ,----
;; |    ;; find-grep configuration for searching the Gnus Cache
;; |
;; |	(nnml "cache"
;; |          (nnml-get-new-mail nil)
;; |          (nnir-search-engine find-grep)
;; |          (nnml-directory "~/News/cache/")
;; |          (nnml-active-file "~/News/cache/active"))
;; `----

;; Developer information:

;; I have tried to make the code expandable.  Basically, it is divided
;; into two layers.  The upper layer is somewhat like the `nnvirtual'
;; backend: given a specification of what articles to show from
;; another backend, it creates a group containing exactly those
;; articles.  The lower layer issues a query to a search engine and
;; produces such a specification of what articles to show from the
;; other backend.

;; The interface between the two layers consists of the single
;; function `nnir-run-query', which just selects the appropriate
;; function for the search engine one is using.  The input to
;; `nnir-run-query' is a string, representing the query as input by
;; the user.  The output of `nnir-run-query' is supposed to be a
;; vector, each element of which should in turn be a three-element
;; vector.  The first element should be full group name of the article,
;; the second element should be the article number, and the third
;; element should be the Retrieval Status Value (RSV) as returned from
;; the search engine.  An RSV is the score assigned to the document by
;; the search engine.  For Boolean search engines, the
;; RSV is always 1000 (or 1 or 100, or whatever you like).

;; The sorting order of the articles in the summary buffer created by
;; nnir is based on the order of the articles in the above mentioned
;; vector, so that's where you can do the sorting you'd like.  Maybe
;; it would be nice to have a way of displaying the search result
;; sorted differently?

;; So what do you need to do when you want to add another search
;; engine?  You write a function that executes the query.  Temporary
;; data from the search engine can be put in `nnir-tmp-buffer'.  This
;; function should return the list of articles as a vector, as
;; described above.  Then, you need to register this backend in
;; `nnir-engines'.  Then, users can choose the backend by setting
;; `nnir-search-engine' as a server variable.

;;; Code:

;;; Setup:

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(require 'nnoo)
(require 'gnus-group)
(require 'message)
(require 'gnus-util)
(eval-when-compile
  (require 'cl))

;;; Internal Variables:

(defvar nnir-current-query nil
  "Internal: stores current query (= group name).")

(defvar nnir-current-server nil
  "Internal: stores current server (does it ever change?).")

(defvar nnir-current-group-marked nil
  "Internal: stores current list of process-marked groups.")

(defvar nnir-artlist nil
  "Internal: stores search result.")

(defvar nnir-tmp-buffer " *nnir*"
  "Internal: temporary buffer.")

(defvar nnir-search-history ()
  "Internal: the history for querying search options in nnir")

(defvar nnir-extra-parms nil
  "Internal: stores request for extra search parms")

;; Imap variables

(defvar nnir-imap-search-arguments
  '(("whole message" . "TEXT")
    ("subject" . "SUBJECT")
    ("to" . "TO")
    ("from" . "FROM")
    ("body" . "BODY")
    ("imap" . ""))
  "Mapping from user readable keys to IMAP search items for use in nnir")

(defvar nnir-imap-search-other "HEADER %S"
  "The IMAP search item to use for anything other than
  `nnir-imap-search-arguments'. By default this is the name of an
  email header field")

(defvar nnir-imap-search-argument-history ()
  "The history for querying search options in nnir")

;;; Helper macros

;; Data type article list.

(defmacro nnir-artlist-length (artlist)
  "Returns number of articles in artlist."
  `(length ,artlist))

(defmacro nnir-artlist-article (artlist n)
  "Returns from ARTLIST the Nth artitem (counting starting at 1)."
  `(when (> ,n 0)
     (elt ,artlist (1- ,n))))

(defmacro nnir-artitem-group (artitem)
  "Returns the group from the ARTITEM."
  `(elt ,artitem 0))

(defmacro nnir-artitem-number (artitem)
  "Returns the number from the ARTITEM."
  `(elt ,artitem 1))

(defmacro nnir-artitem-rsv (artitem)
  "Returns the Retrieval Status Value (RSV, score) from the ARTITEM."
  `(elt ,artitem 2))

(defmacro nnir-article-group (article)
  "Returns the group for ARTICLE"
  `(nnir-artitem-group (nnir-artlist-article nnir-artlist ,article)))

(defmacro nnir-article-number (article)
  "Returns the number for ARTICLE"
  `(nnir-artitem-number (nnir-artlist-article nnir-artlist ,article)))

(defmacro nnir-article-rsv (article)
  "Returns the rsv for ARTICLE"
  `(nnir-artitem-rsv (nnir-artlist-article nnir-artlist ,article)))

(defsubst nnir-article-ids (article)
  "Returns the pair `(nnir id . real id)' of ARTICLE"
  (cons article (nnir-article-number article)))

(defmacro nnir-categorize (sequence keyfunc &optional valuefunc)
  "Sorts a sequence into categories and returns a list of the form
`((key1 (element11 element12)) (key2 (element21 element22))'.
The category key for a member of the sequence is obtained
as `(keyfunc member)' and the corresponding element is just
`member'. If `valuefunc' is non-nil, the element of the list
is `(valuefunc member)'."
  `(unless (null ,sequence)
     (let (value)
       (mapc
	(lambda (member)
	  (let ((y (,keyfunc member))
		(x ,(if valuefunc
			`(,valuefunc member)
		      'member)))
	    (if (assoc y value)
		(push x (cadr (assoc y value)))
	      (push (list y (list x)) value))))
	,sequence)
       value)))

;;; Finish setup:

(require 'gnus-sum)

(eval-when-compile
  (autoload 'nnimap-buffer "nnimap")
  (autoload 'nnimap-command "nnimap")
  (autoload 'nnimap-possibly-change-group "nnimap")
  (autoload 'nnimap-make-thread-query "nnimap")
  (autoload 'gnus-registry-action "gnus-registry"))

(nnoo-declare nnir)
(nnoo-define-basics nnir)

(defvoo nnir-address nil
  "The address of the nnir server.")

(gnus-declare-backend "nnir" 'mail)


;;; User Customizable Variables:

(defgroup nnir nil
  "Search groups in Gnus with assorted search engines."
  :group 'gnus)

(defcustom nnir-ignored-newsgroups ""
  "*A regexp to match newsgroups in the active file that should
  be skipped when searching."
  :version "24.1"
  :type '(regexp)
  :group 'nnir)

(defcustom nnir-summary-line-format nil
  "*The format specification of the lines in an nnir summary buffer.

All the items from `gnus-summary-line-format' are available, along
with three items unique to nnir summary buffers:

%Z    Search retrieval score value (integer)
%G    Article original full group name (string)
%g    Article original short group name (string)

If nil this will use `gnus-summary-line-format'."
  :version "24.1"
  :type '(string)
  :group 'nnir)

(defcustom nnir-retrieve-headers-override-function nil
  "*If non-nil, a function that accepts an article list and group
and populates the `nntp-server-buffer' with the retrieved
headers. Must return either 'nov or 'headers indicating the
retrieved header format.

If this variable is nil, or if the provided function returns nil for a search
result, `gnus-retrieve-headers' will be called instead."
  :version "24.1"
  :type '(function)
  :group 'nnir)

(defcustom nnir-imap-default-search-key "whole message"
  "*The default IMAP search key for an nnir search. Must be one of
  the keys in `nnir-imap-search-arguments'. To use raw imap queries
  by default set this to \"Imap\"."
  :version "24.1"
  :type `(choice ,@(mapcar (lambda (elem) (list 'const (car elem)))
			   nnir-imap-search-arguments))
  :group 'nnir)

(defcustom nnir-swish++-configuration-file
  (expand-file-name "~/Mail/swish++.conf")
  "*Configuration file for swish++."
  :type '(file)
  :group 'nnir)

(defcustom nnir-swish++-program "search"
  "*Name of swish++ search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish++-additional-switches '()
  "*A list of strings, to be given as additional arguments to swish++.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-swish++-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish++-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish++-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by swish++
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-namazu-remove-prefix', except
that it is for swish++, not Namazu."
  :type '(regexp)
  :group 'nnir)

;; Swish-E.
;; URL: http://swish-e.org/
;; Variables `nnir-swish-e-index-file', `nnir-swish-e-program' and
;; `nnir-swish-e-additional-switches'

(make-obsolete-variable 'nnir-swish-e-index-file
			'nnir-swish-e-index-files "Emacs 23.1")
(defcustom nnir-swish-e-index-file
  (expand-file-name "~/Mail/index.swish-e")
  "*Index file for swish-e.
This could be a server parameter.
It is never consulted once `nnir-swish-e-index-files', which should be
used instead, has been customized."
  :type '(file)
  :group 'nnir)

(defcustom nnir-swish-e-index-files
  (list nnir-swish-e-index-file)
  "*List of index files for swish-e.
This could be a server parameter."
  :type '(repeat (file))
  :group 'nnir)

(defcustom nnir-swish-e-program "swish-e"
  "*Name of swish-e search executable.
This cannot be a server parameter."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish-e-additional-switches '()
  "*A list of strings, to be given as additional arguments to swish-e.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-swish-e-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish-e-additional-switches '(\"-i\" \"-w\"))

This could be a server parameter."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish-e-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by swish-e
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-namazu-remove-prefix', except
that it is for swish-e, not Namazu.

This could be a server parameter."
  :type '(regexp)
  :group 'nnir)

;; HyREX engine, see <URL:http://ls6-www.cs.uni-dortmund.de/>

(defcustom nnir-hyrex-program "nnir-search"
  "*Name of the nnir-search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-hyrex-additional-switches '()
  "*A list of strings, to be given as additional arguments for nnir-search.
Note that this should be a list. Ie, do NOT use the following:
    (setq nnir-hyrex-additional-switches \"-ddl ddl.xml -c nnir\") ; wrong !
Instead, use this:
    (setq nnir-hyrex-additional-switches '(\"-ddl\" \"ddl.xml\" \"-c\" \"nnir\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-hyrex-index-directory (getenv "HOME")
  "*Index directory for HyREX."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-hyrex-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by HyREX
in order to get a group name (albeit with / instead of .).

For example, suppose that HyREX returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-hyrex-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory)
  :group 'nnir)

;; Namazu engine, see <URL:http://www.namazu.org/>

(defcustom nnir-namazu-program "namazu"
  "*Name of Namazu search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-namazu-index-directory (expand-file-name "~/Mail/namazu/")
  "*Index directory for Namazu."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-namazu-additional-switches '()
  "*A list of strings, to be given as additional arguments to namazu.
The switches `-q', `-a', and `-s' are always used, very few other switches
make any sense in this context.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-namazu-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-namazu-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-namazu-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by Namazu
in order to get a group name (albeit with / instead of .).

For example, suppose that Namazu returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-namazu-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-notmuch-program "notmuch"
  "*Name of notmuch search executable."
  :version "24.1"
  :type '(string)
  :group 'nnir)

(defcustom nnir-notmuch-additional-switches '()
  "*A list of strings, to be given as additional arguments to notmuch.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-notmuch-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-notmuch-additional-switches '(\"-i\" \"-w\"))"
  :version "24.1"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-notmuch-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by notmuch
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-namazu-remove-prefix', except
that it is for notmuch, not Namazu."
  :version "24.1"
  :type '(regexp)
  :group 'nnir)

;;; Developer Extension Variable:

(defvar nnir-engines
  `((imap    nnir-run-imap
             ((criteria
	       "Imap Search in"                   ; Prompt
	       ,(mapcar 'car nnir-imap-search-arguments) ; alist for completing
	       nil                                ; allow any user input
	       nil                                ; initial value
	       nnir-imap-search-argument-history  ; the history to use
	       ,nnir-imap-default-search-key      ; default
	       )))
    (gmane   nnir-run-gmane
	     ((author . "Gmane Author: ")))
    (swish++ nnir-run-swish++
             ((group . "Swish++ Group spec: ")))
    (swish-e nnir-run-swish-e
             ((group . "Swish-e Group spec: ")))
    (namazu  nnir-run-namazu
             ())
    (notmuch nnir-run-notmuch
             ())
    (hyrex   nnir-run-hyrex
	     ((group . "Hyrex Group spec: ")))
    (find-grep nnir-run-find-grep
	       ((grep-options . "Grep options: "))))
  "Alist of supported search engines.
Each element in the alist is a three-element list (ENGINE FUNCTION ARGS).
ENGINE is a symbol designating the searching engine.  FUNCTION is also
a symbol, giving the function that does the search.  The third element
ARGS is a list of cons pairs (PARAM . PROMPT).  When issuing a query,
the FUNCTION will issue a query for each of the PARAMs, using PROMPT.

The value of `nnir-search-engine' must be one of the ENGINE symbols.
For example, for searching a server using namazu include
    (nnir-search-engine namazu)
in the server definition.  Note that you have to set additional
variables for most backends.  For example, the `namazu' backend
needs the variables `nnir-namazu-program',
`nnir-namazu-index-directory' and `nnir-namazu-remove-prefix'.

Add an entry here when adding a new search engine.")

(defcustom nnir-method-default-engines
  '((nnimap . imap)
    (nntp . gmane))
  "*Alist of default search engines keyed by server method."
  :version "24.1"
  :type `(repeat (cons (choice (const nnimap) (const nttp) (const nnspool)
			       (const nneething) (const nndir) (const nnmbox)
			       (const nnml) (const nnmh) (const nndraft)
			       (const nnfolder) (const nnmaildir))
		       (choice
			,@(mapcar (lambda (elem) (list 'const (car elem)))
				  nnir-engines))))
  :group 'nnir)

;; Gnus glue.

(defun gnus-group-make-nnir-group (nnir-extra-parms &optional parms)
  "Create an nnir group.  Asks for query."
  (interactive "P")
  (setq nnir-current-query nil
	nnir-current-server nil
	nnir-current-group-marked nil
	nnir-artlist nil)
  (let* ((query (unless parms (read-string "Query: " nil 'nnir-search-history)))
	 (parms (or parms (list (cons 'query query))))
	 (srv (or (cdr (assq 'server parms)) (gnus-server-server-name) "nnir")))
   (add-to-list 'parms (cons 'unique-id (message-unique-id)) t)
    (gnus-group-read-ephemeral-group
     (concat "nnir:" (prin1-to-string parms)) (list 'nnir srv) t
     (cons (current-buffer) gnus-current-window-configuration)
     nil)))


;; Gnus backend interface functions.

(deffoo nnir-open-server (server &optional definitions)
  ;; Just set the server variables appropriately.
  (add-hook 'gnus-summary-mode-hook 'nnir-mode)
  (nnoo-change-server 'nnir server definitions))

(deffoo nnir-request-group (group &optional server fast info)
  "GROUP is the query string."
  (nnir-possibly-change-server server)
  ;; Check for cache and return that if appropriate.
  (if (and (equal group nnir-current-query)
           (equal gnus-group-marked nnir-current-group-marked)
           (or (null server)
               (equal server nnir-current-server)))
      nnir-artlist
    ;; Cache miss.
    (setq nnir-artlist (nnir-run-query group)))
  (with-current-buffer nntp-server-buffer
    (setq nnir-current-query group)
    (when server (setq nnir-current-server server))
    (setq nnir-current-group-marked gnus-group-marked)
    (if (zerop (length nnir-artlist))
	(nnheader-report 'nnir "Search produced empty results.")
      ;; Remember data for cache.
      (nnheader-insert "211 %d %d %d %s\n"
		       (nnir-artlist-length nnir-artlist) ; total #
		       1              ; first #
		       (nnir-artlist-length nnir-artlist) ; last #
		       group))))      ; group name

(deffoo nnir-retrieve-headers (articles &optional group server fetch-old)
  (with-current-buffer nntp-server-buffer
    (let ((gnus-inhibit-demon t)
	  (articles-by-group (nnir-categorize
			      articles nnir-article-group nnir-article-ids))
	  headers)
      (while (not (null articles-by-group))
	(let* ((group-articles (pop articles-by-group))
	       (artgroup (car group-articles))
	       (articleids (cadr group-articles))
	       (artlist (sort (mapcar 'cdr articleids) '<))
	       (server (gnus-group-server artgroup))
	       (gnus-override-method (gnus-server-to-method server))
	       parsefunc)
	  ;; (or (numberp art)
	  ;;     (nnheader-report
	  ;;      'nnir
	  ;;      "nnir-retrieve-headers doesn't grok message ids: %s"
	  ;;      art))
	  (nnir-possibly-change-server server)
	  ;; is this needed?
	  (erase-buffer)
	  (case (setq gnus-headers-retrieved-by
		      (or
		       (and
			nnir-retrieve-headers-override-function
			(funcall nnir-retrieve-headers-override-function
				 artlist artgroup))
		       (gnus-retrieve-headers artlist artgroup nil)))
	    (nov
	     (setq parsefunc 'nnheader-parse-nov))
	    (headers
	     (setq parsefunc 'nnheader-parse-head))
	    (t (error "Unknown header type %s while requesting articles \
                    of group %s" gnus-headers-retrieved-by artgroup)))
	  (goto-char (point-min))
	  (while (not (eobp))
	    (let* ((novitem (funcall parsefunc))
		   (artno (and novitem
			       (mail-header-number novitem)))
		   (art (car (rassq artno articleids))))
	      (when art
		(mail-header-set-number novitem art)
		(push novitem headers))
	      (forward-line 1)))))
      (setq headers
	    (sort headers
		  (lambda (x y)
		    (< (mail-header-number x) (mail-header-number y)))))
      (erase-buffer)
      (mapc 'nnheader-insert-nov headers)
      'nov)))

(deffoo nnir-request-article (article &optional group server to-buffer)
  (if (and (stringp article)
	   (not (eq 'nnimap (car (gnus-server-to-method server)))))
      (nnheader-report
       'nnir
       "nnir-request-article only groks message ids for nnimap servers: %s"
       server)
    (save-excursion
      (let ((article article)
	    query)
	(when (stringp article)
	  (setq gnus-override-method (gnus-server-to-method server))
	  (setq query
		(list
		 (cons 'query (format "HEADER Message-ID %s" article))
		 (cons 'unique-id article)
		 (cons 'criteria "")
		 (cons 'shortcut t)))
	  (unless (and (equal query nnir-current-query)
		       (equal server nnir-current-server))
	    (setq nnir-artlist (nnir-run-imap query server))
	    (setq nnir-current-query query)
	    (setq nnir-current-server server))
	  (setq article 1))
	(unless (zerop (length nnir-artlist))
	  (let ((artfullgroup (nnir-article-group article))
		(artno (nnir-article-number article)))
	    (message "Requesting article %d from group %s"
		     artno artfullgroup)
	    (if to-buffer
		(with-current-buffer to-buffer
		  (let ((gnus-article-decode-hook nil))
		    (gnus-request-article-this-buffer artno artfullgroup)))
	      (gnus-request-article artno artfullgroup))
	    (cons artfullgroup artno)))))))

(deffoo nnir-request-move-article (article group server accept-form
					   &optional last internal-move-group)
  (let* ((artfullgroup (nnir-article-group article))
	 (artno (nnir-article-number article))
	 (to-newsgroup (nth 1 accept-form))
	 (to-method (gnus-find-method-for-group to-newsgroup))
	 (from-method (gnus-find-method-for-group artfullgroup))
	 (move-is-internal (gnus-server-equal from-method to-method)))
    (unless (gnus-check-backend-function
	     'request-move-article artfullgroup)
      (error "The group %s does not support article moving" artfullgroup))
    (gnus-request-move-article
     artno
     artfullgroup
     (nth 1 from-method)
     accept-form
     last
     (and move-is-internal
	  to-newsgroup		; Not respooling
	  (gnus-group-real-name to-newsgroup)))))

(deffoo nnir-request-expire-articles (articles group &optional server force)
  (if force
    (let ((articles-by-group (nnir-categorize
			      articles nnir-article-group nnir-article-ids))
	  not-deleted)
      (while (not (null articles-by-group))
	(let* ((group-articles (pop articles-by-group))
	       (artgroup (car group-articles))
	       (articleids (cadr group-articles))
	       (artlist (sort (mapcar 'cdr articleids) '<)))
	  (unless (gnus-check-backend-function 'request-expire-articles
					       artgroup)
	    (error "The group %s does not support article deletion" artgroup))
	  (unless (gnus-check-server (gnus-find-method-for-group artgroup))
	    (error "Couldn't open server for group %s" artgroup))
	  (push (gnus-request-expire-articles
		 artlist artgroup force)
		not-deleted)))
      (sort (delq nil not-deleted) '<))
    articles))

(deffoo nnir-warp-to-article ()
  (let* ((cur (if (> (gnus-summary-article-number) 0)
		  (gnus-summary-article-number)
		(error "This is not a real article")))
	 (backend-article-group (nnir-article-group cur))
         (backend-article-number (nnir-article-number cur))
	 (quit-config (gnus-ephemeral-group-p gnus-newsgroup-name)))
    ;; first exit from the nnir summary buffer.
    (gnus-summary-exit)
    ;; and if the nnir summary buffer in turn came from another
    ;; summary buffer we have to clean that summary up too.
    (when (eq (cdr quit-config) 'summary)
      (gnus-summary-exit))
    (gnus-summary-read-group-1 backend-article-group t t  nil
			       nil (list backend-article-number))))

(nnoo-define-skeleton nnir)


(defmacro nnir-add-result (dirnam artno score prefix server artlist)
  "Ask `nnir-compose-result' to construct a result vector,
and if it is non-nil, add it to artlist."
  `(let ((result (nnir-compose-result ,dirnam ,artno ,score ,prefix ,server)))
     (when (not (null result))
       (push result ,artlist))))

(autoload 'nnmaildir-base-name-to-article-number "nnmaildir")

;; Helper function currently used by the Swish++ and Namazu backends;
;; perhaps useful for other backends as well
(defun nnir-compose-result (dirnam article score prefix server)
  "Extract the group from dirnam, and create a result vector
ready to be added to the list of search results."

  ;; remove nnir-*-remove-prefix from beginning of dirnam filename
  (when (string-match (concat "^" prefix) dirnam)
    (setq dirnam (replace-match "" t t dirnam)))

  (when (file-readable-p (concat prefix dirnam article))
    ;; remove trailing slash and, for nnmaildir, cur/new/tmp
    (setq dirnam
	  (substring dirnam 0
		     (if (string-match "^nnmaildir:" (gnus-group-server server))
			 -5 -1)))

    ;; Set group to dirnam without any leading dots or slashes,
    ;; and with all subsequent slashes replaced by dots
    (let ((group (gnus-replace-in-string
                 (gnus-replace-in-string dirnam "^[./\\]" "" t)
                 "[/\\]" "." t)))

    (vector (gnus-group-full-name group server)
	    (if (string-match "^nnmaildir:" (gnus-group-server server))
		(nnmaildir-base-name-to-article-number
		 (substring article 0 (string-match ":" article))
		 group nil)
	      (string-to-number article))
	    (string-to-number score)))))

;;; Search Engine Interfaces:

;; imap interface
(defun nnir-run-imap (query srv &optional groups)
  "Run a search against an IMAP back-end server.
This uses a custom query language parser; see `nnir-imap-make-query' for
details on the language and supported extensions."
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
          (server (cadr (gnus-server-to-method srv)))
          (defs (caddr (gnus-server-to-method srv)))
          (criteria (or (cdr (assq 'criteria query))
                        (cdr (assoc nnir-imap-default-search-key
                                    nnir-imap-search-arguments))))
          (gnus-inhibit-demon t)
	  (groups (or groups (nnir-get-active srv))))
      (message "Opening server %s" server)
      (apply
       'vconcat
       (catch 'found
	 (mapcar
	  (lambda (group)
	    (let (artlist)
	      (condition-case ()
		  (when (nnimap-possibly-change-group
			 (gnus-group-short-name group) server)
		    (with-current-buffer (nnimap-buffer)
		      (message "Searching %s..." group)
		      (let ((arts 0)
			    (result (nnimap-command "UID SEARCH %s"
						    (if (string= criteria "")
							qstring
						      (nnir-imap-make-query
						       criteria qstring)))))
			(mapc
			 (lambda (artnum)
			   (let ((artn (string-to-number artnum)))
			     (when (> artn 0)
			       (push (vector group artn 100)
				     artlist)
			       (when (assq 'shortcut query)
				 (throw 'found (list artlist)))
			       (setq arts (1+ arts)))))
			 (and (car result) (cdr (assoc "SEARCH" (cdr result)))))
			(message "Searching %s... %d matches" group arts)))
		    (message "Searching %s...done" group))
		(quit nil))
	      (nreverse artlist)))
	  groups))))))

(defun nnir-imap-make-query (criteria qstring)
  "Parse the query string and criteria into an appropriate IMAP search
expression, returning the string query to make.

This implements a little language designed to return the expected results
to an arbitrary query string to the end user.

The search is always case-insensitive, as defined by RFC2060, and supports
the following features (inspired by the Google search input language):

Automatic \"and\" queries
    If you specify multiple words then they will be treated as an \"and\"
    expression intended to match all components.

Phrase searches
    If you wrap your query in double-quotes then it will be treated as a
    literal string.

Negative terms
    If you precede a term with \"-\" then it will negate that.

\"OR\" queries
    If you include an upper-case \"OR\" in your search it will cause the
    term before it and the term after it to be treated as alternatives.

In future the following will be added to the language:
 * support for date matches
 * support for location of text matching within the query
 * from/to/etc headers
 * additional search terms
 * flag based searching
 * anything else that the RFC supports, basically."
  ;; Walk through the query and turn it into an IMAP query string.
  (nnir-imap-query-to-imap criteria (nnir-imap-parse-query qstring)))


(defun nnir-imap-query-to-imap (criteria query)
  "Turn a s-expression format query into IMAP."
  (mapconcat
   ;; Turn the expressions into IMAP text
   (lambda (item)
     (nnir-imap-expr-to-imap criteria item))
   ;; The query, already in s-expr format.
   query
   ;; Append a space between each expression
   " "))


(defun nnir-imap-expr-to-imap (criteria expr)
  "Convert EXPR into an IMAP search expression on CRITERIA"
  ;; What sort of expression is this, eh?
  (cond
   ;; Simple string term
   ((stringp expr)
    (format "%s %S" criteria expr))
   ;; Trivial term: and
   ((eq expr 'and) nil)
   ;; Composite term: or expression
   ((eq (car-safe expr) 'or)
    (format "OR %s %s"
	    (nnir-imap-expr-to-imap criteria (second expr))
	    (nnir-imap-expr-to-imap criteria (third expr))))
   ;; Composite term: just the fax, mam
   ((eq (car-safe expr) 'not)
    (format "NOT (%s)" (nnir-imap-query-to-imap criteria (rest expr))))
   ;; Composite term: just expand it all.
   ((and (not (null expr)) (listp expr))
    (format "(%s)" (nnir-imap-query-to-imap criteria expr)))
   ;; Complex value, give up for now.
   (t (error "Unhandled input: %S" expr))))


(defun nnir-imap-parse-query (string)
  "Turn STRING into an s-expression based query based on the IMAP
query language as defined in `nnir-imap-make-query'.

This involves turning individual tokens into higher level terms
that the search language can then understand and use."
  (with-temp-buffer
    ;; Set up the parsing environment.
    (insert string)
    (goto-char (point-min))
    ;; Now, collect the output terms and return them.
    (let (out)
      (while (not (nnir-imap-end-of-input))
	(push (nnir-imap-next-expr) out))
      (reverse out))))


(defun nnir-imap-next-expr (&optional count)
  "Return the next expression from the current buffer."
  (let ((term (nnir-imap-next-term count))
	(next (nnir-imap-peek-symbol)))
    ;; Are we looking at an 'or' expression?
    (cond
     ;; Handle 'expr or expr'
     ((eq next 'or)
      (list 'or term (nnir-imap-next-expr 2)))
     ;; Anything else
     (t term))))


(defun nnir-imap-next-term (&optional count)
  "Return the next TERM from the current buffer."
  (let ((term (nnir-imap-next-symbol count)))
    ;; What sort of term is this?
    (cond
     ;; and -- just ignore it
     ((eq term 'and) 'and)
     ;; negated term
     ((eq term 'not) (list 'not (nnir-imap-next-expr)))
     ;; generic term
     (t term))))


(defun nnir-imap-peek-symbol ()
  "Return the next symbol from the current buffer, but don't consume it."
  (save-excursion
    (nnir-imap-next-symbol)))

(defun nnir-imap-next-symbol (&optional count)
  "Return the next symbol from the current buffer, or nil if we are
at the end of the buffer.  If supplied COUNT skips some symbols before
returning the one at the supplied position."
  (when (and (numberp count) (> count 1))
    (nnir-imap-next-symbol (1- count)))
  (let ((case-fold-search t))
    ;; end of input stream?
    (unless (nnir-imap-end-of-input)
      ;; No, return the next symbol from the stream.
      (cond
       ;; negated expression -- return it and advance one char.
       ((looking-at "-") (forward-char 1) 'not)
       ;; quoted string
       ((looking-at "\"") (nnir-imap-delimited-string "\""))
       ;; list expression -- we parse the content and return this as a list.
       ((looking-at "(")
	(nnir-imap-parse-query (nnir-imap-delimited-string ")")))
       ;; keyword input -- return a symbol version
       ((looking-at "\\band\\b") (forward-char 3) 'and)
       ((looking-at "\\bor\\b")  (forward-char 2) 'or)
       ((looking-at "\\bnot\\b") (forward-char 3) 'not)
       ;; Simple, boring keyword
       (t (let ((start (point))
		(end (if (search-forward-regexp "[[:blank:]]" nil t)
			 (prog1
			     (match-beginning 0)
			   ;; unskip if we hit a non-blank terminal character.
			   (when (string-match "[^[:blank:]]" (match-string 0))
			     (backward-char 1)))
		       (goto-char (point-max)))))
	    (buffer-substring start end)))))))

(defun nnir-imap-delimited-string (delimiter)
  "Return a delimited string from the current buffer."
  (let ((start (point)) end)
    (forward-char 1)			; skip the first delimiter.
    (while (not end)
      (unless (search-forward delimiter nil t)
	(error "Unmatched delimited input with %s in query" delimiter))
      (let ((here (point)))
	(unless (equal (buffer-substring (- here 2) (- here 1)) "\\")
	  (setq end (point)))))
    (buffer-substring (1+ start) (1- end))))

(defun nnir-imap-end-of-input ()
  "Are we at the end of input?"
  (skip-chars-forward "[[:blank:]]")
  (looking-at "$"))


;; Swish++ interface.
;; -cc- Todo
;; Search by
;; - group
;; Sort by
;; - rank (default)
;; - article number
;; - file size
;; - group
(defun nnir-run-swish++ (query server &optional group)
  "Run QUERY against swish++.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish++ 4.7 on GNU/Linux and with swish++ 5.0b2 on
Windows NT 4.0."

  ;; (when group
  ;;   (error "The swish++ backend cannot search specific groups"))

  (save-excursion
    (let ( (qstring (cdr (assq 'query query)))
	   (groupspec (cdr (assq 'group query)))
	   (prefix (nnir-read-server-parm 'nnir-swish++-remove-prefix server))
           artlist
	   ;; nnml-use-compressed-files might be any string, but probably this
	   ;; is sufficient.  Note that we can't only use the value of
	   ;; nnml-use-compressed-files because old articles might have been
	   ;; saved with a different value.
	   (article-pattern (if (string-match "^nnmaildir:"
					      (gnus-group-server server))
				":[0-9]+"
			      "^[0-9]+\\(\\.[a-z0-9]+\\)?$"))
           score artno dirnam filenam)

      (when (equal "" qstring)
        (error "swish++: You didn't enter anything"))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (if groupspec
          (message "Doing swish++ query %s on %s..." qstring groupspec)
        (message "Doing swish++ query %s..." qstring))

      (let* ((cp-list `( ,nnir-swish++-program
                         nil            ; input from /dev/null
                         t              ; output
                         nil            ; don't redisplay
                         "--config-file" ,(nnir-read-server-parm 'nnir-swish++-configuration-file server)
                         ,@(nnir-read-server-parm 'nnir-swish++-additional-switches server)
                         ,qstring       ; the query, in swish++ format
                         ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-swish++-program
                         (mapconcat 'identity (cddddr cp-list) " ")) ;; ???
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run swish++: %s" exitstatus)
          ;; swish++ failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; V 4.7 Linux
      ;; rank relative-path-name file-size file-title
      ;; V 5.0b2:
      ;; rank relative-path-name file-size topic??
      ;; where rank is an integer from 1 to 100.
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[0-9]+\\) \\([^ ]+\\) [0-9]+ \\(.*\\)$" nil t)
        (setq score (match-string 1)
	      filenam (match-string 2)
              artno (file-name-nondirectory filenam)
              dirnam (file-name-directory filenam))

        ;; don't match directories
        (when (string-match article-pattern artno)
          (when (not (null dirnam))

	    ;; maybe limit results to matching groups.
	    (when (or (not groupspec)
		      (string-match groupspec dirnam))
	      (nnir-add-result dirnam artno score prefix server artlist)))))

      (message "Massaging swish++ output...done")

      ;; Sort by score
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

;; Swish-E interface.
(defun nnir-run-swish-e (query server &optional group)
  "Run given query against swish-e.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish-e-2.0.1 on Windows NT 4.0."

  ;; swish-e crashes with empty parameter to "-w" on commandline...
  ;; (when group
  ;;   (error "The swish-e backend cannot search specific groups"))

  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (prefix
	   (or (nnir-read-server-parm 'nnir-swish-e-remove-prefix server)
	       (error "Missing parameter `nnir-swish-e-remove-prefix'")))
          artlist score artno dirnam group )

      (when (equal "" qstring)
        (error "swish-e: You didn't enter anything"))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (message "Doing swish-e query %s..." query)
      (let* ((index-files
	      (or (nnir-read-server-parm
		   'nnir-swish-e-index-files server)
		  (error "Missing parameter `nnir-swish-e-index-files'")))
	     (additional-switches
	      (nnir-read-server-parm
	       'nnir-swish-e-additional-switches server))
	     (cp-list `(,nnir-swish-e-program
			nil		; input from /dev/null
			t		; output
			nil		; don't redisplay
			"-f" ,@index-files
			,@additional-switches
			"-w"
			,qstring	; the query, in swish-e format
			))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-swish-e-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run swish-e: %s" exitstatus)
          ;; swish-e failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; rank path-name file-title file-size
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[0-9]+\\) \\([^ ]+\\) \"\\([^\"]+\\)\" [0-9]+$" nil t)
        (setq score (match-string 1)
              artno (match-string 3)
              dirnam (file-name-directory (match-string 2)))

        ;; don't match directories
        (when (string-match "^[0-9]+$" artno)
          (when (not (null dirnam))

	    ;; remove nnir-swish-e-remove-prefix from beginning of dirname
            (when (string-match (concat "^" prefix) dirnam)
              (setq dirnam (replace-match "" t t dirnam)))

            (setq dirnam (substring dirnam 0 -1))
	    ;; eliminate all ".", "/", "\" from beginning. Always matches.
            (string-match "^[./\\]*\\(.*\\)$" dirnam)
            ;; "/" -> "."
            (setq group (gnus-replace-in-string (match-string 1 dirnam) "/" "."))
            ;; Windows "\\" -> "."
            (setq group (gnus-replace-in-string group "\\\\" "."))

            (push (vector (gnus-group-full-name group server)
                          (string-to-number artno)
                          (string-to-number score))
                  artlist))))

      (message "Massaging swish-e output...done")

      ;; Sort by score
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

;; HyREX interface
(defun nnir-run-hyrex (query server &optional group)
  (save-excursion
    (let ((artlist nil)
          (groupspec (cdr (assq 'group query)))
          (qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-hyrex-remove-prefix server))
	  score artno dirnam)
      (when (and (not groupspec) group)
        (setq groupspec
	      (regexp-opt
	       (mapcar (lambda (x) (gnus-group-real-name x)) group))))
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (message "Doing hyrex-search query %s..." query)
      (let* ((cp-list
	      `( ,nnir-hyrex-program
		 nil			; input from /dev/null
		 t			; output
		 nil			; don't redisplay
		 "-i",(nnir-read-server-parm 'nnir-hyrex-index-directory server) ; index directory
		 ,@(nnir-read-server-parm 'nnir-hyrex-additional-switches server)
		 ,qstring	   ; the query, in hyrex-search format
		 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-hyrex-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run hyrex-search: %s" exitstatus)
          ;; nnir-search failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer)))) ;; FIXME: Don't clear buffer !
      (message "Doing hyrex-search query \"%s\"...done" qstring)
      (sit-for 0)
      ;; nnir-search returns:
      ;;   for nnml/nnfolder: "filename mailid weight"
      ;;   for nnimap:        "group mailid weight"
      (goto-char (point-min))
      (delete-non-matching-lines "^\\S + [0-9]+ [0-9]+$")
      ;; HyREX doesn't search directly in groups -- so filter out here.
      (when groupspec
	(keep-lines groupspec))
      ;; extract data from result lines
      (goto-char (point-min))
      (while (re-search-forward
	      "\\(\\S +\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
	(setq dirnam (match-string 1)
	      artno (match-string 2)
	      score (match-string 3))
	(when (string-match prefix dirnam)
	  (setq dirnam (replace-match "" t t dirnam)))
	(push (vector (gnus-group-full-name
                       (gnus-replace-in-string dirnam "/" ".") server)
		      (string-to-number artno)
		      (string-to-number score))
	      artlist))
      (message "Massaging hyrex-search output...done.")
      (apply 'vector
	     (sort artlist
                   (function (lambda (x y)
                               (if (string-lessp (nnir-artitem-group x)
                                                 (nnir-artitem-group y))
                                   t
                                 (< (nnir-artitem-number x)
                                    (nnir-artitem-number y)))))))
      )))

;; Namazu interface
(defun nnir-run-namazu (query server &optional group)
  "Run given query against Namazu.  Returns a vector of (group name, file name)
pairs (also vectors, actually).

Tested with Namazu 2.0.6 on a GNU/Linux system."
  ;; (when group
  ;;   (error "The Namazu backend cannot search specific groups"))
  (save-excursion
    (let ((article-pattern (if (string-match "^nnmaildir:"
					     (gnus-group-server server))
			       ":[0-9]+"
			     "^[0-9]+$"))
          artlist
	  (qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-namazu-remove-prefix server))
          score group article
          (process-environment (copy-sequence process-environment)))
      (setenv "LC_MESSAGES" "C")
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (let* ((cp-list
              `( ,nnir-namazu-program
                 nil			; input from /dev/null
                 t			; output
                 nil			; don't redisplay
                 "-q"			; don't be verbose
                 "-a"			; show all matches
                 "-s"			; use short format
                 ,@(nnir-read-server-parm 'nnir-namazu-additional-switches server)
                 ,qstring		; the query, in namazu format
                 ,(nnir-read-server-parm 'nnir-namazu-index-directory server) ; index directory
                 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-namazu-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run namazu: %s" exitstatus)
          ;; Namazu failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; Namazu output looks something like this:
      ;; 2. Re: Gnus agent expire broken (score: 55)
      ;; /home/henrik/Mail/mail/sent/1310 (4,138 bytes)

      (goto-char (point-min))
      (while (re-search-forward
              "^\\([0-9]+\\.\\).*\\((score: \\([0-9]+\\)\\))\n\\([^ ]+\\)"
              nil t)
        (setq score (match-string 3)
              group (file-name-directory (match-string 4))
              article (file-name-nondirectory (match-string 4)))

        ;; make sure article and group is sane
        (when (and (string-match article-pattern article)
                   (not (null group)))
	  (nnir-add-result group article score prefix server artlist)))

      ;; sort artlist by score
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

(defun nnir-run-notmuch (query server &optional group)
  "Run QUERY against notmuch.
Returns a vector of (group name, file name) pairs (also vectors,
actually)."

  ;; (when group
  ;;   (error "The notmuch backend cannot search specific groups"))

  (save-excursion
    (let ( (qstring (cdr (assq 'query query)))
	   (groupspec (cdr (assq 'group query)))
	   (prefix (nnir-read-server-parm 'nnir-notmuch-remove-prefix server))
           artlist
	   (article-pattern (if (string-match "^nnmaildir:"
					      (gnus-group-server server))
			       ":[0-9]+"
			     "^[0-9]+$"))
           artno dirnam filenam)

      (when (equal "" qstring)
        (error "notmuch: You didn't enter anything"))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (if groupspec
          (message "Doing notmuch query %s on %s..." qstring groupspec)
        (message "Doing notmuch query %s..." qstring))

      (let* ((cp-list `( ,nnir-notmuch-program
                         nil            ; input from /dev/null
                         t              ; output
                         nil            ; don't redisplay
                         "search"
                         "--format=text"
                         "--output=files"
                         ,@(nnir-read-server-parm 'nnir-notmuch-additional-switches server)
                         ,qstring       ; the query, in notmuch format
                         ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-notmuch-program
                         (mapconcat 'identity (cddddr cp-list) " ")) ;; ???
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run notmuch: %s" exitstatus)
          ;; notmuch failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; absolute-path-name
      (goto-char (point-min))
      (while (not (eobp))
        (setq filenam (buffer-substring-no-properties (line-beginning-position)
                                                      (line-end-position))
              artno (file-name-nondirectory filenam)
              dirnam (file-name-directory filenam))
        (forward-line 1)

        ;; don't match directories
        (when (string-match article-pattern artno)
          (when (not (null dirnam))

	    ;; maybe limit results to matching groups.
	    (when (or (not groupspec)
		      (string-match groupspec dirnam))
	      (nnir-add-result dirnam artno "" prefix server artlist)))))

      (message "Massaging notmuch output...done")

      artlist)))

(defun nnir-run-find-grep (query server &optional grouplist)
  "Run find and grep to obtain matching articles."
  (let* ((method (gnus-server-to-method server))
	 (sym (intern
	       (concat (symbol-name (car method)) "-directory")))
	 (directory (cadr (assoc sym (cddr method))))
	 (regexp (cdr (assoc 'query query)))
	 (grep-options (cdr (assoc 'grep-options query)))
	 (grouplist (or grouplist (nnir-get-active server)))
	 artlist)
    (unless directory
      (error "No directory found in method specification of server %s"
	     server))
    (apply
     'vconcat
     (mapcar (lambda (x)
	       (let ((group x))
		 (message "Searching %s using find-grep..."
			  (or group server))
		 (save-window-excursion
		   (set-buffer (get-buffer-create nnir-tmp-buffer))
		   (erase-buffer)
		   (if (> gnus-verbose 6)
		       (pop-to-buffer (current-buffer)))
		   (cd directory) ; Using relative paths simplifies
				  ; postprocessing.
		   (let ((group
			  (if (not group)
			      "."
			    ;; Try accessing the group literally as
			    ;; well as interpreting dots as directory
			    ;; separators so the engine works with
			    ;; plain nnml as well as the Gnus Cache.
			    (let ((group (gnus-group-real-name group)))
			      ;; Replace cl-func find-if.
			      (if (file-directory-p group)
				  group
				(if (file-directory-p
				     (setq group
					   (gnus-replace-in-string
					    group
					    "\\." "/" t)))
				    group))))))
		     (unless group
		       (error "Cannot locate directory for group"))
		     (save-excursion
		       (apply
			'call-process "find" nil t
			"find" group "-type" "f" "-name" "[0-9]*" "-exec"
			"grep"
			`("-l" ,@(and grep-options
				      (split-string grep-options "\\s-" t))
			  "-e" ,regexp "{}" "+"))))

		   ;; Translate relative paths to group names.
		   (while (not (eobp))
		     (let* ((path (split-string
				   (buffer-substring
				    (point)
				    (line-end-position)) "/" t))
			    (art (string-to-number (car (last path)))))
		       (while (string= "." (car path))
			 (setq path (cdr path)))
		       (let ((group (mapconcat 'identity
					       ;; Replace cl-func:
					       ;; (subseq path 0 -1)
					       (let ((end (1- (length path)))
						     res)
						 (while
						     (>= (setq end (1- end)) 0)
						   (push (pop path) res))
						 (nreverse res))
					       ".")))
			 (push
			  (vector (gnus-group-full-name group server) art 0)
			  artlist))
		       (forward-line 1)))
		   (message "Searching %s using find-grep...done"
			    (or group server))
		   artlist)))
     grouplist))))

(declare-function mm-url-insert "mm-url" (url &optional follow-refresh))
(declare-function mm-url-encode-www-form-urlencoded "mm-url" (pairs))

;; gmane interface
(defun nnir-run-gmane (query srv &optional groups)
  "Run a search against a gmane back-end server."
      (let* ((case-fold-search t)
	     (qstring (cdr (assq 'query query)))
	     (server (cadr (gnus-server-to-method srv)))
	     (groupspec (mapconcat
			 (lambda (x)
			   (if (gnus-string-match-p "gmane" x)
			       (format "group:%s" (gnus-group-short-name x))
			     (error "Can't search non-gmane groups: %s" x)))
			   groups " "))
	     (authorspec
	      (if (assq 'author query)
		  (format "author:%s" (cdr (assq 'author query))) ""))
	     (search (format "%s %s %s"
			     qstring groupspec authorspec))
	     (gnus-inhibit-demon t)
	     artlist)
	(require 'mm-url)
	(with-current-buffer (get-buffer-create nnir-tmp-buffer)
	  (erase-buffer)
	  (mm-url-insert
	   (concat
	    "http://search.gmane.org/nov.php"
	    "?"
	    (mm-url-encode-www-form-urlencoded
	     `(("query" . ,search)
	       ("HITSPERPAGE" . "999")))))
	  (unless (featurep 'xemacs) (set-buffer-multibyte t))
	  (mm-decode-coding-region (point-min) (point-max) 'utf-8)
	  (goto-char (point-min))
	  (forward-line 1)
	  (while (not (eobp))
	    (unless (or (eolp) (looking-at "\x0d"))
	      (let ((header (nnheader-parse-nov)))
		(let ((xref (mail-header-xref header))
		      (xscore (string-to-number (cdr (assoc 'X-Score
			       (mail-header-extra header))))))
		  (when (string-match " \\([^:]+\\)[:/]\\([0-9]+\\)" xref)
		    (push
		     (vector
		      (gnus-group-prefixed-name (match-string 1 xref) srv)
		      (string-to-number (match-string 2 xref)) xscore)
		     artlist)))))
	    (forward-line 1)))
	(apply 'vector (nreverse (mm-delete-duplicates artlist)))))

;;; Util Code:

(defun nnir-read-parms (query nnir-search-engine)
  "Reads additional search parameters according to `nnir-engines'."
  (let ((parmspec (caddr (assoc nnir-search-engine nnir-engines))))
    (append query
	   (mapcar 'nnir-read-parm parmspec))))

(defun nnir-read-parm (parmspec)
  "Reads a single search parameter.
`parmspec' is a cons cell, the car is a symbol, the cdr is a prompt."
  (let ((sym (car parmspec))
        (prompt (cdr parmspec)))
    (if (listp prompt)
	(let* ((result (apply 'gnus-completing-read prompt))
	       (mapping (or (assoc result nnir-imap-search-arguments)
			    (cons nil nnir-imap-search-other))))
	  (cons sym (format (cdr mapping) result)))
      (cons sym (read-string prompt)))))

(autoload 'gnus-group-topic-name "gnus-topic")

(defun nnir-run-query (query)
  "Invoke appropriate search engine function (see `nnir-engines').
  If some groups were process-marked, run the query for each of the groups
  and concat the results."
  (let ((q (car (read-from-string query)))
        (groups (if (not (string= "nnir" nnir-address))
		    (list (list nnir-address))
		  (nnir-categorize
		   (or gnus-group-marked
		       (if (gnus-group-group-name)
			   (list (gnus-group-group-name))
			 (cdr (assoc (gnus-group-topic-name)
				     gnus-topic-alist))))
		   gnus-group-server))))
    (apply 'vconcat
           (mapcar
	    (lambda (x)
	      (let* ((server (car x))
		     (nnir-search-engine
		      (or (nnir-read-server-parm 'nnir-search-engine
						 server t)
			  (cdr (assoc (car
				       (gnus-server-to-method server))
				      nnir-method-default-engines))))
		     search-func)
		(setq search-func (cadr (assoc nnir-search-engine
					       nnir-engines)))
		(if search-func
		    (funcall
		     search-func
		     (if nnir-extra-parms
			 (or (and (eq nnir-search-engine 'imap)
				  (assq 'criteria q) q)
			     (setq q (nnir-read-parms q nnir-search-engine)))
		       q)
		     server (cadr x))
		  nil)))
	    groups))))

(defun nnir-read-server-parm (key server &optional not-global)
  "Returns the parameter value corresponding to `key' for
`server'. If no server-specific value is found consult the global
environment unless `not-global' is non-nil."
  (let ((method (gnus-server-to-method server)))
    (cond ((and method (assq key (cddr method)))
           (nth 1 (assq key (cddr method))))
          ((and (not not-global) (boundp key)) (symbol-value key))
          (t nil))))


(defun nnir-possibly-change-server (server)
  (unless (and server (nnir-server-opened server))
    (nnir-open-server server)))


(defun nnir-search-thread (header)
  "Make an nnir group based on the thread containing the article header"
  (let ((parm (list
	       (cons 'query
		     (nnimap-make-thread-query header))
	       (cons 'criteria "")
	       (cons 'server (gnus-method-to-server
			      (gnus-find-method-for-group
			       gnus-newsgroup-name))))))
    (gnus-group-make-nnir-group nil parm)
    (gnus-summary-goto-subject (gnus-id-to-article (mail-header-id header)))))

;; unused?
(defun nnir-artlist-groups (artlist)
  "Returns a list of all groups in the given ARTLIST."
  (let ((res nil)
        (with-dups nil))
    ;; from each artitem, extract group component
    (setq with-dups (mapcar 'nnir-artitem-group artlist))
    ;; remove duplicates from above
    (mapc (function (lambda (x) (add-to-list 'res x)))
            with-dups)
    res))

(defun nnir-get-active (srv)
  (let ((method (gnus-server-to-method srv))
	groups)
    (gnus-request-list method)
    (with-current-buffer nntp-server-buffer
      (let ((cur (current-buffer))
	    name)
	(goto-char (point-min))
	(unless (or (null nnir-ignored-newsgroups)
		    (string= nnir-ignored-newsgroups ""))
	  (delete-matching-lines nnir-ignored-newsgroups))
	(if (eq (car method) 'nntp)
	    (while (not (eobp))
	      (ignore-errors
		(push (mm-string-as-unibyte
		       (gnus-group-full-name
			(buffer-substring
			 (point)
			 (progn
			   (skip-chars-forward "^ \t")
			   (point))) method))
		      groups))
	      (forward-line))
	  (while (not (eobp))
	    (ignore-errors
	      (push (mm-string-as-unibyte
		     (if (eq (char-after) ?\")
			 (gnus-group-full-name (read cur) method)
		       (let ((p (point)) (name ""))
			 (skip-chars-forward "^ \t\\\\")
			 (setq name (buffer-substring p (point)))
			 (while (eq (char-after) ?\\)
			   (setq p (1+ (point)))
			   (forward-char 2)
			   (skip-chars-forward "^ \t\\\\")
			   (setq name (concat name (buffer-substring
						    p (point)))))
			 (gnus-group-full-name name method))))
		    groups))
	    (forward-line)))))
    groups))

(defun nnir-registry-action (action data-header from &optional to method)
  "Call `gnus-registry-action' with the original article group."
  (gnus-registry-action
   action
   data-header
   (nnir-article-group (mail-header-number data-header))
   to
   method))

(defun nnir-mode ()
  (when (eq (car (gnus-find-method-for-group gnus-newsgroup-name)) 'nnir)
    (setq gnus-summary-line-format
	  (or nnir-summary-line-format gnus-summary-line-format))
    (when (gnus-bound-and-true-p 'gnus-registry-enabled)
      (remove-hook 'gnus-summary-article-delete-hook 'gnus-registry-action t)
      (remove-hook 'gnus-summary-article-move-hook 'gnus-registry-action t)
      (remove-hook 'gnus-summary-article-expire-hook 'gnus-registry-action t)
      (add-hook 'gnus-summary-article-delete-hook 'nnir-registry-action t t)
      (add-hook 'gnus-summary-article-move-hook 'nnir-registry-action t t)
      (add-hook 'gnus-summary-article-expire-hook 'nnir-registry-action t t))))



;; The end.
(provide 'nnir)

;;; nnir.el ends here
