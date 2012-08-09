;;; url-cookie.el --- URL cookie support

;; Copyright (C) 1996-1999, 2004-2012  Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

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

(require 'url-util)
(require 'url-parse)

(eval-when-compile (require 'cl))	; defstruct

(defgroup url-cookie nil
  "URL cookies."
  :prefix "url-"
  :prefix "url-cookie-"
  :group 'url)

;; A cookie is stored internally as a vector of 7 slots
;; [ url-cookie NAME VALUE EXPIRES LOCALPART DOMAIN SECURE ]

(defstruct (url-cookie
            (:constructor url-cookie-create)
            (:copier nil)
            (:type vector)
            :named)
  name value expires localpart domain secure)

(defvar url-cookie-storage nil         "Where cookies are stored.")
(defvar url-cookie-secure-storage nil  "Where secure cookies are stored.")
(defcustom url-cookie-file nil
  "File where cookies are stored on disk."
  :type '(choice (const :tag "Default" :value nil) file)
  :group 'url-file
  :group 'url-cookie)

(defcustom url-cookie-confirmation nil
  "If non-nil, confirmation by the user is required to accept HTTP cookies."
  :type 'boolean
  :group 'url-cookie)

(defcustom url-cookie-multiple-line nil
  "If nil, HTTP requests put all cookies for the server on one line.
Some web servers, such as http://www.hotmail.com/, only accept cookies
when they are on one line.  This is broken behavior, but just try
telling Microsoft that."
  :type 'boolean
  :group 'url-cookie)

(defvar url-cookies-changed-since-last-save nil
  "Whether the cookies list has changed since the last save operation.")

(defun url-cookie-parse-file (&optional fname)
  "Load FNAME, default `url-cookie-file'."
  ;; It's completely normal for the cookies file not to exist yet.
  (load (or fname url-cookie-file) t t))

(defun url-cookie-clean-up (&optional secure)
  (let ((var (if secure 'url-cookie-secure-storage 'url-cookie-storage))
	new new-cookies)
    (dolist (cur (symbol-value var))
      (setq new-cookies nil)
      (dolist (cur-cookie (cdr cur))
	(or (not (url-cookie-p cur-cookie))
	    (url-cookie-expired-p cur-cookie)
	    (null (url-cookie-expires cur-cookie))
	    (setq new-cookies (cons cur-cookie new-cookies))))
      (when new-cookies
	(setcdr cur new-cookies)
	(setq new (cons cur new))))
    (set var new)))

(defun url-cookie-write-file (&optional fname)
  (when url-cookies-changed-since-last-save
    (or fname (setq fname (expand-file-name url-cookie-file)))
    (if (condition-case nil
            (progn
              (url-make-private-file fname)
              nil)
          (error t))
        (message "Error accessing cookie file `%s'" fname)
    (url-cookie-clean-up)
    (url-cookie-clean-up t)
    (with-temp-buffer
      (insert ";; Emacs-W3 HTTP cookies file\n"
	      ";; Automatically generated file!!! DO NOT EDIT!!!\n\n"
	      "(setq url-cookie-storage\n '")
      (pp url-cookie-storage (current-buffer))
      (insert ")\n(setq url-cookie-secure-storage\n '")
      (pp url-cookie-secure-storage (current-buffer))
      (insert ")\n")
      (insert "\n;; Local Variables:\n"
              ";; version-control: never\n"
              ";; no-byte-compile: t\n"
              ";; End:\n")
      (set (make-local-variable 'version-control) 'never)
      (write-file fname))
    (setq url-cookies-changed-since-last-save nil))))

(defun url-cookie-store (name value &optional expires domain localpart secure)
  "Store a cookie."
  (let ((storage (if secure url-cookie-secure-storage url-cookie-storage))
	tmp found-domain)
    ;; First, look for a matching domain.
    (if (setq found-domain (assoc domain storage))
	;; Need to either stick the new cookie in existing domain storage
	;; or possibly replace an existing cookie if the names match.
	(unless (dolist (cur (setq storage (cdr found-domain)) tmp)
		  (and (equal localpart (url-cookie-localpart cur))
		       (equal name (url-cookie-name cur))
		       (progn
			 (setf (url-cookie-expires cur) expires)
			 (setf (url-cookie-value cur) value)
			 (setq tmp t))))
	  ;; New cookie.
	  (setcdr found-domain (cons
				(url-cookie-create :name name
						   :value value
						   :expires expires
						   :domain domain
						   :localpart localpart
						   :secure secure)
				(cdr found-domain))))
      ;; Need to add a new top-level domain.
      (setq tmp (url-cookie-create :name name
				   :value value
				   :expires expires
				   :domain domain
				   :localpart localpart
				   :secure secure))
      (cond (storage
	     (setcdr storage (cons (list domain tmp) (cdr storage))))
	    (secure
	     (setq url-cookie-secure-storage (list (list domain tmp))))
	    (t
	     (setq url-cookie-storage (list (list domain tmp))))))))

(defun url-cookie-expired-p (cookie)
  "Return non-nil if COOKIE is expired."
  (let ((exp (url-cookie-expires cookie)))
    (and (> (length exp) 0)
	 (> (float-time) (float-time (date-to-time exp))))))

(defun url-cookie-retrieve (host &optional localpart secure)
  "Retrieve all cookies for a specified HOST and LOCALPART."
  (let ((storage (if secure
		     (append url-cookie-secure-storage url-cookie-storage)
		   url-cookie-storage))
	(case-fold-search t)
	cookies retval localpart-match)
    (dolist (cur storage)
      (setq cookies (cdr cur))
      (if (and (car cur)
	       (string-match
                (concat "^.*"
                        (regexp-quote
                         ;; Remove the dot from wildcard domains
                         ;; before matching.
			 (if (eq ?. (aref (car cur) 0))
                             (substring (car cur) 1)
                           (car cur)))
                        "$") host))
	  ;; The domains match - a possible hit!
	  (dolist (cur cookies)
	    (and (if (and (stringp
			   (setq localpart-match (url-cookie-localpart cur)))
			  (stringp localpart))
		     (string-match (concat "^" (regexp-quote localpart-match))
				   localpart)
		   (equal localpart localpart-match))
		 (not (url-cookie-expired-p cur))
		 (setq retval (cons cur retval))))))
    retval))

(defun url-cookie-generate-header-lines (host localpart secure)
  (let ((cookies (url-cookie-retrieve host localpart secure))
	retval chunk)
    ;; Have to sort this for sending most specific cookies first.
    (setq cookies (and cookies
		       (sort cookies
			     (lambda (x y)
			       (> (length (url-cookie-localpart x))
				  (length (url-cookie-localpart y)))))))
    (dolist (cur cookies)
      (setq chunk (format "%s=%s" (url-cookie-name cur) (url-cookie-value cur))
	    retval (if (and url-cookie-multiple-line
			    (< 80 (+ (length retval) (length chunk) 4)))
		       (concat retval "\r\nCookie: " chunk)
		     (if retval
			 (concat retval "; " chunk)
		       (concat "Cookie: " chunk)))))
    (if retval
	(concat retval "\r\n")
      "")))

(defvar url-cookie-two-dot-domains
  (concat "\\.\\("
   (mapconcat 'identity (list "com" "edu" "net" "org" "gov" "mil" "int")
	      "\\|")
   "\\)$")
  "A regexp of top level domains that only require two matching
'.'s in the domain name in order to set a cookie.")

(defcustom url-cookie-trusted-urls nil
  "A list of regular expressions matching URLs to always accept cookies from."
  :type '(repeat regexp)
  :group 'url-cookie)

(defcustom url-cookie-untrusted-urls nil
  "A list of regular expressions matching URLs to never accept cookies from."
  :type '(repeat regexp)
  :group 'url-cookie)

(defun url-cookie-host-can-set-p (host domain)
  (let ((numdots 0)
	(last nil)
	(case-fold-search t)
	(mindots 3))
    (while (setq last (string-match "\\." domain last))
      (setq numdots (1+ numdots)
	    last (1+ last)))
    (if (string-match url-cookie-two-dot-domains domain)
	(setq mindots 2))
    (cond
     ((string= host domain)		; Apparently netscape lets you do this
      t)
     ((>= numdots mindots)		; We have enough dots in domain name
      ;; Need to check and make sure the host is actually _in_ the
      ;; domain it wants to set a cookie for though.
      (string-match (concat (regexp-quote
                             ;; Remove the dot from wildcard domains
                             ;; before matching.
                             (if (eq ?. (aref domain 0))
                                 (substring domain 1)
                               domain))
                            "$") host))
     (t
      nil))))

(defun url-cookie-handle-set-cookie (str)
  (setq url-cookies-changed-since-last-save t)
  (let* ((args (url-parse-args str t))
	 (case-fold-search t)
	 (secure (and (assoc-string "secure" args t) t))
	 (domain (or (cdr-safe (assoc-string "domain" args t))
		     (url-host url-current-object)))
	 (current-url (url-view-url t))
	 (trusted url-cookie-trusted-urls)
	 (untrusted url-cookie-untrusted-urls)
	 (expires (cdr-safe (assoc-string "expires" args t)))
	 (localpart (or (cdr-safe (assoc-string "path" args t))
			(file-name-directory
			 (url-filename url-current-object))))
	 (rest nil))
    (dolist (this args)
      (or (member (downcase (car this)) '("secure" "domain" "expires" "path"))
	  (setq rest (cons this rest))))

    ;; Sometimes we get dates that the timezone package cannot handle very
    ;; gracefully - take care of this here, instead of in url-cookie-expired-p
    ;; to speed things up.
    (and expires
	 (string-match
	  (concat "^[^,]+, +\\(..\\)-\\(...\\)-\\(..\\) +"
		  "\\(..:..:..\\) +\\[*\\([^\]]+\\)\\]*$")
	  expires)
	 (setq expires (concat (match-string 1 expires) " "
			       (match-string 2 expires) " "
			       (match-string 3 expires) " "
			       (match-string 4 expires) " ["
			       (match-string 5 expires) "]")))

    ;; This one is for older Emacs/XEmacs variants that don't
    ;; understand this format without tenths of a second in it.
    ;; Wednesday, 30-Dec-2037 16:00:00 GMT
    ;;       - vs -
    ;; Wednesday, 30-Dec-2037 16:00:00.00 GMT
    (and expires
	 (string-match
	  "\\([0-9]+\\)-\\([A-Za-z]+\\)-\\([0-9]+\\)[ \t]+\\([0-9]+:[0-9]+:[0-9]+\\)\\(\\.[0-9]+\\)*[ \t]+\\([-+a-zA-Z0-9]+\\)"
	  expires)
	 (setq expires (concat (match-string 1 expires) "-"	; day
			       (match-string 2 expires) "-"	; month
			       (match-string 3 expires) " "	; year
			       (match-string 4 expires) ".00 " ; hour:minutes:seconds
			       (match-string 6 expires)))) ":" ; timezone

    (while (consp trusted)
      (if (string-match (car trusted) current-url)
	  (setq trusted (- (match-end 0) (match-beginning 0)))
	(pop trusted)))
    (while (consp untrusted)
      (if (string-match (car untrusted) current-url)
	  (setq untrusted (- (match-end 0) (match-beginning 0)))
	(pop untrusted)))
    (and trusted untrusted
	 ;; Choose the more specific match.
	 (set (if (> trusted untrusted) 'untrusted 'trusted) nil))
    (cond
     (untrusted
      ;; The site was explicitly marked as untrusted by the user.
      nil)
     ((or (eq url-privacy-level 'paranoid)
	  (and (listp url-privacy-level) (memq 'cookies url-privacy-level)))
      ;; User never wants cookies.
      nil)
     ((and url-cookie-confirmation
	   (not trusted)
	   (save-window-excursion
	     (with-output-to-temp-buffer "*Cookie Warning*"
	       (dolist (x rest)
                 (princ (format "%s - %s" (car x) (cdr x)))))
	     (prog1
		 (not (funcall url-confirmation-func
			       (format "Allow %s to set these cookies? "
				       (url-host url-current-object))))
	       (if (get-buffer "*Cookie Warning*")
		   (kill-buffer "*Cookie Warning*")))))
      ;; User wants to be asked, and declined.
      nil)
     ((url-cookie-host-can-set-p (url-host url-current-object) domain)
      ;; Cookie is accepted by the user, and passes our security checks.
      (dolist (cur rest)
	(url-cookie-store (car cur) (cdr cur) expires domain localpart secure)))
     (t
      (url-lazy-message "%s tried to set a cookie for domain %s - rejected."
			(url-host url-current-object) domain)))))

(defvar url-cookie-timer nil)

(defcustom url-cookie-save-interval 3600
  "The number of seconds between automatic saves of cookies.
Default is 1 hour.  Note that if you change this variable outside of
the `customize' interface after `url-do-setup' has been run, you need
to run the `url-cookie-setup-save-timer' function manually."
  :set #'(lambda (var val)
	   (set-default var val)
	   (if (bound-and-true-p url-setup-done)
	       (url-cookie-setup-save-timer)))
  :type 'integer
  :group 'url-cookie)

(defun url-cookie-setup-save-timer ()
  "Reset the cookie saver timer."
  (interactive)
  (ignore-errors (cancel-timer url-cookie-timer))
  (setq url-cookie-timer nil)
  (if url-cookie-save-interval
      (setq url-cookie-timer (run-at-time url-cookie-save-interval
					  url-cookie-save-interval
					  #'url-cookie-write-file))))

(provide 'url-cookie)

;;; url-cookie.el ends here
