;;; ldap.el --- client interface to LDAP for Emacs

;; Copyright (C) 1998-2012  Free Software Foundation, Inc.

;; Author: Oscar Figueiredo <oscar@cpe.fr>
;; Maintainer: FSF
;; Created: April 1998
;; Keywords: comm

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

;;    This package provides basic functionality to perform searches on LDAP
;;    servers.  It requires a command line utility generally named
;;    `ldapsearch' to actually perform the searches.  That program can be
;;    found in all LDAP developer kits such as:
;;      - UM-LDAP 3.3 (http://www.umich.edu/~dirsvcs/ldap/)
;;      - OpenLDAP (http://www.openldap.org/)

;;; Code:

(require 'custom)
(eval-when-compile (require 'cl))

(autoload 'auth-source-search "auth-source")

(defgroup ldap nil
  "Lightweight Directory Access Protocol."
  :version "21.1"
  :group 'comm)

(defcustom ldap-default-host nil
  "Default LDAP server.
A TCP port number can be appended to that name using a colon as
a separator."
  :type '(choice (string :tag "Host name")
		 (const :tag "Use library default" nil))
  :group 'ldap)

(defcustom ldap-default-port nil
  "Default TCP port for LDAP connections.
Initialized from the LDAP library at build time. Default value is 389."
  :type '(choice (const :tag "Use library default" nil)
		 (integer :tag "Port number"))
  :group 'ldap)

(defcustom ldap-default-base nil
  "Default base for LDAP searches.
This is a string using the syntax of RFC 1779.
For instance, \"o=ACME, c=US\" limits the search to the
Acme organization in the United States."
  :type '(choice (const :tag "Use library default" nil)
		 (string :tag "Search base"))
  :group 'ldap)


(defcustom ldap-host-parameters-alist nil
  "Alist of host-specific options for LDAP transactions.
The format of each list element is (HOST PROP1 VAL1 PROP2 VAL2 ...).
HOST is the hostname of an LDAP server (with an optional TCP port number
appended to it using a colon as a separator).
PROPn and VALn are property/value pairs describing parameters for the server.
Valid properties include:
  `binddn' is the distinguished name of the user to bind as
    (in RFC 1779 syntax).
  `passwd' is the password to use for simple authentication.
  `auth' is the authentication method to use.
    Possible values are: `simple', `krbv41' and `krbv42'.
  `base' is the base for the search as described in RFC 1779.
  `scope' is one of the three symbols `subtree', `base' or `onelevel'.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return."
  :type '(repeat :menu-tag "Host parameters"
		 :tag "Host parameters"
		 (list :menu-tag "Host parameters"
		       :tag "Host parameters"
		       :value nil
		       (string :tag "Host name")
		       (checklist :inline t
				  :greedy t
				  (list
				   :tag "Search Base"
				   :inline t
				   (const :tag "Search Base" base)
				   string)
				  (list
				   :tag "Binding DN"
				   :inline t
				   (const :tag "Binding DN" binddn)
				   string)
				  (list
				   :tag "Password"
				   :inline t
				   (const :tag "Password" passwd)
				   string)
				  (list
				   :tag "Authentication Method"
				   :inline t
				   (const :tag "Authentication Method" auth)
				   (choice
				    (const :menu-tag "None" :tag "None" nil)
				    (const :menu-tag "Simple" :tag "Simple" simple)
				    (const :menu-tag "Kerberos 4.1" :tag "Kerberos 4.1" krbv41)
				    (const :menu-tag "Kerberos 4.2" :tag "Kerberos 4.2" krbv42)))
				  (list
				   :tag "Search Scope"
				   :inline t
				   (const :tag "Search Scope" scope)
				   (choice
				    (const :menu-tag "Default" :tag "Default" nil)
				    (const :menu-tag "Subtree" :tag "Subtree" subtree)
				    (const :menu-tag "Base" :tag "Base" base)
				    (const :menu-tag "One Level" :tag "One Level" onelevel)))
				  (list
				   :tag "Dereferencing"
				   :inline t
				   (const :tag "Dereferencing" deref)
				   (choice
				    (const :menu-tag "Default" :tag "Default" nil)
				    (const :menu-tag "Never" :tag "Never" never)
				    (const :menu-tag "Always" :tag "Always" always)
				    (const :menu-tag "When searching" :tag "When searching" search)
				    (const :menu-tag "When locating base" :tag "When locating base" find)))
				  (list
				   :tag "Time Limit"
				   :inline t
				   (const :tag "Time Limit" timelimit)
				   (integer :tag "(in seconds)"))
				  (list
				   :tag "Size Limit"
				   :inline t
				   (const :tag "Size Limit" sizelimit)
				   (integer :tag "(number of records)")))))
  :group 'ldap)

(defcustom ldap-ldapsearch-prog "ldapsearch"
  "The name of the ldapsearch command line program."
  :type '(string :tag "`ldapsearch' Program")
  :group 'ldap)

(defcustom ldap-ldapsearch-args '("-LL" "-tt")
  "A list of additional arguments to pass to `ldapsearch'."
  :type '(repeat :tag "`ldapsearch' Arguments"
		 (string :tag "Argument"))
  :group 'ldap)

(defcustom ldap-ignore-attribute-codings nil
  "If non-nil, do not encode/decode LDAP attribute values."
  :type 'boolean
  :group 'ldap)

(defcustom ldap-default-attribute-decoder nil
  "Decoder function to use for attributes whose syntax is unknown."
  :type 'symbol
  :group 'ldap)

(defcustom ldap-coding-system 'utf-8
  "Coding system of LDAP string values.
LDAP v3 specifies the coding system of strings to be UTF-8."
  :type 'symbol
  :group 'ldap)

(defvar ldap-attribute-syntax-encoders
  [nil					; 1  ACI Item                        N
   nil					; 2  Access Point                    Y
   nil					; 3  Attribute Type Description      Y
   nil					; 4  Audio                           N
   nil					; 5  Binary                          N
   nil					; 6  Bit String                      Y
   ldap-encode-boolean			; 7  Boolean                         Y
   nil					; 8  Certificate                     N
   nil					; 9  Certificate List                N
   nil					; 10 Certificate Pair                N
   ldap-encode-country-string		; 11 Country String                  Y
   ldap-encode-string			; 12 DN                              Y
   nil					; 13 Data Quality Syntax             Y
   nil					; 14 Delivery Method                 Y
   ldap-encode-string			; 15 Directory String                Y
   nil					; 16 DIT Content Rule Description    Y
   nil					; 17 DIT Structure Rule Description  Y
   nil					; 18 DL Submit Permission            Y
   nil					; 19 DSA Quality Syntax              Y
   nil					; 20 DSE Type                        Y
   nil					; 21 Enhanced Guide                  Y
   nil					; 22 Facsimile Telephone Number      Y
   nil					; 23 Fax                             N
   nil					; 24 Generalized Time                Y
   nil					; 25 Guide                           Y
   nil					; 26 IA5 String                      Y
   number-to-string			; 27 INTEGER                         Y
   nil					; 28 JPEG                            N
   nil					; 29 Master And Shadow Access Points Y
   nil					; 30 Matching Rule Description       Y
   nil					; 31 Matching Rule Use Description   Y
   nil					; 32 Mail Preference                 Y
   nil					; 33 MHS OR Address                  Y
   nil					; 34 Name And Optional UID           Y
   nil					; 35 Name Form Description           Y
   nil					; 36 Numeric String                  Y
   nil					; 37 Object Class Description        Y
   nil					; 38 OID                             Y
   nil					; 39 Other Mailbox                   Y
   nil					; 40 Octet String                    Y
   ldap-encode-address			; 41 Postal Address                  Y
   nil					; 42 Protocol Information            Y
   nil					; 43 Presentation Address            Y
   ldap-encode-string			; 44 Printable String                Y
   nil					; 45 Subtree Specification           Y
   nil					; 46 Supplier Information            Y
   nil					; 47 Supplier Or Consumer            Y
   nil					; 48 Supplier And Consumer           Y
   nil					; 49 Supported Algorithm             N
   nil					; 50 Telephone Number                Y
   nil					; 51 Teletex Terminal Identifier     Y
   nil					; 52 Telex Number                    Y
   nil					; 53 UTC Time                        Y
   nil					; 54 LDAP Syntax Description         Y
   nil					; 55 Modify Rights                   Y
   nil					; 56 LDAP Schema Definition          Y
   nil					; 57 LDAP Schema Description         Y
   nil					; 58 Substring Assertion             Y
   ]
  "A vector of functions used to encode LDAP attribute values.
The sequence of functions corresponds to the sequence of LDAP attribute syntax
object identifiers of the form 1.3.6.1.4.1.1466.1115.121.1.* as defined in
RFC2252 section 4.3.2")

(defvar ldap-attribute-syntax-decoders
  [nil					; 1  ACI Item                        N
   nil					; 2  Access Point                    Y
   nil					; 3  Attribute Type Description      Y
   nil					; 4  Audio                           N
   nil					; 5  Binary                          N
   nil					; 6  Bit String                      Y
   ldap-decode-boolean			; 7  Boolean                         Y
   nil					; 8  Certificate                     N
   nil					; 9  Certificate List                N
   nil					; 10 Certificate Pair                N
   ldap-decode-string			; 11 Country String                  Y
   ldap-decode-string			; 12 DN                              Y
   nil					; 13 Data Quality Syntax             Y
   nil					; 14 Delivery Method                 Y
   ldap-decode-string			; 15 Directory String                Y
   nil					; 16 DIT Content Rule Description    Y
   nil					; 17 DIT Structure Rule Description  Y
   nil					; 18 DL Submit Permission            Y
   nil					; 19 DSA Quality Syntax              Y
   nil					; 20 DSE Type                        Y
   nil					; 21 Enhanced Guide                  Y
   nil					; 22 Facsimile Telephone Number      Y
   nil					; 23 Fax                             N
   nil					; 24 Generalized Time                Y
   nil					; 25 Guide                           Y
   nil					; 26 IA5 String                      Y
   string-to-number			; 27 INTEGER                         Y
   nil					; 28 JPEG                            N
   nil					; 29 Master And Shadow Access Points Y
   nil					; 30 Matching Rule Description       Y
   nil					; 31 Matching Rule Use Description   Y
   nil					; 32 Mail Preference                 Y
   nil					; 33 MHS OR Address                  Y
   nil					; 34 Name And Optional UID           Y
   nil					; 35 Name Form Description           Y
   nil					; 36 Numeric String                  Y
   nil					; 37 Object Class Description        Y
   nil					; 38 OID                             Y
   nil					; 39 Other Mailbox                   Y
   nil					; 40 Octet String                    Y
   ldap-decode-address			; 41 Postal Address                  Y
   nil					; 42 Protocol Information            Y
   nil					; 43 Presentation Address            Y
   ldap-decode-string			; 44 Printable String                Y
   nil					; 45 Subtree Specification           Y
   nil					; 46 Supplier Information            Y
   nil					; 47 Supplier Or Consumer            Y
   nil					; 48 Supplier And Consumer           Y
   nil					; 49 Supported Algorithm             N
   nil					; 50 Telephone Number                Y
   nil					; 51 Teletex Terminal Identifier     Y
   nil					; 52 Telex Number                    Y
   nil					; 53 UTC Time                        Y
   nil					; 54 LDAP Syntax Description         Y
   nil					; 55 Modify Rights                   Y
   nil					; 56 LDAP Schema Definition          Y
   nil					; 57 LDAP Schema Description         Y
   nil					; 58 Substring Assertion             Y
   ]
  "A vector of functions used to decode LDAP attribute values.
The sequence of functions corresponds to the sequence of LDAP attribute syntax
object identifiers of the form 1.3.6.1.4.1.1466.1115.121.1.* as defined in
RFC2252 section 4.3.2")


(defvar ldap-attribute-syntaxes-alist
  '((createtimestamp . 24)
    (modifytimestamp . 24)
    (creatorsname . 12)
    (modifiersname . 12)
    (subschemasubentry . 12)
    (attributetypes . 3)
    (objectclasses . 37)
    (matchingrules . 30)
    (matchingruleuse . 31)
    (namingcontexts . 12)
    (altserver . 26)
    (supportedextension . 38)
    (supportedcontrol . 38)
    (supportedsaslmechanisms . 15)
    (supportedldapversion . 27)
    (ldapsyntaxes . 16)
    (ditstructurerules . 17)
    (nameforms . 35)
    (ditcontentrules . 16)
    (objectclass . 38)
    (aliasedobjectname . 12)
    (cn . 15)
    (sn . 15)
    (serialnumber . 44)
    (c . 15)
    (l . 15)
    (st . 15)
    (street . 15)
    (o . 15)
    (ou . 15)
    (title . 15)
    (description . 15)
    (searchguide . 25)
    (businesscategory . 15)
    (postaladdress . 41)
    (postalcode . 15)
    (postofficebox . 15)
    (physicaldeliveryofficename . 15)
    (telephonenumber . 50)
    (telexnumber . 52)
    (telexterminalidentifier . 51)
    (facsimiletelephonenumber . 22)
    (x121address . 36)
    (internationalisdnnumber . 36)
    (registeredaddress . 41)
    (destinationindicator . 44)
    (preferreddeliverymethod . 14)
    (presentationaddress . 43)
    (supportedapplicationcontext . 38)
    (member . 12)
    (owner . 12)
    (roleoccupant . 12)
    (seealso . 12)
    (userpassword . 40)
    (usercertificate . 8)
    (cacertificate . 8)
    (authorityrevocationlist . 9)
    (certificaterevocationlist . 9)
    (crosscertificatepair . 10)
    (name . 15)
    (givenname . 15)
    (initials . 15)
    (generationqualifier . 15)
    (x500uniqueidentifier . 6)
    (dnqualifier . 44)
    (enhancedsearchguide . 21)
    (protocolinformation . 42)
    (distinguishedname . 12)
    (uniquemember . 34)
    (houseidentifier . 15)
    (supportedalgorithms . 49)
    (deltarevocationlist . 9)
    (dmdname . 15))
  "A map of LDAP attribute names to their type object id minor number.
This table is built from RFC2252 Section 5 and RFC2256 Section 5")


;; Coding/decoding functions

(defun ldap-encode-boolean (bool)
  (if bool
      "TRUE"
    "FALSE"))

(defun ldap-decode-boolean (str)
  (cond
   ((string-equal str "TRUE")
    t)
   ((string-equal str "FALSE")
    nil)
   (t
    (error "Wrong LDAP boolean string: %s" str))))

(defun ldap-encode-country-string (str)
  ;; We should do something useful here...
  (if (not (= 2 (length str)))
      (error "Invalid country string: %s" str)))

(defun ldap-decode-string (str)
  (decode-coding-string str ldap-coding-system))

(defun ldap-encode-string (str)
  (encode-coding-string str ldap-coding-system))

(defun ldap-decode-address (str)
  (mapconcat 'ldap-decode-string
	     (split-string str "\\$")
	     "\n"))

(defun ldap-encode-address (str)
  (mapconcat 'ldap-encode-string
	     (split-string str "\n")
	     "$"))


;; LDAP protocol functions

(defun ldap-get-host-parameter (host parameter)
  "Get the value of PARAMETER for HOST in `ldap-host-parameters-alist'."
  (plist-get (cdr (assoc host ldap-host-parameters-alist))
	     parameter))

(defun ldap-decode-attribute (attr)
  "Decode the attribute/value pair ATTR according to LDAP rules.
The attribute name is looked up in `ldap-attribute-syntaxes-alist'
and the corresponding decoder is then retrieved from
`ldap-attribute-syntax-decoders' and applied on the value(s)."
  (let* ((name (car attr))
	 (values (cdr attr))
	 (syntax-id (cdr (assq (intern (downcase name))
			       ldap-attribute-syntaxes-alist)))
	 decoder)
    (if syntax-id
	(setq decoder (aref ldap-attribute-syntax-decoders
			    (1- syntax-id)))
      (setq decoder ldap-default-attribute-decoder))
    (if decoder
	(cons name (mapcar decoder values))
      attr)))

(defun ldap-search (filter &optional host attributes attrsonly withdn)
  "Perform an LDAP search.
FILTER is the search filter in RFC1558 syntax.
HOST is the LDAP host on which to perform the search.
ATTRIBUTES are the specific attributes to retrieve, nil means
retrieve all.
ATTRSONLY, if non-nil, retrieves the attributes only, without
the associated values.
If WITHDN is non-nil, each entry in the result will be prepended with
its distinguished name WITHDN.
Additional search parameters can be specified through
`ldap-host-parameters-alist', which see."
  (interactive "sFilter:")
  (or host
      (setq host ldap-default-host)
      (error "No LDAP host specified"))
  (let ((host-plist (cdr (assoc host ldap-host-parameters-alist)))
	result)
    (setq result (ldap-search-internal (list* 'host host
					      'filter filter
					      'attributes attributes
					      'attrsonly attrsonly
					      'withdn withdn
					      host-plist)))
    (if ldap-ignore-attribute-codings
	result
      (mapcar (lambda (record)
		(mapcar 'ldap-decode-attribute record))
	      result))))


(defun ldap-search-internal (search-plist)
  "Perform a search on a LDAP server.
SEARCH-PLIST is a property list describing the search request.
Valid keys in that list are:

  `auth-source', if non-nil, will use `auth-source-search' and
will grab the :host, :secret, :base, and (:user or :binddn)
tokens into the `host', `passwd', `base', and `binddn' parameters
respectively if they are not provided in SEARCH-PLIST.  So for
instance *each* of these netrc lines has the same effect if you
ask for the host \"ldapserver:2400\":

  machine ldapserver:2400 login myDN secret myPassword base myBase
  machine ldapserver:2400 binddn myDN secret myPassword port ldap
  login myDN secret myPassword base myBase

but if you have more than one in your netrc file, only the first
matching one will be used.  Note the \"port ldap\" part is NOT
required.

  `host' is a string naming one or more (blank-separated) LDAP servers
to try to connect to.  Each host name may optionally be of the form HOST:PORT.
  `filter' is a filter string for the search as described in RFC 1558.
  `attributes' is a list of strings indicating which attributes to retrieve
for each matching entry. If nil, return all available attributes.
  `attrsonly', if non-nil, indicates that only attributes are retrieved,
not their associated values.
  `auth' is one of the symbols `simple', `krbv41' or `krbv42'.
  `base' is the base for the search as described in RFC 1779.
  `scope' is one of the three symbols `sub', `base' or `one'.
  `binddn' is the distinguished name of the user to bind as (in RFC 1779 syntax).
  `auth' is one of the symbols `simple', `krbv41' or `krbv42'
  `passwd' is the password to use for simple authentication.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return.
  `withdn' if non-nil each entry in the result will be prepended with
its distinguished name DN.
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/value pairs."
  (let* ((buf (get-buffer-create " *ldap-search*"))
	(bufval (get-buffer-create " *ldap-value*"))
	(host (or (plist-get search-plist 'host)
		  ldap-default-host))
         ;; find entries with port "ldap" that match the requested host if any
         (asfound (when (plist-get search-plist 'auth-source)
                    (nth 0 (auth-source-search :host (or host t)
                                               :create t))))
         ;; if no host was requested, get it from the auth-source entry
         (host (or host (plist-get asfound :host)))
         ;; get the password from the auth-source
         (passwd (or (plist-get search-plist 'passwd)
                     (plist-get asfound :secret)))
         ;; convert the password from a function call if needed
         (passwd (if (functionp passwd) (funcall passwd) passwd))
         ;; get the binddn from the search-list or from the
         ;; auth-source user or binddn tokens
         (binddn (or (plist-get search-plist 'binddn)
                     (plist-get asfound :user)
                     (plist-get asfound :binddn)))
         (base (or (plist-get search-plist 'base)
                   (plist-get asfound :base)
                   ldap-default-base))
	(filter (plist-get search-plist 'filter))
	(attributes (plist-get search-plist 'attributes))
	(attrsonly (plist-get search-plist 'attrsonly))
	(scope (plist-get search-plist 'scope))
        (auth (plist-get search-plist 'auth))
	(deref (plist-get search-plist 'deref))
	(timelimit (plist-get search-plist 'timelimit))
	(sizelimit (plist-get search-plist 'sizelimit))
	(withdn (plist-get search-plist 'withdn))
	(numres 0)
	arglist dn name value record result)
    (if (or (null filter)
	    (equal "" filter))
	(error "No search filter"))
    (setq filter (cons filter attributes))
    (with-current-buffer buf
      (erase-buffer)
      (if (and host
	       (not (equal "" host)))
	  (setq arglist (nconc arglist (list (format "-h%s" host)))))
      (if (and attrsonly
	       (not (equal "" attrsonly)))
	  (setq arglist (nconc arglist (list "-A"))))
      (if (and base
	       (not (equal "" base)))
	  (setq arglist (nconc arglist (list (format "-b%s" base)))))
      (if (and scope
	       (not (equal "" scope)))
	  (setq arglist (nconc arglist (list (format "-s%s" scope)))))
      (if (and binddn
	       (not (equal "" binddn)))
	  (setq arglist (nconc arglist (list (format "-D%s" binddn)))))
      (if (and auth
	       (equal 'simple auth))
	  (setq arglist (nconc arglist (list "-x"))))
      (if (and passwd
	       (not (equal "" passwd)))
	  (setq arglist (nconc arglist (list (format "-w%s" passwd)))))
      (if (and deref
	       (not (equal "" deref)))
	  (setq arglist (nconc arglist (list (format "-a%s" deref)))))
      (if (and timelimit
	       (not (equal "" timelimit)))
	  (setq arglist (nconc arglist (list (format "-l%s" timelimit)))))
      (if (and sizelimit
	       (not (equal "" sizelimit)))
	  (setq arglist (nconc arglist (list (format "-z%s" sizelimit)))))
      (apply #'call-process ldap-ldapsearch-prog
	     ;; Ignore stderr, which can corrupt results
	     nil (list buf nil) nil
	     (append arglist ldap-ldapsearch-args filter))
      (insert "\n")
      (goto-char (point-min))

      (while (re-search-forward "[\t\n\f]+ " nil t)
	(replace-match "" nil nil))
      (goto-char (point-min))

      (if (looking-at "usage")
	  (error "Incorrect ldapsearch invocation")
	(message "Parsing results... ")
	;; Skip error message when retrieving attribute list
	(if (looking-at "Size limit exceeded")
	    (forward-line 1))
	(while (progn
		 (skip-chars-forward " \t\n")
		 (not (eobp)))
	  (setq dn (buffer-substring (point) (point-at-eol)))
	  (forward-line 1)
          (while (looking-at "^\\([A-Za-z][-A-Za-z0-9]*\
\\|[0-9]+\\(?:\\.[0-9]+\\)*\\)\\(;[-A-Za-z0-9]+\\)*[=:\t ]+\
\\(<[\t ]*file://\\)\\(.*\\)$")
	    (setq name (match-string 1)
		  value (match-string 4))
            ;; Need to handle file:///D:/... as generated by OpenLDAP
            ;; on DOS/Windows as local files.
            (if (and (memq system-type '(windows-nt ms-dos))
                     (eq (string-match "/\\(.:.*\\)$" value) 0))
                (setq value (match-string 1 value)))
	    ;; Do not try to open non-existent files
	    (if (equal value "")
		(setq value " ")
	      (with-current-buffer bufval
		(erase-buffer)
		(set-buffer-multibyte nil)
		(insert-file-contents-literally value)
		(delete-file value)
		(setq value (buffer-string))))
	    (setq record (cons (list name value)
			       record))
	    (forward-line 1))
	  (cond (withdn
		 (push (cons dn (nreverse record)) result))
		(record
		 (push (nreverse record) result)))
	  (setq record nil)
	  (skip-chars-forward " \t\n")
	  (message "Parsing results... %d" numres)
	  (1+ numres))
	(message "Parsing results... done")
	(nreverse result)))))

(provide 'ldap)

;;; ldap.el ends here
