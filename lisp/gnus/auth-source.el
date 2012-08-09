;;; auth-source.el --- authentication sources for Gnus and Emacs

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
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

;; This is the auth-source.el package.  It lets users tell Gnus how to
;; authenticate in a single place.  Simplicity is the goal.  Instead
;; of providing 5000 options, we'll stick to simple, easy to
;; understand options.

;; See the auth.info Info documentation for details.

;; TODO:

;; - never decode the backend file unless it's necessary
;; - a more generic way to match backends and search backend contents
;; - absorb netrc.el and simplify it
;; - protect passwords better
;; - allow creating and changing netrc lines (not files) e.g. change a password

;;; Code:

(require 'password-cache)
(require 'mm-util)
(require 'gnus-util)
(require 'assoc)

(eval-when-compile (require 'cl))
(require 'eieio)

(autoload 'secrets-create-item "secrets")
(autoload 'secrets-delete-item "secrets")
(autoload 'secrets-get-alias "secrets")
(autoload 'secrets-get-attributes "secrets")
(autoload 'secrets-get-secret "secrets")
(autoload 'secrets-list-collections "secrets")
(autoload 'secrets-search-items "secrets")

(autoload 'rfc2104-hash "rfc2104")

(autoload 'plstore-open "plstore")
(autoload 'plstore-find "plstore")
(autoload 'plstore-put "plstore")
(autoload 'plstore-delete "plstore")
(autoload 'plstore-save "plstore")
(autoload 'plstore-get-file "plstore")

(autoload 'epg-make-context "epg")
(autoload 'epg-context-set-passphrase-callback "epg")
(autoload 'epg-decrypt-string "epg")
(autoload 'epg-context-set-armor "epg")
(autoload 'epg-encrypt-string "epg")

(autoload 'help-mode "help-mode" nil t)

(defvar secrets-enabled)

(defgroup auth-source nil
  "Authentication sources."
  :version "23.1" ;; No Gnus
  :group 'gnus)

;;;###autoload
(defcustom auth-source-cache-expiry 7200
  "How many seconds passwords are cached, or nil to disable
expiring.  Overrides `password-cache-expiry' through a
let-binding."
  :version "24.1"
  :group 'auth-source
  :type '(choice (const :tag "Never" nil)
                 (const :tag "All Day" 86400)
                 (const :tag "2 Hours" 7200)
                 (const :tag "30 Minutes" 1800)
                 (integer :tag "Seconds")))

;;; The slots below correspond with the `auth-source-search' spec,
;;; so a backend with :host set, for instance, would match only
;;; searches for that host.  Normally they are nil.
(defclass auth-source-backend ()
  ((type :initarg :type
         :initform 'netrc
         :type symbol
         :custom symbol
         :documentation "The backend type.")
   (source :initarg :source
           :type string
           :custom string
           :documentation "The backend source.")
   (host :initarg :host
         :initform t
         :type t
         :custom string
         :documentation "The backend host.")
   (user :initarg :user
         :initform t
         :type t
         :custom string
         :documentation "The backend user.")
   (port :initarg :port
         :initform t
         :type t
         :custom string
         :documentation "The backend protocol.")
   (data :initarg :data
         :initform nil
         :documentation "Internal backend data.")
   (create-function :initarg :create-function
                    :initform ignore
                    :type function
                    :custom function
                    :documentation "The create function.")
   (search-function :initarg :search-function
                    :initform ignore
                    :type function
                    :custom function
                    :documentation "The search function.")))

(defcustom auth-source-protocols '((imap "imap" "imaps" "143" "993")
                                   (pop3 "pop3" "pop" "pop3s" "110" "995")
                                   (ssh  "ssh" "22")
                                   (sftp "sftp" "115")
                                   (smtp "smtp" "25"))
  "List of authentication protocols and their names"

  :group 'auth-source
  :version "23.2" ;; No Gnus
  :type '(repeat :tag "Authentication Protocols"
                 (cons :tag "Protocol Entry"
                       (symbol :tag "Protocol")
                       (repeat :tag "Names"
                               (string :tag "Name")))))

;;; generate all the protocols in a format Customize can use
;;; TODO: generate on the fly from auth-source-protocols
(defconst auth-source-protocols-customize
  (mapcar (lambda (a)
            (let ((p (car-safe a)))
              (list 'const
                    :tag (upcase (symbol-name p))
                    p)))
          auth-source-protocols))

(defvar auth-source-creation-defaults nil
  "Defaults for creating token values.  Usually let-bound.")

(defvar auth-source-creation-prompts nil
  "Default prompts for token values.  Usually let-bound.")

(make-obsolete 'auth-source-hide-passwords nil "Emacs 24.1")

(defcustom auth-source-save-behavior 'ask
  "If set, auth-source will respect it for save behavior."
  :group 'auth-source
  :version "23.2" ;; No Gnus
  :type `(choice
          :tag "auth-source new token save behavior"
          (const :tag "Always save" t)
          (const :tag "Never save" nil)
          (const :tag "Ask" ask)))

;; TODO: make the default (setq auth-source-netrc-use-gpg-tokens `((,(if (boundp 'epa-file-auto-mode-alist-entry) (car (symbol-value 'epa-file-auto-mode-alist-entry)) "\\.gpg\\'") never) (t gpg)))
;; TODO: or maybe leave as (setq auth-source-netrc-use-gpg-tokens 'never)

(defcustom auth-source-netrc-use-gpg-tokens 'never
  "Set this to tell auth-source when to create GPG password
tokens in netrc files.  It's either an alist or `never'.
Note that if EPA/EPG is not available, this should NOT be used."
  :group 'auth-source
  :version "23.2" ;; No Gnus
  :type `(choice
          (const :tag "Always use GPG password tokens" (t gpg))
          (const :tag "Never use GPG password tokens" never)
          (repeat :tag "Use a lookup list"
                  (list
                   (choice :tag "Matcher"
                           (const :tag "Match anything" t)
                           (const :tag "The EPA encrypted file extensions"
                                  ,(if (boundp 'epa-file-auto-mode-alist-entry)
                                       (car (symbol-value
                                             'epa-file-auto-mode-alist-entry))
                                     "\\.gpg\\'"))
                           (regexp :tag "Regular expression"))
                   (choice :tag "What to do"
                           (const :tag "Save GPG-encrypted password tokens" gpg)
                           (const :tag "Don't encrypt tokens" never))))))

(defvar auth-source-magic "auth-source-magic ")

(defcustom auth-source-do-cache t
  "Whether auth-source should cache information with `password-cache'."
  :group 'auth-source
  :version "23.2" ;; No Gnus
  :type `boolean)

(defcustom auth-source-debug nil
  "Whether auth-source should log debug messages.

If the value is nil, debug messages are not logged.

If the value is t, debug messages are logged with `message'.  In
that case, your authentication data will be in the clear (except
for passwords).

If the value is a function, debug messages are logged by calling
 that function using the same arguments as `message'."
  :group 'auth-source
  :version "23.2" ;; No Gnus
  :type `(choice
          :tag "auth-source debugging mode"
          (const :tag "Log using `message' to the *Messages* buffer" t)
          (const :tag "Log all trivia with `message' to the *Messages* buffer"
                 trivia)
          (function :tag "Function that takes arguments like `message'")
          (const :tag "Don't log anything" nil)))

(defcustom auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")
  "List of authentication sources.

The default will get login and password information from
\"~/.authinfo.gpg\", which you should set up with the EPA/EPG
packages to be encrypted.  If that file doesn't exist, it will
try the unencrypted version \"~/.authinfo\" and the famous
\"~/.netrc\" file.

See the auth.info manual for details.

Each entry is the authentication type with optional properties.

It's best to customize this with `M-x customize-variable' because the choices
can get pretty complex."
  :group 'auth-source
  :version "24.1" ;; No Gnus
  :type `(repeat :tag "Authentication Sources"
                 (choice
                  (string :tag "Just a file")
                  (const :tag "Default Secrets API Collection" 'default)
                  (const :tag "Login Secrets API Collection" "secrets:Login")
                  (const :tag "Temp Secrets API Collection" "secrets:session")
                  (list :tag "Source definition"
                        (const :format "" :value :source)
                        (choice :tag "Authentication backend choice"
                                (string :tag "Authentication Source (file)")
                                (list
                                 :tag "Secret Service API/KWallet/GNOME Keyring"
                                 (const :format "" :value :secrets)
                                 (choice :tag "Collection to use"
                                         (string :tag "Collection name")
                                         (const :tag "Default" 'default)
                                         (const :tag "Login" "Login")
                                         (const
                                          :tag "Temporary" "session"))))
                        (repeat :tag "Extra Parameters" :inline t
                                (choice :tag "Extra parameter"
                                        (list
                                         :tag "Host"
                                         (const :format "" :value :host)
                                         (choice :tag "Host (machine) choice"
                                                 (const :tag "Any" t)
                                                 (regexp
                                                  :tag "Regular expression")))
                                        (list
                                         :tag "Protocol"
                                         (const :format "" :value :port)
                                         (choice
                                          :tag "Protocol"
                                          (const :tag "Any" t)
                                          ,@auth-source-protocols-customize))
                                        (list :tag "User" :inline t
                                              (const :format "" :value :user)
                                              (choice
                                               :tag "Personality/Username"
                                               (const :tag "Any" t)
                                               (string
                                                :tag "Name")))))))))

(defcustom auth-source-gpg-encrypt-to t
  "List of recipient keys that `authinfo.gpg' encrypted to.
If the value is not a list, symmetric encryption will be used."
  :group 'auth-source
  :version "24.1" ;; No Gnus
  :type '(choice (const :tag "Symmetric encryption" t)
                 (repeat :tag "Recipient public keys"
                         (string :tag "Recipient public key"))))

;; temp for debugging
;; (unintern 'auth-source-protocols)
;; (unintern 'auth-sources)
;; (customize-variable 'auth-sources)
;; (setq auth-sources nil)
;; (format "%S" auth-sources)
;; (customize-variable 'auth-source-protocols)
;; (setq auth-source-protocols nil)
;; (format "%S" auth-source-protocols)
;; (auth-source-pick nil :host "a" :port 'imap)
;; (auth-source-user-or-password "login" "imap.myhost.com" 'imap)
;; (auth-source-user-or-password "password" "imap.myhost.com" 'imap)
;; (auth-source-user-or-password-imap "login" "imap.myhost.com")
;; (auth-source-user-or-password-imap "password" "imap.myhost.com")
;; (auth-source-protocol-defaults 'imap)

;; (let ((auth-source-debug 'debug)) (auth-source-do-debug "hello"))
;; (let ((auth-source-debug t)) (auth-source-do-debug "hello"))
;; (let ((auth-source-debug nil)) (auth-source-do-debug "hello"))
(defun auth-source-do-debug (&rest msg)
  (when auth-source-debug
    (apply 'auth-source-do-warn msg)))

(defun auth-source-do-trivia (&rest msg)
  (when (or (eq auth-source-debug 'trivia)
            (functionp auth-source-debug))
    (apply 'auth-source-do-warn msg)))

(defun auth-source-do-warn (&rest msg)
  (apply
   ;; set logger to either the function in auth-source-debug or 'message
   ;; note that it will be 'message if auth-source-debug is nil
   (if (functionp auth-source-debug)
       auth-source-debug
     'message)
   msg))


;;; (auth-source-read-char-choice "enter choice? " '(?a ?b ?q))
(defun auth-source-read-char-choice (prompt choices)
  "Read one of CHOICES by `read-char-choice', or `read-char'.
`dropdown-list' support is disabled because it doesn't work reliably.
Only one of CHOICES will be returned.  The PROMPT is augmented
with \"[a/b/c] \" if CHOICES is '\(?a ?b ?c\)."
  (when choices
    (let* ((prompt-choices
            (apply 'concat (loop for c in choices
                                 collect (format "%c/" c))))
           (prompt-choices (concat "[" (substring prompt-choices 0 -1) "] "))
           (full-prompt (concat prompt prompt-choices))
           k)

      (while (not (memq k choices))
        (setq k (cond
                 ((fboundp 'read-char-choice)
                  (read-char-choice full-prompt choices))
                 (t (message "%s" full-prompt)
                    (setq k (read-char))))))
      k)))

;; (auth-source-pick nil :host "any" :port 'imap :user "joe")
;; (auth-source-pick t :host "any" :port 'imap :user "joe")
;; (setq auth-sources '((:source (:secrets default) :host t :port t :user "joe")
;;                   (:source (:secrets "session") :host t :port t :user "joe")
;;                   (:source (:secrets "Login") :host t :port t)
;;                   (:source "~/.authinfo.gpg" :host t :port t)))

;; (setq auth-sources '((:source (:secrets default) :host t :port t :user "joe")
;;                   (:source (:secrets "session") :host t :port t :user "joe")
;;                   (:source (:secrets "Login") :host t :port t)
;;                   ))

;; (setq auth-sources '((:source "~/.authinfo.gpg" :host t :port t)))

;; (auth-source-backend-parse "myfile.gpg")
;; (auth-source-backend-parse 'default)
;; (auth-source-backend-parse "secrets:Login")

(defun auth-source-backend-parse (entry)
  "Creates an auth-source-backend from an ENTRY in `auth-sources'."
  (auth-source-backend-parse-parameters
   entry
   (cond
    ;; take 'default and recurse to get it as a Secrets API default collection
    ;; matching any user, host, and protocol
    ((eq entry 'default)
     (auth-source-backend-parse '(:source (:secrets default))))
    ;; take secrets:XYZ and recurse to get it as Secrets API collection "XYZ"
    ;; matching any user, host, and protocol
    ((and (stringp entry) (string-match "^secrets:\\(.+\\)" entry))
     (auth-source-backend-parse `(:source (:secrets ,(match-string 1 entry)))))
    ;; take just a file name and recurse to get it as a netrc file
    ;; matching any user, host, and protocol
    ((stringp entry)
     (auth-source-backend-parse `(:source ,entry)))

    ;; a file name with parameters
    ((stringp (plist-get entry :source))
     (if (equal (file-name-extension (plist-get entry :source)) "plist")
         (auth-source-backend
          (plist-get entry :source)
          :source (plist-get entry :source)
          :type 'plstore
          :search-function 'auth-source-plstore-search
          :create-function 'auth-source-plstore-create
          :data (plstore-open (plist-get entry :source)))
       (auth-source-backend
        (plist-get entry :source)
        :source (plist-get entry :source)
        :type 'netrc
        :search-function 'auth-source-netrc-search
        :create-function 'auth-source-netrc-create)))

    ;; the Secrets API.  We require the package, in order to have a
    ;; defined value for `secrets-enabled'.
    ((and
      (not (null (plist-get entry :source))) ; the source must not be nil
      (listp (plist-get entry :source))      ; and it must be a list
      (require 'secrets nil t)               ; and we must load the Secrets API
      secrets-enabled)                       ; and that API must be enabled

     ;; the source is either the :secrets key in ENTRY or
     ;; if that's missing or nil, it's "session"
     (let ((source (or (plist-get (plist-get entry :source) :secrets)
                       "session")))

       ;; if the source is a symbol, we look for the alias named so,
       ;; and if that alias is missing, we use "Login"
       (when (symbolp source)
         (setq source (or (secrets-get-alias (symbol-name source))
                          "Login")))

       (if (featurep 'secrets)
           (auth-source-backend
            (format "Secrets API (%s)" source)
            :source source
            :type 'secrets
            :search-function 'auth-source-secrets-search
            :create-function 'auth-source-secrets-create)
         (auth-source-do-warn
          "auth-source-backend-parse: no Secrets API, ignoring spec: %S" entry)
         (auth-source-backend
          (format "Ignored Secrets API (%s)" source)
          :source ""
          :type 'ignore))))

    ;; none of them
    (t
     (auth-source-do-warn
      "auth-source-backend-parse: invalid backend spec: %S" entry)
     (auth-source-backend
      "Empty"
      :source ""
      :type 'ignore)))))

(defun auth-source-backend-parse-parameters (entry backend)
  "Fills in the extra auth-source-backend parameters of ENTRY.
Using the plist ENTRY, get the :host, :port, and :user search
parameters."
  (let ((entry (if (stringp entry)
                   nil
                 entry))
        val)
    (when (setq val (plist-get entry :host))
      (oset backend host val))
    (when (setq val (plist-get entry :user))
      (oset backend user val))
    (when (setq val (plist-get entry :port))
      (oset backend port val)))
  backend)

;; (mapcar 'auth-source-backend-parse auth-sources)

(defun* auth-source-search (&rest spec
                                  &key type max host user port secret
                                  require create delete
                                  &allow-other-keys)
  "Search or modify authentication backends according to SPEC.

This function parses `auth-sources' for matches of the SPEC
plist.  It can optionally create or update an authentication
token if requested.  A token is just a standard Emacs property
list with a :secret property that can be a function; all the
other properties will always hold scalar values.

Typically the :secret property, if present, contains a password.

Common search keys are :max, :host, :port, and :user.  In
addition, :create specifies how tokens will be or created.
Finally, :type can specify which backend types you want to check.

A string value is always matched literally.  A symbol is matched
as its string value, literally.  All the SPEC values can be
single values (symbol or string) or lists thereof (in which case
any of the search terms matches).

:create t means to create a token if possible.

A new token will be created if no matching tokens were found.
The new token will have only the keys the backend requires.  For
the netrc backend, for instance, that's the user, host, and
port keys.

Here's an example:

\(let ((auth-source-creation-defaults '((user . \"defaultUser\")
                                        (A    . \"default A\"))))
  (auth-source-search :host \"mine\" :type 'netrc :max 1
                      :P \"pppp\" :Q \"qqqq\"
                      :create t))

which says:

\"Search for any entry matching host 'mine' in backends of type
 'netrc', maximum one result.

 Create a new entry if you found none.  The netrc backend will
 automatically require host, user, and port.  The host will be
 'mine'.  We prompt for the user with default 'defaultUser' and
 for the port without a default.  We will not prompt for A, Q,
 or P.  The resulting token will only have keys user, host, and
 port.\"

:create '(A B C) also means to create a token if possible.

The behavior is like :create t but if the list contains any
parameter, that parameter will be required in the resulting
token.  The value for that parameter will be obtained from the
search parameters or from user input.  If any queries are needed,
the alist `auth-source-creation-defaults' will be checked for the
default value.  If the user, host, or port are missing, the alist
`auth-source-creation-prompts' will be used to look up the
prompts IN THAT ORDER (so the 'user prompt will be queried first,
then 'host, then 'port, and finally 'secret).  Each prompt string
can use %u, %h, and %p to show the user, host, and port.

Here's an example:

\(let ((auth-source-creation-defaults '((user . \"defaultUser\")
                                        (A    . \"default A\")))
       (auth-source-creation-prompts
        '((password . \"Enter IMAP password for %h:%p: \"))))
  (auth-source-search :host '(\"nonesuch\" \"twosuch\") :type 'netrc :max 1
                      :P \"pppp\" :Q \"qqqq\"
                      :create '(A B Q)))

which says:

\"Search for any entry matching host 'nonesuch'
 or 'twosuch' in backends of type 'netrc', maximum one result.

 Create a new entry if you found none.  The netrc backend will
 automatically require host, user, and port.  The host will be
 'nonesuch' and Q will be 'qqqq'.  We prompt for the password
 with the shown prompt.  We will not prompt for Q.  The resulting
 token will have keys user, host, port, A, B, and Q.  It will not
 have P with any value, even though P is used in the search to
 find only entries that have P set to 'pppp'.\"

When multiple values are specified in the search parameter, the
user is prompted for which one.  So :host (X Y Z) would ask the
user to choose between X, Y, and Z.

This creation can fail if the search was not specific enough to
create a new token (it's up to the backend to decide that).  You
should `catch' the backend-specific error as usual.  Some
backends (netrc, at least) will prompt the user rather than throw
an error.

:require (A B C) means that only results that contain those
tokens will be returned.  Thus for instance requiring :secret
will ensure that any results will actually have a :secret
property.

:delete t means to delete any found entries.  nil by default.
Use `auth-source-delete' in ELisp code instead of calling
`auth-source-search' directly with this parameter.

:type (X Y Z) will check only those backend types.  'netrc and
'secrets are the only ones supported right now.

:max N means to try to return at most N items (defaults to 1).
When 0 the function will return just t or nil to indicate if any
matches were found.  More than N items may be returned, depending
on the search and the backend.

:host (X Y Z) means to match only hosts X, Y, or Z according to
the match rules above.  Defaults to t.

:user (X Y Z) means to match only users X, Y, or Z according to
the match rules above.  Defaults to t.

:port (P Q R) means to match only protocols P, Q, or R.
Defaults to t.

:K (V1 V2 V3) for any other key K will match values V1, V2, or
V3 (note the match rules above).

The return value is a list with at most :max tokens.  Each token
is a plist with keys :backend :host :port :user, plus any other
keys provided by the backend (notably :secret).  But note the
exception for :max 0, which see above.

The token can hold a :save-function key.  If you call that, the
user will be prompted to save the data to the backend.  You can't
request that this should happen right after creation, because
`auth-source-search' has no way of knowing if the token is
actually useful.  So the caller must arrange to call this function.

The token's :secret key can hold a function.  In that case you
must call it to obtain the actual value."
  (let* ((backends (mapcar 'auth-source-backend-parse auth-sources))
         (max (or max 1))
         (ignored-keys '(:require :create :delete :max))
         (keys (loop for i below (length spec) by 2
                     unless (memq (nth i spec) ignored-keys)
                     collect (nth i spec)))
         (cached (auth-source-remembered-p spec))
         ;; note that we may have cached results but found is still nil
         ;; (there were no results from the search)
         (found (auth-source-recall spec))
         filtered-backends accessor-key backend)

    (if (and cached auth-source-do-cache)
        (auth-source-do-debug
         "auth-source-search: found %d CACHED results matching %S"
         (length found) spec)

      (assert
       (or (eq t create) (listp create)) t
       "Invalid auth-source :create parameter (must be t or a list): %s %s")

      (assert
       (listp require) t
       "Invalid auth-source :require parameter (must be a list): %s")

      (setq filtered-backends (copy-sequence backends))
      (dolist (backend backends)
        (dolist (key keys)
          ;; ignore invalid slots
          (condition-case signal
              (unless (eval `(auth-source-search-collection
                              (plist-get spec key)
                              (oref backend ,key)))
                (setq filtered-backends (delq backend filtered-backends))
                (return))
            (invalid-slot-name))))

      (auth-source-do-trivia
       "auth-source-search: found %d backends matching %S"
       (length filtered-backends) spec)

      ;; (debug spec "filtered" filtered-backends)
      ;; First go through all the backends without :create, so we can
      ;; query them all.
      (setq found (auth-source-search-backends filtered-backends
                                               spec
                                               ;; to exit early
                                               max
                                               ;; create is always nil here
                                               nil delete
                                               require))

      (auth-source-do-debug
       "auth-source-search: found %d results (max %d) matching %S"
       (length found) max spec)

      ;; If we didn't find anything, then we allow the backend(s) to
      ;; create the entries.
      (when (and create
                 (not found))
        (setq found (auth-source-search-backends filtered-backends
                                                 spec
                                                 ;; to exit early
                                                 max
                                                 create delete
                                                 require))
        (auth-source-do-debug
         "auth-source-search: CREATED %d results (max %d) matching %S"
         (length found) max spec))

      ;; note we remember the lack of result too, if it's applicable
      (when auth-source-do-cache
        (auth-source-remember spec found)))

    found))

(defun auth-source-search-backends (backends spec max create delete require)
  (let (matches)
    (dolist (backend backends)
      (when (> max (length matches))   ; when we need more matches...
        (let* ((bmatches (apply
                          (slot-value backend 'search-function)
                          :backend backend
                          ;; note we're overriding whatever the spec
                          ;; has for :require, :create, and :delete
                          :require require
                          :create create
                          :delete delete
                          spec)))
          (when bmatches
            (auth-source-do-trivia
             "auth-source-search-backend: got %d (max %d) in %s:%s matching %S"
             (length bmatches) max
             (slot-value backend :type)
             (slot-value backend :source)
             spec)
            (setq matches (append matches bmatches))))))
    matches))

;;; (auth-source-search :max 1)
;;; (funcall (plist-get (nth 0 (auth-source-search :max 1)) :secret))
;;; (auth-source-search :host "nonesuch" :type 'netrc :K 1)
;;; (auth-source-search :host "nonesuch" :type 'secrets)

(defun* auth-source-delete (&rest spec
                                  &key delete
                                  &allow-other-keys)
  "Delete entries from the authentication backends according to SPEC.
Calls `auth-source-search' with the :delete property in SPEC set to t.
The backend may not actually delete the entries.

Returns the deleted entries."
  (auth-source-search (plist-put spec :delete t)))

(defun auth-source-search-collection (collection value)
  "Returns t is VALUE is t or COLLECTION is t or contains VALUE."
  (when (and (atom collection) (not (eq t collection)))
    (setq collection (list collection)))

  ;; (debug :collection collection :value value)
  (or (eq collection t)
      (eq value t)
      (equal collection value)
      (member value collection)))

(defvar auth-source-netrc-cache nil)

(defun auth-source-forget-all-cached ()
  "Forget all cached auth-source data."
  (interactive)
  (loop for sym being the symbols of password-data
        ;; when the symbol name starts with auth-source-magic
        when (string-match (concat "^" auth-source-magic)
                           (symbol-name sym))
        ;; remove that key
        do (password-cache-remove (symbol-name sym)))
  (setq auth-source-netrc-cache nil))

(defun auth-source-format-cache-entry (spec)
  "Format SPEC entry to put it in the password cache."
  (concat auth-source-magic (format "%S" spec)))

(defun auth-source-remember (spec found)
  "Remember FOUND search results for SPEC."
  (let ((password-cache-expiry auth-source-cache-expiry))
    (password-cache-add
     (auth-source-format-cache-entry spec) found)))

(defun auth-source-recall (spec)
  "Recall FOUND search results for SPEC."
  (password-read-from-cache (auth-source-format-cache-entry spec)))

(defun auth-source-remembered-p (spec)
  "Check if SPEC is remembered."
  (password-in-cache-p
   (auth-source-format-cache-entry spec)))

(defun auth-source-forget (spec)
  "Forget any cached data matching SPEC exactly.

This is the same SPEC you passed to `auth-source-search'.
Returns t or nil for forgotten or not found."
  (password-cache-remove (auth-source-format-cache-entry spec)))

;;; (loop for sym being the symbols of password-data when (string-match (concat "^" auth-source-magic) (symbol-name sym)) collect (symbol-name sym))

;;; (auth-source-remember '(:host "wedd") '(4 5 6))
;;; (auth-source-remembered-p '(:host "wedd"))
;;; (auth-source-remember '(:host "xedd") '(1 2 3))
;;; (auth-source-remembered-p '(:host "xedd"))
;;; (auth-source-remembered-p '(:host "zedd"))
;;; (auth-source-recall '(:host "xedd"))
;;; (auth-source-recall '(:host t))
;;; (auth-source-forget+ :host t)

(defun* auth-source-forget+ (&rest spec &allow-other-keys)
  "Forget any cached data matching SPEC.  Returns forgotten count.

This is not a full `auth-source-search' spec but works similarly.
For instance, \(:host \"myhost\" \"yourhost\") would find all the
cached data that was found with a search for those two hosts,
while \(:host t) would find all host entries."
  (let ((count 0)
        sname)
    (loop for sym being the symbols of password-data
          ;; when the symbol name matches with auth-source-magic
          when (and (setq sname (symbol-name sym))
                    (string-match (concat "^" auth-source-magic "\\(.+\\)")
                                  sname)
                    ;; and the spec matches what was stored in the cache
                    (auth-source-specmatchp spec (read (match-string 1 sname))))
          ;; remove that key
          do (progn
               (password-cache-remove sname)
               (incf count)))
    count))

(defun auth-source-specmatchp (spec stored)
  (let ((keys (loop for i below (length spec) by 2
                    collect (nth i spec))))
    (not (eq
          (dolist (key keys)
            (unless (auth-source-search-collection (plist-get stored key)
                                                   (plist-get spec key))
              (return 'no)))
          'no))))

;;; (auth-source-pick-first-password :host "z.lifelogs.com")
;;; (auth-source-pick-first-password :port "imap")
(defun auth-source-pick-first-password (&rest spec)
  "Pick the first secret found from applying SPEC to `auth-source-search'."
  (let* ((result (nth 0 (apply 'auth-source-search (plist-put spec :max 1))))
         (secret (plist-get result :secret)))

    (if (functionp secret)
        (funcall secret)
      secret)))

;; (auth-source-format-prompt "test %u %h %p" '((?u "user") (?h "host")))
(defun auth-source-format-prompt (prompt alist)
  "Format PROMPT using %x (for any character x) specifiers in ALIST."
  (dolist (cell alist)
    (let ((c (nth 0 cell))
          (v (nth 1 cell)))
      (when (and c v)
        (setq prompt (replace-regexp-in-string (format "%%%c" c)
                                               (format "%s" v)
                                               prompt)))))
  prompt)

(defun auth-source-ensure-strings (values)
  (unless (listp values)
    (setq values (list values)))
  (mapcar (lambda (value)
            (if (numberp value)
                (format "%s" value)
              value))
          values))

;;; Backend specific parsing: netrc/authinfo backend

;;; (auth-source-netrc-parse "~/.authinfo.gpg")
(defun* auth-source-netrc-parse (&rest
                                 spec
                                 &key file max host user port delete require
                                 &allow-other-keys)
  "Parse FILE and return a list of all entries in the file.
Note that the MAX parameter is used so we can exit the parse early."
  (if (listp file)
      ;; We got already parsed contents; just return it.
      file
    (when (file-exists-p file)
      (setq port (auth-source-ensure-strings port))
      (with-temp-buffer
        (let* ((tokens '("machine" "host" "default" "login" "user"
                         "password" "account" "macdef" "force"
                         "port" "protocol"))
               (max (or max 5000))       ; sanity check: default to stop at 5K
               (modified 0)
               (cached (cdr-safe (assoc file auth-source-netrc-cache)))
               (cached-mtime (plist-get cached :mtime))
               (cached-secrets (plist-get cached :secret))
               alist elem result pair)

          (if (and (functionp cached-secrets)
                   (equal cached-mtime
                          (nth 5 (file-attributes file))))
              (progn
                (auth-source-do-trivia
                 "auth-source-netrc-parse: using CACHED file data for %s"
                 file)
                (insert (funcall cached-secrets)))
            (insert-file-contents file)
            ;; cache all netrc files (used to be just .gpg files)
            ;; Store the contents of the file heavily encrypted in memory.
            ;; (note for the irony-impaired: they are just obfuscated)
            (aput 'auth-source-netrc-cache file
                  (list :mtime (nth 5 (file-attributes file))
                        :secret (lexical-let ((v (mapcar '1+ (buffer-string))))
                                  (lambda () (apply 'string (mapcar '1- v)))))))
          (goto-char (point-min))
          ;; Go through the file, line by line.
          (while (and (not (eobp))
                      (> max 0))

            (narrow-to-region (point) (point-at-eol))
            ;; For each line, get the tokens and values.
            (while (not (eobp))
              (skip-chars-forward "\t ")
              ;; Skip lines that begin with a "#".
              (if (eq (char-after) ?#)
                  (goto-char (point-max))
                (unless (eobp)
                  (setq elem
                        (if (= (following-char) ?\")
                            (read (current-buffer))
                          (buffer-substring
                           (point) (progn (skip-chars-forward "^\t ")
                                          (point)))))
                  (cond
                   ((equal elem "macdef")
                    ;; We skip past the macro definition.
                    (widen)
                    (while (and (zerop (forward-line 1))
                                (looking-at "$")))
                    (narrow-to-region (point) (point)))
                   ((member elem tokens)
                    ;; Tokens that don't have a following value are ignored,
                    ;; except "default".
                    (when (and pair (or (cdr pair)
                                        (equal (car pair) "default")))
                      (push pair alist))
                    (setq pair (list elem)))
                   (t
                    ;; Values that haven't got a preceding token are ignored.
                    (when pair
                      (setcdr pair elem)
                      (push pair alist)
                      (setq pair nil)))))))

            (when (and alist
                       (> max 0)
                       (auth-source-search-collection
                        host
                        (or
                         (aget alist "machine")
                         (aget alist "host")
                         t))
                       (auth-source-search-collection
                        user
                        (or
                         (aget alist "login")
                         (aget alist "account")
                         (aget alist "user")
                         t))
                       (auth-source-search-collection
                        port
                        (or
                         (aget alist "port")
                         (aget alist "protocol")
                         t))
                       (or
                        ;; the required list of keys is nil, or
                        (null require)
                        ;; every element of require is in the normalized list
                        (let ((normalized (nth 0 (auth-source-netrc-normalize
                                                  (list alist) file))))
                          (loop for req in require
                                always (plist-get normalized req)))))
              (decf max)
              (push (nreverse alist) result)
              ;; to delete a line, we just comment it out
              (when delete
                (goto-char (point-min))
                (insert "#")
                (incf modified)))
            (setq alist nil
                  pair nil)
            (widen)
            (forward-line 1))

          (when (< 0 modified)
            (when auth-source-gpg-encrypt-to
              ;; (see bug#7487) making `epa-file-encrypt-to' local to
              ;; this buffer lets epa-file skip the key selection query
              ;; (see the `local-variable-p' check in
              ;; `epa-file-write-region').
              (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
                (make-local-variable 'epa-file-encrypt-to))
              (if (listp auth-source-gpg-encrypt-to)
                  (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))

            ;; ask AFTER we've successfully opened the file
            (when (y-or-n-p (format "Save file %s? (%d deletions)"
                                    file modified))
              (write-region (point-min) (point-max) file nil 'silent)
              (auth-source-do-debug
               "auth-source-netrc-parse: modified %d lines in %s"
               modified file)))

          (nreverse result))))))

(defvar auth-source-passphrase-alist nil)

(defun auth-source-token-passphrase-callback-function (context key-id file)
  (let* ((file (file-truename file))
	 (entry (assoc file auth-source-passphrase-alist))
	 passphrase)
    ;; return the saved passphrase, calling a function if needed
    (or (copy-sequence (if (functionp (cdr entry))
			   (funcall (cdr entry))
			 (cdr entry)))
	(progn
	  (unless entry
	    (setq entry (list file))
	    (push entry auth-source-passphrase-alist))
	  (setq passphrase
		(read-passwd
		 (format "Passphrase for %s tokens: " file)
		 t))
	  (setcdr entry (lexical-let ((p (copy-sequence passphrase)))
			  (lambda () p)))
	  passphrase))))

;; (auth-source-epa-extract-gpg-token "gpg:LS0tLS1CRUdJTiBQR1AgTUVTU0FHRS0tLS0tClZlcnNpb246IEdudVBHIHYxLjQuMTEgKEdOVS9MaW51eCkKCmpBMEVBd01DT25qMjB1ak9rZnRneVI3K21iNm9aZWhuLzRad3cySkdlbnVaKzRpeEswWDY5di9icDI1U1dsQT0KPS9yc2wKLS0tLS1FTkQgUEdQIE1FU1NBR0UtLS0tLQo=" "~/.netrc")
(defun auth-source-epa-extract-gpg-token (secret file)
  "Pass either the decoded SECRET or the gpg:BASE64DATA version.
FILE is the file from which we obtained this token."
  (when (string-match "^gpg:\\(.+\\)" secret)
    (setq secret (base64-decode-string (match-string 1 secret))))
  (let ((context (epg-make-context 'OpenPGP))
        plain)
    (epg-context-set-passphrase-callback
     context
     (cons #'auth-source-token-passphrase-callback-function
           file))
    (epg-decrypt-string context secret)))

;; (insert (auth-source-epa-make-gpg-token "mysecret" "~/.netrc"))
(defun auth-source-epa-make-gpg-token (secret file)
  (let ((context (epg-make-context 'OpenPGP))
        (pp-escape-newlines nil)
        cipher)
    (epg-context-set-armor context t)
    (epg-context-set-passphrase-callback
     context
     (cons #'auth-source-token-passphrase-callback-function
           file))
    (setq cipher (epg-encrypt-string context secret nil))
    (with-temp-buffer
      (insert cipher)
      (base64-encode-region (point-min) (point-max) t)
      (concat "gpg:" (buffer-substring-no-properties
                      (point-min)
                      (point-max))))))

(defun auth-source-netrc-normalize (alist filename)
  (mapcar (lambda (entry)
            (let (ret item)
              (while (setq item (pop entry))
                (let ((k (car item))
                      (v (cdr item)))

                  ;; apply key aliases
                  (setq k (cond ((member k '("machine")) "host")
                                ((member k '("login" "account")) "user")
                                ((member k '("protocol")) "port")
                                ((member k '("password")) "secret")
                                (t k)))

                  ;; send back the secret in a function (lexical binding)
                  (when (equal k "secret")
                    (setq v (lexical-let ((lexv v)
                                          (token-decoder nil))
                              (when (string-match "^gpg:" lexv)
                                ;; it's a GPG token: create a token decoder
                                ;; which unsets itself once
                                (setq token-decoder
                                      (lambda (val)
                                        (prog1
                                            (auth-source-epa-extract-gpg-token
                                             val
                                             filename)
                                          (setq token-decoder nil)))))
                              (lambda ()
                                (when token-decoder
                                  (setq lexv (funcall token-decoder lexv)))
                                lexv))))
                  (setq ret (plist-put ret
                                       (intern (concat ":" k))
                                       v))))
              ret))
          alist))

;;; (setq secret (plist-get (nth 0 (auth-source-search :host t :type 'netrc :K 1 :max 1)) :secret))
;;; (funcall secret)

(defun* auth-source-netrc-search (&rest
                                  spec
                                  &key backend require create delete
                                  type max host user port
                                  &allow-other-keys)
  "Given a property list SPEC, return search matches from the :backend.
See `auth-source-search' for details on SPEC."
  ;; just in case, check that the type is correct (null or same as the backend)
  (assert (or (null type) (eq type (oref backend type)))
          t "Invalid netrc search: %s %s")

  (let ((results (auth-source-netrc-normalize
                  (auth-source-netrc-parse
                   :max max
                   :require require
                   :delete delete
                   :file (oref backend source)
                   :host (or host t)
                   :user (or user t)
                   :port (or port t))
                  (oref backend source))))

    ;; if we need to create an entry AND none were found to match
    (when (and create
               (not results))

      ;; create based on the spec and record the value
      (setq results (or
                     ;; if the user did not want to create the entry
                     ;; in the file, it will be returned
                     (apply (slot-value backend 'create-function) spec)
                     ;; if not, we do the search again without :create
                     ;; to get the updated data.

                     ;; the result will be returned, even if the search fails
                     (apply 'auth-source-netrc-search
                            (plist-put spec :create nil)))))
    results))

(defun auth-source-netrc-element-or-first (v)
  (if (listp v)
      (nth 0 v)
    v))

;;; (auth-source-search :host "nonesuch" :type 'netrc :max 1 :create t)
;;; (auth-source-search :host "nonesuch" :type 'netrc :max 1 :create t :create-extra-keys '((A "default A") (B)))

(defun* auth-source-netrc-create (&rest spec
                                        &key backend
                                        secret host user port create
                                        &allow-other-keys)
  (let* ((base-required '(host user port secret))
         ;; we know (because of an assertion in auth-source-search) that the
         ;; :create parameter is either t or a list (which includes nil)
         (create-extra (if (eq t create) nil create))
         (current-data (car (auth-source-search :max 1
                                                :host host
                                                :port port)))
         (required (append base-required create-extra))
         (file (oref backend source))
         (add "")
         ;; `valist' is an alist
         valist
         ;; `artificial' will be returned if no creation is needed
         artificial)

    ;; only for base required elements (defined as function parameters):
    ;; fill in the valist with whatever data we may have from the search
    ;; we complete the first value if it's a list and use the value otherwise
    (dolist (br base-required)
      (when (symbol-value br)
        (let ((br-choice (cond
                          ;; all-accepting choice (predicate is t)
                          ((eq t (symbol-value br)) nil)
                          ;; just the value otherwise
                          (t (symbol-value br)))))
          (when br-choice
            (aput 'valist br br-choice)))))

    ;; for extra required elements, see if the spec includes a value for them
    (dolist (er create-extra)
      (let ((name (concat ":" (symbol-name er)))
            (keys (loop for i below (length spec) by 2
                        collect (nth i spec))))
        (dolist (k keys)
          (when (equal (symbol-name k) name)
            (aput 'valist er (plist-get spec k))))))

    ;; for each required element
    (dolist (r required)
      (let* ((data (aget valist r))
             ;; take the first element if the data is a list
             (data (or (auth-source-netrc-element-or-first data)
                       (plist-get current-data
                                  (intern (format ":%s" r) obarray))))
             ;; this is the default to be offered
             (given-default (aget auth-source-creation-defaults r))
             ;; the default supplementals are simple:
             ;; for the user, try `given-default' and then (user-login-name);
             ;; otherwise take `given-default'
             (default (cond
                       ((and (not given-default) (eq r 'user))
                        (user-login-name))
                       (t given-default)))
             (printable-defaults (list
                                  (cons 'user
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (aget valist 'user))
                                         (plist-get artificial :user)
                                         "[any user]"))
                                  (cons 'host
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (aget valist 'host))
                                         (plist-get artificial :host)
                                         "[any host]"))
                                  (cons 'port
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (aget valist 'port))
                                         (plist-get artificial :port)
                                         "[any port]"))))
             (prompt (or (aget auth-source-creation-prompts r)
                         (case r
                           (secret "%p password for %u@%h: ")
                           (user "%p user name for %h: ")
                           (host "%p host name for user %u: ")
                           (port "%p port for %u@%h: "))
                         (format "Enter %s (%%u@%%h:%%p): " r)))
             (prompt (auth-source-format-prompt
                      prompt
                      `((?u ,(aget printable-defaults 'user))
                        (?h ,(aget printable-defaults 'host))
                        (?p ,(aget printable-defaults 'port))))))

        ;; Store the data, prompting for the password if needed.
        (setq data (or data
                       (if (eq r 'secret)
                           ;; Special case prompt for passwords.
                           ;; TODO: make the default (setq auth-source-netrc-use-gpg-tokens `((,(if (boundp 'epa-file-auto-mode-alist-entry) (car (symbol-value 'epa-file-auto-mode-alist-entry)) "\\.gpg\\'") nil) (t gpg)))
                           ;; TODO: or maybe leave as (setq auth-source-netrc-use-gpg-tokens 'never)
                           (let* ((ep (format "Use GPG password tokens in %s?" file))
                                  (gpg-encrypt
                                   (cond
                                    ((eq auth-source-netrc-use-gpg-tokens 'never)
                                     'never)
                                    ((listp auth-source-netrc-use-gpg-tokens)
                                     (let ((check (copy-sequence
                                                   auth-source-netrc-use-gpg-tokens))
                                           item ret)
                                       (while check
                                         (setq item (pop check))
                                         (when (or (eq (car item) t)
                                                   (string-match (car item) file))
                                           (setq ret (cdr item))
                                           (setq check nil)))))
                                    (t 'never)))
                                  (plain (or (eval default) (read-passwd prompt))))
                             ;; ask if we don't know what to do (in which case
                             ;; auth-source-netrc-use-gpg-tokens must be a list)
                             (unless gpg-encrypt
                               (setq gpg-encrypt (if (y-or-n-p ep) 'gpg 'never))
                               ;; TODO: save the defcustom now? or ask?
                               (setq auth-source-netrc-use-gpg-tokens
                                     (cons `(,file ,gpg-encrypt)
                                           auth-source-netrc-use-gpg-tokens)))
                             (if (eq gpg-encrypt 'gpg)
                                 (auth-source-epa-make-gpg-token plain file)
                               plain))
                         (if (stringp default)
                             (read-string (if (string-match ": *\\'" prompt)
                                              (concat (substring prompt 0 (match-beginning 0))
                                                      " (default " default "): ")
                                            (concat prompt "(default " default ") "))
                                          nil nil default)
                           (eval default)))))

        (when data
          (setq artificial (plist-put artificial
                                      (intern (concat ":" (symbol-name r)))
                                      (if (eq r 'secret)
                                          (lexical-let ((data data))
                                            (lambda () data))
                                        data))))

        ;; When r is not an empty string...
        (when (and (stringp data)
                   (< 0 (length data)))
          ;; this function is not strictly necessary but I think it
          ;; makes the code clearer -tzz
          (let ((printer (lambda ()
                           ;; append the key (the symbol name of r)
                           ;; and the value in r
                           (format "%s%s %s"
                                   ;; prepend a space
                                   (if (zerop (length add)) "" " ")
                                   ;; remap auth-source tokens to netrc
                                   (case r
                                     (user   "login")
                                     (host   "machine")
                                     (secret "password")
                                     (port   "port") ; redundant but clearer
                                     (t (symbol-name r)))
                                   (if (string-match "[\"# ]" data)
                                       (format "%S" data)
                                     data)))))
            (setq add (concat add (funcall printer)))))))

    (plist-put
     artificial
     :save-function
     (lexical-let ((file file)
                   (add add))
       (lambda () (auth-source-netrc-saver file add))))

    (list artificial)))

;;(funcall (plist-get (nth 0 (auth-source-search :host '("nonesuch2") :user "tzz" :port "imap" :create t :max 1)) :save-function))
(defun auth-source-netrc-saver (file add)
  "Save a line ADD in FILE, prompting along the way.
Respects `auth-source-save-behavior'.  Uses
`auth-source-netrc-cache' to avoid prompting more than once."
  (let* ((key (format "%s %s" file (rfc2104-hash 'md5 64 16 file add)))
         (cached (assoc key auth-source-netrc-cache)))

    (if cached
        (auth-source-do-trivia
         "auth-source-netrc-saver: found previous run for key %s, returning"
         key)
      (with-temp-buffer
        (when (file-exists-p file)
          (insert-file-contents file))
        (when auth-source-gpg-encrypt-to
          ;; (see bug#7487) making `epa-file-encrypt-to' local to
          ;; this buffer lets epa-file skip the key selection query
          ;; (see the `local-variable-p' check in
          ;; `epa-file-write-region').
          (unless (local-variable-p 'epa-file-encrypt-to (current-buffer))
            (make-local-variable 'epa-file-encrypt-to))
          (if (listp auth-source-gpg-encrypt-to)
              (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))
        ;; we want the new data to be found first, so insert at beginning
        (goto-char (point-min))

        ;; Ask AFTER we've successfully opened the file.
        (let ((prompt (format "Save auth info to file %s? " file))
              (done (not (eq auth-source-save-behavior 'ask)))
              (bufname "*auth-source Help*")
              k)
          (while (not done)
            (setq k (auth-source-read-char-choice prompt '(?y ?n ?N ?e ??)))
            (case k
              (?y (setq done t))
              (?? (save-excursion
                    (with-output-to-temp-buffer bufname
                      (princ
                       (concat "(y)es, save\n"
                               "(n)o but use the info\n"
                               "(N)o and don't ask to save again\n"
                               "(e)dit the line\n"
                               "(?) for help as you can see.\n"))
                      ;; Why?  Doesn't with-output-to-temp-buffer already do
                      ;; the exact same thing anyway?  --Stef
                      (set-buffer standard-output)
                      (help-mode))))
              (?n (setq add ""
                        done t))
              (?N
               (setq add ""
                     done t)
               (customize-save-variable 'auth-source-save-behavior nil))
              (?e (setq add (read-string "Line to add: " add)))
              (t nil)))

          (when (get-buffer-window bufname)
            (delete-window (get-buffer-window bufname)))

          ;; Make sure the info is not saved.
          (when (null auth-source-save-behavior)
            (setq add ""))

          (when (< 0 (length add))
            (progn
              (unless (bolp)
                (insert "\n"))
              (insert add "\n")
              (write-region (point-min) (point-max) file nil 'silent)
	      ;; Make the .authinfo file non-world-readable.
	      (set-file-modes file #o600)
              (auth-source-do-debug
               "auth-source-netrc-create: wrote 1 new line to %s"
               file)
              (message "Saved new authentication information to %s" file)
              nil))))
      (aput 'auth-source-netrc-cache key "ran"))))

;;; Backend specific parsing: Secrets API backend

;;; (let ((auth-sources '(default))) (auth-source-search :max 1 :create t))
;;; (let ((auth-sources '(default))) (auth-source-search :max 1 :delete t))
;;; (let ((auth-sources '(default))) (auth-source-search :max 1))
;;; (let ((auth-sources '(default))) (auth-source-search))
;;; (let ((auth-sources '("secrets:Login"))) (auth-source-search :max 1))
;;; (let ((auth-sources '("secrets:Login"))) (auth-source-search :max 1 :signon_realm "https://git.gnus.org/Git"))

(defun* auth-source-secrets-search (&rest
                                    spec
                                    &key backend create delete label
                                    type max host user port
                                    &allow-other-keys)
  "Search the Secrets API; spec is like `auth-source'.

The :label key specifies the item's label.  It is the only key
that can specify a substring.  Any :label value besides a string
will allow any label.

All other search keys must match exactly.  If you need substring
matching, do a wider search and narrow it down yourself.

You'll get back all the properties of the token as a plist.

Here's an example that looks for the first item in the 'Login'
Secrets collection:

 \(let ((auth-sources '(\"secrets:Login\")))
    (auth-source-search :max 1)

Here's another that looks for the first item in the 'Login'
Secrets collection whose label contains 'gnus':

 \(let ((auth-sources '(\"secrets:Login\")))
    (auth-source-search :max 1 :label \"gnus\")

And this one looks for the first item in the 'Login' Secrets
collection that's a Google Chrome entry for the git.gnus.org site
authentication tokens:

 \(let ((auth-sources '(\"secrets:Login\")))
    (auth-source-search :max 1 :signon_realm \"https://git.gnus.org/Git\"))
"

  ;; TODO
  (assert (not create) nil
          "The Secrets API auth-source backend doesn't support creation yet")
  ;; TODO
  ;; (secrets-delete-item coll elt)
  (assert (not delete) nil
          "The Secrets API auth-source backend doesn't support deletion yet")

  (let* ((coll (oref backend source))
         (max (or max 5000))     ; sanity check: default to stop at 5K
         (ignored-keys '(:create :delete :max :backend :label))
         (search-keys (loop for i below (length spec) by 2
                            unless (memq (nth i spec) ignored-keys)
                            collect (nth i spec)))
         ;; build a search spec without the ignored keys
         ;; if a search key is nil or t (match anything), we skip it
         (search-spec (apply 'append (mapcar
                                      (lambda (k)
                                        (if (or (null (plist-get spec k))
                                                (eq t (plist-get spec k)))
                                            nil
                                          (list k (plist-get spec k))))
                                      search-keys)))
         ;; needed keys (always including host, login, port, and secret)
         (returned-keys (mm-delete-duplicates (append
                                               '(:host :login :port :secret)
                                               search-keys)))
         (items (loop for item in (apply 'secrets-search-items coll search-spec)
                      unless (and (stringp label)
                                  (not (string-match label item)))
                      collect item))
         ;; TODO: respect max in `secrets-search-items', not after the fact
         (items (butlast items (- (length items) max)))
         ;; convert the item name to a full plist
         (items (mapcar (lambda (item)
                          (append
                           ;; make an entry for the secret (password) element
                           (list
                            :secret
                            (lexical-let ((v (secrets-get-secret coll item)))
                              (lambda () v)))
                           ;; rewrite the entry from ((k1 v1) (k2 v2)) to plist
                           (apply 'append
                                  (mapcar (lambda (entry)
                                            (list (car entry) (cdr entry)))
                                          (secrets-get-attributes coll item)))))
                        items))
         ;; ensure each item has each key in `returned-keys'
         (items (mapcar (lambda (plist)
                          (append
                           (apply 'append
                                  (mapcar (lambda (req)
                                            (if (plist-get plist req)
                                                nil
                                              (list req nil)))
                                          returned-keys))
                           plist))
                        items)))
    items))

(defun* auth-source-secrets-create (&rest
                                    spec
                                    &key backend type max host user port
                                    &allow-other-keys)
  ;; TODO
  ;; (apply 'secrets-create-item (auth-get-source entry) name passwd spec)
  (debug spec))

;;; Backend specific parsing: PLSTORE backend

(defun* auth-source-plstore-search (&rest
                                    spec
                                    &key backend create delete label
                                    type max host user port
                                    &allow-other-keys)
  "Search the PLSTORE; spec is like `auth-source'."
  (let* ((store (oref backend data))
         (max (or max 5000))     ; sanity check: default to stop at 5K
         (ignored-keys '(:create :delete :max :backend :require))
         (search-keys (loop for i below (length spec) by 2
                            unless (memq (nth i spec) ignored-keys)
                            collect (nth i spec)))
         ;; build a search spec without the ignored keys
         ;; if a search key is nil or t (match anything), we skip it
         (search-spec (apply 'append (mapcar
                                      (lambda (k)
                                        (let ((v (plist-get spec k)))
                                          (if (or (null v)
                                                  (eq t v))
                                              nil
                                            (if (stringp v)
                                                (setq v (list v)))
                                            (list k v))))
                                      search-keys)))
         ;; needed keys (always including host, login, port, and secret)
         (returned-keys (mm-delete-duplicates (append
                                               '(:host :login :port :secret)
                                               search-keys)))
         (items (plstore-find store search-spec))
         (item-names (mapcar #'car items))
         (items (butlast items (- (length items) max)))
         ;; convert the item to a full plist
         (items (mapcar (lambda (item)
                          (let* ((plist (copy-tree (cdr item)))
                                 (secret (plist-member plist :secret)))
                            (if secret
                                (setcar
                                 (cdr secret)
                                 (lexical-let ((v (car (cdr secret))))
                                   (lambda () v))))
                            plist))
                        items))
         ;; ensure each item has each key in `returned-keys'
         (items (mapcar (lambda (plist)
                          (append
                           (apply 'append
                                  (mapcar (lambda (req)
                                            (if (plist-get plist req)
                                                nil
                                              (list req nil)))
                                          returned-keys))
                           plist))
                        items)))
    (cond
     ;; if we need to create an entry AND none were found to match
     ((and create
           (not items))

      ;; create based on the spec and record the value
      (setq items (or
                   ;; if the user did not want to create the entry
                   ;; in the file, it will be returned
                   (apply (slot-value backend 'create-function) spec)
                   ;; if not, we do the search again without :create
                   ;; to get the updated data.

                   ;; the result will be returned, even if the search fails
                   (apply 'auth-source-plstore-search
                          (plist-put spec :create nil)))))
     ((and delete
           item-names)
      (dolist (item-name item-names)
        (plstore-delete store item-name))
      (plstore-save store)))
    items))

(defun* auth-source-plstore-create (&rest spec
                                          &key backend
                                          secret host user port create
                                          &allow-other-keys)
  (let* ((base-required '(host user port secret))
         (base-secret '(secret))
         ;; we know (because of an assertion in auth-source-search) that the
         ;; :create parameter is either t or a list (which includes nil)
         (create-extra (if (eq t create) nil create))
         (current-data (car (auth-source-search :max 1
                                                :host host
                                                :port port)))
         (required (append base-required create-extra))
         (file (oref backend source))
         (add "")
         ;; `valist' is an alist
         valist
         ;; `artificial' will be returned if no creation is needed
         artificial
         secret-artificial)

    ;; only for base required elements (defined as function parameters):
    ;; fill in the valist with whatever data we may have from the search
    ;; we complete the first value if it's a list and use the value otherwise
    (dolist (br base-required)
      (when (symbol-value br)
        (let ((br-choice (cond
                          ;; all-accepting choice (predicate is t)
                          ((eq t (symbol-value br)) nil)
                          ;; just the value otherwise
                          (t (symbol-value br)))))
          (when br-choice
            (aput 'valist br br-choice)))))

    ;; for extra required elements, see if the spec includes a value for them
    (dolist (er create-extra)
      (let ((name (concat ":" (symbol-name er)))
            (keys (loop for i below (length spec) by 2
                        collect (nth i spec))))
        (dolist (k keys)
          (when (equal (symbol-name k) name)
            (aput 'valist er (plist-get spec k))))))

    ;; for each required element
    (dolist (r required)
      (let* ((data (aget valist r))
             ;; take the first element if the data is a list
             (data (or (auth-source-netrc-element-or-first data)
                       (plist-get current-data
                                  (intern (format ":%s" r) obarray))))
             ;; this is the default to be offered
             (given-default (aget auth-source-creation-defaults r))
             ;; the default supplementals are simple:
             ;; for the user, try `given-default' and then (user-login-name);
             ;; otherwise take `given-default'
             (default (cond
                       ((and (not given-default) (eq r 'user))
                        (user-login-name))
                       (t given-default)))
             (printable-defaults (list
                                  (cons 'user
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (aget valist 'user))
                                         (plist-get artificial :user)
                                         "[any user]"))
                                  (cons 'host
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (aget valist 'host))
                                         (plist-get artificial :host)
                                         "[any host]"))
                                  (cons 'port
                                        (or
                                         (auth-source-netrc-element-or-first
                                          (aget valist 'port))
                                         (plist-get artificial :port)
                                         "[any port]"))))
             (prompt (or (aget auth-source-creation-prompts r)
                         (case r
                           (secret "%p password for %u@%h: ")
                           (user "%p user name for %h: ")
                           (host "%p host name for user %u: ")
                           (port "%p port for %u@%h: "))
                         (format "Enter %s (%%u@%%h:%%p): " r)))
             (prompt (auth-source-format-prompt
                      prompt
                      `((?u ,(aget printable-defaults 'user))
                        (?h ,(aget printable-defaults 'host))
                        (?p ,(aget printable-defaults 'port))))))

        ;; Store the data, prompting for the password if needed.
        (setq data (or data
                       (if (eq r 'secret)
                           (or (eval default) (read-passwd prompt))
                         (if (stringp default)
                             (read-string (if (string-match ": *\\'" prompt)
                                              (concat (substring prompt 0 (match-beginning 0))
                                                      " (default " default "): ")
                                            (concat prompt "(default " default ") "))
                                          nil nil default)
                           (eval default)))))

        (when data
          (if (member r base-secret)
              (setq secret-artificial
                    (plist-put secret-artificial
                               (intern (concat ":" (symbol-name r)))
                               data))
            (setq artificial (plist-put artificial
                                        (intern (concat ":" (symbol-name r)))
                                        data))))))
    (plstore-put (oref backend data)
                 (sha1 (format "%s@%s:%s"
                               (plist-get artificial :user)
                               (plist-get artificial :host)
                               (plist-get artificial :port)))
                 artificial secret-artificial)
    (if (y-or-n-p (format "Save auth info to file %s? "
                          (plstore-get-file (oref backend data))))
        (plstore-save (oref backend data)))))

;;; older API

;;; (auth-source-user-or-password '("login" "password") "imap.myhost.com" t "tzz")

;; deprecate the old interface
(make-obsolete 'auth-source-user-or-password
               'auth-source-search "Emacs 24.1")
(make-obsolete 'auth-source-forget-user-or-password
               'auth-source-forget "Emacs 24.1")

(defun auth-source-user-or-password
  (mode host port &optional username create-missing delete-existing)
  "Find MODE (string or list of strings) matching HOST and PORT.

DEPRECATED in favor of `auth-source-search'!

USERNAME is optional and will be used as \"login\" in a search
across the Secret Service API (see secrets.el) if the resulting
items don't have a username.  This means that if you search for
username \"joe\" and it matches an item but the item doesn't have
a :user attribute, the username \"joe\" will be returned.

A non nil DELETE-EXISTING means deleting any matching password
entry in the respective sources.  This is useful only when
CREATE-MISSING is non nil as well; the intended use case is to
remove wrong password entries.

If no matching entry is found, and CREATE-MISSING is non nil,
the password will be retrieved interactively, and it will be
stored in the password database which matches best (see
`auth-sources').

MODE can be \"login\" or \"password\"."
  (auth-source-do-debug
   "auth-source-user-or-password: DEPRECATED get %s for %s (%s) + user=%s"
   mode host port username)

  (let* ((listy (listp mode))
         (mode (if listy mode (list mode)))
         (cname (if username
                    (format "%s %s:%s %s" mode host port username)
                  (format "%s %s:%s" mode host port)))
         (search (list :host host :port port))
         (search (if username (append search (list :user username)) search))
         (search (if create-missing
                     (append search (list :create t))
                   search))
         (search (if delete-existing
                     (append search (list :delete t))
                   search))
         ;; (found (if (not delete-existing)
         ;;            (gethash cname auth-source-cache)
         ;;          (remhash cname auth-source-cache)
         ;;          nil)))
         (found nil))
    (if found
        (progn
          (auth-source-do-debug
           "auth-source-user-or-password: DEPRECATED cached %s=%s for %s (%s) + %s"
           mode
           ;; don't show the password
           (if (and (member "password" mode) t)
               "SECRET"
             found)
           host port username)
          found)                        ; return the found data
      ;; else, if not found, search with a max of 1
      (let ((choice (nth 0 (apply 'auth-source-search
                                  (append '(:max 1) search)))))
        (when choice
          (dolist (m mode)
            (cond
             ((equal "password" m)
              (push (if (plist-get choice :secret)
                        (funcall (plist-get choice :secret))
                      nil) found))
             ((equal "login" m)
              (push (plist-get choice :user) found)))))
        (setq found (nreverse found))
        (setq found (if listy found (car-safe found)))))

    found))

(defun auth-source-user-and-password (host &optional user)
  (let* ((auth-info (car
                     (if user
                         (auth-source-search
                          :host host
                          :user "yourusername"
                          :max 1
                          :require '(:user :secret)
                          :create nil)
                       (auth-source-search
                        :host host
                        :max 1
                        :require '(:user :secret)
                        :create nil))))
         (user (plist-get auth-info :user))
         (password (plist-get auth-info :secret)))
    (when (functionp password)
      (setq password (funcall password)))
    (list user password auth-info)))

(provide 'auth-source)

;;; auth-source.el ends here
