;;; sql.el --- specialized comint.el for SQL interpreters

;; Copyright (C) 1998-2012  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Michael Mauger <mmaug@yahoo.com>
;; Version: 3.0
;; Keywords: comm languages processes
;; URL: http://savannah.gnu.org/projects/emacs/

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

;; Please send bug reports and bug fixes to the mailing list at
;; help-gnu-emacs@gnu.org.  If you want to subscribe to the mailing
;; list, see the web page at
;; http://lists.gnu.org/mailman/listinfo/help-gnu-emacs for
;; instructions.  I monitor this list actively.  If you send an e-mail
;; to Alex Schroeder it usually makes it to me when Alex has a chance
;; to forward them along (Thanks, Alex).

;; This file provides a sql-mode and a sql-interactive-mode.  The
;; original goals were two simple modes providing syntactic
;; highlighting.  The interactive mode had to provide a command-line
;; history; the other mode had to provide "send region/buffer to SQL
;; interpreter" functions.  "simple" in this context means easy to
;; use, easy to maintain and little or no bells and whistles.  This
;; has changed somewhat as experience with the mode has accumulated.

;; Support for different flavors of SQL and command interpreters was
;; available in early versions of sql.el.  This support has been
;; extended and formalized in later versions.  Part of the impetus for
;; the improved support of SQL flavors was borne out of the current
;; maintainers consulting experience.  In the past twenty years, I
;; have used Oracle, Sybase, Informix, MySQL, Postgres, and SQLServer.
;; On some assignments, I have used two or more of these concurrently.

;; If anybody feels like extending this sql mode, take a look at the
;; above mentioned modes and write a sqlx-mode on top of this one.  If
;; this proves to be difficult, please suggest changes that will
;; facilitate your plans.  Facilities have been provided to add
;; products and product-specific configuration.

;; sql-interactive-mode is used to interact with a SQL interpreter
;; process in a SQLi buffer (usually called `*SQL*').  The SQLi buffer
;; is created by calling a SQL interpreter-specific entry function or
;; sql-product-interactive.  Do *not* call sql-interactive-mode by
;; itself.

;; The list of currently supported interpreters and the corresponding
;; entry function used to create the SQLi buffers is shown with
;; `sql-help' (M-x sql-help).

;; Since sql-interactive-mode is built on top of the general
;; command-interpreter-in-a-buffer mode (comint mode), it shares a
;; common base functionality, and a common set of bindings, with all
;; modes derived from comint mode.  This makes these modes easier to
;; use.

;; sql-mode can be used to keep editing SQL statements.  The SQL
;; statements can be sent to the SQL process in the SQLi buffer.

;; For documentation on the functionality provided by comint mode, and
;; the hooks available for customizing it, see the file `comint.el'.

;; Hint for newbies: take a look at `dabbrev-expand', `abbrev-mode', and
;; `imenu-add-menubar-index'.

;;; Requirements for Emacs 19.34:

;; If you are using Emacs 19.34, you will have to get and install
;; the file regexp-opt.el
;; <URL:ftp://ftp.ifi.uio.no/pub/emacs/emacs-20.3/lisp/emacs-lisp/regexp-opt.el>
;; and the custom package
;; <URL:http://www.dina.kvl.dk/~abraham/custom/>.

;;; Bugs:

;; sql-ms now uses osql instead of isql.  Osql flushes its error
;; stream more frequently than isql so that error messages are
;; available.  There is no prompt and some output still is buffered.
;; This improves the interaction under Emacs but it still is somewhat
;; awkward.

;; Quoted identifiers are not supported for highlighting.  Most
;; databases support the use of double quoted strings in place of
;; identifiers; ms (Microsoft SQLServer) also supports identifiers
;; enclosed within brackets [].

;;; Product Support:

;; To add support for additional SQL products the following steps
;; must be followed ("xyz" is the name of the product in the examples
;; below):

;; 1) Add the product to the list of known products.

;;     (sql-add-product 'xyz "XyzDB"
;;     	                '(:free-software t))

;; 2) Define font lock settings.  All ANSI keywords will be
;;    highlighted automatically, so only product specific keywords
;;    need to be defined here.

;;     (defvar my-sql-mode-xyz-font-lock-keywords
;;       '(("\\b\\(red\\|orange\\|yellow\\)\\b"
;;          . font-lock-keyword-face))
;;       "XyzDB SQL keywords used by font-lock.")

;;     (sql-set-product-feature 'xyz
;;                              :font-lock
;;                              'my-sql-mode-xyz-font-lock-keywords)

;; 3) Define any special syntax characters including comments and
;;    identifier characters.

;;     (sql-set-product-feature 'xyz
;;                              :syntax-alist ((?# . "_")))

;; 4) Define the interactive command interpreter for the database
;;    product.

;;     (defcustom my-sql-xyz-program "ixyz"
;;       "Command to start ixyz by XyzDB."
;;       :type 'file
;;       :group 'SQL)
;;
;;     (sql-set-product-feature 'xyz
;;                              :sqli-program 'my-sql-xyz-program)
;;     (sql-set-product-feature 'xyz
;;                              :prompt-regexp "^xyzdb> ")
;;     (sql-set-product-feature 'xyz
;;                              :prompt-length 7)

;; 5) Define login parameters and command line formatting.

;;     (defcustom my-sql-xyz-login-params '(user password server database)
;;       "Login parameters to needed to connect to XyzDB."
;;       :type 'sql-login-params
;;       :group 'SQL)
;;
;;     (sql-set-product-feature 'xyz
;;                              :sqli-login 'my-sql-xyz-login-params)

;;     (defcustom my-sql-xyz-options '("-X" "-Y" "-Z")
;;       "List of additional options for `sql-xyz-program'."
;;       :type '(repeat string)
;;       :group 'SQL)
;;
;;     (sql-set-product-feature 'xyz
;;                              :sqli-options 'my-sql-xyz-options))

;;     (defun my-sql-comint-xyz (product options)
;;       "Connect ti XyzDB in a comint buffer."
;;
;;         ;; Do something with `sql-user', `sql-password',
;;         ;; `sql-database', and `sql-server'.
;;         (let ((params options))
;;           (if (not (string= "" sql-server))
;;              (setq params (append (list "-S" sql-server) params)))
;;           (if (not (string= "" sql-database))
;;               (setq params (append (list "-D" sql-database) params)))
;;           (if (not (string= "" sql-password))
;;               (setq params (append (list "-P" sql-password) params)))
;;           (if (not (string= "" sql-user))
;;               (setq params (append (list "-U" sql-user) params)))
;;           (sql-comint product params)))
;;
;;     (sql-set-product-feature 'xyz
;;                              :sqli-comint-func 'my-sql-comint-xyz)

;; 6) Define a convenience function to invoke the SQL interpreter.

;;     (defun my-sql-xyz (&optional buffer)
;;       "Run ixyz by XyzDB as an inferior process."
;;       (interactive "P")
;;       (sql-product-interactive 'xyz buffer))

;;; To Do:

;; Improve keyword highlighting for individual products.  I have tried
;; to update those database that I use.  Feel free to send me updates,
;; or direct me to the reference manuals for your favorite database.

;; When there are no keywords defined, the ANSI keywords are
;; highlighted.  ANSI keywords are highlighted even if the keyword is
;; not used for your current product.  This should help identify
;; portability concerns.

;; Add different highlighting levels.

;; Add support for listing available tables or the columns in a table.

;;; Thanks to all the people who helped me out:

;; Alex Schroeder <alex@gnu.org> -- the original author
;; Kai Blauberg <kai.blauberg@metla.fi>
;; <ibalaban@dalet.com>
;; Yair Friedman <yfriedma@JohnBryce.Co.Il>
;; Gregor Zych <zych@pool.informatik.rwth-aachen.de>
;; nino <nino@inform.dk>
;; Berend de Boer <berend@pobox.com>
;; Adam Jenkins <adam@thejenkins.org>
;; Michael Mauger <mmaug@yahoo.com> -- improved product support
;; Drew Adams <drew.adams@oracle.com> -- Emacs 20 support
;; Harald Maier <maierh@myself.com> -- sql-send-string
;; Stefan Monnier <monnier@iro.umontreal.ca> -- font-lock corrections; code polish
;; Paul Sleigh <bat@flurf.net> -- MySQL keyword enhancement
;; Andrew Schein <andrew@andrewschein.com> -- sql-port bug



;;; Code:

(require 'comint)
;; Need the following to allow GNU Emacs 19 to compile the file.
(eval-when-compile
  (require 'regexp-opt))
(require 'custom)
(require 'thingatpt)
(eval-when-compile ;; needed in Emacs 19, 20
  (setq max-specpdl-size (max max-specpdl-size 2000)))

(defun sql-signum (n)
  "Return 1, 0, or -1 to identify the sign of N."
  (cond
   ((not (numberp n)) nil)
   ((< n 0) -1)
   ((> n 0) 1)
   (t 0)))

(defvar font-lock-keyword-face)
(defvar font-lock-set-defaults)
(defvar font-lock-string-face)

;;; Allow customization

(defgroup SQL nil
  "Running a SQL interpreter from within Emacs buffers."
  :version "20.4"
  :group 'languages
  :group 'processes)

;; These four variables will be used as defaults, if set.

(defcustom sql-user ""
  "Default username."
  :type 'string
  :group 'SQL
  :safe 'stringp)

(defcustom sql-password ""
  "Default password.

Storing your password in a textfile such as ~/.emacs could be dangerous.
Customizing your password will store it in your ~/.emacs file."
  :type 'string
  :group 'SQL
  :risky t)

(defcustom sql-database ""
  "Default database."
  :type 'string
  :group 'SQL
  :safe 'stringp)

(defcustom sql-server ""
  "Default server or host."
  :type 'string
  :group 'SQL
  :safe 'stringp)

(defcustom sql-port 0
  "Default port for connecting to a MySQL or Postgres server."
  :version "24.1"
  :type 'number
  :group 'SQL
  :safe 'numberp)

;; Login parameter type

(define-widget 'sql-login-params 'lazy
  "Widget definition of the login parameters list"
  ;; FIXME: does not implement :default property for the user,
  ;; database and server options.  Anybody have some guidance on how to
  ;; do this.
  :tag "Login Parameters"
  :type '(repeat (choice
                  (const user)
                  (const password)
                  (choice :tag "server"
                          (const server)
                          (list :tag "file"
                                (const :format "" server)
                                (const :format "" :file)
                                regexp)
                          (list :tag "completion"
                                (const :format "" server)
                                (const :format "" :completion)
                                (restricted-sexp
                                 :match-alternatives (listp stringp))))
                  (choice :tag "database"
                          (const database)
                          (list :tag "file"
                                (const :format "" database)
                                (const :format "" :file)
                                regexp)
                          (list :tag "completion"
                                (const :format "" database)
                                (const :format "" :completion)
                                (restricted-sexp
                                 :match-alternatives (listp stringp))))
                  (const port))))

;; SQL Product support

(defvar sql-interactive-product nil
  "Product under `sql-interactive-mode'.")

(defvar sql-connection nil
  "Connection name if interactive session started by `sql-connect'.")

(defvar sql-product-alist
  '((ansi
     :name "ANSI"
     :font-lock sql-mode-ansi-font-lock-keywords
     :statement sql-ansi-statement-starters)

    (db2
     :name "DB2"
     :font-lock sql-mode-db2-font-lock-keywords
     :sqli-program sql-db2-program
     :sqli-options sql-db2-options
     :sqli-login sql-db2-login-params
     :sqli-comint-func sql-comint-db2
     :prompt-regexp "^db2 => "
     :prompt-length 7
     :prompt-cont-regexp "^db2 (cont\.) => "
     :input-filter sql-escape-newlines-filter)

    (informix
     :name "Informix"
     :font-lock sql-mode-informix-font-lock-keywords
     :sqli-program sql-informix-program
     :sqli-options sql-informix-options
     :sqli-login sql-informix-login-params
     :sqli-comint-func sql-comint-informix
     :prompt-regexp "^> "
     :prompt-length 2
     :syntax-alist ((?{ . "<") (?} . ">")))

    (ingres
     :name "Ingres"
     :font-lock sql-mode-ingres-font-lock-keywords
     :sqli-program sql-ingres-program
     :sqli-options sql-ingres-options
     :sqli-login sql-ingres-login-params
     :sqli-comint-func sql-comint-ingres
     :prompt-regexp "^\* "
     :prompt-length 2
     :prompt-cont-regexp "^\* ")

    (interbase
     :name "Interbase"
     :font-lock sql-mode-interbase-font-lock-keywords
     :sqli-program sql-interbase-program
     :sqli-options sql-interbase-options
     :sqli-login sql-interbase-login-params
     :sqli-comint-func sql-comint-interbase
     :prompt-regexp "^SQL> "
     :prompt-length 5)

    (linter
     :name "Linter"
     :font-lock sql-mode-linter-font-lock-keywords
     :sqli-program sql-linter-program
     :sqli-options sql-linter-options
     :sqli-login sql-linter-login-params
     :sqli-comint-func sql-comint-linter
     :prompt-regexp "^SQL>"
     :prompt-length 4)

    (ms
     :name "Microsoft"
     :font-lock sql-mode-ms-font-lock-keywords
     :sqli-program sql-ms-program
     :sqli-options sql-ms-options
     :sqli-login sql-ms-login-params
     :sqli-comint-func sql-comint-ms
     :prompt-regexp "^[0-9]*>"
     :prompt-length 5
     :syntax-alist ((?@ . "_"))
     :terminator ("^go" . "go"))

    (mysql
     :name "MySQL"
     :free-software t
     :font-lock sql-mode-mysql-font-lock-keywords
     :sqli-program sql-mysql-program
     :sqli-options sql-mysql-options
     :sqli-login sql-mysql-login-params
     :sqli-comint-func sql-comint-mysql
     :list-all "SHOW TABLES;"
     :list-table "DESCRIBE %s;"
     :prompt-regexp "^mysql> "
     :prompt-length 6
     :prompt-cont-regexp "^    -> "
     :syntax-alist ((?# . "< b"))
     :input-filter sql-remove-tabs-filter)

    (oracle
     :name "Oracle"
     :font-lock sql-mode-oracle-font-lock-keywords
     :sqli-program sql-oracle-program
     :sqli-options sql-oracle-options
     :sqli-login sql-oracle-login-params
     :sqli-comint-func sql-comint-oracle
     :list-all sql-oracle-list-all
     :list-table sql-oracle-list-table
     :completion-object sql-oracle-completion-object
     :prompt-regexp "^SQL> "
     :prompt-length 5
     :prompt-cont-regexp "^\\s-*[[:digit:]]+  "
     :statement sql-oracle-statement-starters
     :syntax-alist ((?$ . "_") (?# . "_"))
     :terminator ("\\(^/\\|;\\)$" . "/")
     :input-filter sql-placeholders-filter)

    (postgres
     :name "Postgres"
     :free-software t
     :font-lock sql-mode-postgres-font-lock-keywords
     :sqli-program sql-postgres-program
     :sqli-options sql-postgres-options
     :sqli-login sql-postgres-login-params
     :sqli-comint-func sql-comint-postgres
     :list-all ("\\d+" . "\\dS+")
     :list-table ("\\d+ %s" . "\\dS+ %s")
     :completion-object sql-postgres-completion-object
     :prompt-regexp "^\\w*=[#>] "
     :prompt-length 5
     :prompt-cont-regexp "^\\w*[-(][#>] "
     :input-filter sql-remove-tabs-filter
     :terminator ("\\(^\\s-*\\\\g$\\|;\\)" . "\\g"))

    (solid
     :name "Solid"
     :font-lock sql-mode-solid-font-lock-keywords
     :sqli-program sql-solid-program
     :sqli-options sql-solid-options
     :sqli-login sql-solid-login-params
     :sqli-comint-func sql-comint-solid
     :prompt-regexp "^"
     :prompt-length 0)

    (sqlite
     :name "SQLite"
     :free-software t
     :font-lock sql-mode-sqlite-font-lock-keywords
     :sqli-program sql-sqlite-program
     :sqli-options sql-sqlite-options
     :sqli-login sql-sqlite-login-params
     :sqli-comint-func sql-comint-sqlite
     :list-all ".tables"
     :list-table ".schema %s"
     :completion-object sql-sqlite-completion-object
     :prompt-regexp "^sqlite> "
     :prompt-length 8
     :prompt-cont-regexp "^   \.\.\.> "
     :terminator ";")

    (sybase
     :name "Sybase"
     :font-lock sql-mode-sybase-font-lock-keywords
     :sqli-program sql-sybase-program
     :sqli-options sql-sybase-options
     :sqli-login sql-sybase-login-params
     :sqli-comint-func sql-comint-sybase
     :prompt-regexp "^SQL> "
     :prompt-length 5
     :syntax-alist ((?@ . "_"))
     :terminator ("^go" . "go"))
    )
  "An alist of product specific configuration settings.

Without an entry in this list a product will not be properly
highlighted and will not support `sql-interactive-mode'.

Each element in the list is in the following format:

 \(PRODUCT FEATURE VALUE ...)

where PRODUCT is the appropriate value of `sql-product'.  The
product name is then followed by FEATURE-VALUE pairs.  If a
FEATURE is not specified, its VALUE is treated as nil.  FEATURE
may be any one of the following:

 :name                  string containing the displayable name of
                        the product.

 :free-software         is the product Free (as in Freedom) software?

 :font-lock             name of the variable containing the product
                        specific font lock highlighting patterns.

 :sqli-program          name of the variable containing the product
                        specific interactive program name.

 :sqli-options          name of the variable containing the list
                        of product specific options.

 :sqli-login            name of the variable containing the list of
                        login parameters (i.e., user, password,
                        database and server) needed to connect to
                        the database.

 :sqli-comint-func      name of a function which accepts no
                        parameters that will use the values of
                        `sql-user', `sql-password',
                        `sql-database', `sql-server' and
                        `sql-port' to open a comint buffer and
                        connect to the database.  Do product
                        specific configuration of comint in this
                        function.

 :list-all              Command string or function which produces
                        a listing of all objects in the database.
                        If it's a cons cell, then the car
                        produces the standard list of objects and
                        the cdr produces an enhanced list of
                        objects.  What \"enhanced\" means is
                        dependent on the SQL product and may not
                        exist.  In general though, the
                        \"enhanced\" list should include visible
                        objects from other schemas.

 :list-table            Command string or function which produces
                        a detailed listing of a specific database
                        table.  If its a cons cell, then the car
                        produces the standard list and the cdr
                        produces an enhanced list.

 :completion-object     A function that returns a list of
                        objects.  Called with a single
                        parameter--if nil then list objects
                        accessible in the current schema, if
                        not-nil it is the name of a schema whose
                        objects should be listed.

 :completion-column     A function that returns a list of
                        columns.  Called with a single
                        parameter--if nil then list objects
                        accessible in the current schema, if
                        not-nil it is the name of a schema whose
                        objects should be listed.

 :prompt-regexp         regular expression string that matches
                        the prompt issued by the product
                        interpreter.

 :prompt-length         length of the prompt on the line.

 :prompt-cont-regexp    regular expression string that matches
                        the continuation prompt issued by the
                        product interpreter.

 :input-filter          function which can filter strings sent to
                        the command interpreter.  It is also used
                        by the `sql-send-string',
                        `sql-send-region', `sql-send-paragraph'
                        and `sql-send-buffer' functions.  The
                        function is passed the string sent to the
                        command interpreter and must return the
                        filtered string.  May also be a list of
                        such functions.

 :statement             name of a variable containing a regexp that
                        matches the beginning of SQL statements.

 :terminator            the terminator to be sent after a
                        `sql-send-string', `sql-send-region',
                        `sql-send-paragraph' and
                        `sql-send-buffer' command.  May be the
                        literal string or a cons of a regexp to
                        match an existing terminator in the
                        string and the terminator to be used if
                        its absent.  By default \";\".

 :syntax-alist          alist of syntax table entries to enable
                        special character treatment by font-lock
                        and imenu.

Other features can be stored but they will be ignored.  However,
you can develop new functionality which is product independent by
using `sql-get-product-feature' to lookup the product specific
settings.")

(defvar sql-indirect-features
  '(:font-lock :sqli-program :sqli-options :sqli-login :statement))

(defcustom sql-connection-alist nil
  "An alist of connection parameters for interacting with a SQL product.
Each element of the alist is as follows:

  \(CONNECTION \(SQL-VARIABLE VALUE) ...)

Where CONNECTION is a symbol identifying the connection, SQL-VARIABLE
is the symbol name of a SQL mode variable, and VALUE is the value to
be assigned to the variable.  The most common SQL-VARIABLE settings
associated with a connection are: `sql-product', `sql-user',
`sql-password', `sql-port', `sql-server', and `sql-database'.

If a SQL-VARIABLE is part of the connection, it will not be
prompted for during login.  The command `sql-connect' starts a
predefined SQLi session using the parameters from this list.
Connections defined here appear in the submenu SQL->Start...  for
making new SQLi sessions."
  :type `(alist :key-type (string :tag "Connection")
                :value-type
                (set
                 (group (const :tag "Product"  sql-product)
                        (choice
                         ,@(mapcar (lambda (prod-info)
                                     `(const :tag
                                             ,(or (plist-get (cdr prod-info) :name)
                                                  (capitalize (symbol-name (car prod-info))))
                                             (quote ,(car prod-info))))
                                   sql-product-alist)))
                 (group (const :tag "Username" sql-user)     string)
                 (group (const :tag "Password" sql-password) string)
                 (group (const :tag "Server"   sql-server)   string)
                 (group (const :tag "Database" sql-database) string)
                 (group (const :tag "Port"     sql-port)     integer)
                 (repeat :inline t
                         (list :tab "Other"
                               (symbol :tag " Variable Symbol")
                               (sexp   :tag "Value Expression")))))
  :version "24.1"
  :group 'SQL)

(defcustom sql-product 'ansi
  "Select the SQL database product used so that buffers can be
highlighted properly when you open them."
  :type `(choice
          ,@(mapcar (lambda (prod-info)
                      `(const :tag
                              ,(or (plist-get (cdr prod-info) :name)
                                   (capitalize (symbol-name (car prod-info))))
                              ,(car prod-info)))
                    sql-product-alist))
  :group 'SQL
  :safe 'symbolp)
(defvaralias 'sql-dialect 'sql-product)

;; misc customization of sql.el behavior

(defcustom sql-electric-stuff nil
  "Treat some input as electric.
If set to the symbol `semicolon', then hitting `;' will send current
input in the SQLi buffer to the process.
If set to the symbol `go', then hitting `go' on a line by itself will
send current input in the SQLi buffer to the process.
If set to nil, then you must use \\[comint-send-input] in order to send
current input in the SQLi buffer to the process."
  :type '(choice (const :tag "Nothing" nil)
		 (const :tag "The semicolon `;'" semicolon)
		 (const :tag "The string `go' by itself" go))
  :version "20.8"
  :group 'SQL)

(defcustom sql-send-terminator nil
  "When non-nil, add a terminator to text sent to the SQL interpreter.

When text is sent to the SQL interpreter (via `sql-send-string',
`sql-send-region', `sql-send-paragraph' or `sql-send-buffer'), a
command terminator can be automatically sent as well.  The
terminator is not sent, if the string sent already ends with the
terminator.

If this value is t, then the default command terminator for the
SQL interpreter is sent.  If this value is a string, then the
string is sent.

If the value is a cons cell of the form (PAT . TERM), then PAT is
a regexp used to match the terminator in the string and TERM is
the terminator to be sent.  This form is useful if the SQL
interpreter has more than one way of submitting a SQL command.
The PAT regexp can match any of them, and TERM is the way we do
it automatically."

  :type '(choice (const  :tag "No Terminator" nil)
		 (const  :tag "Default Terminator" t)
		 (string :tag "Terminator String")
		 (cons   :tag "Terminator Pattern and String"
			 (string :tag "Terminator Pattern")
			 (string :tag "Terminator String")))
  :version "22.2"
  :group 'SQL)

(defvar sql-contains-names nil
  "When non-nil, the current buffer contains database names.

Globally should be set to nil; it will be non-nil in `sql-mode',
`sql-interactive-mode' and list all buffers.")


(defcustom sql-pop-to-buffer-after-send-region nil
  "When non-nil, pop to the buffer SQL statements are sent to.

After a call to `sql-sent-string', `sql-send-region',
`sql-send-paragraph' or `sql-send-buffer', the window is split
and the SQLi buffer is shown.  If this variable is not nil, that
buffer's window will be selected by calling `pop-to-buffer'.  If
this variable is nil, that buffer is shown using
`display-buffer'."
  :type 'boolean
  :group 'SQL)

;; imenu support for sql-mode.

(defvar sql-imenu-generic-expression
  ;; Items are in reverse order because they are rendered in reverse.
  '(("Rules/Defaults" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*\\(rule\\|default\\)\\s-+\\(\\w+\\)" 3)
    ("Sequences" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*sequence\\s-+\\(\\w+\\)" 2)
    ("Triggers" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*trigger\\s-+\\(\\w+\\)" 2)
    ("Functions" "^\\s-*\\(create\\s-+\\(\\w+\\s-+\\)*\\)?function\\s-+\\(\\w+\\)" 3)
    ("Procedures" "^\\s-*\\(create\\s-+\\(\\w+\\s-+\\)*\\)?proc\\(edure\\)?\\s-+\\(\\w+\\)" 4)
    ("Packages" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*package\\s-+\\(body\\s-+\\)?\\(\\w+\\)" 3)
    ("Types" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*type\\s-+\\(body\\s-+\\)?\\(\\w+\\)" 3)
    ("Indexes" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*index\\s-+\\(\\w+\\)" 2)
    ("Tables/Views" "^\\s-*create\\s-+\\(\\w+\\s-+\\)*\\(table\\|view\\)\\s-+\\(\\w+\\)" 3))
  "Define interesting points in the SQL buffer for `imenu'.

This is used to set `imenu-generic-expression' when SQL mode is
entered.  Subsequent changes to `sql-imenu-generic-expression' will
not affect existing SQL buffers because imenu-generic-expression is
a local variable.")

;; history file

(defcustom sql-input-ring-file-name nil
  "If non-nil, name of the file to read/write input history.

You have to set this variable if you want the history of your commands
saved from one Emacs session to the next.  If this variable is set,
exiting the SQL interpreter in an SQLi buffer will write the input
history to the specified file.  Starting a new process in a SQLi buffer
will read the input history from the specified file.

This is used to initialize `comint-input-ring-file-name'.

Note that the size of the input history is determined by the variable
`comint-input-ring-size'."
  :type '(choice (const :tag "none" nil)
		 (file))
  :group 'SQL)

(defcustom sql-input-ring-separator "\n--\n"
  "Separator between commands in the history file.

If set to \"\\n\", each line in the history file will be interpreted as
one command.  Multi-line commands are split into several commands when
the input ring is initialized from a history file.

This variable used to initialize `comint-input-ring-separator'.
`comint-input-ring-separator' is part of Emacs 21; if your Emacs
does not have it, setting `sql-input-ring-separator' will have no
effect.  In that case multiline commands will be split into several
commands when the input history is read, as if you had set
`sql-input-ring-separator' to \"\\n\"."
  :type 'string
  :group 'SQL)

;; The usual hooks

(defcustom sql-interactive-mode-hook '()
  "Hook for customizing `sql-interactive-mode'."
  :type 'hook
  :group 'SQL)

(defcustom sql-mode-hook '()
  "Hook for customizing `sql-mode'."
  :type 'hook
  :group 'SQL)

(defcustom sql-set-sqli-hook '()
  "Hook for reacting to changes of `sql-buffer'.

This is called by `sql-set-sqli-buffer' when the value of `sql-buffer'
is changed."
  :type 'hook
  :group 'SQL)

(defcustom sql-login-hook '()
  "Hook for interacting with a buffer in `sql-interactive-mode'.

This hook is invoked in a buffer once it is ready to accept input
for the first time."
  :version "24.1"
  :type 'hook
  :group 'SQL)

;; Customization for ANSI

(defcustom sql-ansi-statement-starters (regexp-opt '(
 "create" "alter" "drop"
 "select" "insert" "update" "delete" "merge"
 "grant" "revoke"
))
  "Regexp of keywords that start SQL commands

All products share this list; products should define a regexp to
identify additional keywords in a variable defined by
the :statement feature."
  :version "24.1"
  :type 'string
  :group 'SQL)

;; Customization for Oracle

(defcustom sql-oracle-program "sqlplus"
  "Command to start sqlplus by Oracle.

Starts `sql-interactive-mode' after doing some setup.

On Windows, \"sqlplus\" usually starts the sqlplus \"GUI\".  In order
to start the sqlplus console, use \"plus33\" or something similar.
You will find the file in your Orant\\bin directory."
  :type 'file
  :group 'SQL)

(defcustom sql-oracle-options nil
  "List of additional options for `sql-oracle-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-oracle-login-params '(user password database)
  "List of login parameters needed to connect to Oracle."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

(defcustom sql-oracle-statement-starters
  (regexp-opt '("declare" "begin" "with"))
  "Additional statement starting keywords in Oracle."
  :version "24.1"
  :type 'string
  :group 'SQL)

(defcustom sql-oracle-scan-on t
  "Non-nil if placeholders should be replaced in Oracle SQLi.

When non-nil, Emacs will scan text sent to sqlplus and prompt
for replacement text for & placeholders as sqlplus does.  This
is needed on Windows where SQL*Plus output is buffered and the
prompts are not shown until after the text is entered.

You need to issue the following command in SQL*Plus to be safe:

    SET DEFINE OFF

In older versions of SQL*Plus, this was the SET SCAN OFF command."
  :version "24.1"
  :type 'boolean
  :group 'SQL)

;; Customization for SQLite

(defcustom sql-sqlite-program (or (executable-find "sqlite3")
                                  (executable-find "sqlite")
                                  "sqlite")
  "Command to start SQLite.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-sqlite-options nil
  "List of additional options for `sql-sqlite-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-sqlite-login-params '((database :file ".*\\.\\(db\\|sqlite[23]?\\)"))
  "List of login parameters needed to connect to SQLite."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for MySQL

(defcustom sql-mysql-program "mysql"
  "Command to start mysql by TcX.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-mysql-options nil
  "List of additional options for `sql-mysql-program'.
The following list of options is reported to make things work
on Windows: \"-C\" \"-t\" \"-f\" \"-n\"."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-mysql-login-params '(user password database server)
  "List of login parameters needed to connect to MySQL."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Solid

(defcustom sql-solid-program "solsql"
  "Command to start SOLID SQL Editor.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-solid-login-params '(user password server)
  "List of login parameters needed to connect to Solid."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Sybase

(defcustom sql-sybase-program "isql"
  "Command to start isql by Sybase.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-sybase-options nil
  "List of additional options for `sql-sybase-program'.
Some versions of isql might require the -n option in order to work."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-sybase-login-params '(server user password database)
  "List of login parameters needed to connect to Sybase."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Informix

(defcustom sql-informix-program "dbaccess"
  "Command to start dbaccess by Informix.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-informix-login-params '(database)
  "List of login parameters needed to connect to Informix."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Ingres

(defcustom sql-ingres-program "sql"
  "Command to start sql by Ingres.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-ingres-login-params '(database)
  "List of login parameters needed to connect to Ingres."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Microsoft

(defcustom sql-ms-program "osql"
  "Command to start osql by Microsoft.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-ms-options '("-w" "300" "-n")
  ;; -w is the linesize
  "List of additional options for `sql-ms-program'."
  :type '(repeat string)
  :version "22.1"
  :group 'SQL)

(defcustom sql-ms-login-params '(user password server database)
  "List of login parameters needed to connect to Microsoft."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Postgres

(defcustom sql-postgres-program "psql"
  "Command to start psql by Postgres.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-postgres-options '("-P" "pager=off")
  "List of additional options for `sql-postgres-program'.
The default setting includes the -P option which breaks older versions
of the psql client (such as version 6.5.3).  The -P option is equivalent
to the --pset option.  If you want the psql to prompt you for a user
name, add the string \"-u\" to the list of options.  If you want to
provide a user name on the command line (newer versions such as 7.1),
add your name with a \"-U\" prefix (such as \"-Umark\") to the list."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-postgres-login-params `((user :default ,(user-login-name))
                                       (database :default ,(user-login-name))
                                       server)
  "List of login parameters needed to connect to Postgres."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Interbase

(defcustom sql-interbase-program "isql"
  "Command to start isql by Interbase.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-interbase-options nil
  "List of additional options for `sql-interbase-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-interbase-login-params '(user password database)
  "List of login parameters needed to connect to Interbase."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for DB2

(defcustom sql-db2-program "db2"
  "Command to start db2 by IBM.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-db2-options nil
  "List of additional options for `sql-db2-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-db2-login-params nil
  "List of login parameters needed to connect to DB2."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

;; Customization for Linter

(defcustom sql-linter-program "inl"
  "Command to start inl by RELEX.

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-linter-options nil
  "List of additional options for `sql-linter-program'."
  :type '(repeat string)
  :version "21.3"
  :group 'SQL)

(defcustom sql-linter-login-params '(user password database server)
  "Login parameters to needed to connect to Linter."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)



;;; Variables which do not need customization

(defvar sql-user-history nil
  "History of usernames used.")

(defvar sql-database-history nil
  "History of databases used.")

(defvar sql-server-history nil
  "History of servers used.")

;; Passwords are not kept in a history.

(defvar sql-product-history nil
  "History of products used.")

(defvar sql-connection-history nil
  "History of connections used.")

(defvar sql-buffer nil
  "Current SQLi buffer.

The global value of `sql-buffer' is the name of the latest SQLi buffer
created.  Any SQL buffer created will make a local copy of this value.
See `sql-interactive-mode' for more on multiple sessions.  If you want
to change the SQLi buffer a SQL mode sends its SQL strings to, change
the local value of `sql-buffer' using \\[sql-set-sqli-buffer].")

(defvar sql-prompt-regexp nil
  "Prompt used to initialize `comint-prompt-regexp'.

You can change `sql-prompt-regexp' on `sql-interactive-mode-hook'.")

(defvar sql-prompt-length 0
  "Prompt used to set `left-margin' in `sql-interactive-mode'.

You can change `sql-prompt-length' on `sql-interactive-mode-hook'.")

(defvar sql-prompt-cont-regexp nil
  "Prompt pattern of statement continuation prompts.")

(defvar sql-alternate-buffer-name nil
  "Buffer-local string used to possibly rename the SQLi buffer.

Used by `sql-rename-buffer'.")

(defun sql-buffer-live-p (buffer &optional product connection)
  "Returns non-nil if the process associated with buffer is live.

BUFFER can be a buffer object or a buffer name.  The buffer must
be a live buffer, have an running process attached to it, be in
`sql-interactive-mode', and, if PRODUCT or CONNECTION are
specified, it's `sql-product' or `sql-connection' must match."

  (when buffer
    (setq buffer (get-buffer buffer))
    (and buffer
         (buffer-live-p buffer)
         (get-buffer-process buffer)
         (comint-check-proc buffer)
         (with-current-buffer buffer
           (and (derived-mode-p 'sql-interactive-mode)
                (or (not product)
                    (eq product sql-product))
                (or (not connection)
                    (eq connection sql-connection)))))))

;; Keymap for sql-interactive-mode.

(defvar sql-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (if (fboundp 'set-keymap-parent)
	(set-keymap-parent map comint-mode-map); Emacs
      (if (fboundp 'set-keymap-parents)
	  (set-keymap-parents map (list comint-mode-map)))); XEmacs
    (if (fboundp 'set-keymap-name)
	(set-keymap-name map 'sql-interactive-mode-map)); XEmacs
    (define-key map (kbd "C-j") 'sql-accumulate-and-indent)
    (define-key map (kbd "C-c C-w") 'sql-copy-column)
    (define-key map (kbd "O") 'sql-magic-go)
    (define-key map (kbd "o") 'sql-magic-go)
    (define-key map (kbd ";") 'sql-magic-semicolon)
    (define-key map (kbd "C-c C-l a") 'sql-list-all)
    (define-key map (kbd "C-c C-l t") 'sql-list-table)
    map)
  "Mode map used for `sql-interactive-mode'.
Based on `comint-mode-map'.")

;; Keymap for sql-mode.

(defvar sql-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sql-send-paragraph)
    (define-key map (kbd "C-c C-r") 'sql-send-region)
    (define-key map (kbd "C-c C-s") 'sql-send-string)
    (define-key map (kbd "C-c C-b") 'sql-send-buffer)
    (define-key map (kbd "C-c C-i") 'sql-product-interactive)
    (define-key map (kbd "C-c C-l a") 'sql-list-all)
    (define-key map (kbd "C-c C-l t") 'sql-list-table)
    (define-key map [remap beginning-of-defun] 'sql-beginning-of-statement)
    (define-key map [remap end-of-defun] 'sql-end-of-statement)
    map)
  "Mode map used for `sql-mode'.")

;; easy menu for sql-mode.

(easy-menu-define
 sql-mode-menu sql-mode-map
 "Menu for `sql-mode'."
 `("SQL"
   ["Send Paragraph" sql-send-paragraph (sql-buffer-live-p sql-buffer)]
   ["Send Region" sql-send-region (and mark-active
				       (sql-buffer-live-p sql-buffer))]
   ["Send Buffer" sql-send-buffer (sql-buffer-live-p sql-buffer)]
   ["Send String" sql-send-string (sql-buffer-live-p sql-buffer)]
   "--"
   ["List all objects" sql-list-all (and (sql-buffer-live-p sql-buffer)
                                         (sql-get-product-feature sql-product :list-all))]
   ["List table details" sql-list-table (and (sql-buffer-live-p sql-buffer)
                                             (sql-get-product-feature sql-product :list-table))]
   "--"
   ["Start SQLi session" sql-product-interactive
    :visible (not sql-connection-alist)
    :enable (sql-get-product-feature sql-product :sqli-comint-func)]
   ("Start..."
    :visible sql-connection-alist
    :filter sql-connection-menu-filter
    "--"
    ["New SQLi Session" sql-product-interactive (sql-get-product-feature sql-product :sqli-comint-func)])
   ["--"
    :visible sql-connection-alist]
   ["Show SQLi buffer" sql-show-sqli-buffer t]
   ["Set SQLi buffer" sql-set-sqli-buffer t]
   ["Pop to SQLi buffer after send"
    sql-toggle-pop-to-buffer-after-send-region
    :style toggle
    :selected sql-pop-to-buffer-after-send-region]
   ["--" nil nil]
   ("Product"
    ,@(mapcar (lambda (prod-info)
                (let* ((prod (pop prod-info))
                       (name (or (plist-get prod-info :name)
                                 (capitalize (symbol-name prod))))
                       (cmd (intern (format "sql-highlight-%s-keywords" prod))))
                  (fset cmd `(lambda () ,(format "Highlight %s SQL keywords." name)
                               (interactive)
                               (sql-set-product ',prod)))
                  (vector name cmd
                          :style 'radio
                          :selected `(eq sql-product ',prod))))
              sql-product-alist))))

;; easy menu for sql-interactive-mode.

(easy-menu-define
 sql-interactive-mode-menu sql-interactive-mode-map
 "Menu for `sql-interactive-mode'."
 '("SQL"
   ["Rename Buffer" sql-rename-buffer t]
   ["Save Connection" sql-save-connection (not sql-connection)]
   "--"
   ["List all objects" sql-list-all (sql-get-product-feature sql-product :list-all)]
   ["List table details" sql-list-table (sql-get-product-feature sql-product :list-table)]))

;; Abbreviations -- if you want more of them, define them in your
;; ~/.emacs file.  Abbrevs have to be enabled in your ~/.emacs, too.

(defvar sql-mode-abbrev-table nil
  "Abbrev table used in `sql-mode' and `sql-interactive-mode'.")
(unless sql-mode-abbrev-table
  (define-abbrev-table 'sql-mode-abbrev-table nil))

(mapc
 ;; In Emacs 22+, provide SYSTEM-FLAG to define-abbrev.
 (lambda (abbrev)
   (let ((name (car abbrev))
         (expansion (cdr abbrev)))
     (condition-case nil
         (define-abbrev sql-mode-abbrev-table name expansion nil 0 t)
       (error
        (define-abbrev sql-mode-abbrev-table name expansion)))))
 '(("ins"  . "insert")
   ("upd"  . "update")
   ("del"  . "delete")
   ("sel"  . "select")
   ("proc" . "procedure")
   ("func" . "function")
   ("cr"   . "create")))

;; Syntax Table

(defvar sql-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C-style comments /**/ (see elisp manual "Syntax Flags"))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    ;; double-dash starts comments
    (modify-syntax-entry ?- ". 12b" table)
    ;; newline and formfeed end comments
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\f "> b" table)
    ;; single quotes (') delimit strings
    (modify-syntax-entry ?' "\"" table)
    ;; double quotes (") don't delimit strings
    (modify-syntax-entry ?\" "." table)
    ;; Make these all punctuation
    (mapc (lambda (c) (modify-syntax-entry c "." table))
          (string-to-list "!#$%&+,.:;<=>?@\\|"))
    table)
  "Syntax table used in `sql-mode' and `sql-interactive-mode'.")

;; Font lock support

(defvar sql-mode-font-lock-object-name
  (eval-when-compile
    (list (concat "^\\s-*\\(?:create\\|drop\\|alter\\)\\s-+" ;; lead off with CREATE, DROP or ALTER
		  "\\(?:\\w+\\s-+\\)*"  ;; optional intervening keywords
		  "\\(?:table\\|view\\|\\(?:package\\|type\\)\\(?:\\s-+body\\)?\\|proc\\(?:edure\\)?"
		  "\\|function\\|trigger\\|sequence\\|rule\\|default\\)\\s-+"
		  "\\(\\w+\\)")
	  1 'font-lock-function-name-face))

  "Pattern to match the names of top-level objects.

The pattern matches the name in a CREATE, DROP or ALTER
statement.  The format of variable should be a valid
`font-lock-keywords' entry.")

;; While there are international and American standards for SQL, they
;; are not followed closely, and most vendors offer significant
;; capabilities beyond those defined in the standard specifications.

;; SQL mode provides support for highlighting based on the product.  In
;; addition to highlighting the product keywords, any ANSI keywords not
;; used by the product are also highlighted.  This will help identify
;; keywords that could be restricted in future versions of the product
;; or might be a problem if ported to another product.

;; To reduce the complexity and size of the regular expressions
;; generated to match keywords, ANSI keywords are filtered out of
;; product keywords if they are equivalent.  To do this, we define a
;; function `sql-font-lock-keywords-builder' that removes any keywords
;; that are matched by the ANSI patterns and results in the same face
;; being applied.  For this to work properly, we must play some games
;; with the execution and compile time behavior.  This code is a
;; little tricky but works properly.

;; When defining the keywords for individual products you should
;; include all of the keywords that you want matched.  The filtering
;; against the ANSI keywords will be automatic if you use the
;; `sql-font-lock-keywords-builder' function and follow the
;; implementation pattern used for the other products in this file.

(eval-when-compile
  (defvar sql-mode-ansi-font-lock-keywords)
  (setq sql-mode-ansi-font-lock-keywords nil))

(eval-and-compile
  (defun sql-font-lock-keywords-builder (face boundaries &rest keywords)
    "Generation of regexp matching any one of KEYWORDS."

    (let ((bdy (or boundaries '("\\b" . "\\b")))
	  kwd)

      ;; Remove keywords that are defined in ANSI
      (setq kwd keywords)
      ;; (dolist (k keywords)
      ;;   (catch 'next
      ;;     (dolist (a sql-mode-ansi-font-lock-keywords)
      ;;       (when (and (eq face (cdr a))
      ;;   	       (eq (string-match (car a) k 0) 0)
      ;;   	       (eq (match-end 0) (length k)))
      ;;         (setq kwd (delq k kwd))
      ;;         (throw 'next nil)))))

      ;; Create a properly formed font-lock-keywords item
      (cons (concat (car bdy)
		    (regexp-opt kwd t)
		    (cdr bdy))
	    face)))

  (defun sql-regexp-abbrev (keyword)
    (let ((brk   (string-match "[~]" keyword))
          (len   (length keyword))
          (sep   "\\(?:")
          re i)
      (if (not brk)
          keyword
        (setq re  (substring keyword 0 brk)
              i   (+ 2 brk)
              brk (1+ brk))
        (while (<= i len)
          (setq re  (concat re sep (substring keyword brk i))
                sep "\\|"
                i   (1+ i)))
        (concat re "\\)?"))))

  (defun sql-regexp-abbrev-list (&rest keyw-list)
    (let ((re nil)
          (sep "\\<\\(?:"))
      (while keyw-list
        (setq re (concat re sep (sql-regexp-abbrev (car keyw-list)))
              sep "\\|"
              keyw-list (cdr keyw-list)))
      (concat re "\\)\\>"))))

(eval-when-compile
  (setq sql-mode-ansi-font-lock-keywords
	(list
	 ;; ANSI Non Reserved keywords
	 (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"ada" "asensitive" "assignment" "asymmetric" "atomic" "between"
"bitvar" "called" "catalog_name" "chain" "character_set_catalog"
"character_set_name" "character_set_schema" "checked" "class_origin"
"cobol" "collation_catalog" "collation_name" "collation_schema"
"column_name" "command_function" "command_function_code" "committed"
"condition_number" "connection_name" "constraint_catalog"
"constraint_name" "constraint_schema" "contains" "cursor_name"
"datetime_interval_code" "datetime_interval_precision" "defined"
"definer" "dispatch" "dynamic_function" "dynamic_function_code"
"existing" "exists" "final" "fortran" "generated" "granted"
"hierarchy" "hold" "implementation" "infix" "insensitive" "instance"
"instantiable" "invoker" "key_member" "key_type" "length" "m"
"message_length" "message_octet_length" "message_text" "method" "more"
"mumps" "name" "nullable" "number" "options" "overlaps" "overriding"
"parameter_mode" "parameter_name" "parameter_ordinal_position"
"parameter_specific_catalog" "parameter_specific_name"
"parameter_specific_schema" "pascal" "pli" "position" "repeatable"
"returned_length" "returned_octet_length" "returned_sqlstate"
"routine_catalog" "routine_name" "routine_schema" "row_count" "scale"
"schema_name" "security" "self" "sensitive" "serializable"
"server_name" "similar" "simple" "source" "specific_name" "style"
"subclass_origin" "sublist" "symmetric" "system" "table_name"
"transaction_active" "transactions_committed"
"transactions_rolled_back" "transform" "transforms" "trigger_catalog"
"trigger_name" "trigger_schema" "type" "uncommitted" "unnamed"
"user_defined_type_catalog" "user_defined_type_name"
"user_defined_type_schema"
)

	 ;; ANSI Reserved keywords
	 (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"absolute" "action" "add" "admin" "after" "aggregate" "alias" "all"
"allocate" "alter" "and" "any" "are" "as" "asc" "assertion" "at"
"authorization" "before" "begin" "both" "breadth" "by" "call"
"cascade" "cascaded" "case" "catalog" "check" "class" "close"
"collate" "collation" "column" "commit" "completion" "connect"
"connection" "constraint" "constraints" "constructor" "continue"
"corresponding" "create" "cross" "cube" "current" "cursor" "cycle"
"data" "day" "deallocate" "declare" "default" "deferrable" "deferred"
"delete" "depth" "deref" "desc" "describe" "descriptor" "destroy"
"destructor" "deterministic" "diagnostics" "dictionary" "disconnect"
"distinct" "domain" "drop" "dynamic" "each" "else" "end" "equals"
"escape" "every" "except" "exception" "exec" "execute" "external"
"false" "fetch" "first" "for" "foreign" "found" "free" "from" "full"
"function" "general" "get" "global" "go" "goto" "grant" "group"
"grouping" "having" "host" "hour" "identity" "ignore" "immediate" "in"
"indicator" "initialize" "initially" "inner" "inout" "input" "insert"
"intersect" "into" "is" "isolation" "iterate" "join" "key" "language"
"last" "lateral" "leading" "left" "less" "level" "like" "limit"
"local" "locator" "map" "match" "minute" "modifies" "modify" "module"
"month" "names" "natural" "new" "next" "no" "none" "not" "null" "of"
"off" "old" "on" "only" "open" "operation" "option" "or" "order"
"ordinality" "out" "outer" "output" "pad" "parameter" "parameters"
"partial" "path" "postfix" "prefix" "preorder" "prepare" "preserve"
"primary" "prior" "privileges" "procedure" "public" "read" "reads"
"recursive" "references" "referencing" "relative" "restrict" "result"
"return" "returns" "revoke" "right" "role" "rollback" "rollup"
"routine" "rows" "savepoint" "schema" "scroll" "search" "second"
"section" "select" "sequence" "session" "set" "sets" "size" "some"
"space" "specific" "specifictype" "sql" "sqlexception" "sqlstate"
"sqlwarning" "start" "state" "statement" "static" "structure" "table"
"temporary" "terminate" "than" "then" "timezone_hour"
"timezone_minute" "to" "trailing" "transaction" "translation"
"trigger" "true" "under" "union" "unique" "unknown" "unnest" "update"
"usage" "using" "value" "values" "variable" "view" "when" "whenever"
"where" "with" "without" "work" "write" "year"
)

	 ;; ANSI Functions
	 (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"abs" "avg" "bit_length" "cardinality" "cast" "char_length"
"character_length" "coalesce" "convert" "count" "current_date"
"current_path" "current_role" "current_time" "current_timestamp"
"current_user" "extract" "localtime" "localtimestamp" "lower" "max"
"min" "mod" "nullif" "octet_length" "overlay" "placing" "session_user"
"substring" "sum" "system_user" "translate" "treat" "trim" "upper"
"user"
)

	 ;; ANSI Data Types
	 (sql-font-lock-keywords-builder 'font-lock-type-face nil
"array" "binary" "bit" "blob" "boolean" "char" "character" "clob"
"date" "dec" "decimal" "double" "float" "int" "integer" "interval"
"large" "national" "nchar" "nclob" "numeric" "object" "precision"
"real" "ref" "row" "scope" "smallint" "time" "timestamp" "varchar"
"varying" "zone"
))))

(defvar sql-mode-ansi-font-lock-keywords
  (eval-when-compile sql-mode-ansi-font-lock-keywords)
  "ANSI SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-ansi-font-lock-keywords'.  You may want
to add functions and PL/SQL keywords.")

(defun sql-oracle-show-reserved-words ()
  ;; This function is for use by the maintainer of SQL.EL only.
  (interactive)
  (if (or (and (not (derived-mode-p 'sql-mode))
               (not (derived-mode-p 'sql-interactive-mode)))
          (not sql-buffer)
          (not (eq sql-product 'oracle)))
      (error "Not an Oracle buffer")

    (let ((b "*RESERVED WORDS*"))
      (sql-execute sql-buffer b
                   (concat "SELECT "
                           "  keyword "
                           ", reserved AS \"Res\" "
                           ", res_type AS \"Type\" "
                           ", res_attr AS \"Attr\" "
                           ", res_semi AS \"Semi\" "
                           ", duplicate AS \"Dup\" "
                           "FROM V$RESERVED_WORDS "
                           "WHERE length > 1 "
                           "AND SUBSTR(keyword, 1, 1) BETWEEN 'A' AND 'Z' "
                           "ORDER BY 2 DESC, 3 DESC, 4 DESC, 5 DESC, 6 DESC, 1;")
                   nil nil)
      (with-current-buffer b
        (set (make-local-variable 'sql-product) 'oracle)
        (sql-product-font-lock t nil)
        (font-lock-mode +1)))))

(defvar sql-mode-oracle-font-lock-keywords
  (eval-when-compile
    (list
     ;; Oracle SQL*Plus Commands
     ;;   Only recognized in they start in column 1 and the
     ;;   abbreviation is followed by a space or the end of line.

     "\\|"
     (list (concat "^" (sql-regexp-abbrev "rem~ark") "\\(?:\\s-.*\\)?$")
           0 'font-lock-comment-face t)

     (list
      (concat
       "^\\(?:"
       (sql-regexp-abbrev-list
        "[@]\\{1,2\\}" "acc~ept" "a~ppend" "archive" "attribute"
        "bre~ak" "bti~tle" "c~hange" "cl~ear" "col~umn" "conn~ect"
        "copy" "def~ine" "del" "desc~ribe" "disc~onnect" "ed~it"
        "exec~ute" "exit" "get" "help" "ho~st" "[$]" "i~nput" "l~ist"
        "passw~ord" "pau~se" "pri~nt" "pro~mpt" "quit" "recover"
        "repf~ooter" "reph~eader" "r~un" "sav~e" "sho~w" "shutdown"
        "spo~ol" "sta~rt" "startup" "store" "tim~ing" "tti~tle"
        "undef~ine" "var~iable" "whenever")
       "\\|"
       (concat "\\(?:"
               (sql-regexp-abbrev "comp~ute")
               "\\s-+"
               (sql-regexp-abbrev-list
                "avg" "cou~nt" "min~imum" "max~imum" "num~ber" "sum"
                "std" "var~iance")
               "\\)")
       "\\|"
       (concat "\\(?:set\\s-+"
               (sql-regexp-abbrev-list
                "appi~nfo" "array~size" "auto~commit" "autop~rint"
                "autorecovery" "autot~race" "blo~ckterminator"
                "cmds~ep" "colsep" "com~patibility" "con~cat"
                "copyc~ommit" "copytypecheck" "def~ine" "describe"
                "echo" "editf~ile" "emb~edded" "esc~ape" "feed~back"
                "flagger" "flu~sh" "hea~ding" "heads~ep" "instance"
                "lin~esize" "lobof~fset" "long" "longc~hunksize"
                "mark~up" "newp~age" "null" "numf~ormat" "num~width"
                "pages~ize" "pau~se" "recsep" "recsepchar"
                "scan" "serverout~put" "shift~inout" "show~mode"
                "sqlbl~anklines" "sqlc~ase" "sqlco~ntinue"
                "sqln~umber" "sqlpluscompat~ibility" "sqlpre~fix"
                "sqlp~rompt" "sqlt~erminator" "suf~fix" "tab"
                "term~out" "ti~me" "timi~ng" "trim~out" "trims~pool"
                "und~erline" "ver~ify" "wra~p")
               "\\)")

       "\\)\\(?:\\s-.*\\)?\\(?:[-]\n.*\\)*$")
      0 'font-lock-doc-face t)

     ;; Oracle Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"abs" "acos" "add_months" "appendchildxml" "ascii" "asciistr" "asin"
"atan" "atan2" "avg" "bfilename" "bin_to_num" "bitand" "cardinality"
"cast" "ceil" "chartorowid" "chr" "cluster_id" "cluster_probability"
"cluster_set" "coalesce" "collect" "compose" "concat" "convert" "corr"
"connect_by_root" "connect_by_iscycle" "connect_by_isleaf"
"corr_k" "corr_s" "cos" "cosh" "count" "covar_pop" "covar_samp"
"cube_table" "cume_dist" "current_date" "current_timestamp" "cv"
"dataobj_to_partition" "dbtimezone" "decode" "decompose" "deletexml"
"dense_rank" "depth" "deref" "dump" "empty_blob" "empty_clob"
"existsnode" "exp" "extract" "extractvalue" "feature_id" "feature_set"
"feature_value" "first" "first_value" "floor" "from_tz" "greatest"
"grouping" "grouping_id" "group_id" "hextoraw" "initcap"
"insertchildxml" "insertchildxmlafter" "insertchildxmlbefore"
"insertxmlafter" "insertxmlbefore" "instr" "instr2" "instr4" "instrb"
"instrc" "iteration_number" "lag" "last" "last_day" "last_value"
"lead" "least" "length" "length2" "length4" "lengthb" "lengthc"
"listagg" "ln" "lnnvl" "localtimestamp" "log" "lower" "lpad" "ltrim"
"make_ref" "max" "median" "min" "mod" "months_between" "nanvl" "nchr"
"new_time" "next_day" "nlssort" "nls_charset_decl_len"
"nls_charset_id" "nls_charset_name" "nls_initcap" "nls_lower"
"nls_upper" "nth_value" "ntile" "nullif" "numtodsinterval"
"numtoyminterval" "nvl" "nvl2" "ora_dst_affected" "ora_dst_convert"
"ora_dst_error" "ora_hash" "path" "percentile_cont" "percentile_disc"
"percent_rank" "power" "powermultiset" "powermultiset_by_cardinality"
"prediction" "prediction_bounds" "prediction_cost"
"prediction_details" "prediction_probability" "prediction_set"
"presentnnv" "presentv" "previous" "rank" "ratio_to_report" "rawtohex"
"rawtonhex" "ref" "reftohex" "regexp_count" "regexp_instr"
"regexp_replace" "regexp_substr" "regr_avgx" "regr_avgy" "regr_count"
"regr_intercept" "regr_r2" "regr_slope" "regr_sxx" "regr_sxy"
"regr_syy" "remainder" "replace" "round" "rowidtochar" "rowidtonchar"
"row_number" "rpad" "rtrim" "scn_to_timestamp" "sessiontimezone" "set"
"sign" "sin" "sinh" "soundex" "sqrt" "stats_binomial_test"
"stats_crosstab" "stats_f_test" "stats_ks_test" "stats_mode"
"stats_mw_test" "stats_one_way_anova" "stats_t_test_indep"
"stats_t_test_indepu" "stats_t_test_one" "stats_t_test_paired"
"stats_wsr_test" "stddev" "stddev_pop" "stddev_samp" "substr"
"substr2" "substr4" "substrb" "substrc" "sum" "sysdate" "systimestamp"
"sys_connect_by_path" "sys_context" "sys_dburigen" "sys_extract_utc"
"sys_guid" "sys_typeid" "sys_xmlagg" "sys_xmlgen" "tan" "tanh"
"timestamp_to_scn" "to_binary_double" "to_binary_float" "to_blob"
"to_char" "to_clob" "to_date" "to_dsinterval" "to_lob" "to_multi_byte"
"to_nchar" "to_nclob" "to_number" "to_single_byte" "to_timestamp"
"to_timestamp_tz" "to_yminterval" "translate" "treat" "trim" "trunc"
"tz_offset" "uid" "unistr" "updatexml" "upper" "user" "userenv"
"value" "variance" "var_pop" "var_samp" "vsize" "width_bucket"
"xmlagg" "xmlcast" "xmlcdata" "xmlcolattval" "xmlcomment" "xmlconcat"
"xmldiff" "xmlelement" "xmlexists" "xmlforest" "xmlisvalid" "xmlparse"
"xmlpatch" "xmlpi" "xmlquery" "xmlroot" "xmlsequence" "xmlserialize"
"xmltable" "xmltransform"
)

     ;; See the table V$RESERVED_WORDS
     ;; Oracle Keywords
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"abort" "access" "accessed" "account" "activate" "add" "admin"
"advise" "after" "agent" "aggregate" "all" "allocate" "allow" "alter"
"always" "analyze" "ancillary" "and" "any" "apply" "archive"
"archivelog" "array" "as" "asc" "associate" "at" "attribute"
"attributes" "audit" "authenticated" "authid" "authorization" "auto"
"autoallocate" "automatic" "availability" "backup" "before" "begin"
"behalf" "between" "binding" "bitmap" "block" "blocksize" "body"
"both" "buffer_pool" "build" "by"  "cache" "call" "cancel"
"cascade" "case" "category" "certificate" "chained" "change" "check"
"checkpoint" "child" "chunk" "class" "clear" "clone" "close" "cluster"
"column" "column_value" "columns" "comment" "commit" "committed"
"compatibility" "compile" "complete" "composite_limit" "compress"
"compute" "connect" "connect_time" "consider" "consistent"
"constraint" "constraints" "constructor" "contents" "context"
"continue" "controlfile" "corruption" "cost" "cpu_per_call"
"cpu_per_session" "create" "cross" "cube" "current" "currval" "cycle"
"dangling" "data" "database" "datafile" "datafiles" "day" "ddl"
"deallocate" "debug" "default" "deferrable" "deferred" "definer"
"delay" "delete" "demand" "desc" "determines" "deterministic"
"dictionary" "dimension" "directory" "disable" "disassociate"
"disconnect" "distinct" "distinguished" "distributed" "dml" "drop"
"each" "element" "else" "enable" "end" "equals_path" "escape"
"estimate" "except" "exceptions" "exchange" "excluding" "exists"
"expire" "explain" "extent" "external" "externally"
"failed_login_attempts" "fast" "file" "final" "finish" "flush" "for"
"force" "foreign" "freelist" "freelists" "freepools" "fresh" "from"
"full" "function" "functions" "generated" "global" "global_name"
"globally" "grant" "group" "grouping" "groups" "guard" "hash"
"hashkeys" "having" "heap" "hierarchy" "id" "identified" "identifier"
"idle_time" "immediate" "in" "including" "increment" "index" "indexed"
"indexes" "indextype" "indextypes" "indicator" "initial" "initialized"
"initially" "initrans" "inner" "insert" "instance" "instantiable"
"instead" "intersect" "into" "invalidate" "is" "isolation" "java"
"join"  "keep" "key" "kill" "language" "left" "less" "level"
"levels" "library" "like" "like2" "like4" "likec" "limit" "link"
"list" "lob" "local" "location" "locator" "lock" "log" "logfile"
"logging" "logical" "logical_reads_per_call"
"logical_reads_per_session"  "managed" "management" "manual" "map"
"mapping" "master" "matched" "materialized" "maxdatafiles"
"maxextents" "maximize" "maxinstances" "maxlogfiles" "maxloghistory"
"maxlogmembers" "maxsize" "maxtrans" "maxvalue" "member" "memory"
"merge" "migrate" "minextents" "minimize" "minimum" "minus" "minvalue"
"mode" "modify" "monitoring" "month" "mount" "move" "movement" "name"
"named" "natural" "nested" "never" "new" "next" "nextval" "no"
"noarchivelog" "noaudit" "nocache" "nocompress" "nocopy" "nocycle"
"nodelay" "noforce" "nologging" "nomapping" "nomaxvalue" "nominimize"
"nominvalue" "nomonitoring" "none" "noorder" "noparallel" "norely"
"noresetlogs" "noreverse" "normal" "norowdependencies" "nosort"
"noswitch" "not" "nothing" "notimeout" "novalidate" "nowait" "null"
"nulls" "object" "of" "off" "offline" "oidindex" "old" "on" "online"
"only" "open" "operator" "optimal" "option" "or" "order"
"organization" "out" "outer" "outline" "overflow" "overriding"
"package" "packages" "parallel" "parallel_enable" "parameters"
"parent" "partition" "partitions" "password" "password_grace_time"
"password_life_time" "password_lock_time" "password_reuse_max"
"password_reuse_time" "password_verify_function" "pctfree"
"pctincrease" "pctthreshold" "pctused" "pctversion" "percent"
"performance" "permanent" "pfile" "physical" "pipelined" "plan"
"post_transaction" "pragma" "prebuilt" "preserve" "primary" "private"
"private_sga" "privileges" "procedure" "profile" "protection" "public"
"purge" "query" "quiesce" "quota" "range" "read" "reads" "rebuild"
"records_per_block" "recover" "recovery" "recycle" "reduced" "ref"
"references" "referencing" "refresh" "register" "reject" "relational"
"rely" "rename" "reset" "resetlogs" "resize" "resolve" "resolver"
"resource" "restrict" "restrict_references" "restricted" "result"
"resumable" "resume" "retention" "return" "returning" "reuse"
"reverse" "revoke" "rewrite" "right" "rnds" "rnps" "role" "roles"
"rollback" "rollup" "row" "rowdependencies" "rownum" "rows" "sample"
"savepoint" "scan" "schema" "scn" "scope" "segment" "select"
"selectivity" "self" "sequence" "serializable" "session"
"sessions_per_user" "set" "sets" "settings" "shared" "shared_pool"
"shrink" "shutdown" "siblings" "sid" "single" "size" "skip" "some"
"sort" "source" "space" "specification" "spfile" "split" "standby"
"start" "statement_id" "static" "statistics" "stop" "storage" "store"
"structure" "subpartition" "subpartitions" "substitutable"
"successful" "supplemental" "suspend" "switch" "switchover" "synonym"
"sys" "system" "table" "tables" "tablespace" "tempfile" "template"
"temporary" "test" "than" "then" "thread" "through" "time_zone"
"timeout" "to" "trace" "transaction" "trigger" "triggers" "truncate"
"trust" "type" "types" "unarchived" "under" "under_path" "undo"
"uniform" "union" "unique" "unlimited" "unlock" "unquiesce"
"unrecoverable" "until" "unusable" "unused" "update" "upgrade" "usage"
"use" "using" "validate" "validation" "value" "values" "variable"
"varray" "version" "view" "wait" "when" "whenever" "where" "with"
"without" "wnds" "wnps" "work" "write" "xmldata" "xmlschema" "xmltype"
)

     ;; Oracle Data Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"bfile" "binary_double" "binary_float" "blob" "byte" "char" "charbyte"
"clob" "date" "day" "float" "interval" "local" "long" "longraw"
"minute" "month" "nchar" "nclob" "number" "nvarchar2" "raw" "rowid" "second"
"time" "timestamp" "urowid" "varchar2" "with" "year" "zone"
)

     ;; Oracle PL/SQL Attributes
     (sql-font-lock-keywords-builder 'font-lock-builtin-face '("%" . "\\b")
"bulk_exceptions" "bulk_rowcount" "found" "isopen" "notfound"
"rowcount" "rowtype" "type"
)

     ;; Oracle PL/SQL Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"delete" "trim" "extend" "exists" "first" "last" "count" "limit"
"prior" "next"
)

     ;; Oracle PL/SQL Reserved words
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"all" "alter" "and" "any" "as" "asc" "at" "begin" "between" "by"
"case" "check" "clusters" "cluster" "colauth" "columns" "compress"
"connect" "crash" "create" "cursor" "declare" "default" "desc"
"distinct" "drop" "else" "end" "exception" "exclusive" "fetch" "for"
"from" "function" "goto" "grant" "group" "having" "identified" "if"
"in" "index" "indexes" "insert" "intersect" "into" "is" "like" "lock"
"minus" "mode" "nocompress" "not" "nowait" "null" "of" "on" "option"
"or" "order" "overlaps" "procedure" "public" "resource" "revoke"
"select" "share" "size" "sql" "start" "subtype" "tabauth" "table"
"then" "to" "type" "union" "unique" "update" "values" "view" "views"
"when" "where" "with"

"true" "false"
"raise_application_error"
)

     ;; Oracle PL/SQL Keywords
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"a" "add" "agent" "aggregate" "array" "attribute" "authid" "avg"
"bfile_base" "binary" "blob_base" "block" "body" "both" "bound" "bulk"
"byte" "c" "call" "calling" "cascade" "char" "char_base" "character"
"charset" "charsetform" "charsetid" "clob_base" "close" "collect"
"comment" "commit" "committed" "compiled" "constant" "constructor"
"context" "continue" "convert" "count" "current" "customdatum"
"dangling" "data" "date" "date_base" "day" "define" "delete"
"deterministic" "double" "duration" "element" "elsif" "empty" "escape"
"except" "exceptions" "execute" "exists" "exit" "external" "final"
"fixed" "float" "forall" "force" "general" "hash" "heap" "hidden"
"hour" "immediate" "including" "indicator" "indices" "infinite"
"instantiable" "int" "interface" "interval" "invalidate" "isolation"
"java" "language" "large" "leading" "length" "level" "library" "like2"
"like4" "likec" "limit" "limited" "local" "long" "loop" "map" "max"
"maxlen" "member" "merge" "min" "minute" "mod" "modify" "month"
"multiset" "name" "nan" "national" "native" "nchar" "new" "nocopy"
"number_base" "object" "ocicoll" "ocidate" "ocidatetime" "ociduration"
"ociinterval" "ociloblocator" "ocinumber" "ociraw" "ociref"
"ocirefcursor" "ocirowid" "ocistring" "ocitype" "old" "only" "opaque"
"open" "operator" "oracle" "oradata" "organization" "orlany" "orlvary"
"others" "out" "overriding" "package" "parallel_enable" "parameter"
"parameters" "parent" "partition" "pascal" "pipe" "pipelined" "pragma"
"precision" "prior" "private" "raise" "range" "raw" "read" "record"
"ref" "reference" "relies_on" "rem" "remainder" "rename" "result"
"result_cache" "return" "returning" "reverse" "rollback" "row"
"sample" "save" "savepoint" "sb1" "sb2" "sb4" "second" "segment"
"self" "separate" "sequence" "serializable" "set" "short" "size_t"
"some" "sparse" "sqlcode" "sqldata" "sqlname" "sqlstate" "standard"
"static" "stddev" "stored" "string" "struct" "style" "submultiset"
"subpartition" "substitutable" "sum" "synonym" "tdo" "the" "time"
"timestamp" "timezone_abbr" "timezone_hour" "timezone_minute"
"timezone_region" "trailing" "transaction" "transactional" "trusted"
"ub1" "ub2" "ub4" "under" "unsigned" "untrusted" "use" "using"
"valist" "value" "variable" "variance" "varray" "varying" "void"
"while" "work" "wrapped" "write" "year" "zone"
;; Pragma
"autonomous_transaction" "exception_init" "inline"
"restrict_references" "serially_reusable"
)

     ;; Oracle PL/SQL Data Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"\"BINARY LARGE OBJECT\"" "\"CHAR LARGE OBJECT\"" "\"CHAR VARYING\""
"\"CHARACTER LARGE OBJECT\"" "\"CHARACTER VARYING\""
"\"DOUBLE PRECISION\"" "\"INTERVAL DAY TO SECOND\""
"\"INTERVAL YEAR TO MONTH\"" "\"LONG RAW\"" "\"NATIONAL CHAR\""
"\"NATIONAL CHARACTER LARGE OBJECT\"" "\"NATIONAL CHARACTER\""
"\"NCHAR LARGE OBJECT\"" "\"NCHAR\"" "\"NCLOB\"" "\"NVARCHAR2\""
"\"TIME WITH TIME ZONE\"" "\"TIMESTAMP WITH LOCAL TIME ZONE\""
"\"TIMESTAMP WITH TIME ZONE\""
"bfile" "bfile_base" "binary_double" "binary_float" "binary_integer"
"blob" "blob_base" "boolean" "char" "character" "char_base" "clob"
"clob_base" "cursor" "date" "day" "dec" "decimal"
"dsinterval_unconstrained" "float" "int" "integer" "interval" "local"
"long" "mlslabel" "month" "natural" "naturaln" "nchar_cs" "number"
"number_base" "numeric" "pls_integer" "positive" "positiven" "raw"
"real" "ref" "rowid" "second" "signtype" "simple_double"
"simple_float" "simple_integer" "smallint" "string" "time" "timestamp"
"timestamp_ltz_unconstrained" "timestamp_tz_unconstrained"
"timestamp_unconstrained" "time_tz_unconstrained" "time_unconstrained"
"to" "urowid" "varchar" "varchar2" "with" "year"
"yminterval_unconstrained" "zone"
)

     ;; Oracle PL/SQL Exceptions
     (sql-font-lock-keywords-builder 'font-lock-warning-face nil
"access_into_null" "case_not_found" "collection_is_null"
"cursor_already_open" "dup_val_on_index" "invalid_cursor"
"invalid_number" "login_denied" "no_data_found" "no_data_needed"
"not_logged_on" "program_error" "rowtype_mismatch" "self_is_null"
"storage_error" "subscript_beyond_count" "subscript_outside_limit"
"sys_invalid_rowid" "timeout_on_resource" "too_many_rows"
"value_error" "zero_divide"
)))

  "Oracle SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-oracle-font-lock-keywords'.  You may want
to add functions and PL/SQL keywords.")

(defvar sql-mode-postgres-font-lock-keywords
  (eval-when-compile
    (list
     ;; Postgres psql commands
     '("^\\s-*\\\\.*$" . font-lock-doc-face)

     ;; Postgres unreserved words but may have meaning
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil "a"
"abs" "absent" "according" "ada" "alias" "allocate" "are" "array_agg"
"asensitive" "atomic" "attribute" "attributes" "avg" "base64"
"bernoulli" "bit_length" "bitvar" "blob" "blocked" "bom" "breadth" "c"
"call" "cardinality" "catalog_name" "ceil" "ceiling" "char_length"
"character_length" "character_set_catalog" "character_set_name"
"character_set_schema" "characters" "checked" "class_origin" "clob"
"cobol" "collation" "collation_catalog" "collation_name"
"collation_schema" "collect" "column_name" "columns"
"command_function" "command_function_code" "completion" "condition"
"condition_number" "connect" "connection_name" "constraint_catalog"
"constraint_name" "constraint_schema" "constructor" "contains"
"control" "convert" "corr" "corresponding" "count" "covar_pop"
"covar_samp" "cube" "cume_dist" "current_default_transform_group"
"current_path" "current_transform_group_for_type" "cursor_name"
"datalink" "datetime_interval_code" "datetime_interval_precision" "db"
"defined" "degree" "dense_rank" "depth" "deref" "derived" "describe"
"descriptor" "destroy" "destructor" "deterministic" "diagnostics"
"disconnect" "dispatch" "dlnewcopy" "dlpreviouscopy" "dlurlcomplete"
"dlurlcompleteonly" "dlurlcompletewrite" "dlurlpath" "dlurlpathonly"
"dlurlpathwrite" "dlurlscheme" "dlurlserver" "dlvalue" "dynamic"
"dynamic_function" "dynamic_function_code" "element" "empty"
"end-exec" "equals" "every" "exception" "exec" "existing" "exp" "file"
"filter" "final" "first_value" "flag" "floor" "fortran" "found" "free"
"fs" "fusion" "g" "general" "generated" "get" "go" "goto" "grouping"
"hex" "hierarchy" "host" "id" "ignore" "implementation" "import"
"indent" "indicator" "infix" "initialize" "instance" "instantiable"
"integrity" "intersection" "iterate" "k" "key_member" "key_type" "lag"
"last_value" "lateral" "lead" "length" "less" "library" "like_regex"
"link" "ln" "locator" "lower" "m" "map" "matched" "max"
"max_cardinality" "member" "merge" "message_length"
"message_octet_length" "message_text" "method" "min" "mod" "modifies"
"modify" "module" "more" "multiset" "mumps" "namespace" "nclob"
"nesting" "new" "nfc" "nfd" "nfkc" "nfkd" "nil" "normalize"
"normalized" "nth_value" "ntile" "nullable" "number"
"occurrences_regex" "octet_length" "octets" "old" "open" "operation"
"ordering" "ordinality" "others" "output" "overriding" "p" "pad"
"parameter" "parameter_mode" "parameter_name"
"parameter_ordinal_position" "parameter_specific_catalog"
"parameter_specific_name" "parameter_specific_schema" "parameters"
"pascal" "passing" "passthrough" "percent_rank" "percentile_cont"
"percentile_disc" "permission" "pli" "position_regex" "postfix"
"power" "prefix" "preorder" "public" "rank" "reads" "recovery" "ref"
"referencing" "regr_avgx" "regr_avgy" "regr_count" "regr_intercept"
"regr_r2" "regr_slope" "regr_sxx" "regr_sxy" "regr_syy" "requiring"
"respect" "restore" "result" "return" "returned_cardinality"
"returned_length" "returned_octet_length" "returned_sqlstate" "rollup"
"routine" "routine_catalog" "routine_name" "routine_schema"
"row_count" "row_number" "scale" "schema_name" "scope" "scope_catalog"
"scope_name" "scope_schema" "section" "selective" "self" "sensitive"
"server_name" "sets" "size" "source" "space" "specific"
"specific_name" "specifictype" "sql" "sqlcode" "sqlerror"
"sqlexception" "sqlstate" "sqlwarning" "sqrt" "state" "static"
"stddev_pop" "stddev_samp" "structure" "style" "subclass_origin"
"sublist" "submultiset" "substring_regex" "sum" "system_user" "t"
"table_name" "tablesample" "terminate" "than" "ties" "timezone_hour"
"timezone_minute" "token" "top_level_count" "transaction_active"
"transactions_committed" "transactions_rolled_back" "transform"
"transforms" "translate" "translate_regex" "translation"
"trigger_catalog" "trigger_name" "trigger_schema" "trim_array"
"uescape" "under" "unlink" "unnamed" "unnest" "untyped" "upper" "uri"
"usage" "user_defined_type_catalog" "user_defined_type_code"
"user_defined_type_name" "user_defined_type_schema" "var_pop"
"var_samp" "varbinary" "variable" "whenever" "width_bucket" "within"
"xmlagg" "xmlbinary" "xmlcast" "xmlcomment" "xmldeclaration"
"xmldocument" "xmlexists" "xmliterate" "xmlnamespaces" "xmlquery"
"xmlschema" "xmltable" "xmltext" "xmlvalidate"
)

     ;; Postgres non-reserved words
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"abort" "absolute" "access" "action" "add" "admin" "after" "aggregate"
"also" "alter" "always" "assertion" "assignment" "at" "backward"
"before" "begin" "between" "by" "cache" "called" "cascade" "cascaded"
"catalog" "chain" "characteristics" "checkpoint" "class" "close"
"cluster" "coalesce" "comment" "comments" "commit" "committed"
"configuration" "connection" "constraints" "content" "continue"
"conversion" "copy" "cost" "createdb" "createrole" "createuser" "csv"
"current" "cursor" "cycle" "data" "database" "day" "deallocate" "dec"
"declare" "defaults" "deferred" "definer" "delete" "delimiter"
"delimiters" "dictionary" "disable" "discard" "document" "domain"
"drop" "each" "enable" "encoding" "encrypted" "enum" "escape"
"exclude" "excluding" "exclusive" "execute" "exists" "explain"
"external" "extract" "family" "first" "float" "following" "force"
"forward" "function" "functions" "global" "granted" "greatest"
"handler" "header" "hold" "hour" "identity" "if" "immediate"
"immutable" "implicit" "including" "increment" "index" "indexes"
"inherit" "inherits" "inline" "inout" "input" "insensitive" "insert"
"instead" "invoker" "isolation" "key" "language" "large" "last"
"lc_collate" "lc_ctype" "least" "level" "listen" "load" "local"
"location" "lock" "login" "mapping" "match" "maxvalue" "minute"
"minvalue" "mode" "month" "move" "name" "names" "national" "nchar"
"next" "no" "nocreatedb" "nocreaterole" "nocreateuser" "noinherit"
"nologin" "none" "nosuperuser" "nothing" "notify" "nowait" "nullif"
"nulls" "object" "of" "oids" "operator" "option" "options" "out"
"overlay" "owned" "owner" "parser" "partial" "partition" "password"
"plans" "position" "preceding" "prepare" "prepared" "preserve" "prior"
"privileges" "procedural" "procedure" "quote" "range" "read"
"reassign" "recheck" "recursive" "reindex" "relative" "release"
"rename" "repeatable" "replace" "replica" "reset" "restart" "restrict"
"returns" "revoke" "role" "rollback" "row" "rows" "rule" "savepoint"
"schema" "scroll" "search" "second" "security" "sequence" "sequences"
"serializable" "server" "session" "set" "setof" "share" "show"
"simple" "stable" "standalone" "start" "statement" "statistics"
"stdin" "stdout" "storage" "strict" "strip" "substring" "superuser"
"sysid" "system" "tables" "tablespace" "temp" "template" "temporary"
"transaction" "treat" "trigger" "trim" "truncate" "trusted" "type"
"unbounded" "uncommitted" "unencrypted" "unknown" "unlisten" "until"
"update" "vacuum" "valid" "validator" "value" "values" "version"
"view" "volatile" "whitespace" "work" "wrapper" "write"
"xmlattributes" "xmlconcat" "xmlelement" "xmlforest" "xmlparse"
"xmlpi" "xmlroot" "xmlserialize" "year" "yes"
)

     ;; Postgres Reserved
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"all" "analyse" "analyze" "and" "any" "array" "asc" "as" "asymmetric"
"authorization" "binary" "both" "case" "cast" "check" "collate"
"column" "concurrently" "constraint" "create" "cross"
"current_catalog" "current_date" "current_role" "current_schema"
"current_time" "current_timestamp" "current_user" "default"
"deferrable" "desc" "distinct" "do" "else" "end" "except" "false"
"fetch" "foreign" "for" "freeze" "from" "full" "grant" "group"
"having" "ilike" "initially" "inner" "in" "intersect" "into" "isnull"
"is" "join" "leading" "left" "like" "limit" "localtime"
"localtimestamp" "natural" "notnull" "not" "null" "off" "offset"
"only" "on" "order" "or" "outer" "overlaps" "over" "placing" "primary"
"references" "returning" "right" "select" "session_user" "similar"
"some" "symmetric" "table" "then" "to" "trailing" "true" "union"
"unique" "user" "using" "variadic" "verbose" "when" "where" "window"
"with"
)

     ;; Postgres Data Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"bigint" "bigserial" "bit" "bool" "boolean" "box" "bytea" "char"
"character" "cidr" "circle" "date" "decimal" "double" "float4"
"float8" "inet" "int" "int2" "int4" "int8" "integer" "interval" "line"
"lseg" "macaddr" "money" "numeric" "path" "point" "polygon"
"precision" "real" "serial" "serial4" "serial8" "smallint" "text"
"time" "timestamp" "timestamptz" "timetz" "tsquery" "tsvector"
"txid_snapshot" "uuid" "varbit" "varchar" "varying" "without"
"xml" "zone"
)))

  "Postgres SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-postgres-font-lock-keywords'.")

(defvar sql-mode-linter-font-lock-keywords
  (eval-when-compile
    (list
     ;; Linter Keywords
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"autocommit" "autoinc" "autorowid" "cancel" "cascade" "channel"
"committed" "count" "countblob" "cross" "current" "data" "database"
"datafile" "datafiles" "datesplit" "dba" "dbname" "default" "deferred"
"denied" "description" "device" "difference" "directory" "error"
"escape" "euc" "exclusive" "external" "extfile" "false" "file"
"filename" "filesize" "filetime" "filter" "findblob" "first" "foreign"
"full" "fuzzy" "global" "granted" "ignore" "immediate" "increment"
"indexes" "indexfile" "indexfiles" "indextime" "initial" "integrity"
"internal" "key" "last_autoinc" "last_rowid" "limit" "linter"
"linter_file_device" "linter_file_size" "linter_name_length" "ln"
"local" "login" "maxisn" "maxrow" "maxrowid" "maxvalue" "message"
"minvalue" "module" "names" "national" "natural" "new" "new_table"
"no" "node" "noneuc" "nulliferror" "numbers" "off" "old" "old_table"
"only" "operation" "optimistic" "option" "page" "partially" "password"
"phrase" "plan" "precision" "primary" "priority" "privileges"
"proc_info_size" "proc_par_name_len" "protocol" "quant" "range" "raw"
"read" "record" "records" "references" "remote" "rename" "replication"
"restart" "rewrite" "root" "row" "rule" "savepoint" "security"
"sensitive" "sequence" "serializable" "server" "since" "size" "some"
"startup" "statement" "station" "success" "sys_guid" "tables" "test"
"timeout" "trace" "transaction" "translation" "trigger"
"trigger_info_size" "true" "trunc" "uncommitted" "unicode" "unknown"
"unlimited" "unlisted" "user" "utf8" "value" "varying" "volumes"
"wait" "windows_code" "workspace" "write" "xml"
)

     ;; Linter Reserved
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"access" "action" "add" "address" "after" "all" "alter" "always" "and"
"any" "append" "as" "asc" "ascic" "async" "at_begin" "at_end" "audit"
"aud_obj_name_len" "backup" "base" "before" "between" "blobfile"
"blobfiles" "blobpct" "brief" "browse" "by" "case" "cast" "check"
"clear" "close" "column" "comment" "commit" "connect" "contains"
"correct" "create" "delete" "desc" "disable" "disconnect" "distinct"
"drop" "each" "ef" "else" "enable" "end" "event" "except" "exclude"
"execute" "exists" "extract" "fetch" "finish" "for" "from" "get"
"grant" "group" "having" "identified" "in" "index" "inner" "insert"
"instead" "intersect" "into" "is" "isolation" "join" "left" "level"
"like" "lock" "mode" "modify" "not" "nowait" "null" "of" "on" "open"
"or" "order" "outer" "owner" "press" "prior" "procedure" "public"
"purge" "rebuild" "resource" "restrict" "revoke" "right" "role"
"rollback" "rownum" "select" "session" "set" "share" "shutdown"
"start" "stop" "sync" "synchronize" "synonym" "sysdate" "table" "then"
"to" "union" "unique" "unlock" "until" "update" "using" "values"
"view" "when" "where" "with" "without"
)

     ;; Linter Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"abs" "acos" "asin" "atan" "atan2" "avg" "ceil" "cos" "cosh" "divtime"
"exp" "floor" "getbits" "getblob" "getbyte" "getlong" "getraw"
"getstr" "gettext" "getword" "hextoraw" "lenblob" "length" "log"
"lower" "lpad" "ltrim" "max" "min" "mod" "monthname" "nvl"
"octet_length" "power" "rand" "rawtohex" "repeat_string"
"right_substr" "round" "rpad" "rtrim" "sign" "sin" "sinh" "soundex"
"sqrt" "sum" "tan" "tanh" "timeint_to_days" "to_char" "to_date"
"to_gmtime" "to_localtime" "to_number" "trim" "upper" "decode"
"substr" "substring" "chr" "dayname" "days" "greatest" "hex" "initcap"
"instr" "least" "multime" "replace" "width"
)

     ;; Linter Data Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"bigint" "bitmap" "blob" "boolean" "char" "character" "date"
"datetime" "dec" "decimal" "double" "float" "int" "integer" "nchar"
"number" "numeric" "real" "smallint" "varbyte" "varchar" "byte"
"cursor" "long"
)))

  "Linter SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.")

(defvar sql-mode-ms-font-lock-keywords
  (eval-when-compile
    (list
     ;; MS isql/osql Commands
     (cons
      (concat
       "^\\(?:\\(?:set\\s-+\\(?:"
       (regexp-opt '(
"datefirst" "dateformat" "deadlock_priority" "lock_timeout"
"concat_null_yields_null" "cursor_close_on_commit"
"disable_def_cnst_chk" "fips_flagger" "identity_insert" "language"
"offsets" "quoted_identifier" "arithabort" "arithignore" "fmtonly"
"nocount" "noexec" "numeric_roundabort" "parseonly"
"query_governor_cost_limit" "rowcount" "textsize" "ansi_defaults"
"ansi_null_dflt_off" "ansi_null_dflt_on" "ansi_nulls" "ansi_padding"
"ansi_warnings" "forceplan" "showplan_all" "showplan_text"
"statistics" "implicit_transactions" "remote_proc_transactions"
"transaction" "xact_abort"
) t)
       "\\)\\)\\|go\\s-*\\|use\\s-+\\|setuser\\s-+\\|dbcc\\s-+\\).*$")
      'font-lock-doc-face)

     ;; MS Reserved
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"absolute" "add" "all" "alter" "and" "any" "as" "asc" "authorization"
"avg" "backup" "begin" "between" "break" "browse" "bulk" "by"
"cascade" "case" "check" "checkpoint" "close" "clustered" "coalesce"
"column" "commit" "committed" "compute" "confirm" "constraint"
"contains" "containstable" "continue" "controlrow" "convert" "count"
"create" "cross" "current" "current_date" "current_time"
"current_timestamp" "current_user" "database" "deallocate" "declare"
"default" "delete" "deny" "desc" "disk" "distinct" "distributed"
"double" "drop" "dummy" "dump" "else" "end" "errlvl" "errorexit"
"escape" "except" "exec" "execute" "exists" "exit" "fetch" "file"
"fillfactor" "first" "floppy" "for" "foreign" "freetext"
"freetexttable" "from" "full" "goto" "grant" "group" "having"
"holdlock" "identity" "identity_insert" "identitycol" "if" "in"
"index" "inner" "insert" "intersect" "into" "is" "isolation" "join"
"key" "kill" "last" "left" "level" "like" "lineno" "load" "max" "min"
"mirrorexit" "national" "next" "nocheck" "nolock" "nonclustered" "not"
"null" "nullif" "of" "off" "offsets" "on" "once" "only" "open"
"opendatasource" "openquery" "openrowset" "option" "or" "order"
"outer" "output" "over" "paglock" "percent" "perm" "permanent" "pipe"
"plan" "precision" "prepare" "primary" "print" "prior" "privileges"
"proc" "procedure" "processexit" "public" "raiserror" "read"
"readcommitted" "readpast" "readtext" "readuncommitted" "reconfigure"
"references" "relative" "repeatable" "repeatableread" "replication"
"restore" "restrict" "return" "revoke" "right" "rollback" "rowcount"
"rowguidcol" "rowlock" "rule" "save" "schema" "select" "serializable"
"session_user" "set" "shutdown" "some" "statistics" "sum"
"system_user" "table" "tablock" "tablockx" "tape" "temp" "temporary"
"textsize" "then" "to" "top" "tran" "transaction" "trigger" "truncate"
"tsequal" "uncommitted" "union" "unique" "update" "updatetext"
"updlock" "use" "user" "values" "view" "waitfor" "when" "where"
"while" "with" "work" "writetext" "collate" "function" "openxml"
"returns"
)

     ;; MS Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"@@connections" "@@cpu_busy" "@@cursor_rows" "@@datefirst" "@@dbts"
"@@error" "@@fetch_status" "@@identity" "@@idle" "@@io_busy"
"@@langid" "@@language" "@@lock_timeout" "@@max_connections"
"@@max_precision" "@@nestlevel" "@@options" "@@pack_received"
"@@pack_sent" "@@packet_errors" "@@procid" "@@remserver" "@@rowcount"
"@@servername" "@@servicename" "@@spid" "@@textsize" "@@timeticks"
"@@total_errors" "@@total_read" "@@total_write" "@@trancount"
"@@version" "abs" "acos" "and" "app_name" "ascii" "asin" "atan" "atn2"
"avg" "case" "cast" "ceiling" "char" "charindex" "coalesce"
"col_length" "col_name" "columnproperty" "containstable" "convert"
"cos" "cot" "count" "current_timestamp" "current_user" "cursor_status"
"databaseproperty" "datalength" "dateadd" "datediff" "datename"
"datepart" "day" "db_id" "db_name" "degrees" "difference" "exp"
"file_id" "file_name" "filegroup_id" "filegroup_name"
"filegroupproperty" "fileproperty" "floor" "formatmessage"
"freetexttable" "fulltextcatalogproperty" "fulltextserviceproperty"
"getansinull" "getdate" "grouping" "host_id" "host_name" "ident_incr"
"ident_seed" "identity" "index_col" "indexproperty" "is_member"
"is_srvrolemember" "isdate" "isnull" "isnumeric" "left" "len" "log"
"log10" "lower" "ltrim" "max" "min" "month" "nchar" "newid" "nullif"
"object_id" "object_name" "objectproperty" "openquery" "openrowset"
"parsename" "patindex" "patindex" "permissions" "pi" "power"
"quotename" "radians" "rand" "replace" "replicate" "reverse" "right"
"round" "rtrim" "session_user" "sign" "sin" "soundex" "space" "sqrt"
"square" "stats_date" "stdev" "stdevp" "str" "stuff" "substring" "sum"
"suser_id" "suser_name" "suser_sid" "suser_sname" "system_user" "tan"
"textptr" "textvalid" "typeproperty" "unicode" "upper" "user"
"user_id" "user_name" "var" "varp" "year"
)

     ;; MS Variables
     '("\\b@[a-zA-Z0-9_]*\\b" . font-lock-variable-name-face)

     ;; MS Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"binary" "bit" "char" "character" "cursor" "datetime" "dec" "decimal"
"double" "float" "image" "int" "integer" "money" "national" "nchar"
"ntext" "numeric" "numeric" "nvarchar" "precision" "real"
"smalldatetime" "smallint" "smallmoney" "text" "timestamp" "tinyint"
"uniqueidentifier" "varbinary" "varchar" "varying"
)))

  "Microsoft SQLServer SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-ms-font-lock-keywords'.")

(defvar sql-mode-sybase-font-lock-keywords nil
  "Sybase SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-sybase-font-lock-keywords'.")

(defvar sql-mode-informix-font-lock-keywords nil
  "Informix SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-informix-font-lock-keywords'.")

(defvar sql-mode-interbase-font-lock-keywords nil
  "Interbase SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-interbase-font-lock-keywords'.")

(defvar sql-mode-ingres-font-lock-keywords nil
  "Ingres SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-interbase-font-lock-keywords'.")

(defvar sql-mode-solid-font-lock-keywords nil
  "Solid SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-solid-font-lock-keywords'.")

(defvar sql-mode-mysql-font-lock-keywords
  (eval-when-compile
    (list
     ;; MySQL Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
"ascii" "avg" "bdmpolyfromtext" "bdmpolyfromwkb" "bdpolyfromtext"
"bdpolyfromwkb" "benchmark" "bin" "bit_and" "bit_length" "bit_or"
"bit_xor" "both" "cast" "char_length" "character_length" "coalesce"
"concat" "concat_ws" "connection_id" "conv" "convert" "count"
"curdate" "current_date" "current_time" "current_timestamp" "curtime"
"elt" "encrypt" "export_set" "field" "find_in_set" "found_rows" "from"
"geomcollfromtext" "geomcollfromwkb" "geometrycollectionfromtext"
"geometrycollectionfromwkb" "geometryfromtext" "geometryfromwkb"
"geomfromtext" "geomfromwkb" "get_lock" "group_concat" "hex" "ifnull"
"instr" "interval" "isnull" "last_insert_id" "lcase" "leading"
"length" "linefromtext" "linefromwkb" "linestringfromtext"
"linestringfromwkb" "load_file" "locate" "lower" "lpad" "ltrim"
"make_set" "master_pos_wait" "max" "mid" "min" "mlinefromtext"
"mlinefromwkb" "mpointfromtext" "mpointfromwkb" "mpolyfromtext"
"mpolyfromwkb" "multilinestringfromtext" "multilinestringfromwkb"
"multipointfromtext" "multipointfromwkb" "multipolygonfromtext"
"multipolygonfromwkb" "now" "nullif" "oct" "octet_length" "ord"
"pointfromtext" "pointfromwkb" "polyfromtext" "polyfromwkb"
"polygonfromtext" "polygonfromwkb" "position" "quote" "rand"
"release_lock" "repeat" "replace" "reverse" "rpad" "rtrim" "soundex"
"space" "std" "stddev" "substring" "substring_index" "sum" "sysdate"
"trailing" "trim" "ucase" "unix_timestamp" "upper" "user" "variance"
)

     ;; MySQL Keywords
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"action" "add" "after" "against" "all" "alter" "and" "as" "asc"
"auto_increment" "avg_row_length" "bdb" "between" "by" "cascade"
"case" "change" "character" "check" "checksum" "close" "collate"
"collation" "column" "columns" "comment" "committed" "concurrent"
"constraint" "create" "cross" "data" "database" "default"
"delay_key_write" "delayed" "delete" "desc" "directory" "disable"
"distinct" "distinctrow" "do" "drop" "dumpfile" "duplicate" "else" "elseif"
"enable" "enclosed" "end" "escaped" "exists" "fields" "first" "for"
"force" "foreign" "from" "full" "fulltext" "global" "group" "handler"
"having" "heap" "high_priority" "if" "ignore" "in" "index" "infile"
"inner" "insert" "insert_method" "into" "is" "isam" "isolation" "join"
"key" "keys" "last" "left" "level" "like" "limit" "lines" "load"
"local" "lock" "low_priority" "match" "max_rows" "merge" "min_rows"
"mode" "modify" "mrg_myisam" "myisam" "natural" "next" "no" "not"
"null" "offset" "oj" "on" "open" "optionally" "or" "order" "outer"
"outfile" "pack_keys" "partial" "password" "prev" "primary"
"procedure" "quick" "raid0" "raid_type" "read" "references" "rename"
"repeatable" "restrict" "right" "rollback" "rollup" "row_format"
"savepoint" "select" "separator" "serializable" "session" "set"
"share" "show" "sql_big_result" "sql_buffer_result" "sql_cache"
"sql_calc_found_rows" "sql_no_cache" "sql_small_result" "starting"
"straight_join" "striped" "table" "tables" "temporary" "terminated"
"then" "to" "transaction" "truncate" "type" "uncommitted" "union"
"unique" "unlock" "update" "use" "using" "values" "when" "where"
"with" "write" "xor"
)

     ;; MySQL Data Types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"bigint" "binary" "bit" "blob" "bool" "boolean" "char" "curve" "date"
"datetime" "dec" "decimal" "double" "enum" "fixed" "float" "geometry"
"geometrycollection" "int" "integer" "line" "linearring" "linestring"
"longblob" "longtext" "mediumblob" "mediumint" "mediumtext"
"multicurve" "multilinestring" "multipoint" "multipolygon"
"multisurface" "national" "numeric" "point" "polygon" "precision"
"real" "smallint" "surface" "text" "time" "timestamp" "tinyblob"
"tinyint" "tinytext" "unsigned" "varchar" "year" "year2" "year4"
"zerofill"
)))

  "MySQL SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-mysql-font-lock-keywords'.")

(defvar sql-mode-sqlite-font-lock-keywords
  (eval-when-compile
    (list
     ;; SQLite commands
     '("^[.].*$" . font-lock-doc-face)

     ;; SQLite Keyword
     (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
"abort" "action" "add" "after" "all" "alter" "analyze" "and" "as"
"asc" "attach" "autoincrement" "before" "begin" "between" "by"
"cascade" "case" "cast" "check" "collate" "column" "commit" "conflict"
"constraint" "create" "cross" "database" "default" "deferrable"
"deferred" "delete" "desc" "detach" "distinct" "drop" "each" "else"
"end" "escape" "except" "exclusive" "exists" "explain" "fail" "for"
"foreign" "from" "full" "glob" "group" "having" "if" "ignore"
"immediate" "in" "index" "indexed" "initially" "inner" "insert"
"instead" "intersect" "into" "is" "isnull" "join" "key" "left" "like"
"limit" "match" "natural" "no" "not" "notnull" "null" "of" "offset"
"on" "or" "order" "outer" "plan" "pragma" "primary" "query" "raise"
"references" "regexp" "reindex" "release" "rename" "replace"
"restrict" "right" "rollback" "row" "savepoint" "select" "set" "table"
"temp" "temporary" "then" "to" "transaction" "trigger" "union"
"unique" "update" "using" "vacuum" "values" "view" "virtual" "when"
"where"
)
     ;; SQLite Data types
     (sql-font-lock-keywords-builder 'font-lock-type-face nil
"int" "integer" "tinyint" "smallint" "mediumint" "bigint" "unsigned"
"big" "int2" "int8" "character" "varchar" "varying" "nchar" "native"
"nvarchar" "text" "clob" "blob" "real" "double" "precision" "float"
"numeric" "number" "decimal" "boolean" "date" "datetime"
)
     ;; SQLite Functions
     (sql-font-lock-keywords-builder 'font-lock-builtin-face nil
;; Core functions
"abs" "changes" "coalesce" "glob" "ifnull" "hex" "last_insert_rowid"
"length" "like" "load_extension" "lower" "ltrim" "max" "min" "nullif"
"quote" "random" "randomblob" "replace" "round" "rtrim" "soundex"
"sqlite_compileoption_get" "sqlite_compileoption_used"
"sqlite_source_id" "sqlite_version" "substr" "total_changes" "trim"
"typeof" "upper" "zeroblob"
;; Date/time functions
"time" "julianday" "strftime"
"current_date" "current_time" "current_timestamp"
;; Aggregate functions
"avg" "count" "group_concat" "max" "min" "sum" "total"
)))

  "SQLite SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-sqlite-font-lock-keywords'.")

(defvar sql-mode-db2-font-lock-keywords nil
  "DB2 SQL keywords used by font-lock.

This variable is used by `sql-mode' and `sql-interactive-mode'.  The
regular expressions are created during compilation by calling the
function `regexp-opt'.  Therefore, take a look at the source before
you define your own `sql-mode-db2-font-lock-keywords'.")

(defvar sql-mode-font-lock-keywords nil
  "SQL keywords used by font-lock.

Setting this variable directly no longer has any affect.  Use
`sql-product' and `sql-add-product-keywords' to control the
highlighting rules in SQL mode.")



;;; SQL Product support functions

(defun sql-read-product (prompt &optional initial)
  "Read a valid SQL product."
  (let ((init (or (and initial (symbol-name initial)) "ansi")))
    (intern (completing-read
             prompt
             (mapcar (lambda (info) (symbol-name (car info)))
                     sql-product-alist)
             nil 'require-match
             init 'sql-product-history init))))

(defun sql-add-product (product display &rest plist)
  "Add support for a database product in `sql-mode'.

Add PRODUCT to `sql-product-alist' which enables `sql-mode' to
properly support syntax highlighting and interactive interaction.
DISPLAY is the name of the SQL product that will appear in the
menu bar and in messages.  PLIST initializes the product
configuration."

  ;; Don't do anything if the product is already supported
  (if (assoc product sql-product-alist)
      (message "Product `%s' is already defined" product)

    ;; Add product to the alist
    (add-to-list 'sql-product-alist `((,product :name ,display . ,plist)))
    ;; Add a menu item to the SQL->Product menu
    (easy-menu-add-item sql-mode-menu '("Product")
			;; Each product is represented by a radio
			;; button with it's display name.
			`[,display
			  (sql-set-product ',product)
			 :style radio
			 :selected (eq sql-product ',product)]
			;; Maintain the product list in
			;; (case-insensitive) alphabetic order of the
			;; display names.  Loop thru each keymap item
			;; looking for an item whose display name is
			;; after this product's name.
			(let ((next-item)
			      (down-display (downcase display)))
			  (map-keymap (lambda (k b)
					(when (and (not next-item)
						   (string-lessp down-display
								 (downcase (cadr b))))
					  (setq next-item k)))
				      (easy-menu-get-map sql-mode-menu '("Product")))
			  next-item))
    product))

(defun sql-del-product (product)
  "Remove support for PRODUCT in `sql-mode'."

  ;; Remove the menu item based on the display name
  (easy-menu-remove-item sql-mode-menu '("Product") (sql-get-product-feature product :name))
  ;; Remove the product alist item
  (setq sql-product-alist (assq-delete-all product sql-product-alist))
  nil)

(defun sql-set-product-feature (product feature newvalue)
  "Set FEATURE of database PRODUCT to NEWVALUE.

The PRODUCT must be a symbol which identifies the database
product.  The product must have already exist on the product
list.  See `sql-add-product' to add new products.  The FEATURE
argument must be a plist keyword accepted by
`sql-product-alist'."

  (let* ((p (assoc product sql-product-alist))
         (v (plist-get (cdr p) feature)))
    (if p
        (if (and
             (member feature sql-indirect-features)
             (symbolp v))
            (set v newvalue)
          (setcdr p (plist-put (cdr p) feature newvalue)))
      (message "`%s' is not a known product; use `sql-add-product' to add it first." product))))

(defun sql-get-product-feature (product feature &optional fallback not-indirect)
  "Lookup FEATURE associated with a SQL PRODUCT.

If the FEATURE is nil for PRODUCT, and FALLBACK is specified,
then the FEATURE associated with the FALLBACK product is
returned.

If the FEATURE is in the list `sql-indirect-features', and the
NOT-INDIRECT parameter is not set, then the value of the symbol
stored in the connect alist is returned.

See `sql-product-alist' for a list of products and supported features."
  (let* ((p (assoc product sql-product-alist))
         (v (plist-get (cdr p) feature)))

    (if p
        ;; If no value and fallback, lookup feature for fallback
        (if (and (not v)
                 fallback
                 (not (eq product fallback)))
            (sql-get-product-feature fallback feature)

          (if (and
               (member feature sql-indirect-features)
               (not not-indirect)
               (symbolp v))
              (symbol-value v)
            v))
      (message "`%s' is not a known product; use `sql-add-product' to add it first." product)
      nil)))

(defun sql-product-font-lock (keywords-only imenu)
  "Configure font-lock and imenu with product-specific settings.

The KEYWORDS-ONLY flag is passed to font-lock to specify whether
only keywords should be highlighted and syntactic highlighting
skipped.  The IMENU flag indicates whether `imenu-mode' should
also be configured."

  (let
      ;; Get the product-specific syntax-alist.
      ((syntax-alist (sql-product-font-lock-syntax-alist)))

    ;; Get the product-specific keywords.
    (set (make-local-variable 'sql-mode-font-lock-keywords)
         (append
          (unless (eq sql-product 'ansi)
            (sql-get-product-feature sql-product :font-lock))
          ;; Always highlight ANSI keywords
          (sql-get-product-feature 'ansi :font-lock)
          ;; Fontify object names in CREATE, DROP and ALTER DDL
          ;; statements
          (list sql-mode-font-lock-object-name)))

    ;; Setup font-lock.  Force re-parsing of `font-lock-defaults'.
    (kill-local-variable 'font-lock-set-defaults)
    (set (make-local-variable 'font-lock-defaults)
         (list 'sql-mode-font-lock-keywords
               keywords-only t syntax-alist))

    ;; Force font lock to reinitialize if it is already on
    ;; Otherwise, we can wait until it can be started.
    (when (and (fboundp 'font-lock-mode)
	       (boundp 'font-lock-mode)
	       font-lock-mode)
      (font-lock-mode-internal nil)
      (font-lock-mode-internal t))

    (add-hook 'font-lock-mode-hook
	      (lambda ()
		;; Provide defaults for new font-lock faces.
		(defvar font-lock-builtin-face
		  (if (boundp 'font-lock-preprocessor-face)
		      font-lock-preprocessor-face
		    font-lock-keyword-face))
		(defvar font-lock-doc-face font-lock-string-face))
	      nil t)

    ;; Setup imenu; it needs the same syntax-alist.
    (when imenu
      (setq imenu-syntax-alist syntax-alist))))

;;;###autoload
(defun sql-add-product-keywords (product keywords &optional append)
  "Add highlighting KEYWORDS for SQL PRODUCT.

PRODUCT should be a symbol, the name of a SQL product, such as
`oracle'.  KEYWORDS should be a list; see the variable
`font-lock-keywords'.  By default they are added at the beginning
of the current highlighting list.  If optional argument APPEND is
`set', they are used to replace the current highlighting list.
If APPEND is any other non-nil value, they are added at the end
of the current highlighting list.

For example:

 (sql-add-product-keywords 'ms
  '((\"\\\\b\\\\w+_t\\\\b\" . font-lock-type-face)))

adds a fontification pattern to fontify identifiers ending in
`_t' as data types."

  (let* ((sql-indirect-features nil)
         (font-lock-var (sql-get-product-feature product :font-lock))
         (old-val))

    (setq old-val (symbol-value font-lock-var))
    (set font-lock-var
	 (if (eq append 'set)
	     keywords
	   (if append
	       (append old-val keywords)
	     (append keywords old-val))))))

(defun sql-for-each-login (login-params body)
  "Iterates through login parameters and returns a list of results."

  (delq nil
        (mapcar
         (lambda (param)
           (let ((token (or (and (listp param) (car param)) param))
                 (plist (or (and (listp param) (cdr param)) nil)))

             (funcall body token plist)))
         login-params)))



;;; Functions to switch highlighting

(defun sql-product-syntax-table ()
  (let ((table (copy-syntax-table sql-mode-syntax-table)))
    (mapc (lambda (entry)
            (modify-syntax-entry (car entry) (cdr entry) table))
          (sql-get-product-feature sql-product :syntax-alist))
    table))

(defun sql-product-font-lock-syntax-alist ()
  (append
   ;; Change all symbol character to word characters
   (mapcar
    (lambda (entry) (if (string= (substring (cdr entry) 0 1) "_")
                        (cons (car entry)
                              (concat "w" (substring (cdr entry) 1)))
                      entry))
    (sql-get-product-feature sql-product :syntax-alist))
   '((?_ . "w"))))

(defun sql-highlight-product ()
  "Turn on the font highlighting for the SQL product selected."
  (when (derived-mode-p 'sql-mode)
    ;; Enhance the syntax table for the product
    (set-syntax-table (sql-product-syntax-table))

    ;; Setup font-lock
    (sql-product-font-lock nil t)

    ;; Set the mode name to include the product.
    (setq mode-name (concat "SQL[" (or (sql-get-product-feature sql-product :name)
				       (symbol-name sql-product)) "]"))))

(defun sql-set-product (product)
  "Set `sql-product' to PRODUCT and enable appropriate highlighting."
  (interactive
   (list (sql-read-product "SQL product: ")))
  (if (stringp product) (setq product (intern product)))
  (when (not (assoc product sql-product-alist))
    (error "SQL product %s is not supported; treated as ANSI" product)
    (setq product 'ansi))

  ;; Save product setting and fontify.
  (setq sql-product product)
  (sql-highlight-product))


;;; Compatibility functions

(if (not (fboundp 'comint-line-beginning-position))
    ;; comint-line-beginning-position is defined in Emacs 21
    (defun comint-line-beginning-position ()
      "Return the buffer position of the beginning of the line, after any prompt.
The prompt is assumed to be any text at the beginning of the line
matching the regular expression `comint-prompt-regexp', a buffer
local variable."
      (save-excursion (comint-bol nil) (point))))

;;; Motion Functions

(defun sql-statement-regexp (prod)
  (let* ((ansi-stmt (sql-get-product-feature 'ansi :statement))
         (prod-stmt (sql-get-product-feature prod  :statement)))
    (concat "^\\<"
            (if prod-stmt
                ansi-stmt
              (concat "\\(" ansi-stmt "\\|" prod-stmt "\\)"))
            "\\>")))

(defun sql-beginning-of-statement (arg)
  "Moves the cursor to the beginning of the current SQL statement."
  (interactive "p")

  (let ((here (point))
        (regexp (sql-statement-regexp sql-product))
        last next)

    ;; Go to the end of the statement before the start we desire
    (setq last (or (sql-end-of-statement (- arg))
                   (point-min)))
    ;; And find the end after that
    (setq next (or (sql-end-of-statement 1)
                   (point-max)))

    ;; Our start must be between them
    (goto-char last)
    ;; Find an beginning-of-stmt that's not in a comment
    (while (and (re-search-forward regexp next t 1)
                (nth 7 (syntax-ppss)))
      (goto-char (match-end 0)))
    (goto-char
     (if (match-data)
        (match-beginning 0)
       last))
    (beginning-of-line)
    ;; If we didn't move, try again
    (when (= here (point))
      (sql-beginning-of-statement (* 2 (sql-signum arg))))))

(defun sql-end-of-statement (arg)
  "Moves the cursor to the end of the current SQL statement."
  (interactive "p")
  (let ((term (sql-get-product-feature sql-product :terminator))
        (re-search (if (> 0 arg) 're-search-backward 're-search-forward))
        (here (point))
        (n 0))
    (when (consp term)
      (setq term (car term)))
    ;; Iterate until we've moved the desired number of stmt ends
    (while (not (= (sql-signum arg) 0))
      ;; if we're looking at the terminator, jump by 2
      (if (or (and (> 0 arg) (looking-back term))
              (and (< 0 arg) (looking-at term)))
          (setq n 2)
        (setq n 1))
      ;; If we found another end-of-stmt
      (if (not (apply re-search term nil t n nil))
          (setq arg 0)
        ;; count it if we're not in a comment
        (unless (nth 7 (syntax-ppss))
          (setq arg (- arg (sql-signum arg))))))
    (goto-char (if (match-data)
                   (match-end 0)
                 here))))

;;; Small functions

(defun sql-magic-go (arg)
  "Insert \"o\" and call `comint-send-input'.
`sql-electric-stuff' must be the symbol `go'."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (and (equal sql-electric-stuff 'go)
	   (save-excursion
	     (comint-bol nil)
	     (looking-at "go\\b")))
      (comint-send-input)))

(defun sql-magic-semicolon (arg)
  "Insert semicolon and call `comint-send-input'.
`sql-electric-stuff' must be the symbol `semicolon'."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (equal sql-electric-stuff 'semicolon)
       (comint-send-input)))

(defun sql-accumulate-and-indent ()
  "Continue SQL statement on the next line."
  (interactive)
  (if (fboundp 'comint-accumulate)
      (comint-accumulate)
    (newline))
  (indent-according-to-mode))

(defun sql-help-list-products (indent freep)
  "Generate listing of products available for use under SQLi.

List products with :free-software attribute set to FREEP.  Indent
each line with INDENT."

  (let (sqli-func doc)
    (setq doc "")
    (dolist (p sql-product-alist)
      (setq sqli-func (intern (concat "sql-" (symbol-name (car p)))))

      (if (and (fboundp sqli-func)
	       (eq (sql-get-product-feature (car p) :free-software) freep))
	(setq doc
	      (concat doc
		      indent
		      (or (sql-get-product-feature (car p) :name)
			  (symbol-name (car p)))
		      ":\t"
		      "\\["
		      (symbol-name sqli-func)
		      "]\n"))))
    doc))

;;;###autoload
(defun sql-help ()
  "Show short help for the SQL modes.

Use an entry function to open an interactive SQL buffer.  This buffer is
usually named `*SQL*'.  The name of the major mode is SQLi.

Use the following commands to start a specific SQL interpreter:

    \\\\FREE

Other non-free SQL implementations are also supported:

    \\\\NONFREE

But we urge you to choose a free implementation instead of these.

You can also use \\[sql-product-interactive] to invoke the
interpreter for the current `sql-product'.

Once you have the SQLi buffer, you can enter SQL statements in the
buffer.  The output generated is appended to the buffer and a new prompt
is generated.  See the In/Out menu in the SQLi buffer for some functions
that help you navigate through the buffer, the input history, etc.

If you have a really complex SQL statement or if you are writing a
procedure, you can do this in a separate buffer.  Put the new buffer in
`sql-mode' by calling \\[sql-mode].  The name of this buffer can be
anything.  The name of the major mode is SQL.

In this SQL buffer (SQL mode), you can send the region or the entire
buffer to the interactive SQL buffer (SQLi mode).  The results are
appended to the SQLi buffer without disturbing your SQL buffer."
  (interactive)

  ;; Insert references to loaded products into the help buffer string
  (let ((doc (documentation 'sql-help t))
	changedp)
    (setq changedp nil)

    ;; Insert FREE software list
    (when (string-match "^\\(\\s-*\\)[\\\\][\\\\]FREE\\s-*\n" doc 0)
      (setq doc (replace-match (sql-help-list-products (match-string 1 doc) t)
			       t t doc 0)
	    changedp t))

    ;; Insert non-FREE software list
    (when (string-match "^\\(\\s-*\\)[\\\\][\\\\]NONFREE\\s-*\n" doc 0)
      (setq doc (replace-match (sql-help-list-products (match-string 1 doc) nil)
			       t t doc 0)
	    changedp t))

    ;; If we changed the help text, save the change so that the help
    ;; sub-system will see it
    (when changedp
      (put 'sql-help 'function-documentation doc)))

  ;; Call help on this function
  (describe-function 'sql-help))

(defun sql-read-passwd (prompt &optional default)
  "Read a password using PROMPT.  Optional DEFAULT is password to start with."
  (read-passwd prompt nil default))

(defun sql-get-login-ext (symbol prompt history-var plist)
  "Prompt user with extended login parameters.

The global value of SYMBOL is the last value and the global value 
of the SYMBOL is set based on the user's input.

If PLIST is nil, then the user is simply prompted for a string
value.

The property `:default' specifies the default value.  If the
`:number' property is non-nil then ask for a number.

The `:file' property prompts for a file name that must match the
regexp pattern specified in its value.

The `:completion' property prompts for a string specified by its
value.  (The property value is used as the PREDICATE argument to
`completing-read'.)"
  (set-default 
   symbol
   (let* ((default (plist-get plist :default))
          (last-value (default-value symbol))
          (prompt-def
           (if default
               (if (string-match "\\(\\):[ \t]*\\'" prompt)
                   (replace-match (format " (default \"%s\")" default) t t prompt 1)
                 (replace-regexp-in-string "[ \t]*\\'"
                                           (format " (default \"%s\") " default)
                                           prompt t t))
             prompt))
          (use-dialog-box nil))
     (cond
      ((plist-member plist :file)
       (expand-file-name
        (read-file-name prompt
                        (file-name-directory last-value) default t
                        (file-name-nondirectory last-value)
                        (when (plist-get plist :file)
                          `(lambda (f)
                             (string-match
                              (concat "\\<" ,(plist-get plist :file) "\\>")
                              (file-name-nondirectory f)))))))

      ((plist-member plist :completion)
       (completing-read prompt-def (plist-get plist :completion) nil t
                        last-value history-var default))

      ((plist-get plist :number)
       (read-number prompt (or default last-value 0)))

      (t
       (let ((r (read-from-minibuffer prompt-def last-value nil nil history-var nil)))
         (if (string= "" r) (or default "") r)))))))

(defun sql-get-login (&rest what)
  "Get username, password and database from the user.

The variables `sql-user', `sql-password', `sql-server', and
`sql-database' can be customized.  They are used as the default values.
Usernames, servers and databases are stored in `sql-user-history',
`sql-server-history' and `database-history'.  Passwords are not stored
in a history.

Parameter WHAT is a list of tokens passed as arguments in the
function call.  The function asks for the username if WHAT
contains the symbol `user', for the password if it contains the
symbol `password', for the server if it contains the symbol
`server', and for the database if it contains the symbol
`database'.  The members of WHAT are processed in the order in
which they are provided.

Each token may also be a list with the token in the car and a
plist of options as the cdr.  The following properties are
supported:

    :file <filename-regexp>
    :completion <list-of-strings-or-function>
    :default <default-value>
    :number t

In order to ask the user for username, password and database, call the
function like this: (sql-get-login 'user 'password 'database)."
  (interactive)
  (mapcar
   (lambda (w)
     (let ((token (or (and (consp w) (car w)) w))
           (plist (or (and (consp w) (cdr w)) nil)))

     (cond
      ((eq token 'user)		; user
       (sql-get-login-ext 'sql-user "User: " 'sql-user-history plist))

      ((eq token 'password)	; password
       (setq-default sql-password
                     (sql-read-passwd "Password: " sql-password)))

      ((eq token 'server)	; server
       (sql-get-login-ext 'sql-server "Server: " 'sql-server-history plist))

      ((eq token 'database)	; database
       (sql-get-login-ext 'sql-database "Database: " 'sql-database-history plist))

      ((eq token 'port)		; port
       (sql-get-login-ext 'sql-port "Port: " nil (append '(:number t) plist))))))
   what))

(defun sql-find-sqli-buffer (&optional product connection)
  "Returns the name of the current default SQLi buffer or nil.
In order to qualify, the SQLi buffer must be alive, be in
`sql-interactive-mode' and have a process."
  (let ((buf  sql-buffer)
        (prod (or product sql-product)))
    (or
     ;; Current sql-buffer, if there is one.
     (and (sql-buffer-live-p buf prod connection)
          buf)
     ;; Global sql-buffer
     (and (setq buf (default-value 'sql-buffer))
          (sql-buffer-live-p buf prod connection)
          buf)
     ;; Look thru each buffer
     (car (apply 'append
                 (mapcar (lambda (b)
                           (and (sql-buffer-live-p b prod connection)
                                (list (buffer-name b))))
                         (buffer-list)))))))

(defun sql-set-sqli-buffer-generally ()
  "Set SQLi buffer for all SQL buffers that have none.
This function checks all SQL buffers for their SQLi buffer.  If their
SQLi buffer is nonexistent or has no process, it is set to the current
default SQLi buffer.  The current default SQLi buffer is determined
using `sql-find-sqli-buffer'.  If `sql-buffer' is set,
`sql-set-sqli-hook' is run."
  (interactive)
  (save-excursion
    (let ((buflist (buffer-list))
	  (default-buffer (sql-find-sqli-buffer)))
      (setq-default sql-buffer default-buffer)
      (while (not (null buflist))
	(let ((candidate (car buflist)))
	  (set-buffer candidate)
	  (if (and (derived-mode-p 'sql-mode)
		   (not (sql-buffer-live-p sql-buffer)))
	      (progn
		(setq sql-buffer default-buffer)
		(when default-buffer
                  (run-hooks 'sql-set-sqli-hook)))))
	(setq buflist (cdr buflist))))))

(defun sql-set-sqli-buffer ()
  "Set the SQLi buffer SQL strings are sent to.

Call this function in a SQL buffer in order to set the SQLi buffer SQL
strings are sent to.  Calling this function sets `sql-buffer' and runs
`sql-set-sqli-hook'.

If you call it from a SQL buffer, this sets the local copy of
`sql-buffer'.

If you call it from anywhere else, it sets the global copy of
`sql-buffer'."
  (interactive)
  (let ((default-buffer (sql-find-sqli-buffer)))
    (if (null default-buffer)
        (error "There is no suitable SQLi buffer")
      (let ((new-buffer (read-buffer "New SQLi buffer: " default-buffer t)))
        (if (null (sql-buffer-live-p new-buffer))
            (error "Buffer %s is not a working SQLi buffer" new-buffer)
          (when new-buffer
            (setq sql-buffer new-buffer)
            (run-hooks 'sql-set-sqli-hook)))))))

(defun sql-show-sqli-buffer ()
  "Show the name of current SQLi buffer.

This is the buffer SQL strings are sent to.  It is stored in the
variable `sql-buffer'.  See `sql-help' on how to create such a buffer."
  (interactive)
  (if (or (null sql-buffer)
          (null (buffer-live-p (get-buffer sql-buffer))))
      (message "%s has no SQLi buffer set." (buffer-name (current-buffer)))
    (if (null (get-buffer-process sql-buffer))
	(message "Buffer %s has no process." sql-buffer)
      (message "Current SQLi buffer is %s." sql-buffer))))

(defun sql-make-alternate-buffer-name ()
  "Return a string that can be used to rename a SQLi buffer.

This is used to set `sql-alternate-buffer-name' within
`sql-interactive-mode'.

If the session was started with `sql-connect' then the alternate
name would be the name of the connection.

Otherwise, it uses the parameters identified by the :sqlilogin
parameter.

If all else fails, the alternate name would be the user and
server/database name."

  (let ((name ""))

    ;; Build a name using the :sqli-login setting
    (setq name
          (apply 'concat
                 (cdr
                  (apply 'append nil
                         (sql-for-each-login
                          (sql-get-product-feature sql-product :sqli-login)
                          (lambda (token plist)
                            (cond
                             ((eq token 'user)
                              (unless (string= "" sql-user)
                                (list "/" sql-user)))
                             ((eq token 'port)
                              (unless (or (not (numberp sql-port))
                                          (= 0 sql-port))
                                (list ":" (number-to-string sql-port))))
                             ((eq token 'server)
                              (unless (string= "" sql-server)
                                (list "."
                                      (if (plist-member plist :file)
                                          (file-name-nondirectory sql-server)
                                        sql-server))))
                             ((eq token 'database)
                              (unless (string= "" sql-database)
                                (list "@"
                                      (if (plist-member plist :file)
                                         (file-name-nondirectory sql-database)
                                        sql-database))))

                             ((eq token 'password) nil)
                             (t                    nil))))))))

    ;; If there's a connection, use it and the name thus far
    (if sql-connection
        (format "<%s>%s" sql-connection (or name ""))

      ;; If there is no name, try to create something meaningful
      (if (string= "" (or name ""))
          (concat
           (if (string= "" sql-user)
               (if (string= "" (user-login-name))
                   ()
                 (concat (user-login-name) "/"))
             (concat sql-user "/"))
           (if (string= "" sql-database)
               (if (string= "" sql-server)
               (system-name)
               sql-server)
             sql-database))

        ;; Use the name we've got
        name))))

(defun sql-rename-buffer (&optional new-name)
  "Rename a SQL interactive buffer.

Prompts for the new name if command is preceded by
\\[universal-argument].  If no buffer name is provided, then the
`sql-alternate-buffer-name' is used.

The actual buffer name set will be \"*SQL: NEW-NAME*\".  If
NEW-NAME is empty, then the buffer name will be \"*SQL*\"."
  (interactive "P")

  (if (not (derived-mode-p 'sql-interactive-mode))
      (message "Current buffer is not a SQL interactive buffer")

    (setq sql-alternate-buffer-name
          (cond
           ((stringp new-name) new-name)
           ((consp new-name)
            (read-string "Buffer name (\"*SQL: XXX*\"; enter `XXX'): "
                         sql-alternate-buffer-name))
           (t                  sql-alternate-buffer-name)))

    (rename-buffer (if (string= "" sql-alternate-buffer-name)
                       "*SQL*"
                     (format "*SQL: %s*" sql-alternate-buffer-name))
                   t)))

(defun sql-copy-column ()
  "Copy current column to the end of buffer.
Inserts SELECT or commas if appropriate."
  (interactive)
  (let ((column))
    (save-excursion
      (setq column (buffer-substring-no-properties
		  (progn (forward-char 1) (backward-sexp 1) (point))
		  (progn (forward-sexp 1) (point))))
      (goto-char (point-max))
      (let ((bol (comint-line-beginning-position)))
	(cond
	 ;; if empty command line, insert SELECT
	 ((= bol (point))
	  (insert "SELECT "))
	 ;; else if appending to INTO .* (, SELECT or ORDER BY, insert a comma
	 ((save-excursion
	    (re-search-backward "\\b\\(\\(into\\s-+\\S-+\\s-+(\\)\\|select\\|order by\\) .+"
				bol t))
	  (insert ", "))
	 ;; else insert a space
	 (t
	  (if (eq (preceding-char) ?\s)
	      nil
	    (insert " ")))))
      ;; in any case, insert the column
      (insert column)
      (message "%s" column))))

;; On Windows, SQL*Plus for Oracle turns on full buffering for stdout
;; if it is not attached to a character device; therefore placeholder
;; replacement by SQL*Plus is fully buffered.  The workaround lets
;; Emacs query for the placeholders.

(defvar sql-placeholder-history nil
  "History of placeholder values used.")

(defun sql-placeholders-filter (string)
  "Replace placeholders in STRING.
Placeholders are words starting with an ampersand like &this."

  (when sql-oracle-scan-on
    (while (string-match "&\\(\\sw+\\)" string)
      (setq string (replace-match
		    (read-from-minibuffer
		     (format "Enter value for %s: " (match-string 1 string))
		     nil nil nil 'sql-placeholder-history)
		    t t string))))
  string)

;; Using DB2 interactively, newlines must be escaped with " \".
;; The space before the backslash is relevant.
(defun sql-escape-newlines-filter (string)
  "Escape newlines in STRING.
Every newline in STRING will be preceded with a space and a backslash."
  (let ((result "") (start 0) mb me)
    (while (string-match "\n" string start)
      (setq mb (match-beginning 0)
	    me (match-end 0)
	    result (concat result
			   (substring string start mb)
			   (if (and (> mb 1)
				    (string-equal " \\" (substring string (- mb 2) mb)))
			       "" " \\\n"))
	    start me))
    (concat result (substring string start))))



;;; Input sender for SQLi buffers

(defvar sql-output-newline-count 0
  "Number of newlines in the input string.

Allows the suppression of continuation prompts.")

(defvar sql-output-by-send nil
  "Non-nil if the command in the input was generated by `sql-send-string'.")

(defun sql-input-sender (proc string)
  "Send STRING to PROC after applying filters."

  (let* ((product (with-current-buffer (process-buffer proc) sql-product))
	 (filter  (sql-get-product-feature product :input-filter)))

    ;; Apply filter(s)
    (cond
     ((not filter)
      nil)
     ((functionp filter)
      (setq string (funcall filter string)))
     ((listp filter)
      (mapc (lambda (f) (setq string (funcall f string))) filter))
     (t nil))

    ;; Count how many newlines in the string
    (setq sql-output-newline-count 0)
    (mapc (lambda (ch)
            (when (eq ch ?\n)
              (setq sql-output-newline-count (1+ sql-output-newline-count))))
          string)

    ;; Send the string
    (comint-simple-send proc string)))

;;; Strip out continuation prompts

(defvar sql-preoutput-hold nil)

(defun sql-interactive-remove-continuation-prompt (oline)
  "Strip out continuation prompts out of the OLINE.

Added to the `comint-preoutput-filter-functions' hook in a SQL
interactive buffer.  If `sql-output-newline-count' is greater than
zero, then an output line matching the continuation prompt is filtered
out.  If the count is zero, then a newline is inserted into the output
to force the output from the query to appear on a new line.

The complication to this filter is that the continuation prompts
may arrive in multiple chunks.  If they do, then the function
saves any unfiltered output in a buffer and prepends that buffer
to the next chunk to properly match the broken-up prompt.

If the filter gets confused, it should reset and stop filtering
to avoid deleting non-prompt output."

  (let (did-filter)
    (setq oline (concat (or sql-preoutput-hold "") oline)
          sql-preoutput-hold nil)

    (if (and comint-prompt-regexp
             (integerp sql-output-newline-count)
             (>= sql-output-newline-count 1))
        (progn
          (while (and (not (string= oline ""))
                      (> sql-output-newline-count 0)
                      (string-match comint-prompt-regexp oline)
                      (= (match-beginning 0) 0))

            (setq oline (replace-match "" nil nil oline)
                  sql-output-newline-count (1- sql-output-newline-count)
                  did-filter t))

          (if (= sql-output-newline-count 0)
              (setq sql-output-newline-count nil
                    oline (concat "\n" oline)
                    sql-output-by-send nil)

            (setq sql-preoutput-hold oline
                  oline ""))

          (unless did-filter
            (setq oline (or sql-preoutput-hold "")
                  sql-preoutput-hold nil
                  sql-output-newline-count nil)))

      (setq sql-output-newline-count nil))

    oline))

;;; Sending the region to the SQLi buffer.

(defun sql-send-string (str)
  "Send the string STR to the SQL process."
  (interactive "sSQL Text: ")

  (let ((comint-input-sender-no-newline nil)
        (s (replace-regexp-in-string "[[:space:]\n\r]+\\'" "" str)))
    (if (sql-buffer-live-p sql-buffer)
	(progn
	  ;; Ignore the hoping around...
	  (save-excursion
	    ;; Set product context
	    (with-current-buffer sql-buffer
	      ;; Send the string (trim the trailing whitespace)
	      (sql-input-sender (get-buffer-process sql-buffer) s)

	      ;; Send a command terminator if we must
	      (if sql-send-terminator
		  (sql-send-magic-terminator sql-buffer s sql-send-terminator))

	      (message "Sent string to buffer %s." sql-buffer)))

	  ;; Display the sql buffer
	  (if sql-pop-to-buffer-after-send-region
	      (pop-to-buffer sql-buffer)
	    (display-buffer sql-buffer)))

    ;; We don't have no stinkin' sql
    (message "No SQL process started."))))

(defun sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (sql-send-string (buffer-substring-no-properties start end)))

(defun sql-send-paragraph ()
  "Send the current paragraph to the SQL process."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (sql-send-region start end)))

(defun sql-send-buffer ()
  "Send the buffer contents to the SQL process."
  (interactive)
  (sql-send-region (point-min) (point-max)))

(defun sql-send-magic-terminator (buf str terminator)
  "Send TERMINATOR to buffer BUF if its not present in STR."
  (let (comint-input-sender-no-newline pat term)
    ;; If flag is merely on(t), get product-specific terminator
    (if (eq terminator t)
	(setq terminator (sql-get-product-feature sql-product :terminator)))

    ;; If there is no terminator specified, use default ";"
    (unless terminator
      (setq terminator ";"))

    ;; Parse the setting into the pattern and the terminator string
    (cond ((stringp terminator)
	   (setq pat (regexp-quote terminator)
		 term terminator))
	  ((consp terminator)
	   (setq pat (car terminator)
		 term (cdr terminator)))
	  (t
	   nil))

    ;; Check to see if the pattern is present in the str already sent
    (unless (and pat term
		 (string-match (concat pat "\\'") str))
      (comint-simple-send (get-buffer-process buf) term)
      (setq sql-output-newline-count
            (if sql-output-newline-count
                (1+ sql-output-newline-count)
              1)))
    (setq sql-output-by-send t)))

(defun sql-remove-tabs-filter (str)
  "Replace tab characters with spaces."
  (replace-regexp-in-string "\t" " " str nil t))

(defun sql-toggle-pop-to-buffer-after-send-region (&optional value)
  "Toggle `sql-pop-to-buffer-after-send-region'.

If given the optional parameter VALUE, sets
`sql-toggle-pop-to-buffer-after-send-region' to VALUE."
  (interactive "P")
  (if value
      (setq sql-pop-to-buffer-after-send-region value)
    (setq sql-pop-to-buffer-after-send-region
	  (null sql-pop-to-buffer-after-send-region))))



;;; Redirect output functions

(defvar sql-debug-redirect nil
  "If non-nil, display messages related to the use of redirection.")

(defun sql-str-literal (s)
  (concat "'" (replace-regexp-in-string "[']" "''" s) "'"))

(defun sql-redirect (sqlbuf command &optional outbuf save-prior)
  "Execute the SQL command and send output to OUTBUF.

SQLBUF must be an active SQL interactive buffer.  OUTBUF may be
an existing buffer, or the name of a non-existing buffer.  If
omitted the output is sent to a temporary buffer which will be
killed after the command completes.  COMMAND should be a string
of commands accepted by the SQLi program.  COMMAND may also be a
list of SQLi command strings."

  (let* ((visible (and outbuf
                       (not (string= " " (substring outbuf 0 1))))))
    (when visible
      (message "Executing SQL command..."))
    (if (consp command)
        (mapc (lambda (c) (sql-redirect-one sqlbuf c outbuf save-prior))
              command)
      (sql-redirect-one sqlbuf command outbuf save-prior))
    (when visible
      (message "Executing SQL command...done"))))

(defun sql-redirect-one (sqlbuf command outbuf save-prior)
  (with-current-buffer sqlbuf
    (let ((buf  (get-buffer-create (or outbuf " *SQL-Redirect*")))
          (proc (get-buffer-process (current-buffer)))
          (comint-prompt-regexp (sql-get-product-feature sql-product
                                                         :prompt-regexp))
          (start nil))
      (with-current-buffer buf
        (setq view-read-only nil)
        (unless save-prior
          (erase-buffer))
        (goto-char (point-max))
        (unless (zerop (buffer-size))
          (insert "\n"))
        (setq start (point)))

      (when sql-debug-redirect
        (message ">>SQL> %S" command))

      ;; Run the command
      (comint-redirect-send-command-to-process command buf proc nil t)
      (while (null comint-redirect-completed)
	(accept-process-output nil 1))

      ;; Clean up the output results
      (with-current-buffer buf
        ;; Remove trailing whitespace
        (goto-char (point-max))
        (when (looking-back "[ \t\f\n\r]*" start)
          (delete-region (match-beginning 0) (match-end 0)))
        ;; Remove echo if there was one
        (goto-char start)
        (when (looking-at (concat "^" (regexp-quote command) "[\\n]"))
          (delete-region (match-beginning 0) (match-end 0)))
        ;; Remove Ctrl-Ms
        (goto-char start)
        (while (re-search-forward "\r+$" nil t)
          (replace-match "" t t))
        (goto-char start)))))

(defun sql-redirect-value (sqlbuf command regexp &optional regexp-groups)
  "Execute the SQL command and return part of result.

SQLBUF must be an active SQL interactive buffer.  COMMAND should
be a string of commands accepted by the SQLi program.  From the
output, the REGEXP is repeatedly matched and the list of
REGEXP-GROUPS submatches is returned.  This behaves much like
\\[comint-redirect-results-list-from-process] but instead of
returning a single submatch it returns a list of each submatch
for each match."

  (let ((outbuf " *SQL-Redirect-values*")
        (results nil))
    (sql-redirect sqlbuf command outbuf nil)
    (with-current-buffer outbuf
      (while (re-search-forward regexp nil t)
	(push
         (cond
          ;; no groups-return all of them
          ((null regexp-groups)
           (let ((i (/ (length (match-data)) 2))
                 (r nil))
             (while (> i 0)
               (setq i (1- i))
               (push (match-string i) r))
             r))
          ;; one group specified
          ((numberp regexp-groups)
           (match-string regexp-groups))
          ;; list of numbers; return the specified matches only
          ((consp regexp-groups)
           (mapcar (lambda (c)
                     (cond
                      ((numberp c) (match-string c))
                      ((stringp c) (match-substitute-replacement c))
                      (t (error "sql-redirect-value: unknown REGEXP-GROUPS value - %s" c))))
                   regexp-groups))
          ;; String is specified; return replacement string
          ((stringp regexp-groups)
           (match-substitute-replacement regexp-groups))
          (t
           (error "sql-redirect-value: unknown REGEXP-GROUPS value - %s"
                  regexp-groups)))
         results)))

    (when sql-debug-redirect
      (message ">>SQL> = %S" (reverse results)))

    (nreverse results)))

(defun sql-execute (sqlbuf outbuf command enhanced arg)
  "Executes a command in a SQL interactive buffer and captures the output.

The commands are run in SQLBUF and the output saved in OUTBUF.
COMMAND must be a string, a function or a list of such elements.
Functions are called with SQLBUF, OUTBUF and ARG as parameters;
strings are formatted with ARG and executed.

If the results are empty the OUTBUF is deleted, otherwise the
buffer is popped into a view window. "
  (mapc
   (lambda (c)
     (cond
      ((stringp c)
       (sql-redirect sqlbuf (if arg (format c arg) c) outbuf) t)
      ((functionp c)
       (apply c sqlbuf outbuf enhanced arg nil))
      (t (error "Unknown sql-execute item %s" c))))
   (if (consp command) command (cons command nil)))

  (setq outbuf (get-buffer outbuf))
  (if (zerop (buffer-size outbuf))
      (kill-buffer outbuf)
    (let ((one-win (eq (selected-window)
                       (get-lru-window))))
      (with-current-buffer outbuf
        (set-buffer-modified-p nil)
        (setq view-read-only t))
      (view-buffer-other-window outbuf)
      (when one-win
        (shrink-window-if-larger-than-buffer)))))

(defun sql-execute-feature (sqlbuf outbuf feature enhanced arg)
  "List objects or details in a separate display buffer."
  (let (command)
    (with-current-buffer sqlbuf
      (setq command (sql-get-product-feature sql-product feature)))
    (unless command
      (error "%s does not support %s" sql-product feature))
    (when (consp command)
      (setq command (if enhanced
                        (cdr command)
                      (car command))))
    (sql-execute sqlbuf outbuf command enhanced arg)))

(defvar sql-completion-object nil
  "A list of database objects used for completion.

The list is maintained in SQL interactive buffers.")

(defvar sql-completion-column nil
  "A list of column names used for completion.

The list is maintained in SQL interactive buffers.")

(defun sql-build-completions-1 (schema completion-list feature)
  "Generate a list of objects in the database for use as completions."
  (let ((f (sql-get-product-feature sql-product feature)))
    (when f
      (set completion-list
            (let (cl)
              (dolist (e (append (symbol-value completion-list)
                                 (apply f (current-buffer) (cons schema nil)))
                         cl)
                (unless (member e cl) (setq cl (cons e cl))))
              (sort cl (function string<)))))))

(defun sql-build-completions (schema)
  "Generate a list of names in the database for use as completions."
  (sql-build-completions-1 schema 'sql-completion-object :completion-object)
  (sql-build-completions-1 schema 'sql-completion-column :completion-column))

(defvar sql-completion-sqlbuf nil)

(defun sql-try-completion (string collection &optional predicate)
  (when sql-completion-sqlbuf
      (with-current-buffer sql-completion-sqlbuf
        (let ((schema (and (string-match "\\`\\(\\sw\\(:?\\sw\\|\\s_\\)*\\)[.]" string)
                           (downcase (match-string 1 string)))))

          ;; If we haven't loaded any object name yet, load local schema
          (unless sql-completion-object
            (sql-build-completions nil))

          ;; If they want another schema, load it if we haven't yet
          (when schema
            (let ((schema-dot (concat schema "."))
                  (schema-len (1+ (length schema)))
                  (names sql-completion-object)
                  has-schema)

              (while (and (not has-schema) names)
                (setq has-schema (and
                                  (>= (length (car names)) schema-len)
                                  (string= schema-dot
                                           (downcase (substring (car names)
                                                                0 schema-len))))
                      names (cdr names)))
              (unless has-schema
                (sql-build-completions schema)))))

        ;; Try to find the completion
        (cond
         ((not predicate)
          (try-completion string sql-completion-object))
         ((eq predicate t)
          (all-completions string sql-completion-object))
         ((eq predicate 'lambda)
          (test-completion string sql-completion-object))
         ((eq (car predicate) 'boundaries)
          (completion-boundaries string sql-completion-object nil (cdr predicate)))))))

(defun sql-read-table-name (prompt)
  "Read the name of a database table."
  (let* ((tname
          (and (buffer-local-value 'sql-contains-names (current-buffer))
               (thing-at-point-looking-at
                (concat "\\_<\\sw\\(:?\\sw\\|\\s_\\)*"
                        "\\(?:[.]+\\sw\\(?:\\sw\\|\\s_\\)*\\)*\\_>"))
               (buffer-substring-no-properties (match-beginning 0)
                                               (match-end 0))))
         (sql-completion-sqlbuf (sql-find-sqli-buffer))
         (product (with-current-buffer sql-completion-sqlbuf sql-product))
         (completion-ignore-case t))

    (if (sql-get-product-feature product :completion-object)
        (completing-read prompt (function sql-try-completion)
                         nil nil tname)
      (read-from-minibuffer prompt tname))))

(defun sql-list-all (&optional enhanced)
  "List all database objects.
With optional prefix argument ENHANCED, displays additional
details or extends the listing to include other schemas objects."
  (interactive "P")
  (let ((sqlbuf (sql-find-sqli-buffer)))
    (unless sqlbuf
      (error "No SQL interactive buffer found"))
    (sql-execute-feature sqlbuf "*List All*" :list-all enhanced nil)
    (with-current-buffer sqlbuf
      ;; Contains the name of database objects
      (set (make-local-variable 'sql-contains-names) t)
      (set (make-local-variable 'sql-buffer) sqlbuf))))

(defun sql-list-table (name &optional enhanced)
  "List the details of a database table named NAME.
Displays the columns in the relation.  With optional prefix argument
ENHANCED, displays additional details about each column."
  (interactive
   (list (sql-read-table-name "Table name: ")
         current-prefix-arg))
  (let ((sqlbuf (sql-find-sqli-buffer)))
    (unless sqlbuf
      (error "No SQL interactive buffer found"))
    (unless name
      (error "No table name specified"))
    (sql-execute-feature sqlbuf (format "*List %s*" name)
                         :list-table enhanced name)))


;;; SQL mode -- uses SQL interactive mode

;;;###autoload
(define-derived-mode sql-mode prog-mode "SQL"
  "Major mode to edit SQL.

You can send SQL statements to the SQLi buffer using
\\[sql-send-region].  Such a buffer must exist before you can do this.
See `sql-help' on how to create SQLi buffers.

\\{sql-mode-map}
Customization: Entry to this mode runs the `sql-mode-hook'.

When you put a buffer in SQL mode, the buffer stores the last SQLi
buffer created as its destination in the variable `sql-buffer'.  This
will be the buffer \\[sql-send-region] sends the region to.  If this
SQLi buffer is killed, \\[sql-send-region] is no longer able to
determine where the strings should be sent to.  You can set the
value of `sql-buffer' using \\[sql-set-sqli-buffer].

For information on how to create multiple SQLi buffers, see
`sql-interactive-mode'.

Note that SQL doesn't have an escape character unless you specify
one.  If you specify backslash as escape character in SQL,
you must tell Emacs.  Here's how to do that in your `~/.emacs' file:

\(add-hook 'sql-mode-hook
          (lambda ()
	    (modify-syntax-entry ?\\\\ \".\" sql-mode-syntax-table)))"
  :abbrev-table sql-mode-abbrev-table
  (if sql-mode-menu
      (easy-menu-add sql-mode-menu)); XEmacs

  (set (make-local-variable 'comment-start) "--")
  ;; Make each buffer in sql-mode remember the "current" SQLi buffer.
  (make-local-variable 'sql-buffer)
  ;; Add imenu support for sql-mode.  Note that imenu-generic-expression
  ;; is buffer-local, so we don't need a local-variable for it.  SQL is
  ;; case-insensitive, that's why we have to set imenu-case-fold-search.
  (setq imenu-generic-expression sql-imenu-generic-expression
	imenu-case-fold-search t)
  ;; Make `sql-send-paragraph' work on paragraphs that contain indented
  ;; lines.
  (set (make-local-variable 'paragraph-separate) "[\f]*$")
  (set (make-local-variable 'paragraph-start) "[\n\f]")
  ;; Abbrevs
  (setq abbrev-all-caps 1)
  ;; Contains the name of database objects
  (set (make-local-variable 'sql-contains-names) t)
  ;; Catch changes to sql-product and highlight accordingly
  (add-hook 'hack-local-variables-hook 'sql-highlight-product t t))



;;; SQL interactive mode

(put 'sql-interactive-mode 'mode-class 'special)

(defun sql-interactive-mode ()
  "Major mode to use a SQL interpreter interactively.

Do not call this function by yourself.  The environment must be
initialized by an entry function specific for the SQL interpreter.
See `sql-help' for a list of available entry functions.

\\[comint-send-input] after the end of the process' output sends the
text from the end of process to the end of the current line.
\\[comint-send-input] before end of process output copies the current
line minus the prompt to the end of the buffer and sends it.
\\[comint-copy-old-input] just copies the current line.
Use \\[sql-accumulate-and-indent] to enter multi-line statements.

If you want to make multiple SQL buffers, rename the `*SQL*' buffer
using \\[rename-buffer] or \\[rename-uniquely] and start a new process.
See `sql-help' for a list of available entry functions.  The last buffer
created by such an entry function is the current SQLi buffer.  SQL
buffers will send strings to the SQLi buffer current at the time of
their creation.  See `sql-mode' for details.

Sample session using two connections:

1. Create first SQLi buffer by calling an entry function.
2. Rename buffer \"*SQL*\" to \"*Connection 1*\".
3. Create a SQL buffer \"test1.sql\".
4. Create second SQLi buffer by calling an entry function.
5. Rename buffer \"*SQL*\" to \"*Connection 2*\".
6. Create a SQL buffer \"test2.sql\".

Now \\[sql-send-region] in buffer \"test1.sql\" will send the region to
buffer \"*Connection 1*\", \\[sql-send-region] in buffer \"test2.sql\"
will send the region to buffer \"*Connection 2*\".

If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.  On some operating systems, this will not work because
the signals are not supported.

\\{sql-interactive-mode-map}
Customization: Entry to this mode runs the hooks on `comint-mode-hook'
and `sql-interactive-mode-hook' (in that order).  Before each input, the
hooks on `comint-input-filter-functions' are run.  After each SQL
interpreter output, the hooks on `comint-output-filter-functions' are
run.

Variable `sql-input-ring-file-name' controls the initialization of the
input ring history.

Variables `comint-output-filter-functions', a hook, and
`comint-scroll-to-bottom-on-input' and
`comint-scroll-to-bottom-on-output' control whether input and output
cause the window to scroll to the end of the buffer.

If you want to make SQL buffers limited in length, add the function
`comint-truncate-buffer' to `comint-output-filter-functions'.

Here is an example for your .emacs file.  It keeps the SQLi buffer a
certain length.

\(add-hook 'sql-interactive-mode-hook
    \(function (lambda ()
        \(setq comint-output-filter-functions 'comint-truncate-buffer))))

Here is another example.  It will always put point back to the statement
you entered, right above the output it created.

\(setq comint-output-filter-functions
       \(function (lambda (STR) (comint-show-output))))"
  (delay-mode-hooks (comint-mode))

  ;; Get the `sql-product' for this interactive session.
  (set (make-local-variable 'sql-product)
       (or sql-interactive-product
	   sql-product))

  ;; Setup the mode.
  (setq major-mode 'sql-interactive-mode)
  (setq mode-name
        (concat "SQLi[" (or (sql-get-product-feature sql-product :name)
                            (symbol-name sql-product)) "]"))
  (use-local-map sql-interactive-mode-map)
  (if sql-interactive-mode-menu
      (easy-menu-add sql-interactive-mode-menu)) ; XEmacs
  (set-syntax-table sql-mode-syntax-table)

  ;; Note that making KEYWORDS-ONLY nil will cause havoc if you try
  ;; SELECT 'x' FROM DUAL with SQL*Plus, because the title of the column
  ;; will have just one quote.  Therefore syntactic highlighting is
  ;; disabled for interactive buffers.  No imenu support.
  (sql-product-font-lock t nil)

  ;; Enable commenting and uncommenting of the region.
  (set (make-local-variable 'comment-start) "--")
  ;; Abbreviation table init and case-insensitive.  It is not activated
  ;; by default.
  (setq local-abbrev-table sql-mode-abbrev-table)
  (setq abbrev-all-caps 1)
  ;; Exiting the process will call sql-stop.
  (set-process-sentinel (get-buffer-process (current-buffer)) 'sql-stop)
  ;; Save the connection and login params
  (set (make-local-variable 'sql-user)       sql-user)
  (set (make-local-variable 'sql-database)   sql-database)
  (set (make-local-variable 'sql-server)     sql-server)
  (set (make-local-variable 'sql-port)       sql-port)
  (set (make-local-variable 'sql-connection) sql-connection)
  (setq-default sql-connection nil)
  ;; Contains the name of database objects
  (set (make-local-variable 'sql-contains-names) t)
  ;; Keep track of existing object names
  (set (make-local-variable 'sql-completion-object) nil)
  (set (make-local-variable 'sql-completion-column) nil)
  ;; Create a useful name for renaming this buffer later.
  (set (make-local-variable 'sql-alternate-buffer-name)
       (sql-make-alternate-buffer-name))
  ;; User stuff.  Initialize before the hook.
  (set (make-local-variable 'sql-prompt-regexp)
       (sql-get-product-feature sql-product :prompt-regexp))
  (set (make-local-variable 'sql-prompt-length)
       (sql-get-product-feature sql-product :prompt-length))
  (set (make-local-variable 'sql-prompt-cont-regexp)
       (sql-get-product-feature sql-product :prompt-cont-regexp))
  (make-local-variable 'sql-output-newline-count)
  (make-local-variable 'sql-preoutput-hold)
  (make-local-variable 'sql-output-by-send)
  (add-hook 'comint-preoutput-filter-functions
            'sql-interactive-remove-continuation-prompt nil t)
  (make-local-variable 'sql-input-ring-separator)
  (make-local-variable 'sql-input-ring-file-name)
  ;; Run the mode hook (along with comint's hooks).
  (run-mode-hooks 'sql-interactive-mode-hook)
  ;; Set comint based on user overrides.
  (setq comint-prompt-regexp
        (if sql-prompt-cont-regexp
            (concat "\\(" sql-prompt-regexp
                    "\\|" sql-prompt-cont-regexp "\\)")
          sql-prompt-regexp))
  (setq left-margin sql-prompt-length)
  ;; Install input sender
  (set (make-local-variable 'comint-input-sender) 'sql-input-sender)
  ;; People wanting a different history file for each
  ;; buffer/process/client/whatever can change separator and file-name
  ;; on the sql-interactive-mode-hook.
  (setq comint-input-ring-separator sql-input-ring-separator
	comint-input-ring-file-name sql-input-ring-file-name)
  ;; Calling the hook before calling comint-read-input-ring allows users
  ;; to set comint-input-ring-file-name in sql-interactive-mode-hook.
  (comint-read-input-ring t))

(defun sql-stop (process event)
  "Called when the SQL process is stopped.

Writes the input history to a history file using
`comint-write-input-ring' and inserts a short message in the SQL buffer.

This function is a sentinel watching the SQL interpreter process.
Sentinels will always get the two parameters PROCESS and EVENT."
  (comint-write-input-ring)
  (if (and (eq (current-buffer) sql-buffer)
	   (not buffer-read-only))
      (insert (format "\nProcess %s %s\n" process event))
    (message "Process %s %s" process event)))



;;; Connection handling

(defun sql-read-connection (prompt &optional initial default)
  "Read a connection name."
  (let ((completion-ignore-case t))
    (completing-read prompt
                     (mapcar (lambda (c) (car c))
                             sql-connection-alist)
                     nil t initial 'sql-connection-history default)))

;;;###autoload
(defun sql-connect (connection &optional new-name)
  "Connect to an interactive session using CONNECTION settings.

See `sql-connection-alist' to see how to define connections and
their settings.

The user will not be prompted for any login parameters if a value
is specified in the connection settings."

  ;; Prompt for the connection from those defined in the alist
  (interactive
   (if sql-connection-alist
       (list (sql-read-connection "Connection: " nil '(nil))
             current-prefix-arg)
     nil))

  ;; Are there connections defined
  (if sql-connection-alist
      ;; Was one selected
      (when connection
        ;; Get connection settings
        (let ((connect-set  (assoc connection sql-connection-alist)))
          ;; Settings are defined
          (if connect-set
              ;; Set the desired parameters
              (let (param-var login-params set-params rem-params)

                ;; :sqli-login params variable
                (setq param-var
                      (sql-get-product-feature sql-product :sqli-login nil t))

                ;; :sqli-login params value
                (setq login-params
                      (sql-get-product-feature sql-product :sqli-login))

                ;; Params in the connection
                (setq set-params
                      (mapcar
                       (lambda (v)
                         (cond
                          ((eq (car v) 'sql-user)     'user)
                          ((eq (car v) 'sql-password) 'password)
                          ((eq (car v) 'sql-server)   'server)
                          ((eq (car v) 'sql-database) 'database)
                          ((eq (car v) 'sql-port)     'port)
                          (t                          (car v))))
                       (cdr connect-set)))

                ;; the remaining params (w/o the connection params)
                (setq rem-params
                      (sql-for-each-login login-params
                       (lambda (token plist)
                         (unless (member token set-params)
                           (if plist (cons token plist) token)))))

                ;; Set the parameters and start the interactive session
                (mapc
                 (lambda (vv)
                   (set-default (car vv) (eval (cadr vv))))
                 (cdr connect-set))
                (setq-default sql-connection connection)

                ;; Start the SQLi session with revised list of login parameters
                (eval `(let ((,param-var ',rem-params))
                         (sql-product-interactive sql-product new-name))))

            (message "SQL Connection <%s> does not exist" connection)
            nil)))

    (message "No SQL Connections defined")
    nil))

(defun sql-save-connection (name)
  "Captures the connection information of the current SQLi session.

The information is appended to `sql-connection-alist' and
optionally is saved to the user's init file."

  (interactive "sNew connection name: ")

  (unless (derived-mode-p 'sql-interactive-mode)
    (error "Not in a SQL interactive mode!"))

  ;; Capture the buffer local settings
  (let* ((buf        (current-buffer))
         (connection (buffer-local-value 'sql-connection buf))
         (product    (buffer-local-value 'sql-product    buf))
         (user       (buffer-local-value 'sql-user       buf))
         (database   (buffer-local-value 'sql-database   buf))
         (server     (buffer-local-value 'sql-server     buf))
         (port       (buffer-local-value 'sql-port       buf)))

    (if connection
        (message "This session was started by a connection; it's already been saved.")

      (let ((login (sql-get-product-feature product :sqli-login))
            (alist sql-connection-alist)
            connect)

        ;; Remove the existing connection if the user says so
        (when (and (assoc name alist)
                   (yes-or-no-p (format "Replace connection definition <%s>? " name)))
          (setq alist (assq-delete-all name alist)))

        ;; Add the new connection if it doesn't exist
        (if (assoc name alist)
            (message "Connection <%s> already exists" name)
          (setq connect
                (append (list name)
                        (sql-for-each-login
                         `(product ,@login)
                         (lambda (token _plist)
                           (cond
                            ((eq token 'product)  `(sql-product  ',product))
                            ((eq token 'user)     `(sql-user     ,user))
                            ((eq token 'database) `(sql-database ,database))
                            ((eq token 'server)   `(sql-server   ,server))
                            ((eq token 'port)     `(sql-port     ,port)))))))

          (setq alist (append alist (list connect)))

          ;; confirm whether we want to save the connections
          (if (yes-or-no-p "Save the connections for future sessions? ")
              (customize-save-variable 'sql-connection-alist alist)
            (customize-set-variable 'sql-connection-alist alist)))))))

(defun sql-connection-menu-filter (tail)
  "Generates menu entries for using each connection."
  (append
   (mapcar
    (lambda (conn)
      (vector
       (format "Connection <%s>\t%s" (car conn)
               (let ((sql-user "") (sql-database "")
                     (sql-server "") (sql-port 0))
                 (eval `(let ,(cdr conn) (sql-make-alternate-buffer-name)))))
       (list 'sql-connect (car conn))
       t))
    sql-connection-alist)
   tail))



;;; Entry functions for different SQL interpreters.

;;;###autoload
(defun sql-product-interactive (&optional product new-name)
  "Run PRODUCT interpreter as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer `*SQL*'.

To specify the SQL product, prefix the call with
\\[universal-argument].  To set the buffer name as well, prefix
the call to \\[sql-product-interactive] with
\\[universal-argument] \\[universal-argument].

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")

  ;; Handle universal arguments if specified
  (when (not (or executing-kbd-macro noninteractive))
    (when (and (consp product)
               (not (cdr product))
               (numberp (car product)))
      (when (>= (prefix-numeric-value product) 16)
        (when (not new-name)
          (setq new-name '(4)))
        (setq product '(4)))))

  ;; Get the value of product that we need
  (setq product
        (cond
         ((= (prefix-numeric-value product) 4) ; C-u, prompt for product
          (sql-read-product "SQL product: " sql-product))
         ((and product                  ; Product specified
               (symbolp product)) product)
         (t sql-product)))              ; Default to sql-product

  ;; If we have a product and it has a interactive mode
  (if product
      (when (sql-get-product-feature product :sqli-comint-func)
        ;; If no new name specified, try to pop to an active SQL
        ;; interactive for the same product
        (let ((buf (sql-find-sqli-buffer product sql-connection)))
          (if (and (not new-name) buf)
              (pop-to-buffer buf)

            ;; We have a new name or sql-buffer doesn't exist or match
            ;; Start by remembering where we start
            (let ((start-buffer (current-buffer))
                  new-sqli-buffer)

              ;; Get credentials.
              (apply 'sql-get-login (sql-get-product-feature product :sqli-login))

              ;; Connect to database.
              (message "Login...")
              (let ((sql-user       (default-value 'sql-user))
                    (sql-password   (default-value 'sql-password))
                    (sql-server     (default-value 'sql-server))
                    (sql-database   (default-value 'sql-database))
                    (sql-port       (default-value 'sql-port)))
                (funcall (sql-get-product-feature product :sqli-comint-func)
                         product
                         (sql-get-product-feature product :sqli-options)))

              ;; Set SQLi mode.
              (let ((sql-interactive-product product))
                (sql-interactive-mode))

              ;; Set the new buffer name
              (setq new-sqli-buffer (current-buffer))
              (when new-name
                (sql-rename-buffer new-name))
              (set (make-local-variable 'sql-buffer)
                   (buffer-name new-sqli-buffer))

              ;; Set `sql-buffer' in the start buffer
              (with-current-buffer start-buffer
                (when (derived-mode-p 'sql-mode)
                  (setq sql-buffer (buffer-name new-sqli-buffer))
                  (run-hooks 'sql-set-sqli-hook)))

              ;; All done.
              (message "Login...done")
              (run-hooks 'sql-login-hook)
              (pop-to-buffer new-sqli-buffer)))))
    (message "No default SQL product defined.  Set `sql-product'.")))

(defun sql-comint (product params)
  "Set up a comint buffer to run the SQL processor.

PRODUCT is the SQL product.  PARAMS is a list of strings which are
passed as command line arguments."
  (let ((program (sql-get-product-feature product :sqli-program))
        (buf-name "SQL"))
    ;; make sure we can find the program
    (unless (executable-find program)
      (error "Unable to locate SQL program \'%s\'" program))
    ;; Make sure buffer name is unique
    (when (sql-buffer-live-p (format "*%s*" buf-name))
      (setq buf-name (format "SQL-%s" product))
      (when (sql-buffer-live-p (format "*%s*" buf-name))
        (let ((i 1))
          (while (sql-buffer-live-p
                  (format "*%s*"
                          (setq buf-name (format "SQL-%s%d" product i))))
            (setq i (1+ i))))))
    (set-buffer
     (apply 'make-comint buf-name program nil params))))

;;;###autoload
(defun sql-oracle (&optional buffer)
  "Run sqlplus by Oracle as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-oracle-program'.  Login uses
the variables `sql-user', `sql-password', and `sql-database' as
defaults, if set.  Additional command line parameters can be stored in
the list `sql-oracle-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-oracle].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-oracle].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'oracle buffer))

(defun sql-comint-oracle (product options)
  "Create comint buffer and connect to Oracle."
  ;; Produce user/password@database construct.  Password without user
  ;; is meaningless; database without user/password is meaningless,
  ;; because "@param" will ask sqlplus to interpret the script
  ;; "param".
  (let ((parameter nil))
    (if (not (string= "" sql-user))
	(if (not (string= "" sql-password))
	    (setq parameter (concat sql-user "/" sql-password))
	  (setq parameter sql-user)))
    (if (and parameter (not (string= "" sql-database)))
	(setq parameter (concat parameter "@" sql-database)))
    (if parameter
	(setq parameter (nconc (list parameter) options))
      (setq parameter options))
    (sql-comint product parameter)))

(defun sql-oracle-save-settings (sqlbuf)
  "Saves most SQL*Plus settings so they may be reset by \\[sql-redirect]."
  ;; Note: does not capture the following settings:
  ;;
  ;; APPINFO
  ;; BTITLE
  ;; COMPATIBILITY
  ;; COPYTYPECHECK
  ;; MARKUP
  ;; RELEASE
  ;; REPFOOTER
  ;; REPHEADER
  ;; SQLPLUSCOMPATIBILITY
  ;; TTITLE
  ;; USER
  ;;

  (append
  ;; (apply 'concat (append
  ;;  '("SET")

   ;; option value...
   (sql-redirect-value
    sqlbuf
    (concat "SHOW ARRAYSIZE AUTOCOMMIT AUTOPRINT AUTORECOVERY AUTOTRACE"
            " CMDSEP COLSEP COPYCOMMIT DESCRIBE ECHO EDITFILE EMBEDDED"
            " ESCAPE FLAGGER FLUSH HEADING INSTANCE LINESIZE LNO LOBOFFSET"
            " LOGSOURCE LONG LONGCHUNKSIZE NEWPAGE NULL NUMFORMAT NUMWIDTH"
            " PAGESIZE PAUSE PNO RECSEP SERVEROUTPUT SHIFTINOUT SHOWMODE"
            " SPOOL SQLBLANKLINES SQLCASE SQLCODE SQLCONTINUE SQLNUMBER"
            " SQLPROMPT SUFFIX TAB TERMOUT TIMING TRIMOUT TRIMSPOOL VERIFY")
    "^.+$"
    "SET \\&")

   ;; option "c" (hex xx)
   (sql-redirect-value
    sqlbuf
    (concat "SHOW BLOCKTERMINATOR CONCAT DEFINE SQLPREFIX SQLTERMINATOR"
            " UNDERLINE HEADSEP RECSEPCHAR")
    "^\\(.+\\) (hex ..)$"
    "SET \\1")

   ;; FEEDBACK ON for 99 or more rows
   ;; feedback OFF
   (sql-redirect-value
    sqlbuf
    "SHOW FEEDBACK"
    "^\\(?:FEEDBACK ON for \\([[:digit:]]+\\) or more rows\\|feedback \\(OFF\\)\\)"
    "SET FEEDBACK \\1\\2")

   ;; wrap : lines will be wrapped
   ;; wrap : lines will be truncated
   (list (concat "SET WRAP "
                 (if (string=
                      (car (sql-redirect-value
                            sqlbuf
                            "SHOW WRAP"
                            "^wrap : lines will be \\(wrapped\\|truncated\\)" 1))
                      "wrapped")
                     "ON" "OFF")))))

(defun sql-oracle-restore-settings (sqlbuf saved-settings)
  "Restore the SQL*Plus settings in SAVED-SETTINGS."

  ;; Remove any settings that haven't changed
  (mapc
   (lambda (one-cur-setting)
     (setq saved-settings (delete one-cur-setting saved-settings)))
   (sql-oracle-save-settings sqlbuf))

  ;; Restore the changed settings
  (sql-redirect sqlbuf saved-settings))

(defun sql-oracle-list-all (sqlbuf outbuf enhanced table-name)
  ;; Query from USER_OBJECTS or ALL_OBJECTS
  (let ((settings (sql-oracle-save-settings sqlbuf))
        (simple-sql
         (concat
          "SELECT INITCAP(x.object_type) AS SQL_EL_TYPE "
          ", x.object_name AS SQL_EL_NAME "
          "FROM user_objects                    x "
          "WHERE x.object_type NOT LIKE '%% BODY' "
          "ORDER BY 2, 1;"))
        (enhanced-sql
         (concat
          "SELECT INITCAP(x.object_type) AS SQL_EL_TYPE "
          ", x.owner ||'.'|| x.object_name AS SQL_EL_NAME "
          "FROM all_objects x "
          "WHERE x.object_type NOT LIKE '%% BODY' "
          "AND x.owner <> 'SYS' "
          "ORDER BY 2, 1;")))

    (sql-redirect sqlbuf
                  (concat "SET LINESIZE 80 PAGESIZE 50000 TRIMOUT ON"
                          " TAB OFF TIMING OFF FEEDBACK OFF"))

    (sql-redirect sqlbuf
                  (list "COLUMN SQL_EL_TYPE  HEADING \"Type\" FORMAT A19"
                        "COLUMN SQL_EL_NAME  HEADING \"Name\""
                        (format "COLUMN SQL_EL_NAME  FORMAT A%d"
                                (if enhanced 60 35))))

    (sql-redirect sqlbuf
                  (if enhanced enhanced-sql simple-sql)
                  outbuf)

    (sql-redirect sqlbuf
                  '("COLUMN SQL_EL_NAME CLEAR"
                    "COLUMN SQL_EL_TYPE CLEAR"))

    (sql-oracle-restore-settings sqlbuf settings)))

(defun sql-oracle-list-table (sqlbuf outbuf enhanced table-name)
  "Implements :list-table under Oracle."
  (let ((settings (sql-oracle-save-settings sqlbuf)))

    (sql-redirect sqlbuf
                  (format
                   (concat "SET LINESIZE %d PAGESIZE 50000"
                           " DESCRIBE DEPTH 1 LINENUM OFF INDENT ON")
                   (max 65 (min 120 (window-width)))))

    (sql-redirect sqlbuf (format "DESCRIBE %s" table-name)
                  outbuf)

    (sql-oracle-restore-settings sqlbuf settings)))

(defcustom sql-oracle-completion-types '("FUNCTION" "PACKAGE" "PROCEDURE"
                                         "SEQUENCE" "SYNONYM" "TABLE" "TRIGGER"
                                         "TYPE" "VIEW")
  "List of object types to include for completion under Oracle.

See the distinct values in ALL_OBJECTS.OBJECT_TYPE for possible values."
  :version "24.1"
  :type '(repeat string)
  :group 'SQL)

(defun sql-oracle-completion-object (sqlbuf schema)
  (sql-redirect-value
   sqlbuf
   (concat
    "SELECT CHR(1)||"
    (if schema
        (format "owner||'.'||object_name AS o FROM all_objects WHERE owner = %s AND "
                (sql-str-literal (upcase schema)))
      "object_name AS o FROM user_objects WHERE ")
    "temporary = 'N' AND generated = 'N' AND secondary = 'N' AND "
    "object_type IN ("
    (mapconcat (function sql-str-literal) sql-oracle-completion-types ",")
    ");")
   "^[\001]\\(.+\\)$" 1))


;;;###autoload
(defun sql-sybase (&optional buffer)
  "Run isql by Sybase as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-sybase-program'.  Login uses
the variables `sql-server', `sql-user', `sql-password', and
`sql-database' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-sybase-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-sybase].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-sybase].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'sybase buffer))

(defun sql-comint-sybase (product options)
  "Create comint buffer and connect to Sybase."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params options))
    (if (not (string= "" sql-server))
	(setq params (append (list "-S" sql-server) params)))
    (if (not (string= "" sql-database))
	(setq params (append (list "-D" sql-database) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list "-P" sql-password) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list "-U" sql-user) params)))
    (sql-comint product params)))



;;;###autoload
(defun sql-informix (&optional buffer)
  "Run dbaccess by Informix as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-informix-program'.  Login uses
the variable `sql-database' as default, if set.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-informix].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-informix].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'informix buffer))

(defun sql-comint-informix (product options)
  "Create comint buffer and connect to Informix."
  ;; username and password are ignored.
  (let ((db (if (string= "" sql-database)
		"-"
	      (if (string= "" sql-server)
		  sql-database
		(concat sql-database "@" sql-server)))))
    (sql-comint product (append `(,db "-") options))))



;;;###autoload
(defun sql-sqlite (&optional buffer)
  "Run sqlite as an inferior process.

SQLite is free software.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-sqlite-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-sqlite-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-sqlite].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-sqlite].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'sqlite buffer))

(defun sql-comint-sqlite (product options)
  "Create comint buffer and connect to SQLite."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params))
    (if (not (string= "" sql-database))
	(setq params (append (list (expand-file-name sql-database))
                             params)))
    (setq params (append options params))
    (sql-comint product params)))

(defun sql-sqlite-completion-object (sqlbuf schema)
  (sql-redirect-value sqlbuf ".tables" "\\sw\\(?:\\sw\\|\\s_\\)*" 0))



;;;###autoload
(defun sql-mysql (&optional buffer)
  "Run mysql by TcX as an inferior process.

Mysql versions 3.23 and up are free software.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-mysql-program'.  Login uses
the variables `sql-user', `sql-password', `sql-database', and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-mysql-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-mysql].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-mysql].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'mysql buffer))

(defun sql-comint-mysql (product options)
  "Create comint buffer and connect to MySQL."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params))
    (if (not (string= "" sql-database))
	(setq params (append (list sql-database) params)))
    (if (not (string= "" sql-server))
	(setq params (append (list (concat "--host=" sql-server)) params)))
    (if (not (= 0 sql-port))
	(setq params (append (list (concat "--port=" (number-to-string sql-port))) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list (concat "--password=" sql-password)) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list (concat "--user=" sql-user)) params)))
    (setq params (append options params))
    (sql-comint product params)))



;;;###autoload
(defun sql-solid (&optional buffer)
  "Run solsql by Solid as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-solid-program'.  Login uses
the variables `sql-user', `sql-password', and `sql-server' as
defaults, if set.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-solid].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-solid].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'solid buffer))

(defun sql-comint-solid (product options)
  "Create comint buffer and connect to Solid."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params options))
    ;; It only makes sense if both username and password are there.
    (if (not (or (string= "" sql-user)
		 (string= "" sql-password)))
	(setq params (append (list sql-user sql-password) params)))
    (if (not (string= "" sql-server))
	(setq params (append (list sql-server) params)))
    (sql-comint product params)))



;;;###autoload
(defun sql-ingres (&optional buffer)
  "Run sql by Ingres as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-ingres-program'.  Login uses
the variable `sql-database' as default, if set.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-ingres].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-ingres].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'ingres buffer))

(defun sql-comint-ingres (product options)
  "Create comint buffer and connect to Ingres."
  ;; username and password are ignored.
  (sql-comint product
               (append (if (string= "" sql-database)
                           nil
                         (list sql-database))
                       options)))



;;;###autoload
(defun sql-ms (&optional buffer)
  "Run osql by Microsoft as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-ms-program'.  Login uses the
variables `sql-user', `sql-password', `sql-database', and `sql-server'
as defaults, if set.  Additional command line parameters can be stored
in the list `sql-ms-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-ms].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-ms].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'ms buffer))

(defun sql-comint-ms (product options)
  "Create comint buffer and connect to Microsoft SQL Server."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params options))
    (if (not (string= "" sql-server))
        (setq params (append (list "-S" sql-server) params)))
    (if (not (string= "" sql-database))
        (setq params (append (list "-d" sql-database) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list "-U" sql-user) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list "-P" sql-password) params))
      (if (string= "" sql-user)
	  ;; if neither user nor password is provided, use system
	  ;; credentials.
	  (setq params (append (list "-E") params))
	;; If -P is passed to ISQL as the last argument without a
	;; password, it's considered null.
	(setq params (append params (list "-P")))))
    (sql-comint product params)))



;;;###autoload
(defun sql-postgres (&optional buffer)
  "Run psql by Postgres as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-postgres-program'.  Login uses
the variables `sql-database' and `sql-server' as default, if set.
Additional command line parameters can be stored in the list
`sql-postgres-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-postgres].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-postgres].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.  If your output lines end with ^M,
your might try undecided-dos as a coding system.  If this doesn't help,
Try to set `comint-output-filter-functions' like this:

\(setq comint-output-filter-functions (append comint-output-filter-functions
					     '(comint-strip-ctrl-m)))

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'postgres buffer))

(defun sql-comint-postgres (product options)
  "Create comint buffer and connect to Postgres."
  ;; username and password are ignored.  Mark Stosberg suggest to add
  ;; the database at the end.  Jason Beegan suggest using --pset and
  ;; pager=off instead of \\o|cat.  The later was the solution by
  ;; Gregor Zych.  Jason's suggestion is the default value for
  ;; sql-postgres-options.
  (let ((params options))
    (if (not (string= "" sql-database))
	(setq params (append params (list sql-database))))
    (if (not (string= "" sql-server))
	(setq params (append (list "-h" sql-server) params)))
    (if (not (string= "" sql-user))
	(setq params (append (list "-U" sql-user) params)))
    (if (not (= 0 sql-port))
	(setq params (append (list "-p" (number-to-string sql-port)) params)))
    (sql-comint product params)))

(defun sql-postgres-completion-object (sqlbuf schema)
  (let (cl re fs a r)
    (sql-redirect sqlbuf "\\t on")
    (setq a (car (sql-redirect-value sqlbuf "\\a" "Output format is \\(.*\\)[.]$" 1)))
    (when (string= a "aligned")
      (sql-redirect sqlbuf "\\a"))
    (setq fs (or (car (sql-redirect-value sqlbuf "\\f" "Field separator is \"\\(.\\)[.]$" 1)) "|"))

    (setq re (concat "^\\([^" fs "]*\\)" fs "\\([^" fs "]*\\)" fs "[^" fs "]*" fs  "[^" fs "]*$"))
    (setq cl (if (not schema)
                 (sql-redirect-value sqlbuf "\\d" re '(1 2))
               (append (sql-redirect-value sqlbuf (format "\\dt %s.*" schema) re '(1 2))
                       (sql-redirect-value sqlbuf (format "\\dv %s.*" schema) re '(1 2))
                       (sql-redirect-value sqlbuf (format "\\ds %s.*" schema) re '(1 2)))))

    ;; Restore tuples and alignment to what they were
    (sql-redirect sqlbuf "\\t off")
    (when (not (string= a "aligned"))
      (sql-redirect sqlbuf "\\a"))

    ;; Return the list of table names (public schema name can be omitted)
    (mapcar (lambda (tbl)
              (if (string= (car tbl) "public")
                  (cadr tbl)
                (format "%s.%s" (car tbl) (cadr tbl))))
            cl)))



;;;###autoload
(defun sql-interbase (&optional buffer)
  "Run isql by Interbase as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-interbase-program'.  Login
uses the variables `sql-user', `sql-password', and `sql-database' as
defaults, if set.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-interbase].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-interbase].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'interbase buffer))

(defun sql-comint-interbase (product options)
  "Create comint buffer and connect to Interbase."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params options))
    (if (not (string= "" sql-user))
	(setq params (append (list "-u" sql-user) params)))
    (if (not (string= "" sql-password))
	(setq params (append (list "-p" sql-password) params)))
    (if (not (string= "" sql-database))
        (setq params (cons sql-database params))) ; add to the front!
    (sql-comint product params)))



;;;###autoload
(defun sql-db2 (&optional buffer)
  "Run db2 by IBM as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-db2-program'.  There is not
automatic login.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

If you use \\[sql-accumulate-and-indent] to send multiline commands to
db2, newlines will be escaped if necessary.  If you don't want that, set
`comint-input-sender' back to `comint-simple-send' by writing an after
advice.  See the elisp manual for more information.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-db2].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-db2].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'db2 buffer))

(defun sql-comint-db2 (product options)
  "Create comint buffer and connect to DB2."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (sql-comint product options))

;;;###autoload
(defun sql-linter (&optional buffer)
  "Run inl by RELEX as an inferior process.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-linter-program' - usually `inl'.
Login uses the variables `sql-user', `sql-password', `sql-database' and
`sql-server' as defaults, if set.  Additional command line parameters
can be stored in the list `sql-linter-options'.  Run inl -h to get help on
parameters.

`sql-database' is used to set the LINTER_MBX environment variable for
local connections, `sql-server' refers to the server name from the
`nodetab' file for the network connection (dbc_tcp or friends must run
for this to work).  If `sql-password' is an empty string, inl will use
an empty password.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-linter].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

\(Type \\[describe-mode] in the SQL buffer for a list of commands.)"
  (interactive "P")
  (sql-product-interactive 'linter buffer))

(defun sql-comint-linter (product options)
  "Create comint buffer and connect to Linter."
  ;; Put all parameters to the program (if defined) in a list and call
  ;; make-comint.
  (let ((params options)
        (login nil)
        (old-mbx (getenv "LINTER_MBX")))
    (if (not (string= "" sql-user))
	(setq login (concat sql-user "/" sql-password)))
    (setq params (append (list "-u" login) params))
    (if (not (string= "" sql-server))
	(setq params (append (list "-n" sql-server) params)))
    (if (string= "" sql-database)
	(setenv "LINTER_MBX" nil)
      (setenv "LINTER_MBX" sql-database))
    (sql-comint product params)
    (setenv "LINTER_MBX" old-mbx)))



(provide 'sql)

;;; sql.el ends here

; LocalWords:  sql SQL SQLite sqlite Sybase Informix MySQL
; LocalWords:  Postgres SQLServer SQLi
