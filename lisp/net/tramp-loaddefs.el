;;; tramp-loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tramp-shell-quote-argument tramp-parse-putty tramp-parse-netrc
;;;;;;  tramp-parse-passwd tramp-parse-hosts tramp-parse-shostkeys
;;;;;;  tramp-parse-sconfig tramp-parse-shosts tramp-parse-rhosts
;;;;;;  tramp-completion-mode-p tramp-set-completion-function tramp-tramp-file-p
;;;;;;  tramp-default-user-alist tramp-default-method-alist) "tramp"
;;;;;;  "tramp.el" (20400 62402))
;;; Generated autoloads from tramp.el

(defvar tramp-methods nil "\
*Alist of methods for remote files.
This is a list of entries of the form (NAME PARAM1 PARAM2 ...).
Each NAME stands for a remote access method.  Each PARAM is a
pair of the form (KEY VALUE).  The following KEYs are defined:
  * `tramp-remote-shell'
    This specifies the shell to use on the remote host.  This
    MUST be a Bourne-like shell.  It is normally not necessary to
    set this to any value other than \"/bin/sh\": Tramp wants to
    use a shell which groks tilde expansion, but it can search
    for it.  Also note that \"/bin/sh\" exists on all Unixen,
    this might not be true for the value that you decide to use.
    You Have Been Warned.
  * `tramp-remote-shell-args'
    For implementation of `shell-command', this specifies the
    arguments to let `tramp-remote-shell' run a single command.
  * `tramp-login-program'
    This specifies the name of the program to use for logging in to the
    remote host.  This may be the name of rsh or a workalike program,
    or the name of telnet or a workalike, or the name of su or a workalike.
  * `tramp-login-args'
    This specifies the list of arguments to pass to the above
    mentioned program.  Please note that this is a list of list of arguments,
    that is, normally you don't want to put \"-a -b\" or \"-f foo\"
    here.  Instead, you want a list (\"-a\" \"-b\"), or (\"-f\" \"foo\").
    There are some patterns: \"%h\" in this list is replaced by the host
    name, \"%u\" is replaced by the user name, \"%p\" is replaced by the
    port number, and \"%%\" can be used to obtain a literal percent character.
    If a list containing \"%h\", \"%u\" or \"%p\" is unchanged during
    expansion (i.e. no host or no user specified), this list is not used as
    argument.  By this, arguments like (\"-l\" \"%u\") are optional.
    \"%t\" is replaced by the temporary file name produced with
    `tramp-make-tramp-temp-file'.  \"%k\" indicates the keep-date
    parameter of a program, if exists.
  * `tramp-async-args'
    When an asynchronous process is started, we know already that
    the connection works.  Therefore, we can pass additional
    parameters to suppress diagnostic messages, in order not to
    tamper the process output.
  * `tramp-copy-program'
    This specifies the name of the program to use for remotely copying
    the file; this might be the absolute filename of rcp or the name of
    a workalike program.
  * `tramp-copy-args'
    This specifies the list of parameters to pass to the above mentioned
    program, the hints for `tramp-login-args' also apply here.
  * `tramp-copy-keep-date'
    This specifies whether the copying program when the preserves the
    timestamp of the original file.
  * `tramp-copy-keep-tmpfile'
    This specifies whether a temporary local file shall be kept
    for optimization reasons (useful for \"rsync\" methods).
  * `tramp-copy-recursive'
    Whether the operation copies directories recursively.
  * `tramp-default-port'
    The default port of a method is needed in case of gateway connections.
    Additionally, it is used as indication which method is prepared for
    passing gateways.
  * `tramp-gw-args'
    As the attribute name says, additional arguments are specified here
    when a method is applied via a gateway.
  * `tramp-password-end-of-line'
    This specifies the string to use for terminating the line after
    submitting the password.  If this method parameter is nil, then the
    value of the normal variable `tramp-default-password-end-of-line'
    is used.  This parameter is necessary because the \"plink\" program
    requires any two characters after sending the password.  These do
    not have to be newline or carriage return characters.  Other login
    programs are happy with just one character, the newline character.
    We use \"xy\" as the value for methods using \"plink\".
  * `tramp-tmpdir'
    A directory on the remote host for temporary files.  If not
    specified, \"/tmp\" is taken as default.

What does all this mean?  Well, you should specify `tramp-login-program'
for all methods; this program is used to log in to the remote site.  Then,
there are two ways to actually transfer the files between the local and the
remote side.  One way is using an additional rcp-like program.  If you want
to do this, set `tramp-copy-program' in the method.

Another possibility for file transfer is inline transfer, i.e. the
file is passed through the same buffer used by `tramp-login-program'.  In
this case, the file contents need to be protected since the
`tramp-login-program' might use escape codes or the connection might not
be eight-bit clean.  Therefore, file contents are encoded for transit.
See the variables `tramp-local-coding-commands' and
`tramp-remote-coding-commands' for details.

So, to summarize: if the method is an out-of-band method, then you
must specify `tramp-copy-program' and `tramp-copy-args'.  If it is an
inline method, then these two parameters should be nil.  Methods which
are fit for gateways must have `tramp-default-port' at least.

Notes:

When using `su' or `sudo' the phrase `open connection to a remote
host' sounds strange, but it is used nevertheless, for consistency.
No connection is opened to a remote host, but `su' or `sudo' is
started on the local host.  You should specify a remote host
`localhost' or the name of the local host.  Another host name is
useful only in combination with `tramp-default-proxies-alist'.")

(defvar tramp-default-method-alist nil "\
*Default method to use for specific host/user pairs.
This is an alist of items (HOST USER METHOD).  The first matching item
specifies the method to use for a file name which does not specify a
method.  HOST and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-method' takes effect.

If the file name does not specify the user, lookup is done using the
empty string for the user name.

See `tramp-methods' for a list of possibilities for METHOD.")

(custom-autoload 'tramp-default-method-alist "tramp" t)

(defvar tramp-default-user-alist nil "\
*Default user to use for specific method/host pairs.
This is an alist of items (METHOD HOST USER).  The first matching item
specifies the user to use for a file name which does not specify a
user.  METHOD and USER are regular expressions or nil, which is
interpreted as a regular expression which always matches.  If no entry
matches, the variable `tramp-default-user' takes effect.

If the file name does not specify the method, lookup is done using the
empty string for the method name.")

(custom-autoload 'tramp-default-user-alist "tramp" t)

(defconst tramp-local-host-regexp (concat "\\`" (regexp-opt (list "localhost" "localhost6" (system-name) "127.0.0.1" "::1") t) "\\'") "\
*Host names which are regarded as local host.")

(defconst tramp-prefix-domain-format "%" "\
*String matching delimiter between user and domain names.")

(defconst tramp-prefix-domain-regexp (regexp-quote tramp-prefix-domain-format) "\
*Regexp matching delimiter between user and domain names.
Derived from `tramp-prefix-domain-format'.")

(defvar tramp-foreign-file-name-handler-alist nil "\
Alist of elements (FUNCTION . HANDLER) for foreign methods handled specially.
If (FUNCTION FILENAME) returns non-nil, then all I/O on that file is done by
calling HANDLER.")

(autoload 'tramp-tramp-file-p "tramp" "\
Return t if NAME is a string with Tramp file name syntax.

\(fn NAME)" nil nil)

(autoload 'tramp-set-completion-function "tramp" "\
Sets the list of completion functions for METHOD.
FUNCTION-LIST is a list of entries of the form (FUNCTION FILE).
The FUNCTION is intended to parse FILE according its syntax.
It might be a predefined FUNCTION, or a user defined FUNCTION.
Predefined FUNCTIONs are `tramp-parse-rhosts', `tramp-parse-shosts',
`tramp-parse-sconfig', `tramp-parse-hosts', `tramp-parse-passwd',
and `tramp-parse-netrc'.

Example:

    (tramp-set-completion-function
     \"ssh\"
     '((tramp-parse-sconfig \"/etc/ssh_config\")
       (tramp-parse-sconfig \"~/.ssh/config\")))

\(fn METHOD FUNCTION-LIST)" nil nil)

(autoload 'tramp-completion-mode-p "tramp" "\
Check, whether method / user name / host name completion is active.

\(fn)" nil nil)

(autoload 'tramp-parse-rhosts "tramp" "\
Return a list of (user host) tuples allowed to access.
Either user or host may be nil.

\(fn FILENAME)" nil nil)

(autoload 'tramp-parse-shosts "tramp" "\
Return a list of (user host) tuples allowed to access.
User is always nil.

\(fn FILENAME)" nil nil)

(autoload 'tramp-parse-sconfig "tramp" "\
Return a list of (user host) tuples allowed to access.
User is always nil.

\(fn FILENAME)" nil nil)

(autoload 'tramp-parse-shostkeys "tramp" "\
Return a list of (user host) tuples allowed to access.
User is always nil.

\(fn DIRNAME)" nil nil)

(autoload 'tramp-parse-hosts "tramp" "\
Return a list of (user host) tuples allowed to access.
User is always nil.

\(fn FILENAME)" nil nil)

(autoload 'tramp-parse-passwd "tramp" "\
Return a list of (user host) tuples allowed to access.
Host is always \"localhost\".

\(fn FILENAME)" nil nil)

(autoload 'tramp-parse-netrc "tramp" "\
Return a list of (user host) tuples allowed to access.
User may be nil.

\(fn FILENAME)" nil nil)

(autoload 'tramp-parse-putty "tramp" "\
Return a list of (user host) tuples allowed to access.
User is always nil.

\(fn REGISTRY)" nil nil)

(autoload 'tramp-shell-quote-argument "tramp" "\
Similar to `shell-quote-argument', but groks newlines.
Only works for Bourne-like shells.

\(fn S)" nil nil)

;;;***

;;;### (autoloads (tramp-parse-connection-properties tramp-list-connections
;;;;;;  tramp-cache-print tramp-flush-connection-property with-connection-property
;;;;;;  tramp-set-connection-property tramp-get-connection-property
;;;;;;  tramp-flush-directory-property tramp-flush-file-property
;;;;;;  with-file-property tramp-set-file-property tramp-get-file-property)
;;;;;;  "tramp-cache" "tramp-cache.el" (20352 65510))
;;; Generated autoloads from tramp-cache.el

(defvar tramp-cache-data (make-hash-table :test 'equal) "\
Hash table for remote files properties.")

(autoload 'tramp-get-file-property "tramp-cache" "\
Get the PROPERTY of FILE from the cache context of VEC.
Returns DEFAULT if not set.

\(fn VEC FILE PROPERTY DEFAULT)" nil nil)

(autoload 'tramp-set-file-property "tramp-cache" "\
Set the PROPERTY of FILE to VALUE, in the cache context of VEC.
Returns VALUE.

\(fn VEC FILE PROPERTY VALUE)" nil nil)

(autoload 'with-file-property "tramp-cache" "\
Check in Tramp cache for PROPERTY, otherwise execute BODY and set cache.
FILE must be a local file name on a connection identified via VEC.

\(fn VEC FILE PROPERTY &rest BODY)" nil (quote macro))

(put 'with-file-property 'lisp-indent-function 3)

(autoload 'tramp-flush-file-property "tramp-cache" "\
Remove all properties of FILE in the cache context of VEC.

\(fn VEC FILE)" nil nil)

(autoload 'tramp-flush-directory-property "tramp-cache" "\
Remove all properties of DIRECTORY in the cache context of VEC.
Remove also properties of all files in subdirectories.

\(fn VEC DIRECTORY)" nil nil)

(autoload 'tramp-get-connection-property "tramp-cache" "\
Get the named PROPERTY for the connection.
KEY identifies the connection, it is either a process or a vector.
If the value is not set for the connection, returns DEFAULT.

\(fn KEY PROPERTY DEFAULT)" nil nil)

(autoload 'tramp-set-connection-property "tramp-cache" "\
Set the named PROPERTY of a connection to VALUE.
KEY identifies the connection, it is either a process or a vector.
PROPERTY is set persistent when KEY is a vector.

\(fn KEY PROPERTY VALUE)" nil nil)

(autoload 'with-connection-property "tramp-cache" "\
Check in Tramp for property PROPERTY, otherwise executes BODY and set.

\(fn KEY PROPERTY &rest BODY)" nil (quote macro))

(put 'with-connection-property 'lisp-indent-function 2)

(autoload 'tramp-flush-connection-property "tramp-cache" "\
Remove all properties identified by KEY.
KEY identifies the connection, it is either a process or a vector.

\(fn KEY)" nil nil)

(autoload 'tramp-cache-print "tramp-cache" "\
Print hash table TABLE.

\(fn TABLE)" nil nil)

(autoload 'tramp-list-connections "tramp-cache" "\
Return a list of all known connection vectors according to `tramp-cache'.

\(fn)" nil nil)

(autoload 'tramp-parse-connection-properties "tramp-cache" "\
Return a list of (user host) tuples allowed to access for METHOD.
This function is added always in `tramp-get-completion-function'
for all methods.  Resulting data are derived from connection history.

\(fn METHOD)" nil nil)

;;;***

;;;### (autoloads (tramp-bug tramp-version tramp-cleanup-all-buffers
;;;;;;  tramp-cleanup-all-connections tramp-cleanup-this-connection
;;;;;;  tramp-cleanup-connection) "tramp-cmds" "tramp-cmds.el" (20352
;;;;;;  65510))
;;; Generated autoloads from tramp-cmds.el

(autoload 'tramp-cleanup-connection "tramp-cmds" "\
Flush all connection related objects.
This includes password cache, file cache, connection cache, buffers.
When called interactively, a Tramp connection has to be selected.

\(fn VEC)" t nil)

(autoload 'tramp-cleanup-this-connection "tramp-cmds" "\
Flush all connection related objects of the current buffer's connection.

\(fn)" t nil)

(autoload 'tramp-cleanup-all-connections "tramp-cmds" "\
Flush all Tramp internal objects.
This includes password cache, file cache, connection cache, buffers.

\(fn)" t nil)

(autoload 'tramp-cleanup-all-buffers "tramp-cmds" "\
Kill all remote buffers.

\(fn)" t nil)

(autoload 'tramp-version "tramp-cmds" "\
Print version number of tramp.el in minibuffer or current buffer.

\(fn ARG)" t nil)

(autoload 'tramp-bug "tramp-cmds" "\
Submit a bug report to the Tramp developers.

\(fn)" t nil)

;;;***

;;;### (autoloads (tramp-ftp-file-name-handler) "tramp-ftp" "tramp-ftp.el"
;;;;;;  (20400 62402))
;;; Generated autoloads from tramp-ftp.el

(defconst tramp-ftp-method "ftp" "\
*When this method name is used, forward all calls to Ange-FTP.")

(unless (featurep 'xemacs) (add-to-list 'tramp-methods (cons tramp-ftp-method nil)) (add-to-list 'tramp-default-method-alist (list "\\`ftp\\." nil tramp-ftp-method)) (add-to-list 'tramp-default-method-alist (list nil "\\`\\(anonymous\\|ftp\\)\\'" tramp-ftp-method)))

(eval-after-load 'tramp '(tramp-set-completion-function tramp-ftp-method '((tramp-parse-netrc "~/.netrc"))))

(autoload 'tramp-ftp-file-name-handler "tramp-ftp" "\
Invoke the Ange-FTP handler for OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(defsubst tramp-ftp-file-name-p (filename) "\
Check if it's a filename that should be forwarded to Ange-FTP." (let ((v (tramp-dissect-file-name filename))) (string= (tramp-file-name-method v) tramp-ftp-method)))

(unless (featurep 'xemacs) (add-to-list 'tramp-foreign-file-name-handler-alist (cons 'tramp-ftp-file-name-p 'tramp-ftp-file-name-handler)))

;;;***

;;;### (autoloads (tramp-gvfs-file-name-handler tramp-gvfs-methods)
;;;;;;  "tramp-gvfs" "tramp-gvfs.el" (20400 62402))
;;; Generated autoloads from tramp-gvfs.el

(defvar tramp-gvfs-methods '("dav" "davs" "obex" "synce") "\
*List of methods for remote files, accessed with GVFS.")

(custom-autoload 'tramp-gvfs-methods "tramp-gvfs" t)

(add-to-list 'tramp-default-user-alist '("\\`synce\\'" nil nil))

(when (featurep 'dbusbind) (dolist (elt tramp-gvfs-methods) (unless (assoc elt tramp-methods) (add-to-list 'tramp-methods (cons elt nil)))))

(defsubst tramp-gvfs-file-name-p (filename) "\
Check if it's a filename handled by the GVFS daemon." (and (tramp-tramp-file-p filename) (let ((method (tramp-file-name-method (tramp-dissect-file-name filename)))) (and (stringp method) (member method tramp-gvfs-methods)))))

(autoload 'tramp-gvfs-file-name-handler "tramp-gvfs" "\
Invoke the GVFS related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(when (featurep 'dbusbind) (add-to-list 'tramp-foreign-file-name-handler-alist (cons 'tramp-gvfs-file-name-p 'tramp-gvfs-file-name-handler)))

;;;***

;;;### (autoloads (tramp-gw-open-connection) "tramp-gw" "tramp-gw.el"
;;;;;;  (20400 62402))
;;; Generated autoloads from tramp-gw.el

(defconst tramp-gw-tunnel-method "tunnel" "\
*Method to connect HTTP gateways.")

(defconst tramp-gw-socks-method "socks" "\
*Method to connect SOCKS servers.")

(add-to-list 'tramp-default-user-alist (list (concat "\\`" (regexp-opt (list tramp-gw-tunnel-method tramp-gw-socks-method)) "\\'") nil (user-login-name)))

(autoload 'tramp-gw-open-connection "tramp-gw" "\
Open a remote connection to VEC (see `tramp-file-name' structure).
Take GW-VEC as SOCKS or HTTP gateway, i.e. its method must be a
gateway method.  TARGET-VEC identifies where to connect to via
the gateway, it can be different from VEC when there are more
hops to be applied.

It returns a string like \"localhost#port\", which must be used
instead of the host name declared in TARGET-VEC.

\(fn VEC GW-VEC TARGET-VEC)" nil nil)

;;;***

;;;### (autoloads (tramp-sh-file-name-handler tramp-remote-path tramp-terminal-type)
;;;;;;  "tramp-sh" "tramp-sh.el" (20400 62402))
;;; Generated autoloads from tramp-sh.el

(defvar tramp-terminal-type "dumb" "\
*Value of TERM environment variable for logging in to remote host.
Because Tramp wants to parse the output of the remote shell, it is easily
confused by ANSI color escape sequences and suchlike.  Often, shell init
files conditionalize this setup based on the TERM environment variable.")

(custom-autoload 'tramp-terminal-type "tramp-sh" t)

(defconst tramp-initial-end-of-output "#$ " "\
Prompt when establishing a connection.")

(add-to-list 'tramp-methods '("rcp" (tramp-login-program "rsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "rcp") (tramp-copy-args (("-p" "%k") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t)))

(add-to-list 'tramp-methods '("remcp" (tramp-login-program "remsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "rcp") (tramp-copy-args (("-p" "%k"))) (tramp-copy-keep-date t)))

(add-to-list 'tramp-methods '("scp" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-P" "%p") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("scp1" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-1") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-1") ("-P" "%p") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("scp2" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-2") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-2") ("-P" "%p") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("scpc" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-o" "ControlPath=%t.%%r@%%h:%%p") ("-o" "ControlMaster=yes") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-P" "%p") ("-p" "%k") ("-q") ("-r") ("-o" "ControlPath=%t.%%r@%%h:%%p") ("-o" "ControlMaster=auto"))) (tramp-copy-keep-date t) (tramp-copy-recursive t) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("scpx" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-e" "none") ("-t" "-t") ("%h") ("/bin/sh"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "scp") (tramp-copy-args (("-P" "%p") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("sftp" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "sftp")))

(add-to-list 'tramp-methods '("rsync" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "rsync") (tramp-copy-args (("-e" "ssh") ("-t" "%k") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-keep-tmpfile t) (tramp-copy-recursive t)))

(add-to-list 'tramp-methods `("rsyncc" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-o" "ControlPath=%t.%%r@%%h:%%p") ("-o" "ControlMaster=yes") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "rsync") (tramp-copy-args (("-t" "%k") ("-r"))) (tramp-copy-env (("RSYNC_RSH") (,(concat "ssh" " -o ControlPath=%t.%%r@%%h:%%p" " -o ControlMaster=auto")))) (tramp-copy-keep-date t) (tramp-copy-keep-tmpfile t) (tramp-copy-recursive t)))

(add-to-list 'tramp-methods '("rsh" (tramp-login-program "rsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods '("remsh" (tramp-login-program "remsh") (tramp-login-args (("%h") ("-l" "%u"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods '("ssh" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("ssh1" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-1") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("ssh2" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-2") ("-e" "none") ("%h"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("sshx" (tramp-login-program "ssh") (tramp-login-args (("-l" "%u") ("-p" "%p") ("-e" "none") ("-t" "-t") ("%h") ("/bin/sh"))) (tramp-async-args (("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-gw-args (("-o" "GlobalKnownHostsFile=/dev/null") ("-o" "UserKnownHostsFile=/dev/null") ("-o" "StrictHostKeyChecking=no"))) (tramp-default-port 22)))

(add-to-list 'tramp-methods '("telnet" (tramp-login-program "telnet") (tramp-login-args (("%h") ("%p"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-default-port 23)))

(add-to-list 'tramp-methods '("su" (tramp-login-program "su") (tramp-login-args (("-") ("%u"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods '("sudo" (tramp-login-program "sudo") (tramp-login-args (("-u" "%u") ("-s") ("-H") ("-p" "Password:"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods '("ksu" (tramp-login-program "ksu") (tramp-login-args (("%u") ("-q"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods '("krlogin" (tramp-login-program "krlogin") (tramp-login-args (("%h") ("-l" "%u") ("-x"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods '("plink" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("%h"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-password-end-of-line "xy") (tramp-default-port 22)))

(add-to-list 'tramp-methods '("plink1" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-1" "-ssh") ("%h"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-password-end-of-line "xy") (tramp-default-port 22)))

(add-to-list 'tramp-methods `("plinkx" (tramp-login-program "plink") (tramp-login-args (("-load") ("%h") ("-t") (,(format "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'" tramp-terminal-type tramp-initial-end-of-output)) ("/bin/sh"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c"))))

(add-to-list 'tramp-methods '("pscp" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("%h"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "pscp") (tramp-copy-args (("-l" "%u") ("-P" "%p") ("-scp") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t) (tramp-password-end-of-line "xy") (tramp-default-port 22)))

(add-to-list 'tramp-methods '("psftp" (tramp-login-program "plink") (tramp-login-args (("-l" "%u") ("-P" "%p") ("-ssh") ("%h"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-c")) (tramp-copy-program "pscp") (tramp-copy-args (("-l" "%u") ("-P" "%p") ("-sftp") ("-p" "%k") ("-q") ("-r"))) (tramp-copy-keep-date t) (tramp-copy-recursive t) (tramp-password-end-of-line "xy")))

(add-to-list 'tramp-methods '("fcp" (tramp-login-program "fsh") (tramp-login-args (("%h") ("-l" "%u") ("sh" "-i"))) (tramp-remote-shell "/bin/sh") (tramp-remote-shell-args ("-i") ("-c")) (tramp-copy-program "fcp") (tramp-copy-args (("-p" "%k"))) (tramp-copy-keep-date t)))

(add-to-list 'tramp-default-method-alist `(,tramp-local-host-regexp "\\`root\\'" "su"))

(add-to-list 'tramp-default-user-alist `(,(concat "\\`" (regexp-opt '("su" "sudo" "ksu")) "\\'") nil "root"))

(add-to-list 'tramp-default-user-alist `(,(concat "\\`" (regexp-opt '("rcp" "remcp" "rsh" "telnet" "krlogin" "fcp")) "\\'") nil ,(user-login-name)))

(defconst tramp-completion-function-alist-rsh '((tramp-parse-rhosts "/etc/hosts.equiv") (tramp-parse-rhosts "~/.rhosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for rsh methods.")

(defconst tramp-completion-function-alist-ssh '((tramp-parse-rhosts "/etc/hosts.equiv") (tramp-parse-rhosts "/etc/shosts.equiv") (tramp-parse-shosts "/etc/ssh_known_hosts") (tramp-parse-sconfig "/etc/ssh_config") (tramp-parse-shostkeys "/etc/ssh2/hostkeys") (tramp-parse-sknownhosts "/etc/ssh2/knownhosts") (tramp-parse-rhosts "~/.rhosts") (tramp-parse-rhosts "~/.shosts") (tramp-parse-shosts "~/.ssh/known_hosts") (tramp-parse-sconfig "~/.ssh/config") (tramp-parse-shostkeys "~/.ssh2/hostkeys") (tramp-parse-sknownhosts "~/.ssh2/knownhosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for ssh methods.")

(defconst tramp-completion-function-alist-telnet '((tramp-parse-hosts "/etc/hosts")) "\
Default list of (FUNCTION FILE) pairs to be examined for telnet methods.")

(defconst tramp-completion-function-alist-su '((tramp-parse-passwd "/etc/passwd")) "\
Default list of (FUNCTION FILE) pairs to be examined for su methods.")

(defconst tramp-completion-function-alist-putty '((tramp-parse-putty "HKEY_CURRENT_USER\\Software\\SimonTatham\\PuTTY\\Sessions")) "\
Default list of (FUNCTION REGISTRY) pairs to be examined for putty methods.")

(eval-after-load 'tramp '(progn (tramp-set-completion-function "rcp" tramp-completion-function-alist-rsh) (tramp-set-completion-function "remcp" tramp-completion-function-alist-rsh) (tramp-set-completion-function "scp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "scp1" tramp-completion-function-alist-ssh) (tramp-set-completion-function "scp2" tramp-completion-function-alist-ssh) (tramp-set-completion-function "scpc" tramp-completion-function-alist-ssh) (tramp-set-completion-function "scpx" tramp-completion-function-alist-ssh) (tramp-set-completion-function "sftp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "rsync" tramp-completion-function-alist-ssh) (tramp-set-completion-function "rsyncc" tramp-completion-function-alist-ssh) (tramp-set-completion-function "rsh" tramp-completion-function-alist-rsh) (tramp-set-completion-function "remsh" tramp-completion-function-alist-rsh) (tramp-set-completion-function "ssh" tramp-completion-function-alist-ssh) (tramp-set-completion-function "ssh1" tramp-completion-function-alist-ssh) (tramp-set-completion-function "ssh2" tramp-completion-function-alist-ssh) (tramp-set-completion-function "ssh1_old" tramp-completion-function-alist-ssh) (tramp-set-completion-function "ssh2_old" tramp-completion-function-alist-ssh) (tramp-set-completion-function "sshx" tramp-completion-function-alist-ssh) (tramp-set-completion-function "telnet" tramp-completion-function-alist-telnet) (tramp-set-completion-function "su" tramp-completion-function-alist-su) (tramp-set-completion-function "sudo" tramp-completion-function-alist-su) (tramp-set-completion-function "ksu" tramp-completion-function-alist-su) (tramp-set-completion-function "krlogin" tramp-completion-function-alist-rsh) (tramp-set-completion-function "plink" tramp-completion-function-alist-ssh) (tramp-set-completion-function "plink1" tramp-completion-function-alist-ssh) (tramp-set-completion-function "plinkx" tramp-completion-function-alist-putty) (tramp-set-completion-function "pscp" tramp-completion-function-alist-ssh) (tramp-set-completion-function "fcp" tramp-completion-function-alist-ssh)))

(defvar tramp-remote-path '(tramp-default-remote-path "/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin") "\
*List of directories to search for executables on remote host.
For every remote host, this variable will be set buffer local,
keeping the list of existing directories on that host.

You can use `~' in this list, but when searching for a shell which groks
tilde expansion, all directory names starting with `~' will be ignored.

`Default Directories' represent the list of directories given by
the command \"getconf PATH\".  It is recommended to use this
entry on top of this list, because these are the default
directories for POSIX compatible commands.  On remote hosts which
do not offer the getconf command (like cygwin), the value
\"/bin:/usr/bin\" is used instead of.

`Private Directories' are the settings of the $PATH environment,
as given in your `~/.profile'.")

(custom-autoload 'tramp-remote-path "tramp-sh" t)

(add-to-list 'tramp-foreign-file-name-handler-alist '(identity . tramp-sh-file-name-handler) 'append)

(autoload 'tramp-sh-file-name-handler "tramp-sh" "\
Invoke remote-shell Tramp file name handler.
Fall back to normal file name handler if no Tramp handler exists.

\(fn OPERATION &rest ARGS)" nil nil)

;;;***

;;;### (autoloads (tramp-smb-file-name-handler) "tramp-smb" "tramp-smb.el"
;;;;;;  (20400 62402))
;;; Generated autoloads from tramp-smb.el

(defconst tramp-smb-method "smb" "\
*Method to connect SAMBA and M$ SMB servers.")

(unless (memq system-type '(cygwin windows-nt)) (add-to-list 'tramp-methods `(,tramp-smb-method (tramp-remote-shell "") (tramp-tmpdir "/C$/Temp"))))

(add-to-list 'tramp-default-method-alist `(nil ,tramp-prefix-domain-regexp ,tramp-smb-method))

(add-to-list 'tramp-default-user-alist `(,(concat "\\`" tramp-smb-method "\\'") nil nil))

(eval-after-load 'tramp '(tramp-set-completion-function tramp-smb-method '((tramp-parse-netrc "~/.netrc"))))

(defsubst tramp-smb-file-name-p (filename) "\
Check if it's a filename for SMB servers." (let ((v (tramp-dissect-file-name filename))) (string= (tramp-file-name-method v) tramp-smb-method)))

(autoload 'tramp-smb-file-name-handler "tramp-smb" "\
Invoke the SMB related OPERATION.
First arg specifies the OPERATION, second arg is a list of arguments to
pass to the OPERATION.

\(fn OPERATION &rest ARGS)" nil nil)

(unless (memq system-type '(cygwin windows-nt)) (add-to-list 'tramp-foreign-file-name-handler-alist (cons 'tramp-smb-file-name-p 'tramp-smb-file-name-handler)))

;;;***

;;;### (autoloads (tramp-uuencode-region) "tramp-uu" "tramp-uu.el"
;;;;;;  (20352 65510))
;;; Generated autoloads from tramp-uu.el

(autoload 'tramp-uuencode-region "tramp-uu" "\
UU-encode the region between BEG and END.

\(fn BEG END)" nil nil)

;;;***

;;;### (autoloads nil "trampver" "trampver.el" (20352 65510))
;;; Generated autoloads from trampver.el

(defconst tramp-version "2.2.3-24.1" "\
This version of Tramp.")

(defconst tramp-bug-report-address "tramp-devel@gnu.org" "\
Email address to send bug reports to.")

;;;***

;;;### (autoloads nil nil ("ange-ftp.el" "browse-url.el" "dbus.el"
;;;;;;  "dig.el" "dns.el" "eudc-bob.el" "eudc-export.el" "eudc-hotlist.el"
;;;;;;  "eudc-vars.el" "eudc.el" "eudcb-bbdb.el" "eudcb-ldap.el"
;;;;;;  "eudcb-mab.el" "eudcb-ph.el" "gnutls.el" "goto-addr.el" "hmac-def.el"
;;;;;;  "hmac-md5.el" "imap.el" "ldap.el" "mairix.el" "net-utils.el"
;;;;;;  "netrc.el" "network-stream.el" "newst-backend.el" "newst-plainview.el"
;;;;;;  "newst-reader.el" "newst-ticker.el" "newst-treeview.el" "newsticker.el"
;;;;;;  "ntlm.el" "quickurl.el" "rcirc.el" "rcompile.el" "rlogin.el"
;;;;;;  "sasl-cram.el" "sasl-digest.el" "sasl-ntlm.el" "sasl.el"
;;;;;;  "secrets.el" "snmp-mode.el" "soap-client.el" "soap-inspect.el"
;;;;;;  "socks.el" "telnet.el" "tls.el" "tramp-compat.el" "webjump.el"
;;;;;;  "xesam.el" "zeroconf.el") (20436 19806 110032))

;;;***

(provide 'tramp-loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tramp-loaddefs.el ends here
