;;; ange-ftp.el --- transparent FTP support for GNU Emacs

;; Copyright (C) 1989-1996, 1998, 2000-2012  Free Software Foundation, Inc.

;; Author: Andy Norman (ange@hplb.hpl.hp.com)
;; Maintainer: FSF
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

;; This package attempts to make accessing files and directories using FTP
;; from within GNU Emacs as simple and transparent as possible.  A subset of
;; the common file-handling routines are extended to interact with FTP.

;; Usage:
;;
;; Some of the common GNU Emacs file-handling operations have been made
;; FTP-smart.  If one of these routines is given a filename that matches
;; '/user@host:name' then it will spawn an FTP process connecting to machine
;; 'host' as account 'user' and perform its operation on the file 'name'.
;;
;; For example: if find-file is given a filename of:
;;
;;   /ange@anorman:/tmp/notes
;;
;; then ange-ftp spawns an FTP process, connect to the host 'anorman' as
;; user 'ange', get the file '/tmp/notes' and pop up a buffer containing the
;; contents of that file as if it were on the local filesystem.  If ange-ftp
;; needs a password to connect then it reads one in the echo area.

;; Extended filename syntax:
;;
;; The default extended filename syntax is '/user@host:name', where the
;; 'user@' part may be omitted.  This syntax can be customized to a certain
;; extent by changing ange-ftp-name-format.  There are limitations.
;; The `host' part has an optional suffix `#port' which may be used to
;; specify a non-default port number for the connection.
;;
;; If the user part is omitted then ange-ftp generates a default user
;; instead whose value depends on the variable ange-ftp-default-user.

;; Passwords:
;;
;; A password is required for each host/user pair.  Ange-ftp reads passwords
;; as needed.  You can also specify a password with ange-ftp-set-passwd, or
;; in a *valid* ~/.netrc file.

;; Passwords for user "anonymous":
;;
;; Passwords for the user "anonymous" (or "ftp") are handled
;; specially.  The variable `ange-ftp-generate-anonymous-password'
;; controls what happens: if the value of this variable is a string,
;; then this is used as the password; if non-nil (the default), then
;; the value of `user-mail-address' is used; if nil then the user
;; is prompted for a password as normal.

;; "Dumb" UNIX hosts:
;;
;; The FTP servers on some UNIX machines have problems if the 'ls' command is
;; used.
;;
;; The routine ange-ftp-add-dumb-unix-host can be called to tell ange-ftp to
;; limit itself to the DIR command and not 'ls' for a given UNIX host.  Note
;; that this change will take effect for the current GNU Emacs session only.
;; See below for a discussion of non-UNIX hosts.  If a large number of
;; machines with similar hostnames have this problem then it is easier to set
;; the value of ange-ftp-dumb-unix-host-regexp in your .emacs file. ange-ftp
;; is unable to automatically recognize dumb unix hosts.

;; File name completion:
;;
;; Full file-name completion is supported on UNIX, VMS, CMS, and MTS hosts.
;; To do filename completion, ange-ftp needs a listing from the remote host.
;; Therefore, for very slow connections, it might not save any time.

;; FTP processes:
;;
;; When ange-ftp starts up an FTP process, it leaves it running for speed
;; purposes.  Some FTP servers will close the connection after a period of
;; time, but ange-ftp should be able to quietly reconnect the next time that
;; the process is needed.
;;
;; Killing the "*ftp user@host*" buffer also kills the ftp process.
;; This should not cause ange-ftp any grief.

;; Binary file transfers:
;;
;; By default ange-ftp transfers files in ASCII mode.  If a file being
;; transferred matches the value of ange-ftp-binary-file-name-regexp then
;; binary mode is used for that transfer.

;; Account passwords:
;;
;; Some FTP servers require an additional password which is sent by the
;; ACCOUNT command.  ange-ftp partially supports this by allowing the user to
;; specify an account password by either calling ange-ftp-set-account, or by
;; specifying an account token in the .netrc file.  If the account password
;; is set by either of these methods then ange-ftp will issue an ACCOUNT
;; command upon starting the FTP process.

;; Preloading:
;;
;; ange-ftp can be preloaded, but must be put in the site-init.el file and
;; not the site-load.el file in order for the documentation strings for the
;; functions being overloaded to be available.

;; Status reports:
;;
;; Most ange-ftp commands that talk to the FTP process output a status
;; message on what they are doing.  In addition, ange-ftp can take advantage
;; of the FTP client's HASH command to display the status of transferring
;; files and listing directories.  See the documentation for the variables
;; ange-ftp-{ascii,binary}-hash-mark-size, ange-ftp-send-hash and
;; ange-ftp-process-verbose for more details.

;; Gateways:
;;
;; Sometimes it is necessary for the FTP process to be run on a different
;; machine than the machine running GNU Emacs.  This can happen when the
;; local machine has restrictions on what hosts it can access.
;;
;; ange-ftp has support for running the ftp process on a different (gateway)
;; machine.  The way it works is as follows:
;;
;; 1) Set the variable 'ange-ftp-gateway-host' to the name of a machine
;;    that doesn't have the access restrictions.
;;
;; 2) Set the variable 'ange-ftp-local-host-regexp' to a regular expression
;;    that matches hosts that can be contacted from running a local ftp
;;    process, but fails to match hosts that can't be accessed locally.  For
;;    example:
;;
;;    "\\.hp\\.com$\\|^[^.]*$"
;;
;;    will match all hosts that are in the .hp.com domain, or don't have an
;;    explicit domain in their name, but will fail to match hosts with
;;    explicit domains or that are specified by their ip address.
;;
;; 3) Using NFS and symlinks, make sure that there is a shared directory with
;;    the *same* name between the local machine and the gateway machine.
;;    This directory is necessary for temporary files created by ange-ftp.
;;
;; 4) Set the variable 'ange-ftp-gateway-tmp-name-template' to the name of
;;    this directory plus an identifying filename prefix.  For example:
;;
;;    "/nfs/hplose/ange/ange-ftp"
;;
;;    where /nfs/hplose/ange is a directory that is shared between the
;;    gateway machine and the local machine.
;;
;; The simplest way of getting a ftp process running on the gateway machine
;; is if you can spawn a remote shell using either 'rsh' or 'remsh'.  If you
;; can't do this for some reason such as security then points 7 onwards will
;; discuss an alternative approach.
;;
;; 5) Set the variable ange-ftp-gateway-program to the name of the remote
;;    shell process such as 'remsh' or 'rsh' if the default isn't correct.
;;
;; 6) Set the variable ange-ftp-gateway-program-interactive to nil if it
;;    isn't already.  This tells ange-ftp that you are using a remote shell
;;    rather than logging in using telnet or rlogin.
;;
;; That should be all you need to allow ange-ftp to spawn a ftp process on
;; the gateway machine.  If you have to use telnet or rlogin to get to the
;; gateway machine then follow the instructions below.
;;
;; 7) Set the variable ange-ftp-gateway-program to the name of the program
;;    that lets you log onto the gateway machine.  This may be something like
;;    telnet or rlogin.
;;
;; 8) Set the variable ange-ftp-gateway-prompt-pattern to a regular
;;    expression that matches the prompt you get when you login to the
;;    gateway machine.  Be very specific here; this regexp must not match
;;    *anything* in your login banner except this prompt.
;;    shell-prompt-pattern is far too general as it appears to match some
;;    login banners from Sun machines.  For example:
;;
;;    "^$*$ *"
;;
;; 9) Set the variable ange-ftp-gateway-program-interactive to 't' to let
;;    ange-ftp know that it has to "hand-hold" the login to the gateway
;;    machine.
;;
;; 10) Set the variable ange-ftp-gateway-setup-term-command to a UNIX command
;;     that will put the pty connected to the gateway machine into a
;;     no-echoing mode, and will strip off carriage-returns from output from
;;     the gateway machine.  For example:
;;
;;     "stty -onlcr -echo"
;;
;;     will work on HP-UX machines, whereas:
;;
;;     "stty -echo nl"
;;
;;     appears to work for some Sun machines.
;;
;; That's all there is to it.

;; Smart gateways:
;;
;; If you have a "smart" ftp program that allows you to issue commands like
;; "USER foo@bar" which do nice proxy things, then look at the variables
;; ange-ftp-smart-gateway and ange-ftp-smart-gateway-port.
;;
;; Otherwise, if there is an alternate ftp program that implements proxy in
;; a transparent way (i.e. w/o specifying the proxy host), that will
;; connect you directly to the desired destination host:
;; Set ange-ftp-gateway-ftp-program-name to that program's name.
;; Set ange-ftp-local-host-regexp to a value as stated earlier on.
;; Leave ange-ftp-gateway-host set to nil.
;; Set ange-ftp-smart-gateway to t.

;; Tips for using ange-ftp:
;;
;; 1. For dired to work on a host which marks symlinks with a trailing @ in
;;    an ls -alF listing, you need to (setq dired-ls-F-marks-symlinks t).
;;    Most UNIX systems do not do this, but ULTRIX does. If you think that
;;    there is a chance you might connect to an ULTRIX machine (such as
;;    prep.ai.mit.edu), then set this variable accordingly.  This will have
;;    the side effect that dired will have problems with symlinks whose names
;;    end in an @.  If you get yourself into this situation then editing
;;    dired's ls-switches to remove "F", will temporarily fix things.
;;
;; 2. If you know that you are connecting to a certain non-UNIX machine
;;    frequently, and ange-ftp seems to be unable to guess its host-type,
;;    then setting the appropriate host-type regexp
;;    (ange-ftp-vms-host-regexp, ange-ftp-mts-host-regexp, or
;;    ange-ftp-cms-host-regexp) accordingly should help. Also, please report
;;    ange-ftp's inability to recognize the host-type as a bug.
;;
;; 3. For slow connections, you might get "listing unreadable" error
;;    messages, or get an empty buffer for a file that you know has something
;;    in it. The solution is to increase the value of ange-ftp-retry-time.
;;    Its default value is 5 which is plenty for reasonable connections.
;;    However, for some transatlantic connections I set this to 20.
;;
;; 4. Beware of compressing files on non-UNIX hosts. Ange-ftp will do it by
;;    copying the file to the local machine, compressing it there, and then
;;    sending it back. Binary file transfers between machines of different
;;    architectures can be a risky business. Test things out first on some
;;    test files. See "Bugs" below. Also, note that ange-ftp copies files by
;;    moving them through the local machine. Again, be careful when doing
;;    this with binary files on non-Unix machines.
;;
;; 5. Beware that dired over ftp will use your setting of dired-no-confirm
;;    (list of dired commands for which confirmation is not asked).  You
;;    might want to reconsider your setting of this variable, because you
;;    might want confirmation for more commands on remote direds than on
;;    local direds. For example, I strongly recommend that you not include
;;    compress and uncompress in this list. If there is enough demand it
;;    might be a good idea to have an alist ange-ftp-dired-no-confirm of
;;    pairs ( TYPE . LIST ), where TYPE is an operating system type and LIST
;;    is a list of commands for which confirmation would be suppressed.  Then
;;    remote dired listings would take their (buffer-local) value of
;;    dired-no-confirm from this alist. Who votes for this?

;; ---------------------------------------------------------------------
;; Non-UNIX support:
;; ---------------------------------------------------------------------

;; VMS support:
;;
;; Ange-ftp has full support for VMS hosts.  It
;; should be able to automatically recognize any VMS machine. However, if it
;; fails to do this, you can use the command ange-ftp-add-vms-host.  As well,
;; you can set the variable ange-ftp-vms-host-regexp in your .emacs file. We
;; would be grateful if you would report any failures to automatically
;; recognize a VMS host as a bug.
;;
;; Filename Syntax:
;;
;; For ease of *implementation*, the user enters the VMS filename syntax in a
;; UNIX-y way.  For example:
;;  PUB$:[ANONYMOUS.SDSCPUB.NEXT]README.TXT;1
;; would be entered as:
;;  /PUB$$:/ANONYMOUS/SDSCPUB/NEXT/README.TXT;1
;; i.e. to log in as anonymous on ymir.claremont.edu and grab the file:
;;  [.CSV.POLICY]RULES.MEM
;; you would type:
;;  C-x C-f /anonymous@ymir.claremont.edu:CSV/POLICY/RULES.MEM
;;
;; A valid VMS filename is of the form: FILE.TYPE;##
;; where FILE can be up to 39 characters
;;       TYPE can be up to 39 characters
;;       ## is a version number (an integer between 1 and 32,767)
;; Valid characters in FILE and TYPE are A-Z 0-9 _ - $
;; $ cannot begin a filename, and - cannot be used as the first or last
;; character.
;;
;; Tips:
;; 1. Although VMS is not case sensitive, EMACS running under UNIX is.
;;    Therefore, to access a VMS file, you must enter the filename with upper
;;    case letters.
;; 2. To access the latest version of file under VMS, you use the filename
;;    without the ";" and version number. You should always edit the latest
;;    version of a file. If you want to edit an earlier version, copy it to a
;;    new file first. This has nothing to do with ange-ftp, but is simply
;;    good VMS operating practice. Therefore, to edit FILE.TXT;3 (say 3 is
;;    latest version), do C-x C-f /ymir.claremont.edu:FILE.TXT. If you
;;    inadvertently do C-x C-f /ymir.claremont.edu:FILE.TXT;3, you will find
;;    that VMS will not allow you to save the file because it will refuse to
;;    overwrite FILE.TXT;3, but instead will want to create FILE.TXT;4, and
;;    attach the buffer to this file. To get out of this situation, M-x
;;    write-file /ymir.claremont.edu:FILE.TXT will attach the buffer to
;;    latest version of the file. For this reason, in dired "f"
;;    (dired-find-file), always loads the file sans version, whereas "v",
;;    (dired-view-file), always loads the explicit version number. The
;;    reasoning being that it reasonable to view old versions of a file, but
;;    not to edit them.
;; 3. EMACS has a feature in which it does environment variable substitution
;;    in filenames. Therefore, to enter a $ in a filename, you must quote it
;;    by typing $$.

;; MTS support:
;;
;; Ange-ftp has full support for hosts running
;; the Michigan terminal system.  It should be able to automatically
;; recognize any MTS machine. However, if it fails to do this, you can use
;; the command ange-ftp-add-mts-host.  As well, you can set the variable
;; ange-ftp-mts-host-regexp in your .emacs file. We would be grateful if you
;; would report any failures to automatically recognize a MTS host as a bug.
;;
;; Filename syntax:
;;
;; MTS filenames are entered in a UNIX-y way. For example, if your account
;; was YYYY, the file FILE in the account XXXX: on mtsg.ubc.ca would be
;; entered as
;;   /YYYY@mtsg.ubc.ca:/XXXX:/FILE
;; In other words, MTS accounts are treated as UNIX directories. Of course,
;; to access a file in another account, you must have access permission for
;; it.  If FILE were in your own account, then you could enter it in a
;; relative name fashion as
;;   /YYYY@mtsg.ubc.ca:FILE
;; MTS filenames can be up to 12 characters. Like UNIX, the structure of the
;; filename does not contain a TYPE (i.e. it can have as many "."'s as you
;; like.) MTS filenames are always in upper case, and hence be sure to enter
;; them as such! MTS is not case sensitive, but an EMACS running under UNIX
;; is.

;; CMS support:
;;
;; Ange-ftp has full support for hosts running
;; CMS.  It should be able to automatically recognize any CMS machine.
;; However, if it fails to do this, you can use the command
;; ange-ftp-add-cms-host.  As well, you can set the variable
;; ange-ftp-cms-host-regexp in your .emacs file. We would be grateful if you
;; would report any failures to automatically recognize a CMS host as a bug.
;;
;; Filename syntax:
;;
;; CMS filenames are entered in a UNIX-y way. In otherwords, minidisks are
;; treated as UNIX directories. For example to access the file READ.ME in
;; minidisk *.311 on cuvmb.cc.columbia.edu, you would enter
;;   /anonymous@cuvmb.cc.columbia.edu:/*.311/READ.ME
;; If *.301 is the default minidisk for this account, you could access
;; FOO.BAR on this minidisk as
;;   /anonymous@cuvmb.cc.columbia.edu:FOO.BAR
;; CMS filenames are of the form FILE.TYPE, where both FILE and TYPE can be
;; up to 8 characters. Again, beware that CMS filenames are always upper
;; case, and hence must be entered as such.
;;
;; Tips:
;; 1. CMS machines, with the exception of anonymous accounts, nearly always
;;    need an account password. To have ange-ftp send an account password,
;;    you can either include it in your .netrc file, or use
;;    ange-ftp-set-account.
;; 2. Ange-ftp cannot send "write passwords" for a minidisk. Hopefully, we
;;    can fix this.
;;
;; BS2000 support:
;;
;; Ange-ftp has full support for BS2000 hosts.  It should be able to
;; automatically recognize any BS2000 machine. However, if it fails to
;; do this, you can use the command ange-ftp-add-bs2000-host.  As well,
;; you can set the variable ange-ftp-bs2000-host-regexp in your .emacs
;; file. We would be grateful if you would report any failures to auto-
;; matically recognize a BS2000 host as a bug.
;;
;; If you want to access the POSIX subsystem on BS2000 you MUST use
;; command ange-ftp-add-bs2000-posix-host for that particular
;; hostname.  ange-ftp can't decide if you want to access the native
;; filesystem or the POSIX filesystem, so it accesses the native
;; filesystem by default.  And if you have an ASCII filesystem in
;; your BS2000 POSIX subsystem you must use
;; ange-ftp-binary-file-name-regexp to access its files.
;;
;; Filename Syntax:
;;
;; For ease of *implementation*, the user enters the BS2000 filename
;; syntax in a UNIX-y way.  For example:
;;  :PUB:$PUBLIC.ANONYMOUS.SDSCPUB.NEXT.README.TXT
;; would be entered as:
;;  /:PUB:/$$PUBLIC/ANONYMOUS.SDSCPUB.NEXT.README.TXT
;; You don't have to type pubset and account, if they have default values,
;; i.e. to log in as anonymous on bs2000.anywhere.com and grab the file
;; IMPORTANT.TEXT.ON.BS2000 on the default pubset X on userid PUBLIC
;; (there are only 8 characters in a valid username), you could type:
;;  C-x C-f /public@bs2000.anywhere.com:/IMPORTANT.TEXT.ON.BS2000
;; or
;;  C-x C-f /anonym@bs2000.anywhere.com:/:X:/$$PUBLIC/IMPORTANT.TEXT.ON.BS2000
;;
;; If X is not your default pubset, you could add it as 'subdirectory' (BS2000
;; has a flat architecture) with the command
;; (setq ange-ftp-bs2000-additional-pubsets '(":X:"))
;; and then you could type:
;;  C-x C-f /anonym@bs2000.anywhere.com:/:X:/IMPORTANT.TEXT.ON.BS2000
;;
;; Valid characters in an BS2000 filename are A-Z 0-9 $ # @ . -
;; If the first character in a filename is # or @, this is replaced with
;; ange-ftp-bs2000-special-prefix because names starting with # or @
;; are reserved for temporary files.
;; This is especially important for auto-save files.
;; Valid file generations are ending with ([+|-|*]0-9...) .
;; File generations are not supported yet!
;; A filename must at least contain one character (A-Z) and cannot be longer
;; than 41 characters.
;;
;; Tips:
;; 1. Although BS2000 is not case sensitive, EMACS running under UNIX is.
;;    Therefore, to access a BS2000 file, you must enter the filename with
;;    upper case letters.
;; 2. EMACS has a feature in which it does environment variable substitution
;;    in filenames. Therefore, to enter a $ in a filename, you must quote it
;;    by typing $$.
;; 3. BS2000 machines, with the exception of anonymous accounts, nearly
;;    always need an account password. To have ange-ftp send an account
;;    password, you can either include it in your .netrc file, or use
;;    ange-ftp-set-account.
;;
;; ------------------------------------------------------------------
;; Bugs:
;; ------------------------------------------------------------------
;;
;; 1. Umask problems:
;;    Be warned that files created by using ange-ftp will take account of the
;;    umask of the ftp daemon process rather than the umask of the creating
;;    user.  This is particularly important when logging in as the root user.
;;    The way that I tighten up the ftp daemon's umask under HP-UX is to make
;;    sure that the umask is changed to 027 before I spawn /etc/inetd.  I
;;    suspect that there is something similar on other systems.
;;
;; 2. Some combinations of FTP clients and servers break and get out of sync
;;    when asked to list a non-existent directory.  Some of the ai.mit.edu
;;    machines cause this problem for some FTP clients. Using
;;    ange-ftp-kill-ftp-process can restart the ftp process, which
;;    should get things back in sync.
;;
;; 3. Ange-ftp does not check to make sure that when creating a new file,
;;    you provide a valid filename for the remote operating system.
;;    If you do not, then the remote FTP server will most likely
;;    translate your filename in some way. This may cause ange-ftp to
;;    get confused about what exactly is the name of the file. The
;;    most common causes of this are using lower case filenames on systems
;;    which support only upper case, and using filenames which are too
;;    long.
;;
;; 4. Null (blank) passwords confuse both ange-ftp and some FTP daemons.
;;
;; 5. Ange-ftp likes to use pty's to talk to its FTP processes.  If GNU Emacs
;;    for some reason creates a FTP process that only talks via pipes then
;;    ange-ftp won't be getting the information it requires at the time that
;;    it wants it since pipes flush at different times to pty's.  One
;;    disgusting way around this problem is to talk to the FTP process via
;;    rlogin which does the 'right' things with pty's.
;;
;; 6. For CMS support, we send too many cd's. Since cd's are cheap, I haven't
;;    worried about this too much. Eventually, we should have some caching
;;    of the current minidisk.
;;
;; 7. Some CMS machines do not assign a default minidisk when you ftp them as
;;    anonymous. It is then necessary to guess a valid minidisk name, and cd
;;    to it. This is (understandably) beyond ange-ftp.
;;
;; 8. Remote to remote copying of files on non-Unix machines can be risky.
;;    Depending on the variable ange-ftp-binary-file-name-regexp, ange-ftp
;;    will use binary mode for the copy. Between systems of different
;;    architecture, this still may not be enough to guarantee the integrity
;;    of binary files. Binary file transfers from VMS machines are
;;    particularly problematical. Should ange-ftp-binary-file-name-regexp be
;;    an alist of OS type, regexp pairs?
;;
;; 9. The code to do compression of files over ftp is not as careful as it
;;    should be. It deletes the old remote version of the file, before
;;    actually checking if the local to remote transfer of the compressed
;;    file succeeds. Of course to delete the original version of the file
;;    after transferring the compressed version back is also dangerous,
;;    because some OS's have severe restrictions on the length of filenames,
;;    and when the compressed version is copied back the "-Z" or ".Z" may be
;;    truncated. Then, ange-ftp would delete the only remaining version of
;;    the file.  Maybe ange-ftp should make backups when it compresses files
;;    (of course, the backup "~" could also be truncated off, sigh...).
;;    Suggestions?
;;
;; 10. If a dir listing is attempted for an empty directory on (at least
;;     some) VMS hosts, an ftp error is given. This is really an ftp bug, and
;;     I don't know how to get ange-ftp work to around it.
;;
;; 11. Bombs on filenames that start with a space. Deals well with filenames
;;     containing spaces, but beware that the remote ftpd may not like them
;;     much.
;;
;; 12. The dired support for non-Unix-like systems does not currently work.
;;     It needs to be reimplemented by modifying the parse-...-listing
;;	functions to convert the directory listing to ls -l format.
;;
;; 13. The famous @ bug. As mentioned above in TIPS, ULTRIX marks symlinks
;;     with a trailing @ in a ls -alF listing. In order to account for this
;;     ange-ftp looks to chop trailing @'s off of symlink names when it is
;;     parsing a listing with the F switch. This will cause ange-ftp to
;;     incorrectly get the name of a symlink on a non-ULTRIX host if its name
;;     ends in an @. ange-ftp will correct itself if you take F out of the
;;     dired ls switches (C-u s will allow you to edit the switches). The
;;     dired buffer will be automatically reverted, which will allow ange-ftp
;;     to fix its files hashtable.  A cookie to anyone who can think of a
;;     fast, sure-fire way to recognize ULTRIX over ftp.

;; If you find any bugs or problems with this package, PLEASE either e-mail
;; the above author, or send a message to the ange-ftp-lovers mailing list
;; below.  Ideas and constructive comments are especially welcome.

;; ange-ftp-lovers:
;;
;; ange-ftp has its own mailing list modestly called ange-ftp-lovers.  All
;; users of ange-ftp are welcome to subscribe (see below) and to discuss
;; aspects of ange-ftp.  New versions of ange-ftp are posted periodically to
;; the mailing list.

;; To [un]subscribe to ange-ftp-lovers, or to report mailer problems with the
;; list, please mail one of the following addresses:
;;
;;     ange-ftp-lovers-request@hplb.hpl.hp.com
;;
;; Please don't forget the -request part.
;;
;; For mail to be posted directly to ange-ftp-lovers, send to one of the
;; following addresses:
;;
;;     ange-ftp-lovers@hplb.hpl.hp.com
;;
;; Alternatively, there is a mailing list that only gets announcements of new
;; ange-ftp releases.  This is called ange-ftp-lovers-announce, and can be
;; subscribed to by e-mailing to the -request address as above.  Please make
;; it clear in the request which mailing list you wish to join.

;; -----------------------------------------------------------
;; Technical information on this package:
;; -----------------------------------------------------------

;; ange-ftp works by putting a handler on file-name-handler-alist
;; which is called by many primitives, and a few non-primitives,
;; whenever they see a file name of the appropriate sort.

;; Checklist for adding non-UNIX support for TYPE
;;
;; The following functions may need TYPE versions:
;; (not all functions will be needed for every OS)
;;
;; ange-ftp-fix-name-for-TYPE
;; ange-ftp-fix-dir-name-for-TYPE
;; ange-ftp-TYPE-host
;; ange-ftp-TYPE-add-host
;; ange-ftp-parse-TYPE-listing
;; ange-ftp-TYPE-delete-file-entry
;; ange-ftp-TYPE-add-file-entry
;; ange-ftp-TYPE-file-name-as-directory
;; ange-ftp-TYPE-make-compressed-filename
;; ange-ftp-TYPE-file-name-sans-versions
;;
;; Variables:
;;
;; ange-ftp-TYPE-host-regexp
;; May need to add TYPE to ange-ftp-dumb-host-types
;;
;; Check the following functions for OS dependent coding:
;;
;; ange-ftp-host-type
;; ange-ftp-guess-host-type
;; ange-ftp-allow-child-lookup

;; Host type conventions:
;;
;; The function ange-ftp-host-type and the variable ange-ftp-dired-host-type
;; (mostly) follow the following conventions for remote host types.  At
;; least, I think that future code should try to follow these conventions,
;; and the current code should eventually be made compliant.
;;
;; nil = local host type, whatever that is (probably unix).
;;       Think nil as in "not a remote host". This value is used by
;;       ange-ftp-dired-host-type for local buffers.
;;
;; t = a remote host of unknown type. Think t as in true, it's remote.
;;     Currently, `unix' is used as the default remote host type.
;;     Maybe we should use t.
;;
;; TYPE = a remote host of TYPE type.
;;
;; TYPE:LIST = a remote host of TYPE type, using a specialized ftp listing
;;             program called list. This is currently only used for Unix
;;             dl (descriptive listings), when ange-ftp-dired-host-type
;;             is set to `unix:dl'.

;; Bug report codes:
;;
;; Because of their naive faith in this code, there are certain situations
;; which the writers of this program believe could never happen. However,
;; being realists they have put calls to `error' in the program at these
;; points. These errors provide a code, which is an integer, greater than 1.
;; To aid debugging.  the error codes, and the functions in which they reside
;; are listed below.
;;
;; 1: See ange-ftp-ls
;;

;; -----------------------------------------------------------
;; Hall of fame:
;; -----------------------------------------------------------
;;
;; Thanks to Roland McGrath for improving the filename syntax handling,
;; for suggesting many enhancements and for numerous cleanups to the code.
;;
;; Thanks to Jamie Zawinski for bugfixes and for ideas such as gateways.
;;
;; Thanks to Ken Laprade for improved .netrc parsing, password reading, and
;; dired / shell auto-loading.
;;
;; Thanks to Sebastian Kremer for dired support and for many ideas and
;; bugfixes.
;;
;; Thanks to Joe Wells for bugfixes, the original non-UNIX system support,
;; VOS support, and hostname completion.
;;
;; Thanks to Nakagawa Takayuki for many good ideas, filename-completion, help
;; with file-name expansion, efficiency worries, stylistic concerns and many
;; bugfixes.
;;
;; Thanks to Sandy Rutherford who re-wrote most of ange-ftp to support VMS,
;; MTS, CMS and UNIX-dls.  Sandy also added dired-support for non-UNIX OS and
;; auto-recognition of the host type.
;;
;; Thanks to Dave Smith who wrote the info file for ange-ftp.
;;
;; Finally, thanks to Keith Waclena, Mark D. Baushke, Terence Kelleher, Ping
;; Zhou, Edward Vielmetti, Jack Repenning, Mike Balenger, Todd Kaufmann,
;; Kjetil Svarstad, Tom Wurgler, Linus Tolke, Niko Makila, Carl Edman, Bill
;; Trost, Dave Brennan, Dan Jacobson, Andy Scott, Steve Anderson, Sanjay
;; Mathur, the folks on the ange-ftp-lovers mailing list and many others
;; whose names I've forgotten who have helped to debug and fix problems with
;; ange-ftp.el.

;;; Code:

(require 'comint)

;;;; ------------------------------------------------------------
;;;; User customization variables.
;;;; ------------------------------------------------------------

(defgroup ange-ftp nil
  "Accessing remote files and directories using FTP
   made as simple and transparent as possible."
  :group 'files
  :group 'comm
  :prefix "ange-ftp-")

(defcustom ange-ftp-name-format
  '("\\`/\\(\\([^/:]*\\)@\\)?\\([^@/:]*[^@/:.]\\):\\(.*\\)" . (3 2 4))
  "Format of a fully expanded remote file name.

This is a list of the form \(REGEXP HOST USER NAME\),
where REGEXP is a regular expression matching
the full remote name, and HOST, USER, and NAME are the numbers of
parenthesized expressions in REGEXP for the components (in that order)."
  :group 'ange-ftp
  :type '(list (regexp  :tag "Name regexp")
	       (integer :tag "Host group")
	       (integer :tag "User group")
	       (integer :tag "Name group")))

;; ange-ftp-multi-skip-msgs should only match ###-, where ### is one of
;; the number codes corresponding to ange-ftp-good-msgs or ange-ftp-fatal-msgs.
;; Otherwise, ange-ftp will go into multi-skip mode, and never come out.

(defvar ange-ftp-multi-msgs
  "^150-\\|^220-\\|^230-\\|^226\\|^25.-\\|^221-\\|^200-\\|^331-\\|^4[25]1-\\|^530-"
  "*Regular expression matching the start of a multiline FTP reply.")

(defvar ange-ftp-good-msgs
  "^220 \\|^230 \\|^226 \\|^25. \\|^221 \\|^200 \\|^[Hh]ash mark"
  "*Regular expression matching FTP \"success\" messages.")

;; CMS and the odd VMS machine say 200 Port rather than 200 PORT.
;; Also CMS machines use a multiline 550- reply to say that you
;; don't have write permission. ange-ftp gets into multi-line skip
;; mode and hangs. Have it ignore 550- instead. It will then barf
;; when it gets the 550 line, as it should.

;; RFC2228 "FTP Security Extensions" defines extensions to the FTP
;; protocol which involve the client requesting particular
;; authentication methods (typically) at connection establishment. Non
;; security-aware FTP servers should respond to this with a 500 code,
;; which we ignore.
(defcustom ange-ftp-skip-msgs
  (concat "^200 \\(PORT\\|Port\\) \\|^331 \\|^150 \\|^350 \\|^[0-9]+ bytes \\|"
	  "^Connected \\|^$\\|^Remote system\\|^Using\\|^ \\|Password:\\|"
	  "^Data connection \\|"
	  "^local:\\|^Trying\\|^125 \\|^550-\\|^221 .*oodbye\\|"
          "^500 .*AUTH\\|^KERBEROS\\|"
          "^504 Unknown security mechanism\\|"
	  "^530 Please login with USER and PASS\\|" ; non kerberized vsFTPd
	  "^534 Kerberos Authentication not enabled\\|"
	  "^22[789] .*[Pp]assive\\|^200 EPRT\\|^500 .*EPRT")
  "Regular expression matching FTP messages that can be ignored."
  :group 'ange-ftp
  :type 'regexp)

(defcustom ange-ftp-fatal-msgs
  (concat "^ftp: \\|^Not connected\\|^530 \\|^4[25]1 \\|rcmd: \\|"
	  "^No control connection\\|unknown host\\|^lost connection")
  "Regular expression matching FTP messages that indicate serious errors.

These mean that the FTP process should be (or already has been) killed."
  :group 'ange-ftp
  :type 'regexp)

(defcustom ange-ftp-potential-error-msgs
  ;; On Mac OS X we sometimes get things like:
  ;;
  ;;     ftp> open ftp.nluug.nl
  ;;     Trying 2001:610:1:80aa:192:87:102:36...
  ;;     ftp: connect to address 2001:610:1:80aa:192:87:102:36: No route to host
  ;;     Trying 192.87.102.36...
  ;;     Connected to ftp.nluug.nl.
  "^ftp: connect to address .*: No route to host"
  "Regular expression matching FTP messages that can indicate serious errors.
These mean that something went wrong, but they may be followed by more
messages indicating that the error was somehow corrected."
  :group 'ange-ftp
  :type 'regexp)

(defcustom ange-ftp-gateway-fatal-msgs
  "No route to host\\|Connection closed\\|No such host\\|Login incorrect"
  "Regular expression matching login failure messages from rlogin/telnet."
  :group 'ange-ftp
  :type 'regexp)

(defcustom ange-ftp-xfer-size-msgs
  "^150 .* connection for .* (\\([0-9]+\\) bytes)"
  "Regular expression used to determine the number of bytes in a FTP transfer."
  :group 'ange-ftp
  :type 'regexp)

(defcustom ange-ftp-tmp-name-template
  (expand-file-name "ange-ftp" temporary-file-directory)
  "Template used to create temporary files."
  :group 'ange-ftp
  :type 'directory)

(defcustom ange-ftp-gateway-tmp-name-template "/tmp/ange-ftp"
  "Template used to create temporary files when FTP-ing through a gateway.

Files starting with this prefix need to be accessible from BOTH the local
machine and the gateway machine, and need to have the SAME name on both
machines, that is, /tmp is probably NOT what you want, since that is rarely
cross-mounted."
  :group 'ange-ftp
  :type 'directory)

(defcustom ange-ftp-netrc-filename "~/.netrc"
  "File in .netrc format to search for passwords."
  :group 'ange-ftp
  :type 'file)

(defcustom ange-ftp-disable-netrc-security-check (eq system-type 'windows-nt)
  "If non-nil avoid checking permissions on the .netrc file."
  :group 'ange-ftp
  :type 'boolean)

(defcustom ange-ftp-default-user nil
  "User name to use when none is specified in a file name.

If non-nil but not a string, you are prompted for the name.
If nil, the value of `ange-ftp-netrc-default-user' is used.
If that is nil too, then your login name is used.

Once a connection to a given host has been initiated, the user name
and password information for that host are cached and re-used by
ange-ftp.  Use \\[ange-ftp-set-user] to change the cached values,
since setting `ange-ftp-default-user' directly does not affect
the cached information."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string
		 (other :tag "Prompt" t)))

(defcustom ange-ftp-netrc-default-user nil
  "Alternate default user name to use when none is specified.

This variable is set from the `default' command in your `.netrc' file,
if there is one."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom ange-ftp-default-password nil
  "Password to use when the user name equals `ange-ftp-default-user'."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom ange-ftp-default-account nil
  "Account to use when the user name equals `ange-ftp-default-user'."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom ange-ftp-netrc-default-password nil
  "Password to use when the user name equals `ange-ftp-netrc-default-user'."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom ange-ftp-netrc-default-account nil
  "Account to use when the user name equals `ange-ftp-netrc-default-user'."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom ange-ftp-generate-anonymous-password t
  "If t, use value of `user-mail-address' as password for anonymous FTP.

If a string, then use that string as the password.
If nil, prompt the user for a password."
  :group 'ange-ftp
  :type '(choice (const :tag "Prompt" nil)
		 string
		 (other :tag "User address" t)))

(defcustom ange-ftp-dumb-unix-host-regexp nil
  "If non-nil, regexp matching hosts on which `dir' command lists directory."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom ange-ftp-binary-file-name-regexp ""
  "If a file matches this regexp then it is transferred in binary mode."
  :group 'ange-ftp
  :type 'regexp
  :version "24.1")

(defcustom ange-ftp-gateway-host nil
  "Name of host to use as gateway machine when local FTP isn't possible."
  :group 'ange-ftp
  :type '(choice (const :tag "Default" nil)
		 string))

(defcustom ange-ftp-local-host-regexp ".*"
  "Regexp selecting hosts which can be reached directly with FTP.

For other hosts the FTP process is started on `ange-ftp-gateway-host'
instead, and/or reached via `ange-ftp-gateway-ftp-program-name'."
  :group 'ange-ftp
  :type 'regexp)

(defcustom ange-ftp-gateway-program-interactive nil
  "If non-nil then the gateway program should give a shell prompt.

Both telnet and rlogin do something like this."
  :group 'ange-ftp
  :type 'boolean)

(defcustom ange-ftp-gateway-program remote-shell-program
  "Name of program to spawn a shell on the gateway machine.

Valid candidates are rsh (remsh on some systems), telnet and rlogin.
See also the gateway variable above."
  :group 'ange-ftp
  :type '(choice (const "rsh")
		 (const "telnet")
		 (const "rlogin")
		 string))

(defcustom ange-ftp-gateway-prompt-pattern "^[^#$%>;\n]*[#$%>;] *"
  "Regexp matching prompt after complete login sequence on gateway machine.

A match for this means the shell is now awaiting input.  Make this regexp as
strict as possible; it shouldn't match *anything* at all except the user's
initial prompt.  The above string will fail under most SUN-3's since it
matches the login banner."
  :group 'ange-ftp
  :type 'regexp)

(defvar ange-ftp-gateway-setup-term-command
  (if (eq system-type 'hpux)
      "stty -onlcr -echo\n"
    "stty -echo nl\n")
  "*Set up terminal after logging in to the gateway machine.
This command should stop the terminal from echoing each command, and
arrange to strip out trailing ^M characters.")

(defcustom ange-ftp-smart-gateway nil
  "Non-nil says the FTP gateway (proxy) or gateway FTP program is smart.

Don't bother telnetting, etc., already connected to desired host transparently,
or just issue a user@host command in case `ange-ftp-gateway-host' is non-nil.
See also `ange-ftp-smart-gateway-port'."
  :group 'ange-ftp
  :type 'boolean)

(defcustom ange-ftp-smart-gateway-port "21"
  "Port on gateway machine to use when smart gateway is in operation."
  :group 'ange-ftp
  :type 'string)

(defcustom ange-ftp-send-hash t
  "If non-nil, send the HASH command to the FTP client."
  :group 'ange-ftp
  :type 'boolean)

(defcustom ange-ftp-binary-hash-mark-size nil
  "Default size, in bytes, between hash-marks when transferring a binary file.
If nil, this variable will be locally overridden if the FTP client outputs a
suitable response to the HASH command.  If non-nil, this value takes
precedence over the local value."
  :group 'ange-ftp
  :type '(choice (const :tag "Overridden" nil)
		 integer))

(defcustom ange-ftp-ascii-hash-mark-size 1024
  "Default size, in bytes, between hash-marks when transferring an ASCII file.
This variable is buffer-local and will be locally overridden if the FTP client
outputs a suitable response to the HASH command."
  :group 'ange-ftp
  :type 'integer)

(defcustom ange-ftp-process-verbose t
  "If non-nil then be chatty about interaction with the FTP process."
  :group 'ange-ftp
  :type 'boolean)

(defcustom ange-ftp-ftp-program-name "ftp"
  "Name of FTP program to run."
  :group 'ange-ftp
  :type 'string)

(defcustom ange-ftp-gateway-ftp-program-name "ftp"
  "Name of FTP program to run when accessing non-local hosts.

Some AT&T folks claim to use something called `pftp' here."
  :group 'ange-ftp
  :type 'string)

(defcustom ange-ftp-ftp-program-args '("-i" "-n" "-g" "-v")
  "A list of arguments passed to the FTP program when started."
  :group 'ange-ftp
  :type '(repeat string))

(defcustom ange-ftp-nslookup-program nil
  "If non-nil, this is a string naming the nslookup program."
  :group 'ange-ftp
  :type '(choice (const :tag "None" nil)
		 string))

(defcustom ange-ftp-make-backup-files ()
  "Non-nil means make backup files for \"magic\" remote files."
  :group 'ange-ftp
  :type 'boolean)

(defcustom ange-ftp-retry-time 5
  "Number of seconds to wait before retry if file or listing doesn't arrive.
This might need to be increased for very slow connections."
  :group 'ange-ftp
  :type 'integer)

(defcustom ange-ftp-auto-save 0
  "If 1, allow ange-ftp files to be auto-saved.
If 0, inhibit auto-saving of ange-ftp files.
Don't use any other value."
  :group 'ange-ftp
  :type '(choice (const :tag "Suppress" 0)
		 (const :tag "Allow" 1)))

(defcustom ange-ftp-try-passive-mode nil
  "If t, try to use passive mode in FTP, if the client program supports it."
  :group 'ange-ftp
  :type 'boolean
  :version "21.1")

(defcustom ange-ftp-passive-host-alist nil
  "Alist of FTP servers that need \"passive\" mode.
Each element is of the form (HOSTNAME . SETTING).
HOSTNAME is a regular expression to match the FTP server host name(s).
SETTING is \"on\" to turn passive mode on, \"off\" to turn it off,
or nil meaning don't change it."
  :group 'ange-ftp
  :type '(repeat (cons regexp (choice (const :tag "On" "on")
				      (const :tag "Off" "off")
				      (const :tag "Don't change" nil))))
  :version "22.1")

;;;; ------------------------------------------------------------
;;;; Hash table support.
;;;; ------------------------------------------------------------

(require 'backquote)

(defun ange-ftp-hash-entry-exists-p (key tbl)
  "Return whether there is an association for KEY in table TBL."
  (and tbl (not (eq (gethash key tbl 'unknown) 'unknown))))

(defun ange-ftp-hash-table-keys (tbl)
  "Return a sorted list of all the active keys in table TBL, as strings."
  ;; (let ((keys nil))
  ;;   (maphash (lambda (k v) (push k keys)) tbl)
  ;;   (sort keys 'string-lessp))
  (sort (all-completions "" tbl) 'string-lessp))

;;;; ------------------------------------------------------------
;;;; Internal variables.
;;;; ------------------------------------------------------------

(defvar ange-ftp-data-buffer-name " *ftp data*"
  "Buffer name to hold directory listing data received from FTP process.")

(defvar ange-ftp-netrc-modtime nil
  "Last modified time of the netrc file from file-attributes.")

(defvar ange-ftp-user-hashtable (make-hash-table :test 'equal)
  "Hash table holding associations between HOST, USER pairs.")

(defvar ange-ftp-passwd-hashtable (make-hash-table :test 'equal)
  "Mapping between a HOST, USER pair and a PASSWORD for them.
All HOST values should be in lower case.")

(defvar ange-ftp-account-hashtable (make-hash-table :test 'equal)
  "Mapping between a HOST, USER pair and an ACCOUNT password for them.")

(defvar ange-ftp-files-hashtable (make-hash-table :test 'equal :size 97)
  "Hash table for storing directories and their respective files.")

(defvar ange-ftp-inodes-hashtable (make-hash-table :test 'equal :size 97)
  "Hash table for storing file names and their \"inode numbers\".")

(defvar ange-ftp-next-inode-number 1
  "Next \"inode number\" value.  We give each file name a unique number.")

(defvar ange-ftp-ls-cache-lsargs nil
  "Last set of args used by `ange-ftp-ls'.")

(defvar ange-ftp-ls-cache-file nil
  "Last file passed to `ange-ftp-ls'.")

(defvar ange-ftp-ls-cache-res nil
  "Last result returned from `ange-ftp-ls'.")

(defconst ange-ftp-expand-dir-hashtable (make-hash-table :test 'equal))

(defconst ange-ftp-expand-dir-regexp "^5.0 \\([^: ]+\\):")

;; These are local variables in each FTP process buffer.
(defvar ange-ftp-hash-mark-unit nil)
(defvar ange-ftp-hash-mark-count nil)
(defvar ange-ftp-xfer-size nil)
(defvar ange-ftp-process-string nil)
(defvar ange-ftp-process-result-line nil)
(defvar ange-ftp-pending-error-line nil)
(defvar ange-ftp-process-busy nil)
(defvar ange-ftp-process-result nil)
(defvar ange-ftp-process-multi-skip nil)
(defvar ange-ftp-process-msg nil)
(defvar ange-ftp-process-continue nil)
(defvar ange-ftp-last-percent nil)

;; These variables are bound by one function and examined by another.
;; Leave them void globally for error checking.
(defvar ange-ftp-this-file)
(defvar ange-ftp-this-dir)
(defvar ange-ftp-this-user)
(defvar ange-ftp-this-host)
(defvar ange-ftp-this-msg)
(defvar ange-ftp-completion-ignored-pattern)
(defvar ange-ftp-trample-marker)

;; New error symbols.
(put 'ftp-error 'error-conditions '(ftp-error file-error error))
;; (put 'ftp-error 'error-message "FTP error")

;;; ------------------------------------------------------------
;;; Enhanced message support.
;;; ------------------------------------------------------------

(defun ange-ftp-message (fmt &rest args)
  "Display message in echo area, but indicate if truncated.
Args are as in `message': a format string, plus arguments to be formatted."
  (let ((msg (apply 'format fmt args))
	(max (window-width (minibuffer-window))))
    (if noninteractive
	msg
      (if (>= (length msg) max)
	  ;; Take just the last MAX - 3 chars of the string.
	  (setq msg (concat "> " (substring msg (- 3 max)))))
      (message "%s" msg))))

(defun ange-ftp-abbreviate-filename (file &optional new)
  "Abbreviate the file name FILE relative to the `default-directory'.
If the optional parameter NEW is given and the non-directory parts match,
only return the directory part of FILE."
  (save-match-data
    (if (and default-directory
	     (string-match (concat "\\`"
				   (regexp-quote default-directory)
				   ".") file))
	(setq file (substring file (1- (match-end 0)))))
    (if (and new
	     (string-equal (file-name-nondirectory file)
			   (file-name-nondirectory new)))
	(setq file (file-name-directory file)))
    (or file "./")))

;;;; ------------------------------------------------------------
;;;; User / Host mapping support.
;;;; ------------------------------------------------------------

(defun ange-ftp-set-user (host user)
  "For a given HOST, set or change the default USER."
  (interactive "sHost: \nsUser: ")
  (puthash host user ange-ftp-user-hashtable))

(defun ange-ftp-get-user (host)
  "Given a HOST, return the default user."
  (ange-ftp-parse-netrc)
  (let ((user (gethash host ange-ftp-user-hashtable)))
    (or user
	(prog1
	    (setq user
		  (cond ((stringp ange-ftp-default-user)
			 ;; We have a default name.  Use it.
			 ange-ftp-default-user)
			(ange-ftp-default-user
			 ;; Ask the user.
			 (let ((enable-recursive-minibuffers t))
			   (read-string (format "User for %s: " host)
					(user-login-name))))
			(ange-ftp-netrc-default-user)
			;; Default to the user's login name.
			(t
			 (user-login-name))))
	  (ange-ftp-set-user host user)))))

;;;; ------------------------------------------------------------
;;;; Password support.
;;;; ------------------------------------------------------------

(defmacro ange-ftp-generate-passwd-key (host user)
  `(and (stringp ,host) (stringp ,user) (concat (downcase ,host) "/" ,user)))

(defmacro ange-ftp-lookup-passwd (host user)
  `(gethash (ange-ftp-generate-passwd-key ,host ,user)
	    ange-ftp-passwd-hashtable))

(defun ange-ftp-set-passwd (host user password)
  "For a given HOST and USER, set or change the associated PASSWORD."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (read-passwd "Password: ")))
  (puthash (ange-ftp-generate-passwd-key host user)
	   password ange-ftp-passwd-hashtable))

(defun ange-ftp-get-host-with-passwd (user)
  "Given a USER, return a host we know the password for."
  (ange-ftp-parse-netrc)
  (catch 'found-one
    (maphash
     (lambda (host val)
       (if (ange-ftp-lookup-passwd host user) (throw 'found-one host)))
     ange-ftp-user-hashtable)
    (save-match-data
      (maphash
       (lambda (key value)
	 (if (string-match "\\`[^/]*\\(/\\).*\\'" key)
	     (let ((host (substring key 0 (match-beginning 1))))
	       (if (and (string-equal user (substring key (match-end 1)))
			value)
		   (throw 'found-one host)))))
       ange-ftp-passwd-hashtable))
    nil))

(defun ange-ftp-get-passwd (host user)
  "Return the password for specified HOST and USER, asking user if necessary."
  (ange-ftp-parse-netrc)

  ;; look up password in the hash table first; user might have overridden the
  ;; defaults.
  (cond ((ange-ftp-lookup-passwd host user))

	;; See if default user and password set.
	((and (stringp ange-ftp-default-user)
	      ange-ftp-default-password
	      (string-equal user ange-ftp-default-user))
	 ange-ftp-default-password)

	;; See if default user and password set from .netrc file.
	((and (stringp ange-ftp-netrc-default-user)
	      ange-ftp-netrc-default-password
	      (string-equal user ange-ftp-netrc-default-user))
	 ange-ftp-netrc-default-password)

	;; anonymous ftp password is handled specially since there is an
	;; unwritten rule about how that is used on the Internet.
	((and (or (string-equal user "anonymous")
		  (string-equal user "ftp"))
	      ange-ftp-generate-anonymous-password)
	 (if (stringp ange-ftp-generate-anonymous-password)
	     ange-ftp-generate-anonymous-password
	   user-mail-address))

	;; see if same user has logged in to other hosts; if so then prompt
	;; with the password that was used there.
	(t
	 (let* ((other (ange-ftp-get-host-with-passwd user))
		(passwd (if other

			    ;; found another machine with the same user.
			    ;; Try that account.
			    (read-passwd
			     (format "passwd for %s@%s (default same as %s@%s): "
				     user host user other)
			     nil
			     (ange-ftp-lookup-passwd other user))

			  ;; I give up.  Ask the user for the password.
			  (read-passwd
			   (format "Password for %s@%s: " user host)))))
	   (ange-ftp-set-passwd host user passwd)
	   passwd))))

;;;; ------------------------------------------------------------
;;;; Account support
;;;; ------------------------------------------------------------

;; Account passwords must be either specified in the .netrc file, or set
;; manually by calling ange-ftp-set-account.  For the moment, ange-ftp doesn't
;; check to see whether the FTP process is actually prompting for an account
;; password.

(defun ange-ftp-set-account (host user account)
  "For a given HOST and USER, set or change the associated ACCOUNT password."
  (interactive (list (read-string "Host: ")
		     (read-string "User: ")
		     (read-passwd "Account password: ")))
  (puthash (ange-ftp-generate-passwd-key host user)
	   account ange-ftp-account-hashtable))

(defun ange-ftp-get-account (host user)
  "Given a HOST and USER, return the FTP account."
  (ange-ftp-parse-netrc)
  (or (gethash (ange-ftp-generate-passwd-key host user)
	       ange-ftp-account-hashtable)
      (and (stringp ange-ftp-default-user)
	   (string-equal user ange-ftp-default-user)
	   ange-ftp-default-account)
      (and (stringp ange-ftp-netrc-default-user)
	   (string-equal user ange-ftp-netrc-default-user)
	   ange-ftp-netrc-default-account)))

;;;; ------------------------------------------------------------
;;;; ~/.netrc support
;;;; ------------------------------------------------------------

(defun ange-ftp-chase-symlinks (file)
  "Return the filename that FILE references, following all symbolic links."
  (let (temp)
    (while (setq temp (ange-ftp-real-file-symlink-p file))
      (setq file
	    (if (file-name-absolute-p temp)
		temp
	      ;; Wouldn't `expand-file-name' be better than `concat' ?
	      ;; It would fail when `a/b/..' != `a', tho.  --Stef
	      (concat (file-name-directory file) temp)))))
  file)

;; Move along current line looking for the value of the TOKEN.
;; Valid separators between TOKEN and its value are commas and
;; whitespace.  Second arg LIMIT is a limit for the search.

(defun ange-ftp-parse-netrc-token (token limit)
  (if (search-forward token limit t)
      (let (beg)
	(skip-chars-forward ", \t\r\n" limit)
	(if (eq (following-char) ?\")	;quoted token value
	    (progn (forward-char 1)
		   (setq beg (point))
		   (skip-chars-forward "^\"" limit)
		   (forward-char 1)
		   (buffer-substring beg (1- (point))))
	  (setq beg (point))
	  (skip-chars-forward "^, \t\r\n" limit)
	  (buffer-substring beg (point))))))

;; Extract the values for the tokens `machine', `login',
;; `password' and `account' in the current buffer.  If successful,
;; record the information found.

(defun ange-ftp-parse-netrc-group ()
  (let ((start (point))
	(end (save-excursion
	       (if (looking-at "machine\\>")
		   ;; Skip `machine' and the machine name that follows.
		   (progn
		     (skip-chars-forward "^ \t\r\n")
		     (skip-chars-forward " \t\r\n")
		     (skip-chars-forward "^ \t\r\n"))
		 ;; Skip `default'.
		 (skip-chars-forward "^ \t\r\n"))
	       ;; Find start of the next `machine' or `default'
	       ;; or the end of the buffer.
	       (if (re-search-forward "machine\\>\\|default\\>" nil t)
		   (match-beginning 0)
		 (point-max))))
	machine login password account)
    (setq machine  (ange-ftp-parse-netrc-token "machine"  end)
	  login    (ange-ftp-parse-netrc-token "login"    end)
	  password (ange-ftp-parse-netrc-token "password" end)
	  account  (ange-ftp-parse-netrc-token "account"  end))
    (if (and machine login)
	;; found a `machine` token.
	(progn
	  (ange-ftp-set-user machine login)
	  (ange-ftp-set-passwd machine login password)
	  (and account
	       (ange-ftp-set-account machine login account)))
      (goto-char start)
      (if (search-forward "default" end t)
	  ;; found a `default' token
	  (progn
	    (setq login    (ange-ftp-parse-netrc-token "login"    end)
		  password (ange-ftp-parse-netrc-token "password" end)
		  account  (ange-ftp-parse-netrc-token "account"  end))
	    (and login
		 (setq ange-ftp-netrc-default-user login))
	    (and password
		 (setq ange-ftp-netrc-default-password password))
	    (and account
		 (setq ange-ftp-netrc-default-account account)))))
    (goto-char end)))

;; Read in ~/.netrc, if one exists.  If ~/.netrc file exists and has
;; the correct permissions then extract the \`machine\', \`login\',
;; \`password\' and \`account\' information from within.

(defun ange-ftp-parse-netrc ()
  ;; We set this before actually doing it to avoid the possibility
  ;; of an infinite loop if ange-ftp-netrc-filename is an FTP file.
  (interactive)
  (let (file attr)
    (let ((default-directory "/"))
      (setq file (ange-ftp-chase-symlinks
		  (ange-ftp-real-expand-file-name ange-ftp-netrc-filename)))
      (setq attr (ange-ftp-real-file-attributes file)))
    (if (and attr			; file exists.
	     (not (equal (nth 5 attr) ange-ftp-netrc-modtime)))	; file changed
	(save-match-data
	  (if (or ange-ftp-disable-netrc-security-check
		  (and (eq (nth 2 attr) (user-uid)) ; Same uids.
		       (string-match ".r..------" (nth 8 attr))))
	      (with-current-buffer
		;; we are cheating a bit here.  I'm trying to do the equivalent
		;; of find-file on the .netrc file, but then nuke it afterwards.
		;; with the bit of logic below we should be able to have
		;; encrypted .netrc files.
                  (generate-new-buffer "*ftp-.netrc*")
		(ange-ftp-real-insert-file-contents file)
		(setq buffer-file-name file)
		(setq default-directory (file-name-directory file))
		(normal-mode t)
		(run-hooks 'find-file-hook)
		(setq buffer-file-name nil)
		(goto-char (point-min))
		(while (search-forward-regexp "^[ \t]*#.*$" nil t)
		  (replace-match ""))
		(goto-char (point-min))
		(skip-chars-forward " \t\r\n")
		(while (not (eobp))
		  (ange-ftp-parse-netrc-group))
		(kill-buffer (current-buffer)))
	    (ange-ftp-message "%s either not owned by you or badly protected."
			      ange-ftp-netrc-filename)
	    (sit-for 1))
	  (setq ange-ftp-netrc-modtime (nth 5 attr))))))

;; Return a list of prefixes of the form 'user@host:' to be used when
;; completion is done in the root directory.

(defun ange-ftp-generate-root-prefixes ()
  (ange-ftp-parse-netrc)
  (save-match-data
    (let (res)
      (maphash
       (lambda (key value)
	 (if (string-match "\\`[^/]*\\(/\\).*\\'" key)
	     (let ((host (substring key 0 (match-beginning 1)))
		   (user (substring key (match-end 1))))
	       (push (concat user "@" host ":") res))))
       ange-ftp-passwd-hashtable)
      (maphash
       (lambda (host user) (push (concat host ":") res))
       ange-ftp-user-hashtable)
      (or res (list nil)))))

;;;; ------------------------------------------------------------
;;;; Remote file name syntax support.
;;;; ------------------------------------------------------------

(defmacro ange-ftp-ftp-name-component (n ns name)
  "Extract the Nth FTP file name component from NS."
  `(let ((elt (nth ,n ,ns)))
     (match-string elt ,name)))

(defvar ange-ftp-ftp-name-arg "")
(defvar ange-ftp-ftp-name-res nil)

;; Parse NAME according to `ange-ftp-name-format' (which see).
;; Returns a list (HOST USER NAME), or nil if NAME does not match the format.
(defun ange-ftp-ftp-name (name)
  (if (string-equal name ange-ftp-ftp-name-arg)
      ange-ftp-ftp-name-res
    (setq ange-ftp-ftp-name-arg name
	  ange-ftp-ftp-name-res
	  (save-match-data
	    (if (posix-string-match (car ange-ftp-name-format) name)
		(let* ((ns (cdr ange-ftp-name-format))
		       (host (ange-ftp-ftp-name-component 0 ns name))
		       (user (ange-ftp-ftp-name-component 1 ns name))
		       (name (ange-ftp-ftp-name-component 2 ns name)))
		  (if (zerop (length user))
		      (setq user (ange-ftp-get-user host)))
		  (list host user name))
	      nil)))))

;; Take a FULLNAME that matches according to ange-ftp-name-format and
;; replace the name component with NAME.
(defun ange-ftp-replace-name-component (fullname name)
  (save-match-data
    (if (posix-string-match (car ange-ftp-name-format) fullname)
	(let* ((ns (cdr ange-ftp-name-format))
	       (elt (nth 2 ns)))
	  (concat (substring fullname 0 (match-beginning elt))
		  name
		  (substring fullname (match-end elt)))))))

;;;; ------------------------------------------------------------
;;;; Miscellaneous utils.
;;;; ------------------------------------------------------------

;; (setq ange-ftp-tmp-keymap (make-sparse-keymap))
;; (define-key ange-ftp-tmp-keymap "\C-m" 'exit-minibuffer)

(defun ange-ftp-repaint-minibuffer ()
  "Clear any existing minibuffer message; let the minibuffer contents show."
  (message nil))

;; Return the name of the buffer that collects output from the ftp process
;; connected to the given HOST and USER pair.
(defun ange-ftp-ftp-process-buffer (host user)
  (concat "*ftp " user "@" host "*"))

;; Display the last chunk of output from the ftp process for the given HOST
;; USER pair, and signal an error including MSG in the text.
(defun ange-ftp-error (host user msg)
  (save-excursion  ;; Prevent pop-to-buffer from changing current buffer.
    (let ((cur (selected-window))
	  (pop-up-windows t))
      (pop-to-buffer
       (get-buffer-create
	(ange-ftp-ftp-process-buffer host user)))
      (goto-char (point-max))
      (select-window cur))
    (signal 'ftp-error (list (format "FTP Error: %s" msg)))))

(defun ange-ftp-set-buffer-mode ()
  "Set correct modes for the current buffer if visiting a remote file."
  (if (and (stringp buffer-file-name)
	   (ange-ftp-ftp-name buffer-file-name))
      (auto-save-mode ange-ftp-auto-save)))

(defun ange-ftp-kill-ftp-process (&optional buffer)
  "Kill the FTP process associated with BUFFER (the current buffer, if nil).
If the BUFFER's visited filename or `default-directory' is an FTP filename
then kill the related FTP process."
  (interactive "bKill FTP process associated with buffer: ")
  (if (null buffer)
      (setq buffer (current-buffer))
    (setq buffer (get-buffer buffer)))
  (let ((file (or (buffer-file-name buffer)
		  (with-current-buffer buffer default-directory))))
    (if file
	(let ((parsed (ange-ftp-ftp-name (expand-file-name file))))
	  (if parsed
	      (let ((host (nth 0 parsed))
		    (user (nth 1 parsed)))
		(kill-buffer (get-buffer (ange-ftp-ftp-process-buffer host user)))))))))

(defun ange-ftp-quote-string (string)
  "Quote any characters in STRING that may confuse the FTP process."
  ;; This is said to be wrong; ftp is said to need quoting only for ",
  ;; and that by doubling it.  But experiment says UNIX-style kind of
  ;; quoting is correct when talking to ftp on GNU/Linux systems, and
  ;; W32-style kind of quoting on, yes, W32 systems.
  (if (stringp string)
      (shell-quote-argument string)
    ""))

(defun ange-ftp-barf-if-not-directory (directory)
  (or (file-directory-p directory)
      (signal 'file-error
	      (list "Opening directory"
		    (if (file-exists-p directory)
			"not a directory"
		      "no such file or directory")
		    directory))))

;;;; ------------------------------------------------------------
;;;; FTP process filter support.
;;;; ------------------------------------------------------------

(defun ange-ftp-process-handle-line (line proc)
  "Look at the given LINE from the FTP process PROC.
Try to categorize it into one of four categories:
good, skip, fatal, or unknown."
  (cond ((string-match ange-ftp-xfer-size-msgs line)
	 (setq ange-ftp-xfer-size
	       (/ (string-to-number (match-string 1 line))
		  1024)))
	((string-match ange-ftp-skip-msgs line)
	 t)
	((string-match ange-ftp-good-msgs line)
	 (setq ange-ftp-process-busy nil
	       ange-ftp-process-result t
               ange-ftp-pending-error-line nil
	       ange-ftp-process-result-line line))
	;; Check this before checking for errors.
	;; Otherwise the last line of these three seems to be an error:
	;; 230-see a significant impact from the move.  For those of you who can't
	;; 230-use DNS to resolve hostnames and get an error message like
	;; 230-"ftp.stsci.edu: unknown host", the new IP address will be...
	((string-match ange-ftp-multi-msgs line)
	 (setq ange-ftp-process-multi-skip t))
	((string-match ange-ftp-potential-error-msgs line)
         ;; This looks like an error, but we have to keep reading the output
         ;; to see if it was fixed or not.  E.g. it may indicate that IPv6
         ;; failed, but maybe a subsequent IPv4 fallback succeeded.
         (set (make-local-variable 'ange-ftp-pending-error-line) line)
         t)
	((string-match ange-ftp-fatal-msgs line)
	 (delete-process proc)
	 (setq ange-ftp-process-busy nil
	       ange-ftp-process-result-line line))
        (ange-ftp-process-multi-skip
	 t)
	(t
	 (setq ange-ftp-process-busy nil
	       ange-ftp-process-result-line line))))

(defun ange-ftp-set-xfer-size (host user bytes)
  "Set the size of the next FTP transfer in bytes."
  (let ((proc (ange-ftp-get-process host user)))
    (when proc
      (let ((buf (process-buffer proc)))
	(when buf
	  (with-current-buffer buf
	    (setq ange-ftp-xfer-size
		  ;; For very large files, BYTES can be a float.
		  (if (integerp bytes)
		      (ash bytes -10)
		    (/ bytes 1024)))))))))

(defun ange-ftp-process-handle-hash (string)
  "Remove hash marks from STRING and display count so far."
  (setq string (concat (substring string 0 (match-beginning 0))
		       (substring string (match-end 0)))
	ange-ftp-hash-mark-count (+ (- (match-end 0)
				       (match-beginning 0))
				    ange-ftp-hash-mark-count))
  (and ange-ftp-hash-mark-unit
       ange-ftp-process-msg
       ange-ftp-process-verbose
       (not (eq (selected-window) (minibuffer-window)))
       (not (boundp 'search-message))	;screws up isearch otherwise
       (not cursor-in-echo-area)	;screws up y-or-n-p otherwise
       (let ((kbytes (ash (* ange-ftp-hash-mark-unit
			     ange-ftp-hash-mark-count)
			  -6)))
	 (if (zerop ange-ftp-xfer-size)
	     (ange-ftp-message "%s...%dk" ange-ftp-process-msg kbytes)
	   (let ((percent (/ (* 100 kbytes) ange-ftp-xfer-size)))
	     ;; cut out the redisplay of identical %-age messages.
	     (unless (eq percent ange-ftp-last-percent)
	       (setq ange-ftp-last-percent percent)
	       (ange-ftp-message "%s...%d%%" ange-ftp-process-msg percent))))))
  string)

;; Call the function specified by CONT.  CONT can be either a function
;; or a list of a function and some args.  The first two parameters
;; passed to the function will be RESULT and LINE.  The remaining args
;; will be taken from CONT if a list was passed.

(defun ange-ftp-call-cont (cont result line)
  (when cont
    (if (and (listp cont)
	     (not (eq (car cont) 'lambda)))
	(apply (car cont) result line (cdr cont))
      (funcall cont result line))))

;; Build up a complete line of output from the ftp PROCESS and pass it
;; on to ange-ftp-process-handle-line to deal with.

(defun ange-ftp-process-filter (proc str)
  ;; Eliminate nulls.
  (while (string-match "\000+" str)
    (setq str (replace-match "" nil nil str)))

  ;; see if the buffer is still around... it could have been deleted.
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)

      ;; handle hash mark printing
      (and ange-ftp-process-busy
           (string-match "^#+$" str)
           (setq str (ange-ftp-process-handle-hash str)))
      (comint-output-filter proc str)
      ;; Replace STR by the result of the comint processing.
      (setq str (buffer-substring comint-last-output-start
                                  (process-mark proc)))
      (when ange-ftp-process-busy
	(setq ange-ftp-process-string (concat ange-ftp-process-string
					      str))

	;; if we gave an empty password to the USER command earlier
	;; then we should send a null password now.
	(if (string-match "Password: *$" ange-ftp-process-string)
	    (process-send-string proc "\n")))
      (while (and ange-ftp-process-busy
                  (string-match "\n" ange-ftp-process-string))
        (let ((line (substring ange-ftp-process-string
                               0
                               (match-beginning 0)))
              (seen-prompt nil))
          (setq ange-ftp-process-string (substring ange-ftp-process-string
                                                   (match-end 0)))
          (while (string-match "\\`ftp> *" line)
            (setq seen-prompt t)
            (setq line (substring line (match-end 0))))
          (if (not (and seen-prompt ange-ftp-pending-error-line))
              (ange-ftp-process-handle-line line proc)
            ;; If we've seen a potential error message and it
            ;; hasn't been canceled by a good message before
            ;; seeing a prompt, then the error was real.
            (delete-process proc)
            (setq ange-ftp-process-busy nil
                  ange-ftp-process-result-line ange-ftp-pending-error-line))))

      ;; has the ftp client finished?  if so then do some clean-up
      ;; actions.
      (unless ange-ftp-process-busy
	;; reset the xfer size
	(setq ange-ftp-xfer-size 0)

	;; issue the "done" message since we've finished.
	(when (and ange-ftp-process-msg
		   ange-ftp-process-verbose
		   ange-ftp-process-result)
	  (ange-ftp-message "%s...done" ange-ftp-process-msg)
	  (ange-ftp-repaint-minibuffer)
	  (setq ange-ftp-process-msg nil))

	;; is there a continuation we should be calling?  if so,
	;; we'd better call it, making sure we only call it once.
	(when ange-ftp-process-continue
	  (let ((cont ange-ftp-process-continue))
	    (setq ange-ftp-process-continue nil)
	    (ange-ftp-call-cont cont
				ange-ftp-process-result
				ange-ftp-process-result-line)))))))

(defun ange-ftp-process-sentinel (proc str)
  "When FTP process changes state, nuke all file-entries in cache."
  (let ((name (process-name proc)))
    (when (string-match "\\*ftp \\([^@]+\\)@\\([^*]+\\)\\*" name)
      (let ((user (match-string 1 name))
	    (host (match-string 2 name)))
	(ange-ftp-wipe-file-entries host user))))
  (setq ange-ftp-ls-cache-file nil))

;;;; ------------------------------------------------------------
;;;; Gateway support.
;;;; ------------------------------------------------------------

(defun ange-ftp-use-gateway-p (host)
  "Return whether to access this HOST via a normal (non-smart) gateway."
  ;; yes, I know that I could simplify the following expression, but it is
  ;; clearer (to me at least) this way.
  (and (not ange-ftp-smart-gateway)
       (not (string-match-p ange-ftp-local-host-regexp host))))

(defun ange-ftp-use-smart-gateway-p (host)
  "Returns whether to access this HOST via a smart gateway."
  (and ange-ftp-smart-gateway
       (not (string-match-p ange-ftp-local-host-regexp host))))


;;; ------------------------------------------------------------
;;; Temporary file location and deletion...
;;; ------------------------------------------------------------

(defun ange-ftp-make-tmp-name (host &optional suffix)
  "This routine will return the name of a new file."
  (make-temp-file (if (ange-ftp-use-gateway-p host)
		      ange-ftp-gateway-tmp-name-template
		    ange-ftp-tmp-name-template)
		  nil suffix))

(defun ange-ftp-del-tmp-name (filename)
  "Force to delete temporary file."
  (delete-file filename))


;;;; ------------------------------------------------------------
;;;; Interactive gateway program support.
;;;; ------------------------------------------------------------

(defvar ange-ftp-gwp-running t)
(defvar ange-ftp-gwp-status nil)

(defun ange-ftp-gwp-sentinel (proc str)
  (setq ange-ftp-gwp-running nil))

(defun ange-ftp-gwp-filter (proc str)
  (comint-output-filter proc str)
  (with-current-buffer (process-buffer proc)
    ;; Replace STR by the result of the comint processing.
    (setq str (buffer-substring comint-last-output-start (process-mark proc))))
  (cond ((string-match "login: *$" str)
	 (process-send-string proc
                              (concat
                               (let ((ange-ftp-default-user t))
                                 (ange-ftp-get-user ange-ftp-gateway-host))
                               "\n")))
	((string-match "Password: *$" str)
	 (process-send-string proc
                              (concat
                               (ange-ftp-get-passwd ange-ftp-gateway-host
                                                    (ange-ftp-get-user
                                                     ange-ftp-gateway-host))
                               "\n")))
	((string-match ange-ftp-gateway-fatal-msgs str)
	 (delete-process proc)
	 (setq ange-ftp-gwp-running nil))
	((string-match ange-ftp-gateway-prompt-pattern str)
	 (setq ange-ftp-gwp-running nil
	       ange-ftp-gwp-status t))))

(defun ange-ftp-gwp-start (host user name args)
  "Login to the gateway machine and fire up an FTP process."
  (let (;; It would be nice to make process-connection-type nil,
	;; but that doesn't work: ftp never responds.
	;; Can anyone find a fix for that?
	(proc (let ((process-connection-type t))
		(start-process name name
			       ange-ftp-gateway-program
			       ange-ftp-gateway-host)))
	(ftp (mapconcat 'identity args " ")))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc 'ange-ftp-gwp-sentinel)
    (set-process-filter proc 'ange-ftp-gwp-filter)
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    (setq ange-ftp-gwp-running t
	  ange-ftp-gwp-status nil)
    (ange-ftp-message "Connecting to gateway %s..." ange-ftp-gateway-host)
    (while ange-ftp-gwp-running		;perform login sequence
      (accept-process-output proc))
    (unless ange-ftp-gwp-status
      (ange-ftp-error host user "unable to login to gateway"))
    (ange-ftp-message "Connecting to gateway %s...done" ange-ftp-gateway-host)
    (setq ange-ftp-gwp-running t
	  ange-ftp-gwp-status nil)
    (process-send-string proc ange-ftp-gateway-setup-term-command)
    (while ange-ftp-gwp-running		;zap ^M's and double echoing.
      (accept-process-output proc))
    (unless ange-ftp-gwp-status
      (ange-ftp-error host user "unable to set terminal modes on gateway"))
    (setq ange-ftp-gwp-running t
	  ange-ftp-gwp-status nil)
    (process-send-string proc (concat "exec " ftp "\n")) ;spawn ftp process
    proc))

;;;; ------------------------------------------------------------
;;;; Support for sending commands to the ftp process.
;;;; ------------------------------------------------------------

(defun ange-ftp-raw-send-cmd (proc cmd &optional msg cont nowait)
  "Low-level routine to send the given FTP CMD to the FTP process PROC.
MSG is an optional message to output before and after the command.
If CONT is non-nil then it is either a function or a list of function
and some arguments.  The function will be called when the FTP command
has completed.
If CONT is nil then this routine will return (RESULT . LINE) where RESULT
is whether the command was successful, and LINE is the line from the FTP
process that caused the command to complete.
If NOWAIT is given then the routine will return immediately the command has
been queued with no result.  CONT will still be called, however."
  (if (memq (process-status proc) '(run open))
      (with-current-buffer (process-buffer proc)
	(ange-ftp-wait-not-busy proc)
	(setq ange-ftp-process-string ""
	      ange-ftp-process-result-line ""
	      ange-ftp-process-busy t
	      ange-ftp-process-result nil
	      ange-ftp-process-multi-skip nil
	      ange-ftp-process-msg msg
	      ange-ftp-process-continue cont
	      ange-ftp-hash-mark-count 0
	      ange-ftp-last-percent -1
	      cmd (concat cmd "\n"))
	(and msg ange-ftp-process-verbose (ange-ftp-message "%s..." msg))
	(goto-char (point-max))
	(move-marker comint-last-input-start (point))
	;; don't insert the password into the buffer on the USER command.
	(save-match-data
	  (if (string-match "\\`user \"[^\"]*\"" cmd)
	      (insert (substring cmd 0 (match-end 0)) " Turtle Power!\n")
	    (insert cmd)))
	(move-marker comint-last-input-end (point))
	(process-send-string proc cmd)
	(set-marker (process-mark proc) (point))
	(if nowait
	    nil
	  (ange-ftp-wait-not-busy proc)
	  (if cont
	      nil			;cont has already been called
	    (cons ange-ftp-process-result ange-ftp-process-result-line))))))

;; Wait for the ange-ftp process PROC not to be busy.
(defun ange-ftp-wait-not-busy (proc)
  (with-current-buffer (process-buffer proc)
    (condition-case nil
	;; This is a kludge to let user quit in case ftp gets hung.
	;; It matters because this function can be called from the filter.
	;; It is bad to allow quitting in a filter, but getting hung
	;; is worse.  By binding quit-flag to nil, we might avoid
	;; most of the probability of getting screwed because the user
	;; wants to quit some command.
	(let ((quit-flag nil)
	      (inhibit-quit nil))
	  (while ange-ftp-process-busy
	    (accept-process-output proc)))
      (quit
       ;; If the user does quit out of this,
       ;; kill the process.  That stops any transfer in progress.
       ;; The next operation will open a new ftp connection.
       (delete-process proc)
       (signal 'quit nil)))))

(defun ange-ftp-nslookup-host (hostname)
  "Attempt to resolve the given HOSTNAME using nslookup if possible."
  (interactive "sHost:  ")
  (if ange-ftp-nslookup-program
      (let ((default-directory
	      (if (file-accessible-directory-p default-directory)
		  default-directory
		exec-directory))
	    ;; It would be nice to make process-connection-type nil,
	    ;; but that doesn't work: ftp never responds.
	    ;; Can anyone find a fix for that?
	    (proc (let ((process-connection-type t))
		    (start-process " *nslookup*" " *nslookup*"
				   ange-ftp-nslookup-program hostname)))
	    (res hostname))
	(set-process-query-on-exit-flag proc nil)
	(with-current-buffer (process-buffer proc)
	  (while (memq (process-status proc) '(run open))
	    (accept-process-output proc))
	  (goto-char (point-min))
	  (if (re-search-forward "Name:.*\nAddress: *\\(.*\\)$" nil t)
	      (setq res (match-string 1)))
	  (kill-buffer (current-buffer)))
	res)
    hostname))

(defun ange-ftp-start-process (host user name)
  "Spawn a new FTP process ready to connect to machine HOST and give it NAME.
If HOST is only FTP-able through a gateway machine then spawn a shell
on the gateway machine to do the FTP instead."
  (let* ((use-gateway (ange-ftp-use-gateway-p host))
	 (use-smart-ftp (and (not ange-ftp-gateway-host)
			     (ange-ftp-use-smart-gateway-p host)))
	 (ftp-prog (if (or use-gateway
			   use-smart-ftp)
		       ange-ftp-gateway-ftp-program-name
		     ange-ftp-ftp-program-name))
	 (args (append (list ftp-prog) ange-ftp-ftp-program-args))
	 ;; Without the following binding, ange-ftp-start-process
	 ;; recurses on file-accessible-directory-p, since it needs to
	 ;; restart its process in order to determine anything about
	 ;; default-directory.
	 (file-name-handler-alist)
	 (default-directory
	   (if (file-accessible-directory-p default-directory)
	       default-directory
	     exec-directory))
	 proc)
    ;; It would be nice to make process-connection-type nil,
    ;; but that doesn't work: ftp never responds.
    ;; Can anyone find a fix for that?
    (let ((process-connection-type t)
	  ;; Copy this so we don't alter it permanently.
	  (process-environment (copy-tree process-environment))
	  (buffer (get-buffer-create name)))
      (with-current-buffer buffer
	(internal-ange-ftp-mode))
      ;; This tells GNU ftp not to output any fancy escape sequences.
      (setenv "TERM" "dumb")
      (if use-gateway
	  (if ange-ftp-gateway-program-interactive
	      (setq proc (ange-ftp-gwp-start host user name args))
	    (setq proc (apply 'start-process name name
			      (append (list ange-ftp-gateway-program
					    ange-ftp-gateway-host)
				      args))))
	(setq proc (apply 'start-process name name args))))
    (with-current-buffer (process-buffer proc)
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc 'ange-ftp-process-sentinel)
    (set-process-filter proc 'ange-ftp-process-filter)
    ;; On Windows, the standard ftp client buffers its output (because
    ;; stdout is a pipe handle) so the startup message may never appear:
    ;; `accept-process-output' at this point would hang indefinitely.
    ;; However, sending an innocuous command ("help foo") forces some
    ;; output that will be ignored, which is just as good.  Once we
    ;; start sending normal commands, the output no longer appears to be
    ;; buffered, and everything works correctly.  My guess is that the
    ;; output of interest is being sent to stderr which is not buffered.
    (when (eq system-type 'windows-nt)
      ;; force ftp output to be treated as DOS text, otherwise the
      ;; output of "help foo" confuses the EOL detection logic.
      (set-process-coding-system proc 'raw-text-dos)
      (process-send-string proc "help foo\n"))
    (accept-process-output proc)	;wait for ftp startup message
    proc))

(define-derived-mode internal-ange-ftp-mode comint-mode "Internal Ange-ftp"
  "Major mode for interacting with the FTP process.

\\{comint-mode-map}"
  (make-local-variable 'ange-ftp-process-string)
  (setq ange-ftp-process-string "")
  (make-local-variable 'ange-ftp-process-busy)
  (make-local-variable 'ange-ftp-process-result)
  (make-local-variable 'ange-ftp-process-msg)
  (make-local-variable 'ange-ftp-process-multi-skip)
  (make-local-variable 'ange-ftp-process-result-line)
  (make-local-variable 'ange-ftp-process-continue)
  (make-local-variable 'ange-ftp-hash-mark-count)
  (make-local-variable 'ange-ftp-binary-hash-mark-size)
  (make-local-variable 'ange-ftp-ascii-hash-mark-size)
  (make-local-variable 'ange-ftp-hash-mark-unit)
  (make-local-variable 'ange-ftp-xfer-size)
  (make-local-variable 'ange-ftp-last-percent)
  (setq ange-ftp-hash-mark-count 0)
  (setq ange-ftp-xfer-size 0)
  (setq ange-ftp-process-result-line "")
  (setq comint-prompt-regexp "^ftp> ")
  (make-local-variable 'comint-password-prompt-regexp)
  ;; This is a regexp that can't match anything.
  ;; ange-ftp has its own ways of handling passwords.
  (setq comint-password-prompt-regexp "\\`a\\`")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp))

(defcustom ange-ftp-raw-login nil
  "Use raw FTP commands for login, if account password is not nil.
Some FTP implementations need this, e.g. ftp in NT 4.0."
  :group 'ange-ftp
  :version "21.3"
  :type 'boolean)

(defun ange-ftp-smart-login (host user password account proc)
  "Connect to the FTP-server on HOST as USER using PASSWORD and ACCOUNT.
PROC is the FTP-client's process.  This routine uses the smart-gateway
host specified in `ange-ftp-gateway-host'."
  (let ((result (ange-ftp-raw-send-cmd
		 proc
		 (format "open %s %s"
			 (ange-ftp-nslookup-host ange-ftp-gateway-host)
			 ange-ftp-smart-gateway-port)
		 (format "Opening FTP connection to %s via %s"
			 host
			 ange-ftp-gateway-host))))
    (or (car result)
	(ange-ftp-error host user
			(concat "OPEN request failed: "
				(cdr result))))
    (setq result (ange-ftp-raw-send-cmd
		  proc (format "user \"%s\"@%s %s %s"
			       user
			       (ange-ftp-nslookup-host host)
			       password
			       account)
		  (format "Logging in as user %s@%s"
			  user host)))
    (or (car result)
	(progn
	  (ange-ftp-set-passwd host user nil) ; reset password
	  (ange-ftp-set-account host user nil) ; reset account
	  (ange-ftp-error host user
			  (concat "USER request failed: "
				  (cdr result)))))))

(defun ange-ftp-normal-login (host user password account proc)
  "Connect to the FTP-server on HOST as USER using PASSWORD and ACCOUNT.
PROC is the process to the FTP-client.  HOST may have an optional
suffix of the form #PORT to specify a non-default port."
  (save-match-data
    (string-match "\\`\\([^#]+\\)\\(#\\([0-9]+\\)\\)?\\'" host)
    (let* ((nshost (ange-ftp-nslookup-host (match-string 1 host)))
	   (port (match-string 3 host))
	   (result (ange-ftp-raw-send-cmd
		    proc
		    (if port
			(format "open %s %s" nshost port)
		      (format "open %s" nshost))
		    (format "Opening FTP connection to %s" host))))
      (or (car result)
	  (ange-ftp-error host user
			  (concat "OPEN request failed: "
				  (cdr result))))
      (if (not (and ange-ftp-raw-login (string< "" account)))
	  (setq result (ange-ftp-raw-send-cmd
			proc
			(if (and (ange-ftp-use-smart-gateway-p host)
				 ange-ftp-gateway-host)
			    (format "user \"%s\"@%s %s %s"
				    user nshost password account)
			  (format "user \"%s\" %s %s" user password account))
			(format "Logging in as user %s@%s" user host)))
	(let ((good ange-ftp-good-msgs)
	      (skip ange-ftp-skip-msgs))
	  (setq ange-ftp-good-msgs (concat ange-ftp-good-msgs
					   "\\|^331 \\|^332 "))
	  (if (string-match (regexp-quote "\\|^331 ") ange-ftp-skip-msgs)
	      (setq ange-ftp-skip-msgs
		    (replace-match "" t t ange-ftp-skip-msgs)))
	  (if (string-match (regexp-quote "\\|^332 ") ange-ftp-skip-msgs)
	      (setq ange-ftp-skip-msgs
		    (replace-match "" t t ange-ftp-skip-msgs)))
	  (setq result (ange-ftp-raw-send-cmd
			proc
			(format "quote \"USER %s\"" user)
			(format "Logging in as user %s@%s" user host)))
	  (and (car result)
	       (setq result (ange-ftp-raw-send-cmd
			     proc
			     (format "quote \"PASS %s\"" password)
			     (format "Logging in as user %s@%s" user host)))
	       (and (car result)
		    (setq result (ange-ftp-raw-send-cmd
				  proc
				  (format "quote \"ACCT %s\"" account)
				  (format "Logging in as user %s@%s" user host)))
		    ))
	  (setq ange-ftp-good-msgs good
		ange-ftp-skip-msgs skip)))
      (or (car result)
	  (progn
	    (ange-ftp-set-passwd host user nil)	;reset password.
	    (ange-ftp-set-account host user nil) ;reset account.
	    (ange-ftp-error host user
			    (concat "USER request failed: "
				    (cdr result))))))))

;; ange@hplb.hpl.hp.com says this should not be changed.
(defvar ange-ftp-hash-mark-msgs
  "[hH]ash mark [^0-9]*\\([0-9]+\\)"
  "*Regexp matching the FTP client's output upon doing a HASH command.")

(defun ange-ftp-guess-hash-mark-size (proc)
  (if ange-ftp-send-hash
      (with-current-buffer (process-buffer proc)
	(let* ((status (ange-ftp-raw-send-cmd proc "hash"))
	       (line (cdr status)))
	  (save-match-data
	    (if (string-match ange-ftp-hash-mark-msgs line)
		(let ((size (string-to-number (match-string 1 line))))
		  (setq ange-ftp-ascii-hash-mark-size size
			ange-ftp-hash-mark-unit (ash size -4))

		  ;; if a default value for this is set, use that value.
		  (or ange-ftp-binary-hash-mark-size
		      (setq ange-ftp-binary-hash-mark-size size)))))))))

(defvar ange-ftp-process-startup-hook nil)

(defun ange-ftp-get-process (host user)
  "Return an FTP subprocess connected to HOST and logged in as USER.
Create a new process if needed."
  (let* ((name (ange-ftp-ftp-process-buffer host user))
	 (proc (get-process name)))
    (if (and proc (memq (process-status proc) '(run open)))
	proc
      ;; Must delete dead process so that new process can reuse the name.
      (if proc (delete-process proc))
      (let ((pass (ange-ftp-quote-string
		   (ange-ftp-get-passwd host user)))
	    (account (ange-ftp-quote-string
		      (ange-ftp-get-account host user))))
	;; grab a suitable process.
	(setq proc (ange-ftp-start-process host user name))

	;; login to FTP server.
	(if (and (ange-ftp-use-smart-gateway-p host)
		 ange-ftp-gateway-host)
	    (ange-ftp-smart-login host user pass account proc)
	  (ange-ftp-normal-login host user pass account proc))

	;; Tell client to send back hash-marks as progress.  It isn't usually
	;; fatal if this command fails.
	(ange-ftp-guess-hash-mark-size proc)

	;; Guess at the host type.
	(ange-ftp-guess-host-type host user)

	;; Turn passive mode on or off as requested.
	(let* ((case-fold-search t)
	       (passive
		(or (assoc-default host ange-ftp-passive-host-alist
				   'string-match)
		    (if ange-ftp-try-passive-mode "on"))))
	  (if passive
	      (ange-ftp-passive-mode proc passive)))

	;; Run any user-specified hooks.  Note that proc, host and user are
	;; dynamically bound at this point.
	(let ((ange-ftp-this-user user)
	      (ange-ftp-this-host host))
	  (run-hooks 'ange-ftp-process-startup-hook)))
      proc)))

(defun ange-ftp-passive-mode (proc on-or-off)
  (if (string-match (concat "Passive mode " on-or-off)
                    (cdr (ange-ftp-raw-send-cmd
                          proc (concat "passive " on-or-off)
                          "Trying passive mode..." nil)))
      (ange-ftp-message (concat "Trying passive mode..." on-or-off))
    (error "Trying passive mode...failed")))

;; Variables for caching host and host-type
(defvar ange-ftp-host-cache nil)
(defvar ange-ftp-host-type-cache nil)

;; If ange-ftp-host-type is called with the optional user
;; argument, it will attempt to guess the host type by connecting
;; as user, if necessary. For efficiency, I have tried to give this
;; optional second argument only when necessary. Have I missed any calls
;; to ange-ftp-host-type where it should have been supplied?

(defun ange-ftp-host-type (host &optional user)
  "Return a symbol which represents the type of the HOST given.
If the optional argument USER is given, attempts to guess the
host-type by logging in as USER."
  (cond ((null host) 'unix)
	;; Return `unix' if HOST is nil, since that's the most vanilla
	;; possible return value.
	((eq host ange-ftp-host-cache)
	 ange-ftp-host-type-cache)
	;; Trigger an ftp connection, in case we need to guess at the host type.
	((and user (ange-ftp-get-process host user) (eq host ange-ftp-host-cache))
	 ange-ftp-host-type-cache)
	(t
         (setq ange-ftp-host-cache host
	       ange-ftp-host-type-cache
	       (cond ((ange-ftp-dumb-unix-host host)
		      'dumb-unix)
		     ;;		  ((and (fboundp 'ange-ftp-vos-host)
		     ;;			(ange-ftp-vos-host host))
		     ;;		   'vos)
		     ((and (fboundp 'ange-ftp-vms-host)
			   (ange-ftp-vms-host host))
		      'vms)
		     ((and (fboundp 'ange-ftp-mts-host)
			   (ange-ftp-mts-host host))
		      'mts)
		     ((and (fboundp 'ange-ftp-cms-host)
			   (ange-ftp-cms-host host))
		      'cms)
		     ((and (fboundp 'ange-ftp-bs2000-posix-host)
			   (ange-ftp-bs2000-posix-host host))
		      'text-unix)	; POSIX is a non-ASCII Unix
		     ((and (fboundp 'ange-ftp-bs2000-host)
			   (ange-ftp-bs2000-host host))
		      'bs2000)
		     (t
		      'unix))))))

;; It would be nice to abstract the functions ange-ftp-TYPE-host and
;; ange-ftp-add-TYPE-host. The trick is to abstract these functions
;; without sacrificing speed. Also, having separate variables
;; ange-ftp-TYPE-regexp is more user friendly then requiring the user to
;; set an alist to indicate that a host is of a given type. Even with
;; automatic host type recognition, setting a regexp is still a good idea
;; (for efficiency) if you log into a particular non-UNIX host frequently.

(defvar ange-ftp-fix-name-func-alist nil
  "Alist saying how to convert file name to the host's syntax.
Association list of (TYPE . FUNC) pairs, where FUNC is a routine which can
change a UNIX file name into a name more suitable for a host of type TYPE.")

(defvar ange-ftp-fix-dir-name-func-alist nil
  "Alist saying how to convert directory name to the host's syntax.
Association list of (TYPE . FUNC) pairs, where FUNC is a routine which can
change UNIX directory name into a directory name more suitable for a host
of type TYPE.")

;; *** Perhaps the sense of this variable should be inverted, since there
;; *** is only 1 host type that can take ls-style listing options.
(defvar ange-ftp-dumb-host-types '(dumb-unix)
  "List of host types that can't take UNIX ls-style listing options.")

(defun ange-ftp-send-cmd (host user cmd &optional msg cont nowait)
  "Find an FTP process connected to HOST logged in as USER and send it CMD.
MSG is an optional status message to be output before and after issuing the
command.
See the documentation for `ange-ftp-raw-send-cmd' for a description of CONT
and NOWAIT."
  ;; Handle conversion to remote file name syntax and remote ls option
  ;; capability.
  (let ((cmd0 (car cmd))
	(cmd1 (nth 1 cmd))
	(ange-ftp-this-user user)
	(ange-ftp-this-host host)
	(ange-ftp-this-msg msg)
	cmd2 cmd3 host-type fix-name-func result)

    (cond

     ;; pwd case (We don't care what host-type.)
     ((null cmd1))

     ;; cmd == 'dir "remote-name" "local-name" "ls-switches"
     ((progn
       (setq cmd2 (nth 2 cmd)
	     host-type (ange-ftp-host-type host user))
       ;; This will trigger an FTP login, if one doesn't exist
       (eq cmd0 'dir))
      (setq cmd1 (funcall
		  (or (cdr (assq host-type ange-ftp-fix-dir-name-func-alist))
		      'identity)
		  cmd1)
	    cmd3 (nth 3 cmd))
      ;; Need to deal with the HP-UX ftp bug. This should also allow us to
      ;; resolve symlinks to directories on SysV machines. (Sebastian will
      ;; be happy.)
      (and (eq host-type 'unix)
	   (string-match "/\\'" cmd1)
	   (not (string-match "R" cmd3))
	   (setq cmd1 (concat cmd1 ".")))

      ;; Using "ls -flags foo" has several problems:
      ;; - if foo is a symlink, we may get a single line showing the symlink
      ;;   rather than the listing of the directory it points to.
      ;; - if "foo" has spaces, the parsing of the command may be done wrong.
      ;; - some version of netbsd's ftpd only accept a single argument after
      ;;   `ls', which can either be the directory or the flags.
      ;; So to work around those problems, we use "cd foo; ls -flags".

      ;; If the dir name contains a space, some ftp servers will
      ;; refuse to list it.  We instead change directory to the
      ;; directory in question and ls ".".
      (when (string-match " " cmd1)
	;; Keep the result.  In case of failure, we will (see below)
	;; short-circuit CMD and return this result directly.
	(setq result (ange-ftp-cd host user (nth 1 cmd) 'noerror))
	(setq cmd1 "."))

      ;; If the remote ls can take switches, put them in
      (unless (memq host-type ange-ftp-dumb-host-types)
	(setq cmd0 'ls)
	;; We cd and then use `ls' with no directory argument.
	;; This works around a misfeature of some versions of netbsd ftpd
	;; where `ls' can only take one argument: either one set of flags
	;; or a file/directory name.
	;; If we're trying to `ls' a single file, this fails since we
	;; can't cd to a file.  We can't fix this problem here, tho, because
	;; at this point we don't know whether the argument is a file or
	;; a directory.  Such an `ls' is only ever used (apparently) from
	;; `insert-directory' when the `full-directory-p' argument is nil
	;; (which seems to only be used by dired when updating its display
	;; after operating on a set of files).  So we've changed
	;; `ange-ftp-insert-directory' such that in this case it gets
	;; a full listing of the directory and extracting the line
	;; corresponding to the requested file.
	(unless (equal cmd1 ".")
	  (setq result (ange-ftp-cd host user (nth 1 cmd) 'noerror)))
	(setq cmd1 cmd3)))

     ;; First argument is the remote name
     ((progn
	(setq fix-name-func (or (cdr (assq host-type
					   ange-ftp-fix-name-func-alist))
				'identity))
	(memq cmd0 '(get delete mkdir rmdir cd)))
      (setq cmd1 (funcall fix-name-func cmd1)))

     ;; Second argument is the remote name
     ((or (memq cmd0 '(append put chmod))
          (and (eq cmd0 'quote) (member cmd1 '("mdtm" "size"))))
      (setq cmd2 (funcall fix-name-func cmd2)))
     ;; Both arguments are remote names
     ((eq cmd0 'rename)
      (setq cmd1 (funcall fix-name-func cmd1)
	    cmd2 (funcall fix-name-func cmd2))))

    ;; Turn the command into one long string
    (setq cmd0 (symbol-name cmd0))
    (setq cmd (concat cmd0
		      (and cmd1 (concat " " cmd1))
		      (and cmd2 (concat " " cmd2))))

    ;; Actually send the resulting command.
    (if (and (consp result) (null (car result)))
	;; `ange-ftp-cd' has failed, so there's no point sending `cmd'.
	result
      (let (afsc-result
	    afsc-line)
	(ange-ftp-raw-send-cmd
	 (ange-ftp-get-process host user)
	 cmd
	 msg
	 (list (lambda (result line host user cmd msg cont nowait)
		 (or cont (setq afsc-result result
				afsc-line line))
		 (if result (ange-ftp-call-cont cont result line)
                   (ange-ftp-raw-send-cmd
                    (ange-ftp-get-process host user)
                    cmd
                    msg
                    (list (lambda (result line cont)
                            (or cont (setq afsc-result result
                                           afsc-line line))
                            (ange-ftp-call-cont cont result line))
                          cont)
		    nowait)))
	       host user cmd msg cont nowait)
	 nowait)

	(if nowait
	    nil
	  (if cont
	      nil
	    (cons afsc-result afsc-line)))))))

;; It might be nice to message users about the host type identified,
;; but there is so much other messaging going on, it would not be
;; seen. No point in slowing things down just so users can read
;; a host type message.

(defconst ange-ftp-cms-name-template
  (concat
   "^[-A-Z0-9$*][-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?"
   "[-A-Z0-9$*]?[-A-Z0-9$*]?[-A-Z0-9$*]?\\.[0-9][0-9][0-9A-Z]$"))
(defconst ange-ftp-vms-name-template
  "^[-A-Z0-9_$]+:\\[[-A-Z0-9_$]+\\(\\.[-A-Z0-9_$]+\\)*\\]$")
(defconst ange-ftp-mts-name-template
  "^[A-Z0-9._][A-Z0-9._][A-Z0-9._][A-Z0-9._]:$")
(defconst ange-ftp-bs2000-filename-pubset-regexp
  ":[A-Z0-9]+:"
  "Valid pubset for an BS2000 file name.")
(defconst ange-ftp-bs2000-filename-username-regexp
  (concat
   "\\$[A-Z0-9]*\\.")
  "Valid username for an BS2000 file name.")
(defconst ange-ftp-bs2000-filename-prefix-regexp
  (concat
   ange-ftp-bs2000-filename-pubset-regexp
   ange-ftp-bs2000-filename-username-regexp)
  "Valid prefix for an BS2000 file name (pubset and user).")
(defconst ange-ftp-bs2000-name-template
  (concat "^" ange-ftp-bs2000-filename-prefix-regexp "$"))

(defun ange-ftp-guess-host-type (host user)
  "Guess the host type of HOST.
Works by doing a pwd and examining the directory syntax."
  (let ((host-type (ange-ftp-host-type host))
	(key (concat host "/" user "/~")))
    (if (eq host-type 'unix)
	;; Note that ange-ftp-host-type returns unix as the default value.
	(save-match-data
	  (let* ((result (ange-ftp-get-pwd host user))
		 (dir (car result))
		 fix-name-func)
	    (cond ((null dir)
		   (message "Warning! Unable to get home directory")
		   (sit-for 1)
		   (if (string-match
			"^450 No current working directory defined$"
			(cdr result))

		       ;; We'll assume that if pwd bombs with this
		       ;; error message, then it's CMS.
		       (progn
			 (ange-ftp-add-cms-host host)
			 (setq ange-ftp-host-cache host
			       ange-ftp-host-type-cache 'cms))))

		  ;; try for VMS
		  ((string-match ange-ftp-vms-name-template dir)
		   (ange-ftp-add-vms-host host)
		   ;; The add-host functions clear the host type cache.
		   ;; Therefore, need to set the cache afterwards.
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'vms))

		  ;; try for MTS
		  ((string-match ange-ftp-mts-name-template dir)
		   (ange-ftp-add-mts-host host)
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'mts))

		  ;; try for CMS
		  ((string-match ange-ftp-cms-name-template dir)
		   (ange-ftp-add-cms-host host)
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'cms))

		  ;; try for BS2000-POSIX
		  ((ange-ftp-bs2000-posix-host host)
		   (ange-ftp-add-bs2000-host host)
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'text-unix))
		  ;; try for BS2000
		  ((and (string-match ange-ftp-bs2000-name-template dir)
			(not (ange-ftp-bs2000-posix-host host)))
		   (ange-ftp-add-bs2000-host host)
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'bs2000))
		  ;; assume UN*X
		  (t
		   (setq ange-ftp-host-cache host
			 ange-ftp-host-type-cache 'unix)))

	    ;; Now that we have done a pwd, might as well put it in
	    ;; the expand-dir hashtable.
	    (let ((ange-ftp-this-user user)
		  (ange-ftp-this-host host))
	      (setq fix-name-func (cdr (assq ange-ftp-host-type-cache
					     ange-ftp-fix-name-func-alist)))
	      (if fix-name-func
		  (setq dir (funcall fix-name-func dir 'reverse))))
	    (puthash key dir ange-ftp-expand-dir-hashtable))))

    ;; In the special case of CMS make sure that know the
    ;; expansion of the home minidisk now, because we will
    ;; be doing a lot of cd's.
    (if (and (eq host-type 'cms)
	     (not (ange-ftp-hash-entry-exists-p
		   key ange-ftp-expand-dir-hashtable)))
	(let ((dir (car (ange-ftp-get-pwd host user))))
	  (if dir
	      (puthash key (concat "/" dir) ange-ftp-expand-dir-hashtable)
	    (message "Warning! Unable to get home directory")
	    (sit-for 1))))))


;;;; ------------------------------------------------------------
;;;; Remote file and directory listing support.
;;;; ------------------------------------------------------------

;; Returns whether HOST's FTP server doesn't like \'ls\' or \'dir\' commands
;; to take switch arguments.
(defun ange-ftp-dumb-unix-host (host)
  (and host ange-ftp-dumb-unix-host-regexp
       (string-match-p ange-ftp-dumb-unix-host-regexp host)))

(defun ange-ftp-add-dumb-unix-host (host)
  "Interactively add a given HOST to `ange-ftp-dumb-unix-host-regexp'."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (car (ange-ftp-ftp-name name)))))))
  (if (not (ange-ftp-dumb-unix-host host))
      (setq ange-ftp-dumb-unix-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-dumb-unix-host-regexp "\\|")
		    ange-ftp-dumb-unix-host-regexp)
	    ange-ftp-host-cache nil)))

(defvar ange-ftp-parse-list-func-alist nil
  "Alist saying how to parse directory listings for certain OS types.
Association list of (TYPE . FUNC) pairs.  The FUNC is a routine which
can parse the output from a DIR listing for a host of type TYPE.")

;; With no-error nil, this function returns:
;; an error if file is not an ange-ftp-name
;;                      (This should never happen.)
;; an error if either the listing is unreadable or there is an ftp error.
;; the listing (a string), if everything works.
;;
;; With no-error t, it returns:
;; an error if not an ange-ftp-name
;; error if listing is unreadable (most likely caused by a slow connection)
;; nil if ftp error (this is because although asking to list a nonexistent
;;                   directory on a remote unix machine usually (except
;;                   maybe for dumb hosts) returns an ls error, but no
;;                   ftp error, if the same is done on a VMS machine,
;;                   an ftp error is returned. Need to trap the error
;;                   so we can go on and try to list the parent.)
;; the listing, if everything works.

;; If WILDCARD is non-nil, then this implements the guts of insert-directory
;; in the wildcard case.  Then we make a relative directory listing
;; of FILE within the directory specified by `default-directory'.

(defvar ange-ftp-before-parse-ls-hook nil
  "Normal hook run before parsing the text of an FTP directory listing.")

(defvar ange-ftp-after-parse-ls-hook nil
  "Normal hook run after parsing the text of an FTP directory listing.")

(defun ange-ftp-ls (file lsargs parse &optional no-error wildcard)
  "Return the output of a `DIR' or `ls' command done over FTP.
FILE is the full name of the remote file, LSARGS is any args to pass to the
`ls' command, and PARSE specifies that the output should be parsed and stored
away in the internal cache."
  (when (string-match "^--dired\\s-+" lsargs)
    (setq lsargs (replace-match "" nil t lsargs)))
  ;; If parse is t, we assume that file is a directory. i.e. we only parse
  ;; full directory listings.
  (let* ((ange-ftp-this-file (ange-ftp-expand-file-name file))
	 (parsed (ange-ftp-ftp-name ange-ftp-this-file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (name (ange-ftp-quote-string (nth 2 parsed)))
	       (key (directory-file-name ange-ftp-this-file))
	       (host-type (ange-ftp-host-type host user))
	       (dumb (memq host-type ange-ftp-dumb-host-types))
	       result
	       temp
	       lscmd parse-func)
	  (if (string-equal name "")
	      (setq name
		    (ange-ftp-real-file-name-as-directory
		     (ange-ftp-expand-dir host user "~"))))
	  (if (and ange-ftp-ls-cache-file
		   (string-equal key ange-ftp-ls-cache-file)
		   ;; Don't care about lsargs for dumb hosts.
		   (or dumb (string-equal lsargs ange-ftp-ls-cache-lsargs)))
	      ange-ftp-ls-cache-res
	    (setq temp (ange-ftp-make-tmp-name host))
	    (if wildcard
		(progn
		  (ange-ftp-cd host user (file-name-directory name))
		  (setq lscmd (list 'ls file temp lsargs)))
	      (setq lscmd (list 'dir name temp lsargs)))
	    (unwind-protect
		(if (car (setq result (ange-ftp-send-cmd
				       host
				       user
				       lscmd
				       (format "Listing %s"
					       (ange-ftp-abbreviate-filename
						ange-ftp-this-file)))))
		    (with-current-buffer (get-buffer-create
                                          ange-ftp-data-buffer-name)
		      (erase-buffer)
		      (if (ange-ftp-real-file-readable-p temp)
			  (ange-ftp-real-insert-file-contents temp)
			(sleep-for ange-ftp-retry-time)
					;wait for file to possibly appear
			(if (ange-ftp-real-file-readable-p temp)
			    ;; Try again.
			    (ange-ftp-real-insert-file-contents temp)
			  (ange-ftp-error host user
					  (format
					   "list data file %s not readable"
					   temp))))
                      ;; remove ^M inserted by the win32 ftp client
                      (while (re-search-forward "\r$" nil t)
                        (replace-match ""))
                      (goto-char 1)
		      (run-hooks 'ange-ftp-before-parse-ls-hook)
		      (if parse
			  (ange-ftp-set-files
			   ange-ftp-this-file
			   (if (setq
				parse-func
				(cdr (assq host-type
					   ange-ftp-parse-list-func-alist)))
			       (funcall parse-func)
			     (ange-ftp-parse-dired-listing lsargs))))
                      ;; Place this hook here to convert the contents of the
                      ;; buffer to a ls compatible format if the host system
                      ;; that is being queried is other than Unix i.e. VMS
                      ;; returns an ls format that really sucks.
                      (run-hooks 'ange-ftp-after-parse-ls-hook)
		      (setq ange-ftp-ls-cache-file key
			    ange-ftp-ls-cache-lsargs lsargs
					; For dumb hosts-types this is
					; meaningless but harmless.
			    ange-ftp-ls-cache-res (buffer-string))
		      ;; (kill-buffer (current-buffer))
		      (if (equal ange-ftp-ls-cache-res "total 0\n")
			  ;; wu-ftpd seems to return a successful result
			  ;; with an empty file-listing when doing a
			  ;; `DIR /some/file/.' which leads ange-ftp to
			  ;; believe that /some/file is a directory ;-(
			  nil
			ange-ftp-ls-cache-res))
		  (if no-error
		      nil
		    (ange-ftp-error host user
				    (concat "DIR failed: " (cdr result)))))
	      (ange-ftp-del-tmp-name temp))))
      (error "Should never happen. Please report. Bug ref. no.: 1"))))

;;;; ------------------------------------------------------------
;;;; Directory information caching support.
;;;; ------------------------------------------------------------

(defvar ange-ftp-add-file-entry-alist nil
  "Alist saying how to add file entries on certain OS types.
Association list of pairs (TYPE . FUNC), where FUNC is a function
to be used to add a file entry for the OS TYPE.
The main reason for this alist is to deal with file versions in VMS.")

(defvar ange-ftp-delete-file-entry-alist nil
  "Alist saying how to delete files on certain OS types.
Association list of pairs (TYPE . FUNC), where FUNC is a function
to be used to delete a file entry for the OS TYPE.
The main reason for this alist is to deal with file versions in VMS.")

(defun ange-ftp-add-file-entry (name &optional dir-p)
  "Add a file entry for file NAME, if its directory info exists."
  (funcall (or (cdr (assq (ange-ftp-host-type
			   (car (ange-ftp-ftp-name name)))
			  ange-ftp-add-file-entry-alist))
	       'ange-ftp-internal-add-file-entry)
	   name dir-p)
  (setq ange-ftp-ls-cache-file nil))

(defun ange-ftp-delete-file-entry (name &optional dir-p)
  "Delete the file entry for file NAME, if its directory info exists."
  (funcall (or (cdr (assq (ange-ftp-host-type
			   (car (ange-ftp-ftp-name name)))
			  ange-ftp-delete-file-entry-alist))
	       'ange-ftp-internal-delete-file-entry)
	   name dir-p)
  (setq ange-ftp-ls-cache-file nil))

(defmacro ange-ftp-parse-filename ()
  ;;Extract the filename from the current line of a dired-like listing.
  `(save-match-data
     (let ((eol (progn (end-of-line) (point))))
       (beginning-of-line)
       (if (re-search-forward directory-listing-before-filename-regexp eol t)
	   (buffer-substring (point) eol)))))

;; This deals with the F switch. Should also do something about
;; unquoting names obtained with the SysV b switch and the GNU Q
;; switch. See Sebastian's dired-get-filename.

(defun ange-ftp-ls-parser (switches)
  ;; Meant to be called by ange-ftp-parse-dired-listing
  (let ((tbl (make-hash-table :test 'equal))
	(used-F (and (stringp switches)
		     (string-match "F" switches)))
	file-type symlink directory file)
    (while (setq file (ange-ftp-parse-filename))
      (beginning-of-line)
      (skip-chars-forward "\t 0-9")
      (setq file-type (following-char)
	    directory (eq file-type ?d))
      (if (eq file-type ?l)
	  (let ((end (string-match " -> " file)))
	    (if end
		;; Sometimes `ls' appends a @ at the end of the target.
		(setq symlink (substring file (match-end 0)
					 (string-match "@\\'" file))
		      file (substring file 0 end))
	      ;; Shouldn't happen
	      (setq symlink "")))
	(setq symlink nil))
      ;; Only do a costly regexp search if the F switch was used.
      (if (and used-F
	       (not (string-equal file ""))
	       (looking-at
		".[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)[-r][-w]\\([^ ]\\)"))
	  (let ((socket (eq file-type ?s))
		(executable
		 (and (not symlink) ; x bits don't mean a thing for symlinks
		      (string-match
		       "[xst]"
		       (concat (match-string 1)
			       (match-string 2)
			       (match-string 3))))))
	    ;; Some ls's with the F switch mark symlinks with an @ (ULTRIX)
	    ;; and others don't. (sigh...) Beware, that some Unix's don't
	    ;; seem to believe in the F-switch
	    (if (or (and symlink (string-match "@\\'" file))
		    (and directory (string-match "/\\'" file))
		    (and executable (string-match "*\\'" file))
		    (and socket (string-match "=\\'" file)))
		(setq file (substring file 0 -1)))))
      (puthash file (or symlink directory) tbl)
      (forward-line 1))
    (puthash "." t tbl)
    (puthash ".." t tbl)
    tbl))

;;; The dl stuff for descriptive listings

(defvar ange-ftp-dl-dir-regexp nil
  "Regexp matching directories which are listed in dl format.
This regexp should not be anchored with a trailing `$', because it should
match subdirectories as well.")

(defun ange-ftp-add-dl-dir (dir)
  "Interactively add a DIR to `ange-ftp-dl-dir-regexp'."
  (interactive
   (list (read-string "Directory: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (ange-ftp-ftp-name name)
			     (file-name-directory name))))))
  (if (not (and ange-ftp-dl-dir-regexp
		(string-match ange-ftp-dl-dir-regexp dir)))
      (setq ange-ftp-dl-dir-regexp
	    (concat "^" (regexp-quote dir)
		    (and ange-ftp-dl-dir-regexp "\\|")
		    ange-ftp-dl-dir-regexp))))

(defmacro ange-ftp-dl-parser ()
  ;; Parse the current buffer, which is assumed to be a descriptive
  ;; listing, and return a hashtable.
  `(let ((tbl (make-hash-table :test 'equal)))
     (while (not (eobp))
       (puthash
        (buffer-substring (point)
                          (progn
                            (skip-chars-forward "^ /\n")
                            (point)))
        (eq (following-char) ?/)
        tbl)
       (forward-line 1))
     (puthash "." t tbl)
     (puthash ".." t tbl)
     tbl))

;; Parse the current buffer which is assumed to be in a dired-like listing
;; format, and return a hashtable as the result. If the listing is not really
;; a listing, then return nil.

(defun ange-ftp-parse-dired-listing (&optional switches)
  (save-match-data
    (cond
     ((looking-at "^total [0-9]+$")
      (forward-line 1)
      ;; Some systems put in a blank line here.
      (if (eolp) (forward-line 1))
      (ange-ftp-ls-parser switches))
     ((looking-at "[^\n]+\\( not found\\|: Not a directory\\)\n\\'")
      ;; It's an ls error message.
      nil)
     ((eobp) ; i.e. (zerop (buffer-size))
      ;; This could be one of:
      ;; (1) An Ultrix ls error message
      ;; (2) A listing with the A switch of an empty directory
      ;;     on a machine which doesn't give a total line.
      ;; (3) The twilight zone.
      ;; We'll assume (1) for now.
      nil)
     ((re-search-forward directory-listing-before-filename-regexp nil t)
      (beginning-of-line)
      (ange-ftp-ls-parser switches))
     ((re-search-forward "^[^ \n\t]+ +\\([0-9]+\\|-\\|=\\) " nil t)
      ;; It's a dl listing (I hope).
      ;; file is bound by the call to ange-ftp-ls
      (ange-ftp-add-dl-dir ange-ftp-this-file)
      (beginning-of-line)
      (ange-ftp-dl-parser))
     (t nil))))

(defun ange-ftp-set-files (directory files)
  "For a given DIRECTORY, set or change the associated FILES hashtable."
  (and files (puthash (file-name-as-directory directory)
		      files ange-ftp-files-hashtable)))

(defun ange-ftp-switches-ok (switches)
  "Return SWITCHES (a string) if suitable for our use."
  (and (stringp switches)
       ;; We allow the A switch, which lists all files except "." and
       ;; "..".  This is OK because we manually insert these entries
       ;; in the hash table.
       (string-match
	"--\\(almost-\\)?all\\>\\|\\(\\`\\| \\)-[[:alpha:]]*[aA]" switches)
       (string-match "\\(\\`\\| \\)-[[:alpha:]]*l" switches)
       (not (string-match
	     "--recursive\\>\\|\\(\\`\\| \\)-[[:alpha:]]*R" switches))
       switches))

(defun ange-ftp-get-files (directory &optional no-error)
  "Given a DIRECTORY, return a hashtable of file entries.
This will give an error or return nil, depending on the value of
NO-ERROR, if a listing for DIRECTORY cannot be obtained."
  (setq directory (file-name-as-directory directory)) ;normalize
  (or (gethash directory ange-ftp-files-hashtable)
      (save-match-data
	(and (ange-ftp-ls directory
			  ;; This is an efficiency hack. We try to
			  ;; anticipate what sort of listing dired
			  ;; might want, and cache just such a listing.
			  (or (and (boundp 'dired-actual-switches)
				   (ange-ftp-switches-ok dired-actual-switches))
			      (and (boundp 'dired-listing-switches)
				   (ange-ftp-switches-ok
				    dired-listing-switches))
			      "-al")
			  t no-error)
	     (gethash directory ange-ftp-files-hashtable)))))

;; Given NAME, return the file part that can be used for looking up the
;; file's entry in a hashtable.
(defmacro ange-ftp-get-file-part (name)
  `(let ((file (file-name-nondirectory ,name)))
     (if (string-equal file "")
	 "."
       file)))

;; Return whether ange-ftp-file-entry-p and ange-ftp-get-file-entry are
;; allowed to determine if NAME is a sub-directory by listing it directly,
;; rather than listing its parent directory. This is used for efficiency so
;; that a wasted listing is not done:
;; 1. When looking for a .dired file in dired-x.el.
;; 2. The syntax of FILE and DIR make it impossible that FILE could be a valid
;;     subdirectory. This is of course an OS dependent judgment.

(defvar dired-local-variables-file)
(defmacro ange-ftp-allow-child-lookup (dir file)
  `(not
    (let* ((efile ,file)		; expand once.
           (edir ,dir)
           (parsed (ange-ftp-ftp-name edir))
           (host-type (ange-ftp-host-type
                       (car parsed))))
      (or
       ;; Deal with dired
       (and (boundp 'dired-local-variables-file) ; in the dired-x package
	    (stringp dired-local-variables-file)
	    (string-equal dired-local-variables-file efile))
       ;; No dots in dir names in vms.
       (and (eq host-type 'vms)
	    (string-match "\\." efile))
       ;; No subdirs in mts of cms.
       (and (memq host-type '(mts cms))
	    (not (string-equal "/" (nth 2 parsed))))
       ;; No dots in pseudo-dir names in bs2000.
       (and (eq host-type 'bs2000)
	    (string-match "\\." efile))))))

(defun ange-ftp-file-entry-p (name)
  "Given NAME, return whether there is a file entry for it."
  (let* ((name (directory-file-name name))
	 (dir (file-name-directory name))
	 (ent (gethash dir ange-ftp-files-hashtable))
	 (file (ange-ftp-get-file-part name)))
    (if ent
	(ange-ftp-hash-entry-exists-p file ent)
      (or (and (ange-ftp-allow-child-lookup dir file)
	       (setq ent (ange-ftp-get-files name t))
	       ;; Try a child lookup. i.e. try to list file as a
	       ;; subdirectory of dir. This is a good idea because
	       ;; we may not have read permission for file's parent. Also,
	       ;; people tend to work down directory trees anyway. We use
	       ;; no-error ;; because if file does not exist as a subdir.,
	       ;; then dumb hosts will give an ftp error. Smart unix hosts
	       ;; will simply send back the ls
	       ;; error message.
	       (gethash "." ent))
	  ;; Child lookup failed, so try the parent.
	  (ange-ftp-hash-entry-exists-p
	   file (ange-ftp-get-files dir 'no-error))))))

(defun ange-ftp-get-file-entry (name)
  "Given NAME, return the given file entry.
The entry will be either t for a directory, nil for a normal file,
or a string for a symlink.  If the file isn't in the hashtable,
this also returns nil."
  (let* ((name (directory-file-name name))
	 (dir (file-name-directory name))
	 (ent (gethash dir ange-ftp-files-hashtable))
	 (file (ange-ftp-get-file-part name)))
    (if ent
	(gethash file ent)
      (or (and (ange-ftp-allow-child-lookup dir file)
	       (setq ent (ange-ftp-get-files name t))
	       (gethash "." ent))
	  ;; i.e. it's a directory by child lookup
	  (and (setq ent (ange-ftp-get-files dir t))
	       (gethash file ent))))))

(defun ange-ftp-internal-delete-file-entry (name &optional dir-p)
  (when dir-p
    (setq name (file-name-as-directory name))
    (remhash name ange-ftp-files-hashtable)
    (setq name (directory-file-name name)))
  ;; Note that file-name-as-directory followed by directory-file-name
  ;; serves to canonicalize directory file names to their unix form.
  ;; i.e. in VMS, FOO.DIR -> FOO/ -> FOO
  (let ((files (gethash (file-name-directory name) ange-ftp-files-hashtable)))
    (if files
	(remhash (ange-ftp-get-file-part name) files))))

(defun ange-ftp-internal-add-file-entry (name &optional dir-p)
  (and dir-p
       (setq name (directory-file-name name)))
  (let ((files (gethash (file-name-directory name) ange-ftp-files-hashtable)))
    (if files
	(puthash (ange-ftp-get-file-part name) dir-p files))))

(defun ange-ftp-wipe-file-entries (host user)
  "Get rid of entry for HOST, USER pair from file entry information hashtable."
  (let ((new-tbl (make-hash-table :test 'equal
				  :size (hash-table-size
					 ange-ftp-files-hashtable))))
    (maphash
     (lambda (key val)
       (let ((parsed (ange-ftp-ftp-name key)))
         (if parsed
             (let ((h (nth 0 parsed))
                   (u (nth 1 parsed)))
               (or (and (equal host h) (equal user u))
                   (puthash key val new-tbl))))))
     ange-ftp-files-hashtable)
    (setq ange-ftp-files-hashtable new-tbl)))

;;;; ------------------------------------------------------------
;;;; File transfer mode support.
;;;; ------------------------------------------------------------

(defun ange-ftp-set-binary-mode (host user)
  "Tell the FTP process for the given HOST & USER to switch to binary mode."
  ;; FIXME: We should keep track of the current mode, so as to avoid
  ;; unnecessary roundtrips.
  (let ((result (ange-ftp-send-cmd host user '(type "binary"))))
    (if (not (car result))
	(ange-ftp-error host user (concat "BINARY failed: " (cdr result)))
      (with-current-buffer (process-buffer (ange-ftp-get-process host user))
	(and ange-ftp-binary-hash-mark-size
	     (setq ange-ftp-hash-mark-unit
		   (ash ange-ftp-binary-hash-mark-size -4)))))))

(defun ange-ftp-set-ascii-mode (host user)
  "Tell the FTP process for the given HOST & USER to switch to ASCII mode."
  ;; FIXME: We should keep track of the current mode, so as to avoid
  ;; unnecessary roundtrips.
  (let ((result (ange-ftp-send-cmd host user '(type "ascii"))))
    (if (not (car result))
	(ange-ftp-error host user (concat "ASCII failed: " (cdr result)))
      (with-current-buffer (process-buffer (ange-ftp-get-process host user))
	(and ange-ftp-ascii-hash-mark-size
	     (setq ange-ftp-hash-mark-unit
		   (ash ange-ftp-ascii-hash-mark-size -4)))))))

(defun ange-ftp-cd (host user dir &optional noerror)
  (let ((result (ange-ftp-send-cmd host user (list 'cd dir) "Doing CD")))
    (if noerror result
      (or (car result)
	  (ange-ftp-error host user (concat "CD failed: " (cdr result)))))))

(defun ange-ftp-get-pwd (host user)
  "Attempt to get the current working directory for the given HOST/USER pair.
Returns (DIR . LINE) where DIR is either the directory or nil if not found,
and LINE is the relevant success or fail line from the FTP-client."
  (let* ((result (ange-ftp-send-cmd host user '(pwd) "Getting PWD"))
	 (line (cdr result))
	 dir)
    (if (car result)
	(save-match-data
	  (and (or (string-match "\"\\([^\"]*\\)\"" line)
		   (string-match " \\([^ ]+\\) " line))	; stone-age VMS servers!
	       (setq dir (match-string 1 line)))))
    (cons dir line)))

;;; ------------------------------------------------------------
;;; expand-file-name and friends...which currently don't work
;;; ------------------------------------------------------------

(defun ange-ftp-expand-dir (host user dir)
  "Return the result of doing a PWD in the current FTP session.
Use the connection to machine HOST
logged in as user USER and cd'd to directory DIR."
  (let* ((host-type (ange-ftp-host-type host user))
	 ;; It is more efficient to call ange-ftp-host-type
	 ;; before binding res, because ange-ftp-host-type sometimes
	 ;; adds to the info in the expand-dir-hashtable.
	 (fix-name-func
	  (cdr (assq host-type ange-ftp-fix-name-func-alist)))
	 (key (concat host "/" user "/" dir))
	 (res (gethash key ange-ftp-expand-dir-hashtable)))
    (or res
	(progn
	  (or
	   (string-equal user "anonymous")
	   (string-equal user "ftp")
	   (not (eq host-type 'unix))
	   (let* ((ange-ftp-good-msgs (concat ange-ftp-expand-dir-regexp
					      "\\|"
					      ange-ftp-good-msgs))
		  (result (ange-ftp-send-cmd host user
					     (list 'get dir null-device)
					     (format "expanding %s" dir)))
		  (line (cdr result)))
	     (setq res
		   (if (string-match ange-ftp-expand-dir-regexp line)
		       (match-string 1 line)))))
	  (or res
	      (if (string-equal dir "~")
		  (setq res (car (ange-ftp-get-pwd host user)))
		(let ((home (ange-ftp-expand-dir host user "~")))
		  (unwind-protect
		      (and (ange-ftp-cd host user dir)
			   (setq res (car (ange-ftp-get-pwd host user))))
		    (ange-ftp-cd host user home)))))
	  (if res
	      (let ((ange-ftp-this-user user)
		    (ange-ftp-this-host host))
		(if fix-name-func
		    (setq res (funcall fix-name-func res 'reverse)))
		(puthash key res ange-ftp-expand-dir-hashtable)))
	  res))))

(defun ange-ftp-canonize-filename (n)
  "Take a string N and short-circuit //, /. and /.."
  (if (string-match "[^:]+//" n)		;don't upset Apollo users
      (setq n (substring n (1- (match-end 0)))))
  (let ((parsed (ange-ftp-ftp-name n)))
    (if parsed
	(let ((host (car parsed))
	      (user (nth 1 parsed))
	      (name (nth 2 parsed)))

	  ;; See if remote name is absolute.  If so then just expand it and
	  ;; replace the name component of the overall name.
	  (cond ((string-match "\\`/" name)
		 name)

		;; Name starts with ~ or ~user.  Resolve that part of the name
		;; making it absolute then re-expand it.
		((string-match "\\`~[^/]*" name)
		 (let* ((tilda (match-string 0 name))
			(rest (substring name (match-end 0)))
			(dir (ange-ftp-expand-dir host user tilda)))
		   (if dir
                       ;; C-x d /ftp:anonymous@ftp.gnu.org:~/ RET
                       ;; seems to cause `rest' to sometimes be empty.
                       ;; Maybe it's an error for `rest' to be empty here,
                       ;; but until we figure this out, this quick fix
                       ;; seems to do the trick.
		       (setq name (cond ((string-equal rest "") dir)
					((string-equal dir "/") rest)
					(t (concat dir rest))))
		     (error "User \"%s\" is not known"
			    (substring tilda 1)))))

		;; relative name.  Tack on homedir and re-expand.
		(t
		 (let ((dir (ange-ftp-expand-dir host user "~")))
		   (if dir
		       (setq name (concat
				   (ange-ftp-real-file-name-as-directory dir)
				   name))
		     (error "Unable to obtain CWD")))))

	  ;; If name starts with //, preserve that, for apollo system.
	  (unless (string-match "\\`//" name)
            (if (not (eq system-type 'windows-nt))
                (setq name (ange-ftp-real-expand-file-name name))
              ;; Windows UNC default dirs do not make sense for ftp.
              (setq name (if (and default-directory
				  (string-match "\\`//" default-directory))
                             (ange-ftp-real-expand-file-name name "c:/")
                           (ange-ftp-real-expand-file-name name)))
              ;; Strip off possible drive specifier.
              (if (string-match "\\`[a-zA-Z]:" name)
                  (setq name (substring name 2))))
            (if (string-match "\\`//" name)
                (setq name (substring name 1))))

	  ;; Now substitute the expanded name back into the overall filename.
	  (ange-ftp-replace-name-component n name))

      ;; non-ange-ftp name.  Just expand normally.
      (if (eq (string-to-char n) ?/)
	  (ange-ftp-real-expand-file-name n)
	(ange-ftp-real-expand-file-name
	 (ange-ftp-real-file-name-nondirectory n)
	 (ange-ftp-real-file-name-directory n))))))

(defun ange-ftp-expand-file-name (name &optional default)
  "Documented as `expand-file-name'."
  (save-match-data
    (setq default (or default default-directory))
    (cond ((eq (string-to-char name) ?~)
	   (ange-ftp-real-expand-file-name name))
	  ((eq (string-to-char name) ?/)
	   (ange-ftp-canonize-filename name))
	  ((and (eq system-type 'windows-nt)
		(eq (string-to-char name) ?\\))
	   (ange-ftp-canonize-filename name))
	  ((and (eq system-type 'windows-nt)
		(or (string-match "\\`[a-zA-Z]:" name)
		    (string-match "\\`[a-zA-Z]:" default)))
	   (ange-ftp-real-expand-file-name name default))
	  ((zerop (length name))
	   (ange-ftp-canonize-filename default))
	  ((ange-ftp-canonize-filename
	    (concat (file-name-as-directory default) name))))))

;;; These are problems--they are currently not enabled.

(defvar ange-ftp-file-name-as-directory-alist nil
  "Association list of (TYPE . FUNC) pairs.
FUNC converts a filename to a directory name for the operating
system TYPE.")

(defun ange-ftp-file-name-as-directory (name)
  "Documented as `file-name-as-directory'."
  (let ((parsed (ange-ftp-ftp-name name)))
    (if parsed
	(if (string-equal (nth 2 parsed) "")
	    name
	  (funcall (or (cdr (assq
			     (ange-ftp-host-type (car parsed))
			     ange-ftp-file-name-as-directory-alist))
		       'ange-ftp-real-file-name-as-directory)
		   name))
      (ange-ftp-real-file-name-as-directory name))))

(defun ange-ftp-file-name-directory (name)
  "Documented as `file-name-directory'."
  (let ((parsed (ange-ftp-ftp-name name)))
    (if parsed
	(let ((filename (nth 2 parsed)))
	  (if (string-match-p "\\`~[^/]*\\'" filename)
	      name
	    (ange-ftp-replace-name-component
	     name
	     (ange-ftp-real-file-name-directory filename))))
      (ange-ftp-real-file-name-directory name))))

(defun ange-ftp-file-name-nondirectory (name)
  "Documented as `file-name-nondirectory'."
  (let ((parsed (ange-ftp-ftp-name name)))
    (if parsed
	(let ((filename (nth 2 parsed)))
	  (if (string-match-p "\\`~[^/]*\\'" filename)
	      ""
	    (ange-ftp-real-file-name-nondirectory filename)))
      (ange-ftp-real-file-name-nondirectory name))))

(defun ange-ftp-directory-file-name (dir)
  "Documented as `directory-file-name'."
  (let ((parsed (ange-ftp-ftp-name dir)))
    (if parsed
	(ange-ftp-replace-name-component
	 dir
	 (ange-ftp-real-directory-file-name (nth 2 parsed)))
      (ange-ftp-real-directory-file-name dir))))


;;; Hooks that handle Emacs primitives.

;; Returns non-nil if should transfer FILE in binary mode.
(defun ange-ftp-binary-file (file)
  (string-match-p ange-ftp-binary-file-name-regexp file))

(defun ange-ftp-write-region (start end filename &optional append visit)
  (setq filename (expand-file-name filename))
  (let ((parsed (ange-ftp-ftp-name filename)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (name (ange-ftp-quote-string (nth 2 parsed)))
	       (temp (ange-ftp-make-tmp-name host))
	       ;; What we REALLY need here is a way to determine if the mode
	       ;; of the transfer is irrelevant, i.e. we can use binary mode
	       ;; regardless. Maybe a system-type to host-type lookup?
	       (binary (ange-ftp-binary-file filename))
	       (cmd (if append 'append 'put))
	       (abbr (ange-ftp-abbreviate-filename filename))
	       ;; we need to reset `last-coding-system-used' to its
	       ;; value immediately after calling the real write-region,
	       ;; so that `basic-save-buffer' doesn't see whatever value
	       ;; might be used when communicating with the ftp process.
	       (coding-system-used last-coding-system-used))
	  (unwind-protect
	      (progn
		(let ((filename (buffer-file-name))
		      (mod-p (buffer-modified-p)))
		  (unwind-protect
		      (progn
			(ange-ftp-real-write-region start end temp nil
						    (or visit 'quiet))
			(setq coding-system-used last-coding-system-used))
		    ;; cleanup forms
		    (setq coding-system-used last-coding-system-used)
		    (setq buffer-file-name filename)
		    (restore-buffer-modified-p mod-p)))
		(if binary
		    (ange-ftp-set-binary-mode host user))

		;; tell the process filter what size the transfer will be.
		(let ((attr (file-attributes temp)))
		  (if attr
		      (ange-ftp-set-xfer-size host user (nth 7 attr))))

		;; put or append the file.
		(let ((result (ange-ftp-send-cmd host user
						 (list cmd temp name)
						 (format "Writing %s" abbr))))
		  (or (car result)
		      (signal 'ftp-error
			      (list
			       "Opening output file"
			       (format "FTP Error: \"%s\"" (cdr result))
			       filename)))))
	    (ange-ftp-del-tmp-name temp)
	    (if binary
		(ange-ftp-set-ascii-mode host user)))
	  (if (eq visit t)
	      (progn
		(set-visited-file-modtime (ange-ftp-file-modtime filename))
		(ange-ftp-set-buffer-mode)
		(setq buffer-file-name filename)
		(set-buffer-modified-p nil)))
	  ;; ensure `last-coding-system-used' has an appropriate value
	  (setq last-coding-system-used coding-system-used)
	  (ange-ftp-message "Wrote %s" abbr)
	  (ange-ftp-add-file-entry filename))
      (ange-ftp-real-write-region start end filename append visit))))

(defun ange-ftp-insert-file-contents (filename &optional visit beg end replace)
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (let ((parsed (ange-ftp-ftp-name filename)))
    (if parsed
	(progn
	  (if visit
	      (setq buffer-file-name filename))
	  (if (or (file-exists-p filename)
		  (progn
		    (setq ange-ftp-ls-cache-file nil)
		    (remhash (file-name-directory filename)
			     ange-ftp-files-hashtable)
		    (file-exists-p filename)))
	      (let* ((host (nth 0 parsed))
		     (user (nth 1 parsed))
		     (name (ange-ftp-quote-string (nth 2 parsed)))
		     (temp (ange-ftp-make-tmp-name host))
		     (binary (ange-ftp-binary-file filename))
		     (buffer-file-type buffer-file-type)
		     (abbr (ange-ftp-abbreviate-filename filename))
		     (coding-system-used last-coding-system-used)
		     size)
		(unwind-protect
		    (progn
		      (if binary
			  (ange-ftp-set-binary-mode host user))
		      (let ((result (ange-ftp-send-cmd host user
					      (list 'get name temp)
					      (format "Retrieving %s" abbr))))
			(or (car result)
			    (signal 'ftp-error
				    (list
				     "Opening input file"
				     (format "FTP Error: \"%s\"" (cdr result))
				     filename))))
		      (if (or (ange-ftp-real-file-readable-p temp)
			      (sleep-for ange-ftp-retry-time)
			      ;; Wait for file to hopefully appear.
			      (ange-ftp-real-file-readable-p temp))
			  (setq
			   size
			   (nth 1 (ange-ftp-real-insert-file-contents
				   temp visit beg end replace))
			   coding-system-used last-coding-system-used
			   ;; override autodetection of buffer file type
			   ;; to ensure buffer is saved in DOS format
			   buffer-file-type binary)
			(signal 'ftp-error
				(list
				 "Opening input file:"
				 (format
				  "FTP Error: %s not arrived or readable"
				  filename)))))
		  (if binary
		      ;; We must keep `last-coding-system-used'
		      ;; unchanged.
		      (let (last-coding-system-used)
			(ange-ftp-set-ascii-mode host user)))
		  (ange-ftp-del-tmp-name temp))
		(if visit
		    (progn
		      (set-visited-file-modtime
                       (ange-ftp-file-modtime filename))
		      (setq buffer-file-name filename)))
		(setq last-coding-system-used coding-system-used)
		(list filename size))
	    (signal 'file-error
		    (list
		     "Opening input file"
		     filename))))
      (ange-ftp-real-insert-file-contents filename visit beg end replace))))

(defun ange-ftp-expand-symlink (file dir)
  (let ((res (if (file-name-absolute-p file)
		 (ange-ftp-replace-name-component dir file)
	       (expand-file-name file dir))))
    (if (file-symlink-p res)
	(ange-ftp-expand-symlink
	 (ange-ftp-get-file-entry res)
	 (file-name-directory (directory-file-name res)))
      res)))

(defun ange-ftp-file-symlink-p (file)
  ;; call ange-ftp-expand-file-name rather than the normal
  ;; expand-file-name to stop loops when using a package that
  ;; redefines both file-symlink-p and expand-file-name.
  (setq file (ange-ftp-expand-file-name file))
  (if (ange-ftp-ftp-name file)
      (condition-case nil
	  (let ((ent (ange-ftp-get-files (file-name-directory file))))
	    (and ent
		 (stringp (setq ent
				(gethash (ange-ftp-get-file-part file) ent)))
		 ent))
	;; If we can't read the parent directory, just assume
	;; this file is not a symlink.
	;; This makes it possible to access a directory that
	;; whose parent is not readable.
	(file-error nil))
    (ange-ftp-real-file-symlink-p file)))

(defun ange-ftp-file-exists-p (name)
  (setq name (expand-file-name name))
  (if (ange-ftp-ftp-name name)
      (if (ange-ftp-file-entry-p name)
	  (let ((file-ent (ange-ftp-get-file-entry name)))
	    (if (stringp file-ent)
		(file-exists-p
		 (ange-ftp-expand-symlink file-ent
					  (file-name-directory
					   (directory-file-name name))))
	      t)))
    (ange-ftp-real-file-exists-p name)))

(defun ange-ftp-file-directory-p (name)
  (setq name (expand-file-name name))
  (if (ange-ftp-ftp-name name)
      ;; We do a file-name-as-directory on name here because some
      ;; machines (VMS) use a .DIR to indicate the filename associated
      ;; with a directory. This needs to be canonicalized.
      (let ((file-ent (ange-ftp-get-file-entry
		       (ange-ftp-file-name-as-directory name))))
	(if (stringp file-ent)
	    ;; Calling file-directory-p doesn't work because ange-ftp
	    ;; is temporarily disabled for this operation.
	    (ange-ftp-file-directory-p
	     (ange-ftp-expand-symlink file-ent
				      (file-name-directory
				       (directory-file-name name))))
	  file-ent))
    (ange-ftp-real-file-directory-p name)))

(defun ange-ftp-directory-files (directory &optional full match
					   &rest v19-args)
  (setq directory (expand-file-name directory))
  (if (ange-ftp-ftp-name directory)
      (progn
	(ange-ftp-barf-if-not-directory directory)
	(let ((tail (ange-ftp-hash-table-keys
		     (ange-ftp-get-files directory)))
	      files f)
	  (setq directory (file-name-as-directory directory))
	  (while tail
	    (setq f (car tail)
		  tail (cdr tail))
	    (if (or (not match) (string-match-p match f))
		(setq files
		      (cons (if full (concat directory f) f) files))))
	  (nreverse files)))
    (apply 'ange-ftp-real-directory-files directory full match v19-args)))

(defun ange-ftp-directory-files-and-attributes
  (directory &optional full match nosort id-format)
  (setq directory (expand-file-name directory))
  (if (ange-ftp-ftp-name directory)
      (mapcar
       (lambda (file)
	 (cons file (file-attributes (expand-file-name file directory))))
       (ange-ftp-directory-files directory full match nosort))
    (ange-ftp-real-directory-files-and-attributes
     directory full match nosort id-format)))

(defun ange-ftp-file-attributes (file &optional id-format)
  (setq file (expand-file-name file))
  (let ((parsed (ange-ftp-ftp-name file)))
    (if parsed
	(let ((part (ange-ftp-get-file-part file))
	      (files (ange-ftp-get-files (file-name-directory file))))
	  (if (ange-ftp-hash-entry-exists-p part files)
	      (let ((host (nth 0 parsed))
		    (user (nth 1 parsed))
		    (name (nth 2 parsed))
		    (dirp (gethash part files))
		    (inode (gethash file ange-ftp-inodes-hashtable)))
		(unless inode
		  (setq inode ange-ftp-next-inode-number
			ange-ftp-next-inode-number (1+ inode))
		  (puthash file inode ange-ftp-inodes-hashtable))
		(list (if (and (stringp dirp) (file-name-absolute-p dirp))
			  (ange-ftp-expand-symlink dirp
						   (file-name-directory file))
			dirp)		;0 file type
		      -1		;1 link count
		      -1		;2 uid
		      -1		;3 gid
		      '(0 0)		;4 atime
		      (ange-ftp-file-modtime file) ;5 mtime
		      '(0 0)		;6 ctime
		      (ange-ftp-file-size file)	;7 size
		      (concat (if (stringp dirp) "l" (if dirp "d" "-"))
			      "?????????") ;8 mode
		      nil		;9 gid weird
		      inode		;10 "inode number".
		      -1		;11 device number [v19 only]
		      ))))
      (if id-format
	  (ange-ftp-real-file-attributes file id-format)
	(ange-ftp-real-file-attributes file)))))

(defun ange-ftp-file-newer-than-file-p (f1 f2)
  (let ((f1-parsed (ange-ftp-ftp-name f1))
        (f2-parsed (ange-ftp-ftp-name f2)))
    (if (or f1-parsed f2-parsed)
        (let ((f1-mt (nth 5 (file-attributes f1)))
              (f2-mt (nth 5 (file-attributes f2))))
          (cond ((null f1-mt) nil)
                ((null f2-mt) t)
                (t (> (float-time f1-mt) (float-time f2-mt)))))
      (ange-ftp-real-file-newer-than-file-p f1 f2))))

(defun ange-ftp-file-writable-p (file)
  (let ((ange-ftp-process-verbose nil))
    (setq file (expand-file-name file))
    (if (ange-ftp-ftp-name file)
	(or (file-exists-p file)	;guess here for speed
	    (file-directory-p (file-name-directory file)))
      (ange-ftp-real-file-writable-p file))))

(defun ange-ftp-file-readable-p (file)
  (let ((ange-ftp-process-verbose nil))
    (setq file (expand-file-name file))
    (if (ange-ftp-ftp-name file)
	(file-exists-p file)
      (ange-ftp-real-file-readable-p file))))

(defun ange-ftp-file-executable-p (file)
  (let ((ange-ftp-process-verbose nil))
    (setq file (expand-file-name file))
    (if (ange-ftp-ftp-name file)
	(file-exists-p file)
      (ange-ftp-real-file-executable-p file))))

(defun ange-ftp-delete-file (file &optional trash)
  (interactive (list (read-file-name "Delete file: " nil default-directory)
		     (null current-prefix-arg)))
  (setq file (expand-file-name file))
  (let ((parsed (ange-ftp-ftp-name file)))
    (if parsed
	(let* ((host (nth 0 parsed))
	       (user (nth 1 parsed))
	       (name (ange-ftp-quote-string (nth 2 parsed)))
	       (abbr (ange-ftp-abbreviate-filename file))
	       (result (ange-ftp-send-cmd host user
					  (list 'delete name)
					  (format "Deleting %s" abbr))))
	  (or (car result)
	      (signal 'ftp-error
		      (list
		       "Removing old name"
		       (format "FTP Error: \"%s\"" (cdr result))
		       file)))
	  (ange-ftp-delete-file-entry file))
      (ange-ftp-real-delete-file file trash))))

(defun ange-ftp-file-modtime (file)
  "Return the modification time of remote file FILE.
Value is (0 0) if the modification time cannot be determined."
  (let* ((parsed (ange-ftp-ftp-name file))
	 ;; At least one FTP server (wu-ftpd) can return a "226
	 ;; Transfer complete" before the "213 MODTIME".  Let's skip
	 ;; that.
	 (ange-ftp-skip-msgs (concat ange-ftp-skip-msgs "\\|^226"))
         (res (ange-ftp-send-cmd (car parsed) (cadr parsed)
                                 (list 'quote "mdtm" (cadr (cdr parsed)))))
	 (line (cdr res))
	 (modtime '(0 0)))
    ;; MDTM should return "213 YYYYMMDDhhmmss" GMT on success
    ;; following the Internet draft for FTP extensions.
    ;; Bob@rattlesnake.com reports that is returns something different
    ;; for at least one FTP server.  So, let's use the response only
    ;; if it matches the Internet draft.
    (when (string-match-p "^213 [0-9]\\{14\\}$" line)
      (setq modtime
	    (encode-time
	     (string-to-number (substring line 16 18))
	     (string-to-number (substring line 14 16))
	     (string-to-number (substring line 12 14))
	     (string-to-number (substring line 10 12))
	     (string-to-number (substring line  8 10))
	     (string-to-number (substring line  4  8))
	     0)))
    modtime))

(defun ange-ftp-verify-visited-file-modtime (buf)
  (let ((name (buffer-file-name buf)))
    (if (and (stringp name) (ange-ftp-ftp-name name))
        (let ((file-mdtm (ange-ftp-file-modtime name))
              (buf-mdtm (with-current-buffer buf (visited-file-modtime))))
          (or (zerop (car file-mdtm))
              (<= (float-time file-mdtm) (float-time buf-mdtm))))
      (ange-ftp-real-verify-visited-file-modtime buf))))

(defun ange-ftp-file-size (file &optional ascii-mode)
  "Return the size of remote file FILE. Return -1 if can't get it.
If ascii-mode is non-nil, return the size with the extra octets that
need to be inserted, one at the end of each line, to provide correct
end-of-line semantics for a transfer using TYPE=A. The default is nil,
so return the size on the remote host exactly. See RFC 3659."
  (let* ((parsed (ange-ftp-ftp-name file))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (name (ange-ftp-quote-string (nth 2 parsed)))
	 ;; At least one FTP server (wu-ftpd) can return a "226
	 ;; Transfer complete" before the "213 SIZE".  Let's skip
	 ;; that.
	 (ange-ftp-skip-msgs (concat ange-ftp-skip-msgs "\\|^226"))
	 (res (unwind-protect
                  (progn
                    (unless ascii-mode
                      (ange-ftp-set-binary-mode host user))
                    (ange-ftp-send-cmd host user (list 'quote "size" name)))
		(unless ascii-mode
		  (ange-ftp-set-ascii-mode host user))))
	 (line (cdr res)))
    (if (string-match "^213 \\([0-9]+\\)$" line)
	(string-to-number (match-string 1 line))
      -1)))


;;;; ------------------------------------------------------------
;;;; File copying support... totally re-written 6/24/92.
;;;; ------------------------------------------------------------

(defun ange-ftp-barf-or-query-if-file-exists (absname querystring interactive)
  (if (file-exists-p absname)
      (if (not interactive)
	  (signal 'file-already-exists (list absname))
	(if (not (yes-or-no-p (format "File %s already exists; %s anyway? "
				      absname querystring)))
	    (signal 'file-already-exists (list absname))))))

;; async local copy commented out for now since I don't seem to get
;; the process sentinel called for some processes.
;;
;; (defun ange-ftp-copy-file-locally (filename newname ok-if-already-exists
;; 					    keep-date cont)
;;   "Kludge to copy a local file and call a continuation when the copy
;; finishes."
;;   ;; check to see if we can overwrite
;;   (if (or (not ok-if-already-exists)
;; 	  (numberp ok-if-already-exists))
;;       (ange-ftp-barf-or-query-if-file-exists newname "copy to it"
;; 					     (numberp ok-if-already-exists)))
;;   (let ((proc (start-process " *copy*"
;; 			     (generate-new-buffer "*copy*")
;; 			     "cp"
;; 			     filename
;; 			     newname))
;; 	res)
;;     (set-process-sentinel proc 'ange-ftp-copy-file-locally-sentinel)
;;     (process-kill-without-query proc)
;;     (with-current-buffer (process-buffer proc)
;;       (set (make-local-variable 'copy-cont) cont))))
;;
;; (defun ange-ftp-copy-file-locally-sentinel (proc status)
;;   (with-current-buffer (process-buffer proc)
;;     (let ((cont copy-cont)
;; 	  (result (buffer-string)))
;;       (unwind-protect
;; 	  (if (and (string-equal status "finished\n")
;; 		   (zerop (length result)))
;; 	      (ange-ftp-call-cont cont t nil)
;; 	    (ange-ftp-call-cont cont
;; 				nil
;; 				(if (zerop (length result))
;; 				    (substring status 0 -1)
;; 				  (substring result 0 -1))))
;; 	(kill-buffer (current-buffer))))))

;; this is the extended version of ange-ftp-copy-file-internal that works
;; asynchronously if asked nicely.
(defun ange-ftp-copy-file-internal (filename newname ok-if-already-exists
					     keep-date &optional msg cont nowait)
  (setq filename (expand-file-name filename)
	newname (expand-file-name newname))

  (or (file-exists-p filename)
      (signal 'file-error
	      (list "Copy file" "no such file or directory" filename)))

  ;; canonicalize newname if a directory.
  (if (file-directory-p newname)
      (setq newname (expand-file-name (file-name-nondirectory filename) newname)))

  (let ((f-parsed (ange-ftp-ftp-name filename))
	(t-parsed (ange-ftp-ftp-name newname)))

    ;; local file to local file copy?
    (if (and (not f-parsed) (not t-parsed))
	(progn
	  (ange-ftp-real-copy-file filename newname ok-if-already-exists
				   keep-date)
	  (if cont
	      (ange-ftp-call-cont cont t "Copied locally")))
      ;; one or both files are remote.
      (let* ((f-host (and f-parsed (nth 0 f-parsed)))
	     (f-user (and f-parsed (nth 1 f-parsed)))
	     (f-name (and f-parsed (ange-ftp-quote-string (nth 2 f-parsed))))
	     (f-abbr (ange-ftp-abbreviate-filename filename))
	     (t-host (and t-parsed (nth 0 t-parsed)))
	     (t-user (and t-parsed (nth 1 t-parsed)))
	     (t-name (and t-parsed (ange-ftp-quote-string (nth 2 t-parsed))))
	     (t-abbr (ange-ftp-abbreviate-filename newname filename))
	     (binary (or (ange-ftp-binary-file filename)
			 (ange-ftp-binary-file newname)))
	     temp1
	     temp2)

	;; check to see if we can overwrite
	(if (or (not ok-if-already-exists)
		(numberp ok-if-already-exists))
	    (ange-ftp-barf-or-query-if-file-exists newname "copy to it"
						   (numberp ok-if-already-exists)))

	;; do the copying.
	(if f-parsed

	    ;; filename was remote.
	    (progn
	      (if (or (ange-ftp-use-gateway-p f-host)
		      t-parsed)
		  ;; have to use intermediate file if we are getting via
		  ;; gateway machine or we are doing a remote to remote copy.
		  (setq temp1 (ange-ftp-make-tmp-name f-host)))

	      (if binary
		  (ange-ftp-set-binary-mode f-host f-user))

	      (ange-ftp-send-cmd
	       f-host
	       f-user
	       (list 'get f-name (or temp1 (ange-ftp-quote-string newname)))
	       (or msg
		   (if (and temp1 t-parsed)
		       (format "Getting %s" f-abbr)
		     (format "Copying %s to %s" f-abbr t-abbr)))
	       (list 'ange-ftp-cf1
		     filename newname binary msg
		     f-parsed f-host f-user f-name f-abbr
		     t-parsed t-host t-user t-name t-abbr
		     temp1 temp2 cont nowait)
	       nowait))

	  ;; filename wasn't remote.  newname must be remote.  call the
	  ;; function which does the remainder of the copying work.
	  (ange-ftp-cf1 t nil
			filename newname binary msg
			f-parsed f-host f-user f-name f-abbr
			t-parsed t-host t-user t-name t-abbr
			nil nil cont nowait))))))

(defvar ange-ftp-waiting-flag nil)

;; next part of copying routine.
(defun ange-ftp-cf1 (result line
			    filename newname binary msg
			    f-parsed f-host f-user f-name f-abbr
			    t-parsed t-host t-user t-name t-abbr
			    temp1 temp2 cont nowait)
  (if line
      ;; filename must have been remote, and we must have just done a GET.
      (unwind-protect
	  (or result
	      ;; GET failed for some reason.  Clean up and get out.
	      (progn
		(and temp1 (ange-ftp-del-tmp-name temp1))
		(or cont
		    (if ange-ftp-waiting-flag
			(throw 'ftp-error t)
		      (signal 'ftp-error
			      (list "Opening input file"
				    (format "FTP Error: \"%s\"" line)
				    filename))))))
	;; cleanup
	(if binary
	    (ange-ftp-set-ascii-mode f-host f-user))))

  (if result
      ;; We now have to copy either temp1 or filename to newname.
      (if t-parsed

	  ;; newname was remote.
	  (progn
	    (if (ange-ftp-use-gateway-p t-host)
		(setq temp2 (ange-ftp-make-tmp-name t-host)))

	    ;; make sure data is moved into the right place for the
	    ;; outgoing transfer.  gateway temporary files complicate
	    ;; things nicely.
	    (if temp1
		(if temp2
		    (if (string-equal temp1 temp2)
			(setq temp1 nil)
		      (ange-ftp-real-copy-file temp1 temp2 t))
		  (setq temp2 temp1 temp1 nil))
	      (if temp2
		  (ange-ftp-real-copy-file filename temp2 t)))

	    (if binary
		(ange-ftp-set-binary-mode t-host t-user))

	    ;; tell the process filter what size the file is.
	    (let ((attr (file-attributes (or temp2 filename))))
	      (if attr
		  (ange-ftp-set-xfer-size t-host t-user (nth 7 attr))))

	    (ange-ftp-send-cmd
	     t-host
	     t-user
	     (list 'put (or temp2 (ange-ftp-quote-string filename)) t-name)
	     (or msg
		 (if (and temp2 f-parsed)
		     (format "Putting %s" newname)
		   (format "Copying %s to %s" f-abbr t-abbr)))
	     (list 'ange-ftp-cf2
		   newname t-host t-user binary temp1 temp2 cont)
	     nowait))

	;; newname wasn't remote.
	(ange-ftp-cf2 t nil newname t-host t-user binary temp1 temp2 cont))

    ;; first copy failed, tell caller
    (ange-ftp-call-cont cont result line)))

;; last part of copying routine.
(defun ange-ftp-cf2 (result line newname t-host t-user binary temp1 temp2 cont)
  (unwind-protect
      (if line
	  ;; result from doing a local to remote copy.
	  (unwind-protect
	      (progn
		(or result
		    (or cont
			(if ange-ftp-waiting-flag
			    (throw 'ftp-error t)
			  (signal 'ftp-error
				  (list "Opening output file"
					(format "FTP Error: \"%s\"" line)
					newname)))))

		(ange-ftp-add-file-entry newname))

	    ;; cleanup.
	    (if binary
		(ange-ftp-set-ascii-mode t-host t-user)))

	;; newname was local.
	(if temp1
	    (ange-ftp-real-copy-file temp1 newname t)))

    ;; clean up
    (and temp1 (ange-ftp-del-tmp-name temp1))
    (and temp2 (ange-ftp-del-tmp-name temp2))
    (ange-ftp-call-cont cont result line)))

(defun ange-ftp-copy-file (filename newname &optional ok-if-already-exists
				    keep-date preserve-uid-gid
				    preserve-selinux-context)
  (interactive "fCopy file: \nFCopy %s to file: \np")
  (ange-ftp-copy-file-internal filename
			       newname
			       ok-if-already-exists
			       keep-date
			       nil
			       nil
			       (called-interactively-p 'interactive)))

(defun ange-ftp-copy-files-async (okay-p line verbose-p files)
  "Copy some files in the background.
OKAY-P must be t, and LINE does not matter.  They are here to make this
 function a valid CONT argument for `ange-ftp-raw-send-cmd'.
If VERBOSE-P is non-nil, print progress report in the echo area.
 When all the files have been copied already, a message is shown anyway.
FILES is a list of files to copy in the form
  (from-file to-file ok-if-already-exists keep-date)
E.g.,
  (ange-ftp-copy-files-async t nil t '((\"a\" \"b\" t t) (\"c\" \"d\" t t)))"
  (unless okay-p (error "%s: %s" 'ange-ftp-copy-files-async line))
  (if files
      (let* ((ff (car files))
             (from-file    (nth 0 ff))
             (to-file      (nth 1 ff))
             (ok-if-exists (nth 2 ff))
             (keep-date    (nth 3 ff)))
        (ange-ftp-copy-file-internal
         from-file to-file ok-if-exists keep-date
         (and verbose-p (format "%s --> %s" from-file to-file))
         (list 'ange-ftp-copy-files-async verbose-p (cdr files))
         t))
    (message "%s: done" 'ange-ftp-copy-files-async)))


;;;; ------------------------------------------------------------
;;;; File renaming support.
;;;; ------------------------------------------------------------

(defun ange-ftp-rename-remote-to-remote (filename newname f-parsed t-parsed)
  "Rename remote file FILENAME to remote file NEWNAME."
  (let ((f-host (nth 0 f-parsed))
	(f-user (nth 1 f-parsed))
	(t-host (nth 0 t-parsed))
	(t-user (nth 1 t-parsed)))
    (if (and (string-equal f-host t-host)
	     (string-equal f-user t-user))
	(let* ((f-name (ange-ftp-quote-string (nth 2 f-parsed)))
	       (t-name (ange-ftp-quote-string (nth 2 t-parsed)))
	       (cmd (list 'rename f-name t-name))
	       (fabbr (ange-ftp-abbreviate-filename filename))
	       (nabbr (ange-ftp-abbreviate-filename newname filename))
	       (result (ange-ftp-send-cmd f-host f-user cmd
					  (format "Renaming %s to %s"
						  fabbr
						  nabbr))))
	  (or (car result)
	      (signal 'ftp-error
		      (list
		       "Renaming"
		       (format "FTP Error: \"%s\"" (cdr result))
		       filename
		       newname)))
	  (ange-ftp-add-file-entry newname)
	  (ange-ftp-delete-file-entry filename))
      (ange-ftp-copy-file-internal filename newname t nil)
      (delete-file filename))))

(defun ange-ftp-rename-local-to-remote (filename newname)
  "Rename local file FILENAME to remote file NEWNAME."
  (let* ((fabbr (ange-ftp-abbreviate-filename filename))
	 (nabbr (ange-ftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (ange-ftp-copy-file-internal filename newname t nil msg)
    (let (ange-ftp-process-verbose)
      (delete-file filename))))

(defun ange-ftp-rename-remote-to-local (filename newname)
  "Rename remote file FILENAME to local file NEWNAME."
  (let* ((fabbr (ange-ftp-abbreviate-filename filename))
	 (nabbr (ange-ftp-abbreviate-filename newname filename))
	 (msg (format "Renaming %s to %s" fabbr nabbr)))
    (ange-ftp-copy-file-internal filename newname t nil msg)
    (let (ange-ftp-process-verbose)
      (delete-file filename))))

(defun ange-ftp-rename-file (filename newname &optional ok-if-already-exists)
  (interactive "fRename file: \nFRename %s to file: \np")
  (setq filename (expand-file-name filename))
  (setq newname (expand-file-name newname))
  (let* ((f-parsed (ange-ftp-ftp-name filename))
	 (t-parsed (ange-ftp-ftp-name newname)))
    (if (and (or f-parsed t-parsed)
	     (or (not ok-if-already-exists)
		 (numberp ok-if-already-exists)))
	(ange-ftp-barf-or-query-if-file-exists
	 newname
	 "rename to it"
	 (numberp ok-if-already-exists)))
    (if f-parsed
	(if t-parsed
	    (ange-ftp-rename-remote-to-remote filename newname f-parsed
					      t-parsed)
	  (ange-ftp-rename-remote-to-local filename newname))
      (if t-parsed
	  (ange-ftp-rename-local-to-remote filename newname)
	(ange-ftp-real-rename-file filename newname ok-if-already-exists)))))

;;;; ------------------------------------------------------------
;;;; File name completion support.
;;;; ------------------------------------------------------------

;; If the file entry is not a directory (nor a symlink pointing to a directory)
;; returns whether the file (or file pointed to by the symlink) is ignored
;; by completion-ignored-extensions.
;; Note that `ange-ftp-this-dir' and `ange-ftp-completion-ignored-pattern'
;; are used as free variables.
(defun ange-ftp-file-entry-not-ignored-p (symname val)
  (if (stringp val)
      (let ((file (ange-ftp-expand-symlink val ange-ftp-this-dir)))
	(or (file-directory-p file)
	    (and (file-exists-p file)
		 (not (string-match ange-ftp-completion-ignored-pattern
				    symname)))))
    (or val				; is a directory name
	(not (string-match ange-ftp-completion-ignored-pattern symname)))))

(defun ange-ftp-root-dir-p (dir)
  ;; Maybe we should use something more like
  ;; (equal dir (file-name-directory (directory-file-name dir)))  -stef
  (or (and (eq system-type 'windows-nt)
	   (string-match "\\`[a-zA-Z]:[/\\]\\'" dir))
      (string-equal "/" dir)))

(defun ange-ftp-file-name-all-completions (file dir)
  (let ((ange-ftp-this-dir (expand-file-name dir)))
    (if (ange-ftp-ftp-name ange-ftp-this-dir)
	(progn
	  (ange-ftp-barf-if-not-directory ange-ftp-this-dir)
	  (setq ange-ftp-this-dir
		(ange-ftp-real-file-name-as-directory ange-ftp-this-dir))
	  (let* ((tbl (ange-ftp-get-files ange-ftp-this-dir))
		 (completions (all-completions file tbl)))

	    ;; see whether each matching file is a directory or not...
	    (mapcar
             (lambda (file)
               (let ((ent (gethash file tbl)))
                 (if (and ent
                          (or (not (stringp ent))
                              (file-directory-p
                               (ange-ftp-expand-symlink ent
                                                        ange-ftp-this-dir))))
                     (concat file "/")
		   file)))
	     completions)))

      (if (ange-ftp-root-dir-p ange-ftp-this-dir)
	  (nconc (all-completions file (ange-ftp-generate-root-prefixes))
		 (ange-ftp-real-file-name-all-completions file
							  ange-ftp-this-dir))
	(ange-ftp-real-file-name-all-completions file ange-ftp-this-dir)))))

(defun ange-ftp-file-name-completion (file dir &optional predicate)
  (let ((ange-ftp-this-dir (expand-file-name dir)))
    (if (ange-ftp-ftp-name ange-ftp-this-dir)
	(progn
	  (ange-ftp-barf-if-not-directory ange-ftp-this-dir)
	  (if (equal file "")
	      ""
	    (setq ange-ftp-this-dir
		  (ange-ftp-real-file-name-as-directory ange-ftp-this-dir))	;real?
	    (let* ((tbl (ange-ftp-get-files ange-ftp-this-dir))
		   (ange-ftp-completion-ignored-pattern
		    (mapconcat (lambda (s) (if (stringp s)
                                          (concat (regexp-quote s) "$")
                                        "/")) ; / never in filename
			       completion-ignored-extensions
			       "\\|")))
	      (save-match-data
		(or (ange-ftp-file-name-completion-1
		     file tbl ange-ftp-this-dir
		     'ange-ftp-file-entry-not-ignored-p)
		    (ange-ftp-file-name-completion-1
		     file tbl ange-ftp-this-dir))))))

      (if (ange-ftp-root-dir-p ange-ftp-this-dir)
	  (try-completion
	   file
	   (nconc (ange-ftp-generate-root-prefixes)
		  (ange-ftp-real-file-name-all-completions
		   file ange-ftp-this-dir))
	   predicate)
	(if predicate
	    (ange-ftp-real-file-name-completion
	     file ange-ftp-this-dir predicate)
	  (ange-ftp-real-file-name-completion
	   file ange-ftp-this-dir))))))


(defun ange-ftp-file-name-completion-1 (file tbl dir &optional predicate)
  (let ((bestmatch (try-completion file tbl predicate)))
    (if bestmatch
	(if (eq bestmatch t)
	    (if (file-directory-p (expand-file-name file dir))
		(concat file "/")
	      t)
	  (if (and (eq (try-completion bestmatch tbl predicate) t)
		   (file-directory-p
		    (expand-file-name bestmatch dir)))
	      (concat bestmatch "/")
	    bestmatch)))))

;; Put these lines uncommented in your .emacs if you want C-r to refresh
;; ange-ftp's cache whilst doing filename completion.
;;
;;(define-key minibuffer-local-completion-map "\C-r" 'ange-ftp-re-read-dir)
;;(define-key minibuffer-local-must-match-map "\C-r" 'ange-ftp-re-read-dir)

;;;###autoload
(defalias 'ange-ftp-re-read-dir 'ange-ftp-reread-dir)

;;;###autoload
(defun ange-ftp-reread-dir (&optional dir)
  "Reread remote directory DIR to update the directory cache.
The implementation of remote FTP file names caches directory contents
for speed.  Therefore, when new remote files are created, Emacs
may not know they exist.  You can use this command to reread a specific
directory, so that Emacs will know its current contents."
  (interactive)
  (if dir
      (setq dir (expand-file-name dir))
    (setq dir (file-name-directory (expand-file-name (buffer-string)))))
  (if (ange-ftp-ftp-name dir)
      (progn
	(setq ange-ftp-ls-cache-file nil)
	(remhash dir ange-ftp-files-hashtable)
	(ange-ftp-get-files dir t))))

(defun ange-ftp-make-directory (dir &optional parents)
  (interactive (list (expand-file-name (read-directory-name "Make directory: "))))
  (if parents
      (let ((parent (file-name-directory (directory-file-name dir))))
	(or (file-exists-p parent)
	    (ange-ftp-make-directory parent parents))))
  (if (file-exists-p dir)
      (error "Cannot make directory %s: file already exists" dir)
    (let ((parsed (ange-ftp-ftp-name dir)))
      (if parsed
	  (let* ((host (nth 0 parsed))
		 (user (nth 1 parsed))
		 ;; Some ftp's on unix machines (at least on Suns)
		 ;; insist that mkdir take a filename, and not a
		 ;; directory-name name as an arg. Argh!! This is a bug.
		 ;; Non-unix machines will probably always insist
		 ;; that mkdir takes a directory-name as an arg
		 ;; (as the ftp man page says it should).
		 (name (ange-ftp-quote-string
			(if (eq (ange-ftp-host-type host) 'unix)
			    (ange-ftp-real-directory-file-name (nth 2 parsed))
			  (ange-ftp-real-file-name-as-directory
			   (nth 2 parsed)))))
		 (abbr (ange-ftp-abbreviate-filename dir))
		 (result (ange-ftp-send-cmd host user
					    (list 'mkdir name)
					    (format "Making directory %s"
						    abbr))))
	    (or (car result)
		(ange-ftp-error host user
				(format "Could not make directory %s: %s"
					dir
					(cdr result))))
	    (ange-ftp-add-file-entry dir t))
	(ange-ftp-real-make-directory dir)))))

(defun ange-ftp-delete-directory (dir &optional recursive)
  (if (file-directory-p dir)
      (let ((parsed (ange-ftp-ftp-name dir)))
	(if recursive
	    (mapc
	     (lambda (file)
	       (if (file-directory-p file)
		   (ange-ftp-delete-directory file recursive)
		 (delete-file file)))
	     ;; We do not want to delete "." and "..".
	     (directory-files
	      dir 'full "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*")))
	(if parsed
	    (let* ((host (nth 0 parsed))
		   (user (nth 1 parsed))
		   ;; Some ftp's on unix machines (at least on Suns)
		   ;; insist that rmdir take a filename, and not a
		   ;; directory-name name as an arg. Argh!! This is a bug.
		   ;; Non-unix machines will probably always insist
		   ;; that rmdir takes a directory-name as an arg
		   ;; (as the ftp man page says it should).
		   (name (ange-ftp-quote-string
			  (if (eq (ange-ftp-host-type host) 'unix)
			      (ange-ftp-real-directory-file-name
			       (nth 2 parsed))
			    (ange-ftp-real-file-name-as-directory
			     (nth 2 parsed)))))
		   (abbr (ange-ftp-abbreviate-filename dir))
		   (result
		    (progn
		      ;; CWD must not in this directory.
		      (ange-ftp-cd host user "/" 'noerror)
		      (ange-ftp-send-cmd host user
					 (list 'rmdir name)
					 (format "Removing directory %s"
						 abbr)))))
	      (or (car result)
		  (ange-ftp-error host user
				  (format "Could not remove directory %s: %s"
					  dir
					  (cdr result))))
	      (ange-ftp-delete-file-entry dir t))
	  (ange-ftp-real-delete-directory dir recursive)))
    (error "Not a directory: %s" dir)))

;; Make a local copy of FILE and return its name.

(defun ange-ftp-file-local-copy (file)
  (let* ((fn1 (expand-file-name file))
	 (pa1 (ange-ftp-ftp-name fn1)))
    (if pa1
	(let ((tmp1 (ange-ftp-make-tmp-name (car pa1)
					    (file-name-extension file t))))
	  (ange-ftp-copy-file-internal fn1 tmp1 t nil
				       (format "Getting %s" fn1))
	  tmp1))))

(defun ange-ftp-file-remote-p (file &optional identification connected)
  (let* ((parsed (ange-ftp-ftp-name file))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (localname (nth 2 parsed)))
    (and (or (not connected)
	     (let ((proc (get-process (ange-ftp-ftp-process-buffer host user))))
	       (and proc (processp proc)
		    (memq (process-status proc) '(run open)))))
	 (cond
	  ((eq identification 'method) (and parsed "ftp"))
	  ((eq identification 'user) user)
	  ((eq identification 'host) host)
	  ((eq identification 'localname) localname)
	  (t (ange-ftp-replace-name-component file ""))))))

(defun ange-ftp-load (file &optional noerror nomessage nosuffix)
  (if (ange-ftp-ftp-name file)
      (let ((tryfiles (if nosuffix
			  (list file)
			(list (concat file ".elc") (concat file ".el") file)))
	    ;; make sure there are no references to temp files
	    (load-force-doc-strings t)
	    copy)
	(while (and tryfiles (not copy))
	  (catch 'ftp-error
	    (let ((ange-ftp-waiting-flag t))
	      (condition-case error
		  (setq copy (ange-ftp-file-local-copy (car tryfiles)))
		(ftp-error nil))))
	  (setq tryfiles (cdr tryfiles)))
	(if copy
	    (unwind-protect
		(funcall 'load copy noerror nomessage nosuffix)
	      (delete-file copy))
	  (or noerror
	      (signal 'file-error (list "Cannot open load file" file)))
	  nil))
    (ange-ftp-real-load file noerror nomessage nosuffix)))

;; Calculate default-unhandled-directory for a given ange-ftp buffer.
(defun ange-ftp-unhandled-file-name-directory (filename)
  nil)


;; Need the following functions for making filenames of compressed
;; files, because some OS's (unlike UNIX) do not allow a filename to
;; have two extensions.

(defvar ange-ftp-make-compressed-filename-alist nil
  "Alist of host-type-specific functions to process file names for compression.
Each element has the form (TYPE . FUNC).
FUNC should take one argument, a file name, and return a list
of the form (COMPRESSING NEWNAME).
COMPRESSING should be t if the specified file should be compressed,
and nil if it should be uncompressed (that is, if it is a compressed file).
NEWNAME should be the name to give the new compressed or uncompressed file.")

(declare-function dired-compress-file "dired-aux" (file))

(defun ange-ftp-dired-compress-file (name)
  "Handler used by `dired-compress-file'."
  (let ((parsed (ange-ftp-ftp-name name))
	conversion-func)
    (if (and parsed
	     (setq conversion-func
		   (cdr (assq (ange-ftp-host-type (car parsed))
			      ange-ftp-make-compressed-filename-alist))))
	(let* ((decision
		(save-match-data (funcall conversion-func name)))
	       (compressing (car decision))
	       (newfile (nth 1 decision)))
	  (if compressing
	      (ange-ftp-compress name newfile)
	    (ange-ftp-uncompress name newfile)))
      (let (file-name-handler-alist)
	(dired-compress-file name)))))

;; Copy FILE to this machine, compress it, and copy out to NFILE.
(defun ange-ftp-compress (file nfile)
  (let* ((parsed (ange-ftp-ftp-name file))
	 (tmp1 (ange-ftp-make-tmp-name (car parsed)))
	 (tmp2 (ange-ftp-make-tmp-name (car parsed)))
	 (abbr (ange-ftp-abbreviate-filename file))
	 (nabbr (ange-ftp-abbreviate-filename nfile))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" nabbr)))
    (unwind-protect
	(progn
	  (ange-ftp-copy-file-internal file tmp1 t nil msg1)
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Compressing %s..." abbr))
	  (call-process-region (point)
			       (point)
			       shell-file-name
			       nil
			       t
			       nil
			       "-c"
			       (format "compress -f -c < %s > %s" tmp1 tmp2))
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Compressing %s...done" abbr))
	  (if (zerop (buffer-size))
	      (progn
		(let (ange-ftp-process-verbose)
		  (delete-file file))
		(ange-ftp-copy-file-internal tmp2 nfile t nil msg2))))
      (ange-ftp-del-tmp-name tmp1)
      (ange-ftp-del-tmp-name tmp2))))

;; Copy FILE to this machine, uncompress it, and copy out to NFILE.
(defun ange-ftp-uncompress (file nfile)
  (let* ((parsed (ange-ftp-ftp-name file))
	 (tmp1 (ange-ftp-make-tmp-name (car parsed)))
	 (tmp2 (ange-ftp-make-tmp-name (car parsed)))
	 (abbr (ange-ftp-abbreviate-filename file))
	 (nabbr (ange-ftp-abbreviate-filename nfile))
	 (msg1 (format "Getting %s" abbr))
	 (msg2 (format "Putting %s" nabbr))
;;	 ;; Cheap hack because of problems with binary file transfers from
;;	 ;; VMS hosts.
;;	 (gbinary (not (eq 'vms (ange-ftp-host-type (car parsed)))))
	 )
    (unwind-protect
	(progn
	  (ange-ftp-copy-file-internal file tmp1 t nil msg1)
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Uncompressing %s..." abbr))
	  (call-process-region (point)
			       (point)
			       shell-file-name
			       nil
			       t
			       nil
			       "-c"
			       (format "uncompress -c < %s > %s" tmp1 tmp2))
	  (and ange-ftp-process-verbose
	       (ange-ftp-message "Uncompressing %s...done" abbr))
	  (if (zerop (buffer-size))
	      (progn
		(let (ange-ftp-process-verbose)
		  (delete-file file))
		(ange-ftp-copy-file-internal tmp2 nfile t nil msg2))))
      (ange-ftp-del-tmp-name tmp1)
      (ange-ftp-del-tmp-name tmp2))))

(defun ange-ftp-find-backup-file-name (fn)
  ;; Either return the ordinary backup name, etc.,
  ;; or return nil meaning don't make a backup.
  (if ange-ftp-make-backup-files
      (ange-ftp-real-find-backup-file-name fn)))

;;; Define the handler for special file names
;;; that causes ange-ftp to be invoked.

;;;###autoload
(defun ange-ftp-hook-function (operation &rest args)
  (let ((fn (get operation 'ange-ftp)))
    (if fn
	;; Catch also errors in process-filter.
	(condition-case err
	    (let ((debug-on-error t))
	      (save-match-data (apply fn args)))
	  (error (signal (car err) (cdr err))))
      (ange-ftp-run-real-handler operation args))))

;; The following code is commented out because Tramp now deals with
;; Ange-FTP filenames, too.

;;-;;; This regexp takes care of real ange-ftp file names (with a slash
;;-;;; and colon).
;;-;;; Don't allow the host name to end in a period--some systems use /.:
;;-;;;###autoload
;;-(or (assoc "^/[^/:]*[^/:.]:" file-name-handler-alist)
;;-    (setq file-name-handler-alist
;;-	  (cons '("^/[^/:]*[^/:.]:" . ange-ftp-hook-function)
;;-		file-name-handler-alist)))
;;-
;;-;;; This regexp recognizes absolute filenames with only one component,
;;-;;; for the sake of hostname completion.
;;-;;;###autoload
;;-(or (assoc "^/[^/:]*\\'" file-name-handler-alist)
;;-    (setq file-name-handler-alist
;;-	  (cons '("^/[^/:]*\\'" . ange-ftp-completion-hook-function)
;;-		file-name-handler-alist)))
;;-
;;-;;; This regexp recognizes absolute filenames with only one component
;;-;;; on Windows, for the sake of hostname completion.
;;-;;; NB. Do not mark this as autoload, because it is very common to
;;-;;; do completions in the root directory of drives on Windows.
;;-(and (memq system-type '(ms-dos windows-nt))
;;-     (or (assoc "^[a-zA-Z]:/[^/:]*\\'" file-name-handler-alist)
;;-	 (setq file-name-handler-alist
;;-	       (cons '("^[a-zA-Z]:/[^/:]*\\'" .
;;-		       ange-ftp-completion-hook-function)
;;-		     file-name-handler-alist))))

;;; The above two forms are sufficient to cause this file to be loaded
;;; if the user ever uses a file name with a colon in it.

;;; This sets the mode
(add-hook 'find-file-hook 'ange-ftp-set-buffer-mode)

;;; Now say where to find the handlers for particular operations.

(put 'file-name-directory 'ange-ftp 'ange-ftp-file-name-directory)
(put 'file-name-nondirectory 'ange-ftp 'ange-ftp-file-name-nondirectory)
(put 'file-name-as-directory 'ange-ftp 'ange-ftp-file-name-as-directory)
(put 'directory-file-name 'ange-ftp 'ange-ftp-directory-file-name)
(put 'expand-file-name 'ange-ftp 'ange-ftp-expand-file-name)
(put 'make-directory 'ange-ftp 'ange-ftp-make-directory)
(put 'delete-directory 'ange-ftp 'ange-ftp-delete-directory)
(put 'insert-file-contents 'ange-ftp 'ange-ftp-insert-file-contents)
(put 'directory-files 'ange-ftp 'ange-ftp-directory-files)
(put 'directory-files-and-attributes 'ange-ftp
     'ange-ftp-directory-files-and-attributes)
(put 'file-directory-p 'ange-ftp 'ange-ftp-file-directory-p)
(put 'file-writable-p 'ange-ftp 'ange-ftp-file-writable-p)
(put 'file-readable-p 'ange-ftp 'ange-ftp-file-readable-p)
(put 'file-executable-p 'ange-ftp 'ange-ftp-file-executable-p)
(put 'file-symlink-p 'ange-ftp 'ange-ftp-file-symlink-p)
(put 'delete-file 'ange-ftp 'ange-ftp-delete-file)
(put 'verify-visited-file-modtime 'ange-ftp
     'ange-ftp-verify-visited-file-modtime)
(put 'file-exists-p 'ange-ftp 'ange-ftp-file-exists-p)
(put 'write-region 'ange-ftp 'ange-ftp-write-region)
(put 'copy-file 'ange-ftp 'ange-ftp-copy-file)
(put 'rename-file 'ange-ftp 'ange-ftp-rename-file)
(put 'file-attributes 'ange-ftp 'ange-ftp-file-attributes)
(put 'file-newer-than-file-p 'ange-ftp 'ange-ftp-file-newer-than-file-p)
(put 'file-name-all-completions 'ange-ftp 'ange-ftp-file-name-all-completions)
(put 'file-name-completion 'ange-ftp 'ange-ftp-file-name-completion)
(put 'insert-directory 'ange-ftp 'ange-ftp-insert-directory)
(put 'file-local-copy 'ange-ftp 'ange-ftp-file-local-copy)
(put 'file-remote-p 'ange-ftp 'ange-ftp-file-remote-p)
(put 'unhandled-file-name-directory 'ange-ftp
     'ange-ftp-unhandled-file-name-directory)
(put 'file-name-sans-versions 'ange-ftp 'ange-ftp-file-name-sans-versions)
(put 'dired-uncache 'ange-ftp 'ange-ftp-dired-uncache)
(put 'dired-compress-file 'ange-ftp 'ange-ftp-dired-compress-file)
(put 'load 'ange-ftp 'ange-ftp-load)
(put 'find-backup-file-name 'ange-ftp 'ange-ftp-find-backup-file-name)
(put 'set-file-modes 'ange-ftp 'ange-ftp-set-file-modes)

;; Turn off truename processing to save time.
;; Treat each name as its own truename.
(put 'file-truename 'ange-ftp 'identity)

;; We must return non-nil in order to mask our inability to do the job.
;; Otherwise there are errors when applied to the target file during
;; copying from a (localhost) Tramp file.
(put 'set-file-times 'ange-ftp 'ignore)

;; Turn off RCS/SCCS processing to save time.
;; This returns nil for any file name as argument.
(put 'vc-registered 'ange-ftp 'null)

;; We can handle process-file in a restricted way (just for chown).
;; Nothing possible for `start-file-process'.
(put 'process-file 'ange-ftp 'ange-ftp-process-file)
(put 'start-file-process 'ange-ftp 'ignore)
(put 'shell-command 'ange-ftp 'ange-ftp-shell-command)

;;; Define ways of getting at unmodified Emacs primitives,
;;; turning off our handler.

;(defun ange-ftp-run-real-handler (operation args)
;  (let ((inhibit-file-name-handlers
;	 (cons 'ange-ftp-hook-function
;	       (cons 'ange-ftp-completion-hook-function
;		     (and (eq inhibit-file-name-operation operation)
;			  inhibit-file-name-handlers))))
;	(inhibit-file-name-operation operation))
;    (apply operation args)))

(defalias 'ange-ftp-run-real-handler 'tramp-run-real-handler)

(defun ange-ftp-real-file-name-directory (&rest args)
  (ange-ftp-run-real-handler 'file-name-directory args))
(defun ange-ftp-real-file-name-nondirectory (&rest args)
  (ange-ftp-run-real-handler 'file-name-nondirectory args))
(defun ange-ftp-real-file-name-as-directory (&rest args)
  (ange-ftp-run-real-handler 'file-name-as-directory args))
(defun ange-ftp-real-directory-file-name (&rest args)
  (ange-ftp-run-real-handler 'directory-file-name args))
(defun ange-ftp-real-expand-file-name (&rest args)
  (ange-ftp-run-real-handler 'expand-file-name args))
(defun ange-ftp-real-make-directory (&rest args)
  (ange-ftp-run-real-handler 'make-directory args))
(defun ange-ftp-real-delete-directory (&rest args)
  (ange-ftp-run-real-handler 'delete-directory args))
(defun ange-ftp-real-insert-file-contents (&rest args)
  (ange-ftp-run-real-handler 'insert-file-contents args))
(defun ange-ftp-real-directory-files (&rest args)
  (ange-ftp-run-real-handler 'directory-files args))
(defun ange-ftp-real-directory-files-and-attributes (&rest args)
  (ange-ftp-run-real-handler 'directory-files-and-attributes args))
(defun ange-ftp-real-file-directory-p (&rest args)
  (ange-ftp-run-real-handler 'file-directory-p args))
(defun ange-ftp-real-file-writable-p (&rest args)
  (ange-ftp-run-real-handler 'file-writable-p args))
(defun ange-ftp-real-file-readable-p (&rest args)
  (ange-ftp-run-real-handler 'file-readable-p args))
(defun ange-ftp-real-file-executable-p (&rest args)
  (ange-ftp-run-real-handler 'file-executable-p args))
(defun ange-ftp-real-file-symlink-p (&rest args)
  (ange-ftp-run-real-handler 'file-symlink-p args))
(defun ange-ftp-real-delete-file (&rest args)
  (ange-ftp-run-real-handler 'delete-file args))
(defun ange-ftp-real-verify-visited-file-modtime (&rest args)
  (ange-ftp-run-real-handler 'verify-visited-file-modtime args))
(defun ange-ftp-real-file-exists-p (&rest args)
  (ange-ftp-run-real-handler 'file-exists-p args))
(defun ange-ftp-real-write-region (&rest args)
  (ange-ftp-run-real-handler 'write-region args))
(defun ange-ftp-real-backup-buffer (&rest args)
  (ange-ftp-run-real-handler 'backup-buffer args))
(defun ange-ftp-real-copy-file (&rest args)
  (ange-ftp-run-real-handler 'copy-file args))
(defun ange-ftp-real-rename-file (&rest args)
  (ange-ftp-run-real-handler 'rename-file args))
(defun ange-ftp-real-file-attributes (&rest args)
  (ange-ftp-run-real-handler 'file-attributes args))
(defun ange-ftp-real-file-newer-than-file-p (&rest args)
  (ange-ftp-run-real-handler 'file-newer-than-file-p args))
(defun ange-ftp-real-file-name-all-completions (&rest args)
  (ange-ftp-run-real-handler 'file-name-all-completions args))
(defun ange-ftp-real-file-name-completion (&rest args)
  (ange-ftp-run-real-handler 'file-name-completion args))
(defun ange-ftp-real-insert-directory (&rest args)
  (ange-ftp-run-real-handler 'insert-directory args))
(defun ange-ftp-real-file-name-sans-versions (&rest args)
  (ange-ftp-run-real-handler 'file-name-sans-versions args))
(defun ange-ftp-real-shell-command (&rest args)
  (ange-ftp-run-real-handler 'shell-command args))
(defun ange-ftp-real-load (&rest args)
  (ange-ftp-run-real-handler 'load args))
(defun ange-ftp-real-find-backup-file-name (&rest args)
  (ange-ftp-run-real-handler 'find-backup-file-name args))

;; Here we support using dired on remote hosts.
;; I have turned off the support for using dired on foreign directory formats.
;; That involves too many unclean hooks.
;; It would be cleaner to support such operations by
;; converting the foreign directory format to something dired can understand;
;; something close to ls -l output.
;; The logical place to do this is in the functions ange-ftp-parse-...-listing.

;; Some of the old dired hooks would still be needed even if this is done.
;; I have preserved (and modernized) those hooks.
;; So the format conversion should be all that is needed.

;; When called from dired, SWITCHES may start with "--dired".
;; `ange-ftp-ls' handles this.

(defun ange-ftp-insert-directory (file switches &optional wildcard full)
  (if (not (ange-ftp-ftp-name (expand-file-name file)))
      (ange-ftp-real-insert-directory file switches wildcard full)
    ;; We used to follow symlinks on `file' here.  Apparently it was done
    ;; because some FTP servers react to "ls foo" by listing the symlink foo
    ;; rather than the directory it points to.  Now that ange-ftp-ls uses
    ;; "cd foo; ls" instead, this is not necessary any more.
    (let ((beg (point))
	  (end (point-marker)))
      (set-marker-insertion-type end t)
      (insert
       (cond
	(wildcard
	 (let ((default-directory (file-name-directory file)))
	   (ange-ftp-ls (file-name-nondirectory file) switches nil nil t)))
	(full
	 (ange-ftp-ls file switches 'parse))
	(t
	 ;; If `full' is nil we're going to do `ls' for a single file.
	 ;; Problem is that for various reasons, ange-ftp-ls needs to cd and
	 ;; then do an ls of current dir, which obviously won't work if we
	 ;; want to ls a file.  So instead, we get a full listing of the
	 ;; parent directory and extract the line corresponding to `file'.
	 (when (string-match "-?d\\'" switches)
	   ;; Remove "d" which dired added to `switches'.
	   (setq switches (substring switches 0 (match-beginning 0))))
	 (setq file (directory-file-name file))
	 (let* ((dirlist (ange-ftp-ls (or (file-name-directory file) ".")
				      switches 'parse))
		(filename (file-name-nondirectory file))
		(case-fold-search nil))
	   ;; FIXME: This presumes a particular output format, which is
	   ;; basically Unix.
	   (if (string-match (concat "^.+[^ ] " (regexp-quote filename)
				     "\\( -> .*\\)?[@/*=]?\n") dirlist)
	       (match-string 0 dirlist)
	     "")))))

      ;; Insert "  " for dired's alignment sanity.
      (goto-char beg)
      (while (re-search-forward "^\\(\\S-\\)" end 'move)
	(replace-match "  \\1"))

      ;; The inserted file could be from somewhere else.
      (when (and (not wildcard) (not full)
		 (search-backward
		  (if (zerop (length (file-name-nondirectory
				      (expand-file-name file))))
		      "."
		    (file-name-nondirectory file))
		  nil 'noerror))
	(replace-match (file-relative-name (expand-file-name file)) t)
	(goto-char end))

      (set-marker end nil))))

(defun ange-ftp-dired-uncache (dir)
  (if (ange-ftp-ftp-name (expand-file-name dir))
      (setq ange-ftp-ls-cache-file nil)))

(defvar ange-ftp-sans-version-alist nil
  "Alist of mapping host type into function to remove file version numbers.")

(defun ange-ftp-file-name-sans-versions (file keep-backup-version)
  (let* ((short (ange-ftp-abbreviate-filename file))
	 (parsed (ange-ftp-ftp-name short))
	 (func (if parsed (cdr (assq (ange-ftp-host-type (car parsed))
                                     ange-ftp-sans-version-alist)))))
    (if func (funcall func file keep-backup-version)
      (ange-ftp-real-file-name-sans-versions file keep-backup-version))))

;; This is the handler for shell-command.
(defun ange-ftp-shell-command (command &optional output-buffer error-buffer)
  (let* ((parsed (ange-ftp-ftp-name default-directory))
	 (host (nth 0 parsed))
	 (user (nth 1 parsed))
	 (name (nth 2 parsed)))
    (if (not parsed)
	(ange-ftp-real-shell-command command output-buffer error-buffer)
      (if (> (length name) 0)		; else it's $HOME
	  (setq command (concat "cd " name "; " command)))
      ;; Remove port from the hostname
      (when (string-match "\\(.*\\)#" host)
	(setq host (match-string 1 host)))
      (setq command
	    (format  "%s %s \"%s\""	; remsh -l USER does not work well
					; on a hp-ux machine I tried
		     remote-shell-program host command))
      (ange-ftp-message "Remote command '%s' ..." command)
      ;; Cannot call ange-ftp-real-dired-run-shell-command here as it
      ;; would prepend "cd default-directory" --- which bombs because
      ;; default-directory is in ange-ftp syntax for remote file names.
      (ange-ftp-real-shell-command command output-buffer error-buffer))))

;;; This is the handler for process-file.
(defun ange-ftp-process-file (program infile buffer display &rest arguments)
  ;; PROGRAM is always one of those below in the cond in dired.el.
  ;; The ARGUMENTS are (nearly) always files.
  (if (ange-ftp-ftp-name default-directory)
      ;; Can't use ange-ftp-dired-host-type here because the current
      ;; buffer is *dired-check-process output*
      (condition-case oops
	  (cond ((equal (or (bound-and-true-p dired-chmod-program) "chmod")
			program)
		 (ange-ftp-call-chmod arguments))
		;; ((equal "chgrp" program))
		;; ((equal dired-chown-program program))
		(t (error "Unknown remote command: %s" program)))
	(ftp-error (insert (format "%s: %s, %s\n"
				   (nth 1 oops)
				   (nth 2 oops)
				   (nth 3 oops)))
		   ;; Caller expects nonzero value to mean failure.
		   1)
	(error (insert (format "%s\n" (nth 1 oops)))
	       1))
    (apply 'call-process program infile buffer display arguments)))

;; Handle an attempt to run chmod on a remote file
;; by using the ftp chmod command.
(defun ange-ftp-call-chmod (args)
  (if (< (length args) 2)
      (error "ange-ftp-call-chmod: missing mode and/or filename: %s" args))
  (let ((mode (car args))
	(rest (cdr args)))
    (if (equal "--" (car rest))
	(setq rest (cdr rest)))
    (mapc
     (lambda (file)
       (setq file (expand-file-name file))
       (let ((parsed (ange-ftp-ftp-name file)))
         (if parsed
             (let* ((host (nth 0 parsed))
                    (user (nth 1 parsed))
                    (name (ange-ftp-quote-string (nth 2 parsed)))
                    (abbr (ange-ftp-abbreviate-filename file))
                    (result (ange-ftp-send-cmd host user
                                               (list 'chmod mode name)
                                               (format "doing chmod %s"
                                                       abbr))))
               (or (car result)
		   (ange-ftp-error
		    host user (concat "CHMOD failed: " (cdr result))))))))
     rest))
  (setq ange-ftp-ls-cache-file nil)	;Stop confusing Dired.
  0)

(defun ange-ftp-set-file-modes (filename mode)
  (ange-ftp-call-chmod (list (format "%o" mode) filename)))

;; This is turned off because it has nothing properly to do
;; with dired.  It could be reasonable to adapt this to
;; replace ange-ftp-copy-file.

;;;;; ------------------------------------------------------------
;;;;; Noddy support for async copy-file within dired.
;;;;; ------------------------------------------------------------

;;(defun ange-ftp-dired-copy-file (from to ok-flag &optional cont nowait)
;;  "Documented as original."
;;  (dired-handle-overwrite to)
;;  (ange-ftp-copy-file-internal from to ok-flag dired-copy-preserve-time nil
;;			       cont nowait))

;;(defun ange-ftp-dired-do-create-files (op-symbol file-creator operation arg
;;						 &optional marker-char op1
;;						 how-to)
;;  "Documented as original."
;;  ;; we need to let ange-ftp-dired-create-files know that we indirectly
;;  ;; called it rather than somebody else.
;;  (let ((ange-ftp-dired-do-create-files t)) ; tell who caller is
;;    (ange-ftp-real-dired-do-create-files op-symbol file-creator operation
;;					 arg marker-char op1 how-to)))

;;(defun ange-ftp-dired-create-files (file-creator operation fn-list name-constructor
;;						 &optional marker-char)
;;  "Documented as original."
;;  (if (and (boundp 'ange-ftp-dired-do-create-files)
;;	   ;; called from ange-ftp-dired-do-create-files?
;;	   ange-ftp-dired-do-create-files
;;	   ;; any files worth copying?
;;	   fn-list
;;	   ;; we only support async copy-file at the mo.
;;	   (eq file-creator 'dired-copy-file)
;;	   ;; it is only worth calling the alternative function for remote files
;;	   ;; as we tie ourself in recursive knots otherwise.
;;	   (or (ange-ftp-ftp-name (car fn-list))
;;	       ;; we can only call the name constructor for dired-do-create-files
;;	       ;; since the one for regexps starts prompting here, there and
;;	       ;; everywhere.
;;	       (ange-ftp-ftp-name (funcall name-constructor (car fn-list)))))
;;      ;; use the process-filter driven routine rather than the iterative one.
;;      (ange-ftp-dcf-1 file-creator
;;   		      operation
;; 		      fn-list
;; 		      name-constructor
;; 		      (and (boundp 'target) target)	;dynamically bound
;; 		      marker-char
;; 		      (current-buffer)
;; 		      nil	;overwrite-query
;; 		      nil	;overwrite-backup-query
;; 		      nil	;failures
;; 		      nil	;skipped
;; 		      0		;success-count
;; 		      (length fn-list) ;total
;; 		      )
;;    ;; normal case... use the interactive routine... much cheaper.
;;    (ange-ftp-real-dired-create-files file-creator operation fn-list
;;				      name-constructor marker-char)))

;;(defun ange-ftp-dcf-1 (file-creator operation fn-list name-constructor
;;		       target marker-char buffer overwrite-query
;;		       overwrite-backup-query failures skipped
;;		       success-count total)
;;  (with-current-buffer buffer
;;	  (if (null fn-list)
;;	      (ange-ftp-dcf-3 failures operation total skipped
;;			      success-count buffer)

;;	    (let* ((from (car fn-list))
;;		   (to (funcall name-constructor from)))
;;	      (if (equal to from)
;;		  (progn
;;		    (setq to nil)
;;		    (dired-log "Cannot %s to same file: %s\n"
;;			       (downcase operation) from)))
;;	      (if (not to)
;;		  (ange-ftp-dcf-1 file-creator
;;				  operation
;;				  (cdr fn-list)
;;				  name-constructor
;;				  target
;;				  marker-char
;;				  buffer
;;				  overwrite-query
;;				  overwrite-backup-query
;;				  failures
;;				  (cons (dired-make-relative from) skipped)
;;				  success-count
;;				  total)
;;		(let* ((overwrite (file-exists-p to))
;;		       (overwrite-confirmed	; for dired-handle-overwrite
;;			(and overwrite
;;			     (let ((help-form '(format "\
;;Type SPC or `y' to overwrite file `%s',
;;DEL or `n' to skip to next,
;;ESC or `q' to not overwrite any of the remaining files,
;;`!' to overwrite all remaining files with no more questions." to)))
;;			       (dired-query 'overwrite-query
;;					    "Overwrite `%s'?" to))))
;;		       ;; must determine if FROM is marked before file-creator
;;		       ;; gets a chance to delete it (in case of a move).
;;		       (actual-marker-char
;;			(cond  ((integerp marker-char) marker-char)
;;			       (marker-char (dired-file-marker from)) ; slow
;;			       (t nil))))
;;		  (condition-case err
;;		      (funcall file-creator from to overwrite-confirmed
;;			       (list 'ange-ftp-dcf-2
;;				     nil	;err
;;				     file-creator operation fn-list
;;				     name-constructor
;;				     target
;;				     marker-char actual-marker-char
;;				     buffer to from
;;				     overwrite
;;				     overwrite-confirmed
;;				     overwrite-query
;;				     overwrite-backup-query
;;				     failures skipped success-count
;;				     total)
;;			       t)
;;		    (file-error		; FILE-CREATOR aborted
;;		     (ange-ftp-dcf-2 nil ;result
;;				     nil ;line
;;				     err
;;				     file-creator operation fn-list
;;				     name-constructor
;;				     target
;;				     marker-char actual-marker-char
;;				     buffer to from
;;				     overwrite
;;				     overwrite-confirmed
;;				     overwrite-query
;;				     overwrite-backup-query
;;				     failures skipped success-count
;;				     total)))))))))

;;(defun ange-ftp-dcf-2 (result line err
;;			      file-creator operation fn-list
;;			      name-constructor
;;			      target
;;			      marker-char actual-marker-char
;;			      buffer to from
;;			      overwrite
;;			      overwrite-confirmed
;;			      overwrite-query
;;			      overwrite-backup-query
;;			      failures skipped success-count
;;			      total)
;;  (with-current-buffer buffer
;;	  (if (or err (not result))
;;	      (progn
;;		(setq failures (cons (dired-make-relative from) failures))
;;		(dired-log "%s `%s' to `%s' failed:\n%s\n"
;;			   operation from to (or err line)))
;;	    (if overwrite
;;		;; If we get here, file-creator hasn't been aborted
;;		;; and the old entry (if any) has to be deleted
;;		;; before adding the new entry.
;;		(dired-remove-file to))
;;	    (setq success-count (1+ success-count))
;;	    (message "%s: %d of %d" operation success-count total)
;;	    (dired-add-file to actual-marker-char))

;;	  (ange-ftp-dcf-1 file-creator operation (cdr fn-list)
;;			  name-constructor
;;			  target
;;			  marker-char
;;			  buffer
;;			  overwrite-query
;;			  overwrite-backup-query
;;			  failures skipped success-count
;;			  total)))

;;(defun ange-ftp-dcf-3 (failures operation total skipped success-count
;;				buffer)
;;  (with-current-buffer buffer
;;	  (cond
;;	   (failures
;;	    (dired-log-summary
;;	     (message "%s failed for %d of %d file%s %s"
;;		      operation (length failures) total
;;		      (dired-plural-s total) failures)))
;;	   (skipped
;;	    (dired-log-summary
;;	     (message "%s: %d of %d file%s skipped %s"
;;		      operation (length skipped) total
;;		      (dired-plural-s total) skipped)))
;;	   (t
;;	    (message "%s: %s file%s."
;;		     operation success-count (dired-plural-s success-count))))
;;	  (dired-move-to-filename)))

;;;; -----------------------------------------------
;;;; Unix Descriptive Listing (dl) Support
;;;; -----------------------------------------------

;; This is turned off because nothing uses it currently
;; and because I don't understand what it's supposed to be for. --rms.

;;(defconst ange-ftp-dired-dl-re-dir
;;  "^. [^ /]+/[ \n]"
;;  "Regular expression to use to search for dl directories.")

;;(or (assq 'unix:dl ange-ftp-dired-re-dir-alist)
;;    (setq ange-ftp-dired-re-dir-alist
;;	  (cons (cons 'unix:dl  ange-ftp-dired-dl-re-dir)
;;		ange-ftp-dired-re-dir-alist)))

;;(defun ange-ftp-dired-dl-move-to-filename (&optional raise-error eol)
;;  "In dired, move to the first character of the filename on this line."
;;  ;; This is the Unix dl version.
;;  (or eol (setq eol (progn (end-of-line) (point))))
;;  (let (case-fold-search)
;;    (beginning-of-line)
;;    (if (looking-at ". [^ ]+ +\\([0-9]+\\|-\\|=\\) ")
;;	(goto-char (+ (point) 2))
;;      (if raise-error
;;	  (error "No file on this line")
;;	nil))))

;;(or (assq 'unix:dl ange-ftp-dired-move-to-filename-alist)
;;    (setq ange-ftp-dired-move-to-filename-alist
;;	  (cons '(unix:dl . ange-ftp-dired-dl-move-to-filename)
;;		ange-ftp-dired-move-to-filename-alist)))

;;(defun ange-ftp-dired-dl-move-to-end-of-filename (&optional no-error eol)
;;  ;; Assumes point is at beginning of filename.
;;  ;; So, it should be called only after (dired-move-to-filename t).
;;  ;; On failure, signals an error or returns nil.
;;  ;; This is the Unix dl version.
;;  (let ((opoint (point))
;;	case-fold-search hidden)
;;    (or eol (setq eol (line-end-position)))
;;    (setq hidden (and selective-display
;;		       (save-excursion
;;			 (search-forward "\r" eol t))))
;;    (if hidden
;;	(if no-error
;;	    nil
;;	  (error
;;	   (substitute-command-keys
;;	    "File line is hidden, type \\[dired-hide-subdir] to unhide")))
;;      (skip-chars-forward "^ /" eol)
;;      (if (eq opoint (point))
;;	  (if no-error
;;	      nil
;;	    (error "No file on this line"))
;;	(point)))))

;;(or (assq 'unix:dl ange-ftp-dired-move-to-end-of-filename-alist)
;;    (setq ange-ftp-dired-move-to-end-of-filename-alist
;;	  (cons '(unix:dl . ange-ftp-dired-dl-move-to-end-of-filename)
;;		ange-ftp-dired-move-to-end-of-filename-alist)))

;;;; ------------------------------------------------------------
;;;; VOS support (VOS support is probably broken,
;;;; but I don't know anything about VOS.)
;;;; ------------------------------------------------------------
;
;(defun ange-ftp-fix-name-for-vos (name &optional reverse)
;  (setq name (copy-sequence name))
;  (let ((from (if reverse ?\> ?\/))
;	(to (if reverse ?\/ ?\>))
;	(i (1- (length name))))
;    (while (>= i 0)
;      (if (= (aref name i) from)
;	  (aset name i to))
;      (setq i (1- i)))
;    name))
;
;(or (assq 'vos ange-ftp-fix-name-func-alist)
;    (setq ange-ftp-fix-name-func-alist
;	  (cons '(vos . ange-ftp-fix-name-for-vos)
;		ange-ftp-fix-name-func-alist)))
;
;(or (memq 'vos ange-ftp-dumb-host-types)
;    (setq ange-ftp-dumb-host-types
;	  (cons 'vos ange-ftp-dumb-host-types)))
;
;(defun ange-ftp-fix-dir-name-for-vos (dir-name)
;  (ange-ftp-fix-name-for-vos
;   (concat dir-name
;	   (if (eq ?/ (aref dir-name (1- (length dir-name))))
;	       "" "/")
;	   "*")))
;
;(or (assq 'vos ange-ftp-fix-dir-name-func-alist)
;    (setq ange-ftp-fix-dir-name-func-alist
;	  (cons '(vos . ange-ftp-fix-dir-name-for-vos)
;		ange-ftp-fix-dir-name-func-alist)))
;
;(defvar ange-ftp-vos-host-regexp nil
;  "If a host matches this regexp then it is assumed to be running VOS.")
;
;(defun ange-ftp-vos-host (host)
;  (and ange-ftp-vos-host-regexp
;       (save-match-data
;	 (string-match ange-ftp-vos-host-regexp host))))
;
;(defun ange-ftp-parse-vos-listing ()
;  "Parse the current buffer which is assumed to be in VOS list -all
;format, and return a hashtable as the result."
;  (let ((tbl (ange-ftp-make-hashtable))
;	(type-list
;	 '(("^Files: [0-9]+ +Blocks: [0-9]+\n+" nil 40)
;	   ("^Dirs: [0-9]+\n+" t 30)))
;	type-regexp type-is-dir type-col file)
;    (goto-char (point-min))
;    (save-match-data
;      (while type-list
;	(setq type-regexp (car (car type-list))
;	      type-is-dir (nth 1 (car type-list))
;	      type-col (nth 2 (car type-list))
;	      type-list (cdr type-list))
;	(if (re-search-forward type-regexp nil t)
;	    (while (eq (char-after (point)) ? )
;	      (move-to-column type-col)
;	      (setq file (buffer-substring (point)
;					   (progn
;					     (end-of-line 1)
;					     (point))))
;	      (puthash file type-is-dir tbl)
;	      (forward-line 1))))
;      (puthash "." 'vosdir tbl)
;      (puthash ".." 'vosdir tbl))
;    tbl))
;
;(or (assq 'vos ange-ftp-parse-list-func-alist)
;    (setq ange-ftp-parse-list-func-alist
;	  (cons '(vos . ange-ftp-parse-vos-listing)
;		ange-ftp-parse-list-func-alist)))

;;;; ------------------------------------------------------------
;;;; VMS support.
;;;; ------------------------------------------------------------

;; Convert NAME from UNIX-ish to VMS.  If REVERSE given then convert from VMS
;; to UNIX-ish.
(defun ange-ftp-fix-name-for-vms (name &optional reverse)
  (save-match-data
    (if reverse
	(if (string-match "\\`\\([^:]+:\\)?\\(\\[.*\\]\\)?\\([^][]*\\)\\'" name)
	    (let (drive dir file)
	      (setq drive (match-string 1 name))
	      (setq dir (match-string 2 name))
	      (setq file (match-string 3 name))
	      (and dir
		   (setq dir (subst-char-in-string
                              ?/ ?. (substring dir 1 -1) t)))
	      (concat (and drive
			   (concat "/" drive "/"))
		      dir (and dir "/")
		      file))
	  (error "name %s didn't match" name))
      (let (drive dir file tmp quote)
	(if (string-match "\\`\".+\"\\'" name)
	    (setq name (substring name 1 -1)
		  quote "\"")
	  (setq quote ""))
	(if (string-match "\\`/[^:]+:/" name)
	    (setq drive (substring name 1
				   (1- (match-end 0)))
		  name (substring name (match-end 0))))
	(setq tmp (file-name-directory name))
	(if tmp
	    (setq dir (subst-char-in-string ?/ ?. (substring tmp 0 -1) t)))
	(setq file (file-name-nondirectory name))
	(concat quote drive
		(and dir (concat "[" (if drive nil ".") dir "]"))
		file quote)))))

;; (ange-ftp-fix-name-for-vms "/PUB$:/ANONYMOUS/SDSCPUB/NEXT/Readme.txt;1")
;; (ange-ftp-fix-name-for-vms "/PUB$:[ANONYMOUS.SDSCPUB.NEXT]Readme.txt;1" t)

(or (assq 'vms ange-ftp-fix-name-func-alist)
    (setq ange-ftp-fix-name-func-alist
	  (cons '(vms . ange-ftp-fix-name-for-vms)
		ange-ftp-fix-name-func-alist)))

(or (memq 'vms ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'vms ange-ftp-dumb-host-types)))

;; It is important that this function barf for directories for which we know
;; that we cannot possibly get a directory listing, such as "/" and "/DEV:/".
;; This is because it saves an unnecessary FTP error, or possibly the listing
;; might succeed, but give erroneous info. This last case is particularly
;; likely for OS's (like MTS) for which we need to use a wildcard in order
;; to list a directory.

;; Convert name from UNIX-ish to VMS ready for a DIRectory listing.
(defun ange-ftp-fix-dir-name-for-vms (dir-name)
  ;; Should there be entries for .. -> [-] and . -> [] below. Don't
  ;; think so, because expand-filename should have already short-circuited
  ;; them.
  (cond ((string-equal dir-name "/")
	 (error "Cannot get listing for fictitious \"/\" directory"))
	((string-match "\\`/[-A-Z0-9_$]+:/\\'" dir-name)
	 (error "Cannot get listing for device"))
	((ange-ftp-fix-name-for-vms dir-name))))

(or (assq 'vms ange-ftp-fix-dir-name-func-alist)
    (setq ange-ftp-fix-dir-name-func-alist
	  (cons '(vms . ange-ftp-fix-dir-name-for-vms)
		ange-ftp-fix-dir-name-func-alist)))

(defvar ange-ftp-vms-host-regexp nil)

;; Return non-nil if HOST is running VMS.
(defun ange-ftp-vms-host (host)
  (and ange-ftp-vms-host-regexp
       (string-match-p ange-ftp-vms-host-regexp host)))

;; Because some VMS ftp servers convert filenames to lower case
;; we allow a-z in the filename regexp. I'm not too happy about this.

(defconst ange-ftp-vms-filename-regexp
  (concat
   "\\(\\([_A-Za-z0-9$]?\\|[_A-Za-z0-9$][-_A-Za-z0-9$]*\\)\\."
   "[-_A-Za-z0-9$]*;+[0-9]*\\)")
  "Regular expression to match for a valid VMS file name in Dired buffer.
Stupid freaking bug!  Position of _ and $ shouldn't matter but they do.
Having [A-Z0-9$_] bombs on filename _$$CHANGE_LOG$.TXT$ and $CHANGE_LOG$.TX.
Other orders of $ and _ seem to all work just fine.")

;; These parsing functions are as general as possible because the syntax
;; of ftp listings from VMS hosts is a bit erratic. What saves us is that
;; the VMS filename syntax is so rigid. If they bomb on a listing in the
;; standard VMS Multinet format, then this is a bug. If they bomb on a listing
;; from vms.weird.net, then too bad.

;; Extract the next filename from a VMS dired-like listing.
(defun ange-ftp-parse-vms-filename ()
  (if (re-search-forward
       ange-ftp-vms-filename-regexp
       nil t)
      (match-string 0)))

;; Parse the current buffer which is assumed to be in MultiNet FTP dir
;; format, and return a hashtable as the result.
(defun ange-ftp-parse-vms-listing ()
  (let ((tbl (make-hash-table :test 'equal))
	file)
    (goto-char (point-min))
    (save-match-data
      (while (setq file (ange-ftp-parse-vms-filename))
	(if (string-match "\\.\\(DIR\\|dir\\);[0-9]+" file)
	    ;; deal with directories
	    (puthash (substring file 0 (match-beginning 0)) t tbl)
	  (puthash file nil tbl)
	  (if (string-match ";[0-9]+\\'" file) ; deal with extension
	      ;; sans extension
	      (puthash (substring file 0 (match-beginning 0)) nil tbl)))
	(forward-line 1))
      ;; Would like to look for a "Total" line, or a "Directory" line to
      ;; make sure that the listing isn't complete garbage before putting
      ;; in "." and "..", but we can't even count on all VAX's giving us
      ;; either of these.
      (puthash "." t tbl)
      (puthash ".." t tbl))
    tbl))

(add-to-list 'ange-ftp-parse-list-func-alist
	     '(vms . ange-ftp-parse-vms-listing))

;; This version only deletes file entries which have
;; explicit version numbers, because that is all VMS allows.

;; Can the following two functions be speeded up using file
;; completion functions?

(defun ange-ftp-vms-delete-file-entry (name &optional dir-p)
  (if dir-p
      (ange-ftp-internal-delete-file-entry name t)
    (save-match-data
      (let ((file (ange-ftp-get-file-part name)))
	(if (string-match ";[0-9]+\\'" file)
	    ;; In VMS you can't delete a file without an explicit
	    ;; version number, or wild-card (e.g. FOO;*)
	    ;; For now, we give up on wildcards.
	    (let ((files (gethash (file-name-directory name)
				  ange-ftp-files-hashtable)))
	      (if files
		  (let* ((root (substring file 0
					  (match-beginning 0)))
			 (regexp (concat "^"
					 (regexp-quote root)
					 ";[0-9]+$"))
			 versions)
		    (remhash file files)
		    ;; Now we need to check if there are any
		    ;; versions left. If not, then delete the
		    ;; root entry.
		    (maphash
		     (lambda (key val)
		       (and (string-match regexp key)
			    (setq versions t)))
		     files)
		    (or versions
			(remhash root files))))))))))

(or (assq 'vms ange-ftp-delete-file-entry-alist)
    (setq ange-ftp-delete-file-entry-alist
	  (cons '(vms . ange-ftp-vms-delete-file-entry)
		ange-ftp-delete-file-entry-alist)))

(defun ange-ftp-vms-add-file-entry (name &optional dir-p)
  (if dir-p
      (ange-ftp-internal-add-file-entry name t)
    (let ((files (gethash (file-name-directory name)
			  ange-ftp-files-hashtable)))
      (if files
	  (let ((file (ange-ftp-get-file-part name)))
	    (save-match-data
	      (if (string-match ";[0-9]+\\'" file)
		  (puthash (substring file 0 (match-beginning 0)) nil files)
		;; Need to figure out what version of the file
		;; is being added.
		(let ((regexp (concat "^"
				      (regexp-quote file)
				      ";\\([0-9]+\\)$"))
		      (version 0))
		  (maphash
		   (lambda (name val)
		     (and (string-match regexp name)
			  (setq version
				(max version
				     (string-to-number (match-string 1 name))))))
		   files)
		  (setq version (1+ version))
		  (puthash
		   (concat file ";" (int-to-string version))
		   nil files))))
	    (puthash file nil files))))))

(or (assq 'vms ange-ftp-add-file-entry-alist)
    (setq ange-ftp-add-file-entry-alist
	  (cons '(vms . ange-ftp-vms-add-file-entry)
		ange-ftp-add-file-entry-alist)))


(defun ange-ftp-add-vms-host (host)
  "Mark HOST as the name of a machine running VMS."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (car (ange-ftp-ftp-name name)))))))
  (if (not (ange-ftp-vms-host host))
      (setq ange-ftp-vms-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-vms-host-regexp "\\|")
		    ange-ftp-vms-host-regexp)
	    ange-ftp-host-cache nil)))


(defun ange-ftp-vms-file-name-as-directory (name)
  (save-match-data
    (if (string-match "\\.\\(DIR\\|dir\\)\\(;[0-9]+\\)?\\'" name)
	(setq name (substring name 0 (match-beginning 0))))
    (ange-ftp-real-file-name-as-directory name)))

(or (assq 'vms ange-ftp-file-name-as-directory-alist)
    (setq ange-ftp-file-name-as-directory-alist
	  (cons '(vms . ange-ftp-vms-file-name-as-directory)
		ange-ftp-file-name-as-directory-alist)))

;;; Tree dired support:

;; For this code I have borrowed liberally from Sebastian Kremer's
;; dired-vms.el


;;;; These regexps must be anchored to beginning of line.
;;;; Beware that the ftpd may put the device in front of the filename.

;;(defconst ange-ftp-dired-vms-re-exe "^. [^ \t.]+\\.\\(EXE\\|exe\\)[; ]"
;;  "Regular expression to use to search for VMS executable files.")

;;(defconst ange-ftp-dired-vms-re-dir "^. [^ \t.]+\\.\\(DIR\\|dir\\)[; ]"
;;  "Regular expression to use to search for VMS directories.")

;;(or (assq 'vms ange-ftp-dired-re-exe-alist)
;;    (setq ange-ftp-dired-re-exe-alist
;;	  (cons (cons 'vms  ange-ftp-dired-vms-re-exe)
;;		ange-ftp-dired-re-exe-alist)))

;;(or (assq 'vms ange-ftp-dired-re-dir-alist)
;;    (setq ange-ftp-dired-re-dir-alist
;;	  (cons (cons 'vms  ange-ftp-dired-vms-re-dir)
;;		ange-ftp-dired-re-dir-alist)))

;;(defun ange-ftp-dired-vms-insert-headerline (dir)
;;  ;; VMS inserts a headerline. I would prefer the headerline
;;  ;; to be in ange-ftp format. This version tries to
;;  ;; be careful, because we can't count on a headerline
;;  ;; over ftp, and we wouldn't want to delete anything
;;  ;; important.
;;  (save-excursion
;;    (if (looking-at "^  wildcard ")
;;	(forward-line 1))
;;    (if (looking-at "^[ \n\t]*[^\n]+\\][ \t]*\n")
;;	(delete-region (point) (match-end 0))))
;;  (ange-ftp-real-dired-insert-headerline dir))

;;(or (assq 'vms ange-ftp-dired-insert-headerline-alist)
;;    (setq ange-ftp-dired-insert-headerline-alist
;;	  (cons '(vms . ange-ftp-dired-vms-insert-headerline)
;;		ange-ftp-dired-insert-headerline-alist)))

;;(defun ange-ftp-dired-vms-move-to-filename (&optional raise-error eol)
;;  "In dired, move to first char of filename on this line.
;;Returns position (point) or nil if no filename on this line."
;;  ;; This is the VMS version.
;;  (let (case-fold-search)
;;    (or eol (setq eol (progn (end-of-line) (point))))
;;    (beginning-of-line)
;;    (if (re-search-forward ange-ftp-vms-filename-regexp eol t)
;;	(goto-char (match-beginning 1))
;;      (if raise-error
;;	  (error "No file on this line")
;;	nil))))

;;(or (assq 'vms ange-ftp-dired-move-to-filename-alist)
;;    (setq ange-ftp-dired-move-to-filename-alist
;;	  (cons '(vms . ange-ftp-dired-vms-move-to-filename)
;;		ange-ftp-dired-move-to-filename-alist)))

;;(defun ange-ftp-dired-vms-move-to-end-of-filename (&optional no-error eol)
;;  ;; Assumes point is at beginning of filename.
;;  ;; So, it should be called only after (dired-move-to-filename t).
;;  ;; case-fold-search must be nil, at least for VMS.
;;  ;; On failure, signals an error or returns nil.
;;  ;; This is the VMS version.
;;  (let (opoint hidden case-fold-search)
;;    (setq opoint (point))
;;    (or eol (setq eol (line-end-position)))
;;    (setq hidden (and selective-display
;;		      (save-excursion (search-forward "\r" eol t))))
;;    (if hidden
;;	nil
;;      (re-search-forward ange-ftp-vms-filename-regexp eol t))
;;    (or no-error
;;	(not (eq opoint (point)))
;;	(error
;;	 (if hidden
;;	     (substitute-command-keys
;;	      "File line is hidden, type \\[dired-hide-subdir] to unhide")
;;	   "No file on this line")))
;;    (if (eq opoint (point))
;;	nil
;;      (point))))

;;(or (assq 'vms ange-ftp-dired-move-to-end-of-filename-alist)
;;    (setq ange-ftp-dired-move-to-end-of-filename-alist
;;	  (cons '(vms . ange-ftp-dired-vms-move-to-end-of-filename)
;;		ange-ftp-dired-move-to-end-of-filename-alist)))

;;(defun ange-ftp-dired-vms-between-files ()
;;  (save-excursion
;;    (beginning-of-line)
;;    (or (equal (following-char) 10) ; newline
;;     (equal (following-char) 9)     ; tab
;;     (progn (forward-char 2)
;;	    (or (looking-at "Total of")
;;		(equal (following-char) 32))))))

;;(or (assq 'vms ange-ftp-dired-between-files-alist)
;;    (setq ange-ftp-dired-between-files-alist
;;	  (cons '(vms . ange-ftp-dired-vms-between-files)
;;		ange-ftp-dired-between-files-alist)))

;; Beware! In VMS filenames must be of the form "FILE.TYPE".
;; Therefore, we cannot just append a ".Z" to filenames for
;; compressed files. Instead, we turn "FILE.TYPE" into
;; "FILE.TYPE-Z". Hope that this is a reasonable thing to do.

(defun ange-ftp-vms-make-compressed-filename (name &optional reverse)
  (cond
   ((string-match "-Z;[0-9]+\\'" name)
    (list nil (substring name 0 (match-beginning 0))))
   ((string-match ";[0-9]+\\'" name)
    (list nil (substring name 0 (match-beginning 0))))
   ((string-match "-Z\\'" name)
    (list nil (substring name 0 -2)))
   (t
    (list t
	  (if (string-match ";[0-9]+\\'" name)
	      (concat (substring name 0 (match-beginning 0))
		      "-Z")
	    (concat name "-Z"))))))

(or (assq 'vms ange-ftp-make-compressed-filename-alist)
    (setq ange-ftp-make-compressed-filename-alist
	  (cons '(vms . ange-ftp-vms-make-compressed-filename)
		ange-ftp-make-compressed-filename-alist)))

;;;; When the filename is too long, VMS will use two lines to list a file
;;;; (damn them!) This will confuse dired. To solve this, need to convince
;;;; Sebastian to use a function dired-go-to-end-of-file-line, instead of
;;;; (forward-line 1). This would require a number of changes to dired.el.
;;;; If dired gets confused, revert-buffer will fix it.

;;(defun ange-ftp-dired-vms-ls-trim ()
;;  (goto-char (point-min))
;;  (let ((case-fold-search nil))
;;    (re-search-forward  ange-ftp-vms-filename-regexp))
;;  (beginning-of-line)
;;  (delete-region (point-min) (point))
;;  (forward-line 1)
;;  (delete-region (point) (point-max)))


;;(or (assq 'vms ange-ftp-dired-ls-trim-alist)
;;    (setq ange-ftp-dired-ls-trim-alist
;;	  (cons '(vms . ange-ftp-dired-vms-ls-trim)
;;		ange-ftp-dired-ls-trim-alist)))

(defun ange-ftp-vms-sans-version (name &rest args)
  (save-match-data
    (if (string-match ";[0-9]+\\'" name)
	(substring name 0 (match-beginning 0))
      name)))

(or (assq 'vms ange-ftp-sans-version-alist)
    (setq ange-ftp-sans-version-alist
	  (cons '(vms . ange-ftp-vms-sans-version)
		ange-ftp-sans-version-alist)))

;;(defvar ange-ftp-file-version-alist)

;;;;; The vms version of clean-directory has 2 more optional args
;;;;; than the usual dired version. This is so that it can be used by
;;;;; ange-ftp-dired-vms-flag-backup-files.

;;(defun ange-ftp-dired-vms-clean-directory (keep &optional marker msg)
;;  "Flag numerical backups for deletion.
;;Spares `dired-kept-versions' latest versions, and `kept-old-versions' oldest.
;;Positive prefix arg KEEP overrides `dired-kept-versions';
;;Negative prefix arg KEEP overrides `kept-old-versions' with KEEP made positive.

;;To clear the flags on these files, you can use \\[dired-flag-backup-files]
;;with a prefix argument."
;;;  (interactive "P") ; Never actually called interactively.
;;  (setq keep (max 1 (if keep (prefix-numeric-value keep) dired-kept-versions)))
;;  (let ((early-retention (if (< keep 0) (- keep) kept-old-versions))
;;	;; late-retention must NEVER be allowed to be less than 1 in VMS!
;;	;; This could wipe ALL copies of the file.
;;	(late-retention (max 1 (if (<= keep 0) dired-kept-versions keep)))
;;	(action (or msg "Cleaning"))
;;	(ange-ftp-trample-marker (or marker dired-del-marker))
;;	(ange-ftp-file-version-alist ()))
;;    (message (concat action
;;		     " numerical backups (keeping %d late, %d old)...")
;;	     late-retention early-retention)
;;    ;; Look at each file.
;;    ;; If the file has numeric backup versions,
;;    ;; put on ange-ftp-file-version-alist an element of the form
;;    ;; (FILENAME . VERSION-NUMBER-LIST)
;;    (dired-map-dired-file-lines 'ange-ftp-dired-vms-collect-file-versions)
;;    ;; Sort each VERSION-NUMBER-LIST,
;;    ;; and remove the versions not to be deleted.
;;    (let ((fval ange-ftp-file-version-alist))
;;      (while fval
;;	(let* ((sorted-v-list (cons 'q (sort (cdr (car fval)) '<)))
;;	       (v-count (length sorted-v-list)))
;;	  (if (> v-count (+ early-retention late-retention))
;;	      (rplacd (nthcdr early-retention sorted-v-list)
;;		      (nthcdr (- v-count late-retention)
;;			      sorted-v-list)))
;;	  (rplacd (car fval)
;;		  (cdr sorted-v-list)))
;;	(setq fval (cdr fval))))
;;    ;; Look at each file.  If it is a numeric backup file,
;;    ;; find it in a VERSION-NUMBER-LIST and maybe flag it for deletion.
;;    (dired-map-dired-file-lines
;;     'ange-ftp-dired-vms-trample-file-versions mark)
;;    (message (concat action " numerical backups...done"))))

;;(or (assq 'vms ange-ftp-dired-clean-directory-alist)
;;    (setq ange-ftp-dired-clean-directory-alist
;;	  (cons '(vms . ange-ftp-dired-vms-clean-directory)
;;		ange-ftp-dired-clean-directory-alist)))

;;(defun ange-ftp-dired-vms-collect-file-versions (fn)
;;  ;;  "If it looks like file FN has versions, return a list of the versions.
;;  ;;That is a list of strings which are file names.
;;  ;;The caller may want to flag some of these files for deletion."
;;(let ((name (nth 2 (ange-ftp-ftp-name fn))))
;;  (if (string-match ";[0-9]+$" name)
;;      (let* ((name (substring name 0 (match-beginning 0)))
;;	     (fn (ange-ftp-replace-name-component fn name)))
;;	(if (not (assq fn ange-ftp-file-version-alist))
;;	    (let* ((base-versions
;;		    (concat (file-name-nondirectory name) ";"))
;;		   (bv-length (length base-versions))
;;		   (possibilities (file-name-all-completions
;;				   base-versions
;;				   (file-name-directory fn)))
;;		   (versions (mapcar
;;			      (lambda (arg)
;;				 (if (and (string-match
;;					   "[0-9]+$" arg bv-length)
;;					  (= (match-beginning 0) bv-length))
;;				     (string-to-int (substring arg bv-length))
;;				   0))
;;			      possibilities)))
;;	      (if versions
;;		  (setq
;;		   ange-ftp-file-version-alist
;;		   (cons (cons fn versions)
;;			 ange-ftp-file-version-alist)))))))))

;;(defun ange-ftp-dired-vms-trample-file-versions (fn)
;;  (let* ((start-vn (string-match ";[0-9]+$" fn))
;;	 base-version-list)
;;    (and start-vn
;;	 (setq base-version-list	; there was a base version to which
;;	       (assoc (substring fn 0 start-vn)	; this looks like a
;;		      ange-ftp-file-version-alist))	; subversion
;;	 (not (memq (string-to-int (substring fn (1+ start-vn)))
;;		    base-version-list))	; this one doesn't make the cut
;;	 (progn (beginning-of-line)
;;		(delete-char 1)
;;		(insert ange-ftp-trample-marker)))))

;;(defun ange-ftp-dired-vms-flag-backup-files (&optional unflag-p)
;;  (let ((dired-kept-versions 1)
;;	(kept-old-versions 0)
;;	marker msg)
;;    (if unflag-p
;;	(setq marker ?\040 msg "Unflagging")
;;      (setq marker dired-del-marker msg "Cleaning"))
;;    (ange-ftp-dired-vms-clean-directory nil marker msg)))

;;(or (assq 'vms ange-ftp-dired-flag-backup-files-alist)
;;    (setq ange-ftp-dired-flag-backup-files-alist
;;	  (cons '(vms . ange-ftp-dired-vms-flag-backup-files)
;;		ange-ftp-dired-flag-backup-files-alist)))

;;(defun ange-ftp-dired-vms-backup-diff (&optional switches)
;;  (let ((file (dired-get-filename 'no-dir))
;;	bak)
;;    (if (and (string-match ";[0-9]+$" file)
;;	     ;; Find most recent previous version.
;;	     (let ((root (substring file 0 (match-beginning 0)))
;;		   (ver
;;		    (string-to-int (substring file (1+ (match-beginning 0)))))
;;		   found)
;;	       (setq ver (1- ver))
;;	       (while (and (> ver 0) (not found))
;;		 (setq bak (concat root ";" (int-to-string ver)))
;;		 (and (file-exists-p bak) (setq found t))
;;		 (setq ver (1- ver)))
;;	       found))
;;	(if switches
;;	    (diff (expand-file-name bak) (expand-file-name file) switches)
;;	  (diff (expand-file-name bak) (expand-file-name file)))
;;      (error "No previous version found for %s" file))))

;;(or (assq 'vms ange-ftp-dired-backup-diff-alist)
;;    (setq ange-ftp-dired-backup-diff-alist
;;	  (cons '(vms . ange-ftp-dired-vms-backup-diff)
;;		ange-ftp-dired-backup-diff-alist)))


;;;; ------------------------------------------------------------
;;;; MTS support
;;;; ------------------------------------------------------------


;; Convert NAME from UNIX-ish to MTS. If REVERSE given then convert from
;; MTS to UNIX-ish.
(defun ange-ftp-fix-name-for-mts (name &optional reverse)
  (save-match-data
    (if reverse
	(if (string-match "\\`\\([^:]+:\\)?\\(.*\\)\\'" name)
	    (let (acct file)
	      (setq acct (match-string 1 name))
	      (setq file (match-string 2 name))
	      (concat (and acct (concat "/" acct "/"))
		      file))
	  (error "name %s didn't match" name))
      (if (string-match "\\`/\\([^:]+:\\)/\\(.*\\)\\'" name)
	  (concat (match-string 1 name) (match-string 2 name))
	;; Let's hope that mts will recognize it anyway.
	name))))

(or (assq 'mts ange-ftp-fix-name-func-alist)
    (setq ange-ftp-fix-name-func-alist
	  (cons '(mts . ange-ftp-fix-name-for-mts)
		ange-ftp-fix-name-func-alist)))

;; Convert name from UNIX-ish to MTS ready for a DIRectory listing.
;; Remember that there are no directories in MTS.
(defun ange-ftp-fix-dir-name-for-mts (dir-name)
  (if (string-equal dir-name "/")
      (error "Cannot get listing for fictitious \"/\" directory")
    (let ((dir-name (ange-ftp-fix-name-for-mts dir-name)))
      (cond
       ((string-equal dir-name "")
	"?")
       ((string-match ":\\'" dir-name)
	(concat dir-name "?"))
       (dir-name))))) ; It's just a single file.

(or (assq 'mts ange-ftp-fix-dir-name-func-alist)
    (setq ange-ftp-fix-dir-name-func-alist
	  (cons '(mts . ange-ftp-fix-dir-name-for-mts)
		ange-ftp-fix-dir-name-func-alist)))

(or (memq 'mts ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'mts ange-ftp-dumb-host-types)))

(defvar ange-ftp-mts-host-regexp nil)

;; Return non-nil if HOST is running MTS.
(defun ange-ftp-mts-host (host)
  (and ange-ftp-mts-host-regexp
       (string-match-p ange-ftp-mts-host-regexp host)))

;; Parse the current buffer which is assumed to be in mts ftp dir format.
(defun ange-ftp-parse-mts-listing ()
  (let ((tbl (make-hash-table :test 'equal)))
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward directory-listing-before-filename-regexp nil t)
	(end-of-line)
	(skip-chars-backward " ")
	(let ((end (point)))
	  (skip-chars-backward "-A-Z0-9_.!")
	  (puthash (buffer-substring (point) end) nil tbl))
	(forward-line 1)))
    ;; Don't need to bother with ..
    (puthash "." t tbl)
    tbl))

(add-to-list 'ange-ftp-parse-list-func-alist
	     '(mts . ange-ftp-parse-mts-listing))

(defun ange-ftp-add-mts-host (host)
  "Mark HOST as the name of a machine running MTS."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (car (ange-ftp-ftp-name name)))))))
  (if (not (ange-ftp-mts-host host))
      (setq ange-ftp-mts-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-mts-host-regexp "\\|")
		    ange-ftp-mts-host-regexp)
	    ange-ftp-host-cache nil)))

;;; Tree dired support:

;;;; There aren't too many systems left that use MTS. This dired support will
;;;; work for the implementation of ftp on mtsg.ubc.ca. I hope other mts systems
;;;; implement ftp in the same way. If not, it might be necessary to make the
;;;; following more flexible.

;;(defun ange-ftp-dired-mts-move-to-filename (&optional raise-error eol)
;;  "In dired, move to first char of filename on this line.
;;Returns position (point) or nil if no filename on this line."
;;  ;; This is the MTS version.
;;  (or eol (setq eol (progn (end-of-line) (point))))
;;  (beginning-of-line)
;;  (if (re-search-forward
;;       ange-ftp-date-regexp eol t)
;;      (progn
;;	(skip-chars-forward " ")          ; Eat blanks after date
;;	(skip-chars-forward "0-9:" eol)   ; Eat time or year
;;	(skip-chars-forward " " eol)      ; one space before filename
;;	;; When listing an account other than the users own account it appends
;;	;; ACCT: to the beginning of the filename. Skip over this.
;;	(and (looking-at "[A-Z0-9_.]+:")
;;	     (goto-char (match-end 0)))
;;	(point))
;;    (if raise-error
;;	(error "No file on this line")
;;      nil)))

;;(or (assq 'mts ange-ftp-dired-move-to-filename-alist)
;;    (setq ange-ftp-dired-move-to-filename-alist
;;	  (cons '(mts . ange-ftp-dired-mts-move-to-filename)
;;		ange-ftp-dired-move-to-filename-alist)))

;;(defun ange-ftp-dired-mts-move-to-end-of-filename (&optional no-error eol)
;;  ;; Assumes point is at beginning of filename.
;;  ;; So, it should be called only after (dired-move-to-filename t).
;;  ;; On failure, signals an error or returns nil.
;;  ;; This is the MTS version.
;;  (let (opoint hidden case-fold-search)
;;    (setq opoint (point)
;;	  eol (line-end-position)
;;	  hidden (and selective-display
;;		      (save-excursion (search-forward "\r" eol t))))
;;    (if hidden
;;	nil
;;      (skip-chars-forward "-A-Z0-9._!" eol))
;;    (or no-error
;;	(not (eq opoint (point)))
;;	(error
;;	 (if hidden
;;	     (substitute-command-keys
;;	      "File line is hidden, type \\[dired-hide-subdir] to unhide")
;;	   "No file on this line")))
;;    (if (eq opoint (point))
;;	nil
;;      (point))))

;;(or (assq 'mts ange-ftp-dired-move-to-end-of-filename-alist)
;;    (setq ange-ftp-dired-move-to-end-of-filename-alist
;;	  (cons '(mts . ange-ftp-dired-mts-move-to-end-of-filename)
;;		ange-ftp-dired-move-to-end-of-filename-alist)))

;;;; ------------------------------------------------------------
;;;; CMS support
;;;; ------------------------------------------------------------

;; Since CMS doesn't have any full file name syntax, we have to fudge
;; things with cd's. We actually send too many cd's, but it's dangerous
;; to try to remember the current minidisk, because if the connection
;; is closed and needs to be reopened, we will find ourselves back in
;; the default minidisk. This is fairly likely since CMS ftp servers
;; usually close the connection after 5 minutes of inactivity.

;; Have I got the filename character set right?

(defun ange-ftp-fix-name-for-cms (name &optional reverse)
  (save-match-data
    (if reverse
	;; Since we only convert output from a pwd in this direction,
	;; we'll assume that it's a minidisk, and make it into a
	;; directory file name. Note that the expand-dir-hashtable
	;; stores directories without the trailing /. Is this
	;; consistent?
	(concat "/" name)
      (if (string-match "\\`/\\([-A-Z0-9$*._]+\\)/\\([-A-Z0-9$._]+\\)?\\'"
			name)
	  (let ((minidisk (match-string 1 name)))
	    (if (match-beginning 2)
		(let ((file (match-string 2 name))
		      (cmd (concat "cd " minidisk))

		      ;; Note that host and user are bound in the call
		      ;; to ange-ftp-send-cmd
		      (proc (ange-ftp-get-process ange-ftp-this-host
						  ange-ftp-this-user)))

		  ;; Must use ange-ftp-raw-send-cmd here to avoid
		  ;; an infinite loop.
		  (if (car (ange-ftp-raw-send-cmd proc cmd ange-ftp-this-msg))
		      file
		    ;; failed... try ONCE more.
		    (setq proc (ange-ftp-get-process ange-ftp-this-host
						     ange-ftp-this-user))
		    (let ((result (ange-ftp-raw-send-cmd proc cmd
							 ange-ftp-this-msg)))
		      (if (car result)
			  file
			;; failed.  give up.
			(ange-ftp-error ange-ftp-this-host ange-ftp-this-user
					(format "cd to minidisk %s failed: %s"
						minidisk (cdr result)))))))
	      ;; return the minidisk
	      minidisk))
	(error "Invalid CMS filename")))))

(or (assq 'cms ange-ftp-fix-name-func-alist)
    (setq ange-ftp-fix-name-func-alist
	  (cons '(cms . ange-ftp-fix-name-for-cms)
		ange-ftp-fix-name-func-alist)))

(or (memq 'cms ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'cms ange-ftp-dumb-host-types)))

;; Convert name from UNIX-ish to CMS ready for a DIRectory listing.
(defun ange-ftp-fix-dir-name-for-cms (dir-name)
  (cond
   ((string-equal "/" dir-name)
    (error "Cannot get listing for fictitious \"/\" directory"))
   ((string-match "\\`/\\([-A-Z0-9$*._]+\\)/\\([-A-Z0-9$._]+\\)?\\'" dir-name)
    (let* ((minidisk (match-string 1 dir-name))
	   ;; host and user are bound in the call to ange-ftp-send-cmd
	   (proc (ange-ftp-get-process ange-ftp-this-host ange-ftp-this-user))
	   (cmd (concat "cd " minidisk))
	   (file (if (match-beginning 2)
		     ;; it's a single file
		     (match-string 2 dir-name)
		   ;; use the wild-card
		   "*")))
      (if (car (ange-ftp-raw-send-cmd proc cmd))
	  file
	;; try again...
	(setq proc (ange-ftp-get-process ange-ftp-this-host
					 ange-ftp-this-user))
	(let ((result (ange-ftp-raw-send-cmd proc cmd)))
	  (if (car result)
	      file
	    ;; give up
	    (ange-ftp-error ange-ftp-this-host ange-ftp-this-user
			    (format "cd to minidisk %s failed: %s"
				    minidisk (cdr result))))))))
   (t (error "Invalid CMS file name"))))

(or (assq 'cms ange-ftp-fix-dir-name-func-alist)
    (setq ange-ftp-fix-dir-name-func-alist
	  (cons '(cms . ange-ftp-fix-dir-name-for-cms)
		ange-ftp-fix-dir-name-func-alist)))

(defvar ange-ftp-cms-host-regexp nil
  "Regular expression to match hosts running the CMS operating system.")

;; Return non-nil if HOST is running CMS.
(defun ange-ftp-cms-host (host)
  (and ange-ftp-cms-host-regexp
       (string-match-p ange-ftp-cms-host-regexp host)))

(defun ange-ftp-add-cms-host (host)
  "Mark HOST as the name of a CMS host."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (car (ange-ftp-ftp-name name)))))))
  (if (not (ange-ftp-cms-host host))
      (setq ange-ftp-cms-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-cms-host-regexp "\\|")
		    ange-ftp-cms-host-regexp)
	    ange-ftp-host-cache nil)))

(defun ange-ftp-parse-cms-listing ()
  ;; Parse the current buffer which is assumed to be a CMS directory listing.
  ;; If we succeed in getting a listing, then we will assume that the minidisk
  ;; exists. file is bound by the call to ange-ftp-ls. This doesn't work
  ;; because ange-ftp doesn't know that the root hashtable has only part of
  ;; the info. It will assume that if a minidisk isn't in it, then it doesn't
  ;; exist. It would be nice if completion worked for minidisks, as we
  ;; discover them.
;  (let* ((dir-file (directory-file-name file))
;	 (root (file-name-directory dir-file))
;	 (minidisk (ange-ftp-get-file-part dir-file))
;	 (root-tbl (ange-ftp-get-hash-entry root ange-ftp-files-hashtable)))
;    (if root-tbl
;	(puthash minidisk t root-tbl)
;      (setq root-tbl (ange-ftp-make-hashtable))
;      (puthash minidisk t root-tbl)
;      (puthash "." t root-tbl)
;      (ange-ftp-set-files root root-tbl)))
  ;; Now do the usual parsing
  (let ((tbl (make-hash-table :test 'equal)))
    (goto-char (point-min))
    (save-match-data
      (while
	  (re-search-forward
	   "^\\([-A-Z0-9$_]+\\) +\\([-A-Z0-9$_]+\\) +[VF] +[0-9]+ " nil t)
	(puthash (concat (match-string 1) "." (match-string 2)) nil tbl)
	(forward-line 1))
      (puthash "." t tbl))
    tbl))

(add-to-list 'ange-ftp-parse-list-func-alist
	     '(cms . ange-ftp-parse-cms-listing))

;;;;; Tree dired support:

;;(defconst ange-ftp-dired-cms-re-exe
;;  "^. [-A-Z0-9$_]+ +EXEC "
;;  "Regular expression to use to search for CMS executables.")

;;(or (assq 'cms ange-ftp-dired-re-exe-alist)
;;    (setq ange-ftp-dired-re-exe-alist
;;	  (cons (cons 'cms  ange-ftp-dired-cms-re-exe)
;;		ange-ftp-dired-re-exe-alist)))


;;(defun ange-ftp-dired-cms-insert-headerline (dir)
;;  ;; CMS has no total line, so we insert a blank line for
;;  ;; aesthetics.
;;  (insert "\n")
;;  (forward-char -1)
;;  (ange-ftp-real-dired-insert-headerline dir))

;;(or (assq 'cms ange-ftp-dired-insert-headerline-alist)
;;    (setq ange-ftp-dired-insert-headerline-alist
;;	  (cons '(cms . ange-ftp-dired-cms-insert-headerline)
;;		ange-ftp-dired-insert-headerline-alist)))

;;(defun ange-ftp-dired-cms-move-to-filename (&optional raise-error eol)
;;  "In dired, move to the first char of filename on this line."
;;  ;; This is the CMS version.
;;  (or eol (setq eol (progn (end-of-line) (point))))
;;  (let (case-fold-search)
;;    (beginning-of-line)
;;    (if (re-search-forward " [-A-Z0-9$_]+ +[-A-Z0-9$_]+ +[VF] +[0-9]+ " eol t)
;;	(goto-char (1+ (match-beginning 0)))
;;      (if raise-error
;;	  (error "No file on this line")
;;	nil))))

;;(or (assq 'cms ange-ftp-dired-move-to-filename-alist)
;;    (setq ange-ftp-dired-move-to-filename-alist
;;	  (cons '(cms . ange-ftp-dired-cms-move-to-filename)
;;		ange-ftp-dired-move-to-filename-alist)))

;;(defun ange-ftp-dired-cms-move-to-end-of-filename (&optional no-error eol)
;;  ;; Assumes point is at beginning of filename.
;;  ;; So, it should be called only after (dired-move-to-filename t).
;;  ;; case-fold-search must be nil, at least for VMS.
;;  ;; On failure, signals an error or returns nil.
;;  ;; This is the CMS version.
;;  (let ((opoint (point))
;;	case-fold-search hidden)
;;    (or eol (setq eol (line-end-position)))
;;    (setq hidden (and selective-display
;;		      (save-excursion
;;			(search-forward "\r" eol t))))
;;    (if hidden
;;	(if no-error
;;	    nil
;;	  (error
;;	   (substitute-command-keys
;;	    "File line is hidden, type \\[dired-hide-subdir] to unhide")))
;;      (skip-chars-forward "-A-Z0-9$_" eol)
;;      (skip-chars-forward " " eol)
;;      (skip-chars-forward "-A-Z0-9$_" eol)
;;      (if (eq opoint (point))
;;	  (if no-error
;;	      nil
;;	    (error "No file on this line"))
;;	(point)))))

;;(or (assq 'cms ange-ftp-dired-move-to-end-of-filename-alist)
;;    (setq ange-ftp-dired-move-to-end-of-filename-alist
;;	  (cons '(cms . ange-ftp-dired-cms-move-to-end-of-filename)
;;		ange-ftp-dired-move-to-end-of-filename-alist)))

(defun ange-ftp-cms-make-compressed-filename (name &optional reverse)
  (if (string-match "-Z\\'" name)
      (list nil (substring name 0 -2))
    (list t (concat name "-Z"))))

(or (assq 'cms ange-ftp-make-compressed-filename-alist)
    (setq ange-ftp-make-compressed-filename-alist
	  (cons '(cms . ange-ftp-cms-make-compressed-filename)
		ange-ftp-make-compressed-filename-alist)))

;;(defun ange-ftp-dired-cms-get-filename (&optional localp no-error-if-not-filep)
;;  (let ((name (ange-ftp-real-dired-get-filename localp no-error-if-not-filep)))
;;    (and name
;;	 (if (string-match "^\\([^ ]+\\) +\\([^ ]+\\)$" name)
;;	     (concat (substring name 0 (match-end 1))
;;		     "."
;;		     (substring name (match-beginning 2) (match-end 2)))
;;	   name))))

;;(or (assq 'cms ange-ftp-dired-get-filename-alist)
;;    (setq ange-ftp-dired-get-filename-alist
;;	  (cons '(cms . ange-ftp-dired-cms-get-filename)
;;		ange-ftp-dired-get-filename-alist)))

;;;; ------------------------------------------------------------
;;;; BS2000 support
;;;; ------------------------------------------------------------

;; There seems to be an error with regexps. '-' has to be the first
;; character inside of the square brackets.
(defconst ange-ftp-bs2000-short-filename-regexp
  "[-A-Z0-9$#@.]*[A-Z][-A-Z0-9$#@.]*"
  "Regular expression to match for a valid short BS2000 file name.")

(defconst ange-ftp-bs2000-fix-name-regexp-reverse
  (concat
   "^\\(" ange-ftp-bs2000-filename-pubset-regexp "\\)?"
   "\\(" ange-ftp-bs2000-filename-username-regexp "\\)?"
   "\\(" ange-ftp-bs2000-short-filename-regexp "\\)?")
  "Regular expression used in `ange-ftp-fix-name-for-bs2000'.")

(defconst ange-ftp-bs2000-fix-name-regexp
  (concat
   "/?\\(" ange-ftp-bs2000-filename-pubset-regexp "/\\)?"
   "\\(\\$[A-Z0-9]*/\\)?"
   "\\(" ange-ftp-bs2000-short-filename-regexp "\\)?")
  "Regular expression used in `ange-ftp-fix-name-for-bs2000'.")

(defcustom ange-ftp-bs2000-special-prefix
  "X"
  "Prefix used for filenames starting with '#' or '@'."
  :group 'ange-ftp
  :type 'string)

;; Convert NAME from UNIX-ish to BS2000. If REVERSE given then convert from
;; BS2000 to UNIX-ish.
(defun ange-ftp-fix-name-for-bs2000 (name &optional reverse)
  (save-match-data
    (if reverse
	(if (string-match
	     ange-ftp-bs2000-fix-name-regexp-reverse
	     name)
	    (let ((pubset (if (match-beginning 1)
			      (substring name 0 (match-end 1))))
		  (userid (if (match-beginning 2)
			      (substring name
					 (match-beginning 2)
					 (1- (match-end 2)))))
		  (filename (if (match-beginning 3)
				(substring name (match-beginning 3)))))
	      (concat
	       "/"
	       ;; we have to insert "_/" here to prevent expand-file-name to
	       ;; interpret BS2000 pubsets as the special escape prefix:
	       (and pubset (concat "_/" pubset "/"))
	       (and userid (concat userid "/"))
	       filename))
	  (error "name %s didn't match" name))
      ;; and here we (maybe) have to remove the inserted "_/" 'cause
      ;; of our prevention of the special escape prefix above:
      (if (string-match (concat "^/_/") name)
	  (setq name (substring name 2)))
      (if (string-match
	   ange-ftp-bs2000-fix-name-regexp
	   name)
	  (let ((pubset (if (match-beginning 1)
			    (substring name
				       (match-beginning 1)
				       (1- (match-end 1)))))
		(userid (if (match-beginning 2)
			    (substring name
				       (match-beginning 2)
				       (1- (match-end 2)))))
		(filename (if (match-beginning 3)
			      (substring name (match-beginning 3)))))
	    (if (and (boundp 'filename)
		     (stringp filename)
		     (string-match "[#@].+" filename))
		(setq filename (concat ange-ftp-bs2000-special-prefix
				       (substring filename 1))))
	    (upcase
	     (concat
	      pubset
	      (and userid (concat userid "."))
	      ;; change every '/' in filename to a '.', normally not necessary
	      (and filename
		   (subst-char-in-string ?/ ?. filename)))))
	;; Let's hope that BS2000 recognize this anyway:
	name))))

(or (assq 'bs2000 ange-ftp-fix-name-func-alist)
    (setq ange-ftp-fix-name-func-alist
	  (cons '(bs2000 . ange-ftp-fix-name-for-bs2000)
		ange-ftp-fix-name-func-alist)))

;; Convert name from UNIX-ish to BS2000 ready for a DIRectory listing.
;; Remember that there are no directories in BS2000.
(defun ange-ftp-fix-dir-name-for-bs2000 (dir-name)
  (if (string-equal dir-name "/")
      "*" ;; Don't use an empty string here!
    (ange-ftp-fix-name-for-bs2000 dir-name)))

(or (assq 'bs2000 ange-ftp-fix-dir-name-func-alist)
    (setq ange-ftp-fix-dir-name-func-alist
	  (cons '(bs2000 . ange-ftp-fix-dir-name-for-bs2000)
		ange-ftp-fix-dir-name-func-alist)))

(or (memq 'bs2000 ange-ftp-dumb-host-types)
    (setq ange-ftp-dumb-host-types
	  (cons 'bs2000 ange-ftp-dumb-host-types)))

(defvar ange-ftp-bs2000-host-regexp nil)
(defvar ange-ftp-bs2000-posix-host-regexp nil)

;; Return non-nil if HOST is running BS2000.
(defun ange-ftp-bs2000-host (host)
  (and ange-ftp-bs2000-host-regexp
       (string-match-p ange-ftp-bs2000-host-regexp host)))
;; Return non-nil if HOST is running BS2000 with POSIX subsystem.
(defun ange-ftp-bs2000-posix-host (host)
  (and ange-ftp-bs2000-posix-host-regexp
       (string-match-p ange-ftp-bs2000-posix-host-regexp host)))

(defun ange-ftp-add-bs2000-host (host)
  "Mark HOST as the name of a machine running BS2000."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (car (ange-ftp-ftp-name name)))))))
  (if (not (ange-ftp-bs2000-host host))
      (setq ange-ftp-bs2000-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-bs2000-host-regexp "\\|")
		    ange-ftp-bs2000-host-regexp)
	    ange-ftp-host-cache nil)))

(defun ange-ftp-add-bs2000-posix-host (host)
  "Mark HOST as the name of a machine running BS2000 with POSIX subsystem."
  (interactive
   (list (read-string "Host: "
		      (let ((name (or (buffer-file-name) default-directory)))
			(and name (car (ange-ftp-ftp-name name)))))))
  (if (not (ange-ftp-bs2000-posix-host host))
      (setq ange-ftp-bs2000-posix-host-regexp
	    (concat "^" (regexp-quote host) "$"
		    (and ange-ftp-bs2000-posix-host-regexp "\\|")
		    ange-ftp-bs2000-posix-host-regexp)
	    ange-ftp-host-cache nil))
  ;; Install CD hook to cd to posix on connecting:
  (add-hook 'ange-ftp-process-startup-hook 'ange-ftp-bs2000-cd-to-posix)
  host)

(defconst ange-ftp-bs2000-filename-regexp
  (concat
   "\\(" ange-ftp-bs2000-filename-prefix-regexp "\\)?"
   "\\(" ange-ftp-bs2000-short-filename-regexp "\\)")
  "Regular expression to match for a valid BS2000 file name.")

(defcustom ange-ftp-bs2000-additional-pubsets
  nil
  "*List of additional pubsets available to all users."
  :group 'ange-ftp
  :type '(repeat string))

;; These parsing functions are as general as possible because the syntax
;; of ftp listings from BS2000 hosts is a bit erratic. What saves us is that
;; the BS2000 filename syntax is so rigid.

;; Extract the next filename from a BS2000 dired-like listing.
(defun ange-ftp-parse-bs2000-filename ()
  (if (re-search-forward ange-ftp-bs2000-filename-regexp nil t)
      (match-string 2)))

;; Parse the current buffer which is assumed to be in (some) BS2000 FTP dir
;; format, and return a hashtable as the result.
(defun ange-ftp-parse-bs2000-listing ()
  (let ((tbl (make-hash-table :test 'equal))
	pubset
	file)
    ;; get current pubset
    (goto-char (point-min))
    (if (re-search-forward ange-ftp-bs2000-filename-pubset-regexp nil t)
	(setq pubset (match-string 0)))
    ;; add files to hashtable
    (goto-char (point-min))
    (save-match-data
      (while (setq file (ange-ftp-parse-bs2000-filename))
	(puthash file nil tbl)))
    ;; add . and ..
    (puthash "." t tbl)
    (puthash ".." t tbl)
    ;; add all additional pubsets, if not listing one of them
    (if (not (member pubset ange-ftp-bs2000-additional-pubsets))
	(mapc (lambda (pubset) (puthash pubset t tbl))
	      ange-ftp-bs2000-additional-pubsets))
    tbl))

(add-to-list 'ange-ftp-parse-list-func-alist
	     '(bs2000 . ange-ftp-parse-bs2000-listing))

(defun ange-ftp-bs2000-cd-to-posix ()
  "cd to POSIX subsystem if the current host matches
`ange-ftp-bs2000-posix-host-regexp'.  All BS2000 hosts with POSIX subsystem
MUST BE EXPLICITLY SET with `ange-ftp-add-bs2000-posix-host' for they cannot
be recognized automatically (they are all valid BS2000 hosts too)."
  (if (and ange-ftp-this-host (ange-ftp-bs2000-posix-host ange-ftp-this-host))
      (progn
	;; change to POSIX:
;	(ange-ftp-raw-send-cmd proc "cd %POSIX")
	(ange-ftp-cd ange-ftp-this-host ange-ftp-this-user "%POSIX")
	;; put new home directory in the expand-dir hashtable.
	;; `ange-ftp-this-host' and `ange-ftp-this-user' are bound in
	;; ange-ftp-get-process.
	(puthash (concat ange-ftp-this-host "/" ange-ftp-this-user "/~")
		 (car (ange-ftp-get-pwd ange-ftp-this-host ange-ftp-this-user))
		 ange-ftp-expand-dir-hashtable))))

;; Not available yet:
;; ange-ftp-bs2000-delete-file-entry
;; ange-ftp-bs2000-add-file-entry
;; ange-ftp-bs2000-file-name-as-directory
;; ange-ftp-bs2000-make-compressed-filename
;; ange-ftp-bs2000-file-name-sans-versions

;;;; ------------------------------------------------------------
;;;; Finally provide package.
;;;; ------------------------------------------------------------

(provide 'ange-ftp)

;;; ange-ftp.el ends here
