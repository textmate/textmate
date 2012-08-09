;;; spam.el --- Identifying spam

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Maintainer: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: network, spam, mail, bogofilter, BBDB, dspam, dig, whitelist, blacklist, gmane, hashcash, spamassassin, bsfilter, ifile, stat, crm114, spamoracle

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

;;; This module addresses a few aspects of spam control under Gnus.  Page
;;; breaks are used for grouping declarations and documentation relating to
;;; each particular aspect.

;;; The integration with Gnus is not yet complete.  See various `FIXME'
;;; comments, below, for supplementary explanations or discussions.

;;; Several TODO items are marked as such

;; TODO: cross-server splitting, remote processing, training through files

;;; Code:

;;{{{ compilation directives and autoloads/requires

;; For Emacs <22.2 and XEmacs.
(eval-and-compile
  (unless (fboundp 'declare-function) (defmacro declare-function (&rest r))))

(eval-when-compile (require 'cl))

(require 'message)              ;for the message-fetch-field functions
(require 'gnus-sum)
(require 'gnus-uu)                      ; because of key prefix issues
;;; for the definitions of group content classification and spam processors
(require 'gnus)

(eval-when-compile (require 'spam-report))
(eval-when-compile (require 'hashcash))

;; for nnimap-split-download-body-default
(eval-when-compile (require 'nnimap))

;; autoload query-dig
(autoload 'query-dig "dig")

;; autoload spam-report
(eval-and-compile
  (autoload 'spam-report-gmane "spam-report")
  (autoload 'spam-report-gmane-spam "spam-report")
  (autoload 'spam-report-gmane-ham "spam-report")
  (autoload 'spam-report-resend "spam-report"))

;; autoload gnus-registry
(autoload 'gnus-registry-group-count "gnus-registry")
(autoload 'gnus-registry-get-id-key "gnus-registry")
(autoload 'gnus-registry-set-id-key "gnus-registry")
(autoload 'gnus-registry-handle-action "gnus-registry")

;; autoload dns-query
(autoload 'dns-query "dns")

;;}}}

;;{{{ Main parameters.
(defvar spam-backends nil
  "List of spam.el backends with all the pertinent data.
Populated by `spam-install-backend-super'.")

(defgroup spam nil
  "Spam configuration."
  :version "22.1"
  :group 'mail
  :group 'news)

(defcustom spam-summary-exit-behavior 'default
  "Exit behavior at the time of summary exit.
Note that setting the `spam-use-move' or `spam-use-copy' backends on
a group through group/topic parameters overrides this mechanism."
  :type '(choice
          (const
           'default
           :tag "Move spam out of all groups and ham out of spam groups.")
          (const
           'move-all
           :tag "Move spam out of all groups and ham out of all groups.")
          (const
           'move-none
           :tag "Never move spam or ham out of any groups."))
  :group 'spam)

(defcustom spam-directory (nnheader-concat gnus-directory "spam/")
  "Directory for spam whitelists and blacklists."
  :type 'directory
  :group 'spam)

(defcustom spam-mark-new-messages-in-spam-group-as-spam t
  "Whether new messages in a spam group should get the spam-mark."
  :type 'boolean
  ;; :version "22.1" ;; Gnus 5.10.8 / No Gnus 0.3
  :group 'spam)

(defcustom spam-log-to-registry nil
  "Whether spam/ham processing should be logged in the registry."
  :type 'boolean
  :group 'spam)

(defcustom spam-split-symbolic-return nil
  "Whether `spam-split' should work with symbols or group names."
  :type 'boolean
  :group 'spam)

(defcustom spam-split-symbolic-return-positive nil
  "Whether `spam-split' should ALWAYS work with symbols or group names.
Do not set this if you use `spam-split' in a fancy split method."
  :type 'boolean
  :group 'spam)

(defcustom spam-mark-only-unseen-as-spam t
  "Whether only unseen articles should be marked as spam in spam groups.
When nil, all unread articles in a spam group are marked as
spam.  Set this if you want to leave an article unread in a spam group
without losing it to the automatic spam-marking process."
  :type 'boolean
  :group 'spam)

(defcustom spam-mark-ham-unread-before-move-from-spam-group nil
  "Whether ham should be marked unread before it's moved.
The article is moved out of a spam group according to `ham-process-destination'.
This variable is an official entry in the international Longest Variable Name
Competition."
  :type 'boolean
  :group 'spam)

(defcustom spam-disable-spam-split-during-ham-respool nil
  "Whether `spam-split' should be ignored while resplitting ham.
This is useful to prevent ham from ending up in the same spam
group after the resplit.  Don't set this to t if you have `spam-split' as the
last rule in your split configuration."
  :type 'boolean
  :group 'spam)

(defcustom spam-autodetect-recheck-messages nil
  "Should spam.el recheck all messages when autodetecting?
Normally this is nil, so only unseen messages will be checked."
  :type 'boolean
  :group 'spam)

(defcustom spam-whitelist (expand-file-name "whitelist" spam-directory)
  "The location of the whitelist.
The file format is one regular expression per line.
The regular expression is matched against the address."
  :type 'file
  :group 'spam)

(defcustom spam-blacklist (expand-file-name "blacklist" spam-directory)
  "The location of the blacklist.
The file format is one regular expression per line.
The regular expression is matched against the address."
  :type 'file
  :group 'spam)

(defcustom spam-use-dig t
  "Whether `query-dig' should be used instead of `dns-query'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-gmane-xref nil
  "Whether the Gmane spam xref should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-blacklist nil
  "Whether the blacklist should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-blacklist-ignored-regexes nil
  "Regular expressions that the blacklist should ignore."
  :type '(repeat (regexp :tag "Regular expression to ignore when blacklisting"))
  :group 'spam)

(defcustom spam-use-whitelist nil
  "Whether the whitelist should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-whitelist-exclusive nil
  "Whether whitelist-exclusive should be used by `spam-split'.
Exclusive whitelisting means that all messages from senders not in the whitelist
are considered spam."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-blackholes nil
  "Whether blackholes should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-hashcash nil
  "Whether hashcash payments should be detected by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-regex-headers nil
  "Whether a header regular expression match should be used by `spam-split'.
Also see the variables `spam-regex-headers-spam' and `spam-regex-headers-ham'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-regex-body nil
  "Whether a body regular expression match should be used by `spam-split'.
Also see the variables `spam-regex-body-spam' and `spam-regex-body-ham'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bogofilter-headers nil
  "Whether bogofilter headers should be used by `spam-split'.
Enable this if you pre-process messages with Bogofilter BEFORE Gnus sees them."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bogofilter nil
  "Whether bogofilter should be invoked by `spam-split'.
Enable this if you want Gnus to invoke Bogofilter on new messages."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bsfilter-headers nil
  "Whether bsfilter headers should be used by `spam-split'.
Enable this if you pre-process messages with Bsfilter BEFORE Gnus sees them."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-bsfilter nil
  "Whether bsfilter should be invoked by `spam-split'.
Enable this if you want Gnus to invoke Bsfilter on new messages."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-BBDB nil
  "Whether BBDB should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-BBDB-exclusive nil
  "Whether BBDB-exclusive should be used by `spam-split'.
Exclusive BBDB means that all messages from senders not in the BBDB are
considered spam."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-ifile nil
  "Whether ifile should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-stat nil
  "Whether `spam-stat' should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-spamoracle nil
  "Whether spamoracle should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-spamassassin nil
  "Whether spamassassin should be invoked by `spam-split'.
Enable this if you want Gnus to invoke SpamAssassin on new messages."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-spamassassin-headers nil
  "Whether spamassassin headers should be checked by `spam-split'.
Enable this if you pre-process messages with SpamAssassin BEFORE Gnus sees
them."
  :type 'boolean
  :group 'spam)

(defcustom spam-use-crm114 nil
  "Whether the CRM114 Mailfilter should be used by `spam-split'."
  :type 'boolean
  :group 'spam)

(defcustom spam-install-hooks (or
                               spam-use-dig
                               spam-use-gmane-xref
                               spam-use-blacklist
                               spam-use-whitelist
                               spam-use-whitelist-exclusive
                               spam-use-blackholes
                               spam-use-hashcash
                               spam-use-regex-headers
                               spam-use-regex-body
                               spam-use-bogofilter
                               spam-use-bogofilter-headers
                               spam-use-spamassassin
                               spam-use-spamassassin-headers
                               spam-use-bsfilter
                               spam-use-bsfilter-headers
                               spam-use-BBDB
                               spam-use-BBDB-exclusive
                               spam-use-ifile
                               spam-use-stat
                               spam-use-spamoracle
                               spam-use-crm114)
  "Whether the spam hooks should be installed.
Default to t if one of the spam-use-* variables is set."
  :group 'spam
  :type 'boolean)

(defcustom spam-split-group "spam"
  "Group name where incoming spam should be put by `spam-split'."
  :type 'string
  :group 'spam)

;;; TODO: deprecate this variable, it's confusing since it's a list of strings,
;;; not regular expressions
(defcustom spam-junk-mailgroups (cons
                                 spam-split-group
                                 '("mail.junk" "poste.pourriel"))
  "Mailgroups with spam contents.
All unmarked article in such group receive the spam mark on group entry."
  :type '(repeat (string :tag "Group"))
  :group 'spam)


(defcustom spam-gmane-xref-spam-group "gmane.spam.detected"
  "The group where spam xrefs can be found on Gmane.
Only meaningful if you enable `spam-use-gmane-xref'."
  :type 'string
  :group 'spam)

(defcustom spam-blackhole-servers '("bl.spamcop.net" "relays.ordb.org"
                                    "dev.null.dk" "relays.visi.com")
  "List of blackhole servers.
Only meaningful if you enable `spam-use-blackholes'."
  :type '(repeat (string :tag "Server"))
  :group 'spam)

(defcustom spam-blackhole-good-server-regex nil
  "String matching IP addresses that should not be checked in the blackholes.
Only meaningful if you enable `spam-use-blackholes'."
  :type '(radio (const nil) regexp)
  :group 'spam)

(defface spam
  '((((class color) (type tty) (background dark))
     (:foreground "gray80" :background "gray50"))
    (((class color) (type tty) (background light))
     (:foreground "gray50" :background "gray80"))
    (((class color) (background dark))
     (:foreground "ivory2"))
    (((class color) (background light))
     (:foreground "ivory4"))
    (t :inverse-video t))
  "Face for spam-marked articles."
  :group 'spam)
;; backward-compatibility alias
(put 'spam-face 'face-alias 'spam)
(put 'spam-face 'obsolete-face "22.1")

(defcustom spam-face 'spam
  "Face for spam-marked articles."
  :type 'face
  :group 'spam)

(defcustom spam-regex-headers-spam '("^X-Spam-Flag: YES")
  "Regular expression for positive header spam matches.
Only meaningful if you enable `spam-use-regex-headers'."
  :type '(repeat (regexp :tag "Regular expression to match spam header"))
  :group 'spam)

(defcustom spam-regex-headers-ham '("^X-Spam-Flag: NO")
  "Regular expression for positive header ham matches.
Only meaningful if you enable `spam-use-regex-headers'."
  :type '(repeat (regexp :tag "Regular expression to match ham header"))
  :group 'spam)

(defcustom spam-regex-body-spam '()
  "Regular expression for positive body spam matches.
Only meaningful if you enable `spam-use-regex-body'."
  :type '(repeat (regexp :tag "Regular expression to match spam body"))
  :group 'spam)

(defcustom spam-regex-body-ham '()
  "Regular expression for positive body ham matches.
Only meaningful if you enable `spam-use-regex-body'."
  :type '(repeat (regexp :tag "Regular expression to match ham body"))
  :group 'spam)

(defcustom spam-summary-score-preferred-header nil
  "Preferred header to use for `spam-summary-score'."
  :type '(choice :tag "Header name"
          (symbol :tag "SpamAssassin etc" X-Spam-Status)
          (symbol :tag "Bogofilter"       X-Bogosity)
          (const  :tag "No preference, take best guess." nil))
  :group 'spam)

(defgroup spam-ifile nil
  "Spam ifile configuration."
  :group 'spam)

(make-obsolete-variable 'spam-ifile-path 'spam-ifile-program
                        "Gnus 5.10.9 (Emacs 22.1)")
(defcustom spam-ifile-program (executable-find "ifile")
  "Name of the ifile program."
  :type '(choice (file :tag "Location of ifile")
                 (const :tag "ifile is not installed"))
  :group 'spam-ifile)

(make-obsolete-variable 'spam-ifile-database-path 'spam-ifile-database
                        "Gnus 5.10.9 (Emacs 22.1)")
(defcustom spam-ifile-database nil
  "File name of the ifile database."
  :type '(choice (file :tag "Location of the ifile database")
                 (const :tag "Use the default"))
  :group 'spam-ifile)

(defcustom spam-ifile-spam-category "spam"
  "Name of the spam ifile category."
  :type 'string
  :group 'spam-ifile)

(defcustom spam-ifile-ham-category nil
  "Name of the ham ifile category.
If nil, the current group name will be used."
  :type '(choice (string :tag "Use a fixed category")
                 (const :tag "Use the current group name"))
  :group 'spam-ifile)

(defcustom spam-ifile-all-categories nil
  "Whether the ifile check will return all categories, or just spam.
Set this to t if you want to use the `spam-split' invocation of ifile as
your main source of newsgroup names."
  :type 'boolean
  :group 'spam-ifile)

(defgroup spam-bogofilter nil
  "Spam bogofilter configuration."
  :group 'spam)

(make-obsolete-variable 'spam-bogofilter-path 'spam-bogofilter-program
                        "Gnus 5.10.9 (Emacs 22.1)")
(defcustom spam-bogofilter-program (executable-find "bogofilter")
  "Name of the Bogofilter program."
  :type '(choice (file :tag "Location of bogofilter")
                 (const :tag "Bogofilter is not installed"))
  :group 'spam-bogofilter)

(defvar spam-bogofilter-valid 'unknown "Is the bogofilter version valid?")

(defcustom spam-bogofilter-header "X-Bogosity"
  "The header that Bogofilter inserts in messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-spam-switch "-s"
  "The switch that Bogofilter uses to register spam messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-ham-switch "-n"
  "The switch that Bogofilter uses to register ham messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-spam-strong-switch "-S"
  "The switch that Bogofilter uses to unregister ham messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-ham-strong-switch "-N"
  "The switch that Bogofilter uses to unregister spam messages."
  :type 'string
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-bogosity-positive-spam-header "^\\(Yes\\|Spam\\)"
  "The regex on `spam-bogofilter-header' for positive spam identification."
  :type 'regexp
  :group 'spam-bogofilter)

(defcustom spam-bogofilter-database-directory nil
  "Location of the Bogofilter database.
When nil, use the default location."
  :type '(choice (directory
                  :tag "Location of the Bogofilter database directory")
                 (const :tag "Use the default"))
  :group 'spam-bogofilter)

(defgroup spam-bsfilter nil
  "Spam bsfilter configuration."
  :group 'spam)

(make-obsolete-variable 'spam-bsfilter-path 'spam-bsfilter-program
                        "Gnus 5.10.9 (Emacs 22.1)")
(defcustom spam-bsfilter-program (executable-find "bsfilter")
  "Name of the Bsfilter program."
  :type '(choice (file :tag "Location of bsfilter")
                 (const :tag "Bsfilter is not installed"))
  :group 'spam-bsfilter)

(defcustom spam-bsfilter-header "X-Spam-Flag"
  "The header inserted by Bsfilter to flag spam."
  :type 'string
  :group 'spam-bsfilter)

(defcustom spam-bsfilter-probability-header "X-Spam-Probability"
  "The header that Bsfilter inserts in messages."
  :type 'string
  :group 'spam-bsfilter)

(defcustom spam-bsfilter-spam-switch "--add-spam"
  "The switch that Bsfilter uses to register spam messages."
  :type 'string
  :group 'spam-bsfilter)

(defcustom spam-bsfilter-ham-switch "--add-clean"
  "The switch that Bsfilter uses to register ham messages."
  :type 'string
  :group 'spam-bsfilter)

(defcustom spam-bsfilter-spam-strong-switch "--sub-spam"
  "The switch that Bsfilter uses to unregister ham messages."
  :type 'string
  :group 'spam-bsfilter)

(defcustom spam-bsfilter-ham-strong-switch "--sub-clean"
  "The switch that Bsfilter uses to unregister spam messages."
  :type 'string
  :group 'spam-bsfilter)

(defcustom spam-bsfilter-database-directory nil
  "Directory path of the Bsfilter databases."
  :type '(choice (directory
                  :tag "Location of the Bsfilter database directory")
                 (const :tag "Use the default"))
  :group 'spam-bsfilter)

(defgroup spam-spamoracle nil
  "Spam spamoracle configuration."
  :group 'spam)

(defcustom spam-spamoracle-database nil
  "Location of spamoracle database file.
When nil, use the default spamoracle database."
  :type '(choice (directory :tag "Location of spamoracle database file.")
                 (const :tag "Use the default"))
  :group 'spam-spamoracle)

(defcustom spam-spamoracle-binary (executable-find "spamoracle")
  "Location of the spamoracle binary."
  :type '(choice (directory :tag "Location of the spamoracle binary")
                 (const :tag "Use the default"))
  :group 'spam-spamoracle)

(defgroup spam-spamassassin nil
  "Spam SpamAssassin configuration."
  :group 'spam)

(make-obsolete-variable 'spam-spamassassin-path
  'spam-spamassassin-program "Gnus 5.10.9 (Emacs 22.1)")
(defcustom spam-assassin-program (executable-find "spamassassin")
  "Name of the spamassassin program.
Hint: set this to \"spamc\" if you have spamd running.  See the spamc and
spamd man pages for more information on these programs."
  :type '(choice (file :tag "Location of spamc")
                 (const :tag "spamassassin is not installed"))
  :group 'spam-spamassassin)

(defcustom spam-spamassassin-arguments ()
  "Arguments to pass to the spamassassin executable.
This must be a list.  For example, `(\"-C\" \"configfile\")'."
  :type '(restricted-sexp :match-alternatives (listp))
  :group 'spam-spamassassin)

(defcustom spam-spamassassin-spam-flag-header "X-Spam-Flag"
  "The header inserted by SpamAssassin to flag spam."
  :type 'string
  :group 'spam-spamassassin)

(defcustom spam-spamassassin-positive-spam-flag-header "YES"
  "The regex on `spam-spamassassin-spam-flag-header' for positive spam
identification"
  :type 'string
  :group 'spam-spamassassin)

(defcustom spam-spamassassin-spam-status-header "X-Spam-Status"
  "The header inserted by SpamAssassin, giving extended scoring information"
  :type 'string
  :group 'spam-spamassassin)

(make-obsolete-variable 'spam-sa-learn-path 'spam-sa-learn-program
                        "Gnus 5.10.9 (Emacs 22.1)")
(defcustom spam-sa-learn-program (executable-find "sa-learn")
  "Name of the sa-learn program."
  :type '(choice (file :tag "Location of spamassassin")
                 (const :tag "spamassassin is not installed"))
  :group 'spam-spamassassin)

(defcustom spam-sa-learn-rebuild t
  "Whether sa-learn should rebuild the database every time it is called
Enable this if you want sa-learn to rebuild the database automatically.  Doing
this will slightly increase the running time of the spam registration process.
If you choose not to do this, you will have to run \"sa-learn --rebuild\" in
order for SpamAssassin to recognize the new registered spam."
  :type 'boolean
  :group 'spam-spamassassin)

(defcustom spam-sa-learn-spam-switch "--spam"
  "The switch that sa-learn uses to register spam messages."
  :type 'string
  :group 'spam-spamassassin)

(defcustom spam-sa-learn-ham-switch "--ham"
  "The switch that sa-learn uses to register ham messages."
  :type 'string
  :group 'spam-spamassassin)

(defcustom spam-sa-learn-unregister-switch "--forget"
  "The switch that sa-learn uses to unregister messages messages."
  :type 'string
  :group 'spam-spamassassin)

(defgroup spam-crm114 nil
  "Spam CRM114 Mailfilter configuration."
  :group 'spam)

(defcustom spam-crm114-program (executable-find "mailfilter.crm")
  "File path of the CRM114 Mailfilter executable program."
  :type '(choice (file :tag "Location of CRM114 Mailfilter")
         (const :tag "CRM114 Mailfilter is not installed"))
  :group 'spam-crm114)

(defcustom spam-crm114-header "X-CRM114-Status"
  "The header that CRM114 Mailfilter inserts in messages."
  :type 'string
  :group 'spam-crm114)

(defcustom spam-crm114-spam-switch "--learnspam"
  "The switch that CRM114 Mailfilter uses to register spam messages."
  :type 'string
  :group 'spam-crm114)

(defcustom spam-crm114-ham-switch "--learnnonspam"
  "The switch that CRM114 Mailfilter uses to register ham messages."
  :type 'string
  :group 'spam-crm114)

(defcustom spam-crm114-spam-strong-switch "--unlearn"
  "The switch that CRM114 Mailfilter uses to unregister ham messages."
  :type 'string
  :group 'spam-crm114)

(defcustom spam-crm114-ham-strong-switch "--unlearn"
  "The switch that CRM114 Mailfilter uses to unregister spam messages."
  :type 'string
  :group 'spam-crm114)

(defcustom spam-crm114-positive-spam-header "^SPAM"
  "The regex on `spam-crm114-header' for positive spam identification."
  :type 'regexp
  :group 'spam-crm114)

(defcustom spam-crm114-database-directory nil
  "Directory path of the CRM114 Mailfilter databases."
  :type '(choice (directory
          :tag "Location of the CRM114 Mailfilter database directory")
         (const :tag "Use the default"))
  :group 'spam-crm114)

;;; Key bindings for spam control.

(gnus-define-keys gnus-summary-mode-map
  "St" spam-generic-score
  "Sx" gnus-summary-mark-as-spam
  "Mst" spam-generic-score
  "Msx" gnus-summary-mark-as-spam
  "\M-d" gnus-summary-mark-as-spam
  "$" gnus-summary-mark-as-spam)

(defvar spam-cache-lookups t
  "Whether spam.el will try to cache lookups using `spam-caches'.")

(defvar spam-caches (make-hash-table
                     :size 10
                     :test 'equal)
  "Cache of spam detection entries.")

(defvar spam-old-articles nil
  "List of old ham and spam articles, generated when a group is entered.")

(defvar spam-split-disabled nil
  "If non-nil, `spam-split' is disabled, and always returns nil.")

(defvar spam-split-last-successful-check nil
  "Internal variable.
`spam-split' will set this to nil or a spam-use-XYZ check if it
finds ham or spam.")

;; internal variables for backends
;; TODO: find a way to create these on the fly in spam-install-backend-super
(defvar spam-use-copy nil)
(defvar spam-use-move nil)
(defvar spam-use-gmane nil)
(defvar spam-use-resend nil)

;;}}}

;;{{{ convenience functions

(defun spam-clear-cache (symbol)
  "Clear the `spam-caches' entry for a check."
  (remhash symbol spam-caches))

(defun spam-xor (a b)
  "Logical A xor B."
  (and (or a b) (not (and a b))))

(defun spam-set-difference (list1 list2)
  "Return a set difference of LIST1 and LIST2.
When either list is nil, the other is returned."
  (if (and list1 list2)
      ;; we have two non-nil lists
      (progn
        (dolist (item (append list1 list2))
          (when (and (memq item list1) (memq item list2))
            (setq list1 (delq item list1))
            (setq list2 (delq item list2))))
        (append list1 list2))
    ;; if either of the lists was nil, return the other one
    (if list1 list1 list2)))

(defun spam-group-ham-mark-p (group mark &optional spam)
  "Checks if MARK is considered a ham mark in GROUP."
  (when (stringp group)
    (let* ((marks (spam-group-ham-marks group spam))
           (marks (if (symbolp mark)
                      marks
                    (mapcar 'symbol-value marks))))
      (memq mark marks))))

(defun spam-group-spam-mark-p (group mark)
  "Checks if MARK is considered a spam mark in GROUP."
  (spam-group-ham-mark-p group mark t))

(defun spam-group-ham-marks (group &optional spam)
  "In GROUP, get all the ham marks."
  (when (stringp group)
    (let* ((marks (if spam
                      (gnus-parameter-spam-marks group)
                    (gnus-parameter-ham-marks group)))
           (marks (car marks))
           (marks (if (listp (car marks)) (car marks) marks)))
      marks)))

(defun spam-group-spam-marks (group)
  "In GROUP, get all the spam marks."
  (spam-group-ham-marks group t))

(defun spam-group-spam-contents-p (group)
  "Is GROUP a spam group?"
  (if (and (stringp group) (< 0 (length group)))
      (or (member group spam-junk-mailgroups)
          (memq 'gnus-group-spam-classification-spam
                (gnus-parameter-spam-contents group)))
    nil))

(defun spam-group-ham-contents-p (group)
  "Is GROUP a ham group?"
  (if (stringp group)
      (memq 'gnus-group-spam-classification-ham
            (gnus-parameter-spam-contents group))
    nil))

(defun spam-classifications ()
  "Return list of valid classifications"
  '(spam ham))

(defun spam-classification-valid-p (classification)
  "Is CLASSIFICATION a valid spam/ham classification?"
  (memq classification (spam-classifications)))

(defun spam-backend-properties ()
  "Return list of valid classifications."
  '(statistical mover check hrf srf huf suf))

(defun spam-backend-property-valid-p (property)
  "Is PROPERTY a valid backend property?"
  (memq property (spam-backend-properties)))

(defun spam-backend-function-type-valid-p (type)
  (or (eq type 'registration)
      (eq type 'unregistration)))

(defun spam-process-type-valid-p (process-type)
  (or (eq process-type 'incoming)
      (eq process-type 'process)))

(defun spam-list-articles (articles classification)
  (let ((mark-check (if (eq classification 'spam)
                        'spam-group-spam-mark-p
                      'spam-group-ham-mark-p))
        alist mark-cache-yes mark-cache-no)
    (dolist (article articles)
      (let ((mark (gnus-summary-article-mark article)))
        (unless (or (memq mark mark-cache-yes)
                    (memq mark mark-cache-no))
          (if (funcall mark-check
                       gnus-newsgroup-name
                       mark)
              (push mark mark-cache-yes)
            (push mark mark-cache-no)))
        (when (memq mark mark-cache-yes)
          (push article alist))))
    alist))

;;}}}

;;{{{ backend installation functions and procedures

(defun spam-install-backend-super (backend &rest properties)
  "Install BACKEND for spam.el.
Accepts incoming CHECK, ham registration function HRF, spam
registration function SRF, ham unregistration function HUF, spam
unregistration function SUF, and an indication whether the
backend is STATISTICAL."
  (setq spam-backends (add-to-list 'spam-backends backend))
  (while properties
    (let ((property (pop properties))
          (value (pop properties)))
      (if (spam-backend-property-valid-p property)
          (put backend property value)
        (gnus-error
         5
         "spam-install-backend-super got an invalid property %s"
         property)))))

(defun spam-backend-list (&optional type)
  "Return a list of all the backend symbols, constrained by TYPE.
When TYPE is 'non-mover, only non-mover backends are returned.
When TYPE is 'mover, only mover backends are returned."
  (let (list)
    (dolist (backend spam-backends)
      (when (or
             (null type)                ;either no type was requested
             ;; or the type is 'mover and the backend is a mover
             (and
              (eq type 'mover)
              (spam-backend-mover-p backend))
             ;; or the type is 'non-mover and the backend is not a mover
             (and
              (eq type 'non-mover)
              (not (spam-backend-mover-p backend))))
        (push backend list)))
      list))

(defun spam-backend-check (backend)
  "Get the check function for BACKEND.
Each individual check may return nil, t, or a mailgroup name.
The value nil means that the check does not yield a decision, and
so, that further checks are needed.  The value t means that the
message is definitely not spam, and that further spam checks
should be inhibited.  Otherwise, a mailgroup name or the symbol
'spam (depending on `spam-split-symbolic-return') is returned where
the mail should go, and further checks are also inhibited.  The
usual mailgroup name is the value of `spam-split-group', meaning
that the message is definitely a spam."
  (get backend 'check))

(defun spam-backend-valid-p (backend)
  "Is BACKEND valid?"
  (member backend (spam-backend-list)))

(defun spam-backend-info (backend)
  "Return information about BACKEND."
  (if (spam-backend-valid-p backend)
      (let (info)
        (setq info (format "Backend %s has the following properties:\n"
                           backend))
        (dolist (property (spam-backend-properties))
          (setq info (format "%s%s=%s\n"
                             info
                             property
                             (get backend property))))
        info)
    (gnus-error 5 "spam-backend-info was asked about an invalid backend %s"
                backend)))

(defun spam-backend-function (backend classification type)
  "Get the BACKEND function for CLASSIFICATION and TYPE.
TYPE is 'registration or 'unregistration.
CLASSIFICATION is 'ham or 'spam."
  (if (and
       (spam-classification-valid-p classification)
       (spam-backend-function-type-valid-p type))
      (let ((retrieval
             (intern
              (format "spam-backend-%s-%s-function"
                      classification
                      type))))
        (funcall retrieval backend))
    (gnus-error
     5
     "%s was passed invalid backend %s, classification %s, or type %s"
     "spam-backend-function"
     backend
     classification
     type)))

(defun spam-backend-article-list-property (classification
                                           &optional unregister)
  "Property name of article list with CLASSIFICATION and UNREGISTER."
  (let* ((r (if unregister "unregister" "register"))
         (prop (format "%s-%s" classification r)))
    prop))

(defun spam-backend-get-article-todo-list (backend
                                           classification
                                           &optional unregister)
  "Get the articles to be processed for BACKEND and CLASSIFICATION.
With UNREGISTER, get articles to be unregistered.
This is a temporary storage function - nothing here persists."
  (get
   backend
   (intern (spam-backend-article-list-property classification unregister))))

(defun spam-backend-put-article-todo-list (backend classification list
                                                   &optional unregister)
  "Set the LIST of articles to be processed for BACKEND and CLASSIFICATION.
With UNREGISTER, set articles to be unregistered.
This is a temporary storage function - nothing here persists."
  (put
   backend
   (intern (spam-backend-article-list-property classification unregister))
   list))

(defun spam-backend-ham-registration-function (backend)
  "Get the ham registration function for BACKEND."
  (get backend 'hrf))

(defun spam-backend-spam-registration-function (backend)
  "Get the spam registration function for BACKEND."
  (get backend 'srf))

(defun spam-backend-ham-unregistration-function (backend)
  "Get the ham unregistration function for BACKEND."
  (get backend 'huf))

(defun spam-backend-spam-unregistration-function (backend)
  "Get the spam unregistration function for BACKEND."
  (get backend 'suf))

(defun spam-backend-statistical-p (backend)
  "Is BACKEND statistical?"
  (get backend 'statistical))

(defun spam-backend-mover-p (backend)
  "Is BACKEND a mover?"
  (get backend 'mover))

(defun spam-install-backend-alias (backend alias)
  "Add ALIAS to an existing BACKEND.
The previous backend settings for ALIAS are erased."

  ;; install alias with no properties at first
  (spam-install-backend-super alias)

  (dolist (property (spam-backend-properties))
    (put alias property (get backend property))))

(defun spam-install-checkonly-backend (backend check)
  "Install a BACKEND than can only CHECK for spam."
  (spam-install-backend-super backend 'check check))

(defun spam-install-mover-backend (backend hrf srf huf suf)
  "Install a BACKEND than can move articles at summary exit.
Accepts ham registration function HRF, spam registration function
SRF, ham unregistration function HUF, spam unregistration
function SUF.  The backend has no incoming check and can't be
statistical."
  (spam-install-backend-super
   backend
   'hrf hrf 'srf srf 'huf huf 'suf suf 'mover t))

(defun spam-install-nocheck-backend (backend hrf srf huf suf)
  "Install a BACKEND than has no check.
Accepts ham registration function HRF, spam registration function
SRF, ham unregistration function HUF, spam unregistration
function SUF.  The backend has no incoming check and can't be
statistical (it could be, but in practice that doesn't happen)."
  (spam-install-backend-super
   backend
   'hrf hrf 'srf srf 'huf huf 'suf suf))

(defun spam-install-backend (backend check hrf srf huf suf)
  "Install a BACKEND.
Accepts incoming CHECK, ham registration function HRF, spam
registration function SRF, ham unregistration function HUF, spam
unregistration function SUF.  The backend won't be
statistical (use `spam-install-statistical-backend' for that)."
  (spam-install-backend-super
   backend
   'check check 'hrf hrf 'srf srf 'huf huf 'suf suf))

(defun spam-install-statistical-backend (backend check hrf srf huf suf)
  "Install a BACKEND.
Accepts incoming CHECK, ham registration function HRF, spam
registration function SRF, ham unregistration function HUF, spam
unregistration function SUF.  The backend will be
statistical (use `spam-install-backend' for non-statistical
backends)."
  (spam-install-backend-super
   backend
   'check check 'statistical t 'hrf hrf 'srf srf 'huf huf 'suf suf))

(defun spam-install-statistical-checkonly-backend (backend check)
  "Install a statistical BACKEND than can only CHECK for spam."
  (spam-install-backend-super
   backend
   'check check 'statistical t))

;;}}}

;;{{{ backend installations
(spam-install-checkonly-backend 'spam-use-blackholes
                                'spam-check-blackholes)

(spam-install-checkonly-backend 'spam-use-hashcash
                                'spam-check-hashcash)

(spam-install-checkonly-backend 'spam-use-spamassassin-headers
                                'spam-check-spamassassin-headers)

(spam-install-checkonly-backend 'spam-use-bogofilter-headers
                                'spam-check-bogofilter-headers)

(spam-install-checkonly-backend 'spam-use-bsfilter-headers
                                'spam-check-bsfilter-headers)

(spam-install-checkonly-backend 'spam-use-gmane-xref
                                'spam-check-gmane-xref)

(spam-install-checkonly-backend 'spam-use-regex-headers
                                'spam-check-regex-headers)

(spam-install-statistical-checkonly-backend 'spam-use-regex-body
                                            'spam-check-regex-body)

;; TODO: NOTE: spam-use-ham-copy is now obsolete, use (ham spam-use-copy)
(spam-install-mover-backend 'spam-use-move
                            'spam-move-ham-routine
                            'spam-move-spam-routine
                            nil
                            nil)

(spam-install-nocheck-backend 'spam-use-copy
                              'spam-copy-ham-routine
                              'spam-copy-spam-routine
                              nil
                              nil)

(spam-install-nocheck-backend 'spam-use-gmane
                              'spam-report-gmane-unregister-routine
                              'spam-report-gmane-register-routine
                              'spam-report-gmane-register-routine
                              'spam-report-gmane-unregister-routine)

(spam-install-nocheck-backend 'spam-use-resend
                              'spam-report-resend-register-ham-routine
                              'spam-report-resend-register-routine
                              nil
                              nil)

(spam-install-backend 'spam-use-BBDB
                      'spam-check-BBDB
                      'spam-BBDB-register-routine
                      nil
                      'spam-BBDB-unregister-routine
                      nil)

(spam-install-backend-alias 'spam-use-BBDB 'spam-use-BBDB-exclusive)

(spam-install-backend 'spam-use-blacklist
                      'spam-check-blacklist
                      nil
                      'spam-blacklist-register-routine
                      nil
                      'spam-blacklist-unregister-routine)

(spam-install-backend 'spam-use-whitelist
                      'spam-check-whitelist
                      'spam-whitelist-register-routine
                      nil
                      'spam-whitelist-unregister-routine
                      nil)

(spam-install-statistical-backend 'spam-use-ifile
                                  'spam-check-ifile
                                  'spam-ifile-register-ham-routine
                                  'spam-ifile-register-spam-routine
                                  'spam-ifile-unregister-ham-routine
                                  'spam-ifile-unregister-spam-routine)

(spam-install-statistical-backend 'spam-use-spamoracle
                                  'spam-check-spamoracle
                                  'spam-spamoracle-learn-ham
                                  'spam-spamoracle-learn-spam
                                  'spam-spamoracle-unlearn-ham
                                  'spam-spamoracle-unlearn-spam)

(spam-install-statistical-backend 'spam-use-stat
                                  'spam-check-stat
                                  'spam-stat-register-ham-routine
                                  'spam-stat-register-spam-routine
                                  'spam-stat-unregister-ham-routine
                                  'spam-stat-unregister-spam-routine)

(spam-install-statistical-backend 'spam-use-spamassassin
                                  'spam-check-spamassassin
                                  'spam-spamassassin-register-ham-routine
                                  'spam-spamassassin-register-spam-routine
                                  'spam-spamassassin-unregister-ham-routine
                                  'spam-spamassassin-unregister-spam-routine)

(spam-install-statistical-backend 'spam-use-bogofilter
                                  'spam-check-bogofilter
                                  'spam-bogofilter-register-ham-routine
                                  'spam-bogofilter-register-spam-routine
                                  'spam-bogofilter-unregister-ham-routine
                                  'spam-bogofilter-unregister-spam-routine)

(spam-install-statistical-backend 'spam-use-bsfilter
                                  'spam-check-bsfilter
                                  'spam-bsfilter-register-ham-routine
                                  'spam-bsfilter-register-spam-routine
                                  'spam-bsfilter-unregister-ham-routine
                                  'spam-bsfilter-unregister-spam-routine)

(spam-install-statistical-backend 'spam-use-crm114
                                  'spam-check-crm114
                                  'spam-crm114-register-ham-routine
                                  'spam-crm114-register-spam-routine
                                  'spam-crm114-unregister-ham-routine
                                  'spam-crm114-unregister-spam-routine)
;;}}}

;;{{{ scoring and summary formatting
(defun spam-necessary-extra-headers ()
  "Return the extra headers spam.el thinks are necessary."
  (let (list)
    (when (or spam-use-spamassassin
              spam-use-spamassassin-headers
              spam-use-regex-headers)
      (push 'X-Spam-Status list))
    (when (or spam-use-bogofilter
              spam-use-regex-headers)
      (push 'X-Bogosity list))
    (when (or spam-use-crm114
              spam-use-regex-headers)
      (push 'X-CRM114-Status list))
    list))

(defun spam-user-format-function-S (headers)
  (when headers
    (format "%3.2f"
            (spam-summary-score headers spam-summary-score-preferred-header))))

(defun spam-article-sort-by-spam-status (h1 h2)
  "Sort articles by score."
  (let (result)
    (dolist (header (spam-necessary-extra-headers))
      (let ((s1 (spam-summary-score h1 header))
            (s2 (spam-summary-score h2 header)))
      (unless (= s1 s2)
        (setq result (< s1 s2))
        (return))))
    result))

(defvar spam-spamassassin-score-regexp
  ".*\\b\\(?:score\\|hits\\)=\\(-?[0-9.]+\\)"
  "Regexp matching SpamAssassin score header.
The first group must match the number.")

(defun spam-extra-header-to-number (header headers)
  "Transform an extra HEADER to a number, using list of HEADERS.
Note this has to be fast."
  (let ((header-content (gnus-extra-header header headers)))
    (if header-content
        (cond
         ((eq header 'X-Spam-Status)
          (string-to-number (gnus-replace-in-string
                             header-content
                             spam-spamassassin-score-regexp
                             "\\1")))
         ;; for CRM checking, it's probably faster to just do the string match
         ((string-match "( pR: \\([0-9.-]+\\)" header-content)
          (- (string-to-number (match-string 1 header-content))))
         ((eq header 'X-Bogosity)
          (string-to-number (gnus-replace-in-string
                             (gnus-replace-in-string
                              header-content
                              ".*spamicity=" "")
                             ",.*" "")))
         (t nil))
      nil)))

(defun spam-summary-score (headers &optional specific-header)
  "Score an article for the summary buffer, as fast as possible.
With SPECIFIC-HEADER, returns only that header's score.
Will not return a nil score."
  (let (score)
    (dolist (header
             (if specific-header
                 (list specific-header)
               (spam-necessary-extra-headers)))
      (setq score
            (spam-extra-header-to-number header headers))
      (when score
        (return)))
    (or score 0)))

(defun spam-generic-score (&optional recheck)
  "Invoke whatever scoring method we can."
  (interactive "P")
  (cond
   ((or spam-use-spamassassin spam-use-spamassassin-headers)
    (spam-spamassassin-score recheck))
   ((or spam-use-bsfilter spam-use-bsfilter-headers)
    (spam-bsfilter-score recheck))
   (spam-use-crm114
    (spam-crm114-score))
   (t (spam-bogofilter-score recheck))))
;;}}}

;;{{{ set up widening, processor checks

;;; set up IMAP widening if it's necessary
(defun spam-setup-widening ()
  (when (spam-widening-needed-p)
    (setq nnimap-split-download-body-default t)))

(defun spam-widening-needed-p (&optional force-symbols)
  (let (found)
    (dolist (backend (spam-backend-list))
      (when (and (spam-backend-statistical-p backend)
                 (or (symbol-value backend)
                     (memq backend force-symbols)))
        (setq found backend)))
    found))

(defvar spam-list-of-processors
  ;; note the nil processors are not defined in gnus.el
  '((gnus-group-spam-exit-processor-bogofilter   spam spam-use-bogofilter)
    (gnus-group-spam-exit-processor-bsfilter     spam spam-use-bsfilter)
    (gnus-group-spam-exit-processor-blacklist    spam spam-use-blacklist)
    (gnus-group-spam-exit-processor-ifile        spam spam-use-ifile)
    (gnus-group-spam-exit-processor-stat         spam spam-use-stat)
    (gnus-group-spam-exit-processor-spamoracle   spam spam-use-spamoracle)
    (gnus-group-spam-exit-processor-spamassassin spam spam-use-spamassassin)
    (gnus-group-spam-exit-processor-report-gmane spam spam-use-gmane) ;; Buggy?
    (gnus-group-ham-exit-processor-ifile         ham spam-use-ifile)
    (gnus-group-ham-exit-processor-bogofilter    ham spam-use-bogofilter)
    (gnus-group-ham-exit-processor-bsfilter      ham spam-use-bsfilter)
    (gnus-group-ham-exit-processor-stat          ham spam-use-stat)
    (gnus-group-ham-exit-processor-whitelist     ham spam-use-whitelist)
    (gnus-group-ham-exit-processor-BBDB          ham spam-use-BBDB)
    (gnus-group-ham-exit-processor-copy          ham spam-use-ham-copy)
    (gnus-group-ham-exit-processor-spamassassin  ham spam-use-spamassassin)
    (gnus-group-ham-exit-processor-spamoracle    ham spam-use-spamoracle))
  "The OBSOLETE `spam-list-of-processors' list.
This list contains pairs associating the obsolete ham/spam exit
processor variables with a classification and a spam-use-*
variable.  When the processor variable is nil, just the
classification and spam-use-* check variable are used.  This is
superseded by the new spam backend code, so it's only consulted
for backwards compatibility.")
(make-obsolete-variable 'spam-list-of-processors nil "22.1")

(defun spam-group-processor-p (group backend &optional classification)
  "Checks if GROUP has a BACKEND with CLASSIFICATION registered.
Also accepts the obsolete processors, which can be found in
gnus.el and in spam-list-of-processors.  In the case of mover
backends, checks the setting of `spam-summary-exit-behavior' in
addition to the set values for the group."
  (if (and (stringp group)
           (symbolp backend))
      (let ((old-style (assq backend spam-list-of-processors))
            (parameters (nth 0 (gnus-parameter-spam-process group)))
            found)
        (if old-style  ; old-style processor
            (spam-group-processor-p group (nth 2 old-style) (nth 1 old-style))
          ;; now search for the parameter
          (dolist (parameter parameters)
            (when (and (null found)
                       (listp parameter)
                       (eq classification (nth 0 parameter))
                       (eq backend (nth 1 parameter)))
              (setq found t)))

          ;; now, if the parameter was not found, do the
          ;; spam-summary-exit-behavior-logic for mover backends
          (unless found
            (when (spam-backend-mover-p backend)
              (setq
               found
               (cond
                ((eq spam-summary-exit-behavior 'move-all) t)
                ((eq spam-summary-exit-behavior 'move-none) nil)
                ((eq spam-summary-exit-behavior 'default)
                 (or (eq classification 'spam) ;move spam out of all groups
                     ;; move ham out of spam groups
                     (and (eq classification 'ham)
                          (spam-group-spam-contents-p group))))
                (t (gnus-error 5 "Unknown spam-summary-exit-behavior: %s"
                               spam-summary-exit-behavior))))))

          found))
    nil))

;;}}}

;;{{{ Summary entry and exit processing.

(defun spam-mark-junk-as-spam-routine ()
  ;; check the global list of group names spam-junk-mailgroups and the
  ;; group parameters
  (when (spam-group-spam-contents-p gnus-newsgroup-name)
    (gnus-message 6 "Marking %s articles as spam"
                  (if spam-mark-only-unseen-as-spam
                      "unseen"
                    "unread"))
    (let ((articles (if spam-mark-only-unseen-as-spam
                        gnus-newsgroup-unseen
                      gnus-newsgroup-unreads)))
      (if spam-mark-new-messages-in-spam-group-as-spam
          (dolist (article articles)
            (gnus-summary-mark-article article gnus-spam-mark))
        (gnus-message 9 "Did not mark new messages as spam.")))))

(defun spam-summary-prepare ()
  (setq spam-old-articles
        (list (cons 'ham (spam-list-articles gnus-newsgroup-articles 'ham))
              (cons 'spam (spam-list-articles gnus-newsgroup-articles 'spam))))
  (spam-mark-junk-as-spam-routine))

;; The spam processors are invoked for any group, spam or ham or neither
(defun spam-summary-prepare-exit ()
  (unless gnus-group-is-exiting-without-update-p
    (gnus-message 6 "Exiting summary buffer and applying spam rules")

    ;; before we begin, remove any article limits
;    (ignore-errors
;      (gnus-summary-pop-limit t))

    ;; first of all, unregister any articles that are no longer ham or spam
    ;; we have to iterate over the processors, or else we'll be too slow
    (dolist (classification (spam-classifications))
      (let* ((old-articles (cdr-safe (assq classification spam-old-articles)))
             (new-articles (spam-list-articles
                            gnus-newsgroup-articles
                            classification))
             (changed-articles (spam-set-difference new-articles old-articles)))
        ;; now that we have the changed articles, we go through the processors
        (dolist (backend (spam-backend-list))
          (let (unregister-list)
            (dolist (article changed-articles)
              (let ((id (spam-fetch-field-message-id-fast article)))
                (when (spam-log-unregistration-needed-p
                       id 'process classification backend)
                  (push article unregister-list))))
            ;; call spam-register-routine with specific articles to unregister,
            ;; when there are articles to unregister and the check is enabled
            (when (and unregister-list (symbol-value backend))
              (spam-backend-put-article-todo-list backend
                                                  classification
                                                  unregister-list
                                                  t))))))

    ;; do the non-moving backends first, then the moving ones
    (dolist (backend-type '(non-mover mover))
      (dolist (classification (spam-classifications))
        (dolist (backend (spam-backend-list backend-type))
          (when (spam-group-processor-p
                 gnus-newsgroup-name
                 backend
                 classification)
            (spam-backend-put-article-todo-list backend
                                                classification
                                                (spam-list-articles
                                                 gnus-newsgroup-articles
                                                 classification))))))

    (spam-resolve-registrations-routine) ; do the registrations now

    ;; we mark all the leftover spam articles as expired at the end
    (dolist (article (spam-list-articles
                      gnus-newsgroup-articles
                      'spam))
      (gnus-summary-mark-article article gnus-expirable-mark)))

  (setq spam-old-articles nil))

;;}}}

;;{{{ spam-use-move and spam-use-copy backend support functions

(defun spam-copy-or-move-routine (copy groups articles classification)

  (when (and (car-safe groups) (listp (car-safe groups)))
    (setq groups (pop groups)))

  (unless (listp groups)
    (setq groups (list groups)))

    ;; remove the current process mark
  (gnus-summary-kill-process-mark)

  (let ((backend-supports-deletions
         (gnus-check-backend-function
          'request-move-article gnus-newsgroup-name))
        (respool-method (gnus-find-method-for-group gnus-newsgroup-name))
        article mark deletep respool valid-move-destinations)

    (when (member 'respool groups)
      (setq respool t)                  ; boolean for later
      (setq groups '("fake"))) ; when respooling, groups are dynamic so fake it

    ;; exclude invalid move destinations
    (dolist (group groups)
      (unless
          (or
           (and
            (eq classification 'spam)
            (spam-group-spam-contents-p gnus-newsgroup-name)
            (spam-group-spam-contents-p group)
            (gnus-message
             3
             "Sorry, can't move spam from spam group %s to spam group %s"
             gnus-newsgroup-name
             group))
           (and
            (eq classification 'ham)
            (spam-group-ham-contents-p gnus-newsgroup-name)
            (spam-group-ham-contents-p group)
            (gnus-message
             3
             "Sorry, can't move ham from ham group %s to ham group %s"
             gnus-newsgroup-name
             group)))
        (push group valid-move-destinations)))

    (setq groups (nreverse valid-move-destinations))

    ;; now do the actual move
    (dolist (group groups)

      (when (and articles (stringp group))

        ;; first, mark the article with the process mark and, if needed,
        ;; the unread or expired mark (for ham and spam respectively)
        (dolist (article articles)
          (when (and (eq classification 'ham)
                     spam-mark-ham-unread-before-move-from-spam-group)
            (gnus-message 9 "Marking ham article %d unread before move"
                          article)
            (gnus-summary-mark-article article gnus-unread-mark))
          (when (and (eq classification 'spam)
                     (not copy))
            (gnus-message 9 "Marking spam article %d expirable before move"
                          article)
            (gnus-summary-mark-article article gnus-expirable-mark))
          (gnus-summary-set-process-mark article)

          (if respool              ; respooling is with a "fake" group
              (let ((spam-split-disabled
                     (or spam-split-disabled
                         (and (eq classification 'ham)
                              spam-disable-spam-split-during-ham-respool))))
                (gnus-message 9 "Respooling article %d with method %s"
                              article respool-method)
                (gnus-summary-respool-article nil respool-method))
            ;; else, we are not respooling
            (if (or (not backend-supports-deletions)
                    (> (length groups) 1))
                (progn              ; if copying, copy and set deletep
                  (gnus-message 9 "Copying article %d to group %s"
                                article group)
                  (gnus-summary-copy-article nil group)
                  (setq deletep t))
              (gnus-message 9 "Moving article %d to group %s"
                            article group)
              (gnus-summary-move-article nil group)))))) ; else move articles

    ;; now delete the articles, unless a) copy is t, and there was a copy done
    ;;                                 b) a move was done to a single group
    ;;                                 c) backend-supports-deletions is nil
    (unless copy
      (when (and deletep backend-supports-deletions)
	(dolist (article articles)
	  (gnus-summary-set-process-mark article)
	  (gnus-message 9 "Deleting article %d" article))
	(when articles
	  (let ((gnus-novice-user nil)) ; don't ask me if I'm sure
	    (gnus-summary-delete-article nil)))))
    (gnus-summary-yank-process-mark)
    (length articles)))

(defun spam-copy-spam-routine (articles)
  (spam-copy-or-move-routine
   t
   (gnus-parameter-spam-process-destination gnus-newsgroup-name)
   articles
   'spam))

(defun spam-move-spam-routine (articles)
  (spam-copy-or-move-routine
   nil
   (gnus-parameter-spam-process-destination gnus-newsgroup-name)
   articles
   'spam))

(defun spam-copy-ham-routine (articles)
  (spam-copy-or-move-routine
   t
   (gnus-parameter-ham-process-destination gnus-newsgroup-name)
   articles
   'ham))

(defun spam-move-ham-routine (articles)
  (spam-copy-or-move-routine
   nil
   (gnus-parameter-ham-process-destination gnus-newsgroup-name)
   articles
   'ham))

;;}}}

;;{{{ article and field retrieval code
(defun spam-get-article-as-string (article)
  (when (numberp article)
    (with-temp-buffer
      (gnus-request-article-this-buffer
       article
       gnus-newsgroup-name)
      (buffer-string))))

;; disabled for now
;; (defun spam-get-article-as-filename (article)
;;   (let ((article-filename))
;;     (when (numberp article)
;;       (nnml-possibly-change-directory
;;        (gnus-group-real-name gnus-newsgroup-name))
;;       (setq article-filename (expand-file-name
;;                              (int-to-string article) nnml-current-directory)))
;;     (if (file-exists-p article-filename)
;;      article-filename
;;       nil)))

(defun spam-fetch-field-fast (article field &optional prepared-data-header)
  "Fetch a FIELD for ARTICLE with the internal `gnus-data-list' function.
When PREPARED-DATA-HEADER is given, don't look in the Gnus data.
When FIELD is 'number, ARTICLE can be any number (since we want
to find it out)."
  (when (numberp article)
    (let* ((data-header (or prepared-data-header
                            (spam-fetch-article-header article))))
      (cond
       ((not (arrayp data-header))
        (gnus-message 6 "Article %d has a nil data header" article))
       ((equal field 'number)
	(mail-header-number data-header))
       ((equal field 'from)
	(mail-header-from data-header))
       ((equal field 'message-id)
	(mail-header-message-id data-header))
       ((equal field 'subject)
	(mail-header-subject data-header))
       ((equal field 'references)
	(mail-header-references data-header))
       ((equal field 'date)
	(mail-header-date data-header))
       ((equal field 'xref)
	(mail-header-xref data-header))
       ((equal field 'extra)
	(mail-header-extra data-header))
       (t
	(gnus-error
	 5
	 "spam-fetch-field-fast: unknown field %s requested"
	 field)
	nil)))))

(defun spam-fetch-field-from-fast (article &optional prepared-data-header)
  (spam-fetch-field-fast article 'from prepared-data-header))

(defun spam-fetch-field-subject-fast (article &optional prepared-data-header)
  (spam-fetch-field-fast article 'subject prepared-data-header))

(defun spam-fetch-field-message-id-fast (article &optional prepared-data-header)
  (spam-fetch-field-fast article 'message-id prepared-data-header))

(defun spam-generate-fake-headers (article)
  (let ((dh (spam-fetch-article-header article)))
    (if dh
        (concat
         (format
          ;; 80-character limit makes for strange constructs
          (concat "From: %s\nSubject: %s\nMessage-ID: %s\n"
                  "Date: %s\nReferences: %s\nXref: %s\n")
          (spam-fetch-field-fast article 'from dh)
          (spam-fetch-field-fast article 'subject dh)
          (spam-fetch-field-fast article 'message-id dh)
          (spam-fetch-field-fast article 'date dh)
          (spam-fetch-field-fast article 'references dh)
          (spam-fetch-field-fast article 'xref dh))
         (when (spam-fetch-field-fast article 'extra dh)
           (format "%s\n" (spam-fetch-field-fast article 'extra dh))))
      (gnus-message
       5
       "spam-generate-fake-headers: article %d didn't have a valid header"
       article))))

(defun spam-fetch-article-header (article)
  (with-current-buffer gnus-summary-buffer
    (gnus-read-header article)
    (nth 3 (assq article gnus-newsgroup-data))))
;;}}}

;;{{{ Spam determination.

(defun spam-split (&rest specific-checks)
  "Split this message into the `spam' group if it is spam.
This function can be used as an entry in the variable `nnmail-split-fancy',
for example like this: (: spam-split).  It can take checks as
parameters.  A string as a parameter will set the
`spam-split-group' to that string.

See the Info node `(gnus)Fancy Mail Splitting' for more details."
  (interactive)
  (setq spam-split-last-successful-check nil)
  (unless spam-split-disabled
    (let ((spam-split-group-choice spam-split-group))
      (dolist (check specific-checks)
        (when (stringp check)
          (setq spam-split-group-choice check)
          (setq specific-checks (delq check specific-checks))))

      (let ((spam-split-group spam-split-group-choice)
            (widening-needed-check (spam-widening-needed-p specific-checks)))
        (save-excursion
          (save-restriction
            (when widening-needed-check
              (widen)
              (gnus-message 8 "spam-split: widening the buffer (%s requires it)"
                            widening-needed-check))
            (let ((backends (spam-backend-list))
                  decision)
              (while (and backends (not decision))
                (let* ((backend (pop backends))
                       (check-function (spam-backend-check backend))
                       (spam-split-group (if spam-split-symbolic-return
                                             'spam
                                           spam-split-group)))
                  (when (or
                         ;; either, given specific checks, this is one of them
                         (memq backend specific-checks)
                         ;; or, given no specific checks, spam-use-CHECK is set
                         (and (null specific-checks) (symbol-value backend)))
                    (gnus-message 6 "spam-split: calling the %s function"
                                  check-function)
                    (setq decision (funcall check-function))
                    ;; if we got a decision at all, save the current check
                    (when decision
                      (setq spam-split-last-successful-check backend))

                    (when (eq decision 'spam)
                      (unless spam-split-symbolic-return
                        (gnus-error
                         5
                         (format "spam-split got %s but %s is nil"
                                 decision
                                 spam-split-symbolic-return)))))))
              (if (eq decision t)
                  (if spam-split-symbolic-return-positive 'ham nil)
                decision))))))))

(defun spam-find-spam ()
  "Detect spam in the current newsgroup using `spam-split'."
  (interactive)

  (let* ((group gnus-newsgroup-name)
         (autodetect (gnus-parameter-spam-autodetect group))
         (methods (gnus-parameter-spam-autodetect-methods group))
         (first-method (nth 0 methods))
         (articles (if spam-autodetect-recheck-messages
                       gnus-newsgroup-articles
                     gnus-newsgroup-unseen))
         article-cannot-be-faked)


    (dolist (backend methods)
      (when (spam-backend-statistical-p backend)
        (setq article-cannot-be-faked t)
        (return)))

    (when (memq 'default methods)
      (setq article-cannot-be-faked t))

    (when (and autodetect
               (not (equal first-method 'none)))
      (mapcar
       (lambda (article)
         (let ((id (spam-fetch-field-message-id-fast article))
               (subject (spam-fetch-field-subject-fast article))
               (sender (spam-fetch-field-from-fast article))
               registry-lookup)

           (unless id
             (gnus-message 6 "Article %d has no message ID!" article))

           (when (and id spam-log-to-registry)
             (setq registry-lookup (spam-log-registration-type id 'incoming))
             (when registry-lookup
               (gnus-message
                9
                "spam-find-spam: message %s was already registered incoming"
                id)))

           (let* ((spam-split-symbolic-return t)
                  (spam-split-symbolic-return-positive t)
                  (fake-headers (spam-generate-fake-headers article))
                  (split-return
                   (or registry-lookup
                       (with-temp-buffer
                         (if article-cannot-be-faked
                             (gnus-request-article-this-buffer
                              article
                              group)
                           ;; else, we fake the article
                           (when fake-headers (insert fake-headers)))
                         (if (or (null first-method)
                                 (equal first-method 'default))
                             (spam-split)
                           (apply 'spam-split methods))))))
             (if (equal split-return 'spam)
                 (gnus-summary-mark-article article gnus-spam-mark))

             (when (and id split-return spam-log-to-registry)
               (when (zerop (gnus-registry-group-count id))
                 (gnus-registry-handle-action id nil group subject sender))

               (unless registry-lookup
                 (spam-log-processing-to-registry
                  id
                  'incoming
                  split-return
                  spam-split-last-successful-check
                  group))))))
       articles))))

;;}}}

;;{{{ registration/unregistration functions

(defun spam-resolve-registrations-routine ()
  "Go through the backends and register or unregister articles as needed."
  (dolist (backend-type '(non-mover mover))
    (dolist (classification (spam-classifications))
      (dolist (backend (spam-backend-list backend-type))
        (let ((rlist (spam-backend-get-article-todo-list
                      backend classification))
              (ulist (spam-backend-get-article-todo-list
                      backend classification t))
              (delcount 0))

          ;; clear the old lists right away
          (spam-backend-put-article-todo-list backend
                                              classification
                                              nil
                                              nil)
          (spam-backend-put-article-todo-list backend
                                              classification
                                              nil
                                              t)

          ;; eliminate duplicates
          (dolist (article (copy-sequence ulist))
            (when (memq article rlist)
              (incf delcount)
              (setq rlist (delq article rlist))
              (setq ulist (delq article ulist))))

          (unless (zerop delcount)
            (gnus-message
             9
             "%d messages did not have to unregister and then register"
             delcount))

          ;; unregister articles
          (unless (zerop (length ulist))
            (let ((num (spam-unregister-routine classification backend ulist)))
              (when (> num 0)
                (gnus-message
                 6
                 "%d %s messages were unregistered by backend %s."
                 num
                 classification
                 backend))))

            ;; register articles
            (unless (zerop (length rlist))
              (let ((num (spam-register-routine classification backend rlist)))
                (when (> num 0)
                  (gnus-message
                   6
                   "%d %s messages were registered by backend %s."
                   num
                   classification
                   backend)))))))))

(defun spam-unregister-routine (classification
                                backend
                                specific-articles)
  (spam-register-routine classification backend specific-articles t))

(defun spam-register-routine (classification
                              backend
                              specific-articles
                              &optional unregister)
  (when (and (spam-classification-valid-p classification)
             (spam-backend-valid-p backend))
    (let* ((register-function
            (spam-backend-function backend classification 'registration))
           (unregister-function
            (spam-backend-function backend classification 'unregistration))
           (run-function (if unregister
                             unregister-function
                           register-function))
           (log-function (if unregister
                             'spam-log-undo-registration
                           'spam-log-processing-to-registry))
           article articles)

      (when run-function
        ;; make list of articles, using specific-articles if given
        (setq articles (or specific-articles
                           (spam-list-articles
                            gnus-newsgroup-articles
                            classification)))
        ;; process them
        (when (> (length articles) 0)
          (gnus-message 5 "%s %d %s articles as %s using backend %s"
                        (if unregister "Unregistering" "Registering")
                        (length articles)
                        (if specific-articles "specific" "")
                        classification
                        backend)
          (funcall run-function articles)
          ;; now log all the registrations (or undo them, depending on
          ;; unregister)
          (dolist (article articles)
            (funcall log-function
                     (spam-fetch-field-message-id-fast article)
                     'process
                     classification
                     backend
                     gnus-newsgroup-name))))
      ;; return the number of articles processed
      (length articles))))

;;; log a ham- or spam-processor invocation to the registry
(defun spam-log-processing-to-registry (id type classification backend group)
  (when spam-log-to-registry
    (if (and (stringp id)
             (stringp group)
             (spam-process-type-valid-p type)
             (spam-classification-valid-p classification)
             (spam-backend-valid-p backend))
        (let ((cell-list (gnus-registry-get-id-key id type))
              (cell (list classification backend group)))
          (push cell cell-list)
          (gnus-registry-set-id-key id type cell-list))

      (gnus-error
       7
       (format
        "%s call with bad ID, type, classification, spam-backend, or group"
        "spam-log-processing-to-registry")))))

;;; check if a ham- or spam-processor registration has been done
(defun spam-log-registered-p (id type)
  (when spam-log-to-registry
    (if (and (stringp id)
             (spam-process-type-valid-p type))
        (gnus-registry-get-id-key id type)
      (progn
        (gnus-error
         7
         (format "%s called with bad ID, type, classification, or spam-backend"
                 "spam-log-registered-p"))
        nil))))

;;; check what a ham- or spam-processor registration says
;;; returns nil if conflicting registrations are found
(defun spam-log-registration-type (id type)
  (let ((count 0)
        decision)
    (dolist (reg (spam-log-registered-p id type))
      (let ((classification (nth 0 reg)))
        (when (spam-classification-valid-p classification)
          (when (and decision
                     (not (eq classification decision)))
            (setq count (+ 1 count)))
          (setq decision classification))))
    (if (< 0 count)
        nil
      decision)))


;;; check if a ham- or spam-processor registration needs to be undone
(defun spam-log-unregistration-needed-p (id type classification backend)
  (when spam-log-to-registry
    (if (and (stringp id)
             (spam-process-type-valid-p type)
             (spam-classification-valid-p classification)
             (spam-backend-valid-p backend))
        (let ((cell-list (gnus-registry-get-id-key id type))
              found)
          (dolist (cell cell-list)
            (unless found
              (when (and (eq classification (nth 0 cell))
                         (eq backend (nth 1 cell)))
                (setq found t))))
          found)
      (progn
        (gnus-error
         7
         (format "%s called with bad ID, type, classification, or spam-backend"
                 "spam-log-unregistration-needed-p"))
        nil))))


;;; undo a ham- or spam-processor registration (the group is not used)
(defun spam-log-undo-registration (id type classification backend
                                      &optional group)
  (when (and spam-log-to-registry
             (spam-log-unregistration-needed-p id type classification backend))
    (if (and (stringp id)
             (spam-process-type-valid-p type)
             (spam-classification-valid-p classification)
             (spam-backend-valid-p backend))
        (let ((cell-list (gnus-registry-get-id-key id type))
              new-cell-list found)
          (dolist (cell cell-list)
            (unless (and (eq classification (nth 0 cell))
                         (eq backend (nth 1 cell)))
              (push cell new-cell-list)))
          (gnus-registry-set-id-key id type new-cell-list))
      (progn
        (gnus-error 7 (format
                       "%s call with bad ID, type, spam-backend, or group"
                       "spam-log-undo-registration"))
        nil))))

;;}}}

;;{{{ backend functions

;;{{{ Gmane xrefs
(defun spam-check-gmane-xref ()
  (let ((header (or
                 (message-fetch-field "Xref")
                 (message-fetch-field "Newsgroups"))))
    (when header                        ; return nil when no header
      (when (string-match spam-gmane-xref-spam-group
                          header)
          spam-split-group))))

;;}}}

;;{{{ Regex body

(defun spam-check-regex-body ()
  (let ((spam-regex-headers-ham spam-regex-body-ham)
        (spam-regex-headers-spam spam-regex-body-spam))
    (spam-check-regex-headers t)))

;;}}}

;;{{{ Regex headers

(defun spam-check-regex-headers (&optional body)
  (let ((type (if body "body" "header"))
        ret found)
    (dolist (h-regex spam-regex-headers-ham)
      (unless found
        (goto-char (point-min))
        (when (re-search-forward h-regex nil t)
          (message "Ham regex %s search positive." type)
          (setq found t))))
    (dolist (s-regex spam-regex-headers-spam)
      (unless found
        (goto-char (point-min))
        (when (re-search-forward s-regex nil t)
          (message "Spam regex %s search positive." type)
          (setq found t)
          (setq ret spam-split-group))))
    ret))

;;}}}

;;{{{ Blackholes.

(defun spam-reverse-ip-string (ip)
  (when (stringp ip)
    (mapconcat 'identity
               (nreverse (split-string ip "\\."))
               ".")))

(defun spam-check-blackholes ()
  "Check the Received headers for blackholed relays."
  (let ((headers (message-fetch-field "received"))
        ips matches)
    (when headers
      (with-temp-buffer
        (insert headers)
        (goto-char (point-min))
        (gnus-message 6 "Checking headers for relay addresses")
        (while (re-search-forward
                "\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" nil t)
          (gnus-message 9 "Blackhole search found host IP %s." (match-string 1))
          (push (spam-reverse-ip-string (match-string 1))
                ips)))
      (dolist (server spam-blackhole-servers)
        (dolist (ip ips)
          (unless (and spam-blackhole-good-server-regex
                       ;; match against the reversed (again) IP string
                       (string-match
                        spam-blackhole-good-server-regex
                        (spam-reverse-ip-string ip)))
            (unless matches
              (let ((query-string (concat ip "." server)))
                (if spam-use-dig
                    (let ((query-result (query-dig query-string)))
                      (when query-result
                        (gnus-message 6 "(DIG): positive blackhole check '%s'"
                                      query-result)
                        (push (list ip server query-result)
                              matches)))
                  ;; else, if not using dig.el
                  (when (dns-query query-string)
                    (gnus-message 6 "positive blackhole check")
                    (push (list ip server (dns-query query-string 'TXT))
                          matches)))))))))
    (when matches
      spam-split-group)))
;;}}}

;;{{{ Hashcash.

(defun spam-check-hashcash ()
  "Check the headers for hashcash payments."
  (ignore-errors (mail-check-payment)))  ;mail-check-payment returns a boolean

;;}}}

;;{{{ BBDB

;;; original idea for spam-check-BBDB from Alexander Kotelnikov
;;; <sacha@giotto.sj.ru>

;; all this is done inside a condition-case to trap errors

(eval-when-compile
  (autoload 'bbdb-buffer "bbdb")
  (autoload 'bbdb-create-internal "bbdb")
  (autoload 'bbdb-search-simple "bbdb"))

;; Autoloaded in message, which we require.
(declare-function gnus-extract-address-components "gnus-util" (from))

(eval-and-compile
  (when (condition-case nil
            (progn
              (require 'bbdb)
              (require 'bbdb-com))
          (file-error
           ;; `bbdb-records' should not be bound as an autoload function
           ;; before loading bbdb because of `bbdb-hashtable-size'.
           (defalias 'bbdb-records 'ignore)
           (defalias 'spam-BBDB-register-routine 'ignore)
           (defalias 'spam-enter-ham-BBDB 'ignore)
           nil))

    ;; when the BBDB changes, we want to clear out our cache
    (defun spam-clear-cache-BBDB (&rest immaterial)
      (spam-clear-cache 'spam-use-BBDB))

    (add-hook 'bbdb-change-hook 'spam-clear-cache-BBDB)

    (defun spam-enter-ham-BBDB (addresses &optional remove)
      "Enter an address into the BBDB; implies ham (non-spam) sender"
      (dolist (from addresses)
        (when (stringp from)
          (let* ((parsed-address (gnus-extract-address-components from))
                 (name (or (nth 0 parsed-address) "Ham Sender"))
                 (remove-function (if remove
                                      'bbdb-delete-record-internal
                                    'ignore))
                 (net-address (nth 1 parsed-address))
                 (record (and net-address
                              (bbdb-search-simple nil net-address))))
            (when net-address
              (gnus-message 6 "%s address %s %s BBDB"
                            (if remove "Deleting" "Adding")
                            from
                            (if remove "from" "to"))
              (if record
                  (funcall remove-function record)
                (bbdb-create-internal name nil net-address nil nil
                                      "ham sender added by spam.el")))))))

    (defun spam-BBDB-register-routine (articles &optional unregister)
      (let (addresses)
        (dolist (article articles)
          (when (stringp (spam-fetch-field-from-fast article))
            (push (spam-fetch-field-from-fast article) addresses)))
        ;; now do the register/unregister action
        (spam-enter-ham-BBDB addresses unregister)))

    (defun spam-BBDB-unregister-routine (articles)
      (spam-BBDB-register-routine articles t))

    (defun spam-check-BBDB ()
      "Mail from people in the BBDB is classified as ham or non-spam"
      (let ((who (message-fetch-field "from")))
        (when who
          (setq who (nth 1 (gnus-extract-address-components who)))
          (if
              (if (fboundp 'bbdb-search)
                  (bbdb-search (bbdb-records) who) ;; v3
                (bbdb-search-simple nil who)) ;; v2
              t
            (if spam-use-BBDB-exclusive
                spam-split-group
              nil)))))))

;;}}}

;;{{{ ifile

;;; check the ifile backend; return nil if the mail was NOT classified
;;; as spam


(defun spam-get-ifile-database-parameter ()
  "Return the command-line parameter for ifile's database.
See `spam-ifile-database'."
  (if spam-ifile-database
      (format "--db-file=%s" spam-ifile-database)
    nil))

(defun spam-check-ifile ()
  "Check the ifile backend for the classification of this message."
  (let ((article-buffer-name (buffer-name))
        category return)
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name))
            (db-param (spam-get-ifile-database-parameter)))
        (with-current-buffer article-buffer-name
          (apply 'call-process-region
                 (point-min) (point-max) spam-ifile-program
                 nil temp-buffer-name nil "-c"
                 (if db-param `(,db-param "-q") `("-q"))))
        ;; check the return now (we're back in the temp buffer)
        (goto-char (point-min))
        (if (not (eobp))
            (setq category (buffer-substring (point) (point-at-eol))))
        (when (not (zerop (length category))) ; we need a category here
          (if spam-ifile-all-categories
              (setq return category)
            ;; else, if spam-ifile-all-categories is not set...
            (when (string-equal spam-ifile-spam-category category)
              (setq return spam-split-group)))))) ; note return is nil otherwise
    return))

(defun spam-ifile-register-with-ifile (articles category &optional unregister)
  "Register an article, given as a string, with a category.
Uses `gnus-newsgroup-name' if category is nil (for ham registration)."
  (let ((category (or category gnus-newsgroup-name))
        (add-or-delete-option (if unregister "-d" "-i"))
        (db (spam-get-ifile-database-parameter))
        parameters)
    (with-temp-buffer
      (dolist (article articles)
        (let ((article-string (spam-get-article-as-string article)))
          (when (stringp article-string)
            (insert article-string))))
      (apply 'call-process-region
             (point-min) (point-max) spam-ifile-program
             nil nil nil
             add-or-delete-option category
             (if db `(,db "-h") `("-h"))))))

(defun spam-ifile-register-spam-routine (articles &optional unregister)
  (spam-ifile-register-with-ifile articles spam-ifile-spam-category unregister))

(defun spam-ifile-unregister-spam-routine (articles)
  (spam-ifile-register-spam-routine articles t))

(defun spam-ifile-register-ham-routine (articles &optional unregister)
  (spam-ifile-register-with-ifile articles spam-ifile-ham-category unregister))

(defun spam-ifile-unregister-ham-routine (articles)
  (spam-ifile-register-ham-routine articles t))

;;}}}

;;{{{ spam-stat

(eval-when-compile
  (autoload 'spam-stat-buffer-change-to-non-spam "spam-stat")
  (autoload 'spam-stat-buffer-change-to-spam "spam-stat")
  (autoload 'spam-stat-buffer-is-non-spam "spam-stat")
  (autoload 'spam-stat-buffer-is-spam "spam-stat")
  (autoload 'spam-stat-load "spam-stat")
  (autoload 'spam-stat-save "spam-stat")
  (autoload 'spam-stat-split-fancy "spam-stat"))

(require 'spam-stat)

(defun spam-check-stat ()
  "Check the spam-stat backend for the classification of this message"
  (let ((spam-stat-split-fancy-spam-group spam-split-group) ; override
	(spam-stat-buffer (buffer-name)) ; stat the current buffer
	category return)
    (spam-stat-split-fancy)))

(defun spam-stat-register-spam-routine (articles &optional unregister)
  (dolist (article articles)
    (let ((article-string (spam-get-article-as-string article)))
      (with-temp-buffer
	(insert article-string)
	(if unregister
	    (spam-stat-buffer-change-to-non-spam)
	  (spam-stat-buffer-is-spam))))))

(defun spam-stat-unregister-spam-routine (articles)
  (spam-stat-register-spam-routine articles t))

(defun spam-stat-register-ham-routine (articles &optional unregister)
  (dolist (article articles)
    (let ((article-string (spam-get-article-as-string article)))
      (with-temp-buffer
	(insert article-string)
	(if unregister
	    (spam-stat-buffer-change-to-spam)
	  (spam-stat-buffer-is-non-spam))))))

(defun spam-stat-unregister-ham-routine (articles)
  (spam-stat-register-ham-routine articles t))

(defun spam-maybe-spam-stat-load ()
  (when spam-use-stat (spam-stat-load)))

(defun spam-maybe-spam-stat-save ()
  (when spam-use-stat (spam-stat-save)))

;;}}}

;;{{{ Blacklists and whitelists.

(defvar spam-whitelist-cache nil)
(defvar spam-blacklist-cache nil)

(defun spam-kill-whole-line ()
  (beginning-of-line)
  (let ((kill-whole-line t))
    (kill-line)))

;;; address can be a list, too
(defun spam-enter-whitelist (address &optional remove)
  "Enter ADDRESS (list or single) into the whitelist.
With a non-nil REMOVE, remove them."
  (interactive "sAddress: ")
  (spam-enter-list address spam-whitelist remove)
  (setq spam-whitelist-cache nil)
  (spam-clear-cache 'spam-use-whitelist))

;;; address can be a list, too
(defun spam-enter-blacklist (address &optional remove)
  "Enter ADDRESS (list or single) into the blacklist.
With a non-nil REMOVE, remove them."
  (interactive "sAddress: ")
  (spam-enter-list address spam-blacklist remove)
  (setq spam-blacklist-cache nil)
  (spam-clear-cache 'spam-use-whitelist))

(defun spam-enter-list (addresses file &optional remove)
  "Enter ADDRESSES into the given FILE.
Either the whitelist or the blacklist files can be used.
With a non-nil REMOVE, remove the ADDRESSES."
  (if (stringp addresses)
      (spam-enter-list (list addresses) file remove)
    ;; else, we have a list of addresses here
    (unless (file-exists-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (with-current-buffer
       (find-file-noselect file)
      (dolist (a addresses)
        (when (stringp a)
          (goto-char (point-min))
          (if (re-search-forward (regexp-quote a) nil t)
              ;; found the address
              (when remove
                (spam-kill-whole-line))
            ;; else, the address was not found
            (unless remove
              (goto-char (point-max))
              (unless (bobp)
                (insert "\n"))
              (insert a "\n")))))
      (save-buffer))))

(defun spam-filelist-build-cache (type)
  (let ((cache (if (eq type 'spam-use-blacklist)
                   spam-blacklist-cache
                 spam-whitelist-cache))
        parsed-cache)
    (unless (gethash type spam-caches)
      (while cache
        (let ((address (pop cache)))
          (unless (zerop (length address)) ; 0 for a nil address too
            (setq address (regexp-quote address))
            ;; fix regexp-quote's treatment of user-intended regexes
            (while (string-match "\\\\\\*" address)
              (setq address (replace-match ".*" t t address))))
          (push address parsed-cache)))
      (puthash type parsed-cache spam-caches))))

(defun spam-filelist-check-cache (type from)
  (when (stringp from)
    (spam-filelist-build-cache type)
    (let (found)
      (dolist (address (gethash type spam-caches))
        (when (and address (string-match address from))
          (setq found t)
          (return)))
      found)))

;;; returns t if the sender is in the whitelist, nil or
;;; spam-split-group otherwise
(defun spam-check-whitelist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (unless spam-whitelist-cache
    (setq spam-whitelist-cache (spam-parse-list spam-whitelist)))
  (if (spam-from-listed-p 'spam-use-whitelist)
      t
    (if spam-use-whitelist-exclusive
        spam-split-group
      nil)))

(defun spam-check-blacklist ()
  ;; FIXME!  Should it detect when file timestamps change?
  (unless spam-blacklist-cache
    (setq spam-blacklist-cache (spam-parse-list spam-blacklist)))
  (and (spam-from-listed-p 'spam-use-blacklist)
       spam-split-group))

(defun spam-parse-list (file)
  (when (file-readable-p file)
    (let (contents address)
      (with-temp-buffer
        (insert-file-contents file)
        (while (not (eobp))
          (setq address (buffer-substring (point) (point-at-eol)))
          (forward-line 1)
          ;; insert the e-mail address if detected, otherwise the raw data
          (unless (zerop (length address))
            (let ((pure-address
                   (nth 1 (gnus-extract-address-components address))))
              (push (or pure-address address) contents)))))
      (nreverse contents))))

(defun spam-from-listed-p (type)
  (let ((from (message-fetch-field "from"))
        found)
    (spam-filelist-check-cache type from)))

(defun spam-filelist-register-routine (articles blacklist &optional unregister)
  (let ((de-symbol (if blacklist 'spam-use-whitelist 'spam-use-blacklist))
        (declassification (if blacklist 'ham 'spam))
        (enter-function
         (if blacklist 'spam-enter-blacklist 'spam-enter-whitelist))
        (remove-function
         (if blacklist 'spam-enter-whitelist 'spam-enter-blacklist))
        from addresses unregister-list article-unregister-list)
    (dolist (article articles)
      (let ((from (spam-fetch-field-from-fast article))
            (id (spam-fetch-field-message-id-fast article))
            sender-ignored)
        (when (stringp from)
          (dolist (ignore-regex spam-blacklist-ignored-regexes)
            (when (and (not sender-ignored)
                       (stringp ignore-regex)
                       (string-match ignore-regex from))
              (setq sender-ignored t)))
          ;; remember the messages we need to unregister, unless remove is set
          (when (and
                 (null unregister)
                 (spam-log-unregistration-needed-p
                  id 'process declassification de-symbol))
            (push article article-unregister-list)
            (push from unregister-list))
          (unless sender-ignored
            (push from addresses)))))

    (if unregister
        (funcall enter-function addresses t) ; unregister all these addresses
      ;; else, register normally and unregister what we need to
      (funcall remove-function unregister-list t)
      (dolist (article article-unregister-list)
        (spam-log-undo-registration
         (spam-fetch-field-message-id-fast article)
         'process
         declassification
         de-symbol))
      (funcall enter-function addresses nil))))

(defun spam-blacklist-unregister-routine (articles)
  (spam-blacklist-register-routine articles t))

(defun spam-blacklist-register-routine (articles &optional unregister)
  (spam-filelist-register-routine articles t unregister))

(defun spam-whitelist-unregister-routine (articles)
  (spam-whitelist-register-routine articles t))

(defun spam-whitelist-register-routine (articles &optional unregister)
  (spam-filelist-register-routine articles nil unregister))

;;}}}

;;{{{ Spam-report glue (gmane and resend reporting)
(defun spam-report-gmane-register-routine (articles)
  (when articles
    (apply 'spam-report-gmane-spam articles)))

(defun spam-report-gmane-unregister-routine (articles)
  (when articles
    (apply 'spam-report-gmane-ham articles)))

(defun spam-report-resend-register-ham-routine (articles)
  (spam-report-resend-register-routine articles t))

(defun spam-report-resend-register-routine (articles &optional ham)
  (let* ((resend-to-gp
          (if ham
              (gnus-parameter-ham-resend-to gnus-newsgroup-name)
            (gnus-parameter-spam-resend-to gnus-newsgroup-name)))
         (spam-report-resend-to (or (car-safe resend-to-gp)
                                    spam-report-resend-to)))
    (spam-report-resend articles ham)))

;;}}}

;;{{{ Bogofilter
(defun spam-check-bogofilter-headers (&optional score)
  (let ((header (message-fetch-field spam-bogofilter-header)))
    (when header                        ; return nil when no header
      (if score                         ; scoring mode
          (if (string-match "spamicity=\\([0-9.]+\\)" header)
              (match-string 1 header)
            "0")
        ;; spam detection mode
        (when (string-match spam-bogofilter-bogosity-positive-spam-header
                            header)
          spam-split-group)))))

;; return something sensible if the score can't be determined
(defun spam-bogofilter-score (&optional recheck)
  "Get the Bogofilter spamicity score."
  (interactive "P")
  (save-window-excursion
    (gnus-summary-show-article t)
    (set-buffer gnus-article-buffer)
    (let ((score (or (unless recheck
                       (spam-check-bogofilter-headers t))
                     (spam-check-bogofilter t))))
      (gnus-summary-show-article)
      (message "Spamicity score %s" score)
      (or score "0"))))

(defun spam-verify-bogofilter ()
  "Verify the Bogofilter version is sufficient."
  (when (eq spam-bogofilter-valid 'unknown)
    (setq spam-bogofilter-valid
          (not (string-match "^bogofilter version 0\\.\\([0-9]\\|1[01]\\)\\."
                             (shell-command-to-string
                              (format "%s -V" spam-bogofilter-program))))))
  spam-bogofilter-valid)

(defun spam-check-bogofilter (&optional score)
  "Check the Bogofilter backend for the classification of this message."
  (if (spam-verify-bogofilter)
      (let ((article-buffer-name (buffer-name))
            (db spam-bogofilter-database-directory)
            return)
        (with-temp-buffer
          (let ((temp-buffer-name (buffer-name)))
            (with-current-buffer article-buffer-name
              (apply 'call-process-region
                     (point-min) (point-max)
                     spam-bogofilter-program
                     nil temp-buffer-name nil
                     (if db `("-d" ,db "-v") `("-v"))))
            (setq return (spam-check-bogofilter-headers score))))
        return)
    (gnus-error 5 "`spam.el' doesn't support obsolete bogofilter versions")))

(defun spam-bogofilter-register-with-bogofilter (articles
                                                 spam
                                                 &optional unregister)
  "Register an article, given as a string, as spam or non-spam."
  (if (spam-verify-bogofilter)
      (dolist (article articles)
        (let ((article-string (spam-get-article-as-string article))
              (db spam-bogofilter-database-directory)
              (switch (if unregister
                          (if spam
                              spam-bogofilter-spam-strong-switch
                            spam-bogofilter-ham-strong-switch)
                        (if spam
                            spam-bogofilter-spam-switch
                          spam-bogofilter-ham-switch))))
          (when (stringp article-string)
            (with-temp-buffer
              (insert article-string)

              (apply 'call-process-region
                     (point-min) (point-max)
                     spam-bogofilter-program
                     nil nil nil switch
                     (if db `("-d" ,db "-v") `("-v")))))))
    (gnus-error 5 "`spam.el' doesn't support obsolete bogofilter versions")))

(defun spam-bogofilter-register-spam-routine (articles &optional unregister)
  (spam-bogofilter-register-with-bogofilter articles t unregister))

(defun spam-bogofilter-unregister-spam-routine (articles)
  (spam-bogofilter-register-spam-routine articles t))

(defun spam-bogofilter-register-ham-routine (articles &optional unregister)
  (spam-bogofilter-register-with-bogofilter articles nil unregister))

(defun spam-bogofilter-unregister-ham-routine (articles)
  (spam-bogofilter-register-ham-routine articles t))


;;}}}

;;{{{ spamoracle
(defun spam-check-spamoracle ()
  "Run spamoracle on an article to determine whether it's spam."
  (let ((article-buffer-name (buffer-name)))
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
        (with-current-buffer article-buffer-name
          (let ((status
                 (apply 'call-process-region
                        (point-min) (point-max)
                        spam-spamoracle-binary
                        nil temp-buffer-name nil
                        (if spam-spamoracle-database
                            `("-f" ,spam-spamoracle-database "mark")
                          '("mark")))))
            (if (eq 0 status)
                (progn
                  (set-buffer temp-buffer-name)
                  (goto-char (point-min))
                  (when (re-search-forward "^X-Spam: yes;" nil t)
                    spam-split-group))
              (error "Error running spamoracle: %s" status))))))))

(defun spam-spamoracle-learn (articles article-is-spam-p &optional unregister)
  "Run spamoracle in training mode."
  (with-temp-buffer
    (let ((temp-buffer-name (buffer-name)))
      (save-excursion
        (goto-char (point-min))
        (dolist (article articles)
          (insert (spam-get-article-as-string article)))
        (let* ((arg (if (spam-xor unregister article-is-spam-p)
                        "-spam"
                      "-good"))
               (status
                (apply 'call-process-region
                       (point-min) (point-max)
                       spam-spamoracle-binary
                       nil temp-buffer-name nil
                       (if spam-spamoracle-database
                           `("-f" ,spam-spamoracle-database
                             "add" ,arg)
                         `("add" ,arg)))))
          (unless (eq 0 status)
            (error "Error running spamoracle: %s" status)))))))

(defun spam-spamoracle-learn-ham (articles &optional unregister)
  (spam-spamoracle-learn articles nil unregister))

(defun spam-spamoracle-unlearn-ham (articles &optional unregister)
  (spam-spamoracle-learn-ham articles t))

(defun spam-spamoracle-learn-spam (articles &optional unregister)
  (spam-spamoracle-learn articles t unregister))

(defun spam-spamoracle-unlearn-spam (articles &optional unregister)
  (spam-spamoracle-learn-spam articles t))

;;}}}

;;{{{ SpamAssassin
;;; based mostly on the bogofilter code
(defun spam-check-spamassassin-headers (&optional score)
  "Check the SpamAssassin headers for the classification of this message."
  (if score                             ; scoring mode
      (let ((header (message-fetch-field spam-spamassassin-spam-status-header)))
        (when header
          (if (string-match spam-spamassassin-score-regexp header)
              (match-string 1 header)
            "0")))
    ;; spam detection mode
    (let ((header (message-fetch-field spam-spamassassin-spam-flag-header)))
          (when header                  ; return nil when no header
            (when (string-match spam-spamassassin-positive-spam-flag-header
                                header)
              spam-split-group)))))

(defun spam-check-spamassassin (&optional score)
  "Check the SpamAssassin backend for the classification of this message."
  (let ((article-buffer-name (buffer-name)))
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
        (with-current-buffer article-buffer-name
          (apply 'call-process-region
                 (point-min) (point-max) spam-assassin-program
                 nil temp-buffer-name nil spam-spamassassin-arguments))
        ;; check the return now (we're back in the temp buffer)
        (goto-char (point-min))
        (spam-check-spamassassin-headers score)))))

;; return something sensible if the score can't be determined
(defun spam-spamassassin-score (&optional recheck)
  "Get the SpamAssassin score"
  (interactive "P")
  (save-window-excursion
    (gnus-summary-show-article t)
    (set-buffer gnus-article-buffer)
    (let ((score (or (unless recheck
                       (spam-check-spamassassin-headers t))
                     (spam-check-spamassassin t))))
      (gnus-summary-show-article)
      (message "SpamAssassin score %s" score)
      (or score "0"))))

(defun spam-spamassassin-register-with-sa-learn (articles spam
                                                 &optional unregister)
  "Register articles with spamassassin's sa-learn as spam or non-spam."
  (if articles
      (let ((action (if unregister spam-sa-learn-unregister-switch
                      (if spam spam-sa-learn-spam-switch
                        spam-sa-learn-ham-switch)))
            (summary-buffer-name (buffer-name)))
        (with-temp-buffer
          ;; group the articles into mbox format
          (dolist (article articles)
            (let (article-string)
              (with-current-buffer summary-buffer-name
                (setq article-string (spam-get-article-as-string article)))
              (when (stringp article-string)
                ;; mbox separator
                (insert (concat "From nobody " (current-time-string) "\n"))
                (insert article-string)
                (insert "\n"))))
          ;; call sa-learn on all messages at the same time
          (apply 'call-process-region
                 (point-min) (point-max)
                 spam-sa-learn-program
                 nil nil nil "--mbox"
                 (if spam-sa-learn-rebuild
                     (list action)
                   `("--no-rebuild" ,action)))))))

(defun spam-spamassassin-register-spam-routine (articles &optional unregister)
  (spam-spamassassin-register-with-sa-learn articles t unregister))

(defun spam-spamassassin-register-ham-routine (articles &optional unregister)
  (spam-spamassassin-register-with-sa-learn articles nil unregister))

(defun spam-spamassassin-unregister-spam-routine (articles)
  (spam-spamassassin-register-with-sa-learn articles t t))

(defun spam-spamassassin-unregister-ham-routine (articles)
  (spam-spamassassin-register-with-sa-learn articles nil t))

;;}}}

;;{{{ Bsfilter
;;; based mostly on the bogofilter code
(defun spam-check-bsfilter-headers (&optional score)
  (if score
      (or (nnmail-fetch-field spam-bsfilter-probability-header)
          "0")
    (let ((header (nnmail-fetch-field spam-bsfilter-header)))
      (when header ; return nil when no header
        (when (string-match "YES" header)
          spam-split-group)))))

;; return something sensible if the score can't be determined
(defun spam-bsfilter-score (&optional recheck)
  "Get the Bsfilter spamicity score."
  (interactive "P")
  (save-window-excursion
    (gnus-summary-show-article t)
    (set-buffer gnus-article-buffer)
    (let ((score (or (unless recheck
                       (spam-check-bsfilter-headers t))
                     (spam-check-bsfilter t))))
      (gnus-summary-show-article)
      (message "Spamicity score %s" score)
      (or score "0"))))

(defun spam-check-bsfilter (&optional score)
  "Check the Bsfilter backend for the classification of this message."
  (let ((article-buffer-name (buffer-name))
        (dir spam-bsfilter-database-directory)
        return)
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
        (with-current-buffer article-buffer-name
          (apply 'call-process-region
                 (point-min) (point-max)
                 spam-bsfilter-program
                 nil temp-buffer-name nil
                 "--pipe"
                 "--insert-flag"
                 "--insert-probability"
                 (when dir
                   (list "--homedir" dir))))
        (setq return (spam-check-bsfilter-headers score))))
    return))

(defun spam-bsfilter-register-with-bsfilter (articles
                                             spam
                                             &optional unregister)
  "Register an article, given as a string, as spam or non-spam."
  (dolist (article articles)
    (let ((article-string (spam-get-article-as-string article))
          (switch (if unregister
                      (if spam
                          spam-bsfilter-spam-strong-switch
                        spam-bsfilter-ham-strong-switch)
                    (if spam
                        spam-bsfilter-spam-switch
                      spam-bsfilter-ham-switch))))
      (when (stringp article-string)
        (with-temp-buffer
          (insert article-string)
          (apply 'call-process-region
                 (point-min) (point-max)
                 spam-bsfilter-program
                 nil nil nil switch
                 "--update"
                 (when spam-bsfilter-database-directory
                   (list "--homedir"
                         spam-bsfilter-database-directory))))))))

(defun spam-bsfilter-register-spam-routine (articles &optional unregister)
  (spam-bsfilter-register-with-bsfilter articles t unregister))

(defun spam-bsfilter-unregister-spam-routine (articles)
  (spam-bsfilter-register-spam-routine articles t))

(defun spam-bsfilter-register-ham-routine (articles &optional unregister)
  (spam-bsfilter-register-with-bsfilter articles nil unregister))

(defun spam-bsfilter-unregister-ham-routine (articles)
  (spam-bsfilter-register-ham-routine articles t))

;;}}}

;;{{{ CRM114 Mailfilter
(defun spam-check-crm114-headers (&optional score)
  (let ((header (message-fetch-field spam-crm114-header)))
    (when header                        ; return nil when no header
      (if score                         ; scoring mode
          (if (string-match "( pR: \\([0-9.-]+\\)" header)
              (match-string 1 header)
            "0")
        ;; spam detection mode
        (when (string-match spam-crm114-positive-spam-header
                            header)
          spam-split-group)))))

;; return something sensible if the score can't be determined
(defun spam-crm114-score ()
  "Get the CRM114 Mailfilter pR."
  (interactive)
  (save-window-excursion
    (gnus-summary-show-article t)
    (set-buffer gnus-article-buffer)
    (let ((score (or (spam-check-crm114-headers t)
                     (spam-check-crm114 t))))
      (gnus-summary-show-article)
      (message "pR: %s" score)
      (or score "0"))))

(defun spam-check-crm114 (&optional score)
  "Check the CRM114 Mailfilter backend for the classification of this message."
  (let ((article-buffer-name (buffer-name))
        (db spam-crm114-database-directory)
        return)
    (with-temp-buffer
      (let ((temp-buffer-name (buffer-name)))
        (with-current-buffer article-buffer-name
          (apply 'call-process-region
                 (point-min) (point-max)
                 spam-crm114-program
                 nil temp-buffer-name nil
                 (when db (list (concat "--fileprefix=" db)))))
        (setq return (spam-check-crm114-headers score))))
    return))

(defun spam-crm114-register-with-crm114 (articles
                                         spam
                                         &optional unregister)
  "Register an article, given as a string, as spam or non-spam."
  (dolist (article articles)
    (let ((article-string (spam-get-article-as-string article))
          (db spam-crm114-database-directory)
          (switch (if unregister
                      (if spam
                          spam-crm114-spam-strong-switch
                        spam-crm114-ham-strong-switch)
                    (if spam
                        spam-crm114-spam-switch
                      spam-crm114-ham-switch))))
      (when (stringp article-string)
        (with-temp-buffer
          (insert article-string)

          (apply 'call-process-region
                 (point-min) (point-max)
                 spam-crm114-program
                 nil nil nil
                 (when db (list switch (concat "--fileprefix=" db)))))))))

(defun spam-crm114-register-spam-routine (articles &optional unregister)
  (spam-crm114-register-with-crm114 articles t unregister))

(defun spam-crm114-unregister-spam-routine (articles)
  (spam-crm114-register-spam-routine articles t))

(defun spam-crm114-register-ham-routine (articles &optional unregister)
  (spam-crm114-register-with-crm114 articles nil unregister))

(defun spam-crm114-unregister-ham-routine (articles)
  (spam-crm114-register-ham-routine articles t))

;;}}}

;;}}}

;;{{{ Hooks

;;;###autoload
(defun spam-initialize (&rest symbols)
  "Install the spam.el hooks and do other initialization.
When SYMBOLS is given, set those variables to t.  This is so you
can call `spam-initialize' before you set spam-use-* variables on
explicitly, and matters only if you need the extra headers
installed through `spam-necessary-extra-headers'."
  (interactive)

  (dolist (var symbols)
    (set var t))

  (dolist (header (spam-necessary-extra-headers))
    (add-to-list 'nnmail-extra-headers header)
    (add-to-list 'gnus-extra-headers header))

  (setq spam-install-hooks t)
  ;; TODO: How do we redo this every time the `spam' face is customized?
  (push '((eq mark gnus-spam-mark) . spam)
        gnus-summary-highlight)
  ;; Add hooks for loading and saving the spam stats
  (add-hook 'gnus-save-newsrc-hook 'spam-maybe-spam-stat-save)
  (add-hook 'gnus-get-top-new-news-hook 'spam-maybe-spam-stat-load)
  (add-hook 'gnus-startup-hook 'spam-maybe-spam-stat-load)
  (add-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)
  (add-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)
  (add-hook 'gnus-get-new-news-hook 'spam-setup-widening)
  (add-hook 'gnus-summary-prepared-hook 'spam-find-spam))

(defun spam-unload-hook ()
  "Uninstall the spam.el hooks."
  (interactive)
  (remove-hook 'gnus-save-newsrc-hook 'spam-maybe-spam-stat-save)
  (remove-hook 'gnus-get-top-new-news-hook 'spam-maybe-spam-stat-load)
  (remove-hook 'gnus-startup-hook 'spam-maybe-spam-stat-load)
  (remove-hook 'gnus-summary-prepare-exit-hook 'spam-summary-prepare-exit)
  (remove-hook 'gnus-summary-prepare-hook 'spam-summary-prepare)
  (remove-hook 'gnus-get-new-news-hook 'spam-setup-widening)
  (remove-hook 'gnus-summary-prepare-hook 'spam-find-spam))

(add-hook 'spam-unload-hook 'spam-unload-hook)

(when spam-install-hooks
  (spam-initialize))
;;}}}

(provide 'spam)

;;; spam.el ends here
