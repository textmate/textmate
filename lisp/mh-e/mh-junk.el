;;; mh-junk.el --- MH-E interface to anti-spam measures

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>,
;;         Bill Wohler <wohler@newt.com>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords:  mail, spam

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

;; Spam handling in MH-E.

;;; Change Log:

;;; Code:

(require 'mh-e)
(require 'mh-scan)
(mh-require-cl)

;;;###mh-autoload
(defun mh-junk-blacklist (range)
  "Blacklist RANGE as spam.

This command trains the spam program in use (see the option
`mh-junk-program') with the content of RANGE and then handles the
message(s) as specified by the option `mh-junk-disposition'.

Check the documentation of `mh-interactive-range' to see how RANGE is
read in interactive use.

For more information about using your particular spam fighting
program, see:

  - `mh-spamassassin-blacklist'
  - `mh-bogofilter-blacklist'
  - `mh-spamprobe-blacklist'"
  (interactive (list (mh-interactive-range "Blacklist")))
  (let ((blacklist-func (nth 1 (assoc mh-junk-choice mh-junk-function-alist))))
    (unless blacklist-func
      (error "Customize `mh-junk-program' appropriately"))
    (let ((dest (cond ((null mh-junk-disposition) nil)
                      ((equal mh-junk-disposition "") "+")
                      ((eq (aref mh-junk-disposition 0) ?+)
                       mh-junk-disposition)
                      ((eq (aref mh-junk-disposition 0) ?@)
                       (concat mh-current-folder "/"
                               (substring mh-junk-disposition 1)))
                      (t (concat "+" mh-junk-disposition)))))
      (mh-iterate-on-range msg range
        (message "Blacklisting message %d..." msg)
        (funcall (symbol-function blacklist-func) msg)
        (message "Blacklisting message %d...done" msg)
        (if (not (memq msg mh-seen-list))
            (setq mh-seen-list (cons msg mh-seen-list)))
        (if dest
            (mh-refile-a-msg nil (intern dest))
          (mh-delete-a-msg nil)))
      (mh-next-msg))))

;;;###mh-autoload
(defun mh-junk-whitelist (range)
  "Whitelist RANGE as ham.

This command reclassifies the RANGE as ham if it were incorrectly
classified as spam (see the option `mh-junk-program'). It then
refiles the message into the \"+inbox\" folder.

Check the documentation of `mh-interactive-range' to see how
RANGE is read in interactive use."
  (interactive (list (mh-interactive-range "Whitelist")))
  (let ((whitelist-func (nth 2 (assoc mh-junk-choice mh-junk-function-alist))))
    (unless whitelist-func
      (error "Customize `mh-junk-program' appropriately"))
    (mh-iterate-on-range msg range
      (message "Whitelisting message %d..." msg)
      (funcall (symbol-function whitelist-func) msg)
      (message "Whitelisting message %d...done" msg)
      (mh-refile-a-msg nil (intern mh-inbox)))
    (mh-next-msg)))



;; Spamassassin Interface

(defvar mh-spamassassin-executable (executable-find "spamassassin"))
(defvar mh-sa-learn-executable (executable-find "sa-learn"))

;;;###mh-autoload
(defun mh-spamassassin-blacklist (msg)
  "Blacklist MSG with SpamAssassin.

SpamAssassin is one of the more popular spam filtering programs.
Get it from your local distribution or from the SpamAssassin web
site at URL `http://spamassassin.org/'.

To use SpamAssassin, add the following recipes to
\".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with SpamAssassin.
    :0fw
    | spamc

    # Anything with a spam level of 10 or more is junked immediately.
    :0:
    * ^X-Spam-Level: ..........
    /dev/null

    :0:
    * ^X-Spam-Status: Yes
    spam/.

If you don't use \"spamc\", use \"spamassassin -P -a\".

Note that one of the recipes above throws away messages with a
score greater than or equal to 10. Here's how you can determine a
value that works best for you.

First, run \"spamassassin -t\" on every mail message in your
archive and use Gnumeric to verify that the average plus the
standard deviation of good mail is under 5, the SpamAssassin
default for \"spam\".

Using Gnumeric, sort the messages by score and view the messages
with the highest score. Determine the score which encompasses all
of your interesting messages and add a couple of points to be
conservative. Add that many dots to the \"X-Spam-Level:\" header
field above to send messages with that score down the drain.

In the example above, messages with a score of 5-9 are set aside
in the \"+spam\" folder for later review. The major weakness of
rules-based filters is a plethora of false positives so it is
worthwhile to check.

If SpamAssassin classifies a message incorrectly, or is unsure,
you can use the MH-E commands \\[mh-junk-blacklist] and
\\[mh-junk-whitelist].

The command \\[mh-junk-blacklist] adds a \"blacklist_from\" entry
to \"~/spamassassin/user_prefs\", deletes the message, and sends
the message to the Razor, so that others might not see this spam.
If the \"sa-learn\" command is available, the message is also
recategorized as spam.

The command \\[mh-junk-whitelist] adds a \"whitelist_from\" rule
to the \"~/.spamassassin/user_prefs\" file. If the \"sa-learn\"
command is available, the message is also recategorized as ham.

Over time, you'll observe that the same host or domain occurs
repeatedly in the \"blacklist_from\" entries, so you might think
that you could avoid future spam by blacklisting all mail from a
particular domain. The utility function
`mh-spamassassin-identify-spammers' helps you do precisely that.
This function displays a frequency count of the hosts and domains
in the \"blacklist_from\" entries from the last blank line in
\"~/.spamassassin/user_prefs\" to the end of the file. This
information can be used so that you can replace multiple
\"blacklist_from\" entries with a single wildcard entry such as:

    blacklist_from *@*amazingoffersdirect2u.com

In versions of SpamAssassin (2.50 and on) that support a Bayesian
classifier, \\[mh-junk-blacklist] uses the program \"sa-learn\"
to recategorize the message as spam. Neither MH-E, nor
SpamAssassin, rebuilds the database after adding words, so you
will need to run \"sa-learn --rebuild\" periodically. This can be
done by adding the following to your crontab:

    0 * * * *   sa-learn --rebuild > /dev/null 2>&1"
  (unless mh-spamassassin-executable
    (error "Unable to find the spamassassin executable"))
  (let ((current-folder mh-current-folder)
        (msg-file (mh-msg-filename msg mh-current-folder))
        (sender))
    (message "Reporting message %d..." msg)
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-spamassassin-executable msg-file mh-junk-background nil
                    ;;"--report" "--remove-from-whitelist"
                    "-r" "-R")          ; spamassassin V2.20
    (when mh-sa-learn-executable
      (message "Recategorizing message %d as spam..." msg)
      (mh-truncate-log-buffer)
      (call-process mh-sa-learn-executable msg-file mh-junk-background nil
                    "--single" "--spam" "--local" "--no-rebuild")))
    (message "Blacklisting sender of message %d..." msg)
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (call-process (expand-file-name mh-scan-prog mh-progs)
                    nil t nil
                    (format "%d" msg) current-folder
                    "-format" "%<(mymbox{from})%|%(addr{from})%>")
      (goto-char (point-min))
      (if (search-forward-regexp "^\\(.+\\)$" nil t)
          (progn
            (setq sender (match-string 0))
            (mh-spamassassin-add-rule "blacklist_from" sender)
            (message "Blacklisting sender of message %d...done" msg))
        (message "Blacklisting sender of message %d...not done (from my address)" msg)))))

;;;###mh-autoload
(defun mh-spamassassin-whitelist (msg)
  "Whitelist MSG with SpamAssassin.

The \\[mh-junk-whitelist] command adds a \"whitelist_from\" rule to
the \"~/.spamassassin/user_prefs\" file. If the \"sa-learn\" command
is available, the message is also recategorized as ham.

See `mh-spamassassin-blacklist' for more information."
  (unless mh-spamassassin-executable
    (error "Unable to find the spamassassin executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder))
        (show-buffer (get-buffer mh-show-buffer))
        from)
    (with-current-buffer (get-buffer-create mh-temp-buffer)
      (erase-buffer)
      (message "Removing spamassassin markup from message %d..." msg)
      (call-process mh-spamassassin-executable msg-file t nil
                    ;; "--remove-markup"
                    "-d")               ; spamassassin V2.20
      (if show-buffer
          (kill-buffer show-buffer))
      (write-file msg-file)
      (when mh-sa-learn-executable
        (message "Recategorizing message %d as ham..." msg)
        (mh-truncate-log-buffer)
        ;; Put call-process output in log buffer if we are saving it
        ;; (this happens if mh-junk-background is t).
        (with-current-buffer mh-log-buffer
          (call-process mh-sa-learn-executable msg-file mh-junk-background nil
                        "--single" "--ham" "--local" "--no-rebuild")))
      (message "Whitelisting sender of message %d..." msg)
      (setq from
            (car (mh-funcall-if-exists
                  ietf-drums-parse-address (mh-get-header-field "From:"))))
      (kill-buffer nil)
      (unless (or (null from) (equal from ""))
        (mh-spamassassin-add-rule "whitelist_from" from))
      (message "Whitelisting sender of message %d...done" msg))))

(defun mh-spamassassin-add-rule (rule body)
  "Add a new rule to \"~/.spamassassin/user_prefs\".
The name of the rule is RULE and its body is BODY."
  (save-window-excursion
    (let* ((line (format "%s\t%s\n" rule body))
           (case-fold-search t)
           (file (expand-file-name "~/.spamassassin/user_prefs"))
           (buffer-exists (find-buffer-visiting file)))
      (find-file file)
      (if (not (search-forward (format "\n%s" line) nil t))
          (progn
            (goto-char (point-max))
            (insert (if (bolp) "" "\n") line)
            (save-buffer)))
      (if (not buffer-exists)
          (kill-buffer nil)))))

;;;###mh-autoload
(defun mh-spamassassin-identify-spammers ()
  "Identify spammers who are repeat offenders.

This function displays a frequency count of the hosts and domains
in the \"blacklist_from\" entries from the last blank line in
\"~/.spamassassin/user_prefs\" to the end of the file. This
information can be used so that you can replace multiple
\"blacklist_from\" entries with a single wildcard entry such as:

    blacklist_from *@*amazingoffersdirect2u.com"
  (interactive)
  (let* ((file (expand-file-name "~/.spamassassin/user_prefs"))
         (domains (make-hash-table :test 'equal)))
    (find-file file)
    ;; Only consider entries between last blank line and end of file.
    (goto-char (1- (point-max)))
    (search-backward-regexp "^$")
    ;; Perform frequency count.
    (save-excursion
      (while (search-forward-regexp "^blacklist_from\\s-*\\(.*\\)@\\(.*\\)$"
                                    nil t)
        (let ((host (match-string 2))
              value)
          ;; Remove top-level-domain from hostname.
          (setq host (cdr (reverse (split-string host "\\."))))
          ;; Add counts for each host and domain part.
          (while host
            (setq value (gethash (car host) domains))
            (setf (gethash (car host) domains) (1+ (if (not value) 0 value)))
            (setq host (cdr host))))))

    ;; Output
    (delete-other-windows)
    (pop-to-buffer (get-buffer-create "*MH-E Spammer Frequencies*"))
    (erase-buffer)
    (maphash (lambda (key value) ""
               (if (> value 2)
                   (insert (format "%s %s\n" key value))))
             domains)
    (sort-numeric-fields 2 (point-min) (point-max))
    (reverse-region (point-min) (point-max))
    (goto-char (point-min))))



;; Bogofilter Interface

(defvar mh-bogofilter-executable (executable-find "bogofilter"))

;;;###mh-autoload
(defun mh-bogofilter-blacklist (msg)
  "Blacklist MSG with bogofilter.

Bogofilter is a Bayesian spam filtering program. Get it from your
local distribution or from the bogofilter web site at URL
`http://bogofilter.sourceforge.net/'.

Bogofilter is taught by running:

    bogofilter -n < good-message

on every good message, and

    bogofilter -s < spam-message

on every spam message. This is called a full training; three other
training methods are described in the FAQ that is distributed with
bogofilter. Note that most Bayesian filters need 1000 to 5000 of each
type of message to start doing a good job.

To use bogofilter, add the following recipes to \".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with bogofilter.
    :0fw
    | bogofilter -3 -e -p

    :0:
    * ^X-Bogosity: Yes, tests=bogofilter
    spam/.

    :0:
    * ^X-Bogosity: Unsure, tests=bogofilter
    spam/unsure/.

If bogofilter classifies a message incorrectly, or is unsure, you can
use the MH-E commands \\[mh-junk-blacklist] and \\[mh-junk-whitelist]
to update bogofilter's training.

The \"Bogofilter FAQ\" suggests that you run the following
occasionally to shrink the database:

    bogoutil -d wordlist.db | bogoutil -l wordlist.db.new
    mv wordlist.db wordlist.db.prv
    mv wordlist.db.new wordlist.db

The \"Bogofilter tuning HOWTO\" describes how you can fine-tune Bogofilter."
  (unless mh-bogofilter-executable
    (error "Unable to find the bogofilter executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-bogofilter-executable msg-file mh-junk-background
                    nil "-s"))))

;;;###mh-autoload
(defun mh-bogofilter-whitelist (msg)
  "Whitelist MSG with bogofilter.

See `mh-bogofilter-blacklist' for more information."
  (unless mh-bogofilter-executable
    (error "Unable to find the bogofilter executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-bogofilter-executable msg-file mh-junk-background
                    nil "-n"))))



;; Spamprobe Interface

(defvar mh-spamprobe-executable (executable-find "spamprobe"))

;;;###mh-autoload
(defun mh-spamprobe-blacklist (msg)
  "Blacklist MSG with SpamProbe.

SpamProbe is a Bayesian spam filtering program. Get it from your
local distribution or from the SpamProbe web site at URL
`http://spamprobe.sourceforge.net'.

To use SpamProbe, add the following recipes to \".procmailrc\":

    PATH=$PATH:/usr/bin/mh
    MAILDIR=$HOME/`mhparam Path`

    # Fight spam with SpamProbe.
    :0
    SCORE=| spamprobe receive

    :0 wf
    | formail -I \"X-SpamProbe: $SCORE\"

    :0:
    *^X-SpamProbe: SPAM
    spam/.

If SpamProbe classifies a message incorrectly, you can use the
MH-E commands \\[mh-junk-blacklist] and \\[mh-junk-whitelist] to
update SpamProbe's training."
  (unless mh-spamprobe-executable
    (error "Unable to find the spamprobe executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-spamprobe-executable msg-file mh-junk-background
                    nil "spam"))))

;;;###mh-autoload
(defun mh-spamprobe-whitelist (msg)
  "Whitelist MSG with SpamProbe.

See `mh-spamprobe-blacklist' for more information."
  (unless mh-spamprobe-executable
    (error "Unable to find the spamprobe executable"))
  (let ((msg-file (mh-msg-filename msg mh-current-folder)))
    (mh-truncate-log-buffer)
    ;; Put call-process output in log buffer if we are saving it
    ;; (this happens if mh-junk-background is t).
    (with-current-buffer mh-log-buffer
      (call-process mh-spamprobe-executable msg-file mh-junk-background
                    nil "good"))))

(provide 'mh-junk)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-junk.el ends here
