;;; gnus-setup.el --- Initialization & Setup for Gnus 5

;; Copyright (C) 1995-1996, 2000-2012 Free Software Foundation, Inc.

;; Author: Steven L. Baur <steve@miranova.com>
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
;; My head is starting to spin with all the different mail/news packages.
;; Stop The Madness!

;; Given that Emacs Lisp byte codes may be diverging, it is probably best
;; not to byte compile this, and just arrange to have the .el loaded out
;; of .emacs.

;;; Code:

(eval-when-compile (require 'cl))

(defvar gnus-use-installed-gnus t
  "*If non-nil use installed version of Gnus.")

(defvar gnus-use-installed-mailcrypt (featurep 'xemacs)
  "*If non-nil use installed version of mailcrypt.")

(defvar gnus-emacs-lisp-directory (if (featurep 'xemacs)
				      "/usr/local/lib/xemacs/"
				    "/usr/local/share/emacs/")
  "Directory where Emacs site lisp is located.")

(defvar gnus-gnus-lisp-directory (concat gnus-emacs-lisp-directory
					 "gnus/lisp/")
  "Directory where Gnus Emacs lisp is found.")

(defvar gnus-mailcrypt-lisp-directory (concat gnus-emacs-lisp-directory
					      "site-lisp/mailcrypt/")
  "Directory where Mailcrypt Emacs Lisp is found.")

(defvar gnus-bbdb-lisp-directory (concat gnus-emacs-lisp-directory
					 "site-lisp/bbdb/")
  "Directory where Big Brother Database is found.")

(defvar gnus-use-mhe nil
  "Set this if you want to use MH-E for mail reading.")
(defvar gnus-use-rmail nil
  "Set this if you want to use RMAIL for mail reading.")
(defvar gnus-use-sendmail t
  "Set this if you want to use SENDMAIL for mail reading.")
(defvar gnus-use-vm nil
  "Set this if you want to use the VM package for mail reading.")
(defvar gnus-use-sc nil
  "Set this if you want to use Supercite.")
(defvar gnus-use-mailcrypt t
  "Set this if you want to use Mailcrypt for dealing with PGP messages.")
(defvar gnus-use-bbdb nil
  "Set this if you want to use the Big Brother DataBase.")

(when (and (not gnus-use-installed-gnus)
	   (null (member gnus-gnus-lisp-directory load-path)))
  (push gnus-gnus-lisp-directory load-path))

;;; We can't do this until we know where Gnus is.
(require 'message)

;;; Mailcrypt by
;;; Jin Choi <jin@atype.com>
;;; Patrick LoPresti <patl@lcs.mit.edu>

(when gnus-use-mailcrypt
  (when (and (not gnus-use-installed-mailcrypt)
	     (null (member gnus-mailcrypt-lisp-directory load-path)))
    (setq load-path (cons gnus-mailcrypt-lisp-directory load-path)))
  (autoload 'mc-install-write-mode "mailcrypt" nil t)
  (autoload 'mc-install-read-mode "mailcrypt" nil t)
;;;   (add-hook 'message-mode-hook 'mc-install-write-mode)
;;;   (add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
  (when gnus-use-mhe
    (add-hook 'mh-folder-mode-hook 'mc-install-read-mode)
    (add-hook 'mh-letter-mode-hook 'mc-install-write-mode)))

;;; BBDB by
;;; Jamie Zawinski <jwz@lucid.com>

(when gnus-use-bbdb
  ;; bbdb will never be installed with emacs.
  (when (null (member gnus-bbdb-lisp-directory load-path))
    (setq load-path (cons gnus-bbdb-lisp-directory load-path)))
  (autoload 'bbdb "bbdb-com"
    "Insidious Big Brother Database" t)
  (autoload 'bbdb-name "bbdb-com"
    "Insidious Big Brother Database" t)
  (autoload 'bbdb-company "bbdb-com"
    "Insidious Big Brother Database" t)
  (autoload 'bbdb-net "bbdb-com"
    "Insidious Big Brother Database" t)
  (autoload 'bbdb-notes "bbdb-com"
    "Insidious Big Brother Database" t)

  (when gnus-use-vm
    (autoload 'bbdb-insinuate-vm "bbdb-vm"
      "Hook BBDB into VM" t))

  (when gnus-use-rmail
    (autoload 'bbdb-insinuate-rmail "bbdb-rmail"
      "Hook BBDB into RMAIL" t)
    (add-hook 'rmail-mode-hook 'bbdb-insinuate-rmail))

  (when gnus-use-mhe
    (autoload 'bbdb-insinuate-mh "bbdb-mh"
      "Hook BBDB into MH-E" t)
    (add-hook 'mh-folder-mode-hook 'bbdb-insinuate-mh))

  (autoload 'bbdb-insinuate-gnus "bbdb-gnus"
    "Hook BBDB into Gnus" t)
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

  (when gnus-use-sendmail
    (autoload 'bbdb-insinuate-sendmail "bbdb"
      "Insidious Big Brother Database" t)
    (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)
    (add-hook 'message-setup-hook 'bbdb-insinuate-sendmail)))

(when gnus-use-sc
  (add-hook 'mail-citation-hook 'sc-cite-original)
  (setq message-cite-function 'sc-cite-original))

;;;### (autoloads (gnus gnus-slave gnus-no-server) "gnus" "lisp/gnus.el" (12473 2137))
;;; Generated autoloads from lisp/gnus.el

;; Don't redo this if autoloads already exist
(unless (fboundp 'gnus)
  (autoload 'gnus-slave-no-server "gnus" "\
Read network news as a slave without connecting to local server." t nil)

  (autoload 'gnus-no-server "gnus" "\
Read network news.
If ARG is a positive number, Gnus will use that as the
startup level.  If ARG is nil, Gnus will be started at level 2.
If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use.
As opposed to `gnus', this command will not connect to the local server." t nil)

  (autoload 'gnus-slave "gnus" "\
Read news as a slave." t nil)

  (autoload 'gnus "gnus" "\
Read network news.
If ARG is non-nil and a positive number, Gnus will use that as the
startup level.  If ARG is non-nil and not a positive number, Gnus will
prompt the user for the name of an NNTP server to use." t nil)

;;;***

;;; These have moved out of gnus.el into other files.
;;; FIX FIX FIX: should other things be in gnus-setup? or these not in it?
  (autoload 'gnus-update-format "gnus-spec" "\
Update the format specification near point." t nil)

  (autoload 'gnus-fetch-group "gnus-group" "\
Start Gnus if necessary and enter GROUP.
Returns whether the fetching was successful or not." t nil)

  (defalias 'gnus-batch-kill 'gnus-batch-score)

  (autoload 'gnus-batch-score "gnus-kill" "\
Run batched scoring.
Usage: emacs -batch -l gnus -f gnus-batch-score <newsgroups> ...
Newsgroups is a list of strings in Bnews format.  If you want to score
the comp hierarchy, you'd say \"comp.all\".  If you would not like to
score the alt hierarchy, you'd say \"!alt.all\"." t nil))

(provide 'gnus-setup)

(run-hooks 'gnus-setup-load-hook)

;;; gnus-setup.el ends here
