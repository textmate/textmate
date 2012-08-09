;;; pgg-def.el --- functions/macros for defining PGG functions

;; Copyright (C) 1999, 2002-2012 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;; Created: 1999/11/02
;; Keywords: PGP, OpenPGP, GnuPG
;; Package: pgg
;; Obsolete-since: 24.1

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

;;; Code:

(defgroup pgg ()
  "Glue for the various PGP implementations."
  :group 'mime
  :version "22.1")

(defcustom pgg-default-scheme 'gpg
  "Default PGP scheme."
  :group 'pgg
  :type '(choice (const :tag "GnuPG" gpg)
		 (const :tag "PGP 5" pgp5)
		 (const :tag "PGP" pgp)))

(defcustom pgg-default-user-id (user-login-name)
  "User ID of your default identity."
  :group 'pgg
  :type 'string)

(defcustom pgg-default-keyserver-address "subkeys.pgp.net"
  "Host name of keyserver."
  :group 'pgg
  :type 'string)

(defcustom pgg-query-keyserver nil
  "Whether PGG queries keyservers for missing keys when verifying messages."
  :version "22.1"
  :group 'pgg
  :type 'boolean)

(defcustom pgg-encrypt-for-me t
  "If t, encrypt all outgoing messages with user's public key."
  :group 'pgg
  :type 'boolean)

(defcustom pgg-cache-passphrase t
  "If t, cache passphrase."
  :group 'pgg
  :type 'boolean)

(defcustom pgg-passphrase-cache-expiry 16
  "How many seconds the passphrase is cached.
Whether the passphrase is cached at all is controlled by
`pgg-cache-passphrase'."
  :group 'pgg
  :type 'integer)

(defcustom pgg-passphrase-coding-system nil
  "Coding system to encode passphrase."
  :group 'pgg
  :type 'coding-system)

(defvar pgg-messages-coding-system nil
  "Coding system used when reading from a PGP external process.")

(defvar pgg-status-buffer " *PGG status*")
(defvar pgg-errors-buffer " *PGG errors*")
(defvar pgg-output-buffer " *PGG output*")

(defvar pgg-echo-buffer "*PGG-echo*")

(defvar pgg-scheme nil
  "Current scheme of PGP implementation.")

(defvar pgg-text-mode nil
  "If t, inform the recipient that the input is text.")

(defmacro pgg-truncate-key-identifier (key)
  `(if (> (length ,key) 8) (substring ,key -8) ,key))

(provide 'pgg-def)

;;; pgg-def.el ends here
