;;; password-cache.el --- Read passwords, possibly using a password cache.

;; Copyright (C) 1999-2000, 2003-2012  Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>
;; Created: 2003-12-21
;; Keywords: password cache passphrase key

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

;; Greatly influenced by pgg.el written by Daiki Ueno, with timer
;; fixes for XEmacs by Katsumi Yamaoka.  In fact, this is mostly just
;; a rip-off.
;;
;; (password-read "Password? " "test")
;; ;; Minibuffer prompt for password.
;;  => "foo"
;;
;; (password-cache-add "test" "foo")
;;  => nil

;; (password-read "Password? " "test")
;; ;; No minibuffer prompt
;;  => "foo"
;;
;; (password-read "Password? " "test")
;; ;; No minibuffer prompt
;;  => "foo"
;;
;; ;; Wait `password-cache-expiry' seconds.
;;
;; (password-read "Password? " "test")
;; ;; Minibuffer prompt for password is back.
;;  => "foo"

;;; Code:

;; Options are autoloaded since they are used by eg mml-sec.el.

;;;###autoload
(defcustom password-cache t
  "Whether to cache passwords."
  :group 'password
  :type 'boolean)

;;;###autoload
(defcustom password-cache-expiry 16
  "How many seconds passwords are cached, or nil to disable expiring.
Whether passwords are cached at all is controlled by `password-cache'."
  :group 'password
  :type '(choice (const :tag "Never" nil)
		 (integer :tag "Seconds")))

(defvar password-data (make-vector 7 0))

(defun password-read-from-cache (key)
  "Obtain passphrase for KEY from time-limited passphrase cache.
Custom variables `password-cache' and `password-cache-expiry'
regulate cache behavior."
  (and password-cache
       key
       (symbol-value (intern-soft key password-data))))

;;;###autoload
(defun password-in-cache-p (key)
  "Check if KEY is in the cache."
  (and password-cache
       key
       (intern-soft key password-data)))

(defun password-read (prompt &optional key)
  "Read password, for use with KEY, from user, or from cache if wanted.
KEY indicate the purpose of the password, so the cache can
separate passwords.  The cache is not used if KEY is nil.  It is
typically a string.
The variable `password-cache' control whether the cache is used."
  (or (password-read-from-cache key)
      (read-passwd prompt)))

(defun password-read-and-add (prompt &optional key)
  "Read password, for use with KEY, from user, or from cache if wanted.
Then store the password in the cache.  Uses `password-read' and
`password-cache-add'.  Custom variables `password-cache' and
`password-cache-expiry' regulate cache behavior.

Warning: the password is cached without checking that it is
correct.  It is better to check the password before caching.  If
you must use this function, take care to check passwords and
remove incorrect ones from the cache."
  (let ((password (password-read prompt key)))
    (when (and password key)
      (password-cache-add key password))
    password))

(make-obsolete 'password-read-and-add 'password-read "23.1")

(defun password-cache-remove (key)
  "Remove password indexed by KEY from password cache.
This is typically run by a timer setup from `password-cache-add',
but can be invoked at any time to forcefully remove passwords
from the cache.  This may be useful when it has been detected
that a password is invalid, so that `password-read' query the
user again."
  (let ((sym (intern-soft key password-data)))
    (when sym
      (let ((password (symbol-value sym)))
        (when (stringp password)
          (if (fboundp 'clear-string)
              (clear-string password)
            (fillarray password ?_)))
        (unintern key password-data)))))

(defun password-cache-add (key password)
  "Add password to cache.
The password is removed by a timer after `password-cache-expiry' seconds."
  (when (and password-cache-expiry (null (intern-soft key password-data)))
    (run-at-time password-cache-expiry nil
		 #'password-cache-remove
		 key))
  (set (intern key password-data) password)
  nil)

(defun password-reset ()
  "Clear the password cache."
  (interactive)
  (fillarray password-data 0))

(provide 'password-cache)

;;; password-cache.el ends here
