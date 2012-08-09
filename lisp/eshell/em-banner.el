;;; em-banner.el --- sample module that displays a login banner

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; There is nothing to be done or configured in order to use this
;; module, other than to select it by customizing the variable
;; `eshell-modules-list'.  It will then display a version information
;; message whenever Eshell is loaded.
;;
;; This code is only an example of a how to write a well-formed
;; extension module for Eshell.  The better way to display login text
;; is to use the `eshell-script' module, and to echo the desired
;; strings from the user's `eshell-login-script' file.
;;
;; There is one configuration variable, which demonstrates how to
;; properly define a customization variable in an extension module.
;; In this case, it allows the user to change the string which
;; displays at login time.

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'esh-mode)
  (require 'eshell))

(require 'esh-util)

;;;###autoload
(eshell-defgroup eshell-banner nil
  "This sample module displays a welcome banner at login.
It exists so that others wishing to create their own Eshell extension
modules may have a simple template to begin with."
  :tag "Login banner"
  ;; :link '(info-link "(eshell)Login banner")
  :group 'eshell-module)

;;; User Variables:

(defcustom eshell-banner-message "Welcome to the Emacs shell\n\n"
  "The banner message to be displayed when Eshell is loaded.
This can be any sexp, and should end with at least two newlines."
  :type 'sexp
  :group 'eshell-banner)

(put 'eshell-banner-message 'risky-local-variable t)

(defcustom eshell-banner-load-hook nil
  "A list of functions to run when `eshell-banner' is loaded."
  :version "24.1"                       ; removed eshell-banner-initialize
  :type 'hook
  :group 'eshell-banner)

(defun eshell-banner-initialize ()
  "Output a welcome banner on initialization."
  ;; it's important to use `eshell-interactive-print' rather than
  ;; `insert', because `insert' doesn't know how to interact with the
  ;; I/O code used by Eshell
  (unless eshell-non-interactive-p
    (assert eshell-mode)
    (assert eshell-banner-message)
    (let ((msg (eval eshell-banner-message)))
      (assert msg)
      (eshell-interactive-print msg))))

(provide 'em-banner)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-banner.el ends here
