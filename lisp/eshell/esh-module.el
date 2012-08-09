;;; esh-module.el --- Eshell modules

;; Copyright (C) 1999-2000, 2002-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: processes

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

(provide 'esh-module)

(require 'eshell)
(require 'esh-util)

(defgroup eshell-module nil
  "The `eshell-module' group is for Eshell extension modules, which
provide optional behavior which the user can enable or disable by
customizing the variable `eshell-modules-list'."
  :tag "Extension modules"
  :group 'eshell)

;; load the defgroup's for the standard extension modules, so that
;; documentation can be provided when the user customize's
;; `eshell-modules-list'.
(load "esh-groups" nil 'nomessage)

;;; User Variables:

(defcustom eshell-module-unload-hook
  '(eshell-unload-extension-modules)
  "A hook run when `eshell-module' is unloaded."
  :type 'hook
  :group 'eshell-module)

(defcustom eshell-modules-list
  '(eshell-alias
    eshell-banner
    eshell-basic
    eshell-cmpl
    eshell-dirs
    eshell-glob
    eshell-hist
    eshell-ls
    eshell-pred
    eshell-prompt
    eshell-script
    eshell-term
    eshell-unix)
  "A list of optional add-on modules to be loaded by Eshell.
Changes will only take effect in future Eshell buffers."
  :type (append
	 (list 'set ':tag "Supported modules")
	 (mapcar
	  (function
	   (lambda (modname)
	     (let ((modsym (intern modname)))
	       (list 'const
		     ':tag (format "%s -- %s" modname
				   (get modsym 'custom-tag))
		     ':link (caar (get modsym 'custom-links))
		     ':doc (concat "\n" (get modsym 'group-documentation)
				   "\n ")
		     modsym))))
	  (sort (mapcar 'symbol-name
			(eshell-subgroups 'eshell-module))
		'string-lessp))
	 '((repeat :inline t :tag "Other modules" symbol)))
  :group 'eshell-module)

;;; Code:

(defsubst eshell-using-module (module)
  "Return non-nil if a certain Eshell MODULE is in use.
The MODULE should be a symbol corresponding to that module's
customization group.  Example: `eshell-cmpl' for that module."
  (memq module eshell-modules-list))

(defun eshell-unload-extension-modules ()
  "Unload any memory resident extension modules."
  (dolist (module (eshell-subgroups 'eshell-module))
    (if (featurep module)
	(ignore-errors
	  (message "Unloading %s..." (symbol-name module))
	  (unload-feature module)
	  (message "Unloading %s...done" (symbol-name module))))))

;;; esh-module.el ends here
