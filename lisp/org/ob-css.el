;;; ob-css.el --- org-babel functions for css evaluation

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

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

;; Since CSS can't be executed, this file exists solely for tangling
;; CSS from org-mode files.

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:css '())

(defun org-babel-execute:css (body params)
  "Execute a block of CSS code.
This function is called by `org-babel-execute-src-block'."
 body)

(defun org-babel-prep-session:css (session params)
  "Return an error if the :session header argument is set.
CSS does not support sessions."
  (error "CSS sessions are nonsensical"))

(provide 'ob-css)



;;; ob-css.el ends here
