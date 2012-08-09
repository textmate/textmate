;;; ob-sass.el --- org-babel functions for the sass css generation language

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

;; For more information on sass see http://sass-lang.com/
;;
;; This accepts a 'file' header argument which is the target of the
;; compiled sass.  The default output type for sass evaluation is
;; either file (if a 'file' header argument was given) or scalar if no
;; such header argument was supplied.
;;
;; A 'cmdline' header argument can be supplied to pass arguments to
;; the sass command line.

;;; Requirements:

;; - sass-mode :: http://github.com/nex3/haml/blob/master/extra/sass-mode.el

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:sass '())

(defun org-babel-execute:sass (body params)
  "Execute a block of Sass code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
         (file (cdr (assoc :file params)))
         (out-file (or file (org-babel-temp-file "sass-out-")))
         (cmdline (cdr (assoc :cmdline params)))
         (in-file (org-babel-temp-file "sass-in-"))
         (cmd (concat "sass " (or cmdline "")
		      " " (org-babel-process-file-name in-file)
		      " " (org-babel-process-file-name out-file))))
    (with-temp-file in-file
      (insert (org-babel-expand-body:generic body params)))
    (org-babel-eval cmd "")
    (if file
	nil ;; signal that output has already been written to file
      (with-temp-buffer (insert-file-contents out-file) (buffer-string)))))

(defun org-babel-prep-session:sass (session params)
  "Raise an error because sass does not support sessions."
  (error "Sass does not support sessions"))

(provide 'ob-sass)



;;; ob-sass.el ends here
