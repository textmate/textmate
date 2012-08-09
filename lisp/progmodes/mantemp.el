;;; mantemp.el --- create manual template instantiations from g++ 2.7.2 output

;; Copyright (C) 1996, 2001-2012  Free Software Foundation, Inc.

;; Author: Tom Houlder <thoulder@icor.fr>
;; Created: 10 Dec 1996
;; Keywords: g++, templates

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

;; The following is a typical error message from g++ using STL (here
;; with split lines):
;;
;; AFile.o(.text+0x2d5): undefined reference to
;;   `vector<double>::begin(void)'
;; AFile.o(.text+0x2e7): undefined reference to
;;   `vector<double>::insert(double *, unsigned int, double const &)'
;; AnotherFile.o(.text+0x226b8): undefined reference to
;;   `operator!=(rb_tree<basic_string<char>, pair<basic_string<char>
;;   const, AClass *>, select1st<pair<basic_string<char> const, AClass
;;   *>, basic_string<char> >, less<basic_string<char> > >::iterator
;;   const &, rb_tree<basic_string<char>, pair<basic_string<char>
;;   const, AClass *>, select1st<pair<basic_string<char> const, AClass
;;   *>, basic_string<char> >, less<basic_string<char> > >::iterator
;;   const &)'
;;
;; The message means that in the object file AFile.o there is one
;; uninstantiated template class, vector<double>, and in AnotherFile.o
;; there is one uninstantiated template function, operator!=(...).  To
;; turn this output into manual template instantiations, copy from the
;; first name of an objective file (here this is AFile.o) to right
;; after the very last `'' of the output.  Put this in a buffer and
;; call `mantemp-make-mantemps-buffer' with the point in the buffer.
;; You can also use `mantemp-make-mantemps-region' directly on the
;; region if the output is already in Emacs.
;;
;; The resulting buffer yields (connect the three output lines above
;; if you want to try):
;;
;; template operator!=(rb_tree<basic_string<char>,
;;   pair<basic_string<char> const, AClass *>,
;;   select1st<pair<basic_string<char> const, AClass *>,
;;   basic_string<char> >, less<basic_string<char> > >::iterator const
;;   &, rb_tree<basic_string<char>, pair<basic_string<char> const,
;;   AClass *>, select1st<pair<basic_string<char> const, AClass *>,
;;   basic_string<char> >, less<basic_string<char> > >::iterator const
;;   &);
;; template class vector<double>;
;;
;; which can be included in your C++ program.  However, its probably
;; better to include the necessary header files in the buffer and
;; compile it as a stand alone implementation file.
;;
;; Sometimes, an uninstantiated template may cause a message like the
;; following
;;
;; main.cc:66: invalid use of undefined type
;;   `struct valarray<double,arrayminusopclass<double,c_array<double> > >'
;;
;; Follow the same procedure as above and the line is changed to
;;
;; template struct valarray<double,
;;   arrayminusopclass<double,c_array<double> > >;

;; g++ does not output the templates that are needed by the
;; uninstantiated templates.  Therefore you will often get new error
;; messages after the first instantiations have been included and you
;; must repeat the operation.

;;; Code:

(defun mantemp-remove-comments ()
  "Remove g++ comments surrounding each function and member function."
  (save-excursion
    (goto-char (point-min))
    (message "Removing comments")
    (while (re-search-forward "^[A-z\.()+0-9: ]*`\\|'.*$" nil t)
      (replace-match ""))))

(defun mantemp-remove-memfuncs ()
  "Remove member function extensions so that only class names remain."
  (save-excursion
    ;; Remove conversion operator extensions.
    (goto-char (point-min))
    (message "Removing member function extensions")
    (while (re-search-forward
	    "^[A-z :&*<>~=,0-9+]*>::operator " nil t nil)
      (progn
	(backward-char 11)
	(delete-region (point) (line-end-position))))
    ;; Remove other member function extensions.
    (goto-char (point-min))
    (message "Removing member function extensions")
    (while (re-search-forward "^[A-z :&*<>~=,0-9+]*>::" nil t nil)
      (progn
	(backward-char 2)
	(delete-region (point) (line-end-position))))))

(defun mantemp-sort-and-unique-lines ()
  "Eliminate all consecutive duplicate lines in the buffer."
  (save-excursion
    (message "Sorting")
    (sort-regexp-fields nil "^.*$" "\\&"
			(point-min)
			(point-max))
    (goto-char (point-min))
    (message "Removing consecutive duplicate lines")
    (while (re-search-forward "\\(^.+\\)\n\\1" nil t nil)
      (progn
	(forward-line -1)
	(beginning-of-line)
	(delete-region (point) (progn (forward-line 1) (point)))))))

(defun mantemp-insert-cxx-syntax ()
  "Insert C++ syntax around each template class and function.
Insert 'template class' for classes, 'template' for
functions and add the statement delimiter `;' at the end of
the lines."
  (save-excursion
    ;; Insert ';' at the end of each nonempty line.
    (goto-char (point-min))
    (message "Inserting `;' at the ends")
    (while (re-search-forward ".+$" nil t)
      (progn
	(insert ";")
	(if (not (equal (point) (point-max)))
	    (forward-char))))
    ;; We first insert 'template class' at each nonempty line and
    ;; subsequently remove 'class' for functions so we don't need to
    ;; both scan for classes and functions.
    (goto-char (point-min))
    (message "Inserting 'template class' for classes")
    (while (re-search-forward "^.+" nil t)
      (progn
	(beginning-of-line)
	(if (looking-at "struct[\\t ]+\\|class[\\t ]+")
	    (insert "template ")
	  (insert "template class "))))
    (goto-char (point-min))
    (message "Inserting 'template' for functions")
    (while (re-search-forward
	    "^template class [A-z :&*<>~=,0-9+!]*(" nil t nil)
      (progn
	(beginning-of-line)
	(forward-word 1)
	(delete-region (point) (progn (forward-word 1) (point)))))))

(defun mantemp-make-mantemps ()
  "Gathering interface to the functions modifying the buffer."
  (mantemp-remove-comments)
  (mantemp-remove-memfuncs)
  (mantemp-sort-and-unique-lines)
  (mantemp-insert-cxx-syntax))

(defun mantemp-make-mantemps-buffer ()
  "Make manual template instantiations from g++ error messages in the buffer.
Scan the output of g++ describing uninstantiated template
classes and functions and generate the corresponding C++
manual template instantiations.  The output is supposed to
have been placed in the current buffer and the current buffer
should otherwise be empty.

See the commentary in file mantemp.el for an example of use."
  (interactive)
  (mantemp-make-mantemps)
  (message "Done"))

(defun mantemp-make-mantemps-region ()
  "Make manual template instantiations from g++ error messages in the region.
This function does the same thing as `mantemp-make-mantemps-buffer',
but operates on the region."
  (interactive)
  (let ((cur-buf (current-buffer))
	(mantemp-buffer (generate-new-buffer "*mantemp*"))
	(str (buffer-substring (mark) (point))))
    ;; Copy the region to a temporary buffer, make the C++ code there
    ;; and copy the result back to the current buffer.
    (set-buffer mantemp-buffer)
    (insert str)
    (mantemp-make-mantemps)
    (setq str (buffer-string))
    (set-buffer cur-buf)
    (insert str)
    (kill-buffer mantemp-buffer))
  (message "Done"))

(provide 'mantemp)

;;; mantemp.el ends here
