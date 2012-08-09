;;; srecode/cpp.el --- C++ specific handlers for Semantic Recoder

;; Copyright (C) 2007, 2009-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;         Jan Moringen <scymtym@users.sourceforge.net>

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
;;
;; Supply some C++ specific dictionary fillers and helpers

;;; Code:

(require 'srecode)
(require 'srecode/dictionary)
(require 'srecode/semantic)
(require 'semantic/tag)

;;; Customization
;;

(defgroup srecode-cpp nil
  "C++-specific Semantic Recoder settings."
  :group 'srecode)

(defcustom srecode-cpp-namespaces
  '("std" "boost")
  "List expansion candidates for the :using-namespaces argument.
A dictionary entry of the named PREFIX_NAMESPACE with the value
NAMESPACE:: is created for each namespace unless the current
buffer contains a using NAMESPACE; statement "
  :group 'srecode-cpp
  :type  '(repeat string))

;;; :cpp ARGUMENT HANDLING
;;
;; When a :cpp argument is required, fill the dictionary with
;; information about the current C++ file.
;;
;; Error if not in a C++ mode.

;;;###autoload
(defun srecode-semantic-handle-:cpp (dict)
  "Add macros into the dictionary DICT based on the current c++ file.
Adds the following:
FILENAME_SYMBOL - filename converted into a C compat symbol.
HEADER - Shown section if in a header file."
  ;; A symbol representing
  (let ((fsym (file-name-nondirectory (buffer-file-name)))
	(case-fold-search t))

    ;; Are we in a header file?
    (if (string-match "\\.\\(h\\|hh\\|hpp\\|h\\+\\+\\)$" fsym)
	(srecode-dictionary-show-section dict "HEADER")
      (srecode-dictionary-show-section dict "NOTHEADER"))

    ;; Strip out bad characters
    (while (string-match "\\.\\| " fsym)
      (setq fsym (replace-match "_" t t fsym)))
    (srecode-dictionary-set-value dict "FILENAME_SYMBOL" fsym)
    )
  )

(defun srecode-semantic-handle-:using-namespaces (dict)
  "Add macros into the dictionary DICT based on used namespaces.
Adds the following:
PREFIX_NAMESPACE - for each NAMESPACE in `srecode-cpp-namespaces'."
  (let ((tags (semantic-find-tags-by-class
	       'using (semantic-fetch-tags))))
    (dolist (name srecode-cpp-namespaces)
      (let ((variable (format "PREFIX_%s" (upcase name)))
	    (prefix   (format "%s::"      name)))
	(srecode-dictionary-set-value dict variable prefix)
	(dolist (tag tags)
	  (when (and (eq (semantic-tag-get-attribute tag :kind)
			 'namespace)
		     (string= (semantic-tag-name tag) name))
	    (srecode-dictionary-set-value dict variable ""))))))
  )

(define-mode-local-override srecode-semantic-apply-tag-to-dict
  c++-mode (tag-wrapper dict)
  "Apply C++ specific features from TAG-WRAPPER into DICT.
Calls `srecode-semantic-apply-tag-to-dict-default' first. Adds
special behavior for tag of classes include, using and function."

  ;; Use default implementation to fill in the basic properties.
  (srecode-semantic-apply-tag-to-dict-default tag-wrapper dict)

  ;; Pull out the tag for the individual pieces.
  (let* ((tag   (oref tag-wrapper :prime))
	 (class (semantic-tag-class tag)))

    ;; Add additional information based on the class of the tag.
    (cond
     ;;
     ;; INCLUDE
     ;;
     ((eq class 'include)
      ;; For include tags, we have to discriminate between system-wide
      ;; and local includes.
      (if (semantic-tag-include-system-p tag)
	(srecode-dictionary-show-section dict "SYSTEM")
	(srecode-dictionary-show-section dict "LOCAL")))

     ;;
     ;; USING
     ;;
     ((eq class 'using)
      ;; Insert the subject (a tag) of the include statement as VALUE
      ;; entry into the dictionary.
      (let ((value-tag  (semantic-tag-get-attribute tag :value))
	    (value-dict (srecode-dictionary-add-section-dictionary
			 dict "VALUE")))
	(srecode-semantic-apply-tag-to-dict
	 (srecode-semantic-tag (semantic-tag-name value-tag)
			       :prime value-tag)
	 value-dict))

      ;; Discriminate using statements referring to namespaces and
      ;; types.
      (when (eq (semantic-tag-get-attribute tag :kind) 'namespace)
	(srecode-dictionary-show-section dict "NAMESPACE")))

     ;;
     ;; FUNCTION
     ;;
     ((eq class 'function)
      ;; @todo It would be nice to distinguish member functions from
      ;; free functions and only apply the const and pure modifiers,
      ;; when they make sense. My best bet would be
      ;; (semantic-tag-function-parent tag), but it is not there, when
      ;; the function is defined in the scope of a class.
      (let ((member t)
	    (templates (semantic-tag-get-attribute tag :template))
	    (modifiers (semantic-tag-modifiers tag)))

	;; Add modifiers into the dictionary
	(dolist (modifier modifiers)
	  (let ((modifier-dict (srecode-dictionary-add-section-dictionary
				dict "MODIFIERS")))
	    (srecode-dictionary-set-value modifier-dict "NAME" modifier)))

	;; Add templates into child dictionaries.
	(srecode-cpp-apply-templates dict templates)

	;; When the function is a member function, it can have
	;; additional modifiers.
	(when member

	  ;; For member functions, constness is called
	  ;; 'methodconst-flag'.
	  (when (semantic-tag-get-attribute tag :methodconst-flag)
	    (srecode-dictionary-show-section dict "CONST"))

	  ;; If the member function is pure virtual, add a dictionary
	  ;; entry.
	  (when (semantic-tag-get-attribute tag :pure-virtual-flag)
	    (srecode-dictionary-show-section dict "PURE"))
	  )))

     ;;
     ;; CLASS
     ;;
     ((eq class 'type)
      ;; For classes, add template parameters.
      (when (or (semantic-tag-of-type-p tag "class")
		(semantic-tag-of-type-p tag "struct"))

	;; Add templates into child dictionaries.
	(let ((templates (semantic-tag-get-attribute tag :template)))
	  (srecode-cpp-apply-templates dict templates))))
     ))
  )


;;; Helper functions
;;

(defun srecode-cpp-apply-templates (dict templates)
  "Add section dictionaries for TEMPLATES to DICT."
  (when templates
    (let ((templates-dict (srecode-dictionary-add-section-dictionary
			   dict "TEMPLATES")))
      (dolist (template templates)
	(let ((template-dict (srecode-dictionary-add-section-dictionary
			      templates-dict "ARGS")))
	  (srecode-semantic-apply-tag-to-dict
	   (srecode-semantic-tag (semantic-tag-name template)
				 :prime template)
	   template-dict)))))
  )

(provide 'srecode/cpp)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/cpp"
;; End:

;;; srecode/cpp.el ends here
