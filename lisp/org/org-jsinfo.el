;;; org-jsinfo.el --- Support for org-info.js Javascript in Org HTML export

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
;; This file is part of GNU Emacs.
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements the support for Sebastian Rose's JavaScript
;; org-info.js to display an org-mode file exported to HTML in an
;; Info-like way, or using folding similar to the outline structure
;; org org-mode itself.

;; Documentation for using this module is in the Org manual.  The script
;; itself is documented by Sebastian Rose in a file distributed with
;; the script.  FIXME: Accurate pointers!

;; Org-mode loads this module by default - if this is not what you want,
;; configure the variable `org-modules'.

;;; Code:

(require 'org-exp)
(require 'org-html)

(add-to-list 'org-export-inbuffer-options-extra '("INFOJS_OPT" :infojs-opt))
(add-hook 'org-export-options-filters 'org-infojs-handle-options)

(defgroup org-infojs nil
  "Options specific for using org-info.js in HTML export of Org-mode files."
  :tag "Org Export HTML INFOJS"
  :group 'org-export-html)

(defcustom org-export-html-use-infojs 'when-configured
  "Should Sebastian Rose's Java Script org-info.js be linked into HTML files?
This option can be nil or t to never or always use the script.  It can
also be the symbol `when-configured', meaning that the script will be
linked into the export file if and only if there is a \"#+INFOJS_OPT:\"
line in the buffer.  See also the variable `org-infojs-options'."
  :group 'org-export-html
  :group 'org-infojs
  :type '(choice
	  (const :tag "Never" nil)
	  (const :tag "When configured in buffer" when-configured)
	  (const :tag "Always" t)))

(defconst org-infojs-opts-table
  '((path PATH "http://orgmode.org/org-info.js")
    (view VIEW "info")
    (toc TOC :table-of-contents)
    (ftoc FIXED_TOC "0")
    (tdepth TOC_DEPTH "max")
    (sdepth SECTION_DEPTH "max")
    (mouse MOUSE_HINT "underline")
    (buttons VIEW_BUTTONS "0")
    (ltoc LOCAL_TOC "1")
    (up LINK_UP :link-up)
    (home LINK_HOME :link-home))
  "JavaScript options, long form for script, default values.")

(defvar org-infojs-options)
(when (and (boundp 'org-infojs-options)
	   (assq 'runs org-infojs-options))
  (setq org-infojs-options (delq (assq 'runs org-infojs-options)
				 org-infojs-options)))

(defcustom org-infojs-options
  (mapcar (lambda (x) (cons (car x) (nth 2 x)))
	  org-infojs-opts-table)
  "Options settings for the INFOJS JavaScript.
Each of the options must have an entry in `org-export-html/infojs-opts-table'.
The value can either be a string that will be passed to the script, or
a property.  This property is then assumed to be a property that is defined
by the Export/Publishing setup of Org.
The `sdepth' and `tdepth' parameters can also be set to \"max\", which
means to use the maximum value consistent with other options."
  :group 'org-infojs
  :type
  `(set :greedy t :inline t
	,@(mapcar
	   (lambda (x)
	     (list 'cons (list 'const (car x))
		   '(choice
			    (symbol :tag "Publishing/Export property")
			    (string :tag "Value"))))
	   org-infojs-opts-table)))

(defcustom org-infojs-template
  "<script type=\"text/javascript\" src=\"%SCRIPT_PATH\"></script>
<script type=\"text/javascript\" >
<!--/*--><![CDATA[/*><!--*/
%MANAGER_OPTIONS
org_html_manager.setup();  // activate after the parameters are set
/*]]>*///-->
</script>"
  "The template for the export style additions when org-info.js is used.
Option settings will replace the %MANAGER-OPTIONS cookie."
  :group 'org-infojs
  :type 'string)

(defun org-infojs-handle-options (exp-plist)
  "Analyze JavaScript options in INFO-PLIST and modify EXP-PLIST accordingly."
  (if (or (not org-export-html-use-infojs)
	  (and (eq org-export-html-use-infojs 'when-configured)
	       (or (not (plist-get exp-plist :infojs-opt))
		   (string-match "\\<view:nil\\>"
				 (plist-get exp-plist :infojs-opt)))))
      ;; We do not want to use the script
      exp-plist
    ;; We do want to use the script, set it up
    (let ((template org-infojs-template)
	(ptoc (plist-get exp-plist :table-of-contents))
	(hlevels (plist-get exp-plist :headline-levels))
	tdepth sdepth s v e opt var val table default)
    (setq sdepth hlevels
	  tdepth hlevels)
    (if (integerp ptoc) (setq tdepth (min ptoc tdepth)))
    (setq v (plist-get exp-plist :infojs-opt)
	  table org-infojs-opts-table)
    (while (setq e (pop table))
      (setq opt (car e) var (nth 1 e)
	    default (cdr (assoc opt org-infojs-options)))
      (and (symbolp default) (not (memq default '(t nil)))
	   (setq default (plist-get exp-plist default)))
      (if (and v (string-match (format " %s:\\(\\S-+\\)" opt) v))
	  (setq val (match-string 1 v))
	(setq val default))
      (cond
       ((eq opt 'path)
	(and (string-match "%SCRIPT_PATH" template)
	     (setq template (replace-match val t t template))))
       ((eq opt 'sdepth)
	(if (integerp (read val))
	    (setq sdepth (min (read val) hlevels))))
       ((eq opt 'tdepth)
	(if (integerp (read val))
	    (setq tdepth (min (read val) hlevels))))
       (t
	(setq val
	      (cond
	       ((or (eq val t) (equal val "t")) "1")
	       ((or (eq val nil) (equal val "nil")) "0")
	       ((stringp val) val)
	       (t (format "%s" val))))
	(push (cons var val) s))))

    ;; Now we set the depth of the *generated* TOC to SDEPTH, because the
    ;; toc will actually determine the splitting.  How much of the toc will
    ;; actually be displayed is governed by the TDEPTH option.
    (setq exp-plist (plist-put exp-plist :table-of-contents sdepth))

    ;; The table of contents should not show more sections then we generate
    (setq tdepth (min tdepth sdepth))
    (push (cons "TOC_DEPTH" tdepth) s)

    (setq s (mapconcat
	     (lambda (x) (format "org_html_manager.set(\"%s\", \"%s\");"
				 (car x) (cdr x)))
	     s "\n"))
    (when (and s (> (length s) 0))
      (and (string-match "%MANAGER_OPTIONS" template)
	   (setq s (replace-match s t t template))
	   (setq exp-plist
		 (plist-put
		  exp-plist :style-extra
		  (concat (or (plist-get exp-plist :style-extra) "") "\n" s)))))
    ;; This script absolutely needs the table of contents, to we change that
    ;; setting
    (if (not (plist-get exp-plist :table-of-contents))
	(setq exp-plist (plist-put exp-plist :table-of-contents t)))
    ;; Return the modified property list
    exp-plist)))

(defun org-infojs-options-inbuffer-template ()
  (format "#+INFOJS_OPT: view:%s toc:%s ltoc:%s mouse:%s buttons:%s path:%s"
	  (if (eq t org-export-html-use-infojs) (cdr (assoc 'view org-infojs-options)) nil)
	  (let ((a (cdr (assoc 'toc org-infojs-options))))
	    (cond ((memq a '(nil t)) a)
		  (t (plist-get (org-infile-export-plist) :table-of-contents))))
	  (if (equal (cdr (assoc 'ltoc org-infojs-options)) "1") t nil)
	  (cdr (assoc 'mouse org-infojs-options))
	  (cdr (assoc 'buttons org-infojs-options))
	  (cdr (assoc 'path org-infojs-options))))

(provide 'org-infojs)
(provide 'org-jsinfo)

;;; org-jsinfo.el ends here
