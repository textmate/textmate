;;; rng-nxml.el --- make nxml-mode take advantage of rng-validate-mode

;; Copyright (C) 2003, 2007-2012  Free Software Foundation, Inc.

;; Author: James Clark
;; Keywords: XML, RelaxNG

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

;;; Code:

(require 'easymenu)
(require 'xmltok)
(require 'nxml-util)
(require 'nxml-ns)
(require 'rng-match)
(require 'rng-util)
(require 'rng-valid)
(require 'nxml-mode)
(require 'rng-loc)

(defcustom rng-nxml-auto-validate-flag t
  "Non-nil means automatically turn on validation with nxml-mode."
  :type 'boolean
  :group 'relax-ng)

(defcustom rng-preferred-prefix-alist
  '(("http://www.w3.org/1999/XSL/Transform" . "xsl")
    ("http://www.w3.org/1999/02/22-rdf-syntax-ns#" . "rdf")
    ("http://www.w3.org/1999/xlink" . "xlink")
    ("http://www.w3.org/2001/XmlSchema" . "xsd")
    ("http://www.w3.org/2001/XMLSchema-instance" . "xsi")
    ("http://purl.org/dc/elements/1.1/" . "dc")
    ("http://purl.org/dc/terms/" . "dcterms"))
  "Alist of namespaces vs preferred prefixes."
  :type '(repeat (cons :tag "With"
		       (string :tag "this namespace URI")
		       (string :tag "use this prefix")))
  :group 'relax-ng)

(defvar rng-complete-end-tags-after-< t
  "*Non-nil means immediately after < complete on end-tag names.
Complete on start-tag names regardless.")

(defvar rng-nxml-easy-menu
  '("XML"
    ["Show Outline Only" nxml-hide-all-text-content]
    ["Show Everything" nxml-show-all]
    "---"
    ["Validation" rng-validate-mode
     :style toggle
     :selected rng-validate-mode]
    "---"
    ("Set Schema"
     ["Automatically" rng-auto-set-schema]
     ("For Document Type"
      :filter (lambda (menu)
		(mapcar (lambda (type-id)
			  (vector type-id
				  (list 'rng-set-document-type
					type-id)))
			(rng-possible-type-ids))))
     ["Any Well-Formed XML" rng-set-vacuous-schema]
     ["File..." rng-set-schema-file])
    ["Show Schema Location" rng-what-schema]
    ["Save Schema Location" rng-save-schema-location :help
     "Save the location of the schema currently being used for this buffer"]
    "---"
    ["First Error" rng-first-error :active rng-validate-mode]
    ["Next Error" rng-next-error :active rng-validate-mode]
    "---"
    ["Customize nXML" (customize-group 'nxml)]))

;;;###autoload
(defun rng-nxml-mode-init ()
  "Initialize `nxml-mode' to take advantage of `rng-validate-mode'.
This is typically called from `nxml-mode-hook'.
Validation will be enabled if `rng-nxml-auto-validate-flag' is non-nil."
  (interactive)
  (define-key nxml-mode-map "\C-c\C-v" 'rng-validate-mode)
  (define-key nxml-mode-map "\C-c\C-s\C-w" 'rng-what-schema)
  (define-key nxml-mode-map "\C-c\C-s\C-a" 'rng-auto-set-schema-and-validate)
  (define-key nxml-mode-map "\C-c\C-s\C-f" 'rng-set-schema-file-and-validate)
  (define-key nxml-mode-map "\C-c\C-s\C-l" 'rng-save-schema-location)
  (define-key nxml-mode-map "\C-c\C-s\C-t" 'rng-set-document-type-and-validate)
  (define-key nxml-mode-map "\C-c\C-n" 'rng-next-error)
  (easy-menu-define rng-nxml-menu nxml-mode-map
    "Menu for nxml-mode used with rng-validate-mode."
    rng-nxml-easy-menu)
  (add-to-list 'mode-line-process
               '(rng-validate-mode (:eval (rng-compute-mode-line-string)))
               'append)
  (cond (rng-nxml-auto-validate-flag
	 (rng-validate-mode 1)
	 (add-hook 'nxml-completion-hook 'rng-complete nil t)
	 (add-hook 'nxml-in-mixed-content-hook 'rng-in-mixed-content-p nil t))
	(t
	 (rng-validate-mode 0)
	 (remove-hook 'nxml-completion-hook 'rng-complete t)
	 (remove-hook 'nxml-in-mixed-content-hook 'rng-in-mixed-content-p t))))

(defvar rng-tag-history nil)
(defvar rng-attribute-name-history nil)
(defvar rng-attribute-value-history nil)

(defvar rng-complete-target-names nil)
(defvar rng-complete-name-attribute-flag nil)
(defvar rng-complete-extra-strings nil)

(defun rng-complete ()
  "Complete the string before point using the current schema.
Return non-nil if in a context it understands."
  (interactive)
  (and rng-validate-mode
       (let ((lt-pos (save-excursion (search-backward "<" nil t)))
	     xmltok-dtd)
	 (and lt-pos
	      (= (rng-set-state-after lt-pos) lt-pos)
	      (or (rng-complete-tag lt-pos)
		  (rng-complete-end-tag lt-pos)
		  (rng-complete-attribute-name lt-pos)
		  (rng-complete-attribute-value lt-pos))))))

(defconst rng-in-start-tag-name-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<\\(?:w\\(?::w?\\)?\\)?\\="
   t
   t))

(defun rng-complete-tag (lt-pos)
  (let (rng-complete-extra-strings)
    (when (and (= lt-pos (1- (point)))
	       rng-complete-end-tags-after-<
	       rng-open-elements
	       (not (eq (car rng-open-elements) t))
	       (or rng-collecting-text
		   (rng-match-save
		     (rng-match-end-tag))))
      (setq rng-complete-extra-strings
	    (cons (concat "/"
			  (if (caar rng-open-elements)
			      (concat (caar rng-open-elements)
				      ":"
				      (cdar rng-open-elements))
			    (cdar rng-open-elements)))
		  rng-complete-extra-strings)))
    (when (save-excursion
	    (re-search-backward rng-in-start-tag-name-regex
				lt-pos
				t))
      (and rng-collecting-text (rng-flush-text))
      (let ((completion
	     (let ((rng-complete-target-names
		    (rng-match-possible-start-tag-names))
		   (rng-complete-name-attribute-flag nil))
	       (rng-complete-before-point (1+ lt-pos)
					  'rng-complete-qname-function
					  "Tag: "
					  nil
					  'rng-tag-history)))
	    name)
	(when completion
	  (cond ((rng-qname-p completion)
		 (setq name (rng-expand-qname completion
					      t
					      'rng-start-tag-expand-recover))
		 (when (and name
			    (rng-match-start-tag-open name)
			    (or (not (rng-match-start-tag-close))
				;; need a namespace decl on the root element
				(and (car name)
				     (not rng-open-elements))))
		   ;; attributes are required
		   (insert " ")))
		((member completion rng-complete-extra-strings)
		 (insert ">")))))
      t)))

(defconst rng-in-end-tag-name-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "</\\(?:w\\(?::w?\\)?\\)?\\="
   t
   t))

(defun rng-complete-end-tag (lt-pos)
  (when (save-excursion
	  (re-search-backward rng-in-end-tag-name-regex
			      lt-pos
			      t))
    (cond ((or (not rng-open-elements)
	       (eq (car rng-open-elements) t))
	   (message "No matching start-tag")
	   (ding))
	  (t
	   (let ((start-tag-name
		  (if (caar rng-open-elements)
		      (concat (caar rng-open-elements)
			      ":"
			      (cdar rng-open-elements))
		    (cdar rng-open-elements)))
		 (end-tag-name
		  (buffer-substring-no-properties (+ (match-beginning 0) 2)
						  (point))))
	     (cond ((or (> (length end-tag-name)
			   (length start-tag-name))
			(not (string= (substring start-tag-name
						 0
						 (length end-tag-name))
				      end-tag-name)))
		    (message "Expected end-tag %s"
			     (rng-quote-string
			      (concat "</" start-tag-name ">")))
		    (ding))
		   (t
		    (delete-region (- (point) (length end-tag-name))
				   (point))
		    (insert start-tag-name ">")
		    (when (not (or rng-collecting-text
				   (rng-match-end-tag)))
		      (message "Element %s is incomplete"
			       (rng-quote-string start-tag-name))))))))
    t))

(defconst rng-in-attribute-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
[ \t\r\n]+\\(\\(?:w\\(?::w?\\)?\\)?\\)\\="
   t
   t))

(defvar rng-undeclared-prefixes nil)

(defun rng-complete-attribute-name (lt-pos)
  (when (save-excursion
	  (re-search-backward rng-in-attribute-regex lt-pos t))
    (let ((attribute-start (match-beginning 1))
	  rng-undeclared-prefixes)
      (and (rng-adjust-state-for-attribute lt-pos
					   attribute-start)
	   (let ((rng-complete-target-names
		  (rng-match-possible-attribute-names))
		 (rng-complete-extra-strings
		  (mapcar (lambda (prefix)
			    (if prefix
				(concat "xmlns:" prefix)
			      "xmlns"))
			  rng-undeclared-prefixes))
		 (rng-complete-name-attribute-flag t))
	     (rng-complete-before-point attribute-start
					'rng-complete-qname-function
					"Attribute: "
					nil
					'rng-attribute-name-history))
	   (insert "=\"")))
    t))

(defconst rng-in-attribute-value-regex
  (replace-regexp-in-string
   "w"
   xmltok-ncname-regexp
   "<w\\(?::w\\)?\
\\(?:[ \t\r\n]+w\\(?::w\\)?[ \t\r\n]*=\
[ \t\r\n]*\\(?:\"[^\"]*\"\\|'[^']*'\\)\\)*\
[ \t\r\n]+\\(w\\(:w\\)?\\)[ \t\r\n]*=[ \t\r\n]*\
\\(\"[^\"]*\\|'[^']*\\)\\="
   t
   t))

(defun rng-complete-attribute-value (lt-pos)
  (when (save-excursion
	  (re-search-backward rng-in-attribute-value-regex lt-pos t))
    (let ((name-start (match-beginning 1))
	  (name-end (match-end 1))
	  (colon (match-beginning 2))
	  (value-start (1+ (match-beginning 3))))
      (and (rng-adjust-state-for-attribute lt-pos
					   name-start)
	   (if (string= (buffer-substring-no-properties name-start
							(or colon name-end))
			"xmlns")
	       (rng-complete-before-point
		value-start
		(rng-strings-to-completion-alist
		 (rng-possible-namespace-uris
		  (and colon
		       (buffer-substring-no-properties (1+ colon) name-end))))
		"Namespace URI: "
		nil
		'rng-namespace-uri-history)
	     (rng-adjust-state-for-attribute-value name-start
						   colon
						   name-end)
	     (rng-complete-before-point
	      value-start
	      (rng-strings-to-completion-alist
	       (rng-match-possible-value-strings))
	      "Value: "
	      nil
	      'rng-attribute-value-history))
	   (insert (char-before value-start))))
    t))

(defun rng-possible-namespace-uris (prefix)
  (let ((ns (if prefix (nxml-ns-get-prefix prefix)
	      (nxml-ns-get-default))))
    (if (and ns (memq prefix (nxml-ns-changed-prefixes)))
	(list (nxml-namespace-name ns))
      (mapcar 'nxml-namespace-name
	      (delq nxml-xml-namespace-uri
		    (rng-match-possible-namespace-uris))))))

(defconst rng-qname-regexp
  (concat "\\`"
	  xmltok-ncname-regexp
	  "\\(?:" ":" xmltok-ncname-regexp "\\)" "?" "\\'"))

(defun rng-qname-p (string)
  (and (string-match rng-qname-regexp string) t))

(defun rng-expand-qname (qname &optional defaultp recover-fun)
  (setq qname (rng-split-qname qname))
  (let ((prefix (car qname)))
    (if prefix
	(let ((ns (nxml-ns-get-prefix qname)))
	  (cond (ns (cons ns (cdr qname)))
		(recover-fun (funcall recover-fun prefix (cdr qname)))))
      (cons (and defaultp (nxml-ns-get-default)) (cdr qname)))))

(defun rng-start-tag-expand-recover (prefix local-name)
  (let ((ns (rng-match-infer-start-tag-namespace local-name)))
    (and ns
	 (cons ns local-name))))

(defun rng-split-qname (qname)
  (if (string-match ":" qname)
      (cons (substring qname 0 (match-beginning 0))
	    (substring qname (match-end 0)))
    (cons nil qname)))

(defun rng-in-mixed-content-p ()
  "Return non-nil if point is in mixed content.
Return nil only if point is definitely not in mixed content.
If unsure, return non-nil."
  (if (eq rng-current-schema rng-any-element)
      t
    (rng-set-state-after)
    (rng-match-mixed-text)))

(defun rng-set-state-after (&optional pos)
  "Set the state for after parsing the first token with endpoint >= POS.
This does not change the xmltok state or point.  However, it does
set `xmltok-dtd'.  Returns the position of the end of the token."
  (unless pos (setq pos (point)))
  (when (< rng-validate-up-to-date-end pos)
    (message "Parsing...")
    (while (and (rng-do-some-validation)
		(< rng-validate-up-to-date-end pos))
      ;; Display percentage validated.
      (force-mode-line-update)
      ;; Force redisplay but don't allow idle timers to run.
      (let ((timer-idle-list nil))
	(sit-for 0)))
    (message "Parsing...done"))
  (save-excursion
    (save-restriction
      (widen)
      (nxml-with-invisible-motion
	(if (= pos 1)
	    (rng-set-initial-state)
	  (let ((state (get-text-property (1- pos) 'rng-state)))
	    (cond (state
		   (rng-restore-state state)
		   (goto-char pos))
		  (t
		   (let ((start (previous-single-property-change pos
								 'rng-state)))
		     (cond (start
			    (rng-restore-state (get-text-property (1- start)
								  'rng-state))
			    (goto-char start))
			   (t (rng-set-initial-state))))))))
	(xmltok-save
	  (if (= (point) 1)
	      (xmltok-forward-prolog)
	    (setq xmltok-dtd rng-dtd))
	  (cond ((and (< pos (point))
		      ;; This handles the case where the prolog ends
		      ;; with a < without any following name-start
		      ;; character. This will be treated by the parser
		      ;; as part of the prolog, but we want to treat
		      ;; it as the start of the instance.
		      (eq (char-after pos) ?<)
		      (<= (point)
			  (save-excursion
			    (goto-char (1+ pos))
			    (skip-chars-forward " \t\r\n")
			    (point))))
		 pos)
		((< (point) pos)
		 (let ((rng-dt-namespace-context-getter
			'(nxml-ns-get-context))
		       (rng-parsing-for-state t))
		   (rng-forward pos))
		 (point))
		(t pos)))))))

(defun rng-adjust-state-for-attribute (lt-pos start)
  (xmltok-save
    (save-excursion
      (goto-char lt-pos)
      (when (memq (xmltok-forward)
		  '(start-tag
		    partial-start-tag
		    empty-element
		    partial-empty-element))
	(when (< start (point))
	  (setq xmltok-namespace-attributes
		(rng-prune-attribute-at start
					xmltok-namespace-attributes))
	  (setq xmltok-attributes
		(rng-prune-attribute-at start
					xmltok-attributes)))
	(let ((rng-parsing-for-state t)
	      (rng-dt-namespace-context-getter '(nxml-ns-get-context)))
	  (rng-process-start-tag 'stop)
	  (rng-find-undeclared-prefixes)
	  t)))))

(defun rng-find-undeclared-prefixes ()
  ;; Start with the newly effective namespace declarations.
  ;; (Includes declarations added during recovery.)
  (setq rng-undeclared-prefixes (nxml-ns-changed-prefixes))
  (let ((iter xmltok-attributes)
	(ns-state (nxml-ns-state))
	att)
    ;; Add namespace prefixes used in this tag,
    ;; but not declared in the parent.
    (nxml-ns-pop-state)
    (while iter
      (setq att (car iter))
      (let ((prefix (xmltok-attribute-prefix att)))
	(when (and prefix
		   (not (member prefix rng-undeclared-prefixes))
		   (not (nxml-ns-get-prefix prefix)))
	  (setq rng-undeclared-prefixes
		(cons prefix rng-undeclared-prefixes))))
      (setq iter (cdr iter)))
    (nxml-ns-set-state ns-state)
    ;; Remove namespace prefixes explicitly declared.
    (setq iter xmltok-namespace-attributes)
    (while iter
      (setq att (car iter))
      (setq rng-undeclared-prefixes
	    (delete (and (xmltok-attribute-prefix att)
			 (xmltok-attribute-local-name att))
		  rng-undeclared-prefixes))
      (setq iter (cdr iter)))))

(defun rng-prune-attribute-at (start atts)
  (when atts
    (let ((cur atts))
      (while (if (eq (xmltok-attribute-name-start (car cur)) start)
		 (progn
		   (setq atts (delq (car cur) atts))
		   nil)
	       (setq cur (cdr cur)))))
    atts))

(defun rng-adjust-state-for-attribute-value (name-start
					     colon
					     name-end)
  (let* ((prefix (if colon
		     (buffer-substring-no-properties name-start colon)
		   nil))
	 (local-name (buffer-substring-no-properties (if colon
							 (1+ colon)
						       name-start)
						     name-end))
	 (ns (and prefix (nxml-ns-get-prefix prefix))))
    (and (or (not prefix) ns)
	 (rng-match-attribute-name (cons ns local-name)))))

(defun rng-complete-qname-function (string predicate flag)
  (let ((alist (mapcar (lambda (name) (cons name nil))
		       (rng-generate-qname-list string))))
    (cond ((not flag)
	   (try-completion string alist predicate))
	  ((eq flag t)
	   (all-completions string alist predicate))
	  ((eq flag 'lambda)
	   (and (assoc string alist) t)))))

(defun rng-generate-qname-list (&optional string)
  (let ((forced-prefix (and string
			    (string-match ":" string)
			    (> (match-beginning 0) 0)
			    (substring string
				       0
				       (match-beginning 0))))
	(namespaces (mapcar 'car rng-complete-target-names))
	ns-prefixes-alist ns-prefixes iter ns prefer)
    (while namespaces
      (setq ns (car namespaces))
      (when ns
	(setq ns-prefixes-alist
	      (cons (cons ns (nxml-ns-prefixes-for
			      ns
			      rng-complete-name-attribute-flag))
		    ns-prefixes-alist)))
      (setq namespaces (delq ns (cdr namespaces))))
    (setq iter ns-prefixes-alist)
    (while iter
      (setq ns-prefixes (car iter))
      (setq ns (car ns-prefixes))
      (when (null (cdr ns-prefixes))
	;; No declared prefix for the namespace
	(if forced-prefix
	    ;; If namespace non-nil and prefix undeclared,
	    ;; use forced prefix.
	    (when (and ns
		       (not (nxml-ns-get-prefix forced-prefix)))
	      (setcdr ns-prefixes (list forced-prefix)))
	  (setq prefer (rng-get-preferred-unused-prefix ns))
	  (when prefer
	    (setcdr ns-prefixes (list prefer)))
	  ;; Unless it's an attribute with a non-nil namespace,
	  ;; allow no prefix for this namespace.
	  (unless rng-complete-name-attribute-flag
	    (setcdr ns-prefixes (cons nil (cdr ns-prefixes))))))
      (setq iter (cdr iter)))
    (rng-uniquify-equal
     (sort (apply 'append
		  (cons rng-complete-extra-strings
			(mapcar (lambda (name)
				  (if (car name)
				      (mapcar (lambda (prefix)
						(if prefix
						    (concat prefix
							    ":"
							    (cdr name))
						  (cdr name)))
					(cdr (assoc (car name)
						    ns-prefixes-alist)))
				    (list (cdr name))))
				rng-complete-target-names)))
	   'string<))))

(defun rng-get-preferred-unused-prefix (ns)
  (let ((ns-prefix (assoc (symbol-name ns) rng-preferred-prefix-alist))
	iter prefix)
    (when ns-prefix
      (setq prefix (cdr ns-prefix))
      (when (nxml-ns-get-prefix prefix)
	;; try to find an unused prefix
	(setq iter (memq ns-prefix rng-preferred-prefix-alist))
	(while (and iter
		    (setq ns-prefix (assoc ns iter)))
	  (if (nxml-ns-get-prefix (cdr ns-prefix))
	      (setq iter (memq ns-prefix iter))
	    (setq prefix (cdr ns-prefix))
	    nil))))
    prefix))

(defun rng-strings-to-completion-alist (strings)
  (mapcar (lambda (s) (cons s s))
	  (rng-uniquify-equal (sort (mapcar 'rng-escape-string strings)
				    'string<))))

(provide 'rng-nxml)

;;; rng-nxml.el ends here
