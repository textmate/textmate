;;; ob-tangle.el --- extract source code from org-mode files

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

;; Extract the code from source blocks out into raw source-code files.

;;; Code:
(require 'ob)
(require 'org-src)
(eval-when-compile
  (require 'cl))

(declare-function org-link-escape "org" (text &optional table))
(declare-function org-heading-components "org" ())
(declare-function org-back-to-heading "org" (invisible-ok))
(declare-function org-fill-template "org" (template alist))
(declare-function org-babel-update-block-body "org" (new-body))
(declare-function make-directory "files" (dir &optional parents))

;;;###autoload
(defcustom org-babel-tangle-lang-exts
  '(("emacs-lisp" . "el"))
  "Alist mapping languages to their file extensions.
The key is the language name, the value is the string that should
be inserted as the extension commonly used to identify files
written in this language.  If no entry is found in this list,
then the name of the language is used."
  :group 'org-babel-tangle
  :version "24.1"
  :type '(repeat
	  (cons
	   (string "Language name")
	   (string "File Extension"))))

(defcustom org-babel-post-tangle-hook nil
  "Hook run in code files tangled by `org-babel-tangle'."
  :group 'org-babel
  :version "24.1"
  :type 'hook)

(defcustom org-babel-pre-tangle-hook '(save-buffer)
  "Hook run at the beginning of `org-babel-tangle'."
  :group 'org-babel
  :version "24.1"
  :type 'hook)

(defcustom org-babel-tangle-body-hook nil
  "Hook run over the contents of each code block body."
  :group 'org-babel
  :version "24.1"
  :type 'hook)

(defcustom org-babel-tangle-comment-format-beg "[[%link][%source-name]]"
  "Format of inserted comments in tangled code files.
The following format strings can be used to insert special
information into the output using `org-fill-template'.
%start-line --- the line number at the start of the code block
%file --------- the file from which the code block was tangled
%link --------- Org-mode style link to the code block
%source-name -- name of the code block

Whether or not comments are inserted during tangling is
controlled by the :comments header argument."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defcustom org-babel-tangle-comment-format-end "%source-name ends here"
  "Format of inserted comments in tangled code files.
The following format strings can be used to insert special
information into the output using `org-fill-template'.
%start-line --- the line number at the start of the code block
%file --------- the file from which the code block was tangled
%link --------- Org-mode style link to the code block
%source-name -- name of the code block

Whether or not comments are inserted during tangling is
controlled by the :comments header argument."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defcustom org-babel-process-comment-text #'org-babel-trim
  "Function called to process raw Org-mode text collected to be
inserted as comments in tangled source-code files.  The function
should take a single string argument and return a string
result.  The default value is `org-babel-trim'."
  :group 'org-babel
  :version "24.1"
  :type 'function)

(defun org-babel-find-file-noselect-refresh (file)
  "Find file ensuring that the latest changes on disk are
represented in the file."
  (find-file-noselect file)
  (with-current-buffer (get-file-buffer file)
    (revert-buffer t t t)))

(defmacro org-babel-with-temp-filebuffer (file &rest body)
  "Open FILE into a temporary buffer execute BODY there like
`progn', then kill the FILE buffer returning the result of
evaluating BODY."
  (declare (indent 1))
  (let ((temp-result (make-symbol "temp-result"))
	(temp-file (make-symbol "temp-file"))
	(visited-p (make-symbol "visited-p")))
    `(let (,temp-result ,temp-file
           (,visited-p (get-file-buffer ,file)))
       (org-babel-find-file-noselect-refresh ,file)
       (setf ,temp-file (get-file-buffer ,file))
       (with-current-buffer ,temp-file
	 (setf ,temp-result (progn ,@body)))
       (unless ,visited-p (kill-buffer ,temp-file))
       ,temp-result)))
(def-edebug-spec org-babel-with-temp-filebuffer (form body))

;;;###autoload
(defun org-babel-load-file (file)
  "Load Emacs Lisp source code blocks in the Org-mode FILE.
This function exports the source code using
`org-babel-tangle' and then loads the resulting file using
`load-file'."
  (interactive "fFile to load: ")
  (flet ((age (file)
              (float-time
               (time-subtract (current-time)
                              (nth 5 (or (file-attributes (file-truename file))
                                         (file-attributes file)))))))
    (let* ((base-name (file-name-sans-extension file))
           (exported-file (concat base-name ".el")))
      ;; tangle if the org-mode file is newer than the elisp file
      (unless (and (file-exists-p exported-file)
		   (> (age file) (age exported-file)))
        (org-babel-tangle-file file exported-file "emacs-lisp"))
      (load-file exported-file)
      (message "loaded %s" exported-file))))

;;;###autoload
(defun org-babel-tangle-file (file &optional target-file lang)
  "Extract the bodies of source code blocks in FILE.
Source code blocks are extracted with `org-babel-tangle'.
Optional argument TARGET-FILE can be used to specify a default
export file for all source blocks.  Optional argument LANG can be
used to limit the exported source code blocks by language."
  (interactive "fFile to tangle: \nP")
  (let ((visited-p (get-file-buffer (expand-file-name file)))
	to-be-removed)
    (save-window-excursion
      (find-file file)
      (setq to-be-removed (current-buffer))
      (org-babel-tangle nil target-file lang))
    (unless visited-p
      (kill-buffer to-be-removed))))

(defun org-babel-tangle-publish (_ filename pub-dir)
  "Tangle FILENAME and place the results in PUB-DIR."
  (mapc (lambda (el) (copy-file el pub-dir t)) (org-babel-tangle-file filename)))

;;;###autoload
(defun org-babel-tangle (&optional only-this-block target-file lang)
  "Write code blocks to source-specific files.
Extract the bodies of all source code blocks from the current
file into their own source-specific files.  Optional argument
TARGET-FILE can be used to specify a default export file for all
source blocks.  Optional argument LANG can be used to limit the
exported source code blocks by language."
  (interactive "P")
  (run-hooks 'org-babel-pre-tangle-hook)
  ;; possibly restrict the buffer to the current code block
  (save-restriction
  (when only-this-block
    (unless (org-babel-where-is-src-block-head)
      (error "Point is not currently inside of a code block"))
    (save-match-data
      (unless (or (cdr (assoc :tangle (nth 2 (org-babel-get-src-block-info))))
		  target-file)
	(setq target-file
	      (read-from-minibuffer "Tangle to: " (buffer-file-name)))))
    (narrow-to-region (match-beginning 0) (match-end 0)))
  (save-excursion
    (let ((block-counter 0)
	  (org-babel-default-header-args
	   (if target-file
	       (org-babel-merge-params org-babel-default-header-args
				       (list (cons :tangle target-file)))
	     org-babel-default-header-args))
          path-collector)
      (mapc ;; map over all languages
       (lambda (by-lang)
         (let* ((lang (car by-lang))
                (specs (cdr by-lang))
		(ext (or (cdr (assoc lang org-babel-tangle-lang-exts)) lang))
                (lang-f (intern
			 (concat
			  (or (and (cdr (assoc lang org-src-lang-modes))
				   (symbol-name
				    (cdr (assoc lang org-src-lang-modes))))
			      lang)
			  "-mode")))
                she-banged)
           (mapc
            (lambda (spec)
              (flet ((get-spec (name)
                               (cdr (assoc name (nth 4 spec)))))
                (let* ((tangle (get-spec :tangle))
                       (she-bang ((lambda (sheb) (when (> (length sheb) 0) sheb))
				  (get-spec :shebang)))
                       (base-name (cond
				   ((string= "yes" tangle)
				    (file-name-sans-extension
				     (buffer-file-name)))
				   ((string= "no" tangle) nil)
				   ((> (length tangle) 0) tangle)))
                       (file-name (when base-name
                                    ;; decide if we want to add ext to base-name
                                    (if (and ext (string= "yes" tangle))
                                        (concat base-name "." ext) base-name))))
                  (when file-name
		    ;; possibly create the parent directories for file
		    (when ((lambda (m) (and m (not (string= m "no"))))
			   (get-spec :mkdirp))
		      (make-directory (file-name-directory file-name) 'parents))
                    ;; delete any old versions of file
                    (when (and (file-exists-p file-name)
                               (not (member file-name path-collector)))
                      (delete-file file-name))
                    ;; drop source-block to file
                    (with-temp-buffer
                      (when (fboundp lang-f) (ignore-errors (funcall lang-f)))
                      (when (and she-bang (not (member file-name she-banged)))
                        (insert (concat she-bang "\n"))
                        (setq she-banged (cons file-name she-banged)))
                      (org-babel-spec-to-string spec)
		      ;; We avoid append-to-file as it does not work with tramp.
		      (let ((content (buffer-string)))
			(with-temp-buffer
			  (if (file-exists-p file-name)
			      (insert-file-contents file-name))
			  (goto-char (point-max))
			  (insert content)
			  (write-region nil nil file-name))))
		    ;; if files contain she-bangs, then make the executable
		    (when she-bang (set-file-modes file-name #o755))
                    ;; update counter
                    (setq block-counter (+ 1 block-counter))
                    (add-to-list 'path-collector file-name)))))
            specs)))
       (org-babel-tangle-collect-blocks lang))
      (message "tangled %d code block%s from %s" block-counter
               (if (= block-counter 1) "" "s")
	       (file-name-nondirectory
		(buffer-file-name (or (buffer-base-buffer) (current-buffer)))))
      ;; run `org-babel-post-tangle-hook' in all tangled files
      (when org-babel-post-tangle-hook
	(mapc
	 (lambda (file)
	   (org-babel-with-temp-filebuffer file
	     (run-hooks 'org-babel-post-tangle-hook)))
	 path-collector))
      path-collector))))

(defun org-babel-tangle-clean ()
  "Remove comments inserted by `org-babel-tangle'.
Call this function inside of a source-code file generated by
`org-babel-tangle' to remove all comments inserted automatically
by `org-babel-tangle'.  Warning, this comment removes any lines
containing constructs which resemble org-mode file links or noweb
references."
  (interactive)
  (goto-char (point-min))
  (while (or (re-search-forward "\\[\\[file:.*\\]\\[.*\\]\\]" nil t)
             (re-search-forward "<<[^[:space:]]*>>" nil t))
    (delete-region (save-excursion (beginning-of-line 1) (point))
                   (save-excursion (end-of-line 1) (forward-char 1) (point)))))

(defvar org-stored-links)
(defvar org-bracket-link-regexp)
(defun org-babel-tangle-collect-blocks (&optional language)
  "Collect source blocks in the current Org-mode file.
Return an association list of source-code block specifications of
the form used by `org-babel-spec-to-string' grouped by language.
Optional argument LANG can be used to limit the collected source
code blocks by language."
  (let ((block-counter 1) (current-heading "") blocks)
    (org-babel-map-src-blocks (buffer-file-name)
      ((lambda (new-heading)
	 (if (not (string= new-heading current-heading))
	     (progn
	       (setq block-counter 1)
	       (setq current-heading new-heading))
	   (setq block-counter (+ 1 block-counter))))
       (replace-regexp-in-string "[ \t]" "-"
				 (condition-case nil
				     (nth 4 (org-heading-components))
				   (error (buffer-file-name)))))
      (let* ((start-line (save-restriction (widen)
					   (+ 1 (line-number-at-pos (point)))))
	     (file (buffer-file-name))
	     (info (org-babel-get-src-block-info 'light))
	     (src-lang (nth 0 info)))
        (unless (string= (cdr (assoc :tangle (nth 2 info))) "no")
          (unless (and language (not (string= language src-lang)))
	    (let* ((info (org-babel-get-src-block-info))
		   (params (nth 2 info))
		   (link ((lambda (link)
			    (and (string-match org-bracket-link-regexp link)
				 (match-string 1 link)))
			  (org-babel-clean-text-properties
			   (org-store-link nil))))
		   (source-name
		    (intern (or (nth 4 info)
				(format "%s:%d"
					current-heading block-counter))))
		   (expand-cmd
		    (intern (concat "org-babel-expand-body:" src-lang)))
		   (assignments-cmd
		    (intern (concat "org-babel-variable-assignments:" src-lang)))
		   (body
		    ((lambda (body) ;; run the tangle-body-hook
		       (with-temp-buffer
			 (insert body)
			 (run-hooks 'org-babel-tangle-body-hook)
			 (buffer-string)))
		     ((lambda (body) ;; expand the body in language specific manner
			(if (assoc :no-expand params)
			    body
			  (if (fboundp expand-cmd)
			      (funcall expand-cmd body params)
			    (org-babel-expand-body:generic
			     body params
			     (and (fboundp assignments-cmd)
				  (funcall assignments-cmd params))))))
		      (if (and (cdr (assoc :noweb params)) ;; expand noweb refs
			       (let ((nowebs (split-string
					      (cdr (assoc :noweb params)))))
				 (or (member "yes" nowebs)
				     (member "tangle" nowebs))))
			  (org-babel-expand-noweb-references info)
			(nth 1 info)))))
		   (comment
		    (when (or (string= "both" (cdr (assoc :comments params)))
			      (string= "org" (cdr (assoc :comments params))))
		      ;; from the previous heading or code-block end
		      (funcall
		       org-babel-process-comment-text
		       (buffer-substring
			(max (condition-case nil
				 (save-excursion
				   (org-back-to-heading t)  ; sets match data
				   (match-end 0))
			       (error (point-min)))
			     (save-excursion
			       (if (re-search-backward
				    org-babel-src-block-regexp nil t)
				   (match-end 0)
				 (point-min))))
			(point)))))
		   by-lang)
	      ;; add the spec for this block to blocks under it's language
	      (setq by-lang (cdr (assoc src-lang blocks)))
	      (setq blocks (delq (assoc src-lang blocks) blocks))
	      (setq blocks (cons
			    (cons src-lang
				  (cons (list start-line file link
					      source-name params body comment)
					by-lang)) blocks)))))))
    ;; ensure blocks in the correct order
    (setq blocks
          (mapcar
	   (lambda (by-lang) (cons (car by-lang) (reverse (cdr by-lang))))
	   blocks))
    blocks))

(defun org-babel-spec-to-string (spec)
  "Insert SPEC into the current file.
Insert the source-code specified by SPEC into the current
source code file.  This function uses `comment-region' which
assumes that the appropriate major-mode is set.  SPEC has the
form

  (start-line file link source-name params body comment)"
  (let* ((start-line (nth 0 spec))
	 (file (nth 1 spec))
	 (link (nth 2 spec))
	 (source-name (nth 3 spec))
	 (body (nth 5 spec))
	 (comment (nth 6 spec))
	 (comments (cdr (assoc :comments (nth 4 spec))))
	 (padline (not (string= "no" (cdr (assoc :padline (nth 4 spec))))))
	 (link-p (or (string= comments "both") (string= comments "link")
		     (string= comments "yes") (string= comments "noweb")))
	 (link-data (mapcar (lambda (el)
			      (cons (symbol-name el)
				    ((lambda (le)
				       (if (stringp le) le (format "%S" le)))
				     (eval el))))
			    '(start-line file link source-name))))
    (flet ((insert-comment (text)
            (when (and comments (not (string= comments "no"))
		       (> (length text) 0))
	      (when padline (insert "\n"))
	      (comment-region (point) (progn (insert text) (point)))
	      (end-of-line nil) (insert "\n"))))
      (when comment (insert-comment comment))
      (when link-p
	(insert-comment
	 (org-fill-template org-babel-tangle-comment-format-beg link-data)))
      (when padline (insert "\n"))
      (insert
       (format
	"%s\n"
	(replace-regexp-in-string
	 "^," ""
	 (org-babel-trim body (if org-src-preserve-indentation "[\f\n\r\v]")))))
      (when link-p
	(insert-comment
	 (org-fill-template org-babel-tangle-comment-format-end link-data))))))

(defun org-babel-tangle-comment-links ( &optional info)
  "Return a list of begin and end link comments for the code block at point."
  (let* ((start-line (org-babel-where-is-src-block-head))
	 (file (buffer-file-name))
	 (link (org-link-escape (progn (call-interactively 'org-store-link)
				       (org-babel-clean-text-properties
					(car (pop org-stored-links))))))
	 (source-name (nth 4 (or info (org-babel-get-src-block-info 'light))))
	 (link-data (mapcar (lambda (el)
			      (cons (symbol-name el)
				    ((lambda (le)
				       (if (stringp le) le (format "%S" le)))
				     (eval el))))
			    '(start-line file link source-name))))
    (list (org-fill-template org-babel-tangle-comment-format-beg link-data)
	  (org-fill-template org-babel-tangle-comment-format-end link-data))))

;; de-tangling functions
(defvar org-bracket-link-analytic-regexp)
(defun org-babel-detangle (&optional source-code-file)
  "Propagate changes in source file back original to Org-mode file.
This requires that code blocks were tangled with link comments
which enable the original code blocks to be found."
  (interactive)
  (save-excursion
    (when source-code-file (find-file source-code-file))
    (goto-char (point-min))
    (let ((counter 0) new-body end)
      (while (re-search-forward org-bracket-link-analytic-regexp nil t)
        (when (re-search-forward
	       (concat " " (regexp-quote (match-string 5)) " ends here"))
          (setq end (match-end 0))
          (forward-line -1)
          (save-excursion
	    (when (setq new-body (org-babel-tangle-jump-to-org))
	      (org-babel-update-block-body new-body)))
          (setq counter (+ 1 counter)))
        (goto-char end))
      (prog1 counter (message "detangled %d code blocks" counter)))))

(defun org-babel-tangle-jump-to-org ()
  "Jump from a tangled code file to the related Org-mode file."
  (interactive)
  (let ((mid (point))
	start end done
        target-buffer target-char link path block-name body)
    (save-window-excursion
      (save-excursion
	(while (and (re-search-backward org-bracket-link-analytic-regexp nil t)
		    (not ; ever wider searches until matching block comments
		     (and (setq start (point-at-eol))
			  (setq link (match-string 0))
			  (setq path (match-string 3))
			  (setq block-name (match-string 5))
			  (save-excursion
			    (save-match-data
			      (re-search-forward
			       (concat " " (regexp-quote block-name)
				       " ends here") nil t)
			      (setq end (point-at-bol))))))))
	(unless (and start (< start mid) (< mid end))
	  (error "not in tangled code"))
        (setq body (org-babel-trim (buffer-substring start end))))
      (when (string-match "::" path)
        (setq path (substring path 0 (match-beginning 0))))
      (find-file path) (setq target-buffer (current-buffer))
      (goto-char start) (org-open-link-from-string link)
      (if (string-match "[^ \t\n\r]:\\([[:digit:]]+\\)" block-name)
          (org-babel-next-src-block
           (string-to-number (match-string 1 block-name)))
        (org-babel-goto-named-src-block block-name))
      (setq target-char (point)))
    (pop-to-buffer target-buffer)
    (prog1 body (goto-char target-char))))

(provide 'ob-tangle)



;;; ob-tangle.el ends here
