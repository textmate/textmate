;;; cus-dep.el --- find customization dependencies
;;
;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.
;;
;; Author: Per Abrahamsen <abraham@dina.kvl.dk>
;; Keywords: internal
;; Package: emacs

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

(eval-when-compile (require 'cl))
(require 'widget)
(require 'cus-face)

(defvar generated-custom-dependencies-file "cus-load.el"
  "Output file for `custom-make-dependencies'.")

;; See finder-no-scan-regexp in finder.el.
(defvar custom-dependencies-no-scan-regexp "\\(^\\.#\\|\\(loaddefs\\|\
ldefs-boot\\|cus-load\\|finder-inf\\|esh-groups\\|subdirs\\)\\.el$\\)"
  "Regexp matching file names not to scan for `custom-make-dependencies'.")

(autoload 'autoload-rubric "autoload")

(defun custom-make-dependencies ()
  "Batch function to extract custom dependencies from .el files.
Usage: emacs -batch -l ./cus-dep.el -f custom-make-dependencies DIRS"
  (let ((enable-local-eval nil)
	subdir)
    (with-temp-buffer
      ;; Use up command-line-args-left else Emacs can try to open
      ;; the args as directories after we are done.
      (while (setq subdir (pop command-line-args-left))
        (message "Directory %s" subdir)
        (let ((files (directory-files subdir nil "\\`[^=].*\\.el\\'"))
              (default-directory (expand-file-name subdir))
              (preloaded (concat "\\`"
                                 (regexp-opt (mapcar
                                              (lambda (f)
                                                (file-name-sans-extension
                                                 (file-name-nondirectory f)))
                                              preloaded-file-list) t)
                                 "\\.el\\'")))
          (dolist (file files)
            (unless (or (string-match custom-dependencies-no-scan-regexp file)
                        (string-match preloaded file)
                        (not (file-exists-p file)))
              (erase-buffer)
              (insert-file-contents file)
              (goto-char (point-min))
              (string-match "\\`\\(.*\\)\\.el\\'" file)
              (let ((name (file-name-nondirectory (match-string 1 file)))
                    (load-file-name file))
                (if (save-excursion
                      (re-search-forward
		     (concat "(provide[ \t\n]+\\('\\|(quote[ \t\n]\\)[ \t\n]*"
			     (regexp-quote name) "[ \t\n)]")
		     nil t))
                    (setq name (intern name)))
                (condition-case nil
                    (while (re-search-forward
                            "^(def\\(custom\\|face\\|group\\)" nil t)
                      (beginning-of-line)
                      (let ((expr (read (current-buffer))))
                        (condition-case nil
                            (let ((custom-dont-initialize t))
                              (eval expr)
                              (put (nth 1 expr) 'custom-where name))
                          (error nil))))
                  (error nil)))))))))
  (message "Generating %s..." generated-custom-dependencies-file)
  (set-buffer (find-file-noselect generated-custom-dependencies-file))
  (setq buffer-undo-list t)
  (erase-buffer)
  (insert (autoload-rubric generated-custom-dependencies-file
                           "custom dependencies" t))
  (search-backward "")
  (mapatoms (lambda (symbol)
	      (let ((members (get symbol 'custom-group))
                    where found)
		(when members
		  (dolist (member
                           ;; So x and no-x builds won't differ.
                           (sort (mapcar 'car members) 'string<))
		    (setq where (get member 'custom-where))
		    (unless (or (null where)
				(member where found))
		      (push where found)))
		  (when found
		    (insert "(put '" (symbol-name symbol)
                            " 'custom-loads '")
                    (prin1 (nreverse found) (current-buffer))
                    (insert ")\n"))))))
  (insert "\
;; These are for handling :version.  We need to have a minimum of
;; information so `customize-changed-options' could do its job.

;; For groups we set `custom-version', `group-documentation' and
;; `custom-tag' (which are shown in the customize buffer), so we
;; don't have to load the file containing the group.

;; `custom-versions-load-alist' is an alist that has as car a version
;; number and as elts the files that have variables or faces that
;; contain that version. These files should be loaded before showing
;; the customization buffer that `customize-changed-options'
;; generates.

;; This macro is used so we don't modify the information about
;; variables and groups if it's already set. (We don't know when
;; " (file-name-nondirectory generated-custom-dependencies-file)
      " is going to be loaded and at that time some of the
;; files might be loaded and some others might not).
\(defmacro custom-put-if-not (symbol propname value)
  `(unless (get ,symbol ,propname)
     (put ,symbol ,propname ,value)))

")
  (let ((version-alist nil))
    (mapatoms (lambda (symbol)
		(let ((version (get symbol 'custom-version))
		      where)
		  (when version
		    (setq where (get symbol 'custom-where))
		    (when where
		      (if (or (custom-variable-p symbol)
			      (custom-facep symbol))
			  ;; This means it's a variable or a face.
			  (progn
			    (if (assoc version version-alist)
				(unless
				    (member where
					    (cdr (assoc version version-alist)))
				  (push where (cdr (assoc version version-alist))))
			      (push (list version where) version-alist)))
			;; This is a group
			(insert "(custom-put-if-not '" (symbol-name symbol)
				" 'custom-version ")
			(prin1 version (current-buffer))
			(insert ")\n")
			(insert "(custom-put-if-not '" (symbol-name symbol))
			(insert " 'group-documentation ")
			(prin1 (get symbol 'group-documentation) (current-buffer))
			(insert ")\n")
			(when (get symbol 'custom-tag)
			  (insert "(custom-put-if-not '" (symbol-name symbol))
			  (insert " 'custom-tag ")
			  (prin1 (get symbol 'custom-tag) (current-buffer))
			  (insert ")\n"))
			))))))

    (insert "\n(defvar custom-versions-load-alist "
	    (if version-alist "'" ""))
    (prin1 version-alist (current-buffer))
    (insert "\n \"For internal use by custom.\")\n"))
  (save-buffer)
  (message "Generating %s...done" generated-custom-dependencies-file))



;;; cus-dep.el ends here
