;;; informat.el --- info support functions package for Emacs

;; Copyright (C) 1986, 2001-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: help

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

;; Nowadays, the Texinfo formatting commands always tagify a buffer
;; (as does `makeinfo') since @anchor commands need tag tables.

;;; Code:

(require 'info)

(declare-function texinfo-format-refill "texinfmt" ())

;; From texinfmt.el
(defvar texinfo-command-start)
(defvar texinfo-command-end)

;;;###autoload
(defun Info-tagify (&optional input-buffer-name)
  "Create or update Info file tag table in current buffer or in a region."
  (interactive)
  ;; Save and restore point and restrictions.
  ;; save-restrictions would not work
  ;; because it records the old max relative to the end.
  ;; We record it relative to the beginning.
  (if input-buffer-name
      (message "Tagifying region in %s ..." input-buffer-name)
      (message
       "Tagifying %s ..."  (file-name-nondirectory (buffer-file-name))))
  (let ((omin (point-min))
	(omax (point-max))
	(nomax (= (point-max) (1+ (buffer-size))))
	(opoint (point)))
    (unwind-protect
    (progn
      (widen)
      (goto-char (point-min))
      (if (search-forward "\^_\nIndirect:\n" nil t)
          (message
           "Cannot tagify split info file.  Run this before splitting.")
        (let (tag-list
              refillp
              (case-fold-search t)
              (regexp
               (concat
                "\\("


                "\\("
                "@anchor"        ; match-string 2 matches @anchor
                "\\)"
                "\\(-no\\|-yes\\)"  ; match-string 3 matches -no or -yes
                "\\("
                "-refill"
                "\\)"

                "\\("
                "{"
                "\\)"
                "\\("
                "[^}]+"          ; match-string 6 matches arg to anchor
                "\\)"
                "\\("
                "}"
                "\\)"

                "\\|"

                "\\("
                "\n\^_\\(\^L\\)?"
                "\\)"

                "\\("
                "\n\\(File:[ \t]*\\([^,\n\t]*\\)[,\t\n]+[ \t\n]*\\)?"
                "Node:[ \t]*"
                "\\("
                "[^,\n\t]*"      ; match-string 13 matches arg to node name
                "\\)"
                "[,\t\n]"
                "\\)"

                "\\)"
                )))
          (while (re-search-forward regexp nil t)
            (if (string-equal "@anchor" (match-string 2))
                (progn
                  ;; kludge lest lose match-data
                  (if (string-equal "-yes" (match-string 3))
                      (setq refillp t))
                  (setq tag-list
                        (cons (list
                               (concat "Ref: " (match-string 6))
                               (match-beginning 0))
                              tag-list))
                  (if (eq refillp t)
                      ;; set start and end so texinfo-format-refill works
                      (let ((texinfo-command-start (match-beginning 0))
                            (texinfo-command-end (match-end 0)))
                        (texinfo-format-refill))
                  (delete-region  (match-beginning 0) (match-end 0))))
              ;; else this is a Node
              (setq tag-list
                    (cons (list
                           (concat "Node: " (match-string-no-properties 13))
                           (1+ (match-beginning 10)))
                          tag-list))))

	      (goto-char (point-max))
	      (forward-line -8)
	      (let ((buffer-read-only nil))
		(if (search-forward "\^_\nEnd tag table\n" nil t)
		    (let ((end (point)))
		      (search-backward "\nTag table:\n")
		      (beginning-of-line)
		      (delete-region (point) end)))
		(goto-char (point-max))
		(or (bolp)
		    (newline))
		(insert "\^_\f\nTag table:\n")
		(if (eq major-mode 'info-mode)
		    (move-marker Info-tag-table-marker (point)))
		(setq tag-list (nreverse tag-list))
		(while tag-list
		  (insert (car (car tag-list)) ?\177)
		  (princ (car (cdr (car tag-list))) (current-buffer))
		  (insert ?\n)
		  (setq tag-list (cdr tag-list)))
		(insert "\^_\nEnd tag table\n")))))
      (goto-char opoint)
      (narrow-to-region omin (if nomax (1+ (buffer-size))
			       (min omax (point-max))))))
  (if input-buffer-name
      (message "Tagifying region in %s done" input-buffer-name)
      (message
       "Tagifying %s done"  (file-name-nondirectory (buffer-file-name)))))


;;;###autoload
(defcustom Info-split-threshold 262144
  "The number of characters by which `Info-split' splits an info file."
  :type 'integer
  :version "23.1"
  :group 'texinfo)

;;;###autoload
(defun Info-split ()
  "Split an info file into an indirect file plus bounded-size subfiles.
Each subfile will be up to the number of characters that
`Info-split-threshold' specifies, plus one node.

To use this command, first visit a large Info file that has a tag
table.  The buffer is modified into a (small) indirect info file which
should be saved in place of the original visited file.

The subfiles are written in the same directory the original file is
in, with names generated by appending `-' and a number to the original
file name.  The indirect file still functions as an Info file, but it
contains just the tag table and a directory of subfiles."

  (interactive)
  (if (< (buffer-size) (+ 20000 Info-split-threshold))
      (error "This is too small to be worth splitting"))
  (goto-char (point-min))
  (search-forward "\^_")
  (forward-char -1)
  (let ((start (point))
	(chars-deleted 0)
	subfiles
	(subfile-number 1)
	(case-fold-search t)
	(filename (file-name-sans-versions buffer-file-name)))
    (goto-char (point-max))
    (forward-line -8)
    (setq buffer-read-only nil)
    (or (search-forward "\^_\nEnd tag table\n" nil t)
	(error "Tag table required; use M-x Info-tagify"))
    (search-backward "\nTag table:\n")
    (if (looking-at "\nTag table:\n\^_")
	(error "Tag table is just a skeleton; use M-x Info-tagify"))
    (beginning-of-line)
    (forward-char 1)
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (while (< (1+ (point)) (point-max))
	(goto-char (min (+ (point) Info-split-threshold) (point-max)))
	(search-forward "\^_" nil 'move)
	(setq subfiles
	      (cons (list (+ start chars-deleted)
			  (concat (file-name-nondirectory filename)
				  (format "-%d" subfile-number)))
		    subfiles))
	;; Put a newline at end of split file, to make Unix happier.
	(insert "\n")
	(write-region (point-min) (point)
		      (concat filename (format "-%d" subfile-number)))
	(delete-region (1- (point)) (point))
	;; Back up over the final ^_.
	(forward-char -1)
	(setq chars-deleted (+ chars-deleted (- (point) start)))
	(delete-region start (point))
	(setq subfile-number (1+ subfile-number))))
    (while subfiles
      (goto-char start)
      (insert (nth 1 (car subfiles))
	      (format ": %d" (1- (car (car subfiles))))
	      "\n")
      (setq subfiles (cdr subfiles)))
    (goto-char start)
    (insert "\^_\nIndirect:\n")
    (search-forward "\nTag Table:\n")
    (insert "(Indirect)\n")))

(defvar Info-validate-allnodes)
(defvar Info-validate-thisnode)
(defvar Info-validate-lossages)

;;;###autoload
(defun Info-validate ()
  "Check current buffer for validity as an Info file.
Check that every node pointer points to an existing node."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward "\nTag table:\n(Indirect)\n" nil t)
	  (error "Don't yet know how to validate indirect info files: \"%s\""
		 (buffer-name (current-buffer))))
      (goto-char (point-min))
      (let ((Info-validate-allnodes '(("*")))
	    (regexp "Node:[ \t]*\\([^,\n\t]*\\)[,\t\n]")
	    (case-fold-search t)
	    (tags-losing nil)
	    (Info-validate-lossages ()))
	(while (search-forward "\n\^_" nil t)
	  (forward-line 1)
	  (let ((beg (point)))
	    (forward-line 1)
	    (if (re-search-backward regexp beg t)
		(let ((name (downcase
			     (buffer-substring-no-properties
			      (match-beginning 1)
			      (progn
				(goto-char (match-end 1))
				(skip-chars-backward " \t")
				(point))))))
		  (if (assoc name Info-validate-allnodes)
		      (setq Info-validate-lossages
			    (cons (list name "Duplicate node-name" nil)
				  Info-validate-lossages))
		    (setq Info-validate-allnodes
			  (cons (list name
				      (progn
					(end-of-line)
					(and (re-search-backward
					      "prev[ious]*:" beg t)
					     (progn
					       (goto-char (match-end 0))
					       (downcase
						(Info-following-node-name)))))
				      beg)
				Info-validate-allnodes)))))))
	(goto-char (point-min))
	(while (search-forward "\n\^_" nil t)
	  (forward-line 1)
	  (let ((beg (point))
		Info-validate-thisnode next)
	    (forward-line 1)
	    (if (re-search-backward regexp beg t)
		(save-restriction
		  (let ((md (match-data)))
		    (search-forward "\n\^_" nil 'move)
		    (narrow-to-region beg (point))
		    (set-match-data md))
		  (setq Info-validate-thisnode (downcase
						(buffer-substring-no-properties
						 (match-beginning 1)
						 (progn
						   (goto-char (match-end 1))
						   (skip-chars-backward " \t")
						   (point)))))
		  (end-of-line)
		  (and (search-backward "next:" nil t)
		       (setq next (Info-validate-node-name "invalid Next"))
		       (assoc next Info-validate-allnodes)
		       (if (equal (car (cdr (assoc next Info-validate-allnodes)))
				  Info-validate-thisnode)
			   ;; allow multiple `next' pointers to one node
			   (let ((tem Info-validate-lossages))
			     (while tem
			       (if (and (equal (car (cdr (car tem)))
					       "should have Previous")
					(equal (car (car tem))
					       next))
				   (setq Info-validate-lossages
					 (delq (car tem) Info-validate-lossages)))
			       (setq tem (cdr tem))))
			 (setq Info-validate-lossages
			       (cons (list next
					   "should have Previous"
					   Info-validate-thisnode)
				     Info-validate-lossages))))
		  (end-of-line)
		  (if (re-search-backward "prev[ious]*:" nil t)
		      (Info-validate-node-name "invalid Previous"))
		  (end-of-line)
		  (if (search-backward "up:" nil t)
		      (Info-validate-node-name "invalid Up"))
		  (if (re-search-forward "\n* Menu:" nil t)
		      (while (re-search-forward "\n\\* " nil t)
			(Info-validate-node-name
			 (concat "invalid menu item "
				 (buffer-substring (point)
						   (save-excursion
						     (skip-chars-forward "^:")
						     (point))))
			 (Info-extract-menu-node-name))))
		  (goto-char (point-min))
		  (while (re-search-forward "\\*note[ \n]*[^:\t]*:" nil t)
		    (goto-char (+ (match-beginning 0) 5))
		    (skip-chars-forward " \n")
		    (Info-validate-node-name
		     (concat "invalid reference "
			     (buffer-substring (point)
					       (save-excursion
						 (skip-chars-forward "^:")
						 (point))))
		     (Info-extract-menu-node-name "Bad format cross-reference")))))))
	(setq tags-losing (not (Info-validate-tags-table)))
	(if (or Info-validate-lossages tags-losing)
	    (with-output-to-temp-buffer " *problems in info file*"
	      (while Info-validate-lossages
		(princ "In node \"")
		(princ (car (car Info-validate-lossages)))
		(princ "\", ")
		(let ((tem (nth 1 (car Info-validate-lossages))))
		  (cond ((string-match "\n" tem)
			 (princ (substring tem 0 (match-beginning 0)))
			 (princ "..."))
			(t
			 (princ tem))))
		(if (nth 2 (car Info-validate-lossages))
		    (progn
		      (princ ": ")
		      (let ((tem (nth 2 (car Info-validate-lossages))))
			(cond ((string-match "\n" tem)
			       (princ (substring tem 0 (match-beginning 0)))
			       (princ "..."))
			      (t
			       (princ tem))))))
		(terpri)
		(setq Info-validate-lossages (cdr Info-validate-lossages)))
	      (if tags-losing (princ "\nTags table must be recomputed\n")))
	  ;; Here if info file is valid.
	  ;; If we already made a list of problems, clear it out.
	  (save-excursion
	    (if (get-buffer " *problems in info file*")
		(progn
		  (set-buffer " *problems in info file*")
		  (kill-buffer (current-buffer)))))
	  (message "File appears valid"))))))

(defun Info-validate-node-name (kind &optional name)
  (if name
      nil
    (goto-char (match-end 0))
    (skip-chars-forward " \t")
    (if (= (following-char) ?\()
	nil
      (setq name
	    (buffer-substring-no-properties
	     (point)
	     (progn
	       (skip-chars-forward "^,\t\n")
	       (skip-chars-backward " ")
	       (point))))))
  (if (null name)
      nil
    (setq name (downcase name))
    (or (and (> (length name) 0) (= (aref name 0) ?\())
	(assoc name Info-validate-allnodes)
	(setq Info-validate-lossages
	      (cons (list Info-validate-thisnode kind name)
		    Info-validate-lossages))))
  name)

(defun Info-validate-tags-table ()
  (goto-char (point-min))
  (if (not (search-forward "\^_\nEnd tag table\n" nil t))
      t
    (not (catch 'losing
	   (let* ((end (match-beginning 0))
		  (start (progn (search-backward "\nTag table:\n")
				(1- (match-end 0))))
		  tem)
	     (setq tem Info-validate-allnodes)
	     (while tem
	       (goto-char start)
	       (or (equal (car (car tem)) "*")
		   (search-forward (concat "Node: "
					   (car (car tem))
					   "\177")
				   end t)
		   (throw 'losing 'x))
	       (setq tem (cdr tem)))
	     (goto-char (1+ start))
	     (while (looking-at ".*Node: \\(.*\\)\177\\([0-9]+\\)$")
	       (setq tem (downcase (buffer-substring-no-properties
				     (match-beginning 1)
				     (match-end 1))))
	       (setq tem (assoc tem Info-validate-allnodes))
	       (if (or (not tem)
		       (< 1000 (progn
				 (goto-char (match-beginning 2))
				 (setq tem (- (car (cdr (cdr tem)))
					      (read (current-buffer))))
				 (if (> tem 0) tem (- tem)))))
		   (throw 'losing 'y))
	       (forward-line 1)))
	   (if (looking-at "\^_\n")
	       (forward-line 1))
	   (or (looking-at "End tag table\n")
	       (throw 'losing 'z))
	   nil))))

;;;###autoload
(defun batch-info-validate ()
  "Runs `Info-validate' on the files remaining on the command line.
Must be used only with -batch, and kills Emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-info-validate $info/ ~/*.info\""
  (if (not noninteractive)
      (error "batch-info-validate may only be used -batch"))
  (let ((version-control t)
	(auto-save-default nil)
	(find-file-run-dired nil)
	(kept-old-versions 259259)
	(kept-new-versions 259259))
    (let ((error 0)
	  file
	  (files ()))
      (while command-line-args-left
	(setq file (expand-file-name (car command-line-args-left)))
	(cond ((not (file-exists-p file))
	       (message ">> %s does not exist!" file)
	       (setq error 1
		     command-line-args-left (cdr command-line-args-left)))
	      ((file-directory-p file)
	       (setq command-line-args-left (nconc (directory-files file)
					      (cdr command-line-args-left))))
	      (t
	       (setq files (cons file files)
		     command-line-args-left (cdr command-line-args-left)))))
      (while files
	(setq file (car files)
	      files (cdr files))
	(let ((lose nil))
	  (condition-case err
	      (progn
		(if buffer-file-name (kill-buffer (current-buffer)))
		(find-file file)
		(buffer-disable-undo (current-buffer))
		(set-buffer-modified-p nil)
		(fundamental-mode)
		(let ((case-fold-search nil))
		  (goto-char (point-max))
		  (cond ((search-backward "\n\^_\^L\nTag table:\n" nil t)
			 (message "%s already tagified" file))
			((< (point-max) 30000)
			 (message "%s too small to bother tagifying" file))
			(t
			 (Info-tagify))))
		(let ((loss-name " *problems in info file*"))
		  (message "Checking validity of info file %s..." file)
		  (if (get-buffer loss-name)
		      (kill-buffer loss-name))
		  (Info-validate)
		  (if (not (get-buffer loss-name))
		      nil ;(message "Checking validity of info file %s... OK" file)
		    (message "----------------------------------------------------------------------")
		    (message ">> PROBLEMS IN INFO FILE %s" file)
		    (with-current-buffer loss-name
		      (princ (buffer-substring-no-properties
			      (point-min) (point-max))))
		    (message "----------------------------------------------------------------------")
		    (setq error 1 lose t)))
		(if (and (buffer-modified-p)
			 (not lose))
		    (progn (message "Saving modified %s" file)
			   (save-buffer))))
	    (error (message ">> Error: %s" (prin1-to-string err))))))
      (kill-emacs error))))

(provide 'informat)

;;; informat.el ends here
