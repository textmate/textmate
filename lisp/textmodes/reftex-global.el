;;; reftex-global.el --- operations on entire documents with RefTeX

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Maintainer: auctex-devel@gnu.org
;; Version: 4.31
;; Package: reftex

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
(provide 'reftex-global)
(require 'reftex)
;;;

(defun reftex-create-tags-file ()
  "Create TAGS file by running `etags' on the current document.
The TAGS file is also immediately visited with `visit-tags-table'."
  (interactive)
  (reftex-access-scan-info current-prefix-arg)
  (let* ((master (reftex-TeX-master-file))
         (files  (reftex-all-document-files))
         (cmd    (format "etags %s" (mapconcat 'shell-quote-argument
					       files " "))))
    (with-current-buffer (reftex-get-file-buffer-force master)
      (message "Running etags to create TAGS file...")
      (shell-command cmd)
      (visit-tags-table "TAGS"))))

;; History of grep commands.
(defvar reftex-grep-history nil)
(defvar reftex-grep-command "grep -n "
  "Last grep command used in \\[reftex-grep-document]; default for next grep.")

(defun reftex-grep-document (grep-cmd)
  "Run grep query through all files related to this document.
With prefix arg, force to rescan document.
No active TAGS table is required."

  (interactive
   (list (read-from-minibuffer "Run grep on document (like this): "
                               reftex-grep-command nil nil
                               'reftex-grep-history)))
  (reftex-access-scan-info current-prefix-arg)
  (let* ((files  (reftex-all-document-files t))
         (cmd    (format
                  "%s %s" grep-cmd
                  (mapconcat 'identity files " "))))
    (grep cmd)))

(defun reftex-search-document (&optional regexp)
  "Regexp search through all files of the current document.
Starts always in the master file.  Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].
No active TAGS table is required."
  (interactive)
  (let ((default (reftex-this-word)))
    (unless regexp
      (setq regexp (read-string (format "Search regexp in document [%s]: "
                                        default))))
    (if (string= regexp "") (setq regexp (regexp-quote default)))

    (reftex-access-scan-info current-prefix-arg)
    (tags-search regexp (list 'reftex-all-document-files))))

(defun reftex-query-replace-document (&optional from to delimited)
  "Do `query-replace-regexp' of FROM with TO over the entire document.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].
No active TAGS table is required."
  (interactive)
  (let ((default (reftex-this-word)))
    (unless from
      (setq from (read-string (format "Replace regexp in document [%s]: "
                                      default)))
      (if (string= from "") (setq from (regexp-quote default))))
    (unless to
      (setq to (read-string (format "Replace regexp %s with: " from))))
    (reftex-access-scan-info current-prefix-arg)
    (tags-query-replace from to (or delimited current-prefix-arg)
                        (list 'reftex-all-document-files))))

(defvar TeX-master)
(defvar isearch-next-buffer-function)

(defun reftex-find-duplicate-labels ()
  "Produce a list of all duplicate labels in the document."

  (interactive)

  ;; Rescan the document to make sure
  (reftex-access-scan-info t)

  (let ((master (reftex-TeX-master-file))
        (cnt 0)
        (dlist
         (mapcar
          (lambda (x)
            (let (x1)
              (cond
               ((memq (car x)
                      '(toc bof eof bib thebib label-numbers xr xr-doc
                            master-dir file-error bibview-cache appendix
                            is-multi index))
                nil)
               (t
                (setq x1 (reftex-all-assoc-string
                          (car x) (symbol-value reftex-docstruct-symbol)))
                (if (< 1 (length x1))
                    (append (list (car x))
                            (mapcar (lambda(x)
                                      (abbreviate-file-name (nth 3 x)))
                                    x1))
                  (list nil))))))
          (reftex-uniquify-by-car (symbol-value reftex-docstruct-symbol)))))

    (setq dlist (reftex-uniquify-by-car dlist))
    (if (null dlist) (error "No duplicate labels in document"))
    (switch-to-buffer-other-window "*Duplicate Labels*")
    (set (make-local-variable 'TeX-master) master)
    (erase-buffer)
    (insert "                MULTIPLE LABELS IN CURRENT DOCUMENT:\n")
    (insert
     " Move point to label and type `r' to run a query-replace on the label\n"
     " and its references.  Type `q' to exit this buffer.\n\n")
    (insert " LABEL               FILE\n")
    (insert " -------------------------------------------------------------\n")
    (use-local-map (make-sparse-keymap))
    (local-set-key [?q] (lambda () "Kill this buffer." (interactive)
                          (kill-buffer (current-buffer)) (delete-window)))
    (local-set-key [?r] 'reftex-change-label)
    (while dlist
      (when (and (car (car dlist))
                 (cdr (car dlist)))
        (incf cnt)
        (insert (mapconcat 'identity (car dlist) "\n    ") "\n"))
      (pop dlist))
    (goto-char (point-min))
    (when (= cnt 0)
      (kill-buffer (current-buffer))
      (delete-window)
      (message "Document does not contain duplicate labels."))))

(defun reftex-change-label (&optional from to)
  "Run `query-replace-regexp' of FROM with TO in all macro arguments.
Works on the entire multifile document.
If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
with the command \\[tags-loop-continue].
No active TAGS table is required."
  (interactive)
  (let ((default (reftex-this-word "-a-zA-Z0-9_*.:")))
    (unless from
      (setq from (read-string (format "Replace label globally [%s]: "
                                      default))))
    (if (string= from "") (setq from default))
    (unless to
      (setq to (read-string (format "Replace label %s with: "
                                    from))))
    (reftex-query-replace-document
     (concat "{" (regexp-quote from) "}")
     (format "{%s}" to))))

(defun reftex-renumber-simple-labels ()
  "Renumber all simple labels in the document to make them sequentially.
Simple labels are the ones created by RefTeX, consisting only of the
prefix and a number.  After the command completes, all these labels will
have sequential numbers throughout the document.  Any references to
the labels will be changed as well.  For this, RefTeX looks at the
arguments of any macros which either start or end in the string `ref'.
This command should be used with care, in particular in multifile
documents.  You should not use it if another document refers to this
one with the `xr' package."
  (interactive)
  ;; Rescan the entire document
  (reftex-access-scan-info 1)
  ;; Get some insurance
  (if (and (reftex-is-multi)
           (not (yes-or-no-p "Replacing all simple labels in multiple files is risky.  Continue? ")))
      (error "Abort"))
  ;; Make the translation list
  (let* ((re-core (concat "\\("
                          (mapconcat 'cdr reftex-typekey-to-prefix-alist "\\|")
                          "\\)"))
         (label-re (concat "\\`" re-core "\\([0-9]+\\)\\'"))
         (search-re (concat "[{,]\\(" re-core "\\([0-9]+\\)\\)[,}]"))
         (error-fmt "Undefined label or reference %s. Ignore and continue? ")
         (label-numbers-alist (mapcar (lambda (x) (cons (cdr x) 0))
                                      reftex-typekey-to-prefix-alist))
         (files (reftex-all-document-files))
         (list (symbol-value reftex-docstruct-symbol))
         translate-alist n entry label new-label nr-cell changed-sequence)

    (while (setq entry (pop list))
      (when (and (stringp (car entry))
                 (string-match label-re (car entry)))
        (setq label (car entry)
              nr-cell (assoc (match-string 1 (car entry))
                             label-numbers-alist))
        (if (assoc label translate-alist)
            (error "Duplicate label %s" label))
        (setq new-label (concat (match-string 1 (car entry))
                                (int-to-string (incf (cdr nr-cell)))))
        (push (cons label new-label) translate-alist)
        (or (string= label new-label) (setq changed-sequence t))))

    (unless changed-sequence
      (error "Simple labels are already in correct sequence"))

    (reftex-ensure-write-access (reftex-all-document-files))

    ;; Save all document buffers before this operation
    (reftex-save-all-document-buffers)

    ;; First test to check for errors.
    (setq n (reftex-translate
             files search-re translate-alist error-fmt 'test))

    ;; Now the real thing.
    (if (yes-or-no-p
         (format "Replace %d items at %d places in %d files? "
                 (length translate-alist) n (length files)))
        (progn
          (let ((inhibit-quit t))  ;; Do not disturb...
            (reftex-translate
             files search-re translate-alist error-fmt nil)
            (setq quit-flag nil))
          (if (and (reftex-is-multi)
                   (yes-or-no-p "Save entire document? "))
              (reftex-save-all-document-buffers))
          ;; Rescan again...
          (reftex-access-scan-info 1)
          (message "Done replacing simple labels."))
      (message "No replacements done"))))

(defun reftex-translate (files search-re translate-alist error-fmt test)
  ;; In FILES, look for SEARCH-RE and replace match 1 of it with
  ;; its association in TRANSLATE-ALIST.
  ;; If we do not find an association and TEST is non-nil, query
  ;; to ignore the problematic string.
  ;; If TEST is nil, it is ignored without query.
  ;; Return the number of replacements.
  (let ((n 0) file label match-data buf macro pos cell)
    (while (setq file (pop files))
      (setq buf (reftex-get-file-buffer-force file))
      (unless buf
        (error "No such file %s" file))
      (set-buffer buf)
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (re-search-forward search-re nil t)
            (backward-char)
            (save-excursion
              (setq label (reftex-match-string 1)
                    cell (assoc label translate-alist)
                    match-data (match-data)
                    macro (reftex-what-macro 1)
                    pos (cdr macro))
              (goto-char (or pos (point)))
              (when (and macro
                         (or (looking-at "\\\\ref")
                             (looking-at "\\\\[a-zA-Z]*ref\\(range\\)?[^a-zA-Z]")
                             (looking-at "\\\\ref[a-zA-Z]*[^a-zA-Z]")
                             (looking-at (format
                                          reftex-find-label-regexp-format
                                          (regexp-quote label)))))
                ;; OK, we should replace it.
                (set-match-data match-data)
                (cond
                 ((and test (not cell))
                  ;; We've got a problem
                  (unwind-protect
                      (progn
                        (reftex-highlight 1 (match-beginning 0) (match-end 0))
                        (ding)
                        (or (y-or-n-p (format error-fmt label))
                            (error "Abort")))
                    (reftex-unhighlight 1)))
                 ((and test cell)
                  (incf n))
                 ((and (not test) cell)
                  ;; Replace
                  (goto-char (match-beginning 1))
                  (delete-region (match-beginning 1) (match-end 1))
                  (insert (cdr cell)))
                 (t nil))))))))
    n))

(defun reftex-save-all-document-buffers ()
  "Save all documents associated with the current document.
The function is useful after a global action like replacing or renumbering
labels."
  (interactive)
  (let ((files (reftex-all-document-files))
        file buffer)
    (save-current-buffer
      (while (setq file (pop files))
        (setq buffer (reftex-get-buffer-visiting file))
        (when buffer
          (set-buffer buffer)
          (save-buffer))))))

(defun reftex-ensure-write-access (files)
  "Make sure we have write access to all files in FILES.
Also checks if buffers visiting the files are in read-only mode."
  (let (file buf)
    (while (setq file (pop files))
      (unless (file-exists-p file)
        (ding)
        (or (y-or-n-p (format "No such file %s. Continue? " file))
            (error "Abort")))
      (unless (file-writable-p file)
        (ding)
        (or (y-or-n-p (format "No write access to %s. Continue? " file))
            (error "Abort")))
      (when (and (setq buf (reftex-get-buffer-visiting file))
                 (with-current-buffer buf
                   buffer-read-only))
        (ding)
        (or (y-or-n-p (format "Buffer %s is read-only. Continue? "
                              (buffer-name buf)))
            (error "Abort"))))))

;;; Multi-file RefTeX Isearch

;; `reftex-isearch-wrap-function', `reftex-isearch-push-state-function',
;; `reftex-isearch-pop-state-function', `reftex-isearch-isearch-search'
;; functions remain here only for backward-compatibility with Emacs 22
;; and are obsolete since Emacs 23 that supports a single function
;; variable `multi-isearch-next-buffer-function'.

(defun reftex-isearch-wrap-function ()
  (if (not isearch-word)
      (switch-to-buffer
       (funcall isearch-next-buffer-function (current-buffer) t)))
  (goto-char (if isearch-forward (point-min) (point-max))))

(defun reftex-isearch-push-state-function ()
  `(lambda (cmd)
     (reftex-isearch-pop-state-function cmd ,(current-buffer))))

(defun reftex-isearch-pop-state-function (cmd buffer)
  (switch-to-buffer buffer))

(defun reftex-isearch-isearch-search (string bound noerror)
  (let ((nxt-buff nil)
	(search-fun
	 (cond
	  (isearch-word
	   (if isearch-forward 'word-search-forward 'word-search-backward))
	  (isearch-regexp
	   (if isearch-forward 're-search-forward 're-search-backward))
	  (t
	   (if isearch-forward 'search-forward 'search-backward)))))
    (or
     (funcall search-fun string bound noerror)
     (unless bound
       (condition-case nil
	   (when isearch-next-buffer-function
	     (while (not (funcall search-fun string bound noerror))
	       (cond
		(isearch-forward
		 (setq nxt-buff
		       (funcall isearch-next-buffer-function
				(current-buffer)))
		 (if (not nxt-buff)
		     (progn
		       (error "Wrap forward"))
		   (switch-to-buffer nxt-buff)
		   (goto-char (point-min))))
		(t
		 (setq nxt-buff
		       (funcall isearch-next-buffer-function
					 (current-buffer)))
		 (if (not nxt-buff)
		     (progn
		       (error "Wrap backward"))
		   (switch-to-buffer nxt-buff)
		   (goto-char (point-max))))))
	     (point))
	 (error nil))))))

;; This function is called when isearch reaches the end of a
;; buffer. For reftex what we want to do is not wrap to the
;; beginning, but switch to the next buffer in the logical order of
;; the document.  This function looks through list of files in the
;; document (reftex-all-document-files), searches for the current
;; buffer and switches to the next/previous one in the logical order
;; of the document.  If WRAPP is true then wrap the search to the
;; beginning/end of the file list, depending of the search direction.
(defun reftex-isearch-switch-to-next-file (crt-buf &optional wrapp)
  (reftex-access-scan-info)
  (let ((cb (buffer-file-name crt-buf))
	(flist (reftex-all-document-files)))
    (when flist
      (if wrapp
	  (unless isearch-forward
	      (setq flist (last flist)))
	(unless isearch-forward
	  (setq flist (reverse flist)))
	(while (not (string= (car flist) cb))
	  (setq flist (cdr flist)))
	(setq flist (cdr flist)))
      (when flist
	(find-file-noselect (car flist))))))

;;;###autoload
(defun reftex-isearch-minor-mode (&optional arg)
  "When on, isearch searches the whole document, not only the current file.
This minor mode allows isearch to search through all the files of
the current TeX document.

With no argument, this command toggles
`reftex-isearch-minor-mode'.  With a prefix argument ARG, turn
`reftex-isearch-minor-mode' on if ARG is positive, otherwise turn it off."
  (interactive "P")
  (let ((old-reftex-isearch-minor-mode reftex-isearch-minor-mode))
    (setq reftex-isearch-minor-mode
	  (not (or (and (null arg) reftex-isearch-minor-mode)
		   (<= (prefix-numeric-value arg) 0))))
    (unless (eq reftex-isearch-minor-mode old-reftex-isearch-minor-mode)
      (if reftex-isearch-minor-mode
	  (progn
	    (dolist (crt-buf (buffer-list))
	      (with-current-buffer crt-buf
		(when reftex-mode
		  (if (boundp 'multi-isearch-next-buffer-function)
		      (set (make-local-variable 'multi-isearch-next-buffer-function)
			   'reftex-isearch-switch-to-next-file)
		    (set (make-local-variable 'isearch-wrap-function)
			 'reftex-isearch-wrap-function)
		    (set (make-local-variable 'isearch-search-fun-function)
			 (lambda () 'reftex-isearch-isearch-search))
		    (set (make-local-variable 'isearch-push-state-function)
			 'reftex-isearch-push-state-function)
		    (set (make-local-variable 'isearch-next-buffer-function)
			 'reftex-isearch-switch-to-next-file))
		  (setq reftex-isearch-minor-mode t))))
	    (add-hook 'reftex-mode-hook 'reftex-isearch-minor-mode))
	(dolist (crt-buf (buffer-list))
	  (with-current-buffer crt-buf
	    (when reftex-mode
	      (if (boundp 'multi-isearch-next-buffer-function)
		  (kill-local-variable 'multi-isearch-next-buffer-function)
		(kill-local-variable 'isearch-wrap-function)
		(kill-local-variable 'isearch-search-fun-function)
		(kill-local-variable 'isearch-push-state-function)
		(kill-local-variable 'isearch-next-buffer-function))
	      (setq reftex-isearch-minor-mode nil))))
	(remove-hook 'reftex-mode-hook 'reftex-isearch-minor-mode)))
    ;; Force modeline redisplay.
    (set-buffer-modified-p (buffer-modified-p))))

(add-minor-mode 'reftex-isearch-minor-mode "/I" nil nil
		'reftex-isearch-minor-mode)

;;; reftex-global.el ends here
