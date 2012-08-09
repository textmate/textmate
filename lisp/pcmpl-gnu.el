;;; pcmpl-gnu.el --- completions for GNU project tools -*- lexical-binding: t -*-

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Package: pcomplete

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

(provide 'pcmpl-gnu)

(require 'pcomplete)
(require 'pcmpl-unix)

(defgroup pcmpl-gnu nil
  "Completions for GNU project tools."
  :group 'pcomplete)

;; User Variables:

(defcustom pcmpl-gnu-makefile-regexps
  '("\\`GNUmakefile" "\\`[Mm]akefile" "\\.ma?k\\'")
  "A list of regexps that will match Makefile names."
  :type '(repeat regexp)
  :group 'pcmpl-gnu)

;; Functions:

;;;###autoload
(defun pcomplete/gzip ()
  "Completion for `gzip'."
  (let ((pcomplete-help "(gzip)"))
    (pcomplete-opt "cdfhlLnNqrStvV123456789")
    (while (pcomplete-here
	    (pcmpl-gnu-zipped-files
	     (catch 'has-d-flag
	       (let ((args pcomplete-args))
		 (while args
		   (if (string-match "\\`-.*[dt]" (car args))
		       (throw 'has-d-flag t))
		   (setq args (cdr args))))))))))

(defun pcmpl-gnu-zipped-files (unzip-p)
  "Find all zipped or unzipped files: the inverse of UNZIP-P."
  (pcomplete-entries
   nil
   (function
    (lambda (entry)
      (when (and (file-readable-p entry)
		 (file-regular-p entry))
	(let ((zipped (string-match "\\.\\(t?gz\\|\\(ta\\)?Z\\)\\'"
				    entry)))
	  (or (and unzip-p zipped)
	      (and (not unzip-p) (not zipped)))))))))

;;;###autoload
(defun pcomplete/bzip2 ()
  "Completion for `bzip2'."
  (pcomplete-opt "hdzkftcqvLVs123456789")
  (while (pcomplete-here
	  (pcmpl-gnu-bzipped-files
	   (catch 'has-d-flag
	     (let ((args pcomplete-args))
	       (while args
		 (if (string-match "\\`-.*[dt]" (car args))
		     (throw 'has-d-flag t))
		 (setq args (cdr args)))))))))

(defun pcmpl-gnu-bzipped-files (unzip-p)
  "Find all zipped or unzipped files: the inverse of UNZIP-P."
  (pcomplete-entries
   nil
   (function
    (lambda (entry)
      (when (and (file-readable-p entry)
		 (file-regular-p entry))
	(let ((zipped (string-match "\\.\\(t?z2\\|bz2\\)\\'" entry)))
	  (or (and unzip-p zipped)
	      (and (not unzip-p) (not zipped)))))))))

;;;###autoload
(defun pcomplete/make ()
  "Completion for GNU `make'."
  (let ((pcomplete-help "(make)Top"))
    (pcomplete-opt "bmC/def(pcmpl-gnu-makefile-names)hiI/j?kl?no.pqrsStvwW.")
    (while (pcomplete-here (completion-table-in-turn
                            (pcmpl-gnu-make-rule-names)
                            (pcomplete-entries))
                           nil 'identity))))

(defun pcmpl-gnu-makefile-names ()
  "Return a list of possible makefile names."
  (pcomplete-entries (mapconcat 'identity pcmpl-gnu-makefile-regexps "\\|")))

(defun pcmpl-gnu-make-rule-names ()
  "Return a list of possible make rule names in MAKEFILE."
  (let* ((minus-f (member "-f" pcomplete-args))
	 (makefile (or (cadr minus-f)
		       (cond
                        ((file-exists-p "GNUmakefile") "GNUmakefile")
                        ((file-exists-p "makefile") "makefile")
                        (t "Makefile"))))
	 rules)
    (if (not (file-readable-p makefile))
	(unless minus-f (list "-f"))
      (with-temp-buffer
	(ignore-errors			;Could be a directory or something.
	  (insert-file-contents makefile))
	(while (re-search-forward
		(concat "^\\s-*\\([^\n#%.$][^:=\n]*\\)\\s-*:[^=]") nil t)
	  (setq rules (append (split-string (match-string 1)) rules))))
      (pcomplete-uniqify-list rules))))

(defcustom pcmpl-gnu-tarfile-regexp
  "\\.t\\(ar\\(\\.\\(gz\\|bz2\\|Z\\)\\)?\\|gz\\|a[zZ]\\|z2\\)\\'"
  "A regexp which matches any tar archive."
  :type 'regexp
  :group 'pcmpl-gnu)

;; Only used in tar-mode buffers.
(defvar tar-parse-info)
(declare-function tar-header-name "tar-mode" t t)

(defmacro pcmpl-gnu-with-file-buffer (file &rest body)
  "Run BODY inside a buffer visiting FILE."
  (declare (debug t) (indent 1))
  (let ((exist (make-symbol "exist"))
        (filesym (make-symbol "file"))
        (buf (make-symbol "buf")))
    `(let* ((,filesym ,file)
            (,exist (find-buffer-visiting ,filesym))
            (,buf (or ,exist (find-file-noselect ,filesym))))
       (unwind-protect
           (with-current-buffer ,buf
             ,@body)
         (when (and (not ,exist) (buffer-live-p ,buf))
           (kill-buffer ,buf))))))

;;;###autoload
(defun pcomplete/tar ()
  "Completion for the GNU tar utility."
  ;; options that end in an equal sign will want further completion...
  (let (saw-option complete-within)
    (let ((pcomplete-suffix-list (cons ?= pcomplete-suffix-list)))
      (while (pcomplete-match "^-" 0)
        (setq saw-option t)
        (if (pcomplete-match "^--" 0)
            (if (pcomplete-match "^--\\([^= \t\n\f]*\\)\\'" 0)
                ;; FIXME: Extract this list from "tar --help".
                (pcomplete-here*
                 '("--absolute-names"
                   "--after-date="
                   "--append"
                   "--atime-preserve"
                   "--backup"
                   "--block-number"
                   "--blocking-factor="
                   "--catenate"
                   "--checkpoint"
                   "--compare"
                   "--compress"
                   "--concatenate"
                   "--confirmation"
                   "--create"
                   "--delete"
                   "--dereference"
                   "--diff"
                   "--directory="
                   "--exclude="
                   "--exclude-from="
                   "--extract"
                   "--file="
                   "--files-from="
                   "--force-local"
                   "--get"
                   "--group="
                   "--gzip"
                   "--help"
                   "--ignore-failed-read"
                   "--ignore-zeros"
                   "--incremental"
                   "--info-script="
                   "--interactive"
                   "--keep-old-files"
                   "--label="
                   "--list"
                   "--listed-incremental"
                   "--mode="
                   "--modification-time"
                   "--multi-volume"
                   "--new-volume-script="
                   "--newer="
                   "--newer-mtime"
                   "--no-recursion"
                   "--null"
                   "--numeric-owner"
                   "--old-archive"
                   "--one-file-system"
                   "--owner="
                   "--portability"
                   "--posix"
                   "--preserve"
                   "--preserve-order"
                   "--preserve-permissions"
                   "--read-full-records"
                   "--record-size="
                   "--recursive-unlink"
                   "--remove-files"
                   "--rsh-command="
                   "--same-order"
                   "--same-owner"
                   "--same-permissions"
                   "--sparse"
                   "--starting-file="
                   "--suffix="
                   "--tape-length="
                   "--to-stdout"
                   "--totals"
                   "--uncompress"
                   "--ungzip"
                   "--unlink-first"
                   "--update"
                   "--use-compress-program="
                   "--verbose"
                   "--verify"
                   "--version"
                   "--volno-file=")))
          (pcomplete-opt "01234567ABCFGKLMNOPRSTUVWXZbcdfghiklmoprstuvwxz"))
        (cond
         ((pcomplete-match "\\`-\\'" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--after-date=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--backup=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--blocking-factor=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--directory=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-dirs)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--exclude-from=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-entries)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--exclude=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--\\(extract\\|list\\)\\'" 0)
          (setq complete-within t))
         ((pcomplete-match "\\`--file=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-dirs-or-entries pcmpl-gnu-tarfile-regexp)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--files-from=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-entries)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--group=\\(.*\\)" 0)
          (pcomplete-here* (pcmpl-unix-group-names)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--info-script=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-entries)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--label=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--mode=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--new-volume-script=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-entries)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--newer=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--owner=\\(.*\\)" 0)
          (pcomplete-here* (pcmpl-unix-user-names)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--record-size=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--rsh-command=\\(.*\\)" 0)
          (pcomplete-here* (funcall pcomplete-command-completion-function)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--starting-file=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-entries)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--suffix=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--tape-length=" 0)
          (pcomplete-here*))
         ((pcomplete-match "\\`--use-compress-program=\\(.*\\)" 0)
          (pcomplete-here* (funcall pcomplete-command-completion-function)
                           (pcomplete-match-string 1 0)))
         ((pcomplete-match "\\`--volno-file=\\(.*\\)" 0)
          (pcomplete-here* (pcomplete-entries)
                           (pcomplete-match-string 1 0))))))
    (unless saw-option
      (pcomplete-here
       (mapcar 'char-to-string
	       (string-to-list
		"01234567ABCFGIKLMNOPRSTUVWXZbcdfghiklmoprstuvwxz")))
      (if (pcomplete-match "[xt]" 'first 1)
	  (setq complete-within t)))
    (pcomplete-here (pcomplete-dirs-or-entries pcmpl-gnu-tarfile-regexp))
    (while (pcomplete-here
	    (if (and complete-within
                     (let* ((fa (file-attributes (pcomplete-arg 1)))
                            (size (nth 7 fa)))
                       (and (numberp size)
                            (or (null large-file-warning-threshold)
                                (< size large-file-warning-threshold)))))
                (let ((file (pcomplete-arg 1)))
                  (completion-table-dynamic
                   (lambda (_string)
                     (pcmpl-gnu-with-file-buffer file
                       (mapcar #'tar-header-name tar-parse-info)))))
	      (pcomplete-entries))
	    nil 'identity))))

;;;###autoload
(defalias 'pcomplete/gdb 'pcomplete/xargs)

;;; pcmpl-gnu.el ends here
