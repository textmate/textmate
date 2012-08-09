;;; pcmpl-cvs.el --- functions for dealing with cvs completions

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
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

;; These functions provide completion rules for the `cvs' tool.

;;; Code:

(provide 'pcmpl-cvs)

(require 'pcomplete)
(require 'executable)

(defgroup pcmpl-cvs nil
  "Functions for dealing with CVS completions."
  :group 'pcomplete)

;; User Variables:

(defcustom pcmpl-cvs-binary (or (executable-find "cvs") "cvs")
  "The full path of the 'cvs' binary."
  :type 'file
  :group 'pcmpl-cvs)

;; Functions:

;;;###autoload
(defun pcomplete/cvs ()
  "Completion rules for the `cvs' command."
  (let ((pcomplete-help "(cvs)Invoking CVS"))
    (pcomplete-opt "HQqrwlntvfab/T/e*d/z?s")
    (pcomplete-here* (pcmpl-cvs-commands))
    (cond ((pcomplete-test "add")
	   (setq pcomplete-help "(cvs)Adding files")
	   (pcomplete-opt "k?m?")
	   (while (pcomplete-here (pcmpl-cvs-entries '(??)))))
	  ((pcomplete-test "remove")
	   (setq pcomplete-help "(cvs)Removing files")
	   (pcomplete-opt "flR")
	   (while (pcomplete-here (pcmpl-cvs-entries '(?U)))))
	  ((pcomplete-test "init")
	   (setq pcomplete-help "(cvs)Creating a repository"))
	  ((pcomplete-test '("login" "logout"))
	   (setq pcomplete-help "(cvs)Password authentication client"))
	  ((pcomplete-test "import")
	   (setq pcomplete-help "(cvs)import")
	   (pcomplete-opt "dk?I(pcmpl-cvs-entries '(??))b?m?W?"))
	  ((pcomplete-test "checkout")
	   (setq pcomplete-help "(cvs)checkout")
	   (pcomplete-opt "ANPRcflnpsr?D?d/k?j?")
	   (pcomplete-here (pcmpl-cvs-modules)))
	  ((pcomplete-test "rtag")
	   (setq pcomplete-help "(cvs)Creating a branch")
	   (pcomplete-opt "aflRndbr?DF")
	   (pcomplete-here (pcmpl-cvs-modules)))
	  ((pcomplete-test "release")
	   (setq pcomplete-help "(cvs)release")
	   (pcomplete-opt "d")
	   (while (pcomplete-here (pcomplete-dirs))))
	  ((pcomplete-test "export")
	   (setq pcomplete-help "(cvs)export")
	   (pcomplete-opt "NflRnr?D?d/k?")
	   (pcomplete-here (pcmpl-cvs-modules)))
	  ((pcomplete-test "commit")
	   (setq pcomplete-help "(cvs)commit files")
	   (pcomplete-opt "nRlfF.m?r(pcmpl-cvs-tags '(?M ?R ?A))")
	   (while (pcomplete-here (pcmpl-cvs-entries '(?M ?R ?A)))))
	  ((pcomplete-test "diff")
	   (setq pcomplete-help "(cvs)Viewing differences")
	   (let ((opt-index pcomplete-index)
		 saw-backdate)
	     (pcomplete-opt "lRD?Nr(pcmpl-cvs-tags)")
	     (while (< opt-index pcomplete-index)
	       (if (pcomplete-match "^-[Dr]" (- pcomplete-index opt-index))
		   (setq saw-backdate t opt-index pcomplete-index)
		 (setq opt-index (1+ opt-index))))
	     (while (pcomplete-here
		     (pcmpl-cvs-entries (unless saw-backdate '(?M)))))))
	  ((pcomplete-test "unedit")
	   (setq pcomplete-help "(cvs)Editing files")
	   (pcomplete-opt "lR")
	   (while (pcomplete-here (pcmpl-cvs-entries '(?M ?R ?A)))))
	  ((pcomplete-test "update")
	   (setq pcomplete-help "(cvs)update")
	   (pcomplete-opt
	    (concat "APdflRpk?r(pcmpl-cvs-tags '(?U ?P))D?"
		    "j(pcmpl-cvs-tags '(?U ?P))"
		    "I(pcmpl-cvs-entries '(??))W?"))
	   (while (pcomplete-here (pcmpl-cvs-entries '(?U ?P)))))
	  ((pcomplete-test "status")
	   (setq pcomplete-help "(cvs)File status")
	   (pcomplete-opt "vlR")
	   (while (pcomplete-here (pcmpl-cvs-entries))))
	  (t
	   (while (pcomplete-here (pcmpl-cvs-entries)))))))

(defun pcmpl-cvs-commands ()
  "Return a list of available CVS commands."
  (with-temp-buffer
    (call-process pcmpl-cvs-binary nil t nil "--help-commands")
    (goto-char (point-min))
    (let (cmds)
      (while (re-search-forward "^\\s-+\\([a-z]+\\)" nil t)
	(setq cmds (cons (match-string 1) cmds)))
      (pcomplete-uniqify-list cmds))))

(defun pcmpl-cvs-modules ()
  "Return a list of available modules under CVS."
  (with-temp-buffer
    (call-process pcmpl-cvs-binary nil t nil "checkout" "-c")
    (goto-char (point-min))
    (let (entries)
      (while (re-search-forward "\\(\\S-+\\)$" nil t)
	(setq entries (cons (match-string 1) entries)))
      (pcomplete-uniqify-list entries))))

(defun pcmpl-cvs-tags (&optional opers)
  "Return all the tags which could apply to the files related to OPERS."
  (let ((entries (pcmpl-cvs-entries opers))
	tags)
    (with-temp-buffer
      (apply 'call-process pcmpl-cvs-binary nil t nil
	     "status" "-v" entries)
      (goto-char (point-min))
      (while (re-search-forward "Existing Tags:" nil t)
	(forward-line)
	(while (not (looking-at "^$"))
	  (unless (looking-at "^\\s-+\\(\\S-+\\)\\s-+")
	    (error "Error in output from `cvs status -v'"))
	  (setq tags (cons (match-string 1) tags))
	  (forward-line))))
    (pcomplete-uniqify-list tags)))

(defun pcmpl-cvs-entries (&optional opers)
  "Return the Entries for the current directory.
If OPERS is a list of characters, return entries for which that
operation character applies, as displayed by 'cvs -n update'."
  (let* ((arg (pcomplete-arg))
	 (dir (file-name-as-directory
	       (or (file-name-directory arg) "")))
	 (nondir (or (file-name-nondirectory arg) ""))
	 entries)
    (if opers
	(with-temp-buffer
	  (and dir (cd dir))
	  (call-process pcmpl-cvs-binary nil t nil
			"-q" "-n" "-f" "update"); "-l")
	  (goto-char (point-min))
	  (while (re-search-forward "^\\(.\\) \\(.+\\)$" nil t)
	    (if (memq (string-to-char (match-string 1)) opers)
		(setq entries (cons (match-string 2) entries)))))
      (with-temp-buffer
	(insert-file-contents (concat dir "CVS/Entries"))
	(goto-char (point-min))
	(while (not (eobp))
	  ;; Normal file: /NAME   -> "" "NAME"
	  ;; Directory  : D/NAME  -> "D" "NAME"
	  (let* ((fields (split-string (buffer-substring
					(line-beginning-position)
					(line-end-position))
				       "/"))
		 (text (nth 1 fields)))
	    (when text
	      (if (string= (nth 0 fields) "D")
		  (setq text (file-name-as-directory text)))
	      (setq entries (cons text entries))))
	  (forward-line))))
    (setq pcomplete-stub nondir)
    (pcomplete-uniqify-list entries)))

;;; pcmpl-cvs.el ends here
