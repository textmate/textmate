;;; gnus-agent.el --- Legacy unplugged support for Gnus

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Kevin Greiner <kgreiner@xpediantsolutions.com>
;; Keywords: news

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

;; Conversion functions for the Agent.

;;; Code:
(require 'gnus-start)
(require 'gnus-util)
(require 'gnus-range)
(require 'gnus-agent)

;; Oort Gnus v0.08 - This release updated agent to no longer use
;;                   history file and to support a compressed alist.

(defvar gnus-agent-compressed-agentview-search-only nil)

(defun gnus-agent-convert-to-compressed-agentview (converting-to)
  "Iterates over all agentview files to ensure that they have been
converted to the compressed format."

  (let ((search-in (list gnus-agent-directory))
        here
        members
        member
        converted-something)
    (while (setq here (pop search-in))
      (setq members (directory-files here t))
      (while (setq member (pop members))
        (cond ((string-match "/\\.\\.?$" member)
	       nil)
	      ((file-directory-p member)
	       (push member search-in))
              ((equal (file-name-nondirectory member) ".agentview")
               (setq converted-something
                     (or (gnus-agent-convert-agentview member)
                         converted-something))))))

    (if converted-something
        (gnus-message 4 "Successfully converted Gnus %s offline (agent) files to %s" gnus-newsrc-file-version converting-to))))

(defun gnus-agent-convert-to-compressed-agentview-prompt ()
  (catch 'found-file-to-convert
    (let ((gnus-agent-compressed-agentview-search-only t))
      (gnus-agent-convert-to-compressed-agentview nil))))

(gnus-convert-mark-converter-prompt 'gnus-agent-convert-to-compressed-agentview 'gnus-agent-convert-to-compressed-agentview-prompt)

(defun gnus-agent-convert-agentview (file)
  "Load FILE and do a `read' there."
  (with-temp-buffer
      (nnheader-insert-file-contents file)
      (goto-char (point-min))
      (let ((inhibit-quit t)
            (alist (read (current-buffer)))
            (version (condition-case nil (read (current-buffer))
                       (end-of-file 0)))
            changed-version
            history-file)

        (cond
	 ((= version 0)
	  (let (entry
                (gnus-command-method nil))
            (mm-disable-multibyte) ;; everything is binary
            (erase-buffer)
            (insert "\n")
            (let ((file (concat (file-name-directory file) "/history")))
              (when (file-exists-p file)
                (nnheader-insert-file-contents file)
                (setq history-file file)))

	    (goto-char (point-min))
	    (while (not (eobp))
	      (if (and (looking-at
			"[^\t\n]+\t\\([0-9]+\\)\t\\([^ \n]+\\) \\([0-9]+\\)")
		       (string= (gnus-agent-article-name ".agentview" (match-string 2))
				file)
		       (setq entry (assoc (string-to-number (match-string 3)) alist)))
		  (setcdr entry (string-to-number (match-string 1))))
	      (forward-line 1))
	    (setq changed-version t)))
	 ((= version 1)
	  (setq changed-version t)))

        (when changed-version
	  (when gnus-agent-compressed-agentview-search-only
	    (throw 'found-file-to-convert t))

          (erase-buffer)
          (let (article-id day-of-download comp-list compressed)
	    (while alist
	      (setq article-id (caar alist)
		    day-of-download (cdar alist)
		    comp-list (assq day-of-download compressed)
		    alist (cdr alist))
	      (if comp-list
		  (setcdr comp-list (cons article-id (cdr comp-list)))
		(push (list day-of-download article-id) compressed)))
	    (setq alist compressed)
	    (while alist
	      (setq comp-list (pop alist))
	      (setcdr comp-list
		      (gnus-compress-sequence (nreverse (cdr comp-list)))))
            (princ compressed (current-buffer)))
          (insert "\n2\n")
          (write-file file)
          (when history-file
            (delete-file history-file))
          t))))

;; End of Oort Gnus v0.08 updates

;; No Gnus v0.3 - This release provides a mechanism for upgrading gnus
;;                from previous versions.  Therefore, the previous
;;                hacks to handle a gnus-agent-expire-days that
;;                specifies a list of values can be removed.

(defun gnus-agent-unlist-expire-days (converting-to)
  (when (listp gnus-agent-expire-days)
    (let (buffer)
      (unwind-protect
          (save-window-excursion
            (setq buffer (gnus-get-buffer-create " *Gnus agent upgrade*"))
            (set-buffer buffer)
            (erase-buffer)
            (insert "The definition of gnus-agent-expire-days has been changed.\nYou currently have it set to the list:\n  ")
            (gnus-pp gnus-agent-expire-days)

            (insert "\nIn order to use version '" converting-to "' of gnus, you will need to set\n")
            (insert "gnus-agent-expire-days to an integer. If you still wish to set different\n")
            (insert "expiration days to individual groups, you must instead set the\n")
            (insert "'agent-days-until-old group and/or topic parameter.\n")
            (insert "\n")
            (insert "If you would like, gnus can iterate over every group comparing its name to the\n")
            (insert "regular expressions that you currently have in gnus-agent-expire-days.  When\n")
            (insert "gnus finds a match, it will update that group's 'agent-days-until-old group\n")
            (insert "parameter to the value associated with the regular expression.\n")
            (insert "\n")
            (insert "Whether gnus assigns group parameters, or not, gnus will terminate with an\n")
            (insert "ERROR as soon as this function completes.  The reason is that you must\n")
            (insert "manually edit your configuration to either not set gnus-agent-expire-days or\n")
            (insert "to set it to an integer before gnus can be used.\n")
            (insert "\n")
            (insert "Once you have successfully edited gnus-agent-expire-days, gnus will be able to\n")
            (insert "execute past this function.\n")
            (insert "\n")
            (insert "Should gnus use gnus-agent-expire-days to assign\n")
            (insert "agent-days-until-old parameters to individual groups? (Y/N)")

            (switch-to-buffer buffer)
            (beep)
            (beep)

            (let ((echo-keystrokes 0)
                  c)
              (while (progn (setq c (read-char-exclusive))
                            (cond ((or (eq c ?y) (eq c ?Y))
                                         (save-excursion
                                           (let ((groups (gnus-group-listed-groups)))
                                             (while groups
                                               (let* ((group (pop groups))
                                                      (days gnus-agent-expire-days)
                                                      (day (catch 'found
                                                             (while days
                                                               (when (eq 0 (string-match
                                                                            (caar days)
                                                                            group))
                                                                 (throw 'found (cadr (car days))))
                                                               (setq days (cdr days)))
                                                             nil)))
                                                 (when day
                                                   (gnus-group-set-parameter group 'agent-days-until-old
                                                                             day))))))
                                   nil
                                   )
                                  ((or (eq c ?n) (eq c ?N))
                                   nil)
                                  (t
                                   t))))))
        (kill-buffer buffer))
      (error "Change gnus-agent-expire-days to an integer for gnus to start"))))

;; The gnus-agent-unlist-expire-days has its own conversion prompt.
;; Therefore, hide the default prompt.
(gnus-convert-mark-converter-prompt 'gnus-agent-unlist-expire-days t)

(defun gnus-agent-unhook-expire-days (converting-to)
  "Remove every lambda from gnus-group-prepare-hook that mention the
symbol gnus-agent-do-once in their definition.  This should NOT be
necessary as gnus-agent.el no longer adds them.  However, it is
possible that the hook was persistently saved."
    (let ((h t)) ; iterate from bgn of hook
      (while h
        (let ((func (progn (when (eq h t)
                             ;; init h to list of functions
                             (setq h (cond ((listp gnus-group-prepare-hook)
                                            gnus-group-prepare-hook)
                                           ((boundp 'gnus-group-prepare-hook)
                                            (list gnus-group-prepare-hook)))))
                           (pop h))))

          (when (cond ((eq (type-of func) 'compiled-function)
                       ;; Search def. of compiled function for gnus-agent-do-once string
                       (let* (definition
                               print-level
                               print-length
                               (standard-output
                                (lambda (char)
                                  (setq definition (cons char definition)))))
                         (princ func) ; populates definition with reversed list of characters
                         (let* ((i (length definition))
                                (s (make-string i 0)))
                           (while definition
                             (aset s (setq i (1- i)) (pop definition)))

                           (string-match "\\bgnus-agent-do-once\\b" s))))
                      ((listp func)
                       (eq (cadr (nth 2 func)) 'gnus-agent-do-once) ; handles eval'd lambda
                       ))

            (remove-hook 'gnus-group-prepare-hook func)
            ;; I don't what remove-hook is going to actually do to the
            ;; hook list so start over from the beginning.
            (setq h t))))))

;; gnus-agent-unhook-expire-days is safe in that it does not modify
;; the .newsrc.eld file.
(gnus-convert-mark-converter-prompt 'gnus-agent-unhook-expire-days t)

(provide 'legacy-gnus-agent)

;;; legacy-gnus-agent.el ends here
