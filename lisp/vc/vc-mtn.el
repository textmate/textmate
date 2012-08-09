;;; vc-mtn.el --- VC backend for Monotone

;; Copyright (C) 2007-2012  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: vc
;; Package: vc

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

;;; TODO:

;; - The `previous-version' VC method needs to be supported, 'D' in
;;   log-view-mode uses it.

;;; Code:

(eval-when-compile (require 'cl) (require 'vc))

(defgroup vc-mtn nil
  "VC Monotone (mtn) backend."
  :version "24.1"
  :group 'vc)

(defcustom vc-mtn-diff-switches t
  "String or list of strings specifying switches for monotone diff under VC.
If nil, use the value of `vc-diff-switches'.  If t, use no switches."
  :type '(choice (const :tag "Unspecified" nil)
		 (const :tag "None" t)
		 (string :tag "Argument String")
		 (repeat :tag "Argument List" :value ("") string))
  :version "23.1"
  :group 'vc-mtn)

(define-obsolete-variable-alias 'vc-mtn-command 'vc-mtn-program "23.1")
(defcustom vc-mtn-program "mtn"
  "Name of the monotone executable."
  :type 'string
  :group 'vc-mtn)

;; Clear up the cache to force vc-call to check again and discover
;; new functions when we reload this file.
(put 'Mtn 'vc-functions nil)

(unless (executable-find vc-mtn-program)
  ;; vc-mtn.el is 100% non-functional without the `mtn' executable.
  (setq vc-handled-backends (delq 'Mtn vc-handled-backends)))

;;;###autoload
(defconst vc-mtn-admin-dir "_MTN" "Name of the monotone directory.")
;;;###autoload
(defconst vc-mtn-admin-format (concat vc-mtn-admin-dir "/format")
  "Name of the monotone directory's format file.")

;;;###autoload (defun vc-mtn-registered (file)
;;;###autoload   (if (vc-find-root file vc-mtn-admin-format)
;;;###autoload       (progn
;;;###autoload         (load "vc-mtn")
;;;###autoload         (vc-mtn-registered file))))

(defun vc-mtn-revision-granularity () 'repository)
(defun vc-mtn-checkout-model (files) 'implicit)

(defun vc-mtn-root (file)
  (setq file (if (file-directory-p file)
                 (file-name-as-directory file)
               (file-name-directory file)))
  (or (vc-file-getprop file 'vc-mtn-root)
      (vc-file-setprop file 'vc-mtn-root
                       (vc-find-root file vc-mtn-admin-format))))


(defun vc-mtn-registered (file)
  (let ((root (vc-mtn-root file)))
    (when root
      (vc-mtn-state file))))

(defun vc-mtn-command (buffer okstatus files &rest flags)
  "A wrapper around `vc-do-command' for use in vc-mtn.el."
  (let ((process-environment
         ;; Avoid localization of messages so we can parse the output.
         (cons "LC_MESSAGES=C" process-environment)))
    (apply 'vc-do-command (or buffer "*vc*") okstatus vc-mtn-program
           files flags)))

(defun vc-mtn-state (file)
  ;; If `mtn' fails or returns status>0, or if the search files, just
  ;; return nil.
  (ignore-errors
    (with-temp-buffer
      (vc-mtn-command t 0 file "status")
      (goto-char (point-min))
      (re-search-forward
       "^  \\(?:\\(patched\\)\\|\\(added\\) \\(?:.*\\)\\)\\|no changes$")
      (cond  ((match-end 1) 'edited)
	     ((match-end 2) 'added)
	     (t 'up-to-date)))))

(defun vc-mtn-after-dir-status (update-function)
  (let (result)
    (goto-char (point-min))
    (re-search-forward "\\(?:Current b\\|B\\)ranch:  *\\(.*\\)\n?\nChanges against parent \\(.*\\)" nil t)
    (while (re-search-forward
	    "^  \\(?:\\(patched  \\)\\|\\(added    \\)\\)\\(.*\\)$" nil t)
      (cond  ((match-end 1) (push (list (match-string 3) 'edited) result))
	     ((match-end 2) (push (list (match-string 3) 'added) result))))
    (funcall update-function result)))

(defun vc-mtn-dir-status (dir update-function)
  (vc-mtn-command (current-buffer) 'async dir "status")
  (vc-exec-after
   `(vc-mtn-after-dir-status (quote ,update-function))))

(defun vc-mtn-working-revision (file)
  ;; If `mtn' fails or returns status>0, or if the search fails, just
  ;; return nil.
  (ignore-errors
    (with-temp-buffer
      (vc-mtn-command t 0 file "status")
      (goto-char (point-min))
      (re-search-forward "\\(?:Current b\\|B\\)ranch:  *\\(.*\\)\n?\nChanges against parent \\(.*\\)")
      (match-string 2))))

(defun vc-mtn-workfile-branch (file)
  ;; If `mtn' fails or returns status>0, or if the search files, just
  ;; return nil.
  (ignore-errors
    (with-temp-buffer
      (vc-mtn-command t 0 file "status")
      (goto-char (point-min))
      (re-search-forward "\\(?:Current b\\|B\\)ranch:  *\\(.*\\)\n?\nChanges against parent \\(.*\\)")
      (match-string 1))))

(defun vc-mtn-workfile-unchanged-p (file)
  (not (eq (vc-mtn-state file) 'edited)))

;; Mode-line rewrite code copied from vc-arch.el.

(defcustom vc-mtn-mode-line-rewrite
  '(("\\`[^:/#]*[:/#]" . ""))           ;Drop the host part.
  "Rewrite rules to shorten Mtn's revision names on the mode-line."
  :type '(repeat (cons regexp string))
  :version "22.2"
  :group 'vc-mtn)

(defun vc-mtn-mode-line-string (file)
  "Return string for placement in modeline by `vc-mode-line' for FILE."
  (let ((branch (vc-mtn-workfile-branch file)))
    (dolist (rule vc-mtn-mode-line-rewrite)
      (if (string-match (car rule) branch)
	  (setq branch (replace-match (cdr rule) t nil branch))))
    (format "Mtn%c%s"
	    (case (vc-state file)
	      ((up-to-date needs-update) ?-)
	      (added ?@)
	      (t ?:))
	    branch)))

(defun vc-mtn-register (files &optional rev comment)
  (vc-mtn-command nil 0 files "add"))

(defun vc-mtn-responsible-p (file) (vc-mtn-root file))
(defun vc-mtn-could-register (file) (vc-mtn-root file))

(declare-function log-edit-extract-headers "log-edit" (headers string))

(defun vc-mtn-checkin (files rev comment)
  (apply 'vc-mtn-command nil 0 files
	 (nconc (list "commit" "-m")
		(log-edit-extract-headers '(("Author" . "--author")
					    ("Date" . "--date"))
					  comment))))

(defun vc-mtn-find-revision (file rev buffer)
  (vc-mtn-command buffer 0 file "cat" "-r" rev))

;; (defun vc-mtn-checkout (file &optional editable rev)
;;   )

(defun vc-mtn-revert (file &optional contents-done)
  (unless contents-done
    (vc-mtn-command nil 0 file "revert")))

;; (defun vc-mtn-rollback (files)
;;   )

(defun vc-mtn-print-log (files buffer &optional shortlog start-revision limit)
  (apply 'vc-mtn-command buffer 0 files "log"
	 (append
	  (when start-revision (list "--from" (format "%s" start-revision)))
	  (when limit (list "--last" (format "%s" limit))))))

(defvar log-view-message-re)
(defvar log-view-file-re)
(defvar log-view-font-lock-keywords)
(defvar log-view-per-file-logs)

(define-derived-mode vc-mtn-log-view-mode log-view-mode "Mtn-Log-View"
  ;; Don't match anything.
  (set (make-local-variable 'log-view-file-re) "\\`a\\`")
  (set (make-local-variable 'log-view-per-file-logs) nil)
  ;; TODO: Use a more precise regexp than "[ |/]+" to avoid false positives
  ;; in the ChangeLog text.
  (set (make-local-variable 'log-view-message-re)
       "^[ |/]+Revision: \\([0-9a-f]+\\)")
  (require 'add-log)                    ;For change-log faces.
  (set (make-local-variable 'log-view-font-lock-keywords)
       (append log-view-font-lock-keywords
               '(("^[ |]+Author: \\(.*\\)" (1 'change-log-email))
                 ("^[ |]+Date: \\(.*\\)" (1 'change-log-date-face))))))

;; (defun vc-mtn-show-log-entry (revision)
;;   )

(defun vc-mtn-diff (files &optional rev1 rev2 buffer)
  "Get a difference report using monotone between two revisions of FILES."
  (apply 'vc-mtn-command (or buffer "*vc-diff*") 1 files "diff"
         (append
           (vc-switches 'mtn 'diff)
           (if rev1 (list "-r" rev1)) (if rev2 (list "-r" rev2)))))

(defun vc-mtn-annotate-command (file buf &optional rev)
  (apply 'vc-mtn-command buf 'async file "annotate"
         (if rev (list "-r" rev))))

(declare-function vc-annotate-convert-time "vc-annotate" (time))

(defconst vc-mtn-annotate-full-re
  "^ *\\([0-9a-f]+\\)\\.* by [^ ]+ \\([0-9]+\\)-\\([0-9]+\\)-\\([0-9]+\\): ")
(defconst vc-mtn-annotate-any-re
  (concat "^\\(?: +: \\|" vc-mtn-annotate-full-re "\\)"))

(defun vc-mtn-annotate-time ()
  (when (looking-at vc-mtn-annotate-any-re)
    (goto-char (match-end 0))
    (let ((year (match-string 2)))
      (if (not year)
          ;; Look for the date on a previous line.
          (save-excursion
            (get-text-property (1- (previous-single-property-change
                                    (point) 'vc-mtn-time nil (point-min)))
                               'vc-mtn-time))
        (let ((time (vc-annotate-convert-time
                     (encode-time 0 0 0
                                  (string-to-number (match-string 4))
                                  (string-to-number (match-string 3))
                                  (string-to-number year)
                                  t))))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (put-text-property (match-beginning 0) (match-end 0)
                               'vc-mtn-time time))
          time)))))

(defun vc-mtn-annotate-extract-revision-at-line ()
  (save-excursion
    (when (or (looking-at vc-mtn-annotate-full-re)
              (re-search-backward vc-mtn-annotate-full-re nil t))
      (match-string 1))))

;;; Revision completion.

(defun vc-mtn-list-tags ()
  (with-temp-buffer
    (vc-mtn-command t 0 nil "list" "tags")
    (goto-char (point-min))
    (let ((tags ()))
      (while (re-search-forward "^[^ ]+" nil t)
        (push (match-string 0) tags))
      tags)))

(defun vc-mtn-list-branches ()
  (with-temp-buffer
    (vc-mtn-command t 0 nil "list" "branches")
    (goto-char (point-min))
    (let ((branches ()))
      (while (re-search-forward "^.+" nil t)
        (push (match-string 0) branches))
      branches)))

(defun vc-mtn-list-revision-ids (prefix)
  (with-temp-buffer
    (vc-mtn-command t 0 nil "complete" "revision" prefix)
    (goto-char (point-min))
    (let ((ids ()))
      (while (re-search-forward "^.+" nil t)
        (push (match-string 0) ids))
      ids)))

(defun vc-mtn-revision-completion-table (files)
  ;; TODO: Implement completion for selectors
  ;; TODO: Implement completion for composite selectors.
  (lexical-let ((files files))
    ;; What about using `files'?!?  --Stef
    (lambda (string pred action)
      (cond
       ;; "Tag" selectors.
       ((string-match "\\`t:" string)
        (complete-with-action action
                              (mapcar (lambda (tag) (concat "t:" tag))
                                      (vc-mtn-list-tags))
                              string pred))
       ;; "Branch" selectors.
       ((string-match "\\`b:" string)
        (complete-with-action action
                              (mapcar (lambda (tag) (concat "b:" tag))
                                      (vc-mtn-list-branches))
                              string pred))
       ;; "Head" selectors.  Not sure how they differ from "branch" selectors.
       ((string-match "\\`h:" string)
        (complete-with-action action
                              (mapcar (lambda (tag) (concat "h:" tag))
                                      (vc-mtn-list-branches))
                              string pred))
       ;; "ID" selectors.
       ((string-match "\\`i:" string)
        (complete-with-action action
                              (mapcar (lambda (tag) (concat "i:" tag))
                                      (vc-mtn-list-revision-ids
                                       (substring string (match-end 0))))
                              string pred))
       (t
        (complete-with-action action
                              '("t:" "b:" "h:" "i:"
                                ;; Completion not implemented for these.
                                "a:" "c:" "d:" "e:" "l:")
                              string pred))))))



(provide 'vc-mtn)

;;; vc-mtn.el ends here
