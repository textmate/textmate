;;; gnus-sync.el --- synchronization facility for Gnus

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Keywords: news synchronization nntp nnrss

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

;; This is the gnus-sync.el package.

;; It's due for a rewrite using gnus-after-set-mark-hook and
;; gnus-before-update-mark-hook, and my plan is to do this once No
;; Gnus development is done.  Until then please consider it
;; experimental.

;; Put this in your startup file (~/.gnus.el for instance)

;; possibilities for gnus-sync-backend:
;; Tramp over SSH: /ssh:user@host:/path/to/filename
;; Tramp over IMAP: /imaps:user@yourhosthere.com:/INBOX.test/filename
;; ...or any other file Tramp and Emacs can handle...

;; (setq gnus-sync-backend "/remote:/path.gpg" ; will use Tramp+EPA if loaded
;;       gnus-sync-global-vars `(gnus-newsrc-last-checked-date)
;;       gnus-sync-newsrc-groups `("nntp" "nnrss")
;;       gnus-sync-newsrc-offsets `(2 3))

;; TODO:

;; - after gnus-sync-read, the message counts are wrong.  So it's not
;;   run automatically, you have to call it with M-x gnus-sync-read

;; - use gnus-after-set-mark-hook and gnus-before-update-mark-hook to
;;   catch the mark updates

;;; Code:

(eval-when-compile (require 'cl))
(require 'gnus)
(require 'gnus-start)
(require 'gnus-util)

(defgroup gnus-sync nil
  "The Gnus synchronization facility."
  :version "24.1"
  :group 'gnus)

(defcustom gnus-sync-newsrc-groups `("nntp" "nnrss")
  "List of groups to be synchronized in the gnus-newsrc-alist.
The group names are matched, they don't have to be fully
qualified.  Typically you would choose all of these.  That's the
default because there is no active sync backend by default, so
this setting is harmless until the user chooses a sync backend."
  :group 'gnus-sync
  :type '(repeat regexp))

(defcustom gnus-sync-newsrc-offsets '(2 3)
  "List of per-group data to be synchronized."
  :group 'gnus-sync
  :type '(set (const :tag "Read ranges" 2)
              (const :tag "Marks" 3)))

(defcustom gnus-sync-global-vars nil
  "List of global variables to be synchronized.
You may want to sync `gnus-newsrc-last-checked-date' but pretty
much any symbol is fair game.  You could additionally sync
`gnus-newsrc-alist', `gnus-server-alist', `gnus-topic-topology',
and `gnus-topic-alist' to cover all the variables in
newsrc.eld (except for `gnus-format-specs' which should not be
synchronized, I believe).  Also see `gnus-variable-list'."
  :group 'gnus-sync
  :type '(repeat (choice (variable :tag "A known variable")
                         (symbol :tag "Any symbol"))))

(defcustom gnus-sync-backend nil
  "The synchronization backend."
  :group 'gnus-sync
  :type '(radio (const :format "None" nil)
                (string :tag "Sync to a file")))

(defvar gnus-sync-newsrc-loader nil
  "Carrier for newsrc data")

(defun gnus-sync-save ()
"Save the Gnus sync data to the backend."
  (interactive)
  (cond
   ((stringp gnus-sync-backend)
    (gnus-message 7 "gnus-sync: saving to backend %s" gnus-sync-backend)
    ;; populate gnus-sync-newsrc-loader from all but the first dummy
    ;; entry in gnus-newsrc-alist whose group matches any of the
    ;; gnus-sync-newsrc-groups
    ;; TODO: keep the old contents for groups we don't have!
    (let ((gnus-sync-newsrc-loader
           (loop for entry in (cdr gnus-newsrc-alist)
                 when (gnus-grep-in-list
                       (car entry)     ;the group name
                       gnus-sync-newsrc-groups)
                 collect (cons (car entry)
                               (mapcar (lambda (offset)
                                         (cons offset (nth offset entry)))
                                       gnus-sync-newsrc-offsets)))))
      (with-temp-file gnus-sync-backend
        (progn
          (let ((coding-system-for-write gnus-ding-file-coding-system)
                (standard-output (current-buffer)))
            (princ (format ";; -*- mode:emacs-lisp; coding: %s; -*-\n"
                           gnus-ding-file-coding-system))
            (princ ";; Gnus sync data v. 0.0.1\n")
            (let* ((print-quoted t)
                   (print-readably t)
                   (print-escape-multibyte nil)
                   (print-escape-nonascii t)
                   (print-length nil)
                   (print-level nil)
                   (print-circle nil)
                   (print-escape-newlines t)
                   (variables (cons 'gnus-sync-newsrc-loader
                                    gnus-sync-global-vars))
                   variable)
              (while variables
                (if (and (boundp (setq variable (pop variables)))
                           (symbol-value variable))
                    (progn
                      (princ "\n(setq ")
                      (princ (symbol-name variable))
                      (princ " '")
                      (prin1 (symbol-value variable))
                      (princ ")\n"))
                  (princ "\n;;; skipping empty variable ")
                  (princ (symbol-name variable)))))
            (gnus-message
             7
             "gnus-sync: stored variables %s and %d groups in %s"
             gnus-sync-global-vars
             (length gnus-sync-newsrc-loader)
             gnus-sync-backend)

            ;; Idea from Dan Christensen <jdc@chow.mat.jhu.edu>
            ;; Save the .eld file with extra line breaks.
            (gnus-message 8 "gnus-sync: adding whitespace to %s"
                          gnus-sync-backend)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "^(\\|(\\\"" nil t)
                (replace-match "\n\\&" t))
              (goto-char (point-min))
              (while (re-search-forward " $" nil t)
                (replace-match "" t t))))))))
    ;; the pass-through case: gnus-sync-backend is not a known choice
    (nil)))

(defun gnus-sync-read ()
"Load the Gnus sync data from the backend."
  (interactive)
  (when gnus-sync-backend
    (gnus-message 7 "gnus-sync: loading from backend %s" gnus-sync-backend)
    (cond ((stringp gnus-sync-backend)
           ;; read data here...
           (if (or debug-on-error debug-on-quit)
               (load gnus-sync-backend nil t)
             (condition-case var
                 (load gnus-sync-backend nil t)
               (error
                (error "Error in %s: %s" gnus-sync-backend (cadr var)))))
           (let ((valid-count 0)
                 invalid-groups)
             (dolist (node gnus-sync-newsrc-loader)
               (if (gnus-gethash (car node) gnus-newsrc-hashtb)
                   (progn
                     (incf valid-count)
                     (loop for store in (cdr node)
                           do (setf (nth (car store)
                                         (assoc (car node) gnus-newsrc-alist))
                              (cdr store))))
                 (push (car node) invalid-groups)))
            (gnus-message
             7
             "gnus-sync: loaded %d groups (out of %d) from %s"
             valid-count (length gnus-sync-newsrc-loader)
             gnus-sync-backend)
            (when invalid-groups
              (gnus-message
               7
               "gnus-sync: skipped %d groups (out of %d) from %s"
               (length invalid-groups)
               (length gnus-sync-newsrc-loader)
               gnus-sync-backend)
              (gnus-message 9 "gnus-sync: skipped groups: %s"
                            (mapconcat 'identity invalid-groups ", ")))))
          (nil))
    ;; make the hashtable again because the newsrc-alist may have been modified
    (when gnus-sync-newsrc-offsets
      (gnus-message 9 "gnus-sync: remaking the newsrc hashtable")
      (gnus-make-hashtable-from-newsrc-alist))))

;;;###autoload
(defun gnus-sync-initialize ()
"Initialize the Gnus sync facility."
  (interactive)
  (gnus-message 5 "Initializing the sync facility")
  (gnus-sync-install-hooks))

;;;###autoload
(defun gnus-sync-install-hooks ()
  "Install the sync hooks."
  (interactive)
  ;; (add-hook 'gnus-get-new-news-hook 'gnus-sync-read)
  ;; (add-hook 'gnus-read-newsrc-el-hook 'gnus-sync-read)
  (add-hook 'gnus-save-newsrc-hook 'gnus-sync-save))

(defun gnus-sync-unload-hook ()
  "Uninstall the sync hooks."
  (interactive)
  (remove-hook 'gnus-get-new-news-hook 'gnus-sync-read)
  (remove-hook 'gnus-save-newsrc-hook 'gnus-sync-save)
  (remove-hook 'gnus-read-newsrc-el-hook 'gnus-sync-read))

(add-hook 'gnus-sync-unload-hook 'gnus-sync-unload-hook)

;; this is harmless by default, until the gnus-sync-backend is set
(gnus-sync-initialize)

(provide 'gnus-sync)

;;; gnus-sync.el ends here
