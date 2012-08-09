;;; find-cmd.el --- Build a valid find(1) command with sexps

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Philip Jackson <phil@shellarchive.co.uk>
;; Version: 0.6

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

;; With this module you can build up a (hopefully) valid find(1)
;; string ready for the command line.  For example:

;; (find-cmd '(prune (name ".svn" ".git" ".CVS"))
;;           '(and (or (name "*.pl" "*.pm" "*.t")
;;                     (mtime "+1"))
;;                 (fstype "nfs" "ufs"))))

;; will become (un-wrapped):

;;     "find '/home/phil/' \\( \\( -name '.svn' -or -name '.git' -or
;;      -name '.CVS' \\) -prune -or -true \\) \\( \\( \\( -name '*.pl'
;;      -or -name '*.pm' -or -name '*.t' \\) -or -mtime '+1' \\) -and \\(
;;      -fstype 'nfs' -or -fstype 'ufs' \\) \\)"

;;; Code:

(defconst find-constituents
  '((and . find-and)
    (not . find-not)
    (or  . find-or)

    (a . find-and)
    (n . find-not)
    (o . find-or)

    (prune . find-prune)

    ;; switches
    (L . (0))
    (P . (0))
    (H . (0))

    ;; generic tests
    (amin       . (1))
    (anewer     . (1))
    (atime      . (1))
    (cmin       . (1))
    (cnewer     . (1))
    (ctime      . (1))
    (empty      . (0))
    (false      . (0))
    (fstype     . (1))
    (gid        . (1))
    (group      . (1))
    (ilname     . (1))
    (iname      . (1))
    (inum       . (1))
    (iwholename . (1))
    (iregex     . (1))
    (links      . (1))
    (lname      . (1))
    (mmin       . (1))
    (mtime      . (1))
    (name       . (1))
    (newer      . (1))
    (nouser     . (0))
    (nogroup    . (0))
    (path       . (1))
    (perm       . (0))
    (regex      . (1))
    (wholename  . (1))
    (size       . (1))
    (true       . (0))
    (type       . (1))
    (uid        . (1))
    (used       . (1))
    (user       . (1))
    (xtype      . (nil))

    ;; normal options (always true)
    (depth                 . (0))
    (maxdepth              . (1))
    (mindepth              . (1))
    (mount                 . (0))
    (noleaf                . (0))
    (xdev                  . (0))
    (ignore_readdir_race   . (0))
    (noignore_readdir_race . (0))

    ;; actions
    (delete  . (0))
    (print0  . (0))
    (printf  . (1))
    (fprintf . (2))
    (print   . (0))
    (fprint0 . (1))
    (fprint  . (1))
    (ls      . (0))
    (fls     . (1))
    (prune   . (0))
    (quit    . (0))

    ;; these need to be terminated with a ;
    (exec    . (1 find-command t))
    (ok      . (1 find-command t))
    (execdir . (1 find-command t))
    (okdir   . (1 find-command t)))
  "Holds details of each of the find options.
The car of each alist is the name.  The cdr is minimum args, the
function used to join many occurrences of the argument together,
and whether or not to leave quotes off the string (non-nil means
the string will be quoted).")

;;;###autoload
(defun find-cmd (&rest subfinds)
  "Initiate the building of a find command.
For example:

\(find-cmd '\(prune \(name \".svn\" \".git\" \".CVS\"\)\)
          '\(and \(or \(name \"*.pl\" \"*.pm\" \"*.t\"\)
                    \(mtime \"+1\"\)\)
                \(fstype \"nfs\" \"ufs\"\)\)\)\)

`default-directory' is used as the initial search path.  The
result is a string that should be ready for the command line."
  (concat
   "find " (shell-quote-argument (expand-file-name default-directory)) " "
           (cond
            ((cdr subfinds)
             (mapconcat 'find-to-string subfinds ""))
            (t
             (find-to-string (car subfinds))))))

(defun find-and (form)
  "And FORMs together, so:
  \(and \(mtime \"+1\"\) \(name \"something\"\)\)
will produce:
  find . \\\( -mtime '+1' -and -name 'something' \\\)"
  (if (< (length form) 2)
      (find-to-string (car form))
      (concat "\\( "
              (mapconcat 'find-to-string form "-and ")
              "\\) ")))

(defun find-or (form)
  "Or FORMs together, so:
  \(or \(mtime \"+1\"\) \(name \"something\"\)\)
will produce:
  find . \\\( -mtime '+1' -or -name 'something' \\\)"
  (if (< (length form) 2)
      (find-to-string (car form))
      (concat "\\( "
              (mapconcat 'find-to-string form "-or ")
              "\\) ")))

(defun find-not (form)
  "Or FORMs together and prefix with a -not, so:
  \(not \(mtime \"+1\"\) \(name \"something\"\)\)
will produce:
  -not \\\( -mtime '+1' -or -name 'something' \\\)
If you wanted the FORMs -and(ed) together instead then this would
suffice:
  \(not \(and \(mtime \"+1\"\) \(name \"something\"\)\)\)"
  (concat "-not " (find-or (mapcar 'find-to-string form))))

(defun find-prune (form)
  "-or together FORMs postfix '-prune' and then -or that with a
-true, so:
  \(prune \(name \".svn\" \".git\"\)\) \(name \"*.pm\"\)
will produce (unwrapped):
  \\\( \\\( \\\( -name '.svn' -or -name '.git' \\\) /
  -prune -or -true \\\) -and -name '*.pm' \\\)"
  (find-or
   (list
    (concat (find-or (mapcar 'find-to-string form)) (find-generic "prune"))
    (find-generic "true"))))

(defun find-generic (option &optional oper argcount args dont-quote)
  "Allow an arbitrary string to be used as a form.
OPTION is the name of the form, OPER is the function used to either
OR or AND multiple results together.  ARGCOUNT is the minimum of
args that OPTION can receive and ARGS are the arguments for OPTION.
If DONT-QUOTE is non-nil, arguments are quoted for passing them to
the shell."
  (when (and (numberp argcount) (< (length args) argcount))
    (error "'%s' needs at least %d arguments" option argcount))
  (let ((oper (or oper 'find-or)))
    (if (and args (length args))
        (funcall oper (mapcar (lambda (x)
                                (concat "-" option
                                        (if dont-quote
                                            (concat " " x " ")
                                            (concat " "
                                                    (shell-quote-argument x)
                                                    " "))))
                              args))
        (concat "-" option " "))))

(defun find-command (form)
  "For each item in FORM add a terminating semi-colon and turn
them into valid switches.  The result is -and(ed) together."
  (find-and (mapcar (lambda (x)
                      (concat (find-to-string x) "\\; "))
                    form)))

(defun find-to-string (form)
  "Parse FORM to produce a set of valid find arguments."
  (cond
    ((stringp form)
     form)
    ((consp form)
     (let ((option (cdr (assoc (car form) find-constituents))))
       (cond
         ((and (symbolp option) (fboundp option))
          (funcall option (cdr form)))
         ((consp option)
          (let ((option (symbol-name (car form)))
                (argcnt (car option))
                (oper (cadr option))
                (dont-quote (car (cddr option))))
            (find-to-string
             (find-generic option oper argcnt (cdr form) dont-quote))))
         (t
          (error "Sorry I don't know how to handle '%s'" (car form))))))))

(provide 'find-cmd)

;;; find-cmd.el ends here
