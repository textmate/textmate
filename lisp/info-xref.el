;;; info-xref.el --- check external references in an Info document

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author: Kevin Ryde <user42@zip.com.au>
;; Keywords: docs
;; Version: 3

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

;; This is some simple checking of external cross references in info files,
;; docstrings and custom-links by attempting to visit the nodes specified.
;;
;; `M-x info-xref-check' checks a single info file.  See the docstring for
;; details.
;;
;; `M-x info-xref-check-all' checks all info files in Info-directory-list.
;; This is a good way to check the consistency of the whole system.
;;
;; `M-x info-xref-check-all-custom' loads up all defcustom variables and
;; checks any info references in them.
;;
;; `M-x info-xref-docstrings' checks docstring "Info node ..." hyperlinks in
;; source files (and other files).

;;; History:

;; Version 3 - new M-x info-xref-docstrings, use compilation-mode

;;; Code:

(require 'info)
(eval-when-compile
  (require 'cl)) ;; for `incf'

;;-----------------------------------------------------------------------------
;; vaguely generic

(defun info-xref-lock-file-p (filename)
  "Return non-nil if FILENAME is an Emacs lock file.
A lock file is \".#foo.txt\" etc per `lock-buffer'."
  (string-match "\\(\\`\\|\\/\\)\\.#" filename))

(defun info-xref-subfile-p (filename)
  "Return t if FILENAME is an info subfile.
If removing the last \"-<NUM>\" from the filename gives a file
which exists, then consider FILENAME a subfile.  This is an
imperfect test, probably ought to open up the purported top file
and see what subfiles it says."
  (and (string-match "\\`\\(\\([^-]*-\\)*[^-]*\\)-[0-9]+\\(.*\\)\\'" filename)
       (file-exists-p (concat (match-string 1 filename)
                              (match-string 3 filename)))))

(defmacro info-xref-with-file (filename &rest body)
  ;; checkdoc-params: (filename body)
  "Evaluate BODY in a buffer containing the contents of FILENAME.
If FILENAME is already in a buffer then that's used, otherwise a
temporary buffer.

The current implementation uses `insert-file-contents' rather
than `find-file-noselect' so as not to be held up by queries
about local variables or possible weirdness in a major mode.
`lm-with-file' does a similar thing, but it sets
`emacs-lisp-mode' which is not wanted here."

  (declare (debug t) (indent 1))
  `(let* ((info-xref-with-file--filename ,filename)
          (info-xref-with-file--body     (lambda () ,@body))
          (info-xref-with-file--existing
           (find-buffer-visiting info-xref-with-file--filename)))
     (if info-xref-with-file--existing
         (with-current-buffer info-xref-with-file--existing
           (save-excursion
             (funcall info-xref-with-file--body)))
       (with-temp-buffer
         (insert-file-contents ,filename)
         (funcall info-xref-with-file--body)))))


;;-----------------------------------------------------------------------------
;; output buffer

(defconst info-xref-output-buffer "*info-xref results*"
  "Name of the buffer for info-xref results.")

(defvar info-xref-good 0
  "Count of good cross references, during info-xref processing.")
(defvar info-xref-bad 0
  "Count of bad cross references, during info-xref processing.")
(defvar info-xref-unavail 0
  "Count of unavailable cross references, during info-xref processing.")

(defvar info-xref-output-heading ""
  "A heading string, during info-xref processing.
This is shown if there's an error, but not if successful.")

(defvar info-xref-filename nil
  "The current buffer's filename, during info-xref processing.
When looking at file contents in a temp buffer there's no
`buffer-file-name', hence this variable.")

(defvar info-xref-xfile-alist nil
  "Info files found or not found, during info-xref processing.
Key is \"(foo)\" etc and value nil or t according to whether info
manual \"(foo)\" exists or not.  This is used to suppress
duplicate messages about foo not being available.  (Duplicates
within one top-level file that is.)")

(defvar info-xref-in-progress nil)
(defmacro info-xref-with-output (&rest body)
  "Run BODY with an info-xref output buffer.
This is meant to nest, so you can wrap it around a set of
different info-xref checks and have them write to the one output
buffer created by the outermost `info-xref-with-output', with an
overall good/bad count summary inserted at the very end."

  (declare (debug t))
  `(save-excursion
     (unless info-xref-in-progress
       (display-buffer (get-buffer-create info-xref-output-buffer))
       (set-buffer info-xref-output-buffer)
       (setq buffer-read-only nil)
       (fundamental-mode)
       (erase-buffer)
       (insert ";; info-xref output -*- mode: compilation -*-\n\n")
       (compilation-mode)
       (setq info-xref-good    0
             info-xref-bad     0
             info-xref-unavail 0
             info-xref-xfile-alist nil))

     (let ((info-xref-in-progress t)
           (info-xref-output-heading ""))
       ,@body)

     (unless info-xref-in-progress
       (info-xref-output "done, %d good, %d bad, %d unavailable"
                         info-xref-good info-xref-bad info-xref-unavail))))

(defun info-xref-output (fmt &rest args)
  "Emit a `format'-ed message FMT+ARGS to the `info-xref-output-buffer'."
  (with-current-buffer info-xref-output-buffer
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert info-xref-output-heading
                (apply 'format fmt args)
                "\n")))
    (setq info-xref-output-heading "")
    ;; all this info-xref can be pretty slow, display now so the user sees
    ;; some progress
    (sit-for 0)))
(put 'info-xref-output 'byte-compile-format-like t)

(defun info-xref-output-error (fmt &rest args)
  "Emit a `format'-ed error FMT+ARGS to the `info-xref-output-buffer'.
The error is attributed to `info-xref-filename' and the current
buffer's line and column of point."
  (apply 'info-xref-output
         (concat "%s:%s:%s: " fmt)
         info-xref-filename
         (1+ (count-lines (point-min) (line-beginning-position)))
         (1+ (current-column))
         args))
(put 'info-xref-output-error 'byte-compile-format-like t)


;;-----------------------------------------------------------------------------
;; node checking

;; When asking Info-goto-node to fork, *info* needs to be the current
;; buffer, otherwise it seems to clone the current buffer but then do the
;; goto-node in plain *info*.
;;
;; We only fork if *info* already exists, if it doesn't then can create and
;; destroy just that instead of a new name.
;;
;; If Info-goto-node can't find the file, then no new buffer is created.  If
;; it finds the file but not the node, then a buffer is created.  Handle
;; this difference by checking before killing.
;;
(defun info-xref-goto-node-p (node)
  "Return t if it's possible to go to the given NODE."
  (let ((oldbuf (current-buffer)))
    (save-excursion
      (save-window-excursion
        (prog1
            (condition-case nil
                (progn
                  (Info-goto-node node
                                  (when (get-buffer "*info*")
                                    (set-buffer "*info*")
                                    "xref - temporary"))
                  t)
              (error nil))
          (unless (equal (current-buffer) oldbuf)
            (kill-buffer)))))))

(defun info-xref-check-node (node)

  ;; Collapse spaces as per info.el and `help-make-xrefs'.
  ;; Note defcustom :info-link nodes don't get this whitespace collapsing,
  ;; they should be the exact node name ready to visit.
  ;; `info-xref-check-all-custom' uses `info-xref-goto-node-p' and so
  ;; doesn't come through here.
  ;;
  ;; Could use "[\t\n ]+" but try to avoid uselessly replacing " " with " ".
  (setq node (replace-regexp-in-string "[\t\n][\t\n ]*\\| [\t\n ]+" " "
                                       node t t))

  (if (not (string-match "\\`([^)]*)" node))
      (info-xref-output-error "no `(file)' part at start of node: %s\n" node)
    (let ((file (match-string 0 node)))

      (if (string-equal "()" file)
          (info-xref-output-error "empty filename part: %s" node)

        ;; see if the file exists, if haven't looked before
        (unless (assoc file info-xref-xfile-alist)
          (let ((found (info-xref-goto-node-p file)))
            (push (cons file found) info-xref-xfile-alist)
            (unless found
              (info-xref-output-error "not available to check: %s\n    (this reported once per file)" file))))

        ;; if the file exists, try the node
        (cond ((not (cdr (assoc file info-xref-xfile-alist)))
               (incf info-xref-unavail))
              ((info-xref-goto-node-p node)
               (incf info-xref-good))
              (t
               (incf info-xref-bad)
               (info-xref-output-error "no such node: %s" node)))))))


;;-----------------------------------------------------------------------------

;;;###autoload
(defun info-xref-check (filename)
  "Check external references in FILENAME, an info document.
Interactively from an `Info-mode' or `texinfo-mode' buffer the
current info file is the default.

Results are shown in a `compilation-mode' buffer.  The format is
a bit rough, but there shouldn't be many problems normally.  The
file:line:column: is the info document, but of course normally
any correction should be made in the original .texi file.
Finding the right place in the .texi is a manual process.

When a target info file doesn't exist there's obviously no way to
validate node references within it.  A message is given for
missing target files once per source document.  It could be
simply that you don't have the target installed, or it could be a
mistake in the reference.

Indirect info files are understood, just pass the top-level
foo.info to `info-xref-check' and it traverses all sub-files.
Compressed info files are accepted too as usual for `Info-mode'.

\"makeinfo\" checks references internal to an info document, but
not external references, which makes it rather easy for mistakes
to creep in or node name changes to go unnoticed.
`Info-validate' doesn't check external references either."

  (interactive
   (list
    (let* ((default-filename
             (cond ((eq major-mode 'Info-mode)
                    Info-current-file)
                   ((eq major-mode 'texinfo-mode)
                    ;; look for @setfilename like makeinfo.el does
                    (save-excursion
                      (goto-char (point-min))
                      (if (re-search-forward
                           "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
                           (line-beginning-position 100) t)
                          (expand-file-name (match-string 1)))))))
           (prompt (if default-filename
                       (format "Info file (%s): " default-filename)
                     "Info file: ")))
      (read-file-name prompt nil default-filename t))))

  (info-xref-check-list (list filename)))

;;;###autoload
(defun info-xref-check-all ()
  "Check external references in all info documents in the info path.
`Info-directory-list' and `Info-additional-directory-list' are
the info paths.  See `info-xref-check' for how each file is
checked.

The search for \"all\" info files is rather permissive, since
info files don't necessarily have a \".info\" extension and in
particular the Emacs manuals normally don't.  If you have a
source code directory in `Info-directory-list' then a lot of
extraneous files might be read.  This will be time consuming but
should be harmless."

  (interactive)
  (info-xref-check-list (info-xref-all-info-files)))

;; An alternative for getting only top-level files here would be to simply
;; return all files and have info-xref-check-list not follow "Indirect:".
;; The current way seems better because it (potentially) gets the proper
;; top-level filename into the error messages, and suppresses duplicate "not
;; available" messages for all subfiles of a single document.

(defun info-xref-all-info-files ()
  "Return a list of all available info files.
Only top level files are returned, subfiles are excluded.

Since info files don't have to have a .info suffix, all files in
the relevant directories are considered, which might mean a lot
of extraneous things if for instance a source code directory is
in the path."

  (info-initialize) ;; establish Info-directory-list
  (apply 'nconc
         (mapcar
          (lambda (dir)
            (let ((result nil))
              (dolist (name (directory-files
                             dir
                             t           ;; absolute filenames
                             "\\`[^.]")) ;; not dotfiles, nor .# lockfiles
                (when (and (file-exists-p name) ;; ignore broken symlinks
                           (not (string-match "\\.te?xi\\'" name)) ;; not .texi
                           (not (backup-file-name-p name))
                           (not (file-directory-p name))
                           (not (info-xref-subfile-p name)))
		  (push name result)))
              (nreverse result)))
          (append Info-directory-list Info-additional-directory-list))))

(defun info-xref-check-list (filename-list)
  "Check external references in info documents in FILENAME-LIST."
  (info-xref-with-output
    (dolist (info-xref-filename filename-list)
      (setq info-xref-xfile-alist nil)
      (let ((info-xref-output-heading
             (format "Info file %s\n" info-xref-filename)))
        (with-temp-message (format "Looking at %s" info-xref-filename)
          (with-temp-buffer
            (info-insert-file-contents info-xref-filename)
            (goto-char (point-min))
            (if (search-forward "\^_\nIndirect:\n" nil t)
                (let ((dir (file-name-directory info-xref-filename)))
                  (while (looking-at "\\(.*\\): [0-9]+\n")
                    (let ((info-xref-filename
                           (expand-file-name (match-string 1) dir)))
                      (with-temp-buffer
                        (info-insert-file-contents info-xref-filename)
                        (info-xref-check-buffer)))
                    (forward-line)))
              (info-xref-check-buffer))))))))

(defun info-xref-check-buffer ()
  "Check external references in the info file in the current buffer.
This should be the raw file contents, not `Info-mode'."
  (goto-char (point-min))
  (while (re-search-forward
          "\\*[Nn]ote[ \n\t]+[^:]*:[ \n\t]+\\(\\(([^)]*)\\)[^.,]+\\)[.,]"
          nil t)
    (save-excursion
      (goto-char (match-beginning 1)) ;; start of nodename as error position
      (info-xref-check-node (match-string 1)))))

(defvar viper-mode) ;; quieten the byte compiler
(defvar gnus-registry-install)

;;;###autoload
(defun info-xref-check-all-custom ()
  "Check info references in all customize groups and variables.
Info references can be in `custom-manual' or `info-link' entries
of the `custom-links' for a variable.

Any `custom-load' autoloads in variables are loaded in order to
get full link information.  This will be a lot of Lisp packages
and can take a long time."

  (interactive)
  (info-xref-with-output

   ;; `custom-load-symbol' is not used, since it quietly ignores errors, but
   ;; we want to show them since they mean incomplete checking.
   ;;
   ;; Just one pass through mapatoms is made.  There shouldn't be any new
   ;; custom-loads setup by packages loaded.
   ;;
   (info-xref-output "Loading custom-load autoloads ...")
   (require 'cus-start)
   (require 'cus-load)

   ;; These are `setq' rather than `let' since a let would unbind the
   ;; variables after viper.el/gnus-registry.el have loaded, defeating the
   ;; defvars in those files.  Of course it'd be better if those files
   ;; didn't make interactive queries on loading at all, to allow for
   ;; programmatic loading like here.
   (unless (boundp 'viper-mode)
     (setq viper-mode nil))  ;; avoid viper.el ask about viperizing
   (unless (boundp 'gnus-registry-install)
     (setq gnus-registry-install nil))  ;; avoid gnus-registry.el querying

   (mapatoms
    (lambda (symbol)
      (dolist (load (get symbol 'custom-loads))
        (cond ((symbolp load)
               (condition-case cause (require load)
                 (error
                  (info-xref-output "Symbol `%s': cannot require '%s: %s"
                                    symbol load cause))))
              ;; skip if previously loaded
              ((assoc load load-history))
              ((assoc (locate-library load) load-history))
              (t
               (condition-case err
                   (load load)
                 (error
                  (info-xref-output "Symbol `%s': cannot load \"%s\": %s"
                                    symbol load
                                    (error-message-string err)))))))))

   ;; Don't bother to check whether the info file exists as opposed to just
   ;; a missing node.  If you have the code then you should have the
   ;; documentation, so a wrong node name will be the usual fault.
   ;;
   (info-xref-output "\nChecking custom-links references ...")
   (mapatoms
    (lambda (symbol)
      (dolist (link (get symbol 'custom-links))
        (when (memq (car link) '(custom-manual info-link))
          ;; skip :tag part of (custom-manual :tag "Foo" "(foo)Node")
          (if (eq :tag (cadr link))
              (setq link (cddr link)))
          (if (info-xref-goto-node-p (cadr link))
              (incf info-xref-good)
            (incf info-xref-bad)
            ;; symbol-file gives nil for preloaded variables, would need
            ;; to copy what describe-variable does to show the right place
            (info-xref-output "Symbol `%s' (file %s): cannot goto node: %s"
                              symbol
                              (symbol-file symbol 'defvar)
                              (cadr link)))))))))

;;;###autoload
(defun info-xref-docstrings (filename-list)
  ;; checkdoc-params: (filename-list)
  "Check docstring info node references in source files.
The given files are searched for docstring hyperlinks like

    Info node `(elisp)Documentation Tips'

and those links checked by attempting to visit the target nodes
as per `info-xref-check' does.

Interactively filenames are read as a wildcard pattern like
\"foo*.el\", with the current file as a default.  Usually this
will be lisp sources, but anything with such hyperlinks can be
checked, including the Emacs .c sources (or the etc/DOC file of
all builtins).

Because info node hyperlinks are found by a simple regexp search
in the files, the Lisp code checked doesn't have to be loaded,
and links can be in the file commentary or elsewhere too.  Even
.elc files can usually be checked successfully if you don't have
the sources handy."
  (interactive
   (let* ((default (and buffer-file-name
                              (file-relative-name buffer-file-name)))
          (prompt (if default
                      (format "Filename with wildcards (%s): "
                              default)
                    "Filename with wildcards: "))
          (pattern (read-file-name prompt nil default))
          ;; absolute filenames
          (filename-list (file-expand-wildcards pattern t))
          newlist)
     (setq filename-list
           (dolist (file filename-list (nreverse newlist))
             (or (info-xref-lock-file-p file)
                 (file-directory-p file)
                 (push file newlist))))
     (unless filename-list
       (error "No files: %S" pattern))
     (list filename-list)))

  (eval-and-compile
    (require 'help-mode)) ;; for `help-xref-info-regexp'

  (info-xref-with-output
   (dolist (info-xref-filename filename-list)
     (setq info-xref-xfile-alist nil)  ;; "not found"s once per file

     (info-xref-with-file info-xref-filename
       (goto-char (point-min))
       (while (re-search-forward help-xref-info-regexp nil t)
         (let ((node (match-string 2)))
           (save-excursion
             (goto-char (match-beginning 2)) ;; start of node as error position

             ;; skip nodes with "%" as probably `format' strings such as in
             ;; info-look.el
             (unless (string-match "%" node)

               ;; "(emacs)" is the default manual for docstring hyperlinks,
               ;; per `help-make-xrefs'
               (unless (string-match "\\`(" node)
                 (setq node (concat "(emacs)" node)))

               (info-xref-check-node node)))))))))


(provide 'info-xref)

;;; info-xref.el ends here
