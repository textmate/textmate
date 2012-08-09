;;; dired-x.el --- extra Dired functionality

;; Copyright (C) 1993-1994, 1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Sebastian Kremer <sk@thp.uni-koeln.de>
;;	Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Maintainer: Romain Francoise <rfrancoise@gnu.org>
;; Keywords: dired extensions files
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

;; This is based on Sebastian Kremer's excellent dired-x.el (Dired Extra),
;; version 1.191, adapted for GNU Emacs.  See the `dired-x' info pages.

;; USAGE: In your ~/.emacs,
;;
;; (add-hook 'dired-load-hook
;;           (lambda ()
;;                       (load "dired-x")
;;                       ;; Set global variables here.  For example:
;;                       ;; (setq dired-guess-shell-gnutar "gtar")
;;                       ))
;; (add-hook 'dired-mode-hook
;;           (lambda ()
;;                       ;; Set buffer-local variables here.  For example:
;;                       ;; (dired-omit-mode 1)
;;                       ))
;;
;; At load time dired-x.el will install itself and bind some dired keys.
;; Some dired.el and dired-aux.el functions have extra features if
;; dired-x is loaded.

;; User customization: M-x customize-group RET dired-x RET.

;; *Please* see the `dired-x' info pages for more details.


;;; Code:

;; This is a no-op if dired-x is being loaded via `dired-load-hook',
;; but maybe not if a dired-x function is being autoloaded.
(require 'dired)

;;; User-defined variables.

(defgroup dired-x nil
  "Extended directory editing (dired-x)."
  :group 'dired)

(defgroup dired-keys nil
  "Dired keys customizations."
  :prefix "dired-"
  :group 'dired-x)

(defcustom dired-bind-vm nil
  "Non-nil means \"V\" runs `dired-vm', otherwise \"V\" runs `dired-rmail'.
RMAIL files in the old Babyl format (used before before Emacs 23.1)
contain \"-*- rmail -*-\" at the top, so `dired-find-file'
will run `rmail' on these files.  New RMAIL files use the standard
mbox format, and so cannot be distinguished in this way."
  :type 'boolean
  :group 'dired-keys)

(defcustom dired-bind-jump t
  "Non-nil means bind `dired-jump' to C-x C-j, otherwise do not.
Setting this variable directly after dired-x is loaded has no effect -
use \\[customize]."
  :type 'boolean
  :set (lambda (sym val)
         (if (set sym val)
             (progn
               (define-key ctl-x-map "\C-j" 'dired-jump)
               (define-key ctl-x-4-map "\C-j" 'dired-jump-other-window))
           (if (eq 'dired-jump (lookup-key ctl-x-map "\C-j"))
               (define-key ctl-x-map "\C-j" nil))
           (if (eq 'dired-jump-other-window (lookup-key ctl-x-4-map "\C-j"))
               (define-key ctl-x-4-map "\C-j" nil))))
  :group 'dired-keys)

(defcustom dired-bind-man t
  "Non-nil means bind `dired-man' to \"N\" in dired-mode, otherwise do not.
Setting this variable directly after dired-x is loaded has no effect -
use \\[customize]."
  :type 'boolean
  :set (lambda (sym val)
         (if (set sym val)
             (define-key dired-mode-map "N" 'dired-man)
           (if (eq 'dired-man (lookup-key dired-mode-map "N"))
               (define-key dired-mode-map "N" nil))))
  :group 'dired-keys)

(defcustom dired-bind-info t
  "Non-nil means bind `dired-info' to \"I\" in dired-mode, otherwise do not.
Setting this variable directly after dired-x is loaded has no effect -
use \\[customize]."
  :type 'boolean
  :set (lambda (sym val)
         (if (set sym val)
             (define-key dired-mode-map "I" 'dired-info)
           (if (eq 'dired-info (lookup-key dired-mode-map "I"))
               (define-key dired-mode-map "I" nil))))
  :group 'dired-keys)

(defcustom dired-vm-read-only-folders nil
  "If non-nil, \\[dired-vm] will visit all folders read-only.
If neither nil nor t, e.g. the symbol `if-file-read-only', only
files not writable by you are visited read-only."
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (other :tag "non-writable only" if-file-read-only))
  :group 'dired-x)

(defcustom dired-omit-size-limit 30000
  "Maximum size for the \"omitting\" feature.
If nil, there is no maximum size."
  :type '(choice (const :tag "no maximum" nil) integer)
  :group 'dired-x)

(define-minor-mode dired-omit-mode
  "Toggle omission of uninteresting files in Dired (Dired-Omit mode).
With a prefix argument ARG, enable Dired-Omit mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Dired-Omit mode is a buffer-local minor mode.  When enabled in a
Dired buffer, Dired does not list files whose filenames match
regexp `dired-omit-files', nor files ending with extensions in
`dired-omit-extensions'.

To enable omitting in every Dired buffer, you can put this in
your init file:

  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode)))

See Info node `(dired-x) Omitting Variables' for more information."
  :group 'dired-x
  (if dired-omit-mode
      ;; This will mention how many lines were omitted:
      (let ((dired-omit-size-limit nil)) (dired-omit-expunge))
    (revert-buffer)))

(put 'dired-omit-mode 'safe-local-variable 'booleanp)

;; For backward compatibility
(define-obsolete-variable-alias 'dired-omit-files-p 'dired-omit-mode "22.1")

(defcustom dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$"
  "Filenames matching this regexp will not be displayed.
This only has effect when `dired-omit-mode' is t.  See interactive function
`dired-omit-mode' \(\\[dired-omit-mode]\) and variable
`dired-omit-extensions'.  The default is to omit  `.', `..', auto-save
files and lock files."
  :type 'regexp
  :group 'dired-x)

(defcustom dired-omit-verbose t
  "When non-nil, show messages when omitting files.
When nil, don't show messages."
  :version "24.1"
  :type 'boolean
  :group 'dired-x)

(defcustom dired-find-subdir nil           ; t is pretty near to DWIM...
  "If non-nil, Dired always finds a directory in a buffer of its own.
If nil, Dired finds the directory as a subdirectory in some other buffer
if it is present as one.

If there are several dired buffers for a directory, the most recently
used is chosen.

Dired avoids switching to the current buffer, so that if you have
a normal and a wildcard buffer for the same directory, \\[dired] will
toggle between those two."
  :type 'boolean
  :group 'dired-x)

(defcustom dired-enable-local-variables t
  "Control use of local-variables lists in Dired.
This temporarily overrides the value of `enable-local-variables' when
listing a directory.  See also `dired-local-variables-file'."
  :risky t
  :type '(choice (const :tag "Query Unsafe" t)
		 (const :tag "Safe Only" :safe)
		 (const :tag "Do all" :all)
		 (const :tag "Ignore" nil)
		 (other :tag "Query" other))
  :group 'dired-x)

(make-obsolete-variable 'dired-enable-local-variables
                        "use a standard `dir-locals-file' instead." "24.1")

(defcustom dired-guess-shell-gnutar
  (catch 'found
    (dolist (exe '("tar" "gtar"))
      (if (with-temp-buffer
            (ignore-errors (call-process exe nil t nil "--version"))
            (and (re-search-backward "GNU tar" nil t) t))
          (throw 'found exe))))
  "If non-nil, name of GNU tar executable.
\(E.g., \"tar\" or \"gtar\").  The `z' switch will be used with it for
compressed or gzip'ed tar files.  If you don't have GNU tar, set this
to nil: a pipe using `zcat' or `gunzip -c' will be used."
  ;; Changed from system-type test to testing --version output.
  ;; Maybe test --help for -z instead?
  :version "24.1"
  :type '(choice (const :tag "Not GNU tar" nil)
		 (string :tag "Command name"))
  :group 'dired-x)

(defcustom dired-guess-shell-gzip-quiet t
  "Non-nil says pass -q to gzip overriding verbose GZIP environment."
  :type 'boolean
  :group 'dired-x)

(defcustom dired-guess-shell-znew-switches nil
  "If non-nil, then string of switches passed to `znew', example: \"-K\"."
  :type '(choice (const :tag "None" nil)
		 (string :tag "Switches"))
  :group 'dired-x)

(defcustom dired-clean-up-buffers-too t
  "Non-nil means offer to kill buffers visiting files and dirs deleted in Dired."
  :type 'boolean
  :group 'dired-x)

;;; KEY BINDINGS.

(define-key dired-mode-map "\M-o" 'dired-omit-mode)
(define-key dired-mode-map "*O" 'dired-mark-omitted)
(define-key dired-mode-map "\M-(" 'dired-mark-sexp)
(define-key dired-mode-map "*(" 'dired-mark-sexp)
(define-key dired-mode-map "*." 'dired-mark-extension)
(define-key dired-mode-map "\M-!" 'dired-smart-shell-command)
(define-key dired-mode-map "\M-G" 'dired-goto-subdir)
(define-key dired-mode-map "F" 'dired-do-find-marked-files)
(define-key dired-mode-map "Y"  'dired-do-relsymlink)
(define-key dired-mode-map "%Y" 'dired-do-relsymlink-regexp)
(define-key dired-mode-map "V" 'dired-do-run-mail)

;;; MENU BINDINGS

(require 'easymenu)

(let ((menu (lookup-key dired-mode-map [menu-bar])))
  (easy-menu-add-item menu '("Operate")
                      ["Find Files" dired-do-find-marked-files
                       :help "Find current or marked files"]
                      "Shell Command...")
  (easy-menu-add-item menu '("Operate")
                      ["Relative Symlink to..." dired-do-relsymlink
                       :visible (fboundp 'make-symbolic-link)
                       :help "Make relative symbolic links for current or \
marked files"]
                      "Hardlink to...")
  (easy-menu-add-item menu '("Mark")
                      ["Flag Extension..." dired-flag-extension
                       :help "Flag files with a certain extension for deletion"]
                      "Mark Executables")
  (easy-menu-add-item menu '("Mark")
                      ["Mark Extension..." dired-mark-extension
                       :help "Mark files with a certain extension"]
                      "Unmark All")
  (easy-menu-add-item menu '("Mark")
                      ["Mark Omitted" dired-mark-omitted
                       :help "Mark files matching `dired-omit-files' \
and `dired-omit-extensions'"]
                      "Unmark All")
  (easy-menu-add-item menu '("Regexp")
                      ["Relative Symlink..." dired-do-relsymlink-regexp
                       :visible (fboundp 'make-symbolic-link)
                       :help "Make relative symbolic links for files \
matching regexp"]
                      "Hardlink...")
  (easy-menu-add-item menu '("Immediate")
                      ["Omit Mode" dired-omit-mode
                       :style toggle :selected dired-omit-mode
                       :help "Enable or disable omitting \"uninteresting\" \
files"]
                      "Refresh"))


;; Install into appropriate hooks.

(add-hook 'dired-mode-hook 'dired-extra-startup)
(add-hook 'dired-after-readin-hook 'dired-omit-expunge)

(defun dired-extra-startup ()
  "Automatically put on `dired-mode-hook' to get extra Dired features:
\\<dired-mode-map>
  \\[dired-do-run-mail]\t-- run mail on folder (see `dired-bind-vm')
  \\[dired-info]\t-- run info on file
  \\[dired-man]\t-- run man on file
  \\[dired-do-find-marked-files]\t-- visit all marked files simultaneously
  \\[dired-omit-mode]\t-- toggle omitting of files
  \\[dired-mark-sexp]\t-- mark by Lisp expression

To see the options you can set, use M-x customize-group RET dired-x RET.
See also the functions:
  `dired-flag-extension'
  `dired-virtual'
  `dired-jump'
  `dired-man'
  `dired-vm'
  `dired-rmail'
  `dired-info'
  `dired-do-find-marked-files'"
  (interactive)
  ;; These must be done in each new dired buffer.
  (dired-hack-local-variables)
  (dired-omit-startup))


;;; EXTENSION MARKING FUNCTIONS.

;; Mark files with some extension.
(defun dired-mark-extension (extension &optional marker-char)
  "Mark all files with a certain EXTENSION for use in later commands.
A `.' is *not* automatically prepended to the string entered."
  ;; EXTENSION may also be a list of extensions instead of a single one.
  ;; Optional MARKER-CHAR is marker to use.
  (interactive "sMarking extension: \nP")
  (or (listp extension)
      (setq extension (list extension)))
  (dired-mark-files-regexp
   (concat ".";; don't match names with nothing but an extension
           "\\("
           (mapconcat 'regexp-quote extension "\\|")
           "\\)$")
   marker-char))

(defun dired-flag-extension (extension)
  "In dired, flag all files with a certain EXTENSION for deletion.
A `.' is *not* automatically prepended to the string entered."
  (interactive "sFlagging extension: ")
  (dired-mark-extension extension dired-del-marker))

;; Define some unpopular file extensions.  Used for cleaning and omitting.

(defvar dired-patch-unclean-extensions
  '(".rej" ".orig")
  "List of extensions of dispensable files created by the `patch' program.")

(defvar dired-tex-unclean-extensions
  '(".toc" ".log" ".aux");; these are already in completion-ignored-extensions
  "List of extensions of dispensable files created by TeX.")

(defvar dired-latex-unclean-extensions
  '(".idx" ".lof" ".lot" ".glo")
  "List of extensions of dispensable files created by LaTeX.")

(defvar dired-bibtex-unclean-extensions
  '(".blg" ".bbl")
  "List of extensions of dispensable files created by BibTeX.")

(defvar dired-texinfo-unclean-extensions
  '(".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs"
    ".tp" ".tps" ".vr" ".vrs")
  "List of extensions of dispensable files created by texinfo.")

(defun dired-clean-patch ()
  "Flag dispensable files created by patch for deletion.
See variable `dired-patch-unclean-extensions'."
  (interactive)
  (dired-flag-extension dired-patch-unclean-extensions))

(defun dired-clean-tex ()
  "Flag dispensable files created by [La]TeX etc. for deletion.
See variables `dired-tex-unclean-extensions',
`dired-latex-unclean-extensions', `dired-bibtex-unclean-extensions' and
`dired-texinfo-unclean-extensions'."
  (interactive)
  (dired-flag-extension (append dired-texinfo-unclean-extensions
                                dired-latex-unclean-extensions
                                dired-bibtex-unclean-extensions
                                dired-tex-unclean-extensions)))

(defun dired-very-clean-tex ()
  "Flag dispensable files created by [La]TeX *and* \".dvi\" for deletion.
See variables `dired-texinfo-unclean-extensions',
`dired-latex-unclean-extensions', `dired-bibtex-unclean-extensions' and
`dired-texinfo-unclean-extensions'."
  (interactive)
  (dired-flag-extension (append dired-texinfo-unclean-extensions
                                dired-latex-unclean-extensions
                                dired-bibtex-unclean-extensions
                                dired-tex-unclean-extensions
                                (list ".dvi"))))

;;; JUMP.

;;;###autoload
(defun dired-jump (&optional other-window file-name)
  "Jump to dired buffer corresponding to current buffer.
If in a file, dired the current directory and move to file's line.
If in Dired already, pop up a level and goto old directory's line.
In case the proper dired file line cannot be found, refresh the dired
buffer and try again.
When OTHER-WINDOW is non-nil, jump to dired buffer in other window.
Interactively with prefix argument, read FILE-NAME and
move to its line in dired."
  (interactive
   (list nil (and current-prefix-arg
		  (read-file-name "Jump to dired file: "))))
  (let* ((file (or file-name buffer-file-name))
         (dir (if file (file-name-directory file) default-directory)))
    (if (and (eq major-mode 'dired-mode) (null file-name))
        (progn
          (setq dir (dired-current-directory))
          (dired-up-directory other-window)
          (unless (dired-goto-file dir)
              ;; refresh and try again
            (dired-insert-subdir (file-name-directory dir))
            (dired-goto-file dir)))
      (if other-window
          (dired-other-window dir)
        (dired dir))
      (if file
          (or (dired-goto-file file)
              ;; refresh and try again
              (progn
                (dired-insert-subdir (file-name-directory file))
                (dired-goto-file file))
              ;; Toggle omitting, if it is on, and try again.
	      (when dired-omit-mode
                (dired-omit-mode)
                (dired-goto-file file)))))))

;;;###autoload
(defun dired-jump-other-window (&optional file-name)
  "Like \\[dired-jump] (`dired-jump') but in other window."
  (interactive
   (list (and current-prefix-arg
	      (read-file-name "Jump to dired file: "))))
  (dired-jump t file-name))

;;; OMITTING.

;; Enhanced omitting of lines from directory listings.
;; Marked files are never omitted.

;; should probably get rid of this and always use 'no-dir.
;; sk 28-Aug-1991 09:37
(defvar dired-omit-localp 'no-dir
  "The LOCALP argument `dired-omit-expunge' passes to `dired-get-filename'.
If it is `no-dir', omitting is much faster, but you can only match
against the non-directory part of the file name.  Set it to nil if you
need to match the entire file name.")

;; \017=^O for Omit - other packages can chose other control characters.
(defvar dired-omit-marker-char ?\017
  "Temporary marker used by Dired-Omit.
Should never be used as marker by the user or other packages.")

(defun dired-omit-startup ()
  (or (assq 'dired-omit-mode minor-mode-alist)
      (setq minor-mode-alist
            (append '((dired-omit-mode
		       (:eval (if (eq major-mode 'dired-mode)
				  " Omit" ""))))
		    minor-mode-alist))))

(defun dired-mark-omitted ()
  "Mark files matching `dired-omit-files' and `dired-omit-extensions'."
  (interactive)
  (let ((dired-omit-mode nil)) (revert-buffer)) ;; Show omitted files
  (dired-mark-unmarked-files (dired-omit-regexp) nil nil dired-omit-localp))

(defcustom dired-omit-extensions
  (append completion-ignored-extensions
          dired-latex-unclean-extensions
          dired-bibtex-unclean-extensions
          dired-texinfo-unclean-extensions)
  "If non-nil, a list of extensions \(strings\) to omit from Dired listings.
Defaults to elements of `completion-ignored-extensions',
`dired-latex-unclean-extensions', `dired-bibtex-unclean-extensions', and
`dired-texinfo-unclean-extensions'.

See interactive function `dired-omit-mode' \(\\[dired-omit-mode]\) and
variables `dired-omit-mode' and `dired-omit-files'."
  :type '(repeat string)
  :group 'dired-x)

(defun dired-omit-expunge (&optional regexp)
  "Erases all unmarked files matching REGEXP.
Does nothing if global variable `dired-omit-mode' is nil, or if called
  non-interactively and buffer is bigger than `dired-omit-size-limit'.
If REGEXP is nil or not specified, uses `dired-omit-files', and also omits
  filenames ending in `dired-omit-extensions'.
If REGEXP is the empty string, this function is a no-op.

This functions works by temporarily binding `dired-marker-char' to
`dired-omit-marker-char' and calling `dired-do-kill-lines'."
  (interactive "sOmit files (regexp): ")
  (if (and dired-omit-mode
           (or (called-interactively-p 'interactive)
               (not dired-omit-size-limit)
               (< (buffer-size) dired-omit-size-limit)
	       (progn
		 (when dired-omit-verbose
		   (message "Not omitting: directory larger than %d characters."
			    dired-omit-size-limit))
		 (setq dired-omit-mode nil)
		 nil)))
      (let ((omit-re (or regexp (dired-omit-regexp)))
            (old-modified-p (buffer-modified-p))
            count)
        (or (string= omit-re "")
            (let ((dired-marker-char dired-omit-marker-char))
              (when dired-omit-verbose (message "Omitting..."))
              (if (dired-mark-unmarked-files omit-re nil nil dired-omit-localp)
                  (progn
                    (setq count (dired-do-kill-lines
				 nil
				 (if dired-omit-verbose "Omitted %d line%s." "")))
                    (force-mode-line-update))
                (when dired-omit-verbose (message "(Nothing to omit)")))))
        ;; Try to preserve modified state of buffer.  So `%*' doesn't appear
        ;; in mode-line of omitted buffers.
        (set-buffer-modified-p (and old-modified-p
                                    (save-excursion
                                      (goto-char (point-min))
                                      (re-search-forward dired-re-mark nil t))))
        count)))

(defun dired-omit-regexp ()
  (concat (if dired-omit-files (concat "\\(" dired-omit-files "\\)") "")
          (if (and dired-omit-files dired-omit-extensions) "\\|" "")
          (if dired-omit-extensions
              (concat ".";; a non-extension part should exist
                      "\\("
                      (mapconcat 'regexp-quote dired-omit-extensions "\\|")
                      "\\)$")
            "")))

;; Returns t if any work was done, nil otherwise.
(defun dired-mark-unmarked-files (regexp msg &optional unflag-p localp)
  "Mark unmarked files matching REGEXP, displaying MSG.
REGEXP is matched against the entire file name.  When called
interactively, prompt for REGEXP.
With prefix argument, unflag all those files.
Optional fourth argument LOCALP is as in `dired-get-filename'."
  (interactive
   (list (dired-read-regexp
	  "Mark unmarked files matching regexp (default all): ")
	 nil current-prefix-arg nil))
  (let ((dired-marker-char (if unflag-p ?\s dired-marker-char)))
    (dired-mark-if
     (and
      ;; not already marked
      (looking-at " ")
      ;; uninteresting
      (let ((fn (dired-get-filename localp t)))
        (and fn (string-match regexp fn))))
     msg)))


;;; VIRTUAL DIRED MODE.

;; For browsing `ls -lR' listings in a dired-like fashion.

(defalias 'virtual-dired 'dired-virtual)
(defun dired-virtual (dirname &optional switches)
  "Put this buffer into Virtual Dired mode.

In Virtual Dired mode, all commands that do not actually consult the
filesystem will work.

This is useful if you want to peruse and move around in an ls -lR
output file, for example one you got from an ftp server.  With
ange-ftp, you can even dired a directory containing an ls-lR file,
visit that file and turn on virtual dired mode.  But don't try to save
this file, as dired-virtual indents the listing and thus changes the
buffer.

If you have save a Dired buffer in a file you can use \\[dired-virtual] to
resume it in a later session.

Type \\<dired-mode-map>\\[revert-buffer] \
in the Virtual Dired buffer and answer `y' to convert
the virtual to a real dired buffer again.  You don't have to do this, though:
you can relist single subdirs using \\[dired-do-redisplay]."

  ;; DIRNAME is the top level directory of the buffer.  It will become
  ;; its `default-directory'.  If nil, the old value of
  ;; default-directory is used.

  ;; Optional SWITCHES are the ls switches to use.

  ;; Shell wildcards will be used if there already is a `wildcard'
  ;; line in the buffer (thus it is a saved Dired buffer), but there
  ;; is no other way to get wildcards.  Insert a `wildcard' line by
  ;; hand if you want them.

  (interactive
   (list (read-string "Virtual Dired directory: " (dired-virtual-guess-dir))))
  (goto-char (point-min))
  (or (looking-at "  ")
      ;; if not already indented, do it now:
      (indent-region (point-min) (point-max) 2))
  (or dirname (setq dirname default-directory))
  (setq dirname (expand-file-name (file-name-as-directory dirname)))
  (setq default-directory dirname)      ; contains no wildcards
  (let ((wildcard (save-excursion
                    (goto-char (point-min))
                    (forward-line 1)
                    (and (looking-at "^  wildcard ")
                         (buffer-substring (match-end 0)
                                           (line-end-position))))))
  (if wildcard
        (setq dirname (expand-file-name wildcard default-directory))))
  ;; If raw ls listing (not a saved old dired buffer), give it a
  ;; decent subdir headerline:
  (goto-char (point-min))
  (or (looking-at dired-subdir-regexp)
      (insert "  "
	      (directory-file-name (file-name-directory default-directory))
	      ":\n"))
  (dired-mode dirname (or switches dired-listing-switches))
  (setq mode-name "Virtual Dired"
        revert-buffer-function 'dired-virtual-revert)
  (set (make-local-variable 'dired-subdir-alist) nil)
  (dired-build-subdir-alist)
  (goto-char (point-min))
  (dired-initial-position dirname))

(defun dired-virtual-guess-dir ()
  "Guess and return appropriate working directory of this buffer.
The buffer is assumed to be in Dired or ls -lR format.  The guess is
based upon buffer contents.  If nothing could be guessed, returns
nil."

  (let ((regexp "^\\(  \\)?\\([^ \n\r]*\\)\\(:\\)[\n\r]")
        (subexpr 2))
    (goto-char (point-min))
    (cond ((looking-at regexp)
           ;; If a saved dired buffer, look to which dir and
           ;; perhaps wildcard it belongs:
           (let ((dir (buffer-substring (match-beginning subexpr)
                                        (match-end subexpr))))
             (file-name-as-directory dir)))
          ;; Else no match for headerline found.  It's a raw ls listing.
          ;; In raw ls listings the directory does not have a headerline
          ;; try parent of first subdir, if any
          ((re-search-forward regexp nil t)
           (file-name-directory
            (directory-file-name
             (file-name-as-directory
              (buffer-substring (match-beginning subexpr)
                                (match-end subexpr))))))
          (t                            ; if all else fails
           nil))))


(defun dired-virtual-revert (&optional _arg _noconfirm)
  (if (not
       (y-or-n-p "Cannot revert a Virtual Dired buffer - switch to Real Dired mode? "))
      (error "Cannot revert a Virtual Dired buffer")
    (setq mode-name "Dired"
          revert-buffer-function 'dired-revert)
    (revert-buffer)))

;; A zero-arg version of dired-virtual.
(defun dired-virtual-mode ()
  "Put current buffer into Virtual Dired mode (see `dired-virtual').
Useful on `magic-mode-alist' with the regexp

  \"^  \\\\(/[^ /]+\\\\)+/?:$\"

to put saved dired buffers automatically into Virtual Dired mode.

Also useful for `auto-mode-alist' like this:

  (add-to-list 'auto-mode-alist
               '(\"[^/]\\\\.dired\\\\'\" . dired-virtual-mode))"
  (interactive)
  (dired-virtual (dired-virtual-guess-dir)))


;;; SMART SHELL.

;; An Emacs buffer can have but one working directory, stored in the
;; buffer-local variable `default-directory'.  A Dired buffer may have
;; several subdirectories inserted, but still has but one working directory:
;; that of the top level Dired directory in that buffer.  For some commands
;; it is appropriate that they use the current Dired directory instead of
;; `default-directory', e.g., `find-file' and `compile'.  This is a general
;; mechanism is provided for special handling of the working directory in
;; special major modes.

(define-obsolete-variable-alias 'default-directory-alist
  'dired-default-directory-alist "24.1")

;; It's easier to add to this alist than redefine function
;; default-directory while keeping the old information.
(defconst dired-default-directory-alist
  '((dired-mode . (if (fboundp 'dired-current-directory)
                      (dired-current-directory)
                    default-directory)))
  "Alist of major modes and their opinion on `default-directory'.
Each element has the form (MAJOR . EXPRESSION).
The function `dired-default-directory' evaluates EXPRESSION to
determine a default directory.")

(put 'dired-default-directory-alist 'risky-local-variable t) ; gets eval'd
(make-obsolete-variable 'dired-default-directory-alist
                        "this feature is due to be removed." "24.1")

(defun dired-default-directory ()
  "Return the `dired-default-directory-alist' entry for the current major-mode.
If none, return `default-directory'."
  (or (eval (cdr (assq major-mode dired-default-directory-alist)))
      default-directory))

;; It looks like this was intended to be something of a "general" feature,
;; but it only ever seems to have been used in dired-smart-shell-command,
;; and does not seem worth keeping around (?).
(make-obsolete 'dired-default-directory
               "this feature is due to be removed." "24.1")

(defun dired-smart-shell-command (command &optional output-buffer error-buffer)
  "Like function `shell-command', but in the current Virtual Dired directory."
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
			(cond
			 (buffer-file-name (file-relative-name buffer-file-name))
			 ((eq major-mode 'dired-mode) (dired-get-filename t t))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (shell-command command output-buffer error-buffer)))


;;; LOCAL VARIABLES FOR DIRED BUFFERS.

;; Brief Description  (This feature is obsolete as of Emacs 24.1)
;;
;; * `dired-extra-startup' is part of the `dired-mode-hook'.
;;
;; * `dired-extra-startup' calls `dired-hack-local-variables'
;;
;; * `dired-hack-local-variables' checks the value of
;;   `dired-local-variables-file'
;;
;; * Check if `dired-local-variables-file' is a non-nil string and is a
;;   filename found in the directory of the Dired Buffer being created.
;;
;; * If `dired-local-variables-file' satisfies the above, then temporarily
;;   include it in the Dired Buffer at the bottom.
;;
;; * Set `enable-local-variables' temporarily to the user variable
;;   `dired-enable-local-variables' and run `hack-local-variables' on the
;;   Dired Buffer.

(defcustom dired-local-variables-file (convert-standard-filename ".dired")
  "Filename, as string, containing local dired buffer variables to be hacked.
If this file found in current directory, then it will be inserted into dired
buffer and `hack-local-variables' will be run.  See Info node
`(emacs)File Variables' for more information on local variables.
See also `dired-enable-local-variables'."
  :type 'file
  :group 'dired)

(make-obsolete-variable 'dired-local-variables-file 'dir-locals-file "24.1")

(defun dired-hack-local-variables ()
  "Evaluate local variables in `dired-local-variables-file' for dired buffer."
  (and (stringp dired-local-variables-file)
       (file-exists-p dired-local-variables-file)
       (let ((opoint (point-max))
             (inhibit-read-only t)
             ;; In case user has `enable-local-variables' set to nil we
             ;; override it locally with dired's variable.
             (enable-local-variables dired-enable-local-variables))
         ;; Insert 'em.
         (save-excursion
           (goto-char opoint)
           (insert "\^L\n")
           (insert-file-contents dired-local-variables-file))
         ;; Hack 'em.
         (unwind-protect
             (let ((buffer-file-name dired-local-variables-file))
               (hack-local-variables))
           ;; Delete this stuff: `eobp' is used to find last subdir by dired.el.
           (delete-region opoint (point-max)))
         ;; Make sure that the modeline shows the proper information.
         (dired-sort-set-modeline))))

(make-obsolete 'dired-hack-local-variables
               'hack-dir-local-variables-non-file-buffer "24.1")

;; Does not seem worth a dedicated command.
;; See the more general features in files-x.el.
(defun dired-omit-here-always ()
  "Create `dir-locals-file' setting `dired-omit-mode' to t in `dired-mode'.
If in a Dired buffer, reverts it."
  (interactive)
  (if (file-exists-p dired-local-variables-file)
      (error "Old-style dired-local-variables-file `./%s' found;
replace it with a dir-locals-file `./%s'"
             dired-local-variables-file
             dir-locals-file))
  (if (file-exists-p dir-locals-file)
      (message "File `./%s' already exists." dir-locals-file)
    (with-temp-buffer
      (insert "\
\((dired-mode . ((subdirs . nil)
                (dired-omit-mode . t))))\n")
      (write-file dir-locals-file))
    ;; Run extra-hooks and revert directory.
    (when (derived-mode-p 'dired-mode)
      (hack-dir-local-variables-non-file-buffer)
      (dired-extra-startup)
      (dired-revert))))

(make-obsolete 'dired-omit-here-always 'add-dir-local-variable "24.1")


;;; GUESS SHELL COMMAND.

;; Brief Description:
;;;
;; * `dired-do-shell-command' is bound to `!' by dired.el.
;;;
;; * `dired-guess-shell-command' provides smarter defaults for
;;;    dired-aux.el's `dired-read-shell-command'.
;;;
;; * `dired-guess-shell-command' calls `dired-guess-default' with list of
;;;    marked files.
;;;
;; * Parse `dired-guess-shell-alist-user' and
;;;   `dired-guess-shell-alist-default' (in that order) for the first REGEXP
;;;   that matches the first file in the file list.
;;;
;; * If the REGEXP matches all the entries of the file list then evaluate
;;;   COMMAND, which is either a string or a Lisp expression returning a
;;;   string.  COMMAND may be a list of commands.
;;;
;; * Return this command to `dired-guess-shell-command' which prompts user
;;;   with it.  The list of commands is put into the list of default values.
;;;   If a command is used successfully then it is stored permanently in
;;;   `dired-shell-command-history'.

;; Guess what shell command to apply to a file.
(defvar dired-shell-command-history nil
  "History list for commands that read dired-shell commands.")

;; Default list of shell commands.

;; NOTE: Use `gunzip -c' instead of `zcat' on `.gz' files.  Some do not
;; install GNU zip's version of zcat.

(autoload 'Man-support-local-filenames "man")

(defvar dired-guess-shell-alist-default
  (list
   (list "\\.tar\\'"
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " xvf")
	    "tar xvf")
	 ;; Extract files into a separate subdirectory
	 '(if dired-guess-shell-gnutar
	      (concat "mkdir " (file-name-sans-extension file)
		      "; " dired-guess-shell-gnutar " -C "
		      (file-name-sans-extension file) " -xvf")
	    (concat "mkdir " (file-name-sans-extension file)
		    "; tar -C " (file-name-sans-extension file) " -xvf"))
	 ;; List archive contents.
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " tvf")
	    "tar tvf"))

   ;; REGEXPS for compressed archives must come before the .Z rule to
   ;; be recognized:
   (list "\\.tar\\.Z\\'"
	 ;; Untar it.
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " zxvf")
	    (concat "zcat * | tar xvf -"))
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   ;; gzip'ed archives
   (list "\\.t\\(ar\\.\\)?gz\\'"
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " zxvf")
	    (concat "gunzip -qc * | tar xvf -"))
	 ;; Extract files into a separate subdirectory
	 '(if dired-guess-shell-gnutar
	      (concat "mkdir " (file-name-sans-extension file)
		      "; " dired-guess-shell-gnutar " -C "
		      (file-name-sans-extension file) " -zxvf")
	    (concat "mkdir " (file-name-sans-extension file)
		    "; gunzip -qc * | tar -C "
		    (file-name-sans-extension file) " -xvf -"))
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q" ""))
	 ;; List archive contents.
	 '(if dired-guess-shell-gnutar
	      (concat dired-guess-shell-gnutar " ztvf")
	    (concat "gunzip -qc * | tar tvf -")))

   ;; bzip2'ed archives
   (list "\\.t\\(ar\\.bz2\\|bz\\)\\'"
	 "bunzip2 -c * | tar xvf -"
	 ;; Extract files into a separate subdirectory
	 '(concat "mkdir " (file-name-sans-extension file)
		  "; bunzip2 -c * | tar -C "
		  (file-name-sans-extension file) " -xvf -")
	 ;; Optional decompression.
	 "bunzip2")

   ;; xz'ed archives
   (list "\\.t\\(ar\\.\\)?xz\\'"
	 "unxz -c * | tar xvf -"
	 ;; Extract files into a separate subdirectory
	 '(concat "mkdir " (file-name-sans-extension file)
		  "; unxz -c * | tar -C "
		  (file-name-sans-extension file) " -xvf -")
	 ;; Optional decompression.
	 "unxz")

   '("\\.shar\\.Z\\'" "zcat * | unshar")
   '("\\.shar\\.g?z\\'" "gunzip -qc * | unshar")

   '("\\.e?ps\\'" "ghostview" "xloadimage" "lpr")
   (list "\\.e?ps\\.g?z\\'" "gunzip -qc * | ghostview -"
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.e?ps\\.Z\\'" "zcat * | ghostview -"
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   '("\\.patch\\'" "cat * | patch")
   (list "\\.patch\\.g?z\\'" "gunzip -qc * | patch"
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.patch\\.Z\\'" "zcat * | patch"
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   ;; The following four extensions are useful with dired-man ("N" key)
   ;; FIXME "man ./" does not work with dired-do-shell-command,
   ;; because there seems to be no way for us to modify the filename,
   ;; only the command.  Hmph.  `dired-man' works though.
   (list "\\.\\(?:[0-9]\\|man\\)\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db) "man -l")
                  ((eq loc 'man) "man ./")
                  (t
                   "cat * | tbl | nroff -man -h | col -b"))))
   (list "\\.\\(?:[0-9]\\|man\\)\\.g?z\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db)
                   "man -l")
                  ((eq loc 'man)
                   "man ./")
                  (t "gunzip -qc * | tbl | nroff -man -h | col -b")))
	 ;; Optional decompression.
	 '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.[0-9]\\.Z\\'"
         '(let ((loc (Man-support-local-filenames)))
            (cond ((eq loc 'man-db) "man -l")
                  ((eq loc 'man) "man ./")
                  (t "zcat * | tbl | nroff -man -h | col -b")))
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))
   '("\\.pod\\'" "perldoc" "pod2man * | nroff -man")

   '("\\.dvi\\'" "xdvi" "dvips")		; preview and printing
   '("\\.au\\'" "play")			; play Sun audiofiles
   '("\\.mpe?g\\'\\|\\.avi\\'" "xine -p")
   '("\\.ogg\\'" "ogg123")
   '("\\.mp3\\'" "mpg123")
   '("\\.wav\\'" "play")
   '("\\.uu\\'" "uudecode")		; for uudecoded files
   '("\\.hqx\\'" "mcvert")
   '("\\.sh\\'" "sh")			; execute shell scripts
   '("\\.xbm\\'" "bitmap")		; view X11 bitmaps
   '("\\.gp\\'" "gnuplot")
   '("\\.p[bgpn]m\\'" "xloadimage")
   '("\\.gif\\'" "xloadimage")		; view gif pictures
   '("\\.tif\\'" "xloadimage")
   '("\\.png\\'" "display")		; xloadimage 4.1 doesn't grok PNG
   '("\\.jpe?g\\'" "xloadimage")
   '("\\.fig\\'" "xfig")			; edit fig pictures
   '("\\.out\\'" "xgraph")		; for plotting purposes.
   '("\\.tex\\'" "latex" "tex")
   '("\\.texi\\(nfo\\)?\\'" "makeinfo" "texi2dvi")
   '("\\.pdf\\'" "xpdf")
   '("\\.doc\\'" "antiword" "strings")
   '("\\.rpm\\'" "rpm -qilp" "rpm -ivh")
   '("\\.dia\\'" "dia")
   '("\\.mgp\\'" "mgp")

   ;; Some other popular archivers.
   (list "\\.zip\\'" "unzip" "unzip -l"
	 ;; Extract files into a separate subdirectory
	 '(concat "unzip" (if dired-guess-shell-gzip-quiet " -q")
		  " -d " (file-name-sans-extension file)))
   '("\\.zoo\\'" "zoo x//")
   '("\\.lzh\\'" "lharc x")
   '("\\.arc\\'" "arc x")
   '("\\.shar\\'" "unshar")
   '("\\.rar\\'" "unrar x")
   '("\\.7z\\'" "7z x")

   ;; Compression.
   (list "\\.g?z\\'" '(concat "gunzip" (if dired-guess-shell-gzip-quiet " -q")))
   (list "\\.dz\\'" "dictunzip")
   (list "\\.bz2\\'" "bunzip2")
   (list "\\.xz\\'" "unxz")
   (list "\\.Z\\'" "uncompress"
	 ;; Optional conversion to gzip format.
	 '(concat "znew" (if dired-guess-shell-gzip-quiet " -q")
		  " " dired-guess-shell-znew-switches))

   '("\\.sign?\\'" "gpg --verify"))

  "Default alist used for shell command guessing.
See `dired-guess-shell-alist-user'.")

(defcustom dired-guess-shell-alist-user nil
  "User-defined alist of rules for suggested commands.
These rules take precedence over the predefined rules in the variable
`dired-guess-shell-alist-default' (to which they are prepended).

Each element of this list looks like

    \(REGEXP COMMAND...\)

where each COMMAND can either be a string or a Lisp expression that evaluates
to a string.  If several COMMANDs are given, the first one will be the default
and the rest will be added temporarily to the history and can be retrieved
with \\[previous-history-element] (M-p) .

The variable `dired-guess-shell-case-fold-search' controls whether
REGEXP is matched case-sensitively.

You can set this variable in your ~/.emacs.  For example, to add rules for
`.foo' and `.bar' files, write

 \(setq dired-guess-shell-alist-user
        '((\"\\\\.foo\\\\'\" \"FOO-COMMAND\")
          (\"\\\\.bar\\\\'\"
           (if condition
              \"BAR-COMMAND-1\"
            \"BAR-COMMAND-2\"))))"
  :group 'dired-x
  :type '(alist :key-type regexp :value-type (repeat sexp)))

(defcustom dired-guess-shell-case-fold-search t
  "If non-nil, `dired-guess-shell-alist-default' and
`dired-guess-shell-alist-user' are matched case-insensitively."
  :group 'dired-x
  :type 'boolean)

(defun dired-guess-default (files)
  "Return a shell command, or a list of commands, appropriate for FILES.
See `dired-guess-shell-alist-user'."

  (let* ((case-fold-search dired-guess-shell-case-fold-search)
         ;; Prepend the user's alist to the default alist.
         (alist (append dired-guess-shell-alist-user
                        dired-guess-shell-alist-default))
         (file (car files))
         (flist (cdr files))
         elt regexp cmds)

    ;; Find the first match in the alist for first file in FILES.
    (while alist
      (setq elt (car alist)
            regexp (car elt)
            alist (cdr alist))
      (if (string-match regexp file)
          (setq cmds (cdr elt)
                alist nil)))

    ;; If more than one file, see if all of FILES match regular expression.
    (while (and flist
                (string-match regexp (car flist)))
      (setq flist (cdr flist)))

    ;; If flist is still non-nil, then do not guess since this means that not
    ;; all the files in FILES were matched by the regexp.
    (setq cmds (and (not flist) cmds))

    ;; Return commands or nil if flist is still non-nil.
    ;; Evaluate the commands in order that any logical testing will be done.
    (if (cdr cmds)
	(delete-dups (mapcar #'eval cmds))
      (eval (car cmds)))))		; single command

(defun dired-guess-shell-command (prompt files)
  "Ask user with PROMPT for a shell command, guessing a default from FILES."
  (let ((default (dired-guess-default files))
        default-list val)
    (if (null default)
        ;; Nothing to guess
        (read-shell-command prompt nil 'dired-shell-command-history)
      (if (listp default)
          ;; More than one guess
          (setq default-list default
                default (car default)
                prompt (concat
                        prompt
                        (format "{%d guesses} " (length default-list))))
        ;; Just one guess
        (setq default-list (list default)))
      ;; Put the first guess in the prompt but not in the initial value.
      (setq prompt (concat prompt (format "[%s] " default)))
      ;; All guesses can be retrieved with M-n
      (setq val (read-shell-command prompt nil
                                    'dired-shell-command-history
                                    default-list))
      ;; If we got a return, then return default.
      (if (equal val "") default val))))


;;; RELATIVE SYMBOLIC LINKS.

(declare-function make-symbolic-link "fileio.c")

(defvar dired-keep-marker-relsymlink ?S
  "See variable `dired-keep-marker-move'.")

(defun dired-make-relative-symlink (file1 file2 &optional ok-if-already-exists)
  "Make a symbolic link (pointing to FILE1) in FILE2.
The link is relative (if possible), for example

    \"/vol/tex/bin/foo\" \"/vol/local/bin/foo\"

results in

    \"../../tex/bin/foo\" \"/vol/local/bin/foo\""
  (interactive "FRelSymLink: \nFRelSymLink %s: \np")
  (let (name1 name2 len1 len2 (index 0) sub)
    (setq file1 (expand-file-name file1)
          file2 (expand-file-name file2)
          len1 (length file1)
          len2 (length file2))
    ;; Find common initial file name components:
    (let (next)
      (while (and (setq next (string-match "/" file1 index))
                  (< (setq next (1+ next)) (min len1 len2))
                  ;; For the comparison, both substrings must end in
                  ;; `/', so NEXT is *one plus* the result of the
                  ;; string-match.
                  ;; E.g., consider the case of linking "/tmp/a/abc"
                  ;; to "/tmp/abc" erroneously giving "/tmp/a" instead
                  ;; of "/tmp/" as common initial component
                  (string-equal (substring file1 0 next)
                                (substring file2 0 next)))
        (setq index next))
      (setq name2 file2
            sub (substring file1 0 index)
            name1 (substring file1 index)))
    (if (string-equal sub "/")
        ;; No common initial file name found
        (setq name1 file1)
      ;; Else they have a common parent directory
      (let ((tem (substring file2 index))
            (start 0)
            (count 0))
        ;; Count number of slashes we must compensate for ...
        (while (setq start (string-match "/" tem start))
          (setq count (1+ count)
                start (1+ start)))
        ;; ... and prepend a "../" for each slash found:
        (dotimes (_n count)
          (setq name1 (concat "../" name1)))))
    (make-symbolic-link
     (directory-file-name name1)        ; must not link to foo/
                                        ; (trailing slash!)
     name2 ok-if-already-exists)))

(autoload 'dired-do-create-files "dired-aux")

;;;###autoload
(defun dired-do-relsymlink (&optional arg)
   "Relative symlink all marked (or next ARG) files into a directory.
Otherwise make a relative symbolic link to the current file.
This creates relative symbolic links like

    foo -> ../bar/foo

not absolute ones like

    foo -> /ugly/file/name/that/may/change/any/day/bar/foo

For absolute symlinks, use \\[dired-do-symlink]."
  (interactive "P")
  (dired-do-create-files 'relsymlink #'dired-make-relative-symlink
                           "RelSymLink" arg dired-keep-marker-relsymlink))

(autoload 'dired-mark-read-regexp "dired-aux")
(autoload 'dired-do-create-files-regexp "dired-aux")

(defun dired-do-relsymlink-regexp (regexp newname &optional arg whole-name)
  "RelSymlink all marked files containing REGEXP to NEWNAME.
See functions `dired-do-rename-regexp' and `dired-do-relsymlink'
for more info."
  (interactive (dired-mark-read-regexp "RelSymLink"))
  (dired-do-create-files-regexp
   #'dired-make-relative-symlink
   "RelSymLink" arg regexp newname whole-name dired-keep-marker-relsymlink))


;;; VISIT ALL MARKED FILES SIMULTANEOUSLY.

;; Brief Description:
;;;
;; `dired-do-find-marked-files' is bound to `F' by dired-x.el.
;;;
;; * Use `dired-get-marked-files' to collect the marked files in the current
;;;   Dired Buffer into a list of filenames `FILE-LIST'.
;;;
;; * Pass FILE-LIST to `dired-simultaneous-find-file' all with
;;;   `dired-do-find-marked-files''s prefix argument NOSELECT.
;;;
;; * `dired-simultaneous-find-file' runs through FILE-LIST decrementing the
;;;   list each time.
;;;
;; * If NOSELECT is non-nil then just run `find-file-noselect' on each
;;;   element of FILE-LIST.
;;;
;; * If NOSELECT is nil then calculate the `size' of the window for each file
;;;   by dividing the `window-height' by length of FILE-LIST.  Thus, `size' is
;;;   cognizant of the window-configuration.
;;;
;; * If `size' is too small abort, otherwise run `find-file' on each element
;;;   of FILE-LIST giving each a window of height `size'.

(defun dired-do-find-marked-files (&optional noselect)
  "Find all marked files displaying all of them simultaneously.
With optional NOSELECT just find files but do not select them.

The current window is split across all files marked, as evenly as possible.
Remaining lines go to bottom-most window.  The number of files that can be
displayed this way is restricted by the height of the current window and
`window-min-height'.

To keep dired buffer displayed, type \\[split-window-below] first.
To display just marked files, type \\[delete-other-windows] first."
  (interactive "P")
  (dired-simultaneous-find-file (dired-get-marked-files) noselect))

(defun dired-simultaneous-find-file (file-list noselect)
  "Visit all files in FILE-LIST and display them simultaneously.
The current window is split across all files in FILE-LIST, as evenly as
possible.  Remaining lines go to the bottom-most window.  The number of
files that can be displayed this way is restricted by the height of the
current window and the variable `window-min-height'.  With non-nil
NOSELECT the files are merely found but not selected."
  ;; We don't make this function interactive because it is usually too clumsy
  ;; to specify FILE-LIST interactively unless via dired.
  (let (size)
    (if noselect
        ;; Do not select the buffer.
        (find-file-noselect (car file-list))
      ;; We will have to select the buffer.  Calculate and check window size.
      (setq size (/ (window-height) (length file-list)))
      (or (<= window-min-height size)
          (error "Too many files to visit simultaneously.  Try C-u prefix"))
      (find-file (car file-list)))
    ;; Decrement.
    (dolist (file (cdr file-list))
      (if noselect
          ;; Do not select the buffer.
          (find-file-noselect file)
        ;; Vertically split off a window of desired size.  Upper window will
        ;; have SIZE lines.  Select lower (larger) window.  We split it again.
        (select-window (split-window nil size))
        (find-file file)))))


;;; MISCELLANEOUS COMMANDS.

;; Run man on files.

(declare-function Man-getpage-in-background "man" (topic))

(defvar manual-program) ; from man.el

(defun dired-man ()
  "Run `man' on this file."
;; Used also to say: "Display old buffer if buffer name matches filename."
;; but I have no idea what that means.
  (interactive)
  (require 'man)
  (let* ((file (dired-get-filename))
         (manual-program (replace-regexp-in-string "\\*" "%s"
                          (dired-guess-shell-command
                           "Man command: " (list file)))))
    (Man-getpage-in-background file)))

;; Run Info on files.

(defun dired-info ()
  "Run `info' on this file."
  (interactive)
  (info (dired-get-filename)))

;; Run mail on mail folders.

(declare-function vm-visit-folder "ext:vm" (folder &optional read-only))
(defvar vm-folder-directory)

(defun dired-vm (&optional read-only)
  "Run VM on this file.
With optional prefix argument, visits the folder read-only.
Otherwise obeys the value of `dired-vm-read-only-folders'."
  (interactive "P")
  (let ((dir (dired-current-directory))
        (fil (dired-get-filename)))
    (vm-visit-folder fil (or read-only
                             (eq t dired-vm-read-only-folders)
                             (and dired-vm-read-only-folders
                                  (not (file-writable-p fil)))))
    ;; So that pressing `v' inside VM does prompt within current directory:
    (set (make-local-variable 'vm-folder-directory) dir)))

(defun dired-rmail ()
  "Run RMAIL on this file."
  (interactive)
  (rmail (dired-get-filename)))

(defun dired-do-run-mail ()
  "If `dired-bind-vm' is non-nil, call `dired-vm', else call `dired-rmail'."
  (interactive)
  (if dired-bind-vm
      ;; Read mail folder using vm.
      (dired-vm)
    ;; Read mail folder using rmail.
    (dired-rmail)))


;;; MISCELLANEOUS INTERNAL FUNCTIONS.

;; This should be a builtin
(defun dired-buffer-more-recently-used-p (buffer1 buffer2)
  "Return t if BUFFER1 is more recently used than BUFFER2.
Considers buffers closer to the car of `buffer-list' to be more recent."
  (and (not (equal buffer1 buffer2))
       (memq buffer1 (buffer-list))
       (not (memq buffer1 (memq buffer2 (buffer-list))))))

;; Same thing as `dired-buffers-for-dir' of dired.el? - lrd 11/23/93
;; (defun dired-buffers-for-dir-exact (dir)
;; ;; Return a list of buffers that dired DIR (a directory or wildcard)
;; ;; at top level, or as subdirectory.
;; ;; Top level matches must match the wildcard part too, if any.
;; ;; The list is in reverse order of buffer creation, most recent last.
;; ;; As a side effect, killed dired buffers for DIR are removed from
;; ;; dired-buffers.
;;   (let ((alist dired-buffers) result elt)
;;     (while alist
;;       (setq elt (car alist)
;;             alist (cdr alist))
;;       (let ((buf (cdr elt)))
;;         (if (buffer-name buf)
;;             ;; Top level must match exactly against dired-directory in
;;             ;; case one of them is a wildcard.
;;             (if (or (equal dir (with-current-buffer buf dired-directory))
;;                     (assoc dir (with-current-buffer buf dired-subdir-alist)))
;;                 (setq result (cons buf result)))
;;           ;; else buffer is killed - clean up:
;;           (setq dired-buffers (delq elt dired-buffers)))))
;;     result))


;; Does anyone use this? - lrd 6/29/93.
;; Apparently people do use it. - lrd 12/22/97.

(with-no-warnings
  ;; Warnings are suppressed to avoid "global/dynamic var `X' lacks a prefix".
  ;; This is unbearably ugly, but not more than having global variables
  ;; named size, time, name or s, however practical it can be while writing
  ;; `dired-mark-sexp' predicates.
  (defvar inode)
  (defvar s)
  (defvar mode)
  (defvar nlink)
  (defvar uid)
  (defvar gid)
  (defvar size)
  (defvar time)
  (defvar name)
  (defvar sym))

(defun dired-mark-sexp (predicate &optional unflag-p)
  "Mark files for which PREDICATE returns non-nil.
With a prefix arg, unmark or unflag those files instead.

PREDICATE is a lisp expression that can refer to the following symbols:

    inode  [integer] the inode of the file (only for ls -i output)
    s      [integer] the size of the file for ls -s output
                     (usually in blocks or, with -k, in KByte)
    mode   [string]  file permission bits, e.g. \"-rw-r--r--\"
    nlink  [integer] number of links to file
    uid    [string]  owner
    gid    [string]  group  (If the gid is not displayed by ls,
                     this will still be set (to the same as uid))
    size   [integer] file size in bytes
    time   [string]  the time that ls displays, e.g. \"Feb 12 14:17\"
    name   [string]  the name of the file
    sym    [string]  if file is a symbolic link, the linked-to name, else \"\"

For example, use

        (equal 0 size)

to mark all zero length files."
  ;; Using sym="" instead of nil avoids the trap of
  ;; (string-match "foo" sym) into which a user would soon fall.
  ;; Give `equal' instead of `=' in the example, as this works on
  ;; integers and strings.
  (interactive "xMark if (lisp expr): \nP")
  (message "%s" predicate)
  (let ((dired-marker-char (if unflag-p ?\040 dired-marker-char))
        inode s mode nlink uid gid size time name sym)
    (dired-mark-if
     (save-excursion
       (and
        ;; Sets vars
        ;;                inode s mode nlink uid gid size time name sym

        ;; according to current file line.  Returns t for success, nil if
        ;; there is no file line.  Upon success, all variables are set, either
        ;; to nil or the appropriate value, so they need not be initialized.
        ;; Moves point within the current line.
        (dired-move-to-filename)
        (let (pos
              (mode-len 10) ; length of mode string
              ;; like in dired.el, but with subexpressions \1=inode, \2=s:
              (dired-re-inode-size "\\s *\\([0-9]*\\)\\s *\\([0-9]*\\) ?"))
          (beginning-of-line)
          (forward-char 2)
          (if (looking-at dired-re-inode-size)
              (progn
                (goto-char (match-end 0))
                (setq inode (string-to-number
                             (buffer-substring (match-beginning 1)
                                               (match-end 1)))
                      s (string-to-number
                         (buffer-substring (match-beginning 2)
                                           (match-end 2)))))
            (setq inode nil
                  s nil))
          (setq mode (buffer-substring (point) (+ mode-len (point))))
          (forward-char mode-len)
          (setq nlink (read (current-buffer)))
          ;; Karsten Wenger <kw@cis.uni-muenchen.de> fixed uid.
          (setq uid (buffer-substring (1+ (point))
                                      (progn (forward-word 1) (point))))
          (re-search-forward directory-listing-before-filename-regexp)
          (goto-char (match-beginning 1))
          (forward-char -1)
          (setq size (string-to-number
                      (buffer-substring (save-excursion
                                          (backward-word 1)
                                          (setq pos (point)))
                                        (point))))
          (goto-char pos)
          (backward-word 1)
          ;; if no gid is displayed, gid will be set to uid
          ;; but user will then not reference it anyway in PREDICATE.
          (setq gid (buffer-substring (save-excursion
                                        (forward-word 1) (point))
                                      (point))
                time (buffer-substring (match-beginning 1)
                                       (1- (dired-move-to-filename)))
                name (buffer-substring (point)
                                       (or
                                        (dired-move-to-end-of-filename t)
                                        (point)))
                sym (if (looking-at " -> ")
                        (buffer-substring (progn (forward-char 4) (point))
                                          (line-end-position))
                      ""))
          t)
        (eval predicate)))
     (format "'%s file" predicate))))


;;; FIND FILE AT POINT.

(defcustom dired-x-hands-off-my-keys t
  "Non-nil means don't remap `find-file' to `dired-x-find-file'.
Similarly for `find-file-other-window' and `dired-x-find-file-other-window'.
If you change this variable without using \\[customize] after `dired-x.el'
is loaded then call \\[dired-x-bind-find-file]."
  :type 'boolean
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (set symbol value)
         (dired-x-bind-find-file))
  :group 'dired-x)

(defun dired-x-bind-find-file ()
  "Bind `dired-x-find-file' in place of `find-file' (or vice-versa).
Similarly for `dired-x-find-file-other-window' and `find-file-other-window'.
Binding direction based on `dired-x-hands-off-my-keys'."
  (interactive)
  (if (called-interactively-p 'interactive)
      (setq dired-x-hands-off-my-keys
            (not (y-or-n-p "Bind dired-x-find-file over find-file? "))))
  (define-key (current-global-map) [remap find-file]
    (if (not dired-x-hands-off-my-keys) 'dired-x-find-file))
  (define-key (current-global-map) [remap find-file-other-window]
    (if (not dired-x-hands-off-my-keys) 'dired-x-find-file-other-window)))

;; Now call it so binding is correct.  This could go in the :initialize
;; slot, but then dired-x-bind-find-file has to be defined before the
;; defcustom, and we get free variable warnings.
(dired-x-bind-find-file)

(defun dired-x-find-file (filename)
  "Edit file FILENAME.
Like `find-file', except that when called interactively with a
prefix argument, it offers the filename near point as a default."
  (interactive (list (dired-x-read-filename-at-point "Find file: ")))
  (find-file filename))

(defun dired-x-find-file-other-window (filename)
  "Edit file FILENAME, in another window.
Like `find-file-other-window', except that when called interactively with
a prefix argument, when it offers the filename near point as a default."
  (interactive (list (dired-x-read-filename-at-point "Find file: ")))
  (find-file-other-window filename))

;;; Internal functions.

;; Fixme: This should probably use `thing-at-point'.  -- fx
(defun dired-filename-at-point ()
  "Return the filename closest to point, expanded.
Point should be in or after a filename."
  (save-excursion
    ;; First see if just past a filename.
    (or (eobp)                             ; why?
        (when (looking-at "[] \t\n[{}()]") ; whitespace or some parens
          (skip-chars-backward " \n\t\r({[]})")
          (or (bobp) (backward-char 1))))
    (let ((filename-chars "-.[:alnum:]_/:$+@")
          start prefix)
      (if (looking-at (format "[%s]" filename-chars))
          (progn
            (skip-chars-backward filename-chars)
            (setq start (point)
                  prefix
                  ;; This is something to do with ange-ftp filenames.
                  ;; It convert foo@bar to /foo@bar.
                  ;; But when does the former occur in dired buffers?
		  (and (string-match
			"^\\w+@"
			(buffer-substring start (line-end-position)))
		       "/"))
            (if (string-match "[/~]" (char-to-string (preceding-char)))
                (setq start (1- start)))
            (skip-chars-forward filename-chars))
        (error "No file found around point!"))
      ;; Return string.
      (expand-file-name (concat prefix (buffer-substring start (point)))))))

(defun dired-x-read-filename-at-point (prompt)
  "Return filename prompting with PROMPT with completion.
If `current-prefix-arg' is non-nil, uses name at point as guess."
  (if current-prefix-arg
      (let ((guess (dired-filename-at-point)))
        (read-file-name prompt
                        (file-name-directory guess)
                        guess
                        nil (file-name-nondirectory guess)))
    (read-file-name prompt default-directory)))

(define-obsolete-function-alias 'read-filename-at-point
  'dired-x-read-filename-at-point "24.1") ; is this even needed?

;;; BUG REPORTS

(define-obsolete-function-alias 'dired-x-submit-report 'report-emacs-bug "24.1")


;; As Barry Warsaw would say: "This might be useful..."
(provide 'dired-x)

;; Local Variables:
;; byte-compile-dynamic: t
;; generated-autoload-file: "dired.el"
;; End:

;;; dired-x.el ends here
