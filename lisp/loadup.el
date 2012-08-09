;;; loadup.el --- load up standardly loaded Lisp files for Emacs

;; Copyright (C) 1985-1986, 1992, 1994, 2001-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: internal
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

;; This is loaded into a bare Emacs to make a dumpable one.

;; If you add/remove Lisp files to be loaded here, consider the
;; following issues:

;; i) Any file loaded on any platform should appear in $lisp in src/lisp.mk.
;; Use the .el or .elc version as appropriate.

;; This ensures both that the Lisp files are compiled (if necessary)
;; before the emacs executable is dumped, and that they are passed to
;; make-docfile.  (Any that are not processed for DOC will not have
;; doc strings in the dumped Emacs.)  Because of this:

;; ii) If the file is loaded uncompiled, it should (where possible)
;; obey the doc-string conventions expected by make-docfile.

;;; Code:

;; Add subdirectories to the load-path for files that might get
;; autoloaded when bootstrapping.
(if (or (equal (nth 3 command-line-args) "bootstrap")
	(equal (nth 4 command-line-args) "bootstrap")
	(equal (nth 3 command-line-args) "unidata-gen.el")
	(equal (nth 4 command-line-args) "unidata-gen-files")
	;; In case CANNOT_DUMP.
	(string-match "src/bootstrap-emacs" (nth 0 command-line-args)))
    (let ((dir (car load-path)))
      ;; We'll probably overflow the pure space.
      (setq purify-flag nil)
      (setq load-path (list dir
			    (expand-file-name "emacs-lisp" dir)
			    (expand-file-name "language" dir)
			    (expand-file-name "international" dir)
			    (expand-file-name "textmodes" dir)))))

(if (eq t purify-flag)
    ;; Hash consing saved around 11% of pure space in my tests.
    (setq purify-flag (make-hash-table :test 'equal)))

(message "Using load-path %s" load-path)

(if (or (member (nth 3 command-line-args) '("dump" "bootstrap"))
	(member (nth 4 command-line-args) '("dump" "bootstrap")))
    ;; To reduce the size of dumped Emacs, we avoid making huge
    ;; char-tables.
    (setq inhibit-load-charset-map t))

;; We don't want to have any undo records in the dumped Emacs.
(set-buffer "*scratch*")
(setq buffer-undo-list t)

(load "emacs-lisp/byte-run")
(load "emacs-lisp/backquote")
(load "subr")

;; Do it after subr, since both after-load-functions and add-hook are
;; implemented in subr.el.
(add-hook 'after-load-functions (lambda (f) (garbage-collect)))

;; We specify .el in case someone compiled version.el by mistake.
(load "version.el")

(load "widget")
(load "custom")
(load "emacs-lisp/map-ynp")
(load "cus-start")
(load "international/mule")
(load "international/mule-conf")
(load "env")
(load "format")
(load "bindings")
(load "window")  ; Needed here for `replace-buffer-in-windows'.
(setq load-source-file-function 'load-with-code-conversion)
(load "files")

(load "cus-face")
(load "faces")  ; after here, `defface' may be used.

(load "button")
(load "startup")

(condition-case nil
    ;; Don't get confused if someone compiled this by mistake.
    (load "loaddefs.el")
  ;; In case loaddefs hasn't been generated yet.
  (file-error (load "ldefs-boot.el")))

(load "minibuffer")
(load "abbrev")         ;lisp-mode.el and simple.el use define-abbrev-table.
(load "simple")

(load "help")

(load "jka-cmpr-hook")
(load "epa-hook")
;; Any Emacs Lisp source file (*.el) loaded here after can contain
;; multilingual text.
(load "international/mule-cmds")
(load "case-table")
;; This file doesn't exist when building a development version of Emacs
;; from the repository.  It is generated just after temacs is built.
(load "international/charprop.el" t)
(load "international/characters")
(load "composite")

;; Load language-specific files.
(load "language/chinese")
(load "language/cyrillic")
(load "language/indian")
(load "language/sinhala")
(load "language/english")
(load "language/ethiopic")
(load "language/european")
(load "language/czech")
(load "language/slovak")
(load "language/romanian")
(load "language/greek")
(load "language/hebrew")
(load "language/japanese")
(load "language/korean")
(load "language/lao")
(load "language/tai-viet")
(load "language/thai")
(load "language/tibetan")
(load "language/vietnamese")
(load "language/misc-lang")
(load "language/utf-8-lang")
(load "language/georgian")
(load "language/khmer")
(load "language/burmese")
(load "language/cham")

(load "indent")
(load "frame")
(load "term/tty-colors")
(load "font-core")
;; facemenu must be loaded before font-lock, because `facemenu-keymap'
;; needs to be defined when font-lock is loaded.
(load "facemenu")
(load "emacs-lisp/syntax")
(load "font-lock")
(load "jit-lock")

(if (fboundp 'track-mouse)
    (progn
      (load "mouse")
      (and (boundp 'x-toolkit-scroll-bars)
	   (load "scroll-bar"))
      (load "select")))
(load "emacs-lisp/timer")
(load "isearch")
(load "rfn-eshadow")

(load "menu-bar")
(load "paths.el")  ;Don't get confused if someone compiled paths by mistake.
(load "emacs-lisp/lisp")
(load "textmodes/page")
(load "register")
(load "textmodes/paragraphs")
(load "emacs-lisp/lisp-mode")
(load "textmodes/text-mode")
(load "textmodes/fill")

(load "replace")
(load "buff-menu")

(if (fboundp 'x-create-frame)
    (progn
      (load "fringe")
      (load "image")
      (load "international/fontset")
      (load "dnd")
      (load "tool-bar")))

(if (featurep 'dynamic-setting)
    (load "dynamic-setting"))

(if (featurep 'x)
    (progn
      (load "x-dnd")
      (load "term/common-win")
      (load "term/x-win")))

(if (eq system-type 'windows-nt)
    (progn
      (load "w32-vars")
      (load "term/common-win")
      (load "term/w32-win")
      (load "ls-lisp")
      (load "disp-table")
      (load "dos-w32")
      (load "w32-fns")))
(if (eq system-type 'ms-dos)
    (progn
      (load "dos-w32")
      (load "dos-fns")
      (load "dos-vars")
      ;; Don't load term/common-win: it isn't appropriate for the `pc'
      ;; ``window system'', which generally behaves like a terminal.
      (load "term/pc-win")
      (load "ls-lisp")
      (load "disp-table"))) ; needed to setup ibm-pc char set, see internal.el
(if (featurep 'ns)
    (progn
      (load "term/common-win")
      (load "term/ns-win")))
(if (fboundp 'x-create-frame)
    ;; Do it after loading term/foo-win.el since the value of the
    ;; mouse-wheel-*-event vars depends on those files being loaded or not.
    (load "mwheel"))
;; Preload some constants and floating point functions.
(load "emacs-lisp/float-sup")

(load "vc/vc-hooks")
(load "vc/ediff-hook")
(if (fboundp 'x-show-tip) (load "tooltip"))

;If you want additional libraries to be preloaded and their
;doc strings kept in the DOC file rather than in core,
;you may load them with a "site-load.el" file.
;But you must also cause them to be scanned when the DOC file
;is generated.
;For other systems, you must edit ../src/Makefile.in.
(load "site-load" t)

;; Determine which last version number to use
;; based on the executables that now exist.
(if (and (or (equal (nth 3 command-line-args) "dump")
	     (equal (nth 4 command-line-args) "dump"))
	 (not (eq system-type 'ms-dos)))
    (let* ((base (concat "emacs-" emacs-version "."))
	   (files (file-name-all-completions base default-directory))
	   (versions (mapcar (function (lambda (name)
					 (string-to-number (substring name (length base)))))
			     files)))
      ;; `emacs-version' is a constant, so we shouldn't change it with `setq'.
      (defconst emacs-version
	(format "%s.%d"
		emacs-version (if versions (1+ (apply 'max versions)) 1)))))


(message "Finding pointers to doc strings...")
(if (or (equal (nth 3 command-line-args) "dump")
	(equal (nth 4 command-line-args) "dump"))
    (let ((name emacs-version))
      (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	(setq name (concat (downcase (substring name 0 (match-beginning 0)))
			   "-"
			   (substring name (match-end 0)))))
      (if (memq system-type '(ms-dos windows-nt))
	  (setq name (expand-file-name
		      (if (fboundp 'x-create-frame) "DOC-X" "DOC") "../etc"))
	(setq name (concat (expand-file-name "../etc/DOC-") name))
	(if (file-exists-p name)
	    (delete-file name))
	(copy-file (expand-file-name "../etc/DOC") name t))
      (Snarf-documentation (file-name-nondirectory name)))
    (condition-case nil
	(Snarf-documentation "DOC")
      (error nil)))
(message "Finding pointers to doc strings...done")

;; Note: You can cause additional libraries to be preloaded
;; by writing a site-init.el that loads them.
;; See also "site-load" above.
(load "site-init" t)
(setq current-load-list nil)

;; We keep the load-history data in PURE space.
;; Make sure that the spine of the list is not in pure space because it can
;; be destructively mutated in lread.c:build_load_history.
(setq load-history (mapcar 'purecopy load-history))

(set-buffer-modified-p nil)

;; reset the load-path.  See lread.c:init_lread why.
(if (or (equal (nth 3 command-line-args) "bootstrap")
	(equal (nth 4 command-line-args) "bootstrap"))
    (setcdr load-path nil))

(remove-hook 'after-load-functions (lambda (f) (garbage-collect)))

(setq inhibit-load-charset-map nil)
(clear-charset-maps)
(garbage-collect)

;; At this point, we're ready to resume undo recording for scratch.
(buffer-enable-undo "*scratch*")

;; Avoid error if user loads some more libraries now and make sure the
;; hash-consing hash table is GC'd.
(setq purify-flag nil)

(if (null (garbage-collect))
    (setq pure-space-overflow t))

(if (or (member (nth 3 command-line-args) '("dump" "bootstrap"))
	(member (nth 4 command-line-args) '("dump" "bootstrap")))
    (progn
      (if (memq system-type '(ms-dos windows-nt cygwin))
          (message "Dumping under the name emacs")
        (message "Dumping under the name emacs"))
      (condition-case ()
	  (delete-file "emacs")
	(file-error nil))
      ;; We used to dump under the name xemacs, but that occasionally
      ;; confused people installing Emacs (they'd install the file
      ;; under the name `xemacs'), and it's inconsistent with every
      ;; other GNU program's build process.
      (dump-emacs "emacs" "temacs")
      (message "%d pure bytes used" pure-bytes-used)
      ;; Recompute NAME now, so that it isn't set when we dump.
      (if (not (or (memq system-type '(ms-dos windows-nt))
                   ;; Don't bother adding another name if we're just
                   ;; building bootstrap-emacs.
                   (equal (nth 3 command-line-args) "bootstrap")
                   (equal (nth 4 command-line-args) "bootstrap")))
	  (let ((name (concat "emacs-" emacs-version)))
	    (while (string-match "[^-+_.a-zA-Z0-9]+" name)
	      (setq name (concat (downcase (substring name 0 (match-beginning 0)))
				 "-"
				 (substring name (match-end 0)))))
            (message "Adding name %s" name)
	    (add-name-to-file "emacs" name t)))
      (kill-emacs)))

;; For machines with CANNOT_DUMP defined in config.h,
;; this file must be loaded each time Emacs is run.
;; So run the startup code now.  First, remove `-l loadup' from args.

(if (and (equal (nth 1 command-line-args) "-l")
	 (equal (nth 2 command-line-args) "loadup"))
    (setcdr command-line-args (nthcdr 3 command-line-args)))

(eval top-level)


;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:

;;; loadup.el ends here
