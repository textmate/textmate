;;; url-handlers.el --- file-name-handler stuff for URL loading

;; Copyright (C) 1996-1999, 2004-2012  Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

;; This file is part of GNU Emacs.
;;
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

;; (require 'url)
(require 'url-parse)
;; (require 'url-util)
(eval-when-compile (require 'mm-decode))
;; (require 'mailcap)
;; The following functions in the byte compiler's warnings are known not
;; to cause any real problem for the following reasons:
;; - mm-save-part-to-file, mm-destroy-parts: always used
;;   after mm-dissect-buffer and defined in the same file.
;; The following are autoloaded instead of `require'd to avoid eagerly
;; loading all of URL when turning on url-handler-mode in the .emacs.
(autoload 'url-expand-file-name "url-expand" "Convert url to a fully specified url, and canonicalize it.")
(autoload 'mm-dissect-buffer "mm-decode" "Dissect the current buffer and return a list of MIME handles.")
(autoload 'url-scheme-get-property "url-methods" "Get property of a URL SCHEME.")

;; Implementation status
;; ---------------------
;; Function				Status
;; ------------------------------------------------------------
;; add-name-to-file			Needs DAV Bindings
;; copy-file				Broken (assumes 1st item is URL)
;; delete-directory			Finished (DAV)
;; delete-file				Finished (DAV)
;; diff-latest-backup-file
;; directory-file-name			unnecessary (what about VMS)?
;; directory-files			Finished (DAV)
;; dired-call-process
;; dired-compress-file
;; dired-uncache
;; expand-file-name			Finished
;; file-accessible-directory-p
;; file-attributes			Finished, better with DAV
;; file-directory-p			Needs DAV, finished
;; file-executable-p			Finished
;; file-exists-p			Finished
;; file-local-copy
;; file-modes
;; file-name-all-completions		Finished (DAV)
;; file-name-as-directory
;; file-name-completion			Finished (DAV)
;; file-name-directory
;; file-name-nondirectory
;; file-name-sans-versions		why?
;; file-newer-than-file-p
;; file-ownership-preserved-p		No way to know
;; file-readable-p			Finished
;; file-regular-p			!directory_p
;; file-remote-p			Finished
;; file-symlink-p			Needs DAV bindings
;; file-truename			Needs DAV bindings
;; file-writable-p			Check for LOCK?
;; find-backup-file-name		why?
;; get-file-buffer			why?
;; insert-directory			Use DAV
;; insert-file-contents			Finished
;; load
;; make-directory			Finished (DAV)
;; make-symbolic-link			Needs DAV bindings
;; rename-file				Finished (DAV)
;; set-file-modes			Use mod_dav specific executable flag?
;; set-visited-file-modtime		Impossible?
;; shell-command			Impossible?
;; unhandled-file-name-directory
;; vc-registered			Finished (DAV)
;; verify-visited-file-modtime
;; write-region

(defvar url-handler-regexp
  "\\`\\(https?\\|ftp\\|file\\|nfs\\)://"
  "*A regular expression for matching URLs handled by `file-name-handler-alist'.
Some valid URL protocols just do not make sense to visit interactively
\(about, data, info, irc, mailto, etc\).  This regular expression
avoids conflicts with local files that look like URLs \(Gnus is
particularly bad at this\).")

;;;###autoload
(define-minor-mode url-handler-mode
  "Toggle using `url' library for URL filenames (URL Handler mode).
With a prefix argument ARG, enable URL Handler mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  :global t :group 'url
  (if (not (boundp 'file-name-handler-alist))
      ;; Can't be turned ON anyway.
      (setq url-handler-mode nil)
    ;; Remove old entry, if any.
    (setq file-name-handler-alist
	  (delq (rassq 'url-file-handler file-name-handler-alist)
		file-name-handler-alist))
    (if url-handler-mode
	(push (cons url-handler-regexp 'url-file-handler)
	      file-name-handler-alist))))

(defun url-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers (cons 'url-file-handler
					  (if (eq operation inhibit-file-name-operation)
					      inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

;;;###autoload
(defun url-file-handler (operation &rest args)
  "Function called from the `file-name-handler-alist' routines.
OPERATION is what needs to be done (`file-exists-p', etc).  ARGS are
the arguments that would have been passed to OPERATION."
  (let ((fn (or (get operation 'url-file-handlers)
		(intern-soft (format "url-%s" operation))))
	(val nil)
	(hooked nil))
    (if (and fn (fboundp fn))
	(setq hooked t
	      val (save-match-data (apply fn args)))
      (setq hooked nil
	    val (url-run-real-handler operation args)))
    (url-debug 'handlers "%s %S%S => %S" (if hooked "Hooked" "Real")
	       operation args val)
    val))

(defun url-file-handler-identity (&rest args)
  ;; Identity function
  (car args))

;; These are operations that we can fully support
(put 'file-readable-p 'url-file-handlers 'url-file-exists-p)
(put 'substitute-in-file-name 'url-file-handlers 'url-file-handler-identity)
(put 'file-name-absolute-p 'url-file-handlers (lambda (&rest ignored) t))
(put 'expand-file-name 'url-file-handlers 'url-handler-expand-file-name)
(put 'directory-file-name 'url-file-handlers 'url-handler-directory-file-name)
(put 'unhandled-file-name-directory 'url-file-handlers 'url-handler-unhandled-file-name-directory)
(put 'file-remote-p 'url-file-handlers 'url-handler-file-remote-p)
;; (put 'file-name-as-directory 'url-file-handlers 'url-handler-file-name-as-directory)

;; These are operations that we do not support yet (DAV!!!)
(put 'file-writable-p 'url-file-handlers 'ignore)
(put 'file-symlink-p 'url-file-handlers 'ignore)
;; Just like for ange-ftp: let's not waste time trying to look for RCS/foo,v
;; files and such since we can't do anything clever with them anyway.
(put 'vc-registered 'url-file-handlers 'ignore)

(defun url-handler-expand-file-name (file &optional base)
  ;; When we see "/foo/bar" in a file whose working dir is "http://bla/bla",
  ;; there are two interpretations possible: either it's a local "/foo/bar"
  ;; or it's "http:/bla/foo/bar".  When working with URLs, the second
  ;; interpretation is the right one, but when working with Emacs file
  ;; names, the first is preferred.
  (if (file-name-absolute-p file)
      (expand-file-name file "/")
    (url-expand-file-name file base)))

;; directory-file-name and file-name-as-directory are kind of hard to
;; implement really right for URLs since URLs can have repeated / chars.
;; We'd want the following behavior:
;; idempotence: (d-f-n (d-f-n X) == (d-f-n X)
;; idempotence: (f-n-a-d (f-n-a-d X) == (f-n-a-d X)
;; reversible:  (d-f-n (f-n-a-d (d-f-n X))) == (d-f-n X)
;; reversible:  (f-n-a-d (d-f-n (f-n-a-d X))) == (f-n-a-d X)
(defun url-handler-directory-file-name (dir)
  ;; When there's more than a single /, just don't touch the slashes at all.
  (if (string-match "//\\'" dir) dir
    (url-run-real-handler 'directory-file-name (list dir))))

(defun url-handler-unhandled-file-name-directory (filename)
  (let ((url (url-generic-parse-url filename)))
    (if (equal (url-type url) "file")
        ;; `file' URLs are actually local.  The filename part may be ""
        ;; which really stands for "/".
        ;; FIXME: maybe we should check that the host part is "" or "localhost"
        ;; or some name that represents the local host?
        (or (file-name-directory (url-filename url)) "/")
      ;; All other URLs are not expected to be directly accessible from
      ;; a local process.
      nil)))

(defun url-handler-file-remote-p (filename &optional identification connected)
  (let ((url (url-generic-parse-url filename)))
    (if (and (url-type url) (not (equal (url-type url) "file")))
	;; Maybe we can find a suitable check for CONNECTED.  For now,
	;; we ignore it.
	(cond
	 ((eq identification 'method) (url-type url))
	 ((eq identification 'user) (url-user url))
	 ((eq identification 'host) (url-host url))
	 ((eq identification 'localname) (url-filename url))
	 (t (url-recreate-url
	     (url-parse-make-urlobj (url-type url) (url-user url) nil
				    (url-host url) (url-port url)))))
      ;; If there is no URL type, or it is a "file://" URL, the
      ;; filename is expected to be non remote.  A more subtle check
      ;; for "file://" URLs could be applied, as said in
      ;; `url-handler-unhandled-file-name-directory'.
      nil)))

;; The actual implementation
;;;###autoload
(defun url-copy-file (url newname &optional ok-if-already-exists
			  keep-time preserve-uid-gid)
  "Copy URL to NEWNAME.  Both args must be strings.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists.
This is what happens in interactive use with M-x.
Fourth arg KEEP-TIME non-nil means give the new file the same
last-modified time as the old one.  (This works on only some systems.)
Fifth arg PRESERVE-UID-GID is ignored.
A prefix arg makes KEEP-TIME non-nil."
  (if (and (file-exists-p newname)
	   (not ok-if-already-exists))
      (error "Opening output file: File already exists, %s" newname))
  (let ((buffer (url-retrieve-synchronously url))
	(handle nil))
    (if (not buffer)
	(error "Opening input file: No such file or directory, %s" url))
    (with-current-buffer buffer
      (setq handle (mm-dissect-buffer t)))
    (mm-save-part-to-file handle newname)
    (kill-buffer buffer)
    (mm-destroy-parts handle)))

;;;###autoload
(defun url-file-local-copy (url &rest ignored)
  "Copy URL into a temporary file on this machine.
Returns the name of the local copy, or nil, if FILE is directly
accessible."
  (let ((filename (make-temp-file "url")))
    (url-copy-file url filename 'ok-if-already-exists)
    filename))

(defun url-insert (buffer &optional beg end)
  "Insert the body of a URL object.
BUFFER should be a complete URL buffer as returned by `url-retrieve'.
If the headers specify a coding-system, it is applied to the body before it is inserted.
Returns a list of the form (SIZE CHARSET), where SIZE is the size in bytes
of the inserted text and CHARSET is the charset that was specified in the header,
or nil if none was found.
BEG and END can be used to only insert a subpart of the body.
They count bytes from the beginning of the body."
  (let* ((handle (with-current-buffer buffer (mm-dissect-buffer t)))
         (data (with-current-buffer (mm-handle-buffer handle)
                 (if beg
                     (buffer-substring (+ (point-min) beg)
                                       (if end (+ (point-min) end) (point-max)))
		   (buffer-string))))
         (charset (mail-content-type-get (mm-handle-type handle)
                                          'charset)))
    (mm-destroy-parts handle)
    (if charset
        (insert (mm-decode-string data (mm-charset-to-coding-system charset)))
      (insert data))
    (list (length data) charset)))

;;;###autoload
(defun url-insert-file-contents (url &optional visit beg end replace)
  (let ((buffer (url-retrieve-synchronously url)))
    (if (not buffer)
	(error "Opening input file: No such file or directory, %s" url))
    (if visit (setq buffer-file-name url))
    (save-excursion
      (let* ((start (point))
             (size-and-charset (url-insert buffer beg end)))
        (kill-buffer buffer)
        (when replace
          (delete-region (point-min) start)
          (delete-region (point) (point-max)))
        (unless (cadr size-and-charset)
          ;; If the headers don't specify any particular charset, use the
          ;; usual heuristic/rules that we apply to files.
          (decode-coding-inserted-region start (point) url visit beg end replace))
        (list url (car size-and-charset))))))

(defun url-file-name-completion (url directory &optional predicate)
  (error "Unimplemented"))

(defun url-file-name-all-completions (file directory)
  (error "Unimplemented"))

;; All other handlers map onto their respective backends.
(defmacro url-handlers-create-wrapper (method args)
  `(defun ,(intern (format "url-%s" method)) ,args
     ,(format "URL file-name-handler wrapper for `%s' call.\n---\n%s" method
              (or (documentation method t) "No original documentation."))
     (setq url (url-generic-parse-url url))
     (when (url-type url)
       (funcall (url-scheme-get-property (url-type url) (quote ,method))
                ,@(remove '&rest (remove '&optional args))))))

(url-handlers-create-wrapper file-exists-p (url))
(url-handlers-create-wrapper file-attributes (url &optional id-format))
(url-handlers-create-wrapper file-symlink-p (url))
(url-handlers-create-wrapper file-writable-p (url))
(url-handlers-create-wrapper file-directory-p (url))
(url-handlers-create-wrapper file-executable-p (url))
(url-handlers-create-wrapper directory-files (url &optional full match nosort))
(url-handlers-create-wrapper file-truename (url &optional counter prev-dirs))

(add-hook 'find-file-hook 'url-handlers-set-buffer-mode)

(defun url-handlers-set-buffer-mode ()
  "Set correct modes for the current buffer if visiting a remote file."
  (and (stringp buffer-file-name)
       (string-match url-handler-regexp buffer-file-name)
       (auto-save-mode 0)))

(provide 'url-handlers)

;;; url-handlers.el ends here
