;;; ede-system.el --- EDE working with the system (VC, FTP, ETC)

;; Copyright (C) 2001-2003, 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make, vc

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
;; EDE system contains some routines to work with EDE projects saved in
;; CVS repositories, and services such as sourceforge which lets you
;; perform releases via FTP.

(require 'ede)

;;; Code:

;;; Web/FTP site node.

;;;###autoload
(defun ede-web-browse-home ()
  "Browse the home page of the current project."
  (interactive)
  (if (not (ede-toplevel))
      (error "No project"))
  (let ((home (oref (ede-toplevel) web-site-url)))
    (if (string= "" home)
	(error "Now URL is stored in this project"))
    (require 'browse-url)
    (browse-url home)
    ))

;;;###autoload
(defun ede-edit-web-page ()
  "Edit the web site for this project."
  (interactive)
  (let* ((toplevel (ede-toplevel))
	 (dir (oref toplevel web-site-directory))
	 (file (oref toplevel web-site-file))
	 (endfile (concat (file-name-as-directory dir) file)))
    (if (string-match "^/r[:@]" endfile)
	(require 'tramp))
    (when (not (file-exists-p endfile))
      (setq endfile file)
      (if (string-match "^/r[:@]" endfile)
	  (require 'tramp))
      (if (not (file-exists-p endfile))
	  (error "No project file found")))
    (find-file endfile)))

;;;###autoload
(defun ede-upload-distribution ()
  "Upload the current distribution to the correct location.
Use /user@ftp.site.com: file names for FTP sites.
Download tramp, and use /r:machine: for names on remote sites w/out FTP access."
  (interactive)
  (let* ((files (project-dist-files (ede-toplevel)))
	 (upload (if (string= (oref (ede-toplevel) ftp-upload-site) "")
		     (oref (ede-toplevel) ftp-site)
		   (oref (ede-toplevel) ftp-upload-site))))
    (when (or (string= upload "")
	      (not (file-exists-p upload)))
      (error "Upload directory %S does not exist" upload))
    (while files
      (let ((localfile (concat (file-name-directory (oref (ede-toplevel) file))
			       (car files))))
	(if (not (file-exists-p localfile))
	    (progn
	      (message "File %s does not exist yet.  Building a distribution"
		       localfile)
	      (ede-make-dist)
	      (error "File %s does not exist yet.  Building a distribution"
		     localfile)
	      ))
	(setq upload
	      (concat (directory-file-name upload)
		      "/"
		      (file-name-nondirectory localfile)))
	(copy-file localfile upload)
	(setq files (cdr files)))))
  (message "Done uploading files...")
  )

;;;###autoload
(defun ede-upload-html-documentation ()
  "Upload the current distributions documentation as HTML.
Use /user@ftp.site.com: file names for FTP sites.
Download tramp, and use /r:machine: for names on remote sites w/out FTP access."
  (interactive)
  (let* ((files nil) ;(ede-html-doc-files (ede-toplevel)))
	 (upload (if (string= (oref (ede-toplevel) ftp-upload-site) "")
		     (oref (ede-toplevel) ftp-site)
		   (oref (ede-toplevel) ftp-upload-site))))
    (when (or (string= upload "")
	      (not (file-exists-p upload)))
      (error "Upload directory %S does not exist" upload))
    (while files
      (let ((localfile (concat (file-name-directory (oref (ede-toplevel) file))
			       (car files))))
	(if (not (file-exists-p localfile))
	    (progn
	      (message "File %s does not exist yet.  Building a distribution"
		       localfile)
	      ;;(project-compile-target ... )
	      (error "File %s does not exist yet.  Building a distribution"
		     localfile)
	      ))
	(copy-file localfile upload)
	(setq files (cdr files)))))
  (message "Done uploading files...")
  )

;;; Version Control
;;
;; Do a few nice things with Version control systems.

;;;###autoload
(defun ede-vc-project-directory ()
  "Run `vc-dir' on the current project."
  (interactive)
  (let ((top (ede-toplevel-project-or-nil default-directory)))
    (vc-dir top nil)))

(provide 'ede/system)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/system"
;; End:

;;; ede/system.el ends here
