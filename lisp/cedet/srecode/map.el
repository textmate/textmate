;;; srecode/map.el --- Manage a template file map

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; Read template files, and build a map of where they can be found.
;; Save the map to disk, and refer to it when bootstrapping a new
;; Emacs session with srecode.

(require 'semantic)
(require 'eieio-base)
(require 'srecode)

;;; Code:

;; The defcustom is given at the end of the file.
(defvar srecode-map-load-path)

(defun srecode-map-base-template-dir ()
  "Find the base template directory for SRecode."
  (expand-file-name "srecode" data-directory))

;;; Current MAP
;;

(defvar srecode-current-map nil
  "The current map for global SRecode templates.")

(defcustom srecode-map-save-file
  (locate-user-emacs-file "srecode-map.el" ".srecode/srecode-map")
  "The save location for SRecode's map file.
If the save file is nil, then the MAP is not saved between sessions."
  :group 'srecode
  :type 'file)

(defclass srecode-map (eieio-persistent)
  ((fileheaderline :initform ";; SRECODE TEMPLATE MAP")
   (files :initarg :files
	  :initform nil
	  :type list
	  :documentation
	  "An alist of files and the major-mode that they cover.")
   (apps :initarg :apps
	 :initform nil
	 :type list
	 :documentation
	 "An alist of applications.
Each app keys to an alist of files and modes (as above.)")
   )
  "A map of srecode templates.")

(defmethod srecode-map-entry-for-file ((map srecode-map) file)
  "Return the entry in MAP for FILE."
  (assoc file (oref map files)))

(defmethod srecode-map-entries-for-mode ((map srecode-map) mode)
  "Return the entries in MAP for major MODE."
  (let ((ans nil))
    (dolist (f (oref map files))
      (when (mode-local-use-bindings-p mode (cdr f))
	(setq ans (cons f ans))))
    ans))

(defmethod srecode-map-entry-for-app ((map srecode-map) app)
  "Return the entry in MAP for APP'lication."
  (assoc app (oref map apps))
  )

(defmethod srecode-map-entries-for-app-and-mode ((map srecode-map) app mode)
  "Return the entries in MAP for major MODE."
  (let ((ans nil)
	(appentry (srecode-map-entry-for-app map app)))
    (dolist (f (cdr appentry))
      (when (eq (cdr f) mode)
	(setq ans (cons f ans))))
    ans))

(defmethod srecode-map-entry-for-file-anywhere ((map srecode-map) file)
  "Search in all entry points in MAP for FILE.
Return a list ( APP . FILE-ASSOC ) where APP is nil
in the global map."
  (or
   ;; Look in the global entry
   (let ((globalentry (srecode-map-entry-for-file map file)))
     (when globalentry
       (cons nil globalentry)))
   ;; Look in each app.
   (let ((match nil))
     (dolist (app (oref map apps))
       (let ((appmatch (assoc file (cdr app))))
	 (when appmatch
	   (setq match (cons app appmatch)))))
     match)
   ;; Other?
   ))

(defmethod srecode-map-delete-file-entry ((map srecode-map) file)
  "Update MAP to exclude FILE from the file list."
  (let ((entry (srecode-map-entry-for-file map file)))
    (when entry
      (object-remove-from-list map 'files entry))))

(defmethod srecode-map-update-file-entry ((map srecode-map) file mode)
  "Update a MAP entry for FILE to be used with MODE.
Return non-nil if the MAP was changed."
  (let ((entry (srecode-map-entry-for-file map file))
	(dirty t))
    (cond
     ;; It is already a match.. do nothing.
     ((and entry (eq (cdr entry) mode))
      (setq dirty nil))
     ;; We have a non-matching entry.  Change the cdr.
     (entry
      (setcdr entry mode))
     ;; No entry, just add it to the list.
     (t
      (object-add-to-list map 'files (cons file mode))
      ))
    dirty))

(defmethod srecode-map-delete-file-entry-from-app ((map srecode-map) file app)
  "Delete from MAP the FILE entry within the APP'lication."
  (let* ((appe (srecode-map-entry-for-app map app))
	 (fentry (assoc file (cdr appe))))
    (setcdr appe (delete fentry (cdr appe))))
  )

(defmethod srecode-map-update-app-file-entry ((map srecode-map) file mode app)
  "Update the MAP entry for FILE to be used with MODE within APP.
Return non-nil if the map was changed."
  (let* ((appentry (srecode-map-entry-for-app map app))
	 (appfileentry (assoc file (cdr appentry)))
	 (dirty t)
	 )
    (cond
     ;; Option 1 - We have this file in this application already
     ;;            with the correct mode.
     ((and appfileentry (eq (cdr appfileentry) mode))
      (setq dirty nil)
      )
     ;; Option 2 - We have a non-matching entry.  Change Cdr.
     (appfileentry
      (setcdr appfileentry mode))
     (t
      ;; For option 3 & 4 - remove the entry from any other lists
      ;; we can find.
      (let ((any (srecode-map-entry-for-file-anywhere map file)))
	(when any
	  (if (null (car any))
	      ;; Global map entry
	      (srecode-map-delete-file-entry map file)
	    ;; Some app
	    (let ((appentry (srecode-map-entry-for-app map app)))
	      (setcdr appentry (delete (cdr any) (cdr appentry))))
	  )))
      ;; Now do option 3 and 4
      (cond
       ;; Option 3 - No entry for app.  Add to the list.
       (appentry
	(setcdr appentry (cons (cons file mode) (cdr appentry)))
	)
       ;; Option 4 - No app entry.  Add app to list with this file.
       (t
	(object-add-to-list map 'apps (list app (cons file mode)))
	)))
     )
    dirty))


;;; MAP Updating
;;
;;;###autoload
(defun srecode-get-maps (&optional reset)
  "Get a list of maps relevant to the current buffer.
Optional argument RESET forces a reset of the current map."
  (interactive "P")
  ;; Always update the map, but only do a full reset if
  ;; the user asks for one.
  (srecode-map-update-map (not reset))

  (if (called-interactively-p 'any)
      ;; Dump this map.
      (with-output-to-temp-buffer "*SRECODE MAP*"
	(princ "   -- SRecode Global map --\n")
	(srecode-maps-dump-file-list (oref srecode-current-map files))
	(princ "\n   -- Application Maps --\n")
	(dolist (ap (oref srecode-current-map apps))
	  (let ((app (car ap))
		(files (cdr ap)))
	    (princ app)
	    (princ " :\n")
	    (srecode-maps-dump-file-list files))
	  (princ "\n"))
	(princ "\nUse:\n\n M-x customize-variable RET srecode-map-load-path RET")
	(princ "\n To change the path where SRecode loads templates from.")
	)
    ;; Eventually, I want to return many maps to search through.
    (list srecode-current-map)))

(eval-when-compile (require 'data-debug))

(defun srecode-adebug-maps ()
  "Run ADEBUG on the output of `srecode-get-maps'."
  (interactive)
  (require 'data-debug)
  (let ((start (current-time))
	(p (srecode-get-maps t)) ;; Time the reset.
	(end (current-time))
	)
    (message "Updating the map took %.2f seconds."
	     (semantic-elapsed-time start end))
    (data-debug-new-buffer "*SRECODE ADEBUG*")
    (data-debug-insert-stuff-list p "*")))

(defun srecode-maps-dump-file-list (flist)
  "Dump a file list FLIST to `standard-output'."
  (princ "Mode\t\t\tFilename\n")
  (princ "------\t\t\t------------------\n")
  (dolist (fe flist)
    (prin1 (cdr fe))
    (princ "\t")
    (when (> (* 2 8) (length (symbol-name (cdr fe))))
      (princ "\t"))
    (when (> 8 (length (symbol-name (cdr fe))))
      (princ "\t"))
    (princ (car fe))
    (princ "\n")
    ))

(defun srecode-map-file-still-valid-p (filename map)
  "Return t if FILENAME should be in MAP still."
  (let ((valid nil))
    (and (file-exists-p filename)
	 (progn
	   (dolist (p srecode-map-load-path)
	     (when (and (< (length p) (length filename))
			(string= p (substring filename 0 (length p))))
	       (setq valid t))
	     )
	   valid))
    ))

(defun srecode-map-update-map (&optional fast)
  "Update the current map from `srecode-map-load-path'.
Scans all the files on the path, and makes sure we have entries
for them.
If option FAST is non-nil, then only parse a file for the mode-string
if that file is NEW, otherwise assume the mode has not changed."
  (interactive)

  ;; When no map file, we are configured to not use a save file.
  (if (not srecode-map-save-file)
      ;; 0) Create a MAP when in no save file mode.
      (when (not srecode-current-map)
	(setq srecode-current-map (srecode-map "SRecode Map"))
	(message "SRecode map created in non-save mode.")
	)

    ;; 1) Do we even have a MAP or save file?
    (when (and (not srecode-current-map)
	       (not (file-exists-p srecode-map-save-file)))
      (when (not (file-exists-p (file-name-directory srecode-map-save-file)))
	;; Only bother with this interactively, not during a build
	;; or test.
	(when (not noninteractive)
	  ;; No map, make the dir?
	  (if (y-or-n-p (format "Create dir %s? "
				(file-name-directory srecode-map-save-file)))
	      (make-directory (file-name-directory srecode-map-save-file))
	    ;; No make, change save file
	    (customize-variable 'srecode-map-save-file)
	    (error "Change your SRecode map file"))))
      ;; Have a dir.  Make the object.
      (setq srecode-current-map
	    (srecode-map "SRecode Map"
			 :file srecode-map-save-file)))

    ;; 2) Do we not have a current map?  If so load.
    (when (not srecode-current-map)
      (condition-case nil
	  (setq srecode-current-map
		(eieio-persistent-read srecode-map-save-file))
	(error
	 ;; There was an error loading the old map.  Create a new one.
	 (setq srecode-current-map
	       (srecode-map "SRecode Map"
			    :file srecode-map-save-file))))
      )

    )

  ;;
  ;; We better have a MAP object now.
  ;;
  (let ((dirty nil))
    ;; 3) - Purge dead files from the file list.
    (dolist (entry (copy-sequence (oref srecode-current-map files)))
      (when (not (srecode-map-file-still-valid-p
		  (car entry) srecode-current-map))
	(srecode-map-delete-file-entry srecode-current-map (car entry))
	(setq dirty t)
	))
    (dolist (app (copy-sequence (oref srecode-current-map apps)))
      (dolist (entry (copy-sequence (cdr app)))
	(when (not (srecode-map-file-still-valid-p
		    (car entry) srecode-current-map))
	  (srecode-map-delete-file-entry-from-app
	   srecode-current-map (car entry) (car app))
	  (setq dirty t)
	  )))
    ;; 4) - Find new files and add them to the map.
    (dolist (dir srecode-map-load-path)
      (when (file-exists-p dir)
	(dolist (f (directory-files dir t "\\.srt$"))
	  (when (and (not (backup-file-name-p f))
		     (not (auto-save-file-name-p f))
		     (file-readable-p f))
	    (let ((fdirty (srecode-map-validate-file-for-mode f fast)))
	      (setq dirty (or dirty fdirty))))
	  )))
    ;; Only do the save if we are dirty, or if we are in an interactive
    ;; Emacs.
    (when (and dirty (not noninteractive)
	       (slot-boundp srecode-current-map :file))
      (eieio-persistent-save srecode-current-map))
    ))

(defun srecode-map-validate-file-for-mode (file fast)
  "Read and validate FILE via the parser.  Return the mode.
Argument FAST implies that the file should not be reparsed if there
is already an entry for it.
Return non-nil if the map changed."
  (when (or (not fast)
	    (not (srecode-map-entry-for-file-anywhere srecode-current-map file)))
    (let ((buff-orig (get-file-buffer file))
	  (dirty nil))
      (save-excursion
	(if buff-orig
	    (set-buffer buff-orig)
	  (set-buffer (get-buffer-create " *srecode-map-tmp*"))
	  (insert-file-contents file nil nil nil t)
	  ;; Force it to be ready to parse.
	  (srecode-template-mode)
	  (let ((semantic-init-hook nil))
	    (semantic-new-buffer-fcn))
	  )

	(semantic-fetch-tags)
	(let* ((mode-tag
		(semantic-find-first-tag-by-name "mode" (current-buffer)))
	       (val nil)
	       (app-tag
		(semantic-find-first-tag-by-name "application" (current-buffer)))
	       (app nil))
	  (if mode-tag
	      (setq val (car (semantic-tag-variable-default mode-tag)))
	    (error "There should be a mode declaration in %s" file))
	  (when app-tag
	    (setq app (car (semantic-tag-variable-default app-tag))))

	  (setq dirty
		(if app
		    (srecode-map-update-app-file-entry srecode-current-map
						       file
						       (read val)
						       (read app))
		  (srecode-map-update-file-entry srecode-current-map
						 file
						 (read val))))
	  )
	)
      dirty)))


;;; THE PATH
;;
;; We need to do this last since the setter needs the above code.

(defun srecode-map-load-path-set (sym val)
  "Set SYM to the new VAL, then update the srecode map."
  (set-default sym val)
  (srecode-map-update-map t))

(defcustom srecode-map-load-path
  (list (srecode-map-base-template-dir)
	(expand-file-name "~/.srecode/")
	)
  "Global load path for SRecode template files."
  :group 'srecode
  :type '(repeat file)
  :set 'srecode-map-load-path-set)

(provide 'srecode/map)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/map"
;; End:

;;; srecode/map.el ends here
