;;; gnus-dired.el --- utility functions where gnus and dired meet

;; Copyright (C) 1996-1999, 2001-2012  Free Software Foundation, Inc.

;; Authors: Benjamin Rutt <brutt@bloomington.in.us>,
;;          Shenghuo Zhu <zsh@cs.rochester.edu>
;; Keywords: mail, news, extensions

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

;; This package provides utility functions for intersections of gnus
;; and dired.  To enable the gnus-dired-mode minor mode which will
;; have the effect of installing keybindings in dired-mode, place the
;; following in your ~/.gnus:

;; (require 'gnus-dired) ;, isn't needed due to autoload cookies
;; (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Note that if you visit dired buffers before your ~/.gnus file has
;; been read, those dired buffers won't have the keybindings in
;; effect.  To get around that problem, you may want to add the above
;; statements to your ~/.emacs instead.

;;; Code:

(eval-when-compile
  (when (featurep 'xemacs)
    (require 'easy-mmode))) ; for `define-minor-mode'
(require 'dired)
(autoload 'mml-attach-file "mml")
(autoload 'mm-default-file-encoding "mm-decode");; Shift this to `mailcap.el'?
(autoload 'mailcap-extension-to-mime "mailcap")
(autoload 'mailcap-mime-info "mailcap")

;; Maybe shift this function to `mailcap.el'?
(autoload 'mm-mailcap-command "mm-decode")

(autoload 'ps-print-preprint "ps-print")

;; Autoloads to avoid byte-compiler warnings.  These are used only if the user
;; customizes `gnus-dired-mail-mode' to use Message and/or Gnus.
(autoload 'message-buffers "message")
(autoload 'gnus-print-buffer "gnus-sum")

(defvar gnus-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-m\C-a" 'gnus-dired-attach)
    (define-key map "\C-c\C-m\C-l" 'gnus-dired-find-file-mailcap)
    (define-key map "\C-c\C-m\C-p" 'gnus-dired-print)
    map))

;; FIXME: Make it customizable, change the default to `mail-user-agent' when
;; this file is renamed (e.g. to `dired-mime.el').

(defcustom gnus-dired-mail-mode 'gnus-user-agent ;; mail-user-agent
  "Your preference for a mail composition package.
See `mail-user-agent' for more information."
  :group 'mail ;; dired?
  :version "23.1" ;; No Gnus
  :type '(radio (function-item :tag "Default Emacs mail"
			       :format "%t\n"
			       sendmail-user-agent)
		(function-item :tag "Emacs interface to MH"
			       :format "%t\n"
			       mh-e-user-agent)
		(function-item :tag "Gnus Message package"
			       :format "%t\n"
			       message-user-agent)
		(function-item :tag "Gnus Message with full Gnus features"
			       :format "%t\n"
			       gnus-user-agent)
		(function :tag "Other")))

(eval-when-compile
  (when (featurep 'xemacs)
    (defvar gnus-dired-mode-hook)
    (defvar gnus-dired-mode-on-hook)
    (defvar gnus-dired-mode-off-hook)))

(define-minor-mode gnus-dired-mode
  "Minor mode for intersections of gnus and dired.

\\{gnus-dired-mode-map}"
  :keymap gnus-dired-mode-map
  (unless (derived-mode-p 'dired-mode)
    (setq gnus-dired-mode nil)))

;;;###autoload
(defun turn-on-gnus-dired-mode ()
  "Convenience method to turn on gnus-dired-mode."
  (interactive)
  (gnus-dired-mode 1))

(defun gnus-dired-mail-buffers ()
  "Return a list of active mail composition buffers."
  (if (and (memq gnus-dired-mail-mode '(message-user-agent gnus-user-agent))
	   (require 'message)
	   (fboundp 'message-buffers))
      (message-buffers)
    ;; Cf. `message-buffers' in `message.el':
    (let (buffers)
      (save-excursion
	(dolist (buffer (buffer-list t))
	  (set-buffer buffer)
	  (when (eq major-mode 'mail-mode)
	    (push (buffer-name buffer) buffers))))
      (nreverse buffers))))

(autoload 'gnus-completing-read "gnus-util")

;; Method to attach files to a mail composition.
(defun gnus-dired-attach (files-to-attach)
  "Attach dired's marked files to a gnus message composition.
If called non-interactively, FILES-TO-ATTACH should be a list of
filenames."
  (interactive
   (list
    (delq nil
	  (mapcar
	   ;; don't attach directories
	   (lambda (f) (if (file-directory-p f) nil f))
	   (nreverse
	    (let ((arg nil)) ;; Silence XEmacs 21.5 when compiling.
	      (dired-map-over-marks (dired-get-filename) arg)))))))
  (let ((destination nil)
	(files-str nil)
	(bufs nil))
    ;; warn if user tries to attach without any files marked
    (if (null files-to-attach)
	(error "No files to attach")
      (setq files-str
	    (mapconcat
	     (lambda (f) (file-name-nondirectory f))
	     files-to-attach ", "))
      (setq bufs (gnus-dired-mail-buffers))

      ;; set up destination mail composition buffer
      (if (and bufs
	       (y-or-n-p "Attach files to existing mail composition buffer? "))
	  (setq destination
		(if (= (length bufs) 1)
		    (get-buffer (car bufs))
		  (gnus-completing-read "Attach to which mail composition buffer"
                                         bufs t)))
	;; setup a new mail composition buffer
	(let ((mail-user-agent gnus-dired-mail-mode)
	      ;; A workaround to prevent Gnus from displaying the Gnus
	      ;; logo when invoking this command without loading Gnus.
	      ;; Gnus demonstrates it when gnus.elc is being loaded if
	      ;; a command of which the name is prefixed with "gnus"
	      ;; causes that autoloading.  See the code in question,
	      ;; that is the one first found in gnus.el by performing
	      ;; `C-s this-command'.
	      (this-command (if (eq gnus-dired-mail-mode 'gnus-user-agent)
				'gnoose-dired-attach
			      this-command)))
	  (compose-mail))
	(setq destination (current-buffer)))

      ;; set buffer to destination buffer, and attach files
      (set-buffer destination)
      (goto-char (point-max))		;attach at end of buffer
      (while files-to-attach
	(mml-attach-file (car files-to-attach)
			 (or (mm-default-file-encoding (car files-to-attach))
			     "application/octet-stream") nil)
	(setq files-to-attach (cdr files-to-attach)))
      (message "Attached file(s) %s" files-str))))

(autoload 'mailcap-parse-mailcaps "mailcap" "" t)

(defun gnus-dired-find-file-mailcap (&optional file-name arg)
  "In dired, visit FILE-NAME according to the mailcap file.
If ARG is non-nil, open it in a new buffer."
  (interactive (list
		(file-name-sans-versions (dired-get-filename) t)
		current-prefix-arg))
  (mailcap-parse-mailcaps)
  (if (file-exists-p file-name)
      (let (mime-type method)
	(if (and (not arg)
		 (not (file-directory-p file-name))
		 (string-match "\\.[^\\.]+$" file-name)
		 (setq mime-type
		       (mailcap-extension-to-mime
			(match-string 0 file-name)))
		 (stringp
		  (setq method
			(cdr (assoc 'viewer
				    (car (mailcap-mime-info mime-type
							    'all
							    'no-decode)))))))
	    (let ((view-command (mm-mailcap-command method file-name nil)))
	      (message "viewing via %s" view-command)
	      (start-process "*display*"
			     nil
			     shell-file-name
			     shell-command-switch
			     view-command))
	  (find-file file-name)))
    (if (file-symlink-p file-name)
	(error "File is a symlink to a nonexistent target")
      (error "File no longer exists; type `g' to update Dired buffer"))))

(defun gnus-dired-print (&optional file-name print-to)
  "In dired, print FILE-NAME according to the mailcap file.

If there is no print command, print in a PostScript image. If the
optional argument PRINT-TO is nil, send the image to the printer. If
PRINT-TO is a string, save the PostScript image in a file with that
name.  If PRINT-TO is a number, prompt the user for the name of the
file to save in."
  (interactive (list
		(file-name-sans-versions (dired-get-filename) t)
		(ps-print-preprint current-prefix-arg)))
  (mailcap-parse-mailcaps)
  (cond
   ((file-directory-p file-name)
    (error "Can't print a directory"))
   ((file-exists-p file-name)
    (let (mime-type method)
      (if (and (string-match "\\.[^\\.]+$" file-name)
	       (setq mime-type
		     (mailcap-extension-to-mime
		      (match-string 0 file-name)))
	       (stringp
		(setq method (mailcap-mime-info mime-type "print"
						'no-decode))))
	  (call-process shell-file-name nil
			(generate-new-buffer " *mm*")
			nil
			shell-command-switch
			(mm-mailcap-command method file-name mime-type))
	(with-temp-buffer
	  (insert-file-contents file-name)
	  (if (eq gnus-dired-mail-mode 'gnus-user-agent)
	      (gnus-print-buffer)
	    ;; FIXME:
	    (error "MIME print only implemented via Gnus")))
	(ps-despool print-to))))
   ((file-symlink-p file-name)
     (error "File is a symlink to a nonexistent target"))
    (t
     (error "File no longer exists; type `g' to update Dired buffer"))))

(provide 'gnus-dired)

;;; gnus-dired.el ends here
