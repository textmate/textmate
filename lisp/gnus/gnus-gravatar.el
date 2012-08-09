;;; gnus-gravatar.el --- Gnus Gravatar support

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: news

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

;;; Code:

(require 'gravatar)
(require 'gnus-art)
(require 'mail-extr) ;; Because of binding `mail-extr-disable-voodoo'.

(defgroup gnus-gravatar nil
  "Gnus Gravatar."
  :group 'gnus-visual)

(defcustom gnus-gravatar-size nil
  "How big should gravatars be displayed.
If nil, default to `gravatar-size'."
  :type 'integer
  :version "24.1"
  :group 'gnus-gravatar)

(defcustom gnus-gravatar-properties '(:ascent center :relief 1)
  "List of image properties applied to Gravatar images."
  :type 'list
  :version "24.1"
  :group 'gnus-gravatar)

(defcustom gnus-gravatar-too-ugly gnus-article-x-face-too-ugly
  "Regexp matching posters whose avatar shouldn't be shown automatically."
  :type '(choice regexp (const nil))
  :version "24.1"
  :group 'gnus-gravatar)

(defun gnus-gravatar-transform-address (header category &optional force)
  (gnus-with-article-headers
    (let* ((mail-extr-disable-voodoo t)
           (mail-extr-ignore-realname-equals-mailbox-name nil)
	   (addresses (mail-extract-address-components
		       (or (mail-fetch-field header) "") t))
	   (gravatar-size (or gnus-gravatar-size gravatar-size))
	   name)
      (dolist (address addresses)
	(when (and (setq name (car address))
		   (string-match "\\` +" name))
	  (setcar address (setq name (substring name (match-end 0)))))
	(when (or force
		  (not (and gnus-gravatar-too-ugly
			    (or (string-match gnus-gravatar-too-ugly
					      (or (cadr address) ""))
				(and name
				     (string-match gnus-gravatar-too-ugly
						   name))))))
	  (ignore-errors
	    (gravatar-retrieve
	     (cadr address)
	     'gnus-gravatar-insert
	     (list header address category))))))))

(defun gnus-gravatar-insert (gravatar header address category)
  "Insert GRAVATAR for ADDRESS in HEADER in current article buffer.
Set image category to CATEGORY."
  (unless (eq gravatar 'error)
    (gnus-with-article-buffer
      (let ((mark (point-marker))
	    (inhibit-point-motion-hooks t)
	    (case-fold-search t))
	(save-restriction
	  (article-narrow-to-head)
	  ;; The buffer can be gone at this time
	  (when (buffer-live-p (current-buffer))
	    (gnus-article-goto-header header)
	    (mail-header-narrow-to-field)
	    (let ((real-name (car address))
		  (mail-address (cadr address)))
	      (when (if real-name
			(re-search-forward
			 (concat (gnus-replace-in-string
				  (regexp-quote real-name) "[\t ]+" "[\t\n ]+")
				 "\\|"
				 (regexp-quote mail-address))
			 nil t)
		      (search-forward mail-address nil t))
		(goto-char (1- (match-beginning 0)))
		;; If we're on the " quoting the name, go backward
		(when (looking-at "[\"<]")
		  (goto-char (1- (point))))
		;; Do not do anything if there's already a gravatar. This can
		;; happens if the buffer has been regenerated in the mean time, for
		;; example we were fetching someaddress, and then we change to
		;; another mail with the same someaddress.
		(unless (memq 'gnus-gravatar (text-properties-at (point)))
		  (let ((point (point)))
		    (unless (featurep 'xemacs)
		      (setq gravatar (append gravatar gnus-gravatar-properties)))
		    (gnus-put-image gravatar (buffer-substring (point) (1+ point)) category)
		    (put-text-property point (point) 'gnus-gravatar address)
		    (gnus-add-wash-type category)
		    (gnus-add-image category gravatar)))))))
	(goto-char (marker-position mark))))))

;;;###autoload
(defun gnus-treat-from-gravatar (&optional force)
  "Display gravatar in the From header.
If gravatar is already displayed, remove it."
  (interactive (list t)) ;; When type `W D g'
  (gnus-with-article-buffer
    (if (memq 'from-gravatar gnus-article-wash-types)
	(gnus-delete-images 'from-gravatar)
      (gnus-gravatar-transform-address "from" 'from-gravatar force))))

;;;###autoload
(defun gnus-treat-mail-gravatar (&optional force)
  "Display gravatars in the Cc and To headers.
If gravatars are already displayed, remove them."
  (interactive (list t)) ;; When type `W D h'
    (gnus-with-article-buffer
      (if (memq 'mail-gravatar gnus-article-wash-types)
          (gnus-delete-images 'mail-gravatar)
	(gnus-gravatar-transform-address "cc" 'mail-gravatar force)
	(gnus-gravatar-transform-address "to" 'mail-gravatar force))))

(provide 'gnus-gravatar)

;;; gnus-gravatar.el ends here
