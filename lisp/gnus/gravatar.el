;;; gravatar.el --- Get Gravatars

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

(require 'url)
(require 'url-cache)

(defgroup gravatar nil
  "Gravatar."
  :version "24.1"
  :group 'comm)

(defcustom gravatar-automatic-caching t
  "Whether cache retrieved gravatar."
  :group 'gravatar)

(defcustom gravatar-cache-ttl (days-to-time 30)
  "Time to live for gravatar cache entries."
  :group 'gravatar)

(defcustom gravatar-rating "g"
  "Default rating for gravatar."
  :group 'gravatar)

(defcustom gravatar-size 32
  "Default size in pixels for gravatars."
  :group 'gravatar)

(defconst gravatar-base-url
  "http://www.gravatar.com/avatar"
  "Base URL for getting gravatars.")

(defun gravatar-hash (mail-address)
  "Create an hash from MAIL-ADDRESS."
  (md5 (downcase mail-address)))

(defun gravatar-build-url (mail-address)
  "Return an URL to retrieve MAIL-ADDRESS gravatar."
  (format "%s/%s?d=404&r=%s&s=%d"
          gravatar-base-url
          (gravatar-hash mail-address)
          gravatar-rating
          gravatar-size))

(defun gravatar-cache-expired (url)
  "Check if URL is cached for more than `gravatar-cache-ttl'."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   cache-time
                   gravatar-cache-ttl)
                  (current-time))
               t)))))

(defun gravatar-get-data ()
  "Get data from current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/.+ 200 OK$" nil (line-end-position))
      (when (search-forward "\n\n" nil t)
        (buffer-substring (point) (point-max))))))

(eval-and-compile
  (cond ((featurep 'xemacs)
	 (require 'gnus-xmas)
	 (defalias 'gravatar-create-image 'gnus-xmas-create-image))
	((featurep 'gnus-ems)
	 (defalias 'gravatar-create-image 'gnus-create-image))
	(t
	 (require 'image)
	 (defalias 'gravatar-create-image 'create-image))))

(defun gravatar-data->image ()
  "Get data of current buffer and return an image.
If no image available, return 'error."
  (let ((data (gravatar-get-data)))
    (if data
	(gravatar-create-image data nil t)
      'error)))

;;;###autoload
(defun gravatar-retrieve (mail-address cb &optional cbargs)
  "Retrieve MAIL-ADDRESS gravatar and call CB on retrieval.
You can provide a list of argument to pass to CB in CBARGS."
  (let ((url (gravatar-build-url mail-address)))
    (if (gravatar-cache-expired url)
	(let ((args (list url
			  'gravatar-retrieved
			  (list cb (when cbargs cbargs)))))
	  (when (> (length (if (featurep 'xemacs)
			       (cdr (split-string (function-arglist 'url-retrieve)))
			     (help-function-arglist 'url-retrieve)))
		   4)
	    (setq args (nconc args (list t))))
	  (apply #'url-retrieve args))
      (apply cb
               (with-temp-buffer
                 (mm-disable-multibyte)
                 (url-cache-extract (url-cache-create-filename url))
                 (gravatar-data->image))
               cbargs))))

;;;###autoload
(defun gravatar-retrieve-synchronously (mail-address)
  "Retrieve MAIL-ADDRESS gravatar and returns it."
  (let ((url (gravatar-build-url mail-address)))
    (if (gravatar-cache-expired url)
        (with-current-buffer (if (featurep 'xemacs)
				 (url-retrieve url)
			       (url-retrieve-synchronously url))
	  (when gravatar-automatic-caching
            (url-store-in-cache (current-buffer)))
          (let ((data (gravatar-data->image)))
            (kill-buffer (current-buffer))
            data))
      (with-temp-buffer
        (mm-disable-multibyte)
        (url-cache-extract (url-cache-create-filename url))
        (gravatar-data->image)))))


(defun gravatar-retrieved (status cb &optional cbargs)
  "Callback function used by `gravatar-retrieve'."
  ;; Store gravatar?
  (when gravatar-automatic-caching
    (url-store-in-cache (current-buffer)))
  (if (plist-get status :error)
      ;; Error happened.
      (apply cb 'error cbargs)
    (apply cb (gravatar-data->image) cbargs))
  (kill-buffer (current-buffer)))

(provide 'gravatar)

;;; gravatar.el ends here
