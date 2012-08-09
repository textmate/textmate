;;; gnus-ml.el --- Mailing list minor mode for Gnus

;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Julien Gilles  <jgilles@free.fr>
;; Keywords: news, mail

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

;; implement (small subset of) RFC 2369

;;; Code:

(require 'gnus)
(require 'gnus-msg)
(eval-when-compile (require 'cl))
(eval-when-compile
  (when (featurep 'xemacs)
    (require 'easy-mmode))) ; for `define-minor-mode'

;;; Mailing list minor mode

(defvar gnus-mailing-list-mode-map
  (let ((map (make-sparse-keymap)))
    (gnus-define-keys map
      "\C-c\C-nh" gnus-mailing-list-help
      "\C-c\C-ns" gnus-mailing-list-subscribe
      "\C-c\C-nu" gnus-mailing-list-unsubscribe
      "\C-c\C-np" gnus-mailing-list-post
      "\C-c\C-no" gnus-mailing-list-owner
      "\C-c\C-na" gnus-mailing-list-archive)
    map))

(defvar gnus-mailing-list-menu)

(defun gnus-mailing-list-make-menu-bar ()
  (unless (boundp 'gnus-mailing-list-menu)
    (easy-menu-define
     gnus-mailing-list-menu gnus-mailing-list-mode-map ""
     '("Mailing-Lists"
       ["Get help" gnus-mailing-list-help t]
       ["Subscribe" gnus-mailing-list-subscribe t]
       ["Unsubscribe" gnus-mailing-list-unsubscribe t]
       ["Post a message" gnus-mailing-list-post t]
       ["Mail to owner" gnus-mailing-list-owner t]
       ["Browse archive" gnus-mailing-list-archive t]))))

;;;###autoload
(defun turn-on-gnus-mailing-list-mode ()
  (when (gnus-group-find-parameter gnus-newsgroup-name 'to-list)
    (gnus-mailing-list-mode 1)))

;;;###autoload
(defun gnus-mailing-list-insinuate (&optional force)
  "Setup group parameters from List-Post header.
If FORCE is non-nil, replace the old ones."
  (interactive "P")
  (let ((list-post
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-post"))))
    (if list-post
	(if (and (not force)
		 (gnus-group-get-parameter gnus-newsgroup-name 'to-list))
	    (gnus-message 1 "to-list is non-nil.")
	  (if (string-match "<mailto:\\([^>]*\\)>" list-post)
	      (setq list-post (match-string 1 list-post)))
	  (gnus-group-add-parameter gnus-newsgroup-name
				    (cons 'to-list list-post))
	  (gnus-mailing-list-mode 1))
      (gnus-message 1 "no list-post in this message."))))

(eval-when-compile
  (when (featurep 'xemacs)
    (defvar gnus-mailing-list-mode-hook)
    (defvar gnus-mailing-list-mode-on-hook)
    (defvar gnus-mailing-list-mode-off-hook)))

;;;###autoload
(define-minor-mode gnus-mailing-list-mode
  "Minor mode for providing mailing-list commands.

\\{gnus-mailing-list-mode-map}"
  :lighter " Mailing-List"
  :keymap gnus-mailing-list-mode-map
  (cond
   ((not (derived-mode-p 'gnus-summary-mode))
    (setq gnus-mailing-list-mode nil))
   (gnus-mailing-list-mode
    ;; Set up the menu.
    (when (gnus-visual-p 'mailing-list-menu 'menu)
      (gnus-mailing-list-make-menu-bar)))))

;;; Commands

(defun gnus-mailing-list-help ()
  "Get help from mailing list server."
  (interactive)
  (let ((list-help
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-help"))))
    (cond (list-help (gnus-mailing-list-message list-help))
	  (t (gnus-message 1 "no list-help in this group")))))

(defun gnus-mailing-list-subscribe ()
  "Subscribe to mailing list."
  (interactive)
  (let ((list-subscribe
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-subscribe"))))
    (cond (list-subscribe (gnus-mailing-list-message list-subscribe))
	  (t (gnus-message 1 "no list-subscribe in this group")))))

(defun gnus-mailing-list-unsubscribe ()
  "Unsubscribe from mailing list."
  (interactive)
  (let ((list-unsubscribe
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-unsubscribe"))))
    (cond (list-unsubscribe (gnus-mailing-list-message list-unsubscribe))
	  (t (gnus-message 1 "no list-unsubscribe in this group")))))

(defun gnus-mailing-list-post ()
  "Post message (really useful ?)"
  (interactive)
  (let ((list-post
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-post"))))
    (cond (list-post (gnus-mailing-list-message list-post))
	  (t (gnus-message 1 "no list-post in this group")))))

(defun gnus-mailing-list-owner ()
  "Mail to the mailing list owner."
  (interactive)
  (let ((list-owner
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-owner"))))
    (cond (list-owner (gnus-mailing-list-message list-owner))
	  (t (gnus-message 1 "no list-owner in this group")))))

(defun gnus-mailing-list-archive ()
  "Browse archive."
  (interactive)
  (require 'browse-url)
  (let ((list-archive
	 (with-current-buffer gnus-original-article-buffer
	   (gnus-fetch-field "list-archive"))))
    (cond (list-archive
	   (if (string-match "<\\(http:[^>]*\\)>" list-archive)
	       (browse-url (match-string 1 list-archive))
	     (browse-url list-archive)))
	  (t (gnus-message 1 "no list-archive in this group")))))

;;; Utility functions

(defun gnus-mailing-list-message (address)
  "Send message to ADDRESS.
ADDRESS is specified by a \"mailto:\" URL."
  (cond
   ((string-match "<\\(mailto:[^>]*\\)>" address)
    (require 'gnus-art)
    (gnus-url-mailto (match-string 1 address)))
   ;; other case <http://...> to be done.
   (t nil)))

(provide 'gnus-ml)

;;; gnus-ml.el ends here
