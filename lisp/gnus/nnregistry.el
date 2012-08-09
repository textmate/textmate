;;; nnregistry.el --- access to articles via Gnus' message-id registry
;;; -*- coding: utf-8 -*-

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Authors: Ludovic Courtès <ludo@gnu.org>
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

;; This file provides the `nnregistry' Gnus back-end.  It can be used
;; in `gnus-refer-article-method' to quickly search for a message by
;; id, regardless of the back-end that stores it.  See the Gnus manual
;; for usage examples and more information.

;;; Code:

(require 'nnoo)
(require 'gnus-registry)
(require 'gnus-int)

(nnoo-declare nnregistry)

(deffoo nnregistry-server-opened (server)
  gnus-registry-enabled)

(deffoo nnregistry-close-server (server)
  t)

(deffoo nnregistry-status-message (server)
  nil)

(deffoo nnregistry-open-server (server &optional defs)
  gnus-registry-enabled)

(defvar nnregistry-within-nnregistry nil)

(deffoo nnregistry-request-article (id &optional group server buffer)
  (and (not nnregistry-within-nnregistry)
       (let* ((nnregistry-within-nnregistry t)
	      (group (nth 0 (gnus-registry-get-id-key id 'group)))
	      (gnus-override-method nil))
	 (message "nnregistry: requesting article `%s' in group `%s'"
		  id group)
	 (and group
	      (gnus-check-group group)
	      (gnus-request-article id group buffer)))))

(provide 'nnregistry)

;;; nnregistry.el ends here
