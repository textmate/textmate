;;; sasl-cram.el --- CRAM-MD5 module for the SASL client framework

;; Copyright (C) 2000, 2007-2012  Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@unixuser.org>
;;	Kenichi OKADA <okada@opaopa.org>
;; Keywords: SASL, CRAM-MD5
;; Package: sasl

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

(require 'sasl)
(require 'hmac-md5)

(defconst sasl-cram-md5-steps
  '(ignore				;no initial response
    sasl-cram-md5-response))

(defun sasl-cram-md5-response (client step)
  (let ((passphrase
	 (sasl-read-passphrase
	  (format "CRAM-MD5 passphrase for %s: "
		  (sasl-client-name client)))))
    (unwind-protect
	(concat (sasl-client-name client) " "
		(encode-hex-string
		 (hmac-md5 (sasl-step-data step) passphrase)))
      (fillarray passphrase 0))))

(put 'sasl-cram 'sasl-mechanism
     (sasl-make-mechanism "CRAM-MD5" sasl-cram-md5-steps))

(provide 'sasl-cram)

;;; sasl-cram.el ends here
