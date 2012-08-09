;;; rfc2045.el --- Functions for decoding rfc2045 headers

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;; RFC 2045 is: "Multipurpose Internet Mail Extensions (MIME) Part
;; One:  Format of Internet Message Bodies".

;;; Commentary:

;;; Code:

(require 'ietf-drums)

(defun rfc2045-encode-string (param value)
  "Return and PARAM=VALUE string encoded according to RFC2045."
  (if (or (string-match (concat "[" ietf-drums-no-ws-ctl-token "]") value)
	  (string-match (concat "[" ietf-drums-tspecials "]") value)
	  (string-match "[ \n\t]" value)
	  (not (string-match (concat "[" ietf-drums-text-token "]") value)))
      (concat param "=" (format "%S" value))
    (concat param "=" value)))

(provide 'rfc2045)

;;; rfc2045.el ends here
