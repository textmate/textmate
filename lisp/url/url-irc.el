;;; url-irc.el --- IRC URL interface

;; Copyright (C) 1996-1999, 2004-2012  Free Software Foundation, Inc.

;; Keywords: comm, data, processes

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

;; IRC URLs are defined in
;; http://www.w3.org/Addressing/draft-mirashi-url-irc-01.txt

;;; Code:

(require 'url-vars)
(require 'url-parse)

(defconst url-irc-default-port 6667 "Default port for IRC connections.")

(defcustom url-irc-function 'url-irc-rcirc
  "Function to actually open an IRC connection.
The function should take the following arguments:
    HOST - the hostname of the IRC server to contact
    PORT - the port number of the IRC server to contact
 CHANNEL - What channel on the server to visit right away (can be nil)
    USER - What username to use
PASSWORD - What password to use"
  :type '(choice (const :tag "rcirc" :value url-irc-rcirc)
		 (const :tag "ERC" :value url-irc-erc)
		 (const :tag "ZEN IRC" :value url-irc-zenirc)
		 (function :tag "Other"))
  :group 'url)

;; External.
(declare-function zenirc "ext:zenirc" (&optional prefix))
(declare-function zenirc-send-line "ext:zenirc" ())

(defun url-irc-zenirc (host port channel user password)
  (let ((zenirc-buffer-name (if (and user host port)
				(format "%s@%s:%d" user host port)
			      (format "%s:%d" host port)))
	(zenirc-server-alist
	 (list
	  (list host port password nil user))))
    (zenirc)
    (goto-char (point-max))
    (if (not channel)
	nil
      (insert "/join " channel)
      (zenirc-send-line))))

(defun url-irc-rcirc (host port channel user password)
  (let ((chan (when channel (concat "#" channel))))
    (rcirc-connect host port user nil nil (when chan (list chan)))
    (when chan
      (switch-to-buffer (concat chan "@" host)))))

(defun url-irc-erc (host port channel user password)
  (erc-handle-irc-url host port channel user password))

;;;###autoload
(defun url-irc (url)
  (let* ((host (url-host url))
	 (port (url-port url))
	 (pass (url-password url))
	 (user (url-user url))
	 (chan (url-filename url)))
    (if (url-target url)
	(setq chan (concat chan "#" (url-target url))))
    (if (string-match "^/" chan)
	(setq chan (substring chan 1 nil)))
    (if (= (length chan) 0)
	(setq chan nil))
    (funcall url-irc-function host port chan user pass)
    nil))

(provide 'url-irc)

;;; url-irc.el ends here
