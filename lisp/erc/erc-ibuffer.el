;;; erc-ibuffer.el --- ibuffer integration with ERC

;; Copyright (C) 2002, 2004, 2006-2012  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: comm
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcIbuffer

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

;; This file contains code related to Ibuffer and ERC.  Totally alpha,
;; needs work.  Usage:  Type / C-e C-h when in Ibuffer-mode to see new
;; limiting commands

;;; Code:

(require 'ibuffer)
(require 'ibuf-ext)
(require 'erc)

(defgroup erc-ibuffer nil
  "The Ibuffer group for ERC."
  :group 'erc)

(defcustom erc-ibuffer-keyword-char ?k
  "Char used to indicate a channel which had keyword traffic lately (hidden)."
  :group 'erc-ibuffer
  :type 'character)
(defcustom erc-ibuffer-pal-char ?p
  "Char used to indicate a channel which had pal traffic lately (hidden)."
  :group 'erc-ibuffer
  :type 'character)
(defcustom erc-ibuffer-fool-char ?f
  "Char used to indicate a channel which had fool traffic lately (hidden)."
  :group 'erc-ibuffer
  :type 'character)
(defcustom erc-ibuffer-dangerous-host-char ?d
  "Char used to indicate a channel which had dangerous-host traffic lately
\(hidden)."
  :group 'erc-ibuffer
  :type 'character)

(define-ibuffer-filter erc-server
  "Toggle current view to buffers which are related to ERC servers."
  (:description "erc servers"
   :reader
   (let ((regexp
	  (read-from-minibuffer "Limit by server (regexp) (RET for all): ")))
     (if (string= regexp "")
	 ".*"
       regexp)))
  (with-current-buffer buf
    (and (eq major-mode 'erc-mode)
	 (string-match qualifier (or erc-server-announced-name
				     erc-session-server)))))

;; Silence the byte-compiler
(defvar erc-modified-channels-alist)

(define-ibuffer-column erc-modified (:name "M")
  (if (and (boundp 'erc-track-mode)
	   erc-track-mode)
      (let ((entry (assq (current-buffer) erc-modified-channels-alist)))
	(if entry
	    (if (> (length entry) 1)
		(cond ((eq 'pal (nth 1 entry))
		       (string erc-ibuffer-pal-char))
		      ((eq 'fool (nth 1 entry))
		       (string erc-ibuffer-fool-char))
		      ((eq 'keyword (nth 1 entry))
		       (string erc-ibuffer-keyword-char))
		      ((eq 'dangerous-host (nth 1 entry))
		       (string erc-ibuffer-dangerous-host-char))
		      (t "$"))
	      (string ibuffer-modified-char))
	  " "))
    " "))

(define-ibuffer-column erc-server-name (:name "Server")
  (if (and erc-server-process (processp erc-server-process))
      (with-current-buffer (process-buffer erc-server-process)
	(or erc-server-announced-name erc-session-server))
    ""))

(define-ibuffer-column erc-target (:name "Target")
  (if (eq major-mode 'erc-mode)
      (cond ((and erc-server-process (processp erc-server-process)
		  (eq (current-buffer) (process-buffer erc-server-process)))
	     (concat "Server " erc-session-server ":"
		     (erc-port-to-string erc-session-port)))
	    ((erc-channel-p (erc-default-target))
	     (concat (erc-default-target)))
	    ((erc-default-target)
	     (concat "Query: " (erc-default-target)))
	    (t "(parted)"))
    (buffer-name)))

(define-ibuffer-column erc-topic (:name "Topic")
  (if (and (eq major-mode 'erc-mode)
	   erc-channel-topic)
      (erc-controls-interpret erc-channel-topic)
    ""))

(define-ibuffer-column
 erc-members (:name "Users")
  (if (and (eq major-mode 'erc-mode)
	   (boundp 'erc-channel-users)
	   (hash-table-p erc-channel-users)
	   (> (hash-table-size erc-channel-users) 0))
     (number-to-string (hash-table-size erc-channel-users))
    ""))

(define-ibuffer-column erc-away (:name "A")
  (if (and erc-server-process
	   (processp erc-server-process)
	   (erc-away-time))
      "A"
    " "))

(define-ibuffer-column
 erc-op (:name "O")
  (if (and (eq major-mode 'erc-mode)
	   (erc-channel-user-op-p (erc-current-nick)))
      "@"
    " "))

(define-ibuffer-column erc-voice (:name "V")
  (if (and (eq major-mode 'erc-mode)
	   (erc-channel-user-voice-p (erc-current-nick)))
      "+"
    " "))

(define-ibuffer-column erc-channel-modes (:name "Mode")
  (if (and (eq major-mode 'erc-mode)
	   (or (> (length erc-channel-modes) 0)
	       erc-channel-user-limit))
      (concat (apply 'concat
		     "(+" erc-channel-modes)
	      (if erc-channel-user-limit
		  (format "l %d" erc-channel-user-limit)
		"")
	      ")")
    (if (not (derived-mode-p 'erc-mode))
	(format-mode-line mode-name nil nil (current-buffer))
      "")))

(define-ibuffer-column erc-nick (:name "Nick")
  (if (eq major-mode 'erc-mode)
      (erc-current-nick)
    ""))

(defvar erc-ibuffer-formats
  '((mark erc-modified erc-away erc-op erc-voice " " (erc-nick 8 8) " "
	  (erc-target 18 40) (erc-members 5 5 :center)
	  (erc-channel-modes 6 16 :center) " " (erc-server-name 20 30) " "
	  (erc-topic 10 -1))
    (mark erc-modified erc-away erc-op erc-voice " " (erc-target 18 40)
	  (erc-members 5 5 :center) (erc-channel-modes 9 20 :center) " "
	  (erc-topic 10 -1))))
(setq ibuffer-formats (append ibuffer-formats erc-ibuffer-formats))

(defvar erc-ibuffer-limit-map nil
  "Prefix keymap to use for ERC related limiting.")
(define-prefix-command 'erc-ibuffer-limit-map)
(define-key 'erc-ibuffer-limit-map (kbd "s") 'ibuffer-limit-by-erc-server)
(define-key ibuffer-mode-map (kbd "/ \C-e") 'erc-ibuffer-limit-map)

(provide 'erc-ibuffer)

;;; erc-ibuffer.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

