;;; erc-services.el --- Identify to NickServ

;; Copyright (C) 2002-2004, 2006-2012 Free Software Foundation, Inc.

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

;; There are two ways to go about identifying yourself automatically to
;; NickServ with this module.  The more secure way is to listen for identify
;; requests from the user NickServ.  Another way is to identify yourself to
;; NickServ directly after a successful connection and every time you change
;; your nickname.  This method is rather insecure, though, because no checks
;; are made to test if NickServ is the real NickServ for a given network or
;; server.

;; As a default, ERC has the data for the official nickname services on
;; the networks Austnet, BrasNET, Dalnet, freenode, GalaxyNet, GRnet,
;; and Slashnet.  You can add more by using M-x customize-variable RET
;; erc-nickserv-alist.

;; Usage:
;;
;; Put into your .emacs:
;;
;; (require 'erc-services)
;; (erc-services-mode 1)
;;
;; Add your nickname and NickServ password to `erc-nickserv-passwords'.
;; Using the freenode network as an example:
;;
;; (setq erc-nickserv-passwords '((freenode (("nickname" "password")))))
;;
;; The default automatic identification mode is autodetection of NickServ
;; identify requests.  Set the variable `erc-nickserv-identify-mode' if
;; you'd like to change this behavior.  You can also change the way
;; automatic identification is handled by using:
;;
;; M-x erc-nickserv-identify-mode
;;
;; If you'd rather not identify yourself automatically but would like access
;; to the functions contained in this file, just load this file without
;; enabling `erc-services-mode'.
;;

;;; Code:

(require 'erc)
(require 'erc-networks)
(eval-when-compile (require 'cl))

;; Customization:

(defgroup erc-services nil
  "Configuration for IRC services.

On some networks, there exists a special type of automated irc bot,
called Services.  Those usually allow you to register your nickname,
post/read memos to other registered users who are currently offline,
and do various other things.

This group allows you to set variables to somewhat automate
communication with those Services."
  :group 'erc)

(defcustom erc-nickserv-identify-mode 'both
  "The mode which is used when identifying to Nickserv.

Possible settings are:.

'autodetect  - Identify when the real Nickserv sends an identify request.
'nick-change - Identify when you log in or change your nickname.
'both        - Do the former if the network supports it, otherwise do the
               latter.
nil          - Disables automatic Nickserv identification.

You can also use M-x erc-nickserv-identify-mode to change modes."
  :group 'erc-services
  :type '(choice (const autodetect)
		 (const nick-change)
		 (const both)
		 (const nil))
  :set (lambda (sym val)
	 (set sym val)
	 ;; avoid recursive load at startup
	 (when (featurep 'erc-services)
	   (erc-nickserv-identify-mode val))))

;;;###autoload (autoload 'erc-services-mode "erc-services" nil t)
(define-erc-module services nickserv
  "This mode automates communication with services."
  ((erc-nickserv-identify-mode erc-nickserv-identify-mode))
  ((remove-hook 'erc-server-NOTICE-functions
		'erc-nickserv-identify-autodetect)
   (remove-hook 'erc-after-connect
		'erc-nickserv-identify-on-connect)
   (remove-hook 'erc-nick-changed-functions
		'erc-nickserv-identify-on-nick-change)
   (remove-hook 'erc-server-NOTICE-functions
		'erc-nickserv-identification-autodetect)))

;;;###autoload
(defun erc-nickserv-identify-mode (mode)
  "Set up hooks according to which MODE the user has chosen."
  (interactive
   (list (intern (completing-read
		  "Choose Nickserv identify mode (RET to disable): "
		  '(("autodetect") ("nick-change") ("both")) nil t))))
  (add-hook 'erc-server-NOTICE-functions
	    'erc-nickserv-identification-autodetect)
  (unless erc-networks-mode
    ;; Force-enable networks module, because we need it to set
    ;; erc-network for us.
    (erc-networks-enable))
  (cond ((eq mode 'autodetect)
	 (setq erc-nickserv-identify-mode 'autodetect)
	 (add-hook 'erc-server-NOTICE-functions
		   'erc-nickserv-identify-autodetect)
	 (remove-hook 'erc-nick-changed-functions
		      'erc-nickserv-identify-on-nick-change)
	 (remove-hook 'erc-after-connect
		      'erc-nickserv-identify-on-connect))
	((eq mode 'nick-change)
	 (setq erc-nickserv-identify-mode 'nick-change)
	 (add-hook 'erc-after-connect
		   'erc-nickserv-identify-on-connect)
	 (add-hook 'erc-nick-changed-functions
		   'erc-nickserv-identify-on-nick-change)
	 (remove-hook 'erc-server-NOTICE-functions
		      'erc-nickserv-identify-autodetect))
	((eq mode 'both)
	 (setq erc-nickserv-identify-mode 'both)
	 (add-hook 'erc-server-NOTICE-functions
		   'erc-nickserv-identify-autodetect)
	 (add-hook 'erc-after-connect
		   'erc-nickserv-identify-on-connect)
	 (add-hook 'erc-nick-changed-functions
		   'erc-nickserv-identify-on-nick-change))
	(t
	 (setq erc-nickserv-identify-mode nil)
	 (remove-hook 'erc-server-NOTICE-functions
		      'erc-nickserv-identify-autodetect)
	 (remove-hook 'erc-after-connect
		      'erc-nickserv-identify-on-connect)
	 (remove-hook 'erc-nick-changed-functions
		      'erc-nickserv-identify-on-nick-change)
	 (remove-hook 'erc-server-NOTICE-functions
		      'erc-nickserv-identification-autodetect))))

(defcustom erc-prompt-for-nickserv-password t
  "Ask for the password when identifying to NickServ."
  :group 'erc-services
  :type 'boolean)

(defcustom erc-nickserv-passwords nil
  "Passwords used when identifying to NickServ automatically.

Example of use:
  (setq erc-nickserv-passwords
        '((freenode ((\"nick-one\" . \"password\")
                     (\"nick-two\" . \"password\")))
          (DALnet ((\"nick\" . \"password\")))))"
  :group 'erc-services
  :type '(repeat
	  (list :tag "Network"
		(choice :tag "Network name"
			(const Ars)
			(const Austnet)
			(const Azzurra)
			(const BitlBee)
			(const BRASnet)
			(const DALnet)
			(const freenode)
			(const GalaxyNet)
			(const GRnet)
			(const iip)
			(const OFTC)
			(const QuakeNet)
			(const Rizon)
			(const SlashNET)
			(symbol :tag "Network name"))
		(repeat :tag "Nickname and password"
			(cons :tag "Identity"
			      (string :tag "Nick")
			      (string :tag "Password"))))))

;; Variables:

(defcustom erc-nickserv-alist
  '((Ars
     nil nil
     "Census"
     "IDENTIFY" nil nil nil)
    (Austnet
     "NickOP!service@austnet.org"
     "/msg\\s-NickOP@austnet.org\\s-identify\\s-<password>"
     "nickop@austnet.org"
     "identify" nil nil nil)
    (Azzurra
     "NickServ!service@azzurra.org"
     "/ns\\s-IDENTIFY\\s-password"
     "NickServ"
     "IDENTIFY" nil nil nil)
    (BitlBee
     nil nil
     "&bitlbee"
     "identify" nil nil nil)
    (BRASnet
     "NickServ!services@brasnet.org"
     "/NickServ\\s-IDENTIFY\\s-senha"
     "NickServ"
     "IDENTIFY" nil "" nil)
    (DALnet
     "NickServ!service@dal.net"
     "/msg\\s-NickServ@services.dal.net\\s-IDENTIFY\\s-<password>"
     "NickServ@services.dal.net"
     "IDENTIFY" nil nil nil)
    (freenode
     "NickServ!NickServ@services."
     ;; freenode also accepts a password at login, see the `erc'
     ;; :password argument.
     "This\\s-nickname\\s-is\\s-registered.\\s-Please\\s-choose"
     "NickServ"
     "IDENTIFY" nil nil
     ;; See also the 901 response code message.
     "You\\s-are\\s-now\\s-identified\\s-for\\s-")
    (GalaxyNet
     "NS!nickserv@galaxynet.org"
     "Please\\s-change\\s-nicks\\s-or\\s-authenticate."
     "NS@services.galaxynet.org"
     "AUTH" t nil nil)
    (GRnet
     "NickServ!service@irc.gr"
     "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected."
     "NickServ"
     "IDENTIFY" nil nil
     "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.")
    (iip
     "Trent@anon.iip"
     "type\\s-/squery\\s-Trent\\s-identify\\s-<password>"
     "Trent@anon.iip"
     "IDENTIFY" nil "SQUERY" nil)
    (OFTC
     "NickServ!services@services.oftc.net"
     ;; OFTC's NickServ doesn't ask you to identify anymore.
     nil
     "NickServ"
     "IDENTIFY" nil nil
     "You\\s-are\\s-successfully\\s-identified\\s-as\\s-")
    (Rizon
     "NickServ!service@rizon.net"
     "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected."
     "NickServ"
     "IDENTIFY" nil nil
     "Password\\s-accepted\\s--\\s-you\\s-are\\s-now\\s-recognized.")
    (QuakeNet
     nil nil
     "Q@CServe.quakenet.org"
     "auth" t nil nil)
    (SlashNET
     "NickServ!services@services.slashnet.org"
     "/msg\\s-NickServ\\s-IDENTIFY\\s-password"
     "NickServ@services.slashnet.org"
     "IDENTIFY" nil nil nil))
   "Alist of NickServer details, sorted by network.
Every element in the list has the form
  \(SYMBOL NICKSERV REGEXP NICK KEYWORD USE-CURRENT ANSWER SUCCESS-REGEXP)

SYMBOL is a network identifier, a symbol, as used in `erc-networks-alist'.
NICKSERV is the description of the nickserv in the form nick!user@host.
REGEXP is a regular expression matching the message from nickserv.
NICK is nickserv's nickname.  Use nick@server where necessary/possible.
KEYWORD is the keyword to use in the reply message to identify yourself.
USE-CURRENT indicates whether the current nickname must be used when
  identifying.
ANSWER is the command to use for the answer.  The default is 'privmsg.
SUCCESS-REGEXP is a regular expression matching the message nickserv
  sends when you've successfully identified.
The last two elements are optional."
   :group 'erc-services
   :type '(repeat
	   (list :tag "Nickserv data"
		 (symbol :tag "Network name")
		 (choice (string :tag "Nickserv's nick!user@host")
			 (const :tag "No message sent by Nickserv" nil))
		 (choice (regexp :tag "Identify request sent by Nickserv")
			 (const :tag "No message sent by Nickserv" nil))
		 (string :tag "Identify to")
		 (string :tag "Identify keyword")
		 (boolean :tag "Use current nick in identify message?")
		 (choice :tag "Command to use (optional)"
		  (string :tag "Command")
		  (const :tag "No special command necessary" nil))
		 (choice :tag "Detect Success"
			 (regexp :tag "Pattern to match")
			 (const :tag "Do not try to detect success" nil)))))


(defsubst erc-nickserv-alist-sender (network &optional entry)
  (nth 1 (or entry (assoc network erc-nickserv-alist))))

(defsubst erc-nickserv-alist-regexp (network &optional entry)
  (nth 2 (or entry (assoc network erc-nickserv-alist))))

(defsubst erc-nickserv-alist-nickserv (network &optional entry)
  (nth 3 (or entry (assoc network erc-nickserv-alist))))

(defsubst erc-nickserv-alist-ident-keyword (network &optional entry)
  (nth 4 (or entry (assoc network erc-nickserv-alist))))

(defsubst erc-nickserv-alist-use-nick-p (network &optional entry)
  (nth 5 (or entry (assoc network erc-nickserv-alist))))

(defsubst erc-nickserv-alist-ident-command (network &optional entry)
  (nth 6 (or entry (assoc network erc-nickserv-alist))))

(defsubst erc-nickserv-alist-identified-regexp (network &optional entry)
  (nth 7 (or entry (assoc network erc-nickserv-alist))))

;; Functions:

(defcustom erc-nickserv-identified-hook nil
  "Run this hook when NickServ acknowledged successful identification.
Hooks are called with arguments (NETWORK NICK)."
  :group 'erc-services
  :type 'hook)

(defun erc-nickserv-identification-autodetect (proc parsed)
  "Check for NickServ's successful identification notice.
Make sure it is the real NickServ for this network and that it has
specifically confirmed a successful identification attempt.
If this is the case, run `erc-nickserv-identified-hook'."
  (let* ((network (erc-network))
	 (sender (erc-nickserv-alist-sender network))
	 (success-regex (erc-nickserv-alist-identified-regexp network))
	 (sspec (erc-response.sender parsed))
	 (nick (car (erc-response.command-args parsed)))
	 (msg (erc-response.contents parsed)))
    ;; continue only if we're sure it's the real nickserv for this network
    ;; and it's told us we've successfully identified
    (when (and sender (equal sspec sender)
	       success-regex
	       (string-match success-regex msg))
      (erc-log "NickServ IDENTIFY success notification detected")
      (run-hook-with-args 'erc-nickserv-identified-hook network nick)
      nil)))

(defun erc-nickserv-identify-autodetect (proc parsed)
  "Identify to NickServ when an identify request is received.
Make sure it is the real NickServ for this network.
If `erc-prompt-for-nickserv-password' is non-nil, prompt the user for the
password for this nickname, otherwise try to send it automatically."
  (unless (and (null erc-nickserv-passwords)
	       (null erc-prompt-for-nickserv-password))
    (let* ((network (erc-network))
	   (sender (erc-nickserv-alist-sender network))
	   (identify-regex (erc-nickserv-alist-regexp network))
	   (sspec (erc-response.sender parsed))
	   (nick (car (erc-response.command-args parsed)))
	   (msg (erc-response.contents parsed)))
      ;; continue only if we're sure it's the real nickserv for this network
      ;; and it's asked us to identify
      (when (and sender (equal sspec sender)
		 identify-regex
		 (string-match identify-regex msg))
	(erc-log "NickServ IDENTIFY request detected")
	(erc-nickserv-call-identify-function nick)
	nil))))

(defun erc-nickserv-identify-on-connect (server nick)
  "Identify to Nickserv after the connection to the server is established."
  (unless (or (and (null erc-nickserv-passwords)
		   (null erc-prompt-for-nickserv-password))
	      (and (eq erc-nickserv-identify-mode 'both)
		   (erc-nickserv-alist-regexp (erc-network))))
    (erc-nickserv-call-identify-function nick)))

(defun erc-nickserv-identify-on-nick-change (nick old-nick)
  "Identify to Nickserv whenever your nick changes."
  (unless (or (and (null erc-nickserv-passwords)
		   (null erc-prompt-for-nickserv-password))
	      (and (eq erc-nickserv-identify-mode 'both)
		   (erc-nickserv-alist-regexp (erc-network))))
    (erc-nickserv-call-identify-function nick)))

(defun erc-nickserv-call-identify-function (nickname)
  "Call `erc-nickserv-identify' interactively or run it with NICKNAME's
password.
The action is determined by the value of `erc-prompt-for-nickserv-password'."
  (if erc-prompt-for-nickserv-password
      (call-interactively 'erc-nickserv-identify)
    (when erc-nickserv-passwords
      (erc-nickserv-identify
       (cdr (assoc nickname
		   (nth 1 (assoc (erc-network)
				 erc-nickserv-passwords))))))))

;;;###autoload
(defun erc-nickserv-identify (password)
  "Send an \"identify <PASSWORD>\" message to NickServ.
When called interactively, read the password using `read-passwd'."
  (interactive
   (list (read-passwd
	  (format "NickServ password for %s on %s (RET to cancel): "
		  (erc-current-nick)
		  (or (and (erc-network)
			   (symbol-name (erc-network)))
		      "Unknown network")))))
  (when (and password (not (string= "" password)))
    (let* ((erc-auto-discard-away nil)
	   (network (erc-network))
	   (nickserv-info (assoc network erc-nickserv-alist))
	   (nickserv (or (erc-nickserv-alist-nickserv nil nickserv-info)
			 "NickServ"))
	   (identify-word (or (erc-nickserv-alist-ident-keyword
			       nil nickserv-info)
			      "IDENTIFY"))
	   (nick (if (erc-nickserv-alist-use-nick-p nil nickserv-info)
		     (concat (erc-current-nick) " ")
		   ""))
	   (msgtype (or (erc-nickserv-alist-ident-command nil nickserv-info)
			"PRIVMSG")))
      (erc-message msgtype
		   (concat nickserv " " identify-word " " nick password)))))

(provide 'erc-services)

;;; erc-services.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:

