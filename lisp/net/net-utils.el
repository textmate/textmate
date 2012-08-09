;;; net-utils.el --- network functions

;; Copyright (C) 1998-2012  Free Software Foundation, Inc.

;; Author:  Peter Breton <pbreton@cs.umb.edu>
;; Created: Sun Mar 16 1997
;; Keywords: network comm

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

;;
;; There are three main areas of functionality:
;;
;; * Wrap common network utility programs (ping, traceroute, netstat,
;; nslookup, arp, route). Note that these wrappers are of the diagnostic
;; functions of these programs only.
;;
;; * Implement some very basic protocols in Emacs Lisp (finger and whois)
;;
;; * Support connections to HOST/PORT, generally for debugging and the like.
;; In other words, for doing much the same thing as "telnet HOST PORT", and
;; then typing commands.
;;
;; PATHS
;;
;; On some systems, some of these programs are not in normal user path,
;; but rather in /sbin, /usr/sbin, and so on.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup net-utils nil
  "Network utility functions."
  :prefix "net-utils-"
  :group 'comm
  :version "20.3")

(defcustom net-utils-remove-ctl-m (memq system-type '(windows-nt msdos))
  "If non-nil, remove control-Ms from output."
  :group 'net-utils
  :type  'boolean)

(defcustom traceroute-program
  (if (eq system-type 'windows-nt)
      "tracert"
    "traceroute")
  "Program to trace network hops to a destination."
  :group 'net-utils
  :type  'string)

(defcustom traceroute-program-options nil
  "Options for the traceroute program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom ping-program "ping"
  "Program to send network test packets to a host."
  :group 'net-utils
  :type  'string)

;; On GNU/Linux and Irix, the system's ping program seems to send packets
;; indefinitely unless told otherwise
(defcustom ping-program-options
  (and (memq system-type '(gnu/linux irix))
       (list "-c" "4"))
  "Options for the ping program.
These options can be used to limit how many ICMP packets are emitted."
  :group 'net-utils
  :type  '(repeat string))

(define-obsolete-variable-alias 'ipconfig-program 'ifconfig-program "22.2")

(defcustom ifconfig-program
  (if (eq system-type 'windows-nt)
      "ipconfig"
    "ifconfig")
  "Program to print network configuration information."
  :group 'net-utils
  :type  'string)

(define-obsolete-variable-alias 'ipconfig-program-options
  'ifconfig-program-options "22.2")

(defcustom ifconfig-program-options
  (list
   (if (eq system-type 'windows-nt)
       "/all" "-a"))
  "Options for the ifconfig program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom iwconfig-program "iwconfig"
  "Program to print wireless network configuration information."
  :group 'net-utils
  :type 'string
  :version "23.1")

(defcustom iwconfig-program-options nil
 "Options for the iwconfig program."
 :group 'net-utils
 :type '(repeat string)
 :version "23.1")

(defcustom netstat-program "netstat"
  "Program to print network statistics."
  :group 'net-utils
  :type  'string)

(defcustom netstat-program-options
  (list "-a")
  "Options for the netstat program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom arp-program "arp"
  "Program to print IP to address translation tables."
  :group 'net-utils
  :type  'string)

(defcustom arp-program-options
  (list "-a")
  "Options for the arp program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom route-program
  (if (eq system-type 'windows-nt)
      "route"
    "netstat")
  "Program to print routing tables."
  :group 'net-utils
  :type  'string)

(defcustom route-program-options
  (if (eq system-type 'windows-nt)
      (list "print")
    (list "-r"))
  "Options for the route program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom nslookup-program "nslookup"
  "Program to interactively query DNS information."
  :group 'net-utils
  :type  'string)

(defcustom nslookup-program-options nil
  "Options for the nslookup program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom nslookup-prompt-regexp "^> "
  "Regexp to match the nslookup prompt.

This variable is only used if the variable
`comint-use-prompt-regexp' is non-nil."
  :group 'net-utils
  :type  'regexp)

(defcustom dig-program "dig"
  "Program to query DNS information."
  :group 'net-utils
  :type  'string)

(defcustom ftp-program "ftp"
  "Program to run to do FTP transfers."
  :group 'net-utils
  :type  'string)

(defcustom ftp-program-options nil
  "Options for the ftp program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom ftp-prompt-regexp "^ftp>"
  "Regexp which matches the FTP program's prompt.

This variable is only used if the variable
`comint-use-prompt-regexp' is non-nil."
  :group 'net-utils
  :type  'regexp)

(defcustom smbclient-program "smbclient"
  "Smbclient program."
  :group 'net-utils
  :type  'string)

(defcustom smbclient-program-options nil
  "Options for the smbclient program."
  :group 'net-utils
  :type  '(repeat string))

(defcustom smbclient-prompt-regexp "^smb: \>"
  "Regexp which matches the smbclient program's prompt.

This variable is only used if the variable
`comint-use-prompt-regexp' is non-nil."
  :group 'net-utils
  :type  'regexp)

(defcustom dns-lookup-program "host"
  "Program to interactively query DNS information."
  :group 'net-utils
  :type  'string)

(defcustom dns-lookup-program-options nil
  "Options for the dns-lookup program."
  :group 'net-utils
  :type  '(repeat string))

;; Internal variables
(defvar network-connection-service nil)
(defvar network-connection-host    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nslookup goodies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar nslookup-font-lock-keywords
  (list
   (list "^[A-Za-z0-9 _]+:" 0 'font-lock-type-face)
   (list "\\<\\(SOA\\|NS\\|MX\\|A\\|CNAME\\)\\>"
         1 'font-lock-keyword-face)
   ;; Dotted quads
   (list
    (mapconcat 'identity
               (make-list 4 "[0-9]+")
               "\\.")
    0 'font-lock-variable-name-face)
   ;; Host names
   (list
    (let ((host-expression "[-A-Za-z0-9]+"))
      (concat
       (mapconcat 'identity
                  (make-list 2 host-expression)
                  "\\.")
       "\\(\\." host-expression "\\)*"))
    0 'font-lock-variable-name-face))
  "Expressions to font-lock for nslookup.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General network utilities mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar net-utils-font-lock-keywords
  (list
   ;; Dotted quads
   (list
    (mapconcat 'identity (make-list 4 "[0-9]+") "\\.")
    0 'font-lock-variable-name-face)
   ;; Simple rfc4291 addresses
   (list (concat
	  "\\( \\([[:xdigit:]]+\\(:\\|::\\)\\)+[[:xdigit:]]+\\)"
	  "\\|"
	  "\\(::[[:xdigit:]]+\\)")
    0 'font-lock-variable-name-face)
   ;; Host names
   (list
    (let ((host-expression "[-A-Za-z0-9]+"))
      (concat
       (mapconcat 'identity (make-list 2 host-expression) "\\.")
       "\\(\\." host-expression "\\)*"))
    0 'font-lock-variable-name-face))
  "Expressions to font-lock for general network utilities.")

(define-derived-mode net-utils-mode special-mode "NetworkUtil"
  "Major mode for interacting with an external network utility."
  (set (make-local-variable 'font-lock-defaults)
       '((net-utils-font-lock-keywords))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simplified versions of some at-point functions from ffap.el.
;; It's not worth loading all of ffap just for these.
(defun net-utils-machine-at-point ()
  (let ((pt (point)))
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "-a-zA-Z0-9.")
       (point))
     (save-excursion
       (skip-chars-forward "-a-zA-Z0-9.")
       (skip-chars-backward "." pt)
       (point)))))

(defun net-utils-url-at-point ()
  (let ((pt (point)))
    (buffer-substring-no-properties
     (save-excursion
       (skip-chars-backward "--:=&?$+@-Z_a-z~#,%")
       (skip-chars-forward "^A-Za-z0-9" pt)
       (point))
     (save-excursion
       (skip-chars-forward "--:=&?$+@-Z_a-z~#,%")
       (skip-chars-backward ":;.,!?" pt)
       (point)))))

(defun net-utils-remove-ctrl-m-filter (process output-string)
  "Remove trailing control Ms."
  (let ((old-buffer (current-buffer))
	(filtered-string output-string))
    (unwind-protect
	(let ((moving))
	  (set-buffer (process-buffer process))
	  (let ((inhibit-read-only t))
	    (setq moving (= (point) (process-mark process)))

	    (while (string-match "\r" filtered-string)
	      (setq filtered-string
		    (replace-match "" nil nil filtered-string)))

	    (save-excursion
	      ;; Insert the text, moving the process-marker.
	      (goto-char (process-mark process))
	      (insert filtered-string)
	      (set-marker (process-mark process) (point))))
	  (if moving (goto-char (process-mark process))))
      (set-buffer old-buffer))))

(defun net-utils-run-program (name header program args)
  "Run a network information program."
  (let ((buf (get-buffer-create (concat "*" name "*"))))
    (set-buffer buf)
    (erase-buffer)
    (insert header "\n")
    (set-process-filter
     (apply 'start-process name buf program args)
     'net-utils-remove-ctrl-m-filter)
    (display-buffer buf)
    buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General network utilities (diagnostic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun net-utils-run-simple (buffer-name program-name args)
  "Run a network utility for diagnostic output only."
  (interactive)
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (get-buffer-create buffer-name)
  (with-current-buffer buffer-name
    (net-utils-mode)
    (set-process-filter
     (apply 'start-process (format "%s" program-name)
	    buffer-name program-name args)
     'net-utils-remove-ctrl-m-filter)
    (goto-char (point-min)))
  (display-buffer buffer-name))

;;;###autoload
(defun ifconfig ()
  "Run ifconfig and display diagnostic output."
  (interactive)
  (net-utils-run-simple
   (format "*%s*" ifconfig-program)
   ifconfig-program
   ifconfig-program-options))

(defalias 'ipconfig 'ifconfig)

;;;###autoload
(defun iwconfig ()
  "Run iwconfig and display diagnostic output."
  (interactive)
  (net-utils-run-simple
   (format "*%s*" iwconfig-program)
   iwconfig-program
   iwconfig-program-options))

;;;###autoload
(defun netstat ()
  "Run netstat and display diagnostic output."
  (interactive)
  (net-utils-run-simple
   (format "*%s*" netstat-program)
   netstat-program
   netstat-program-options))

;;;###autoload
(defun arp ()
  "Run arp and display diagnostic output."
  (interactive)
  (net-utils-run-simple
   (format "*%s*" arp-program)
   arp-program
   arp-program-options))

;;;###autoload
(defun route ()
  "Run route and display diagnostic output."
  (interactive)
  (net-utils-run-simple
   (format "*%s*" route-program)
   route-program
   route-program-options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers for external network programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun traceroute (target)
  "Run traceroute program for TARGET."
  (interactive "sTarget: ")
  (let ((options
	 (if traceroute-program-options
	     (append traceroute-program-options (list target))
	   (list target))))
    (net-utils-run-program
     (concat "Traceroute" " " target)
     (concat "** Traceroute ** " traceroute-program " ** " target)
     traceroute-program
     options)))

;;;###autoload
(defun ping (host)
  "Ping HOST.
If your system's ping continues until interrupted, you can try setting
`ping-program-options'."
  (interactive
   (list (read-from-minibuffer "Ping host: " (net-utils-machine-at-point))))
  (let ((options
	 (if ping-program-options
	     (append ping-program-options (list host))
	   (list host))))
    (net-utils-run-program
     (concat "Ping" " " host)
     (concat "** Ping ** " ping-program " ** " host)
     ping-program
     options)))

;; FIXME -- Needs to be a process filter
;; (defun netstat-with-filter (filter)
;;   "Run netstat program."
;;   (interactive "sFilter: ")
;;   (netstat)
;;   (set-buffer (get-buffer "*Netstat*"))
;;   (goto-char (point-min))
;;   (delete-matching-lines filter))

;;;###autoload
(defun nslookup-host (host)
  "Lookup the DNS information for HOST."
  (interactive
   (list (read-from-minibuffer "Lookup host: " (net-utils-machine-at-point))))
  (let ((options
	 (if nslookup-program-options
	     (append nslookup-program-options (list host))
	   (list host))))
    (net-utils-run-program
     "Nslookup"
     (concat "** "
      (mapconcat 'identity
		(list "Nslookup" host nslookup-program)
		" ** "))
     nslookup-program
     options)))

;;;###autoload
(defun nslookup ()
  "Run nslookup program."
  (interactive)
  (switch-to-buffer (make-comint "nslookup" nslookup-program))
  (nslookup-mode))

(defvar comint-prompt-regexp)
(defvar comint-input-autoexpand)

(autoload 'comint-mode "comint" nil t)

(defvar nslookup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'comint-dynamic-complete)
    map))

;; Using a derived mode gives us keymaps, hooks, etc.
(define-derived-mode nslookup-mode comint-mode "Nslookup"
  "Major mode for interacting with the nslookup program."
  (set
   (make-local-variable 'font-lock-defaults)
   '((nslookup-font-lock-keywords)))
  (setq comint-prompt-regexp nslookup-prompt-regexp)
  (setq comint-input-autoexpand t))

;;;###autoload
(defun dns-lookup-host (host)
  "Lookup the DNS information for HOST (name or IP address)."
  (interactive
   (list (read-from-minibuffer "Lookup host: " (net-utils-machine-at-point))))
  (let ((options
	 (if dns-lookup-program-options
	     (append dns-lookup-program-options (list host))
	   (list host))))
    (net-utils-run-program
     (concat "DNS Lookup [" host "]")
     (concat "** "
      (mapconcat 'identity
		(list "DNS Lookup" host dns-lookup-program)
		" ** "))
     dns-lookup-program
     options)))

(autoload 'ffap-string-at-point "ffap")

;;;###autoload
(defun run-dig (host)
  "Run dig program."
  (interactive
   (list
    (read-from-minibuffer "Lookup host: "
                          (or (ffap-string-at-point 'machine) ""))))
  (net-utils-run-program
   "Dig"
   (concat "** "
	   (mapconcat 'identity
		      (list "Dig" host dig-program)
		      " ** "))
   dig-program
   (list host)))

(autoload 'comint-exec "comint")

;; This is a lot less than ange-ftp, but much simpler.
;;;###autoload
(defun ftp (host)
  "Run ftp program."
  (interactive
   (list
    (read-from-minibuffer
     "Ftp to Host: " (net-utils-machine-at-point))))
  (let ((buf (get-buffer-create (concat "*ftp [" host "]*"))))
    (set-buffer buf)
    (ftp-mode)
    (comint-exec buf (concat "ftp-" host) ftp-program nil
		 (if ftp-program-options
		     (append (list host) ftp-program-options)
		   (list host)))
    (pop-to-buffer buf)))

(defvar ftp-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Occasionally useful
    (define-key map "\t" 'comint-dynamic-complete)
    map))

(define-derived-mode ftp-mode comint-mode "FTP"
  "Major mode for interacting with the ftp program."
  (setq comint-prompt-regexp ftp-prompt-regexp)
  (setq comint-input-autoexpand t)
  ;; Only add the password-prompting hook if it's not already in the
  ;; global hook list.  This stands a small chance of losing, if it's
  ;; later removed from the global list (very small, since any
  ;; password prompts will probably immediately follow the initial
  ;; connection), but it's better than getting prompted twice for the
  ;; same password.
  (unless (memq 'comint-watch-for-password-prompt
		(default-value 'comint-output-filter-functions))
    (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt
	      nil t)))

(defun smbclient (host service)
  "Connect to SERVICE on HOST via SMB."
  (interactive
   (list
    (read-from-minibuffer
     "Connect to Host: " (net-utils-machine-at-point))
    (read-from-minibuffer "SMB Service: ")))
  (let* ((name (format "smbclient [%s\\%s]" host service))
	 (buf (get-buffer-create (concat "*" name "*")))
	 (service-name (concat "\\\\" host "\\" service)))
    (set-buffer buf)
    (smbclient-mode)
    (comint-exec buf name smbclient-program nil
		 (if smbclient-program-options
		     (append (list service-name) smbclient-program-options)
		   (list service-name)))
    (pop-to-buffer buf)))

(defun smbclient-list-shares (host)
  "List services on HOST."
  (interactive
   (list
    (read-from-minibuffer
     "Connect to Host: " (net-utils-machine-at-point))))
  (let ((buf (get-buffer-create (format "*SMB Shares on %s*" host))))
    (set-buffer buf)
    (smbclient-mode)
    (comint-exec buf "smbclient-list-shares"
		 smbclient-program nil (list "-L" host))
    (pop-to-buffer buf)))

(define-derived-mode smbclient-mode comint-mode "smbclient"
  "Major mode for interacting with the smbclient program."
  (setq comint-prompt-regexp smbclient-prompt-regexp)
  (setq comint-input-autoexpand t)
  ;; Only add the password-prompting hook if it's not already in the
  ;; global hook list.  This stands a small chance of losing, if it's
  ;; later removed from the global list (very small, since any
  ;; password prompts will probably immediately follow the initial
  ;; connection), but it's better than getting prompted twice for the
  ;; same password.
  (unless (memq 'comint-watch-for-password-prompt
		(default-value 'comint-output-filter-functions))
    (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt
	      nil t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network Connections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Full list is available at:
;; http://www.iana.org/assignments/port-numbers
(defvar network-connection-service-alist
  (list
    (cons 'echo          7)
    (cons 'active-users 11)
    (cons 'daytime      13)
    (cons 'chargen      19)
    (cons 'ftp          21)
    (cons 'telnet	23)
    (cons 'smtp		25)
    (cons 'time		37)
    (cons 'whois        43)
    (cons 'gopher       70)
    (cons 'finger       79)
    (cons 'www		80)
    (cons 'pop2		109)
    (cons 'pop3		110)
    (cons 'sun-rpc	111)
    (cons 'nntp		119)
    (cons 'ntp		123)
    (cons 'netbios-name 137)
    (cons 'netbios-data 139)
    (cons 'irc		194)
    (cons 'https	443)
    (cons 'rlogin	513))
  "Alist of services and associated TCP port numbers.
This list is not complete.")

;; Workhorse routine
(defun run-network-program (process-name host port &optional initial-string)
  (let ((tcp-connection)
	(buf))
    (setq buf (get-buffer-create (concat "*" process-name "*")))
    (set-buffer buf)
    (or
     (setq tcp-connection
	   (open-network-stream process-name buf host port))
     (error "Could not open connection to %s" host))
    (erase-buffer)
    (set-marker (process-mark tcp-connection) (point-min))
    (set-process-filter tcp-connection 'net-utils-remove-ctrl-m-filter)
    (and initial-string
	 (process-send-string tcp-connection
			      (concat initial-string "\r\n")))
    (display-buffer buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom finger-X.500-host-regexps nil
  "A list of regular expressions matching host names.
If a host name passed to `finger' matches one of these regular
expressions, it is assumed to be a host that doesn't accept
queries of the form USER@HOST, and wants a query containing USER only."
  :group 'net-utils
  :type '(repeat regexp)
  :version "21.1")

;; Finger protocol
;;;###autoload
(defun finger (user host)
  "Finger USER on HOST."
  ;; One of those great interactive statements that's actually
  ;; longer than the function call! The idea is that if the user
  ;; uses a string like "pbreton@cs.umb.edu", we won't ask for the
  ;; host name. If we don't see an "@", we'll prompt for the host.
  (interactive
    (let* ((answer (read-from-minibuffer "Finger User: "
					 (net-utils-url-at-point)))
	   (index  (string-match (regexp-quote "@") answer)))
      (if index
	  (list (substring answer 0 index)
		(substring answer (1+ index)))
	(list answer
	      (read-from-minibuffer "At Host: "
				    (net-utils-machine-at-point))))))
  (let* ((user-and-host (concat user "@" host))
	 (process-name (concat "Finger [" user-and-host "]"))
	 (regexps finger-X.500-host-regexps)
	 found)
    (and regexps
	 (while (not (string-match (car regexps) host))
	   (setq regexps (cdr regexps)))
	 (when regexps
	   (setq user-and-host user)))
    (run-network-program
     process-name
     host
     (cdr (assoc 'finger network-connection-service-alist))
     user-and-host)))

(defcustom whois-server-name "rs.internic.net"
  "Default host name for the whois service."
  :group 'net-utils
  :type  'string)

(defcustom whois-server-list
  '(("whois.arin.net")     ; Networks, ASN's, and related POC's (numbers)
    ("rs.internic.net")  ; domain related info
    ("whois.publicinterestregistry.net")
    ("whois.abuse.net")
    ("whois.apnic.net")
    ("nic.ddn.mil")
    ("whois.nic.mil")
    ("whois.nic.gov")
    ("whois.ripe.net"))
  "A list of whois servers that can be queried."
  :group 'net-utils
  :type '(repeat (list string)))

;; FIXME: modern whois clients include a much better tld <-> whois server
;; list, Emacs should probably avoid specifying the server as the client
;; will DTRT anyway... -rfr
(defcustom whois-server-tld
  '(("rs.internic.net" . "com")
    ("whois.publicinterestregistry.net" . "org")
    ("whois.ripe.net" . "be")
    ("whois.ripe.net" . "de")
    ("whois.ripe.net" . "dk")
    ("whois.ripe.net" . "it")
    ("whois.ripe.net" . "fi")
    ("whois.ripe.net" . "fr")
    ("whois.ripe.net" . "uk")
    ("whois.apnic.net" . "au")
    ("whois.apnic.net" . "ch")
    ("whois.apnic.net" . "hk")
    ("whois.apnic.net" . "jp")
    ("whois.nic.gov" . "gov")
    ("whois.nic.mil" . "mil"))
  "Alist to map top level domains to whois servers."
  :group 'net-utils
  :type '(repeat (cons string string)))

(defcustom whois-guess-server t
  "If non-nil then whois will try to deduce the appropriate whois
server from the query.  If the query doesn't look like a domain or hostname
then the server named by `whois-server-name' is used."
  :group 'net-utils
  :type 'boolean)

(defun whois-get-tld (host)
  "Return the top level domain of `host', or nil if it isn't a domain name."
  (let ((i (1- (length host)))
	(max-len (- (length host) 5)))
    (while (not (or (= i max-len) (char-equal (aref host i) ?.)))
      (setq i (1- i)))
    (if (= i max-len)
	nil
      (substring host (1+ i)))))

;; Whois protocol
;;;###autoload
(defun whois (arg search-string)
  "Send SEARCH-STRING to server defined by the `whois-server-name' variable.
If `whois-guess-server' is non-nil, then try to deduce the correct server
from SEARCH-STRING.  With argument, prompt for whois server."
  (interactive "P\nsWhois: ")
  (let* ((whois-apropos-host (if whois-guess-server
				 (rassoc (whois-get-tld search-string)
					 whois-server-tld)
			       nil))
	 (server-name (if whois-apropos-host
			  (car whois-apropos-host)
			whois-server-name))
	 (host
	  (if arg
	      (completing-read "Whois server name: "
			       whois-server-list nil nil "whois.")
	    server-name)))
    (run-network-program
     "Whois"
     host
     (cdr (assoc 'whois network-connection-service-alist))
     search-string)))

(defcustom whois-reverse-lookup-server "whois.arin.net"
  "Server which provides inverse DNS mapping."
  :group 'net-utils
  :type  'string)

;;;###autoload
(defun whois-reverse-lookup ()
  (interactive)
  (let ((whois-server-name whois-reverse-lookup-server))
    (call-interactively 'whois)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Network connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Using a derived mode gives us keymaps, hooks, etc.
(define-derived-mode
  network-connection-mode comint-mode "Network-Connection"
  "Major mode for interacting with the network-connection program.")

(defun network-connection-mode-setup (host service)
  (make-local-variable 'network-connection-host)
  (setq network-connection-host host)
  (make-local-variable 'network-connection-service)
  (setq network-connection-service service))

;;;###autoload
(defun network-connection-to-service (host service)
  "Open a network connection to SERVICE on HOST."
  (interactive
   (list
    (read-from-minibuffer "Host: " (net-utils-machine-at-point))
    (completing-read "Service: "
		     (mapcar
		      (function
		       (lambda (elt)
			 (list (symbol-name (car elt)))))
		      network-connection-service-alist))))
  (network-connection
   host
   (cdr (assoc (intern service) network-connection-service-alist))))

;;;###autoload
(defun network-connection (host port)
  "Open a network connection to HOST on PORT."
  (interactive "sHost: \nnPort: ")
  (network-service-connection host (number-to-string port)))

(defun network-service-connection (host service)
  "Open a network connection to SERVICE on HOST."
  (let* ((process-name (concat "Network Connection [" host " " service "]"))
	 (portnum (string-to-number service))
	 (buf (get-buffer-create (concat "*" process-name "*"))))
    (or (zerop portnum) (setq service portnum))
    (make-comint
     process-name
     (cons host service))
    (set-buffer buf)
    (network-connection-mode)
    (network-connection-mode-setup host service)
    (pop-to-buffer buf)))

(defvar comint-input-ring)

(defun network-connection-reconnect  ()
  "Reconnect a network connection, preserving the old input ring."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer)))
	(old-comint-input-ring comint-input-ring)
	(host network-connection-host)
	(service network-connection-service))
    (if (not (or (not proc)
		 (eq (process-status proc) 'closed)))
	(message "Still connected")
      (goto-char (point-max))
      (insert (format "Reopening connection to %s\n" host))
      (network-connection host
			  (if (numberp service)
			      service
			    (cdr (assoc service network-connection-service-alist))))
      (and old-comint-input-ring
	   (setq comint-input-ring old-comint-input-ring)))))

(provide 'net-utils)

;;; net-utils.el ends here
