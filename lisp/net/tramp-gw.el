;;; tramp-gw.el --- Tramp utility functions for HTTP tunnels and SOCKS gateways

;; Copyright (C) 2007-2012 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, processes
;; Package: tramp

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

;; Access functions for HTTP tunnels and SOCKS gateways from Tramp.
;; SOCKS functionality is implemented by socks.el from the w3 package.
;; HTTP tunnels are partly implemented in socks.el and url-http.el;
;; both implementations are not complete.  Therefore, it is
;; implemented in this package.

;;; Code:

(require 'tramp)

;; Pacify byte-compiler
(eval-when-compile
  (require 'cl)
  (require 'custom))

;; Avoid byte-compiler warnings if the byte-compiler supports this.
;; Currently, XEmacs supports this.
(eval-when-compile
  (when (featurep 'xemacs)
      (byte-compiler-options (warnings (- unused-vars)))))

;; We don't add the following methods to `tramp-methods', in order to
;; exclude them from file name completion.

;; Define HTTP tunnel method ...
;;;###tramp-autoload
(defconst tramp-gw-tunnel-method "tunnel"
  "*Method to connect HTTP gateways.")

;; ... and port.
(defconst tramp-gw-default-tunnel-port 8080
  "*Default port for HTTP gateways.")

;; Define SOCKS method ...
;;;###tramp-autoload
(defconst tramp-gw-socks-method "socks"
  "*Method to connect SOCKS servers.")

;; ... and port.
(defconst tramp-gw-default-socks-port 1080
  "*Default port for SOCKS servers.")

;; Autoload the socks library.  It is used only when we access a SOCKS server.
(autoload 'socks-open-network-stream "socks")
(defvar socks-username (user-login-name))
(defvar socks-server
  (list "Default server" "socks" tramp-gw-default-socks-port 5))

;; Add a default for `tramp-default-user-alist'.  Default is the local user.
;;;###tramp-autoload
(add-to-list
 'tramp-default-user-alist
 (list (concat "\\`"
	       (regexp-opt (list tramp-gw-tunnel-method tramp-gw-socks-method))
	       "\\'")
       nil (user-login-name)))

;; Internal file name functions and variables.

(defvar tramp-gw-vector nil
  "Keeps the remote host identification.  Needed for Tramp messages.")

(defvar tramp-gw-gw-vector nil
  "Current gateway identification vector.")

(defvar tramp-gw-gw-proc nil
  "Current gateway process.")

;; This variable keeps the listening process, in order to reuse it for
;; new processes.
(defvar tramp-gw-aux-proc nil
  "Process listening on local port, as mediation between SSH and the gateway.")

(defun tramp-gw-gw-proc-sentinel (proc event)
  "Delete auxiliary process when we are deleted."
  (unless (memq (process-status proc) '(run open))
    (tramp-message
     tramp-gw-vector 4 "Deleting auxiliary process `%s'" tramp-gw-gw-proc)
    (let* (tramp-verbose
	   (p (tramp-get-connection-property proc "process" nil)))
      (when (processp p) (delete-process p)))))

(defun tramp-gw-aux-proc-sentinel (proc event)
  "Activate the different filters for involved gateway and auxiliary processes."
  (when (memq (process-status proc) '(run open))
    ;; A new process has been spawned from `tramp-gw-aux-proc'.
    (tramp-message
     tramp-gw-vector 4
     "Opening auxiliary process `%s', speaking with process `%s'"
     proc tramp-gw-gw-proc)
    (tramp-compat-set-process-query-on-exit-flag proc nil)
    ;; We don't want debug messages, because the corresponding debug
    ;; buffer might be undecided.
    (let (tramp-verbose)
      (tramp-set-connection-property tramp-gw-gw-proc "process" proc)
      (tramp-set-connection-property proc "process" tramp-gw-gw-proc))
    ;; Set the process-filter functions for both processes.
    (set-process-filter proc 'tramp-gw-process-filter)
    (set-process-filter tramp-gw-gw-proc 'tramp-gw-process-filter)
    ;; There might be already some output from the gateway process.
    (with-current-buffer (process-buffer tramp-gw-gw-proc)
      (unless (= (point-min) (point-max))
	(let ((s (buffer-string)))
	  (delete-region (point) (point-max))
	  (tramp-gw-process-filter tramp-gw-gw-proc s))))))

(defun tramp-gw-process-filter (proc string)
  (let (tramp-verbose)
    (process-send-string
     (tramp-get-connection-property proc "process" nil) string)))

;;;###tramp-autoload
(defun tramp-gw-open-connection (vec gw-vec target-vec)
  "Open a remote connection to VEC (see `tramp-file-name' structure).
Take GW-VEC as SOCKS or HTTP gateway, i.e. its method must be a
gateway method.  TARGET-VEC identifies where to connect to via
the gateway, it can be different from VEC when there are more
hops to be applied.

It returns a string like \"localhost#port\", which must be used
instead of the host name declared in TARGET-VEC."

  ;; Remember vectors for property retrieval.
  (setq tramp-gw-vector vec
	tramp-gw-gw-vector gw-vec)

  ;; Start listening auxiliary process.
  (unless (and (processp tramp-gw-aux-proc)
	       (memq (process-status tramp-gw-aux-proc) '(listen)))
    (let ((aux-vec
	   (vector "aux" (tramp-file-name-user gw-vec)
		   (tramp-file-name-host gw-vec) nil)))
      (setq tramp-gw-aux-proc
	    (make-network-process
	     :name (tramp-buffer-name aux-vec) :buffer nil :host 'local
	     :server t :noquery t :service t :coding 'binary))
      (set-process-sentinel tramp-gw-aux-proc 'tramp-gw-aux-proc-sentinel)
      (tramp-compat-set-process-query-on-exit-flag tramp-gw-aux-proc nil)
      (tramp-message
       vec 4 "Opening auxiliary process `%s', listening on port %d"
       tramp-gw-aux-proc (process-contact tramp-gw-aux-proc :service))))

  (let* ((gw-method
	  (intern
	   (tramp-find-method
	    (tramp-file-name-method gw-vec)
	    (tramp-file-name-user gw-vec)
	    (tramp-file-name-host gw-vec))))
	 (socks-username
	  (tramp-find-user
	   (tramp-file-name-method gw-vec)
	   (tramp-file-name-user gw-vec)
	   (tramp-file-name-host gw-vec)))
	 ;; Declare the SOCKS server to be used.
	 (socks-server
	  (list "Tramp temporary socks server list"
		;; Host name.
		(tramp-file-name-real-host gw-vec)
		;; Port number.
		(or (tramp-file-name-port gw-vec)
		    (case gw-method
		      (tunnel tramp-gw-default-tunnel-port)
		      (socks tramp-gw-default-socks-port)))
		;; Type.  We support only http and socks5, NO socks4.
		;; 'http could be used when HTTP tunnel works in socks.el.
		5))
	 ;; The function to be called.
	 (socks-function
	  (case gw-method
	    (tunnel 'tramp-gw-open-network-stream)
	    (socks 'socks-open-network-stream)))
	 socks-noproxy)

    ;; Open SOCKS process.
    (setq tramp-gw-gw-proc
	  (funcall
	   socks-function
	   (tramp-get-connection-name gw-vec)
	   (tramp-get-connection-buffer gw-vec)
	   (tramp-file-name-real-host target-vec)
	   (tramp-file-name-port target-vec)))
    (set-process-sentinel tramp-gw-gw-proc 'tramp-gw-gw-proc-sentinel)
    (tramp-compat-set-process-query-on-exit-flag tramp-gw-gw-proc nil)
    (tramp-message
     vec 4 "Opened %s process `%s'"
     (case gw-method ('tunnel "HTTP tunnel") ('socks "SOCKS"))
     tramp-gw-gw-proc)

    ;; Return the new host for gateway access.
    (format "localhost#%d" (process-contact tramp-gw-aux-proc :service))))

(defun tramp-gw-open-network-stream (name buffer host service)
  "Open stream to proxy server HOST:SERVICE.
Resulting process has name NAME and buffer BUFFER.  If
authentication is requested from proxy server, provide it."
  (let ((command (format (concat
			  "CONNECT %s:%d HTTP/1.1\r\n"
			  "Host: %s:%d\r\n"
			  "Connection: keep-alive\r\n"
			  "User-Agent: Tramp/%s\r\n")
			 host service host service tramp-version))
	(authentication "")
	(first t)
	found proc)

    (while (not found)
      ;; Clean up.
      (when (processp proc) (delete-process proc))
      (with-current-buffer buffer (erase-buffer))
      ;; Open network stream.
      (setq proc (open-network-stream
		  name buffer (nth 1 socks-server) (nth 2 socks-server)))
      (set-process-coding-system proc 'binary 'binary)
      (tramp-compat-set-process-query-on-exit-flag proc nil)
      ;; Send CONNECT command.
      (process-send-string proc (format "%s%s\r\n" command authentication))
      (tramp-message
       tramp-gw-vector 6 "\n%s"
       (format
	"%s%s\r\n" command
	(replace-regexp-in-string ;; no password in trace!
	 "Basic [^\r\n]+" "Basic xxxxx" authentication t)))
      (with-current-buffer buffer
	;; Trap errors to be traced in the right trace buffer.  Often,
	;; proxies have a timeout of 60".  We wait 65" in order to
	;; receive an answer this case.
	(ignore-errors
	  (let (tramp-verbose)
	    (tramp-wait-for-regexp proc 65 "\r?\n\r?\n")))
	;; Check return code.
	(goto-char (point-min))
	(narrow-to-region
	 (point-min)
	 (or (search-forward-regexp "\r?\n\r?\n" nil t) (point-max)))
	(tramp-message tramp-gw-vector 6 "\n%s" (buffer-string))
	(goto-char (point-min))
	(search-forward-regexp "^HTTP/[1-9]\\.[0-9]" nil t)
	(case (condition-case nil (read (current-buffer)) (error))
	  ;; Connected.
	  (200 (setq found t))
	  ;; We need basic authentication.
	  (401 (setq authentication (tramp-gw-basic-authentication nil first)))
	  ;; Target host not found.
	  (404 (tramp-error-with-buffer
		(current-buffer) tramp-gw-vector 'file-error
		"Host %s not found." host))
	  ;; We need basic proxy authentication.
	  (407 (setq authentication (tramp-gw-basic-authentication t first)))
	  ;; Connection failed.
	  (503 (tramp-error-with-buffer
		(current-buffer) tramp-gw-vector 'file-error
		"Connection to %s:%d failed." host service))
	  ;; That doesn't work at all.
	  (t (tramp-error-with-buffer
	      (current-buffer) tramp-gw-vector 'file-error
	      "Access to HTTP server %s:%d failed."
	      (nth 1 socks-server) (nth 2 socks-server))))
	;; Remove HTTP headers.
	(delete-region (point-min) (point-max))
	(widen)
	(setq first nil)))
    ;; Return the process.
    proc))

(defun tramp-gw-basic-authentication (proxy pw-cache)
  "Return authentication header for CONNECT, based on server request.
PROXY is an indication whether we need a Proxy-Authorization header
or an Authorization header.  If PW-CACHE is non-nil, check for
password in password cache.  This is done for the first try only."

  ;; `tramp-current-*' must be set for `tramp-read-passwd'.
  (let ((tramp-current-method (tramp-file-name-method tramp-gw-gw-vector))
	(tramp-current-user (tramp-file-name-user tramp-gw-gw-vector))
	(tramp-current-host (tramp-file-name-host tramp-gw-gw-vector)))
    (unless pw-cache (tramp-clear-passwd tramp-gw-gw-vector))
    ;; We are already in the right buffer.
    (tramp-message
     tramp-gw-vector 5 "%s required"
     (if proxy "Proxy authentication" "Authentication"))
    ;; Search for request header.  We accept only basic authentication.
    (goto-char (point-min))
    (search-forward-regexp
     "^\\(Proxy\\|WWW\\)-Authenticate:\\s-*Basic\\s-+realm=")
    ;; Return authentication string.
    (format
     "%s: Basic %s\r\n"
     (if proxy "Proxy-Authorization" "Authorization")
     (base64-encode-string
      (format
       "%s:%s"
       socks-username
       (tramp-read-passwd
	nil
	(format
	 "Password for %s@[%s]: " socks-username (read (current-buffer)))))))))

(add-hook 'tramp-unload-hook
	  (lambda ()
	    (unload-feature 'tramp-gw 'force)))

(provide 'tramp-gw)

;;; TODO:

;; * Provide descriptive Commentary.
;; * Enable it for several gateway processes in parallel.

;;; tramp-gw.el ends here
