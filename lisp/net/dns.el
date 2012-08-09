;;; dns.el --- Domain Name Service lookups

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
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

;;; Code:

(defvar dns-timeout 5
  "How many seconds to wait when doing DNS queries.")

(defvar dns-servers nil
  "List of DNS servers to query.
If nil, /etc/resolv.conf and nslookup will be consulted.")

;;; Internal code:

(defvar dns-query-types
  '((A 1)
    (NS 2)
    (MD 3)
    (MF 4)
    (CNAME 5)
    (SOA 6)
    (MB 7)
    (MG 8)
    (MR 9)
    (NULL 10)
    (WKS 11)
    (PTR 12)
    (HINFO 13)
    (MINFO 14)
    (MX 15)
    (TXT 16)
    (AAAA 28) ; RFC3596
    (SRV 33) ; RFC2782
    (AXFR 252)
    (MAILB 253)
    (MAILA 254)
    (* 255))
  "Names of query types and their values.")

(defvar dns-classes
  '((IN 1)
    (CS 2)
    (CH 3)
    (HS 4))
  "Classes of queries.")

(defun dns-write-bytes (value &optional length)
  (let (bytes)
    (dotimes (i (or length 1))
      (push (% value 256) bytes)
      (setq value (/ value 256)))
    (dolist (byte bytes)
      (insert byte))))

(defun dns-read-bytes (length)
  (let ((value 0))
    (dotimes (i length)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun dns-get (type spec)
  (cadr (assq type spec)))

(defun dns-inverse-get (value spec)
  (let ((found nil))
    (while (and (not found)
		spec)
      (if (eq value (cadr (car spec)))
	  (setq found (caar spec))
	(pop spec)))
    found))

(defun dns-write-name (name)
  (dolist (part (split-string name "\\."))
    (dns-write-bytes (length part))
    (insert part))
  (dns-write-bytes 0))

(defun dns-read-string-name (string buffer)
  (with-temp-buffer
    (unless (featurep 'xemacs) (set-buffer-multibyte nil))
    (insert string)
    (goto-char (point-min))
    (dns-read-name buffer)))

(defun dns-read-name (&optional buffer)
  (let ((ended nil)
	(name nil)
	length)
    (while (not ended)
      (setq length (dns-read-bytes 1))
      (if (= 192 (logand length (lsh 3 6)))
	  (let ((offset (+ (* (logand 63 length) 256)
			   (dns-read-bytes 1))))
	    (save-excursion
	      (when buffer
		(set-buffer buffer))
	      (goto-char (1+ offset))
	      (setq ended (dns-read-name buffer))))
	(if (zerop length)
	    (setq ended t)
	  (push (buffer-substring (point)
				  (progn (forward-char length) (point)))
		name))))
    (if (stringp ended)
	(if (null name)
	    ended
	  (concat (mapconcat 'identity (nreverse name) ".") "." ended))
      (mapconcat 'identity (nreverse name) "."))))

(defun dns-write (spec &optional tcp-p)
  "Write a DNS packet according to SPEC.
If TCP-P, the first two bytes of the package with be the length field."
  (with-temp-buffer
    (unless (featurep 'xemacs) (set-buffer-multibyte nil))
    (dns-write-bytes (dns-get 'id spec) 2)
    (dns-write-bytes
     (logior
      (lsh (if (dns-get 'response-p spec) 1 0) -7)
      (lsh
       (cond
	((eq (dns-get 'opcode spec) 'query) 0)
	((eq (dns-get 'opcode spec) 'inverse-query) 1)
	((eq (dns-get 'opcode spec) 'status) 2)
	(t (error "No such opcode: %s" (dns-get 'opcode spec))))
       -3)
      (lsh (if (dns-get 'authoritative-p spec) 1 0) -2)
      (lsh (if (dns-get 'truncated-p spec) 1 0) -1)
      (lsh (if (dns-get 'recursion-desired-p spec) 1 0) 0)))
    (dns-write-bytes
     (cond
      ((eq (dns-get 'response-code spec) 'no-error) 0)
      ((eq (dns-get 'response-code spec) 'format-error) 1)
      ((eq (dns-get 'response-code spec) 'server-failure) 2)
      ((eq (dns-get 'response-code spec) 'name-error) 3)
      ((eq (dns-get 'response-code spec) 'not-implemented) 4)
      ((eq (dns-get 'response-code spec) 'refused) 5)
      (t 0)))
    (dns-write-bytes (length (dns-get 'queries spec)) 2)
    (dns-write-bytes (length (dns-get 'answers spec)) 2)
    (dns-write-bytes (length (dns-get 'authorities spec)) 2)
    (dns-write-bytes (length (dns-get 'additionals spec)) 2)
    (dolist (query (dns-get 'queries spec))
      (dns-write-name (car query))
      (dns-write-bytes (cadr (assq (or (dns-get 'type query) 'A)
				   dns-query-types)) 2)
      (dns-write-bytes (cadr (assq (or (dns-get 'class query) 'IN)
				   dns-classes)) 2))
    (dolist (slot '(answers authorities additionals))
      (dolist (resource (dns-get slot spec))
	(dns-write-name (car resource))
      (dns-write-bytes (cadr (assq (dns-get 'type resource) dns-query-types))
		       2)
      (dns-write-bytes (cadr (assq (dns-get 'class resource) dns-classes))
		       2)
      (dns-write-bytes (dns-get 'ttl resource) 4)
      (dns-write-bytes (length (dns-get 'data resource)) 2)
      (insert (dns-get 'data resource))))
    (when tcp-p
      (goto-char (point-min))
      (dns-write-bytes (buffer-size) 2))
    (buffer-string)))

(defun dns-read (packet)
  (with-temp-buffer
    (unless (featurep 'xemacs) (set-buffer-multibyte nil))
    (let ((spec nil)
          queries answers authorities additionals)
      (insert packet)
      (goto-char (point-min))
      (push (list 'id (dns-read-bytes 2)) spec)
      (let ((byte (dns-read-bytes 1)))
        (push (list 'response-p (if (zerop (logand byte (lsh 1 7))) nil t))
              spec)
        (let ((opcode (logand byte (lsh 7 3))))
          (push (list 'opcode
                      (cond ((eq opcode 0) 'query)
                            ((eq opcode 1) 'inverse-query)
                            ((eq opcode 2) 'status)))
                spec))
        (push (list 'authoritative-p (if (zerop (logand byte (lsh 1 2)))
                                         nil t)) spec)
        (push (list 'truncated-p (if (zerop (logand byte (lsh 1 2))) nil t))
              spec)
        (push (list 'recursion-desired-p
                    (if (zerop (logand byte (lsh 1 0))) nil t)) spec))
      (let ((rc (logand (dns-read-bytes 1) 15)))
        (push (list 'response-code
                    (cond
                     ((eq rc 0) 'no-error)
                     ((eq rc 1) 'format-error)
                     ((eq rc 2) 'server-failure)
                     ((eq rc 3) 'name-error)
                     ((eq rc 4) 'not-implemented)
                     ((eq rc 5) 'refused)))
              spec))
      (setq queries (dns-read-bytes 2))
      (setq answers (dns-read-bytes 2))
      (setq authorities (dns-read-bytes 2))
      (setq additionals (dns-read-bytes 2))
      (let ((qs nil))
        (dotimes (i queries)
          (push (list (dns-read-name)
                      (list 'type (dns-inverse-get (dns-read-bytes 2)
                                                   dns-query-types))
                      (list 'class (dns-inverse-get (dns-read-bytes 2)
                                                    dns-classes)))
                qs))
        (push (list 'queries qs) spec))
      (dolist (slot '(answers authorities additionals))
        (let ((qs nil)
              type)
          (dotimes (i (symbol-value slot))
            (push (list (dns-read-name)
                        (list 'type
                              (setq type (dns-inverse-get (dns-read-bytes 2)
                                                          dns-query-types)))
                        (list 'class (dns-inverse-get (dns-read-bytes 2)
                                                      dns-classes))
                        (list 'ttl (dns-read-bytes 4))
                        (let ((length (dns-read-bytes 2)))
                          (list 'data
                                (dns-read-type
                                 (buffer-substring
                                  (point)
                                  (progn (forward-char length) (point)))
                                 type))))
                  qs))
          (push (list slot qs) spec)))
      (nreverse spec))))

(defun dns-read-int32 ()
  ;; Full 32 bit Integers can't be handled by 32-bit Emacsen.  If we
  ;; use floats, it works.
  (format "%.0f" (+ (* (dns-read-bytes 1) 16777216.0)
		    (dns-read-bytes 3))))

(defun dns-read-type (string type)
  (let ((buffer (current-buffer))
	(point (point)))
    (prog1
        (with-temp-buffer
	  (unless (featurep 'xemacs) (set-buffer-multibyte nil))
          (insert string)
          (goto-char (point-min))
          (cond
           ((eq type 'A)
            (let ((bytes nil))
              (dotimes (i 4)
                (push (dns-read-bytes 1) bytes))
              (mapconcat 'number-to-string (nreverse bytes) ".")))
           ((eq type 'AAAA)
            (let (hextets)
              (dotimes (i 8)
                (push (dns-read-bytes 2) hextets))
              (mapconcat (lambda (n) (format "%x" n))
                         (nreverse hextets) ":")))
           ((eq type 'SOA)
            (list (list 'mname (dns-read-name buffer))
                  (list 'rname (dns-read-name buffer))
                  (list 'serial (dns-read-int32))
                  (list 'refresh (dns-read-int32))
                  (list 'retry (dns-read-int32))
                  (list 'expire (dns-read-int32))
                  (list 'minimum (dns-read-int32))))
           ((eq type 'SRV)
            (list (list 'priority (dns-read-bytes 2))
                  (list 'weight (dns-read-bytes 2))
                  (list 'port (dns-read-bytes 2))
                  (list 'target (dns-read-name buffer))))
           ((eq type 'MX)
            (cons (dns-read-bytes 2) (dns-read-name buffer)))
           ((or (eq type 'CNAME) (eq type 'NS) (eq type 'PTR))
            (dns-read-string-name string buffer))
           (t string)))
      (goto-char point))))

(defun dns-set-servers ()
  "Set `dns-servers' to a list of DNS servers or nil if none are found.
Parses \"/etc/resolv.conf\" or calls \"nslookup\"."
  (or (when (file-exists-p "/etc/resolv.conf")
	(setq dns-servers nil)
	(with-temp-buffer
	  (insert-file-contents "/etc/resolv.conf")
	  (goto-char (point-min))
	  (while (re-search-forward "^nameserver[\t ]+\\([^ \t\n]+\\)" nil t)
	    (push (match-string 1) dns-servers))
	  (setq dns-servers (nreverse dns-servers))))
      (when (executable-find "nslookup")
	(with-temp-buffer
	  (call-process "nslookup" nil t nil "localhost")
	  (goto-char (point-min))
	  (re-search-forward
	   "^Address:[ \t]*\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" nil t)
	  (setq dns-servers (list (match-string 1)))))))

(defun dns-read-txt (string)
  (if (> (length string) 1)
      (substring string 1)
    string))

(defun dns-get-txt-answer (answers)
  (let ((result "")
	(do-next nil))
    (dolist (answer answers)
      (dolist (elem answer)
	(when (consp elem)
	  (cond
	   ((eq (car elem) 'type)
	    (setq do-next (eq (cadr elem) 'TXT)))
	   ((eq (car elem) 'data)
	    (when do-next
	      (setq result (concat result (dns-read-txt (cadr elem))))))))))
    result))

;;; Interface functions.
(defmacro dns-make-network-process (server)
  (if (featurep 'xemacs)
      `(let ((coding-system-for-read 'binary)
	     (coding-system-for-write 'binary))
	 (open-network-stream "dns" (current-buffer)
			      ,server "domain" 'udp))
    `(let ((server ,server)
	   (coding-system-for-read 'binary)
	   (coding-system-for-write 'binary))
       (if (fboundp 'make-network-process)
	   (make-network-process
	    :name "dns"
	    :coding 'binary
	    :buffer (current-buffer)
	    :host server
	    :service "domain"
	    :type 'datagram)
	 ;; Older versions of Emacs doesn't have
	 ;; `make-network-process', so we fall back on opening a TCP
	 ;; connection to the DNS server.
	 (open-network-stream "dns" (current-buffer) server "domain")))))

(defvar dns-cache (make-vector 4096 0))

(defun dns-query-cached (name &optional type fullp reversep)
  (let* ((key (format "%s:%s:%s:%s" name type fullp reversep))
	 (sym (intern-soft key dns-cache)))
    (if (and sym
	     (boundp sym))
	(symbol-value sym)
      (let ((result (dns-query name type fullp reversep)))
	(set (intern key dns-cache) result)
	result))))

;; The old names `query-dns' and `query-dns-cached' weren't used in Emacs 23
;; yet, so no alias are provided.  --rsteib

(defun dns-query (name &optional type fullp reversep)
  "Query a DNS server for NAME of TYPE.
If FULLP, return the entire record returned.
If REVERSEP, look up an IP address."
  (setq type (or type 'A))
  (unless dns-servers
    (dns-set-servers))

  (when reversep
    (setq name (concat
		(mapconcat 'identity (nreverse (split-string name "\\.")) ".")
		".in-addr.arpa")
	  type 'PTR))

  (if (not dns-servers)
      (message "No DNS server configuration found")
    (with-temp-buffer
      (unless (featurep 'xemacs) (set-buffer-multibyte nil))
      (let ((process (condition-case ()
                         (dns-make-network-process (car dns-servers))
                       (error
                        (message
                         "dns: Got an error while trying to talk to %s"
                         (car dns-servers))
                        nil)))
            (tcp-p (and (not (fboundp 'make-network-process))
                        (not (featurep 'xemacs))))
            (step 100)
            (times (* dns-timeout 1000))
            (id (random 65000)))
        (when process
          (process-send-string
           process
           (dns-write `((id ,id)
                        (opcode query)
                        (queries ((,name (type ,type))))
                        (recursion-desired-p t))
                      tcp-p))
          (while (and (zerop (buffer-size))
                      (> times 0))
            (sit-for (/ step 1000.0))
            (accept-process-output process 0 step)
            (setq times (- times step)))
          (condition-case nil
              (delete-process process)
            (error nil))
          (when (and tcp-p
                     (>= (buffer-size) 2))
            (goto-char (point-min))
            (delete-region (point) (+ (point) 2)))
          (when (and (>= (buffer-size) 2)
                     ;; We had a time-out.
                     (> times 0))
            (let ((result (dns-read (buffer-string))))
              (if fullp
                  result
                (let ((answer (car (dns-get 'answers result))))
                  (when (eq type (dns-get 'type answer))
                    (if (eq type 'TXT)
                        (dns-get-txt-answer (dns-get 'answers result))
                      (dns-get 'data answer))))))))))))

(provide 'dns)

;;; dns.el ends here
