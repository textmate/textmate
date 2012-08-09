;;; url-ns.el --- Various netscape-ish functions for proxy definitions

;; Copyright (C) 1997-1999, 2004-2012 Free Software Foundation, Inc.

;; Keywords: comm, data, processes, hypermedia

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

;;; Code:

(require 'url-gw)

;;;###autoload
(defun isPlainHostName (host)
  (not (string-match "\\." host)))

;;;###autoload
(defun dnsDomainIs (host dom)
  (string-match (concat (regexp-quote dom) "$") host))

;;;###autoload
(defun dnsResolve (host)
  (url-gateway-nslookup-host host))

;;;###autoload
(defun isResolvable (host)
  (if (string-match "^[0-9.]+$" host)
      t
    (not (string= host (url-gateway-nslookup-host host)))))

;;;###autoload
(defun isInNet (ip net mask)
  (let ((netc (split-string ip "\\."))
	(ipc  (split-string net "\\."))
	(maskc (split-string mask "\\.")))
    (if (or (/= (length netc) (length ipc))
	    (/= (length ipc) (length maskc)))
	nil
      (setq netc (mapcar 'string-to-number netc)
	    ipc (mapcar 'string-to-number ipc)
	    maskc (mapcar 'string-to-number maskc))
      (and
       (= (logand (nth 0 netc) (nth 0 maskc))
	  (logand (nth 0 ipc)  (nth 0 maskc)))
       (= (logand (nth 1 netc) (nth 1 maskc))
	  (logand (nth 1 ipc)  (nth 1 maskc)))
       (= (logand (nth 2 netc) (nth 2 maskc))
	  (logand (nth 2 ipc)  (nth 2 maskc)))
       (= (logand (nth 3 netc) (nth 3 maskc))
	  (logand (nth 3 ipc)  (nth 3 maskc)))))))

;; Netscape configuration file parsing
(defvar url-ns-user-prefs nil
  "Internal, do not use.")

;;;###autoload
(defun url-ns-prefs (&optional file)
  (if (not file)
      (setq file (expand-file-name "~/.netscape/preferences.js")))
  (if (not (and (file-exists-p file)
		(file-readable-p file)))
      (message "Could not open %s for reading" file)
    (save-excursion
      (let ((false nil)
	    (true t))
	(setq url-ns-user-prefs (make-hash-table :size 13 :test 'equal))
	(set-buffer (get-buffer-create " *ns-parse*"))
	(erase-buffer)
	(insert-file-contents file)
	(goto-char (point-min))
	(while (re-search-forward "^//" nil t)
	  (replace-match ";;"))
	(goto-char (point-min))
	(while (re-search-forward "^user_pref(" nil t)
	  (replace-match "(url-ns-set-user-pref "))
	(goto-char (point-min))
	(while (re-search-forward "\"," nil t)
	  (replace-match "\""))
	(goto-char (point-min))
	(eval-buffer)))))

(defun url-ns-set-user-pref (key val)
  (puthash key val url-ns-user-prefs))

;;;###autoload
(defun url-ns-user-pref (key &optional default)
  (gethash key url-ns-user-prefs default))

(provide 'url-ns)

;;; url-ns.el ends here
