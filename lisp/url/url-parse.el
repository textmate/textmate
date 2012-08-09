;;; url-parse.el --- Uniform Resource Locator parser

;; Copyright (C) 1996-1999, 2004-2012 Free Software Foundation, Inc.

;; Keywords: comm, data, processes

;; This file is part of GNU Emacs.
;;
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

(require 'url-vars)
(require 'auth-source)
(eval-when-compile (require 'cl))

(autoload 'url-scheme-get-property "url-methods")

(defstruct (url
            (:constructor nil)
            (:constructor url-parse-make-urlobj
                          (&optional type user password host portspec filename
                                     target attributes fullness))
            (:copier nil))
  type user password host portspec filename target attributes fullness
  silent (use-cookies t))

(defsubst url-port (urlobj)
  (or (url-portspec urlobj)
      (if (url-fullness urlobj)
          (url-scheme-get-property (url-type urlobj) 'default-port))))

(defsetf url-port (urlobj) (port) `(setf (url-portspec ,urlobj) ,port))

;;;###autoload
(defun url-recreate-url (urlobj)
  "Recreate a URL string from the parsed URLOBJ."
  (concat (url-type urlobj) ":" (if (url-host urlobj) "//" "")
	  (if (url-user urlobj)
	      (concat (url-user urlobj)
		      (if (url-password urlobj)
			  (concat ":" (url-password urlobj)))
		      "@"))
	  (url-host urlobj)
	  (if (and (url-port urlobj)
		   (not (equal (url-port urlobj)
			       (url-scheme-get-property (url-type urlobj) 'default-port))))
	      (format ":%d" (url-port urlobj)))
	  (or (url-filename urlobj) "/")          
	  (url-recreate-url-attributes urlobj)
	  (if (url-target urlobj)
	      (concat "#" (url-target urlobj)))))

(defun url-recreate-url-attributes (urlobj)
  "Recreate the attributes of an URL string from the parsed URLOBJ."
  (when (url-attributes urlobj)
    (concat ";"
	    (mapconcat (lambda (x)
                         (if (cdr x)
                             (concat (car x) "=" (cdr x))
                           (car x)))
                       (url-attributes urlobj) ";"))))

;;;###autoload
(defun url-generic-parse-url (url)
  "Return an URL-struct of the parts of URL.
The CL-style struct contains the following fields:
TYPE USER PASSWORD HOST PORTSPEC FILENAME TARGET ATTRIBUTES FULLNESS."
  ;; See RFC 3986.
  (cond
   ((null url)
    (url-parse-make-urlobj))
   ((or (not (string-match url-nonrelative-link url))
	(= ?/ (string-to-char url)))
    ;; This isn't correct, as a relative URL can be a fragment link
    ;; (e.g. "#foo") and many other things (see section 4.2).
    ;; However, let's not fix something that isn't broken, especially
    ;; when close to a release.
    (url-parse-make-urlobj nil nil nil nil nil url))
   (t
    (with-temp-buffer
      ;; Don't let those temp-buffer modifications accidentally
      ;; deactivate the mark of the current-buffer.
      (let ((deactivate-mark nil))
        (set-syntax-table url-parse-syntax-table)
        (let ((save-pos nil)
              (prot nil)
              (user nil)
              (pass nil)
              (host nil)
              (port nil)
              (file nil)
              (refs nil)
              (attr nil)
              (full nil)
              (inhibit-read-only t))
          (erase-buffer)
          (insert url)
          (goto-char (point-min))
          (setq save-pos (point))

          ;; 3.1. Scheme
          (unless (looking-at "//")
            (skip-chars-forward "a-zA-Z+.\\-")
            (downcase-region save-pos (point))
            (setq prot (buffer-substring save-pos (point)))
            (skip-chars-forward ":")
            (setq save-pos (point)))

          ;; 3.2. Authority
          (when (looking-at "//")
            (setq full t)
            (forward-char 2)
            (setq save-pos (point))
            (skip-chars-forward "^/")
            (setq host (buffer-substring save-pos (point)))
            (if (string-match "^\\([^@]+\\)@" host)
                (setq user (match-string 1 host)
                      host (substring host (match-end 0) nil)))
            (if (and user (string-match "\\([^:]+\\):\\(.*\\)" user))
                (setq pass (match-string 2 user)
                      user (match-string 1 user)))
            ;; This gives wrong results for IPv6 literal addresses.
            (if (string-match ":\\([0-9+]+\\)" host)
                (setq port (string-to-number (match-string 1 host))
                      host (substring host 0 (match-beginning 0))))
            (if (string-match ":$" host)
                (setq host (substring host 0 (match-beginning 0))))
            (setq host (downcase host)
                  save-pos (point)))

          (if (not port)
              (setq port (url-scheme-get-property prot 'default-port)))

          ;; 3.3. Path
          ;; Gross hack to preserve ';' in data URLs
          (setq save-pos (point))

          ;; 3.4. Query
          (if (string= "data" prot)
              (goto-char (point-max))
            ;; Now check for references
            (skip-chars-forward "^#")
            (if (eobp)
                nil
              (delete-region
               (point)
               (progn
                 (skip-chars-forward "#")
                 (setq refs (buffer-substring (point) (point-max)))
                 (point-max))))
            (goto-char save-pos)
            (skip-chars-forward "^;")
            (unless (eobp)
              (setq attr (url-parse-args (buffer-substring (point) (point-max))
                                         t)
		    attr (nreverse attr))))

          (setq file (buffer-substring save-pos (point)))
          (if (and host (string-match "%[0-9][0-9]" host))
              (setq host (url-unhex-string host)))
          (url-parse-make-urlobj
           prot user pass host port file refs attr full)))))))

(defmacro url-bit-for-url (method lookfor url)
  `(let* ((urlobj (url-generic-parse-url url))
          (bit (funcall ,method urlobj))
          (methods (list 'url-recreate-url
                         'url-host))
          auth-info)
     (while (and (not bit) (> (length methods) 0))
       (setq auth-info (auth-source-search
                        :max 1
                        :host (funcall (pop methods) urlobj)
                        :port (url-type urlobj)))
       (setq bit (plist-get (nth 0 auth-info) ,lookfor))
       (when (functionp bit)
         (setq bit (funcall bit))))
     bit))

(defun url-user-for-url (url)
  "Attempt to use .authinfo to find a user for this URL."
  (url-bit-for-url 'url-user :user url))

(defun url-password-for-url (url)
  "Attempt to use .authinfo to find a password for this URL."
  (url-bit-for-url 'url-password :secret url))

(provide 'url-parse)

;;; url-parse.el ends here
