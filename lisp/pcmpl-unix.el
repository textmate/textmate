;;; pcmpl-unix.el --- standard UNIX completions

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Package: pcomplete

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

(require 'pcomplete)

;; User Variables:

(defcustom pcmpl-unix-group-file "/etc/group"
  "If non-nil, a string naming the group file on your system."
  :type '(choice file (const nil))
  :group 'pcmpl-unix)

(defcustom pcmpl-unix-passwd-file "/etc/passwd"
  "If non-nil, a string naming the passwd file on your system."
  :type '(choice file (const nil))
  :group 'pcmpl-unix)

(defcustom pcmpl-ssh-known-hosts-file "~/.ssh/known_hosts"
  "If non-nil, a string naming your SSH \"known_hosts\" file.
This allows one method of completion of SSH host names, the other
being via `pcmpl-ssh-config-file'.  Note that newer versions of
ssh hash the hosts by default, to prevent Island-hopping SSH
attacks.  This can be disabled, at some risk, with the SSH option
\"HashKnownHosts no\"."
  :type '(choice file (const nil))
  :group 'pcmpl-unix
  :version "23.1")

(defcustom pcmpl-ssh-config-file "~/.ssh/config"
  "If non-nil, a string naming your SSH \"config\" file.
This allows one method of completion of SSH host names, the other
being via `pcmpl-ssh-known-hosts-file'."
  :type '(choice file (const nil))
  :group 'pcmpl-unix
  :version "24.1")

;; Functions:

;;;###autoload
(defun pcomplete/cd ()
  "Completion for `cd'."
  (while (pcomplete-here (pcomplete-dirs))))

;;;###autoload
(defalias 'pcomplete/pushd 'pcomplete/cd)

;;;###autoload
(defun pcomplete/rmdir ()
  "Completion for `rmdir'."
  (while (pcomplete-here (pcomplete-dirs))))

;;;###autoload
(defun pcomplete/rm ()
  "Completion for `rm'."
  (let ((pcomplete-help "(fileutils)rm invocation"))
    (pcomplete-opt "dfirRv")
    (while (pcomplete-here (pcomplete-all-entries) nil
			   'expand-file-name))))

;;;###autoload
(defun pcomplete/xargs ()
  "Completion for `xargs'."
  (pcomplete-here (funcall pcomplete-command-completion-function))
  (funcall (or (pcomplete-find-completion-function (pcomplete-arg 1))
	       pcomplete-default-completion-function)))

;;;###autoload
(defalias 'pcomplete/time 'pcomplete/xargs)

;;;###autoload
(defun pcomplete/which ()
  "Completion for `which'."
  (while (pcomplete-here (funcall pcomplete-command-completion-function))))

(defun pcmpl-unix-read-passwd-file (file)
  "Return an alist correlating gids to group names in FILE.

If FILE is in hashed format (as described in the OpenSSH
documentation), this function returns nil."
  (let (names)
    (when (file-readable-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-min))
	(while (not (eobp))
	  (let* ((fields
		  (split-string (buffer-substring
				 (point) (progn (end-of-line)
						(point))) ":")))
	    (setq names (cons (nth 0 fields) names)))
	  (forward-line))))
    (pcomplete-uniqify-list names)))

(defsubst pcmpl-unix-group-names ()
  "Read the contents of /etc/group for group names."
  (if pcmpl-unix-group-file
      (pcmpl-unix-read-passwd-file pcmpl-unix-group-file)))

(defsubst pcmpl-unix-user-names ()
  "Read the contents of /etc/passwd for user names."
  (if pcmpl-unix-passwd-file
      (pcmpl-unix-read-passwd-file pcmpl-unix-passwd-file)))

;;;###autoload
(defun pcomplete/chown ()
  "Completion for the `chown' command."
  (unless (pcomplete-match "\\`-")
    (if (pcomplete-match "\\`[^.]*\\'" 0)
	(pcomplete-here* (pcmpl-unix-user-names))
      (if (pcomplete-match "\\.\\([^.]*\\)\\'" 0)
	  (pcomplete-here* (pcmpl-unix-group-names)
			   (pcomplete-match-string 1 0))
	(pcomplete-here*))))
  (while (pcomplete-here (pcomplete-entries))))

;;;###autoload
(defun pcomplete/chgrp ()
  "Completion for the `chgrp' command."
  (unless (pcomplete-match "\\`-")
    (pcomplete-here* (pcmpl-unix-group-names)))
  (while (pcomplete-here (pcomplete-entries))))


;; ssh support by Phil Hagelberg.
;; http://www.emacswiki.org/cgi-bin/wiki/pcmpl-ssh.el

(defun pcmpl-ssh-known-hosts ()
  "Return a list of hosts found in `pcmpl-ssh-known-hosts-file'."
  (when (and pcmpl-ssh-known-hosts-file
             (file-readable-p pcmpl-ssh-known-hosts-file))
    (with-temp-buffer
      (insert-file-contents-literally pcmpl-ssh-known-hosts-file)
      (let ((host-re "\\(?:\\([-.[:alnum:]]+\\)\\|\\[\\([-.[:alnum:]]+\\)\\]:[0-9]+\\)[, ]")
            ssh-hosts-list)
        (while (re-search-forward (concat "^ *" host-re) nil t)
          (add-to-list 'ssh-hosts-list (concat (match-string 1)
                                               (match-string 2)))
          (while (and (looking-back ",")
                      (re-search-forward host-re (line-end-position) t))
            (add-to-list 'ssh-hosts-list (concat (match-string 1)
                                                 (match-string 2)))))
        ssh-hosts-list))))

(defun pcmpl-ssh-config-hosts ()
  "Return a list of hosts found in `pcmpl-ssh-config-file'."
  (when (and pcmpl-ssh-config-file
             (file-readable-p pcmpl-ssh-config-file))
    (with-temp-buffer
      (insert-file-contents-literally pcmpl-ssh-config-file)
      (let (ssh-hosts-list
            (case-fold-search t))
        (while (re-search-forward "^ *host\\(name\\)? +\\([-.[:alnum:]]+\\)"
                                  nil t)
          (add-to-list 'ssh-hosts-list (match-string 2)))
        ssh-hosts-list))))

(defun pcmpl-ssh-hosts ()
  "Return a list of known SSH hosts.
Uses both `pcmpl-ssh-config-file' and `pcmpl-ssh-known-hosts-file'."
  (let ((hosts (pcmpl-ssh-known-hosts)))
    (dolist (h (pcmpl-ssh-config-hosts))
      (add-to-list 'hosts h))
    hosts))

;;;###autoload
(defun pcomplete/ssh ()
  "Completion rules for the `ssh' command."
  (pcomplete-opt "1246AaCfgKkMNnqsTtVvXxYbcDeFiLlmOopRSw")
  (pcomplete-here (pcmpl-ssh-hosts)))

;;;###autoload
(defun pcomplete/scp ()
  "Completion rules for the `scp' command.
Includes files as well as host names followed by a colon."
  (pcomplete-opt "1246BCpqrvcFiloPS")
  (while t (pcomplete-here
            (lambda (string pred action)
              (let ((table
                     (cond
                      ((string-match "\\`[^:/]+:" string) ; Remote file name.
		       (if (and (eq action 'lambda)
				(eq (match-end 0) (length string)))
			   ;; Avoid connecting to the remote host when we're
			   ;; only completing the host name.
			   (list string)
			 (comint--table-subvert (pcomplete-all-entries)
						"" "/ssh:")))
                      ((string-match "/" string) ; Local file name.
                       (pcomplete-all-entries))
                      (t                ;Host name or local file name.
                       (append (all-completions string (pcomplete-all-entries))
                               (mapcar (lambda (host) (concat host ":"))
                                       (pcmpl-ssh-hosts)))))))
                (complete-with-action action table string pred))))))

(provide 'pcmpl-unix)

;;; pcmpl-unix.el ends here
