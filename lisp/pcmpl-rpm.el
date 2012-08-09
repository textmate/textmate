;;; pcmpl-rpm.el --- functions for dealing with rpm completions

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

;; These functions provide completion rules for the `rpm' command.

;;; Code:

(require 'pcomplete)

;; Functions:

;; FIXME rpm -qa can be slow, so:
;; Adding --nodigest --nosignature is MUCH faster.
;; (Probably need to test --help for those options though.)
;; Consider caching the result (cf woman).
;; Consider printing an explanatory message before running -qa.
;;
;; Seems pointless for this to be a defsubst.
(defsubst pcmpl-rpm-packages ()
  (split-string (pcomplete-process-result "rpm" "-q" "-a")))

(defun pcmpl-rpm-all-query (flag)
  (message "Querying all packages with `%s'..." flag)
  (let ((pkgs (pcmpl-rpm-packages))
	(provs (list t)))
    (while pkgs
      (nconc provs (split-string
		    (pcomplete-process-result
		     "rpm" "-q" (car pkgs) flag)))
      (setq pkgs (cdr pkgs)))
    (pcomplete-uniqify-list (cdr provs))))

(defsubst pcmpl-rpm-files ()
  (pcomplete-dirs-or-entries "\\.rpm\\'"))

;;;###autoload
(defun pcomplete/rpm ()
  "Completion for the `rpm' command."
  ;; Originally taken from the output of `rpm --help' on a Red Hat 6.1 system.
  (let (mode)
    (while (<= pcomplete-index pcomplete-last)
      (unless mode
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (pcomplete-here*
	     '("--addsign"
	       "--checksig"
	       "--erase"
	       "--help"
	       "--initdb"
	       "--install"
	       "--pipe"
	       "--querytags"
	       "--rebuild"
	       "--rebuilddb"
	       "--recompile"
	       "--resign"
	       "--rmsource"
	       "--setperms"
	       "--setugids"
	       "--upgrade"
	       "--verify"
	       "--version"))
	  (pcomplete-opt "vqVyiUebtK")))
;     -b<stage> <spec>
;     -t<stage> <tarball>    - build package, where <stage> is one of:
;	  p                - prep (unpack sources and apply patches)
;	  l                - list check (do some cursory checks on %files)
;	  c                - compile (prep and compile)
;	  i                - install (prep, compile, install)
;	  b                - binary package (prep, compile, install, package)
;	  a                - bin/src package (prep, compile, install, package)
      (cond
       ((or (eq mode 'query)
	    (pcomplete-match "-[^-]*q"))
	(setq mode 'query)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--changelog"
		 "--dbpath"
		 "--dump"
		 "--file"
		 "--ftpport"            ;nyi for the next four
		 "--ftpproxy"
		 "--httpport"
		 "--httpproxy"
		 "--provides"
		 "--queryformat"
		 "--rcfile"
		 "--requires"
		 "--root"
		 "--scripts"
		 "--triggeredby"
		 "--whatprovides"
		 "--whatrequires"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--queryformat")
		(pcomplete-here*))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--file")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--scripts")
		(if (pcomplete-match "^--\\(.*\\)" 0)
		    (pcomplete-here* '("--triggers"))))
	       ((pcomplete-test "--triggeredby")
		(pcomplete-here* (pcmpl-rpm-packages)))
	       ((pcomplete-test "--whatprovides")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--provides")))
	       ((pcomplete-test "--whatrequires")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--requires")))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "af.p(pcmpl-rpm-files)ilsdcvR")
	    (if (pcomplete-test "-[^-]*p" 'first 1)
		(pcomplete-here (pcmpl-rpm-files))
              (if (pcomplete-test "-[^-]*f" 'first 1)
                  (pcomplete-here* (pcomplete-entries))
                (pcomplete-here (pcmpl-rpm-packages)))))))
       ((pcomplete-test "--pipe")
	(pcomplete-here* (funcall pcomplete-command-completion-function)))
       ((pcomplete-test "--rmsource")
	(pcomplete-here* (pcomplete-entries))
	(throw 'pcomplete-completions nil))
       ((pcomplete-match "\\`--re\\(build\\|compile\\)\\'")
	(pcomplete-here (pcmpl-rpm-files))
	(throw 'pcomplete-completions nil))
       ((pcomplete-match "\\`--\\(resign\\|addsign\\)\\'")
	(while (pcomplete-here (pcmpl-rpm-files))))
       ((or (eq mode 'checksig)
	    (pcomplete-test "--checksig"))
	(setq mode 'checksig)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--nopgp"
		 "--nogpg"
		 "--nomd5"
		 "--rcfile"))
	      (cond
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here (pcmpl-rpm-files)))))
       ((or (eq mode 'rebuilddb)
	    (pcomplete-test "--rebuilddb"))
	(setq mode 'rebuilddb)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--dbpath"
		 "--root"
		 "--rcfile"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here))))
       ((memq mode '(install upgrade))
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       (append
		'("--allfiles"
		  "--badreloc"
		  "--dbpath"
		  "--excludedocs"
		  "--excludepath"
		  "--force"
		  "--hash"
		  "--ignorearch"
		  "--ignoreos"
		  "--ignoresize"
		  "--includedocs"
		  "--justdb"
		  "--nodeps"
		  "--noorder"
		  "--noscripts"
		  "--notriggers")
		(if (eq mode 'upgrade)
		    '("--oldpackage"))
		'("--percent"
		  "--prefix"
		  "--rcfile"
		  "--relocate"
		  "--replacefiles"
		  "--replacepkgs"
		  "--root")))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--relocate")
		(pcomplete-here*))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--excludepath")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--prefix")
		(pcomplete-here* (pcomplete-dirs)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "vh")
	    (pcomplete-here (pcmpl-rpm-files)))))
       ((or (pcomplete-test "--install")
	    (pcomplete-match "-[^-]*i"))
	(setq mode 'install))
       ((or (pcomplete-test "--upgrade")
	    (pcomplete-match "-[^-]*U"))
	(setq mode 'upgrade))
       ((or (eq mode 'erase)
	    (pcomplete-test "--erase")
	    (pcomplete-match "-[^-]*e"))
	(setq mode 'erase)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--allmatches"
		 "--dbpath"
		 "--justdb"
		 "--nodeps"
		 "--noorder"
		 "--noscripts"
		 "--notriggers"
		 "--rcfile"
		 "--root"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here (pcmpl-rpm-packages)))))
       ((or (eq mode 'verify)
	    (pcomplete-test "--verify"))
	(setq mode 'verify)
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--dbpath"
		 "--nodeps"
		 "--nofiles"
		 "--nomd5"
		 "--rcfile"
		 "--root"
		 "--triggeredby"
		 "--whatprovides"
		 "--whatrequires"))
	      (cond
	       ((pcomplete-test "--dbpath")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--root")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--triggeredby")
		(pcomplete-here* (pcmpl-rpm-packages)))
	       ((pcomplete-test "--whatprovides")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--provides")))
	       ((pcomplete-test "--whatrequires")
		(pcomplete-here*
		 (pcmpl-rpm-all-query "--requires")))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "af.p(pcmpl-rpm-files)v")
	    (pcomplete-here (pcmpl-rpm-packages)))))
       ((or (memq mode '(build test))
	    (pcomplete-match "\\`-[bt]"))
	(setq mode (if (pcomplete-match "\\`-b")
		       'build
		     'test))
	(if (pcomplete-match "^--\\(.*\\)" 0)
	    (progn
	      (pcomplete-here*
	       '("--buildroot"
		 "--clean"
		 "--nobuild"
		 "--rcfile"
		 "--rmsource"
		 "--short-circuit"
		 "--sign"
		 "--target"
		 "--timecheck"))
	      (cond
	       ((pcomplete-test "--buildroot")
		(pcomplete-here* (pcomplete-dirs)))
	       ((pcomplete-test "--rcfile")
		(pcomplete-here* (pcomplete-entries)))
	       ((pcomplete-test "--timecheck")
		(pcomplete-here*))))
	  (if (pcomplete-match "^-" 0)
	      (pcomplete-opt "v")
	    (pcomplete-here
	     (pcomplete-dirs-or-entries (if (eq mode 'test)
                                            "\\.tar\\'"
                                          "\\.spec\\'"))))))
       (t
	(error "You must select a mode: -q, -i, -U, --verify, etc"))))))

(provide 'pcmpl-rpm)

;;; pcmpl-rpm.el ends here
