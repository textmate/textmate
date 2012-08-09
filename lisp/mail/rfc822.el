;;; rfc822.el --- hairy rfc822 parser for mail and news and suchlike

;; Copyright (C) 1986-1987, 1990, 2001-2012 Free Software Foundation, Inc.

;; Author: Richard Mlynarik <mly@eddie.mit.edu>
;; Maintainer: FSF
;; Keywords: mail

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

;; Support functions for parsing RFC-822 headers, used by mail and news
;; modes.

;;; Code:

(defvar rfc822-address-start)

;; uses rfc822-address-start free, throws to address
(defun rfc822-bad-address (reason)
  (save-restriction
    (insert "_^_")
    (narrow-to-region rfc822-address-start
		      (if (re-search-forward "[,;]" nil t)
			  (max (point-min) (1- (point)))
			(point-max)))
    ;; make the error string be suitable for inclusion in (...)
    (let ((losers '("\\" "(" ")" "\n")))
      (while losers
	(goto-char (point-min))
	(while (search-forward (car losers) nil t)
	  (backward-char 1)
	  (insert ?\\)
	  (forward-char 1))
	(setq losers (cdr losers))))
    (goto-char (point-min)) (insert "(Unparsable address -- "
				    reason
				    ": \"")
    (goto-char (point-max)) (insert "\")"))
  (rfc822-nuke-whitespace)
  (throw 'address (buffer-substring rfc822-address-start (point))))

(defun rfc822-nuke-whitespace (&optional leave-space)
  (let (ch)
    (while (cond ((eobp)
		  nil)
		 ((= (setq ch (following-char)) ?\()
		  (forward-char 1)
		  (while (if (eobp)
			     (rfc822-bad-address "Unbalanced comment (...)")
			   (/= (setq ch (following-char)) ?\)))
		    (cond ((looking-at "[^()\\]+")
			   (replace-match ""))
			  ((= ch ?\()
			   (rfc822-nuke-whitespace))
			  ((< (point) (1- (point-max)))
			   (delete-char 2))
			  (t
			   (rfc822-bad-address "orphaned backslash"))))
		  ;; delete remaining "()"
		  (forward-char -1)
		  (delete-char 2)
		  t)
		 ((memq ch '(?\  ?\t ?\n))
		  (delete-region (point)
				 (progn (skip-chars-forward " \t\n") (point)))
		  t)
		 (t
		  nil)))
    (or (not leave-space)
	(eobp)
	(bobp)
	(= (preceding-char) ?\ )
	(insert ?\ ))))

(defun rfc822-looking-at (regex &optional leave-space)
  (if (cond ((stringp regex)
	     (if (looking-at regex)
		 (progn (goto-char (match-end 0))
			t)))
	    (t
	     (if (and (not (eobp))
		      (= (following-char) regex))
		 (progn (forward-char 1)
			t))))
      (let ((tem (match-data)))
	(rfc822-nuke-whitespace leave-space)
	(set-match-data tem)
	t)))

(defun rfc822-snarf-word ()
  ;; word is atom | quoted-string
  (cond ((= (following-char) ?\")
	 ;; quoted-string
	 (or (rfc822-looking-at "\"\\([^\"\\\n]\\|\\\\.\\|\\\\\n\\)*\"")
	     (rfc822-bad-address "Unterminated quoted string")))
	((rfc822-looking-at "[^][\000-\037 ()<>@,;:\\\".]+")
	 ;; atom
	 )
	(t
	 (rfc822-bad-address "Rubbish in address"))))

(defun rfc822-snarf-words ()
  (rfc822-snarf-word)
  (while (rfc822-looking-at ?.)
    (rfc822-snarf-word)))

(defun rfc822-snarf-subdomain ()
  ;; sub-domain is domain-ref | domain-literal
  (cond ((= (following-char) ?\[)
	 ;; domain-ref
	 (or (rfc822-looking-at "\\[\\([^][\\\n]\\|\\\\.\\|\\\\\n\\)*\\]")
	     (rfc822-bad-address "Unterminated domain literal [...]")))
	((rfc822-looking-at "[^][\000-\037 ()<>@,;:\\\".]+")
	 ;; domain-literal = atom
	 )
	(t
	 (rfc822-bad-address "Rubbish in host/domain specification"))))

(defun rfc822-snarf-domain ()
  (rfc822-snarf-subdomain)
  (while (rfc822-looking-at ?.)
    (rfc822-snarf-subdomain)))

(defun rfc822-snarf-frob-list (name separator terminator snarfer
				    &optional return)
  (let ((first t)
	(list ())
	tem)
    (while (cond ((eobp)
		  (rfc822-bad-address
		    (format "End of addresses in middle of %s" name)))
		 ((rfc822-looking-at terminator)
		  nil)
		 ((rfc822-looking-at separator)
		  ;; multiple separators are allowed and do nothing.
		  (while (rfc822-looking-at separator))
		  t)
		 (first
		  t)
		 (t
		  (rfc822-bad-address
		    (format "Gubbish in middle of %s" name))))
      (setq tem (funcall snarfer)
	    first nil)
      (and return tem
	   (setq list (if (listp tem)
			  (nconc (reverse tem) list)
			  (cons tem list)))))
    (nreverse list)))

;; return either an address (a string) or a list of addresses
(defun rfc822-addresses-1 (&optional allow-groups)
  ;; Looking for an rfc822 `address'
  ;; Either a group (1*word ":" [#mailbox] ";")
  ;; or a mailbox (addr-spec | 1*word route-addr)
  ;;  addr-spec is (local-part "@" domain)
  ;;  route-addr is ("<" [1#("@" domain) ":"] addr-spec ">")
  ;;  local-part is (word *("." word))
  ;;  word is (atom | quoted-string)
  ;;  quoted-string is ("\([^\"\\n]\|\\.\|\\\n\)")
  ;;  atom is [^\000-\037\177 ()<>@,;:\".[]]+
  ;;  domain is sub-domain *("." sub-domain)
  ;;  sub-domain is domain-ref | domain-literal
  ;;  domain-literal is  "[" *(dtext | quoted-pair) "]"
  ;;  dtext is "[^][\\n"
  ;;  domain-ref is atom
  (let ((rfc822-address-start (point))
	(n 0))
    (catch 'address
      ;; optimize common cases:
      ;;  foo
      ;;  foo.bar@bar.zap
      ;; followed by "\\'\\|,\\|([^()\\]*)\\'"
      ;; other common cases are:
      ;;  foo bar <foo.bar@baz.zap>
      ;;  "foo bar" <foo.bar@baz.zap>
      ;;  those aren't hacked yet.
      (if (and (rfc822-looking-at "[^][\000-\037 ()<>@,;:\\\"]+\\(\\|@[^][\000-\037 ()<>@,;:\\\"]+\\)" t)
	       (progn (or (eobp)
			  (rfc822-looking-at ?,))))
	  (progn
	    ;; rfc822-looking-at may have inserted a space
	    (or (bobp) (/= (preceding-char) ?\ ) (delete-char -1))
	    ;; relying on the fact that rfc822-looking-at <char>
	    ;;  doesn't mung match-data
	    (throw 'address (buffer-substring rfc822-address-start (match-end 0)))))
      (goto-char rfc822-address-start)
      (while t
	(cond ((and (= n 1) (rfc822-looking-at ?@))
	       ;; local-part@domain
	       (rfc822-snarf-domain)
	       (throw 'address
		 (buffer-substring rfc822-address-start (point))))
	      ((rfc822-looking-at ?:)
	       (cond ((not allow-groups)
		      (rfc822-bad-address "A group name may not appear here"))
		     ((= n 0)
		      (rfc822-bad-address "No name for :...; group")))
	       ;; group
	       (throw 'address
		 ;; return a list of addresses
		 (rfc822-snarf-frob-list ":...; group" ?\, ?\;
					 'rfc822-addresses-1 t)))
	      ((rfc822-looking-at ?<)
	       (let ((start (point))
		     (strip t))
		 (cond ((rfc822-looking-at ?>)
			;; empty path
			())
		       ((and (not (eobp)) (= (following-char) ?\@))
			;; <@foo.bar,@baz:quux@abcd.efg>
			(rfc822-snarf-frob-list "<...> address" ?\, ?\:
			  (function (lambda ()
				      (if (rfc822-looking-at ?\@)
					  (rfc822-snarf-domain)
					(rfc822-bad-address
					  "Gubbish in route-addr")))))
			(rfc822-snarf-words)
			(or (rfc822-looking-at ?@)
			    (rfc822-bad-address "Malformed <..@..> address"))
			(rfc822-snarf-domain)
			(setq strip nil))
		       ((progn (rfc822-snarf-words) (rfc822-looking-at ?@))
			; allow <foo> (losing unix seems to do this)
			(rfc822-snarf-domain)))
		 (let ((end (point)))
		   (if (rfc822-looking-at ?\>)
		       (throw 'address
			 (buffer-substring (if strip start (1- start))
					   (if strip end (1+ end))))
		     (rfc822-bad-address "Unterminated <...> address")))))
	      ((looking-at "[^][\000-\037 ()<>@,;:\\.]")
	       ;; this allows "." to be part of the words preceding
	       ;; an addr-spec, since many broken mailers output
	       ;; "Hern K. Herklemeyer III
	       ;;   <yank@megadeath.dod.gods-own-country>"
               (let ((again t))
                 (while again
                   (or (= n 0) (bobp) (= (preceding-char) ?\ )
                       (insert ?\ ))
                   (rfc822-snarf-words)
                   (setq n (1+ n))
                   (setq again (or (rfc822-looking-at ?.)
                                   (looking-at "[^][\000-\037 ()<>@,;:\\.]"))))))
	      ((= n 0)
	       (throw 'address nil))
	      ((= n 1) ; allow "foo" (losing unix seems to do this)
	       (throw 'address
		 (buffer-substring rfc822-address-start (point))))
              ((> n 1)
               (rfc822-bad-address "Missing comma between addresses or badly-formatted address"))
	      ((or (eobp) (= (following-char) ?,))
	       (rfc822-bad-address "Missing comma or route-spec"))
	      (t
	       (rfc822-bad-address "Strange character or missing comma")))))))


(defun rfc822-addresses (header-text)
  (if (string-match "\\`[ \t]*\\([^][\000-\037 ()<>@,;:\\\".]+\\)[ \t]*\\'"
                    header-text)
      ;; Make very simple case moderately fast.
      (list (substring header-text (match-beginning 1) (match-end 1)))
    (let ((buf (generate-new-buffer " rfc822")))
      (unwind-protect
          (with-current-buffer buf
            (make-local-variable 'case-fold-search)
            (setq case-fold-search nil)	;For speed(?)
            (insert header-text)
            ;; unfold continuation lines
            (goto-char (point-min))

            (while (re-search-forward "\\([^\\]\\(\\\\\\\\\\)*\\)\n[ \t]"
                                      nil t)
              (replace-match "\\1 " t))

            (goto-char (point-min))
	    ;; Give `rfc822-address-start' a non-nil initial value to
	    ;; prevent `rfc822-bad-address' from raising a
	    ;; `wrong-type-argument' error.
            (let* ((rfc822-address-start (point))
		   list tem
		   (err
		    (catch 'address
		      ;; Note that `rfc822-nuke-whitespace' and
		      ;; `rfc822-looking-at' can throw.
		      (rfc822-nuke-whitespace)
		      (while (not (eobp))
			(setq rfc822-address-start (point))
			(setq tem
			      (cond ((rfc822-looking-at ?\,)
				     nil)
				    ((looking-at "[][\000-\037@;:\\.>)]")
				     (forward-char)
				     (catch 'address ; For rfc822-bad-address
				       (rfc822-bad-address
					(format "Strange character \\%c found"
						(preceding-char)))))
				    (t
				     (rfc822-addresses-1 t))))
			(cond ((null tem))
			      ((stringp tem)
			       (setq list (cons tem list)))
			      (t
			       (setq list (nconc (nreverse tem) list)))))
		      nil)))
	      (nreverse (append (if err (list err)) list))))
	(and buf (kill-buffer buf))))))

(provide 'rfc822)

;;; rfc822.el ends here
