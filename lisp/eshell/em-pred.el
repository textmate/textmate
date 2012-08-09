;;; em-pred.el --- argument predicates and modifiers (ala zsh)

;; Copyright (C) 1999-2012  Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;; Argument predication is used to affect which members of a list are
;; selected for use as argument.  This is most useful with globbing,
;; but can be used on any list argument, to select certain members.
;;
;; Argument modifiers are used to manipulate argument values.  For
;; example, sorting lists, upcasing words, substituting characters,
;; etc.
;;
;; Here are some examples of how to use argument predication.  Most of
;; the predicates and modifiers are modeled after those provided by
;; zsh.
;;
;;   ls -ld *(/)           ; list all directories
;;   ls -l *(@u'johnw')    ; list all symlinks owned by 'johnw'
;;   bzip2 -9v **/*(a+30)  ; compress everything which hasn't been
;;                           accessed in 30 days
;;   echo *.c(:o:R)     ; a reversed, sorted list of C files
;;   *(^@:U^u0)         ; all non-symlinks not owned by 'root', upcased
;;   chmod u-x *(U*)    : remove exec bit on all executables owned by user
;;
;; See the zsh docs for more on the syntax ([(zsh.info)Filename
;; Generation]).

;;; Code:

(eval-when-compile (require 'eshell))

;;;###autoload
(eshell-defgroup eshell-pred nil
  "This module allows for predicates to be applied to globbing
patterns (similar to zsh), in addition to string modifiers which can
be applied either to globbing results, variable references, or just
ordinary strings."
  :tag "Value modifiers and predicates"
  :group 'eshell-module)

;;; User Variables:

(defcustom eshell-pred-load-hook nil
  "A list of functions to run when `eshell-pred' is loaded."
  :version "24.1"			; removed eshell-pred-initialize
  :type 'hook
  :group 'eshell-pred)

(defcustom eshell-predicate-alist
  '((?/ . (eshell-pred-file-type ?d))   ; directories
    (?. . (eshell-pred-file-type ?-))   ; regular files
    (?s . (eshell-pred-file-type ?s))   ; sockets
    (?p . (eshell-pred-file-type ?p))   ; named pipes
    (?@ . (eshell-pred-file-type ?l))   ; symbolic links
    (?% . (eshell-pred-file-type ?%))   ; allow user to specify (c def.)
    (?r . (eshell-pred-file-mode 0400)) ; owner-readable
    (?w . (eshell-pred-file-mode 0200)) ; owner-writable
    (?x . (eshell-pred-file-mode 0100)) ; owner-executable
    (?A . (eshell-pred-file-mode 0040)) ; group-readable
    (?I . (eshell-pred-file-mode 0020)) ; group-writable
    (?E . (eshell-pred-file-mode 0010)) ; group-executable
    (?R . (eshell-pred-file-mode 0004)) ; world-readable
    (?W . (eshell-pred-file-mode 0002)) ; world-writable
    (?X . (eshell-pred-file-mode 0001)) ; world-executable
    (?s . (eshell-pred-file-mode 4000)) ; setuid
    (?S . (eshell-pred-file-mode 2000)) ; setgid
    (?t . (eshell-pred-file-mode 1000)) ; sticky bit
    (?U . #'(lambda (file)                   ; owned by effective uid
              (if (file-exists-p file)
                  (= (nth 2 (file-attributes file)) (user-uid)))))
    ;; (?G . #'(lambda (file)               ; owned by effective gid
    ;;          (if (file-exists-p file)
    ;;              (= (nth 2 (file-attributes file)) (user-uid)))))
    (?* . #'(lambda (file)
              (and (file-regular-p file)
                   (not (file-symlink-p file))
                   (file-executable-p file))))
    (?l . (eshell-pred-file-links))
    (?u . (eshell-pred-user-or-group ?u "user" 2 'eshell-user-id))
    (?g . (eshell-pred-user-or-group ?g "group" 3 'eshell-group-id))
    (?a . (eshell-pred-file-time ?a "access" 4))
    (?m . (eshell-pred-file-time ?m "modification" 5))
    (?c . (eshell-pred-file-time ?c "change" 6))
    (?L . (eshell-pred-file-size)))
  "A list of predicates than can be applied to a globbing pattern.
The format of each entry is

  (CHAR . PREDICATE-FUNC-SEXP)"
  :type '(repeat (cons character sexp))
  :group 'eshell-pred)

(put 'eshell-predicate-alist 'risky-local-variable t)

(defcustom eshell-modifier-alist
  '((?E . #'(lambda (lst)
              (mapcar
               (function
                (lambda (str)
                  (eshell-stringify
                   (car (eshell-parse-argument str))))) lst)))
    (?L . #'(lambda (lst) (mapcar 'downcase lst)))
    (?U . #'(lambda (lst) (mapcar 'upcase lst)))
    (?C . #'(lambda (lst) (mapcar 'capitalize lst)))
    (?h . #'(lambda (lst) (mapcar 'file-name-directory lst)))
    (?i . (eshell-include-members))
    (?x . (eshell-include-members t))
    (?r . #'(lambda (lst) (mapcar 'file-name-sans-extension lst)))
    (?e . #'(lambda (lst) (mapcar 'file-name-extension lst)))
    (?t . #'(lambda (lst) (mapcar 'file-name-nondirectory lst)))
    (?q . #'(lambda (lst) (mapcar 'eshell-escape-arg lst)))
    (?u . #'(lambda (lst) (eshell-uniqify-list lst)))
    (?o . #'(lambda (lst) (sort lst 'string-lessp)))
    (?O . #'(lambda (lst) (nreverse (sort lst 'string-lessp))))
    (?j . (eshell-join-members))
    (?S . (eshell-split-members))
    (?R . 'reverse)
    (?g . (progn
	    (forward-char)
	    (if (eq (char-before) ?s)
		(eshell-pred-substitute t)
	      (error "`g' modifier cannot be used alone"))))
    (?s . (eshell-pred-substitute)))
  "A list of modifiers than can be applied to an argument expansion.
The format of each entry is

  (CHAR ENTRYWISE-P MODIFIER-FUNC-SEXP)"
  :type '(repeat (cons character sexp))
  :group 'eshell-pred)

(put 'eshell-modifier-alist 'risky-local-variable t)

(defvar eshell-predicate-help-string
  "Eshell predicate quick reference:

  -  follow symbolic references for predicates after the `-'
  ^  invert sense of predicates after the `^'

FILE TYPE:
  /  directories              s  sockets
  .  regular files            p  named pipes
  *  executable (files only)  @  symbolic links

  %x  file type == `x' (as by ls -l; so `c' = char device, etc.)

PERMISSION BITS (for owner/group/world):
  r/A/R  readable    s  setuid
  w/I/W  writable    S  setgid
  x/E/X  executable  t  sticky bit

OWNERSHIP:
  U               owned by effective uid
  u(UID|'user')   owned by UID/user
  g(GID|'group')  owned by GID/group

FILE ATTRIBUTES:
  l[+-]N                 +/-/= N links
  a[Mwhms][+-](N|'FILE') access time +/-/= N months/weeks/hours/mins/secs
			 (days if unspecified) if FILE specified,
			 use as comparison basis; so a+'file.c'
			 shows files accessed before file.c was
			 last accessed
  m[Mwhms][+-](N|'FILE') modification time...
  c[Mwhms][+-](N|'FILE') change time...
  L[kmp][+-]N            file size +/-/= N Kb/Mb/blocks

EXAMPLES:
  *(^@)         all non-dot files which are not symlinks
  .#*(^@)       all files which are not symbolic links
  **/.#*(*)     all executable files, searched recursively
  ***/*~f*(-/)  recursively (though not traversing symlinks),
		find all directories (or symlinks referring to
		directories) whose names do not begin with f.
  e*(*Lk+50)    executables 50k or larger beginning with 'e'")

(defvar eshell-modifier-help-string
  "Eshell modifier quick reference:

FOR SINGLE ARGUMENTS, or each argument of a list of strings:
  E  evaluate again
  L  lowercase
  U  uppercase
  C  capitalize
  h  dirname
  t  basename
  e  file extension
  r  strip file extension
  q  escape special characters

  S       split string at any whitespace character
  S/PAT/  split string at each occurrence of PAT

FOR LISTS OF ARGUMENTS:
  o  sort alphabetically
  O  reverse sort alphabetically
  u  uniq list (typically used after :o or :O)
  R  reverse list

  j       join list members, separated by a space
  j/PAT/  join list members, separated by PAT
  i/PAT/  exclude all members not matching PAT
  x/PAT/  exclude all members matching PAT

  s/pat/match/  substitute PAT with MATCH
  g/pat/match/  substitute PAT with MATCH for all occurrences

EXAMPLES:
  *.c(:o)  sorted list of .c files")

;;; Functions:

(defun eshell-display-predicate-help ()
  (interactive)
  (with-electric-help
   (function
    (lambda ()
      (insert eshell-predicate-help-string)))))

(defun eshell-display-modifier-help ()
  (interactive)
  (with-electric-help
   (function
    (lambda ()
      (insert eshell-modifier-help-string)))))

(defun eshell-pred-initialize ()
  "Initialize the predicate/modifier code."
  (add-hook 'eshell-parse-argument-hook
	    'eshell-parse-arg-modifier t t)
  (define-key eshell-command-map [(meta ?q)] 'eshell-display-predicate-help)
  (define-key eshell-command-map [(meta ?m)] 'eshell-display-modifier-help))

(defun eshell-apply-modifiers (lst predicates modifiers)
  "Apply to LIST a series of PREDICATES and MODIFIERS."
  (let (stringified)
    (if (stringp lst)
	(setq lst (list lst)
	      stringified t))
    (when (listp lst)
      (setq lst (eshell-winnow-list lst nil predicates))
      (while modifiers
	(setq lst (funcall (car modifiers) lst)
	      modifiers (cdr modifiers)))
      (if (and stringified
	       (= (length lst) 1))
	  (car lst)
	lst))))

(defun eshell-parse-arg-modifier ()
  "Parse a modifier that has been specified after an argument.
This function is specially for adding onto `eshell-parse-argument-hook'."
  (when (eq (char-after) ?\()
    (forward-char)
    (let ((end (eshell-find-delimiter ?\( ?\))))
      (if (not end)
	  (throw 'eshell-incomplete ?\()
	(when (eshell-arg-delimiter (1+ end))
	  (save-restriction
	    (narrow-to-region (point) end)
	    (let* ((modifiers (eshell-parse-modifiers))
		   (preds (car modifiers))
		   (mods (cdr modifiers)))
	      (if (or preds mods)
		  ;; has to go at the end, which is only natural since
		  ;; syntactically it can only occur at the end
		  (setq eshell-current-modifiers
			(append
			 eshell-current-modifiers
			 (list
			  `(lambda (lst)
			     (eshell-apply-modifiers
			      lst (quote ,preds) (quote ,mods)))))))))
	  (goto-char (1+ end))
	  (eshell-finish-arg))))))

(defun eshell-parse-modifiers ()
  "Parse value modifiers and predicates at point.
If ALLOW-PREDS is non-nil, predicates will be parsed as well.
Return a cons cell of the form

  (PRED-FUNC-LIST . MOD-FUNC-LIST)

NEW-STRING is STRING minus any modifiers.  PRED-FUNC-LIST is a list of
predicate functions.  MOD-FUNC-LIST is a list of result modifier
functions.  PRED-FUNCS take a filename and return t if the test
succeeds; MOD-FUNCS take any string and preform a modification,
returning the resultant string."
  (let (result negate follow preds mods)
    (condition-case err
	(while (not (eobp))
	  (let ((char (char-after)))
	    (cond
	     ((eq char ?')
	      (forward-char)
	      (if (looking-at "[^|':]")
		  (let ((func (read (current-buffer))))
		    (if (and func (functionp func))
			(setq preds (eshell-add-pred-func func preds
							  negate follow))
		      (error "Invalid function predicate '%s'"
			     (eshell-stringify func))))
		(error "Invalid function predicate")))
	     ((eq char ?^)
	      (forward-char)
	      (setq negate (not negate)))
	     ((eq char ?-)
	      (forward-char)
	      (setq follow (not follow)))
	     ((eq char ?|)
	      (forward-char)
	      (if (looking-at "[^|':]")
		  (let ((func (read (current-buffer))))
		    (if (and func (functionp func))
			(setq mods
			      (cons `(lambda (lst)
				       (mapcar (function ,func) lst))
				    mods))
		      (error "Invalid function modifier '%s'"
			     (eshell-stringify func))))
		(error "Invalid function modifier")))
	     ((eq char ?:)
	      (forward-char)
	      (let ((mod (assq (char-after) eshell-modifier-alist)))
		(if (not mod)
		    (error "Unknown modifier character '%c'" (char-after))
		  (forward-char)
		  (setq mods (cons (eval (cdr mod)) mods)))))
	     (t
	      (let ((pred (assq char eshell-predicate-alist)))
		(if (not pred)
		    (error "Unknown predicate character '%c'" char)
		  (forward-char)
		  (setq preds
			(eshell-add-pred-func (eval (cdr pred)) preds
					      negate follow))))))))
      (end-of-buffer
       (error "Predicate or modifier ended prematurely")))
    (cons (nreverse preds) (nreverse mods))))

(defun eshell-add-pred-func (pred funcs negate follow)
  "Add the predicate function PRED to FUNCS."
  (if negate
      (setq pred `(lambda (file)
		    (not (funcall ,pred file)))))
  (if follow
      (setq pred `(lambda (file)
		    (funcall ,pred (file-truename file)))))
  (cons pred funcs))

(defun eshell-pred-user-or-group (mod-char mod-type attr-index get-id-func)
  "Return a predicate to test whether a file match a given user/group id."
  (let (ugid open close end)
    (if (looking-at "[0-9]+")
	(progn
	  (setq ugid (string-to-number (match-string 0)))
	  (goto-char (match-end 0)))
      (setq open (char-after))
      (if (setq close (memq open '(?\( ?\[ ?\< ?\{)))
	  (setq close (car (last '(?\) ?\] ?\> ?\})
				 (length close))))
	(setq close open))
      (forward-char)
      (setq end (eshell-find-delimiter open close))
      (unless end
	(error "Malformed %s name string for modifier `%c'"
	       mod-type mod-char))
      (setq ugid
	    (funcall get-id-func (buffer-substring (point) end)))
      (goto-char (1+ end)))
    (unless ugid
      (error "Unknown %s name specified for modifier `%c'"
	     mod-type mod-char))
    `(lambda (file)
       (let ((attrs (file-attributes file)))
	 (if attrs
	     (= (nth ,attr-index attrs) ,ugid))))))

(defun eshell-pred-file-time (mod-char mod-type attr-index)
  "Return a predicate to test whether a file matches a certain time."
  (let* ((quantum 86400)
	 qual amount when open close end)
    (when (memq (char-after) '(?M ?w ?h ?m ?s))
      (setq quantum (char-after))
      (cond
       ((eq quantum ?M)
	(setq quantum (* 60 60 24 30)))
       ((eq quantum ?w)
	(setq quantum (* 60 60 24 7)))
       ((eq quantum ?h)
	(setq quantum (* 60 60)))
       ((eq quantum ?m)
	(setq quantum 60))
       ((eq quantum ?s)
	(setq quantum 1)))
      (forward-char))
    (when (memq (char-after) '(?+ ?-))
      (setq qual (char-after))
      (forward-char))
    (if (looking-at "[0-9]+")
	(progn
	  (setq when (- (float-time)
			(* (string-to-number (match-string 0))
			   quantum)))
	  (goto-char (match-end 0)))
      (setq open (char-after))
      (if (setq close (memq open '(?\( ?\[ ?\< ?\{)))
	  (setq close (car (last '(?\) ?\] ?\> ?\})
				 (length close))))
	(setq close open))
      (forward-char)
      (setq end (eshell-find-delimiter open close))
      (unless end
	(error "Malformed %s time modifier `%c'" mod-type mod-char))
      (let* ((file (buffer-substring (point) end))
	     (attrs (file-attributes file)))
	(unless attrs
	  (error "Cannot stat file `%s'" file))
	(setq when (float-time (nth attr-index attrs))))
      (goto-char (1+ end)))
    `(lambda (file)
       (let ((attrs (file-attributes file)))
	 (if attrs
	     (,(if (eq qual ?-)
		   '<
		 (if (eq qual ?+)
		     '>
		   '=)) ,when (float-time
			       (nth ,attr-index attrs))))))))

(defun eshell-pred-file-type (type)
  "Return a test which tests that the file is of a certain TYPE.
TYPE must be a character, and should be one of the possible options
that 'ls -l' will show in the first column of its display. "
  (when (eq type ?%)
    (setq type (char-after))
    (if (memq type '(?b ?c))
	(forward-char)
      (setq type ?%)))
  `(lambda (file)
     (let ((attrs (eshell-file-attributes (directory-file-name file))))
       (if attrs
	   (memq (aref (nth 8 attrs) 0)
		 ,(if (eq type ?%)
		      '(?b ?c)
		    (list 'quote (list type))))))))

(defsubst eshell-pred-file-mode (mode)
  "Return a test which tests that MODE pertains to the file."
  `(lambda (file)
     (let ((modes (file-modes file)))
       (if modes
	   (logand ,mode modes)))))

(defun eshell-pred-file-links ()
  "Return a predicate to test whether a file has a given number of links."
  (let (qual amount)
    (when (memq (char-after) '(?- ?+))
      (setq qual (char-after))
      (forward-char))
    (unless (looking-at "[0-9]+")
      (error "Invalid file link count modifier `l'"))
    (setq amount (string-to-number (match-string 0)))
    (goto-char (match-end 0))
    `(lambda (file)
       (let ((attrs (eshell-file-attributes file)))
	 (if attrs
	     (,(if (eq qual ?-)
		   '<
		 (if (eq qual ?+)
		     '>
		   '=)) (nth 1 attrs) ,amount))))))

(defun eshell-pred-file-size ()
  "Return a predicate to test whether a file is of a given size."
  (let ((quantum 1) qual amount)
    (when (memq (downcase (char-after)) '(?k ?m ?p))
      (setq qual (downcase (char-after)))
      (cond
       ((eq qual ?k)
	(setq quantum 1024))
       ((eq qual ?m)
	(setq quantum (* 1024 1024)))
       ((eq qual ?p)
	(setq quantum 512)))
      (forward-char))
    (when (memq (char-after) '(?- ?+))
      (setq qual (char-after))
      (forward-char))
    (unless (looking-at "[0-9]+")
      (error "Invalid file size modifier `L'"))
    (setq amount (* (string-to-number (match-string 0)) quantum))
    (goto-char (match-end 0))
    `(lambda (file)
       (let ((attrs (eshell-file-attributes file)))
	 (if attrs
	     (,(if (eq qual ?-)
		   '<
		 (if (eq qual ?+)
		     '>
		   '=)) (nth 7 attrs) ,amount))))))

(defun eshell-pred-substitute (&optional repeat)
  "Return a modifier function that will substitute matches."
  (let ((delim (char-after))
	match replace end)
    (forward-char)
    (setq end (eshell-find-delimiter delim delim nil nil t)
	  match (buffer-substring-no-properties (point) end))
    (goto-char (1+ end))
    (setq end (eshell-find-delimiter delim delim nil nil t)
	  replace (buffer-substring-no-properties (point) end))
    (goto-char (1+ end))
    (if repeat
	`(lambda (lst)
	   (mapcar
	    (function
	     (lambda (str)
	       (let ((i 0))
		 (while (setq i (string-match ,match str i))
		   (setq str (replace-match ,replace t nil str))))
	       str)) lst))
      `(lambda (lst)
	 (mapcar
	  (function
	   (lambda (str)
	     (if (string-match ,match str)
		 (setq str (replace-match ,replace t nil str)))
	     str)) lst)))))

(defun eshell-include-members (&optional invert-p)
  "Include only lisp members matching a regexp."
  (let ((delim (char-after))
	regexp end)
    (forward-char)
    (setq end (eshell-find-delimiter delim delim nil nil t)
	  regexp (buffer-substring-no-properties (point) end))
    (goto-char (1+ end))
    `(lambda (lst)
       (eshell-winnow-list
	lst nil '((lambda (elem)
		    ,(if invert-p
			 `(not (string-match ,regexp elem))
		       `(string-match ,regexp elem))))))))

(defun eshell-join-members ()
  "Return a modifier function that join matches."
  (let ((delim (char-after))
	str end)
    (if (not (memq delim '(?' ?/)))
	(setq delim " ")
      (forward-char)
      (setq end (eshell-find-delimiter delim delim nil nil t)
	    str (buffer-substring-no-properties (point) end))
      (goto-char (1+ end)))
    `(lambda (lst)
       (mapconcat 'identity lst ,str))))

(defun eshell-split-members ()
  "Return a modifier function that splits members."
  (let ((delim (char-after))
	sep end)
    (when (memq delim '(?' ?/))
      (forward-char)
      (setq end (eshell-find-delimiter delim delim nil nil t)
	    sep (buffer-substring-no-properties (point) end))
      (goto-char (1+ end)))
    `(lambda (lst)
       (mapcar
	(function
	 (lambda (str)
	   (split-string str ,sep))) lst))))

(provide 'em-pred)

;; Local Variables:
;; generated-autoload-file: "esh-groups.el"
;; End:

;;; em-pred.el ends here
