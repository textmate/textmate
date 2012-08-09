;;; robin.el --- yet another input method (smaller than quail)

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number: H15PRO110

;; Author: TAKAHASHI Naoto <ntakahas@m17n.org>
;; Keywords: mule, multilingual, input method, i18n

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

;; Functionalities
;; ---------------

;; Robin is a new input method for GNU Emacs.  It has three
;; functionalities:

;; 1. It serves as a simple input method.  When the user types an ASCII
;;    key sequence, robin converts it into a string.  This functionality
;;    is most likely used to input non-ASCII characters.

;; 2. It converts existing buffer substring into another string.
;;    This functionality is similar to the 1. above, but the input is
;;    buffer substring rather than key strokes.

;; 3. It offers reverse conversion.  Each character produced by a
;;    robin rule can hold the original ASCII sequence as a
;;    char-code-property.


;; How to define conversion rules
;; ------------------------------

;; Each conversion rule belongs to a robin package.  A robin package is
;; identified by a string called package name.  Use robin-define-package
;; to define a robin package.

;; (robin-define-package NAME DOCSTRING
;;   (INPUT1 OUTPUT1)
;;   (INPUT2 OUTPUT2)
;;   ...)

;; NAME is a string identifying the robin package.  It often starts with a
;; language name and followed by a method name.  For example,
;; french-postfix, greek-prefix, etc.

;; DOCSTRING is a documentation string for the robin method.

;; Each INPUTn is a string.  It represents a transliteration of the
;; corresponding OUTPUTn.

;; Each OUTPUTn is a string or a character that is to be inserted as the
;; result of conversion.

;; Neither INPUT* nor OUTPUT* are evaluated.  Do not use a variable or a
;; function in those parts.  Instead, use a string or character literal
;; directly.

;; If multiple rules have the same input pattern but different output
;; patterns, only the latest definition is effective.


;; Example
;; -------

;; (robin-define-package "german-example"
;;  "An example for German

;; AE -> Ä   OE -> Ö   UE -> Ü
;; ae -> ä   oe -> ö   ue -> ü   ss -> ß

;; Repeat E or S to input itself.

;; AEE -> AE   OEE -> OE   UEE -> UE
;; aee -> ae   oee -> oe   uee -> ue   sss -> ss"

;;  ("AE" ?Ä)
;;  ("OE" ?Ö)
;;  ("UE" ?Ü)
;;  ("ae" ?ä)
;;  ("oe" ?ö)
;;  ("ue" ?ü)
;;  ("ss" ?ß)

;;  ("AEE" "AE")
;;  ("OEE" "OE")
;;  ("UEE" "UE")
;;  ("aee" "ae")
;;  ("oee" "oe")
;;  ("uee" "ue")
;;  ("sss" "ss")
;; )


;; Using robin as an input method
;; ------------------------------

;; To use a defined robin package as an input method, register it with
;; the register-input-method function.  For example,

;; (register-input-method
;;  "german-example"
;;  "german"
;;  'robin-use-package
;;  "de"
;;  "An example for German")

;; The first argument is the robin package name.

;; The second argument is the language environment for which this robin
;; package is used.

;; Use the symbol `robin-use-package' as the third argument.

;; The fourth argument is the prompt that appears in modeline when this
;; input method is active.

;; The fifth argument is a documentation string; it may or may not be
;; identical to the one that you specified in robin-define-package.

;; You can activate the robin input method by typing

;;  C-u C-\ german-example RET

;; Just like a quail package, only C-\ suffices for subsequent
;; invocation.


;; Using robin as a buffer translator
;; ----------------------------------

;; To transliterate buffer substring, use the following functions.

;; (robin-convert-buffer &optional name)

;; Convert the content of current buffer using a robin package.

;; NAME, if given, is a string specifying a robin package.  If NAME is
;; not given or nil, the value of `robin-current-package-name' is used.

;; (robin-convert-region begin end &optional name)

;; Convert the region using a robin package.

;; NAME, if given, is a string specifying a robin package.  If NAME is
;; not given or nil, the value of `robin-current-package-name' is used.


;; Reverse conversion
;; ------------------

;; If the output pattern defined in a robin rule is a character, robin
;; gives to the character a char-code-property whose key is the symbol
;; representation of the robin package name and whose value is the input
;; pattern of that character.  For example, with the "german-example"
;; definition above,

;; (get-char-code-property ?Ä 'german-example) => "AE"

;; etc.

;; If you do not want to assign a char-code-property to a character, use
;; a string of length one as the output pattern, e.g.

;; (robin-define-package "german-example2"
;;  "Another example for German."

;;  ("AE" "Ä")
;;  ("OE" "Ö")
;;  ...)

;; Then

;; (get-char-code-property ?Ä 'german-example2) => nil

;; etc.

;; If multiple input patterns in a robin package generate the same
;; character, the lastly used input pattern is given as the value of the
;; char-code-property.

;; There are two functions for reverse conversion.

;; (robin-invert-buffer &optional name)

;; Apply reverse conversion to the content of current buffer.  NAME, if
;; given, is a string specifying a robin package.  If NAME is not given
;; or nil, the value of `robin-current-package-name' is used.

;; (robin-invert-region begin end &optional name)

;; Apply reverse conversion to the region.  NAME, if given, is a string
;; specifying a robin package.  If NAME is not given or nil, the value of
;; `robin-current-package-name' is used.


;; Modifying an existing rule
;; --------------------------

;; Use the robin-modify-package function to modify a rule already defined
;; in a Robin package.

;; (robin-modify-package name input output)

;; Change a rule in an already defined Robin package.
;; NAME is the string specifying a robin package.
;; INPUT is a string that specifies the input pattern.
;; OUTPUT is either a character or a string to be generated.


;; The name of the game
;; --------------------

;; As stated in Murphy's law, it took longer than expected to develop the
;; very first version of Japanese input subsystem in NEmacs (Nihongo
;; Emacs).  So the subsystem was named "TAMAGO", which is an acronym of
;; "TAkusan Matasete GOmen-nasai" (Sorry to have kept you waiting so
;; long).  "Tamago" as a Japanese word means "egg", so the word "egg" was
;; also used for related filenames and function names.

;; Since it was designed to input CJK characters, Egg was rather big as a
;; subsystem.  So later in Mule (Multilingual Enhancement to GNU Emacs),
;; we designed and implemented a smaller input subsystem.  We had to give
;; it a name.  "So, what's smaller than an egg?"  "A quail egg, of
;; course."  Therefore it was named "quail".

;; As time went by, quail became more and more complicated.  That
;; tendency was inevitable as long as we support CJK input.  However, if
;; we can limit ourselves to non-CJK characters, a much simpler
;; transliteration mechanism suffices.  So I wrote "robin", whose name
;; was chosen because a robin is smaller than a quail.  I could name it
;; "hummingbird" or "nightingale", but those spellings seemed too long.


;;; Code:

(defvar robin-package-alist nil
  "List of robin packages.
A robin package is of the form (NAME DOCSTRING &rest RULES).
NAME is a string specifying a particular robin package.
DOCSTRING is a documentation string for the robin package.

RULE is of the form (KEY OUTPUT &rest rules).
KEY is a string.
OUTPUT is a character or a string.
For example, if you evaluate the following,

\(robin-define-package \"test\" \"Uppercase input characters\"
  (\"a\" \"A\")
  (\"ab\" \"AB\")
  (\"ac\" \"AC\")
  (\"acd\" \"ACD\")
  (\"ace\" \"ACE\")
  (\"b\" \"B\"))

this robin package will be the following.

  (\"test\" \"Uppercase input characters\"
   (?a \"A\"
       (?b \"AB\")
       (?c \"AC\"
	   (?d \"ACD\")
	   (?e \"ACE\")))
   (?b \"B\"))
")

;;;###autoload
(defmacro robin-define-package (name docstring &rest rules)
  "Define a robin package.

NAME is the string of this robin package.
DOCSTRING is the documentation string of this robin package.
Each RULE is of the form (INPUT OUTPUT) where INPUT is a string and
OUTPUT is either a character or a string.  RULES are not evaluated.

If there already exists a robin package whose name is NAME, the new
one replaces the old one."

  (let ((iname (intern name))
	(new (list name ""))		; "" as a fake output
	input output pairs)
    (dolist (r rules)
      (setq input (car r)
	    output (cadr r))
      (robin-add-rule name new input output)
      (cond
       ((not (stringp input))
	(error "Bad input sequence %S" r))
       ((characterp output)
	(setq pairs
	      (cons (cons input output)
		    pairs)))
       ((not (stringp output))
	(error "Bad output pattern %S" r))))
    (setcar (cdr new) docstring)	; replace "" above with real docstring
    `(let ((slot (assoc ,name robin-package-alist))
	   (newdef ',new)
	   (prop ',iname)
	   (lst ',pairs))
       (if slot
	   (setcdr slot (cdr newdef))
	 (setq robin-package-alist
	       (cons newdef robin-package-alist)))
       (dolist (l lst)
	 (put-char-code-property (cdr l) prop (car l))))))

;;;###autoload
(defun robin-modify-package (name input output)
  "Change a rule in an already defined robin package.

NAME is the string specifying a robin package.
INPUT is a string that specifies the input pattern.
OUTPUT is either a character or a string to be generated."

  (let ((tree (assoc name robin-package-alist))
	docstring)
    (if (not tree)
	(error "No such robin package")
      (setq docstring (cadr tree))
      (setcar (cdr tree) "")
      (robin-add-rule name tree input output)
      (setcar (cdr tree) docstring)
      (if (characterp output)
	  (put-char-code-property output (intern name) input))))
  output)

(defun robin-add-rule (name tree input output)
  "Add translation rule (INPUT OUTPUT) to TREE whose name is NAME.
Internal use only."

  (let* ((head (aref input 0))
	 (branch (assoc head tree))
	 (sofar (cadr tree)))

    (if (= (length input) 1)
	(if branch

	    ;; A definition already exists for this input.
	    ;; We do not cancel old char-code-property of OUTPUT
	    ;; so that n-to-1 reverse conversion is possible.
	    (setcar (cdr branch) output)

	  ;; New definition for this input.
	  (setcdr (last tree) (list (list head output))))

      (unless branch
	(if (characterp sofar)
	    (setq sofar (char-to-string sofar)))
	(setq branch
	      (list head
		    (concat sofar
			    (char-to-string head))))
	(setcdr (last tree) (list branch)))

      (robin-add-rule name branch (substring input 1) output))))

;;; Interactive use

(defvar robin-mode nil
  "If non-nil, `robin-input-method' is active.")
(make-variable-buffer-local 'robin-mode)

(defvar robin-current-package-name nil
  "String representing the name of the current robin package.
A nil value means no package is selected.")
(make-variable-buffer-local 'robin-current-package-name)

;;;###autoload
(defun robin-use-package (name)
  "Start using robin package NAME, which is a string."

  (let ((package (assoc name robin-package-alist)))
    (unless package
      (error "No such robin package"))
    (setq robin-current-package-name name)
    (robin-activate)))

(defun robin-inactivate ()
  "Inactivate robin input method."

  (interactive)
  (robin-activate -1))

(defun robin-activate (&optional arg)
  "Activate robin input method.

With ARG, activate robin input method if and only if ARG is positive.

While this input method is active, the variable
`input-method-function' is bound to the function `robin-input-method'."
  (if (and arg
	   (< (prefix-numeric-value arg) 0))

      ;; inactivate robin input method.
      (unwind-protect
	  (progn
	    (setq robin-mode nil)
	    (setq describe-current-input-method-function nil)
	    (run-hooks 'robin-inactivate-hook))
	(kill-local-variable 'input-method-function))

    ;; activate robin input method.
    (setq robin-mode t
      	  describe-current-input-method-function 'robin-help
	  inactivate-current-input-method-function 'robin-inactivate)
    (if (eq (selected-window) (minibuffer-window))
	(add-hook 'minibuffer-exit-hook 'robin-exit-from-minibuffer))
    (run-hooks 'input-method-activate-hook
	       'robin-activate-hook)
    (set (make-local-variable 'input-method-function)
	 'robin-input-method)))

(defun robin-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'robin-exit-from-minibuffer)))

(defun robin-input-method (key)
  "Interpret typed key sequence and insert into buffer."

  (if (or buffer-read-only
	  overriding-terminal-local-map
	  overriding-local-map)
      (list key)

    (let ((echo-keystrokes 0)
	  (input-method-function nil)
	  (start (point))
	  (tree (cddr (assoc robin-current-package-name robin-package-alist)))
	  branch
	  output)

      (while (setq branch (assq key tree))
	(delete-region start (point))
	(insert (setq output (cadr branch)))
	(setq tree (cddr branch))
	(if tree
	    (setq key (read-event))
	  (setq key nil)))

      (if (null output)
	  ;; body of the `while' above was not executed
	  (list key)
	(delete-region start (point))
	(if key
	    (setq unread-command-events (list key)))
	(if (stringp output)
	    (string-to-list output)
	  (list output))))))

(defun robin-help ()
  "Display the docstring of the current robin package."

  (interactive)
  (let ((buf (get-buffer-create "*Robin Help*"))
	(doc (cadr (assoc robin-current-package-name robin-package-alist))))
    (set-buffer buf)
    (erase-buffer)
    (insert doc)
    (goto-char (point-min))
    (display-buffer buf)))

;;; Batch mode

(defun robin-convert-buffer (&optional name)
  "Convert the content of current buffer using a robin package.
NAME, if given, is a string specifying a robin package.  If NAME
is not given or nil, the value of `robin-current-package-name' is
used."

  (interactive "*")
  (robin-convert-region (point-min) (point-max) name))

(defun robin-convert-region (begin end &optional name)
  "Convert the region using a robin package.
NAME, if given, is a string specifying a robin package.  If NAME
is not given or nil, the value of `robin-current-package-name' is
used."

  (interactive "*r")
  (or name
      (setq name robin-current-package-name)
      (error "No robin package specified"))

  (let ((tree (assoc name robin-package-alist)))
    (unless tree
      (error "No such robin package"))

    (save-excursion
      (save-restriction
	(narrow-to-region begin end)
	(goto-char (point-min))
	(while (not (eobp))
	  (robin-convert-region-internal tree))))))

(defun robin-convert-region-internal (tree)
  "Apply a robin rule defined in TREE to the current point.
Use the longest match method to select a rule."

  (let ((begin (point))
	end branch)
    (while (setq branch (assq (following-char) tree))
      (setq tree branch)
      (forward-char 1))

    (setq end (point))
    (if (= begin end)
	;; no matching rule found; leave it as it is
	(forward-char 1)
      ;; replace the string
      (goto-char begin)
      (insert (cadr tree))
      (delete-char (- end begin)))))

;; for backward compatibility

(fset 'robin-transliterate-region 'robin-convert-region)
(fset 'robin-transliterate-buffer 'robin-convert-buffer)

;;; Reverse conversion

(defun robin-invert-buffer (&optional name)
  "Apply reverse conversion to the content of current buffer.
NAME, if given, is a string specifying a robin package.  If NAME
is not given or nil, the value of `robin-current-package-name' is
used."

  (interactive "*")
  (robin-invert-region (point-min) (point-max) name))

(defun robin-invert-region (begin end &optional name)
  "Apply reverse conversion to the region.
NAME, if given, is a string specifying a robin package.  If NAME
is not given or nil, the value of `robin-current-package-name' is
used."

  (interactive "*r")
  (or name
      (setq name robin-current-package-name)
      (error "No robin package specified"))

  (setq name (intern name))
  (let (str)
    (save-restriction
      (narrow-to-region begin end)
      (goto-char (point-min))
      (while (not (eobp))
	(if (not (setq str (get-char-code-property (following-char) name)))
	    (forward-char 1)
	  (insert str)
	  (delete-char 1))))))

(provide 'robin)

;; Local Variables:
;; coding: utf-8-emacs
;; End:

;;; robin.el ends here
