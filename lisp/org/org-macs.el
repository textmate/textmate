;;; org-macs.el --- Top-level definitions for Org-mode

;; Copyright (C) 2004-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains macro definitions, defsubst definitions, other
;; stuff needed for compilation and top-level forms in Org-mode, as well
;; lots of small functions that are not org-mode specific but simply
;; generally useful stuff.

;;; Code:

(eval-and-compile
  (unless (fboundp 'declare-function)
    (defmacro declare-function (fn file &optional arglist fileonly)))
  (if (>= emacs-major-version 23)
      (defsubst org-char-to-string(c)
	"Defsubst to decode UTF-8 character values in emacs 23 and beyond."
	(char-to-string c))
    (defsubst org-char-to-string (c)
      "Defsubst to decode UTF-8 character values in emacs 22."
      (string (decode-char 'ucs c)))))

(declare-function org-add-props "org-compat" (string plist &rest props))
(declare-function org-string-match-p "org-compat" (&rest args))

(defmacro org-with-gensyms (symbols &rest body)
  `(let ,(mapcar (lambda (s)
		   `(,s (make-symbol (concat "--" (symbol-name ',s))))) symbols)
     ,@body))
(def-edebug-spec org-with-gensyms (sexp body))
(put 'org-with-gensyms 'lisp-indent-function 1)

(defmacro org-called-interactively-p (&optional kind)
  (if (featurep 'xemacs)
       `(interactive-p)
     (if (or (> emacs-major-version 23)
	     (and (>= emacs-major-version 23)
		  (>= emacs-minor-version 2)))
	 `(with-no-warnings (called-interactively-p ,kind)) ;; defined with no argument in <=23.1
       `(interactive-p))))
(def-edebug-spec org-called-interactively-p (&optional ("quote" symbolp)))

(when (and (not (fboundp 'with-silent-modifications))
	 (or (< emacs-major-version 23)
	     (and (= emacs-major-version 23)
		  (< emacs-minor-version 2))))
    (defmacro with-silent-modifications (&rest body)
      `(org-unmodified ,@body))
    (def-edebug-spec with-silent-modifications (body)))

(defmacro org-bound-and-true-p (var)
  "Return the value of symbol VAR if it is bound, else nil."
  `(and (boundp (quote ,var)) ,var))
(def-edebug-spec org-bound-and-true-p (symbolp))

(defun org-string-nw-p (s)
  "Is S a string with a non-white character?"
  (and (stringp s)
       (org-string-match-p "\\S-" s)
       s))

(defun org-not-nil (v)
  "If V not nil, and also not the string \"nil\", then return V.
Otherwise return nil."
  (and v (not (equal v "nil")) v))

(defmacro org-unmodified (&rest body)
  "Execute body without changing `buffer-modified-p'.
Also, do not record undo information."
  `(set-buffer-modified-p
    (prog1 (buffer-modified-p)
      (let ((buffer-undo-list t)
	    before-change-functions after-change-functions)
	,@body))))
(def-edebug-spec org-unmodified (body))

(defun org-substitute-posix-classes (re)
  "Substitute posix classes in regular expression RE."
  (let ((ss re))
    (save-match-data
      (while (string-match "\\[:alnum:\\]" ss)
	(setq ss (replace-match "a-zA-Z0-9" t t ss)))
      (while (string-match "\\[:word:\\]" ss)
	(setq ss (replace-match "a-zA-Z0-9" t t ss)))
      (while (string-match "\\[:alpha:\\]" ss)
	(setq ss (replace-match "a-zA-Z" t t ss)))
      (while (string-match "\\[:punct:\\]" ss)
	(setq ss (replace-match "\001-@[-`{-~" t t ss)))
      ss)))

(defmacro org-re (s)
  "Replace posix classes in regular expression."
  (if (featurep 'xemacs) `(org-substitute-posix-classes ,s) s))
(def-edebug-spec org-re (form))

(defmacro org-preserve-lc (&rest body)
  (org-with-gensyms (line col)
    `(let ((,line (org-current-line))
	   (,col (current-column)))
       (unwind-protect
	   (progn ,@body)
	 (org-goto-line ,line)
	 (org-move-to-column ,col)))))
(def-edebug-spec org-preserve-lc (body))

(defmacro org-without-partial-completion (&rest body)
  `(if (and (boundp 'partial-completion-mode)
	    partial-completion-mode
	    (fboundp 'partial-completion-mode))
     (unwind-protect
	 (progn
	   (partial-completion-mode -1)
	   ,@body)
       (partial-completion-mode 1))
     ,@body))
(def-edebug-spec org-without-partial-completion (body))

;; FIXME: Slated for removal. Current Org mode does not support Emacs < 22
(defmacro org-maybe-intangible (props)
  "Add '(intangible t) to PROPS if Emacs version is earlier than Emacs 22.
In Emacs 21, invisible text is not avoided by the command loop, so the
intangible property is needed to make sure point skips this text.
In Emacs 22, this is not necessary.  The intangible text property has
led to problems with flyspell.  These problems are fixed in flyspell.el,
but we still avoid setting the property in Emacs 22 and later.
We use a macro so that the test can happen at compilation time."
  (if (< emacs-major-version 22)
      `(append '(intangible t) ,props)
    props))

(defmacro org-with-point-at (pom &rest body)
  "Move to buffer and point of point-or-marker POM for the duration of BODY."
  (org-with-gensyms (mpom)
    `(let ((,mpom ,pom))
       (save-excursion
	 (if (markerp ,mpom) (set-buffer (marker-buffer ,mpom)))
	 (save-excursion
	   (goto-char (or ,mpom (point)))
	   ,@body)))))
(def-edebug-spec org-with-point-at (form body))
(put 'org-with-point-at 'lisp-indent-function 1)

(defmacro org-no-warnings (&rest body)
  (cons (if (fboundp 'with-no-warnings) 'with-no-warnings 'progn) body))
(def-edebug-spec org-no-warnings (body))

(defmacro org-if-unprotected (&rest body)
  "Execute BODY if there is no `org-protected' text property at point."
  `(unless (get-text-property (point) 'org-protected)
     ,@body))
(def-edebug-spec org-if-unprotected (body))

(defmacro org-if-unprotected-1 (&rest body)
  "Execute BODY if there is no `org-protected' text property at point-1."
  `(unless (get-text-property (1- (point)) 'org-protected)
     ,@body))
(def-edebug-spec org-if-unprotected-1 (body))

(defmacro org-if-unprotected-at (pos &rest body)
  "Execute BODY if there is no `org-protected' text property at POS."
  `(unless (get-text-property ,pos 'org-protected)
     ,@body))
(def-edebug-spec org-if-unprotected-at (form body))
(put 'org-if-unprotected-at 'lisp-indent-function 1)

(defun org-re-search-forward-unprotected (&rest args)
  "Like re-search-forward, but stop only in unprotected places."
  (catch 'exit
    (while t
      (unless (apply 're-search-forward args)
	(throw 'exit nil))
      (unless (get-text-property (match-beginning 0) 'org-protected)
	(throw 'exit (point))))))

;; FIXME: Normalize argument names
(defmacro org-with-remote-undo (_buffer &rest _body)
  "Execute BODY while recording undo information in two buffers."
  (org-with-gensyms (cline cmd buf1 buf2 undo1 undo2 c1 c2)
    `(let ((,cline (org-current-line))
	   (,cmd this-command)
	   (,buf1 (current-buffer))
	   (,buf2 ,_buffer)
	   (,undo1 buffer-undo-list)
	   (,undo2 (with-current-buffer ,_buffer buffer-undo-list))
	   ,c1 ,c2)
       ,@_body
       (when org-agenda-allow-remote-undo
	 (setq ,c1 (org-verify-change-for-undo
		    ,undo1 (with-current-buffer ,buf1 buffer-undo-list))
	       ,c2 (org-verify-change-for-undo
		    ,undo2 (with-current-buffer ,buf2 buffer-undo-list)))
	 (when (or ,c1 ,c2)
	   ;; make sure there are undo boundaries
	   (and ,c1 (with-current-buffer ,buf1 (undo-boundary)))
	   (and ,c2 (with-current-buffer ,buf2 (undo-boundary)))
	   ;; remember which buffer to undo
	   (push (list ,cmd ,cline ,buf1 ,c1 ,buf2 ,c2)
		 org-agenda-undo-list))))))
(def-edebug-spec org-with-remote-undo (form body))
(put 'org-with-remote-undo 'lisp-indent-function 1)

(defmacro org-no-read-only (&rest body)
  "Inhibit read-only for BODY."
  `(let ((inhibit-read-only t)) ,@body))
(def-edebug-spec org-no-read-only (body))

(defconst org-rm-props '(invisible t face t keymap t intangible t mouse-face t
				   rear-nonsticky t mouse-map t fontified t
				   org-emphasis t)
  "Properties to remove when a string without properties is wanted.")

(defsubst org-match-string-no-properties (num &optional string)
  (if (featurep 'xemacs)
      (let ((s (match-string num string)))
	(and s (remove-text-properties 0 (length s) org-rm-props s))
	s)
    (match-string-no-properties num string)))

(defsubst org-no-properties (s)
  (if (fboundp 'set-text-properties)
      (set-text-properties 0 (length s) nil s)
    (remove-text-properties 0 (length s) org-rm-props s))
  s)

(defsubst org-get-alist-option (option key)
  (cond ((eq key t) t)
	((eq option t) t)
	((assoc key option) (cdr (assoc key option)))
	(t (cdr (assq 'default option)))))

(defsubst org-check-external-command (cmd &optional use no-error)
  "Check if external program CMD for USE exists, error if not.
When the program does exist, return its path.
When it does not exist and NO-ERROR is set, return nil.
Otherwise, throw an error.  The optional argument USE can describe what this
program is needed for, so that the error message can be more informative."
  (or (executable-find cmd)
      (if no-error
	  nil
	(error "Can't find `%s'%s" cmd
	       (if use (format " (%s)" use) "")))))

(defsubst org-inhibit-invisibility ()
  "Modified `buffer-invisibility-spec' for Emacs 21.
Some ops with invisible text do not work correctly on Emacs 21.  For these
we turn off invisibility temporarily.  Use this in a `let' form."
  (if (< emacs-major-version 22) nil buffer-invisibility-spec))

(defsubst org-set-local (var value)
  "Make VAR local in current buffer and set it to VALUE."
  (set (make-local-variable var) value))

(defsubst org-last (list)
  "Return the last element of LIST."
  (car (last list)))

(defun org-let (list &rest body)
  (eval (cons 'let (cons list body))))
(put 'org-let 'lisp-indent-function 1)

(defun org-let2 (list1 list2 &rest body)
  (eval (cons 'let (cons list1 (list (cons 'let (cons list2 body)))))))
(put 'org-let2 'lisp-indent-function 2)

(defsubst org-call-with-arg (command arg)
  "Call COMMAND interactively, but pretend prefix arg was ARG."
  (let ((current-prefix-arg arg)) (call-interactively command)))

(defsubst org-current-line (&optional pos)
  (save-excursion
    (and pos (goto-char pos))
    ;; works also in narrowed buffer, because we start at 1, not point-min
    (+ (if (bolp) 1 0) (count-lines 1 (point)))))

(defsubst org-goto-line (N)
  (save-restriction
    (widen)
    (goto-char (point-min))
    (forward-line (1- N))))

(defsubst org-current-line-string (&optional to-here)
  (buffer-substring (point-at-bol) (if to-here (point) (point-at-eol))))

(defsubst org-pos-in-match-range (pos n)
  (and (match-beginning n)
       (<= (match-beginning n) pos)
       (>= (match-end n) pos)))

(defun org-autoload (file functions)
  "Establish autoload for all FUNCTIONS in FILE, if not bound already."
  (let ((d (format "Documentation will be available after `%s.el' is loaded."
		   file))
	f)
    (while (setq f (pop functions))
      (or (fboundp f) (autoload f file d t)))))

(defun org-match-line (re)
  "Looking-at at the beginning of the current line."
  (save-excursion
    (goto-char (point-at-bol))
    (looking-at re)))

(defun org-plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
	  (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun org-replace-match-keep-properties (newtext &optional fixedcase
						  literal string)
  "Like `replace-match', but add the text properties found original text."
  (setq newtext (org-add-props newtext (text-properties-at
					(match-beginning 0) string)))
  (replace-match newtext fixedcase literal string))

(defmacro org-save-outline-visibility (use-markers &rest body)
  "Save and restore outline visibility around BODY.
If USE-MARKERS is non-nil, use markers for the positions.
This means that the buffer may change while running BODY,
but it also means that the buffer should stay alive
during the operation, because otherwise all these markers will
point nowhere."
  (declare (indent 1))
  (org-with-gensyms (data rtn)
    `(let ((,data (org-outline-overlay-data ,use-markers))
	   ,rtn)
       (unwind-protect
	   (progn
	     (setq ,rtn (progn ,@body))
	     (org-set-outline-overlay-data ,data))
	 (when ,use-markers
	   (mapc (lambda (c)
		   (and (markerp (car c)) (move-marker (car c) nil))
		   (and (markerp (cdr c)) (move-marker (cdr c) nil)))
		 ,data)))
       ,rtn)))
(def-edebug-spec org-save-outline-visibility (form body))

(defmacro org-with-wide-buffer (&rest body)
 "Execute body while temporarily widening the buffer."
 `(save-excursion
    (save-restriction
       (widen)
       ,@body)))
(def-edebug-spec org-with-wide-buffer (body))

(defmacro org-with-limited-levels (&rest body)
  "Execute BODY with limited number of outline levels."
  `(let* ((org-outline-regexp (org-get-limited-outline-regexp))
	  (outline-regexp org-outline-regexp)
	  (org-outline-regexp-at-bol (concat "^" org-outline-regexp)))
     ,@body))
(def-edebug-spec org-with-limited-levels (body))

(defvar org-outline-regexp) ; defined in org.el
(defvar org-odd-levels-only) ; defined in org.el
(defvar org-inlinetask-min-level) ; defined in org-inlinetask.el
(defun org-get-limited-outline-regexp ()
  "Return outline-regexp with limited number of levels.
The number of levels is controlled by `org-inlinetask-min-level'"
  (if (or (not (eq major-mode 'org-mode)) (not (featurep 'org-inlinetask)))
      org-outline-regexp
    (let* ((limit-level (1- org-inlinetask-min-level))
	   (nstars (if org-odd-levels-only (1- (* limit-level 2)) limit-level)))
      (format "\\*\\{1,%d\\} " nstars))))

(defun org-format-seconds (string seconds)
  "Compatibility function replacing format-seconds"
  (if (fboundp 'format-seconds)
      (format-seconds string seconds)
    (format-time-string string (seconds-to-time seconds))))

(defmacro org-eval-in-environment (environment form)
  `(eval (list 'let ,environment ',form)))
(def-edebug-spec org-eval-in-environment (form form))
(put 'org-eval-in-environment 'lisp-indent-function 1)

(defun org-make-parameter-alist (flat)
  "Return alist based on FLAT.
FLAT is a list with alternating symbol names and values. The
returned alist is a list of lists with the symbol name in car and
the value in cdr."
  (when flat
    (cons (list (car flat) (cadr flat))
         (org-make-parameter-alist (cddr flat)))))

(provide 'org-macs)

;;; org-macs.el ends here
