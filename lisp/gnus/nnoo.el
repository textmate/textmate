;;; nnoo.el --- OO Gnus Backends

;; Copyright (C) 1996-2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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

(require 'nnheader)
(eval-when-compile (require 'cl))

(defvar nnoo-definition-alist nil)
(defvar nnoo-state-alist nil)
(defvar nnoo-parent-backend nil)

(defmacro defvoo (var init &optional doc &rest map)
  "The same as `defvar', only takes list of variables to MAP to."
  `(prog1
       ,(if doc
	    `(defvar ,var ,init ,(concat doc "\n\nThis is a Gnus server variable.  See Info node `(gnus)Select Methods'."))
	  `(defvar ,var ,init))
     (nnoo-define ',var ',map)))
(put 'defvoo 'lisp-indent-function 2)
(put 'defvoo 'edebug-form-spec '(var init &optional doc &rest map))

(defmacro deffoo (func args &rest forms)
  "The same as `defun', only register FUNC."
  `(prog1
       (defun ,func ,args ,@forms)
     (nnoo-register-function ',func)))
(put 'deffoo 'lisp-indent-function 2)
(put 'deffoo 'edebug-form-spec '(&define name lambda-list def-body))

(defun nnoo-register-function (func)
  (let ((funcs (nthcdr 3 (assoc (nnoo-backend func)
				nnoo-definition-alist))))
    (unless funcs
      (error "%s belongs to a backend that hasn't been declared" func))
    (setcar funcs (cons func (car funcs)))))

(defmacro nnoo-declare (backend &rest parents)
  `(eval-and-compile
     (if (assq ',backend nnoo-definition-alist)
	 (setcar (cdr (assq ',backend nnoo-definition-alist))
		 (mapcar 'list ',parents))
       (push (list ',backend
		   (mapcar 'list ',parents)
		   nil nil)
	     nnoo-definition-alist))
     (unless (assq ',backend nnoo-state-alist)
       (push (list ',backend "*internal-non-initialized-backend*")
	     nnoo-state-alist))))
(put 'nnoo-declare 'lisp-indent-function 1)

(defun nnoo-parents (backend)
  (nth 1 (assoc backend nnoo-definition-alist)))

(defun nnoo-variables (backend)
  (nth 2 (assoc backend nnoo-definition-alist)))

(defun nnoo-functions (backend)
  (nth 3 (assoc backend nnoo-definition-alist)))

(defmacro nnoo-import (backend &rest imports)
  `(nnoo-import-1 ',backend ',imports))
(put 'nnoo-import 'lisp-indent-function 1)

(defun nnoo-import-1 (backend imports)
  (let ((call-function
	 (if (symbolp (car imports)) (pop imports) 'nnoo-parent-function))
	imp functions function)
    (while (setq imp (pop imports))
      (setq functions
	    (or (cdr imp)
		(nnoo-functions (car imp))))
      (while functions
	(unless (fboundp
		 (setq function
		       (nnoo-symbol backend
				    (nnoo-rest-symbol (car functions)))))
	  (eval `(deffoo ,function (&rest args)
		   (,call-function ',backend ',(car functions) args))))
	(pop functions)))))

(defun nnoo-parent-function (backend function args)
  (let ((pbackend (nnoo-backend function))
	(nnoo-parent-backend backend))
    (nnoo-change-server pbackend
			(nnoo-current-server backend)
			(cdr (assq pbackend (nnoo-parents backend))))
    (prog1
	(apply function args)
      ;; Copy the changed variables back into the child.
      (let ((vars (cdr (assq pbackend (nnoo-parents backend)))))
	(while vars
	  (set (cadar vars) (symbol-value (caar vars)))
	  (setq vars (cdr vars)))))))

(defun nnoo-execute (backend function &rest args)
  "Execute FUNCTION on behalf of BACKEND."
  (let ((pbackend (nnoo-backend function))
	(nnoo-parent-backend backend))
    (nnoo-change-server pbackend
			(nnoo-current-server backend)
			(cdr (assq pbackend (nnoo-parents backend))))
    (prog1
	(apply function args)
      ;; Copy the changed variables back into the child.
      (let ((vars (cdr (assq pbackend (nnoo-parents backend)))))
	(while vars
	  (set (cadar vars) (symbol-value (caar vars)))
	  (setq vars (cdr vars)))))))

(defmacro nnoo-map-functions (backend &rest maps)
  `(nnoo-map-functions-1 ',backend ',maps))
(put 'nnoo-map-functions 'lisp-indent-function 1)

(defun nnoo-map-functions-1 (backend maps)
  (let (m margs i)
    (while (setq m (pop maps))
      (setq i 0
	    margs nil)
      (while (< i (length (cdr m)))
	(if (numberp (nth i (cdr m)))
	    (push `(nth ,i args) margs)
	  (push (nth i (cdr m)) margs))
	(incf i))
      (eval `(deffoo ,(nnoo-symbol backend (nnoo-rest-symbol (car m)))
		 (&rest args)
	       (nnoo-parent-function ',backend ',(car m)
				     ,(cons 'list (nreverse margs))))))))

(defun nnoo-backend (symbol)
  (string-match "^[^-]+-" (symbol-name symbol))
  (intern (substring (symbol-name symbol) 0 (1- (match-end 0)))))

(defun nnoo-rest-symbol (symbol)
  (string-match "^[^-]+-" (symbol-name symbol))
  (intern (substring (symbol-name symbol) (match-end 0))))

(defun nnoo-symbol (backend symbol)
  (intern (format "%s-%s" backend symbol)))

(defun nnoo-define (var map)
  (let* ((backend (nnoo-backend var))
	 (def (assq backend nnoo-definition-alist))
	 (parents (nth 1 def)))
    (unless def
      (error "%s belongs to a backend that hasn't been declared" var))
    (setcar (nthcdr 2 def)
	    (delq (assq var (nth 2 def)) (nth 2 def)))
    (setcar (nthcdr 2 def)
	    (cons (cons var (symbol-value var))
		  (nth 2 def)))
    (while map
      (nconc (assq (nnoo-backend (car map)) parents)
	     (list (list (pop map) var))))))

(defun nnoo-change-server (backend server defs)
  (let* ((bstate (cdr (assq backend nnoo-state-alist)))
	 (current (car bstate))
	 (parents (nnoo-parents backend))
	 (server (if nnoo-parent-backend
		     (format "%s+%s" nnoo-parent-backend server)
		   server))
	 (bvariables (nnoo-variables backend))
	 state def)
    ;; If we don't have a current state, we push an empty state
    ;; onto the alist.
    (unless bstate
      (push (setq bstate (list backend nil))
	    nnoo-state-alist)
      (pop bstate))
    (if (equal server current)
	t
      (nnoo-push-server backend current)
      (setq state (or (cdr (assoc server (cddr bstate)))
		      (nnoo-variables backend)))
      (while state
	(set (caar state) (cdar state))
	(pop state))
      (setcar bstate server)
      (unless (cdr (assoc server (cddr bstate)))
	(while (setq def (pop defs))
	  (unless (assq (car def) bvariables)
	    (nconc bvariables
		   (list (cons (car def) (and (boundp (car def))
					      (symbol-value (car def)))))))
	  (if (equal server "*internal-non-initialized-backend*")
	      (set (car def) (symbol-value (cadr def)))
	    (set (car def) (cadr def)))))
      (while parents
	(nnoo-change-server
	 (caar parents) (format "%s+%s" backend server)
	 (mapcar (lambda (def) (list (car def) (symbol-value (cadr def))))
		 (cdar parents)))
	(pop parents))))
  t)

(defun nnoo-push-server (backend current)
  (let ((bstate (assq backend nnoo-state-alist))
	(defs (nnoo-variables backend)))
    ;; Remove the old definition.
    (setcdr (cdr bstate) (delq (assoc current (cddr bstate)) (cddr bstate)))
    ;; If this is the first time we push the server (i. e., this is
    ;; the nil server), then we update the default values of
    ;; all the variables to reflect the current values.
    (when (equal current "*internal-non-initialized-backend*")
      (let ((defaults (nnoo-variables backend))
	    def)
	(while (setq def (pop defaults))
	  (setcdr def (symbol-value (car def))))))
    (let (state)
      (while defs
	(push (cons (caar defs) (symbol-value (caar defs)))
	      state)
	(pop defs))
      (nconc bstate (list (cons current state))))))

(defsubst nnoo-current-server-p (backend server)
  (equal (nnoo-current-server backend)
	 (if nnoo-parent-backend
	     (format "%s+%s" nnoo-parent-backend server)
	   server)))

(defun nnoo-current-server (backend)
  (nth 1 (assq backend nnoo-state-alist)))

(defun nnoo-close-server (backend &optional server)
  (unless server
    (setq server (nnoo-current-server backend)))
  (when server
    (let* ((bstate (cdr (assq backend nnoo-state-alist)))
	   (defs (assoc server (cdr bstate))))
      (when bstate
	(setcar bstate nil)
	(setcdr bstate (delq defs (cdr bstate)))
	(pop defs)
	(while defs
	  (set (car (pop defs)) nil)))))
  t)

(defun nnoo-close (backend)
  (setq nnoo-state-alist
	(delq (assq backend nnoo-state-alist)
	      nnoo-state-alist))
  t)

(defun nnoo-status-message (backend server)
  (nnheader-get-report backend))

(defun nnoo-server-opened (backend server)
  (and (nnoo-current-server-p backend server)
       nntp-server-buffer
       (buffer-name nntp-server-buffer)))

(defmacro nnoo-define-basics (backend)
  "Define `close-server', `server-opened' and `status-message'."
  `(eval-and-compile
     (nnoo-define-basics-1 ',backend)))

(defun nnoo-define-basics-1 (backend)
  (let ((functions '(close-server server-opened status-message)))
    (while functions
      (eval `(deffoo ,(nnoo-symbol backend (car functions))
		 (&optional server)
	       (,(nnoo-symbol 'nnoo (pop functions)) ',backend server)))))
  (eval `(deffoo ,(nnoo-symbol backend 'open-server)
	     (server &optional defs)
	   (nnoo-change-server ',backend server defs))))

(defmacro nnoo-define-skeleton (backend)
  "Define all required backend functions for BACKEND.
All functions will return nil and report an error."
  `(eval-and-compile
     (nnoo-define-skeleton-1 ',backend)))

(defun nnoo-define-skeleton-1 (backend)
  (let ((functions '(retrieve-headers
		     request-close request-article
		     request-group close-group
		     request-list request-post request-list-newsgroups))
	function fun)
    (while (setq function (pop functions))
      (when (not (fboundp (setq fun (nnoo-symbol backend function))))
	(eval `(deffoo ,fun
		   (&rest args)
		 (nnheader-report ',backend ,(format "%s-%s not implemented"
						     backend function))))))))

(defun nnoo-set (server &rest args)
  (let ((parents (nnoo-parents (car server)))
	(nnoo-parent-backend (car server)))
    (while parents
      (nnoo-change-server (caar parents)
			  (cadr server)
			  (cdar parents))
      (pop parents)))
  (nnoo-change-server (car server)
		      (cadr server) (cddr server))
  (while args
    (set (pop args) (pop args))))

(provide 'nnoo)

;;; nnoo.el ends here
