;;; calc-undo.el --- undo functions for Calc

;; Copyright (C) 1990-1993, 2001-2012 Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

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

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)

;;; Undo.

;;;###autoload
(defun calc-undo (n)
  (interactive "p")
  (when calc-executing-macro
    (error "Use C-x e, not X, to run a keyboard macro that uses Undo"))
  (if (<= n 0)
      (if (< n 0)
	  (calc-redo (- n))
	(calc-last-args 1))
    (calc-wrapper
     (when (null (nthcdr (1- n) calc-undo-list))
       (error "No further undo information available"))
     (setq calc-undo-list
	   (prog1
	       (nthcdr n calc-undo-list)
	     (let ((saved-stack-top calc-stack-top))
	       (let ((calc-stack-top 0))
		 (calc-handle-undos calc-undo-list n))
	       (setq calc-stack-top saved-stack-top))))
     (message "Undo!"))))

(defun calc-handle-undos (cl n)
  (when (> n 0)
    (let ((old-redo calc-redo-list))
      (setq calc-undo-list nil)
      (calc-handle-undo (car cl))
      (setq calc-redo-list (append calc-undo-list old-redo)))
    (calc-handle-undos (cdr cl) (1- n))))

(defun calc-handle-undo (list)
  (when list
       (let ((action (car list)))
	 (cond
	  ((eq (car action) 'push)
	   (calc-pop-stack 1 (nth 1 action) t))
	  ((eq (car action) 'pop)
	   (calc-push-list (nth 2 action) (nth 1 action)))
	  ((eq (car action) 'set)
	   (calc-record-undo (list 'set (nth 1 action)
				   (symbol-value (nth 1 action))))
	   (set (nth 1 action) (nth 2 action)))
	  ((eq (car action) 'store)
	   (let ((v (intern (nth 1 action))))
	     (calc-record-undo (list 'store (nth 1 action)
				     (and (boundp v) (symbol-value v))))
	     (if (y-or-n-p (format "Un-store variable %s? " 
                                   (calc-var-name (nth 1 action))))
		 (progn
		   (if (nth 2 action)
		       (set v (nth 2 action))
		     (makunbound v))
		   (calc-refresh-evaltos v)))))
	  ((eq (car action) 'eval)
	   (calc-record-undo (append (list 'eval (nth 2 action) (nth 1 action))
				     (cdr (cdr (cdr action)))))
	   (apply (nth 1 action) (cdr (cdr (cdr action))))))
	 (calc-handle-undo (cdr list)))))

(defun calc-redo (n)
  (interactive "p")
  (when calc-executing-macro
    (error "Use C-x e, not X, to run a keyboard macro that uses Redo"))
  (if (<= n 0)
      (calc-undo (- n))
    (calc-wrapper
     (when (null (nthcdr (1- n) calc-redo-list))
       (error "Unable to redo"))
     (setq calc-redo-list
	   (prog1
	       (nthcdr n calc-redo-list)
	     (let ((saved-stack-top calc-stack-top))
	       (let ((calc-stack-top 0))
		 (calc-handle-redos calc-redo-list n))
	       (setq calc-stack-top saved-stack-top))))
     (message "Redo!"))))

(defun calc-handle-redos (cl n)
  (when (> n 0)
    (let ((old-undo calc-undo-list))
      (setq calc-undo-list nil)
      (calc-handle-undo (car cl))
      (setq calc-undo-list (append calc-undo-list old-undo)))
    (calc-handle-redos (cdr cl) (1- n))))

(defun calc-last-args (n)
  (interactive "p")
  (when calc-executing-macro
    (error "Use C-x e, not X, to run a keyboard macro that uses last-args"))
  (calc-wrapper
   (let ((urec (calc-find-last-x calc-undo-list n)))
     (if urec
	 (calc-handle-last-x urec)
       (error "Not enough undo information available")))))

(defun calc-handle-last-x (list)
  (when list
    (let ((action (car list)))
      (if (eq (car action) 'pop)
	  (calc-pop-push-record-list 0 "larg"
				     (delq 'top-of-stack (nth 2 action))))
      (calc-handle-last-x (cdr list)))))

(defun calc-find-last-x (ul n)
  (when ul
    (if (calc-undo-does-pushes (car ul))
	(if (<= n 1)
	    (car ul)
	  (calc-find-last-x (cdr ul) (1- n)))
      (calc-find-last-x (cdr ul) n))))

(defun calc-undo-does-pushes (list)
  (and list
       (or (eq (car (car list)) 'pop)
	   (calc-undo-does-pushes (cdr list)))))

(provide 'calc-undo)

;;; calc-undo.el ends here
