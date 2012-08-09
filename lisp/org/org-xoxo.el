;;; org-xoxo.el --- XOXO export for Org-mode

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

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
;; XOXO export

;;; Code:

(require 'org-exp)

(defvar org-export-xoxo-final-hook nil
  "Hook run after XOXO export, in the new buffer.")

(defun org-export-as-xoxo-insert-into (buffer &rest output)
  (with-current-buffer buffer
    (apply 'insert output)))
(put 'org-export-as-xoxo-insert-into 'lisp-indent-function 1)

;;;###autoload
(defun org-export-as-xoxo (&optional buffer)
  "Export the org buffer as XOXO.
The XOXO buffer is named *xoxo-<source buffer name>*"
  (interactive (list (current-buffer)))
  (run-hooks 'org-export-first-hook)
  ;; A quickie abstraction

  ;; Output everything as XOXO
  (with-current-buffer (get-buffer buffer)
    (let* ((pos (point))
	   (opt-plist (org-combine-plists (org-default-export-plist)
					(org-infile-export-plist)))
	   (filename (concat (file-name-as-directory
			      (org-export-directory :xoxo opt-plist))
			     (file-name-sans-extension
			      (file-name-nondirectory buffer-file-name))
			     ".html"))
	   (out (find-file-noselect filename))
	   (last-level 1)
	   (hanging-li nil))
      (goto-char (point-min))  ;; CD:  beginning-of-buffer is not allowed.
      ;; Check the output buffer is empty.
      (with-current-buffer out (erase-buffer))
      ;; Kick off the output
      (org-export-as-xoxo-insert-into out "<ol class='xoxo'>\n")
      (while (re-search-forward "^\\(\\*+\\)[ \t]+\\(.+\\)" (point-max) 't)
	(let* ((hd (match-string-no-properties 1))
	       (level (length hd))
	       (text (concat
		      (match-string-no-properties 2)
		      (save-excursion
			(goto-char (match-end 0))
			(let ((str ""))
			  (catch 'loop
			    (while 't
			      (forward-line)
			      (if (looking-at "^[ \t]\\(.*\\)")
				  (setq str (concat str (match-string-no-properties 1)))
				(throw 'loop str)))))))))

	  ;; Handle level rendering
	  (cond
	   ((> level last-level)
	    (org-export-as-xoxo-insert-into out "\n<ol>\n"))

	   ((< level last-level)
	    (dotimes (- (- last-level level) 1)
	      (if hanging-li
		  (org-export-as-xoxo-insert-into out "</li>\n"))
	      (org-export-as-xoxo-insert-into out "</ol>\n"))
	    (when hanging-li
	      (org-export-as-xoxo-insert-into out "</li>\n")
	      (setq hanging-li nil)))

	   ((equal level last-level)
	    (if hanging-li
		(org-export-as-xoxo-insert-into out "</li>\n")))
	   )

	  (setq last-level level)

	  ;; And output the new li
	  (setq hanging-li 't)
	  (if (equal ?+ (elt text 0))
	      (org-export-as-xoxo-insert-into out "<li class='" (substring text 1) "'>")
	    (org-export-as-xoxo-insert-into out "<li>" text))))

      ;; Finally finish off the ol
      (dotimes (- last-level 1)
	(if hanging-li
	    (org-export-as-xoxo-insert-into out "</li>\n"))
	(org-export-as-xoxo-insert-into out "</ol>\n"))

      (goto-char pos)
      ;; Finish the buffer off and clean it up.
      (switch-to-buffer-other-window out)
      (indent-region (point-min) (point-max) nil)
      (run-hooks 'org-export-xoxo-final-hook)
      (save-buffer)
      (goto-char (point-min))
      )))

(provide 'org-xoxo)

;;; org-xoxo.el ends here
