;;; ede/dired.el --- EDE extensions to dired.

;; Copyright (C) 1998-2000, 2003, 2009-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.4
;; Keywords: project, make

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
;;
;; This provides a dired interface to EDE, allowing users to modify
;; their project file by adding files (or whatever) directly from a
;; dired buffer.
(eval-when-compile (require 'cl))
(require 'easymenu)
(require 'dired)
(require 'ede)

;;; Code:
(defvar ede-dired-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map ".a" 'ede-dired-add-to-target)
    (define-key map ".t" 'ede-new-target)
    (define-key map ".s" 'ede-speedbar)
    (define-key map ".C" 'ede-compile-project)
    (define-key map ".d" 'ede-make-dist)

    (easy-menu-define
      ede-dired-menu map "EDE Dired Minor Mode Menu"
      '("Project"
        [ "Add files to target" ede-dired-add-to-target (ede-current-project) ]
        ( "Build" :filter ede-build-forms-menu)
        "-"
        [ "Create Project" ede-new (not (ede-current-project)) ]
        [ "Create Target" ede-new-target (ede-current-project) ]
        "-"
        ( "Customize Project" :filter ede-customize-forms-menu )
        [ "View Project Tree" ede-speedbar (ede-current-project) ]
        ))
    map)
  "Keymap used for ede dired minor mode.")

(define-minor-mode ede-dired-minor-mode
  "A minor mode that should only be activated in DIRED buffers.
If ARG is nil or a positive number, force on, if
negative, force off."
  :lighter " EDE" :keymap ede-dired-keymap
  (unless (derived-mode-p 'dired-mode)
    (setq ede-dired-minor-mode nil)
    (error "Not in DIRED mode"))
  (unless (or (ede-directory-project-p default-directory)
              (interactive-p))
    (setq ede-dired-minor-mode nil)))

(defun ede-dired-add-to-target (target)
  "Add a file, or all marked files into a TARGET."
  (interactive (list
		(let ((ede-object (ede-current-project)))
		  (ede-invoke-method 'project-interactive-select-target
				     "Add files to Target: "))))
  (dolist (file (dired-get-marked-files t))
    (project-add-file target file)
    ;; Find the buffer for this files, and set its ede-object
    (if (get-file-buffer file)
	(with-current-buffer (get-file-buffer file)
	  (setq ede-object nil)
	  (setq ede-object (ede-buffer-object (current-buffer)))))))

(provide 'ede/dired)

;;; ede/dired.el ends here
