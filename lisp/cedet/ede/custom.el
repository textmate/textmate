;;; ede/custom.el --- customization of EDE projects.

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>

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
;; Customization commands/hooks for EDE.
;;
;; EIEIO supports customizing objects, and EDE uses this to allow
;; users to change basic settings in their projects.
;;

;;; Code:
;;; Customization
;;
;; Routines for customizing projects and targets.

(require 'ede)
(eval-when-compile (require 'eieio-custom))

(defvar eieio-ede-old-variables nil
  "The old variables for a project.")

;;; Customization Commands
;;
;; These commands initialize customization of EDE control objects.

;;;###autoload
(defun ede-customize-project ()
  "Edit fields of the current project through EIEIO & Custom."
  (interactive)
  (require 'eieio-custom)
  (let* ((ov (oref (ede-current-project) local-variables))
	 (cp (ede-current-project)))
    (ede-customize cp)
    (make-local-variable 'eieio-ede-old-variables)
    (setq eieio-ede-old-variables ov)))

;;;###autoload
(defalias 'customize-project 'ede-customize-project)

;;;###autoload
(defun ede-customize-current-target()
  "Edit fields of the current target through EIEIO & Custom."
  (interactive)
  (require 'eieio-custom)
  (if (not (obj-of-class-p ede-object ede-target))
      (error "Current file is not part of a target"))
  (ede-customize-target ede-object))

;;;###autoload
(defalias 'customize-target 'ede-customize-current-target)

(defun ede-customize-target (obj)
  "Edit fields of the current target through EIEIO & Custom.
OBJ is the target object to customize."
  (require 'eieio-custom)
  (if (and obj (not (obj-of-class-p obj ede-target)))
      (error "No logical target to customize"))
  (ede-customize obj))

(defmethod ede-customize ((proj ede-project))
  "Customize the EDE project PROJ."
  (eieio-customize-object proj 'default))

(defmethod ede-customize ((target ede-target))
  "Customize the EDE TARGET."
  (eieio-customize-object target 'default))

;;; Target Sorting
;;
;; Target order can be important, but custom doesn't support a way
;; to resort items in a list.  This function by David Engster allows
;; targets to be re-arranged.

(defvar ede-project-sort-targets-order nil
  "Variable for tracking target order in `ede-project-sort-targets'.")

;;;###autoload
(defun ede-project-sort-targets ()
  "Create a custom-like buffer for sorting targets of current project."
  (interactive)
  (let ((proj (ede-current-project))
        (count 1)
        current order)
    (switch-to-buffer (get-buffer-create "*EDE sort targets*"))
    (erase-buffer)
    (setq ede-object-project proj)
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
                             (let ((targets (oref ede-object-project targets))
                                   cur newtargets)
                               (while (setq cur (pop ede-project-sort-targets-order))
                                 (setq newtargets (append newtargets
                                                          (list (nth cur targets)))))
                               (oset ede-object-project targets newtargets))
                             (ede-commit-project ede-object-project)
                             (kill-buffer))
                   " Accept ")
    (widget-insert "   ")
    (widget-create 'push-button
                   :notify (lambda (&rest ignore)
			     (kill-buffer))
                   " Cancel ")
    (widget-insert "\n\n")
    (setq ede-project-sort-targets-order nil)
    (mapc (lambda (x)
            (add-to-ordered-list
             'ede-project-sort-targets-order
             x x))
          (number-sequence 0 (1- (length (oref proj targets)))))
    (ede-project-sort-targets-list)
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))))

(defun ede-project-sort-targets-list ()
  "Sort the target list while using `ede-project-sort-targets'."
  (save-excursion
    (let ((count 0)
          (targets (oref ede-object-project targets))
          (inhibit-read-only t)
          (inhibit-modification-hooks t))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point) (point-max))
      (while (< count (length targets))
        (if (> count 0)
            (widget-create 'push-button
                           :notify `(lambda (&rest ignore)
                                      (let ((cur ede-project-sort-targets-order))
                                        (add-to-ordered-list
                                         'ede-project-sort-targets-order
                                         (nth ,count cur)
                                         (1- ,count))
                                        (add-to-ordered-list
                                         'ede-project-sort-targets-order
                                         (nth (1- ,count) cur) ,count))
                                      (ede-project-sort-targets-list))
                           " Up ")
          (widget-insert "      "))
        (if (< count (1- (length targets)))
            (widget-create 'push-button
                           :notify `(lambda (&rest ignore)
                                      (let ((cur ede-project-sort-targets-order))
                                        (add-to-ordered-list
                                         'ede-project-sort-targets-order
                                         (nth ,count cur) (1+ ,count))
                                        (add-to-ordered-list
                                         'ede-project-sort-targets-order
                                         (nth (1+ ,count) cur) ,count))
                                      (ede-project-sort-targets-list))
                           " Down ")
          (widget-insert "        "))
        (widget-insert (concat " " (number-to-string (1+ count)) ".:   "
                               (oref (nth (nth count ede-project-sort-targets-order)
                                          targets) name) "\n"))
        (setq count (1+ count))))))

;;; Customization hooks
;;
;; These hooks are used when finishing up a customization.
(defmethod eieio-done-customizing ((proj ede-project))
  "Call this when a user finishes customizing PROJ."
  (let ((ov eieio-ede-old-variables)
	(nv (oref proj local-variables)))
    (setq eieio-ede-old-variables nil)
    (while ov
      (if (not (assoc (car (car ov)) nv))
	  (save-excursion
	    (mapc (lambda (b)
		    (set-buffer b)
		    (kill-local-variable (car (car ov))))
		  (ede-project-buffers proj))))
      (setq ov (cdr ov)))
    (mapc (lambda (b) (ede-set-project-variables proj b))
	  (ede-project-buffers proj))))

;; These two methods should be implemented by subclasses of
;; project and targets in order to account for user specified
;; changes.
(defmethod eieio-done-customizing ((target ede-target))
  "Call this when a user finishes customizing TARGET."
  nil)

(defmethod ede-commit-project ((proj ede-project))
  "Commit any change to PROJ to its file."
  nil
  )

(provide 'ede/custom)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "ede/custom"
;; End:

;;; ede/custom.el ends here
