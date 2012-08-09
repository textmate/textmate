;;; mh-identity.el --- multiple identify support for MH-E

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Peter S. Galbraith <psg@debian.org>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

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

;; Multiple identity support for MH-E.

;; Used to easily set different fields such as From and Organization,
;; as well as different signature files.

;; Customize the variable `mh-identity-list' and see the Identity menu
;; in MH-Letter mode. The command `mh-insert-identity' can be used
;; to manually insert an identity.

;;; Change Log:

;;; Code:

(require 'mh-e)

(autoload 'mml-insert-tag "mml")

(defvar mh-identity-pgg-default-user-id nil
  "Holds the GPG key ID to be used by pgg.el.
This is normally set as part of an Identity in
`mh-identity-list'.")
(make-variable-buffer-local 'mh-identity-pgg-default-user-id)

(defvar mh-identity-menu nil
  "The Identity menu.")

(defalias 'mh-identity-make-menu-no-autoload 'mh-identity-make-menu)

;;;###mh-autoload
(defun mh-identity-make-menu ()
  "Build the Identity menu.
This should be called any time `mh-identity-list' or
`mh-auto-fields-list' change.
See `mh-identity-add-menu'."
  (easy-menu-define mh-identity-menu mh-letter-mode-map
    "MH-E identity menu"
    (append
     '("Identity")
     ;; Dynamically render :type corresponding to `mh-identity-list'
     ;; e.g.:
     ;;  ["Home" (mh-insert-identity "Home")
     ;;   :style radio :active (not (equal mh-identity-local "Home"))
     ;;   :selected (equal mh-identity-local "Home")]
     '(["Insert Auto Fields"
        (mh-insert-auto-fields) mh-auto-fields-list]
       "--")

     (mapcar (function
              (lambda (arg)
                `[,arg  (mh-insert-identity ,arg) :style radio
                        :selected (equal mh-identity-local ,arg)]))
             (mapcar 'car mh-identity-list))
     '(["None"
        (mh-insert-identity "None") :style radio
        :selected (not mh-identity-local)]
       "--"
       ["Set Default for Session"
        (setq mh-identity-default mh-identity-local) t]
       ["Save as Default"
        (customize-save-variable 'mh-identity-default mh-identity-local) t]
       ["Customize Identities" (customize-variable 'mh-identity-list) t]
       ))))

;;;###mh-autoload
(defun mh-identity-add-menu ()
  "Add the current Identity menu.
See `mh-identity-make-menu'."
  (if mh-identity-menu
      (easy-menu-add mh-identity-menu)))

(defvar mh-identity-local nil
  "Buffer-local variable that holds the identity currently in use.")
(make-variable-buffer-local 'mh-identity-local)

(defun mh-header-field-delete (field value-only)
  "Delete header FIELD, or only its value if VALUE-ONLY is t.
Return t if anything is deleted."
  (let ((field-colon (if (string-match "^.*:$" field)
                         field
                       (concat field ":"))))
    (when (mh-goto-header-field field-colon)
      (if (not value-only)
          (beginning-of-line)
        (forward-char))
      (delete-region (point)
                     (progn (mh-header-field-end)
                            (if (not value-only) (forward-char 1))
                            (point)))
      t)))

(defvar mh-identity-signature-start nil
  "Marker for the beginning of a signature inserted by `mh-insert-identity'.")
(defvar mh-identity-signature-end nil
  "Marker for the end of a signature inserted by `mh-insert-identity'.")

(defun mh-identity-field-handler (field)
  "Return the handler for header FIELD or nil if none set.
The field name is downcased. If the FIELD begins with the
character \":\", then it must have a special handler defined in
`mh-identity-handlers', else return an error since it is not a
valid header field."
  (or (cdr (mh-assoc-string field mh-identity-handlers t))
      (and (eq (aref field 0) ?:)
           (error "Field %s not found in `mh-identity-handlers'" field))
      (cdr (assoc ":default" mh-identity-handlers))
      'mh-identity-handler-default))

;;;###mh-autoload
(defun mh-insert-identity (identity &optional maybe-insert)
  "Insert fields specified by given IDENTITY.

In a program, do not insert fields if MAYBE-INSERT is non-nil,
`mh-identity-default' is non-nil, and fields have already been
inserted.

See `mh-identity-list'."
  (interactive
   (list (completing-read
          "Identity: "
          (if mh-identity-local
              (cons '("None")
                    (mapcar 'list (mapcar 'car mh-identity-list)))
            (mapcar 'list (mapcar 'car mh-identity-list)))
          nil t)
         nil))

  (when (or (not maybe-insert)
            (and (boundp 'mh-identity-default)
                 mh-identity-default
                 (not mh-identity-local)))
    (save-excursion
      ;;First remove old settings, if any.
      (when mh-identity-local
        (let ((pers-list (cadr (assoc mh-identity-local mh-identity-list))))
          (while pers-list
            (let* ((field (caar pers-list))
                   (handler (mh-identity-field-handler field)))
              (funcall handler field 'remove))
            (setq pers-list (cdr pers-list)))))
      ;; Then insert the replacement
      (when (not (equal "None" identity))
        (let ((pers-list (cadr (assoc identity mh-identity-list))))
          (while pers-list
            (let* ((field (caar pers-list))
                   (value (cdar pers-list))
                   (handler (mh-identity-field-handler field)))
              (funcall handler field 'add value))
            (setq pers-list (cdr pers-list))))))
    ;; Remember what is in use in this buffer
    (if (equal "None" identity)
        (setq mh-identity-local nil)
      (setq mh-identity-local identity))))

;;;###mh-autoload
(defun mh-identity-handler-gpg-identity (field action &optional value)
  "Process header FIELD \":pgg-default-user-id\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is added.
The buffer-local variable `mh-identity-pgg-default-user-id' is set to
VALUE when action 'add is selected."
  (cond
   ((or (equal action 'remove)
        (not value)
        (string= value ""))
    (setq mh-identity-pgg-default-user-id nil))
   ((equal action 'add)
    (setq mh-identity-pgg-default-user-id value))))

;;;###mh-autoload
(defun mh-identity-handler-signature (field action &optional value)
  "Process header FIELD \":signature\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added."
  (cond
   ((equal action 'remove)
    (when (and (markerp mh-identity-signature-start)
               (markerp mh-identity-signature-end))
      (delete-region mh-identity-signature-start
                     mh-identity-signature-end)))
   (t
    ;; Insert "signature". Nil value means to use `mh-signature-file-name'.
    (when (not (mh-signature-separator-p)) ;...unless already present
      (goto-char (point-max))
      (save-restriction
        (narrow-to-region (point) (point))
        (if (null value)
            (mh-insert-signature)
          (mh-insert-signature value))
        (set (make-local-variable 'mh-identity-signature-start)
             (point-min-marker))
        (set-marker-insertion-type mh-identity-signature-start t)
        (set (make-local-variable 'mh-identity-signature-end)
             (point-max-marker)))))))

(defvar mh-identity-attribution-verb-start nil
  "Marker for the beginning of the attribution verb.")
(defvar mh-identity-attribution-verb-end nil
  "Marker for the end of the attribution verb.")

;;;###mh-autoload
(defun mh-identity-handler-attribution-verb (field action &optional value)
  "Process header FIELD \":attribution-verb\".
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added."
  (when (and (markerp mh-identity-attribution-verb-start)
             (markerp mh-identity-attribution-verb-end))
    (delete-region mh-identity-attribution-verb-start
                   mh-identity-attribution-verb-end)
    (goto-char mh-identity-attribution-verb-start)
    (cond
     ((equal action 'remove)              ; Replace with default
      (mh-identity-insert-attribution-verb nil))
     (t                                   ; Insert attribution verb.
      (mh-identity-insert-attribution-verb value)))))

;;;###mh-autoload
(defun mh-identity-insert-attribution-verb (value)
  "Insert VALUE as attribution verb, setting up delimiting markers.
If VALUE is nil, use `mh-extract-from-attribution-verb'."
  (save-restriction
    (narrow-to-region (point) (point))
    (if (null value)
        (insert mh-extract-from-attribution-verb)
      (insert value))
    (set (make-local-variable 'mh-identity-attribution-verb-start)
         (point-min-marker))
    (set-marker-insertion-type mh-identity-attribution-verb-start t)
    (set (make-local-variable 'mh-identity-attribution-verb-end)
         (point-max-marker))))

(defun mh-identity-handler-default (field action top &optional value)
  "Process header FIELD.
The ACTION is one of 'remove or 'add. If TOP is non-nil, add the
field and its VALUE at the top of the header, else add it at the
bottom of the header. If action is 'add, the VALUE is added."
  (let ((field-colon (if (string-match "^.*:$" field)
                         field
                       (concat field ":"))))
    (cond
     ((equal action 'remove)
      (mh-header-field-delete field-colon nil))
     (t
      (cond
       ;; No value, remove field
       ((or (not value)
            (string= value ""))
        (mh-header-field-delete field-colon nil))
       ;; Existing field, replace
       ((mh-header-field-delete field-colon t)
        (insert value))
       ;; Other field, add at end or top
       (t
        (goto-char (point-min))
        (if (not top)
            (mh-goto-header-end 0))
        (insert field-colon " " value "\n")))))))

;;;###mh-autoload
(defun mh-identity-handler-top (field action &optional value)
  "Process header FIELD.
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added. If the field wasn't present, it is added to the top of the
header."
  (mh-identity-handler-default field action t value))

;;;###mh-autoload
(defun mh-identity-handler-bottom (field action &optional value)
  "Process header FIELD.
The ACTION is one of 'remove or 'add. If 'add, the VALUE is
added. If the field wasn't present, it is added to the bottom of
the header."
  (mh-identity-handler-default field action nil value))

(provide 'mh-identity)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-identity.el ends here
