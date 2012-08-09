;;; mh-inc.el --- MH-E "inc" and separate mail spool handling

;; Copyright (C) 2003-2004, 2006-2012  Free Software Foundation, Inc.

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

;; Support for inc. In addition to reading from the system mailbox,
;; inc can also be used to incorporate mail from multiple spool files
;; into separate folders. See "C-h v mh-inc-spool-list".

;;; Change Log:

;;; Code:

(require 'mh-e)
(mh-require-cl)

(defvar mh-inc-spool-map-help nil
  "Help text for `mh-inc-spool-map'.")

(define-key mh-inc-spool-map "?"
  (lambda ()
    (interactive)
    (if mh-inc-spool-map-help
        (mh-help mh-inc-spool-map-help)
      (mh-ephem-message
       "There are no keys defined yet; customize `mh-inc-spool-list'"))))

;;;###mh-autoload
(defun mh-inc-spool-make ()
  "Make all commands and defines keys for contents of `mh-inc-spool-list'."
  (setq mh-inc-spool-map-help nil)
  (when mh-inc-spool-list
    (loop for elem in mh-inc-spool-list
          do (let ((spool (nth 0 elem))
                   (folder (nth 1 elem))
                   (key (nth 2 elem)))
               (progn
                 (mh-inc-spool-generator folder spool)
                 (mh-inc-spool-def-key key folder))))))

(defalias 'mh-inc-spool-make-no-autoload 'mh-inc-spool-make)

(defun mh-inc-spool-generator (folder spool)
  "Create a command to inc into FOLDER from SPOOL file."
  (let ((folder1 (make-symbol "folder"))
        (spool1 (make-symbol "spool")))
    (set folder1 folder)
    (set spool1 spool)
    (setf (symbol-function (intern (concat "mh-inc-spool-" folder)))
          `(lambda ()
             ,(format "Inc spool file %s into folder %s." spool folder)
             (interactive)
             (mh-inc-folder ,spool1 (concat "+" ,folder1))))))

(defun mh-inc-spool-def-key (key folder)
  "Define a KEY in `mh-inc-spool-map' to inc FOLDER and collect help string."
  (when (not (= 0 key))
    (define-key mh-inc-spool-map (format "%c" key)
       (intern (concat "mh-inc-spool-" folder)))
    (add-to-list 'mh-inc-spool-map-help
                 (concat "[" (char-to-string key) "] inc " folder " folder\n")
                 t)))

(provide 'mh-inc)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-inc.el ends here
