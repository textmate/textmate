;;; forms-pass.el --- passwd file demo for forms-mode -*- no-byte-compile: t -*-

;; This file is part of GNU Emacs.

;;; Commentary:

;; This demo visits your passwd file.

;;; Code:

;; use yp if present
(or (file-exists-p (setq forms-file "/var/yp/src/passwd"))
    (setq forms-file "/etc/passwd"))

(setq forms-read-only t)		; to make sure
(setq forms-field-sep ":")
(setq forms-number-of-fields 7)

(setq forms-format-list
      (list
       "====== Visiting " forms-file " ======\n\n"
       "User : "	1
       "   Uid: "	3
       "   Gid: "	4
       "\n\n"
       "Name : "	5
       "\n\n"
       "Home : "	6
       "\n\n"
       "Shell: "	7
       "\n"))

;;; forms-pass.el ends here
