;;; sieve-mode.el --- Sieve code editing commands for Emacs

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Simon Josefsson <simon@josefsson.org>

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

;; This file contain editing mode functions and font-lock support for
;; editing Sieve scripts.  It sets up C-mode with support for
;; sieve-style #-comments and a lightly hacked syntax table.  It was
;; strongly influenced by awk-mode.el.
;;
;; Put something similar to the following in your .emacs to use this file:
;;
;; (load "~/lisp/sieve")
;; (setq auto-mode-alist (cons '("\\.siv\\'" . sieve-mode) auto-mode-alist))
;;
;; References:
;;
;; RFC 3028,
;; "Sieve: A Mail Filtering Language",
;; by Tim Showalter.
;;
;; Release history:
;;
;; 2001-03-02 version 1.0 posted to gnu.emacs.sources
;;            version 1.1 change file extension into ".siv" (official one)
;;                        added keymap and menubar to hook into sieve-manage
;; 2001-10-31 version 1.2 committed to Oort Gnus

;;; Code:

(autoload 'sieve-manage "sieve")
(autoload 'sieve-upload "sieve")
(eval-when-compile
  (require 'font-lock))

(defgroup sieve nil
  "Sieve."
  :group 'languages)

(defcustom sieve-mode-hook nil
  "Hook run in sieve mode buffers."
  :group 'sieve
  :type 'hook)

;; Font-lock

(defvar sieve-control-commands-face 'sieve-control-commands
  "Face name used for Sieve Control Commands.")

(defface sieve-control-commands
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Face used for Sieve Control Commands."
  :group 'sieve)
;; backward-compatibility alias
(put 'sieve-control-commands-face 'face-alias 'sieve-control-commands)
(put 'sieve-control-commands-face 'obsolete-face "22.1")

(defvar sieve-action-commands-face 'sieve-action-commands
  "Face name used for Sieve Action Commands.")

(defface sieve-action-commands
  '((((type tty) (class color)) (:foreground "blue" :weight bold))
    (((class color) (background light)) (:foreground "Blue"))
    (((class color) (background dark)) (:foreground "LightSkyBlue"))
    (t (:inverse-video t :bold t)))
  "Face used for Sieve Action Commands."
  :group 'sieve)
;; backward-compatibility alias
(put 'sieve-action-commands-face 'face-alias 'sieve-action-commands)
(put 'sieve-action-commands-face 'obsolete-face "22.1")

(defvar sieve-test-commands-face 'sieve-test-commands
  "Face name used for Sieve Test Commands.")

(defface sieve-test-commands
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t))
    (((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:bold t :underline t)))
  "Face used for Sieve Test Commands."
  :group 'sieve)
;; backward-compatibility alias
(put 'sieve-test-commands-face 'face-alias 'sieve-test-commands)
(put 'sieve-test-commands-face 'obsolete-face "22.1")

(defvar sieve-tagged-arguments-face 'sieve-tagged-arguments
  "Face name used for Sieve Tagged Arguments.")

(defface sieve-tagged-arguments
  '((((type tty) (class color)) (:foreground "cyan" :weight bold))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Purple"))
    (((class color) (background dark)) (:foreground "Cyan"))
    (t (:bold t)))
  "Face used for Sieve Tagged Arguments."
  :group 'sieve)
;; backward-compatibility alias
(put 'sieve-tagged-arguments-face 'face-alias 'sieve-tagged-arguments)
(put 'sieve-tagged-arguments-face 'obsolete-face "22.1")


(defconst sieve-font-lock-keywords
  (eval-when-compile
    (list
     ;; control commands
     (cons (regexp-opt '("require" "if" "else" "elsif" "stop"))
	   'sieve-control-commands-face)
     ;; action commands
     (cons (regexp-opt '("fileinto" "redirect" "reject" "keep" "discard"))
	   'sieve-action-commands-face)
     ;; test commands
     (cons (regexp-opt '("address" "allof" "anyof" "exists" "false"
			 "true" "header" "not" "size" "envelope"))
	   'sieve-test-commands-face)
     (cons "\\Sw+:\\sw+"
	   'sieve-tagged-arguments-face))))

;; Syntax table

(defvar sieve-mode-syntax-table nil
  "Syntax table in use in sieve-mode buffers.")

(if sieve-mode-syntax-table
    ()
  (setq sieve-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" sieve-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " sieve-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " sieve-mode-syntax-table)
  (modify-syntax-entry ?\# "<   " sieve-mode-syntax-table)
  (modify-syntax-entry ?/ "." sieve-mode-syntax-table)
  (modify-syntax-entry ?* "." sieve-mode-syntax-table)
  (modify-syntax-entry ?+ "." sieve-mode-syntax-table)
  (modify-syntax-entry ?- "." sieve-mode-syntax-table)
  (modify-syntax-entry ?= "." sieve-mode-syntax-table)
  (modify-syntax-entry ?% "." sieve-mode-syntax-table)
  (modify-syntax-entry ?< "." sieve-mode-syntax-table)
  (modify-syntax-entry ?> "." sieve-mode-syntax-table)
  (modify-syntax-entry ?& "." sieve-mode-syntax-table)
  (modify-syntax-entry ?| "." sieve-mode-syntax-table)
  (modify-syntax-entry ?_ "_" sieve-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" sieve-mode-syntax-table))

;; Key map definition

(defvar sieve-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l" 'sieve-upload)
    (define-key map "\C-c\C-c" 'sieve-upload-and-bury)
    (define-key map "\C-c\C-m" 'sieve-manage)
    map)
  "Key map used in sieve mode.")

;; Menu definition

(defvar sieve-mode-menu nil
  "Menubar used in sieve mode.")

;; Code for Sieve editing mode.
(autoload 'easy-menu-add-item "easymenu")

;;;###autoload
(define-derived-mode sieve-mode c-mode "Sieve"
  "Major mode for editing Sieve code.
This is much like C mode except for the syntax of comments.  Its keymap
inherits from C mode's and it has the same variables for customizing
indentation.  It has its own abbrev table and its own syntax table.

Turning on Sieve mode runs `sieve-mode-hook'."
  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  ;;(set (make-local-variable 'comment-start-skip) "\\(^\\|\\s-\\);?#+ *")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (unless (featurep 'xemacs)
    (set (make-local-variable 'font-lock-defaults)
	 '(sieve-font-lock-keywords nil nil ((?_ . "w")))))
  (easy-menu-add-item nil nil sieve-mode-menu))

;; Menu

(easy-menu-define sieve-mode-menu sieve-mode-map
  "Sieve Menu."
  '("Sieve"
    ["Upload script" sieve-upload t]
    ["Manage scripts on server" sieve-manage t]))

(provide 'sieve-mode)

;; sieve-mode.el ends here
