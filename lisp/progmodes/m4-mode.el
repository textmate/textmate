;;; m4-mode.el --- m4 code editing commands for Emacs

;; Copyright (C) 1996-1997, 2001-2012  Free Software Foundation, Inc.

;; Author: Andrew Csillag <drew_csillag@geocities.com>
;; Maintainer: Andrew Csillag <drew_csillag@geocities.com>
;; Keywords: languages, faces

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

;; A smart editing mode for m4 macro definitions.  It seems to have most of the
;; syntax right (sexp motion commands work, but function motion commands don't).
;; It also sets the font-lock syntax stuff for colorization

;; To Do's:

;; * want to make m4-m4-(buffer|region) look sorta like M-x compile look&feel ?
;; * sexp motion commands don't seem to work right

;;; Thanks:
;;;         to Akim Demaille and Terry Jones for the bug reports
;;;         to Simon Marshall for the regexp tip
;;;         to Martin Buchholz for some general fixes

;;; Code:

(defgroup m4 nil
  "m4 code editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "m4-"
  :group 'languages)

(defcustom m4-program
  (cond
   ((file-exists-p "/usr/local/bin/m4") "/usr/local/bin/m4")
   ((file-exists-p "/usr/bin/m4") "/usr/bin/m4")
   ((file-exists-p "/bin/m4") "/bin/m4")
   ((file-exists-p "/usr/ccs/bin/m4") "/usr/ccs/bin/m4")
   ( t "m4")
   )
  "File name of the m4 executable."
  :type 'file
  :group 'm4)

;;options to m4
(defcustom m4-program-options nil
  "Options to pass to `m4-program'."
  :type '(repeat string)
  :group 'm4)

;;to use --prefix-builtins, you can use
;;(defconst m4-program-options '("-P"))
;;or
;;(defconst m4-program-options '("--prefix-builtins"))

(defvar m4-font-lock-keywords
  `(
    ("\\(\\b\\(m4_\\)?dnl\\b\\|^\\#\\).*$" . font-lock-comment-face)
;    ("\\(\\bdnl\\b\\|\\bm4_dnl\\b\\|^\\#\\).*$" . font-lock-comment-face)
    ("\\$[*#@0-9]" . font-lock-variable-name-face)
    ("\\\$\\\@" . font-lock-variable-name-face)
    ("\\\$\\\*" . font-lock-variable-name-face)
    ("\\b\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|gnu\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|un\\(d\\(efine\\|ivert\\)\\|ix\\)\\)\\b" . font-lock-keyword-face)
    ("\\b\\(m4_\\(builtin\\|change\\(com\\|quote\\|word\\)\\|d\\(e\\(bug\\(file\\|mode\\)\\|cr\\|f\\(ine\\|n\\)\\)\\|iv\\(ert\\|num\\)\\|nl\\|umpdef\\)\\|e\\(rrprint\\|syscmd\\|val\\)\\|f\\(ile\\|ormat\\)\\|i\\(f\\(def\\|else\\)\\|n\\(c\\(lude\\|r\\)\\|d\\(ex\\|ir\\)\\)\\)\\|l\\(en\\|ine\\)\\|m\\(4\\(_undefine\\|exit\\|wrap\\)\\|aketemp\\)\\|p\\(atsubst\\|opdef\\|ushdef\\)\\|regexp\\|s\\(hift\\|include\\|ubstr\\|ys\\(cmd\\|val\\)\\)\\|tra\\(ceo\\(ff\\|n\\)\\|nslit\\)\\|undivert\\)\\)\\b" . font-lock-keyword-face))
  "Default font-lock-keywords for `m4 mode'.")

(defcustom m4-mode-hook nil
  "*Hook called by `m4-mode'."
  :type 'hook
  :group 'm4)

;;this may still need some work
(defvar m4-mode-syntax-table nil
  "Syntax table used while in `m4-mode'.")
(setq m4-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?` "('" m4-mode-syntax-table)
(modify-syntax-entry ?' ")`" m4-mode-syntax-table)
(modify-syntax-entry ?# "<\n" m4-mode-syntax-table)
(modify-syntax-entry ?\n ">#" m4-mode-syntax-table)
(modify-syntax-entry ?{  "_" m4-mode-syntax-table)
(modify-syntax-entry ?}  "_" m4-mode-syntax-table)
(modify-syntax-entry ?*  "w" m4-mode-syntax-table)
(modify-syntax-entry ?_  "w" m4-mode-syntax-table)
(modify-syntax-entry ?\"  "w" m4-mode-syntax-table)
(modify-syntax-entry ?\"  "w" m4-mode-syntax-table)

(defvar m4-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'm4-m4-buffer)
    (define-key map "\C-c\C-r" 'm4-m4-region)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map [menu-bar m4-mode] (cons "M4" menu-map))
    (define-key menu-map [m4c]
      '(menu-item "Comment Region" comment-region
		  :help "Comment Region"))
    (define-key menu-map [m4b]
      '(menu-item "M4 Buffer" m4-m4-buffer
		  :help "Send contents of the current buffer to m4"))
    (define-key menu-map [m4r]
      '(menu-item "M4 Region" m4-m4-region
		  :help "Send contents of the current region to m4"))
    map))

(defvar m4-mode-abbrev-table nil
  "Abbrev table used while in `m4-mode'.")

(unless m4-mode-abbrev-table
  (define-abbrev-table 'm4-mode-abbrev-table ()))

(defun m4-m4-buffer ()
  "Send contents of the current buffer to m4."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (mapconcat 'identity (cons m4-program m4-program-options) "\s")
   "*m4-output*" nil)
  (switch-to-buffer-other-window "*m4-output*"))

(defun m4-m4-region ()
  "Send contents of the current region to m4."
  (interactive)
  (shell-command-on-region
   (point) (mark)
   (mapconcat 'identity (cons m4-program m4-program-options) "\s")
   "*m4-output*" nil)
  (switch-to-buffer-other-window "*m4-output*"))

;;;###autoload
(define-derived-mode m4-mode prog-mode "m4"
  "A major mode to edit m4 macro files."
  :abbrev-table m4-mode-abbrev-table
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults) '(m4-font-lock-keywords nil)))

(provide 'm4-mode)
;;stuff to play with for debugging
;(char-to-string (char-syntax ?`))

;;;how I generate the nasty looking regexps at the top
;;;(make-regexp '("builtin" "changecom" "changequote" "changeword" "debugfile"
;;;		  "debugmode" "decr" "define" "defn" "divert" "divnum" "dnl"
;;;		  "dumpdef" "errprint" "esyscmd" "eval" "file" "format" "gnu"
;;;		  "ifdef" "ifelse" "include" "incr" "index" "indir" "len" "line"
;;;		  "m4exit" "m4wrap" "maketemp" "patsubst" "popdef" "pushdef" "regexp"
;;;		  "shift" "sinclude" "substr" "syscmd" "sysval" "traceoff" "traceon"
;;;		  "translit" "undefine" "undivert" "unix"))
;;;(make-regexp '("m4_builtin" "m4_changecom" "m4_changequote" "m4_changeword"
;;;		  "m4_debugfile" "m4_debugmode" "m4_decr" "m4_define" "m4_defn"
;;;		  "m4_divert" "m4_divnum" "m4_dnl" "m4_dumpdef" "m4_errprint"
;;;		  "m4_esyscmd" "m4_eval" "m4_file" "m4_format" "m4_ifdef" "m4_ifelse"
;;;		  "m4_include" "m4_incr" "m4_index" "m4_indir" "m4_len" "m4_line"
;;;		  "m4_m4exit" "m4_m4wrap" "m4_maketemp" "m4_patsubst" "m4_popdef"
;;;		  "m4_pushdef" "m4_regexp" "m4_shift" "m4_sinclude" "m4_substr"
;;;		  "m4_syscmd" "m4_sysval" "m4_traceoff" "m4_traceon" "m4_translit"
;;;		  "m4_m4_undefine" "m4_undivert"))

;;; m4-mode.el ends here
