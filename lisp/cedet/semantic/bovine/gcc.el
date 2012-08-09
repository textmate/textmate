;;; semantic/bovine/gcc.el --- gcc querying special code for the C parser

;; Copyright (C) 2008-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <eric@siege-engine.com>

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
;; GCC stores things in special places.  These functions will query
;; GCC, and set up the preprocessor and include paths.

(require 'semantic/dep)

(defvar semantic-lex-c-preprocessor-symbol-file)
(defvar semantic-lex-c-preprocessor-symbol-map)
(declare-function semantic-c-reset-preprocessor-symbol-map "semantic/bovine/c")

;;; Code:

(defun semantic-gcc-query (gcc-cmd &rest gcc-options)
  "Return program output to both standard output and standard error.
GCC-CMD is the program to execute and GCC-OPTIONS are the options
to give to the program."
  ;; $ gcc -v
  ;;
  (let ((buff (get-buffer-create " *gcc-query*"))
        (old-lc-messages (getenv "LC_ALL")))
    (with-current-buffer buff
      (erase-buffer)
      (setenv "LC_ALL" "C")
      (condition-case nil
          (apply 'call-process gcc-cmd nil (cons buff t) nil gcc-options)
        (error ;; Some bogus directory for the first time perhaps?
         (let ((default-directory (expand-file-name "~/")))
           (condition-case nil
               (apply 'call-process gcc-cmd nil (cons buff t) nil gcc-options)
             (error ;; gcc doesn't exist???
              nil)))))
      (setenv "LC_ALL" old-lc-messages)
      (prog1
          (buffer-string)
        (kill-buffer buff)
        )
      )))

;;(semantic-gcc-get-include-paths "c")
;;(semantic-gcc-get-include-paths "c++")
(defun semantic-gcc-get-include-paths (lang)
  "Return include paths as gcc uses them for language LANG."
  (let* ((gcc-cmd (cond
                   ((string= lang "c") "gcc")
                   ((string= lang "c++") "c++")
                   (t (if (stringp lang)
                          (error "Unknown lang: %s" lang)
                        (error "LANG=%S, should be a string" lang)))))
         (gcc-output (semantic-gcc-query gcc-cmd "-v" "-E" "-x" lang null-device))
         (lines (split-string gcc-output "\n"))
         (include-marks 0)
         (inc-mark "#include ")
         (inc-mark-len (length "#include "))
         inc-path)
    ;;(message "gcc-output=%s" gcc-output)
    (dolist (line lines)
      (when (> (length line) 1)
        (if (= 0 include-marks)
            (when (and (> (length line) inc-mark-len)
                       (string= inc-mark (substring line 0 inc-mark-len)))
              (setq include-marks (1+ include-marks)))
          (let ((chars (append line nil)))
            (when (= 32 (nth 0 chars))
              (let ((path (substring line 1)))
                (when (file-accessible-directory-p path)
                  (when (if (memq system-type '(windows-nt))
                            (/= ?/ (nth 1 chars))
                          (= ?/ (nth 1 chars)))
                    (add-to-list 'inc-path
                                 (expand-file-name (substring line 1))
                                 t)))))))))
    inc-path))


(defun semantic-cpp-defs (str)
  "Convert CPP output STR into a list of cons cells with defines for C++."
  (let ((lines (split-string str "\n"))
        (lst nil))
    (dolist (L lines)
      (let ((dat (split-string L)))
        (when (= (length dat) 3)
          (add-to-list 'lst (cons (nth 1 dat) (nth 2 dat))))))
    lst))

(defun semantic-gcc-fields (str)
  "Convert GCC output STR into an alist of fields."
  (let ((fields nil)
        (lines (split-string str "\n"))
        )
    (dolist (L lines)
      ;; For any line, what do we do with it?
      (cond ((or (string-match "Configured with\\(:\\)" L)
                 (string-match "\\(:\\)\\s-*[^ ]*configure " L))
             (let* ((parts (substring L (match-end 1)))
                    (opts (split-string parts " " t))
                    )
               (dolist (O (cdr opts))
                 (let* ((data (split-string O "="))
                        (sym (intern (car data)))
                        (val (car (cdr data))))
                   (push (cons sym val) fields)
                   ))
               ))
            ((string-match "gcc[ -][vV]ersion" L)
             (let* ((vline (substring L (match-end 0)))
                    (parts (split-string vline " ")))
               (push (cons 'version (nth 1 parts)) fields)))
            ((string-match "Target: " L)
             (let ((parts (split-string L " ")))
               (push (cons 'target (nth 1 parts)) fields)))
            ))
    fields))

(defvar semantic-gcc-setup-data nil
  "The GCC setup data.
This is setup by `semantic-gcc-setup'.
This is an alist, and should include keys of:
  'version  - the version of gcc
  '--host   - the host symbol (used in include directories)
  '--prefix - where GCC was installed.
It should also include other symbols GCC was compiled with.")

;;;###autoload
(defun semantic-gcc-setup ()
  "Setup Semantic C/C++ parsing based on GCC output."
  (interactive)
  (let* ((fields (or semantic-gcc-setup-data
                     (semantic-gcc-fields (semantic-gcc-query "gcc" "-v"))))
         (defines (semantic-cpp-defs (semantic-gcc-query "cpp" "-E" "-dM" "-x" "c++" null-device)))
         (ver (cdr (assoc 'version fields)))
         (host (or (cdr (assoc 'target fields))
                   (cdr (assoc '--target fields))
                   (cdr (assoc '--host fields))))
         (prefix (cdr (assoc '--prefix fields)))
         ;; gcc output supplied paths
         (c-include-path (semantic-gcc-get-include-paths "c"))
         (c++-include-path (semantic-gcc-get-include-paths "c++")))
    ;; Remember so we don't have to call GCC twice.
    (setq semantic-gcc-setup-data fields)
    (unless c-include-path
      ;; Fallback to guesses
      (let* ( ;; gcc include dirs
             (gcc-exe (locate-file "gcc" exec-path exec-suffixes 'executable))
             (gcc-root (expand-file-name ".." (file-name-directory gcc-exe)))
             (gcc-include (expand-file-name "include" gcc-root))
             (gcc-include-c++ (expand-file-name "c++" gcc-include))
             (gcc-include-c++-ver (expand-file-name ver gcc-include-c++))
             (gcc-include-c++-ver-host (expand-file-name host gcc-include-c++-ver)))
        (setq c-include-path
              ;; Replace cl-function remove-if-not.
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include" gcc-include))))
        (setq c++-include-path
              (delq nil (mapcar (lambda (d)
                                  (if (file-accessible-directory-p d) d))
                                (list "/usr/include"
                                      gcc-include
                                      gcc-include-c++
                                      gcc-include-c++-ver
                                      gcc-include-c++-ver-host))))))

    ;;; Fix-me: I think this part might have been a misunderstanding, but I am not sure.
    ;; If this option is specified, try it both with and without prefix, and with and without host
    ;; (if (assoc '--with-gxx-include-dir fields)
    ;;     (let ((gxx-include-dir (cdr (assoc '--with-gxx-include-dir fields))))
    ;;       (nconc try-paths (list gxx-include-dir
    ;;                              (concat prefix gxx-include-dir)
    ;;                              (concat gxx-include-dir "/" host)
    ;;                              (concat prefix gxx-include-dir "/" host)))))

    ;; Now setup include paths etc
    (dolist (D (semantic-gcc-get-include-paths "c"))
      (semantic-add-system-include D 'c-mode))
    (dolist (D (semantic-gcc-get-include-paths "c++"))
      (semantic-add-system-include D 'c++-mode)
      (let ((cppconfig (concat D "/bits/c++config.h")))
        ;; Presumably there will be only one of these files in the try-paths list...
        (when (file-readable-p cppconfig)
          ;; Add it to the symbol file
          (if (boundp 'semantic-lex-c-preprocessor-symbol-file)
              ;; Add to the core macro header list
              (add-to-list 'semantic-lex-c-preprocessor-symbol-file cppconfig)
            ;; Setup the core macro header
            (setq semantic-lex-c-preprocessor-symbol-file (list cppconfig)))
          )))
    (if (not (boundp 'semantic-lex-c-preprocessor-symbol-map))
        (setq semantic-lex-c-preprocessor-symbol-map nil))
    (dolist (D defines)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-map D))
    (when (featurep 'semantic/bovine/c)
      (semantic-c-reset-preprocessor-symbol-map))
    nil))

(provide 'semantic/bovine/gcc)

;; Local variables:
;; generated-autoload-file: "../loaddefs.el"
;; generated-autoload-load-name: "semantic/bovine/gcc"
;; End:

;;; semantic/bovine/gcc.el ends here
