;;; makeinfo.el --- run makeinfo conveniently

;; Copyright (C) 1991, 1993, 2001-2012  Free Software Foundation, Inc.

;; Author: Robert J. Chassell
;; Maintainer: FSF
;; Keywords: docs convenience

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

;;; The Texinfo mode `makeinfo' related commands are:

;; makeinfo-region      to run makeinfo on the current region.
;; makeinfo-buffer      to run makeinfo on the current buffer, or
;;                        with optional prefix arg, on current region
;; kill-compilation     to kill currently running makeinfo job
;; makeinfo-recenter-makeinfo-buffer  to redisplay *compilation* buffer

;;; Keybindings (defined in `texinfo.el')

;; makeinfo bindings
; (define-key texinfo-mode-map "\C-c\C-m\C-r" 'makeinfo-region)
; (define-key texinfo-mode-map "\C-c\C-m\C-b" 'makeinfo-buffer)
; (define-key texinfo-mode-map "\C-c\C-m\C-k" 'kill-compilation)
; (define-key texinfo-mode-map "\C-c\C-m\C-l"
;   'makeinfo-recenter-compilation-buffer)

;;; Code:

;;; Variables used by `makeinfo'

(require 'compile)
(require 'info)

(defvar tex-end-of-header)
(defvar tex-start-of-header)


(defgroup makeinfo nil
  "Run makeinfo conveniently."
  :group 'docs)


(defcustom makeinfo-run-command "makeinfo"
  "Command used to run `makeinfo' subjob.
The name of the file is appended to this string, separated by a space."
  :type 'string
  :group 'makeinfo)

(defcustom makeinfo-options "--fill-column=70"
  "String containing options for running `makeinfo'.
Do not include `--footnote-style' or `--paragraph-indent';
the proper way to specify those is with the Texinfo commands
`@footnotestyle` and `@paragraphindent'."
  :type 'string
  :group 'makeinfo)

(require 'texinfo)

(defvar makeinfo-compilation-process nil
  "Process that runs `makeinfo'.  Should start out nil.")

(defvar makeinfo-temp-file nil
  "Temporary file name used for text being sent as input to `makeinfo'.")

(defvar makeinfo-output-file-name nil
  "Info file name used for text output by `makeinfo'.")

(defvar makeinfo-output-node-name nil
  "Node name to visit in output file, for `makeinfo-buffer'.")


;;; The `makeinfo' function definitions

(defun makeinfo-region (region-beginning region-end)
  "Make Info file from region of current Texinfo file, and switch to it.

This command does not offer the `next-error' feature since it would
apply to a temporary file, not the original; use the `makeinfo-buffer'
command to gain use of `next-error'."

  (interactive "r")
  (let (filename-or-header
        filename-or-header-beginning
        filename-or-header-end)
    ;; Cannot use `let' for makeinfo-temp-file or
    ;; makeinfo-output-file-name since `makeinfo-compilation-sentinel'
    ;; needs them.

    (setq makeinfo-temp-file
          (concat
           (make-temp-file
            (substring (buffer-file-name)
                       0
                       (or (string-match "\\.tex" (buffer-file-name))
                           (length (buffer-file-name)))))
           ".texinfo"))

    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((search-end (save-excursion (forward-line 100) (point))))
          ;; Find and record the Info filename,
          ;; or else explain that a filename is needed.
          (if (re-search-forward
               "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
               search-end t)
              (setq makeinfo-output-file-name
                    (buffer-substring (match-beginning 1) (match-end 1)))
            (error
             "The texinfo file needs a line saying: @setfilename <name>"))

          ;; Find header and specify its beginning and end.
          (goto-char (point-min))
          (if (and
               (prog1
                   (search-forward tex-start-of-header search-end t)
                 (beginning-of-line)
                 ;; Mark beginning of header.
                 (setq filename-or-header-beginning (point)))
               (prog1
                   (search-forward tex-end-of-header nil t)
                 (beginning-of-line)
                 ;; Mark end of header
                 (setq filename-or-header-end (point))))

              ;; Insert the header into the temporary file.
              (write-region
               (min filename-or-header-beginning region-beginning)
               filename-or-header-end
               makeinfo-temp-file nil nil)

            ;; Else no header; insert @filename line into temporary file.
            (goto-char (point-min))
            (search-forward "@setfilename" search-end t)
            (beginning-of-line)
            (setq filename-or-header-beginning (point))
            (forward-line 1)
            (setq filename-or-header-end (point))
            (write-region
             (min filename-or-header-beginning region-beginning)
             filename-or-header-end
             makeinfo-temp-file nil nil))

          ;; Insert the region into the file.
          (write-region
           (max region-beginning filename-or-header-end)
           region-end
           makeinfo-temp-file t nil)

          ;; Run the `makeinfo-compile' command in the *compilation* buffer
          (save-excursion
            (makeinfo-compile
             (concat makeinfo-run-command
                     " "
                     makeinfo-options
                     " "
                     makeinfo-temp-file)
	     t
             'makeinfo-compilation-sentinel-region)))))))

(defun makeinfo-next-error (arg reset)
  "This function is used to disable `next-error' if the user has
used `makeinfo-region'.  Since the compilation process is used on
a temporary file in that case, calling `next-error' would give
nonsensical results."
  (error "Use `makeinfo-buffer' to gain use of the `next-error' command"))

;; Actually run makeinfo.  COMMAND is the command to run.  If
;; DISABLE-ERRORS is non-nil, disable `next-error' by setting
;; `next-error-function' to `makeinfo-next-error' in the compilation
;; buffer.
(defun makeinfo-compile (command disable-errors sentinel)
  (let ((buffer (compilation-start command)))
    (with-current-buffer buffer
      (setq next-error-function
	    (if disable-errors
		'makeinfo-next-error
	      'compilation-next-error-function)))
    (set-process-sentinel (get-buffer-process buffer) sentinel)))

;; Delete makeinfo-temp-file after processing is finished,
;; and visit Info file.
;; This function is called when the compilation process changes state.
;; Based on `compilation-sentinel' in compile.el
(defun makeinfo-compilation-sentinel-region (proc msg)
  "Sentinel for `makeinfo-compile' run from `makeinfo-region'."
  (compilation-sentinel proc msg)
  (when (memq (process-status proc) '(signal exit))
    (if (file-exists-p makeinfo-temp-file)
	(delete-file makeinfo-temp-file))
    ;; Always use the version on disk.
    (let ((buffer (get-file-buffer makeinfo-output-file-name)))
      (if buffer
	  (with-current-buffer buffer
	    (revert-buffer t t))
	(setq buffer (find-file-noselect makeinfo-output-file-name)))
      (if (window-dedicated-p (selected-window))
	  (switch-to-buffer-other-window buffer)
	(switch-to-buffer buffer)))
    (goto-char (point-min))))

(defun makeinfo-current-node ()
  "Return the name of the node containing point, in a texinfo file."
  (save-excursion
    (end-of-line)           ; in case point is at the start of an @node line
    (if (re-search-backward "^@node\\s-+\\([^,\n]+\\)" (point-min) t)
        (match-string 1)
      "Top")))

(defun makeinfo-buffer ()
  "Make Info file from current buffer.

Use the \\[next-error] command to move to the next error
\(if there are errors\)."

  (interactive)
  (cond ((null buffer-file-name)
         (error "Buffer not visiting any file"))
        ((buffer-modified-p)
         (if (y-or-n-p "Buffer modified; do you want to save it? ")
             (save-buffer))))

  ;; Find and record the Info filename,
  ;; or else explain that a filename is needed.
  (save-excursion
    (goto-char (point-min))
    (let ((search-end (save-excursion (forward-line 100) (point))))
      (if (re-search-forward
           "^@setfilename[ \t]+\\([^ \t\n]+\\)[ \t]*"
           search-end t)
          (setq makeinfo-output-file-name
                (expand-file-name
                 (buffer-substring (match-beginning 1) (match-end 1))))
        (error
         "The texinfo file needs a line saying: @setfilename <name>"))))
  (setq makeinfo-output-node-name (makeinfo-current-node))

  (save-excursion
    (makeinfo-compile
     (concat makeinfo-run-command " " makeinfo-options
	     " " buffer-file-name)
     nil
     'makeinfo-compilation-sentinel-buffer)))

(defun makeinfo-compilation-sentinel-buffer (proc msg)
  "Sentinel for `makeinfo-compile' run from `makeinfo-buffer'."
  (compilation-sentinel proc msg)
  (when (memq (process-status proc) '(signal exit))
    (when (file-exists-p makeinfo-output-file-name)
      (Info-revert-find-node
       makeinfo-output-file-name makeinfo-output-node-name))))

(defun makeinfo-recenter-compilation-buffer (linenum)
  "Redisplay `*compilation*' buffer so most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((makeinfo-buffer (get-buffer "*compilation*"))
	(old-buffer (current-buffer)))
    (if (null makeinfo-buffer)
	(message "No *compilation* buffer")
      (pop-to-buffer makeinfo-buffer)
      (bury-buffer makeinfo-buffer)
      (goto-char (point-max))
      (recenter (if linenum
		    (prefix-numeric-value linenum)
		  (/ (window-height) 2)))
      (pop-to-buffer old-buffer)
      )))

;;; Place `provide' at end of file.
(provide 'makeinfo)

;;; makeinfo.el ends here
