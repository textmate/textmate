;;; ob-lilypond.el --- org-babel functions for lilypond evaluation

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Martyn Jago
;; Keywords: babel language, literate programming
;; Homepage: http://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lilypond.html

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

;; Installation / usage info, and examples are available at
;; https://github.com/mjago/ob-lilypond

;;; Code:
(require 'ob)
(require 'ob-eval)
(require 'ob-tangle)
(require 'outline)
(defalias 'lilypond-mode 'LilyPond-mode)

(add-to-list 'org-babel-tangle-lang-exts '("LilyPond" . "ly"))

(defvar org-babel-default-header-args:lilypond '()
  "Default header arguments for js code blocks.")

(defvar ly-compile-post-tangle t
  "Following the org-babel-tangle (C-c C-v t) command,
ly-compile-post-tangle determines whether ob-lilypond should
automatically attempt to compile the resultant tangled file.
If the value is nil, no automated compilation takes place.
Default value is t")

(defvar ly-display-pdf-post-tangle t
  "Following a successful LilyPond compilation
ly-display-pdf-post-tangle determines whether to automate the
drawing / redrawing of the resultant pdf. If the value is nil,
the pdf is not automatically redrawn. Default value is t")

(defvar ly-play-midi-post-tangle t
  "Following a successful LilyPond compilation
ly-play-midi-post-tangle determines whether to automate the
playing of the resultant midi file. If the value is nil,
the midi file is not automatically played. Default value is t")

(defvar ly-OSX-ly-path
  "/Applications/lilypond.app/Contents/Resources/bin/lilypond")
(defvar ly-OSX-pdf-path "open")
(defvar ly-OSX-midi-path "open")

(defvar ly-nix-ly-path "/usr/bin/lilypond")
(defvar ly-nix-pdf-path "evince")
(defvar ly-nix-midi-path "timidity")

(defvar ly-win32-ly-path "lilypond")
(defvar ly-win32-pdf-path "")
(defvar ly-win32-midi-path "")

(defvar ly-gen-png nil
"Image generation (png) can be turned on by default by setting
LY-GEN-PNG to t")

(defvar ly-gen-svg nil
"Image generation (SVG) can be turned on by default by setting
LY-GEN-SVG to t")

(defvar ly-gen-html nil
"HTML generation can be turned on by default by setting
LY-GEN-HTML to t")

(defvar ly-use-eps nil
"You can force the compiler to use the EPS backend by setting
LY-USE-EPS to t")

(defvar ly-arrange-mode nil
  "Arrange mode is turned on by setting LY-ARRANGE-MODE
to t. In Arrange mode the following settings are altered
from default...
:tangle yes,    :noweb yes
:results silent :comments yes.
In addition lilypond block execution causes tangling of all lilypond
blocks")

(defun org-babel-expand-body:lilypond (body params)
  "Expand BODY according to PARAMS, return the expanded body."

  (let ((vars (mapcar #'cdr (org-babel-get-header params :var))))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
	     (value (cdr pair)))
	 (setq body
	       (replace-regexp-in-string
		(concat "\$" (regexp-quote name))
		(if (stringp value) value (format "%S" value))
		body))))
     vars)
    body))

(defun org-babel-execute:lilypond (body params)
  "This function is called by `org-babel-execute-src-block'.
Depending on whether we are in arrange mode either:
1. Attempt to execute lilypond block according to header settings
  (This is the default basic mode)
2. Tangle all lilypond blocks and process the result (arrange mode)"

  (ly-set-header-args ly-arrange-mode)
  (if ly-arrange-mode
      (ly-tangle)
    (ly-process-basic body params)))

(defun ly-tangle ()
  "ob-lilypond specific tangle, attempts to invoke
=ly-execute-tangled-ly= if tangle is successful. Also passes
specific arguments to =org-babel-tangle="

  (interactive)
  (if (org-babel-tangle nil "yes" "lilypond")
      (ly-execute-tangled-ly) nil))

(defun ly-process-basic (body params)
  "Execute a lilypond block in basic mode"

  (let* ((result-params (cdr (assoc :result-params params)))
	 (out-file (cdr (assoc :file params)))
	 (cmdline (or (cdr (assoc :cmdline params))
		      ""))
	 (in-file (org-babel-temp-file "lilypond-")))

    (with-temp-file in-file
      (insert (org-babel-expand-body:generic body params)))

    (org-babel-eval
     (concat
      (ly-determine-ly-path)
      " -dbackend=eps "
      "-dno-gs-load-fonts "
      "-dinclude-eps-fonts "
      "--png "
      "--output="
      (file-name-sans-extension out-file)
      " "
      cmdline
      in-file) "")
    ) nil)

(defun org-babel-prep-session:lilypond (session params)
  "Return an error because LilyPond exporter does not support sessions."

  (error "Sorry, LilyPond does not currently support sessions!"))

(defun ly-execute-tangled-ly ()
  "Compile result of block tangle with lilypond.
If error in compilation, attempt to mark the error in lilypond org file"

  (when ly-compile-post-tangle
    (let ((ly-tangled-file (ly-switch-extension
                            (buffer-file-name) ".lilypond"))
          (ly-temp-file (ly-switch-extension
                         (buffer-file-name) ".ly")))
      (if (file-exists-p ly-tangled-file)
          (progn
            (when (file-exists-p ly-temp-file)
              (delete-file ly-temp-file))
            (rename-file ly-tangled-file
                         ly-temp-file))
        (error "Error: Tangle Failed!") t)
      (switch-to-buffer-other-window "*lilypond*")
      (erase-buffer)
      (ly-compile-lilyfile ly-temp-file)
      (goto-char (point-min))
      (if (not (ly-check-for-compile-error ly-temp-file))
          (progn
            (other-window -1)
            (ly-attempt-to-open-pdf ly-temp-file)
            (ly-attempt-to-play-midi ly-temp-file))
        (error "Error in Compilation!")))) nil)

(defun ly-compile-lilyfile (file-name &optional test)
  "Compile lilypond file and check for compile errors
FILE-NAME is full path to lilypond (.ly) file"

  (message "Compiling LilyPond...")
  (let ((arg-1 (ly-determine-ly-path)) ;program
        (arg-2 nil)                    ;infile
        (arg-3 "*lilypond*")           ;buffer
        (arg-4 t)                      ;display
        (arg-5 (if ly-gen-png  "--png"  "")) ;&rest...
  (arg-6 (if ly-gen-html "--html" ""))
        (arg-7 (if ly-use-eps  "-dbackend=eps" ""))
        (arg-8 (if ly-gen-svg  "-dbackend=svg" ""))
        (arg-9 (concat "--output=" (file-name-sans-extension file-name)))
        (arg-10 file-name))
    (if test
        `(,arg-1 ,arg-2 ,arg-3 ,arg-4 ,arg-5
                 ,arg-6 ,arg-7 ,arg-8 ,arg-9 ,arg-10)
      (call-process
       arg-1 arg-2 arg-3 arg-4 arg-5
       arg-6 arg-7 arg-8 arg-9 arg-10))))

(defun ly-check-for-compile-error (file-name &optional test)
  "Check for compile error.
This is performed by parsing the *lilypond* buffer
containing the output message from the compilation.
FILE-NAME is full path to lilypond file.
If TEST is t just return nil if no error found, and pass
nil as file-name since it is unused in this context"
  (let ((is-error (search-forward "error:" nil t)))
    (if (not test)
        (if (not is-error)
            nil
          (ly-process-compile-error file-name))
      is-error)))

(defun ly-process-compile-error (file-name)
  "Process the compilation error that has occurred.
FILE-NAME is full path to lilypond file"

  (let ((line-num (ly-parse-line-num)))
    (let ((error-lines (ly-parse-error-line file-name line-num)))
      (ly-mark-error-line file-name error-lines)
      (error "Error: Compilation Failed!"))))

(defun ly-mark-error-line (file-name line)
  "Mark the erroneous lines in the lilypond org buffer.
FILE-NAME is full path to lilypond file.
LINE is the erroneous line"

  (switch-to-buffer-other-window
   (concat (file-name-nondirectory
            (ly-switch-extension file-name ".org"))))
  (let ((temp (point)))
    (goto-char (point-min))
    (setq case-fold-search nil)
    (if (search-forward line nil t)
        (progn
          (show-all)
          (set-mark (point))
          (goto-char (- (point) (length line))))
      (goto-char temp))))

(defun ly-parse-line-num (&optional buffer)
  "Extract error line number."

  (when buffer
    (set-buffer buffer))
  (let ((start
         (and (search-backward ":" nil t)
              (search-backward ":" nil t)
              (search-backward ":" nil t)
              (search-backward ":" nil t)))
        (num nil))
    (if start
        (progn
          (forward-char)
          (let ((num (buffer-substring
                      (+ 1 start)
                      (- (search-forward ":" nil t) 1))))
            (setq num (string-to-number num))
            (if (numberp num)
                num
              nil)))
      nil)))

(defun ly-parse-error-line (file-name lineNo)
  "Extract the erroneous line from the tangled .ly file
FILE-NAME is full path to lilypond file.
LINENO is the number of the erroneous line"

  (with-temp-buffer
    (insert-file-contents (ly-switch-extension file-name ".ly")
			  nil nil nil t)
    (if (> lineNo 0)
	(progn
	  (goto-char (point-min))
	  (forward-line (- lineNo 1))
	  (buffer-substring (point) (point-at-eol)))
      nil)))

(defun ly-attempt-to-open-pdf (file-name &optional test)
  "Attempt to display the generated pdf file
FILE-NAME is full path to lilypond file
If TEST is non-nil, the shell command is returned and is not run"

  (when ly-display-pdf-post-tangle
    (let ((pdf-file (ly-switch-extension file-name ".pdf")))
      (if (file-exists-p pdf-file)
          (let ((cmd-string
                 (concat (ly-determine-pdf-path) " " pdf-file)))
            (if test
                cmd-string
              (shell-command cmd-string)))
        (message  "No pdf file generated so can't display!")))))

(defun ly-attempt-to-play-midi (file-name &optional test)
  "Attempt to play the generated MIDI file
FILE-NAME is full path to lilypond file
If TEST is non-nil, the shell command is returned and is not run"

  (when ly-play-midi-post-tangle
    (let ((midi-file (ly-switch-extension file-name ".midi")))
      (if (file-exists-p midi-file)
          (let ((cmd-string
                 (concat (ly-determine-midi-path) " " midi-file)))
            (if test
                cmd-string
              (shell-command cmd-string)))
        (message "No midi file generated so can't play!")))))

(defun ly-determine-ly-path (&optional test)
  "Return correct path to ly binary depending on OS
If TEST is non-nil, it contains a simulation of the OS for test purposes"

  (let ((sys-type
         (or test system-type)))
    (cond ((string= sys-type  "darwin")
           ly-OSX-ly-path)
          ((string= sys-type "win32")
           ly-win32-ly-path)
          (t ly-nix-ly-path))))

(defun ly-determine-pdf-path (&optional test)
  "Return correct path to pdf viewer depending on OS
If TEST is non-nil, it contains a simulation of the OS for test purposes"

  (let ((sys-type
         (or test system-type)))
    (cond ((string= sys-type  "darwin")
           ly-OSX-pdf-path)
          ((string= sys-type "win32")
           ly-win32-pdf-path)
          (t ly-nix-pdf-path))))

(defun ly-determine-midi-path (&optional test)
  "Return correct path to midi player depending on OS
If TEST is non-nil, it contains a simulation of the OS for test purposes"

  (let ((sys-type
         (or test test system-type)))
    (cond ((string= sys-type  "darwin")
           ly-OSX-midi-path)
          ((string= sys-type "win32")
           ly-win32-midi-path)
          (t ly-nix-midi-path))))

(defun ly-toggle-midi-play ()
  "Toggle whether midi will be played following a successful compilation"

  (interactive)
  (setq ly-play-midi-post-tangle
        (not ly-play-midi-post-tangle))
  (message (concat "Post-Tangle MIDI play has been "
                   (if ly-play-midi-post-tangle
                       "ENABLED." "DISABLED."))))

(defun ly-toggle-pdf-display ()
  "Toggle whether pdf will be displayed following a successful compilation"

  (interactive)
  (setq ly-display-pdf-post-tangle
        (not ly-display-pdf-post-tangle))
  (message (concat "Post-Tangle PDF display has been "
                   (if ly-display-pdf-post-tangle
                       "ENABLED." "DISABLED."))))

(defun ly-toggle-png-generation ()
  "Toggle whether png image will be generated by compilation"

  (interactive)
  (setq ly-gen-png
        (not ly-gen-png))
  (message (concat "PNG image generation has been "
                   (if ly-gen-png "ENABLED." "DISABLED."))))

(defun ly-toggle-html-generation ()
  "Toggle whether html will be generated by compilation"

  (interactive)
  (setq ly-gen-html
        (not ly-gen-html))
  (message (concat "HTML generation has been "
                   (if ly-gen-html "ENABLED." "DISABLED."))))

(defun ly-toggle-arrange-mode ()
  "Toggle whether in Arrange mode or Basic mode"

  (interactive)
  (setq ly-arrange-mode
        (not ly-arrange-mode))
  (message (concat "Arrange mode has been "
                   (if ly-arrange-mode "ENABLED." "DISABLED."))))

(defun ly-switch-extension (file-name ext)
  "Utility command to swap current FILE-NAME extension with EXT"

  (concat (file-name-sans-extension
           file-name) ext))

(defun ly-get-header-args (mode)
  "Default arguments to use when evaluating a lilypond
source block. These depend upon whether we are in arrange
mode i.e. ARRANGE-MODE is t"
  (cond (mode
         '((:tangle . "yes")
           (:noweb . "yes")
           (:results . "silent")
           (:comments . "yes")))
        (t
         '((:results . "file")
           (:exports . "results")))))

(defun ly-set-header-args (mode)
  "Set org-babel-default-header-args:lilypond
dependent on LY-ARRANGE-MODE"
  (setq org-babel-default-header-args:lilypond
        (ly-get-header-args mode)))

(provide 'ob-lilypond)



;;; ob-lilypond.el ends here
