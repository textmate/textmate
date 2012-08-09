;;; octave-inf.el --- running Octave as an inferior Emacs process

;; Copyright (C) 1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
;; 	   John Eaton <jwe@bevo.che.wisc.edu>
;; Maintainer: FSF
;; Keywords: languages
;; Package: octave-mod

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

;;; Code:

(require 'octave-mod)
(require 'comint)

(defgroup octave-inferior nil
  "Running Octave as an inferior Emacs process."
  :group 'octave)

(defcustom inferior-octave-program "octave"
  "Program invoked by `inferior-octave'."
  :type 'string
  :group 'octave-inferior)

(defcustom inferior-octave-prompt
  "\\(^octave\\(\\|.bin\\|.exe\\)\\(-[.0-9]+\\)?\\(:[0-9]+\\)?\\|^debug\\|^\\)>+ "
  "Regexp to match prompts for the inferior Octave process."
  :type 'regexp
  :group 'octave-inferior)

(defcustom inferior-octave-startup-file nil
  "Name of the inferior Octave startup file.
The contents of this file are sent to the inferior Octave process on
startup."
  :type '(choice (const :tag "None" nil)
		 file)
  :group 'octave-inferior)

(defcustom inferior-octave-startup-args nil
  "List of command line arguments for the inferior Octave process.
For example, for suppressing the startup message and using `traditional'
mode, set this to (\"-q\" \"--traditional\")."
  :type '(repeat string)
  :group 'octave-inferior)

(defvar inferior-octave-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-?" 'comint-dynamic-list-filename-completions)
    (define-key map "\C-c\C-l" 'inferior-octave-dynamic-list-input-ring)
    (define-key map [menu-bar inout list-history]
      '("List Input History" . inferior-octave-dynamic-list-input-ring))
    ;; FIXME: free C-h so it can do the describe-prefix-bindings.
    (define-key map "\C-c\C-h" 'info-lookup-symbol)
    map)
  "Keymap used in Inferior Octave mode.")

(defvar inferior-octave-mode-syntax-table
  (let ((table (make-syntax-table octave-mode-syntax-table)))
    table)
  "Syntax table in use in inferior-octave-mode buffers.")

(defcustom inferior-octave-mode-hook nil
  "*Hook to be run when Inferior Octave mode is started."
  :type 'hook
  :group 'octave-inferior)

(defvar inferior-octave-font-lock-keywords
  (list
   (cons inferior-octave-prompt 'font-lock-type-face))
  ;; Could certainly do more font locking in inferior Octave ...
  "Additional expressions to highlight in Inferior Octave mode.")


;;; Compatibility functions
(if (not (fboundp 'comint-line-beginning-position))
    ;; comint-line-beginning-position is defined in Emacs 21
    (defun comint-line-beginning-position ()
      "Returns the buffer position of the beginning of the line, after any prompt.
The prompt is assumed to be any text at the beginning of the line matching
the regular expression `comint-prompt-regexp', a buffer local variable."
      (save-excursion (comint-bol nil) (point))))


(defvar inferior-octave-output-list nil)
(defvar inferior-octave-output-string nil)
(defvar inferior-octave-receive-in-progress nil)

(defvar inferior-octave-startup-hook nil)

(defvar inferior-octave-complete-impossible nil
  "Non-nil means that `inferior-octave-complete' is impossible.")

(defvar inferior-octave-has-built-in-variables nil
  "Non-nil means that Octave has built-in variables.")

(defvar inferior-octave-dynamic-complete-functions
  '(inferior-octave-completion-at-point comint-filename-completion)
  "List of functions called to perform completion for inferior Octave.
This variable is used to initialize `comint-dynamic-complete-functions'
in the Inferior Octave buffer.")

(defvar info-lookup-mode)

(define-derived-mode inferior-octave-mode comint-mode "Inferior Octave"
  "Major mode for interacting with an inferior Octave process.
Runs Octave as a subprocess of Emacs, with Octave I/O through an Emacs
buffer.

Entry to this mode successively runs the hooks `comint-mode-hook' and
`inferior-octave-mode-hook'."
  (setq comint-prompt-regexp inferior-octave-prompt
	mode-line-process '(":%s")
	local-abbrev-table octave-abbrev-table)

  (set (make-local-variable 'comment-start) octave-comment-start)
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)
  (set (make-local-variable 'comment-start-skip) octave-comment-start-skip)

  (set (make-local-variable 'font-lock-defaults)
       '(inferior-octave-font-lock-keywords nil nil))

  (set (make-local-variable 'info-lookup-mode) 'octave-mode)

  (setq comint-input-ring-file-name
	(or (getenv "OCTAVE_HISTFILE") "~/.octave_hist")
	comint-input-ring-size (or (getenv "OCTAVE_HISTSIZE") 1024))
  (set (make-local-variable 'comint-dynamic-complete-functions)
       inferior-octave-dynamic-complete-functions)
  (add-hook 'comint-input-filter-functions
	'inferior-octave-directory-tracker nil t)
  (comint-read-input-ring t))

;;;###autoload
(defun inferior-octave (&optional arg)
  "Run an inferior Octave process, I/O via `inferior-octave-buffer'.
This buffer is put in Inferior Octave mode.  See `inferior-octave-mode'.

Unless ARG is non-nil, switches to this buffer.

The elements of the list `inferior-octave-startup-args' are sent as
command line arguments to the inferior Octave process on startup.

Additional commands to be executed on startup can be provided either in
the file specified by `inferior-octave-startup-file' or by the default
startup file, `~/.emacs-octave'."
  (interactive "P")
  (let ((buffer inferior-octave-buffer))
    (get-buffer-create buffer)
    (if (comint-check-proc buffer)
	()
      (with-current-buffer buffer
	(comint-mode)
	(inferior-octave-startup)
	(inferior-octave-mode)))
    (if (not arg)
	(pop-to-buffer buffer))))

;;;###autoload
(defalias 'run-octave 'inferior-octave)

(defun inferior-octave-startup ()
  "Start an inferior Octave process."
  (let ((proc (comint-exec-1
	       (substring inferior-octave-buffer 1 -1)
	       inferior-octave-buffer
	       inferior-octave-program
	       (append (list "-i" "--no-line-editing")
		       inferior-octave-startup-args))))
    (set-process-filter proc 'inferior-octave-output-digest)
    (setq comint-ptyp process-connection-type
	  inferior-octave-process proc
	  inferior-octave-output-list nil
	  inferior-octave-output-string nil
	  inferior-octave-receive-in-progress t)

    ;; This may look complicated ... However, we need to make sure that
    ;; we additional startup code only AFTER Octave is ready (otherwise,
    ;; output may be mixed up).  Hence, we need to digest the Octave
    ;; output to see when it issues a prompt.
    (while inferior-octave-receive-in-progress
      (accept-process-output inferior-octave-process))
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (insert-before-markers
     (concat
      (if (not (bobp)) "\n")
      (if inferior-octave-output-list
	  (concat (mapconcat
		   'identity inferior-octave-output-list "\n")
		  "\n"))))

     ;; Find out whether Octave has built-in variables.
     (inferior-octave-send-list-and-digest
      (list "exist \"LOADPATH\"\n"))
     (setq inferior-octave-has-built-in-variables
 	  (string-match "101$" (car inferior-octave-output-list)))

    ;; An empty secondary prompt, as e.g. obtained by '--braindead',
    ;; means trouble.
    (inferior-octave-send-list-and-digest (list "PS2\n"))
    (if (string-match "\\(PS2\\|ans\\) = *$" (car inferior-octave-output-list))
 	(inferior-octave-send-list-and-digest
 	 (list (if inferior-octave-has-built-in-variables
 		   "PS2 = \"> \"\n"
 		 "PS2 (\"> \");\n"))))

    ;; O.k., now we are ready for the Inferior Octave startup commands.
    (let* (commands
	   (program (file-name-nondirectory inferior-octave-program))
	   (file (or inferior-octave-startup-file
			  (concat "~/.emacs-" program))))
      (setq commands
	    (list "more off;\n"
		  (if (not (string-equal
			    inferior-octave-output-string ">> "))
		      (if inferior-octave-has-built-in-variables
			  "PS1=\"\\\\s> \";\n"
			"PS1 (\"\\\\s> \");\n"))
		  (if (file-exists-p file)
		      (format "source (\"%s\");\n" file))))
      (inferior-octave-send-list-and-digest commands))
    (insert-before-markers
     (concat
      (if inferior-octave-output-list
	  (concat (mapconcat
		   'identity inferior-octave-output-list "\n")
		  "\n"))
      inferior-octave-output-string))
    ;; Next, we check whether Octave supports `completion_matches' ...
    (inferior-octave-send-list-and-digest
     (list "exist \"completion_matches\"\n"))
    (setq inferior-octave-complete-impossible
	  (not (string-match "5$" (car inferior-octave-output-list))))

    ;; And finally, everything is back to normal.
    (set-process-filter proc 'inferior-octave-output-filter)
    (run-hooks 'inferior-octave-startup-hook)
    (run-hooks 'inferior-octave-startup-hook)
    ;; Just in case, to be sure a cd in the startup file
    ;; won't have detrimental effects.
    (inferior-octave-resync-dirs)))


(defun inferior-octave-completion-at-point ()
  "Return the data to complete the Octave symbol at point."
  (let* ((end (point))
	 (start
	  (save-excursion
	    (skip-syntax-backward "w_" (comint-line-beginning-position))
            (point))))
    (cond ((eq start end) nil)
	  (inferior-octave-complete-impossible
           (message (concat
                     "Your Octave does not have `completion_matches'.  "
                     "Please upgrade to version 2.X."))
           nil)
	  (t
           (list
            start end
            (completion-table-dynamic
             (lambda (command)
               (inferior-octave-send-list-and-digest
                (list (concat "completion_matches (\"" command "\");\n")))
               (sort (delete-dups inferior-octave-output-list)
                     'string-lessp))))))))

(define-obsolete-function-alias 'inferior-octave-complete
  'completion-at-point "24.1")

(defun inferior-octave-dynamic-list-input-ring ()
  "List the buffer's input history in a help buffer."
  ;; We cannot use `comint-dynamic-list-input-ring', because it replaces
  ;; "completion" by "history reference" ...
  (interactive)
  (if (or (not (ring-p comint-input-ring))
          (ring-empty-p comint-input-ring))
      (message "No history")
    (let ((history nil)
          (history-buffer " *Input History*")
          (index (1- (ring-length comint-input-ring)))
          (conf (current-window-configuration)))
      ;; We have to build up a list ourselves from the ring vector.
      (while (>= index 0)
        (setq history (cons (ring-ref comint-input-ring index) history)
              index (1- index)))
      ;; Change "completion" to "history reference"
      ;; to make the display accurate.
      (with-output-to-temp-buffer history-buffer
        (display-completion-list history)
        (set-buffer history-buffer))
      (message "Hit space to flush")
      (let ((ch (read-event)))
        (if (eq ch ?\ )
            (set-window-configuration conf)
          (setq unread-command-events (list ch)))))))

(defun inferior-octave-strip-ctrl-g (string)
  "Strip leading `^G' character.
If STRING starts with a `^G', ring the bell and strip it."
  (if (string-match "^\a" string)
      (progn
        (ding)
        (setq string (substring string 1))))
  string)

(defun inferior-octave-output-filter (proc string)
  "Standard output filter for the inferior Octave process.
Ring Emacs bell if process output starts with an ASCII bell, and pass
the rest to `comint-output-filter'."
  (comint-output-filter proc (inferior-octave-strip-ctrl-g string)))

(defun inferior-octave-output-digest (_proc string)
  "Special output filter for the inferior Octave process.
Save all output between newlines into `inferior-octave-output-list', and
the rest to `inferior-octave-output-string'."
  (setq string (concat inferior-octave-output-string string))
  (while (string-match "\n" string)
    (setq inferior-octave-output-list
	  (append inferior-octave-output-list
		  (list (substring string 0 (match-beginning 0))))
	  string (substring string (match-end 0))))
  (if (string-match inferior-octave-prompt string)
      (setq inferior-octave-receive-in-progress nil))
  (setq inferior-octave-output-string string))

(defun inferior-octave-send-list-and-digest (list)
  "Send LIST to the inferior Octave process and digest the output.
The elements of LIST have to be strings and are sent one by one.  All
output is passed to the filter `inferior-octave-output-digest'."
  (let* ((proc inferior-octave-process)
	 (filter (process-filter proc))
	 string)
    (set-process-filter proc 'inferior-octave-output-digest)
    (setq inferior-octave-output-list nil)
    (unwind-protect
	(while (setq string (car list))
	  (setq inferior-octave-output-string nil
		inferior-octave-receive-in-progress t)
	  (comint-send-string proc string)
	  (while inferior-octave-receive-in-progress
	    (accept-process-output proc))
	  (setq list (cdr list)))
      (set-process-filter proc filter))))

(defun inferior-octave-directory-tracker (string)
  "Tracks `cd' commands issued to the inferior Octave process.
Use \\[inferior-octave-resync-dirs] to resync if Emacs gets confused."
  (cond
   ((string-match "^[ \t]*cd[ \t;]*$" string)
    (cd "~"))
   ((string-match "^[ \t]*cd[ \t]+\\([^ \t\n;]*\\)[ \t\n;]*" string)
    (cd (substring string (match-beginning 1) (match-end 1))))))

(defun inferior-octave-resync-dirs ()
  "Resync the buffer's idea of the current directory.
This command queries the inferior Octave process about its current
directory and makes this the current buffer's default directory."
  (interactive)
  (inferior-octave-send-list-and-digest '("disp (pwd ())\n"))
  (cd (car inferior-octave-output-list)))

;;; provide ourself

(provide 'octave-inf)

;;; octave-inf.el ends here
