;;; ielm.el --- interaction mode for Emacs Lisp

;; Copyright (C) 1994, 2001-2012 Free Software Foundation, Inc.

;; Author: David Smith <maa036@lancaster.ac.uk>
;; Maintainer: FSF
;; Created: 25 Feb 1994
;; Keywords: lisp

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

;; Provides a nice interface to evaluating Emacs Lisp expressions.
;; Input is handled by the comint package, and output is passed
;; through the pretty-printer.

;; To start: M-x ielm.  Type C-h m in the *ielm* buffer for more info.

;;; Code:

(require 'comint)
(require 'pp)

;;; User variables

(defgroup ielm nil
  "Interaction mode for Emacs Lisp."
  :group 'lisp)


(defcustom ielm-noisy t
  "If non-nil, IELM will beep on error."
  :type 'boolean
  :group 'ielm)

(defcustom ielm-prompt-read-only t
  "If non-nil, the IELM prompt is read only.
The read only region includes the newline before the prompt.
Setting this variable does not affect existing IELM runs.
This works by setting the buffer-local value of `comint-prompt-read-only'.
Setting that value directly affects new prompts in the current buffer.

If this option is enabled, then the safe way to temporarily
override the read-only-ness of IELM prompts is to call
`comint-kill-whole-line' or `comint-kill-region' with no
narrowing in effect.  This way you will be certain that none of
the remaining prompts will be accidentally messed up.  You may
wish to put something like the following in your `.emacs' file:

\(add-hook 'ielm-mode-hook
	  (lambda ()
	     (define-key ielm-map \"\\C-w\" 'comint-kill-region)
	     (define-key ielm-map [C-S-backspace]
	       'comint-kill-whole-line)))

If you set `comint-prompt-read-only' to t, you might wish to use
`comint-mode-hook' and `comint-mode-map' instead of
`ielm-mode-hook' and `ielm-map'.  That will affect all comint
buffers, including IELM buffers.  If you sometimes use IELM on
text-only terminals or with `emacs -nw', you might wish to use
another binding for `comint-kill-whole-line'."
  :type 'boolean
  :group 'ielm
  :version "22.1")

(defcustom ielm-prompt "ELISP> "
  "Prompt used in IELM.
Setting this variable does not affect existing IELM runs.

Interrupting the IELM process with \\<ielm-map>\\[comint-interrupt-subjob],
and then restarting it using \\[ielm], makes the then current
default value affect _new_ prompts.  Unless the new prompt
differs only in text properties from the old one, IELM will no
longer recognize the old prompts.  However, executing \\[ielm]
does not update the prompt of an *ielm* buffer with a running process.
For IELM buffers that are not called `*ielm*', you can execute
\\[inferior-emacs-lisp-mode] in that IELM buffer to update the value,
for new prompts.  This works even if the buffer has a running process."
  :type 'string
  :group 'ielm)

(defvar ielm-prompt-internal "ELISP> "
  "Stored value of `ielm-prompt' in the current IELM buffer.
This is an internal variable used by IELM.  Its purpose is to
prevent a running IELM process from being messed up when the user
customizes `ielm-prompt'.")

(defcustom ielm-dynamic-return t
  "Controls whether \\<ielm-map>\\[ielm-return] has intelligent behavior in IELM.
If non-nil, \\[ielm-return] evaluates input for complete sexps, or inserts a newline
and indents for incomplete sexps.  If nil, always inserts newlines."
  :type 'boolean
  :group 'ielm)

(defcustom ielm-dynamic-multiline-inputs t
  "Force multiline inputs to start from column zero?
If non-nil, after entering the first line of an incomplete sexp, a newline
will be inserted after the prompt, moving the input to the next line.
This gives more frame width for large indented sexps, and allows functions
such as `edebug-defun' to work with such inputs."
  :type 'boolean
  :group 'ielm)

(defcustom ielm-mode-hook nil
  "Hooks to be run when IELM (`inferior-emacs-lisp-mode') is started."
  :options '(turn-on-eldoc-mode)
  :type 'hook
  :group 'ielm)
(defvaralias 'inferior-emacs-lisp-mode-hook 'ielm-mode-hook)

(defvar * nil
  "Most recent value evaluated in IELM.")

(defvar ** nil
  "Second-most-recent value evaluated in IELM.")

(defvar *** nil
  "Third-most-recent value evaluated in IELM.")

(defvar ielm-match-data nil
  "Match data saved at the end of last command.")

(defvar *1 nil
  "During IELM evaluation, most recent value evaluated in IELM.
Normally identical to `*'.  However, if the working buffer is an IELM
buffer, distinct from the process buffer, then `*' gives the value in
the working buffer, `*1' the value in the process buffer.
The intended value is only accessible during IELM evaluation.")

(defvar *2 nil
  "During IELM evaluation, second-most-recent value evaluated in IELM.
Normally identical to `**'.  However, if the working buffer is an IELM
buffer, distinct from the process buffer, then `**' gives the value in
the working buffer, `*2' the value in the process buffer.
The intended value is only accessible during IELM evaluation.")

(defvar *3 nil
  "During IELM evaluation, third-most-recent value evaluated in IELM.
Normally identical to `***'.  However, if the working buffer is an IELM
buffer, distinct from the process buffer, then `***' gives the value in
the working buffer, `*3' the value in the process buffer.
The intended value is only accessible during IELM evaluation.")

;;; System variables

(defvar ielm-working-buffer nil
  "Buffer in which IELM sexps will be evaluated.
This variable is buffer-local.")

(defvar ielm-header
  "*** Welcome to IELM ***  Type (describe-mode) for help.\n"
  "Message to display when IELM is started.")

(defvar ielm-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\C-m" 'ielm-return)
    (define-key map "\C-j" 'ielm-send-input)
    (define-key map "\e\C-x" 'eval-defun)         ; for consistency with
    (define-key map "\e\t" 'completion-at-point)  ; lisp-interaction-mode
    ;; These bindings are from `lisp-mode-shared-map' -- can you inherit
    ;; from more than one keymap??
    (define-key map "\e\C-q" 'indent-sexp)
    (define-key map "\177" 'backward-delete-char-untabify)
    ;; Some convenience bindings for setting the working buffer
    (define-key map "\C-c\C-b" 'ielm-change-working-buffer)
    (define-key map "\C-c\C-f" 'ielm-display-working-buffer)
    (define-key map "\C-c\C-v" 'ielm-print-working-buffer)
    map)
  "Keymap for IELM mode.")
(defvaralias 'inferior-emacs-lisp-mode-map 'ielm-map)

(defvar ielm-font-lock-keywords
  '(("\\(^\\*\\*\\*[^*]+\\*\\*\\*\\)\\(.*$\\)"
     (1 font-lock-comment-face)
     (2 font-lock-constant-face)))
  "Additional expressions to highlight in IELM buffers.")

;;; Completion stuff

(defun ielm-tab nil
  "Possibly indent the current line as Lisp code."
  (interactive)
  (when (or (eq (preceding-char) ?\n)
	    (eq (char-syntax (preceding-char)) ?\s))
    (ielm-indent-line)
    t))

(defun ielm-complete-symbol nil
  "Complete the Lisp symbol before point."
  ;; A wrapper for lisp-complete symbol that returns non-nil if
  ;; completion has occurred
  (let* ((btick (buffer-modified-tick))
	 (cbuffer (get-buffer "*Completions*"))
	 (ctick (and cbuffer (buffer-modified-tick cbuffer))))
    (lisp-complete-symbol)
     ;; completion has occurred if:
    (or
     ;; the buffer has been modified
     (not (= btick (buffer-modified-tick)))
     ;; a completions buffer has been modified or created
     (if cbuffer
	 (not (= ctick (buffer-modified-tick cbuffer)))
       (get-buffer "*Completions*")))))

(defun ielm-complete-filename nil
  "Dynamically complete filename before point, if in a string."
  (when (nth 3 (parse-partial-sexp comint-last-input-start (point)))
    (comint-dynamic-complete-filename)))

(defun ielm-indent-line nil
  "Indent the current line as Lisp code if it is not a prompt line."
  (when (save-excursion (comint-bol) (bolp))
    (lisp-indent-line)))

;;; Working buffer manipulation

(defun ielm-print-working-buffer nil
  "Print the current IELM working buffer's name in the echo area."
  (interactive)
  (message "The current working buffer is: %s" (buffer-name ielm-working-buffer)))

(defun ielm-display-working-buffer nil
  "Display the current IELM working buffer.
Don't forget that selecting that buffer will change its value of `point'
to its value of `window-point'!"
  (interactive)
  (display-buffer ielm-working-buffer)
  (ielm-print-working-buffer))

(defun ielm-change-working-buffer (buf)
  "Change the current IELM working buffer to BUF.
This is the buffer in which all sexps entered at the IELM prompt are
evaluated.  You can achieve the same effect with a call to
`set-buffer' at the IELM prompt."
  (interactive "bSet working buffer to: ")
  (let ((buffer (get-buffer buf)))
    (if (and buffer (buffer-live-p buffer))
  	(setq ielm-working-buffer buffer)
      (error "No such buffer: %S" buf)))
  (ielm-print-working-buffer))

;;; Other bindings

(defun ielm-return nil
  "Newline and indent, or evaluate the sexp before the prompt.
Complete sexps are evaluated; for incomplete sexps inserts a newline
and indents.  If however `ielm-dynamic-return' is nil, this always
simply inserts a newline."
  (interactive)
  (if ielm-dynamic-return
      (let ((state
	     (save-excursion
	       (end-of-line)
	       (parse-partial-sexp (ielm-pm)
				   (point)))))
	(if (and (< (car state) 1) (not (nth 3 state)))
	    (ielm-send-input)
	  (when (and ielm-dynamic-multiline-inputs
		     (save-excursion
		       (beginning-of-line)
		       (looking-at-p comint-prompt-regexp)))
	    (save-excursion
	      (goto-char (ielm-pm))
	      (newline 1)))
	  (newline-and-indent)))
    (newline)))

(defvar ielm-input)

(defun ielm-input-sender (_proc input)
  ;; Just sets the variable ielm-input, which is in the scope of
  ;; `ielm-send-input's call.
  (setq ielm-input input))

(defun ielm-send-input nil
  "Evaluate the Emacs Lisp expression after the prompt."
  (interactive)
  (let (ielm-input)			; set by ielm-input-sender
    (comint-send-input)			; update history, markers etc.
    (ielm-eval-input ielm-input)))

;;; Utility functions

(defun ielm-is-whitespace-or-comment (string)
  "Return non-nil if STRING is all whitespace or a comment."
  (or (string= string "")
      (string-match-p "\\`[ \t\n]*\\(?:;.*\\)*\\'" string)))

;;; Evaluation

(defvar ielm-string)
(defvar ielm-form)
(defvar ielm-pos)
(defvar ielm-result)
(defvar ielm-error-type)
(defvar ielm-output)
(defvar ielm-wbuf)
(defvar ielm-pmark)

(defun ielm-eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  ;; This is the function that actually `sends' the input to the
  ;; `inferior Lisp process'. All comint-send-input does is works out
  ;; what that input is.  What this function does is evaluates that
  ;; input and produces `output' which gets inserted into the buffer,
  ;; along with a new prompt.  A better way of doing this might have
  ;; been to actually send the output to the `cat' process, and write
  ;; this as in output filter that converted sexps in the output
  ;; stream to their evaluated value.  But that would have involved
  ;; more process coordination than I was happy to deal with.
  ;;
  ;; NOTE: all temporary variables in this function will be in scope
  ;; during the eval, and so need to have non-clashing names.
  (let ((ielm-string input-string)      ; input expression, as a string
        ielm-form			; form to evaluate
	ielm-pos			; End posn of parse in string
	ielm-result			; Result, or error message
	ielm-error-type			; string, nil if no error
	(ielm-output "")		; result to display
	(ielm-wbuf ielm-working-buffer)	; current buffer after evaluation
	(ielm-pmark (ielm-pm)))
    (unless (ielm-is-whitespace-or-comment ielm-string)
      (condition-case err
	  (let ((rout (read-from-string ielm-string)))
	    (setq ielm-form (car rout)
		  ielm-pos (cdr rout)))
	(error (setq ielm-result (error-message-string err))
	       (setq ielm-error-type "Read error")))
      (unless ielm-error-type
	;; Make sure working buffer has not been killed
	(if (not (buffer-name ielm-working-buffer))
	    (setq ielm-result "Working buffer has been killed"
		  ielm-error-type "IELM Error"
		  ielm-wbuf (current-buffer))
	  (if (ielm-is-whitespace-or-comment (substring ielm-string ielm-pos))
	      ;; To correctly handle the ielm-local variables *,
	      ;; ** and ***, we need a temporary buffer to be
	      ;; current at entry to the inner of the next two let
	      ;; forms.  We need another temporary buffer to exit
	      ;; that same let.  To avoid problems, neither of
	      ;; these buffers should be alive during the
	      ;; evaluation of ielm-form.
	      (let ((*1 *)
		    (*2 **)
		    (*3 ***)
		    ielm-temp-buffer)
		(set-match-data ielm-match-data)
		(save-excursion
		  (with-temp-buffer
		    (condition-case err
			(unwind-protect
			     ;; The next let form creates default
			     ;; bindings for *, ** and ***.  But
			     ;; these default bindings are
			     ;; identical to the ielm-local
			     ;; bindings.  Hence, during the
			     ;; evaluation of ielm-form, the
			     ;; ielm-local values are going to be
			     ;; used in all buffers except for
			     ;; other ielm buffers, which override
			     ;; them.  Normally, the variables *1,
			     ;; *2 and *3 also have default
			     ;; bindings, which are not overridden.
			     (let ((* *1)
				   (** *2)
				   (*** *3))
			       (kill-buffer (current-buffer))
			       (set-buffer ielm-wbuf)
			       (setq ielm-result
                                     (eval ielm-form lexical-binding))
			       (setq ielm-wbuf (current-buffer))
			       (setq
				ielm-temp-buffer
				(generate-new-buffer " *ielm-temp*"))
			       (set-buffer ielm-temp-buffer))
			  (when ielm-temp-buffer
			    (kill-buffer ielm-temp-buffer)))
		      (error (setq ielm-result (error-message-string err))
			     (setq ielm-error-type "Eval error"))
		      (quit (setq ielm-result "Quit during evaluation")
			    (setq ielm-error-type "Eval error")))))
		(setq ielm-match-data (match-data)))
	    (setq ielm-error-type "IELM error")
	    (setq ielm-result "More than one sexp in input"))))

      ;; If the eval changed the current buffer, mention it here
      (unless (eq ielm-wbuf ielm-working-buffer)
	(message "current buffer is now: %s" ielm-wbuf)
	(setq ielm-working-buffer ielm-wbuf))

      (goto-char ielm-pmark)
      (unless ielm-error-type
	(condition-case nil
	    ;; Self-referential objects cause loops in the printer, so
	    ;; trap quits here. May as well do errors, too
	    (setq ielm-output (concat ielm-output (pp-to-string ielm-result)))
	  (error (setq ielm-error-type "IELM Error")
		 (setq ielm-result "Error during pretty-printing (bug in pp)"))
	  (quit  (setq ielm-error-type "IELM Error")
		 (setq ielm-result "Quit during pretty-printing"))))
      (if ielm-error-type
	  (progn
	    (when ielm-noisy (ding))
	    (setq ielm-output (concat ielm-output "*** " ielm-error-type " ***  "))
	    (setq ielm-output (concat ielm-output ielm-result)))
	;; There was no error, so shift the *** values
	(setq *** **)
	(setq ** *)
	(setq * ielm-result))
      (setq ielm-output (concat ielm-output "\n")))
    (setq ielm-output (concat ielm-output ielm-prompt-internal))
    (comint-output-filter (ielm-process) ielm-output)))

;;; Process and marker utilities

(defun ielm-process nil
  ;; Return the current buffer's process.
  (get-buffer-process (current-buffer)))

(defun ielm-pm nil
  ;; Return the process mark of the current buffer.
  (process-mark (get-buffer-process (current-buffer))))

(defun ielm-set-pm (pos)
  ;; Set the process mark in the current buffer to POS.
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

;;; Major mode

(define-derived-mode inferior-emacs-lisp-mode comint-mode "IELM"
  "Major mode for interactively evaluating Emacs Lisp expressions.
Uses the interface provided by `comint-mode' (which see).

* \\<ielm-map>\\[ielm-send-input] evaluates the sexp following the prompt.  There must be at most
  one top level sexp per prompt.

* \\[ielm-return] inserts a newline and indents, or evaluates a
  complete expression (but see variable `ielm-dynamic-return').
  Inputs longer than one line are moved to the line following the
  prompt (but see variable `ielm-dynamic-multiline-inputs').

* \\[comint-dynamic-complete] completes Lisp symbols (or filenames, within strings),
  or indents the line if there is nothing to complete.

The current working buffer may be changed (with a call to `set-buffer',
or with \\[ielm-change-working-buffer]), and its value is preserved between successive
evaluations.  In this way, expressions may be evaluated in a different
buffer than the *ielm* buffer.  By default, its name is shown on the
mode line; you can always display it with \\[ielm-print-working-buffer], or the buffer itself
with \\[ielm-display-working-buffer].

During evaluations, the values of the variables `*', `**', and `***'
are the results of the previous, second previous and third previous
evaluations respectively.  If the working buffer is another IELM
buffer, then the values in the working buffer are used.  The variables
`*1', `*2' and `*3', yield the process buffer values.

Expressions evaluated by IELM are not subject to `debug-on-quit' or
`debug-on-error'.

The behavior of IELM may be customized with the following variables:
* To stop beeping on error, set `ielm-noisy' to nil.
* If you don't like the prompt, you can change it by setting `ielm-prompt'.
* If you do not like that the prompt is (by default) read-only, set
  `ielm-prompt-read-only' to nil.
* Set `ielm-dynamic-return' to nil for bindings like `lisp-interaction-mode'.
* Entry to this mode runs `comint-mode-hook' and `ielm-mode-hook'
 (in that order).

Customized bindings may be defined in `ielm-map', which currently contains:
\\{ielm-map}"
  :syntax-table emacs-lisp-mode-syntax-table

  (setq comint-prompt-regexp (concat "^" (regexp-quote ielm-prompt)))
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) comint-prompt-regexp)
  (setq comint-input-sender 'ielm-input-sender)
  (setq comint-process-echoes nil)
  (set (make-local-variable 'comint-dynamic-complete-functions)
       '(ielm-tab comint-replace-by-expanded-history
	 ielm-complete-filename ielm-complete-symbol))
  (set (make-local-variable 'ielm-prompt-internal) ielm-prompt)
  (set (make-local-variable 'comint-prompt-read-only) ielm-prompt-read-only)
  (setq comint-get-old-input 'ielm-get-old-input)
  (set (make-local-variable 'comint-completion-addsuffix) '("/" . ""))
  (setq mode-line-process '(":%s on " (:eval (buffer-name ielm-working-buffer))))

  (set (make-local-variable 'indent-line-function) 'ielm-indent-line)
  (set (make-local-variable 'ielm-working-buffer) (current-buffer))
  (set (make-local-variable 'fill-paragraph-function) 'lisp-fill-paragraph)
  (add-hook 'completion-at-point-functions
            'lisp-completion-at-point nil 'local)

  ;; Value holders
  (set (make-local-variable '*) nil)
  (set (make-local-variable '**) nil)
  (set (make-local-variable '***) nil)
  (set (make-local-variable 'ielm-match-data) nil)

  ;; font-lock support
  (set (make-local-variable 'font-lock-defaults)
       '(ielm-font-lock-keywords nil nil ((?: . "w") (?- . "w") (?* . "w"))))

  ;; A dummy process to keep comint happy. It will never get any input
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
	(start-process "ielm" (current-buffer) "hexl")
      (file-error (start-process "ielm" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (ielm-process) nil)
    (goto-char (point-max))

    ;; Lisp output can include raw characters that confuse comint's
    ;; carriage control code.
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)

    ;; Add a silly header
    (insert ielm-header)
    (ielm-set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (ielm-process) ielm-prompt-internal)
    (set-marker comint-last-input-start (ielm-pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter)))

(defun ielm-get-old-input nil
  ;; Return the previous input surrounding point
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;;; User command

;;;###autoload
(defun ielm nil
  "Interactively evaluate Emacs Lisp expressions.
Switches to the buffer `*ielm*', or creates it if it does not exist."
  (interactive)
  (let (old-point)
    (unless (comint-check-proc "*ielm*")
      (with-current-buffer (get-buffer-create "*ielm*")
	(unless (zerop (buffer-size)) (setq old-point (point)))
	(inferior-emacs-lisp-mode)))
    (switch-to-buffer "*ielm*")
    (when old-point (push-mark old-point))))

(provide 'ielm)

;;; ielm.el ends here
