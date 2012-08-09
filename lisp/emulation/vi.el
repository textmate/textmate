;;; vi.el --- major mode for emulating "vi" editor under GNU Emacs

;; This file is in the public domain because the authors distributed it
;; without a copyright notice before the US signed the Bern Convention.

;; This file is part of GNU Emacs.

;; Author: Neal Ziring <nz@rsch.wisc.edu>
;;	Felix S. T. Wu <wu@crys.wisc.edu>
;; Keywords: emulations

;;; Commentary:

;; Originally written by : seismo!wucs!nz@rsch.wisc.edu (Neal Ziring)
;; Extensively redesigned and rewritten by wu@crys.wisc.edu (Felix S.T. Wu)
;; Last revision: 01/07/87 Wed (for GNU Emacs 18.33)

;; INSTALLATION PROCEDURE:
;; 1) Add a global key binding for command "vi-mode" (I use ESC ESC instead of
;;    the single ESC used in real "vi", so I can access other ESC prefixed emacs
;;    commands while I'm in "vi"), say, by putting the following line in your
;;    ".emacs" file:
;;    (define-key global-map "\e\e" 'vi-mode) ;quick switch into vi-mode
;; 2) If you wish you can define "find-file-hook" to enter "vi" automatically
;;    after a file is loaded into the buffer. For example, I defined it as:
;;    (setq find-file-hook (list
;;                           (function (lambda ()
;;                               (if (not (or (eq major-mode 'Info-mode)
;; 	                                     (eq major-mode 'vi-mode)))
;;                                   (vi-mode))))))
;; 3) In your .emacs file you can define the command "vi-mode" to be "autoload"
;;    or you can execute the "load" command to load "vi" directly.
;; 4) Read the comments for command "vi-mode" before you start using it.

;; COULD DO
;; 1). A general 'define-operator' function to replace current hack
;; 2). In operator handling, should allow other point moving Emacs commands
;;     (such as ESC <, ESC >) to be used as arguments.

;;; Code:

(defvar vi-mode-old-major-mode)
(defvar vi-mode-old-mode-name)
(defvar vi-mode-old-local-map)
(defvar vi-mode-old-case-fold)

(if (null (where-is-internal 'vi-switch-mode (current-local-map)))
    (define-key ctl-x-map "~" 'vi-switch-mode))

(defvar vi-tilde-map nil
  "Keymap used for \\[vi-switch-mode] prefix key.  Link to various major modes.")

(if vi-tilde-map
    nil
  (setq vi-tilde-map (make-keymap))
  (define-key vi-tilde-map "a" 'abbrev-mode)
  (define-key vi-tilde-map "c" 'c-mode)
  (define-key vi-tilde-map "d" 'vi-debugging)
  (define-key vi-tilde-map "e" 'emacs-lisp-mode)
  (define-key vi-tilde-map "f" 'auto-fill-mode)
  (define-key vi-tilde-map "g" 'prolog-mode)
  (define-key vi-tilde-map "h" 'hanoi)
  (define-key vi-tilde-map "i" 'info-mode)
  (define-key vi-tilde-map "l" 'lisp-mode)
  (define-key vi-tilde-map "n" 'nroff-mode)
  (define-key vi-tilde-map "o" 'overwrite-mode)
  (define-key vi-tilde-map "O" 'outline-mode)
  (define-key vi-tilde-map "P" 'picture-mode)
  (define-key vi-tilde-map "r" 'vi-readonly-mode)
  (define-key vi-tilde-map "t" 'text-mode)
  (define-key vi-tilde-map "v" 'vi-mode)
  (define-key vi-tilde-map "x" 'tex-mode)
  (define-key vi-tilde-map "~" 'vi-back-to-old-mode))

(defun vi-switch-mode (arg mode-char)
  "Switch the major mode of current buffer as specified by the following char \\{vi-tilde-map}"
  (interactive "P\nc")
  (let ((mode-cmd (lookup-key vi-tilde-map (char-to-string mode-char))))
    (if (null mode-cmd)
	(with-output-to-temp-buffer "*Help*"
	  (princ (substitute-command-keys "Possible major modes to switch to: \\{vi-tilde-map}"))
	  (with-current-buffer standard-output
	    (help-mode)))
      (setq prefix-arg arg)		; prefix arg will be passed down
      (command-execute mode-cmd nil)	; may need to save mode-line-format etc
      (force-mode-line-update))))	; just in case


(defun vi-debugging (arg)
  "Toggle debug-on-error flag.  If prefix arg is given, set t."
  (interactive "P")
  (if arg
      (setq debug-on-error t)
    (setq debug-on-error (not debug-on-error)))
  (if debug-on-error
      (message "Debug-on-error ...")
    (message "NO more debug-on-error")))

(defun vi-back-to-old-mode ()
  "Go back to the previous mode without setting up for insertion."
  (interactive)
  (if vi-mode-old-major-mode
      (progn
	(setq mode-name vi-mode-old-mode-name)
	(use-local-map vi-mode-old-local-map)
	(setq major-mode vi-mode-old-major-mode)
	(setq case-fold-search vi-mode-old-case-fold)
	(force-mode-line-update))))

(defun vi-readonly-mode ()
  "Toggle current buffer's readonly flag."
  (interactive)
  (setq buffer-read-only (not buffer-read-only)))

(defvar vi-com-map nil
   "Keymap used in Evi's command state
Command state includes most of the vi editing commands, with some Emacs
command extensions.")

(put 'vi-undefined 'suppress-keymap t)
(if vi-com-map nil
  (setq vi-com-map (make-keymap))
;;(fillarray vi-com-map 'vi-undefined)
  (define-key vi-com-map "\C-@" 'vi-mark-region) ; extension
  (define-key vi-com-map "\C-a" 'vi-ask-for-info)  ; extension
  (define-key vi-com-map "\C-b" 'vi-backward-windowful)
  (define-key vi-com-map "\C-c" 'vi-do-old-mode-C-c-command) ; extension
  (define-key vi-com-map "\C-d" 'vi-scroll-down-window)
  (define-key vi-com-map "\C-e" 'vi-expose-line-below)
  (define-key vi-com-map "\C-f" 'vi-forward-windowful)
  (define-key vi-com-map "\C-g" 'keyboard-quit)
  (define-key vi-com-map "\C-i" 'indent-relative-maybe) ; TAB
  (define-key vi-com-map "\C-j" 'vi-next-line) ; LFD
  (define-key vi-com-map "\C-k" 'vi-kill-line) ; extension
  (define-key vi-com-map "\C-l" 'recenter)
  (define-key vi-com-map "\C-m" 'vi-next-line-first-nonwhite) ; RET
  (define-key vi-com-map "\C-n" 'vi-next-line)
  (define-key vi-com-map "\C-o" 'vi-split-open-line)
  (define-key vi-com-map "\C-p" 'previous-line)
  (define-key vi-com-map "\C-q" 'vi-query-replace) ; extension
  (define-key vi-com-map "\C-r" 'vi-isearch-backward) ; modification
  (define-key vi-com-map "\C-s" 'vi-isearch-forward)  ; extension
  (define-key vi-com-map "\C-t" 'vi-transpose-objects) ; extension
  (define-key vi-com-map "\C-u" 'vi-scroll-up-window)
  (define-key vi-com-map "\C-v" 'scroll-up-command) ; extension
  (define-key vi-com-map "\C-w" 'vi-kill-region)   ; extension
  (define-key vi-com-map "\C-x" 'Control-X-prefix) ; extension
  (define-key vi-com-map "\C-y" 'vi-expose-line-above)
  (define-key vi-com-map "\C-z" 'suspend-emacs)

  (define-key vi-com-map "\e"   'ESC-prefix); C-[ (ESC)
  (define-key vi-com-map "\C-\\" 'vi-unimplemented)
  (define-key vi-com-map "\C-]" 'find-tag)
  (define-key vi-com-map "\C-^" 'vi-locate-def)  ; extension
  (define-key vi-com-map "\C-_" 'vi-undefined)

  (define-key vi-com-map " " 'forward-char)
  (define-key vi-com-map "!"  'vi-operator)
  (define-key vi-com-map "\"" 'vi-char-argument)
  (define-key vi-com-map "#"  'universal-argument) ; extension
  (define-key vi-com-map "$"  'end-of-line)
  (define-key vi-com-map "%"  'vi-find-matching-paren)
  (define-key vi-com-map "&"  'vi-unimplemented)
  (define-key vi-com-map "'"  'vi-goto-line-mark)
  (define-key vi-com-map "("  'backward-sexp)
  (define-key vi-com-map ")"  'forward-sexp)
  (define-key vi-com-map "*"  'vi-name-last-change-or-macro) ; extension
  (define-key vi-com-map "+"  'vi-next-line-first-nonwhite)
  (define-key vi-com-map ","  'vi-reverse-last-find-char)
  (define-key vi-com-map "-"  'vi-previous-line-first-nonwhite)
  (define-key vi-com-map "."  'vi-redo-last-change-command)
  (define-key vi-com-map "/"  'vi-search-forward)
  (define-key vi-com-map "0"  'beginning-of-line)

  (define-key vi-com-map "1"  'vi-digit-argument)
  (define-key vi-com-map "2"  'vi-digit-argument)
  (define-key vi-com-map "3"  'vi-digit-argument)
  (define-key vi-com-map "4"  'vi-digit-argument)
  (define-key vi-com-map "5"  'vi-digit-argument)
  (define-key vi-com-map "6"  'vi-digit-argument)
  (define-key vi-com-map "7"  'vi-digit-argument)
  (define-key vi-com-map "8"  'vi-digit-argument)
  (define-key vi-com-map "9"  'vi-digit-argument)

  (define-key vi-com-map ":"  'vi-ex-cmd)
  (define-key vi-com-map ";"  'vi-repeat-last-find-char)
  (define-key vi-com-map "<"  'vi-operator)
  (define-key vi-com-map "="  'vi-operator)
  (define-key vi-com-map ">"  'vi-operator)
  (define-key vi-com-map "?"  'vi-search-backward)
  (define-key vi-com-map "@"  'vi-call-named-change-or-macro) ; extension

  (define-key vi-com-map "A"  'vi-append-at-end-of-line)
  (define-key vi-com-map "B"  'vi-backward-blank-delimited-word)
  (define-key vi-com-map "C"  'vi-change-rest-of-line)
  (define-key vi-com-map "D"  'vi-kill-line)
  (define-key vi-com-map "E"  'vi-end-of-blank-delimited-word)
  (define-key vi-com-map "F"  'vi-backward-find-char)
  (define-key vi-com-map "G"  'vi-goto-line)
  (define-key vi-com-map "H"  'vi-home-window-line)
  (define-key vi-com-map "I"  'vi-insert-before-first-nonwhite)
  (define-key vi-com-map "J"  'vi-join-lines)
  (define-key vi-com-map "K"  'vi-undefined)
  (define-key vi-com-map "L"  'vi-last-window-line)
  (define-key vi-com-map "M"  'vi-middle-window-line)
  (define-key vi-com-map "N"  'vi-reverse-last-search)
  (define-key vi-com-map "O"  'vi-open-above)
  (define-key vi-com-map "P"  'vi-put-before)
  (define-key vi-com-map "Q"  'vi-quote-words) ; extension
  (define-key vi-com-map "R"  'vi-replace-chars)
  (define-key vi-com-map "S"  'vi-substitute-lines)
  (define-key vi-com-map "T"  'vi-backward-upto-char)
  (define-key vi-com-map "U"  'vi-unimplemented)
  (define-key vi-com-map "V"  'vi-undefined)
  (define-key vi-com-map "W"  'vi-forward-blank-delimited-word)
  (define-key vi-com-map "X"  'call-last-kbd-macro) ; modification/extension
  (define-key vi-com-map "Y"  'vi-yank-line)
  (define-key vi-com-map "Z" (make-sparse-keymap)) ;allow below prefix command
  (define-key vi-com-map "ZZ" 'vi-save-all-and-exit)

  (define-key vi-com-map "["  'vi-unimplemented)
  (define-key vi-com-map "\\" 'vi-operator) ; extension for vi-narrow-op
  (define-key vi-com-map "]"  'vi-unimplemented)
  (define-key vi-com-map "^"  'back-to-indentation)
  (define-key vi-com-map "_"  'vi-undefined)
  (define-key vi-com-map "`"  'vi-goto-char-mark)

  (define-key vi-com-map "a"  'vi-insert-after)
  (define-key vi-com-map "b"  'backward-word)
  (define-key vi-com-map "c"  'vi-operator)
  (define-key vi-com-map "d"  'vi-operator)
  (define-key vi-com-map "e"  'vi-end-of-word)
  (define-key vi-com-map "f"  'vi-forward-find-char)
  (define-key vi-com-map "g"  'vi-beginning-of-buffer) ; extension
  (define-key vi-com-map "h"  'backward-char)
  (define-key vi-com-map "i"  'vi-insert-before)
  (define-key vi-com-map "j"  'vi-next-line)
  (define-key vi-com-map "k"  'previous-line)
  (define-key vi-com-map "l"  'forward-char)
  (define-key vi-com-map "m"  'vi-set-mark)
  (define-key vi-com-map "n"  'vi-repeat-last-search)
  (define-key vi-com-map "o"  'vi-open-below)
  (define-key vi-com-map "p"  'vi-put-after)
  (define-key vi-com-map "q"  'vi-replace)
  (define-key vi-com-map "r"  'vi-replace-1-char)
  (define-key vi-com-map "s"  'vi-substitute-chars)
  (define-key vi-com-map "t"  'vi-forward-upto-char)
  (define-key vi-com-map "u"  'undo)
  (define-key vi-com-map "v"  'vi-verify-spelling)
  (define-key vi-com-map "w"  'vi-forward-word)
  (define-key vi-com-map "x"  'vi-kill-char)
  (define-key vi-com-map "y"  'vi-operator)
  (define-key vi-com-map "z"  'vi-adjust-window)

  (define-key vi-com-map "{"  'backward-paragraph)
  (define-key vi-com-map "|"  'vi-goto-column)
  (define-key vi-com-map "}"  'forward-paragraph)
  (define-key vi-com-map "~"  'vi-change-case)
  (define-key vi-com-map "\177" 'delete-backward-char))

(put 'backward-char 'point-moving-unit 'char)
(put 'vi-next-line 'point-moving-unit 'line)
(put 'next-line 'point-moving-unit 'line)
(put 'forward-line 'point-moving-unit 'line)
(put 'previous-line 'point-moving-unit 'line)
(put 'vi-isearch-backward 'point-moving-unit 'search)
(put 'vi-search-backward 'point-moving-unit 'search)
(put 'vi-isearch-forward 'point-moving-unit 'search)
(put 'vi-search-forward 'point-moving-unit 'search)
(put 'forward-char 'point-moving-unit 'char)
(put 'end-of-line 'point-moving-unit 'char)
(put 'vi-find-matching-paren 'point-moving-unit 'match)
(put 'vi-goto-line-mark 'point-moving-unit 'line)
(put 'backward-sexp 'point-moving-unit 'sexp)
(put 'forward-sexp 'point-moving-unit 'sexp)
(put 'vi-next-line-first-nonwhite 'point-moving-unit 'line)
(put 'vi-previous-line-first-nonwhite 'point-moving-unit 'line)
(put 'vi-reverse-last-find-char 'point-moving-unit 'rev-find)
(put 'vi-re-search-forward 'point-moving-unit 'search)
(put 'beginning-of-line 'point-moving-unit 'char)
(put 'vi-beginning-of-buffer 'point-moving-unit 'char)
(put 'vi-repeat-last-find-char 'point-moving-unit 'find)
(put 'vi-re-search-backward 'point-moving-unit 'search)
(put 'vi-backward-blank-delimited-word 'point-moving-unit 'WORD)
(put 'vi-end-of-blank-delimited-word 'point-moving-unit 'match)
(put 'vi-backward-find-char 'point-moving-unit 'find)
(put 'vi-goto-line 'point-moving-unit 'line)
(put 'vi-home-window-line 'point-moving-unit 'line)
(put 'vi-last-window-line 'point-moving-unit 'line)
(put 'vi-middle-window-line 'point-moving-unit 'line)
(put 'vi-reverse-last-search 'point-moving-unit 'rev-search)
(put 'vi-backward-upto-char 'point-moving-unit 'find)
(put 'vi-forward-blank-delimited-word 'point-moving-unit 'WORD)
(put 'back-to-indentation 'point-moving-unit 'char)
(put 'vi-goto-char-mark 'point-moving-unit 'char)
(put 'backward-word 'point-moving-unit 'word)
(put 'vi-end-of-word 'point-moving-unit 'match)
(put 'vi-forward-find-char 'point-moving-unit 'find)
(put 'backward-char 'point-moving-unit 'char)
(put 'vi-forward-char 'point-moving-unit 'char)
(put 'vi-repeat-last-search 'point-moving-unit 'search)
(put 'vi-forward-upto-char 'point-moving-unit 'find)
(put 'vi-forward-word 'point-moving-unit 'word)
(put 'vi-goto-column 'point-moving-unit 'match)
(put 'forward-paragraph 'point-moving-unit 'paragraph)
(put 'backward-paragraph 'point-moving-unit 'paragraph)

;;; region mark commands
(put 'mark-page 'point-moving-unit 'region)
(put 'mark-paragraph 'point-moving-unit 'region)
(put 'mark-word 'point-moving-unit 'region)
(put 'mark-sexp 'point-moving-unit 'region)
(put 'mark-defun 'point-moving-unit 'region)
(put 'mark-whole-buffer 'point-moving-unit 'region)
(put 'mark-end-of-sentence 'point-moving-unit 'region)
(put 'c-mark-function 'point-moving-unit 'region)
;;;

(defvar vi-mark-alist nil
  "Alist of (NAME . MARK), marks are local to each buffer.")

(defvar vi-scroll-amount (/ (window-height) 2)
  "Default amount of lines for scrolling (used by \"^D\"/\"^U\").")

(defvar vi-shift-width 4
  "Shift amount for \"<\"/\">\" operators.")

(defvar vi-ins-point nil		; integer
  "Last insertion point.  Should use `mark' instead.")

(defvar vi-ins-length nil		; integer
  "Length of last insertion.")

(defvar vi-ins-repetition nil		; integer
  "The repetition required for last insertion.")

(defvar vi-ins-overwrt-p nil		; boolean
  "T if last insertion was a replace actually.")

(defvar vi-ins-prefix-code nil		; ready-to-eval sexp
  "Code to be eval'ed before (redo-)insertion begins.")

(defvar vi-last-find-char nil		; cons cell
  "Save last direction, char and upto-flag used for char finding.")

(defvar vi-last-change-command nil	; cons cell
  "Save commands for redoing last changes.  Each command is in (FUNC . ARGS)
form that is ready to be `apply'ed.")

(defvar vi-last-shell-command nil	; last shell op command line
  "Save last shell command given for \"!\" operator.")

(defvar vi-insert-state nil             ; boolean
  "Non-nil if it is in insert state.")

; in "loaddefs.el"
;(defvar search-last-string ""
;  "Last string search for by a search command.")

(defvar vi-search-last-command nil	; (re-)search-forward(backward)
  "Save last search command for possible redo.")

(defvar vi-mode-old-local-map nil
  "Save the local-map used before entering vi-mode.")

(defvar vi-mode-old-mode-name nil
  "Save the mode-name before entering vi-mode.")

(defvar vi-mode-old-major-mode nil
  "Save the major-mode before entering vi-mode.")

(defvar vi-mode-old-case-fold nil)

;(defconst vi-add-to-mode-line-1
;  '(overwrite-mode nil " Insert"))

;; Value is same as vi-add-to-mode-line-1 when in vi mode,
;; but nil in other buffers.
;(defvar vi-add-to-mode-line nil)

(defun vi-mode-setup ()
  "Setup a buffer for vi-mode by creating necessary buffer-local variables."
;  (make-local-variable 'vi-add-to-mode-line)
;  (setq vi-add-to-mode-line vi-add-to-mode-line-1)
;  (or (memq vi-add-to-mode-line minor-mode-alist)
;      (setq minor-mode-alist (cons vi-add-to-mode-line minor-mode-alist)))
  (make-local-variable 'vi-scroll-amount)
  (setq vi-scroll-amount (/ (window-height) 2))
  (make-local-variable 'vi-shift-width)
  (setq vi-shift-width 4)
  (make-local-variable 'vi-ins-point)
  (make-local-variable 'vi-ins-length)
  (make-local-variable 'vi-ins-repetition)
  (make-local-variable 'vi-ins-overwrt-p)
  (make-local-variable 'vi-ins-prefix-code)
  (make-local-variable 'vi-last-change-command)
  (make-local-variable 'vi-last-shell-command)
  (make-local-variable 'vi-last-find-char)
  (make-local-variable 'vi-mark-alist)
  (make-local-variable 'vi-insert-state)
  (make-local-variable 'vi-mode-old-local-map)
  (make-local-variable 'vi-mode-old-mode-name)
  (make-local-variable 'vi-mode-old-major-mode)
  (make-local-variable 'vi-mode-old-case-fold)
  (run-mode-hooks 'vi-mode-hook))

;;;###autoload
(defun vi-mode ()
  "Major mode that acts like the `vi' editor.
The purpose of this mode is to provide you the combined power of vi (namely,
the \"cross product\" effect of commands and repeat last changes) and Emacs.

This command redefines nearly all keys to look like vi commands.
It records the previous major mode, and any vi command for input
\(`i', `a', `s', etc.) switches back to that mode.
Thus, ordinary Emacs (in whatever major mode you had been using)
is \"input\" mode as far as vi is concerned.

To get back into vi from \"input\" mode, you must issue this command again.
Therefore, it is recommended that you assign it to a key.

Major differences between this mode and real vi :

* Limitations and unsupported features
  - Search patterns with line offset (e.g. /pat/+3 or /pat/z.) are
    not supported.
  - Ex commands are not implemented; try ':' to get some hints.
  - No line undo (i.e. the 'U' command), but multi-undo is a standard feature.

* Modifications
  - The stopping positions for some point motion commands (word boundary,
    pattern search) are slightly different from standard 'vi'.
    Also, no automatic wrap around at end of buffer for pattern searching.
  - Since changes are done in two steps (deletion then insertion), you need
    to undo twice to completely undo a change command.  But this is not needed
    for undoing a repeated change command.
  - No need to set/unset 'magic', to search for a string with regular expr
    in it just put a prefix arg for the search commands.  Replace cmds too.
  - ^R is bound to incremental backward search, so use ^L to redraw screen.

* Extensions
  - Some standard (or modified) Emacs commands were integrated, such as
    incremental search, query replace, transpose objects, and keyboard macros.
  - In command state, ^X links to the 'ctl-x-map', and ESC can be linked to
    esc-map or set undefined.  These can give you the full power of Emacs.
  - See vi-com-map for those keys that are extensions to standard vi, e.g.
    `vi-name-last-change-or-macro', `vi-verify-spelling', `vi-locate-def',
    `vi-mark-region', and 'vi-quote-words'.  Some of them are quite handy.
  - Use \\[vi-switch-mode] to switch among different modes quickly.

Syntax table and abbrevs while in vi mode remain as they were in Emacs."
   (interactive)
   (if (null vi-mode-old-major-mode)	; very first call for current buffer
       (vi-mode-setup))

   (if (eq major-mode 'vi-mode)
       (progn (ding) (message "Already in vi-mode."))
     (setq vi-mode-old-local-map (current-local-map))
     (setq vi-mode-old-mode-name mode-name)
     (setq vi-mode-old-major-mode major-mode)
     (setq vi-mode-old-case-fold case-fold-search) ; this is needed !!
     (setq case-fold-search nil)	; exact case match in searching
     (use-local-map vi-com-map)
     (setq major-mode 'vi-mode)
     (setq mode-name "VI")
     (force-mode-line-update)		; force mode line update
     (if vi-insert-state	        ; this is a return from insertion
         (vi-end-of-insert-state))))

(defun vi-ding()
  "Ding !"
  (interactive)
  (ding))

(defun vi-save-all-and-exit ()
  "Save all modified buffers without asking, then exits emacs."
  (interactive)
  (save-some-buffers t)
  (kill-emacs))

;; to be used by "ex" commands
(defvar vi-replaced-string nil)
(defvar vi-replacing-string nil)

(defun vi-ex-cmd ()
  "Ex commands are not implemented in Evi mode.  For some commonly used ex
commands, you can use the following alternatives for similar effect :
w            C-x C-s (save-buffer)
wq           C-x C-c (save-buffers-kill-emacs)
w fname      C-x C-w (write-file)
e fname      C-x C-f (find-file)
r fname      C-x i   (insert-file)
s/old/new    use q (vi-replace) to do unconditional replace
             use C-q (vi-query-replace) to do query replace
set sw=n     M-x set-variable vi-shift-width n "
  (interactive)
;; (let ((cmd (read-string ":")) (lines 1))
;;  (cond ((string-match "s"))))
  (with-output-to-temp-buffer "*Help*"
    (princ (documentation 'vi-ex-cmd))
    (with-current-buffer standard-output
      (help-mode))))

(defun vi-undefined ()
  (interactive)
  (message "Command key \"%s\" is undefined in Evi."
	   (single-key-description last-command-event))
  (ding))

(defun vi-unimplemented ()
  (interactive)
  (message "Command key \"%s\" is not implemented in Evi."
	   (single-key-description last-command-event))
  (ding))

;;;;;
(defun vi-goto-insert-state (repetition &optional prefix-code do-it-now-p)
  "Go into insert state, the text entered will be repeated if REPETITION > 1.
If PREFIX-CODE is given, do it before insertion begins if DO-IT-NOW-P is T.
In any case, the prefix-code will be done before each 'redo-insert'.
This function expects `overwrite-mode' being set properly beforehand."
  (if do-it-now-p (apply (car prefix-code) (cdr prefix-code)))
  (setq vi-ins-point (point))
  (setq vi-ins-repetition repetition)
  (setq vi-ins-prefix-code prefix-code)
  (setq mode-name vi-mode-old-mode-name)
  (setq case-fold-search vi-mode-old-case-fold)
  (use-local-map vi-mode-old-local-map)
  (setq major-mode vi-mode-old-major-mode)
  (force-mode-line-update)
  (setq vi-insert-state t))

(defun vi-end-of-insert-state ()
  "Terminate insertion and set up last change command."
  (if (or (< (point) vi-ins-point)    ;Check if there is any effective change
	  (and (= (point) vi-ins-point) (null vi-ins-prefix-code))
	  (<= vi-ins-repetition 0))
      (vi-goto-command-state t)
    (if (> vi-ins-repetition 1)
	(progn
	  (let ((str (buffer-substring vi-ins-point (point))))
	    (while (> vi-ins-repetition 1)
	      (insert str)
	      (setq vi-ins-repetition (1- vi-ins-repetition))))))
    (vi-set-last-change-command 'vi-first-redo-insertion vi-ins-point (point)
			     overwrite-mode vi-ins-prefix-code)
    (vi-goto-command-state t)))

(defun vi-first-redo-insertion (begin end &optional overwrite-p prefix-code)
  "Redo last insertion the first time.  Extract the string and save it for
future redoes.  Do prefix-code if it's given, use overwrite mode if asked."
  (let ((str (buffer-substring begin end)))
    (if prefix-code (apply (car prefix-code) (cdr prefix-code)))
    (if overwrite-p (delete-region (point) (+ (point) (length str))))
    (insert str)
    (vi-set-last-change-command 'vi-more-redo-insertion str overwrite-p prefix-code)))

(defun vi-more-redo-insertion (str &optional overwrite-p prefix-code)
  "Redo more insertion : copy string from STR to point, use overwrite mode
if overwrite-p is T; apply prefix-code first if it's non-nil."
  (if prefix-code (apply (car prefix-code) (cdr prefix-code)))
  (if overwrite-p (delete-region (point) (+ (point) (length str))))
  (insert str))

(defun vi-goto-command-state (&optional from-insert-state-p)
  "Go to vi-mode command state.  If optional arg exists, means return from
insert state."
  (use-local-map vi-com-map)
  (setq vi-insert-state nil)
  (if from-insert-state-p
      (if overwrite-mode
	  (overwrite-mode 0)
;	(set-minor-mode 'ins "Insert" nil)
	)))

(defun vi-kill-line (arg)
   "kill specified number of lines (=d$), text saved in the kill ring."
   (interactive "*P")
   (kill-line arg)
   (vi-set-last-change-command 'kill-line arg))

(defun vi-kill-region (start end)
  (interactive "*r")
  (kill-region start end)
  (vi-set-last-change-command 'kill-region))

(defun vi-append-at-end-of-line (arg)
   "go to end of line and then go into vi insert state."
   (interactive "*p")
   (vi-goto-insert-state arg '(end-of-line) t))

(defun vi-change-rest-of-line (arg)
  "Change the rest of (ARG) lines (= c$ in vi)."
  (interactive "*P")
  (vi-goto-insert-state 1 (list 'kill-line arg) t))

(defun vi-insert-before-first-nonwhite (arg)
  "(= ^i in vi)"
  (interactive "*p")
  (vi-goto-insert-state arg '(back-to-indentation) t))

(defun vi-open-above (arg)
  "open new line(s) above current line and enter insert state."
  (interactive "*p")
  (vi-goto-insert-state 1
			(list (function (lambda (x)
					  (or (beginning-of-line)
					      (open-line x)))) arg)
			t))

(defun vi-open-below (arg)
  "open new line(s) and go into insert mode on the last line."
  (interactive "*p")
  (vi-goto-insert-state 1
			(list (function (lambda (x)
					  (or (end-of-line)
					    (open-line x)
					    (forward-line x)))) arg)
			t))

(defun vi-insert-after (arg)
   "start vi insert state after cursor."
   (interactive "*p")
   (vi-goto-insert-state arg
			 (list (function (lambda ()
					   (if (not (eolp)) (forward-char)))))
			 t))

(defun vi-insert-before (arg)
  "enter insert state before the cursor."
  (interactive "*p")
  (vi-goto-insert-state arg))

(defun vi-goto-line (arg)
   "Go to ARGth line."
   (interactive "P")
   (if (null (vi-raw-numeric-prefix arg))
       (with-no-warnings
	 (end-of-buffer))
     (with-no-warnings (goto-line (vi-prefix-numeric-value arg)))))

(defun vi-beginning-of-buffer ()
  "Move point to the beginning of current buffer."
  (interactive)
  (goto-char (point-min)))

;;;;; not used now
;;(defvar regexp-search t		; string
;;  "*T if search string can contain regular expressions. (= set magic in vi)")
;;;;;

(defun vi-isearch-forward (arg)
  "Incremental search forward.  Use regexp version if ARG is non-nil."
  (interactive "P")
  (let ((scmd (if arg 'isearch-forward-regexp 'isearch-forward))
	(opoint (point)))
    (call-interactively scmd)
    (if (= opoint (point))
	nil
      (setq vi-search-last-command (if arg 're-search-forward 'search-forward)))))

(defun vi-isearch-backward (arg)
  "Incremental search backward.  Use regexp version if ARG is non-nil."
  (interactive "P")
  (let ((scmd (if arg 'isearch-backward-regexp 'isearch-backward))
	(opoint (point)))
    (call-interactively scmd)
    (if (= opoint (point))
	nil
      (setq vi-search-last-command (if arg 're-search-backward 'search-backward)))))

(defun vi-search-forward (arg string)
   "Nonincremental search forward. Use regexp version if ARG is non-nil."
   (interactive (if current-prefix-arg
		    (list t (read-string "regexp/" nil))
		  (list nil (read-string "/" nil))))
   (setq vi-search-last-command (if arg 're-search-forward 'search-forward))
   (if (> (length string) 0)
       (isearch-update-ring string arg))
   (funcall vi-search-last-command string nil nil 1))

(defun vi-search-backward (arg string)
   "Nonincremental search backward.  Use regexp version if ARG is non-nil."
   (interactive (if current-prefix-arg
		    (list t (read-string "regexp?" nil))
		  (list nil (read-string "?" nil))))
   (setq vi-search-last-command (if arg 're-search-backward 'search-backward))
   (if (> (length string) 0)
       (isearch-update-ring string arg))
   (funcall vi-search-last-command string nil nil 1))

(defun vi-repeat-last-search (arg &optional search-command search-string)
  "Repeat last search command.
If optional search-command/string are given,
use those instead of the ones saved."
  (interactive "p")
  (if (null search-command) (setq search-command vi-search-last-command))
  (if (null search-string)
      (setq search-string
	    (car (if (memq search-command
			   '(re-search-forward re-search-backward))
		     regexp-search-ring
		   search-ring))))
  (if (null search-command)
      (progn (ding) (message "No last search command to repeat."))
    (funcall search-command search-string nil nil arg)))

(defun vi-reverse-last-search (arg &optional search-command search-string)
  "Redo last search command in reverse direction.
If the optional search args are given, use those instead of the ones saved."
  (interactive "p")
  (if (null search-command) (setq search-command vi-search-last-command))
  (if (null search-string)
      (setq search-string
	    (car (if (memq search-command
			   '(re-search-forward re-search-backward))
		     regexp-search-ring
		   search-ring))))
  (if (null search-command)
      (progn (ding) (message "No last search command to repeat."))
    (funcall (cond ((eq search-command 're-search-forward) 're-search-backward)
		   ((eq search-command 're-search-backward) 're-search-forward)
		   ((eq search-command 'search-forward) 'search-backward)
		   ((eq search-command 'search-backward) 'search-forward))
	     search-string nil nil arg)))

(defun vi-join-lines (arg)
   "join ARG lines from current line (default 2), cleaning up white space."
   (interactive "P")
   (if (null (vi-raw-numeric-prefix arg))
       (delete-indentation t)
     (let ((count (vi-prefix-numeric-value arg)))
       (while (>= count 2)
	 (delete-indentation t)
	 (setq count (1- count)))))
   (vi-set-last-change-command 'vi-join-lines arg))

(defun vi-backward-kill-line ()
   "kill the current line.  Only works in insert state."
   (interactive)
   (if (not vi-insert-state)
       nil
     (beginning-of-line 1)
     (kill-line nil)))

(defun vi-abort-ins ()
  "abort insert state, kill inserted text and go back to command state."
  (interactive)
  (if (not vi-insert-state)
      nil
    (if (> (point) vi-ins-point)
	   (kill-region vi-ins-point (point)))
    (vi-goto-command-state t)))

(defun vi-backward-windowful (count)
  "Backward COUNT windowfuls. Default is one."
  (interactive "p")
; (set-mark-command nil)
  (while (> count 0)
    (scroll-down nil)
    (setq count (1- count))))

(defun vi-scroll-down-window (count)
  "Scrolls down window COUNT lines.
If COUNT is nil (actually, non-integer), scrolls default amount.
The given COUNT is remembered for future scrollings."
  (interactive "P")
  (if (integerp count)
      (setq vi-scroll-amount count))
  (scroll-up vi-scroll-amount))

(defun vi-expose-line-below (count)
  "Expose COUNT more lines below the current window.  Default COUNT is 1."
  (interactive "p")
  (scroll-up count))

(defun vi-forward-windowful (count)
  "Forward COUNT windowfuls. Default is one."
  (interactive "p")
; (set-mark-command nil)
  (while (> count 0)
    (scroll-up nil)
    (setq count (1- count))))

(defun vi-next-line (count)
  "Go down count lines, try to keep at the same column."
  (interactive "p")
  (setq this-command 'next-line)	; this is a needed trick
  (if (= (point) (progn (line-move count) (point)))
      (ding)				; no moving, already at end of buffer
    (setq last-command 'next-line)))

(defun vi-next-line-first-nonwhite (count)
  "Go down COUNT lines.  Stop at first non-white."
  (interactive "p")
  (if (= (point) (progn (forward-line count) (back-to-indentation) (point)))
      (ding)))				; no moving, already at end of buffer

(defun vi-previous-line-first-nonwhite (count)
  "Go up COUNT lines.  Stop at first non-white."
  (interactive "p")
  (forward-line (- count))
  (back-to-indentation))

(defun vi-scroll-up-window (count)
  "Scrolls up window COUNT lines.
If COUNT is nil (actually, non-integer), scrolls default amount.
The given COUNT is remembered for future scrollings."
  (interactive "P")
  (if (integerp count)
      (setq vi-scroll-amount count))
  (scroll-down vi-scroll-amount))

(defun vi-expose-line-above (count)
  "Expose COUNT more lines above the current window.  Default COUNT is 1."
  (interactive "p")
  (scroll-down count))

(defun vi-char-argument (arg)
  "Get following character (could be any CHAR) as part of the prefix argument.
Possible prefix-arg cases are nil, INTEGER, (nil . CHAR) or (INTEGER . CHAR)."
  (interactive "P")
  (let ((char (read-char)))
    (cond ((null arg) (setq prefix-arg (cons nil char)))
	  ((integerp arg) (setq prefix-arg (cons arg char)))
	  ; This can happen only if the user changed his/her mind for CHAR,
	  ; Or there are some leading "universal-argument"s
	  (t (setq prefix-arg (cons (car arg) char))))))

(defun vi-goto-mark (mark-char &optional line-flag)
  "Go to marked position or line (if line-flag is given).
Goto mark '@' means jump into and pop the top mark on the mark ring."
  (cond ((char-equal mark-char last-command-event)	; `` or ''
	 (exchange-point-and-mark) (if line-flag (back-to-indentation)))
	((char-equal mark-char ?@)	; jump and pop mark
	 (set-mark-command t) (if line-flag (back-to-indentation)))
	(t
	 (let ((mark (vi-get-mark mark-char)))
	   (if (null mark)
	       (progn (vi-ding) (message "Mark register undefined."))
	     (set-mark-command nil)
	     (goto-char mark)
	     (if line-flag (back-to-indentation)))))))

(defun vi-goto-line-mark (char)
  "Go to the line (at first non-white) marked by next char."
  (interactive "c")
  (vi-goto-mark char t))

(defun vi-goto-char-mark (char)
  "Go to the char position marked by next mark-char."
  (interactive "c")
  (vi-goto-mark char))

(defun vi-digit-argument (arg)
  "Set numeric prefix argument."
  (interactive "P")
  (cond ((null arg) (digit-argument arg))
	((integerp arg) (digit-argument nil)
	                 (setq prefix-arg (* prefix-arg arg)))
	(t (digit-argument nil)		; in (NIL . CHAR) or (NUM . CHAR) form
	   (setq prefix-arg (cons (* prefix-arg
				     (if (null (car arg)) 1 (car arg)))
				  (cdr arg))))))

(defun vi-raw-numeric-prefix (arg)
  "Return the raw value of numeric part prefix argument."
  (if (consp arg) (car arg) arg))

(defun vi-prefix-numeric-value (arg)
  "Return numeric meaning of the raw prefix argument.  This is a modification
to the standard one provided in `callint.c' to handle (_ . CHAR) cases."
  (cond ((null arg) 1)
	((integerp arg) arg)
	((consp arg) (if (car arg) (car arg) 1))))

(defun vi-reverse-last-find-char (count &optional find-arg)
  "Reverse last f F t T operation COUNT times.  If the optional FIND-ARG
is given, it is used instead of the saved one."
  (interactive "p")
  (if (null find-arg) (setq find-arg vi-last-find-char))
  (if (null find-arg)
      (progn (ding) (message "No last find char to repeat."))
    (vi-find-char (cons (* (car find-arg) -1) (cdr find-arg)) count))) ;6/13/86

(defun vi-find-char (arg count)
  "Find in DIRECTION (1/-1) for CHAR of COUNT'th times on current line.
If UPTO-FLAG is T, stop before the char. ARG = (DIRECTION.CHAR.UPTO-FLAG."
  (let* ((direction (car arg)) (char (car (cdr arg)))
	 (upto-flag (cdr (cdr arg))) (pos (+ (point) direction)))
    (if (catch 'exit-find-char
	  (while t
	    (cond ((null (char-after pos)) (throw 'exit-find-char nil))
		  ((char-equal (char-after pos) ?\n) (throw 'exit-find-char nil))
		  ((char-equal char (char-after pos)) (setq count (1- count))
		   (if (= count 0)
		       (throw 'exit-find-char
			      (if upto-flag
				  (setq pos (- pos direction))
				pos)))))
	    (setq pos (+ pos direction))))
      (goto-char pos)
    (ding))))

(defun vi-repeat-last-find-char (count &optional find-arg)
  "Repeat last f F t T operation COUNT times.  If optional FIND-ARG is given,
it is used instead of the saved one."
  (interactive "p")
  (if (null find-arg) (setq find-arg vi-last-find-char))
  (if (null find-arg)
      (progn (ding) (message "No last find char to repeat."))
    (vi-find-char find-arg count)))

(defun vi-backward-find-char (count char)
  "Find the COUNT'th CHAR backward on current line."
  (interactive "p\nc")
  (setq vi-last-find-char (cons -1 (cons char nil)))
  (vi-repeat-last-find-char count))

(defun vi-forward-find-char (count char)
  "Find the COUNT'th CHAR forward on current line."
  (interactive "p\nc")
  (setq vi-last-find-char (cons 1 (cons char nil)))
  (vi-repeat-last-find-char count))

(defun vi-backward-upto-char (count char)
  "Find upto the COUNT'th CHAR backward on current line."
  (interactive "p\nc")
  (setq vi-last-find-char (cons -1 (cons char t)))
  (vi-repeat-last-find-char count))

(defun vi-forward-upto-char (count char)
  "Find upto the COUNT'th CHAR forward on current line."
  (interactive "p\nc")
  (setq vi-last-find-char (cons 1 (cons char t)))
  (vi-repeat-last-find-char count))

(defun vi-end-of-word (count)
  "Move forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (if (not (eobp)) (forward-char))
  (if (re-search-forward "\\W*\\w+\\>" nil t count)
      (backward-char)))

(defun vi-replace-1-char (count char)
  "Replace char after point by CHAR.  Repeat COUNT times."
  (interactive "p\nc")
  (delete-char count nil)       ; don't save in kill ring
  (setq last-command-event char)
  (self-insert-command count)
  (vi-set-last-change-command 'vi-replace-1-char count char))

(defun vi-replace-chars (arg)
  "Replace chars over old ones."
  (interactive "*p")
  (overwrite-mode 1)
  (vi-goto-insert-state arg))

(defun vi-substitute-chars (count)
  "Substitute COUNT chars by the input chars, enter insert state."
  (interactive "*p")
  (vi-goto-insert-state 1 (list (function (lambda (c) ; this is a bit tricky
					    (delete-region (point)
							   (+ (point) c))))
				count) t))

(defun vi-substitute-lines (count)
  "Substitute COUNT lines by the input chars. (=cc in vi)"
  (interactive "*p")
  (vi-goto-insert-state 1 (list 'vi-delete-op 'next-line (1- count)) t))

(defun vi-prefix-char-value (arg)
  "Get the char part of the current prefix argument."
  (cond ((null arg) nil)
	((integerp arg) nil)
	((consp arg) (cdr arg))
	(t nil)))

(defun vi-operator (arg)
  "Handling vi operators (d/c/</>/!/=/y).  Current implementation requires
the key bindings of the operators being fixed."
  (interactive "P")
  (catch 'vi-exit-op
    (let ((this-op-char last-command-event))
      (setq last-command-event (read-char))
      (setq this-command (lookup-key vi-com-map (char-to-string last-command-event)))
      (if (not (eq this-command 'vi-digit-argument))
	  (setq prefix-arg arg)
	(vi-digit-argument arg)
	(setq last-command-event (read-char))
	(setq this-command (lookup-key vi-com-map (char-to-string last-command-event))))
      (cond ((char-equal this-op-char last-command-event) ; line op
	     (vi-execute-op this-op-char 'next-line
			    (cons (1- (vi-prefix-numeric-value prefix-arg))
				  (vi-prefix-char-value prefix-arg))))
	    ;; We assume any command that has no property 'point-moving-unit'
	    ;; as having that property with the value 'CHAR'.  3/12/86
	    (t   ;;  (get this-command 'point-moving-unit)
	     (vi-execute-op this-op-char this-command prefix-arg))))))
	    ;;   (t (throw 'vi-exit-op (ding)))))))

(defun vi-execute-op (op-char motion-command arg)
  "Execute vi edit operator as specified by OP-CHAR, the operand is the region
determined by the MOTION-COMMAND with ARG."
  (cond ((= op-char ?d)
	 (if (vi-delete-op motion-command arg)
	     (vi-set-last-change-command 'vi-delete-op (vi-repeat-command-of motion-command) arg)))
	 ((= op-char ?c)
	  (if (vi-delete-op motion-command arg)
	      (vi-goto-insert-state 1 (list 'vi-delete-op
			      (vi-repeat-command-of motion-command) arg) nil)))
	 ((= op-char ?y)
	  (if (vi-yank-op motion-command arg)
	      (vi-set-last-change-command 'vi-yank-op (vi-repeat-command-of motion-command) arg)))
	 ((= op-char ?!)
	  (if (vi-shell-op motion-command arg)
	      (vi-set-last-change-command 'vi-shell-op (vi-repeat-command-of motion-command) arg vi-last-shell-command)))
	 ((= op-char ?<)
	  (if (vi-shift-op motion-command arg (- vi-shift-width))
	      (vi-set-last-change-command 'vi-shift-op (vi-repeat-command-of motion-command) arg (- vi-shift-width))))
	 ((= op-char ?>)
	  (if (vi-shift-op motion-command arg vi-shift-width)
	      (vi-set-last-change-command 'vi-shift-op (vi-repeat-command-of motion-command) arg vi-shift-width)))
	 ((= op-char ?=)
	  (if (vi-indent-op motion-command arg)
	      (vi-set-last-change-command 'vi-indent-op (vi-repeat-command-of motion-command) arg)))
	 ((= op-char ?\\)
	  (vi-narrow-op motion-command arg))))

(defun vi-repeat-command-of (command)
  "Return the command for redo the given command."
  (let ((cmd-type (get command 'point-moving-unit)))
    (cond ((eq cmd-type 'search) 'vi-repeat-last-search)
	  ((eq cmd-type 'find) 'vi-repeat-last-find-char)
	  (t command))))

(defun vi-effective-range (motion-command arg)
  "Return (begin . end) of the range spanned by executing the given
MOTION-COMMAND with ARG.
   MOTION-COMMAND in ready-to-eval list form is not yet supported."
  (save-excursion
    (let ((begin (point)) end opoint
	  (moving-unit (get motion-command 'point-moving-unit)))
      (setq prefix-arg arg)
      (setq opoint (point))
      (command-execute motion-command nil)
;; Check if there is any effective motion. Note that for single line operation
;; the motion-command causes no effective point movement (since it moves up or
;; down zero lines), but it should be counted as effectively moved.
      (if (and (= (point) opoint) (not (eq moving-unit 'line)))
	  (cons opoint opoint)		; no effective motion
	(if (eq moving-unit 'region)
	    (setq begin (or (mark) (point))))
	(if (<= begin (point))
	    (setq end (point))
	  (setq end begin)
	  (setq begin (point)))
	(cond ((or (eq moving-unit 'match) (eq moving-unit 'find))
	       (setq end (1+ end)))
	      ((eq moving-unit 'line)
	       (goto-char begin) (beginning-of-line) (setq begin (point))
	       (goto-char end) (forward-line 1) (beginning-of-line) (setq end (point))))
	(if (> end (point-max)) (setq end (point-max))) ; force in buffer region
	(cons begin end)))))

(defun vi-delete-op (motion-command arg)
  "Delete range specified by MOTION-COMMAND with ARG."
  (let* ((range (vi-effective-range motion-command arg))
	 (begin (car range)) (end (cdr range)) reg)
    (if (= begin end)
	nil				; point not moved, abort op
      (setq reg (vi-prefix-char-value arg))
      (if (null reg)
	  (kill-region begin end)	; kill ring as unnamed registers
	(if (and (>= reg ?A) (<= reg ?Z))
	    (append-to-register (downcase reg) begin end t)
	  (copy-to-register reg begin end t)))
      t)))

(defun vi-yank-op (motion-command arg)
  "Yank (in vi sense) range specified by MOTION-COMMAND with ARG."
  (let* ((range (vi-effective-range motion-command arg))
	 (begin (car range)) (end (cdr range)) reg)
    (if (= begin end)
	nil				; point not moved, abort op
      (setq reg (vi-prefix-char-value arg))
      (if (null reg)
	  (copy-region-as-kill begin end); kill ring as unnamed registers
	(if (and (>= reg ?A) (<= reg ?Z))
	    (append-to-register (downcase reg) begin end nil)
	  (copy-to-register reg begin end nil)))
      t)))

(defun vi-yank-line (arg)
  "Yank (in vi sense) lines (= `yy' command)."
  (interactive "*P")
  (setq arg (cons (1- (vi-prefix-numeric-value arg)) (vi-prefix-char-value arg)))
  (if (vi-yank-op 'next-line arg)
      (vi-set-last-change-command 'vi-yank-op 'next-line arg)))

(defun vi-string-end-with-nl-p (string)
  "See if STRING ends with a newline char.
Used in checking whether the yanked text should be put back as lines or not."
  (= (aref string (1- (length string))) ?\n))

(defun vi-put-before (arg &optional after-p)
  "Put yanked (in vi sense) text back before/above cursor.
If a numeric prefix value (currently it should be >1) is given, put back
text as lines.  If the optional after-p is given, put after/below the cursor."
  (interactive "P")
  (let ((reg (vi-prefix-char-value arg)) put-text)
    (if (and reg (or (< reg ?1) (> reg ?9)) (null (get-register reg)))
	(error "Nothing in register %c" reg)
      (if (null reg) (setq reg ?1))	; the default is the last text killed
      (setq put-text
	    (cond
	     ((and (>= reg ?1) (<= reg ?9))
	      (setq this-command 'yank) ; So we may yank-pop !!
	      (current-kill (- reg ?0 1) 'do-not-rotate))
	     ((stringp (get-register reg)) (get-register reg))
	     (t (error "Register %c is not containing text string" reg))))
      (if (vi-string-end-with-nl-p put-text) ; put back text as lines
	  (if after-p
	      (progn (forward-line 1) (beginning-of-line))
	    (beginning-of-line))
	(if after-p (forward-char 1)))
      (push-mark (point))
      (insert put-text)
      (exchange-point-and-mark)
;;    (back-to-indentation)      ; this is not allowed if we allow yank-pop
      (vi-set-last-change-command 'vi-put-before arg after-p))))

(defun vi-put-after (arg)
  "Put yanked (in vi sense) text back after/below cursor."
  (interactive "P")
  (vi-put-before arg t))

(defun vi-shell-op (motion-command arg &optional shell-command)
  "Perform shell command (as filter).
Performs command on range specified by MOTION-COMMAND
with ARG. If SHELL-COMMAND is not given, ask for one from minibuffer.
If char argument is given, it directs the output to a *temp* buffer."
  (let* ((range (vi-effective-range motion-command arg))
	 (begin (car range)) (end (cdr range)))
    (if (= begin end)
	nil				; point not moved, abort op
      (cond ((null shell-command)
	     (setq shell-command (read-string "!" nil))
	     (setq vi-last-shell-command shell-command)))
      (shell-command-on-region begin end shell-command (not (vi-prefix-char-value arg)))
      t)))

(defun vi-shift-op (motion-command arg amount)
  "Perform shift command on range specified by MOTION-COMMAND with ARG for
AMOUNT on each line.  Negative amount means shift left.
SPECIAL FEATURE: char argument can be used to specify shift amount(1-9)."
  (let* ((range (vi-effective-range motion-command arg))
	 (begin (car range)) (end (cdr range)))
    (if (= begin end)
	nil				; point not moved, abort op
      (if (vi-prefix-char-value arg)
	  (setq amount (if (> amount 0)
			   (- (vi-prefix-char-value arg) ?0)
			 (- ?0 (vi-prefix-char-value arg)))))
      (indent-rigidly begin end amount)
      t)))

(defun vi-indent-op (motion-command arg)
  "Perform indent command on range specified by MOTION-COMMAND with ARG."
  (let* ((range (vi-effective-range motion-command arg))
	 (begin (car range)) (end (cdr range)))
    (if (= begin end)
	nil				; point not moved, abort op
      (indent-region begin end nil)	; insert TAB as indent command
      t)))

(defun vi-narrow-op (motion-command arg)
  "Narrow to region specified by MOTION-COMMAND with ARG."
  (let* ((range (vi-effective-range motion-command arg))
	 (begin (car range)) (end (cdr range)) reg)
    (if (= begin end)
	nil				; point not moved, abort op
      (narrow-to-region begin end))))

(defun vi-get-mark (char)
  "Return contents of vi mark register named CHAR, or nil if undefined."
  (cdr (assq char vi-mark-alist)))

(defun vi-set-mark (char)
  "Set contents of vi mark register named CHAR to current point.
'@' is the special anonymous mark register."
  (interactive "c")
  (if (char-equal char ?@)
      (set-mark-command nil)
    (let ((aelt (assq char vi-mark-alist)))
      (if aelt
	  (move-marker (cdr aelt) (point)) ; fixed 6/12/86
	(setq aelt (cons char (copy-marker (point))))
	(setq vi-mark-alist (cons aelt vi-mark-alist))))))

(defun vi-find-matching-paren ()
  "Locate the matching paren.  It's a hack right now."
  (interactive)
  (cond ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
	((looking-at "[])}]") (forward-char 1) (backward-sexp 1))
        (t (ding))))

(defun vi-backward-blank-delimited-word (count)
  "Backward COUNT blank-delimited words."
  (interactive "p")
  (if (re-search-backward "[ \t\n\`][^ \t\n\`]+" nil t count)
      (if (not (bobp)) (forward-char 1))))

(defun vi-forward-blank-delimited-word (count)
  "Forward COUNT blank-delimited words."
  (interactive "p")
  (if (re-search-forward "[^ \t\n]*[ \t\n]+[^ \t\n]" nil t count)
      (if (not (eobp)) (backward-char 1))))

(defun vi-end-of-blank-delimited-word (count)
  "Forward to the end of the COUNT'th blank-delimited word."
  (interactive "p")
  (if (re-search-forward "[^ \t\n\']+[ \t\n\']" nil t count)
      (if (not (eobp)) (backward-char 2))))

(defun vi-home-window-line (arg)
  "To window home or arg'th line from the top of the window."
  (interactive "p")
  (move-to-window-line (1- arg))
  (back-to-indentation))

(defun vi-last-window-line (arg)
  "To window last line or arg'th line from the bottom of the window."
  (interactive "p")
  (move-to-window-line (- arg))
  (back-to-indentation))

(defun vi-middle-window-line ()
  "To the middle line of the window."
  (interactive)
  (move-to-window-line nil)
  (back-to-indentation))

(defun vi-forward-word (count)
  "Stop at the beginning of the COUNT'th words from point."
  (interactive "p")
  (if (re-search-forward "\\w*\\W+\\<" nil t count)
      t
    (vi-ding)))

(defun vi-set-last-change-command (fun &rest args)
  "Set (FUN . ARGS) as the `last-change-command'."
  (setq vi-last-change-command (cons fun args)))

(defun vi-redo-last-change-command (count &optional command)
  "Redo last change command COUNT times.  If the optional COMMAND is given,
it is used instead of the current `last-change-command'."
  (interactive "p")
  (if (null command)
      (setq command vi-last-change-command))
  (if (null command)
      (message "No last change command available.")
    (while (> count 0)
      (apply (car command) (cdr command))
      (setq count (1- count)))))

(defun vi-kill-char (count)
  "Kill COUNT chars from current point."
  (interactive "*p")
  (delete-char count t)			; save in kill ring
  (vi-set-last-change-command 'delete-char count t))

(defun vi-transpose-objects (arg unit)
  "Transpose objects.
The following char specifies unit of objects to be
transposed -- \"c\" for chars, \"l\" for lines, \"w\" for words, \"s\" for
 sexp, \"p\" for paragraph.
For the use of the prefix-arg, refer to individual functions called."
  (interactive "*P\nc")
  (if (char-equal unit ??)
      (progn
	(message "Transpose: c(har), l(ine), p(aragraph), s(-exp), w(ord),")
	(setq unit (read-char))))
  (vi-set-last-change-command 'vi-transpose-objects arg unit)
  (cond ((char-equal unit ?c) (transpose-chars arg))
	((char-equal unit ?l) (transpose-lines (vi-prefix-numeric-value arg)))
	((char-equal unit ?p) (transpose-paragraphs (vi-prefix-numeric-value arg)))
	((char-equal unit ?s) (transpose-sexps (vi-prefix-numeric-value arg)))
	((char-equal unit ?w) (transpose-words (vi-prefix-numeric-value arg)))
	(t (vi-transpose-objects arg ??))))

(defun vi-query-replace (arg)
  "Query replace, use regexp version if ARG is non-nil."
  (interactive "*P")
  (let ((rcmd (if arg 'query-replace-regexp 'query-replace)))
    (call-interactively rcmd nil)))

(defun vi-replace (arg)
  "Replace strings, use regexp version if ARG is non-nil."
  (interactive "*P")
  (let ((rcmd (if arg 'replace-regexp 'replace-string)))
    (call-interactively rcmd nil)))

(defun vi-adjust-window (arg position)
  "Move current line to the top/center/bottom of the window."
  (interactive "p\nc")
  (cond ((char-equal position ?\r) (recenter 0))
	((char-equal position ?-) (recenter -1))
	((char-equal position ?.) (recenter (/ (window-height) 2)))
	(t (message "Move current line to: \\r(top) -(bottom) .(middle)")
	   (setq position (read-char))
	   (vi-adjust-window arg position))))

(defun vi-goto-column (col)
  "Go to given column of the current line."
  (interactive "p")
  (let ((opoint (point)))
    (beginning-of-line)
    (while (> col 1)
      (if (eolp)
	  (setq col 0)
	(forward-char 1)
	(setq col (1- col))))
    (if (= col 1)
	t
      (goto-char opoint)
      (ding))))

(defun vi-name-last-change-or-macro (arg char)
  "Give name to the last change command or just defined kbd macro.
If prefix ARG is given, name last macro, otherwise name last change command.
The following CHAR will be the name for the command or macro."
  (interactive "P\nc")
  (if arg
      (name-last-kbd-macro (intern (char-to-string char)))
    (if (eq (car vi-last-change-command) 'vi-first-redo-insertion)
	(let* ((args (cdr vi-last-change-command)) ; save the insertion text
	       (str (buffer-substring (nth 0 args) (nth 1 args)))
	       (overwrite-p (nth 2 args))
	       (prefix-code (nth 3 args)))
	  (vi-set-last-change-command 'vi-more-redo-insertion str
				   overwrite-p prefix-code)))
    (fset (intern (char-to-string char)) vi-last-change-command)))

(defun vi-call-named-change-or-macro (count char)
  "Execute COUNT times the keyboard macro definition named by the following CHAR."
  (interactive "p\nc")
  (if (stringp (symbol-function (intern (char-to-string char))))
      (execute-kbd-macro (intern (char-to-string char)) count)
    (vi-redo-last-change-command count (symbol-function (intern (char-to-string char))))))

(defun vi-change-case (arg)		; could be made as an operator ?
  "Change the case of the char after point."
  (interactive "*p")
  (catch 'exit
    (if (looking-at "[a-z]")
	(upcase-region (point) (+ (point) arg))
      (if (looking-at "[A-Z]")
	  (downcase-region (point) (+ (point) arg))
	(ding)
	(throw 'exit nil)))
    (vi-set-last-change-command 'vi-change-case arg) ;should avoid redundant save
    (forward-char arg)))

(defun vi-ask-for-info (char)
  "Inquire status info. The next CHAR will specify the particular info requested."
  (interactive "c")
  (cond ((char-equal char ?l) (what-line))
	((char-equal char ?c) (what-cursor-position))
	((char-equal char ?p) (what-page))
	(t (message "Ask for: l(ine number), c(ursor position), p(age number)")
	   (setq char (read-char))
	   (vi-ask-for-info char))))

(declare-function c-mark-function "cc-cmds" ())

(defun vi-mark-region (arg region)
  "Mark region appropriately.  The next char REGION is d(efun),s(-exp),b(uffer),
p(aragraph), P(age), f(unction in C/Pascal etc.), w(ord), e(nd of sentence),
l(ines)."
  (interactive "p\nc")
  (cond ((char-equal region ?d) (mark-defun))
	((char-equal region ?s) (mark-sexp arg))
	((char-equal region ?b) (mark-whole-buffer))
	((char-equal region ?p) (mark-paragraph))
	((char-equal region ?P) (mark-page arg))
	((char-equal region ?f) (c-mark-function))
	((char-equal region ?w) (mark-word arg))
	((char-equal region ?e) (mark-end-of-sentence arg))
	((char-equal region ?l) (vi-mark-lines arg))
	(t (message "Mark: d(efun),s(-exp),b(uf),p(arag),P(age),f(unct),w(ord),e(os),l(ines)")
	   (setq region (read-char))
	   (vi-mark-region arg region))))

(defun vi-mark-lines (num)
  "Mark NUM of lines from current line as current region."
  (beginning-of-line 1)
  (push-mark)
  (end-of-line num))

(defun vi-verify-spelling (arg unit)
  "Verify spelling for the objects specified by char UNIT : [b(uffer),
r(egion), s(tring), w(ord) ]."
  (interactive "P\nc")
  (setq prefix-arg arg)			; seems not needed
  (cond ((char-equal unit ?b) (call-interactively 'spell-buffer))
	((char-equal unit ?r) (call-interactively 'spell-region))
	((char-equal unit ?s) (call-interactively 'spell-string))
	((char-equal unit ?w) (call-interactively 'spell-word))
	(t (message "Spell check: b(uffer), r(egion), s(tring), w(ord)")
	   (setq unit (read-char))
	   (vi-verify-spelling arg unit))))

(defun vi-do-old-mode-C-c-command (arg)
  "This is a hack for accessing mode specific C-c commands in vi-mode."
  (interactive "P")
  (let ((cmd (lookup-key vi-mode-old-local-map
			 (concat "\C-c" (char-to-string (read-char))))))
    (if (catch 'exit-vi-mode	; kludge hack due to dynamic binding
				; of case-fold-search
	  (if (null cmd)
	      (progn (ding) nil)
	    (let ((case-fold-search vi-mode-old-case-fold)) ; a hack
	      (setq prefix-arg arg)
	      (command-execute cmd nil)
	      nil)))
	(progn
	  (vi-back-to-old-mode)
	  (setq prefix-arg arg)
	  (command-execute cmd nil)))))

(defun vi-quote-words (arg char)
  "Quote ARG words from the word point is on with pattern specified by CHAR.
Currently, CHAR could be [,{,(,\",',`,<,*, etc."
  (interactive "*p\nc")
  (while (not (string-match "[[({<\"'`*]" (char-to-string char)))
    (message "Enter any of [,{,(,<,\",',`,* as quoting character.")
    (setq char (read-char)))
  (vi-set-last-change-command 'vi-quote-words arg char)
  (if (not (looking-at "\\<")) (forward-word -1))
  (insert char)
  (cond ((char-equal char ?[) (setq char ?]))
	((char-equal char ?{) (setq char ?}))
	((char-equal char ?<) (setq char ?>))
	((char-equal char ?() (setq char ?)))
	((char-equal char ?`) (setq char ?')))
  (vi-end-of-word arg)
  (forward-char 1)
  (insert char))

(defun vi-locate-def ()
  "Locate definition in current file for the name before the point.
It assumes a `(def..' always starts at the beginning of a line."
  (interactive)
  (let (name)
    (save-excursion
      (setq name (buffer-substring (progn (vi-backward-blank-delimited-word 1)
					  (skip-chars-forward "^a-zA-Z")
					  (point))
				   (progn (vi-end-of-blank-delimited-word 1)
					  (forward-char)
					  (skip-chars-backward "^a-zA-Z")
					  (point)))))
    (set-mark-command nil)
    (goto-char (point-min))
    (if (re-search-forward (concat "^(def[unvarconst ]*" name) nil t)
	nil
      (ding)
      (message "No definition for \"%s\" in current file." name)
      (set-mark-command t))))

(defun vi-split-open-line (arg)
  "Insert a newline and leave point before it.
With ARG, inserts that many newlines."
  (interactive "*p")
  (vi-goto-insert-state 1
    (list (function (lambda (arg)
		      (let ((flag (and (bolp) (not (bobp)))))
			(if flag (forward-char -1))
			(while (> arg 0)
			  (save-excursion
			    (insert ?\n)
			    (if fill-prefix (insert fill-prefix)))
			  (setq arg (1- arg)))
			(if flag (forward-char 1))))) arg)
    t))

(provide 'vi)

;;; vi.el ends here
