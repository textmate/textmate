;;; hideif.el --- hides selected code within ifdef

;; Copyright (C) 1988, 1994, 2001-2012  Free Software Foundation, Inc.

;; Author: Brian Marick
;;	Daniel LaLiberte <liberte@holonexus.org>
;; Maintainer: FSF
;; Keywords: c, outlines

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

;; To initialize, toggle the hide-ifdef minor mode with
;;
;; M-x hide-ifdef-mode
;;
;; This will set up key bindings and call hide-ifdef-mode-hook if it
;; has a value.  To explicitly hide ifdefs using a buffer-local
;; define list (default empty), type
;;
;; M-x hide-ifdefs  or C-c @ h
;;
;; Hide-ifdef suppresses the display of code that the preprocessor wouldn't
;; pass through.  The support of constant expressions in #if lines is
;; limited to identifiers, parens, and the operators: &&, ||, !, and
;; "defined".  Please extend this.
;;
;; The hidden code is marked by ellipses (...).  Be
;; cautious when editing near ellipses, since the hidden text is
;; still in the buffer, and you can move the point into it and modify
;; text unawares.
;; You can make your buffer read-only while hide-ifdef-hiding by setting
;; hide-ifdef-read-only to a non-nil value.  You can toggle this
;; variable with hide-ifdef-toggle-read-only (C-c @ C-q).
;;
;; You can undo the effect of hide-ifdefs by typing
;;
;; M-x show-ifdefs  or C-c @ s
;;
;; Use M-x hide-ifdef-define (C-c @ d) to define a symbol.
;; Use M-x hide-ifdef-undef (C-c @ u) to undefine a symbol.
;;
;; If you define or undefine a symbol while hide-ifdef-mode is in effect,
;; the display will be updated.  Only the define list for the current
;; buffer will be affected.  You can save changes to the local define
;; list with hide-ifdef-set-define-alist.  This adds entries
;; to hide-ifdef-define-alist.
;;
;; If you have defined a hide-ifdef-mode-hook, you can set
;; up a list of symbols that may be used by hide-ifdefs as in the
;; following example:
;;
;; (add-hook 'hide-ifdef-mode-hook
;;      (lambda ()
;;	 (unless hide-ifdef-define-alist
;;	   (setq hide-ifdef-define-alist
;;		'((list1 ONE TWO)
;;		  (list2 TWO THREE))))
;;	 (hide-ifdef-use-define-alist 'list2))) ; use list2 by default
;;
;; You can call hide-ifdef-use-define-alist (C-c @ U) at any time to specify
;; another list to use.
;;
;; To cause ifdefs to be hidden as soon as hide-ifdef-mode is called,
;; set hide-ifdef-initially to non-nil.
;;
;; If you set hide-ifdef-lines to t, hide-ifdefs hides all the #ifdef lines.
;; In the absence of highlighting, that might be a bad idea.  If you set
;; hide-ifdef-lines to nil (the default), the surrounding preprocessor
;; lines will be displayed.  That can be confusing in its own
;; right.  Other variations on display are possible, but not much
;; better.
;;
;; You can explicitly hide or show individual ifdef blocks irrespective
;; of the define list by using hide-ifdef-block and show-ifdef-block.
;;
;; You can move the point between ifdefs with forward-ifdef, backward-ifdef,
;; up-ifdef, down-ifdef, next-ifdef, and previous-ifdef.
;;
;; If you have minor-mode-alist in your mode line (the default) two labels
;; may appear.  "Ifdef" will appear when hide-ifdef-mode is active.  "Hiding"
;; will appear when text may be hidden ("hide-ifdef-hiding" is non-nil).
;;
;; Written by Brian Marick, at Gould, Computer Systems Division, Urbana IL.
;; Extensively modified by Daniel LaLiberte (while at Gould).

;;; Code:

(require 'cc-mode)

(defgroup hide-ifdef nil
  "Hide selected code within `ifdef'."
  :group 'c)

(defcustom hide-ifdef-initially nil
  "Non-nil means call `hide-ifdefs' when Hide-Ifdef mode is first activated."
  :type 'boolean
  :group 'hide-ifdef)

(defcustom hide-ifdef-read-only nil
  "Set to non-nil if you want buffer to be read-only while hiding text."
  :type 'boolean
  :group 'hide-ifdef)

(defcustom hide-ifdef-lines nil
  "Non-nil means hide the #ifX, #else, and #endif lines."
  :type 'boolean
  :group 'hide-ifdef)

(defcustom hide-ifdef-shadow nil
  "Non-nil means shadow text instead of hiding it."
  :type 'boolean
  :group 'hide-ifdef
  :version "23.1")

(defface hide-ifdef-shadow '((t (:inherit shadow)))
  "Face for shadowing ifdef blocks."
  :group 'hide-ifdef
  :version "23.1")


(defvar hide-ifdef-mode-submap
  ;; Set up the submap that goes after the prefix key.
  (let ((map (make-sparse-keymap)))
    (define-key map "d" 'hide-ifdef-define)
    (define-key map "u" 'hide-ifdef-undef)
    (define-key map "D" 'hide-ifdef-set-define-alist)
    (define-key map "U" 'hide-ifdef-use-define-alist)

    (define-key map "h" 'hide-ifdefs)
    (define-key map "s" 'show-ifdefs)
    (define-key map "\C-d" 'hide-ifdef-block)
    (define-key map "\C-s" 'show-ifdef-block)

    (define-key map "\C-q" 'hide-ifdef-toggle-read-only)
    (define-key map "\C-w" 'hide-ifdef-toggle-shadowing)
    (substitute-key-definition
     'toggle-read-only 'hide-ifdef-toggle-outside-read-only map)
    map)
  "Keymap used by `hide-ifdef-mode' under `hide-ifdef-mode-prefix-key'.")

(defconst hide-ifdef-mode-prefix-key "\C-c@"
  "Prefix key for all Hide-Ifdef mode commands.")

(defvar hide-ifdef-mode-map
  ;; Set up the mode's main map, which leads via the prefix key to the submap.
  (let ((map (make-sparse-keymap)))
    (define-key map hide-ifdef-mode-prefix-key hide-ifdef-mode-submap)
    map)
  "Keymap used with `hide-ifdef-mode'.")

(easy-menu-define hide-ifdef-mode-menu hide-ifdef-mode-map
  "Menu for `hide-ifdef-mode'."
  '("Hide-Ifdef"
    ["Hide some ifdefs" hide-ifdefs
     :help "Hide the contents of some #ifdefs"]
    ["Show all ifdefs" show-ifdefs
     :help "Cancel the effects of `hide-ifdef': show the contents of all #ifdefs"]
    ["Hide ifdef block" hide-ifdef-block
     :help "Hide the ifdef block (true or false part) enclosing or before the cursor"]
    ["Show ifdef block" show-ifdef-block
     :help "Show the ifdef block (true or false part) enclosing or before the cursor"]
    ["Define a variable..." hide-ifdef-define
     :help "Define a VAR so that #ifdef VAR would be included"]
    ["Undefine a variable..." hide-ifdef-undef
     :help "Undefine a VAR so that #ifdef VAR would not be included"]
    ["Define an alist..." hide-ifdef-set-define-alist
     :help "Set the association for NAME to `hide-ifdef-env'"]
    ["Use an alist..." hide-ifdef-use-define-alist
     :help "Set `hide-ifdef-env' to the define list specified by NAME"]
    ["Toggle read only" hide-ifdef-toggle-read-only
     :style toggle :selected hide-ifdef-read-only
     :help "Buffer should be read-only while hiding text"]
    ["Toggle shadowing" hide-ifdef-toggle-shadowing
     :style toggle :selected hide-ifdef-shadow
     :help "Text should be shadowed instead of hidden"]))

(defvar hide-ifdef-hiding nil
  "Non-nil when text may be hidden.")

(or (assq 'hide-ifdef-hiding minor-mode-alist)
    (setq minor-mode-alist
          (cons '(hide-ifdef-hiding " Hiding")
                minor-mode-alist)))

;; fix c-mode syntax table so we can recognize whole symbols.
(defvar hide-ifdef-syntax-table
  (let ((st (copy-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?\| "." st)
    st)
  "Syntax table used for tokenizing #if expressions.")

(defvar hide-ifdef-env nil
  "An alist of defined symbols and their values.")

(defvar hif-outside-read-only nil
  "Internal variable.  Saves the value of `buffer-read-only' while hiding.")

;;;###autoload
(define-minor-mode hide-ifdef-mode
  "Toggle features to hide/show #ifdef blocks (Hide-Ifdef mode).
With a prefix argument ARG, enable Hide-Ifdef mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Hide-Ifdef mode is a buffer-local minor mode for use with C and
C-like major modes.  When enabled, code within #ifdef constructs
that the C preprocessor would eliminate may be hidden from view.
Several variables affect how the hiding is done:

`hide-ifdef-env'
	An association list of defined and undefined symbols for the
	current buffer.  Initially, the global value of `hide-ifdef-env'
	is used.

`hide-ifdef-define-alist'
	An association list of defined symbol lists.
        Use `hide-ifdef-set-define-alist' to save the current `hide-ifdef-env'
        and `hide-ifdef-use-define-alist' to set the current `hide-ifdef-env'
        from one of the lists in `hide-ifdef-define-alist'.

`hide-ifdef-lines'
	Set to non-nil to not show #if, #ifdef, #ifndef, #else, and
	#endif lines when hiding.

`hide-ifdef-initially'
	Indicates whether `hide-ifdefs' should be called when Hide-Ifdef mode
	is activated.

`hide-ifdef-read-only'
	Set to non-nil if you want to make buffers read only while hiding.
	After `show-ifdefs', read-only status is restored to previous value.

\\{hide-ifdef-mode-map}"
  :group 'hide-ifdef :lighter " Ifdef"
  (if hide-ifdef-mode
      (progn
	;; inherit global values
	(set (make-local-variable 'hide-ifdef-env)
	     (default-value 'hide-ifdef-env))
	(set (make-local-variable 'hide-ifdef-hiding)
	     (default-value 'hide-ifdef-hiding))
	(set (make-local-variable 'hif-outside-read-only) buffer-read-only)
	(set (make-local-variable 'line-move-ignore-invisible) t)
	(add-hook 'change-major-mode-hook
		  (lambda () (hide-ifdef-mode -1)) nil t)

	(add-to-invisibility-spec '(hide-ifdef . t))

	(if hide-ifdef-initially
	    (hide-ifdefs)
	  (show-ifdefs)))
    ;; else end hide-ifdef-mode
    (kill-local-variable 'line-move-ignore-invisible)
    (remove-from-invisibility-spec '(hide-ifdef . t))
    (when hide-ifdef-hiding
      (show-ifdefs))))


(defun hif-show-all ()
  "Show all of the text in the current buffer."
  (interactive)
  (hif-show-ifdef-region (point-min) (point-max)))

;; By putting this on after-revert-hook, we arrange that it only
;; does anything when revert-buffer avoids turning off the mode.
;; (That can happen in VC.)
(defun hif-after-revert-function ()
  (and hide-ifdef-mode hide-ifdef-hiding
       (hide-ifdefs t)))
(add-hook 'after-revert-hook 'hif-after-revert-function)

(defun hif-end-of-line ()
  (end-of-line)
  (while (= (logand 1 (skip-chars-backward "\\\\")) 1)
    (end-of-line 2)))

(defun hide-ifdef-region-internal (start end)
  (remove-overlays start end 'hide-ifdef t)
  (let ((o (make-overlay start end)))
    (overlay-put o 'hide-ifdef t)
    (if hide-ifdef-shadow
	(overlay-put o 'face 'hide-ifdef-shadow)
      (overlay-put o 'invisible 'hide-ifdef))))

(defun hide-ifdef-region (start end)
  "START is the start of a #if or #else form.  END is the ending part.
Everything including these lines is made invisible."
  (save-excursion
    (goto-char start) (hif-end-of-line) (setq start (point))
    (goto-char end) (hif-end-of-line) (setq end (point))
    (hide-ifdef-region-internal start end)))

(defun hif-show-ifdef-region (start end)
  "Everything between START and END is made visible."
  (remove-overlays start end 'hide-ifdef t))


;;===%%SF%% evaluation (Start)  ===

;; It is not useful to set this to anything but `eval'.
;; In fact, the variable might as well be eliminated.
(defvar hide-ifdef-evaluator 'eval
  "The function to use to evaluate a form.
The evaluator is given a canonical form and returns t if text under
that form should be displayed.")

(defvar hif-undefined-symbol nil
  "...is by default considered to be false.")


(defun hif-set-var (var value)
  "Prepend (var value) pair to hide-ifdef-env."
  (setq hide-ifdef-env (cons (cons var value) hide-ifdef-env)))


(defun hif-lookup (var)
  ;; (message "hif-lookup %s" var)
  (let ((val (assoc var hide-ifdef-env)))
    (if val
	(cdr val)
      hif-undefined-symbol)))

(defun hif-defined (var)
   (if (assoc var hide-ifdef-env) 1 0))

;;===%%SF%% evaluation (End)  ===



;;===%%SF%% parsing (Start)  ===
;;;  The code that understands what ifs and ifdef in files look like.

(defconst hif-cpp-prefix "\\(^\\|\r\\)[ \t]*#[ \t]*")
(defconst hif-ifndef-regexp (concat hif-cpp-prefix "ifndef"))
(defconst hif-ifx-regexp (concat hif-cpp-prefix "if\\(n?def\\)?[ \t]+"))
(defconst hif-else-regexp (concat hif-cpp-prefix "else"))
(defconst hif-endif-regexp (concat hif-cpp-prefix "endif"))
(defconst hif-ifx-else-endif-regexp
  (concat hif-ifx-regexp "\\|" hif-else-regexp "\\|" hif-endif-regexp))

;; Used to store the current token and the whole token list during parsing.
;; Only bound dynamically.
(defvar hif-token)
(defvar hif-token-list)

(defconst hif-token-alist
  '(("||" . or)
    ("&&" . and)
    ("|"  . hif-logior)
    ("&"  . hif-logand)
    ("==" . equal)
    ("!=" . hif-notequal)
    ("!"  . not)
    ("("  . lparen)
    (")"  . rparen)
    (">"  . hif-greater)
    ("<"  . hif-less)
    (">=" . hif-greater-equal)
    ("<=" . hif-less-equal)
    ("+"  . hif-plus)
    ("-"  . hif-minus)
    ("?"  . hif-conditional)
    (":"  . hif-colon)))

(defconst hif-token-regexp
  (concat (regexp-opt (mapcar 'car hif-token-alist)) "\\|\\w+"))

(defun hif-tokenize (start end)
  "Separate string between START and END into a list of tokens."
  (let ((token-list nil))
    (with-syntax-table hide-ifdef-syntax-table
      (save-excursion
	(goto-char start)
	(while (progn (forward-comment (point-max)) (< (point) end))
	  ;; (message "expr-start = %d" expr-start) (sit-for 1)
	  (cond
	   ((looking-at "\\\\\n")
	    (forward-char 2))

	   ((looking-at hif-token-regexp)
	    (let ((token (buffer-substring (point) (match-end 0))))
	      (goto-char (match-end 0))
	      ;; (message "token: %s" token) (sit-for 1)
	      (push (or (cdr (assoc token hif-token-alist))
                        (if (string-equal token "defined") 'hif-defined)
                        (if (string-match "\\`[0-9]*\\'" token)
                            (string-to-number token))
                        (intern token))
		    token-list)))
	   (t (error "Bad #if expression: %s" (buffer-string)))))))
    (nreverse token-list)))

;;;-----------------------------------------------------------------
;;; Translate C preprocessor #if expressions using recursive descent.
;;; This parser is limited to the operators &&, ||, !, and "defined".
;;; Added ==, !=, +, and -.  Gary Oberbrunner, garyo@avs.com, 8/9/94

(defsubst hif-nexttoken ()
  "Pop the next token from token-list into the let variable \"hif-token\"."
  (setq hif-token (pop hif-token-list)))

(defun hif-parse-if-exp (token-list)
  "Parse the TOKEN-LIST.  Return translated list in prefix form."
  (let ((hif-token-list token-list))
    (hif-nexttoken)
    (prog1
        (hif-expr)
      (if hif-token ; is there still a token?
          (error "Error: unexpected token: %s" hif-token)))))

(defun hif-expr ()
  "Parse an expression as found in #if.
       expr : or-expr | or-expr '?' expr ':' expr."
  (let ((result (hif-or-expr))
	middle)
    (while (eq hif-token 'hif-conditional)
      (hif-nexttoken)
      (setq middle (hif-expr))
      (if (eq hif-token 'hif-colon)
	  (progn
	    (hif-nexttoken)
	    (setq result (list 'hif-conditional result middle (hif-expr))))
	(error "Error: unexpected token: %s" hif-token)))
    result))

(defun hif-or-expr ()
  "Parse n or-expr : and-expr | or-expr '||' and-expr."
  (let ((result (hif-and-expr)))
    (while (eq hif-token 'or)
      (hif-nexttoken)
      (setq result (list 'hif-or result (hif-and-expr))))
  result))

(defun hif-and-expr ()
  "Parse an and-expr : eq-expr | and-expr '&&' eq-expr."
  (let ((result (hif-eq-expr)))
    (while (eq hif-token 'and)
      (hif-nexttoken)
      (setq result (list 'hif-and result (hif-eq-expr))))
    result))

(defun hif-eq-expr ()
  "Parse an eq-expr : math | eq-expr `=='|`!='|`<'|`>'|`>='|`<=' math."
  (let ((result (hif-math))
	(eq-token nil))
    (while (memq hif-token '(equal hif-notequal hif-greater hif-less
			     hif-greater-equal hif-less-equal))
      (setq eq-token hif-token)
      (hif-nexttoken)
      (setq result (list eq-token result (hif-math))))
    result))

(defun hif-math ()
  "Parse an expression with + or - and simpler things.
       math : factor | math '+|-' factor."
  (let ((result (hif-factor))
	(math-op nil))
    (while (memq hif-token '(hif-plus hif-minus hif-logior hif-logand))
      (setq math-op hif-token)
      (hif-nexttoken)
      (setq result (list math-op result (hif-factor))))
  result))

(defun hif-factor ()
  "Parse a factor: '!' factor | '(' expr ')' | 'defined(' id ')' | id."
  (cond
   ((eq hif-token 'not)
    (hif-nexttoken)
    (list 'hif-not (hif-factor)))

   ((eq hif-token 'lparen)
    (hif-nexttoken)
    (let ((result (hif-expr)))
      (if (not (eq hif-token 'rparen))
	  (error "Bad token in parenthesized expression: %s" hif-token)
	(hif-nexttoken)
	result)))

   ((eq hif-token 'hif-defined)
    (hif-nexttoken)
    (let ((paren (when (eq hif-token 'lparen) (hif-nexttoken) t))
	  (ident hif-token))
      (if (memq hif-token '(or and not hif-defined lparen rparen))
	  (error "Error: unexpected token: %s" hif-token))
      (when paren
	(hif-nexttoken)
	(unless (eq hif-token 'rparen)
	  (error "Error: expected \")\" after identifier")))
      (hif-nexttoken)
      `(hif-defined (quote ,ident))))

   ((numberp hif-token)
    (prog1 hif-token (hif-nexttoken)))

   ;; Unary plus/minus.
   ((memq hif-token '(hif-minus hif-plus))
    (list (prog1 hif-token (hif-nexttoken)) 0 (hif-factor)))

   (t					; identifier
    (let ((ident hif-token))
      (if (memq ident '(or and))
	  (error "Error: missing identifier"))
      (hif-nexttoken)
      `(hif-lookup (quote ,ident))))))

(defun hif-mathify (val)
  "Treat VAL as a number: if it's t or nil, use 1 or 0."
  (cond ((eq val t) 1)
	((null val) 0)
	(t val)))

(defun hif-conditional (a b c)
  (if (not (zerop (hif-mathify a))) (hif-mathify b) (hif-mathify c)))
(defun hif-and (a b)
  (and (not (zerop (hif-mathify a))) (not (zerop (hif-mathify b)))))
(defun hif-or (a b)
  (or (not (zerop (hif-mathify a))) (not (zerop (hif-mathify b)))))
(defun hif-not (a)
  (zerop (hif-mathify a)))

(defmacro hif-mathify-binop (fun)
  `(lambda (a b)
     ,(format "Like `%s' but treat t and nil as 1 and 0." fun)
     (,fun (hif-mathify a) (hif-mathify b))))

(defalias 'hif-plus          (hif-mathify-binop +))
(defalias 'hif-minus         (hif-mathify-binop -))
(defalias 'hif-notequal      (hif-mathify-binop /=))
(defalias 'hif-greater       (hif-mathify-binop >))
(defalias 'hif-less          (hif-mathify-binop <))
(defalias 'hif-greater-equal (hif-mathify-binop >=))
(defalias 'hif-less-equal    (hif-mathify-binop <=))
(defalias 'hif-logior        (hif-mathify-binop logior))
(defalias 'hif-logand        (hif-mathify-binop logand))

;;;----------- end of parser -----------------------


(defun hif-canonicalize ()
  "When at beginning of #ifX, return a Lisp expression for its condition."
  (save-excursion
    (let ((negate (looking-at hif-ifndef-regexp)))
      (re-search-forward hif-ifx-regexp)
      (let* ((tokens (hif-tokenize (point)
				   (progn (hif-end-of-line) (point))))
	     (expr (hif-parse-if-exp tokens)))
	;; (message "hif-canonicalized: %s" expr)
	(if negate
	    (list 'hif-not expr)
	  expr)))))


(defun hif-find-any-ifX ()
  "Move to next #if..., or #ifndef, at point or after."
  ;; (message "find ifX at %d" (point))
  (prog1
      (re-search-forward hif-ifx-regexp (point-max) t)
    (beginning-of-line)))


(defun hif-find-next-relevant ()
  "Move to next #if..., #else, or #endif, after the current line."
  ;; (message "hif-find-next-relevant at %d" (point))
  (end-of-line)
  ;; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-forward hif-ifx-else-endif-regexp (point-max) t)
      (beginning-of-line)))

(defun hif-find-previous-relevant ()
  "Move to previous #if..., #else, or #endif, before the current line."
  ;; (message "hif-find-previous-relevant at %d" (point))
  (beginning-of-line)
  ;; avoid infinite recursion by only going to beginning of line if match found
  (if (re-search-backward hif-ifx-else-endif-regexp (point-min) t)
     (beginning-of-line)))


(defun hif-looking-at-ifX ()		;; Should eventually see #if
  (looking-at hif-ifx-regexp))
(defun hif-looking-at-endif ()
  (looking-at hif-endif-regexp))
(defun hif-looking-at-else ()
  (looking-at hif-else-regexp))



(defun hif-ifdef-to-endif ()
  "If positioned at #ifX or #else form, skip to corresponding #endif."
  ;; (message "hif-ifdef-to-endif at %d" (point)) (sit-for 1)
  (hif-find-next-relevant)
  (cond ((hif-looking-at-ifX)
	 (hif-ifdef-to-endif) ; find endif of nested if
	 (hif-ifdef-to-endif)) ; find outer endif or else
	((hif-looking-at-else)
	 (hif-ifdef-to-endif)) ; find endif following else
	((hif-looking-at-endif)
	 'done)
	(t
	 (error "Mismatched #ifdef #endif pair"))))


(defun hif-endif-to-ifdef ()
  "If positioned at #endif form, skip backward to corresponding #ifX."
  ;; (message "hif-endif-to-ifdef at %d" (point))
  (let ((start (point)))
    (hif-find-previous-relevant)
    (if (= start (point))
	(error "Mismatched #ifdef #endif pair")))
  (cond ((hif-looking-at-endif)
	 (hif-endif-to-ifdef) ; find beginning of nested if
	 (hif-endif-to-ifdef)) ; find beginning of outer if or else
	((hif-looking-at-else)
	 (hif-endif-to-ifdef))
	((hif-looking-at-ifX)
	 'done)
	(t)))			; never gets here


(defun forward-ifdef (&optional arg)
  "Move point to beginning of line of the next ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (backward-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (- arg))
      (let ((start (point)))
	(unless (hif-looking-at-ifX)
	  (hif-find-next-relevant))
	(if (hif-looking-at-ifX)
	    (hif-ifdef-to-endif)
	  (goto-char start)
	  (error "No following #ifdef"))))))


(defun backward-ifdef (&optional arg)
  "Move point to beginning of the previous ifdef-endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (forward-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (1- arg))
      (beginning-of-line)
      (let ((start (point)))
	(unless (hif-looking-at-endif)
	  (hif-find-previous-relevant))
	(if (hif-looking-at-endif)
	    (hif-endif-to-ifdef)
	  (goto-char start)
	  (error "No previous #ifdef"))))))


(defun down-ifdef ()
  "Move point to beginning of nested ifdef or else-part."
    (interactive)
    (let ((start (point)))
      (hif-find-next-relevant)
      (if (or (hif-looking-at-ifX) (hif-looking-at-else))
	  ()
	(goto-char start)
	(error "No following #ifdef"))))


(defun up-ifdef ()
  "Move point to beginning of enclosing ifdef or else-part."
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (unless (hif-looking-at-endif)
      (hif-find-previous-relevant))
    (if (hif-looking-at-endif)
	(hif-endif-to-ifdef))
      (if (= start (point))
	  (error "No previous #ifdef"))))

(defun next-ifdef (&optional arg)
  "Move to the beginning of the next #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (previous-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (1- arg))
      (hif-find-next-relevant)
      (when (eolp)
	(beginning-of-line)
	(error "No following #ifdefs, #elses, or #endifs")))))

(defun previous-ifdef (&optional arg)
  "Move to the beginning of the previous #ifX, #else, or #endif.
With argument, do this that many times."
  (interactive "p")
  (or arg (setq arg 1))
  (if (< arg 0) (next-ifdef (- arg))
    (while (< 0 arg)
      (setq arg (1- arg))
      (let ((start (point)))
	(hif-find-previous-relevant)
	(if (= start (point))
	    (error "No previous #ifdefs, #elses, or #endifs"))))))


;;===%%SF%% parsing (End)  ===


;;===%%SF%% hide-ifdef-hiding (Start)  ===


;;; A range is a structure with four components:
;;; ELSE-P	True if there was an else clause for the ifdef.
;;; START	The start of the range. (beginning of line)
;;; ELSE	The else marker (beginning of line)
;;;			Only valid if ELSE-P is true.
;;; END		The end of the range.  (beginning of line)

(defsubst hif-make-range (start end &optional else)
  (list start else end))

(defsubst hif-range-start (range) (elt range 0))
(defsubst hif-range-else (range) (elt range 1))
(defsubst hif-range-end (range) (elt range 2))



;;; Find-Range
;;; The workhorse, it delimits the #if region.  Reasonably simple:
;;; Skip until an #else or #endif is found, remembering positions.  If
;;; an #else was found, skip some more, looking for the true #endif.

(defun hif-find-range ()
  "Return a Range structure describing the current #if region.
Point is left unchanged."
  ;; (message "hif-find-range at %d" (point))
  (save-excursion
    (beginning-of-line)
    (let ((start (point))
	  (else nil)
	  (end nil))
      ;; Part one.  Look for either #endif or #else.
      ;; This loop-and-a-half dedicated to E. Dijkstra.
      (while (progn
	       (hif-find-next-relevant)
	       (hif-looking-at-ifX))		; Skip nested ifdef
	(hif-ifdef-to-endif))
      ;; Found either a #else or an #endif.
      (cond ((hif-looking-at-else)
	     (setq else (point)))
	    (t
	     (setq end (point)))) ; (line-end-position)
      ;; If found #else, look for #endif.
      (when else
	(while (progn
		 (hif-find-next-relevant)
		 (hif-looking-at-ifX))	; Skip nested ifdef
	  (hif-ifdef-to-endif))
	(if (hif-looking-at-else)
	    (error "Found two elses in a row?  Broken!"))
	(setq end (point)))	       ; (line-end-position)
      (hif-make-range start end else))))


;;; A bit slimy.

(defun hif-hide-line (point)
  "Hide the line containing point.  Does nothing if `hide-ifdef-lines' is nil."
  (when hide-ifdef-lines
    (save-excursion
      (goto-char point)
      (hide-ifdef-region-internal
       (line-beginning-position) (progn (hif-end-of-line) (point))))))


;;;  Hif-Possibly-Hide
;;;  There are four cases.  The #ifX expression is "taken" if it
;;;  the hide-ifdef-evaluator returns T.  Presumably, this means the code
;;;  inside the #ifdef would be included when the program was
;;;  compiled.
;;;
;;;  Case 1:  #ifX taken, and there's an #else.
;;;	The #else part must be hidden.  The #if (then) part must be
;;;	processed for nested #ifX's.
;;;  Case 2:  #ifX taken, and there's no #else.
;;;	The #if part must be processed for nested #ifX's.
;;;  Case 3:  #ifX not taken, and there's an #else.
;;;	The #if part must be hidden.  The #else part must be processed
;;;	for nested #ifs.
;;;  Case 4:  #ifX not taken, and there's no #else.
;;;	The #ifX part must be hidden.
;;;
;;;  Further processing is done by narrowing to the relevant region
;;;  and just recursively calling hide-ifdef-guts.
;;;
;;;  When hif-possibly-hide returns, point is at the end of the
;;;  possibly-hidden range.

(defun hif-recurse-on (start end)
  "Call `hide-ifdef-guts' after narrowing to end of START line and END line."
  (save-excursion
    (save-restriction
      (goto-char start)
      (end-of-line)
      (narrow-to-region (point) end)
      (hide-ifdef-guts))))

(defun hif-possibly-hide ()
  "Called at #ifX expression, this hides those parts that should be hidden.
It uses the judgment of `hide-ifdef-evaluator'."
  ;; (message "hif-possibly-hide") (sit-for 1)
  (let ((test (hif-canonicalize))
	(range (hif-find-range)))
    ;; (message "test = %s" test) (sit-for 1)

    (hif-hide-line (hif-range-end range))
    (if (not (hif-not (funcall hide-ifdef-evaluator test)))
	(cond ((hif-range-else range)	; case 1
	       (hif-hide-line (hif-range-else range))
	       (hide-ifdef-region (hif-range-else range)
				  (1- (hif-range-end range)))
	       (hif-recurse-on (hif-range-start range)
			       (hif-range-else range)))
	      (t			; case 2
	       (hif-recurse-on (hif-range-start range)
			       (hif-range-end range))))
      (cond ((hif-range-else range)	; case 3
	     (hif-hide-line (hif-range-else range))
	     (hide-ifdef-region (hif-range-start range)
				(1- (hif-range-else range)))
	     (hif-recurse-on (hif-range-else range)
			     (hif-range-end range)))
	    (t				; case 4
	     (hide-ifdef-region (point)
				(1- (hif-range-end range))))))
    (hif-hide-line (hif-range-start range)) ; Always hide start.
    (goto-char (hif-range-end range))
    (end-of-line)))



(defun hide-ifdef-guts ()
  "Does most of the work of `hide-ifdefs'.
It does not do the work that's pointless to redo on a recursive entry."
  ;; (message "hide-ifdef-guts")
  (save-excursion
    (goto-char (point-min))
    (while (hif-find-any-ifX)
      (hif-possibly-hide))))

;;===%%SF%% hide-ifdef-hiding (End)  ===


;;===%%SF%% exports (Start)  ===

(defun hide-ifdef-toggle-read-only ()
  "Toggle `hide-ifdef-read-only'."
  (interactive)
  (setq hide-ifdef-read-only (not hide-ifdef-read-only))
  (message "Hide-Read-Only %s"
	   (if hide-ifdef-read-only "ON" "OFF"))
  (if hide-ifdef-hiding
      (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))
  (force-mode-line-update))

(defun hide-ifdef-toggle-outside-read-only ()
  "Replacement for `toggle-read-only' within Hide-Ifdef mode."
  (interactive)
  (setq hif-outside-read-only (not hif-outside-read-only))
  (message "Read only %s"
	   (if hif-outside-read-only "ON" "OFF"))
  (setq buffer-read-only
	(or (and hide-ifdef-hiding hide-ifdef-read-only)
	    hif-outside-read-only))
  (force-mode-line-update))

(defun hide-ifdef-toggle-shadowing ()
  "Toggle shadowing."
  (interactive)
  (set (make-local-variable 'hide-ifdef-shadow) (not hide-ifdef-shadow))
  (message "Shadowing %s" (if hide-ifdef-shadow "ON" "OFF"))
  (save-restriction
    (widen)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (overlay-get overlay 'hide-ifdef)
	(if hide-ifdef-shadow
	    (progn
	      (overlay-put overlay 'invisible nil)
	      (overlay-put overlay 'face 'hide-ifdef-shadow))
	  (overlay-put overlay 'face nil)
	  (overlay-put overlay 'invisible 'hide-ifdef))))))

(defun hide-ifdef-define (var)
  "Define a VAR so that #ifdef VAR would be included."
  (interactive "SDefine what? ")
  (hif-set-var var 1)
  (if hide-ifdef-hiding (hide-ifdefs)))

(defun hide-ifdef-undef (var)
  "Undefine a VAR so that #ifdef VAR would not be included."
  (interactive "SUndefine what? ")
  (hif-set-var var nil)
  (if hide-ifdef-hiding (hide-ifdefs)))


(defun hide-ifdefs (&optional nomsg)
  "Hide the contents of some #ifdefs.
Assume that defined symbols have been added to `hide-ifdef-env'.
The text hidden is the text that would not be included by the C
preprocessor if it were given the file with those symbols defined.

Turn off hiding by calling `show-ifdefs'."

  (interactive)
  (message "Hiding...")
  (setq hif-outside-read-only buffer-read-only)
  (unless hide-ifdef-mode (hide-ifdef-mode 1)) ; turn on hide-ifdef-mode
  (if hide-ifdef-hiding
      (show-ifdefs))			; Otherwise, deep confusion.
  (setq hide-ifdef-hiding t)
  (hide-ifdef-guts)
  (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only))
  (or nomsg
      (message "Hiding done")))


(defun show-ifdefs ()
  "Cancel the effects of `hide-ifdef': show the contents of all #ifdefs."
  (interactive)
  (setq buffer-read-only hif-outside-read-only)
  (hif-show-all)
  (setq hide-ifdef-hiding nil))


(defun hif-find-ifdef-block ()
  "Utility for hide and show `ifdef-block'.
Return as (TOP . BOTTOM) the extent of ifdef block."
  (let (max-bottom)
    (cons (save-excursion
	    (beginning-of-line)
	    (unless (or (hif-looking-at-else) (hif-looking-at-ifX))
	      (up-ifdef))
	    (prog1 (point)
	      (hif-ifdef-to-endif)
	      (setq max-bottom (1- (point)))))
	  (save-excursion
	    (beginning-of-line)
	    (unless (hif-looking-at-endif)
	      (hif-find-next-relevant))
	    (while (hif-looking-at-ifX)
	      (hif-ifdef-to-endif)
	      (hif-find-next-relevant))
	    (min max-bottom (1- (point)))))))


(defun hide-ifdef-block ()
  "Hide the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (unless hide-ifdef-mode (hide-ifdef-mode 1))
  (let ((top-bottom (hif-find-ifdef-block)))
    (hide-ifdef-region (car top-bottom) (cdr top-bottom))
    (when hide-ifdef-lines
      (hif-hide-line (car top-bottom))
      (hif-hide-line (1+ (cdr top-bottom))))
    (setq hide-ifdef-hiding t))
  (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only)))

(defun show-ifdef-block ()
  "Show the ifdef block (true or false part) enclosing or before the cursor."
  (interactive)
  (let ((top-bottom (hif-find-ifdef-block)))
    (if hide-ifdef-lines
 	(hif-show-ifdef-region
 	 (save-excursion
 	   (goto-char (car top-bottom)) (line-beginning-position))
 	 (save-excursion
 	   (goto-char (1+ (cdr top-bottom)))
	   (hif-end-of-line) (point)))
      (hif-show-ifdef-region (1- (car top-bottom)) (cdr top-bottom)))))


;;;  definition alist support

(defvar hide-ifdef-define-alist nil
  "A global assoc list of pre-defined symbol lists.")

(defun hif-compress-define-list (env)
  "Compress the define list ENV into a list of defined symbols only."
  (let ((new-defs nil))
    (dolist (def env new-defs)
      (if (hif-lookup (car def)) (push (car env) new-defs)))))

(defun hide-ifdef-set-define-alist (name)
  "Set the association for NAME to `hide-ifdef-env'."
  (interactive "SSet define list: ")
  (push (cons name (hif-compress-define-list hide-ifdef-env))
	hide-ifdef-define-alist))

(defun hide-ifdef-use-define-alist (name)
  "Set `hide-ifdef-env' to the define list specified by NAME."
  (interactive
   (list (completing-read "Use define list: "
			  (mapcar (lambda (x) (symbol-name (car x)))
                                  hide-ifdef-define-alist)
                          nil t)))
  (if (stringp name) (setq name (intern name)))
  (let ((define-list (assoc name hide-ifdef-define-alist)))
    (if define-list
	(setq hide-ifdef-env
	      (mapcar (lambda (arg) (cons arg t))
		      (cdr define-list)))
      (error "No define list for %s" name))
    (if hide-ifdef-hiding (hide-ifdefs))))

(provide 'hideif)

;;; hideif.el ends here
