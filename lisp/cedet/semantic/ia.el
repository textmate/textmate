;;; semantic/ia.el --- Interactive Analysis functions

;;; Copyright (C) 2000-2012 Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax

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
;; Interactive access to `semantic-analyze'.
;;
;; These routines are fairly simple, and show how to use the Semantic
;; analyzer to provide things such as completion lists, summaries,
;; locations, or documentation.
;;

;;; TODO
;;
;; fast-jump.  For a virtual method, offer some of the possible
;; implementations in various sub-classes.

(require 'semantic/analyze)
(require 'semantic/format)
(require 'pulse)
(eval-when-compile
  (require 'semantic/analyze)
  (require 'semantic/analyze/refs)
  (require 'semantic/find))

(declare-function imenu--mouse-menu "imenu")

;;; Code:

;;; COMPLETION
;;
;; This set of routines provides some simplisting completion
;; functions.

(defcustom semantic-ia-completion-format-tag-function
  'semantic-format-tag-prototype
  "Function used to convert a tag to a string during completion."
  :group 'semantic
  :type semantic-format-tag-custom-list)

;;; COMPLETION HELPER
;;
;; This overload function handles inserting a tag
;; into a buffer for these local completion routines.
;;
;; By creating the functions as overloadable, it can be
;; customized.  For example, the default will put a paren "("
;; character after function names.  For Lisp, it might check
;; to put a "(" in front of a function name.

(define-overloadable-function semantic-ia-insert-tag (tag)
  "Insert TAG into the current buffer based on completion.")

(defun semantic-ia-insert-tag-default (tag)
  "Insert TAG into the current buffer based on completion."
  (insert (semantic-tag-name tag))
  (let ((tt (semantic-tag-class tag)))
    (cond ((eq tt 'function)
	   (insert "("))
	  (t nil))))

(defalias 'semantic-ia-get-completions 'semantic-ia-get-completions-deprecated
  "`Semantic-ia-get-completions' is obsolete.
Use `semantic-analyze-possible-completions' instead.")

(defun semantic-ia-get-completions-deprecated (context point)
  "A function to help transition away from `semantic-ia-get-completions'.
Return completions based on CONTEXT at POINT.
You should not use this, nor the aliased version.
Use `semantic-analyze-possible-completions' instead."
  (semantic-analyze-possible-completions context))

;;;###autoload
(defun semantic-ia-complete-symbol (&optional pos)
  "Complete the current symbol at POS.
If POS is nil, default to point.
Completion options are calculated with `semantic-analyze-possible-completions'."
  (interactive "d")
  (when (semantic-active-p)
    (or pos (setq pos (point)))
    ;; Calculating completions is a two step process.
    ;;
    ;; The first analyzer the current context, which finds tags for
    ;; all the stuff that may be references by the code around POS.
    ;;
    ;; The second step derives completions from that context.
    (let* ((a (semantic-analyze-current-context pos))
	   (syms (semantic-analyze-possible-completions a))
	   (pre (car (reverse (oref a prefix)))))
      ;; If PRE was actually an already completed symbol, it doesn't
      ;; come in as a string, but as a tag instead.
      (if (semantic-tag-p pre)
	  ;; We will try completions on it anyway.
	  (setq pre (semantic-tag-name pre)))
      ;; Complete this symbol.
      (if (null syms)
	  (if (semantic-analyze-context-p a)
	      ;; This is a clever hack.  If we were unable to find any
	      ;; smart completions, let's divert to how senator derives
	      ;; completions.
	      ;;
	      ;; This is a way of making this fcn more useful since
	      ;; the smart completion engine sometimes fails.
	      (semantic-complete-symbol))
	;; Use try completion to seek a common substring.
	(let ((tc (try-completion (or pre "")  syms)))
	  (if (and (stringp tc) (not (string= tc (or pre ""))))
	      (let ((tok (semantic-find-first-tag-by-name
			  tc syms)))
		;; Delete what came before...
		(when (and (car (oref a bounds)) (cdr (oref a bounds)))
		  (delete-region (car (oref a bounds))
				 (cdr (oref a bounds)))
		  (goto-char (car (oref a bounds))))
		;; We have some new text.  Stick it in.
		(if tok
		    (semantic-ia-insert-tag tok)
		  (insert tc)))
	    ;; We don't have new text.  Show all completions.
	    (when (cdr (oref a bounds))
	      (goto-char (cdr (oref a bounds))))
	    (with-output-to-temp-buffer "*Completions*"
	      (display-completion-list
	       (mapcar semantic-ia-completion-format-tag-function syms)))))))))

(defcustom semantic-ia-completion-menu-format-tag-function
  'semantic-uml-concise-prototype-nonterminal
  "*Function used to convert a tag to a string during completion."
  :group 'semantic
  :type semantic-format-tag-custom-list)

;;; Completions Tip
;;
;; This functions shows how to get the list of completions,
;; to place in a tooltip.  It doesn't actually do any completion.

;;;###autoload
(defun semantic-ia-complete-tip (point)
  "Pop up a tooltip for completion at POINT."
  (interactive "d")
  (let* ((a (semantic-analyze-current-context point))
	 (syms (semantic-analyze-possible-completions a))
         (x (mod (- (current-column) (window-hscroll))
                 (window-width)))
         (y (save-excursion
              (save-restriction
                (widen)
                (narrow-to-region (window-start) (point))
                (goto-char (point-min))
                (1+ (vertical-motion (buffer-size))))))
	 (str (mapconcat #'semantic-tag-name
			 syms
			 "\n"))
	 )
    (cond ((fboundp 'x-show-tip)
	   (x-show-tip str
		       (selected-frame)
		       nil
		       nil
		       x y)
	   )
	  (t (message str))
	  )))

;;; Summary
;;
;; Like idle-summary-mode, this shows how to get something to
;; show a summary on.

;;;###autoload
(defun semantic-ia-show-summary (point)
  "Display a summary for the symbol under POINT."
  (interactive "P")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (when ctxt
	       ;; The CTXT is an EIEIO object.  The below
	       ;; method will attempt to pick the most interesting
	       ;; tag associated with the current context.
	       (semantic-analyze-interesting-tag ctxt)))
	)
    (if pf
	(message "%s" (semantic-format-tag-summarize pf nil t))
      (message "No summary info available"))))

;;; Variants
;;
;; Show all variants for the symbol under point.

;;;###autoload
(defun semantic-ia-show-variants (point)
  "Display a list of all variants for the symbol under POINT."
  (interactive "P")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (comp nil))

    ;; We really want to look at the function if we are on an
    ;; argument.  Are there some additional rules we care about for
    ;; changing the CTXT we look at?
    (when (semantic-analyze-context-functionarg-p ctxt)
      (goto-char (cdr (oref ctxt bounds)))
      (setq ctxt (semantic-analyze-current-context (point))))

    ;; Get the "completion list", but remove ALL filters to get the master list
    ;; of all the possible things.
    (setq comp (semantic-analyze-possible-completions ctxt 'no-unique 'no-tc))

    ;; Special case for a single type.  List the constructors?
    (when (and (= (length comp) 1) (semantic-tag-of-class-p (car comp) 'type))
      (setq comp (semantic-find-tags-by-name (semantic-tag-name (car comp))
					     (semantic-tag-type-members (car comp)))))

    ;; Display the results.
    (cond ((= (length comp) 0)
	   (message "No Variants found."))
	  ((= (length comp) 1)
	   (message "%s" (semantic-format-tag-summarize (car comp) nil t)))
	  (t
	   (with-output-to-temp-buffer "*Symbol Variants*"
	     (semantic-analyze-princ-sequence comp "" (current-buffer)))
	   (shrink-window-if-larger-than-buffer
	    (get-buffer-window "*Symbol Variants*")))
	  )))

;;; FAST Jump
;;
;; Jump to a destination based on the local context.
;;
;; This shows how to use the analyzer context, and the
;; analyzer references objects to choose a good destination.

(defun semantic-ia--fast-jump-helper (dest)
  "Jump to DEST, a Semantic tag.
This helper manages the mark, buffer switching, and pulsing."
  ;; We have a tag, but in C++, we usually get a prototype instead
  ;; because of header files.  Let's try to find the actual
  ;; implementation instead.
  (when (semantic-tag-prototype-p dest)
    (let* ((refs (semantic-analyze-tag-references dest))
	   (impl (semantic-analyze-refs-impl refs t))
	   )
      (when impl (setq dest (car impl)))))

  ;; Make sure we have a place to go...
  (if (not (and (or (semantic-tag-with-position-p dest)
		    (semantic-tag-get-attribute dest :line))
		(semantic-tag-file-name dest)))
      (error "Tag %s has no buffer information"
	     (semantic-format-tag-name dest)))

  ;; Once we have the tag, we can jump to it.  Here
  ;; are the key bits to the jump:

  ;; 1) Push the mark, so you can pop global mark back, or
  ;;    use semantic-mru-bookmark mode to do so.
  (push-mark)
  (when (fboundp 'push-tag-mark)
    (push-tag-mark))
  ;; 2) Visits the tag.
  (semantic-go-to-tag dest)
  ;; 3) go-to-tag doesn't switch the buffer in the current window,
  ;;    so it is like find-file-noselect.  Bring it forward.
  (switch-to-buffer (current-buffer))
  ;; 4) Fancy pulsing.
  (pulse-momentary-highlight-one-line (point))
  )

(declare-function semantic-decoration-include-visit "semantic/decorate/include")

;;;###autoload
(defun semantic-ia-fast-jump (point)
  "Jump to the tag referred to by the code at POINT.
Uses `semantic-analyze-current-context' output to identify an accurate
origin of the code at point."
  (interactive "d")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (and ctxt (reverse (oref ctxt prefix))))
	 ;; In the analyzer context, the PREFIX is the list of items
	 ;; that makes up the code context at point.  Thus the c++ code
	 ;; this.that().theothe
	 ;; would make a list:
	 ;; ( ("this" variable ..) ("that" function ...) "theothe")
	 ;; Where the first two elements are the semantic tags of the prefix.
	 ;;
	 ;; PF is the reverse of this list.  If the first item is a string,
	 ;; then it is an incomplete symbol, thus we pick the second.
	 ;; The second cannot be a string, as that would have been an error.
	 (first (car pf))
	 (second (nth 1 pf))
	 )
    (cond
     ((semantic-tag-p first)
      ;; We have a match.  Just go there.
      (semantic-ia--fast-jump-helper first))

     ((semantic-tag-p second)
      ;; Because FIRST failed, we should visit our second tag.
      ;; HOWEVER, the tag we actually want that was only an unfound
      ;; string may be related to some take in the datatype that belongs
      ;; to SECOND.  Thus, instead of visiting second directly, we
      ;; can offer to find the type of SECOND, and go there.
      (let ((secondclass (car (reverse (oref ctxt prefixtypes)))))
	(cond
	 ((and (semantic-tag-with-position-p secondclass)
	       (y-or-n-p (format "Could not find `%s'.  Jump to %s? "
				 first (semantic-tag-name secondclass))))
	  (semantic-ia--fast-jump-helper secondclass)
	  )
	 ;; If we missed out on the class of the second item, then
	 ;; just visit SECOND.
	 ((and (semantic-tag-p second)
	       (y-or-n-p (format "Could not find `%s'.  Jump to %s? "
				 first (semantic-tag-name second))))
	  (semantic-ia--fast-jump-helper second)
	  ))))

     ((semantic-tag-of-class-p (semantic-current-tag) 'include)
      ;; Just borrow this cool fcn.
      (require 'semantic/decorate/include)
      (semantic-decoration-include-visit)
      )

     (t
      (error "Could not find suitable jump point for %s"
	     first))
     )))

;;;###autoload
(defun semantic-ia-fast-mouse-jump (evt)
  "Jump to the tag referred to by the point clicked on.
See `semantic-ia-fast-jump' for details on how it works.
 This command is meant to be bound to a mouse event."
  (interactive "e")
  (semantic-ia-fast-jump
   (save-excursion
     (posn-set-point (event-end evt))
     (point))))

;;; DOC/DESCRIBE
;;
;; These routines show how to get additional information about a tag
;; for purposes of describing or showing documentation about them.
;;;###autoload
(defun semantic-ia-show-doc (point)
  "Display the code-level documentation for the symbol at POINT."
  (interactive "d")
  (let* ((ctxt (semantic-analyze-current-context point))
	 (pf (reverse (oref ctxt prefix)))
	 )
    ;; If PF, the prefix is non-nil, then the last element is either
    ;; a string (incomplete type), or a semantic TAG.  If it is a TAG
    ;; then we should be able to find DOC for it.
    (cond
     ((stringp (car pf))
      (message "Incomplete symbol name."))
     ((semantic-tag-p (car pf))
      ;; The `semantic-documentation-for-tag' fcn is language
      ;; specific.  If it doesn't return what you expect, you may
      ;; need to implement something for your language.
      ;;
      ;; The default tries to find a comment in front of the tag
      ;; and then strings off comment prefixes.
      (let ((doc (semantic-documentation-for-tag (car pf))))
	(if (or (null doc) (string= doc ""))
	    (message "Doc unavailable for: %s"
		     (semantic-format-tag-prototype (car pf)))
	  (with-output-to-temp-buffer "*TAG DOCUMENTATION*"
	    (princ "Tag: ")
	    (princ (semantic-format-tag-prototype (car pf)))
	    (princ "\n")
	    (princ "\n")
	    (princ "Snarfed Documentation: ")
	    (princ "\n")
	    (princ "\n")
	    (if doc
		(princ doc)
	      (princ "  Documentation unavailable."))
	    ))))
     (t
      (message "Unknown tag.")))
    ))

;;;###autoload
(defun semantic-ia-describe-class (typename)
  "Display all known parts for the datatype TYPENAME.
If the type in question is a class, all methods and other accessible
parts of the parent classes are displayed."
  ;; @todo - use a fancy completing reader.
  (interactive "sType Name: ")

  ;; When looking for a tag of any name there are a couple ways to do
  ;; it.  The simple `semanticdb-find-tag-by-...' are simple, and
  ;; you need to pass it the exact name you want.
  ;;
  ;; The analyzer function `semantic-analyze-tag-name' will take
  ;; more complex names, such as the cpp symbol foo::bar::baz,
  ;; and break it up, and dive through the namespaces.
  (let ((class (semantic-analyze-find-tag typename)))

    (when (not (semantic-tag-p class))
      (error "Cannot find class %s" class))
    (with-output-to-temp-buffer "*TAG DOCUMENTATION*"
      ;; There are many semantic-format-tag-* fcns.
      ;; The summarize routine is a fairly generic one.
      (princ (semantic-format-tag-summarize class))
      (princ "\n")
      (princ "  Type Members:\n")
      ;; The type tag contains all the parts of the type.
      ;; In complex languages with inheritance, not all the
      ;; parts are in the tag.  This analyzer fcn will traverse
      ;; the inheritance tree, and find all the pieces that
      ;; are inherited.
      (let ((parts (semantic-analyze-scoped-type-parts class)))
	(while parts
	  (princ "    ")
	  (princ (semantic-format-tag-summarize (car parts)))
	  (princ "\n")
	  (setq parts (cdr parts)))
	)
      )))

(provide 'semantic/ia)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "semantic/ia"
;; End:

;;; semantic/ia.el ends here
