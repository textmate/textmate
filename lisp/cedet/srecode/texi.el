;;; srecode/texi.el --- Srecode texinfo support.

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
;; Texinfo semantic recoder support.
;;
;; Contains some handlers, and a few simple texinfo srecoder applications.

(require 'semantic)
(require 'semantic/texi)
(require 'srecode/semantic)

;;; Code:

(defun srecode-texi-add-menu (newnode)
  "Add an item into the current menu.  Add @node statements as well.
Argument NEWNODE is the name of the new node."
  (interactive "sName of new node: ")
  (srecode-load-tables-for-mode major-mode)
  (semantic-fetch-tags)
  (let ((currnode (reverse (semantic-find-tag-by-overlay)))
	(nodebounds nil))
    (when (not currnode)
      (error "Cannot find node to put menu item into"))
    (setq currnode (car currnode))
    (setq nodebounds (semantic-tag-texi-section-text-bounds currnode))
    ;; Step 1:
    ;;   Limit search within this node.
    ;; Step 2:
    ;;   Find the menu.  If there isn't one, add one to the end.
    ;; Step 3:
    ;;   Add new item to end of menu list.
    ;; Step 4:
    ;;   Find correct node new item should show up after, and stick
    ;;   the new node there.
    (if (string= (semantic-texi-current-environment) "menu")
	;; We are already in a menu, so insert the new item right here.
	(beginning-of-line)
      ;; Else, try to find a menu item to append to.
      (goto-char (car nodebounds))
      (if (not (re-search-forward "^@menu" (car (cdr nodebounds)) t))
	  (progn
	    (goto-char (car (cdr nodebounds)))
	    (if (not (y-or-n-p "Add menu here? "))
		(error "Abort"))
	    (srecode-insert "declaration:menu"))
	;; Else, find the end
	(re-search-forward "@end menu")
	(beginning-of-line)))
    ;; At this point, we are in a menu... or not.
    ;; If we are, do stuff, else error.
    (when (string= (semantic-texi-current-environment) "menu")
      (let ((menuname newnode)
	    (returnpoint nil))
	(srecode-insert "declaration:menuitem" "NAME" menuname)
	(set-mark (point))
	(setq returnpoint (make-marker))
	;; Update the bound since we added text
	(setq nodebounds (semantic-tag-texi-section-text-bounds currnode))
	(beginning-of-line)
	(forward-char -1)
	(beginning-of-line)
	(let ((end nil))
	  (if (not (looking-at "\\* \\([^:]+\\):"))
	      (setq end (car (cdr nodebounds)))
	    (let* ((nname (match-string 1))
		   (tag
		    (semantic-deep-find-tags-by-name nname (current-buffer))))
	      (when tag
		(setq end (semantic-tag-end (car tag))))
	      ))
	  (when (not end)
	    (goto-char returnpoint)
	    (error "Could not find location for new node" ))
	  (when end
	    (goto-char end)
	    (when (bolp) (forward-char -1))
	    (insert "\n")
	    (if (eq (semantic-current-tag) currnode)
		(srecode-insert "declaration:subnode" "NAME" menuname)
	      (srecode-insert "declaration:node" "NAME" menuname))
	    )
	  )))
    ))

;;;###autoload
(defun srecode-semantic-handle-:texi (dict)
  "Add macros into the dictionary DICT based on the current texinfo file.
Adds the following:
  LEVEL - chapter, section, subsection, etc
  NEXTLEVEL - One below level"

  ;; LEVEL and NEXTLEVEL calculation
  (semantic-fetch-tags)
  (let ((tags (reverse (semantic-find-tag-by-overlay)))
	(level nil))
    (while (and tags (not (semantic-tag-of-class-p (car tags) 'section)))
      (setq tags (cdr tags)))
    (when tags
      (save-excursion
	(goto-char (semantic-tag-start (car tags)))
	(when (looking-at "@node")
	  (forward-line 1)
	  (beginning-of-line))
	(when (looking-at "@\\(\\w+\\)")
	  (setq level (match-string 1))
	  )))
    (srecode-dictionary-set-value dict "LEVEL" (or level "chapter"))
    (let ((nl (assoc level '( ( nil . "top" )
			      ("top" . "chapter")
			      ("chapter" . "section")
			      ("section" . "subsection")
			      ("subsection" . "subsubsection")
			      ("subsubsection" . "subsubsection")
			      ))))
      (srecode-dictionary-set-value dict "NEXTLEVEL" (cdr nl))))
  )

;;;###autoload
(defun srecode-semantic-handle-:texitag (dict)
  "Add macros into the dictionary DICT based on the current :tag file.
Adds the following:
  TAGDOC - Texinfo formatted doc string for :tag."

  ;; If we also have a TAG, what is the doc?
  (let ((tag (srecode-dictionary-lookup-name dict "TAG"))
	(doc nil)
	)

    ;; If the user didn't apply :tag, then do so now.
    (when (not tag)
      (srecode-semantic-handle-:tag dict))

    (setq tag (srecode-dictionary-lookup-name dict "TAG"))

    (when (not tag)
      (error "No tag to insert for :texitag template argument"))

    ;; Extract the tag out of the compound object.
    (setq tag (oref tag :prime))

    ;; Extract the doc string
    (setq doc (semantic-documentation-for-tag tag))

    (when doc
      (srecode-dictionary-set-value dict "TAGDOC"
				    (srecode-texi-massage-to-texinfo
				     tag (semantic-tag-buffer tag)
				     doc)))
    ))

;;; OVERRIDES
;;
;; Override some semantic and srecode features with texi specific
;; versions.

(define-mode-local-override semantic-insert-foreign-tag
  texinfo-mode (foreign-tag)
  "Insert FOREIGN-TAG from a foreign buffer in TAGFILE.
Assume TAGFILE is a source buffer, and create a documentation
thingy from it using the `document' tool."
  (srecode-texi-insert-tag-as-doc foreign-tag))

(defun srecode-texi-insert-tag-as-doc (tag)
  "Insert TAG into the current buffer with SRecode."
  (when (not (eq major-mode 'texinfo-mode))
    (error "Can only insert tags into texinfo in texinfo mode"))
  (let ((srecode-semantic-selected-tag tag))
    (srecode-load-tables-for-mode major-mode)
    ;; @todo - choose of the many types of tags to insert,
    ;; or put all that logic into srecode.
    (srecode-insert "declaration:function")))



;;; Texinfo mangling.

(define-overloadable-function srecode-texi-texify-docstring
  (docstring)
  "Texify the doc string DOCSTRING.
Takes plain text formatting that may exist, and converts it to
using TeXinfo formatting.")

(defun srecode-texi-texify-docstring-default (docstring)
  "Texify the doc string DOCSTRING.
Takes a few very generic guesses as to what the formatting is."
  (let ((case-fold-search nil)
	(start 0))
    (while (string-match
	    "\\(^\\|[^{]\\)\\<\\([A-Z0-9_-]+\\)\\>\\($\\|[^}]\\)"
	    docstring start)
      (let ((ms (match-string 2 docstring)))
	;(when (eq mode 'emacs-lisp-mode)
	;  (setq ms (downcase ms)))

	(when (not (or (string= ms "A")
		       (string= ms "a")
		       ))
	  (setq docstring (concat (substring docstring 0 (match-beginning 2))
			       "@var{"
			       ms
			       "}"
			       (substring docstring (match-end 2))))))
      (setq start (match-end 2)))
    ;; Return our modified doc string.
    docstring))

(defun srecode-texi-massage-to-texinfo (tag buffer string)
  "Massage TAG's documentation from BUFFER as STRING.
This is to take advantage of TeXinfo's markup symbols."
  (save-excursion
    (if buffer
	(progn (set-buffer buffer)
	       (srecode-texi-texify-docstring string))
      ;; Else, no buffer, so let's do something else
      (with-mode-local texinfo-mode
	(srecode-texi-texify-docstring string)))))

(define-mode-local-override srecode-texi-texify-docstring emacs-lisp-mode
  (string)
  "Take STRING, (a normal doc string), and convert it into a texinfo string.
For instances where CLASS is the class being referenced, do not Xref
that class.

 `function' => @dfn{function}
 `variable' => @code{variable}
 `class'    => @code{class} @xref{class}
 `unknown'  => @code{unknown}
 \"text\"     => ``text''
 'quoteme   => @code{quoteme}
 non-nil    => non-@code{nil}
 t          => @code{t}
 :tag       => @code{:tag}
 [ stuff ]  => @code{[ stuff ]}
 Key        => @kbd{Key}     (key is C\\-h, M\\-h, SPC, RET, TAB and the like)
 ...        => @dots{}"
  (while (string-match "`\\([-a-zA-Z0-9<>.]+\\)'" string)
    (let* ((vs (substring string (match-beginning 1) (match-end 1)))
	   (v (intern-soft vs)))
      (setq string
	    (concat
	     (replace-match (concat
			     (if (fboundp v)
				 "@dfn{" "@code{")
			     vs "}")
		    nil t string)))))
  (while (string-match "\\( \\|^\\)\\(nil\\|t\\|'[-a-zA-Z0-9]+\\|:[-a-zA-Z0-9]+\\)\\([. ,]\\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(non-\\)\\(nil\\)\\)\\([. ,]\\|$\\)" string)
    (setq string (replace-match "\\3@code{\\4}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\[[^]]+\\]\\)\\( \\|$\\)" string)
    (setq string (replace-match "@code{\\2}" t nil string 2)))
  (while (string-match "\\( \\|^\\)\\(\\(\\(C-\\|M-\\|S-\\)+\\([^ \t\n]\\|RET\\|SPC\\|TAB\\)\\)\\|\\(RET\\|SPC\\|TAB\\)\\)\\( \\|\\s.\\|$\\)" string)
    (setq string (replace-match "@kbd{\\2}" t nil string 2)))
  (while (string-match "\"\\(.+\\)\"" string)
    (setq string (replace-match "``\\1''" t nil string 0)))
  (while (string-match "\\.\\.\\." string)
    (setq string (replace-match "@dots{}" t nil string 0)))
  ;; Also do base docstring type.
  (srecode-texi-texify-docstring-default string))

(provide 'srecode/texi)

;; Local variables:
;; generated-autoload-file: "loaddefs.el"
;; generated-autoload-load-name: "srecode/texi"
;; End:

;;; srecode/texi.el ends here
