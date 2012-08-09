;;; scribe.el --- scribe mode, and its idiosyncratic commands

;; Copyright (C) 1985, 2001-2012  Free Software Foundation, Inc.

;; Author: William Sommerfeld
;; (according to ack.texi)
;; Maintainer: FSF
;; Keywords: wp
;; Obsolete-since: 22.1

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

;; A major mode for editing source in written for the Scribe text formatter.
;; Knows about Scribe syntax and standard layout rules.  The command to
;; run Scribe on a buffer is bogus; someone interested should fix it.

;;; Code:

(defvar compile-command)

(defgroup scribe nil
  "Scribe mode."
  :prefix "scribe-"
  :group 'wp)

(defvar scribe-mode-syntax-table nil
  "Syntax table used while in scribe mode.")

(defvar scribe-mode-abbrev-table nil
  "Abbrev table used while in scribe mode.")

(defcustom scribe-fancy-paragraphs nil
  "*Non-nil makes Scribe mode use a different style of paragraph separation."
  :type 'boolean
  :group 'scribe)

(defcustom scribe-electric-quote nil
  "*Non-nil makes insert of double quote use `` or '' depending on context."
  :type 'boolean
  :group 'scribe)

(defcustom scribe-electric-parenthesis nil
  "*Non-nil makes parenthesis char ( (]}> ) automatically insert its close
if typed after an @Command form."
  :type 'boolean
  :group 'scribe)

(defconst scribe-open-parentheses "[({<"
  "Open parenthesis characters for Scribe.")

(defconst scribe-close-parentheses "])}>"
  "Close parenthesis characters for Scribe.
These should match up with `scribe-open-parenthesis'.")

(if (null scribe-mode-syntax-table)
    (let ((st (syntax-table)))
      (unwind-protect
       (progn
	(setq scribe-mode-syntax-table (copy-syntax-table
					text-mode-syntax-table))
	(set-syntax-table scribe-mode-syntax-table)
	(modify-syntax-entry ?\" "    ")
	(modify-syntax-entry ?\\ "    ")
	(modify-syntax-entry ?@ "w   ")
	(modify-syntax-entry ?< "(>  ")
	(modify-syntax-entry ?> ")<  ")
	(modify-syntax-entry ?[ "(]  ")
	(modify-syntax-entry ?] ")[  ")
	(modify-syntax-entry ?{ "(}  ")
	(modify-syntax-entry ?} "){  ")
	(modify-syntax-entry ?' "w   "))
       (set-syntax-table st))))

(defvar scribe-mode-map nil)

(if scribe-mode-map
    nil
  (setq scribe-mode-map (make-sparse-keymap))
  (define-key scribe-mode-map "\t" 'scribe-tab)
  (define-key scribe-mode-map "\e\t" 'tab-to-tab-stop)
  (define-key scribe-mode-map "\es" 'center-line)
  (define-key scribe-mode-map "\e}" 'up-list)
  (define-key scribe-mode-map "\eS" 'center-paragraph)
  (define-key scribe-mode-map "\"" 'scribe-insert-quote)
  (define-key scribe-mode-map "(" 'scribe-parenthesis)
  (define-key scribe-mode-map "[" 'scribe-parenthesis)
  (define-key scribe-mode-map "{" 'scribe-parenthesis)
  (define-key scribe-mode-map "<" 'scribe-parenthesis)
  (define-key scribe-mode-map "\C-c\C-c" 'scribe-chapter)
  (define-key scribe-mode-map "\C-c\C-t" 'scribe-section)
  (define-key scribe-mode-map "\C-c\C-s" 'scribe-subsection)
  (define-key scribe-mode-map "\C-c\C-v" 'scribe-insert-environment)
  (define-key scribe-mode-map "\C-c\C-e" 'scribe-bracket-region-be)
  (define-key scribe-mode-map "\C-c[" 'scribe-begin)
  (define-key scribe-mode-map "\C-c]" 'scribe-end)
  (define-key scribe-mode-map "\C-c\C-i" 'scribe-italicize-word)
  (define-key scribe-mode-map "\C-c\C-b" 'scribe-bold-word)
  (define-key scribe-mode-map "\C-c\C-u" 'scribe-underline-word))

;;;###autoload
(define-derived-mode scribe-mode text-mode "Scribe"
  "Major mode for editing files of Scribe (a text formatter) source.
Scribe-mode is similar to text-mode, with a few extra commands added.
\\{scribe-mode-map}

Interesting variables:

`scribe-fancy-paragraphs'
  Non-nil makes Scribe mode use a different style of paragraph separation.

`scribe-electric-quote'
  Non-nil makes insert of double quote use `` or '' depending on context.

`scribe-electric-parenthesis'
  Non-nil makes an open-parenthesis char (one of `([<{')
  automatically insert its close if typed after an @Command form."
  (set (make-local-variable 'comment-start) "@Comment[")
  (set (make-local-variable 'comment-start-skip) (concat "@Comment[" scribe-open-parentheses "]"))
  (set (make-local-variable 'comment-column) 0)
  (set (make-local-variable 'comment-end) "]")
  (set (make-local-variable 'paragraph-start)
       (concat "\\([\n\f]\\)\\|\\(@\\w+["
	       scribe-open-parentheses
	       "].*["
	       scribe-close-parentheses
	       "]$\\)"))
  (set (make-local-variable 'paragraph-separate)
       (if scribe-fancy-paragraphs paragraph-start "$"))
  (set (make-local-variable 'sentence-end)
       "\\([.?!]\\|@:\\)[]\"')}]*\\($\\| $\\|\t\\|  \\)[ \t\n]*")
  (set (make-local-variable 'compile-command)
       (concat "scribe " (buffer-file-name))))

(defun scribe-tab ()
  (interactive)
  (insert "@\\"))

;; This algorithm could probably be improved somewhat.
;;  Right now, it loses seriously...

(defun scribe ()
  "Run Scribe on the current buffer."
  (interactive)
  (call-interactively 'compile))

(defun scribe-envelop-word (string count)
  "Surround current word with Scribe construct @STRING[...].
COUNT specifies how many words to surround.  A negative count means
to skip backward."
  (let ((spos (point)) (epos (point)) (ccoun 0) noparens)
    (if (not (zerop count))
	(progn (if (= (char-syntax (preceding-char)) ?w)
		   (forward-sexp (min -1 count)))
	       (setq spos (point))
	       (if (looking-at (concat "@\\w[" scribe-open-parentheses "]"))
		   (forward-char 2)
		 (goto-char epos)
		 (skip-chars-backward "\\W")
		 (forward-char -1))
	       (forward-sexp (max count 1))
	       (setq epos (point))))
    (goto-char spos)
    (while (and (< ccoun (length scribe-open-parentheses))
		(save-excursion
		  (or (search-forward (char-to-string
				       (aref scribe-open-parentheses ccoun))
				      epos t)
		      (search-forward (char-to-string
				       (aref scribe-close-parentheses ccoun))
				      epos t)))
		(setq ccoun (1+ ccoun))))
    (if (>= ccoun (length scribe-open-parentheses))
	(progn (goto-char epos)
	       (insert "@end(" string ")")
	       (goto-char spos)
	       (insert "@begin(" string ")"))
      (goto-char epos)
      (insert (aref scribe-close-parentheses ccoun))
      (goto-char spos)
      (insert "@" string (aref scribe-open-parentheses ccoun))
      (goto-char epos)
      (forward-char 3)
      (skip-chars-forward scribe-close-parentheses))))

(defun scribe-underline-word (count)
  "Underline COUNT words around point by means of Scribe constructs."
  (interactive "p")
  (scribe-envelop-word "u" count))

(defun scribe-bold-word (count)
  "Boldface COUNT words around point by means of Scribe constructs."
  (interactive "p")
  (scribe-envelop-word "b" count))

(defun scribe-italicize-word (count)
  "Italicize COUNT words around point by means of Scribe constructs."
  (interactive "p")
  (scribe-envelop-word "i" count))

(defun scribe-begin ()
  (interactive)
  (insert "\n")
  (forward-char -1)
  (scribe-envelop-word "Begin" 0)
  (re-search-forward (concat "[" scribe-open-parentheses "]")))

(defun scribe-end ()
  (interactive)
  (insert "\n")
  (forward-char -1)
  (scribe-envelop-word "End" 0)
  (re-search-forward (concat "[" scribe-open-parentheses "]")))

(defun scribe-chapter ()
  (interactive)
  (insert "\n")
  (forward-char -1)
  (scribe-envelop-word "Chapter" 0)
  (re-search-forward (concat "[" scribe-open-parentheses "]")))

(defun scribe-section ()
  (interactive)
  (insert "\n")
  (forward-char -1)
  (scribe-envelop-word "Section" 0)
  (re-search-forward (concat "[" scribe-open-parentheses "]")))

(defun scribe-subsection ()
  (interactive)
  (insert "\n")
  (forward-char -1)
  (scribe-envelop-word "SubSection" 0)
  (re-search-forward (concat "[" scribe-open-parentheses "]")))

(defun scribe-bracket-region-be (env min max)
  (interactive "sEnvironment: \nr")
  (save-excursion
    (goto-char max)
    (insert "@end(" env ")\n")
    (goto-char min)
    (insert "@begin(" env ")\n")))

(defun scribe-insert-environment (env)
  (interactive "sEnvironment: ")
  (scribe-bracket-region-be env (point) (point))
  (forward-line 1)
  (insert ?\n)
  (forward-char -1))

(defun scribe-insert-quote (count)
  "Insert ``, '' or \" according to preceding character.
If `scribe-electric-quote' is non-nil, insert ``, '' or \" according
to preceding character.  With numeric arg N, always insert N \" characters.
Else just insert \"."
  (interactive "P")
  (if (or count (not scribe-electric-quote))
      (self-insert-command (prefix-numeric-value count))
    (let (lastfore lastback lastquote)
      (insert
       (cond
	((= (preceding-char) ?\\) ?\")
	((bobp) "``")
	(t
	 (setq lastfore (save-excursion (and (search-backward
					      "``" (- (point) 1000) t)
					     (point)))
	       lastback (save-excursion (and (search-backward
					      "''" (- (point) 1000) t)
					     (point)))
	       lastquote (save-excursion (and (search-backward
					       "\"" (- (point) 100) t)
					      (point))))
	 (if (not lastquote)
	     (cond ((not lastfore) "``")
		   ((not lastback) "''")
		   ((> lastfore lastback) "''")
		   (t "``"))
	   (cond ((and (not lastback) (not lastfore)) "\"")
		 ((and lastback (not lastfore) (> lastquote lastback)) "\"")
		 ((and lastback (not lastfore) (> lastback lastquote)) "``")
		 ((and lastfore (not lastback) (> lastquote lastfore)) "\"")
		 ((and lastfore (not lastback) (> lastfore lastquote)) "''")
		 ((and (> lastquote lastfore) (> lastquote lastback)) "\"")
		 ((> lastfore lastback) "''")
		 (t "``")))))))))

(defun scribe-parenthesis (count)
  "If scribe-electric-parenthesis is non-nil, insertion of an open-parenthesis
character inserts the following close parenthesis character if the
preceding text is of the form @Command."
  (interactive "P")
  (self-insert-command (prefix-numeric-value count))
  (let (at-command paren-char point-save)
    (if (or count (not scribe-electric-parenthesis))
	nil
      (save-excursion
	(forward-char -1)
	(setq point-save (point))
	(skip-chars-backward (concat "^ \n\t\f" scribe-open-parentheses))
	(setq at-command (and (equal (following-char) ?@)
			      (/= (point) (1- point-save)))))
      (if (and at-command
	       (setq paren-char
		     (string-match (regexp-quote
				    (char-to-string (preceding-char)))
				   scribe-open-parentheses)))
	  (save-excursion
	    (insert (aref scribe-close-parentheses paren-char)))))))

(provide 'scribe)

;;; scribe.el ends here
