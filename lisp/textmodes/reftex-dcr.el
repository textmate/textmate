;;; reftex-dcr.el --- viewing cross references and citations with RefTeX

;; Copyright (C) 1997-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Maintainer: auctex-devel@gnu.org
;; Version: 4.31
;; Package: reftex

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

(eval-when-compile (require 'cl))
(provide 'reftex-dcr)
(provide 'reftex-vcr)
(require 'reftex)
;;;

(defun reftex-view-crossref (&optional arg auto-how fail-quietly)
  "View cross reference of macro at point.  Point must be on the KEY
argument.  When at a `\\ref' macro, show corresponding `\\label'
definition, also in external documents (`xr').  When on a label, show
a locations where KEY is referenced.  Subsequent calls find additional
locations.  When on a `\\cite', show the associated `\\bibitem' macro or
the BibTeX database entry.  When on a `\\bibitem', show a `\\cite' macro
which uses this KEY. When on an `\\index', show other locations marked
by the same index entry.
To define additional cross referencing items, use the option
`reftex-view-crossref-extra'.  See also `reftex-view-crossref-from-bibtex'.
With one or two C-u prefixes, enforce rescanning of the document.
With argument 2, select the window showing the cross reference.
AUTO-HOW is only for the automatic crossref display and is handed through
to the functions `reftex-view-cr-cite' and `reftex-view-cr-ref'."

  (interactive "P")
  ;; See where we are.
  (let* ((macro (car (reftex-what-macro-safe 1)))
         (key (reftex-this-word "^{}%\n\r, \t"))
         dw)

    (if (or (null macro) (reftex-in-comment))
	(or fail-quietly
	    (error "Not on a crossref macro argument"))

      (setq reftex-call-back-to-this-buffer (current-buffer))

      (cond
       ((string-match "\\`\\\\cite\\|cite\\*?\\'\\|bibentry" macro)
	;; A citation macro: search for bibitems or BibTeX entries
	(setq dw (reftex-view-cr-cite arg key auto-how)))
       ((string-match "\\`\\\\ref\\|ref\\(range\\)?\\*?\\'" macro)
	;; A reference macro: search for labels
	(setq dw (reftex-view-cr-ref arg key auto-how)))
       (auto-how nil)  ;; No further action for automatic display (speed)
       ((or (equal macro "\\label")
	    (member macro reftex-macros-with-labels))
	;; A label macro: search for reference macros
	(reftex-access-scan-info arg)
	(setq dw (reftex-view-regexp-match
		  (format reftex-find-reference-format (regexp-quote key))
		  4 nil nil)))
       ((equal macro "\\bibitem")
	;; A bibitem macro: search for citations
	(reftex-access-scan-info arg)
	(setq dw (reftex-view-regexp-match
		  (format reftex-find-citation-regexp-format (regexp-quote key))
		  4 nil nil)))
       ((member macro reftex-macros-with-index)
	(reftex-access-scan-info arg)
	(setq dw (reftex-view-regexp-match
		  (format reftex-find-index-entry-regexp-format
			  (regexp-quote key))
		  3 nil nil)))
       (t
	(reftex-access-scan-info arg)
	(catch 'exit
	  (let ((list reftex-view-crossref-extra)
		entry mre action group)
	    (while (setq entry (pop list))
	      (setq mre (car entry)
		    action (nth 1 entry)
		    group (nth 2 entry))
	      (when (string-match mre macro)
		(setq dw (reftex-view-regexp-match
			  (format action key) group nil nil))
		(throw 'exit t))))
	  (error "Not on a crossref macro argument"))))
      (if (and (eq arg 2) (windowp dw)) (select-window dw)))))

(defun reftex-view-cr-cite (arg key how)
  ;; View crossreference of a ref cite.  HOW can have the values
  ;; nil:         Show in another window.
  ;; echo:        Show one-line info in echo area.
  ;; tmp-window:  Show in small window and arrange for window to disappear.

  ;; Ensure access to scanning info
  (reftex-access-scan-info (or arg current-prefix-arg))

  (if (eq how 'tmp-window)
      ;; Remember the window configuration
      (put 'reftex-auto-view-crossref 'last-window-conf
           (current-window-configuration)))

  (let (files size item (pos (point)) (win (selected-window)) pop-win
              (bibtype (reftex-bib-or-thebib)))
    ;; Find the citation mode and the file list
    (cond
;     ((assq 'bib (symbol-value reftex-docstruct-symbol))
     ((eq bibtype 'bib)
      (setq item nil
            files (reftex-get-bibfile-list)))
;     ((assq 'thebib (symbol-value reftex-docstruct-symbol))
     ((eq bibtype 'thebib)
      (setq item t
            files (reftex-uniquify
                   (mapcar 'cdr
                           (reftex-all-assq
                            'thebib (symbol-value reftex-docstruct-symbol))))))
     (reftex-default-bibliography
      (setq item nil
            files (reftex-default-bibliography)))
     (how)  ;; don't throw for special display
     (t (error "Cannot display crossref")))

    (if (eq how 'echo)
        ;; Display in Echo area
        (reftex-echo-cite key files item)
      ;; Display in a window
      (if (not (eq how 'tmp-window))
          ;; Normal display
          (reftex-pop-to-bibtex-entry key files nil t item)
        ;; A temporary window
        (condition-case nil
            (reftex-pop-to-bibtex-entry key files nil t item)
          (error (goto-char pos)
                 (message "cite: no such citation key %s" key)
                 (error "")))
        ;; Resize the window
        (setq size (max 1 (count-lines (point)
                                       (reftex-end-of-bib-entry item))))
        (let ((window-min-height 2))
          (shrink-window (1- (- (window-height) size)))
          (recenter 0))
        ;; Arrange restoration
        (add-hook 'pre-command-hook 'reftex-restore-window-conf))

        ;; Normal display in other window
      (add-hook 'pre-command-hook 'reftex-highlight-shall-die)
      (setq pop-win (selected-window))
      (select-window win)
      (goto-char pos)
      (when (equal arg 2)
        (select-window pop-win)))))

(defun reftex-view-cr-ref (arg label how)
  ;; View crossreference of a ref macro.  HOW can have the values
  ;; nil:         Show in another window.
  ;; echo:        Show one-line info in echo area.
  ;; tmp-window:  Show in small window and arrange for window to disappear.

  ;; Ensure access to scanning info
  (reftex-access-scan-info (or arg current-prefix-arg))

  (if (eq how 'tmp-window)
      ;; Remember the window configuration
      (put 'reftex-auto-view-crossref 'last-window-conf
           (current-window-configuration)))

  (let* ((xr-data (assoc 'xr (symbol-value reftex-docstruct-symbol)))
         (xr-re (nth 2 xr-data))
         (entry (assoc label (symbol-value reftex-docstruct-symbol)))
         (win (selected-window)) pop-win (pos (point)))

    (if (and (not entry) (stringp label) xr-re (string-match xr-re label))
        ;; Label is defined in external document
        (save-excursion
          (save-match-data
            (set-buffer
             (or (reftex-get-file-buffer-force
                  (cdr (assoc (match-string 1 label) (nth 1
                                                          xr-data))))
                 (error "Problem with external label %s" label))))
          (setq label (substring label (match-end 1)))
          (reftex-access-scan-info)
          (setq entry
                (assoc label (symbol-value reftex-docstruct-symbol)))))
    (if (eq how 'echo)
        ;; Display in echo area
        (reftex-echo-ref label entry (symbol-value reftex-docstruct-symbol))
      (let ((window-conf (current-window-configuration)))
        (condition-case nil
            (reftex-show-label-location entry t nil t t)
          (error (set-window-configuration window-conf)
                 (message "ref: Label %s not found" label)
                 (error "ref: Label %s not found" label)))) ;; 2nd is line OK
      (add-hook 'pre-command-hook 'reftex-highlight-shall-die)

      (when (eq how 'tmp-window)
        ;; Resize window and arrange restoration
        (shrink-window (1- (- (window-height) 9)))
        (recenter '(4))
        (add-hook 'pre-command-hook 'reftex-restore-window-conf))
      (setq pop-win (selected-window))
      (select-window win)
      (goto-char pos)
      (when (equal arg 2)
        (select-window pop-win)))))

(defun reftex-mouse-view-crossref (ev)
  "View cross reference of \\ref or \\cite macro where you click.
If the macro at point is a \\ref, show the corresponding label definition.
If it is a \\cite, show the BibTeX database entry.
If there is no such macro at point, search forward to find one.
With argument, actually select the window showing the cross reference."
  (interactive "e")
  (mouse-set-point ev)
  (reftex-view-crossref current-prefix-arg))

(defun reftex-view-crossref-when-idle ()
  ;; Display info about crossref at point in echo area or a window.
  ;; This function was designed to work with an idle timer.
  ;; We try to get out of here as quickly as possible if the call is useless.
  (and reftex-mode
       ;; Make sure message area is free if we need it.
       (or (eq reftex-auto-view-crossref 'window) (not (current-message)))
       ;; Make sure we are not already displaying this one
       (not (memq last-command '(reftex-view-crossref
                                 reftex-mouse-view-crossref)))
       ;; Quick precheck if this might be a relevant spot
       ;; `reftex-view-crossref' will do a more thorough check.
       (save-excursion
         (search-backward "\\" nil t)
         (looking-at "\\\\[a-zA-Z]*\\(cite\\|ref\\|bibentry\\)"))

       (condition-case nil
           (let ((current-prefix-arg nil))
             (cond
              ((eq reftex-auto-view-crossref t)
               (reftex-view-crossref -1 'echo 'quiet))
              ((eq reftex-auto-view-crossref 'window)
               (reftex-view-crossref -1 'tmp-window 'quiet))
              (t nil)))
         (error nil))))

(defun reftex-restore-window-conf ()
  (set-window-configuration (get 'reftex-auto-view-crossref 'last-window-conf))
  (put 'reftex-auto-view-crossref 'last-window-conf nil)
  (remove-hook 'pre-command-hook 'reftex-restore-window-conf))

(defun reftex-echo-ref (label entry docstruct)
  ;; Display crossref info in echo area.
  (cond
   ((null docstruct)
    (message "%s"
	     (substitute-command-keys (format reftex-no-info-message "ref"))))
   ((null entry)
    (message "ref: unknown label: %s" label))
   (t
    (when (stringp (nth 2 entry))
      (message "ref(%s): %s" (nth 1 entry) (nth 2 entry)))
    (let ((buf (get-buffer " *Echo Area*")))
      (when buf
        (with-current-buffer buf
          (run-hooks 'reftex-display-copied-context-hook)))))))

(defun reftex-echo-cite (key files item)
  ;; Display citation info in echo area.
  (let* ((cache (assq 'bibview-cache (symbol-value reftex-docstruct-symbol)))
         (cache-entry (assoc key (cdr cache)))
         entry string buf (all-files files))

    (if (and reftex-cache-cite-echo cache-entry)
        ;; We can just use the cache
        (setq string (cdr cache-entry))

      ;; Need to look in the database
      (unless reftex-revisit-to-echo
        (setq files (reftex-visited-files files)))

      (setq entry
            (condition-case nil
                (save-excursion
                  (reftex-pop-to-bibtex-entry key files nil nil item t))
              (error
               (if (and files (= (length all-files) (length files)))
                   (message "cite: no such database entry: %s" key)
                 (message "%s" (substitute-command-keys
				(format reftex-no-info-message "cite"))))
               nil)))
      (when entry
        (if item
            (setq string (reftex-nicify-text entry))
          (setq string (reftex-make-cite-echo-string
                        (reftex-parse-bibtex-entry entry)
                        reftex-docstruct-symbol)))))
    (unless (or (null string) (equal string ""))
      (message "cite: %s" string))
    (when (setq buf (get-buffer " *Echo Area*"))
      (with-current-buffer buf
        (run-hooks 'reftex-display-copied-context-hook)))))

(defvar reftex-use-itimer-in-xemacs nil
  "*Non-nil means use the idle timers in XEmacs for crossref display.
Currently, idle timer restart is broken and we use the post-command-hook.")

(defun reftex-toggle-auto-view-crossref ()
  "Toggle the automatic display of crossref information in the echo area.
When active, leaving point idle in the argument of a \\ref or \\cite macro
will display info in the echo area."
  (interactive)
  (if reftex-auto-view-crossref-timer
      (progn
        (if (featurep 'xemacs)
            (if reftex-use-itimer-in-xemacs
                (delete-itimer reftex-auto-view-crossref-timer)
              (remove-hook 'post-command-hook 'reftex-start-itimer-once))
          (cancel-timer reftex-auto-view-crossref-timer))
        (setq reftex-auto-view-crossref-timer nil)
        (message "Automatic display of crossref information was turned off"))
    (setq reftex-auto-view-crossref-timer
          (if (featurep 'xemacs)
              (if reftex-use-itimer-in-xemacs
                  (start-itimer "RefTeX Idle Timer"
                                'reftex-view-crossref-when-idle
                                reftex-idle-time reftex-idle-time t)
                (add-hook 'post-command-hook 'reftex-start-itimer-once)
                t)
            (run-with-idle-timer
             reftex-idle-time t 'reftex-view-crossref-when-idle)))
    (unless reftex-auto-view-crossref
      (setq reftex-auto-view-crossref t))
    (message "Automatic display of crossref information was turned on")))

(defun reftex-start-itimer-once ()
   (and (featurep 'xemacs) reftex-mode
        (not (itimer-live-p reftex-auto-view-crossref-timer))
        (setq reftex-auto-view-crossref-timer
              (start-itimer "RefTeX Idle Timer"
                            'reftex-view-crossref-when-idle
                            reftex-idle-time nil t))))

(declare-function bibtex-beginning-of-entry "bibtex" ())

(defun reftex-view-crossref-from-bibtex (&optional arg)
  "View location in a LaTeX document which cites the BibTeX entry at point.
Since BibTeX files can be used by many LaTeX documents, this function
prompts upon first use for a buffer in RefTeX mode.  To reset this
link to a document, call the function with a prefix arg.
Calling this function several times find successive citation locations."
  (interactive "P")
  (when arg
    ;; Break connection to reference buffer
    (put 'reftex-bibtex-view-cite-locations :ref-buffer nil))
  (let ((ref-buffer (get 'reftex-bibtex-view-cite-locations :ref-buffer)))
    ;; Establish connection to reference buffer
    (unless ref-buffer
      (setq ref-buffer
            (save-current-buffer
              (completing-read
               "Reference buffer: "
               (delq nil
                     (mapcar
                      (lambda (b)
                        (set-buffer b)
                        (if reftex-mode (list (buffer-name b)) nil))
                      (buffer-list)))
               nil t)))
      (put 'reftex-bibtex-view-cite-locations :ref-buffer ref-buffer))
    ;; Search for citations
    (bibtex-beginning-of-entry)
    (if (looking-at
         "@[a-zA-Z]+[ \t\n\r]*[{(][ \t\n\r]*\\([^, \t\r\n}]+\\)")
        (progn
          (goto-char (match-beginning 1))
          (reftex-view-regexp-match
           (format reftex-find-citation-regexp-format
                   (regexp-quote (match-string 1)))
           4 arg ref-buffer))
      (error "Cannot find citation key in BibTeX entry"))))

(defun reftex-view-regexp-match (re &optional highlight-group new ref-buffer)
  ;; Search for RE in current document or in the document of REF-BUFFER.
  ;; Continue the search, if the same re was searched last.
  ;; Highlight the group HIGHLIGHT-GROUP of the match.
  ;; When NEW is non-nil, start a new search regardless.
  ;; Match point is displayed in another window.
  ;; Upon success, returns the window which displays the match.

  ;;; Decide if new search or continued search
  (let* ((oldprop (get 'reftex-view-regexp-match :props))
         (newprop (list (current-buffer) re))
         (cont (and (not new) (equal oldprop newprop)))
         (cnt (if cont (get 'reftex-view-regexp-match :cnt) 0))
         (current-window (selected-window))
         (window-conf (current-window-configuration))
         match pop-window)
    (switch-to-buffer-other-window (or ref-buffer (current-buffer)))
    ;; Search
    (condition-case nil
        (if cont
            (setq match (reftex-global-search-continue))
          (reftex-access-scan-info)
          (setq match (reftex-global-search re (reftex-all-document-files))))
      (error nil))
    ;; Evaluate the match.
    (if match
        (progn
          (put 'reftex-view-regexp-match :props newprop)
          (put 'reftex-view-regexp-match :cnt (incf cnt))
          (reftex-highlight 0 (match-beginning highlight-group)
                            (match-end highlight-group))
          (add-hook 'pre-command-hook 'reftex-highlight-shall-die)
          (setq pop-window (selected-window)))
      (put 'reftex-view-regexp-match :props nil)
      (or cont (set-window-configuration window-conf)))
    (select-window current-window)
    (if match
        (progn
          (message "Match Nr. %s" cnt)
          pop-window)
      (if cont
          (error "No further matches (total number of matches: %d)" cnt)
        (error "No matches")))))

(defvar reftex-global-search-marker (make-marker))
(defun reftex-global-search (regexp file-list)
  ;; Start a search for REGEXP in all files of FILE-LIST
  (put 'reftex-global-search :file-list file-list)
  (put 'reftex-global-search :regexp regexp)
  (move-marker reftex-global-search-marker nil)
  (reftex-global-search-continue))

(defun reftex-global-search-continue ()
  ;; Continue a global search started with `reftex-global-search'
  (unless (get 'reftex-global-search :file-list)
    (error "No global search to continue"))
  (let* ((file-list (get 'reftex-global-search :file-list))
         (regexp (get 'reftex-global-search :regexp))
         (buf (or (marker-buffer reftex-global-search-marker)
                  (reftex-get-file-buffer-force (car file-list))))
         (pos (or (marker-position reftex-global-search-marker) 1))
         file)
    ;; Take up starting position
    (unless buf (error "No such buffer %s" buf))
    (switch-to-buffer buf)
    (widen)
    (goto-char pos)
    ;; Search and switch file if necessary
    (if (catch 'exit
          (while t
            (when (re-search-forward regexp nil t)
              (move-marker reftex-global-search-marker (point))
              (throw 'exit t))
            ;; No match - goto next file
            (pop file-list)
            (or file-list (throw 'exit nil))
            (setq file (car file-list)
                  buf (reftex-get-file-buffer-force file))
            (unless buf (error "Cannot access file %s" file))
            (put 'reftex-global-search :file-list file-list)
            (switch-to-buffer buf)
            (widen)
            (goto-char 1)))
        t
      (move-marker reftex-global-search-marker nil)
      (error "All files processed"))))

;;; reftex-dcr.el ends here
