;;; reftex-parse.el --- parser functions for RefTeX

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
(provide 'reftex-parse)
(require 'reftex)

(defmacro reftex-with-special-syntax (&rest body)
  `(let ((saved-syntax (syntax-table)))
     (unwind-protect
         (progn
           (set-syntax-table reftex-syntax-table)
           (let ((case-fold-search nil))
             ,@body))
       (set-syntax-table saved-syntax))))

(defun reftex-parse-one ()
  "Re-parse this file."
  (interactive)
  (let ((reftex-enable-partial-scans t))
    (reftex-access-scan-info '(4))))

(defun reftex-parse-all ()
  "Re-parse entire document."
  (interactive)
  (reftex-access-scan-info '(16)))

(defun reftex-do-parse (rescan &optional file)
  "Do a document rescan.  When allowed, do only a partial scan from FILE."

  ;; Normalize the rescan argument
  (setq rescan (cond ((eq rescan t) t)
                     ((eq rescan 1) 1)
                     ((equal rescan '(4)) t)
                     ((equal rescan '(16)) 1)
                     (t 1)))

  ;; Partial scans only when allowed
  (unless reftex-enable-partial-scans
    (setq rescan 1))

  ;; Do the scanning.

  (let* ((old-list (symbol-value reftex-docstruct-symbol))
         (master (reftex-TeX-master-file))
         (true-master (file-truename master))
         (master-dir (file-name-as-directory (file-name-directory master)))
         (file (or file (buffer-file-name)))
         (true-file (file-truename file))
         (bibview-cache (assq 'bibview-cache old-list))
         (index-tags (cdr (assq 'index-tags old-list)))
         from-file appendix docstruct tmp)

    ;; Make sure replacement is really an option here
    (when (and (eq rescan t)
               (not (and (member (list 'bof file) old-list)
                         (member (list 'eof file) old-list))))
      ;; Scan whole document because no such file section exists
      (setq rescan 1))
    (when (string= true-file true-master)
      ;; Scan whole document because this file is the master
      (setq rescan 1))

    ;; From which file do we start?
    (setq from-file
          (cond ((eq rescan t) (or file master))
                ((eq rescan 1) master)
                (t (error "This should not happen (reftex-do-parse)"))))

    ;; Reset index-tags if we scan everything
    (if (equal rescan 1) (setq index-tags nil))

    ;; Find active toc entry and initialize section-numbers
    (setq reftex-active-toc (reftex-last-assoc-before-elt
                             'toc (list 'bof from-file) old-list)
          appendix (reftex-last-assoc-before-elt
                    'appendix (list 'bof from-file) old-list))

    (reftex-init-section-numbers reftex-active-toc appendix)

    (if (eq rescan 1)
        (message "Scanning entire document...")
      (message "Scanning document from %s..." from-file))

    (reftex-with-special-syntax
     (save-window-excursion
       (save-excursion
         (unwind-protect
             (setq docstruct
                   (reftex-parse-from-file
                    from-file docstruct master-dir))
           (reftex-kill-temporary-buffers)))))

    (message "Scanning document... done")

    ;; Turn the list around.
    (setq docstruct (nreverse docstruct))

    ;; Set or insert
    (setq docstruct (reftex-replace-label-list-segment
                     old-list docstruct (eq rescan 1)))

    ;; Add all missing information
    (unless (assq 'label-numbers docstruct)
      (push (cons 'label-numbers nil) docstruct))
    (unless (assq 'master-dir docstruct)
      (push (cons 'master-dir master-dir) docstruct))
    (unless (assq 'bibview-cache docstruct)
      (push (cons 'bibview-cache (cdr bibview-cache)) docstruct))
    (let* ((bof1 (memq (assq 'bof docstruct) docstruct))
           (bof2 (assq 'bof (cdr bof1)))
           (is-multi (not (not (and bof1 bof2))))
           (entry (or (assq 'is-multi docstruct)
                      (car (push (list 'is-multi is-multi) docstruct)))))
      (setcdr entry (cons is-multi nil)))
    (and index-tags (setq index-tags (sort index-tags 'string<)))
    (let ((index-tag-cell (assq 'index-tags docstruct)))
      (if index-tag-cell
          (setcdr index-tag-cell index-tags)
        (push (cons 'index-tags index-tags) docstruct)))
    (unless (assq 'xr docstruct)
      (let* ((allxr (reftex-all-assq 'xr-doc docstruct))
             (alist (mapcar
                     (lambda (x)
                       (if (setq tmp (reftex-locate-file (nth 2 x) "tex"
                                                         master-dir))
                           (cons (nth 1 x) tmp)
                         (message "Can't find external document %s"
                                  (nth 2 x))
                         nil))
                     allxr))
             (alist (delq nil alist))
             (allprefix (delq nil (mapcar 'car alist)))
             (regexp (if allprefix
                         (concat "\\`\\("
                                 (mapconcat 'identity allprefix "\\|")
                                 "\\)")
                       "\\\\\\\\\\\\")))   ; this will never match
        (push (list 'xr alist regexp) docstruct)))

    (set reftex-docstruct-symbol docstruct)
    (put reftex-docstruct-symbol 'modified t)))

(defun reftex-everything-regexp ()
  (if reftex-support-index
      reftex-everything-regexp
    reftex-everything-regexp-no-index))

;;;###autoload
(defun reftex-all-document-files (&optional relative)
  "Return a list of all files belonging to the current document.
When RELATIVE is non-nil, give file names relative to directory
of master file."
  (let* ((all (symbol-value reftex-docstruct-symbol))
         (master-dir (file-name-directory (reftex-TeX-master-file)))
         (re (concat "\\`" (regexp-quote master-dir)))
        file-list tmp file)
    (while (setq tmp (assoc 'bof all))
      (setq file (nth 1 tmp)
            all (cdr (memq tmp all)))
      (and relative
           (string-match re file)
           (setq file (substring file (match-end 0))))
      (push file file-list))
    (nreverse file-list)))

;; Bound in the caller, reftex-do-parse.
(defvar index-tags)

(defun reftex-parse-from-file (file docstruct master-dir)
  ;; Scan the buffer for labels and save them in a list.
  (let ((regexp (reftex-everything-regexp))
        (bound 0)
        file-found tmp include-file
        (level 1)
        (highest-level 100)
        toc-entry index-entry next-buf buf)

    (catch 'exit
      (setq file-found (reftex-locate-file file "tex" master-dir))
      (if (and (not file-found)
               (setq buf (reftex-get-buffer-visiting file)))
          (setq file-found (buffer-file-name buf)))

      (unless file-found
        (push (list 'file-error file) docstruct)
        (throw 'exit nil))

      (save-excursion

        (message "Scanning file %s" file)
        (set-buffer
         (setq next-buf
               (reftex-get-file-buffer-force
                file-found
                (not (eq t reftex-keep-temporary-buffers)))))

        ;; Begin of file mark
        (setq file (buffer-file-name))
        (push (list 'bof file) docstruct)

        (reftex-with-special-syntax
         (save-excursion
           (save-restriction
             (widen)
             (goto-char 1)

             (while (re-search-forward regexp nil t)

               (cond

                ((match-end 1)
                 ;; It is a label
                 (push (reftex-label-info (reftex-match-string 1) file bound)
                       docstruct))

                ((match-end 3)
                 ;; It is a section
                 (setq bound (point))

                 ;; Insert in List
                 (setq toc-entry (reftex-section-info file))
                 (when toc-entry
                   ;; It can happen that section info returns nil
                   (setq level (nth 5 toc-entry))
                   (setq highest-level (min highest-level level))
                   (if (= level highest-level)
                       (message
                        "Scanning %s %s ..."
                        (car (rassoc level reftex-section-levels-all))
                        (nth 6 toc-entry)))

                   (push toc-entry docstruct)
                   (setq reftex-active-toc toc-entry)))

                ((match-end 7)
                 ;; It's an include or input
                 (setq include-file (reftex-match-string 7))
                 ;; Test if this file should be ignored
                 (unless (delq nil (mapcar
                                    (lambda (x) (string-match x include-file))
                                    reftex-no-include-regexps))
                   ;; Parse it
                   (setq docstruct
                         (reftex-parse-from-file
                          include-file
                          docstruct master-dir))))

                ((match-end 9)
                 ;; Appendix starts here
                 (reftex-init-section-numbers nil t)
                 (push (cons 'appendix t) docstruct))

                ((match-end 10)
                 ;; Index entry
                 (when reftex-support-index
                   (setq index-entry (reftex-index-info file))
                   (when index-entry
                     (add-to-list 'index-tags (nth 1 index-entry))
                     (push index-entry docstruct))))

                ((match-end 11)
                 ;; A macro with label
                 (save-excursion
                   (let* ((mac (reftex-match-string 11))
                          (label (progn (goto-char (match-end 11))
                                        (save-match-data
                                          (reftex-no-props
                                           (reftex-nth-arg-wrapper
                                            mac)))))
                          (typekey (nth 1 (assoc mac reftex-env-or-mac-alist)))
                          (entry (progn (if typekey
                                            ;; A typing macro
                                            (goto-char (match-end 0))
                                          ;; A neutral macro
                                          (goto-char (match-end 11))
                                          (reftex-move-over-touching-args))
                                        (reftex-label-info
                                         label file bound nil nil))))
                     (push entry docstruct))))
                (t (error "This should not happen (reftex-parse-from-file)")))
               )

             ;; Find bibliography statement
             (when (setq tmp (reftex-locate-bibliography-files master-dir))
               (push (cons 'bib tmp) docstruct))

             (goto-char 1)
             (when (re-search-forward
                    "\\(\\`\\|[\n\r]\\)[ \t]*\\\\begin{thebibliography}" nil t)
               (push (cons 'thebib file) docstruct))

             ;; Find external document specifications
             (goto-char 1)
             (while (re-search-forward "[\n\r][ \t]*\\\\externaldocument\\(\\[\\([^]]*\\)\\]\\)?{\\([^}]+\\)}" nil t)
               (push (list 'xr-doc (reftex-match-string 2)
                           (reftex-match-string 3))
                     docstruct))

             ;; End of file mark
             (push (list 'eof file) docstruct)))))

      ;; Kill the scanned buffer
      (reftex-kill-temporary-buffers next-buf))

    ;; Return the list
    docstruct))

(defun reftex-locate-bibliography-files (master-dir &optional files)
  ;; Scan buffer for bibliography macro and return file list.

  (unless files
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward
           (concat
;           "\\(\\`\\|[\n\r]\\)[^%]*\\\\\\("
            "\\(^\\)[^%\n\r]*\\\\\\("
            (mapconcat 'identity reftex-bibliography-commands "\\|")
            "\\){[ \t]*\\([^}]+\\)") nil t)
          (setq files
                (split-string (reftex-match-string 3)
                              "[ \t\n\r]*,[ \t\n\r]*")))))
  (when files
    (setq files
          (mapcar
           (lambda (x)
             (if (or (member x reftex-bibfile-ignore-list)
                     (delq nil (mapcar (lambda (re) (string-match re x))
                                       reftex-bibfile-ignore-regexps)))
                 ;; excluded file
                 nil
               ;; find the file
               (reftex-locate-file x "bib" master-dir)))
           files))
    (delq nil files)))

(defun reftex-replace-label-list-segment (old insert &optional entirely)
  ;; Replace the segment in OLD which corresponds to INSERT.
  ;; Works with side effects, directly changes old.
  ;; If entirely is t, just return INSERT.
  ;; This function also makes sure the old toc markers do not point anywhere.

  (cond
   (entirely
    (reftex-silence-toc-markers old (length old))
    insert)
   (t (let* ((new old)
             (file (nth 1 (car insert)))
             (eof-list (member (list 'eof file) old))
             (bof-list (member (list 'bof file) old))
             n)
        (if (not (and bof-list eof-list))
            (error "Cannot splice")
          ;; Splice
          (reftex-silence-toc-markers bof-list (- (length bof-list)
                                                  (length eof-list)))
          (setq n (- (length old) (length bof-list)))
          (setcdr (nthcdr n new) (cdr insert))
          (setcdr (nthcdr (1- (length new)) new) (cdr eof-list)))
        new))))

(defun reftex-section-info (file)
  ;; Return a section entry for the current match.
  ;; Careful: This function expects the match-data to be still in place!
  (let* ((marker (set-marker (make-marker) (1- (match-beginning 3))))
         (macro (reftex-match-string 3))
         (prefix (save-match-data
                   (if (string-match "begin{\\([^}]+\\)}" macro)
                       (match-string 1 macro))))
         (level-exp (cdr (assoc macro reftex-section-levels-all)))
         (level (if (symbolp level-exp)
                    (save-match-data (funcall level-exp))
                  level-exp))
         (star (= ?* (char-after (match-end 3))))
         (unnumbered (or star (< level 0)))
         (level (abs level))
         (section-number (reftex-section-number level unnumbered))
         (text1 (save-match-data
                  (save-excursion
                    (reftex-context-substring prefix))))
         (literal (buffer-substring-no-properties
                   (1- (match-beginning 3))
                   (min (point-max) (+ (match-end 0) (length text1) 1))))
         ;; Literal can be too short since text1 too short. No big problem.
         (text (reftex-nicify-text text1)))

    ;; Add section number and indentation
    (setq text
          (concat
           (make-string (* reftex-level-indent level) ?\ )
           (if (nth 1 reftex-label-menu-flags) ; section number flag
               (concat section-number " "))
           (if prefix (concat (capitalize prefix) ": ") "")
           text))
    (list 'toc "toc" text file marker level section-number
          literal (marker-position marker))))

(defun reftex-ensure-index-support (&optional abort)
  ;; When index support is turned off, ask to turn it on and
  ;; set the current prefix argument so that `reftex-access-scan-info'
  ;; will rescan the entire document.
  (cond
   (reftex-support-index t)
   ((y-or-n-p "Turn on index support and rescan entire document? ")
    (setq reftex-support-index 'demanded
          current-prefix-arg '(16)))
   (t (if abort
          (error "No index support")
        (message "No index support")
        (ding)
        (sit-for 1)))))

(defun reftex-index-info-safe (file)
  (reftex-with-special-syntax
   (reftex-index-info file)))

(defvar test-dummy)
(defun reftex-index-info (file)
  ;; Return an index entry for the current match.
  ;; Careful: This function expects the match-data to be still in place!
  (catch 'exit
    (let* ((macro (reftex-match-string 10))
           (bom (match-beginning 10))
           (boa (match-end 10))
           (entry (or (assoc macro reftex-index-macro-alist)
                      (throw 'exit nil)))
           (exclude (nth 3 entry))
           ;; The following is a test if this match should be excluded
           (test-dummy (and (fboundp exclude)
                            (funcall exclude)
                            (throw 'exit nil)))
           (itag (nth 1 entry))
           (prefix (nth 2 entry))
           (index-tag
            (cond ((stringp itag) itag)
                  ((integerp itag)
                   (progn (goto-char boa)
                          (or (reftex-nth-arg itag (nth 6 entry)) "idx")))
                  (t "idx")))
           (arg (or (progn (goto-char boa)
                           (reftex-nth-arg (nth 5 entry) (nth 6 entry)))
                    ""))
           (end-of-args (progn (goto-char boa)
                               (reftex-move-over-touching-args)
                               (point)))
           (end-of-context (progn (skip-chars-forward "^ \t\n\r") (point)))
           (begin-of-context
            (progn (goto-char bom)
                   (skip-chars-backward "^ \t\r\n")
                   (point)))
           (context (buffer-substring-no-properties
                     begin-of-context end-of-context))
           (key-end (if (string-match reftex-index-key-end-re arg)
                        (1+ (match-beginning 0))))
           (rawkey (substring arg 0 key-end))

           (key (if prefix (concat prefix rawkey) rawkey))
           (sortkey (downcase key))
           (showkey (mapconcat 'identity
                               (split-string key reftex-index-level-re)
                               " ! ")))
      (goto-char end-of-args)
      ;;       0        1       2      3   4   5  6      7       8      9
      (list 'index index-tag context file bom arg key showkey sortkey key-end))))

(defun reftex-short-context (env parse &optional bound derive)
  ;; Get about one line of useful context for the label definition at point.

  (if (consp parse)
      (setq parse (if derive (cdr parse) (car parse))))

  (reftex-nicify-text

   (cond

    ((null parse)
     (save-excursion
       (reftex-context-substring)))

    ((eq parse t)
     (if (string= env "section")
         ;; special treatment for section labels
         (save-excursion
           (if (and (re-search-backward reftex-section-or-include-regexp
                                        (point-min) t)
                    (match-end 2))
               (progn
                 (goto-char (match-end 0))
                 (reftex-context-substring))
             (if reftex-active-toc
                 (progn
                   (string-match "{\\([^}]*\\)" (nth 7 reftex-active-toc))
                   (match-string 1 (nth 7 reftex-active-toc)))
               "SECTION HEADING NOT FOUND")))
       (save-excursion
         (goto-char reftex-default-context-position)
         (unless (eq (string-to-char env) ?\\)
           (reftex-move-over-touching-args))
         (reftex-context-substring))))

    ((stringp parse)
     (save-excursion
       (if (re-search-backward parse bound t)
           (progn
             (goto-char (match-end 0))
             (reftex-context-substring))
         "NO MATCH FOR CONTEXT REGEXP")))

    ((integerp parse)
     (or (save-excursion
           (goto-char reftex-default-context-position)
           (reftex-nth-arg
            parse
            (nth 6 (assoc env reftex-env-or-mac-alist))))
         ""))

    ((fboundp parse)
     ;; A hook function.  Call it.
     (save-excursion
       (condition-case error-var
           (funcall parse env)
         (error (format "HOOK ERROR: %s" (cdr error-var))))))
    (t
     "INVALID VALUE OF PARSE"))))

(defun reftex-where-am-I ()
  ;; Return the docstruct entry above point.  Actually returns a cons
  ;; cell in which the cdr is a flag indicating if the information is
  ;; exact (t) or approximate (nil).

  (let ((docstruct (symbol-value reftex-docstruct-symbol))
        (cnt 0) rtn rtn-if-no-other
        found)
    (save-excursion
      (while (not rtn)
        (incf cnt)
        (setq found (re-search-backward (reftex-everything-regexp) nil t))
        (setq rtn
              (cond
               ((not found)
                ;; no match
                (or
                 (car (member (list 'bof (buffer-file-name)) docstruct))
                 (not (setq cnt 2))
                 (assq 'bof docstruct)  ;; for safety reasons
                 'corrupted))
               ((match-end 1)
                ;; Label
                (assoc (reftex-match-string 1)
                       (symbol-value reftex-docstruct-symbol)))
               ((match-end 3)
                ;; Section
                (goto-char (1- (match-beginning 3)))
                (let* ((list (member (list 'bof (buffer-file-name))
                                     docstruct))
                       (endelt (car (member (list 'eof (buffer-file-name))
                                            list)))
                       rtn1)
                  (while (and list (not (eq endelt (car list))))
                    (if (and (eq (car (car list)) 'toc)
                             (string= (buffer-file-name)
                                      (nth 3 (car list))))
                        (cond
                         ((equal (point)
                                 (or (and (markerp (nth 4 (car list)))
                                          (marker-position (nth 4 (car list))))
                                     (nth 8 (car list))))
                          ;; Fits with marker position or recorded position
                          (setq rtn1 (car list) list nil))
                         ((looking-at (reftex-make-regexp-allow-for-ctrl-m
                                       (nth 7 (car list))))
                          ;; Same title: remember, but keep looking
                          (setq rtn-if-no-other (car list)))))
                    (pop list))
                  rtn1))
               ((match-end 7)
                ;; Input or include...
                (car
                 (member (list 'eof (reftex-locate-file
                                     (reftex-match-string 7) "tex"
                                     (cdr (assq 'master-dir docstruct))))
                         docstruct)))
               ((match-end 9)
                (assq 'appendix (symbol-value reftex-docstruct-symbol)))
               ((match-end 10)
                ;; Index entry
                (when reftex-support-index
                  (let* ((index-info (save-excursion
                                       (reftex-index-info-safe nil)))
                         (list (member (list 'bof (buffer-file-name))
                                       docstruct))
                         (endelt (car (member (list 'eof (buffer-file-name))
                                              list)))
                         dist last-dist last (n 0))
                    ;; Check all index entries with equal text
                    (while (and list (not (eq endelt (car list))))
                      (when (and (eq (car (car list)) 'index)
                                 (string= (nth 2 index-info)
                                          (nth 2 (car list))))
                        (incf n)
                        (setq dist (abs (- (point) (nth 4 (car list)))))
                        (if (or (not last-dist) (< dist last-dist))
                            (setq last-dist dist last (car list))))
                      (setq list (cdr list)))
                    ;; We are sure if we have only one, or a zero distance
                    (cond ((or (= n 1) (equal dist 0)) last)
                          ((> n 1) (setq cnt 2) last)
                          (t nil)))))
               ((match-end 11)
                (save-excursion
                  (goto-char (match-end 11))
                  (assoc (reftex-no-props
                          (reftex-nth-arg-wrapper
                           (reftex-match-string 11)))
                         (symbol-value reftex-docstruct-symbol))))
               (t
                (error "This should not happen (reftex-where-am-I)"))))))
    ;; Check if there was only a by-name match for the section.
    (when (and (not rtn) rtn-if-no-other)
      (setq rtn rtn-if-no-other
            cnt 2))
    (cons rtn (eq cnt 1))))

(defun reftex-notice-new (&optional n force)
  "Hook to handshake with RefTeX after something new has been inserted."
  ;; Add a new entry to the docstruct list.  If it is a section, renumber
  ;; the following sections.
  ;; FIXME:  Put in a WHAT parameter and search backward until one is found.
  ;; When N is given, go back that many matches of reftex-everything-regexp
  ;; When FORCE is non-nil, also insert if `reftex-where-am-I' was uncertain.
  (condition-case nil
      (catch 'exit
        (unless reftex-mode (throw 'exit nil))
        (reftex-access-scan-info)
        (let* ((docstruct (symbol-value reftex-docstruct-symbol))
               here-I-am appendix tail entry star level
               section-number context)

     (save-excursion
       (when (re-search-backward (reftex-everything-regexp) nil t (or n 1))

         ;; Find where we are
         (setq here-I-am (reftex-where-am-I))
         (or here-I-am (throw 'exit nil))
         (unless (or force (cdr here-I-am)) (throw 'exit nil))
         (setq tail (memq (car here-I-am) docstruct))
         (or tail (throw 'exit nil))
         (setq reftex-active-toc (reftex-last-assoc-before-elt
                                  'toc (car here-I-am) docstruct)
               appendix (reftex-last-assoc-before-elt
                         'appendix (car here-I-am) docstruct))

         ;; Initialize section numbers
         (if (eq (car (car here-I-am)) 'appendix)
             (reftex-init-section-numbers nil t)
           (reftex-init-section-numbers reftex-active-toc appendix))

         ;; Match the section command
         (when (re-search-forward (reftex-everything-regexp) nil t)
           (cond
            ((match-end 1)
             (push (reftex-label-info (reftex-match-string 1) buffer-file-name)
                   (cdr tail)))

            ((match-end 3)
             (setq star (= ?* (char-after (match-end 3)))
                   entry (reftex-section-info (buffer-file-name))
                   level (nth 5 entry))
             ;; Insert the section info
             (push entry (cdr tail))

             ;; We are done unless we use section numbers
             (unless (nth 1 reftex-label-menu-flags) (throw 'exit nil))

             ;; Update the remaining toc items
             (setq tail (cdr tail))
             (while (and (setq tail (memq (assq 'toc (cdr tail)) tail))
                         (setq entry (car tail))
                         (>= (nth 5 entry) level))
               (setq star (string-match "\\*" (nth 6 entry))
                     context (nth 2 entry)
                     section-number
                     (reftex-section-number (nth 5 entry) star))
               (when (string-match "\\`\\([ \t]*\\)\\([.0-9A-Z]+\\)\\(.*\\)"
                                   context)
                 (when (and (not appendix)
                            (>= (string-to-char (match-string 2)) ?A))
                   ;; Just entered the appendix.  Get out.
                   (throw 'exit nil))

                 ;; Change the section number.
                 (setf (nth 2 entry)
                       (concat (match-string 1 context)
                               section-number
                               (match-string 3 context))))))
            ((match-end 10)
             ;; Index entry
             (and reftex-support-index
                  (setq entry (reftex-index-info-safe buffer-file-name))
                  ;; FIXME: (add-to-list 'index-tags (nth 1 index-entry))
                  (push entry (cdr tail))))))))))

    (error nil))
  )

(defsubst reftex-move-to-previous-arg (&optional bound)
  ;; Assuming that we are in front of a macro argument,
  ;; move backward to the closing parenthesis of the previous argument.
  ;; This function understands the splitting of macros over several lines
  ;; in TeX.
  (cond
   ;; Just to be quick:
   ((memq (preceding-char) '(?\] ?\})))
   ;; Do a search
   ((and reftex-allow-detached-macro-args
         (re-search-backward
          "[]}][ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*\\=" bound t))
    (goto-char (1+ (match-beginning 0)))
    t)
   (t nil)))

(defun reftex-what-macro-safe (which &optional bound)
  ;; reftex-what-macro with special syntax table.
  (reftex-with-special-syntax
   (reftex-what-macro which bound)))

(defun reftex-what-macro (which &optional bound)
  ;; Find out if point is within the arguments of any TeX-macro.
  ;; The return value is either ("\\macro" . (point)) or a list of them.

  ;; If WHICH is nil, immediately return nil.
  ;; If WHICH is 1, return innermost enclosing macro.
  ;; If WHICH is t, return list of all macros enclosing point.
  ;; If WHICH is a list of macros, look only for those macros and return the
  ;;    name of the first macro in this list found to enclose point.
  ;; If the optional BOUND is an integer, bound backwards directed
  ;;    searches to this point.  If it is nil, limit to nearest \section -
  ;;    like statement.

  ;; This function is pretty stable, but can be fooled if the text contains
  ;; things like \macro{aa}{bb} where \macro is defined to take only one
  ;; argument.  As RefTeX cannot know this, the string "bb" would still be
  ;; considered an argument of macro \macro.

  (unless reftex-section-regexp (reftex-compile-variables))
  (catch 'exit
    (if (null which) (throw 'exit nil))
    (let ((bound (or bound (save-excursion (re-search-backward
                                            reftex-section-regexp nil 1)
                                           (point))))
          pos cmd-list cmd cnt cnt-opt entry)
      (save-restriction
        (save-excursion
          (narrow-to-region (max (point-min) bound) (point-max))
          ;; move back out of the current parenthesis
          (while (condition-case nil
                     (let ((forward-sexp-function nil))
                       (up-list -1) t)
                   (error nil))
            (setq cnt 1 cnt-opt 0)
            ;; move back over any touching sexps
            (while (and (reftex-move-to-previous-arg bound)
                        (condition-case nil
                            (let ((forward-sexp-function nil))
                              (backward-sexp) t)
                          (error nil)))
              (if (eq (following-char) ?\[) (incf cnt-opt))
              (incf cnt))
            (setq pos (point))
            (when (and (or (= (following-char) ?\[)
                           (= (following-char) ?\{))
                       (re-search-backward "\\\\[*a-zA-Z]+\\=" nil t))
              (setq cmd (reftex-match-string 0))
              (when (looking-at "\\\\begin{[^}]*}")
                (setq cmd (reftex-match-string 0)
                      cnt (1- cnt)))
              ;; This does ignore optional arguments.  Very hard to fix.
              (when (setq entry (assoc cmd reftex-env-or-mac-alist))
                (if (> cnt (or (nth 4 entry) 100))
                    (setq cmd nil)))
              (cond
               ((null cmd))
               ((eq t which)
                (push (cons cmd (point)) cmd-list))
               ((or (eq 1 which) (member cmd which))
                (throw 'exit (cons cmd (point))))))
            (goto-char pos)))
        (nreverse cmd-list)))))

(defun reftex-what-environment (which &optional bound)
  ;; Find out if point is inside a LaTeX environment.
  ;; The return value is (e.g.) either ("equation" . (point)) or a list of
  ;; them.

  ;; If WHICH is nil, immediately return nil.
  ;; If WHICH is 1, return innermost enclosing environment.
  ;; If WHICH is t, return list of all environments enclosing point.
  ;; If WHICH is a list of environments, look only for those environments and
  ;;   return the name of the first environment in this list found to enclose
  ;;   point.

  ;; If the optional BOUND is an integer, bound backwards directed searches to
  ;; this point.  If it is nil, limit to nearest \section - like statement.

  (unless reftex-section-regexp (reftex-compile-variables))
  (catch 'exit
    (save-excursion
      (if (null which) (throw 'exit nil))
      (let ((bound (or bound (save-excursion (re-search-backward
                                              reftex-section-regexp nil 1)
                                             (point))))
            env-list end-list env)
        (while (re-search-backward "\\\\\\(begin\\|end\\){\\([^}]+\\)}"
                                   bound t)
          (setq env (buffer-substring-no-properties
                     (match-beginning 2) (match-end 2)))
          (cond
           ((string= (match-string 1) "end")
            (push env end-list))
           ((equal env (car end-list))
            (setq end-list (cdr end-list)))
           ((eq t which)
            (push (cons env (point)) env-list))
           ((or (eq 1 which) (member env which))
            (throw 'exit (cons env (point))))))
        (nreverse env-list)))))

(defun reftex-what-special-env (which &optional bound)
  ;; Run the special environment parsers and return the matches.
  ;;
  ;; The return value is (e.g.) either ("my-parser-function" . (point))
  ;; or a list of them.

  ;; If WHICH is nil, immediately return nil.
  ;; If WHICH is 1, return innermost enclosing environment.
  ;; If WHICH is t, return list of all environments enclosing point.
  ;; If WHICH is a list of environments, look only for those environments and
  ;;   return the name of the first environment in this list found to enclose
  ;;   point.

  (unless reftex-section-regexp (reftex-compile-variables))
  (catch 'exit
    (save-excursion
      (if (null reftex-special-env-parsers) (throw 'exit nil))
      (if (null which) (throw 'exit nil))
      (let ((bound (or bound (save-excursion (re-search-backward
                                              reftex-section-regexp nil 1)
                                             (point))))
            (fun-list (if (listp which)
                          (mapcar (lambda (x) (if (memq x which) x nil))
                                  reftex-special-env-parsers)
                        reftex-special-env-parsers))
            specials rtn)
        ;; Call all functions
        (setq specials (mapcar
                        (lambda (fun)
                          (save-excursion
                            (setq rtn (and fun (funcall fun bound)))
                            (if rtn (cons (symbol-name fun) rtn) nil)))
                        fun-list))
        ;; Delete the non-matches
        (setq specials (delq nil specials))
        ;; Sort
        (setq specials (sort specials (lambda (a b) (> (cdr a) (cdr b)))))
        (if (eq which t)
            specials
          (car specials))))))

(defsubst reftex-move-to-next-arg (&optional ignore)
  ;; Assuming that we are at the end of a macro name or a macro argument,
  ;; move forward to the opening parenthesis of the next argument.
  ;; This function understands the splitting of macros over several lines
  ;; in TeX.
  (cond
   ;; Just to be quick:
   ((memq (following-char) '(?\[ ?\{)))
   ;; Do a search
   ((and reftex-allow-detached-macro-args
         (looking-at "[ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*[[{]"))
    (goto-char (1- (match-end 0)))
    t)
   (t nil)))

(defun reftex-nth-arg-wrapper (key)
  (let ((entry (assoc key reftex-env-or-mac-alist)))
    (reftex-nth-arg (nth 5 entry) (nth 6 entry))))

(defun reftex-nth-arg (n &optional opt-args)
  ;; Return the nth following {} or [] parentheses content.
  ;; OPT-ARGS is a list of argument numbers which are optional.

  ;; If we are sitting at a macro start, skip to end of macro name.
  (and (eq (following-char) ?\\) (skip-chars-forward "a-zA-Z*\\\\"))

  (if (= n 1000)
      ;; Special case:  Skip all touching arguments
      (progn
        (reftex-move-over-touching-args)
        (reftex-context-substring))

    ;; Do the real thing.
    (let ((cnt 1))

      (when (reftex-move-to-next-arg)

        (while (< cnt n)
          (while (and (member cnt opt-args)
                      (eq (following-char) ?\{))
            (incf cnt))
          (when (< cnt n)
            (unless (and (condition-case nil
                             (or (forward-list 1) t)
                           (error nil))
                         (reftex-move-to-next-arg)
                         (incf cnt))
              (setq cnt 1000))))

        (while (and (memq cnt opt-args)
                    (eq (following-char) ?\{))
          (incf cnt)))
      (if (and (= n cnt)
               (> (skip-chars-forward "{\\[") 0))
          (reftex-context-substring)
        nil))))

(defun reftex-move-over-touching-args ()
  (condition-case nil
      (while (memq (following-char) '(?\[ ?\{))
        (forward-list 1))
    (error nil)))

(defun reftex-context-substring (&optional to-end)
  ;; Return up to 150 chars from point
  ;; When point is just after a { or [, limit string to matching parenthesis
  (cond
   (to-end
    ;; Environment - find next \end
    (buffer-substring-no-properties
     (point)
     (min (+ (point) 150)
          (save-match-data
            ;; FIXME: This is not perfect
            (if (re-search-forward "\\\\end{" nil t)
                (match-beginning 0)
              (point-max))))))
   ((memq (preceding-char) '(?\{ ?\[))
    ;; Inside a list - get only the list.
    (buffer-substring-no-properties
     (point)
     (min (+ (point) 150)
          (point-max)
          (condition-case nil
              (let ((forward-sexp-function nil)) ;Unneeded fanciness.
                (up-list 1)
                (1- (point)))
            (error (point-max))))))
   (t
    ;; no list - just grab 150 characters
    (buffer-substring-no-properties (point)
                                    (min (+ (point) 150) (point-max))))))

;; Variable holding the vector with section numbers
(defvar reftex-section-numbers (make-vector reftex-max-section-depth 0))

(defun reftex-init-section-numbers (&optional toc-entry appendix)
  ;; Initialize the section numbers with zeros or with what is found
  ;; in the toc entry.
  (let* ((level  (or (nth 5 toc-entry) -1))
         (numbers (nreverse (split-string (or (nth 6 toc-entry) "") "\\.")))
         (depth (1- (length reftex-section-numbers)))
         (i depth) number-string)
    (while (>= i 0)
      (if (> i level)
          (aset reftex-section-numbers i 0)
        (setq number-string (or (car numbers) "0"))
        (if (string-match "\\`[A-Z]\\'" number-string)
            (aset reftex-section-numbers i
                  (- (string-to-char number-string) ?A -1))
            (aset reftex-section-numbers i (string-to-number number-string)))
        (pop numbers))
      (decf i)))
  (put 'reftex-section-numbers 'appendix appendix))

(defun reftex-section-number (&optional level star)
  ;; Return a string with the current section number.
  ;; When LEVEL is non-nil, increase section numbers on that level.
  (let* ((depth (1- (length reftex-section-numbers))) idx n (string "")
         (appendix (get 'reftex-section-numbers 'appendix))
         (partspecial (and (not reftex-part-resets-chapter)
                           (equal level 0))))
    ;; partspecial means, this is a part statement.
    ;; Parts do not reset the chapter counter, and the part number is
    ;; not included in the numbering of other sectioning levels.
    (when level
      (when (and (> level -1) (not star))
        (aset reftex-section-numbers
              level (1+ (aref reftex-section-numbers level))))
      (setq idx (1+ level))
      (when (not star)
        (while (<= idx depth)
          (if (or (not partspecial)
                  (not (= idx 1)))
              (aset reftex-section-numbers idx 0))
          (incf idx))))
    (if partspecial
        (setq string (concat "Part " (reftex-roman-number
                                      (aref reftex-section-numbers 0))))
      (setq idx (if reftex-part-resets-chapter 0 1))
      (while (<= idx depth)
        (setq n (aref reftex-section-numbers idx))
        (if (not (and partspecial (not (equal string ""))))
            (setq string (concat string (if (not (string= string "")) "." "")
                                 (int-to-string n))))
        (incf idx))
      (save-match-data
        (if (string-match "\\`\\([@0]\\.\\)+" string)
            (setq string (replace-match "" nil nil string)))
        (if (string-match "\\(\\.0\\)+\\'" string)
            (setq string (replace-match "" nil nil string)))
        (if (and appendix
                 (string-match "\\`[0-9]+" string))
            (setq string
                  (concat
                   (char-to-string
                    (1- (+ ?A (string-to-number (match-string 0 string)))))
                   (substring string (match-end 0))))))
      (if star
          (concat (make-string (1- (length string)) ?\ ) "*")
        string))))

(defun reftex-roman-number (n)
  ;; Return as a string the roman number equal to N.
  (let ((nrest n)
        (string "")
        (list '((1000 . "M") ( 900 . "CM") ( 500 . "D") ( 400 . "CD")
                ( 100 . "C") (  90 . "XC") (  50 . "L") (  40 . "XL")
                (  10 . "X") (   9 . "IX") (   5 . "V") (   4 . "IV")
                (   1 . "I")))
        listel i s)
    (while (>= nrest 1)
      (setq listel (pop list)
            i (car listel)
            s (cdr listel))
      (while (>= nrest i)
        (setq string (concat string s)
              nrest (- nrest i))))
    string))

;;; reftex-parse.el ends here
