;;; reftex-ref.el --- code to create labels and references with RefTeX

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
(provide 'reftex-ref)
(require 'reftex)
(require 'reftex-parse)
;;;

(defun reftex-label-location (&optional bound)
  "Return the environment or macro which determines the label type at point.
If optional BOUND is an integer, limit backward searches to that point."

  (let* ((loc1 (reftex-what-macro reftex-label-mac-list bound))
         (loc2 (reftex-what-environment reftex-label-env-list bound))
         (loc3 (reftex-what-special-env 1 bound))
         (p1 (or (cdr loc1) 0))
         (p2 (or (cdr loc2) 0))
         (p3 (or (cdr loc3) 0))
         (pmax (max p1 p2 p3)))

    (setq reftex-location-start pmax)
    (cond
     ((= p1 pmax)
      ;; A macro.  Default context after macro name.
      (setq reftex-default-context-position (+ p1 (length (car loc1))))
      (or (car loc1) "section"))
     ((= p2 pmax)
      ;; An environment.  Default context after \begin{name}.
      (setq reftex-default-context-position (+ p2 8 (length (car loc2))))
      (or (car loc2) "section"))
     ((= p3 pmax)
      ;; A special.  Default context right there.
      (setq reftex-default-context-position p3)
      (setq loc3 (car loc3))
      (cond ((null loc3) "section")
            ((symbolp loc3) (symbol-name loc3))
            ((stringp loc3) loc3)
            (t "section")))
     (t ;; This should not happen, I think?
      "section"))))

(defun reftex-label-info-update (cell)
  ;; Update information about just one label in a different file.
  ;; CELL contains the old info list
  (let* ((label   (nth 0 cell))
         (typekey (nth 1 cell))
         ;; (text    (nth 2 cell))
         (file    (nth 3 cell))
         (comment (nth 4 cell))
         (note    (nth 5 cell))
         (buf (reftex-get-file-buffer-force
               file (not (eq t reftex-keep-temporary-buffers)))))
    (if (not buf)
        (list label typekey "" file comment "LOST LABEL.  RESCAN TO FIX.")
      (with-current-buffer buf
        (save-restriction
          (widen)
          (goto-char 1)

          (if (or (re-search-forward
                   (format reftex-find-label-regexp-format
                           (regexp-quote label)) nil t)
                  (re-search-forward
                   (format reftex-find-label-regexp-format2
                           (regexp-quote label)) nil t))

              (progn
                (backward-char 1)
                (append (reftex-label-info label file) (list note)))
            (list label typekey "" file "LOST LABEL.  RESCAN TO FIX.")))))))

(defun reftex-label-info (label &optional file bound derive env-or-mac)
  ;; Return info list on LABEL at point.
  (let* ((prefix (if (string-match "^[a-zA-Z0-9]+:" label)
                     (match-string 0 label)))
         (typekey (cdr (assoc prefix reftex-prefix-to-typekey-alist)))
         (file (or file (buffer-file-name)))
         (trust reftex-trust-label-prefix)
         (in-comment (reftex-in-comment)))
    (if (and typekey
             (cond ((eq trust t) t)
                   ((null trust) nil)
                   ((stringp trust) (string-match trust typekey))
                   ((listp trust) (member typekey trust))
                   (t nil)))
        (list label typekey
              (reftex-nicify-text (reftex-context-substring))
              file in-comment)
      (let* ((env-or-mac (or env-or-mac (reftex-label-location bound)))
             (typekey (nth 1 (assoc env-or-mac reftex-env-or-mac-alist)))
             (parse (nth 2 (assoc env-or-mac reftex-env-or-mac-alist)))
             (text (reftex-short-context env-or-mac parse reftex-location-start
                                         derive)))
        (list label typekey text file in-comment)))))

;;; Creating labels ---------------------------------------------------------

(defun reftex-label (&optional environment no-insert)
  "Insert a unique label.  Return the label.
If ENVIRONMENT is given, don't bother to find out yourself.
If NO-INSERT is non-nil, do not insert label into buffer.
With prefix arg, force to rescan document first.
When you are prompted to enter or confirm a label, and you reply with
just the prefix or an empty string, no label at all will be inserted.
A new label is also recorded into the label list.
This function is controlled by the settings of reftex-insert-label-flags."

  (interactive)

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4).
  (reftex-access-scan-info current-prefix-arg)

  ;; Find out what kind of environment this is and abort if necessary.
  (if (or (not environment)
          (not (assoc environment reftex-env-or-mac-alist)))
      (setq environment (reftex-label-location)))
  (unless environment
    (error "Can't figure out what kind of label should be inserted"))

  ;; Ok, go ahead.
  (catch 'exit
    (let* ((entry (assoc environment reftex-env-or-mac-alist))
           (typekey (nth 1 entry))
           (format (nth 3 entry))
           (macro-cell (reftex-what-macro 1))
           (entry1 (assoc (car macro-cell) reftex-env-or-mac-alist))
           label naked prefix valid default force-prompt rescan-is-useful)
      (when (and (or (nth 5 entry) (nth 5 entry1))
                 (memq (preceding-char) '(?\[ ?\{)))
        ;; This is an argument of a label macro.  Insert naked label.
        (setq naked t format "%s"))

      (setq prefix (or (cdr (assoc typekey reftex-typekey-to-prefix-alist))
                       (concat typekey "-")))
      ;; Replace any escapes in the prefix
      (setq prefix (reftex-replace-prefix-escapes prefix))

      ;; Make a default label.
      (cond

       ((reftex-typekey-check typekey (nth 0 reftex-insert-label-flags))
        ;; Derive a label from context.
        (setq reftex-active-toc (reftex-last-assoc-before-elt
                                 'toc (car (reftex-where-am-I))
                                 (symbol-value reftex-docstruct-symbol)))
        (setq default (reftex-no-props
                       (nth 2 (reftex-label-info " " nil nil t))))
        ;; Catch the cases where the is actually no context available.
        (if (or (string-match "NO MATCH FOR CONTEXT REGEXP" default)
                (string-match "INVALID VALUE OF PARSE" default)
                (string-match "SECTION HEADING NOT FOUND" default)
                (string-match "HOOK ERROR" default)
                (string-match "^[ \t]*$" default))
            (setq default prefix
                  force-prompt t)       ; need to prompt
          (setq default
                (concat prefix
                        (funcall reftex-string-to-label-function default)))

          ;; Make it unique.
          (setq default (reftex-uniquify-label default nil "-"))))

       ((reftex-typekey-check typekey (nth 1 reftex-insert-label-flags))
        ;; Minimal default: the user will be prompted.
        (setq default prefix))

       (t
        ;; Make an automatic label.
        (setq default (reftex-uniquify-label prefix t))))

      ;; Should we ask the user?
      (if (or (reftex-typekey-check typekey
                                    (nth 1 reftex-insert-label-flags)) ; prompt
              force-prompt)

          (while (not valid)
            ;; iterate until we get a valid label

            (setq label (read-string
                         (if naked "Naked Label: " "Label: ")
                         default))

            ;; Let's make sure that this is a valid label
            (cond

             ((string-match (concat "\\`\\(" (regexp-quote prefix)
                                    "\\)?[ \t]*\\'")
                            label)
              ;; No label at all, please
              (message "No label inserted.")
              (throw 'exit nil))

             ;; Test if label contains strange characters
             ((string-match reftex-label-illegal-re label)
              (message "Label \"%s\" contains invalid characters" label)
              (ding)
              (sit-for 2))

             ;; Look it up in the label list
             ((setq entry (assoc label
                                 (symbol-value reftex-docstruct-symbol)))
              (ding)
              (if (y-or-n-p
                   (format "Label '%s' exists. Use anyway? " label))
                  (setq valid t)))

             ;; Label is ok
             (t
              (setq valid t))))
        (setq label default))

      ;; Insert the label into the label list
      (let* ((here-I-am-info
              (save-excursion
                (if (and (or naked no-insert)
                         (integerp (cdr macro-cell)))
                    (goto-char (cdr macro-cell)))
                (reftex-where-am-I)))
             (here-I-am (car here-I-am-info))
             (note (if (cdr here-I-am-info)
                       ""
                     "POSITION UNCERTAIN.  RESCAN TO FIX."))
             (file (buffer-file-name))
             (text nil)
             (tail (memq here-I-am (symbol-value reftex-docstruct-symbol))))

        (or (cdr here-I-am-info) (setq rescan-is-useful t))

        (when tail
          (push (list label typekey text file nil note) (cdr tail))
          (put reftex-docstruct-symbol 'modified t)))

      ;; Insert the label into the buffer
      (unless no-insert
        (insert
         (if reftex-format-label-function
             (funcall reftex-format-label-function label format)
           (format format label)))
        (if (and reftex-plug-into-AUCTeX
                 (fboundp 'LaTeX-add-labels))
            ;; Tell AUCTeX about this
            (LaTeX-add-labels label)))

      ;; Delete the corresponding selection buffers to force update on next use.
      (when reftex-auto-update-selection-buffers
        (reftex-erase-buffer (reftex-make-selection-buffer-name typekey))
        (reftex-erase-buffer (reftex-make-selection-buffer-name " ")))

      (when (and rescan-is-useful reftex-allow-automatic-rescan)
        (reftex-parse-one))

      ;; return value of the function is the label
      label)))

(defun reftex-string-to-label (string)
  "Convert a string (a sentence) to a label.
Uses `reftex-derive-label-parameters' and `reftex-label-illegal-re'.  It
also applies `reftex-translate-to-ascii-function' to the string."
  (when (and reftex-translate-to-ascii-function
             (fboundp reftex-translate-to-ascii-function))
    (setq string (funcall reftex-translate-to-ascii-function string)))
  (apply 'reftex-convert-string string
         "[-~ \t\n\r,;]+" reftex-label-illegal-re nil nil
         reftex-derive-label-parameters))

(defun reftex-latin1-to-ascii (string)
  ;; Translate the upper 128 chars in the Latin-1 charset to ASCII equivalents
  (let ((tab "@@@@@@@@@@@@@@@@@@'@@@@@@@@@@@@@ icLxY|S\"ca<--R-o|23'uq..1o>423?AAAAAAACEEEEIIIIDNOOOOOXOUUUUYP3aaaaaaaceeeeiiiidnooooo:ouuuuypy")
        (emacsp (not (featurep 'xemacs))))
    (mapconcat
     (lambda (c)
       (cond ((and (> c 127) (< c 256))                 ; 8 bit Latin-1
              (char-to-string (aref tab (- c 128))))
             ((and emacsp                               ; Not for XEmacs
                   (> c 2175) (< c 2304))               ; Mule Latin-1
              (char-to-string (aref tab (- c 2176))))
             (t (char-to-string c))))
     string "")))

(defun reftex-replace-prefix-escapes (prefix)
  ;; Replace %escapes in a label prefix
  (save-match-data
    (let (letter (num 0) replace)
      (while (string-match "\\%\\([a-zA-Z]\\)" prefix num)
        (setq letter (match-string 1 prefix))
        (setq replace
              (save-match-data
                (cond
                 ((equal letter "f")
                  (file-name-sans-extension
                   (file-name-nondirectory (buffer-file-name))))
                 ((equal letter "F")
                  (let ((masterdir (file-name-directory (reftex-TeX-master-file)))
                        (file (file-name-sans-extension (buffer-file-name))))
                    (if (string-match (concat "\\`" (regexp-quote masterdir))
                                      file)
                        (substring file (length masterdir))
                      file)))
                 ((equal letter "m")
                  (file-name-sans-extension
                   (file-name-nondirectory (reftex-TeX-master-file))))
                 ((equal letter "M")
                  (file-name-nondirectory
                   (substring (file-name-directory (reftex-TeX-master-file))
                              0 -1)))
                 ((equal letter "u")
                  (or (user-login-name) ""))
                 ((equal letter "S")
                  (let* (macro level-exp level)
                    (save-excursion
                      (save-match-data
                        (when (re-search-backward reftex-section-regexp nil t)
                          (setq macro (reftex-match-string 2)
                                level-exp (cdr (assoc macro reftex-section-levels-all))
                                level (if (symbolp level-exp)
                                          (abs (save-match-data
                                                 (funcall level-exp)))
                                        (abs level-exp))))
                        (cdr (or (assoc macro reftex-section-prefixes)
                                 (assoc level reftex-section-prefixes)
                                 (assq t reftex-section-prefixes)
                                 (list t "sec:")))))))
                 (t ""))))
        (setq num (1- (+ (match-beginning 1) (length replace)))
              prefix (replace-match replace nil nil prefix)))
      prefix)))

(defun reftex-uniquify-label (label &optional force separator)
  ;; Make label unique by appending a number.
  ;; Optional FORCE means, force appending a number, even if label is unique.
  ;; Optional SEPARATOR is a string to stick between label and number.

  ;; Ensure access to scanning info
  (reftex-access-scan-info)

  (cond
   ((and (not force)
         (not (assoc label (symbol-value reftex-docstruct-symbol))))
    label)
   (t
    (let* ((label-numbers (assq 'label-numbers
                                (symbol-value reftex-docstruct-symbol)))
           (label-numbers-alist (cdr label-numbers))
           (cell (or (assoc label label-numbers-alist)
                     (car (setcdr label-numbers
                                  (cons (cons label 0)
                                        label-numbers-alist)))))
           (num (1+ (cdr cell)))
           (sep (or separator "")))
      (while (assoc (concat label sep (int-to-string num))
                    (symbol-value reftex-docstruct-symbol))
        (incf num))
      (setcdr cell num)
      (concat label sep (int-to-string num))))))

;;; Referencing labels ------------------------------------------------------

;; Help string for the reference label menu
(defconst reftex-select-label-prompt
  "Select: [n]ext [p]revious [r]escan [ ]context e[x]tern [q]uit RET [?]HELP+more")

(defconst reftex-select-label-help
  " n / p      Go to next/previous label (Cursor motion works as well)
 C-c C-n/p  Go to next/previous section heading.
 b / l      Jump back to previous selection / Reuse last referenced label.
 z          Jump to a specific section, e.g. '3 z' jumps to section 3.
 g / s      Update menu      / Switch label type.
 r / C-u r  Reparse document / Reparse entire document.
 x          Switch to label menu of external document (with LaTeX package `xr').
 F t c      Toggle:  [F]ile borders, [t]able of contents,  [c]ontext
 # %        Toggle:  [#] label counters,   [%] labels in comments
 SPC / f    Show full context in other window / Toggle follow mode.
 .          Show insertion point in other window.
 v   / V    Toggle \\ref <-> \\vref / Rotate \\ref <=> \\fref <=> \\Fref
 TAB        Enter a label with completion.
 m , - +    Mark entry. `,-+' also assign a separator.
 a / A      Put all marked entries into one/many \\ref commands.
 q / RET    Quit without referencing / Accept current label (also on mouse-2).")

(defun reftex-reference (&optional type no-insert cut)
  "Make a LaTeX reference.  Look only for labels of a certain TYPE.
With prefix arg, force to rescan buffer for labels.  This should only be
necessary if you have recently entered labels yourself without using
reftex-label.  Rescanning of the buffer can also be requested from the
label selection menu.
The function returns the selected label or nil.
If NO-INSERT is non-nil, do not insert \\ref command, just return label.
When called with 2 C-u prefix args, disable magic word recognition."

  (interactive)

  ;; check for active recursive edits
  (reftex-check-recursive-edit)

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (unless type
    ;; guess type from context
    (if (and reftex-guess-label-type
             (setq type (reftex-guess-label-type)))
        (setq cut (cdr type)
              type (car type))
      (setq type (reftex-query-label-type))))

  (let* ((reftex-refstyle
          (cond ((reftex-typekey-check type reftex-vref-is-default) "\\vref")
                ((reftex-typekey-check type reftex-fref-is-default) "\\fref")
                (t "\\ref")))
         (reftex-format-ref-function reftex-format-ref-function)
         (form "\\ref{%s}")
         label labels sep sep1)

    ;; Have the user select a label
    (set-marker reftex-select-return-marker (point))
    (setq labels (save-excursion
                   (reftex-offer-label-menu type)))
    (reftex-ensure-compiled-variables)
    (set-marker reftex-select-return-marker nil)
    ;; If the first entry is the symbol 'concat, concat all labels.
    ;; We keep the cdr of the first label for typekey etc information.
    (if (eq (car labels) 'concat)
        (setq labels (list (list (mapconcat 'car (cdr labels) ",")
                                 (cdr (nth 1 labels))))))
    (setq type (nth 1 (car labels))
          form (or (cdr (assoc type reftex-typekey-to-format-alist))
                   form))

    (cond
     (no-insert
      ;; Just return the first label
      (car (car labels)))
     ((null labels)
      (message "Quit")
      nil)
     (t
      (while labels
        (setq label (car (car labels))
              sep (nth 2 (car labels))
              sep1 (cdr (assoc sep reftex-multiref-punctuation))
              labels (cdr labels))
        (when cut
          (backward-delete-char cut)
          (setq cut nil))

        ;; remove ~ if we do already have a space
        (when (and (= ?~ (string-to-char form))
                   (member (preceding-char) '(?\ ?\t ?\n ?~)))
          (setq form (substring form 1)))
        ;; do we have a special format?
        (setq reftex-format-ref-function
              (cond
               ((string= reftex-refstyle "\\vref") 'reftex-format-vref)
               ((string= reftex-refstyle "\\fref") 'reftex-format-fref)
               ((string= reftex-refstyle "\\Fref") 'reftex-format-Fref)
               (t reftex-format-ref-function)))
        ;; ok, insert the reference
        (if sep1 (insert sep1))
        (insert
         (if reftex-format-ref-function
             (funcall reftex-format-ref-function label form)
           (format form label label)))
        ;; take out the initial ~ for good
        (and (= ?~ (string-to-char form))
             (setq form (substring form 1))))
      (message "")
      label))))

(defun reftex-guess-label-type ()
  ;; Examine context to guess what a \ref might want to reference.
  (let ((words reftex-words-to-typekey-alist)
        (case-fold-search t)
        (bound (max (point-min) (- (point) 35)))
        matched cell)
    (save-excursion
      (while (and (setq cell (pop words))
                  (not (setq matched
                             (re-search-backward (car cell) bound t))))))
    (if matched
        (cons (cdr cell) (- (match-end 0) (match-end 1)))
      nil)))

(defvar reftex-select-label-map)
(defun reftex-offer-label-menu (typekey)
  ;; Offer a menu with the appropriate labels.
  (let* ((buf (current-buffer))
         (xr-data (assq 'xr (symbol-value reftex-docstruct-symbol)))
         (xr-alist (cons (cons "" (buffer-file-name)) (nth 1 xr-data)))
         (xr-index 0)
         (here-I-am (car (reftex-where-am-I)))
         (here-I-am1 here-I-am)
         (toc (reftex-typekey-check typekey reftex-label-menu-flags 0))
         (files (reftex-typekey-check typekey reftex-label-menu-flags 7))
         (context (not (reftex-typekey-check
                        typekey reftex-label-menu-flags 3)))
         (counter (reftex-typekey-check
                   typekey reftex-label-menu-flags 2))
         (follow  (reftex-typekey-check
                   typekey reftex-label-menu-flags 4))
         (commented (nth 5 reftex-label-menu-flags))
         (prefix "")
         selection-buffers
         offset rtn key data last-data entries)

    (unwind-protect
        (catch 'exit
          (while t
            (save-window-excursion
              (delete-other-windows)
              (setq reftex-call-back-to-this-buffer buf
                    reftex-latex-syntax-table (syntax-table))
              (if reftex-use-multiple-selection-buffers
                  (switch-to-buffer-other-window
                   (with-current-buffer buf
                     (reftex-make-selection-buffer-name typekey)))
                (switch-to-buffer-other-window "*RefTeX Select*")
                (reftex-erase-buffer))
              (unless (eq major-mode 'reftex-select-label-mode)
                (reftex-select-label-mode))
              (add-to-list 'selection-buffers (current-buffer))
              (setq truncate-lines t)
              (setq mode-line-format
                    (list "----  " 'mode-line-buffer-identification
                          "  " 'global-mode-string "   (" mode-name ")"
                          "  S<" 'reftex-refstyle ">"
                          " -%-"))
              (cond
               ((= 0 (buffer-size))
                (let ((buffer-read-only nil))
                  (message "Creating Selection Buffer...")
                  (setq offset (reftex-insert-docstruct
                                buf
                                toc
                                typekey
                                nil ; index
                                files
                                context
                                counter
                                commented
                                (or here-I-am offset)
                                prefix
                                nil  ; no a toc buffer
                                ))))
               (here-I-am
                (setq offset (reftex-get-offset buf here-I-am typekey)))
               (t (setq offset t)))
              (setq buffer-read-only t)
              (setq offset (or offset t))

              (setq here-I-am nil) ; turn off determination of offset
              (setq rtn
                    (reftex-select-item
                     reftex-select-label-prompt
                     reftex-select-label-help
                     reftex-select-label-map
                     offset
                     'reftex-show-label-location follow))
              (setq key       (car rtn)
                    data      (nth 1 rtn)
                    last-data (nth 2 rtn)
                    offset    t)
              (unless key (throw 'exit nil))
              (cond
               ((eq key ?g)
                ;; update buffer
                (reftex-erase-buffer))
               ((or (eq key ?r)
                    (eq key ?R))
                ;; rescan buffer
                (and current-prefix-arg (setq key ?R))
                (reftex-erase-buffer)
                (reftex-reparse-document buf last-data key))
               ((eq key ?c)
                ;; toggle context mode
                (reftex-erase-buffer)
                (setq context (not context)))
               ((eq key ?s)
                ;; switch type
                (setq here-I-am here-I-am1)
                (setq typekey (reftex-query-label-type)))
               ((eq key ?t)
                ;; toggle table of contents display, or change depth
                (reftex-erase-buffer)
                (if current-prefix-arg
                    (setq reftex-toc-max-level (prefix-numeric-value
                                                current-prefix-arg))
                  (setq toc (not toc))))
               ((eq key ?F)
                ;; toggle display of included file borders
                (reftex-erase-buffer)
                (setq files (not files)))
               ((eq key ?#)
                ;; toggle counter display
                (reftex-erase-buffer)
                (setq counter (not counter)))
               ((eq key ?%)
                ;; toggle display of commented labels
                (reftex-erase-buffer)
                (setq commented (not commented)))
               ((eq key ?l)
                ;; reuse the last referenced label again
                (setq entries reftex-last-used-reference)
                (throw 'exit t))
               ((eq key ?x)
                ;; select an external document
                (setq xr-index (reftex-select-external-document
                                xr-alist xr-index))
                (setq buf (or (reftex-get-file-buffer-force
                               (cdr (nth xr-index xr-alist)))
                              (error "Cannot switch document"))
                      prefix (or (car (nth xr-index xr-alist)) ""))
                (set-buffer buf)
                (reftex-access-scan-info))
               ((stringp key)
                (setq entries
                      (list
                       (list
                        (or (assoc key (symbol-value reftex-docstruct-symbol))
                            (list key typekey)))))
                (throw 'exit t))
               ((memq key '(?a ?A return))
                (cond
                 (reftex-select-marked
                  (setq entries (nreverse reftex-select-marked)))
                 (data
                  (setq entries (list (list data))))
                 (t (setq entries nil)))
                (when entries
                  (if (equal key ?a) (push 'concat entries))
                  (setq reftex-last-used-reference entries))
                (set-buffer buf)
                (throw 'exit t))
               (t (error "This should not happen (reftex-offer-label-menu)"))))))
      (save-excursion
        (while reftex-buffers-with-changed-invisibility
          (set-buffer (car (car reftex-buffers-with-changed-invisibility)))
          (setq buffer-invisibility-spec
                (cdr (pop reftex-buffers-with-changed-invisibility)))))
      (mapc (lambda (buf) (and (buffer-live-p buf) (bury-buffer buf)))
            selection-buffers)
      (reftex-kill-temporary-buffers))
    ;; Add the prefixes, put together the relevant information in the form
    ;; (LABEL TYPEKEY SEPARATOR) and return a list of those.
    (mapcar (lambda (x)
              (if (listp x)
                  (list (concat prefix (car (car x)))
                        (nth 1 (car x))
                        (nth 2 x))
                x))
            entries)))

(defun reftex-reparse-document (&optional buffer data key)
  ;; Rescan the document.
  (save-window-excursion
    (save-excursion
      (if buffer
          (if (not (bufferp buffer))
              (error "No such buffer %s" (buffer-name buffer))
            (set-buffer buffer)))
      (let ((arg (if (eq key ?R) '(16) '(4)))
            (file (nth 3 data)))
        (reftex-access-scan-info arg file)))))

(defun reftex-query-label-type ()
  ;; Ask for label type
  (let ((key (reftex-select-with-char
              reftex-type-query-prompt reftex-type-query-help 3)))
    (unless (member (char-to-string key) reftex-typekey-list)
      (error "No such label type: %s" (char-to-string key)))
    (char-to-string key)))

(defun reftex-show-label-location (data forward no-revisit
                                        &optional stay error)
  ;; View the definition site of a label in another window.
  ;; DATA is an entry from the docstruct list.
  ;; FORWARD indicates if the label is likely forward from current point.
  ;; NO-REVISIT means do not load a file to show this label.
  ;; STAY means leave the new window selected.
  ;; ERROR means throw an error exception when the label cannot be found.
  ;; If ERROR is nil, the return value of this function indicates success.
  (let* ((this-window (selected-window))
         (errorf (if error 'error 'message))
         label file buffer re found)

    (catch 'exit
      (setq label (nth 0 data)
            file  (nth 3 data))

      (unless file
        (funcall errorf "Unknown label - reparse might help")
        (throw 'exit nil))

      ;; Goto the file in another window
      (setq buffer
            (if no-revisit
                (reftex-get-buffer-visiting file)
              (reftex-get-file-buffer-force
               file (not reftex-keep-temporary-buffers))))
      (if buffer
          ;; good - the file is available
          (switch-to-buffer-other-window buffer)
        ;; we have got a problem here.  The file does not exist.
        ;; Let' get out of here..
        (funcall errorf "Label %s not found" label)
        (throw 'exit nil))

      ;; search for that label
      (setq re (format reftex-find-label-regexp-format (regexp-quote label)))
      (setq found
            (if forward
                (re-search-forward re nil t)
              (re-search-backward re nil t)))
      (unless found
        (goto-char (point-min))
        (unless (setq found (re-search-forward re nil t))
          ;; Ooops.  Must be in a macro with distributed args.
          (setq found
                (re-search-forward
                 (format reftex-find-label-regexp-format2
                         (regexp-quote label)) nil t))))
      (if (match-end 3)
          (progn
            (reftex-highlight 0 (match-beginning 3) (match-end 3))
            (reftex-show-entry (match-beginning 3) (match-end 3))
            (recenter '(4))
            (unless stay (select-window this-window)))
        (select-window this-window)
        (funcall errorf "Label %s not found" label))
      found)))

(defvar font-lock-mode)
(defun reftex-show-entry (beg-hlt end-hlt)
  ;; Show entry if point is hidden
  (let* ((n (/ (reftex-window-height) 2))
         (beg (save-excursion
               (re-search-backward "[\n\r]" nil 1 n) (point)))
         (end (save-excursion
                (re-search-forward  "[\n\r]" nil 1 n) (point))))
    (cond
     ((and (boundp 'buffer-invisibility-spec) buffer-invisibility-spec
           (get-char-property (1+ beg-hlt) 'invisible))
      ;; Invisible with text properties.  That is easy to change.
      (push (cons (current-buffer) buffer-invisibility-spec)
            reftex-buffers-with-changed-invisibility)
      (setq buffer-invisibility-spec nil))
     ((string-match "\r" (buffer-substring beg end))
      ;; Invisible with selective display.  We need to copy it.
      (let ((string (buffer-substring-no-properties beg end)))
        (switch-to-buffer "*RefTeX Context Copy*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert string)
        (subst-char-in-region (point-min) (point-max) ?\r ?\n t)
        (goto-char (- beg-hlt beg))
        (reftex-highlight 0 (1+ (- beg-hlt beg)) (1+ (- end-hlt beg)))
        (if (reftex-refontify)
            (when (or (not (eq major-mode 'latex-mode))
                      (not font-lock-mode))
              (latex-mode)
              (run-hook-with-args
               'reftex-pre-refontification-functions
               reftex-call-back-to-this-buffer 'reftex-hidden)
              (turn-on-font-lock))
          (when (or (not (eq major-mode 'fundamental-mode))
                    font-lock-mode)
            (fundamental-mode)))
        (run-hooks 'reftex-display-copied-context-hook)
        (setq buffer-read-only t))))))

(defun reftex-varioref-vref ()
  "Insert a reference using the `\\vref' macro from the varioref package."
  (interactive)
  (let ((reftex-format-ref-function 'reftex-format-vref))
    (reftex-reference)))
(defun reftex-fancyref-fref ()
  "Insert a reference using the `\\fref' macro from the fancyref package."
  (interactive)
  (let ((reftex-format-ref-function 'reftex-format-fref)
        ;;(reftex-guess-label-type nil) ;FIXME do we want this????
        )
    (reftex-reference)))
(defun reftex-fancyref-Fref ()
  "Insert a reference using the `\\Fref' macro from the fancyref package."
  (interactive)
  (let ((reftex-format-ref-function 'reftex-format-Fref)
        ;;(reftex-guess-label-type nil) ;FIXME do we want this????
        )
    (reftex-reference)))

(defun reftex-format-vref (label fmt)
  (while (string-match "\\\\ref{" fmt)
    (setq fmt (replace-match "\\vref{" t t fmt)))
  (format fmt label label))
(defun reftex-format-Fref (label def-fmt)
  (format "\\Fref{%s}" label))
(defun reftex-format-fref (label def-fmt)
  (format "\\fref{%s}" label))

(defun reftex-goto-label (&optional other-window)
  "Prompt for a label (with completion) and jump to the location of this label.
Optional prefix argument OTHER-WINDOW goes to the label in another window."
  (interactive "P")
  (reftex-access-scan-info)
  (let* ((wcfg (current-window-configuration))
         (docstruct (symbol-value reftex-docstruct-symbol))
	 ;; If point is inside a \ref{} or \pageref{}, use that as
	 ;; default value.
	 (default (when (looking-back "\\\\\\(?:page\\)?ref{[-a-zA-Z0-9_*.:]*")
		    (reftex-this-word "-a-zA-Z0-9_*.:")))
         (label (completing-read (if default
				     (format "Label (default %s): " default)
				   "Label: ")
				 docstruct
                                 (lambda (x) (stringp (car x))) t nil nil
				 default))
         (selection (assoc label docstruct))
         (where (progn
                  (reftex-show-label-location selection t nil 'stay)
                  (point-marker))))
    (unless other-window
      (set-window-configuration wcfg)
      (switch-to-buffer (marker-buffer where))
      (goto-char where))
    (reftex-unhighlight 0)))


;;; reftex-ref.el ends here
