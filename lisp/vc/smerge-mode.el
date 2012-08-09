;;; smerge-mode.el --- Minor mode to resolve diff3 conflicts -*- lexical-binding: t -*-

;; Copyright (C) 1999-2012 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: vc, tools, revision control, merge, diff3, cvs, conflict

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

;; Provides a lightweight alternative to emerge/ediff.
;; To use it, simply add to your .emacs the following lines:
;;
;;   (autoload 'smerge-mode "smerge-mode" nil t)
;;
;; you can even have it turned on automatically with the following
;; piece of code in your .emacs:
;;
;;   (defun sm-try-smerge ()
;;     (save-excursion
;;   	 (goto-char (point-min))
;;   	 (when (re-search-forward "^<<<<<<< " nil t)
;;   	   (smerge-mode 1))))
;;   (add-hook 'find-file-hook 'sm-try-smerge t)

;;; Todo:

;; - if requested, ask the user whether he wants to call ediff right away

;;; Code:

(eval-when-compile (require 'cl))
(require 'diff-mode)                    ;For diff-auto-refine-mode.
(require 'newcomment)

;;; The real definition comes later.
(defvar smerge-mode)

(defgroup smerge ()
  "Minor mode to highlight and resolve diff3 conflicts."
  :group 'tools
  :prefix "smerge-")

(defcustom smerge-diff-buffer-name "*vc-diff*"
  "Buffer name to use for displaying diffs."
  :group 'smerge
  :type '(choice
	  (const "*vc-diff*")
	  (const "*cvs-diff*")
	  (const "*smerge-diff*")
	  string))

(defcustom smerge-diff-switches
  (append '("-d" "-b")
	  (if (listp diff-switches) diff-switches (list diff-switches)))
  "A list of strings specifying switches to be passed to diff.
Used in `smerge-diff-base-mine' and related functions."
  :group 'smerge
  :type '(repeat string))

(defcustom smerge-auto-leave t
  "Non-nil means to leave `smerge-mode' when the last conflict is resolved."
  :group 'smerge
  :type 'boolean)

(defface smerge-mine
  '((((min-colors 88) (background light))
     (:foreground "blue1"))
    (((background light))
     (:foreground "blue"))
    (((min-colors 88) (background dark))
     (:foreground "cyan1"))
    (((background dark))
     (:foreground "cyan")))
  "Face for your code."
  :group 'smerge)
(define-obsolete-face-alias 'smerge-mine-face 'smerge-mine "22.1")
(defvar smerge-mine-face 'smerge-mine)

(defface smerge-other
  '((((background light))
     (:foreground "darkgreen"))
    (((background dark))
     (:foreground "lightgreen")))
  "Face for the other code."
  :group 'smerge)
(define-obsolete-face-alias 'smerge-other-face 'smerge-other "22.1")
(defvar smerge-other-face 'smerge-other)

(defface smerge-base
  '((((min-colors 88) (background light))
     (:foreground "red1"))
    (((background light))
     (:foreground "red"))
    (((background dark))
     (:foreground "orange")))
  "Face for the base code."
  :group 'smerge)
(define-obsolete-face-alias 'smerge-base-face 'smerge-base "22.1")
(defvar smerge-base-face 'smerge-base)

(defface smerge-markers
  '((((background light))
     (:background "grey85"))
    (((background dark))
     (:background "grey30")))
  "Face for the conflict markers."
  :group 'smerge)
(define-obsolete-face-alias 'smerge-markers-face 'smerge-markers "22.1")
(defvar smerge-markers-face 'smerge-markers)

(defface smerge-refined-change
  '((t :background "yellow"))
  "Face used for char-based changes shown by `smerge-refine'."
  :group 'smerge)

(easy-mmode-defmap smerge-basic-map
  `(("n" . smerge-next)
    ("p" . smerge-prev)
    ("r" . smerge-resolve)
    ("a" . smerge-keep-all)
    ("b" . smerge-keep-base)
    ("o" . smerge-keep-other)
    ("m" . smerge-keep-mine)
    ("E" . smerge-ediff)
    ("C" . smerge-combine-with-next)
    ("R" . smerge-refine)
    ("\C-m" . smerge-keep-current)
    ("=" . ,(make-sparse-keymap "Diff"))
    ("=<" "base-mine" . smerge-diff-base-mine)
    ("=>" "base-other" . smerge-diff-base-other)
    ("==" "mine-other" . smerge-diff-mine-other))
  "The base keymap for `smerge-mode'.")

(defcustom smerge-command-prefix "\C-c^"
  "Prefix for `smerge-mode' commands."
  :group 'smerge
  :type '(choice (const :tag "ESC"   "\e")
		 (const :tag "C-c ^" "\C-c^" )
		 (const :tag "none"  "")
		 string))

(easy-mmode-defmap smerge-mode-map
  `((,smerge-command-prefix . ,smerge-basic-map))
  "Keymap for `smerge-mode'.")

(defvar smerge-check-cache nil)
(make-variable-buffer-local 'smerge-check-cache)
(defun smerge-check (n)
  (condition-case nil
      (let ((state (cons (point) (buffer-modified-tick))))
	(unless (equal (cdr smerge-check-cache) state)
	  (smerge-match-conflict)
	  (setq smerge-check-cache (cons (match-data) state)))
	(nth (* 2 n) (car smerge-check-cache)))
    (error nil)))

(easy-menu-define smerge-mode-menu smerge-mode-map
  "Menu for `smerge-mode'."
  '("SMerge"
    ["Next" smerge-next :help "Go to next conflict"]
    ["Previous" smerge-prev :help "Go to previous conflict"]
    "--"
    ["Keep All" smerge-keep-all :help "Keep all three versions"
     :active (smerge-check 1)]
    ["Keep Current" smerge-keep-current :help "Use current (at point) version"
     :active (and (smerge-check 1) (> (smerge-get-current) 0))]
    "--"
    ["Revert to Base" smerge-keep-base :help "Revert to base version"
     :active (smerge-check 2)]
    ["Keep Other" smerge-keep-other :help "Keep `other' version"
     :active (smerge-check 3)]
    ["Keep Yours" smerge-keep-mine :help "Keep your version"
     :active (smerge-check 1)]
    "--"
    ["Diff Base/Mine" smerge-diff-base-mine
     :help "Diff `base' and `mine' for current conflict"
     :active (smerge-check 2)]
    ["Diff Base/Other" smerge-diff-base-other
     :help "Diff `base' and `other' for current conflict"
     :active (smerge-check 2)]
    ["Diff Mine/Other" smerge-diff-mine-other
     :help "Diff `mine' and `other' for current conflict"
     :active (smerge-check 1)]
    "--"
    ["Invoke Ediff" smerge-ediff
     :help "Use Ediff to resolve the conflicts"
     :active (smerge-check 1)]
    ["Auto Resolve" smerge-resolve
     :help "Try auto-resolution heuristics"
     :active (smerge-check 1)]
    ["Combine" smerge-combine-with-next
     :help "Combine current conflict with next"
     :active (smerge-check 1)]
    ))

(easy-menu-define smerge-context-menu nil
  "Context menu for mine area in `smerge-mode'."
  '(nil
    ["Keep Current" smerge-keep-current :help "Use current (at point) version"]
    ["Kill Current" smerge-kill-current :help "Remove current (at point) version"]
    ["Keep All" smerge-keep-all :help "Keep all three versions"]
    "---"
    ["More..." (popup-menu smerge-mode-menu) :help "Show full SMerge mode menu"]
    ))

(defconst smerge-font-lock-keywords
  '((smerge-find-conflict
     (1 smerge-mine-face prepend t)
     (2 smerge-base-face prepend t)
     (3 smerge-other-face prepend t)
     ;; FIXME: `keep' doesn't work right with syntactic fontification.
     (0 smerge-markers-face keep)
     (4 nil t t)
     (5 nil t t)))
  "Font lock patterns for `smerge-mode'.")

(defconst smerge-begin-re "^<<<<<<< \\(.*\\)\n")
(defconst smerge-end-re "^>>>>>>> .*\n")
(defconst smerge-base-re "^||||||| .*\n")
(defconst smerge-other-re "^=======\n")

(defvar smerge-conflict-style nil
  "Keep track of which style of conflict is in use.
Can be nil if the style is undecided, or else:
- `diff3-E'
- `diff3-A'")

;; Compiler pacifiers
(defvar font-lock-mode)
(defvar font-lock-keywords)

;;;;
;;;; Actual code
;;;;

;; Define smerge-next and smerge-prev
(easy-mmode-define-navigation smerge smerge-begin-re "conflict" nil nil
  (if diff-auto-refine-mode
      (condition-case nil (smerge-refine) (error nil))))

(defconst smerge-match-names ["conflict" "mine" "base" "other"])

(defun smerge-ensure-match (n)
  (unless (match-end n)
    (error "No `%s'" (aref smerge-match-names n))))

(defun smerge-auto-leave ()
  (when (and smerge-auto-leave
	     (save-excursion (goto-char (point-min))
			     (not (re-search-forward smerge-begin-re nil t))))
    (when (and (listp buffer-undo-list) smerge-mode)
      (push (list 'apply 'smerge-mode 1) buffer-undo-list))
    (smerge-mode -1)))


(defun smerge-keep-all ()
  "Concatenate all versions."
  (interactive)
  (smerge-match-conflict)
  (let ((mb2 (or (match-beginning 2) (point-max)))
	(me2 (or (match-end 2) (point-min))))
    (delete-region (match-end 3) (match-end 0))
    (delete-region (max me2 (match-end 1)) (match-beginning 3))
    (if (and (match-end 2) (/= (match-end 1) (match-end 3)))
	(delete-region (match-end 1) (match-beginning 2)))
    (delete-region (match-beginning 0) (min (match-beginning 1) mb2))
    (smerge-auto-leave)))

(defun smerge-keep-n (n)
  (smerge-remove-props (match-beginning 0) (match-end 0))
  ;; We used to use replace-match, but that did not preserve markers so well.
  (delete-region (match-end n) (match-end 0))
  (delete-region (match-beginning 0) (match-beginning n)))

(defun smerge-combine-with-next ()
  "Combine the current conflict with the next one."
  ;; `smerge-auto-combine' relies on the finish position (at the beginning
  ;; of the closing marker).
  (interactive)
  (smerge-match-conflict)
  (let ((ends nil))
    (dolist (i '(3 2 1 0))
      (push (if (match-end i) (copy-marker (match-end i) t)) ends))
    (setq ends (apply 'vector ends))
    (goto-char (aref ends 0))
    (if (not (re-search-forward smerge-begin-re nil t))
	(error "No next conflict")
      (smerge-match-conflict)
      (let ((match-data (mapcar (lambda (m) (if m (copy-marker m)))
				(match-data))))
	;; First copy the in-between text in each alternative.
	(dolist (i '(1 2 3))
	  (when (aref ends i)
	    (goto-char (aref ends i))
	    (insert-buffer-substring (current-buffer)
				     (aref ends 0) (car match-data))))
	(delete-region (aref ends 0) (car match-data))
	;; Then move the second conflict's alternatives into the first.
	(dolist (i '(1 2 3))
	  (set-match-data match-data)
	  (when (and (aref ends i) (match-end i))
	    (goto-char (aref ends i))
	    (insert-buffer-substring (current-buffer)
				     (match-beginning i) (match-end i))))
	(delete-region (car match-data) (cadr match-data))
	;; Free the markers.
	(dolist (m match-data) (if m (move-marker m nil)))
	(mapc (lambda (m) (if m (move-marker m nil))) ends)))))

(defvar smerge-auto-combine-max-separation 2
  "Max number of lines between conflicts that should be combined.")

(defun smerge-auto-combine ()
  "Automatically combine conflicts that are near each other."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (smerge-find-conflict)
      ;; 2 is 1 (default) + 1 (the begin markers).
      (while (save-excursion
               (smerge-find-conflict
                (line-beginning-position
                 (+ 2 smerge-auto-combine-max-separation))))
        (forward-line -1)               ;Go back inside the conflict.
        (smerge-combine-with-next)
        (forward-line 1)                ;Move past the end of the conflict.
        ))))

(defvar smerge-resolve-function
  (lambda () (error "Don't know how to resolve"))
  "Mode-specific merge function.
The function is called with zero or one argument (non-nil if the resolution
function should only apply safe heuristics) and with the match data set
according to `smerge-match-conflict'.")
(add-to-list 'debug-ignored-errors "Don't know how to resolve")

(defvar smerge-text-properties
  `(help-echo "merge conflict: mouse-3 shows a menu"
    ;; mouse-face highlight
    keymap (keymap (down-mouse-3 . smerge-popup-context-menu))))

(defun smerge-remove-props (beg end)
  (remove-overlays beg end 'smerge 'refine)
  (remove-overlays beg end 'smerge 'conflict)
  ;; Now that we use overlays rather than text-properties, this function
  ;; does not cause refontification any more.  It can be seen very clearly
  ;; in buffers where jit-lock-contextually is not t, in which case deleting
  ;; the "<<<<<<< foobar" leading line leaves the rest of the conflict
  ;; highlighted as if it were still a valid conflict.  Note that in many
  ;; important cases (such as the previous example) we're actually called
  ;; during font-locking so inhibit-modification-hooks is non-nil, so we
  ;; can't just modify the buffer and expect font-lock to be triggered as in:
  ;; (put-text-property beg end 'smerge-force-highlighting nil)
  (with-silent-modifications
    (remove-text-properties beg end '(fontified nil))))

(defun smerge-popup-context-menu (event)
  "Pop up the Smerge mode context menu under mouse."
  (interactive "e")
  (if (and smerge-mode
	   (save-excursion (posn-set-point (event-end event)) (smerge-check 1)))
      (progn
	(posn-set-point (event-end event))
	(smerge-match-conflict)
	(let ((i (smerge-get-current))
	      o)
	  (if (<= i 0)
	      ;; Out of range
	      (popup-menu smerge-mode-menu)
	    ;; Install overlay.
	    (setq o (make-overlay (match-beginning i) (match-end i)))
	    (unwind-protect
		(progn
		  (overlay-put o 'face 'highlight)
		  (sit-for 0)		;Display the new highlighting.
		  (popup-menu smerge-context-menu))
	      ;; Delete overlay.
	      (delete-overlay o)))))
    ;; There's no conflict at point, the text-props are just obsolete.
    (save-excursion
      (let ((beg (re-search-backward smerge-end-re nil t))
	    (end (re-search-forward smerge-begin-re nil t)))
	(smerge-remove-props (or beg (point-min)) (or end (point-max)))
	(push event unread-command-events)))))

(defun smerge-apply-resolution-patch (buf m0b m0e m3b m3e &optional m2b)
  "Replace the conflict with a bunch of subconflicts.
BUF contains a plain diff between match-1 and match-3."
  (let ((line 1)
        (textbuf (current-buffer))
        (name1 (progn (goto-char m0b)
                      (buffer-substring (+ (point) 8) (line-end-position))))
        (name2 (when m2b (goto-char m2b) (forward-line -1)
                     (buffer-substring (+ (point) 8) (line-end-position))))
        (name3 (progn (goto-char m0e) (forward-line -1)
                      (buffer-substring (+ (point) 8) (line-end-position)))))
    (smerge-remove-props m0b m0e)
    (delete-region m3e m0e)
    (delete-region m0b m3b)
    (setq m3b m0b)
    (setq m3e (- m3e (- m3b m0b)))
    (goto-char m3b)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (if (not (looking-at "\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?\\([acd]\\)\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?$"))
            (error "Unexpected patch hunk header: %s"
                   (buffer-substring (point) (line-end-position)))
          (let* ((op (char-after (match-beginning 3)))
                 (startline (+ (string-to-number (match-string 1))
                               ;; No clue why this is the way it is, but line
                               ;; numbers seem to be off-by-one for `a' ops.
                               (if (eq op ?a) 1 0)))
                 (endline (if (eq op ?a) startline
                            (1+ (if (match-end 2)
                                    (string-to-number (match-string 2))
                                  startline))))
                 (lines (- endline startline))
                 (otherlines (cond
                              ((eq op ?d) nil)
                              ((null (match-end 5)) 1)
                              (t (- (string-to-number (match-string 5))
                                    (string-to-number (match-string 4)) -1))))
                 othertext)
            (forward-line 1)                             ;Skip header.
            (forward-line lines)                         ;Skip deleted text.
            (if (eq op ?c) (forward-line 1))             ;Skip separator.
            (setq othertext
                  (if (null otherlines) ""
                    (let ((pos (point)))
                      (dotimes (_i otherlines) (delete-char 2) (forward-line 1))
                      (buffer-substring pos (point)))))
            (with-current-buffer textbuf
              (forward-line (- startline line))
              (insert "<<<<<<< " name1 "\n" othertext
                      (if name2 (concat "||||||| " name2 "\n") "")
                      "=======\n")
              (forward-line lines)
              (insert ">>>>>>> " name3 "\n")
              (setq line endline))))))))

(defconst smerge-resolve--normalize-re "[\n\t][ \t\n]*\\| [ \t\n]+")

(defun smerge-resolve--extract-comment (beg end)
  "Extract the text within the comments that span BEG..END."
  (save-excursion
    (let ((comments ())
          combeg)
      (goto-char beg)
      (while (and (< (point) end)
                  (setq combeg (comment-search-forward end t)))
        (let ((beg (point)))
          (goto-char combeg)
          (comment-forward 1)
          (save-excursion
            (comment-enter-backward)
            (push " " comments)
            (push (buffer-substring-no-properties beg (point)) comments))))
      (push " " comments)
      (with-temp-buffer
        (apply #'insert (nreverse comments))
        (goto-char (point-min))
        (while (re-search-forward smerge-resolve--normalize-re
                                  nil t)
          (replace-match " "))
        (buffer-string)))))

(defun smerge-resolve--normalize (beg end)
  (replace-regexp-in-string
   smerge-resolve--normalize-re " "
   (concat " " (buffer-substring-no-properties beg end) " ")))

(defun smerge-resolve (&optional safe)
  "Resolve the conflict at point intelligently.
This relies on mode-specific knowledge and thus only works in some
major modes.  Uses `smerge-resolve-function' to do the actual work."
  (interactive)
  (smerge-match-conflict)
  (smerge-remove-props (match-beginning 0) (match-end 0))
  (let ((md (match-data))
	(m0b (match-beginning 0))
	(m1b (match-beginning 1))
	(m2b (match-beginning 2))
	(m3b (match-beginning 3))
	(m0e (match-end 0))
	(m1e (match-end 1))
	(m2e (match-end 2))
	(m3e (match-end 3))
	(buf (generate-new-buffer " *smerge*"))
        m b o
        choice)
    (unwind-protect
	(progn
          (cond
           ;; Trivial diff3 -A non-conflicts.
           ((and (eq (match-end 1) (match-end 3))
                 (eq (match-beginning 1) (match-beginning 3)))
            (smerge-keep-n 3))
           ;; Mode-specific conflict resolution.
           ((condition-case nil
                (atomic-change-group
                  (if safe
                      (funcall smerge-resolve-function safe)
                    (funcall smerge-resolve-function))
                  t)
              (error nil))
            ;; Nothing to do: the resolution function has done it already.
            nil)
           ;; Non-conflict.
	   ((and (eq m1e m3e) (eq m1b m3b))
	    (set-match-data md) (smerge-keep-n 3))
           ;; Refine a 2-way conflict using "diff -b".
           ;; In case of a 3-way conflict with an empty base
           ;; (i.e. 2 conflicting additions), we do the same, presuming
           ;; that the 2 additions should be somehow merged rather
           ;; than concatenated.
	   ((let ((lines (count-lines m3b m3e)))
              (setq m (make-temp-file "smm"))
              (write-region m1b m1e m nil 'silent)
              (setq o (make-temp-file "smo"))
              (write-region m3b m3e o nil 'silent)
              (not (or (eq m1b m1e) (eq m3b m3e)
                       (and (not (zerop (call-process diff-command
                                                      nil buf nil "-b" o m)))
                            ;; TODO: We don't know how to do the refinement
                            ;; if there's a non-empty ancestor and m1 and m3
                            ;; aren't just plain equal.
                            m2b (not (eq m2b m2e)))
                       (with-current-buffer buf
                         (goto-char (point-min))
                         ;; Make sure there's some refinement.
                         (looking-at
                          (concat "1," (number-to-string lines) "c"))))))
            (smerge-apply-resolution-patch buf m0b m0e m3b m3e m2b))
	   ;; "Mere whitespace changes" conflicts.
           ((when m2e
              (setq b (make-temp-file "smb"))
              (write-region m2b m2e b nil 'silent)
              (with-current-buffer buf (erase-buffer))
              ;; Only minor whitespace changes made locally.
              ;; BEWARE: pass "-c" 'cause the output is reused in the next test.
              (zerop (call-process diff-command nil buf nil "-bc" b m)))
            (set-match-data md)
	    (smerge-keep-n 3))
	   ;; Try "diff -b BASE MINE | patch OTHER".
	   ((when (and (not safe) m2e b
                       ;; If the BASE is empty, this would just concatenate
                       ;; the two, which is rarely right.
                       (not (eq m2b m2e)))
              ;; BEWARE: we're using here the patch of the previous test.
	      (with-current-buffer buf
		(zerop (call-process-region
			(point-min) (point-max) "patch" t nil nil
			"-r" null-device "--no-backup-if-mismatch"
			"-fl" o))))
	    (save-restriction
	      (narrow-to-region m0b m0e)
              (smerge-remove-props m0b m0e)
	      (insert-file-contents o nil nil nil t)))
	   ;; Try "diff -b BASE OTHER | patch MINE".
	   ((when (and (not safe) m2e b
                       ;; If the BASE is empty, this would just concatenate
                       ;; the two, which is rarely right.
                       (not (eq m2b m2e)))
	      (write-region m3b m3e o nil 'silent)
	      (call-process diff-command nil buf nil "-bc" b o)
	      (with-current-buffer buf
		(zerop (call-process-region
			(point-min) (point-max) "patch" t nil nil
			"-r" null-device "--no-backup-if-mismatch"
			"-fl" m))))
	    (save-restriction
	      (narrow-to-region m0b m0e)
              (smerge-remove-props m0b m0e)
	      (insert-file-contents m nil nil nil t)))
           ;; If the conflict is only made of comments, and one of the two
           ;; changes is only rearranging spaces (e.g. reflowing text) while
           ;; the other is a real change, drop the space-rearrangement.
           ((and m2e
                 (comment-only-p m1b m1e)
                 (comment-only-p m2b m2e)
                 (comment-only-p m3b m3e)
                 (let ((t1 (smerge-resolve--extract-comment m1b m1e))
                       (t2 (smerge-resolve--extract-comment m2b m2e))
                       (t3 (smerge-resolve--extract-comment m3b m3e)))
                   (cond
                    ((and (equal t1 t2) (not (equal t2 t3)))
                     (setq choice 3))
                    ((and (not (equal t1 t2)) (equal t2 t3))
                     (setq choice 1)))))
            (set-match-data md)
	    (smerge-keep-n choice))
           ;; Idem, when the conflict is contained within a single comment.
           ((save-excursion
              (and m2e
                   (nth 4 (syntax-ppss m0b))
                   ;; If there's a conflict earlier in the file,
                   ;; syntax-ppss is not reliable.
                   (not (re-search-backward smerge-begin-re nil t))
                   (progn (goto-char (nth 8 (syntax-ppss m0b)))
                          (forward-comment 1)
                          (> (point) m0e))
                   (let ((t1 (smerge-resolve--normalize m1b m1e))
                         (t2 (smerge-resolve--normalize m2b m2e))
                         (t3 (smerge-resolve--normalize m3b m3e)))
                     (cond
                    ((and (equal t1 t2) (not (equal t2 t3)))
                     (setq choice 3))
                    ((and (not (equal t1 t2)) (equal t2 t3))
                     (setq choice 1))))))
            (set-match-data md)
	    (smerge-keep-n choice))
           (t
            (error "Don't know how to resolve"))))
      (if (buffer-name buf) (kill-buffer buf))
      (if m (delete-file m))
      (if b (delete-file b))
      (if o (delete-file o))))
  (smerge-auto-leave))

(defun smerge-resolve-all ()
  "Perform automatic resolution on all conflicts."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward smerge-begin-re nil t)
      (condition-case nil
          (progn
            (smerge-match-conflict)
            (smerge-resolve 'safe))
        (error nil)))))

(defun smerge-batch-resolve ()
  ;; command-line-args-left is what is left of the command line.
  (if (not noninteractive)
      (error "`smerge-batch-resolve' is to be used only with -batch"))
  (while command-line-args-left
    (let ((file (pop command-line-args-left)))
      (if (string-match "\\.rej\\'" file)
          ;; .rej files should never contain diff3 markers, on the other hand,
          ;; in Arch, .rej files are sometimes used to indicate that the
          ;; main file has diff3 markers.  So you can pass **/*.rej and
          ;; it will DTRT.
          (setq file (substring file 0 (match-beginning 0))))
      (message "Resolving conflicts in %s..." file)
      (when (file-readable-p file)
        (with-current-buffer (find-file-noselect file)
          (smerge-resolve-all)
          (save-buffer)
          (kill-buffer (current-buffer)))))))

(defun smerge-keep-base ()
  "Revert to the base version."
  (interactive)
  (smerge-match-conflict)
  (smerge-ensure-match 2)
  (smerge-keep-n 2)
  (smerge-auto-leave))

(defun smerge-keep-other ()
  "Use \"other\" version."
  (interactive)
  (smerge-match-conflict)
  ;;(smerge-ensure-match 3)
  (smerge-keep-n 3)
  (smerge-auto-leave))

(defun smerge-keep-mine ()
  "Keep your version."
  (interactive)
  (smerge-match-conflict)
  ;;(smerge-ensure-match 1)
  (smerge-keep-n 1)
  (smerge-auto-leave))

(defun smerge-get-current ()
  (let ((i 3))
    (while (or (not (match-end i))
	       (< (point) (match-beginning i))
	       (>= (point) (match-end i)))
      (decf i))
    i))

(defun smerge-keep-current ()
  "Use the current (under the cursor) version."
  (interactive)
  (smerge-match-conflict)
  (let ((i (smerge-get-current)))
    (if (<= i 0) (error "Not inside a version")
      (smerge-keep-n i)
      (smerge-auto-leave))))

(defun smerge-kill-current ()
  "Remove the current (under the cursor) version."
  (interactive)
  (smerge-match-conflict)
  (let ((i (smerge-get-current)))
    (if (<= i 0) (error "Not inside a version")
      (let ((left nil))
	(dolist (n '(3 2 1))
	  (if (and (match-end n) (/= (match-end n) (match-end i)))
	      (push n left)))
	(if (and (cdr left)
		 (/= (match-end (car left)) (match-end (cadr left))))
	    (ding)			;We don't know how to do that.
	  (smerge-keep-n (car left))
	  (smerge-auto-leave))))))

(defun smerge-diff-base-mine ()
  "Diff 'base' and 'mine' version in current conflict region."
  (interactive)
  (smerge-diff 2 1))

(defun smerge-diff-base-other ()
  "Diff 'base' and 'other' version in current conflict region."
  (interactive)
  (smerge-diff 2 3))

(defun smerge-diff-mine-other ()
  "Diff 'mine' and 'other' version in current conflict region."
  (interactive)
  (smerge-diff 1 3))

(defun smerge-match-conflict ()
  "Get info about the conflict.  Puts the info in the `match-data'.
The submatches contain:
 0:  the whole conflict.
 1:  your code.
 2:  the base code.
 3:  other code.
An error is raised if not inside a conflict."
  (save-excursion
    (condition-case nil
	(let* ((orig-point (point))

	       (_ (forward-line 1))
	       (_ (re-search-backward smerge-begin-re))

	       (start (match-beginning 0))
	       (mine-start (match-end 0))
	       (filename (or (match-string 1) ""))

	       (_ (re-search-forward smerge-end-re))
	       (_ (assert (< orig-point (match-end 0))))

	       (other-end (match-beginning 0))
	       (end (match-end 0))

	       (_ (re-search-backward smerge-other-re start))

	       (mine-end (match-beginning 0))
	       (other-start (match-end 0))

	       base-start base-end)

	  ;; handle the various conflict styles
	  (cond
	   ((save-excursion
	      (goto-char mine-start)
	      (re-search-forward smerge-begin-re end t))
	    ;; There's a nested conflict and we're after the beginning
	    ;; of the outer one but before the beginning of the inner one.
	    ;; Of course, maybe this is not a nested conflict but in that
	    ;; case it can only be something nastier that we don't know how
	    ;; to handle, so may as well arbitrarily decide to treat it as
	    ;; a nested conflict.  --Stef
	    (error "There is a nested conflict"))

	   ((re-search-backward smerge-base-re start t)
	    ;; a 3-parts conflict
	    (set (make-local-variable 'smerge-conflict-style) 'diff3-A)
	    (setq base-end mine-end)
	    (setq mine-end (match-beginning 0))
	    (setq base-start (match-end 0)))

	   ((string= filename (file-name-nondirectory
			       (or buffer-file-name "")))
	    ;; a 2-parts conflict
	    (set (make-local-variable 'smerge-conflict-style) 'diff3-E))

	   ((and (not base-start)
		 (or (eq smerge-conflict-style 'diff3-A)
		     (equal filename "ANCESTOR")
		     (string-match "\\`[.0-9]+\\'" filename)))
	    ;; a same-diff conflict
	    (setq base-start mine-start)
	    (setq base-end   mine-end)
	    (setq mine-start other-start)
	    (setq mine-end   other-end)))

	  (store-match-data (list start end
				  mine-start mine-end
				  base-start base-end
				  other-start other-end
				  (when base-start (1- base-start)) base-start
				  (1- other-start) other-start))
	  t)
      (search-failed (error "Point not in conflict region")))))

(add-to-list 'debug-ignored-errors "Point not in conflict region")

(defun smerge-conflict-overlay (pos)
  "Return the conflict overlay at POS if any."
  (let ((ols (overlays-at pos))
        conflict)
    (dolist (ol ols)
      (if (and (eq (overlay-get ol 'smerge) 'conflict)
               (> (overlay-end ol) pos))
          (setq conflict ol)))
    conflict))

(defun smerge-find-conflict (&optional limit)
  "Find and match a conflict region.  Intended as a font-lock MATCHER.
The submatches are the same as in `smerge-match-conflict'.
Returns non-nil if a match is found between point and LIMIT.
Point is moved to the end of the conflict."
  (let ((found nil)
        (pos (point))
        conflict)
    ;; First check to see if point is already inside a conflict, using
    ;; the conflict overlays.
    (while (and (not found) (setq conflict (smerge-conflict-overlay pos)))
      ;; Check the overlay's validity and kill it if it's out of date.
      (condition-case nil
          (progn
            (goto-char (overlay-start conflict))
            (smerge-match-conflict)
            (goto-char (match-end 0))
            (if (<= (point) pos)
                (error "Matching backward!")
              (setq found t)))
        (error (smerge-remove-props
                (overlay-start conflict) (overlay-end conflict))
               (goto-char pos))))
    ;; If we're not already inside a conflict, look for the next conflict
    ;; and add/update its overlay.
    (while (and (not found) (re-search-forward smerge-begin-re limit t))
      (condition-case nil
          (progn
            (smerge-match-conflict)
            (goto-char (match-end 0))
            (let ((conflict (smerge-conflict-overlay (1- (point)))))
              (if conflict
                  ;; Update its location, just in case it got messed up.
                  (move-overlay conflict (match-beginning 0) (match-end 0))
                (setq conflict (make-overlay (match-beginning 0) (match-end 0)
                                             nil 'front-advance nil))
                (overlay-put conflict 'evaporate t)
                (overlay-put conflict 'smerge 'conflict)
                (let ((props smerge-text-properties))
                  (while props
                    (overlay-put conflict (pop props) (pop props))))))
            (setq found t))
        (error nil)))
    found))

;;; Refined change highlighting

(defvar smerge-refine-forward-function 'smerge-refine-forward
  "Function used to determine an \"atomic\" element.
You can set it to `forward-char' to get char-level granularity.
Its behavior has mainly two restrictions:
- if this function encounters a newline, it's important that it stops right
  after the newline.
  This only matters if `smerge-refine-ignore-whitespace' is nil.
- it needs to be unaffected by changes performed by the `preproc' argument
  to `smerge-refine-subst'.
  This only matters if `smerge-refine-weight-hack' is nil.")

(defvar smerge-refine-ignore-whitespace t
  "If non-nil, indicate that `smerge-refine' should try to ignore change in whitespace.")

(defvar smerge-refine-weight-hack t
  "If non-nil, pass to diff as many lines as there are chars in the region.
I.e. each atomic element (e.g. word) will be copied as many times (on different
lines) as it has chars.  This has two advantages:
- if `diff' tries to minimize the number *lines* (rather than chars)
  added/removed, this adjust the weights so that adding/removing long
  symbols is considered correspondingly more costly.
- `smerge-refine-forward-function' only needs to be called when chopping up
  the regions, and `forward-char' can be used afterwards.
It has the following disadvantages:
- cannot use `diff -w' because the weighting causes added spaces in a line
  to be represented as added copies of some line, so `diff -w' can't do the
  right thing any more.
- may in degenerate cases take a 1KB input region and turn it into a 1MB
  file to pass to diff.")

(defun smerge-refine-forward (n)
  (let ((case-fold-search nil)
        (re "[[:upper:]]?[[:lower:]]+\\|[[:upper:]]+\\|[[:digit:]]+\\|.\\|\n"))
    (when (and smerge-refine-ignore-whitespace
               ;; smerge-refine-weight-hack causes additional spaces to
               ;; appear as additional lines as well, so even if diff ignore
               ;; whitespace changes, it'll report added/removed lines :-(
               (not smerge-refine-weight-hack))
      (setq re (concat "[ \t]*\\(?:" re "\\)")))
    (dotimes (_i n)
      (unless (looking-at re) (error "Smerge refine internal error"))
      (goto-char (match-end 0)))))

(defun smerge-refine-chopup-region (beg end file &optional preproc)
  "Chopup the region into small elements, one per line.
Save the result into FILE.
If non-nil, PREPROC is called with no argument in a buffer that contains
a copy of the text, just before chopping it up.  It can be used to replace
chars to try and eliminate some spurious differences."
  ;; We used to chop up char-by-char rather than word-by-word like ediff
  ;; does.  It had the benefit of simplicity and very fine results, but it
  ;; often suffered from problem that diff would find correlations where
  ;; there aren't any, so the resulting "change" didn't make much sense.
  ;; You can still get this behavior by setting
  ;; `smerge-refine-forward-function' to `forward-char'.
  (let ((buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring buf beg end)
      (when preproc (goto-char (point-min)) (funcall preproc))
      (when smerge-refine-ignore-whitespace
        ;; It doesn't make much of a difference for diff-fine-highlight
        ;; because we still have the _/+/</>/! prefix anyway.  Can still be
        ;; useful in other circumstances.
        (subst-char-in-region (point-min) (point-max) ?\n ?\s))
      (goto-char (point-min))
      (while (not (eobp))
        (funcall smerge-refine-forward-function 1)
        (let ((s (if (prog2 (forward-char -1) (bolp) (forward-char 1))
                     nil
                   (buffer-substring (line-beginning-position) (point)))))
          ;; We add \n after each char except after \n, so we get
          ;; one line per text char, where each line contains
          ;; just one char, except for \n chars which are
          ;; represented by the empty line.
          (unless (eq (char-before) ?\n) (insert ?\n))
          ;; HACK ALERT!!
          (if smerge-refine-weight-hack
              (dotimes (_i (1- (length s))) (insert s "\n")))))
      (unless (bolp) (error "Smerge refine internal error"))
      (let ((coding-system-for-write 'emacs-mule))
        (write-region (point-min) (point-max) file nil 'nomessage)))))

(defun smerge-refine-highlight-change (buf beg match-num1 match-num2 props)
  (with-current-buffer buf
    (goto-char beg)
    (let* ((startline (- (string-to-number match-num1) 1))
           (beg (progn (funcall (if smerge-refine-weight-hack
                                    'forward-char
                                  smerge-refine-forward-function)
                                startline)
                       (point)))
           (end (progn (funcall (if smerge-refine-weight-hack
                                    'forward-char
                                  smerge-refine-forward-function)
                          (if match-num2
                              (- (string-to-number match-num2)
                                 startline)
                            1))
                       (point))))
      (when smerge-refine-ignore-whitespace
        (skip-chars-backward " \t\n" beg) (setq end (point))
        (goto-char beg)
        (skip-chars-forward " \t\n" end)  (setq beg (point)))
      (when (> end beg)
        (let ((ol (make-overlay
                   beg end nil
                   ;; Make them tend to shrink rather than spread when editing.
                   'front-advance nil)))
          (overlay-put ol 'evaporate t)
          (dolist (x props) (overlay-put ol (car x) (cdr x)))
          ol)))))

(defun smerge-refine-subst (beg1 end1 beg2 end2 props &optional preproc)
  "Show fine differences in the two regions BEG1..END1 and BEG2..END2.
PROPS is an alist of properties to put (via overlays) on the changes.
If non-nil, PREPROC is called with no argument in a buffer that contains
a copy of a region, just before preparing it to for `diff'.  It can be
used to replace chars to try and eliminate some spurious differences."
  (let* ((buf (current-buffer))
         (pos (point))
         deactivate-mark         ; The code does not modify any visible buffer.
         (file1 (make-temp-file "diff1"))
         (file2 (make-temp-file "diff2")))
    ;; Chop up regions into smaller elements and save into files.
    (smerge-refine-chopup-region beg1 end1 file1 preproc)
    (smerge-refine-chopup-region beg2 end2 file2 preproc)

    ;; Call diff on those files.
    (unwind-protect
        (with-temp-buffer
          (let ((coding-system-for-read 'emacs-mule))
            (call-process diff-command nil t nil
                          (if (and smerge-refine-ignore-whitespace
                                   (not smerge-refine-weight-hack))
                              ;; Pass -a so diff treats it as a text file even
                              ;; if it contains \0 and such.
                              ;; Pass -d so as to get the smallest change, but
                              ;; also and more importantly because otherwise it
                              ;; may happen that diff doesn't behave like
                              ;; smerge-refine-weight-hack expects it to.
                              ;; See http://thread.gmane.org/gmane.emacs.devel/82685.
                              "-awd" "-ad")
                          file1 file2))
          ;; Process diff's output.
          (goto-char (point-min))
          (let ((last1 nil)
                (last2 nil))
            (while (not (eobp))
              (if (not (looking-at "\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?\\([acd]\\)\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)?$"))
                  (error "Unexpected patch hunk header: %s"
                         (buffer-substring (point) (line-end-position))))
              (let ((op (char-after (match-beginning 3)))
                    (m1 (match-string 1))
                    (m2 (match-string 2))
                    (m4 (match-string 4))
                    (m5 (match-string 5)))
                (when (memq op '(?d ?c))
                  (setq last1
                        (smerge-refine-highlight-change buf beg1 m1 m2 props)))
                (when (memq op '(?a ?c))
                  (setq last2
                        (smerge-refine-highlight-change buf beg2 m4 m5 props))))
              (forward-line 1)                            ;Skip hunk header.
              (and (re-search-forward "^[0-9]" nil 'move) ;Skip hunk body.
                   (goto-char (match-beginning 0))))
            ;; (assert (or (null last1) (< (overlay-start last1) end1)))
            ;; (assert (or (null last2) (< (overlay-start last2) end2)))
            (if smerge-refine-weight-hack
                (progn
                  ;; (assert (or (null last1) (<= (overlay-end last1) end1)))
                  ;; (assert (or (null last2) (<= (overlay-end last2) end2)))
                  )
              ;; smerge-refine-forward-function when calling in chopup may
              ;; have stopped because it bumped into EOB whereas in
              ;; smerge-refine-weight-hack it may go a bit further.
              (if (and last1 (> (overlay-end last1) end1))
                  (move-overlay last1 (overlay-start last1) end1))
              (if (and last2 (> (overlay-end last2) end2))
                  (move-overlay last2 (overlay-start last2) end2))
              )))
      (goto-char pos)
      (delete-file file1)
      (delete-file file2))))

(defun smerge-refine (&optional part)
  "Highlight the words of the conflict that are different.
For 3-way conflicts, highlights only two of the three parts.
A numeric argument PART can be used to specify which two parts;
repeating the command will highlight other two parts."
  (interactive
   (if (integerp current-prefix-arg) (list current-prefix-arg)
     (smerge-match-conflict)
     (let* ((prop (get-text-property (match-beginning 0) 'smerge-refine-part))
            (part (if (and (consp prop)
                           (eq (buffer-chars-modified-tick) (car prop)))
                      (cdr prop))))
       ;; If already highlighted, cycle.
       (list (if (integerp part) (1+ (mod part 3)))))))

  (if (and (integerp part) (or (< part 1) (> part 3)))
      (error "No conflict part nb %s" part))
  (smerge-match-conflict)
  (remove-overlays (match-beginning 0) (match-end 0) 'smerge 'refine)
  ;; Ignore `part' if not applicable, and default it if not provided.
  (setq part (cond ((null (match-end 2)) 2)
                   ((eq (match-end 1) (match-end 3)) 1)
                   ((integerp part) part)
                   ;; If one of the parts is empty, any refinement using
                   ;; it will be trivial and uninteresting.
                   ((eq (match-end 1) (match-beginning 1)) 1)
                   ((eq (match-end 3) (match-beginning 3)) 3)
                   (t 2)))
  (let ((n1 (if (eq part 1) 2 1))
        (n2 (if (eq part 3) 2 3)))
    (smerge-ensure-match n1)
    (smerge-ensure-match n2)
    (with-silent-modifications
      (put-text-property (match-beginning 0) (1+ (match-beginning 0))
                         'smerge-refine-part
                         (cons (buffer-chars-modified-tick) part)))
    (smerge-refine-subst (match-beginning n1) (match-end n1)
                         (match-beginning n2)  (match-end n2)
                         '((smerge . refine)
                           (face . smerge-refined-change)))))

(defun smerge-diff (n1 n2)
  (smerge-match-conflict)
  (smerge-ensure-match n1)
  (smerge-ensure-match n2)
  (let ((name1 (aref smerge-match-names n1))
	(name2 (aref smerge-match-names n2))
	;; Read them before the match-data gets clobbered.
	(beg1 (match-beginning n1))
	(end1 (match-end n1))
	(beg2 (match-beginning n2))
	(end2 (match-end n2))
	(file1 (make-temp-file "smerge1"))
	(file2 (make-temp-file "smerge2"))
	(dir default-directory)
	(file (if buffer-file-name (file-relative-name buffer-file-name)))
        ;; We would want to use `emacs-mule-unix' for read&write, but we
        ;; bump into problems with the coding-system used by diff to write
        ;; the file names and the time stamps in the header.
        ;; `buffer-file-coding-system' is not always correct either, but if
        ;; the OS/user uses only one coding-system, then it works.
	(coding-system-for-read buffer-file-coding-system))
    (write-region beg1 end1 file1 nil 'nomessage)
    (write-region beg2 end2 file2 nil 'nomessage)
    (unwind-protect
	(with-current-buffer (get-buffer-create smerge-diff-buffer-name)
	  (setq default-directory dir)
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (let ((status
		   (apply 'call-process diff-command nil t nil
			  (append smerge-diff-switches
				  (list "-L" (concat name1 "/" file)
					"-L" (concat name2 "/" file)
					file1 file2)))))
	      (if (eq status 0) (insert "No differences found.\n"))))
	  (goto-char (point-min))
	  (diff-mode)
	  (display-buffer (current-buffer) t))
      (delete-file file1)
      (delete-file file2))))

;; compiler pacifiers
(defvar smerge-ediff-windows)
(defvar smerge-ediff-buf)
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
(defvar ediff-ancestor-buffer)
(defvar ediff-quit-hook)
(declare-function ediff-cleanup-mess "ediff-util" nil)

;;;###autoload
(defun smerge-ediff (&optional name-mine name-other name-base)
  "Invoke ediff to resolve the conflicts.
NAME-MINE, NAME-OTHER, and NAME-BASE, if non-nil, are used for the
buffer names."
  (interactive)
  (let* ((buf (current-buffer))
	 (mode major-mode)
	 ;;(ediff-default-variant 'default-B)
	 (config (current-window-configuration))
	 (filename (file-name-nondirectory buffer-file-name))
	 (mine (generate-new-buffer
		(or name-mine (concat "*" filename " MINE*"))))
	 (other (generate-new-buffer
		 (or name-other (concat "*" filename " OTHER*"))))
	 base)
    (with-current-buffer mine
      (buffer-disable-undo)
      (insert-buffer-substring buf)
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(when (match-beginning 2) (setq base t))
	(smerge-keep-n 1))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (funcall mode))

    (with-current-buffer other
      (buffer-disable-undo)
      (insert-buffer-substring buf)
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(smerge-keep-n 3))
      (buffer-enable-undo)
      (set-buffer-modified-p nil)
      (funcall mode))

    (when base
      (setq base (generate-new-buffer
		  (or name-base (concat "*" filename " BASE*"))))
      (with-current-buffer base
	(buffer-disable-undo)
	(insert-buffer-substring buf)
	(goto-char (point-min))
	(while (smerge-find-conflict)
	  (if (match-end 2)
	      (smerge-keep-n 2)
	    (delete-region (match-beginning 0) (match-end 0))))
	(buffer-enable-undo)
	(set-buffer-modified-p nil)
	(funcall mode)))

    ;; the rest of the code is inspired from vc.el
    ;; Fire up ediff.
    (set-buffer
     (if base
	 (ediff-merge-buffers-with-ancestor mine other base)
	  ;; nil 'ediff-merge-revisions-with-ancestor buffer-file-name)
       (ediff-merge-buffers mine other)))
        ;; nil 'ediff-merge-revisions buffer-file-name)))

    ;; Ediff is now set up, and we are in the control buffer.
    ;; Do a few further adjustments and take precautions for exit.
    (set (make-local-variable 'smerge-ediff-windows) config)
    (set (make-local-variable 'smerge-ediff-buf) buf)
    (set (make-local-variable 'ediff-quit-hook)
	 (lambda ()
	   (let ((buffer-A ediff-buffer-A)
		 (buffer-B ediff-buffer-B)
		 (buffer-C ediff-buffer-C)
		 (buffer-Ancestor ediff-ancestor-buffer)
		 (buf smerge-ediff-buf)
		 (windows smerge-ediff-windows))
	     (ediff-cleanup-mess)
	     (with-current-buffer buf
	       (erase-buffer)
	       (insert-buffer-substring buffer-C)
	       (kill-buffer buffer-A)
	       (kill-buffer buffer-B)
	       (kill-buffer buffer-C)
	       (when (bufferp buffer-Ancestor) (kill-buffer buffer-Ancestor))
	       (set-window-configuration windows)
	       (message "Conflict resolution finished; you may save the buffer")))))
    (message "Please resolve conflicts now; exit ediff when done")))

(defun smerge-makeup-conflict (pt1 pt2 pt3 &optional pt4)
  "Insert diff3 markers to make a new conflict.
Uses point and mark for two of the relevant positions and previous marks
for the other ones.
By default, makes up a 2-way conflict,
with a \\[universal-argument] prefix, makes up a 3-way conflict."
  (interactive
   (list (point)
         (mark)
         (progn (pop-mark) (mark))
         (when current-prefix-arg (pop-mark) (mark))))
  ;; Start from the end so as to avoid problems with pos-changes.
  (destructuring-bind (pt1 pt2 pt3 &optional pt4)
      (sort (list* pt1 pt2 pt3 (if pt4 (list pt4))) '>=)
    (goto-char pt1) (beginning-of-line)
    (insert ">>>>>>> OTHER\n")
    (goto-char pt2) (beginning-of-line)
    (insert "=======\n")
    (goto-char pt3) (beginning-of-line)
    (when pt4
      (insert "||||||| BASE\n")
      (goto-char pt4) (beginning-of-line))
    (insert "<<<<<<< MINE\n"))
  (if smerge-mode nil (smerge-mode 1))
  (smerge-refine))


(defconst smerge-parsep-re
  (concat smerge-begin-re "\\|" smerge-end-re "\\|"
          smerge-base-re "\\|" smerge-other-re "\\|"))

;;;###autoload
(define-minor-mode smerge-mode
  "Minor mode to simplify editing output from the diff3 program.
With a prefix argument ARG, enable the mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil.
\\{smerge-mode-map}"
  :group 'smerge :lighter " SMerge"
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (save-excursion
      (if smerge-mode
	  (font-lock-add-keywords nil smerge-font-lock-keywords 'append)
	(font-lock-remove-keywords nil smerge-font-lock-keywords))
      (goto-char (point-min))
      (while (smerge-find-conflict)
	(save-excursion
	  (font-lock-fontify-region (match-beginning 0) (match-end 0) nil)))))
  (if (string-match (regexp-quote smerge-parsep-re) paragraph-separate)
      (unless smerge-mode
        (set (make-local-variable 'paragraph-separate)
             (replace-match "" t t paragraph-separate)))
    (when smerge-mode
        (set (make-local-variable 'paragraph-separate)
             (concat smerge-parsep-re paragraph-separate))))
  (unless smerge-mode
    (smerge-remove-props (point-min) (point-max))))

;;;###autoload
(defun smerge-start-session ()
  "Turn on `smerge-mode' and move point to first conflict marker.
If no conflict maker is found, turn off `smerge-mode'."
  (interactive)
  (smerge-mode 1)
  (condition-case nil
      (unless (looking-at smerge-begin-re)
        (smerge-next))
    (error (smerge-auto-leave))))

(provide 'smerge-mode)

;;; smerge-mode.el ends here
