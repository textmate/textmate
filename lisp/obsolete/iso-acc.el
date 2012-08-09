;;; iso-acc.el --- minor mode providing electric accent keys

;; Copyright (C) 1993-1994, 1996, 2001-2012 Free Software Foundation, Inc.

;; Author: Johan Vromans
;; Maintainer: FSF
;; Keywords: i18n
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

;; Function `iso-accents-mode' activates a minor mode in which
;; typewriter "dead keys" are emulated.  The purpose of this emulation
;; is to provide a simple means for inserting accented characters
;; according to the ISO-8859-1...3 character sets.
;;
;; In `iso-accents-mode', pseudo accent characters are used to
;; introduce accented keys.  The pseudo-accent characters are:
;;
;;   '  (minute)    -> acute accent
;;   `  (backtick)  -> grave accent
;;   "  (second)    -> diaeresis
;;   ^  (caret)     -> circumflex
;;   ~  (tilde)     -> tilde over the character
;;   /  (slash)     -> slash through the character.
;;                     Also:  /A is A-with-ring and /E is AE ligature.
;; These two are enabled only if you set iso-accents-enable
;; to include them:
;;   .  (period)    -> dot over the character (some languages only)
;;   ,  (cedilla)   -> cedilla under the character (some languages only)
;;
;; The action taken depends on the key that follows the pseudo accent.
;; In general:
;;
;;   pseudo-accent + appropriate letter -> accented letter
;;   pseudo-accent + space -> pseudo-accent (except comma and period)
;;   pseudo-accent + pseudo-accent -> accent (if available)
;;   pseudo-accent + other -> pseudo-accent + other
;;
;; If the pseudo-accent is followed by anything else than a
;; self-insert-command, the dead-key code is terminated, the
;; pseudo-accent inserted 'as is' and the bell is rung to signal this.
;;
;; Function `iso-accents-mode' can be used to enable the iso accents
;; minor mode, or disable it.

;; If you want only some of these characters to serve as accents,
;; add a language to `iso-languages' which specifies the accent characters
;; that you want, then select the language with `iso-accents-customize'.

;;; Code:

(provide 'iso-acc)

(defgroup iso-acc nil
  "Minor mode providing electric accent keys."
  :prefix "iso-accents-"
  :group 'i18n)

(defcustom iso-accents-insert-offset nonascii-insert-offset
  "*Offset added by ISO Accents mode to character codes 0200 and above."
  :type 'integer
  :group 'iso-acc)

(defvar iso-languages
  '(("catalan"
     ;; Note this includes some extra characters used in Spanish,
     ;; on the idea that someone who uses Catalan is likely to use Spanish
     ;; as well.
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363) (?u . ?\372)
	 (?\  . ?'))
     (?` (?A . ?\300) (?E . ?\310) (?O . ?\322)
	 (?a . ?\340) (?e . ?\350) (?o . ?\362)
	 (?\  . ?`))
     (?\" (?I . ?\317) (?U . ?\334) (?i . ?\357) (?u . ?\374)
	  (?\  . ?\"))
     (?~ (?C . ?\307) (?N . ?\321) (?c . ?\347) (?n . ?\361)
	 (?> . ?\273) (?< . ?\253) (?! . ?\241) (?? . ?\277)
	 (?\  . ?\~)))

    ("esperanto"
     (?^ (?H . ?\246) (?J . ?\254) (?h . ?\266) (?j . ?\274) (?C . ?\306)
	 (?G . ?\330) (?S . ?\336) (?c . ?\346) (?g . ?\370) (?s . ?\376)
	 (?^ . ?^) (?\  . ?^))
     (?~ (?U . ?\335) (?u . ?\375) (?\  . ?~)))

    ("french"
     (?' (?E . ?\311) (?C . ?\307) (?e . ?\351) (?c . ?\347)
	 (?\  . ?'))
     (?` (?A . ?\300) (?E . ?\310) (?U . ?\331)
         (?a . ?\340) (?e . ?\350) (?u . ?\371)
	 (?\  . ?`))
     (?^ (?A . ?\302) (?E . ?\312) (?I . ?\316) (?O . ?\324) (?U . ?\333)
	 (?a . ?\342) (?e . ?\352) (?i . ?\356) (?o . ?\364) (?u . ?\373)
	 (?\  . ?^))
     (?\" (?E . ?\313) (?I . ?\317)
          (?e . ?\353) (?i . ?\357)
	  (?\  . ?\"))
     (?~ (?< . ?\253) (?> . ?\273) (?C . ?\307) (?c . ?\347)
	 (?\  . ?~))
     (?, (?C . ?\307) (?c . ?\347) (?\  . ?\,)))

    ("german"
     (?\" (?A . ?\304) (?O . ?\326) (?U . ?\334)
	  (?a . ?\344) (?o . ?\366) (?u . ?\374) (?s . ?\337) (?\  . ?\")))

    ("irish"
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363) (?u . ?\372)
	 (?\  . ?')))

    ("portuguese"
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?C . ?\307) (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363)
	 (?u . ?\372) (?c . ?\347)
	 (?\  . ?'))
     (?` (?A . ?\300) (?a . ?\340)
	 (?\  . ?`))
     (?^ (?A . ?\302) (?E . ?\312) (?O . ?\324)
	 (?a . ?\342) (?e . ?\352) (?o . ?\364)
	 (?\  . ?^))
     (?\" (?U . ?\334) (?u . ?\374)
	  (?\  . ?\"))
     (?~ (?A . ?\303) (?O . ?\325) (?a . ?\343) (?o . ?\365)
         (?C . ?\307) (?N . ?\321) (?c . ?\347) (?n . ?\361)
	 (?\  . ?~))
     (?, (?c . ?\347) (?C . ?\307) (?, . ?,)))

    ("spanish"
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363) (?u . ?\372)
	 (?\  . ?'))
     (?\" (?U . ?\334) (?u . ?\374) (?\  . ?\"))
     (?\~ (?N . ?\321) (?n . ?\361) (?> . ?\273) (?< . ?\253) (?! . ?\241)
          (?? . ?\277) (?\  . ?\~)))

    ("latin-1"
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?Y . ?\335) (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363)
	 (?u . ?\372) (?y . ?\375) (?' . ?\264)
	 (?\  . ?'))
     (?` (?A . ?\300) (?E . ?\310) (?I . ?\314) (?O . ?\322) (?U . ?\331)
	 (?a . ?\340) (?e . ?\350) (?i . ?\354) (?o . ?\362) (?u . ?\371)
	 (?` . ?`) (?\  . ?`))
     (?^ (?A . ?\302) (?E . ?\312) (?I . ?\316) (?O . ?\324) (?U . ?\333)
	 (?a . ?\342) (?e . ?\352) (?i . ?\356) (?o . ?\364) (?u . ?\373)
	 (?^ . ?^) (?\  . ?^))
     (?\" (?A . ?\304) (?E . ?\313) (?I . ?\317) (?O . ?\326) (?U . ?\334)
	  (?a . ?\344) (?e . ?\353) (?i . ?\357) (?o . ?\366) (?s . ?\337)
	  (?u . ?\374) (?y . ?\377)
	  (?\" . ?\250) (?\  . ?\"))
     (?~ (?A . ?\303) (?C . ?\307) (?D . ?\320) (?N . ?\321) (?O . ?\325)
	 (?T . ?\336) (?a . ?\343) (?c . ?\347) (?d . ?\360) (?n . ?\361)
	 (?o . ?\365) (?t . ?\376)
	 (?> . ?\273) (?< . ?\253) (?! . ?\241) (?? . ?\277)
	 (?\~ . ?\270) (?\  . ?~))
     (?/ (?A . ?\305) (?E . ?\306) (?O . ?\330) (?a . ?\345) (?e . ?\346)
	 (?o . ?\370)
	 (?/ . ?\260) (?\  . ?/)))

    ("latin-2" latin-iso8859-2
     (?' (?A . ?\301) (?C . ?\306) (?D . ?\320) (?E . ?\311) (?I . ?\315)
	 (?L . ?\305) (?N . ?\321) (?O . ?\323) (?R . ?\300) (?S . ?\246)
	 (?U . ?\332) (?Y . ?\335) (?Z . ?\254)
	 (?a . ?\341) (?c . ?\346) (?d . ?\360) (?e . ?\351) (?i . ?\355)
	 (?l . ?\345) (?n . ?\361) (?o . ?\363) (?r . ?\340) (?s . ?\266)
	 (?u . ?\372) (?y . ?\375) (?z . ?\274)
	 (?' . ?\264) (?\  . ?'))
     (?` (?A . ?\241) (?C . ?\307) (?E . ?\312) (?L . ?\243) (?S . ?\252)
	 (?T . ?\336) (?Z . ?\257)
	 (?a . ?\261) (?l . ?\263) (?c . ?\347) (?e . ?\352) (?s . ?\272)
	 (?t . ?\376) (?z . ?\277)
	 (?` . ?\252)
	 (?. . ?\377) (?\  . ?`))
     (?^ (?A . ?\302) (?I . ?\316) (?O . ?\324)
	 (?a . ?\342) (?i . ?\356) (?o . ?\364)
	 (?^ . ?^)			; no special code?
	 (?\  . ?^))
     (?\" (?A . ?\304) (?E . ?\313) (?O . ?\326) (?U . ?\334)
	  (?a . ?\344) (?e . ?\353) (?o . ?\366) (?s . ?\337) (?u . ?\374)
	  (?\" . ?\250)
	  (?\  . ?\"))
     (?~ (?A . ?\303) (?C . ?\310) (?D . ?\317) (?L . ?\245) (?N . ?\322)
	 (?O . ?\325) (?R . ?\330) (?S . ?\251) (?T . ?\253) (?U . ?\333)
	 (?Z . ?\256)
	 (?a . ?\343) (?c . ?\350) (?d . ?\357) (?l . ?\265) (?n . ?\362)
	 (?o . ?\365) (?r . ?\370) (?s . ?\271) (?t . ?\273) (?u . ?\373)
	 (?z . ?\276)
	 (?v . ?\242)			; v accent
	 (?\~ . ?\242)			; v accent
	 (?\. . ?\270)			; cedilla accent
	 (?\  . ?~)))

    ("latin-3" latin-iso8859-3
     (?' (?A . ?\301) (?E . ?\311) (?I . ?\315) (?O . ?\323) (?U . ?\332)
	 (?a . ?\341) (?e . ?\351) (?i . ?\355) (?o . ?\363) (?u . ?\372)
	 (?' . ?\264) (?\  . ?'))
     (?` (?A . ?\300) (?E . ?\310) (?I . ?\314) (?O . ?\322) (?U . ?\331)
	 (?a . ?\340) (?e . ?\350) (?i . ?\354) (?o . ?\362) (?u . ?\371)
	 (?` . ?`) (?\  . ?`))
     (?^ (?A . ?\302) (?C . ?\306) (?E . ?\312) (?G . ?\330) (?H . ?\246)
	 (?I . ?\316) (?J . ?\254) (?O . ?\324) (?S . ?\336) (?U . ?\333)
	 (?a . ?\342) (?c . ?\346) (?e . ?\352) (?g . ?\370) (?h . ?\266)
	 (?i . ?\356) (?j . ?\274) (?o . ?\364) (?s . ?\376) (?u . ?\373)
	 (?^ . ?^) (?\  . ?^))
     (?\" (?A . ?\304) (?E . ?\313) (?I . ?\317) (?O . ?\326) (?U . ?\334)
	  (?a . ?\344) (?e . ?\353) (?i . ?\357) (?o . ?\366) (?u . ?\374)
	  (?s . ?\337)
	  (?\" . ?\250) (?\  . ?\"))
     (?~ (?A . ?\303) (?C . ?\307) (?D . ?\320) (?N . ?\321) (?O . ?\325)
	 (?a . ?\343) (?c . ?\347) (?d . ?\360) (?n . ?\361) (?o . ?\365)
	 (?$ . ?\245) (?S . ?\252) (?s . ?\272) (?G . ?\253) (?g . ?\273)
	 (?U . ?\335) (?u . ?\375) (?` . ?\242)
	 (?~ . ?\270) (?\  . ?~))
     (?/ (?C . ?\305) (?G . ?\325) (?H . ?\241) (?I . ?\251) (?Z . ?\257)
	 (?c . ?\345) (?g . ?\365) (?h . ?\261) (?i . ?\271) (?z . ?\277)
	 (?r . ?\256)
	 (?. . ?\377) (?# . ?\243) (?$ . ?\244)
	 (?/ . ?\260) (?\  . ?/))
     (?. (?C . ?\305) (?G . ?\325) (?I . ?\251) (?Z . ?\257)
 	 (?c . ?\345) (?g . ?\365) (?z . ?\277))))
  "List of language-specific customizations for the ISO Accents mode.

Each element of the list is of the form

    (LANGUAGE [CHARSET]
     (PSEUDO-ACCENT MAPPINGS)
     (PSEUDO-ACCENT MAPPINGS)
     ...)

LANGUAGE is a string naming the language.
CHARSET (which may be omitted) is the symbol name
 of the character set used in this language.
 If CHARSET is omitted, latin-iso8859-1 is the default.
PSEUDO-ACCENT is a char specifying an accent key.
MAPPINGS are cons cells of the form (CHAR . ISO-CHAR).

The net effect is that the key sequence PSEUDO-ACCENT CHAR is mapped
to ISO-CHAR on input.")

(defvar iso-language nil
  "Language for which ISO Accents mode is currently customized.
Change it with the `iso-accents-customize' function.")

(defvar iso-accents-list nil
  "Association list for ISO accent combinations, for the chosen language.")

(defcustom iso-accents-mode nil
  "*Non-nil enables ISO Accents mode.
Setting this variable makes it local to the current buffer.
See the function `iso-accents-mode'."
  :type 'boolean
  :group 'iso-acc)
(make-variable-buffer-local 'iso-accents-mode)

(defcustom iso-accents-enable '(?' ?` ?^ ?\" ?~ ?/)
  "*List of accent keys that become prefixes in ISO Accents mode.
The default is (?' ?` ?^ ?\" ?~ ?/), which contains all the supported
accent keys.  If you set this variable to a list in which some of those
characters are missing, the missing ones do not act as accents.

Note that if you specify a language with `iso-accents-customize',
that can also turn off certain prefixes (whichever ones are not needed in
the language you choose)."
  :type '(repeat character)
  :group 'iso-acc)

(defun iso-accents-accent-key (prompt)
  "Modify the following character by adding an accent to it."
  ;; Pick up the accent character.
  (if (and iso-accents-mode
	   (memq last-input-event iso-accents-enable))
      (iso-accents-compose prompt)
    (vector last-input-event)))


;; The iso-accents-compose function is called deep inside Emacs' read
;; key sequence machinery, so the call to read-event below actually
;; recurses into that machinery.  Doing that does not cause any
;; problem on its own, but read-event will have marked the window's
;; display matrix to be accurate -- which is broken by the subsequent
;; call to delete-region.  Therefore, we must call force-window-update
;; after delete-region to explicitly clear the accurate state of the
;; window's display matrix.

(defun iso-accents-compose (prompt)
  (let* ((first-char last-input-event)
	 (list (assq first-char iso-accents-list))
	 ;; Wait for the second key and look up the combination.
	 (second-char (if (or prompt
			      (not (eq (key-binding "a")
				       'self-insert-command))
			      ;; Not at start of a key sequence.
			      (> (length (this-single-command-keys)) 1)
			      ;; Called from anything but the command loop.
			      this-command)
			  (progn
			    (message "%s%c"
				     (or prompt "Compose with ")
				     first-char)
			    (read-event))
			(insert first-char)
			(prog1 (read-event)
			  (delete-region (1- (point)) (point))
			  ;; Display is no longer up-to-date.
			  (force-window-update (selected-window)))))
	 (entry (cdr (assq second-char list))))
    (if entry
	;; Found it: return the mapped char
	(vector
	 (if (and enable-multibyte-characters
		  (>= entry ?\200))
	     (+ iso-accents-insert-offset entry)
	   entry))
      ;; Otherwise, advance and schedule the second key for execution.
      (push second-char unread-command-events)
      (vector first-char))))

;; It is a matter of taste if you want the minor mode indicated
;; in the mode line...
;; If so, uncomment the next four lines.
;; (or (assq 'iso-accents-mode minor-mode-alist)
;;     (setq minor-mode-alist
;; 	  (append minor-mode-alist
;; 		  '((iso-accents-mode " ISO-Acc")))))

;;;###autoload
(defun iso-accents-mode (&optional arg)
  "Toggle ISO Accents mode, in which accents modify the following letter.
This permits easy insertion of accented characters according to ISO-8859-1.
When Iso-accents mode is enabled, accent character keys
\(`, ', \", ^, / and ~) do not self-insert; instead, they modify the following
letter key so that it inserts an ISO accented letter.

You can customize ISO Accents mode to a particular language
with the command `iso-accents-customize'.

Special combinations: ~c gives a c with cedilla,
~d gives an Icelandic eth (d with dash).
~t gives an Icelandic thorn.
\"s gives German sharp s.
/a gives a with ring.
/e gives an a-e ligature.
~< and ~> give guillemots.
~! gives an inverted exclamation mark.
~? gives an inverted question mark.

With an argument, a positive argument enables ISO Accents mode,
and a negative argument disables it."

  (interactive "P")

  (if (if arg
	  ;; Negative arg means switch it off.
	  (<= (prefix-numeric-value arg) 0)
	;; No arg means toggle.
	iso-accents-mode)
      (setq iso-accents-mode nil)

    ;; Enable electric accents.
    (setq iso-accents-mode t)))

(defun iso-accents-customize (language)
  "Customize the ISO accents machinery for a particular language.
It selects the customization based on the specifications in the
`iso-languages' variable."
  (interactive (list (completing-read "Language: " iso-languages nil t)))
  (let ((table (cdr (assoc language iso-languages)))
	all-accents tail)
    (if (not table)
	(error "Unknown language `%s'" language)
      (setq iso-accents-insert-offset (- (make-char (if (symbolp (car table))
							(car table)
						      'latin-iso8859-1))
					 128))
      (if (symbolp (car table))
	  (setq table (cdr table)))
      (setq iso-language language
	    iso-accents-list table)
      (if key-translation-map
	  (substitute-key-definition
	   'iso-accents-accent-key nil key-translation-map)
	(setq key-translation-map (make-sparse-keymap)))
      ;; Set up translations for all the characters that are used as
      ;; accent prefixes in this language.
      (setq tail iso-accents-list)
      (while tail
	(define-key key-translation-map (vector (car (car tail)))
	  'iso-accents-accent-key)
	(setq tail (cdr tail))))))

(defun iso-accentuate (start end)
  "Convert two-character sequences in region into accented characters.
Noninteractively, this operates on text from START to END.
This uses the same conversion that ISO Accents mode uses for type-in."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (forward-char 1)
      (let (entry)
	(while (< (point) end)
	  (if (and (memq (preceding-char) iso-accents-enable)
		   (setq entry (cdr (assq (following-char) (assq (preceding-char) iso-accents-list)))))
	      (progn
		(forward-char -1)
		(delete-char 2)
		(insert entry)
		(setq end (1- end)))
	    (forward-char 1)))))))

(defun iso-accent-rassoc-unit (value alist)
  (let (elt acc)
    (while (and alist (not elt))
      (setq acc (car (car alist))
	    elt (car (rassq value (cdr (car alist))))
	    alist (cdr alist)))
    (if elt
	(cons acc elt))))

(defun iso-unaccentuate (start end)
  "Convert accented characters in the region into two-character sequences.
Noninteractively, this operates on text from START to END.
This uses the opposite of the conversion done by ISO Accents mode for type-in."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (entry)
	(while (< (point) end)
	  (if (and (> (following-char) 127)
		   (setq entry (iso-accent-rassoc-unit (following-char)
						       iso-accents-list)))
	      (progn
		(delete-char 1)
		(insert (car entry) (cdr entry))
		(setq end (1+ end)))
	    (forward-char 1)))))))

(defun iso-deaccentuate (start end)
  "Convert accented characters in the region into unaccented characters.
Noninteractively, this operates on text from START to END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (let (entry)
	(while (< (point) end)
	  (if (and (> (following-char) 127)
		   (setq entry (iso-accent-rassoc-unit (following-char)
						       iso-accents-list)))
	      (progn
		(delete-char 1)
		(insert (cdr entry)))
	    (forward-char 1)))))))

;; Set up the default settings.
(iso-accents-customize "latin-1")

;; Use Iso-Accents mode in the minibuffer
;; if it was in use in the previous buffer.
(defun iso-acc-minibuf-setup ()
  (setq iso-accents-mode
	(with-current-buffer (window-buffer minibuffer-scroll-window)
	  iso-accents-mode)))

(add-hook 'minibuffer-setup-hook 'iso-acc-minibuf-setup)

;;; iso-acc.el ends here
