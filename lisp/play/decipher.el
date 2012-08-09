;;; decipher.el --- cryptanalyze monoalphabetic substitution ciphers
;;
;; Copyright (C) 1995-1996, 2001-2012  Free Software Foundation, Inc.
;;
;; Author: Christopher J. Madsen <chris_madsen@geocities.com>
;; Keywords: games
;;
;; This file is part of GNU Emacs.
;;
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
;;; Quick Start:
;;
;; To decipher a message, type or load it into a buffer and type
;; `M-x decipher'.  This will format the buffer and place it into
;; Decipher mode.  You can save your work to a file with the normal
;; Emacs save commands; when you reload the file it will automatically
;; enter Decipher mode.
;;
;; I'm not going to discuss how to go about breaking a cipher; try
;; your local library for a book on cryptanalysis.  One book you might
;; find is:
;;   Cryptanalysis:  A study of ciphers and their solution
;;   Helen Fouche Gaines
;;   ISBN 0-486-20097-3

;; This package is designed to help you crack simple substitution
;; ciphers where one letter stands for another.  It works for ciphers
;; with or without word divisions.  (You must set the variable
;; decipher-ignore-spaces for ciphers without word divisions.)
;;
;; First, some quick definitions:
;;   ciphertext   The encrypted message (what you start with)
;;   plaintext    The decrypted message (what you are trying to get)
;;
;; Decipher mode displays ciphertext in uppercase and plaintext in
;; lowercase.  You must enter the plaintext in lowercase; uppercase
;; letters are interpreted as commands.  The ciphertext may be entered
;; in mixed case; `M-x decipher' will convert it to uppercase.
;;
;; Decipher mode depends on special characters in the first column of
;; each line.  The command `M-x decipher' inserts these characters for
;; you.  The characters and their meanings are:
;;   (   The plaintext & ciphertext alphabets on the first line
;;   )   The ciphertext & plaintext alphabets on the second line
;;   :   A line of ciphertext (with plaintext below)
;;   >   A line of plaintext  (with ciphertext above)
;;   %   A comment
;; Each line in the buffer MUST begin with one of these characters (or
;; be left blank).  In addition, comments beginning with `%!' are reserved
;; for checkpoints; see decipher-make-checkpoint & decipher-restore-checkpoint
;; for more information.
;;
;; While the cipher message may contain digits or punctuation, Decipher
;; mode will ignore these characters.
;;
;; The buffer is made read-only so it can't be modified by normal
;; Emacs commands.
;;
;; Decipher supports Font Lock mode.  To use it, you can also add
;;     (add-hook 'decipher-mode-hook 'turn-on-font-lock)
;; See the variable `decipher-font-lock-keywords' if you want to customize
;; the faces used.  I'd like to thank Simon Marshall for his help in making
;; Decipher work well with Font Lock.

;;; Things To Do:
;;
;; Email me if you have any suggestions or would like to help.
;; But be aware that I work on Decipher only sporadically.
;;
;; 1. The consonant-line shortcut
;; 2. More functions for analyzing ciphertext

;;;===================================================================
;;; Variables:
;;;===================================================================

(eval-when-compile
  (require 'cl))

(defgroup decipher nil
  "Cryptanalyze monoalphabetic substitution ciphers."
  :prefix "decipher-"
  :group 'games)

(defcustom decipher-force-uppercase t
  "Non-nil means to convert ciphertext to uppercase.
nil means the case of the ciphertext is preserved.
This variable must be set before typing `\\[decipher]'."
  :type 'boolean
  :group 'decipher)


(defcustom decipher-ignore-spaces nil
  "Non-nil means to ignore spaces and punctuation when counting digrams.
You should set this to nil if the cipher message is divided into words,
or t if it is not.
This variable is buffer-local."
  :type 'boolean
  :group 'decipher)
(make-variable-buffer-local 'decipher-ignore-spaces)

(defcustom decipher-undo-limit 5000
  "The maximum number of entries in the undo list.
When the undo list exceeds this number, 100 entries are deleted from
the tail of the list."
  :type 'integer
  :group 'decipher)

(defcustom decipher-mode-hook nil
  "Hook to run upon entry to decipher."
  :type 'hook
  :group 'decipher)

;; End of user modifiable variables
;;--------------------------------------------------------------------

(defvar decipher-font-lock-keywords
  '(("^:.*"  . font-lock-keyword-face)
    ("^>.*"  . font-lock-string-face)
    ("^%!.*" . font-lock-constant-face)
    ("^%.*"  . font-lock-comment-face)
    ("\\`(\\([a-z]+\\) +\\([A-Z]+\\)"
     (1 font-lock-string-face)
     (2 font-lock-keyword-face))
    ("^)\\([A-Z ]+\\)\\([a-z ]+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face)))
  "Expressions to fontify in Decipher mode.

Ciphertext uses `font-lock-keyword-face', plaintext uses
`font-lock-string-face', comments use `font-lock-comment-face', and
checkpoints use `font-lock-constant-face'.  You can customize the
display by changing these variables.  For best results, I recommend
that all faces use the same background color.

For example, to display ciphertext in the `bold' face, use
  (add-hook 'decipher-mode-hook
            (lambda () (set (make-local-variable 'font-lock-keyword-face)
                            'bold)))
in your `.emacs' file.")

(defvar decipher-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "A" 'decipher-show-alphabet)
    (define-key map "C" 'decipher-complete-alphabet)
    (define-key map "D" 'decipher-digram-list)
    (define-key map "F" 'decipher-frequency-count)
    (define-key map "M" 'decipher-make-checkpoint)
    (define-key map "N" 'decipher-adjacency-list)
    (define-key map "R" 'decipher-restore-checkpoint)
    (define-key map "U" 'decipher-undo)
    (define-key map " " 'decipher-keypress)
    (define-key map [remap undo] 'decipher-undo)
    (define-key map [remap advertised-undo] 'decipher-undo)
    (let ((key ?a))
      (while (<= key ?z)
	(define-key map (vector key) 'decipher-keypress)
	(incf key)))
    map)
  "Keymap for Decipher mode.")


(defvar decipher-stats-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "D" 'decipher-digram-list)
    (define-key map "F" 'decipher-frequency-count)
    (define-key map "N" 'decipher-adjacency-list)
    map)
"Keymap for Decipher-Stats mode.")


(defvar decipher-mode-syntax-table nil
  "Decipher mode syntax table")

(if decipher-mode-syntax-table
    ()
  (let ((table (make-syntax-table))
        (c ?0))
    (while (<= c ?9)
      (modify-syntax-entry c "_" table) ;Digits are not part of words
      (incf c))
    (setq decipher-mode-syntax-table table)))

(defvar decipher-alphabet nil)
;; This is an alist containing entries (PLAIN-CHAR . CIPHER-CHAR),
;; where PLAIN-CHAR runs from ?a to ?z and CIPHER-CHAR is an uppercase
;; letter or space (which means no mapping is known for that letter).
;; This *must* contain entries for all lowercase characters.
(make-variable-buffer-local 'decipher-alphabet)

(defvar decipher-stats-buffer nil
  "The buffer which displays statistics for this ciphertext.
Do not access this variable directly, use the function
`decipher-stats-buffer' instead.")
(make-variable-buffer-local 'decipher-stats-buffer)

(defvar decipher-undo-list-size 0
  "The number of entries in the undo list.")
(make-variable-buffer-local 'decipher-undo-list-size)

(defvar decipher-undo-list nil
  "The undo list for this buffer.
Each element is either a cons cell (PLAIN-CHAR . CIPHER-CHAR) or a
list of such cons cells.")
(make-variable-buffer-local 'decipher-undo-list)

(defvar decipher-pending-undo-list nil)

;; The following variables are used by the analysis functions
;; and are defined here to avoid byte-compiler warnings.
;; Don't mess with them unless you know what you're doing.
(defvar decipher-char nil
  "See the functions decipher-loop-with-breaks and decipher-loop-no-breaks.")
(defvar decipher--prev-char)
(defvar decipher--digram)
(defvar decipher--digram-list)
(defvar decipher--before)
(defvar decipher--after)
(defvar decipher--freqs)

;;;===================================================================
;;; Code:
;;;===================================================================
;; Main entry points:
;;--------------------------------------------------------------------

;;;###autoload
(defun decipher ()
  "Format a buffer of ciphertext for cryptanalysis and enter Decipher mode."
  (interactive)
  ;; Make sure the buffer ends in a newline:
  (goto-char (point-max))
  (or (bolp)
      (insert "\n"))
  ;; See if it's already in decipher format:
  (goto-char (point-min))
  (if (looking-at "^(abcdefghijklmnopqrstuvwxyz   \
ABCDEFGHIJKLMNOPQRSTUVWXYZ   -\\*-decipher-\\*-\n)")
      (message "Buffer is already formatted, entering Decipher mode...")
    ;; Add the alphabet at the beginning of the file
    (insert "(abcdefghijklmnopqrstuvwxyz   \
ABCDEFGHIJKLMNOPQRSTUVWXYZ   -*-decipher-*-\n)\n\n")
    ;; Add lines for the solution:
    (let (begin)
      (while (not (eobp))
        (if (looking-at "^%")
            (forward-line)              ;Leave comments alone
          (delete-horizontal-space)
          (if (eolp)
              (forward-line)            ;Just leave blank lines alone
            (insert ":")                ;Mark ciphertext line
            (setq begin (point))
            (forward-line)
            (if decipher-force-uppercase
                (upcase-region begin (point))) ;Convert ciphertext to uppercase
            (insert ">\n")))))          ;Mark plaintext line
    (delete-blank-lines)                ;Remove any blank lines
    (delete-blank-lines))               ; at end of buffer
  (goto-char (point-min))
  (forward-line 3)
  (decipher-mode))

;;;###autoload
(defun decipher-mode ()
  "Major mode for decrypting monoalphabetic substitution ciphers.
Lower-case letters enter plaintext.
Upper-case letters are commands.

The buffer is made read-only so that normal Emacs commands cannot
modify it.

The most useful commands are:
\\<decipher-mode-map>
\\[decipher-digram-list]  Display a list of all digrams & their frequency
\\[decipher-frequency-count]  Display the frequency of each ciphertext letter
\\[decipher-adjacency-list]\
  Show adjacency list for current letter (lists letters appearing next to it)
\\[decipher-make-checkpoint]  Save the current cipher alphabet (checkpoint)
\\[decipher-restore-checkpoint]  Restore a saved cipher alphabet (checkpoint)"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-undo-list  t             ;Disable undo
        indent-tabs-mode  nil           ;Do not use tab characters
        major-mode       'decipher-mode
        mode-name        "Decipher")
  (if decipher-force-uppercase
      (setq case-fold-search nil))      ;Case is significant when searching
  (use-local-map decipher-mode-map)
  (set-syntax-table decipher-mode-syntax-table)
  (unless (= (point-min) (point-max))
    (decipher-read-alphabet))
  (set (make-local-variable 'font-lock-defaults)
       '(decipher-font-lock-keywords t))
  ;; Make the buffer writable when we exit Decipher mode:
  (add-hook 'change-major-mode-hook
            (lambda () (setq buffer-read-only nil
                             buffer-undo-list nil))
            nil t)
  (run-mode-hooks 'decipher-mode-hook)
  (setq buffer-read-only t))
(put 'decipher-mode 'mode-class 'special)

;;--------------------------------------------------------------------
;; Normal key handling:
;;--------------------------------------------------------------------

(defmacro decipher-last-command-char ()
  ;; Return the char which ran this command (for compatibility with XEmacs)
  (if (fboundp 'event-to-character)
      '(event-to-character last-command-event)
    'last-command-event))

(defun decipher-keypress ()
  "Enter a plaintext or ciphertext character."
  (interactive)
  (let ((decipher-function 'decipher-set-map)
        buffer-read-only)               ;Make buffer writable
    (save-excursion
      (or (save-excursion
            (beginning-of-line)
            (let ((first-char (following-char)))
              (cond
               ((= ?: first-char)
                t)
               ((= ?> first-char)
                nil)
               ((= ?\( first-char)
                (setq decipher-function 'decipher-alphabet-keypress)
                t)
               ((= ?\) first-char)
                (setq decipher-function 'decipher-alphabet-keypress)
                nil)
               (t
                (error "Bad location")))))
          (let (goal-column)
            (forward-line -1)))
      (let ((char-a (following-char))
            (char-b (decipher-last-command-char)))
        (or (and (not (= ?w (char-syntax char-a)))
                 (= char-b ?\s)) ;Spacebar just advances on non-letters
            (funcall decipher-function char-a char-b)))))
  (forward-char))

(defun decipher-alphabet-keypress (a b)
  ;; Handle keypresses in the alphabet lines.
  ;; A is the character in the alphabet row (which starts with '(')
  ;; B is the character pressed
  (cond ((and (>= a ?A) (<= a ?Z))
         ;; If A is uppercase, then it is in the ciphertext alphabet:
         (decipher-set-map a b))
        ((and (>= a ?a) (<= a ?z))
         ;; If A is lowercase, then it is in the plaintext alphabet:
         (if (= b ?\s)
             ;; We are clearing the association (if any):
             (if (/= ?\s (setq b (cdr (assoc a decipher-alphabet))))
                 (decipher-set-map b ?\s))
           ;; Associate the plaintext char with the char pressed:
           (decipher-set-map b a)))
        (t
         ;; If A is not a letter, that's a problem:
         (error "Bad character"))))

;;--------------------------------------------------------------------
;; Undo:
;;--------------------------------------------------------------------

(defun decipher-undo ()
  "Undo a change in Decipher mode."
  (interactive)
  ;; If we don't get all the way thru, make last-command indicate that
  ;; for the following command.
  (setq this-command t)
  (or (eq major-mode 'decipher-mode)
      (error "This buffer is not in Decipher mode"))
  (or (eq last-command 'decipher-undo)
      (setq decipher-pending-undo-list decipher-undo-list))
  (or decipher-pending-undo-list
      (error "No further undo information"))
  (let ((undo-rec (pop decipher-pending-undo-list))
        buffer-read-only                ;Make buffer writable
        redo-map redo-rec undo-map)
    (or (consp (car undo-rec))
        (setq undo-rec (list undo-rec)))
    (while (setq undo-map (pop undo-rec))
      (setq redo-map (decipher-get-undo (cdr undo-map) (car undo-map)))
      (if redo-map
          (setq redo-rec
                (if (consp (car redo-map))
                    (append redo-map redo-rec)
                  (cons redo-map redo-rec))))
      (decipher-set-map (cdr undo-map) (car undo-map) t))
    (decipher-add-undo redo-rec))
  (setq this-command 'decipher-undo)
  (message "Undo!"))

(defun decipher-add-undo (undo-rec)
  "Add UNDO-REC to the undo list."
  (if undo-rec
      (progn
        (push undo-rec decipher-undo-list)
        (incf decipher-undo-list-size)
        (if (> decipher-undo-list-size decipher-undo-limit)
            (let ((new-size (- decipher-undo-limit 100)))
              ;; Truncate undo list to NEW-SIZE elements:
              (setcdr (nthcdr (1- new-size) decipher-undo-list) nil)
              (setq decipher-undo-list-size new-size))))))

(defun decipher-copy-cons (cons)
  (if cons
      (cons (car cons) (cdr cons))))

(defun decipher-get-undo (cipher-char plain-char)
  ;; Return an undo record that will undo the result of
  ;;   (decipher-set-map CIPHER-CHAR PLAIN-CHAR)
  ;; We must copy the cons cell because the original cons cells will be
  ;; modified using setcdr.
  (let ((cipher-map (decipher-copy-cons (rassoc cipher-char decipher-alphabet)))
        (plain-map  (decipher-copy-cons (assoc  plain-char  decipher-alphabet))))
    (cond ((equal ?\s plain-char)
           cipher-map)
          ((equal cipher-char (cdr plain-map))
           nil)                         ;We aren't changing anything
          ((equal ?\s (cdr plain-map))
           (or cipher-map (cons ?\s cipher-char)))
          (cipher-map
           (list plain-map cipher-map))
          (t
           plain-map))))

;;--------------------------------------------------------------------
;; Mapping ciphertext and plaintext:
;;--------------------------------------------------------------------

(defun decipher-set-map (cipher-char plain-char &optional no-undo)
  ;; Associate a ciphertext letter with a plaintext letter
  ;; CIPHER-CHAR must be an uppercase or lowercase letter
  ;; PLAIN-CHAR must be a lowercase letter (or a space)
  ;; NO-UNDO if non-nil means do not record undo information
  ;; Any existing associations for CIPHER-CHAR or PLAIN-CHAR will be erased.
  (setq cipher-char (upcase cipher-char))
  (or (and (>= cipher-char ?A) (<= cipher-char ?Z))
      (error "Bad character"))          ;Cipher char must be uppercase letter
  (or no-undo
      (decipher-add-undo (decipher-get-undo cipher-char plain-char)))
  (let ((cipher-string (char-to-string cipher-char))
        (plain-string  (char-to-string plain-char))
        case-fold-search                ;Case is significant
        mapping bound)
    (save-excursion
      (goto-char (point-min))
      (if (setq mapping (rassoc cipher-char decipher-alphabet))
          (progn
            (setcdr mapping ?\s)
            (search-forward-regexp (concat "^([a-z]*"
                                           (char-to-string (car mapping))))
            (decipher-insert ?\s)
            (beginning-of-line)))
      (if (setq mapping (assoc plain-char decipher-alphabet))
          (progn
            (if (/= ?\s (cdr mapping))
                (decipher-set-map (cdr mapping) ?\s t))
            (setcdr mapping cipher-char)
            (search-forward-regexp (concat "^([a-z]*" plain-string))
            (decipher-insert cipher-char)
            (beginning-of-line)))
      (search-forward-regexp (concat "^([a-z]+   [A-Z]*" cipher-string))
      (decipher-insert plain-char)
      (setq case-fold-search t          ;Case is not significant
            cipher-string    (downcase cipher-string))
      (let ((font-lock-fontify-region-function 'ignore))
        ;; insert-and-inherit will pick the right face automatically
        (while (search-forward-regexp "^:" nil t)
          (setq bound (point-at-eol))
          (while (search-forward cipher-string bound 'end)
            (decipher-insert plain-char)))))))

(defun decipher-insert (char)
  ;; Insert CHAR in the row below point.  It replaces any existing
  ;; character in that position.
  (let ((col (1- (current-column))))
    (save-excursion
      (forward-line)
      (or (= ?\> (following-char))
          (= ?\) (following-char))
          (error "Bad location"))
      (move-to-column col t)
      (or (eolp)
          (delete-char 1))
      (insert-and-inherit char))))

;;--------------------------------------------------------------------
;; Checkpoints:
;;--------------------------------------------------------------------
;; A checkpoint is a comment of the form:
;;   %!ABCDEFGHIJKLMNOPQRSTUVWXYZ! Description
;; Such comments are usually placed at the end of the buffer following
;; this header (which is inserted by decipher-make-checkpoint):
;;   %---------------------------
;;   % Checkpoints:
;;   % abcdefghijklmnopqrstuvwxyz
;; but this is not required; checkpoints can be placed anywhere.
;;
;; The description is optional; all that is required is the alphabet.

(defun decipher-make-checkpoint (desc)
  "Checkpoint the current cipher alphabet.
This records the current alphabet so you can return to it later.
You may have any number of checkpoints.
Type `\\[decipher-restore-checkpoint]' to restore a checkpoint."
  (interactive "sCheckpoint description: ")
  (or (stringp desc)
      (setq desc ""))
  (let (alphabet
        buffer-read-only)               ;Make buffer writable
    (goto-char (point-min))
    (re-search-forward "^)")
    (move-to-column 27 t)
    (setq alphabet (buffer-substring-no-properties (- (point) 26) (point)))
    (if (re-search-forward "^%![A-Z ]+!" nil 'end)
       nil ; Add new checkpoint with others
      (if (re-search-backward "^% *Local Variables:" nil t)
          ;; Add checkpoints before local variables list:
          (progn (forward-line -1)
                 (or (looking-at "^ *$")
                     (progn (forward-line) (insert ?\n) (forward-line -1)))))
      (insert "\n%" (make-string 69 ?\-)
              "\n% Checkpoints:\n% abcdefghijklmnopqrstuvwxyz\n"))
    (beginning-of-line)
    (insert "%!" alphabet "! " desc ?\n)))

(defun decipher-restore-checkpoint ()
  "Restore the cipher alphabet from a checkpoint.
If point is not on a checkpoint line, moves to the first checkpoint line.
If point is on a checkpoint, restores that checkpoint.

Type `\\[decipher-make-checkpoint]' to make a checkpoint."
  (interactive)
  (beginning-of-line)
  (if (looking-at "%!\\([A-Z ]+\\)!")
      ;; Restore this checkpoint:
      (let ((alphabet (match-string 1))
            buffer-read-only)           ;Make buffer writable
        (goto-char (point-min))
        (re-search-forward "^)")
        (or (eolp)
            (delete-region (point) (progn (end-of-line) (point))))
        (insert alphabet)
        (decipher-resync))
    ;; Move to the first checkpoint:
    (goto-char (point-min))
    (if (re-search-forward "^%![A-Z ]+!" nil t)
        (message "Select the checkpoint to restore and type `%s'"
                 (substitute-command-keys "\\[decipher-restore-checkpoint]"))
      (error "No checkpoints in this buffer"))))

;;--------------------------------------------------------------------
;; Miscellaneous commands:
;;--------------------------------------------------------------------

(defun decipher-complete-alphabet ()
  "Complete the cipher alphabet.
This fills any blanks in the cipher alphabet with the unused letters
in alphabetical order.  Use this when you have a keyword cipher and
you have determined the keyword."
  (interactive)
  (let ((cipher-char ?A)
        (ptr decipher-alphabet)
        buffer-read-only                ;Make buffer writable
        plain-map undo-rec)
    (while (setq plain-map (pop ptr))
      (if (equal ?\s (cdr plain-map))
          (progn
            (while (rassoc cipher-char decipher-alphabet)
              ;; Find the next unused letter
              (incf cipher-char))
            (push (cons ?\s cipher-char) undo-rec)
            (decipher-set-map cipher-char (car plain-map) t))))
    (decipher-add-undo undo-rec)))

(defun decipher-show-alphabet ()
  "Display the current cipher alphabet in the message line."
  (interactive)
  (message "%s"
   (mapconcat (lambda (a)
                (concat
                 (char-to-string (car a))
                 (char-to-string (cdr a))))
              decipher-alphabet
              "")))

(defun decipher-resync ()
  "Reprocess the buffer using the alphabet from the top.
This regenerates all deciphered plaintext and clears the undo list.
You should use this if you edit the ciphertext."
  (interactive)
  (message "Reprocessing buffer...")
  (let (alphabet
        buffer-read-only                ;Make buffer writable
        mapping)
    (save-excursion
      (decipher-read-alphabet)
      (setq alphabet decipher-alphabet)
      (goto-char (point-min))
      (and (re-search-forward "^).+" nil t)
           (replace-match ")" nil nil))
      (while (re-search-forward "^>.+" nil t)
        (replace-match ">" nil nil))
      (decipher-read-alphabet)
      (while (setq mapping (pop alphabet))
        (or (equal ?\s (cdr mapping))
            (decipher-set-map (cdr mapping) (car mapping))))))
  (setq decipher-undo-list       nil
        decipher-undo-list-size  0)
  (message "Reprocessing buffer...done"))

;;--------------------------------------------------------------------
;; Miscellaneous functions:
;;--------------------------------------------------------------------

(defun decipher-read-alphabet ()
  "Build the decipher-alphabet from the alphabet line in the buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward-regexp "^)")
    (move-to-column 27 t)
    (setq decipher-alphabet nil)
    (let ((plain-char ?z))
      (while (>= plain-char ?a)
        (backward-char)
        (push (cons plain-char (following-char)) decipher-alphabet)
        (decf plain-char)))))

;;;===================================================================
;;; Analyzing ciphertext:
;;;===================================================================

(defun decipher-frequency-count ()
  "Display the frequency count in the statistics buffer."
  (interactive)
  (decipher-analyze)
  (decipher-display-regexp "^A" "^[A-Z][A-Z]"))

(defun decipher-digram-list ()
  "Display the list of digrams in the statistics buffer."
  (interactive)
  (decipher-analyze)
  (decipher-display-regexp "[A-Z][A-Z] +[0-9]" "^$"))

(defun decipher-adjacency-list (cipher-char)
  "Display the adjacency list for the letter at point.
The adjacency list shows all letters which come next to CIPHER-CHAR.

An adjacency list (for the letter X) looks like this:
       1 1         1     1   1       3 2 1             3   8
X: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z *  11   14   9%
     1 1                 1       2   1   1     2       5   7
This says that X comes before D once, and after B once.  X begins 5
words, and ends 3 words (`*' represents a space).  X comes before 8
different letters, after 7 different letters, and is next to a total
of 11 different letters.  It occurs 14 times, making up 9% of the
ciphertext."
  (interactive (list (upcase (following-char))))
  (decipher-analyze)
  (let (start end)
    (with-current-buffer (decipher-stats-buffer)
      (goto-char (point-min))
      (or (re-search-forward (format "^%c: " cipher-char) nil t)
          (error "Character `%c' is not used in ciphertext" cipher-char))
      (forward-line -1)
      (setq start (point))
      (forward-line 3)
      (setq end (point)))
    (decipher-display-range start end)))

;;--------------------------------------------------------------------
(defun decipher-analyze ()
  "Perform frequency analysis on the current buffer if necessary."
  (cond
   ;; If this is the statistics buffer, do nothing:
   ((eq major-mode 'decipher-stats-mode))
   ;; If this is the Decipher buffer, see if the stats buffer exists:
   ((eq major-mode 'decipher-mode)
    (or (and (bufferp decipher-stats-buffer)
             (buffer-name decipher-stats-buffer))
        (decipher-analyze-buffer)))
   ;; Otherwise:
   (t (error "This buffer is not in Decipher mode"))))

;;--------------------------------------------------------------------
(defun decipher-display-range (start end)
  "Display text between START and END in the statistics buffer.
START and END are positions in the statistics buffer.  Makes the
statistics buffer visible and sizes the window to just fit the
displayed text, but leaves the current window selected."
  (let ((stats-buffer (decipher-stats-buffer))
        (current-window (selected-window))
        (pop-up-windows t))
    (or (eq (current-buffer) stats-buffer)
        (pop-to-buffer stats-buffer))
    (goto-char start)
    (or (one-window-p t)
        (enlarge-window (- (1+ (count-lines start end)) (window-height))))
    (recenter 0)
    (select-window current-window)))

(defun decipher-display-regexp (start-regexp end-regexp)
  "Display text between two regexps in the statistics buffer.

START-REGEXP matches the first line to display.
END-REGEXP matches the line after that which ends the display.
The ending line is included in the display unless it is blank."
  (let (start end)
    (with-current-buffer (decipher-stats-buffer)
      (goto-char (point-min))
      (re-search-forward start-regexp)
      (beginning-of-line)
      (setq start (point))
      (re-search-forward end-regexp)
      (beginning-of-line)
      (or (looking-at "^ *$")
          (forward-line 1))
      (setq end (point)))
    (decipher-display-range start end)))

;;--------------------------------------------------------------------
(defun decipher-loop-with-breaks (func)
  "Loop through ciphertext, calling FUNC once for each letter & word division.

FUNC is called with no arguments, and its return value is unimportant.
It may examine `decipher-char' to see the current ciphertext
character.  `decipher-char' contains either an uppercase letter or a space.

FUNC is called exactly once between words, with `decipher-char' set to
a space.

See `decipher-loop-no-breaks' if you do not care about word divisions."
  (let ((decipher-char ?\s)
        (decipher--loop-prev-char ?\s))
    (save-excursion
      (goto-char (point-min))
      (funcall func)              ;Space marks beginning of first word
      (while (search-forward-regexp "^:" nil t)
        (while (not (eolp))
          (setq decipher-char (upcase (following-char)))
          (or (and (>= decipher-char ?A) (<= decipher-char ?Z))
              (setq decipher-char ?\s))
          (or (and (equal decipher-char ?\s)
                   (equal decipher--loop-prev-char ?\s))
              (funcall func))
          (setq decipher--loop-prev-char decipher-char)
          (forward-char))
        (or (equal decipher-char ?\s)
            (progn
              (setq decipher-char ?\s
                    decipher--loop-prev-char ?\s)
              (funcall func)))))))

(defun decipher-loop-no-breaks (func)
  "Loop through ciphertext, calling FUNC once for each letter.

FUNC is called with no arguments, and its return value is unimportant.
It may examine `decipher-char' to see the current ciphertext letter.
`decipher-char' contains an uppercase letter.

Punctuation and spacing in the ciphertext are ignored.
See `decipher-loop-with-breaks' if you care about word divisions."
  (let (decipher-char)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^:" nil t)
        (while (not (eolp))
          (setq decipher-char (upcase (following-char)))
          (and (>= decipher-char ?A)
               (<= decipher-char ?Z)
               (funcall func))
          (forward-char))))))

;;--------------------------------------------------------------------
;; Perform the analysis:
;;--------------------------------------------------------------------

(defun decipher-insert-frequency-counts (freq-list total)
  "Insert frequency counts in current buffer.
Each element of FREQ-LIST is a list (LETTER FREQ ...).
TOTAL is the total number of letters in the ciphertext."
  (let ((i 4) temp-list)
    (while (> i 0)
      (setq temp-list freq-list)
      (while temp-list
        (insert (caar temp-list)
                (format "%4d%3d%%  "
                        (cadar temp-list)
                        (/ (* 100 (cadar temp-list)) total)))
        (setq temp-list (nthcdr 4 temp-list)))
      (insert ?\n)
      (setq freq-list (cdr freq-list)
            i         (1- i)))))

(defun decipher--analyze ()
  ;; Perform frequency analysis on ciphertext.
  ;;
  ;; This function is called repeatedly with decipher-char set to each
  ;; character of ciphertext.  It uses decipher--prev-char to remember
  ;; the previous ciphertext character.
  ;;
  ;; It builds several data structures, which must be initialized
  ;; before the first call to decipher--analyze.  The arrays are
  ;; indexed with A = 0, B = 1, ..., Z = 25, SPC = 26 (if used).
  ;;   decipher--after: (initialize to zeros)
  ;;     A vector of 26 vectors of 27 integers.  The first vector
  ;;     represents the number of times A follows each character, the
  ;;     second vector represents B, and so on.
  ;;   decipher--before: (initialize to zeros)
  ;;     The same as decipher--after, but representing the number of
  ;;     times the character precedes each other character.
  ;;   decipher--digram-list: (initialize to nil)
  ;;     An alist with an entry for each digram (2-character sequence)
  ;;     encountered.  Each element is a cons cell (DIGRAM . FREQ),
  ;;     where DIGRAM is a 2 character string and FREQ is the number
  ;;     of times it occurs.
  ;;   decipher--freqs: (initialize to zeros)
  ;;     A vector of 26 integers, counting the number of occurrences
  ;;     of the corresponding characters.
  (setq decipher--digram (format "%c%c" decipher--prev-char decipher-char))
  (incf (cdr (or (assoc decipher--digram decipher--digram-list)
                 (car (push (cons decipher--digram 0)
                            decipher--digram-list)))))
  (and (>= decipher--prev-char ?A)
       (incf (aref (aref decipher--before (- decipher--prev-char ?A))
                   (if (equal decipher-char ?\s)
                       26
                     (- decipher-char ?A)))))
  (and (>= decipher-char ?A)
       (incf (aref decipher--freqs (- decipher-char ?A)))
       (incf (aref (aref decipher--after (- decipher-char ?A))
                   (if (equal decipher--prev-char ?\s)
                       26
                     (- decipher--prev-char ?A)))))
  (setq decipher--prev-char decipher-char))

(defun decipher--digram-counts (counts)
  "Generate the counts for an adjacency list."
  (let ((total 0))
    (concat
     (mapconcat (lambda (x)
                  (cond ((> x 99) (incf total) "XX")
                        ((> x 0)  (incf total) (format "%2d" x))
                        (t        "  ")))
                counts
                "")
     (format "%4d" (if (> (aref counts 26) 0)
                       (1- total)    ;Don't count space
                     total)))))

(defun decipher--digram-total (before-count after-count)
  "Count the number of different letters a letter appears next to."
  ;; We do not include spaces (word divisions) in this count.
  (let ((total 0)
        (i 26))
    (while (>= (decf i) 0)
      (if (or (> (aref before-count i) 0)
              (> (aref after-count  i) 0))
          (incf total)))
    total))

(defun decipher-analyze-buffer ()
  "Perform frequency analysis and store results in statistics buffer.
Creates the statistics buffer if it doesn't exist."
  (let ((decipher--prev-char (if decipher-ignore-spaces ?\s ?\*))
        (decipher--before (make-vector 26 nil))
        (decipher--after  (make-vector 26 nil))
        (decipher--freqs   (make-vector 26 0))
        (total-chars  0)
        decipher--digram decipher--digram-list freq-list)
    (message "Scanning buffer...")
    (let ((i 26))
      (while (>= (decf i) 0)
        (aset decipher--before i (make-vector 27 0))
        (aset decipher--after  i (make-vector 27 0))))
    (if decipher-ignore-spaces
        (progn
          (decipher-loop-no-breaks 'decipher--analyze)
          ;; The first character of ciphertext was marked as following a space:
          (let ((i 26))
            (while (>= (decf i) 0)
              (aset (aref decipher--after  i) 26 0))))
      (decipher-loop-with-breaks 'decipher--analyze))
    (message "Processing results...")
    (setcdr (last decipher--digram-list 2) nil)   ;Delete the phony "* " digram
    ;; Sort the digram list by frequency and alphabetical order:
    (setq decipher--digram-list (sort (sort decipher--digram-list
                                  (lambda (a b) (string< (car a) (car b))))
                            (lambda (a b) (> (cdr a) (cdr b)))))
    ;; Generate the frequency list:
    ;;   Each element is a list of 3 elements (LETTER FREQ DIFFERENT),
    ;;   where LETTER is the ciphertext character, FREQ is the number
    ;;   of times it occurs, and DIFFERENT is the number of different
    ;;   letters it appears next to.
    (let ((i 26))
      (while (>= (decf i) 0)
        (setq freq-list
              (cons (list (+ i ?A)
                          (aref decipher--freqs i)
                          (decipher--digram-total (aref decipher--before i)
                                                  (aref decipher--after  i)))
                    freq-list)
              total-chars (+ total-chars (aref decipher--freqs i)))))
    ;; Switch to statistics buffer, creating it if necessary:
    (with-current-buffer (decipher-stats-buffer t)
      ;; This can't happen, but it never hurts to double-check:
      (or (eq major-mode 'decipher-stats-mode)
          (error "Buffer %s is not in Decipher-Stats mode" (buffer-name)))
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; Display frequency counts for letters A-Z:
      (decipher-insert-frequency-counts freq-list total-chars)
      (insert ?\n)
      ;; Display frequency counts for letters in order of frequency:
      (setq freq-list (sort freq-list
                            (lambda (a b) (> (second a) (second b)))))
      (decipher-insert-frequency-counts freq-list total-chars)
      ;; Display letters in order of frequency:
      (insert ?\n (mapconcat (lambda (a) (char-to-string (car a)))
                             freq-list nil)
              "\n\n")
      ;; Display list of digrams in order of frequency:
      (let* ((rows (floor (+ (length decipher--digram-list) 9) 10))
             (i rows)
             temp-list)
        (while (> i 0)
          (setq temp-list decipher--digram-list)
          (while temp-list
            (insert (caar temp-list)
                    (format "%3d   "
                            (cdar temp-list)))
            (setq temp-list (nthcdr rows temp-list)))
          (delete-horizontal-space)
          (insert ?\n)
          (setq decipher--digram-list (cdr decipher--digram-list)
                i           (1- i))))
      ;; Display adjacency list for each letter, sorted in descending
      ;; order of the number of adjacent letters:
      (setq freq-list (sort freq-list
                            (lambda (a b) (> (third a) (third b)))))
      (let ((temp-list freq-list)
            entry i)
        (while (setq entry (pop temp-list))
          (if (equal 0 (second entry))
              nil                       ;This letter was not used
            (setq i (- (car entry) ?A))
            (insert ?\n "  "
                    (decipher--digram-counts (aref decipher--before i)) ?\n
                    (car entry)
                    ": A B C D E F G H I J K L M N O P Q R S T U V W X Y Z *"
                    (format "%4d %4d %3d%%\n  "
                            (third entry) (second entry)
                            (/ (* 100 (second entry)) total-chars))
                    (decipher--digram-counts (aref decipher--after  i)) ?\n))))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      ))
  (message nil))

;;====================================================================
;; Statistics Buffer:
;;====================================================================

(defun decipher-stats-mode ()
  "Major mode for displaying ciphertext statistics."
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only  t
        buffer-undo-list  t             ;Disable undo
        case-fold-search  nil           ;Case is significant when searching
        indent-tabs-mode  nil           ;Do not use tab characters
        major-mode       'decipher-stats-mode
        mode-name        "Decipher-Stats")
  (use-local-map decipher-stats-mode-map)
  (run-mode-hooks 'decipher-stats-mode-hook))
(put 'decipher-stats-mode 'mode-class 'special)

;;--------------------------------------------------------------------

(defun decipher-display-stats-buffer ()
  "Make the statistics buffer visible, but do not select it."
  (let ((stats-buffer (decipher-stats-buffer))
        (current-window (selected-window)))
    (or (eq (current-buffer) stats-buffer)
        (progn
          (pop-to-buffer stats-buffer)
          (select-window current-window)))))

(defun decipher-stats-buffer (&optional create)
  "Return the buffer used for decipher statistics.
If CREATE is non-nil, create the buffer if it doesn't exist.
This is guaranteed to return a buffer in Decipher-Stats mode;
if it can't, it signals an error."
  (cond
   ;; We may already be in the statistics buffer:
   ((eq major-mode 'decipher-stats-mode)
    (current-buffer))
   ;; See if decipher-stats-buffer exists:
   ((and (bufferp decipher-stats-buffer)
         (buffer-name decipher-stats-buffer))
    (or (with-current-buffer decipher-stats-buffer
          (eq major-mode 'decipher-stats-mode))
        (error "Buffer %s is not in Decipher-Stats mode"
               (buffer-name decipher-stats-buffer)))
    decipher-stats-buffer)
   ;; Create a new buffer if requested:
   (create
    (let ((stats-name (concat "*" (buffer-name) "*")))
      (setq decipher-stats-buffer
            (if (eq 'decipher-stats-mode
                    (cdr-safe (assoc 'major-mode
                                     (buffer-local-variables
                                      (get-buffer stats-name)))))
                ;; We just lost track of the statistics buffer:
                (get-buffer stats-name)
              (generate-new-buffer stats-name))))
    (with-current-buffer decipher-stats-buffer
      (decipher-stats-mode))
    decipher-stats-buffer)
   ;; Give up:
   (t (error "No statistics buffer"))))

;;====================================================================

(provide 'decipher)

;;(defun decipher-show-undo-list ()
;;  "Display the undo list (for debugging purposes)."
;;  (interactive)
;;  (with-output-to-temp-buffer "*Decipher Undo*"
;;    (let ((undo-list decipher-undo-list)
;;          undo-rec undo-map)
;;      (with-current-buffer "*Decipher Undo*"
;;        (while (setq undo-rec (pop undo-list))
;;          (or (consp (car undo-rec))
;;              (setq undo-rec (list undo-rec)))
;;          (insert ?\()
;;          (while (setq undo-map (pop undo-rec))
;;            (insert (cdr undo-map) (car undo-map) ?\s))
;;          (delete-char -1)
;;          (insert ")\n"))))))

;;; decipher.el ends here
