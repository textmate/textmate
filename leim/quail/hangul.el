;;; hangul.el --- Korean Hangul input method

;; Copyright (C) 2008-2012  Free Software Foundation, Inc.

;; Author: Jihyun Cho <jihyun.jo@gmail.com>
;; Keywords: multilingual, input method, Korean, Hangul

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

;; This file is to implement the following hangul automata:
;; - Hangul 2-Bulsik input method
;; - Hangul 3-Bulsik final input method
;; - Hangul 3-Bulsik 390 input method

;;; Code:

(require 'quail)
(eval-when-compile (require 'cl))       ; for setf
(require 'hanja-util)

;; Hangul double Jamo table.
;; The format is an alist of JAMO-TYPE vs. DOUBLE-JAMO-TABLE.
;;
;; JAMO-TYPE is a symbol `cho' for Choseong, `jung' for Jungseong, and
;; `jong' for Jongseong.
;;
;; DOUBLE-JAMO-TABLE is an alist of Jamo index vs. the vector of Jamo
;; indies that can be combined with the car part.
;;
;; Jamo index is a relative index in `hangul Compatibility Jamo' area
;; of the Unicode (i.e. 1 for U+3131).

(defconst hangul-djamo-table
  '((cho . ((1 . [1])                   ; Choseong
            (7 . [7])
            (18 . [18])
            (21 . [21])
            (24 . [24])))
    (jung . ((39 . [31 32 51])          ; Jungseong
             (44 . [35 36 51])
             (49 . [51])))
    (jong . ((1 . [1 21])               ; Jongseong
             (4 . [24 30])
             (9 . [1 17 18 21 28 29 30])
             (18 . [18 21])
             (21 . [21])))))

;; Hangul 2-Bulsik keymap.
;; It converts an ASCII code A-Z, a-z, to the corresponding hangul
;; Jamo index.

(defconst hangul2-keymap
  [17 48 26 23 7 9 30 39 33 35 31 51 49 44 32 36 18 1 4 21 37 29 24 28 43 27])

;; Hangul 3-Bulsik final keymap.  3-Bulsik use full keyboard layout.
;; Therefore, we must map all printable ASCII codes (`!' to `~')
;; to Hangul 3-Bulsik codes.
;; Other parts are the same as `hangul2-keymap'.
(defconst hangul3-keymap
  [2 183 24 15 14 8220 120 39 126 8221 43 44 41 46 74 119 30 22 18 78 83
     68 73 85 79 52 110 44 62 46 33 10 7 63 27 12 5 11 69 48 55 49 50 51
     34 45 56 57 29 16 6 13 54 3 28 20 53 26 40 58 60 61 59 42 23 79 71
     86 72 66 84 96 109 115 93 116 122 113 118 121 21 67 4 70 99 74 9 1
     101 17 37 92 47 8251])

;; Hangul 3-Bulsik 390 keymap.
;; The role is the same as `hangul3-keymap'.
(defconst hangul390-keymap
  [24 34 35 36 37 38 120 40 41 42 43 44 45 46 73 119 30 22 18 77 82 67 72
      84 78 58 110 50 61 51 63 64 7 33 11 10 27 2 47 39 56 52 53 54 49 48
      57 62 29 68 6 59 55 16 28 20 60 26 91 92 93 94 95 96 23 78 70 85 71
      65 83 90 109 115 87 116 122 113 118 121 21 66 4 69 99 73 9 1 101 17
      123 124 125 126])

(defvar hangul-im-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\d" 'hangul-delete-backward-char)
    (define-key map [f9] 'hangul-to-hanja-conversion)
    (define-key map [Hangul_Hanja] 'hangul-to-hanja-conversion)
    map)
  "Keymap for Hangul method.  It is used by all Hangul input methods.")

;; Current input character buffer. Store separated hangul character.
;; The first and second are Choseong position.
;; The third and forth are Jungseong position.
;; The fifth and sixth are Jongseong position.
;; The second, forth and sixth are double Jamo position.
(defvar hangul-queue
  (make-vector 6 0))

(defsubst notzerop (number)
  (not (zerop number)))

(defsubst alphabetp (char)
  (or (and (>= char ?A) (<= char ?Z))
      (and (>= char ?a) (<= char ?z))))

(defun hangul-character (cho jung jong)
  "Convert CHO, JUNG, JONG to the precomposed `Hangul Syllables' character.
CHO, JUNG, JONG are relative indices in `Hangul Compatibility Jamo' of Unicode.
Return a zero-length string if the conversion fails."
  (or
   (decode-char
    'ucs
    (if (and (/= cho 0) (/= jung 0))
        (+ #xac00
           (* 588
              (- cho
                 (cond ((< cho 3) 1)
                       ((< cho 5) 2)
                       ((< cho 10) 4)
                       ((< cho 20) 11)
                       (t 12))))
           (* 28 (- jung 31))
           (- jong
              (cond ((< jong 8) 0)
                    ((< jong 19) 1)
                    ((< jong 25) 2)
                    (t 3))))
      (+ #x3130
	 (cond ((/= cho 0) cho)
	       ((/= jung 0) jung)
	       ((/= jong 0) jong)))))
   ""))

(defun hangul-insert-character (&rest queues)
  "Insert characters generated from QUEUES.
Each queue has the same form as `hangul-queue'.
Setup `quail-overlay' to the last character."
  (if (and mark-active transient-mark-mode)
      (progn
        (delete-region (region-beginning) (region-end))
        (deactivate-mark)))
  (quail-delete-region)
  (let ((first (car queues)))
    (insert
     (hangul-character
      (+ (aref first 0) (hangul-djamo 'cho (aref first 0) (aref first 1)))
      (+ (aref first 2) (hangul-djamo 'jung (aref first 2) (aref first 3)))
      (+ (aref first 4) (hangul-djamo 'jong (aref first 4) (aref first 5))))))
  (move-overlay quail-overlay (overlay-start quail-overlay) (point))
  (dolist (queue (cdr queues))
    (insert
     (hangul-character
      (+ (aref queue 0) (hangul-djamo 'cho (aref queue 0) (aref queue 1)))
      (+ (aref queue 2) (hangul-djamo 'jung (aref queue 2) (aref queue 3)))
      (+ (aref queue 4) (hangul-djamo 'jong (aref queue 4) (aref queue 5)))))
    (move-overlay quail-overlay (1+ (overlay-start quail-overlay)) (point))))

(defun hangul-djamo (jamo char1 char2)
  "Return the double Jamo index calculated from the arguments.
JAMO is a type of Hangul Jamo; `cho', `jung', or `jong'.
CHAR1 and CHAR2 are Hangul Jamo indices.
Return nil if CHAR1 and CHAR2 can not be combined."
  (let* ((jamo (cdr (assoc jamo hangul-djamo-table)))
         (char1 (cdr (assoc char1 jamo))))
    (if char1
        (let ((i (length char1)))
          (or (catch 'found
                (while (> i 0)
                  (if (= char2 (aref char1 (1- i)))
                      (throw 'found i))
                  (setf i (1- i))))
              0))
      0)))

(defsubst hangul2-input-method-jaum (char)
  "Store Hangul Jamo indice CHAR in `hangul-queue'.
It is a Hangul 2-Bulsik Jaum.
This function processes a Hangul 2-Bulsik Jaum.
The Hangul 2-Bulsik is composed of a Jaum and a Moum.
The Jaum can be located in a Choseong position and a Jongseong position.
Unless the function inserts CHAR to `hangul-queue',
commit current `hangul-queue' and then set a new `hangul-queue',
and insert CHAR to new `hangul-queue'."
  (if (cond ((zerop (aref hangul-queue 0))
             (aset hangul-queue 0 char))
            ((and (zerop (aref hangul-queue 1))
                  (zerop (aref hangul-queue 2))
                  (notzerop (hangul-djamo 'cho (aref hangul-queue 0) char)))
             (aset hangul-queue 1 char))
            ((and (zerop (aref hangul-queue 4))
                  (notzerop (aref hangul-queue 2))
                  (/= char 8)
                  (/= char 19)
                  (/= char 25)
                  (numberp
                   (hangul-character
                    (+ (aref hangul-queue 0)
                       (hangul-djamo
                        'cho
                        (aref hangul-queue 0)
                        (aref hangul-queue 1)))
                    (+ (aref hangul-queue 2)
                       (hangul-djamo
                        'jung
                        (aref hangul-queue 2)
                        (aref hangul-queue 3)))
                    char)))
             (aset hangul-queue 4 char))
            ((and (zerop (aref hangul-queue 5))
                  (notzerop (hangul-djamo 'jong (aref hangul-queue 4) char))
                  (numberp
                   (hangul-character
                    (+ (aref hangul-queue 0)
                       (hangul-djamo
                        'cho
                        (aref hangul-queue 0)
                        (aref hangul-queue 1)))
                    (+ (aref hangul-queue 2)
                       (hangul-djamo
                        'jung
                        (aref hangul-queue 2)
                        (aref hangul-queue 3)))
                    (+ (aref hangul-queue 4)
                       (hangul-djamo
                        'jong
                        (aref hangul-queue 4)
                        char)))))
             (aset hangul-queue 5 char)))
      (hangul-insert-character hangul-queue)
    (hangul-insert-character hangul-queue
			     (setq hangul-queue (vector char 0 0 0 0 0)))))

(defsubst hangul2-input-method-moum (char)
  "Store Hangul Jamo indice CHAR in `hangul-queue'.
It is a Hangul 2-Bulsik Moum.
This function processes a Hangul 2-Bulsik Moum.
The Moum can be located in a Jungseong position.
Other parts are the same as a `hangul2-input-method-jaum'."
  (if (cond ((zerop (aref hangul-queue 2))
             (aset hangul-queue 2 char))
            ((and (zerop (aref hangul-queue 3))
                  (zerop (aref hangul-queue 4))
                  (notzerop (hangul-djamo 'jung (aref hangul-queue 2) char)))
             (aset hangul-queue 3 char)))
      (hangul-insert-character hangul-queue)
    (let ((next-char (vector 0 0 char 0 0 0)))
      (cond ((notzerop (aref hangul-queue 5))
	     (aset next-char 0 (aref hangul-queue 5))
	     (aset hangul-queue 5 0))
	    ((notzerop (aref hangul-queue 4))
	     (aset next-char 0 (aref hangul-queue 4))
	     (aset hangul-queue 4 0)))
      (hangul-insert-character hangul-queue
			       (setq hangul-queue next-char)))))

(defsubst hangul3-input-method-cho (char)
  "Store Hangul Jamo indice CHAR in `hangul-queue'.
It is a Hangul 3-Bulsik Choseong.
This function processes a Hangul 3-Bulsik Choseong.
The Hangul 3-Bulsik is composed of a Choseong, a Jungseong and a Jongseong.
The Choseong can be located in a Choseong position.
Other parts are the same as a `hangul2-input-method-jaum'."
  (if (cond ((and (zerop (aref hangul-queue 0))
                  (zerop (aref hangul-queue 4)))
             (aset hangul-queue 0 char))
            ((and (zerop (aref hangul-queue 1))
                  (zerop (aref hangul-queue 2))
                  (notzerop (hangul-djamo 'cho (aref hangul-queue 0) char)))
             (aset hangul-queue 1 char)))
      (hangul-insert-character hangul-queue)
    (hangul-insert-character hangul-queue
			     (setq hangul-queue (vector char 0 0 0 0 0)))))

(defsubst hangul3-input-method-jung (char)
  "Store Hangul Jamo indice CHAR in `hangul-queue'.
It is a Hangul 3-Bulsik Jungseong.
This function processes a Hangul 3-Bulsik Jungseong.
The Jungseong can be located in a Jungseong position.
Other parts are the same as a `hangul3-input-method-cho'."
  (if (cond ((and (zerop (aref hangul-queue 2))
                  (zerop (aref hangul-queue 4)))
             (aset hangul-queue 2 char))
            ((and (zerop (aref hangul-queue 3))
                  (notzerop (hangul-djamo 'jung (aref hangul-queue 2) char)))
             (aset hangul-queue 3 char)))
      (hangul-insert-character hangul-queue)
    (hangul-insert-character hangul-queue
			     (setq hangul-queue (vector 0 0 char 0 0 0)))))

(defsubst hangul3-input-method-jong (char)
  "Store Hangul Jamo indice CHAR in `hangul-queue'.
It is a Hangul 3-Bulsik Jongseong.
This function processes a Hangul 3-Bulsik Jongseong.
The Jongseong can be located in a Jongseong position.
Other parts are the same as a `hangul3-input-method-cho'."
  (if (cond ((and (zerop (aref hangul-queue 4))
                  (notzerop (aref hangul-queue 0))
                  (notzerop (aref hangul-queue 2))
                  (numberp
                   (hangul-character
                    (+ (aref hangul-queue 0)
                       (hangul-djamo
                        'cho
                        (aref hangul-queue 0)
                        (aref hangul-queue 1)))
                    (+ (aref hangul-queue 2)
                       (hangul-djamo
                        'jung
                        (aref hangul-queue 2)
                        (aref hangul-queue 3)))
                    char)))
             (aset hangul-queue 4 char))
            ((and (zerop (aref hangul-queue 5))
                  (notzerop (hangul-djamo 'jong (aref hangul-queue 4) char))
                  (numberp
                   (hangul-character
                    (+ (aref hangul-queue 0)
                       (hangul-djamo
                        'cho
                        (aref hangul-queue 0)
                        (aref hangul-queue 1)))
                    (+ (aref hangul-queue 2)
                       (hangul-djamo
                        'jung
                        (aref hangul-queue 2)
                        (aref hangul-queue 3)))
                    (+ (aref hangul-queue 4)
                       (hangul-djamo
                        'jong
                        (aref hangul-queue 4)
                        char)))))
             (aset hangul-queue 5 char)))
      (hangul-insert-character hangul-queue)
    (if (zerop (apply '+ (append hangul-queue nil)))
	(hangul-insert-character (setq hangul-queue (vector 0 0 0 0 char 0)))
      (hangul-insert-character hangul-queue
			       (setq hangul-queue (vector 0 0 0 0 char 0))))))

(defun hangul-delete-backward-char ()
  "Delete the previous hangul character by Jaso units."
  (interactive)
  (let ((i 5))
    (while (and (> i 0) (zerop (aref hangul-queue i)))
      (setq i (1- i)))
    (aset hangul-queue i 0))
  (if (notzerop (apply '+ (append hangul-queue nil)))
      (hangul-insert-character hangul-queue)
    (delete-backward-char 1)))

(defun hangul-to-hanja-conversion ()
  "Convert the previous hangul character to the corresponding hanja character.
When a Korean input method is off, convert the following hangul character."
  (interactive)
  (let ((echo-keystrokes 0)
        delete-func
        hanja-character)
    (if (and (overlayp quail-overlay) (overlay-start quail-overlay))
        (progn
	  (setq hanja-character (hangul-to-hanja-char (preceding-char)))
	  (setq delete-func (lambda () (delete-backward-char 1))))
      (setq hanja-character (hangul-to-hanja-char (following-char)))
      (setq delete-func (lambda () (delete-char 1))))
    (when hanja-character
      (funcall delete-func)
      (insert hanja-character)
      (setq hangul-queue (make-vector 6 0))
      (if (and (overlayp quail-overlay) (overlay-start quail-overlay))
	  (move-overlay quail-overlay (point) (point))))))

;; Support function for `hangul2-input-method'.  Actually, this
;; function handles the Hangul 2-Bulsik.  KEY is an entered key code
;; used for looking up `hangul2-keymap'."
(defun hangul2-input-method-internal (key)
  (let ((char (+ (aref hangul2-keymap (1- (% key 32)))
                 (cond ((or (= key ?O) (= key ?P)) 2)
                       ((or (= key ?E) (= key ?Q) (= key ?R)
                            (= key ?T) (= key ?W)) 1)
                       (t 0)))))
    (if (< char 31)
        (hangul2-input-method-jaum char)
      (hangul2-input-method-moum char))))

(defun hangul2-input-method (key)
  "2-Bulsik input method."
  (if (or buffer-read-only (not (alphabetp key)))
      (list key)
    (quail-setup-overlays nil)
    (let ((input-method-function nil)
	  (echo-keystrokes 0)
	  (help-char nil))
      (setq hangul-queue (make-vector 6 0))
      (hangul2-input-method-internal key)
      (unwind-protect
	  (catch 'exit-input-loop
	    (while t
	      (let* ((seq (read-key-sequence nil))
		     (cmd (lookup-key hangul-im-keymap seq))
		     key)
		(cond ((and (stringp seq)
			    (= 1 (length seq))
			    (setq key (aref seq 0))
			    (alphabetp key))
		       (hangul2-input-method-internal key))
		      ((commandp cmd)
		       (call-interactively cmd))
		      (t
		       (setq unread-command-events (listify-key-sequence seq))
		       (throw 'exit-input-loop nil))))))
	(quail-delete-overlays)))))

;; Support function for `hangul3-input-method'.  Actually, this
;; function handles the Hangul 3-Bulsik final.  KEY is an entered key
;; code used for looking up `hangul3-keymap'."
(defun hangul3-input-method-internal (key)
  (let ((char (aref hangul3-keymap (- key 33))))
    (cond ((and (> char 92) (< char 123))
           (hangul3-input-method-cho (- char 92)))
          ((and (> char 65) (< char 87))
           (hangul3-input-method-jung (- char 35)))
          ((< char 31)
           (hangul3-input-method-jong char))
          (t
           (setq hangul-queue (make-vector 6 0))
           (insert (decode-char 'ucs char))
           (move-overlay quail-overlay (point) (point))))))

(defun hangul3-input-method (key)
  "3-Bulsik final input method."
  (if (or buffer-read-only (< key 33) (>= key 127))
      (list key)
    (quail-setup-overlays nil)
    (let ((input-method-function nil)
	  (echo-keystrokes 0)
	  (help-char nil))
      (setq hangul-queue (make-vector 6 0))
      (hangul3-input-method-internal key)
      (unwind-protect
	  (catch 'exit-input-loop
	    (while t
	      (let* ((seq (read-key-sequence nil))
		     (cmd (lookup-key hangul-im-keymap seq))
		     key)
		(cond ((and (stringp seq)
			    (= 1 (length seq))
			    (setq key (aref seq 0))
			    (and (>= key 33) (< key 127)))
		       (hangul3-input-method-internal key))
		      ((commandp cmd)
		       (call-interactively cmd))
		      (t
		       (setq unread-command-events (listify-key-sequence seq))
		       (throw 'exit-input-loop nil))))))
	(quail-delete-overlays)))))

;; Support function for `hangul390-input-method'.  Actually, this
;; function handles the Hangul 3-Bulsik 390.  KEY is an entered key
;; code used for looking up `hangul390-keymap'."
(defun hangul390-input-method-internal (key)
  (let ((char (aref hangul390-keymap (- key 33))))
    (cond ((or (and (> char 86) (< char 91))
               (and (> char 96) (< char 123)))
           (hangul3-input-method-cho (- char (if (< char 97) 86 92))))
          ((and (> char 64) (< char 86))
           (hangul3-input-method-jung (- char 34)))
          ((< char 31)
           (hangul3-input-method-jong char))
          (t
           (setq hangul-queue (make-vector 6 0))
           (insert (decode-char 'ucs char))
           (move-overlay quail-overlay (point) (point))))))

(defun hangul390-input-method (key)
  "3-Bulsik 390 input method."
  (if (or buffer-read-only (< key 33) (>= key 127))
      (list key)
    (quail-setup-overlays nil)
    (let ((input-method-function nil)
	  (echo-keystrokes 0)
	  (help-char nil))
      (setq hangul-queue (make-vector 6 0))
      (hangul390-input-method-internal key)
      (unwind-protect
	  (catch 'exit-input-loop
	    (while t
	      (let* ((seq (read-key-sequence nil))
		     (cmd (lookup-key hangul-im-keymap seq))
		     key)
		(cond ((and (stringp seq)
			    (= 1 (length seq))
			    (setq key (aref seq 0))
			    (and (>= key 33) (< key 127)))
		       (hangul390-input-method-internal key))
		      ((commandp cmd)
		       (call-interactively cmd))
		      (t
		       (setq unread-command-events (listify-key-sequence seq))
		       (throw 'exit-input-loop nil))))))
	(quail-delete-overlays)))))

;; Text shown by describe-input-method.  Set to a proper text by
;; hangul-input-method-activate.
(defvar hangul-input-method-help-text nil)
(make-variable-buffer-local 'hangul-input-method-help-text)

(defun hangul-input-method-activate (input-method func help-text &rest args)
  "Activate Hangul input method INPUT-METHOD.
FUNC is a function to handle input key.
HELP-TEXT is a text set in `hangul-input-method-help-text'."
  (setq inactivate-current-input-method-function 'hangul-input-method-inactivate
	describe-current-input-method-function 'hangul-input-method-help
	hangul-input-method-help-text help-text)
  (quail-delete-overlays)
  (if (eq (selected-window) (minibuffer-window))
      (add-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer))
  (set (make-local-variable 'input-method-function) func))

(defun hangul-input-method-inactivate ()
  "Inactivate the current Hangul input method."
  (interactive)
  (unwind-protect
      (progn
        (quail-hide-guidance)
        (quail-delete-overlays)
        (setq describe-current-input-method-function nil))
    (kill-local-variable 'input-method-function)))

(defun hangul-input-method-help ()
  "Describe the current Hangul input method."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ hangul-input-method-help-text)))

(provide 'hangul)

;;; hangul.el ends here
