;;; ucs-normalize.el --- Unicode normalization NFC/NFD/NFKD/NFKC

;; Copyright (C) 2009-2012  Free Software Foundation, Inc.

;; Author: Taichi Kawabata <kawabata.taichi@gmail.com>
;; Keywords: unicode, normalization

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
;; This program has passed the NormalizationTest-5.2.0.txt.
;;
;; References:
;; http://www.unicode.org/reports/tr15/
;; http://www.unicode.org/review/pr-29.html
;;
;; HFS-Normalization:
;; Reference:
;; http://developer.apple.com/technotes/tn/tn1150.html
;;
;; HFS Normalization excludes following area for decomposition.
;;
;;  U+02000 .. U+02FFF :: Punctuation, symbols, dingbats, arrows, etc.
;;                        (Characters in this region will be composed.)
;;  U+0F900 .. U+0FAFF :: CJK compatibility Ideographs.
;;  U+2F800 .. U+2FFFF :: CJK compatibility Ideographs.
;;
;; HFS-Normalization is useful for normalizing text involving CJK Ideographs.
;;
;;;
;;; Implementation Notes on NFC/HFS-NFC.
;;;
;;
;;    <Stages>   Decomposition    Composition
;;   NFD:        'nfd             nil
;;   NFC:        'nfd             t
;;   NFKD:       'nfkd            nil
;;   NFKC:       'nfkd            t
;;   HFS-NFD:    'hfs-nfd         'hfs-nfd-comp-p
;;   HFS-NFC:    'hfs-nfd         t
;;
;; Algorithm for Normalization
;;
;; Before normalization, following data will be prepared.
;;
;; 1. quick-check-list
;;
;;  `quick-check-list' consists of characters that will be decomposed
;;  during normalization.  It includes composition-exclusions,
;;  singletons, non-starter-decompositions and decomposable
;;  characters.
;;
;;  `quick-check-regexp' will search the above characters plus
;;  combining characters.
;;
;; 2. decomposition-translation
;;
;;  `decomposition-translation' is a translation table that will be
;;  used to decompose the characters.
;;
;;
;; Normalization Process
;;
;; A. Searching (`ucs-normalize-region')
;;
;;    Region is searched for `quick-check-regexp' to find possibly
;;    normalizable point.
;;
;; B. Identification of Normalization Block
;;
;;    (1) start of the block
;;        If the searched character is a starter and not combining
;;        with previous character, then the beginning of the block is
;;        the searched character.  If searched character is combining
;;        character, then previous character will be the target
;;        character
;;    (2) end of the block
;;        Block ends at non-composable starter character.
;;
;; C. Decomposition  (`ucs-normalize-block')
;;
;;    The entire block will be decomposed by
;;    `decomposition-translation' table.
;;
;; D. Sorting and Composition  of Smaller Blocks (`ucs-normalize-block-compose-chars')
;;
;;    The block will be split to multiple samller blocks by starter
;;    characters.  Each block is sorted, and composed if necessary.
;;
;; E. Composition of Entire Block (`ucs-normalize-compose-chars')
;;
;;   Composed blocks are collected and again composed.

;;; Code:

(defconst ucs-normalize-version "1.2")

(eval-when-compile (require 'cl))

(declare-function nfd "ucs-normalize" (char))

(eval-when-compile

  (defconst ucs-normalize-composition-exclusions
    '(#x0958 #x0959 #x095A #x095B #x095C #x095D #x095E #x095F
      #x09DC #x09DD #x09DF #x0A33 #x0A36 #x0A59 #x0A5A #x0A5B
      #x0A5E #x0B5C #x0B5D #x0F43 #x0F4D #x0F52 #x0F57 #x0F5C
      #x0F69 #x0F76 #x0F78 #x0F93 #x0F9D #x0FA2 #x0FA7 #x0FAC
      #x0FB9 #xFB1D #xFB1F #xFB2A #xFB2B #xFB2C #xFB2D #xFB2E
      #xFB2F #xFB30 #xFB31 #xFB32 #xFB33 #xFB34 #xFB35 #xFB36
      #xFB38 #xFB39 #xFB3A #xFB3B #xFB3C #xFB3E #xFB40 #xFB41
      #xFB43 #xFB44 #xFB46 #xFB47 #xFB48 #xFB49 #xFB4A #xFB4B
      #xFB4C #xFB4D #xFB4E #x2ADC #x1D15E #x1D15F #x1D160 #x1D161
      #x1D162 #x1D163 #x1D164 #x1D1BB #x1D1BC #x1D1BD #x1D1BE
      #x1D1BF #x1D1C0)
   "Composition Exclusion List.
  This list is taken from
    http://www.unicode.org/Public/UNIDATA/5.2/CompositionExclusions.txt")

  ;; Unicode ranges that decompositions & combinings are defined.
  (defvar check-range nil)
    (setq check-range '((#x00a0 . #x3400) (#xA600 . #xAC00) (#xF900 . #x110ff) (#x1d000 . #x1dfff) (#x1f100 . #x1f2ff) (#x2f800 . #x2faff)))

  ;; Basic normalization functions
  (defun nfd (char)
    (let ((decomposition
           (get-char-code-property char 'decomposition)))
      (if (and decomposition (numberp (car decomposition))
	       (or (> (length decomposition) 1)
		   (/= (car decomposition) char)))
          decomposition)))

  (defun nfkd (char)
    (let ((decomposition
           (get-char-code-property char 'decomposition)))
      (if (symbolp (car decomposition)) (cdr decomposition)
        (if (or (> (length decomposition) 1)
		(/= (car decomposition) char)) decomposition))))

  (defun hfs-nfd (char)
    (when (or (and (>= char 0) (< char #x2000))
              (and (>= char #x3000) (< char #xf900))
              (and (>= char #xfb00) (< char #x2f800))
              (>= char #x30000))
      (nfd char))))

(eval-and-compile
(defun ucs-normalize-hfs-nfd-comp-p (char)
  (and (>= char #x2000) (< char #x3000)))

(defsubst ucs-normalize-ccc (char)
  (get-char-code-property char 'canonical-combining-class))
)

;; Data common to all normalizations

(eval-when-compile

  (defvar combining-chars nil)
    (setq combining-chars nil)
  (defvar decomposition-pair-to-composition nil)
    (setq decomposition-pair-to-composition nil)
  (defvar non-starter-decompositions nil)
    (setq non-starter-decompositions nil)
  (let ((char 0) ccc decomposition)
    (mapc
     (lambda (start-end)
       (do ((char (car start-end) (+ char 1))) ((> char (cdr start-end)))
         (setq ccc (ucs-normalize-ccc char))
         (setq decomposition (get-char-code-property
                              char 'decomposition))
	 (if (and (= (length decomposition) 1)
		  (= (car decomposition) char))
	     (setq decomposition nil))
         (if (and ccc (/= 0 ccc)) (add-to-list 'combining-chars char))
         (if (and (numberp (car decomposition))
                  (/= (ucs-normalize-ccc (car decomposition))
                      0))
             (add-to-list 'non-starter-decompositions char))
         (when (numberp (car decomposition))
           (if (and (= 2 (length decomposition))
                    (null (memq char ucs-normalize-composition-exclusions))
                    (null (memq char non-starter-decompositions)))
               (setq decomposition-pair-to-composition
                     (cons (cons decomposition char)
                           decomposition-pair-to-composition)))
           ;; If not singleton decomposition, second and later characters in
           ;; decomposition will be the subject of combining characters.
           (if (cdr decomposition)
               (dolist (char (cdr decomposition))
                 (add-to-list 'combining-chars char))))))
     check-range))

  (setq combining-chars
        (append combining-chars
                '(?ᅡ ?ᅢ ?ᅣ ?ᅤ ?ᅥ ?ᅦ ?ᅧ ?ᅨ ?ᅩ ?ᅪ
                ?ᅫ ?ᅬ ?ᅭ ?ᅮ ?ᅯ ?ᅰ ?ᅱ ?ᅲ ?ᅳ ?ᅴ ?ᅵ
                ?ᆨ ?ᆩ ?ᆪ ?ᆫ ?ᆬ ?ᆭ ?ᆮ ?ᆯ ?ᆰ ?ᆱ ?ᆲ ?ᆳ ?ᆴ
                ?ᆵ ?ᆶ ?ᆷ ?ᆸ ?ᆹ ?ᆺ ?ᆻ ?ᆼ ?ᆽ ?ᆾ ?ᆿ ?ᇀ ?ᇁ ?ᇂ)))
  )

(eval-and-compile
(defun ucs-normalize-make-hash-table-from-alist (alist)
  (let ((table (make-hash-table :test 'equal :size 2000)))
    (mapc (lambda (x) (puthash (car x) (cdr x) table)) alist)
    table))

(defvar ucs-normalize-decomposition-pair-to-primary-composite nil
  "Hashtable of decomposed pair to primary composite.
Note that Hangul are excluded.")
  (setq ucs-normalize-decomposition-pair-to-primary-composite
        (ucs-normalize-make-hash-table-from-alist
         (eval-when-compile decomposition-pair-to-composition)))

(defun ucs-normalize-primary-composite (decomposition-pair composition-predicate)
  "Convert DECOMPOSITION-PAIR to primary composite using COMPOSITION-PREDICATE."
  (let ((char (or (gethash decomposition-pair
                           ucs-normalize-decomposition-pair-to-primary-composite)
                  (and (<= #x1100 (car decomposition-pair))
                       (< (car decomposition-pair) #x1113)
                       (<= #x1161 (cadr decomposition-pair))
                       (< (car decomposition-pair) #x1176)
                       (let ((lindex (- (car decomposition-pair) #x1100))
                             (vindex (- (cadr decomposition-pair) #x1161)))
                         (+ #xAC00 (* (+ (* lindex  21) vindex) 28))))
                  (and (<= #xac00 (car decomposition-pair))
                       (< (car decomposition-pair) #xd7a4)
                       (<= #x11a7 (cadr decomposition-pair))
                       (< (cadr decomposition-pair) #x11c3)
                       (= 0 (% (- (car decomposition-pair) #xac00) 28))
                       (let ((tindex (- (cadr decomposition-pair) #x11a7)))
                         (+ (car decomposition-pair) tindex))))))
    (if (and char
             (functionp composition-predicate)
             (null (funcall composition-predicate char)))
        nil char)))
)

(defvar ucs-normalize-combining-chars nil)
  (setq ucs-normalize-combining-chars (eval-when-compile combining-chars))

(defvar ucs-normalize-combining-chars-regexp nil
  "Regular expression to match sequence of combining characters.")
  (setq ucs-normalize-combining-chars-regexp
  (eval-when-compile (concat (regexp-opt (mapcar 'char-to-string combining-chars)) "+")))

(declare-function decomposition-translation-alist "ucs-normalize"
                  (decomposition-function))
(declare-function decomposition-char-recursively "ucs-normalize"
                  (char decomposition-function))
(declare-function alist-list-to-vector "ucs-normalize" (alist))

(eval-when-compile

  (defun decomposition-translation-alist (decomposition-function)
    (let (decomposition alist)
      (mapc
       (lambda (start-end)
         (do ((char (car start-end) (+ char 1))) ((> char (cdr start-end)))
           (setq decomposition (funcall decomposition-function char))
           (if decomposition
               (setq alist (cons (cons char
                                       (apply 'append
                                              (mapcar (lambda (x)
                                                        (decomposition-char-recursively
                                                         x decomposition-function))
                                                      decomposition)))
                                 alist)))))
       check-range)
      alist))

  (defun decomposition-char-recursively (char decomposition-function)
    (let ((decomposition (funcall decomposition-function char)))
      (if decomposition
          (apply 'append
                 (mapcar (lambda (x)
                           (decomposition-char-recursively x decomposition-function))
                         decomposition))
        (list char))))

  (defun alist-list-to-vector (alist)
    (mapcar (lambda (x) (cons (car x) (apply 'vector (cdr x)))) alist))

  (defvar nfd-alist nil)
    (setq nfd-alist (alist-list-to-vector (decomposition-translation-alist 'nfd)))
  (defvar nfkd-alist nil)
    (setq nfkd-alist (alist-list-to-vector (decomposition-translation-alist 'nfkd)))
  (defvar hfs-nfd-alist nil)
    (setq hfs-nfd-alist (alist-list-to-vector (decomposition-translation-alist 'hfs-nfd)))
  )

(eval-and-compile
(defvar ucs-normalize-hangul-translation-alist nil)
  (setq ucs-normalize-hangul-translation-alist
        (let ((i 0) entries)
          (while (< i 11172)
            (setq entries
                  (cons (cons (+ #xac00 i)
                              (if (= 0 (% i 28))
                                  (vector (+ #x1100 (/ i 588))
                                          (+ #x1161 (/ (% i 588) 28)))
                                (vector (+ #x1100 (/ i 588))
                                        (+ #x1161 (/ (% i 588) 28))
                                        (+ #x11a7 (% i 28)))))
                        entries)
                  i (1+ i))) entries))

(defun ucs-normalize-make-translation-table-from-alist (alist)
  (make-translation-table-from-alist
     (append alist ucs-normalize-hangul-translation-alist)))

(define-translation-table 'ucs-normalize-nfd-table
  (ucs-normalize-make-translation-table-from-alist (eval-when-compile nfd-alist)))
(define-translation-table 'ucs-normalize-nfkd-table
  (ucs-normalize-make-translation-table-from-alist (eval-when-compile nfkd-alist)))
(define-translation-table 'ucs-normalize-hfs-nfd-table
  (ucs-normalize-make-translation-table-from-alist (eval-when-compile hfs-nfd-alist)))

(defun ucs-normalize-sort (chars)
  "Sort by canonical combining class of CHARS."
  (sort chars
        (lambda (ch1 ch2)
          (< (ucs-normalize-ccc ch1) (ucs-normalize-ccc ch2)))))

(defun ucs-normalize-compose-chars (chars composition-predicate)
  "Compose CHARS by COMPOSITION-PREDICATE.
CHARS must be sorted and normalized in starter-combining pairs."
  (if composition-predicate
      (let* ((starter (car chars))
             remain result prev-ccc
             (target-chars (cdr chars))
             target target-ccc
             primary-composite)
     (while target-chars
       (setq target     (car target-chars)
             target-ccc (ucs-normalize-ccc target))
       (if (and (or (null prev-ccc)
                    (< prev-ccc target-ccc))
                (setq primary-composite
                      (ucs-normalize-primary-composite (list starter target)
                                                       composition-predicate)))
           ;; case 1: composable
           (setq starter primary-composite
                 prev-ccc nil)
         (if (= 0 target-ccc)
             ;; case 2: move starter
             (setq result (nconc result (cons starter (nreverse remain)))
                   starter target
                   remain nil)
           ;; case 3: move target
           (setq prev-ccc target-ccc
                 remain (cons target remain))))
       (setq target-chars (cdr target-chars)))
     (nconc result (cons starter (nreverse remain))))
    chars))

(defun ucs-normalize-block-compose-chars (chars composition-predicate)
  "Try composing CHARS by COMPOSITION-PREDICATE.
If COMPOSITION-PREDICATE is not given, then do nothing."
  (let ((chars (ucs-normalize-sort chars)))
    (if composition-predicate
        (ucs-normalize-compose-chars chars composition-predicate)
      chars)))
)

(declare-function quick-check-list "ucs-normalize"
                  (decomposition-translation &optional composition-predicate))
(declare-function quick-check-list-to-regexp "ucs-normalize" (quick-check-list))

(eval-when-compile

  (defun quick-check-list (decomposition-translation
                           &optional composition-predicate)
    "Quick-Check List for DECOMPOSITION-TRANSLATION and COMPOSITION-PREDICATE.
It includes Singletons, CompositionExclusions, and Non-Starter
decomposition."
    (let (entries decomposition composition)
      (mapc
       (lambda (start-end)
         (do ((i (car start-end) (+ i 1))) ((> i (cdr start-end)))
           (setq decomposition
                 (string-to-list
                  (with-temp-buffer
                    (insert i)
                    (translate-region 1 2 decomposition-translation)
                    (buffer-string))))
           (setq composition
                 (ucs-normalize-block-compose-chars decomposition composition-predicate))
           (when (not (equal composition (list i)))
             (setq entries (cons i entries)))))
       check-range)
      ;;(remove-duplicates
       (append entries
               ucs-normalize-composition-exclusions
               non-starter-decompositions)))
  ;;)

  (defvar nfd-quick-check-list nil)
    (setq nfd-quick-check-list     (quick-check-list 'ucs-normalize-nfd-table      ))
  (defvar nfc-quick-check-list nil)
    (setq nfc-quick-check-list     (quick-check-list 'ucs-normalize-nfd-table     t ))
  (defvar nfkd-quick-check-list nil)
    (setq nfkd-quick-check-list    (quick-check-list 'ucs-normalize-nfkd-table      ))
  (defvar nfkc-quick-check-list nil)
    (setq nfkc-quick-check-list    (quick-check-list 'ucs-normalize-nfkd-table    t ))
  (defvar hfs-nfd-quick-check-list nil)
    (setq hfs-nfd-quick-check-list (quick-check-list 'ucs-normalize-hfs-nfd-table
                                                     'ucs-normalize-hfs-nfd-comp-p))
  (defvar hfs-nfc-quick-check-list nil)
    (setq hfs-nfc-quick-check-list (quick-check-list 'ucs-normalize-hfs-nfd-table t ))

  (defun quick-check-list-to-regexp (quick-check-list)
    (regexp-opt (mapcar 'char-to-string (append quick-check-list combining-chars))))

  (defun quick-check-decomposition-list-to-regexp (quick-check-list)
    (concat (quick-check-list-to-regexp quick-check-list) "\\|[가-힣]"))

  (defun quick-check-composition-list-to-regexp (quick-check-list)
    (concat (quick-check-list-to-regexp quick-check-list) "\\|[ᅡ-ᅵᆨ-ᇂ]"))
)


;; NFD/NFC
(defvar ucs-normalize-nfd-quick-check-regexp nil)
  (setq ucs-normalize-nfd-quick-check-regexp
  (eval-when-compile (quick-check-decomposition-list-to-regexp nfd-quick-check-list)))
(defvar ucs-normalize-nfc-quick-check-regexp nil)
  (setq ucs-normalize-nfc-quick-check-regexp
  (eval-when-compile (quick-check-composition-list-to-regexp nfc-quick-check-list)))

;; NFKD/NFKC
(defvar ucs-normalize-nfkd-quick-check-regexp nil)
  (setq ucs-normalize-nfkd-quick-check-regexp
  (eval-when-compile (quick-check-decomposition-list-to-regexp nfkd-quick-check-list)))
(defvar ucs-normalize-nfkc-quick-check-regexp nil)
  (setq ucs-normalize-nfkc-quick-check-regexp
  (eval-when-compile (quick-check-composition-list-to-regexp nfkc-quick-check-list)))

;; HFS-NFD/HFS-NFC
(defvar ucs-normalize-hfs-nfd-quick-check-regexp nil)
  (setq ucs-normalize-hfs-nfd-quick-check-regexp
  (eval-when-compile (concat (quick-check-decomposition-list-to-regexp hfs-nfd-quick-check-list))))
(defvar ucs-normalize-hfs-nfc-quick-check-regexp nil)
  (setq ucs-normalize-hfs-nfc-quick-check-regexp
  (eval-when-compile (quick-check-composition-list-to-regexp hfs-nfc-quick-check-list)))

;;------------------------------------------------------------------------------------------

;; Normalize local region.

(defun ucs-normalize-block
  (from to &optional decomposition-translation-table composition-predicate)
  "Normalize region FROM TO, by sorting the region with canonical-cc.
If DECOMPOSITION-TRANSLATION-TABLE is given, translate region
before sorting.  If COMPOSITION-PREDICATE is given, then compose
the region by using it."
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (if decomposition-translation-table
        (translate-region from to decomposition-translation-table))
    (goto-char (point-min))
    (let ((start (point)) chars); ccc)
      (while (not (eobp))
        (forward-char)
        (when (or (eobp)
                  (= 0 (ucs-normalize-ccc (char-after (point)))))
          (setq chars
                (nconc chars
                       (ucs-normalize-block-compose-chars
                        (string-to-list (buffer-substring start (point)))
                        composition-predicate))
                start (point)))
        ;;(unless ccc (error "Undefined character can not be normalized!"))
        )
      (delete-region (point-min) (point-max))
      (apply 'insert
             (ucs-normalize-compose-chars
              chars composition-predicate)))))

(defun ucs-normalize-region
  (from to quick-check-regexp translation-table composition-predicate)
  "Normalize region from FROM to TO.
QUICK-CHECK-REGEXP is applied for searching the region.
TRANSLATION-TABLE will be used to decompose region.
COMPOSITION-PREDICATE will be used to compose region."
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char (point-min))
      (let (start-pos starter)
        (while (re-search-forward quick-check-regexp nil t)
          (setq starter (string-to-char (match-string 0)))
          (setq start-pos (match-beginning 0))
          (ucs-normalize-block
           ;; from
           (if (or (= start-pos (point-min))
                   (and (= 0 (ucs-normalize-ccc starter))
                        (not (memq starter ucs-normalize-combining-chars))))
               start-pos (1- start-pos))
           ;; to
           (if (looking-at ucs-normalize-combining-chars-regexp)
               (match-end 0) (1+ start-pos))
           translation-table composition-predicate))))))

;; --------------------------------------------------------------------------------

(defmacro ucs-normalize-string (ucs-normalize-region)
  `(with-temp-buffer
     (insert str)
     (,ucs-normalize-region (point-min) (point-max))
     (buffer-string)))

;;;###autoload
(defun ucs-normalize-NFD-region (from to)
  "Normalize the current region by the Unicode NFD."
  (interactive "r")
  (ucs-normalize-region from to
                        ucs-normalize-nfd-quick-check-regexp
                        'ucs-normalize-nfd-table nil))
;;;###autoload
(defun ucs-normalize-NFD-string (str)
  "Normalize the string STR by the Unicode NFD."
  (ucs-normalize-string ucs-normalize-NFD-region))

;;;###autoload
(defun ucs-normalize-NFC-region (from to)
  "Normalize the current region by the Unicode NFC."
  (interactive "r")
  (ucs-normalize-region from to
                        ucs-normalize-nfc-quick-check-regexp
                        'ucs-normalize-nfd-table t))
;;;###autoload
(defun ucs-normalize-NFC-string (str)
  "Normalize the string STR by the Unicode NFC."
  (ucs-normalize-string ucs-normalize-NFC-region))

;;;###autoload
(defun ucs-normalize-NFKD-region (from to)
  "Normalize the current region by the Unicode NFKD."
  (interactive "r")
  (ucs-normalize-region from to
                        ucs-normalize-nfkd-quick-check-regexp
                        'ucs-normalize-nfkd-table nil))
;;;###autoload
(defun ucs-normalize-NFKD-string (str)
  "Normalize the string STR by the Unicode NFKD."
  (ucs-normalize-string ucs-normalize-NFKD-region))

;;;###autoload
(defun ucs-normalize-NFKC-region (from to)
  "Normalize the current region by the Unicode NFKC."
  (interactive "r")
  (ucs-normalize-region from to
                        ucs-normalize-nfkc-quick-check-regexp
                        'ucs-normalize-nfkd-table t))
;;;###autoload
(defun ucs-normalize-NFKC-string (str)
  "Normalize the string STR by the Unicode NFKC."
  (ucs-normalize-string ucs-normalize-NFKC-region))

;;;###autoload
(defun ucs-normalize-HFS-NFD-region (from to)
  "Normalize the current region by the Unicode NFD and Mac OS's HFS Plus."
  (interactive "r")
  (ucs-normalize-region from to
                        ucs-normalize-hfs-nfd-quick-check-regexp
                        'ucs-normalize-hfs-nfd-table
                        'ucs-normalize-hfs-nfd-comp-p))
;;;###autoload
(defun ucs-normalize-HFS-NFD-string (str)
  "Normalize the string STR by the Unicode NFD and Mac OS's HFS Plus."
  (ucs-normalize-string ucs-normalize-HFS-NFD-region))
;;;###autoload
(defun ucs-normalize-HFS-NFC-region (from to)
  "Normalize the current region by the Unicode NFC and Mac OS's HFS Plus."
  (interactive "r")
  (ucs-normalize-region from to
                        ucs-normalize-hfs-nfc-quick-check-regexp
                        'ucs-normalize-hfs-nfd-table t))
;;;###autoload
(defun ucs-normalize-HFS-NFC-string (str)
  "Normalize the string STR by the Unicode NFC and Mac OS's HFS Plus."
  (ucs-normalize-string ucs-normalize-HFS-NFC-region))

;; Post-read-conversion function for `utf-8-hfs'.
(defun ucs-normalize-hfs-nfd-post-read-conversion (len)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (+ (point) len))
      (ucs-normalize-HFS-NFC-region (point-min) (point-max))
      (- (point-max) (point-min)))))

;; Pre-write conversion for `utf-8-hfs'.
(defun ucs-normalize-hfs-nfd-pre-write-conversion (from to)
  (let ((old-buf (current-buffer)))
    (set-buffer (generate-new-buffer " *temp*"))
    (if (stringp from)
        (insert from)
      (insert-buffer-substring old-buf from to))
    (ucs-normalize-HFS-NFD-region (point-min) (point-max))
    nil))

;;; coding-system definition
(define-coding-system 'utf-8-hfs
  "UTF-8 based coding system for MacOS HFS file names.
The singleton characters in HFS normalization exclusion will not
be decomposed."
  :coding-type 'utf-8
  :mnemonic ?U
  :charset-list '(unicode)
  :post-read-conversion 'ucs-normalize-hfs-nfd-post-read-conversion
  :pre-write-conversion 'ucs-normalize-hfs-nfd-pre-write-conversion
  )

(provide 'ucs-normalize)

;; Local Variables:
;; coding: utf-8
;; End:

;;; ucs-normalize.el ends here
