;;; ogonek.el --- change the encoding of Polish diacritics

;; Copyright (C) 1997-1998, 2001-2012  Free Software Foundation, Inc.

;; Author: W{\l}odek Bzyl
;;	   Ryszard Kubiak
;; Maintainer: Ryszard Kubiak <rysiek@ipipan.gda.pl>
;; Keywords: i18n

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

;; To use this library load it using
;;                 M-x load-library [enter] ogonek
;;              Then, you may get a short info by calling one of
;;                 M-x ogonek-jak        -- in Polish
;;                 M-x ogonek-how        -- in English  "

;;; Code:

(defgroup ogonek nil
  "Change the encoding of Polish diacritic characters."
  :prefix "ogonek-"
  :group 'i18n)

(defconst ogonek-name-encoding-alist
  '(("ascii"      . (?A  ?C  ?E  ?L  ?N  ?O  ?S  ?Z  ?Z
                     ?a  ?c  ?e  ?l  ?n  ?o  ?s  ?z  ?z))
    ("iso8859-2"  . (161 198 202 163 209 211 166 172 175
                     177 230 234 179 241 243 182 188 191))
    ("mazovia"    . (143 149 144 156 165 163 152 160 161
                     134 141 145 146 164 162 158 166 167))
    ("windows-EE" . (165 198 202 163 209 211 140 143 175
                     185 230 234 179 241 243 156 159 191))
    ("windows-PL" . (165 198 202 163 209 211 140 143 175
                     185 230 234 179 241 243 156 159 191))
    ("latin-2"    . (164 143 168 157 227 224 151 141 189
                     165 134 169 136 228 162 152 171 190))
    ("CP852"      . (164 143 168 157 227 224 151 141 189
                     165 134 169 136 228 162 152 171 190))
    ("MeX"        . (129 130 134 138 139 211 145 153 155
                     161 162 166 170 171 243 177 185 187))
    ("CorelDraw"  . (197 242 201 163 209 211 255 225 237
                     229 236 230 198 241 243 165 170 186))
    ("Amiga"      . (194 202 203 206 207 211 212 218 219
                     226 234 235 238 239 243 244 250 251))
    ("Mac"        . (132 140 162 252 193 238 229 143 251
                     136 141 171 184 196 151 230 144 253))
 )
  "The constant `ogonek-name-encoding-alist' is a list of (NAME.LIST) pairs.
Each LIST contains codes for 18 Polish diacritic characters. The codes
are given in the following order:
  Aogonek Cacute Eogonek Lslash Nacute Oacute Sacute Zacute Zdotaccent
  aogonek cacute eogonek lslash nacute oacute sacute zacute zdotaccent.")

;; ------ A Little Info in Polish ---------------

(defconst ogonek-informacja
  "   FUNKCJE INTERAKCYJNE UDOST/EPNIANE PRZEZ BIBLIOTEK/E `ogonek'.

Je/sli czytasz ten tekst, to albo przegl/adasz plik /xr/od/lowy
biblioteki `ogonek.el', albo wywo/la/le/s polecenie `ogonek-jak'.
W drugim przypadku mo/zesz usun/a/c tekst z ekranu, stosuj/ac
polecenie `M-x kill-buffer'.

Niniejsza biblioteka dostarcza funkcji do zmiany kodowania polskich
znak/ow diakrytycznych. Funkcje te mo/zna pogrupowa/c nast/epuj/aco.

 1. `ogonek-recode-region' oraz `ogonek-recode-buffer'
    przekodowuj/a zaznaczony fragment wzgl/ednie ca/ly buffor.
    Po wywo/laniu interakcyjnym funkcji zadawane s/a
    pytania o parametry przekodowania: nazw/e kodowania
    w tek/scie /xr/od/lowym i nazw/e kodowania docelowego.
    Poni/zsze przyk/lady powinny wyja/sni/c, jakich parametr/ow
    oczekuj/a wymienione funkcje:

      (ogonek-recode-region (poczatek-fragmentu) (koniec-fragmentu)
         nazwa-kodowania-w-tekscie-zrodlowym nazwa-kodowania-docelowa)
      (ogonek-recode-buffer
         nazwa-kodowania-w-tekscie-zrodlowym nazwa-kodowania-docelowa)

 2. `ogonek-prefixify-region' oraz `ogonek-prefixify-buffer'
    s/lu/z/a do wprowadzania notacji prefiksowej.

      (ogonek-prefixify-region (poczatek-fragmentu) (koniec-fragmentu)
         nazwa-kodowania-w-tekscie-zrodlowym znak-prefiksu)
      (ogonek-prefixify-buffer
         nazwa-kodowania-w-tekscie-zrodlowym znak-prefiksu)

 3. `ogonek-deprefixify-region' oraz `ogonek-deprefixify-buffer'
    s/lu/z/a do usuwania notacji prefiksowej.

      (ogonek-deprefixify-region (poczatek-fragmentu) (koniec-fragmentu)
         znak-prefiksu nazwa-kodowania-docelowa)
      (ogonek-prefixify-buffer
         znak-prefiksu nazwa-kodowania-docelowa)

 U/zycie klawisza TAB w trybie interakcyjnym powoduje wy/swietlenie
 listy dopuszczalnych nazw kod/ow, pami/etanych w sta/lej
 `ogonek-name-encoding-alist'.

 Funkcje biblioteki odwo/luj/a si/e do pi/eciu zmiennych, kt/ore
 przechowuj/a podpowiedzi do zadawanych pyta/n. Nazwy tych zmiennych
 oraz ich warto/sci domy/slne s/a nast/epuj/ace:

   ogonek-from-encoding           iso8859-2
   ogonek-to-encoding             ascii
   ogonek-prefix-char              /
   ogonek-prefix-from-encoding    iso8859-2
   ogonek-prefix-to-encoding      iso8859-2

 Powy/zsze warto/sci domy/slne mo/zna zmieni/c przez umieszczenie w pliku
 konfiguracyjnym `~/.emacs' odpowiednich przypisa/n, na przyk/lad:

   (setq ogonek-prefix-char ?/)
   (setq ogonek-prefix-to-encoding \"iso8859-2\")

 Zamiast wczytywania ca/lej biblioteki `ogonek.el' mo/zna w pliku
 `~/.emacs' za/z/ada/c wczytania wybranych funkcji, na dodatek dopiero
 w chwili ich rzeczywistego u/zycia:

   (autoload 'ogonek-jak \"ogonek\")
   (autoload 'ogonek-recode-region \"ogonek\")
   (autoload 'ogonek-prefixify-region \"ogonek\")
   (autoload 'ogonek-deprefixify-region \"ogonek\")

 Cz/esto wyst/epuj/ace kombinacje wywo/la/n funkcji mo/zna dla wygody
 skr/oci/c i przypisa/c klawiszom. Oto praktyczne przyk/lady:

   (defun deprefixify-iso8859-2-region (start end)
     (interactive \"*r\")
     (ogonek-deprefixify-region start end ?/ \"iso8859-2\"))
   (global-set-key \"\\C-cd\" 'deprefixify-iso8859-2-region) ; ctrl-c d

   (defun mazovia-to-iso8859-2 (start end)
     (interactive \"*r\")
     (ogonek-recode-region start end \"mazovia\" \"iso8859-2\"))
   (global-set-key \"\\C-cr\" 'mazovia-to-iso8859-2) ; ctrl-c r

   (defun prefixify-iso8859-2-region (start end)
     (interactive \"*r\")
     (ogonek-prefixify-region start end \"iso8859-2\" ?/))
   (global-set-key \"\\C-cp\" 'prefixify-iso8859-2-region) ; ctrl-c p

 Ka/zd/a operacj/e przekodowania mo/zna w ca/lo/sci odwo/la/c
 przez wykonanie polecenia `undo'.")

(defun ogonek-jak ()
  "Display `ogonek-informacja' in an auxiliary *ogonek-jak* buffer."
  (interactive)
  (set-buffer  (get-buffer-create " *ogonek-jak*"))
  (insert ogonek-informacja)
  (switch-to-buffer " *ogonek-jak*")
  (goto-char (point-min)))

;; ------ A Little Info in English --------

(defconst ogonek-information
  "  THE INTERACTIVE FUNCTIONS PROVIDED BY THE LIBRARY `ogonek'.

If you read this text then you are either looking at the library's
source text or you have called the `ogonek-how' command. In the
latter case you may remove this text using `M-x kill-buffer'.

The library provides functions for changing the encoding of Polish
diacritic characters, the ones with an `ogonek' below or above them.
The functions come in the following groups.

 1. `ogonek-recode-region' and `ogonek-recode-buffer' to change
    between one-character encodings, such as `iso-8859-2', `mazovia',
    plain `ascii' or `TeX'. As the names suggest you may recode
    either the entire current buffer or just a marked region
    in it. You may use the functions interactively as commands.
    Once you call a command you will be asked about the code
    currently used in your text and the target encoding, the one
    you want to get. The following example shows a non-interactive
    use of the functions in a program. This also illustrates what
    type of parameters the functions expect to be called with:

      (ogonek-recode-region
        (region-beginning) (region-end) from-code-name to-code-name)
      (ogonek-recode-buffer from-code-name to-code-name)

 2. `ogonek-prefixify-region' and `ogonek-prefixify-buffer' for
    introducing prefix notation:

      (ogonek-prefixify-region
        (region-beginning) (region-end) from-code-name prefix-char)
      (ogonek-prefixify-buffer from-code-name prefix-char)

 3. `ogonek-deprefixify-region' and `ogonek-deprefixify-buffer' for
    removing prefix notation:

      (ogonek-deprefixify-region
        (region-beginning) (region-end) prefix-char to-code-name)
      (ogonek-prefixify-buffer prefix-char to-code-name)

 The TAB character used in interactive mode makes `emacs'
 display the list of encodings recognized by the library. The list
 is stored in the constant `ogonek-name-encoding-alist'.

 The `ogonek' functions refer to five variables in which the suggested
 answers to dialogue questions are stored. The variables and their
 default values are:

   ogonek-from-encoding           iso8859-2
   ogonek-to-encoding             ascii
   ogonek-prefix-char             /
   ogonek-prefix-from-encoding    iso8859-2
   ogonek-prefix-to-encoding      iso8859-2

 The above default values can be changed by placing appropriate settings
 in the '~/.emacs' file:

   (setq ogonek-prefix-char ?/)
   (setq ogonek-prefix-to-encoding \"iso8859-2\")

 Instead of loading the whole library `ogonek' it may be better to
 autoload the needed functions, for example by placing in `~/.emacs':

   (autoload 'ogonek-how \"ogonek\")
   (autoload 'ogonek-recode-region \"ogonek\")
   (autoload 'ogonek-prefixify-region \"ogonek\")
   (autoload 'ogonek-deprefixify-region \"ogonek\")

 The most frequent function calls can be abbreviated and assigned to
 keyboard keys. Here are a few practical examples:

   (defun deprefixify-iso8859-2-region (start end)
     (interactive \"*r\")
     (ogonek-deprefixify-region start end ?/ \"iso8859-2\"))
   (global-set-key \"\\C-cd\" 'deprefixify-iso8859-2-region) ; ctrl-c d

   (defun mazovia-to-iso8859-2 (start end)
     (interactive \"*r\")
     (ogonek-recode-region start end \"mazovia\" \"iso8859-2\"))
   (global-set-key \"\\C-cr\" 'mazovia-to-iso8859-2) ; ctrl-c r

   (defun prefixify-iso8859-2-region (start end)
     (interactive \"*r\")
     (ogonek-prefixify-region start end \"iso8859-2\" ?/))
   (global-set-key \"\\C-cp\" 'prefixify-iso8859-2-region) ; ctrl-c p

 Each recoding operation can be called off using the `undo' command.")

(defun ogonek-how ()
  "Display `ogonek-information' in an auxiliary *recode-how* buffer."
  (interactive "*")
  (set-buffer  (get-buffer-create " *ogonek-how*"))
  (insert ogonek-information)
  (switch-to-buffer " *ogonek-how*")
  (goto-char (point-min)))

;; ---- Variables keeping the suggested answers to dialogue questions -----
(defvar ogonek-encoding-choices
  (cons 'choice
	(mapcar (lambda (x) (list 'const (car x)))
		ogonek-name-encoding-alist))
  "List of ogonek encodings.  Used only for customization.")
(defcustom ogonek-from-encoding "iso8859-2"
  "Encoding in the source file of recoding."
  :type ogonek-encoding-choices
  :group 'ogonek)
(defcustom ogonek-to-encoding "ascii"
  "Encoding in the target file of recoding."
  :type ogonek-encoding-choices
  :group 'ogonek)
(defcustom ogonek-prefix-char ?/
  "Prefix character for prefix encodings."
  :type 'character
  :group 'ogonek)
(defcustom ogonek-prefix-from-encoding "iso8859-2"
  "Encoding in the source file subject to prefixation."
  :type ogonek-encoding-choices
  :group 'ogonek)
(defcustom ogonek-prefix-to-encoding "iso8859-2"
  "Encoding in the target file subject to deprefixation."
  :type ogonek-encoding-choices
  :group 'ogonek)

;; ---- Auxiliary functions for reading parameters in interactive mode ----

(defun ogonek-read-encoding (prompt default-name-var)
  "Read encoding name with completion based on `ogonek-name-encoding-alist'.
Store the name in the parameter-variable DEFAULT-NAME-VAR.
PROMPT is a string to be shown when the user is asked for a name."
 (let ((encoding
        (completing-read
         (format "%s (default %s): " prompt (eval default-name-var))
         ogonek-name-encoding-alist nil t)))
  ;; change the default name to the one just read
  (set default-name-var
    (if (string= encoding "") (eval default-name-var) encoding))
  ;; return the new default as the name you read
  (eval default-name-var)))

(defun ogonek-read-prefix (prompt default-prefix-var)
  "Read a prefix character for prefix notation.
The result is stored in the variable DEFAULT-PREFIX-VAR.
PROMPT is a string to be shown when the user is asked for a new prefix."
  (let ((prefix-string
         (read-string
          (format "%s (default %s): " prompt
                  (char-to-string (eval default-prefix-var))))))
    (if (> (length prefix-string) 1)
        (error "! Only one character expected")
      ;; set the default prefix character to the one just read
      (set default-prefix-var
           (if (string= prefix-string "")
             (eval default-prefix-var)
           (string-to-char prefix-string)))
      ;; the new default prefix is the function's result:
      (eval default-prefix-var))))

(defun ogonek-lookup-encoding (encoding)
  "Pick up an association for ENCODING in `ogonek-name-encoding-alist'.
Before returning a result test whether the string ENCODING is in
the list `ogonek-name-encoding-alist'"
  (let ((code-list (assoc encoding ogonek-name-encoding-alist)))
    (if (null code-list)
      (error "! Name `%s' not known in `ogonek-name-encoding-alist'"
               encoding)
      (cdr code-list))))

;; ----  An auxiliary function for zipping two lists of equal length ----

(defun ogonek-zip-lists (xs ys)
  "Build a list of pairs from lists XS and YS of the same length."
  (let ((pairs nil))
    (while xs
      (setq pairs (cons (cons (car xs) (car ys)) pairs))
      (setq xs (cdr xs))
      (setq ys (cdr ys)))
    ;; `pairs' are the function's result
    pairs))

;; ---- An auxiliary function building a one-to-one recoding table -----

(defun ogonek-build-table (recoding-pairs)
  "Build a table required by Emacs's `translate-region' function.
RECODING-PAIRS is a list of character pairs for which recoding
is not an identity.
By using the built-in `translate-region' function
we gain better performance compared to converting characters
by a hand-written routine as it is done for prefix encodings."
  (let ((table (make-string 256 0))
        (i 0))
    (while (< i 256)
      (aset table i i)
      (setq i (1+ i)))
    ;; make changes in `table' according to `recoding-pairs'
    (while recoding-pairs
      (aset table (car (car recoding-pairs)) (cdr (car recoding-pairs)))
      (setq recoding-pairs (cdr recoding-pairs)))
    ;; return the table just built
    table))

;; ---- Commands for one-to-one recoding -------------------------------

(defun ogonek-recode-region (start end from-encoding to-encoding)
  "Recode text in a marked region in one-to-one manner.
When called interactively ask the user for the names of the FROM-
and TO- encodings."
  (interactive (progn (barf-if-buffer-read-only)
                (list
                (region-beginning)
                (region-end)
                (ogonek-read-encoding "From code" 'ogonek-from-encoding)
                (ogonek-read-encoding "To code" 'ogonek-to-encoding))))
  (save-excursion
    (translate-region
     start end
     (ogonek-build-table
      (ogonek-zip-lists
       (ogonek-lookup-encoding from-encoding)
       (ogonek-lookup-encoding to-encoding))))))

(defun ogonek-recode-buffer (from-encoding to-encoding)
  "Call `ogonek-recode-region' on the entire buffer.
When called interactively ask the user for the names of the FROM-
and TO- encodings."
  (interactive (progn (barf-if-buffer-read-only)
                (list
                (ogonek-read-encoding "From code" 'ogonek-from-encoding)
                (ogonek-read-encoding "To code" 'ogonek-to-encoding))))
  (ogonek-recode-region
   (point-min) (point-max) from-encoding to-encoding))

;; ---- Recoding with prefix notation -------------------------------

(defconst ogonek-prefix-code '(?A  ?C  ?E  ?L  ?N  ?O  ?S  ?X  ?Z
                               ?a  ?c  ?e  ?l  ?n  ?o  ?s  ?x  ?z))

(defun ogonek-prefixify-region (start end from-encoding prefix-char)
  "In a region, replace FROM-encoded Polish characters with PREFIX pairs.
A PREFIX pair generated consists of PREFIX-CHAR and the respective
character listed in the `ogonek-prefix-code' constant.
PREFIX-CHAR itself gets doubled."
  (interactive (progn (barf-if-buffer-read-only)
    (list
    (region-beginning)
    (region-end)
    (ogonek-read-encoding "From code" 'ogonek-prefix-from-encoding)
    (ogonek-read-prefix "Prefix character" 'ogonek-prefix-char))))
  (let*
      ((from-code (ogonek-lookup-encoding from-encoding))
       (to-code ogonek-prefix-code)
       (recoding-pairs  ; `ogonek-prefix-char' added for doubling
        (ogonek-zip-lists
         (cons prefix-char from-code)
         (cons prefix-char to-code))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((pair (assoc (following-char) recoding-pairs)))
          (if (null pair)
              ;; not a Polish character -- skip it
              (forward-char 1)
            ;; Polish character -- insert a prefix pair instead
            (delete-char 1)
            (insert ogonek-prefix-char)
            (insert (cdr pair))
            ;; the region is now one character longer
            (setq end (1+ end))))))))

(defun ogonek-prefixify-buffer (from-encoding prefix-char)
  "Call `ogonek-prefixify-region' on the entire buffer."
  (interactive (progn (barf-if-buffer-read-only)
    (list
     (ogonek-read-encoding "From code" 'ogonek-prefix-from-encoding)
     (ogonek-read-prefix "Prefix character" 'ogonek-prefix-char))))
  (ogonek-prefixify-region
   (point-min) (point-max) from-encoding prefix-char))

(defun ogonek-deprefixify-region (start end prefix-char to-encoding)
  "In a region, replace PREFIX pairs with their corresponding TO-encodings.
PREFIX-CHAR followed by a Polish character from the `ogonek-prefix-code'
list is replaced with the corresponding TO-encoded character. A doubled
PREFIX-CHAR gets replaced with a single one. A combination of PREFIX-CHAR
followed by a non-Polish character, that is one not listed in the
`ogonek-prefix-code' constant, is left unchanged."
  (interactive (progn (barf-if-buffer-read-only)
                (list (region-beginning)
                      (region-end)
                      (ogonek-read-prefix
                        "Prefix character" 'ogonek-prefix-char)
                      (ogonek-read-encoding
                       "To code" 'ogonek-prefix-to-encoding))))
  (let*
      ((from-code ogonek-prefix-code)
       (to-code (ogonek-lookup-encoding to-encoding))
       (recoding-pairs
        (ogonek-zip-lists
          (cons prefix-char from-code)
          (cons prefix-char to-code))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (forward-char 1)
       (if (or (not (= (preceding-char) prefix-char)) (= (point) end))
           ;; non-prefix character or the end-of-region -- do nothing
           ()
         ;; now, we can check the next character
         (let ((pair (assoc (following-char) recoding-pairs)))
           (if (null pair)
               ;; `following-char' is not a Polish character nor it is
               ;;  `prefix-char' since the one is among `recoding-pairs'
               (forward-char 1)
           ;; else prefix followed by a Polish character has been found
           ;; replace it by the corresponding Polish character
           (backward-char 1)
           (delete-char 2)
           (insert (cdr pair))
           ;; the region got shorter by one character
           (setq end (1- end)))))))))

(defun ogonek-deprefixify-buffer (prefix-char to-encoding)
  "Call `ogonek-deprefixify-region' on the entire buffer."
  (interactive (progn (barf-if-buffer-read-only)
    (list
     (ogonek-read-prefix "Prefix character" 'ogonek-prefix-char)
     (ogonek-read-encoding "To code" 'ogonek-prefix-to-encoding))))
  (ogonek-deprefixify-region
   (point-min) (point-max) prefix-char to-encoding))

(provide 'ogonek)

;;; ogonek.el ends here
