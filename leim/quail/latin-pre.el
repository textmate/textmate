;;; latin-pre.el --- Quail packages for inputting various European characters  -*-coding: utf-8;-*-

;; Copyright (C) 1997-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: mule, multilingual, latin, input method

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

;; Key translation maps were originally copied from iso-acc.el.
;; latin-1-prefix: extra special characters added, adapted from the vim
;;                 digraphs (from J.H.M.Dassen <jdassen@wi.leidenuniv.nl>)
;;                 by R.F. Smith <rsmith@xs4all.nl>
;;
;; polish-slash:
;; Author: Włodek Bzyl <matwb@univ.gda.pl>
;; Maintainer: Włodek Bzyl <matwb@univ.gda.pl>
;;
;; latin-[89]-prefix: Dave Love <fx@gnu.org>

;; You might make extra input sequences on the basis of the X
;; locale/*/Compose files (which have both prefix and postfix
;; sequences), but bear in mind that sequences which are logical in
;; that context may not be sensible when they're not signaled with
;; the Compose key.  An example is a double space for NBSP.

;;; Code:

(require 'quail)

(quail-define-package
 "latin-1-prefix" "Latin-1" "1>" t
 "Latin-1 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á, '' -> ´
    grave    |   `    | `a -> à
  circumflex |   ^    | ^a -> â
  diaeresis  |   \"    | \"a -> ä  \"\" -> ¨
    tilde    |   ~    | ~a -> ã
   cedilla   |   ~    | ~c -> ç
    misc     | \" ~ /  | \"s -> ß  ~d -> ð  ~t -> þ  /a -> å  /e -> æ  /o -> ø
   symbol    |   ~    | ~> -> »  ~< -> «  ~! -> ¡  ~? -> ¿  ~~ -> ¸
             |   ~    | ~s -> §  ~x -> ¤  ~. -> ·  ~$ -> £  ~u -> µ
             |   ~    | ~p -> ¶  ~- -> ­  ~= -> ¯  ~| -> ¦
   symbol    |  _ /   | _o -> º  _a -> ª  // -> °  /\\ -> ×  _y -> ¥
             |  _ /   | _: -> ÷  /c -> ¢  /2 -> ½  /4 -> ¼  /3 -> ¾
             |  _ /   | /= -> ¬
   symbol    |   ^    | ^r -> ®  ^c -> ©  ^1 -> ¹  ^2 -> ²  ^3 -> ³
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'Y" ?Ý)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("'y" ?ý)
 ("''" ?´)
 ("' " ?')
 ("`A" ?À)
 ("`E" ?È)
 ("`I" ?Ì)
 ("`O" ?Ò)
 ("`U" ?Ù)
 ("`a" ?à)
 ("`e" ?è)
 ("`i" ?ì)
 ("`o" ?ò)
 ("`u" ?ù)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?Â)
 ("^E" ?Ê)
 ("^I" ?Î)
 ("^O" ?Ô)
 ("^U" ?Û)
 ("^a" ?â)
 ("^e" ?ê)
 ("^i" ?î)
 ("^o" ?ô)
 ("^u" ?û)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?Ä)
 ("\"E" ?Ë)
 ("\"I" ?Ï)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"a" ?ä)
 ("\"e" ?ë)
 ("\"i" ?ï)
 ("\"o" ?ö)
 ("\"s" ?ß)
 ("\"u" ?ü)
 ("\"y" ?ÿ)
 ("\"\"" ?¨)
 ("\" " ?\")
 ("~A" ?Ã)
 ("~C" ?Ç)
 ("~D" ?Ð)
 ("~N" ?Ñ)
 ("~O" ?Õ)
 ("~T" ?Þ)
 ("~a" ?ã)
 ("~c" ?ç)
 ("~d" ?ð)
 ("~n" ?ñ)
 ("~o" ?õ)
 ("~t" ?þ)
 ("~>" ?\»)
 ("~<" ?\«)
 ("~!" ?¡)
 ("~?" ?¿)
 ("~~" ?¸)
 ("~ " ?~)
 ("/A" ?Å)
 ("/E" ?Æ)
 ("/O" ?Ø)
 ("/a" ?å)
 ("/e" ?æ)
 ("/o" ?ø)
 ("//" ?°)
 ("/ " ?/)
 ("_o" ?º)
 ("_a" ?ª)
 ("_ " ? )
;; Symbols added by Roland Smith <rsmith@xs4all.nl>
 ("_+" ?±)
 ("_y" ?¥)
 ("_:" ?÷)
 ("__" ?_)
 ("/c" ?¢)
 ("/\\" ?×)
 ("/2" ?½)
 ("/4" ?¼)
 ("/3" ?¾)
 ("~s" ?§)
 ("~p" ?¶)
 ("~x" ?¤)
 ("~." ?·)
 ("~$" ?£)
 ("~u" ?µ)
 ("^r" ?®)
 ("^c" ?©)
 ("^1" ?¹)
 ("^2" ?²)
 ("^3" ?³)
 ("~-" ?­)
 ("~|" ?¦)
 ("/=" ?¬)
 ("~=" ?¯)
)

(quail-define-package
 "catalan-prefix" "Latin-1" "CA>" t
 "Catalan and Spanish input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á   '' -> ´
    grave    |   `    | `a -> à
  diaeresis  |   \"    | \"i -> ï   \"\" -> ¨
    tilde    |   ~    | ~n -> ñ
   cedilla   |   ~    | ~c -> ç
   symbol    |   ~    | ~> -> »   ~< -> «   ~! -> ¡   ~? -> ¿
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("' " ?')
 ("`A" ?À)
 ("`E" ?È)
 ("`O" ?Ò)
 ("`a" ?à)
 ("`e" ?è)
 ("`o" ?ò)
 ("` " ?`)
 ("\"I" ?Ï)
 ("\"U" ?Ü)
 ("\"i" ?ï)
 ("\"u" ?ü)
 ("\" " ?\")
 ("~C" ?Ç)
 ("~N" ?Ñ)
 ("~c" ?ç)
 ("~n" ?ñ)
 ("~>" ?\»)
 ("~<" ?\«)
 ("~!" ?¡)
 ("~?" ?¿)
 ("~ " ?~)
)

(quail-define-package
 "esperanto-prefix" "Latin-3" "EO>" t
 "Esperanto input method with prefix modifiers
Key translation rules are:
 ^H -> ?Ĥ   ^J -> ?Ĵ   ^h -> ?ĥ   ^j -> ?ĵ   ^C -> ?Ĉ   ^G -> ?Ĝ,
 ^S -> ?Ŝ   ^c -> ?ĉ   ^g -> ?ĝ   ^s -> ?ŝ   ~U -> ?Ŭ   ~u -> ?ŭ
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("^H" ?Ĥ)
 ("^J" ?Ĵ)
 ("^h" ?ĥ)
 ("^j" ?ĵ)
 ("^C" ?Ĉ)
 ("^G" ?Ĝ)
 ("^S" ?Ŝ)
 ("^c" ?ĉ)
 ("^g" ?ĝ)
 ("^s" ?ŝ)
 ("^^" ?^)
 ("^ " ?^)
 ("~U" ?Ŭ)
 ("~u" ?ŭ)
 ("~ " ?~)
)

(quail-define-package
 "french-prefix" "French" "FR>" t
 "French (Français) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'e -> é
    grave    |   `    | `a -> à
  circumflex |   ^    | ^a -> â
  diaeresis  |   \"    | \"i -> ï
   cedilla   | ~ or , | ~c -> ç   ,c -> ç
   symbol    |   ~    | ~> -> »   ~< -> «
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'E" ?É)
 ("'C" ?Ç)
 ("'e" ?é)
 ("'c" ?ç)
 ("' " ?')
 ("`A" ?À)
 ("`E" ?È)
 ("`U" ?Ù)
 ("`a" ?à)
 ("`e" ?è)
 ("`u" ?ù)
 ("` " ?`)
 ("^A" ?Â)
 ("^E" ?Ê)
 ("^I" ?Î)
 ("^O" ?Ô)
 ("^U" ?Û)
 ("^a" ?â)
 ("^e" ?ê)
 ("^i" ?î)
 ("^o" ?ô)
 ("^u" ?û)
 ("^ " ?^)
 ("\"E" ?Ë)
 ("\"I" ?Ï)
 ("\"e" ?ë)
 ("\"i" ?ï)
 ("\" " ?\")
 ("~<" ?\«)
 ("~>" ?\»)
 ("~C" ?Ç)
 ("~c" ?ç)
 ("~ " ?~)
 (",C" ?Ç)
 (",c" ?ç)
 (", " ?,)
)

(quail-define-package
 "romanian-prefix" "Romanian" "RO>" t
 "Romanian (româneşte) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+------------------
    tilde    |   ~    | ~a -> ă
  circumflex |   ^    | ^a -> â, ^i -> î
   cedilla   |   ,    | ,s -> ş, ,t -> ţ
   ~         |   ~    | ~~ -> ~
   ^         |   ^    | ^^ -> ^
   ,         |   ,    | ,, -> ,
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("~A" ?Ă) ("~a" ?ă)
 ("^A" ?Â) ("^a" ?â)
 ("^I" ?Î) ("^i" ?î)
 (",S" ?Ş) (",s" ?ş)
 (",T" ?Ţ) (",t" ?ţ)
 ("^^" ?^) ("~~" ?~) (",," ?,))

(quail-define-package
 "romanian-alt-prefix" "Romanian" "RO>" t
 "Alternative Romanian (româneşte) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+------------------
    tilde    |   \"    | \"a -> â
  circumflex |   '    | 'a -> â, 'i -> î
   cedilla   |   '    | 's -> ş, 't -> ţ
   '         |   '    | '' -> '
   \"         |   \"    | \"\" -> \"
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Ă) ("'a" ?ă)
 ("\"A" ?Â) ("\"a" ?â)
 ("'I" ?Î) ("'i" ?î)
 ("'S" ?Ş) ("'s" ?ş)
 ("'T" ?Ţ) ("'t" ?ţ)
 ("''" ?') ("\"\"" ?\"))

(quail-define-package
 "german-prefix" "German" "DE>" t
 "German (Deutsch) input method with prefix modifiers
Key translation rules are:
 \"A -> Ä ->   \"O -> Ö   \"U -> Ü   \"s -> ß
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("\"A" ?Ä)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"a" ?ä)
 ("\"o" ?ö)
 ("\"u" ?ü)
 ("\"s" ?ß)
 ("\" " ?\")
)

(quail-define-package
 "irish-prefix" "Latin-1" "GA>" t
 "Irish input method with prefix modifiers
Key translation rules are:
 'A -> Á   'E -> É   'I -> Í   'O -> Ó   'U -> Ú
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("' " ?')
)

(quail-define-package
 "portuguese-prefix" "Latin-1" "PT>" t
 "Portuguese input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á   '' -> ´
    grave    |   `    | `a -> à
  circumflex |   ^    | ^a -> â
  diaeresis  |   \"    | \"u -> ü
    tilde    |   ~    | ~a -> ã
   cedilla   | ' or , | 'c -> ç   ,c -> ç
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'C" ?Ç)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("'c" ?ç)
 ("' " ?')
 ("`A" ?À)
 ("`a" ?à)
 ("` " ?`)
 ("^A" ?Â)
 ("^E" ?Ê)
 ("^O" ?Ô)
 ("^a" ?â)
 ("^e" ?ê)
 ("^o" ?ô)
 ("^ " ?^)
 ("\"U" ?Ü)
 ("\"u" ?ü)
 ("\" " ?\")
 ("~A" ?Ã)
 ("~O" ?Õ)
 ("~a" ?ã)
 ("~o" ?õ)
 ("~ " ?~)
 (",c" ?ç)
 (",C" ?Ç)
 (",," ?,)
)

(quail-define-package
 "spanish-prefix" "Spanish" "ES>" t
 "Spanish (Español) input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á
  diaeresis  |   \"    | \"u -> ü
    tilde    |   ~    | ~n -> ñ
   symbol    |   ~    | ~> -> »   ~< -> «   ~! -> ¡   ~? -> ¿
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("' " ?')
 ("\"U" ?Ü)
 ("\"u" ?ü)
 ("\" " ?\")
 ("~N" ?Ñ)
 ("~n" ?ñ)
 ("~>" ?\»)
 ("~<" ?\«)
 ("~!" ?¡)
 ("~?" ?¿)
 ("~ " ?~)
)

(quail-define-package
 "latin-2-prefix" "Latin-2" "2>" t
 "Latin-2 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á   '' -> ?´
  circumflex |   ^    | ^a -> â
  diaeresis  |   \"    | \"a -> ä   \"\" -> ¨
    breve    |   ~    | ~a -> ă
    caron    |   ~    | ~c -> č
   cedilla   |   `    | `c -> ç   `e -> ?ę
    misc     | ' ` ~  | 'd -> đ   `l -> ł   `z -> ż   ~o -> ő   ~u -> ű
   symbol    |   ~    | `. -> ˙   ~~ -> ˘   ~. -> ?¸
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'C" ?Ć)
 ("'D" ?Đ)
 ("'E" ?É)
 ("'I" ?Í)
 ("'L" ?Ĺ)
 ("'N" ?Ń)
 ("'O" ?Ó)
 ("'R" ?Ŕ)
 ("'S" ?Ś)
 ("'U" ?Ú)
 ("'Y" ?Ý)
 ("'Z" ?Ź)
 ("'a" ?á)
 ("'c" ?ć)
 ("'d" ?đ)
 ("'e" ?é)
 ("'i" ?í)
 ("'l" ?ĺ)
 ("'n" ?ń)
 ("'o" ?ó)
 ("'r" ?ŕ)
 ("'s" ?ś)
 ("'u" ?ú)
 ("'y" ?ý)
 ("'z" ?ź)
 ("''" ?´)
 ("' " ?')
 ("`A" ?Ą)
 ("`C" ?Ç)
 ("`E" ?Ę)
 ("`L" ?Ł)
 ("`S" ?Ş)
 ("`T" ?Ţ)
 ("`Z" ?Ż)
 ("`a" ?ą)
 ("`l" ?ł)
 ("`c" ?ç)
 ("`e" ?ę)
 ("`s" ?ş)
 ("`t" ?ţ)
 ("`z" ?ż)
 ("``" ?Ş)
 ("`." ?˙)
 ("` " ?`)
 ("^A" ?Â)
 ("^I" ?Î)
 ("^O" ?Ô)
 ("^a" ?â)
 ("^i" ?î)
 ("^o" ?ô)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?Ä)
 ("\"E" ?Ë)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"a" ?ä)
 ("\"e" ?ë)
 ("\"o" ?ö)
 ("\"s" ?ß)
 ("\"u" ?ü)
 ("\"\"" ?¨)
 ("\" " ?\")
 ("~A" ?Ă)
 ("~C" ?Č)
 ("~D" ?Ď)
 ("~E" ?Ě)
 ("~L" ?Ľ)
 ("~N" ?Ň)
 ("~O" ?Ő)
 ("~R" ?Ř)
 ("~S" ?Š)
 ("~T" ?Ť)
 ("~U" ?Ű)
 ("~Z" ?Ž)
 ("~a" ?ă)
 ("~c" ?č)
 ("~d" ?ď)
 ("~e" ?ě)
 ("~l" ?ľ)
 ("~n" ?ň)
 ("~o" ?ő)
 ("~r" ?ř)
 ("~s" ?š)
 ("~t" ?ť)
 ("~u" ?ű)
 ("~z" ?ž)
 ("~v" ?˘)
 ("~~" ?˘)
 ("~." ?¸)
 ("~ " ?~)
)

(quail-define-package
 "latin-3-prefix" "Latin-3" "3>" t
 "Latin-3 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á   '' -> ?´
    grave    |   `    | `a -> à
  circumflex |   ^    | ^a -> â
  diaeresis  |   \"    | \"a -> ä   \"\" -> ¨
   cedilla   |   ~    | ~c -> ç   ~s -> ş   ~~ -> ¸
  dot above  |   / .  | /g -> ġ   .o -> ġ
    misc     | \" ~ /  | \"s -> ß   ~g -> ğ   ~u -> ŭ   /h -> ħ   /i -> ı
   symbol    |   ~    | ~` -> ˘   /# -> £   /$ -> ¤   // -> °
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("''" ?´)
 ("' " ?')
 ("`A" ?À)
 ("`E" ?È)
 ("`I" ?Ì)
 ("`O" ?Ò)
 ("`U" ?Ù)
 ("`a" ?à)
 ("`e" ?è)
 ("`i" ?ì)
 ("`o" ?ò)
 ("`u" ?ù)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?Â)
 ("^C" ?Ĉ)
 ("^E" ?Ê)
 ("^G" ?Ĝ)
 ("^H" ?Ĥ)
 ("^I" ?Î)
 ("^J" ?Ĵ)
 ("^O" ?Ô)
 ("^S" ?Ŝ)
 ("^U" ?Û)
 ("^a" ?â)
 ("^c" ?ĉ)
 ("^e" ?ê)
 ("^g" ?ĝ)
 ("^h" ?ĥ)
 ("^i" ?î)
 ("^j" ?ĵ)
 ("^o" ?ô)
 ("^s" ?ŝ)
 ("^u" ?û)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?Ä)
 ("\"E" ?Ë)
 ("\"I" ?Ï)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"a" ?ä)
 ("\"e" ?ë)
 ("\"i" ?ï)
 ("\"o" ?ö)
 ("\"u" ?ü)
 ("\"s" ?ß)
 ("\"\"" ?¨)
 ("\" " ?\")
 ("~C" ?Ç)
 ("~N" ?Ñ)
 ("~c" ?ç)
 ("~n" ?ñ)
 ("~S" ?Ş)
 ("~s" ?ş)
 ("~G" ?Ğ)
 ("~g" ?ğ)
 ("~U" ?Ŭ)
 ("~u" ?ŭ)
 ("~`" ?˘)
 ("~~" ?¸)
 ("~ " ?~)
 ("/C" ?Ċ)
 ("/G" ?Ġ)
 ("/H" ?Ħ)
 ("/I" ?İ)
 ("/Z" ?Ż)
 ("/c" ?ċ)
 ("/g" ?ġ)
 ("/h" ?ħ)
 ("/i" ?ı)
 ("/z" ?ż)
 ("/." ?˙)
 ("/#" ?£)
 ("/$" ?¤)
 ("//" ?°)
 ("/ " ?/)
 (".C" ?Ċ)
 (".G" ?Ġ)
 (".I" ?İ)
 (".Z" ?Ż)
 (".c" ?ċ)
 (".g" ?ġ)
 (".z" ?ż)
)


(quail-define-package
 "polish-slash" "Polish" "PL>" nil
 "Polish diacritics and slash character are input as `/[acelnosxzACELNOSXZ/]'.
For example, the character named `aogonek' is obtained by `/a'."
 nil t t t nil nil nil nil nil nil t)

(quail-define-rules
 ("//" ?/)
 ("/a" ?ą)
 ("/c" ?ć)
 ("/e" ?ę)
 ("/l" ?ł)
 ("/n" ?ń)
 ("/o" ?ó)
 ("/s" ?ś)
 ("/x" ?ź)
 ("/z" ?ż)
 ("/A" ?Ą)
 ("/C" ?Ć)
 ("/E" ?Ę)
 ("/L" ?Ł)
 ("/N" ?Ń)
 ("/O" ?Ó)
 ("/S" ?Ś)
 ("/X" ?Ź)
 ("/Z" ?Ż))

(quail-define-package
 "latin-9-prefix" "Latin-9" "0>" t
 "Latin-9 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á
    grave    |   `    | `a -> à
  circumflex |   ^    | ^a -> â
  diaeresis  |   \"    | \"a -> ä, \"Y -> Ÿ
    tilde    |   ~    | ~a -> ã
    caron    |   ~    | ~z -> ž
   cedilla   |   ~    | ~c -> ç
    misc     | \" ~ /  | \"s -> ß  ~d -> ð  ~t -> þ  /a -> å  /e -> æ  /o -> ø
             | \" ~ /  | /o -> œ
   symbol    |   ~    | ~> -> »  ~< -> «  ~! -> ¡  ~? -> ¿  ~~ -> ž
             |   ~    | ~s -> §  ~e -> €  ~. -> ·  ~$ -> £  ~u -> µ
             |   ~    | ~- -> ­  ~= -> ¯
   symbol    |  _ /   | _o -> º  _a -> ª  // -> °  /\\ -> ×  _y -> ¥
             |  _ /   | _: -> ÷  /c -> ¢  ~p -> ¶
             |  _ /   | /= -> ¬
   symbol    |   ^    | ^r -> ®  ^c -> ©  ^1 -> ¹  ^2 -> ²  ^3 -> ³  _a -> ª
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'Y" ?Ý)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("'y" ?ý)
 ("' " ?')
 ("`A" ?À)
 ("`E" ?È)
 ("`I" ?Ì)
 ("`O" ?Ò)
 ("`U" ?Ù)
 ("`a" ?à)
 ("`e" ?è)
 ("`i" ?ì)
 ("`o" ?ò)
 ("`u" ?ù)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?Â)
 ("^E" ?Ê)
 ("^I" ?Î)
 ("^O" ?Ô)
 ("^U" ?Û)
 ("^a" ?â)
 ("^e" ?ê)
 ("^i" ?î)
 ("^o" ?ô)
 ("^u" ?û)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?Ä)
 ("\"E" ?Ë)
 ("\"I" ?Ï)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"a" ?ä)
 ("\"e" ?ë)
 ("\"i" ?ï)
 ("\"o" ?ö)
 ("\"s" ?ß)
 ("\"u" ?ü)
 ("\"y" ?ÿ)
 ("\" " ?\")
 ("~A" ?Ã)
 ("~C" ?Ç)
 ("~D" ?Ð)
 ("~N" ?Ñ)
 ("~O" ?Õ)
 ("~S" ?Š)
 ("~T" ?Þ)
 ("~Z" ?Ž)
 ("~a" ?ã)
 ("~c" ?ç)
 ("~d" ?ð)
 ("~n" ?ñ)
 ("~o" ?õ)
 ("~s" ?š)
 ("~t" ?þ)
 ("~z" ?ž)
 ("~>" ?\»)
 ("~<" ?\«)
 ("~!" ?¡)
 ("~?" ?¿)
 ("~ " ?~)
 ("/A" ?Å)
 ("/E" ?Æ)
 ("/O" ?Ø)
 ("/a" ?å)
 ("/e" ?æ)
 ("/o" ?ø)
 ("//" ?°)
 ("/ " ?/)
 ("_o" ?º)
 ("_a" ?ª)
 ("_+" ?±)
 ("_y" ?¥)
 ("_:" ?÷)
 ("_ " ? )
 ("__" ?_)
 ("/c" ?¢)
 ("/\\" ?×)
 ("/o" ?œ)				; clash with ø, but æ uses /
 ("/O" ?Œ)
 ("\"Y" ?Ÿ)
 ("~s" ?§)
 ("~p" ?¶)
 ;; Is this the best option for Euro entry?
 ("~e" ?€)
 ("~." ?·)
 ("~$" ?£)
 ("~u" ?µ)
 ("^r" ?®)
 ("^c" ?©)
 ("^1" ?¹)
 ("^2" ?²)
 ("^3" ?³)
 ("~-" ?­)
 ("~=" ?¯)
 ("/=" ?¬))

;; Latin-8 was done by an Englishman -- Johnny Celt should take a
;; squint at it.

(quail-define-package
 "latin-8-prefix" "Latin-8" "8>" t
 "Latin-8 characters input method with prefix modifiers

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á
    grave    |   `    | `a -> à
  circumflex |   ^    | ^w -> ŵ
  diaeresis  |   \"    | \"a -> ä
  dot above  |   .    | .b -> ḃ
    tilde    |   ~    | ~a -> ã
   cedilla   |   ~    | ~c -> ç
    misc     | \" ~ /  | \"s -> ß   /a -> å  /e -> æ  /o -> ø
             |   ~    | ~s -> §  ~$ -> £  ~p -> ¶
   symbol    |   ^    | ^r -> ®  ^c -> ©
" nil t nil nil nil nil nil nil nil nil t)

;; Basically following Latin-1, plus dottiness from Latin-3.
(quail-define-rules
 (".B" ?Ḃ)
 (".b" ?ḃ)
 (".c" ?ċ)
 (".C" ?Ċ)
 (".D" ?Ḋ)
 (".d" ?ḋ)
 (".f" ?ḟ)
 (".F" ?Ḟ)
 (".g" ?ġ)
 (".G" ?Ġ)
 (".m" ?ṁ)
 (".M" ?Ṁ)
 (".p" ?ṗ)
 (".P" ?Ṗ)
 (".s" ?ṡ)
 (".S" ?Ṡ)
 (".t" ?ṫ)
 (".T" ?Ṫ)
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'Y" ?Ý)
 ("'W" ?Ẃ)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("'w" ?ẃ)
 ("'y" ?ý)
 ("' " ?')
 ("`A" ?À)
 ("`E" ?È)
 ("`I" ?Ì)
 ("`O" ?Ò)
 ("`U" ?Ù)
 ("`W" ?Ẁ)
 ("`Y" ?Ỳ)
 ("`a" ?à)
 ("`e" ?è)
 ("`i" ?ì)
 ("`o" ?ò)
 ("`u" ?ù)
 ("`w" ?ẁ)
 ("`y" ?ỳ)
 ("``" ?`)
 ("` " ?`)
 ("^A" ?Â)
 ("^E" ?Ê)
 ("^I" ?Î)
 ("^O" ?Ô)
 ("^U" ?Û)
 ("^a" ?â)
 ("^e" ?ê)
 ("^i" ?î)
 ("^o" ?ô)
 ("^u" ?û)
 ("^w" ?ŵ)
 ("^W" ?Ŵ)
 ("^y" ?ŷ)
 ("^Y" ?Ŷ)
 ("^^" ?^)
 ("^ " ?^)
 ("\"A" ?Ä)
 ("\"E" ?Ë)
 ("\"I" ?Ï)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"a" ?ä)
 ("\"e" ?ë)
 ("\"i" ?ï)
 ("\"o" ?ö)
 ("\"s" ?ß)
 ("\"u" ?ü)
 ("\"w" ?ẅ)
 ("\"W" ?Ẅ)
 ("\"y" ?ÿ)
 ("\"Y" ?Ÿ)
 ("\" " ?\")
 ("~A" ?Ã)
 ("~C" ?Ç)
 ("~N" ?Ñ)
 ("~O" ?Õ)
 ("~a" ?ã)
 ("~c" ?ç)
 ("~n" ?ñ)
 ("~o" ?õ)
 ("~ " ?~)
 ("/A" ?Å)
 ("/E" ?Æ)
 ("/O" ?Ø)
 ("/a" ?å)
 ("/e" ?æ)
 ("/o" ?ø)
 ("/ " ?/)
 ("~p" ?¶)
 ("~s" ?§)
 ("~$" ?£)
 ("^r" ?®)
 ("^c" ?©))

(quail-define-package
 "latin-prefix" "Latin" "L>" t
 "Latin characters input method with prefix modifiers.
This is the union of various input methods originally made for input
of characters from a single Latin-N charset.

    effect   | prefix | examples
 ------------+--------+----------
    acute    |   '    | 'a -> á, '' -> ´
    grave    |   `    | `a -> à
  circumflex |   ^    | ^a -> â
  diaeresis  |   \"    | \"a -> ä  \"\" -> ¨
    tilde    |   ~    | ~a -> ã
   cedilla   |   ~    | ~c -> ç
    breve    |   ~    | ~a -> ă
    caron    |   ~    | ~c -> č
  dot above  | ~ / .  | ~o -> ġ   /o -> ġ   .o -> ġ
    misc     | \" ~ /  | \"s -> ß  ~d -> ð  ~t -> þ  /a -> å  /e -> æ  /o -> ø
   symbol    |   ~    | ~> -> »  ~< -> «  ~! -> ¡  ~? -> ¿  ~~ -> ¸
   symbol    |  _ /   | _o -> º  _a -> ª  // -> °  /\\ -> ×  _y -> ¥
   symbol    |   ^    | ^r -> ®  ^c -> ©  ^1 -> ¹  ^2 -> ²  ^3 -> ³
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("' " ?')
 ("''" ?´)
 ("'A" ?Á)
 ("'E" ?É)
 ("'I" ?Í)
 ("'O" ?Ó)
 ("'U" ?Ú)
 ("'W" ?Ẃ)
 ("'Y" ?Ý)
 ("'a" ?á)
 ("'e" ?é)
 ("'i" ?í)
 ("'o" ?ó)
 ("'u" ?ú)
 ("'w" ?ẃ)
 ("'y" ?ý)
 (".B" ?Ḃ)
 (".C" ?Ċ)
 (".D" ?Ḋ)
 (".F" ?Ḟ)
 (".G" ?Ġ)
 (".I" ?İ)
 (".M" ?Ṁ)
 (".P" ?Ṗ)
 (".S" ?Ṡ)
 (".T" ?Ṫ)
 (".Z" ?Ż)
 (".b" ?ḃ)
 (".c" ?ċ)
 (".d" ?ḋ)
 (".f" ?ḟ)
 (".g" ?ġ)
 (".m" ?ṁ)
 (".p" ?ṗ)
 (".s" ?ṡ)
 (".t" ?ṫ)
 (".z" ?ż)
 ("/ " ?/)
 ("/#" ?£)
 ("/$" ?¤)
 ("/." ?˙)
 ("//" ?°)
 ("/2" ?½)
 ("/3" ?¾)
 ("/4" ?¼)
 ("/=" ?¬)
 ("/A" ?Å)
 ("/C" ?Ċ)
 ("/E" ?Æ)
 ("/G" ?Ġ)
 ("/H" ?Ħ)
 ("/I" ?İ)
 ("/O" ?Ø)
 ("/O" ?Œ)
 ("/Z" ?Ż)
 ("/\\" ?×)
 ("/a" ?å)
 ("/c" ?¢)
 ("/c" ?ċ)
 ("/e" ?æ)
 ("/g" ?ġ)
 ("/h" ?ħ)
 ("/i" ?ı)
 ("/o" ?ø)
 ("/o" ?œ)
 ("/z" ?ż)
 ("\" " ?\")
 ("\"A" ?Ä)
 ("\"E" ?Ë)
 ("\"I" ?Ï)
 ("\"O" ?Ö)
 ("\"U" ?Ü)
 ("\"W" ?Ẅ)
 ("\"Y" ?Ÿ)
 ("\"\"" ?¨)
 ("\"a" ?ä)
 ("\"e" ?ë)
 ("\"i" ?ï)
 ("\"o" ?ö)
 ("\"s" ?ß)
 ("\"u" ?ü)
 ("\"w" ?ẅ)
 ("\"y" ?ÿ)
 ("^ " ?^)
 ("^1" ?¹)
 ("^2" ?²)
 ("^3" ?³)
 ("^A" ?Â)
 ("^C" ?Ĉ)
 ("^E" ?Ê)
 ("^G" ?Ĝ)
 ("^H" ?Ĥ)
 ("^I" ?Î)
 ("^J" ?Ĵ)
 ("^O" ?Ô)
 ("^S" ?Ŝ)
 ("^U" ?Û)
 ("^W" ?Ŵ)
 ("^Y" ?Ŷ)
 ("^^" ?^)
 ("^a" ?â)
 ("^c" ?©)
 ("^c" ?ĉ)
 ("^e" ?ê)
 ("^g" ?ĝ)
 ("^h" ?ĥ)
 ("^i" ?î)
 ("^j" ?ĵ)
 ("^o" ?ô)
 ("^r" ?®)
 ("^s" ?ŝ)
 ("^u" ?û)
 ("^w" ?ŵ)
 ("^y" ?ŷ)
 ("_+" ?±)
 ("_:" ?÷)
 ("_a" ?ª)
 ("_o" ?º)
 ("_y" ?¥)
 ("_ " ? )
 ("` " ?`)
 ("`A" ?À)
 ("`E" ?È)
 ("`I" ?Ì)
 ("`O" ?Ò)
 ("`U" ?Ù)
 ("`W" ?Ẁ)
 ("`Y" ?Ỳ)
 ("``" ?`)
 ("`a" ?à)
 ("`e" ?è)
 ("`i" ?ì)
 ("`o" ?ò)
 ("`u" ?ù)
 ("`w" ?ẁ)
 ("`y" ?ỳ)
 ("~ " ?~)
 ("~!" ?¡)
 ("~$" ?£)
 ("~-" ?­)
 ("~." ?·)
 ("~<" ?\«)
 ("~=" ?¯)
 ("~>" ?\»)
 ("~?" ?¿)
 ("~A" ?Ã)
 ("~C" ?Ç)
 ("~D" ?Ð)
 ("~G" ?Ğ)
 ("~N" ?Ñ)
 ("~O" ?Õ)
 ("~O" ?Ġ)
 ("~S" ?Ş)
 ("~S" ?Š)
 ("~T" ?Þ)
 ("~U" ?Ŭ)
 ("~Z" ?Ž)
 ("~`" ?˘)
 ("~a" ?ã)
 ("~c" ?ç)
 ("~d" ?ð)
 ("~e" ?€)
 ("~g" ?ğ)
 ("~n" ?ñ)
 ("~o" ?õ)
 ("~o" ?ġ)
 ("~p" ?¶)
 ("~s" ?§)
 ("~s" ?ş)
 ("~s" ?š)
 ("~t" ?þ)
 ("~u" ?µ)
 ("~u" ?ŭ)
 ("~x" ?¤)
 ("~z" ?ž)
 ("~|" ?¦)
 ("~~" ?¸)
)

;;; latin-pre.el ends here
