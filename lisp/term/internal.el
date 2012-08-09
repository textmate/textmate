;;; internal.el --- support for PC internal terminal

;; Copyright (C) 1993-1994, 1998-1999, 2001-2012
;;   Free Software Foundation, Inc.

;; Author: Morten Welinder <terra@diku.dk>

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

;; ---------------------------------------------------------------------------
(defvar msdos-key-remapping-map
  (let ((map (make-sparse-keymap)))
    ;; keyboard setup -- that's simple!
    (define-key map [M-backspace] [?\M-\d])
    (define-key map [M-delete] [?\M-d])
    (define-key map [M-tab] [?\M-\t])
    (define-key map [M-linefeed] [?\M-\n])
    (define-key map [M-clear] [?\M-\013])
    (define-key map [M-return] [?\M-\015])
    (define-key map [M-escape] [?\M-\e])
    map)
  "Keymap for remapping special keys on MS-DOS keyboard.")

(defun msdos-setup-keyboard (frame)
  "Setup `local-function-key-map' for MS-DOS keyboard."
  ;; Don't do this twice on the same display, or it would break
  ;; normal-erase-is-backspace-mode.
  (unless (terminal-parameter frame 'msdos-setup-keyboard)
    ;; Map certain keypad keys into ASCII characters that people usually expect.
    (with-selected-frame frame
      (let ((map (copy-keymap msdos-key-remapping-map)))
        (set-keymap-parent map (keymap-parent local-function-key-map))
        (set-keymap-parent local-function-key-map map)))
    (set-terminal-parameter frame 'msdos-setup-keyboard t))
  (set-input-mode nil nil 0))

;; ----------------------------------------------------------------------
;;   DOS display setup
;;   =================
;;
;;   DOS can only support a single font.  On most systems (with the
;;   possible exception of Far Eastern DOS versions), this means that
;;   two character sets are available at any given time: the ASCII
;;   charset, and a single national charset, usually mapped to codes
;;   above 128 (i.e., with 8th bit set).  Which national charset is
;;   supported depends on the codepage loaded by the system when it
;;   boots; usually, this codepage cannot be changed without
;;   rebooting.
;;
;;   Since each codepage can usually display character of a single
;;   MULE charset, Emacs can display a single MULE charset with the
;;   glyphs of the current codepage.  When Emacs starts on DOS, it
;;   automatically sets its default coding systems for file I/O and
;;   terminal output according to the current DOS codepage, given by
;;   the `dos-codepage' variable.
;;
;;   This leaves us with the problem of displaying character sets
;;   other than the one which maps directly into the current codepage.
;;   The following functions and variables handle this nuisance by
;;   defining a display table where each character that doesn't have a
;;   glyph in some codepage is mapped to a string which represents it.
;;   For example, a small c with cedilla is mapped to the string
;;   ",c".  A nice feature of the display tables is that Emacs
;;   knows that the string represents a single character, and thus
;;   cursor motion works as you'd expect: a single `C-f' moves past
;;   the entire string which represents a single character.
;; ----------------------------------------------------------------------

(defvar IT-unicode-translations
  '(
    (160 563				; first, last
     [ 255 "!I" "|c" "Pd" "$$" "Ye" "|" "SE" "\"" "(c)" ; Latin-1
       "_a" "<<" "~" "--" "(R)" "'-" "^o" "+-" "^2" "^3"
       "'" "u" ".P" "^." "'," "^1" "_o" ">>" "1/4" "1/2"
       "3/4" "?I" "`A" "A'" "A^" "~A" "\"A" "Ao" "AE" ",C"
       "`E" "E'" "E^" "\"E" "`I" "I'" "I^" "\"I" "-D" "~N"
       "`O" "O'" "O^" "~O" "\"O" "*x" "/O" "`U" "U'" "U^"
       "\"U" "Y'" "-P" "ss" "`a" "a'" "a^" "~a" "\"a" "ao"
       "ae" ",c" "`e" "e'" "e^" "\"e" "`i" "i'" "i^" "\"i"
       "-d" "~n" "`o" "o'" "o^" "~o" "\"o" "-:" "/o" "`u"
       "u'" "u^" "\"u" "y'" "-p" "\"y" ; 255
       "A-" "a-" "A(" "a(" "A;" "a;" "C'" "c'" "C>" "c>" ; Latin Extended-A
       "C." "c." "C<" "c<" "D<" "d<" "D/" "d/" "E-" "e-"
       "E(" "e(" "E." "e." "E;" "e;" "E<" "e<" "G>" "g>"
       "G(" "g(" "G." "g." "G," "g," "H>" "h>" "H/" "h/"
       "I~" "i~" "I-" "i-" "I(" "i(" "I;" "i;" "I." "i."
       "IJ" "ij" "J>" "j>" "K," "k," "kk" "L'" "l'" "L,"
       "l," "L<" "l<" "L." "l." "L/" "l/" "N'" "n'" "N,"
       "n," "N<" "n<" "'n" "NG" "ng" "O-" "o-" "O(" "o("
       "O\"" "o\"" "OE" "oe" "R'" "r'" "R," "r," "R<" "r<"
       "S'" "s'" "S>" "s>" "S," "s," "S<" "s<" "T," "t,"
       "T<" "t<" "T/" "t/" "U~" "u~" "U-" "u-" "U(" "u("
       "U0" "u0" "U\"" "u\"" "U;" "u;" "W>" "w>" "Y>" "y>"
       "Y:" "Z'" "z'" "Z." "z." "Z<" "z<" "s1"                ; 017f
       "b/" "B2" "=B" "=b" "B6" "b6" "!C" "C2" "c2" "-D" ;Lat. Extended-B
       "D2" "=D" "=d" "!d" "!E" "-E" "Eps" "F2" "f2" "G2"
       "V0" "hv" "io" "-I" "K2" "k2" "-l"  "la-" "!M" "2N"
       "n_" "-O" "O9" "o9" "OI" "oi" "P2" "p2" "'R" "!S"
       "!s" "Esh" "!esh" "t~" "T2" "t2" "T~" "U9" "u9" "Ups"
       "V2" "Y2" "y2" "Z/" "z/" "ED" "!ED" "!ed" "ed;" "2/"
       "5-" "_5-" "ts" "wn" "|_" "||" "|=" "!_" "DZ<" "Dz<"
       "dz<" "LJ3" "Lj3" "lj3" "NJ3" "Nj3" "nj3" "A<" "a<" "I<"
       "i<" "O<" "o<" "U<" "u<" "U:-" "u:-" "U:'" "u:'" "U:<"
       "u:<" "U:!" "u:!" "e1" "A:-" "a:-" "A.-" "a.-" "AE-" "ae-"
       "G/" "g/" "G<" "g<" "K<" "k<" "O;" "o;" "O1" "o1"
       "EZ" "ez" "j<" "DZ3" "Dz3" "dz3" "G'" "g'" "Hv" "Wn"
       "N`" "n`" "AA'" "aa'" "AE'" "ae'" "O/'" "o/'" "A!!" "a!!"
       "A)" "a)" "E!!" "e!!" "E)" "e)" "I!!" "i!!" "I)" "i)"
       "O!!" "o!!" "O)" "o)" "R!!" "r!!" "R)" "r)" "U!!" "u!!"
       "U)" "u)" ",S" ",s" ",T" ",t" "'3" "'3_" "H<" "h<"
       nil nil "8" "8_" "Z2" "z2" "A." "a." "E," "e,"
       "O:-" "o:-" "O~-" "o~-" "O." "o." "O.-" "o.-" "Y-" "y-"] ; 0x233

     )

    (884 1123				; first, last
     [ "'" "," nil nil nil nil "j3" nil nil nil        ; Greek
       "?;" nil nil nil nil nil "'*" "'%" "A%" ".*"
       "E%" "Y%" "I%" nil "O%" nil "U%" "W%" "i3" "A*"
       "B*" "G*" "D*" "E*" "Z*" "H*" "Th*" "I*" "K*" "L*"
       "M*" "N*" "C*" "O*" "P*" "R*" nil "S*" "T*" "U*"
       "F*" "X*" "Q*" "W*" "J*" "V*" "a%" "e%" "y%" "i%"
       "u3" "a*" "b*" "g*" "d*" "e*" "z*" "h*" "th*" "i*"
       "k*" "l*" "m*" "n*" "c*" "o*" "p*" "r*" "*s" "s*"
       "t*" "u*" "f*" "x*" "q*" "w*" "j*" "v*" "o%" "u%"
       "w%" nil "b3" "th%" "U2*" "'U2*" "U:2*" "ph*" "pi*" "ka*"
       nil nil "Sti" "sti" "Dig" "dig" "Kop" "kop" "Sam" "sam"
       "She" "she" "Fei" "fei" "Khe" "khe" "Hor" "hor" "Gan" "gan"
       "Shi" "shi" "Dei" "dei" "ka*" "rh*" "ls*" "yo*" nil nil
       nil nil nil nil nil nil nil nil nil nil
       "IE'" "E:" "D%" "G%" "IE" "DS" "II" "YI" "J%" "LJ" ; Cyrillic
       "NJ" "Ts"  "KJ" "`I=" "V%" "DZ" 65 "B=" 66 "G="
       68 69 "Z%" 51 85 "J=" 75 "L=" 77 72
       79 "P=" 80 67 84 89 "F=" 88 "C=" "C%"
       "S%" "Sc" "=\"" "Y=" "%\"" "Ee" "JU" "JA" 97 "b="
       98 "g=" 103 101 "z%" "z=" 117 "j=" 107 "l="
       109 "n=" 111 "p=" 112 99 "t=" 121 "f=" 120
       "c=" "c%" "s%" "sc" "='" "y=" "%'" "ee" "ju" "ja"
       "ie'" "e:" "d%" "g%" "ie" "ds" "ii" "yi" "j%" "lj"
       "nj" "ts" "kj" "v%" "`i=" "dz" "OM=" "om=" "Y3" "y3"] ; 0x463
     )

    (1454 1645				; first, last
     [ nil nil ":'" "v:" "-:" "-':" ".'" ".." "v'" "-'"
       "-," "`." nil "\\." "(.)" "|'" "`-" nil "||" nil
       nil "::"  nil nil nil nil nil nil nil nil
       nil nil nil nil
       "A+" "B+" "G+" "D+" "H+" "W+" "Z+" "X+" "Tj" "J+" ; Hebrew
       "K%" "K+" "L+" "M%" "M+" "N%" "N+" "S+" "E+" "P%"
       "P+" "Zj" "ZJ" "Q+" "R+" "Sh" "T+" nil nil nil
       nil nil "WW+" "WJ+" "JJ+" "'+" "\"+" nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       ",+" nil nil nil nil nil nil nil nil nil          ; Arabic
       nil nil nil nil nil ";+" nil nil nil "?+"
       nil "H'" "aM" "aH" "wH" "ah" "yH" "a+" "b+" "tm"
       "t+" "tk" "g+" "hk" "x+" "d+" "dk" "r+" "z+" "s+"
       "sn" "c+" "dd" "tj" "zH" "e+" "i+" nil nil nil
       nil nil "++" "f+" "q+" "k+" "l+" "m+" "n+" "h+"
       "w+" "j+" "y+" ":+" "\"+" "=+" "/+" "'+" "1+" "3+"
       "0+" nil nil nil nil nil nil nil nil nil
       nil nil nil nil "0a" "1a" "2a" "3a" "4a" "5a"
       "6a" "7a" "8a" "9a" "a%" "a." "a," "a*" ]
     )

    (7680 9450				; first, last
     [ "A-0" "a-0" "B." "b." "B-." "b-." "B_" "b_" "C,'" "c,'" ; Lat Ext Add
       "D." "d." "D-." "d-." "D_" "d_" "D," "d," "D->" "d->"
       "E-!" "e-!" "E-'" "e-'" "E->" "e->" "E-?" "e-?" "E,(" "e,("
       "F." "f." "G-" "g-" "H." "h." "H-." "h-." "H:" "h:"
       "H," "h," "H-(" "h-(" "I-?" "i-?" "I:'" "i:'" "K'" "k'"
       "K-." "k-." "K_" "k_" "L-." "l-." "_L-." "_l-." "L_" "l_"
       "L->" "l->" "M'" "m'" "M." "m." "M-." "m-." "N." "n."
       "N-." "n-." "N_" "n_" "N->" "n->" "O?'" "o?'" "O?:" "o?:"
       "O-!" "o-!" "O-'" "o-'" "P'" "p'" "P." "p." "R." "r."
       "R-." "r-." "_R-." "_r-." "R_" "r_" "S." "s." "S-." "s-."
       "S'." "s'." "S<." "s<." ".S-." ".s-." "T." "t." "T-." "t-."
       "T_" "t_" "T->" "t->" "U_:" "u_:" "U-?" "u-?" "U->" "u->"
       "U?'" "u?'" "U-:" "u-:" "V?" "v?" "V-." "v-." "W!" "w!"
       "W'" "w'" "W:" "w:" "W." "w." "W-." "w-." "X."  "x."
       "X:" "x:" "Y." "y." "Z>" "z>" "Z-." "z-." "Z_" "z_"
       "h_" "t:" "w0" "y0" "a))" "s1." nil nil nil nil
       "A-." "a-." "A2" "a2" "A>'" "a>'" "A>!" "a>!" "A>2" "a>2"
       "A>~" "a>~" ".A>" ".a>" "A('" "a('" "A(!" "a(!" "A(2" "a(2"
       "A(~" "a(~" ".A(" ".a(" "E-." "e-." "E2" "e2" "E~" "e~"
       "E>'" "e>'" "E>!" "e>!" "E>2" "e>2" "E>~" "e>~" ".E>" ".e>"
       "I2" "i2" "I-." "i-." "O-." "o-." "O2" "o2" "O>'" "o>'"
       "O>!" "o>!" "O>2" "o>2" "O>~" "o>~" ".O>" ".o>" "O9'" "o9'"
       "O9!" "o9!" "O92" "o92" "O9~" "o9~" ".O9" ".o9" "U-." "u-."
       "U2" "u2" "U9'" "u9'" "U9!" "u9!" "U92" "u92" "U9~" "u9~"
       ".U9" ".u9" "Y!" "y!" "Y-." "y-." "Y2" "y2" "Y~" "y~"
       nil nil nil nil nil nil "a*," "a*;" nil nil ; Greek Ext (0x1f00)
       nil nil nil nil "A*," "A*;" nil nil nil nil
       nil nil "e*," "e*;" nil nil nil nil nil nil
       "E*," "E*;" nil nil nil nil nil nil "y*," "y*;"
       nil nil nil nil nil nil "Y*," "Y*;" nil nil
       nil nil nil nil "i*," "i*;" nil nil nil nil
       nil nil "I*," "I*;" nil nil nil nil nil nil
       "o*," "o*;" nil nil nil nil nil nil "O*," "O*;"
       nil nil nil nil nil nil "u*," "u*;" nil nil
       nil nil nil nil nil "U*;" nil nil nil nil
       nil nil "w*," "w*;" nil nil nil nil nil nil
       "W*," "W*;" nil nil nil nil nil nil "a*!" "a*'"
       "e*!" "e*'" "y*!" "y*'" "i*!" "i*'" "o*!" "o*'" "u*!" "u*'"
       "w*!" "w*'" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil "a*(" "a*-" nil "a*j" nil nil "a*~" nil
       "A*(" "A*-" "A*!" "A*'" "A*J" ")*" "J3" ",," "?*" "?:"
       nil "y*j" nil nil "y*?" nil "E*!"  "E*'" "Y*!" "Y*'"
       "Y*J" ",!" ",'" "?," "i*(" "i*-" nil nil nil nil
       "i*?" nil "I*(" "I*-" "I*!" "I*'" nil ";!" ";'" "?;"
       "u*(" "u*-" nil nil "r*," "r*;" "u*?" nil "U*(" "U*-"
       "U*!" "U*'" "R*;" "!:" ":'" "!*" nil nil nil "w*j"
       nil nil "w*?" nil "O*!" "O*'" "W*!" "W*'" "W*J" "/*"
       ";;" nil nil nil "1N" "1M" "3M" "4M" "6M" nil          ; Gen Punct
       nil "1T" "1H" nil nil nil "LRM" "RLM" "-1" nil
       nil "--" "---" "===" "!2" "=2" "6`" "'9" ".9" "9'"
       "``" "''" ":9" "9``" "/-" "/=" "sb" "3b" nil ".."
       "..." ".-" "LSep" "PSep" "LR[" "RL[" "PDF" "LRO" "RLO" 255
       "%o" "%oo" "'" "''" "\"'" "`" "``" "```" ".^" "<,"
       ",>" ":X" "!!" "?!" "'-" nil nil nil nil "-b"
       "/f" nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil "^0" nil nil nil "^4" "^5"
       "^6" "^7" "^8" "^9" "^+" "^-" "^=" "^(" "^)" "^n"
       "_0" "_1" "_2" "_3" "_4" "_5" "_6" "_7" "_8" "_9"
       "_+" "_-" "_=" "_(" "_)" nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil "Ff" "Li" nil nil "Pt"
       nil "W=" "NIS" nil "E=" nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil "a/c" "a/s"
       nil "oC" nil "c/o" "c/u" "=e" nil "oF" nil nil
       nil nil "-h" "=h" nil nil nil nil nil nil
       "N0" "PO" nil nil nil nil "Re" nil "Rx" nil
       "SM" "TEL" "TM" nil nil nil "Om" nil nil nil
       "oK" "AO" nil nil "Est" nil nil nil nil nil
       nil "Aleph" "Bet" "Gimel" "Dalet" "=i=" nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil "1/3" "2/3" "1/5" "2/5" "3/5" "4/5" "1/6" "5/6" "1/8"
       "3/8" "5/8" "7/8" "1/" ".I" "II" "III" "IV" ".V" "VI"
       "VII" "VIII" "IX" "X" "XI" "XII" ".L" ".C" ".D" ".M"
       ".i" "ii" "iii" "iv" ".v" "vi" "vii" "viii" "ix" ".x"
       "xi" ".l" ".c" ".d" ".m" "CD" "DD" "CoD" "CI" nil
       nil nil nil nil nil nil nil nil nil nil
       nil "<-" "|^" "->" "|v" "<->" "v|^" "^\\" "/^" "\\v"
       "v/" "<-/" "/->" "<~" "~>" "<<-" "|^^" "->>" "|vv" "<-<"
       ">->" "<-|" "_|^" "|->" "-|v" "_v|^" "<-?" "?->" "<-o" "o->"
       "<~>" "<-/>" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil "<=/" "<=/>" "/=>" "<=" "||^" "=>" "||v"
       "<=>" "v||^" "^\\\\" "//^" "\\\\v" "v//" "<-=" "=->" nil nil
       nil nil "<.." ":^" "..>" ":v" nil nil "<::" "::^"
       "::>" "::v" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil "FA" "C." "dP" "TE" "~TE" "/0"
       "DE" "NB" "(-" "~(-" "e-" "-)" "~-)" "-e" "QED" "*P"
       nil "+Z" "--" "-+" ".+" "./" ".\\" "*-" "Ob" "Sb"
       "SQR" "CBR" nil "0(" "00" "-L" "-V" nil nil ".|"
       "~.|" "||" "/||" "AND" "OR" "(U" ")U" "In" "DI" nil
       "Io" nil nil nil nil nil ".:" ":." ":R" "::"
       ".-." "-:" ":-:" ":~:" "?~" "~?" "??" nil nil "/~"
       "-~" "~-" "/~-" "~=" "~/=" "/~=" "~~" "/~~" nil nil
       "=?" ")(" "v^" "^_" ".=" "=;" ".=." nil ":=" "=:"
       nil "=o" "=)" "=^" "=v" "*=" "=<>" "=df" nil "?="
       "!=" "-=" "!-=" "==" "=<" ">=" nil nil nil nil
       "<<" ">>" "()" "/)(" "!<" "!>" nil nil nil nil
       nil nil nil nil nil nil "<'" "`>" "=<'" "`>="
       "~<'" "`>~" "/<'" "/`>" "(C" ")C" "/(C" "/)C" "(_" ")_"
       "/(_" "/)_" nil nil nil nil nil nil nil nil
       nil nil nil "0+" "0-" "0x" "0/" "0." "0o" "0*"
       "0=" "0_" nil nil nil nil "|T" "T|" "-T" "_T"
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil "-,-"
       nil "XOR" "NAND" "NOR" nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil "<." ".>"
       "<<<" ">>>" nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil ":3" "..." nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil "Eh" nil  nil nil nil nil "<7" ">7"
       "7<" "7>" nil nil nil nil "~I" nil "(A" nil
       nil "TR" nil "=||" "88" nil nil nil nil nil
       nil nil "Iu" "Il" nil nil "-^-" "-`-" "D->" nil
       nil "</" "/>" "<-D" nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil "NUL" "SOH" "STX" "ETX"
       "EOT" "ENQ" "ACK" "BEL" "BS" "HT" "LF" "VT" "FF" "CR"
       "SS" "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB"
       "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US" "SP" "DEL"
       "b/" ",_," "NL" nil "?^" nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil nil nil nil nil nil nil nil nil
       nil nil "1-o" "2-o" "3-o" "4-o" "5-o" "6-o" "7-o" "8-o"
       "9-o" "10-o" "11-o" "12-o" "13-o" "14-o" "15-o" "16-o" "17-o" "18-o"
       "19-o" "20-o" "(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" "(8)"
       "(9)" "(10)" "(11)" "(12)" "(13)" "(14)" "(15)" "(16)" "(17)" "(18)"
       "(19)" "(20)" "1." "2." "3." "4." "5." "6." "7." "8."
       "9." "10." "11." "12." "13." "14." "15." "16." "17." "18."
       "19." "20." "(a)" "(b)" "(c)" "(d)" "(e)" "(f)" "(g)" "(h)"
       "(i)" "(j)" "(k)" "(l)" "(m)" "(n)" "(o)" "(p)" "(q)" "(r)"
       "(s)" "(t)" "(u)" "(v)" "(w)" "(x)" "(y)" "(z)" "A-o" "B-o"
       "C-o" "D-o" "E-o" "F-o" "G-o" "H-o" "I-o" "J-o" "K-o" "L-o"
       "M-o" "N-o" "O-o" "P-o" "Q-o" "R-o" "S-o" "T-o" "U-o" "V-o"
       "W-o" "X-o" "Y-o" "Z-o" "a-o" "b-o" "c-o" "d-o" "e-o" "f-o"
       "g-o" "h-o" "i-o" "j-o" "k-o" "l-o" "m-o" "n-o" "o-o" "p-o"
       "q-o" "r-o" "s-o" "t-o" "u-o" "v-o" "w-o" "x-o" "y-o" "z-o"
       "0-o" ]
     )
    )

  "A list of strings that should be used to represent Unicode
characters on a DOS terminal which does not have corresponding
glyphs built into the installed codepage.")

(defun IT-setup-unicode-display (coding &optional table)
  "Set up display table TABLE for displaying mule-unicode-* characters
on a DOS terminal whose codepage provides the coding-system CODING.
If TABLE is nil or omitted, `standard-display-table' is used."
  (interactive "zCode page: ")
  (let ((disp-tab (or table standard-display-table))
	(tail IT-unicode-translations)
	translation)
    (while tail
      (setq translation (car tail) tail (cdr tail))
      (let* ((first (car translation))
	     (last (nth 1 translation))
	     (table (nth 2 translation))
	     (i 0)
	     (this first)
	     glyph)
	(while (<= i (- last first))
	  (setq glyph (aref table i))
	  (when (and glyph
		     (unencodable-char-position 0 1 coding nil (string this)))
	    (aset disp-tab this
		  (vconcat (if (numberp glyph)
			       (string glyph)
			     glyph))))
	  (setq i (1+ i) this (1+ this)))))))

(defvar dos-codepage)
(defvar dos-country-code)

;; The following alist was compiled from:
;;
;; Ralf Brown's Interrupt List. file INTERRUP.F, D-2138, Table 01400
;; http://www.ethnologue.com/country_index.asp (official languages)
;; http://unicode.org/onlinedat/languages.html
;; http://unicode.org/onlinedat/countries.html
;;
;; Only the official languages listed for each country.
;;
(defvar dos-locale-alist
  '((  1 . "en_US")
    (  2 . "fr_CA")
    (  3 . "es_MX")  ; what the heck is "Latin America"?
    (  4 . "en_CA")
    (  7 . "ru_RU")
    ( 20 . "ar_EG")
    ( 27 . "af_ZA")
    ( 30 . "el_GR")
    ( 31 . "nl_NL")
    ( 32 . "nl_BE")
    ( 33 . "fr_FR")
    ( 34 . "es_ES")
    ( 35 . "bg_BG")
    ( 36 . "hu_HU")
    ( 38 . "sh_YU")
    ( 39 . "it_IT")
    ( 40 . "ro_RO")
    ( 41 . "de_CH")
    ( 42 . "cs_CZ")
    ( 43 . "de_AT")
    ( 44 . "en_UK")
    ( 45 . "da_DK")
    ( 46 . "sv_SE")
    ( 47 . "no_NO")
    ( 48 . "pl_PL")
    ( 49 . "de_DE")
    ( 51 . "es_PE")
    ( 52 . "es_MX")
    ( 53 . "es_CU")
    ( 54 . "es_AR")
    ( 55 . "pt_BR")
    ( 56 . "es_CL")
    ( 57 . "es_CO")
    ( 58 . "es_VE")
    ( 60 . "ms_MY")
    ( 61 . "en_AU")
    ( 62 . "id_ID")
    ( 63 . "fil_PH")
    ( 64 . "en_NZ")
    ( 65 . "zh_SG")
    ( 66 . "th_TH")
    ( 81 . "ja_JP")
    ( 82 . "ko_KR")
    ( 84 . "vi_VN")
    ( 86 . "zh_CN")
    ( 88 . "zh_TW")
    ( 90 . "tr_TR")
    ( 91 . "hi_IN")
    ( 92 . "ur_PK")
    ( 93 . "ps_AF")
    ( 94 . "si_LK")
    ( 98 . "fa_IR")
    ( 99 . "en"   )
    (102 . "he_IL")
    (112 . "be_BY")
    (212 . "ar_MA")
    (213 . "ar_DZ")
    (216 . "ar_TN")
    (218 . "ar_LY")
    (220 . "en_GM")
    (221 . "fr_SN")
    (222 . "mey_MR")
    (223 . "fr_ML")
    (224 . "fr_GN")
    (227 . "fr_NE")
    (228 . "fr_TG")
    (230 . "fr_MU")
    (231 . "en_LR")
    (232 . "en_SL")
    (233 . "en_GH")
    (234 . "en_NG")
    (235 . "ar_TD")
    (236 . "fr_CF")
    (237 . "fr_CM")
    (241 . "fr_GA")
    (242 . "fr_CG")
    (243 . "sw_ZR")
    (244 . "pt_AO")
    (245 . "pt_GW")
    (249 . "ar_SD")
    (250 . "fr_RW")
    (251 . "am_ET")
    (252 . "so_SO")
    (253 . "fr_DJ")
    (254 . "sw_KE")
    (255 . "sw_TZ")
    (256 . "en_UG")
    (257 . "fr_BI")
    (259 . "pt_MZ")
    (260 . "en_ZM")
    (261 . "mg_MG")
    (263 . "en_ZW")
    (264 . "en_NA")
    (265 . "en_MW")
    (266 . "st_LS")
    (267 . "en_BW")
    (268 . "en_SZ")
    (299 . "kl_GL")
    (350 . "en_GI")
    (351 . "pt_PT")
    (352 . "fr_LU")
    (353 . "ga_IE")
    (354 . "is_IS")
    (355 . "sq_AL")
    (356 . "mt_MT")
    (357 . "gr_CY")
    (358 . "fi_FI")
    (359 . "bg_BG")
    (370 . "lt_LT")
    (371 . "lv_LV")
    (372 . "et_EE")
    (373 . "mo_MD")
    (380 . "uk_UA")
    (381 . "sr_RS")
    (384 . "hr_HR")
    (385 . "hr_HR")
    (386 . "sl_SI")
    (387 . "bs_BA")
    (388 . "sr_BA")
    (389 . "mk_MK")
    (421 . "cs_CZ")
    (422 . "sk_SK")
    (502 . "es_GT")
    (503 . "es_SV")
    (504 . "es_HN")
    (505 . "es_NI")
    (506 . "es_CR")
    (507 . "es_PA")
    (509 . "ht_HT")
    (590 . "fr_GP")
    (591 . "es_BO")
    (592 . "en_GY")
    (593 . "es_EC")
    (594 . "fr_GF")
    (595 . "gn_PY")
    (596 . "fr_MQ")
    (597 . "nl_SR")
    (598 . "es_UY")
    (785 . "ar"   )
    (804 . "uk_UA")
    (850 . "ko_KP")
    (855 . "km_KH")
    (856 . "lo_LA")
    (880 . "bn_BD")
    (886 . "zh_TW")
    (960 . "dv_MV")
    (961 . "ar_LB")
    (962 . "ar_JO")
    (963 . "ar_SY")
    (964 . "ar_IQ")
    (965 . "ar_KW")
    (966 . "ar_SA")
    (967 . "ar_YE")
    (968 . "ar_OM")
    (969 . "ar_YE")
    (971 . "ar_AE")
    (972 . "he_IL")
    (973 . "ar_BH")
    (974 . "ar_QA")
    (975 . "dz_BT")
    (976 . "mn_MN")
    (977 . "ne_NP")
    (995 . "my_MM")
    )
    "Alist of MS-DOS country codes and the corresponding locale names.")

(defun dos-codepage-setup ()
  "Set up multilingual environment for the installed DOS codepage.

This function sets coding systems, display tables, and the language
environment options as appropriate for the current value of `dos-codepage'.

This function is automatically run at startup via the `after-init-hook'
list.  You can (and should) also run it if and when the value of
`dos-codepage' changes."
  (interactive)
  (let ((locale (cdr (assq dos-country-code dos-locale-alist)))
	(coding (format "cp%s" dos-codepage))
	coding-dos coding-unix)
    (setq coding-dos (intern (format "%s-dos" coding))
	  coding-unix (intern (format "%s-unix" coding)))
    (setq locale (if locale
		     (format "%s.cp%s" locale dos-codepage)
		   "en_US.cp437"))
    (set-locale-environment locale)
    (set-selection-coding-system coding-dos)
    (IT-setup-unicode-display coding-unix)
    (prefer-coding-system coding-dos)
    (and (default-value 'enable-multibyte-characters)
	 (setq unibyte-display-via-language-environment t))
    ;; Some codepages have sporadic support for Latin-1, Greek, and
    ;; symbol glyphs, which don't belong to their native character
    ;; set.  It's a nuisance to have all those glyphs here, for all
    ;; the codepages (for starters, I don't even have references for
    ;; all the codepages).  So provide a hook for those who want to
    ;; squeeze every bit of support out of their terminal/font.
    (run-hooks 'dos-codepage-setup-hook)
    ))

;;; internal.el ends here
