;;; org-entities.el --- Support for special entities in Org-mode

;; Copyright (C) 2010-2012 Free Software Foundation, Inc.

;; Author: Carsten Dominik <carsten at orgmode dot org>,
;;         Ulf Stegemann <ulf at zeitform dot de>
;; Keywords: outlines, calendar, wp
;; Homepage: http://orgmode.org
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;;; Code:

(require 'org-macs)

(declare-function org-table-align "org-table" ())

(eval-when-compile
  (require 'cl))

(defgroup org-entities nil
  "Options concerning entities in Org-mode."
  :tag "Org Entities"
  :group 'org)

(defcustom org-entities-ascii-explanatory nil
  "Non-nil means replace special entities in ASCII.
For example, this will replace \"\\nsup\" with \"[not a superset of]\"
in backends where the corresponding character is not available."
  :group 'org-entities
  :version "24.1"
  :type 'boolean)

(defcustom org-entities-user nil
  "User-defined entities used in Org-mode to produce special characters.
Each entry in this list is a list of strings.  It associates the name
of the entity that can be inserted into an Org file as \\name with the
appropriate replacements for the different export backends.  The order
of the fields is the following

name                 As a string, without the leading backslash
LaTeX replacement    In ready LaTeX, no further processing will take place
LaTeX mathp          A Boolean, either t or nil.  t if this entity needs
                     to be in math mode.
HTML replacement     In ready HTML, no further processing will take place.
                     Usually this will be an &...; entity.
ASCII replacement    Plain ASCII, no extensions.  Symbols that cannot be
                     represented will be left as they are, but see the.
                     variable `org-entities-ascii-explanatory'.
Latin1 replacement   Use the special characters available in latin1.
utf-8 replacement    Use the special characters available in utf-8.

If you define new entities here that require specific LaTeX packages to be
loaded, add these packages to `org-export-latex-packages-alist'."
  :group 'org-entities
  :version "24.1"
  :type '(repeat
	  (list
	   (string :tag "name  ")
	   (string :tag "LaTeX ")
	   (boolean :tag "Require LaTeX math?")
	   (string :tag "HTML  ")
	   (string :tag "ASCII ")
	   (string :tag "Latin1")
	   (string :tag "utf-8 "))))

(defconst org-entities
  '(
    "* Letters"
    "** Latin"
    ("Agrave" "\\`{A}" nil "&Agrave;" "A" "À" "À")
    ("agrave" "\\`{a}" nil "&agrave;" "a" "à" "à")
    ("Aacute" "\\'{A}" nil "&Aacute;" "A" "Á" "Á")
    ("aacute" "\\'{a}" nil "&aacute;" "a" "á" "á")
    ("Acirc" "\\^{A}" nil "&Acirc;" "A" "Â" "Â")
    ("acirc" "\\^{a}" nil "&acirc;" "a" "â" "â")
    ("Atilde" "\\~{A}" nil "&Atilde;" "A" "Ã" "Ã")
    ("atilde" "\\~{a}" nil "&atilde;" "a" "ã" "ã")
    ("Auml" "\\\"{A}" nil "&Auml;" "Ae" "Ä" "Ä")
    ("auml" "\\\"{a}" nil "&auml;" "ae" "ä" "ä")
    ("Aring" "\\AA{}" nil "&Aring;" "A" "Å" "Å")
    ("AA" "\\AA{}" nil "&Aring;" "A" "Å" "Å")
    ("aring" "\\aa{}" nil "&aring;" "a" "å" "å")
    ("AElig" "\\AE{}" nil "&AElig;" "AE" "Æ" "Æ")
    ("aelig" "\\ae{}" nil "&aelig;" "ae" "æ" "æ")
    ("Ccedil" "\\c{C}" nil "&Ccedil;" "C" "Ç" "Ç")
    ("ccedil" "\\c{c}" nil "&ccedil;" "c" "ç" "ç")
    ("Egrave" "\\`{E}" nil "&Egrave;" "E" "È" "È")
    ("egrave" "\\`{e}" nil "&egrave;" "e" "è" "è")
    ("Eacute" "\\'{E}" nil "&Eacute;" "E" "É" "É")
    ("eacute" "\\'{e}" nil "&eacute;" "e" "é" "é")
    ("Ecirc" "\\^{E}" nil "&Ecirc;" "E" "Ê" "Ê")
    ("ecirc" "\\^{e}" nil "&ecirc;" "e" "ê" "ê")
    ("Euml" "\\\"{E}" nil "&Euml;" "E" "Ë" "Ë")
    ("euml" "\\\"{e}" nil "&euml;" "e" "ë" "ë")
    ("Igrave" "\\`{I}" nil "&Igrave;" "I" "Ì" "Ì")
    ("igrave" "\\`{i}" nil "&igrave;" "i" "ì" "ì")
    ("Iacute" "\\'{I}" nil "&Iacute;" "I" "Í" "Í")
    ("iacute" "\\'{i}" nil "&iacute;" "i" "í" "í")
    ("Icirc" "\\^{I}" nil "&Icirc;" "I" "Î" "Î")
    ("icirc" "\\^{i}" nil "&icirc;" "i" "î" "î")
    ("Iuml" "\\\"{I}" nil "&Iuml;" "I" "Ï" "Ï")
    ("iuml" "\\\"{i}" nil "&iuml;" "i" "ï" "ï")
    ("Ntilde" "\\~{N}" nil "&Ntilde;" "N" "Ñ" "Ñ")
    ("ntilde" "\\~{n}" nil "&ntilde;" "n" "ñ" "ñ")
    ("Ograve" "\\`{O}" nil "&Ograve;" "O" "Ò" "Ò")
    ("ograve" "\\`{o}" nil "&ograve;" "o" "ò" "ò")
    ("Oacute" "\\'{O}" nil "&Oacute;" "O" "Ó" "Ó")
    ("oacute" "\\'{o}" nil "&oacute;" "o" "ó" "ó")
    ("Ocirc" "\\^{O}" nil "&Ocirc;" "O" "Ô" "Ô")
    ("ocirc" "\\^{o}" nil "&ocirc;" "o" "ô" "ô")
    ("Otilde" "\\~{O}" nil "&Otilde;" "O" "Õ" "Õ")
    ("otilde" "\\~{o}" nil "&otilde;" "o" "õ" "õ")
    ("Ouml" "\\\"{O}" nil "&Ouml;" "Oe" "Ö" "Ö")
    ("ouml" "\\\"{o}" nil "&ouml;" "oe" "ö" "ö")
    ("Oslash" "\\O" nil "&Oslash;" "O" "Ø" "Ø")
    ("oslash" "\\o{}" nil "&oslash;" "o" "ø" "ø")
    ("OElig" "\\OE{}" nil "&OElig;" "OE" "OE" "Œ")
    ("oelig" "\\oe{}" nil "&oelig;" "oe" "oe" "œ")
    ("Scaron" "\\v{S}" nil "&Scaron;" "S" "S" "Š")
    ("scaron" "\\v{s}" nil "&scaron;" "s" "s" "š")
    ("szlig" "\\ss{}" nil "&szlig;" "ss" "ß" "ß")
    ("Ugrave" "\\`{U}" nil "&Ugrave;" "U" "Ù" "Ù")
    ("ugrave" "\\`{u}" nil "&ugrave;" "u" "ù" "ù")
    ("Uacute" "\\'{U}" nil "&Uacute;" "U" "Ú" "Ú")
    ("uacute" "\\'{u}" nil "&uacute;" "u" "ú" "ú")
    ("Ucirc" "\\^{U}" nil "&Ucirc;" "U" "Û" "Û")
    ("ucirc" "\\^{u}" nil "&ucirc;" "u" "û" "û")
    ("Uuml" "\\\"{U}" nil "&Uuml;" "Ue" "Ü" "Ü")
    ("uuml" "\\\"{u}" nil "&uuml;" "ue" "ü" "ü")
    ("Yacute" "\\'{Y}" nil "&Yacute;" "Y" "Ý" "Ý")
    ("yacute" "\\'{y}" nil "&yacute;" "y" "ý" "ý")
    ("Yuml" "\\\"{Y}" nil "&Yuml;" "Y" "Y" "Ÿ")
    ("yuml" "\\\"{y}" nil "&yuml;" "y" "ÿ" "ÿ")

    "** Latin (special face)"
    ("fnof" "\\textit{f}" nil "&fnof;" "f" "f" "ƒ")
    ("real" "\\Re" t "&real;" "R" "R" "ℜ")
    ("image" "\\Im" t "&image;" "I" "I" "ℑ")
    ("weierp" "\\wp" t "&weierp;" "P" "P" "℘")

    "** Greek"
    ("Alpha" "A" nil "&Alpha;" "Alpha" "Alpha" "Α")
    ("alpha" "\\alpha" t "&alpha;" "alpha" "alpha" "α")
    ("Beta" "B" nil "&Beta;" "Beta" "Beta" "Β")
    ("beta" "\\beta" t "&beta;" "beta" "beta" "β")
    ("Gamma" "\\Gamma" t "&Gamma;" "Gamma" "Gamma" "Γ")
    ("gamma" "\\gamma" t "&gamma;" "gamma" "gamma" "γ")
    ("Delta" "\\Delta" t "&Delta;" "Delta" "Gamma" "Δ")
    ("delta" "\\delta" t "&delta;" "delta" "delta" "δ")
    ("Epsilon" "E" nil "&Epsilon;" "Epsilon" "Epsilon" "Ε")
    ("epsilon" "\\epsilon" t "&epsilon;" "epsilon" "epsilon" "ε")
    ("varepsilon" "\\varepsilon" t "&epsilon;" "varepsilon" "varepsilon" "ε")
    ("Zeta" "Z" nil "&Zeta;" "Zeta" "Zeta" "Ζ")
    ("zeta" "\\zeta" t "&zeta;" "zeta" "zeta" "ζ")
    ("Eta" "H" nil "&Eta;" "Eta" "Eta" "Η")
    ("eta" "\\eta" t "&eta;" "eta" "eta" "η")
    ("Theta" "\\Theta" t "&Theta;" "Theta" "Theta" "Θ")
    ("theta" "\\theta" t "&theta;" "theta" "theta" "θ")
    ("thetasym" "\\vartheta" t "&thetasym;" "theta" "theta" "ϑ")
    ("vartheta" "\\vartheta" t "&thetasym;" "theta" "theta" "ϑ")
    ("Iota" "I" nil "&Iota;" "Iota" "Iota" "Ι")
    ("iota" "\\iota" t "&iota;" "iota" "iota" "ι")
    ("Kappa" "K" nil "&Kappa;" "Kappa" "Kappa" "Κ")
    ("kappa" "\\kappa" t "&kappa;" "kappa" "kappa" "κ")
    ("Lambda" "\\Lambda" t "&Lambda;" "Lambda" "Lambda" "Λ")
    ("lambda" "\\lambda" t "&lambda;" "lambda" "lambda" "λ")
    ("Mu" "M" nil "&Mu;" "Mu" "Mu" "Μ")
    ("mu" "\\mu" t "&mu;" "mu" "mu" "μ")
    ("nu" "\\nu" t "&nu;" "nu" "nu" "ν")
    ("Nu" "N" nil "&Nu;" "Nu" "Nu" "Ν")
    ("Xi" "\\Xi" t "&Xi;" "Xi" "Xi" "Ξ")
    ("xi" "\\xi" t "&xi;" "xi" "xi" "ξ")
    ("Omicron" "O" nil "&Omicron;" "Omicron" "Omicron" "Ο")
    ("omicron" "\\textit{o}" nil "&omicron;" "omicron" "omicron" "ο")
    ("Pi" "\\Pi" t "&Pi;" "Pi" "Pi" "Π")
    ("pi" "\\pi" t "&pi;" "pi" "pi" "π")
    ("Rho" "P" nil "&Rho;" "Rho" "Rho" "Ρ")
    ("rho" "\\rho" t "&rho;" "rho" "rho" "ρ")
    ("Sigma" "\\Sigma" t "&Sigma;" "Sigma" "Sigma" "Σ")
    ("sigma" "\\sigma" t "&sigma;" "sigma" "sigma" "σ")
    ("sigmaf" "\\varsigma" t "&sigmaf;" "sigmaf" "sigmaf" "ς")
    ("varsigma" "\\varsigma" t "&sigmaf;" "varsigma" "varsigma" "ς")
    ("Tau" "T" nil "&Tau;" "Tau" "Tau" "Τ")
    ("Upsilon" "\\Upsilon" t "&Upsilon;" "Upsilon" "Upsilon" "Υ")
    ("upsih" "\\Upsilon" t "&upsih;" "upsilon" "upsilon" "ϒ")
    ("upsilon" "\\upsilon" t "&upsilon;" "upsilon" "upsilon" "υ")
    ("Phi" "\\Phi" t "&Phi;" "Phi" "Phi" "Φ")
    ("phi" "\\phi" t "&phi;" "phi" "phi" "φ")
    ("Chi" "X" nil "&Chi;" "Chi" "Chi" "Χ")
    ("chi" "\\chi" t "&chi;" "chi" "chi" "χ")
    ("acutex" "\\acute x" t "&acute;x" "'x" "'x" "𝑥́")
    ("Psi" "\\Psi" t "&Psi;" "Psi" "Psi" "Ψ")
    ("psi" "\\psi" t "&psi;" "psi" "psi" "ψ")
    ("tau" "\\tau" t "&tau;" "tau" "tau" "τ")
    ("Omega" "\\Omega" t "&Omega;" "Omega" "Omega" "Ω")
    ("omega" "\\omega" t "&omega;" "omega" "omega" "ω")
    ("piv" "\\varpi" t "&piv;" "omega-pi" "omega-pi" "ϖ")
    ("partial" "\\partial" t "&part;" "[partial differential]" "[partial differential]" "∂")

    "** Hebrew"
    ("alefsym" "\\aleph" t "&alefsym;" "aleph" "aleph" "ℵ")

    "** Dead languages"
    ("ETH" "\\DH{}" nil "&ETH;" "D" "Ð" "Ð")
    ("eth" "\\dh{}" nil "&eth;" "dh" "ð" "ð")
    ("THORN" "\\TH{}" nil "&THORN;" "TH" "Þ" "Þ")
    ("thorn" "\\th{}" nil "&thorn;" "th" "þ" "þ")

    "* Punctuation"
    "** Dots and Marks"
    ("dots" "\\dots{}" nil "&hellip;" "..." "..." "…")
    ("hellip" "\\dots{}" nil "&hellip;" "..." "..." "…")
    ("middot" "\\textperiodcentered{}" nil "&middot;" "." "·" "·")
    ("iexcl" "!`" nil "&iexcl;" "!" "¡" "¡")
    ("iquest" "?`" nil "&iquest;" "?" "¿" "¿")

    "** Dash-like"
    ("shy" "\\-" nil "&shy;" "" "" "")
    ("ndash" "--" nil "&ndash;" "-" "-" "–")
    ("mdash" "---" nil "&mdash;" "--" "--" "—")

    "** Quotations"
    ("quot" "\\textquotedbl{}" nil "&quot;" "\"" "\"" "\"")
    ("acute" "\\textasciiacute{}" nil "&acute;" "'" "´" "´")
    ("ldquo" "\\textquotedblleft{}" nil "&ldquo;" "\"" "\"" "“")
    ("rdquo" "\\textquotedblright{}" nil "&rdquo;" "\"" "\"" "”")
    ("bdquo" "\\quotedblbase{}" nil "&bdquo;" "\"" "\"" "„")
    ("lsquo" "\\textquoteleft{}" nil "&lsquo;" "`" "`" "‘")
    ("rsquo" "\\textquoteright{}" nil "&rsquo;" "'" "'" "’")
    ("sbquo" "\\quotesinglbase{}" nil "&sbquo;" "," "," "‚")
    ("laquo" "\\guillemotleft{}" nil "&laquo;" "<<" "«" "«")
    ("raquo" "\\guillemotright{}" nil "&raquo;" ">>" "»" "»")
    ("lsaquo" "\\guilsinglleft{}" nil "&lsaquo;" "<" "<" "‹")
    ("rsaquo" "\\guilsinglright{}" nil "&rsaquo;" ">" ">" "›")

    "* Other"
    "** Misc. (often used)"
    ("circ" "\\circ" t "&circ;" "^" "^" "ˆ")
    ("vert" "\\vert{}" t "&#124;" "|" "|" "|")
    ("brvbar" "\\textbrokenbar{}" nil "&brvbar;" "|" "¦" "¦")
    ("sect" "\\S" nil "&sect;" "paragraph" "§" "§")
    ("amp" "\\&" nil "&amp;" "&" "&" "&")
    ("lt" "\\textless{}" nil "&lt;" "<" "<" "<")
    ("gt" "\\textgreater{}" nil "&gt;" ">" ">" ">")
    ("tilde" "\\~{}" nil "&tilde;" "~" "~" "~")
    ("dagger" "\\textdagger{}" nil "&dagger;" "[dagger]" "[dagger]" "†")
    ("Dagger" "\\textdaggerdbl{}" nil "&Dagger;" "[doubledagger]" "[doubledagger]" "‡")

    "** Whitespace"
    ("nbsp" "~" nil "&nbsp;" " " " " " ")
    ("ensp" "\\hspace*{.5em}" nil "&ensp;" " " " " " ")
    ("emsp" "\\hspace*{1em}" nil "&emsp;" " " " " " ")
    ("thinsp" "\\hspace*{.2em}" nil "&thinsp;" " " " " " ")

    "** Currency"
    ("curren" "\\textcurrency{}" nil "&curren;" "curr." "¤" "¤")
    ("cent" "\\textcent{}" nil "&cent;" "cent" "¢" "¢")
    ("pound" "\\pounds{}" nil "&pound;" "pound" "£" "£")
    ("yen" "\\textyen{}" nil "&yen;" "yen" "¥" "¥")
    ("euro" "\\texteuro{}" nil "&euro;" "EUR" "EUR" "€")
    ("EUR" "\\EUR{}" nil "&euro;" "EUR" "EUR" "€")
    ("EURdig" "\\EURdig{}" nil "&euro;" "EUR" "EUR" "€")
    ("EURhv" "\\EURhv{}" nil "&euro;" "EUR" "EUR" "€")
    ("EURcr" "\\EURcr{}" nil "&euro;" "EUR" "EUR" "€")
    ("EURtm" "\\EURtm{}" nil "&euro;" "EUR" "EUR" "€")

    "** Property Marks"
    ("copy" "\\textcopyright{}" nil "&copy;" "(c)" "©" "©")
    ("reg" "\\textregistered{}" nil "&reg;" "(r)" "®" "®")
    ("trade" "\\texttrademark{}" nil "&trade;" "TM" "TM" "™")

    "** Science et al."
    ("minus" "\\minus" t "&minus;" "-" "-" "−")
    ("pm" "\\textpm{}" nil "&plusmn;" "+-" "±" "±")
    ("plusmn" "\\textpm{}" nil "&plusmn;" "+-" "±" "±")
    ("times" "\\texttimes{}" nil "&times;" "*" "×" "×")
    ("frasl" "/" nil "&frasl;" "/" "/" "⁄")
    ("div" "\\textdiv{}" nil "&divide;" "/" "÷" "÷")
    ("frac12" "\\textonehalf{}" nil "&frac12;" "1/2" "½" "½")
    ("frac14" "\\textonequarter{}" nil "&frac14;" "1/4" "¼" "¼")
    ("frac34" "\\textthreequarters{}" nil "&frac34;" "3/4" "¾" "¾")
    ("permil" "\\textperthousand{}" nil "&permil;" "per thousand" "per thousand" "‰")
    ("sup1" "\\textonesuperior{}" nil "&sup1;" "^1" "¹" "¹")
    ("sup2" "\\texttwosuperior{}" nil "&sup2;" "^2" "²" "²")
    ("sup3" "\\textthreesuperior{}" nil "&sup3;" "^3" "³" "³")
    ("radic" "\\sqrt{\\,}" t "&radic;" "[square root]" "[square root]" "√")
    ("sum" "\\sum" t "&sum;" "[sum]" "[sum]" "∑")
    ("prod" "\\prod" t "&prod;" "[product]" "[n-ary product]" "∏")
    ("micro" "\\textmu{}" nil "&micro;" "micro" "µ" "µ")
    ("macr" "\\textasciimacron{}" nil "&macr;" "[macron]" "¯" "¯")
    ("deg" "\\textdegree{}" nil "&deg;" "degree" "°" "°")
    ("prime" "\\prime" t "&prime;" "'" "'" "′")
    ("Prime" "\\prime{}\\prime" t "&Prime;" "''" "''" "″")
    ("infin" "\\propto" t "&infin;" "[infinity]" "[infinity]" "∞")
    ("infty" "\\infty" t "&infin;" "[infinity]" "[infinity]" "∞")
    ("prop" "\\propto" t "&prop;" "[proportional to]" "[proportional to]" "∝")
    ("proptp" "\\propto" t "&prop;" "[proportional to]" "[proportional to]" "∝")
    ("not" "\\textlnot{}" nil "&not;" "[angled dash]" "¬" "¬")
    ("land" "\\land" t "&and;" "[logical and]" "[logical and]" "∧")
    ("wedge" "\\wedge" t "&and;" "[logical and]" "[logical and]" "∧")
    ("lor" "\\lor" t "&or;" "[logical or]" "[logical or]" "∨")
    ("vee" "\\vee" t "&or;" "[logical or]" "[logical or]" "∨")
    ("cap" "\\cap" t "&cap;" "[intersection]" "[intersection]" "∩")
    ("cup" "\\cup" t "&cup;" "[union]" "[union]" "∪")
    ("int" "\\int" t "&int;" "[integral]" "[integral]" "∫")
    ("there4" "\\therefore" t "&there4;" "[therefore]" "[therefore]" "∴")
    ("sim" "\\sim" t "&sim;" "~" "~" "∼")
    ("cong" "\\cong" t "&cong;" "[approx. equal to]" "[approx. equal to]" "≅")
    ("simeq" "\\simeq" t "&cong;"  "[approx. equal to]" "[approx. equal to]" "≅")
    ("asymp" "\\asymp" t "&asymp;" "[almost equal to]" "[almost equal to]" "≈")
    ("approx" "\\approx" t "&asymp;" "[almost equal to]" "[almost equal to]" "≈")
    ("ne" "\\ne" t "&ne;" "[not equal to]" "[not equal to]" "≠")
    ("neq" "\\neq" t "&ne;" "[not equal to]" "[not equal to]" "≠")
    ("equiv" "\\equiv" t "&equiv;" "[identical to]" "[identical to]" "≡")
    ("le" "\\le" t "&le;" "<=" "<=" "≤")
    ("ge" "\\ge" t "&ge;" ">=" ">=" "≥")
    ("sub" "\\subset" t "&sub;" "[subset of]" "[subset of]" "⊂")
    ("subset" "\\subset" t "&sub;" "[subset of]" "[subset of]" "⊂")
    ("sup" "\\supset" t "&sup;" "[superset of]" "[superset of]" "⊃")
    ("supset" "\\supset" t "&sup;" "[superset of]" "[superset of]" "⊃")
    ("nsub" "\\not\\subset" t "&nsub;" "[not a subset of]" "[not a subset of" "⊄")
    ("sube" "\\subseteq" t "&sube;" "[subset of or equal to]" "[subset of or equal to]" "⊆")
    ("nsup" "\\not\\supset" t "&nsup;" "[not a superset of]" "[not a superset of]" "⊅")
    ("supe" "\\supseteq" t "&supe;" "[superset of or equal to]" "[superset of or equal to]" "⊇")
    ("forall" "\\forall" t "&forall;" "[for all]" "[for all]" "∀")
    ("exist" "\\exists" t "&exist;" "[there exists]" "[there exists]" "∃")
    ("exists" "\\exists" t "&exist;" "[there exists]" "[there exists]" "∃")
    ("empty" "\\empty" t "&empty;" "[empty set]" "[empty set]" "∅")
    ("emptyset" "\\emptyset" t "&empty;" "[empty set]" "[empty set]" "∅")
    ("isin" "\\in" t "&isin;" "[element of]" "[element of]" "∈")
    ("in" "\\in" t "&isin;" "[element of]" "[element of]" "∈")
    ("notin" "\\notin" t "&notin;" "[not an element of]" "[not an element of]" "∉")
    ("ni" "\\ni" t "&ni;" "[contains as member]" "[contains as member]" "∋")
    ("nabla" "\\nabla" t "&nabla;" "[nabla]" "[nabla]" "∇")
    ("ang" "\\angle" t "&ang;" "[angle]" "[angle]" "∠")
    ("angle" "\\angle" t "&ang;" "[angle]" "[angle]" "∠")
    ("perp" "\\perp" t "&perp;" "[up tack]" "[up tack]" "⊥")
    ("sdot" "\\cdot" t "&sdot;" "[dot]" "[dot]" "⋅")
    ("cdot" "\\cdot" t "&sdot;" "[dot]" "[dot]" "⋅")
    ("lceil" "\\lceil" t "&lceil;" "[left ceiling]" "[left ceiling]" "⌈")
    ("rceil" "\\rceil" t "&rceil;" "[right ceiling]" "[right ceiling]" "⌉")
    ("lfloor" "\\lfloor" t "&lfloor;" "[left floor]" "[left floor]" "⌊")
    ("rfloor" "\\rfloor" t "&rfloor;" "[right floor]" "[right floor]" "⌋")
    ("lang" "\\langle" t "&lang;" "<" "<" "⟨")
    ("rang" "\\rangle" t "&rang;" ">" ">" "⟩")

    "** Arrows"
    ("larr" "\\leftarrow" t "&larr;" "<-" "<-" "←")
    ("leftarrow" "\\leftarrow" t "&larr;"  "<-" "<-" "←")
    ("gets" "\\gets" t "&larr;"  "<-" "<-" "←")
    ("lArr" "\\Leftarrow" t "&lArr;" "<=" "<=" "⇐")
    ("Leftarrow" "\\Leftarrow" t "&lArr;" "<=" "<=" "⇐")
    ("uarr" "\\uparrow" t "&uarr;" "[uparrow]" "[uparrow]" "↑")
    ("uparrow" "\\uparrow" t "&uarr;" "[uparrow]" "[uparrow]" "↑")
    ("uArr" "\\Uparrow" t "&uArr;" "[dbluparrow]" "[dbluparrow]" "⇑")
    ("Uparrow" "\\Uparrow" t "&uArr;" "[dbluparrow]" "[dbluparrow]" "⇑")
    ("rarr" "\\rightarrow" t "&rarr;" "->" "->" "→")
    ("to" "\\to" t "&rarr;" "->" "->" "→")
    ("rightarrow" "\\rightarrow" t "&rarr;"  "->" "->" "→")
    ("rArr" "\\Rightarrow" t "&rArr;" "=>" "=>" "⇒")
    ("Rightarrow" "\\Rightarrow" t "&rArr;" "=>" "=>" "⇒")
    ("darr" "\\downarrow" t "&darr;" "[downarrow]" "[downarrow]" "↓")
    ("downarrow" "\\downarrow" t "&darr;" "[downarrow]" "[downarrow]" "↓")
    ("dArr" "\\Downarrow" t "&dArr;" "[dbldownarrow]" "[dbldownarrow]" "⇓")
    ("Downarrow" "\\Downarrow" t "&dArr;" "[dbldownarrow]" "[dbldownarrow]" "⇓")
    ("harr" "\\leftrightarrow" t "&harr;" "<->" "<->" "↔")
    ("leftrightarrow" "\\leftrightarrow" t "&harr;"  "<->" "<->" "↔")
    ("hArr" "\\Leftrightarrow" t "&hArr;" "<=>" "<=>" "⇔")
    ("Leftrightarrow" "\\Leftrightarrow" t "&hArr;" "<=>" "<=>" "⇔")
    ("crarr" "\\hookleftarrow" t "&crarr;" "<-'" "<-'" "↵")
    ("hookleftarrow" "\\hookleftarrow" t "&crarr;"  "<-'" "<-'" "↵")

    "** Function names"
    ("arccos" "\\arccos" t "arccos" "arccos" "arccos" "arccos")
    ("arcsin" "\\arcsin" t "arcsin" "arcsin" "arcsin" "arcsin")
    ("arctan" "\\arctan" t "arctan" "arctan" "arctan" "arctan")
    ("arg" "\\arg" t "arg" "arg" "arg" "arg")
    ("cos" "\\cos" t "cos" "cos" "cos" "cos")
    ("cosh" "\\cosh" t "cosh" "cosh" "cosh" "cosh")
    ("cot" "\\cot" t "cot" "cot" "cot" "cot")
    ("coth" "\\coth" t "coth" "coth" "coth" "coth")
    ("csc" "\\csc" t "csc" "csc" "csc" "csc")
    ("deg" "\\deg" t "&deg;" "deg" "deg" "deg")
    ("det" "\\det" t "det" "det" "det" "det")
    ("dim" "\\dim" t "dim" "dim" "dim" "dim")
    ("exp" "\\exp" t "exp" "exp" "exp" "exp")
    ("gcd" "\\gcd" t "gcd" "gcd" "gcd" "gcd")
    ("hom" "\\hom" t "hom" "hom" "hom" "hom")
    ("inf" "\\inf" t "inf" "inf" "inf" "inf")
    ("ker" "\\ker" t "ker" "ker" "ker" "ker")
    ("lg" "\\lg" t "lg" "lg" "lg" "lg")
    ("lim" "\\lim" t "lim" "lim" "lim" "lim")
    ("liminf" "\\liminf" t "liminf" "liminf" "liminf" "liminf")
    ("limsup" "\\limsup" t "limsup" "limsup" "limsup" "limsup")
    ("ln" "\\ln" t "ln" "ln" "ln" "ln")
    ("log" "\\log" t "log" "log" "log" "log")
    ("max" "\\max" t "max" "max" "max" "max")
    ("min" "\\min" t "min" "min" "min" "min")
    ("Pr" "\\Pr" t "Pr" "Pr" "Pr" "Pr")
    ("sec" "\\sec" t "sec" "sec" "sec" "sec")
    ("sin" "\\sin" t "sin" "sin" "sin" "sin")
    ("sinh" "\\sinh" t "sinh" "sinh" "sinh" "sinh")
    ("sup" "\\sup" t "&sup;" "sup" "sup" "sup")
    ("tan" "\\tan" t "tan" "tan" "tan" "tan")
    ("tanh" "\\tanh" t "tanh" "tanh" "tanh" "tanh")

    "** Signs & Symbols"
    ("bull" "\\textbullet{}" nil "&bull;" "*" "*" "•")
    ("bullet" "\\textbullet{}" nil "&bull;" "*" "*" "•")
    ("star" "\\star" t "*" "*" "*" "⋆")
    ("lowast" "\\ast" t "&lowast;" "*" "*" "∗")
    ("ast" "\\ast" t "&lowast;" "*" "*" "*")
    ("odot" "\\odot" t "o" "[circled dot]" "[circled dot]" "ʘ")
    ("oplus" "\\oplus" t "&oplus;" "[circled plus]" "[circled plus]" "⊕")
    ("otimes" "\\otimes" t "&otimes;" "[circled times]" "[circled times]" "⊗")
    ("checkmark" "\\checkmark" t "&#10003;" "[checkmark]" "[checkmark]" "✓")

    "** Miscellaneous (seldom used)"
    ("para" "\\P{}" nil "&para;" "[pilcrow]" "¶" "¶")
    ("ordf" "\\textordfeminine{}" nil "&ordf;" "_a_" "ª" "ª")
    ("ordm" "\\textordmasculine{}" nil "&ordm;" "_o_" "º" "º")
    ("cedil" "\\c{}" nil "&cedil;" "[cedilla]" "¸" "¸")
    ("oline" "\\overline{~}" t "&oline;" "[overline]" "¯" "‾")
    ("uml" "\\textasciidieresis{}" nil "&uml;" "[diaeresis]" "¨" "¨")
    ("zwnj" "\\/{}" nil "&zwnj;" "" "" "‌")
    ("zwj" "" nil "&zwj;" "" "" "‍")
    ("lrm" "" nil "&lrm;" "" "" "‎")
    ("rlm" "" nil "&rlm;" "" "" "‏")

    "** Smilies"
    ("smile" "\\smile" t "&#9786;" ":-)" ":-)" "⌣")
    ("smiley" "\\smiley{}" nil "&#9786;" ":-)" ":-)" "☺")
    ("blacksmile" "\\blacksmiley{}" nil "&#9787;" ":-)" ":-)" "☻")
    ("sad" "\\frownie{}" nil "&#9785;" ":-(" ":-(" "☹")

    "** Suits"
    ("clubs" "\\clubsuit" t "&clubs;" "[clubs]" "[clubs]" "♣")
    ("clubsuit" "\\clubsuit" t "&clubs;" "[clubs]" "[clubs]" "♣")
    ("spades" "\\spadesuit" t "&spades;" "[spades]" "[spades]" "♠")
    ("spadesuit" "\\spadesuit" t "&spades;" "[spades]" "[spades]" "♠")
    ("hearts" "\\heartsuit" t "&hearts;" "[hearts]" "[hearts]" "♥")
    ("heartsuit" "\\heartsuit" t "&heartsuit;" "[hearts]" "[hearts]" "♥")
    ("diams" "\\diamondsuit" t "&diams;" "[diamonds]" "[diamonds]" "♦")
    ("diamondsuit" "\\diamondsuit" t "&diams;" "[diamonds]" "[diamonds]" "♦")
    ("Diamond" "\\diamond" t "&diamond;" "[diamond]" "[diamond]" "⋄")
    ("loz" "\\diamond" t "&loz;" "[lozenge]" "[lozenge]" "◊")
    )
  "Default entities used in Org-mode to produce special characters.
For details see `org-entities-user'.")

(defsubst org-entity-get (name)
  "Get the proper association for NAME from the entity lists.
This first checks the user list, then the built-in list."
  (or (assoc name org-entities-user)
      (assoc name org-entities)))

(defun org-entity-get-representation (name kind)
  "Get the correct representation of entity NAME for export type KIND.
Kind can be any of `latex', `html', `ascii', `latin1', or `utf8'."
  (let* ((e (org-entity-get name))
	 (n (cdr (assq kind '((latex . 1) (html . 3) (ascii . 4)
			      (latin1 . 5) (utf8 . 6)))))
	 (r (and e n (nth n e))))
    (if (and e r
	     (not org-entities-ascii-explanatory)
	     (memq kind '(ascii latin1 utf8))
	     (= (string-to-char r) ?\[))
	(concat "\\" name)
      r)))

(defsubst org-entity-latex-math-p (name)
  "Does entity NAME require math mode in LaTeX?"
  (nth 2 (org-entity-get name)))

;; Helpfunctions to create a table for orgmode.org/worg/org-symbols.org

(defun org-entities-create-table ()
  "Create an org-mode table with all entities."
  (interactive)
  (let ((ll org-entities)
	(pos (point))
	e latex mathp html latin utf8 name ascii)
    (insert "|Name|LaTeX code|LaTeX|HTML code |HTML|ASCII|Latin1|UTF-8\n|-\n")
    (while ll
      (when (listp e)
	(setq e (pop ll))
	(setq name (car e)
	      latex (nth 1 e)
	      mathp (nth 2 e)
	      html (nth 3 e)
	      ascii (nth 4 e)
	      latin (nth 5 e)
	      utf8 (nth 6 e))
	(if (equal ascii "|") (setq ascii "\\vert"))
	(if (equal latin "|") (setq latin "\\vert"))
	(if (equal utf8  "|") (setq utf8  "\\vert"))
	(if (equal ascii "=>") (setq ascii "= >"))
	(if (equal latin "=>") (setq latin "= >"))
	(insert "|" name
		"|" (format "=%s=" latex)
		"|" (format (if mathp "$%s$" "$\\mbox{%s}$")
			    latex)
		"|" (format "=%s=" html) "|" html
		"|" ascii "|" latin "|" utf8
		"|\n")))
    (goto-char pos)
    (org-table-align)))

(defun org-entities-help ()
  "Create a Help buffer with all available entities."
  (interactive)
  (with-output-to-temp-buffer "*Org Entity Help*"
    (princ "Org-mode entities\n=================\n\n")
    (let ((ll (append '("* User-defined additions (variable org-entities-user)")
		      org-entities-user
		      org-entities))
	  e latex mathp html latin utf8 name ascii
	  (lastwasstring t)
	  (head (concat
		 "\n"
		 "   Symbol   Org entity        LaTeX code             HTML code\n"
		 "   -----------------------------------------------------------\n")))
      (while ll
	(setq e (pop ll))
	(if (stringp e)
	    (progn
	      (princ e)
	      (princ "\n")
	      (setq lastwasstring t))
	  (if lastwasstring (princ head))
	  (setq lastwasstring nil)
	  (setq name (car e)
		latex (nth 1 e)
		html (nth 3 e)
		utf8 (nth 6 e))
	  (princ (format "   %-8s \\%-16s %-22s %-13s\n"
			 utf8 name latex html))))))
  (with-current-buffer "*Org Entity Help*"
    (org-mode))
  (select-window (get-buffer-window "*Org Entity Help*")))


(defun replace-amp ()
  "Postprocess HTML file to unescape the ampersand."
  (interactive)
  (while (re-search-forward "<td>&amp;\\([^<;]+;\\)" nil t)
    (replace-match (concat "<td>&" (match-string 1)) t t)))

(provide 'org-entities)

;; Local variables:
;; coding: utf-8
;; End:

;;; org-entities.el ends here
