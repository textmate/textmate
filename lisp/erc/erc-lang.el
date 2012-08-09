;;; erc-lang.el --- provide the LANG command to ERC

;; Copyright (C) 2002, 2004, 2006-2012  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.0.0
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcLang
;; Keywords: comm languages processes

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

;; This provides two commands: `language' is for everyday use, and
;; `erc-cmd-LANG' provides the /LANG command to ERC.

;;; Code:

(require 'erc)

(defvar iso-638-languages
  '(("aa" . "Afar")
    ("ab" . "Abkhazian")
    ("af" . "Afrikaans")
    ("am" . "Amharic")
    ("ar" . "Arabic")
    ("as" . "Assamese")
    ("ay" . "Aymara")
    ("az" . "Azerbaijani")
    ("ba" . "Bashkir")
    ("be" . "Byelorussian")
    ("bg" . "Bulgarian")
    ("bh" . "Bihari")
    ("bi" . "Bislama")
    ("bn" . "Bengali; Bangla")
    ("bo" . "Tibetan")
    ("br" . "Breton")
    ("ca" . "Catalan")
    ("co" . "Corsican")
    ("cs" . "Czech")
    ("cy" . "Welsh")
    ("da" . "Danish")
    ("de" . "German")
    ("dz" . "Bhutani")
    ("el" . "Greek")
    ("en" . "English")
    ("eo" . "Esperanto")
    ("es" . "Spanish")
    ("et" . "Estonian")
    ("eu" . "Basque")
    ("fa" . "Persian")
    ("fi" . "Finnish")
    ("fj" . "Fiji")
    ("fo" . "Faroese")
    ("fr" . "French")
    ("fy" . "Frisian")
    ("ga" . "Irish")
    ("gd" . "Scots Gaelic")
    ("gl" . "Galician")
    ("gn" . "Guarani")
    ("gu" . "Gujarati")
    ("ha" . "Hausa")
    ("he" . "Hebrew (formerly iw)")
    ("hi" . "Hindi")
    ("hr" . "Croatian")
    ("hu" . "Hungarian")
    ("hy" . "Armenian")
    ("ia" . "Interlingua")
    ("id" . "Indonesian (formerly in)")
    ("ie" . "Interlingue")
    ("ik" . "Inupiak")
    ("is" . "Icelandic")
    ("it" . "Italian")
    ("iu" . "Inuktitut")
    ("ja" . "Japanese")
    ("jw" . "Javanese")
    ("ka" . "Georgian")
    ("kk" . "Kazakh")
    ("kl" . "Greenlandic")
    ("km" . "Cambodian")
    ("kn" . "Kannada")
    ("ko" . "Korean")
    ("ks" . "Kashmiri")
    ("ku" . "Kurdish")
    ("ky" . "Kirghiz")
    ("la" . "Latin")
    ("ln" . "Lingala")
    ("lo" . "Laothian")
    ("lt" . "Lithuanian")
    ("lv" . "Latvian, Lettish")
    ("mg" . "Malagasy")
    ("mi" . "Maori")
    ("mk" . "Macedonian")
    ("ml" . "Malayalam")
    ("mn" . "Mongolian")
    ("mo" . "Moldavian")
    ("mr" . "Marathi")
    ("ms" . "Malay")
    ("mt" . "Maltese")
    ("my" . "Burmese")
    ("na" . "Nauru")
    ("ne" . "Nepali")
    ("nl" . "Dutch")
    ("no" . "Norwegian")
    ("oc" . "Occitan")
    ("om" . "(Afan) Oromo")
    ("or" . "Oriya")
    ("pa" . "Punjabi")
    ("pl" . "Polish")
    ("ps" . "Pashto, Pushto")
    ("pt" . "Portuguese")
    ("qu" . "Quechua")
    ("rm" . "Rhaeto-Romance")
    ("rn" . "Kirundi")
    ("ro" . "Romanian")
    ("ru" . "Russian")
    ("rw" . "Kinyarwanda")
    ("sa" . "Sanskrit")
    ("sd" . "Sindhi")
    ("sg" . "Sangho")
    ("sh" . "Serbo-Croatian")
    ("si" . "Sinhalese")
    ("sk" . "Slovak")
    ("sl" . "Slovenian")
    ("sm" . "Samoan")
    ("sn" . "Shona")
    ("so" . "Somali")
    ("sq" . "Albanian")
    ("sr" . "Serbian")
    ("ss" . "Siswati")
    ("st" . "Sesotho")
    ("su" . "Sundanese")
    ("sv" . "Swedish")
    ("sw" . "Swahili")
    ("ta" . "Tamil")
    ("te" . "Telugu")
    ("tg" . "Tajik")
    ("th" . "Thai")
    ("ti" . "Tigrinya")
    ("tk" . "Turkmen")
    ("tl" . "Tagalog")
    ("tn" . "Setswana")
    ("to" . "Tonga")
    ("tr" . "Turkish")
    ("ts" . "Tsonga")
    ("tt" . "Tatar")
    ("tw" . "Twi")
    ("ug" . "Uighur")
    ("uk" . "Ukrainian")
    ("ur" . "Urdu")
    ("uz" . "Uzbek")
    ("vi" . "Vietnamese")
    ("vo" . "Volapuk")
    ("wo" . "Wolof")
    ("xh" . "Xhosa")
    ("yi" . "Yiddish (formerly ji)")
    ("yo" . "Yoruba")
    ("za" . "Zhuang")
    ("zh" . "Chinese")
    ("zu" . "Zulu"))
  "Alist of ISO language codes and language names.
This is based on the technical contents of ISO 639:1988 (E/F)
\"Code for the representation of names of languages\".

Typed by Keld.Simonsen@dkuug.dk 1990-11-30
   <ftp://dkuug.dk/i18n/ISO_639>
Minor corrections, 1992-09-08 by Keld Simonsen
Sundanese corrected, 1992-11-11 by Keld Simonsen
Telugu corrected, 1995-08-24 by Keld Simonsen
Hebrew, Indonesian, Yiddish corrected 1995-10-10 by Michael Everson
Inuktitut, Uighur, Zhuang added 1995-10-10 by Michael Everson
Sinhalese corrected, 1995-10-10 by Michael Everson
Faeroese corrected to Faroese, 1995-11-18 by Keld Simonsen
Sangro corrected to Sangho, 1996-07-28 by Keld Simonsen

Two-letter lower-case symbols are used.
The Registration Authority for ISO 639 is Infoterm, Osterreichisches
Normungsinstitut (ON), Postfach 130, A-1021 Vienna, Austria.")

(defun language (code)
  "Return the language name for the ISO CODE."
  (interactive (list (completing-read "ISO language code: "
				      iso-638-languages)))
  (message "%s" (cdr (assoc code iso-638-languages))))

(defun erc-cmd-LANG (language)
  "Display the language name for the language code given by LANGUAGE."
  (let ((lang (cdr (assoc language iso-638-languages))))
    (erc-display-message
     nil 'notice 'active
     (or lang (concat language ": No such domain"))))
  t)

(provide 'erc-lang)

;;; erc-lang.el ends here
