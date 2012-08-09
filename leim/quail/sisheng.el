;;; sisheng.el --- sisheng input method for Chinese pinyin transliteration

;; Copyright (C) 2004-2012  Free Software Foundation, Inc.

;; Author: Werner LEMBERG <wl@gnu.org>

;; Keywords: multilingual, input method, Chinese, pinyin, sisheng

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

(require 'quail)

(defconst sisheng-regexp
  "[āēīōūǖ]\\|üē")

;; First element is the key,
;; second element is the vowel used for the input sequence,
;; last four elements are the resulting tones.
;;
(defconst sisheng-vowel-table
  '(("ā" "a" "ā" "á" "ǎ" "à")
    ("ē" "e" "ē" "é" "ě" "è")
    ("ī" "i" "ī" "í" "ǐ" "ì")
    ("ō" "o" "ō" "ó" "ǒ" "ò")
    ("ū" "u" "ū" "ú" "ǔ" "ù")
    ("ǖ" "v" "ǖ" "ǘ" "ǚ" "ǜ")
    ("üē" "ve" "üē" "üé" "üě" "üè")))


;; All possible syllables in Mandarin Chinese, presented in the first
;; tone.  Note that make-sisheng-rules always constructs rules for all
;; four tones even if some of those tones aren't used in Mandarin.
;;
(defconst sisheng-syllable-table
  '("ā" "āi" "ān" "āng" "āo"

    "bā" "bāi" "bān" "bāng" "bāo"
    "bēi" "bēn" "bēng"
    "bī" "biān" "biāo" "biē" "bīn" "bīng"
    "bō"
    "bū"

    "cā" "cāi" "cān" "cāng" "cāo"
    "cē" "cēn" "cēng"
    "cī"
    "cōng" "cōu"
    "cū" "cuān" "cuī" "cūn" "cuō"

    "chā" "chāi" "chān" "chāng" "chāo"
    "chē" "chēn" "chēng"
    "chī"
    "chōng" "chōu"
    "chū" "chuā" "chuāi" "chuān" "chuāng" "chuī" "chūn" "chuō"

    "dā" "dāi" "dān" "dāng" "dāo"
    "dē" "dēi" "dēn" "dēng"
    "dī" "diān" "diāo" "diē" "dīng" "diū"
    "dōng" "dōu"
    "dū" "duān" "duī" "dūn" "duō"

    "ē" "ēi" "ēn" "ēng" "ēr"

    "fā" "fān" "fāng"
    "fēi" "fēn" "fēng"
    "fiāo"
    "fō" "fōu"
    "fū"

    "gā" "gāi" "gān" "gāng" "gāo"
    "gē" "gēi" "gēn" "gēng"
    "gōng" "gōu"
    "gū" "guā" "guāi" "guān" "guāng" "guī" "gūn" "guō"

    "hā" "hāi" "hān" "hāng" "hāo"
    "hē" "hēi" "hēn" "hēng"
    "hōng" "hōu"
    "hū" "huā" "huāi" "huān" "huāng" "huī" "hūn" "huō"

    "jī" "jiā" "jiān" "jiāng" "jiāo" "jiē" "jīn" "jīng" "jiōng" "jiū"
    "jū" "juān" "juē" "jūn"

    "kā" "kāi" "kān" "kāng" "kāo"
    "kē" "kēi" "kēn" "kēng"
    "kōng" "kōu"
    "kū" "kuā" "kuāi" "kuān" "kuāng" "kuī" "kūn" "kuō"

    "lā" "lāi" "lān" "lāng" "lāo"
    "lē" "lēi" "lēng"
    "lī" "liā" "liān" "liāng" "liāo" "liē" "līn" "līng" "liū"
    "lōng" "lōu"
    "lū" "luān" "lūn" "luō"
    "lǖ" "lüē"

    "mā" "māi" "mān" "māng" "māo"
    "mē" "mēi" "mēn" "mēng"
    "mī" "miān" "miāo" "miē" "mīn" "mīng" "miū"
    "mō" "mōu"
    "mū"

    "nā" "nāi" "nān" "nāng" "nāo"
    "nē" "nēi" "nēn" "nēng"
    "nī" "niān" "niāng" "niāo" "niē" "nīn" "nīng" "niū"
    "nōng" "nōu"
    "nū" "nuān" "nuō"
    "nǖ" "nüē"

    "ō" "ōu"

    "pā" "pāi" "pān" "pāng" "pāo"
    "pēi" "pēn" "pēng"
    "pī" "piān" "piāo" "piē" "pīn" "pīng"
    "pō" "pōu"
    "pū"

    "qī" "qiā" "qiān" "qiāng" "qiāo" "qiē" "qīn" "qīng" "qiōng" "qiū"
    "qū" "quān" "quē" "qūn"

    "rān" "rāng" "rāo"
    "rē" "rēn" "rēng"
    "rī"
    "rōng" "rōu"
    "rū" "ruā" "ruān" "ruī" "rūn" "ruō"

    "sā" "sāi" "sān" "sāng" "sāo"
    "sē" "sēn" "sēng"
    "sī"
    "sōng" "sōu"
    "sū" "suān" "suī" "sūn" "suō"

    "shā" "shāi" "shān" "shāng" "shāo"
    "shē" "shēi" "shēn" "shēng"
    "shī"
    "shōu"
    "shū" "shuā" "shuāi" "shuān" "shuāng" "shuī" "shūn" "shuō"

    "tā" "tāi" "tān" "tāng" "tāo"
    "tē" "tēi" "tēng"
    "tī" "tiān" "tiāo" "tiē" "tīng"
    "tōng" "tōu"
    "tū" "tuān" "tuī" "tūn" "tuō"

    "wā" "wāi" "wān" "wāng"
    "wēi" "wēn" "wēng"
    "wō"
    "wū"

    "xī" "xiā" "xiān" "xiāng" "xiāo" "xiē" "xīn" "xīng" "xiōng" "xiū"
    "xū" "xuān" "xuē" "xūn"

    "yā" "yān" "yāng" "yāo"
    "yē"
    "yī" "yīn" "yīng"
    "yō" "yōng" "yōu"
    "yū" "yuān" "yuē" "yūn"

    "zā" "zāi" "zān" "zāng" "zāo"
    "zē" "zēi" "zēn" "zēng"
    "zī"
    "zōng" "zōu"
    "zū" "zuān" "zuī" "zūn" "zuō"

    "zhā" "zhāi" "zhān" "zhāng" "zhāo"
    "zhē" "zhēi" "zhēn" "zhēng"
    "zhī"
    "zhōng" "zhōu"
    "zhū" "zhuā" "zhuāi" "zhuān" "zhuāng" "zhuī" "zhūn" "zhuō"))

;; This function converts e.g.
;;
;;   "zhuō"
;;
;; into
;;
;;   (("zhuo4" ["zhuò"])
;;    ("zhuo3" ["zhuǒ"])
;;    ("zhuo2" ["zhuó"])
;;    ("zhuo1" ["zhuō"]))
;;
(defun quail-make-sisheng-rules (syllable)
  (let ((case-fold-search t)
	vowel-match
	vowel-list
	input-vowel
	base-key
	key
	value
	key-value-list
	(i 1))
    (string-match sisheng-regexp syllable)
    (setq vowel-match (downcase (match-string 0 syllable)))
    (setq vowel-list
	  (cdr (assoc-string vowel-match sisheng-vowel-table)))
    (setq input-vowel (car vowel-list))
    (setq base-key (replace-match input-vowel nil nil syllable))
    (while (<= i 4)
      (setq key (concat base-key (number-to-string i)))
      (setq value (vector (replace-match (nth i vowel-list) nil nil syllable)))
      (push (list key value) key-value-list)
      (setq i (1+ i)))
    key-value-list))

;; Set up sisheng input method.
;;
(quail-define-package
 "chinese-sisheng"			; name
 "Chinese"				; language
 "ǚ"					; title
 t					; guidance
 "Sìshēng input method for pīnyīn transliteration of Chinese.

Examples: shuang1 -> shuāng
          Lv3     -> Lǚ
          AN4     -> ÀN

Use the fifth (unstressed) tone for syllables containing `ü'
without a tone mark.

Example:  nve5    -> nüe
"					; docstring
 nil					; translation-keys
 t					; forget-last-selection
 nil					; deterministic
 nil					; kbd-translate
 nil					; show-layout
 nil					; create-decode-map
 nil					; maximum-shortest
 nil					; overlay-plist
 nil					; update-translation-function
 nil					; conversion-keys
 t					; simple
 )

;; Call quail-make-sisheng-rules for all syllables in sisheng-syllable-table.
;;
(let ((case-table-save (current-case-table))
      sisheng-list)
  (set-case-table (standard-case-table))
  (dolist (syllable sisheng-syllable-table)
    (setq sisheng-list
	  (append (quail-make-sisheng-rules syllable)
		  sisheng-list)))

  (dolist (syllable sisheng-syllable-table)
    (setq sisheng-list
	  (append (quail-make-sisheng-rules (upcase-initials syllable))
		  sisheng-list)))

  (dolist (syllable sisheng-syllable-table)
    (setq sisheng-list
	  (append (quail-make-sisheng-rules (upcase syllable))
		  sisheng-list)))

  (eval `(quail-define-rules
	  ,@sisheng-list

	  ("lv5" ["lü"])
	  ("lve5" ["lüe"])
	  ("nv5" ["nü"])
	  ("nve5" ["nüe"])

	  ("Lv5" ["Lü"])
	  ("Lve5" ["Lüe"])
	  ("Nv5" ["Nü"])
	  ("Nve5" ["Nüe"])

	  ("LV5" ["LÜ"])
	  ("LVE5" ["LÜE"])
	  ("NV5" ["NÜ"])
	  ("NVE5" ["NÜE"])))
  (set-case-table case-table-save))

;; Local Variables:
;; coding: utf-8
;; End:

;;; sisheng.el ends here
