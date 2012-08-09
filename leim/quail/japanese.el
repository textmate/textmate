;;; japanese.el --- Quail package for inputting Japanese  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, input method, Japanese

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
(require 'kkc)

(defvar quail-japanese-use-double-n nil
  "If non-nil, use type \"nn\" to insert ん.")

;; Update Quail translation region while considering Japanese bizarre
;; translation rules.
(defun quail-japanese-update-translation (control-flag)
  (if (null control-flag)
      (setq quail-current-str
	    (if (/= (aref quail-current-key 0) ?q)
		(or quail-current-str quail-current-key)
	      ""))
    (if (integerp control-flag)
	(let ((keylen (length quail-current-key)))
	  (cond ((= control-flag 0)
		 (setq quail-current-str (aref quail-current-key 0)
		       control-flag t))
		((= (aref quail-current-key 0) ?n)
		 (setq quail-current-str ?ん)
		 (if (and quail-japanese-use-double-n
			  (> keylen 0)
			  (= (aref quail-current-key 1) ?n))
		     (setq control-flag t)))
		((and (> keylen 1)
		      (= (aref quail-current-key 0) (aref quail-current-key 1)))
		 (setq quail-current-str ?っ))
		(t
		 (setq quail-current-str (aref quail-current-key 0))))
	  (if (integerp control-flag)
	      (setq unread-command-events
		    (string-to-list
		     (substring quail-current-key control-flag)))))))
  control-flag)

;; Convert Hiragana <-> Katakana in the current translation region.
(defun quail-japanese-toggle-kana ()
  (interactive)
  (setq quail-translating nil)
  (let ((start (overlay-start quail-conv-overlay))
	(end (overlay-end quail-conv-overlay)))
    (save-excursion
      (goto-char start)
      (if (re-search-forward "\\cH" end t)
	  (japanese-katakana-region start end)
	(japanese-hiragana-region start end)))
    (setq quail-conversion-str
	  (buffer-substring (overlay-start quail-conv-overlay)
			    (overlay-end quail-conv-overlay)))))

;; Convert Hiragana in the current translation region to Kanji by KKC
;; (Kana Kanji Converter) utility.
(defun quail-japanese-kanji-kkc ()
  (interactive)
  (when (= (char-before (overlay-end quail-conv-overlay)) ?n)
    ;; The last char is `n'.  We had better convert it to `ん'
    ;; before kana-kanji conversion.
    (goto-char (1- (overlay-end quail-conv-overlay)))
    (insert ?ん)
    (delete-char 1))
  (let* ((from (copy-marker (overlay-start quail-conv-overlay)))
	 (len (- (overlay-end quail-conv-overlay) from)))
    (quail-delete-overlays)
    (setq quail-current-str nil)
    (unwind-protect
	(let ((result (kkc-region from (+ from len))))
	  (move-overlay quail-conv-overlay from (point))
	  (setq quail-conversion-str (buffer-substring from (point)))
	  (if (= (+ from result) (point))
	      (setq quail-converting nil))
	  (setq quail-translating nil))
      (set-marker from nil))))

(defun quail-japanese-self-insert-and-switch-to-alpha (key idx)
  (quail-delete-region)
  (setq unread-command-events (list (aref key (1- idx))))
  (quail-japanese-switch-package "q" 1))

(defvar quail-japanese-switch-table
  '((?z . "japanese-zenkaku")
    (?k . "japanese-hankaku-kana")
    (?h . "japanese")
    (?q . ("japanese-ascii"))))

(defvar quail-japanese-package-saved nil)
(make-variable-buffer-local 'quail-japanese-package-saved)
(put 'quail-japanese-package-saved 'permanent-local t)

(defun quail-japanese-switch-package (key idx)
  (quail-delete-region)
  (setq quail-current-str nil
	quail-converting nil
	quail-conversion-str "")
  (let ((pkg (cdr (assq (aref key (1- idx)) quail-japanese-switch-table))))
    (if (null pkg)
	(quail-error "No package to be switched")
      (if (stringp pkg)
	  (activate-input-method pkg)
	(if (string= (car pkg) current-input-method)
	    (if quail-japanese-package-saved
		(activate-input-method quail-japanese-package-saved))
	  (setq quail-japanese-package-saved current-input-method)
	  (activate-input-method (car pkg))))))
  (throw 'quail-tag nil))

(defvar quail-japanese-transliteration-rules
  '(( "a" "あ") ( "i" "い") ( "u" "う") ( "e" "え") ( "o" "お")
    ("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
    ("sa" "さ") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
    ("ta" "た") ("ti" "ち") ("tu" "つ") ("te" "て") ("to" "と")
    ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
    ("ha" "は") ("hi" "ひ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
    ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
    ("ya" "や")             ("yu" "ゆ")             ("yo" "よ")
    ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
    ("la" "ら") ("li" "り") ("lu" "る") ("le" "れ") ("lo" "ろ")
    ("wa" "わ") ("wi" "ゐ") ("wu" "う") ("we" "ゑ") ("wo" "を")
    ("n'" "ん")
    ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
    ("za" "ざ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
    ("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
    ("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
    ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ")

    ("kya" ["きゃ"]) ("kyu" ["きゅ"]) ("kye" ["きぇ"]) ("kyo" ["きょ"])
    ("sya" ["しゃ"]) ("syu" ["しゅ"]) ("sye" ["しぇ"]) ("syo" ["しょ"])
    ("sha" ["しゃ"]) ("shu" ["しゅ"]) ("she" ["しぇ"]) ("sho" ["しょ"])
    ("cha" ["ちゃ"]) ("chu" ["ちゅ"]) ("che" ["ちぇ"]) ("cho" ["ちょ"])
    ("tya" ["ちゃ"]) ("tyu" ["ちゅ"]) ("tye" ["ちぇ"]) ("tyo" ["ちょ"])
    ("nya" ["にゃ"]) ("nyu" ["にゅ"]) ("nye" ["にぇ"]) ("nyo" ["にょ"])
    ("hya" ["ひゃ"]) ("hyu" ["ひゅ"]) ("hye" ["ひぇ"]) ("hyo" ["ひょ"])
    ("mya" ["みゃ"]) ("myu" ["みゅ"]) ("mye" ["みぇ"]) ("myo" ["みょ"])
    ("rya" ["りゃ"]) ("ryu" ["りゅ"]) ("rye" ["りぇ"]) ("ryo" ["りょ"])
    ("lya" ["りゃ"]) ("lyu" ["りゅ"]) ("lye" ["りぇ"]) ("lyo" ["りょ"])
    ("gya" ["ぎゃ"]) ("gyu" ["ぎゅ"]) ("gye" ["ぎぇ"]) ("gyo" ["ぎょ"])
    ("zya" ["じゃ"]) ("zyu" ["じゅ"]) ("zye" ["じぇ"]) ("zyo" ["じょ"])
    ("jya" ["じゃ"]) ("jyu" ["じゅ"]) ("jye" ["じぇ"]) ("jyo" ["じょ"])
    ( "ja" ["じゃ"]) ( "ju" ["じゅ"]) ( "je" ["じぇ"]) ( "jo" ["じょ"])
    ("bya" ["びゃ"]) ("byu" ["びゅ"]) ("bye" ["びぇ"]) ("byo" ["びょ"])
    ("pya" ["ぴゃ"]) ("pyu" ["ぴゅ"]) ("pye" ["ぴぇ"]) ("pyo" ["ぴょ"])

    ("kwa" ["くゎ"]) ("kwi" ["くぃ"]) ("kwe" ["くぇ"]) ("kwo" ["くぉ"])
    ("tsa" ["つぁ"]) ("tsi" ["つぃ"]) ("tse" ["つぇ"]) ("tso" ["つぉ"])
    ( "fa" ["ふぁ"]) ( "fi" ["ふぃ"]) ( "fe" ["ふぇ"]) ( "fo" ["ふぉ"])
    ("gwa" ["ぐゎ"]) ("gwi" ["ぐぃ"]) ("gwe" ["ぐぇ"]) ("gwo" ["ぐぉ"])

    ("dyi" ["でぃ"]) ("dyu" ["どぅ"]) ("dye" ["でぇ"]) ("dyo" ["どぉ"])
    ("xwi" ["うぃ"])                  ("xwe" ["うぇ"]) ("xwo" ["うぉ"])

    ("shi" "し") ("tyi" ["てぃ"]) ("chi" "ち") ("tsu" "つ") ("ji" "じ")
    ("fu"  "ふ")
    ("ye" ["いぇ"])

    ("va" ["ヴぁ"]) ("vi" ["ヴぃ"]) ("vu" "ヴ") ("ve" ["ヴぇ"]) ("vo" ["ヴぉ"])

    ("xa"  "ぁ") ("xi"  "ぃ") ("xu"  "ぅ") ("xe"  "ぇ") ("xo"  "ぉ")
    ("xtu" "っ") ("xya" "ゃ") ("xyu" "ゅ") ("xyo" "ょ") ("xwa" "ゎ")
    ("xka" "ヵ") ("xke" "ヶ")

    ("1" "１") ("2" "２") ("3" "３") ("4" "４") ("5" "５")
    ("6" "６") ("7" "７") ("8" "８") ("9" "９") ("0" "０")

    ("!" "！") ("@" "＠") ("#" "＃") ("$" "＄") ("%" "％")
    ("^" "＾") ("&" "＆") ("*" "＊") ("(" "（") (")" "）")
    ("-" "ー") ("=" "＝") ("`" "｀") ("\\" "￥") ("|" "｜")
    ("_" "＿") ("+" "＋") ("~" "￣") ("[" "「") ("]" "」")
    ("{" "｛") ("}" "｝") (":" "：") (";" "；") ("\""  "”")
    ("'" "’") ("." "。") ("," "、") ("<" "＜") (">" "＞")
    ("?" "？") ("/" "／")

    ("z1" "○") ("z!" "●")
    ("z2" "▽") ("z@" "▼")
    ("z3" "△") ("z#" "▲")
    ("z4" "□") ("z$" "■")
    ("z5" "◇") ("z%" "◆")
    ("z6" "☆") ("z^" "★")
    ("z7" "◎") ("z&" "£")
    ("z8" "¢") ("z*" "×")
    ("z9" "♂") ("z(" "【")
    ("z0" "♀") ("z)" "】")
    ("z-" "〜") ("z_" "∴")
    ("z=" "≠") ("z+" "±")
    ("z\\" "＼") ("z|" "‖")
    ("z`" "´") ("z~" "¨")

    ("zq" "《") ("zQ" "〈")
    ("zw" "》") ("zW" "〉")
    ("zr" "々") ("zR" "仝")
    ("zt" "〆") ("zT" "§")
    ("zp" "〒") ("zP" "↑")
    ("z[" "『") ("z{" "〔")
    ("z]" "』") ("z}" "〕")

    ("zs" "ヽ") ("zS" "ヾ")
    ("zd" "ゝ") ("zD" "ゞ")
    ("zf" "〃") ("zF" "→")
    ("zg" "‐") ("zG" "―")
    ("zh" "←")
    ("zj" "↓")
    ("zk" "↑")
    ("zl" "→")
    ("z;" "゛") ("z:" "゜")
    ("z\'" "‘") ("z\"" "“")

    ("zx" [":-"]) ("zX" [":-)"])
    ("zc" "〇") ("zC" "℃")
    ("zv" "※") ("zV" "÷")
    ("zb" "°") ("zB" "←")
    ("zn" "′") ("zN" "↓")
    ("zm" "″") ("zM" "〓")
    ("z," "‥") ("z<" "≦")
    ("z." "…") ("z>" "≧")
    ("z/" "・") ("z?" "∞")

    ("\\\\" quail-japanese-self-insert-and-switch-to-alpha)
    ("{{" quail-japanese-self-insert-and-switch-to-alpha)
    ("}}" quail-japanese-self-insert-and-switch-to-alpha)

    ("qq" quail-japanese-switch-package)
    ("qz" quail-japanese-switch-package)
    ))


;; ローマ字入力及び仮名漢字変換による日本語入力メソッド
;;
;; この入力メソッドでの日本語の入力は二つのステージ「ローマ字仮名変換」
;; と「仮名漢字変換」からなる。最初はローマ字仮名変換のステージで、ス
;; ペースキーを押すことにより、次のステージ「仮名漢字変換」へ進む。
;;
;; 「ローマ字仮名変換」
;;
;; 平仮名は小文字キー（列）を打つことにより入力。句読点、括弧類は対応
;; する英字キーを打つことにより入力。その他のシンボルは `z' に続けて何
;; れかのキーを打つことにより入力。下に全ての可能なキーシーケンスリス
;; トアップされている。入力された文字は下線で示される。
;;
;; さらに以下のキーで特別な処理を行う。
;;
;; K	平仮名を片仮名に、あるいは片仮名を平仮名に変換
;; qq	この入力メソッドと `japanese-ascii' 入力メソッドをトグル切替
;; qz	`japanese-zenkaku' 入力メソッドにシフト
;;	qh と打てば元に戻る
;; RET	現在の入力文字列を確定
;; SPC	仮名漢字変換に進む
;;
;; `japanese-ascii' 入力メソッドは ASCII 文字を入力するのに使う。これ
;; は入力メソッドをオフにするのとほとんど同じである。異なるのは qq と
;; 打つことにより、`japanese' 入力メソッドに戻れる点である。
;;
;; `japanese-zenkaku' 入力メソッドは全角英数字を入力するのに使う。
;;
;; 「ローマ字仮名変換」ステージでのキーシーケンスのリストは最後に付け
;; てある。
;;
;; 「仮名漢字変換」
;;
;; このステージでは、前ステージで入力された文字列を仮名漢字変換する。
;; 変換された文字列は、注目文節（反転表示）と残りの入力（下線表示）に
;; 分けられる。注目文節に対しては以下のコマンドが使える。
;;
;; SPC, C-n	kkc-next
;;	次の変換候補を表示
;;	kkc-show-conversion-list-count 以上続けて打てば、変換候補リス
;;	トをエコーエリアに表示
;; C-p		kkc-prev
;;	前の変換候補を表示
;;	kkc-show-conversion-list-count 以上続けて打てば、変換候補リス
;;	トをエコーエリアに表示
;; l		kkc-show-conversion-list-or-next-group
;;	最高１０個までの変換候補をエコーエリアに表示。
;;	続けて打たれれば、次の１０候補を表示。
;; L		kkc-show-conversion-list-or-prev-group
;;	最高１０個までの変換候補をエコーエリアに表示。
;;	続けて打たれれば、前の１０候補を表示。
;; 0..9		kkc-select-from-list
;;	打たれた数字の変換候補を選択
;; H		kkc-hiragana
;;	注目文節を平仮名に変換
;; K		kkc-katakana
;;	注目文節を片仮名に変換
;; C-o		kkc-longer
;;	注目文節を後ろに一文字伸ばす
;; C-i		kkc-shorter
;;	注目文節を後ろから一文字縮める
;; C-f		kkc-next-phrase
;;	注目文節を確定させる。もし残りの入力がまだあれば、最初の文節を
;;	選択し、それを注目文節とし、その最初の変換候補を表示する。
;; DEL, C-c	kkc-cancel
;;	仮名漢字変換をキャンセルし、ローマ字仮名変換のステージに戻る。
;; return		kkc-terminate
;;	全文節を確定させる。
;; C-SPC, C-@	kkc-first-char-only
;;	最初の文字を確定させ、残りは削除する。
;; C-h		kkc-help
;;	これらのキーバインドのリストを表示する。あ

(quail-define-package
 "japanese" "Japanese" "Aあ"
 nil
 "Japanese input method by Roman transliteration and Kana-Kanji conversion.

When you use this input method, text entry proceeds in two stages:
Roman-Kana transliteration and Kana-Kanji conversion.  When you start
to enter text, you are in the first stage, Roman-Kana transliteration.
Type SPC to proceed to the next stage, Kana-Kanji conversion.

:: Roman-Kana transliteration ::

You can input any Hiragana character as a sequence of lower-case
letters, Japanese punctuation characters by typing punctuation keys,
Japanese symbols by typing `z' followed by another key.  See below for
a list of all available sequences.  The characters you input are
underlined.

In addition, the following keys provide special effects:

K	Change Hiragana to Katakana or Katakana to Hiragana.
qq	Toggle between this input method and the input method `japanese-ascii'.
qz	Shift to the input method `japanese-zenkaku'.
	Typing \"qh\" will put you back to this input method.
RET	Accept the current character sequence.
SPC	Proceed to the next stage, Kana-Kanji conversion.

The input method `japanese-ascii' is used to enter ASCII characters.
This is almost the same as turning off the input method.  The only
difference is that typing `qq' will put you back into the Japanese
input method.

The input method `japanese-zenkaku' is used to enter full width
JISX0208 characters corresponding to typed ASCII characters.

List of the all key sequences for Roman-Kana transliteration is shown
at the tail.

:: Kana-Kanji conversion ::

You can convert the current Japanese characters (underlined) to
Kana-Kanji mixed text.  In this stage, the converted text is divided
into two parts, the current phrase (highlighted) and the remaining
input (underlined).  The following commands can be used on the
current phrase.

SPC, C-n	kkc-next
	Show the next candidate for the current phrase.
	If successively typed `kkc-show-conversion-list-count' times,
	conversion candidates are shown in the echo area.
C-p		kkc-prev
	Show the previous candidate for the current phrase.
	If successively typed `kkc-show-conversion-list-count' times,
	conversion candidates are shown in the echo area.
l		kkc-show-conversion-list-or-next-group
	Show at most 10 candidates for the current phrase in echo area.
	If typed repeatedly, show the next 10 candidates.
L		kkc-show-conversion-list-or-prev-group
	Show at most 10 candidates for the current phrase in echo area.
	If typed repeatedly, show the previous 10 candidates.
0..9		kkc-select-from-list
	Select a candidate corresponding to the typed number.
H		kkc-hiragana
	Convert the current phrase to Hiragana
K		kkc-katakana
	Convert the current phrase to Katakana
C-o		kkc-longer
	Extend the current phrase; pull in the first character of
	the remaining input.
C-i		kkc-shorter
	Contract the current phrase; drop its last character
	back into the remaining input.
C-f		kkc-next-phrase
	Accept the current phrase.  If there remains input, select
	the first phrase as the current one, and show the first
	candidate for the conversion.
DEL, C-c	kkc-cancel
	Cancel the conversion, shift back to the Roman-Kana
	transliteration.
return		kkc-terminate
	Accept the whole conversion.
C-SPC, C-@	kkc-first-char-only
	Accept the first character of the current conversion,
	delete the remaining input.
C-h		kkc-help
	List these key bindings.
"
 nil t t nil nil nil nil nil
 'quail-japanese-update-translation
 '(("K" . quail-japanese-toggle-kana)
   (" " . quail-japanese-kanji-kkc)
   ("\C-m" . quail-no-conversion)
   ([return] . quail-no-conversion))
 )

(dolist (elt quail-japanese-transliteration-rules)
  (quail-defrule (car elt) (nth 1 elt)))

(quail-define-package
 "japanese-ascii" "Japanese" "Aa"
 nil
 "Temporary ASCII input mode used within the input method `japanese'.
Type \"qq\" to go back to previous input method."
 nil t t)

(quail-define-rules ("qq" quail-japanese-switch-package))

(quail-define-package
 "japanese-zenkaku" "Japanese" "Ａ"
 nil
 "Japanese zenkaku alpha numeric character input method.
---- Special key bindings ----
qq:	toggle between this input method and the input method `japanese-ascii'.
qh:	shift to the input method `japanese',
	typing \"qz\" puts you back to this input method.
"
 nil t t)

(quail-define-rules

(" " "　") ("!" "！") ("\"" "″") ("#" "＃")
("$" "＄") ("%" "％") ("&" "＆") ("'" "′")
("(" "（") (")" "）") ("*" "＊") ("+" "＋")
("," "，") ("-" "−") ("." "．") ("/" "／")
("0" "０") ("1" "１") ("2" "２") ("3" "３")
("4" "４") ("5" "５") ("6" "６") ("7" "７")
("8" "８") ("9" "９") (":" "：") (";" "；")
("<" "＜") ("=" "＝") (">" "＞") ("?" "？")
("@" "＠") ("A" "Ａ") ("B" "Ｂ") ("C" "Ｃ")
("D" "Ｄ") ("E" "Ｅ") ("F" "Ｆ") ("G" "Ｇ")
("H" "Ｈ") ("I" "Ｉ") ("J" "Ｊ") ("K" "Ｋ")
("L" "Ｌ") ("M" "Ｍ") ("N" "Ｎ") ("O" "Ｏ")
("P" "Ｐ") ("Q" "Ｑ") ("R" "Ｒ") ("S" "Ｓ")
("T" "Ｔ") ("U" "Ｕ") ("V" "Ｖ") ("W" "Ｗ")
("X" "Ｘ") ("Y" "Ｙ") ("Z" "Ｚ") ("[" "［")
("\\" "￥") ("]" "］") ("^" "＾") ("_" "＿")
("`" "‘") ("a" "ａ") ("b" "ｂ") ("c" "ｃ")
("d" "ｄ") ("e" "ｅ") ("f" "ｆ") ("g" "ｇ")
("h" "ｈ") ("i" "ｉ") ("j" "ｊ") ("k" "ｋ")
("l" "ｌ") ("m" "ｍ") ("n" "ｎ") ("o" "ｏ")
("p" "ｐ") ("q" "ｑ") ("r" "ｒ") ("s" "ｓ")
("t" "ｔ") ("u" "ｕ") ("v" "ｖ") ("w" "ｗ")
("x" "ｘ") ("y" "ｙ") ("z" "ｚ") ("{" "｛")
("|" "｜") ("}" "｝") ("~" "〜")

("qq" quail-japanese-switch-package)
("qh" quail-japanese-switch-package)
)

(defun quail-japanese-hankaku-update-translation (control-flag)
  (setq control-flag
	(quail-japanese-update-translation control-flag))
  (if (or (and (stringp quail-current-str)
	       (> (length quail-current-str) 0))
	  (integerp quail-current-str))
      (setq quail-current-str (japanese-hankaku quail-current-str)))
  control-flag)

(quail-define-package
 "japanese-hankaku-kana"
 "Japanese" "ｱ"
 nil
 "Japanese hankaku katakana input method by Roman transliteration.
---- Special key bindings ----
qq:	toggle between this input method and the input method `japanese-ascii'.
"
 nil t t nil nil nil nil nil
 'quail-japanese-hankaku-update-translation)

(dolist (elt quail-japanese-transliteration-rules)
  (quail-defrule (car elt)
		 (let ((trans (nth 1 elt)))
		   (when (or (stringp trans) (vectorp trans))
		     (let ((s (japanese-hankaku (if (stringp trans)
						    trans
						  (aref trans 0)))))
		       ;; If the result of the conversion is a string
		       ;; containing more than one character, make the
		       ;; result a vector, so that quail-defrule
		       ;; recognizes the whole string is the
		       ;; translation, instead of interpreting
		       ;; individual characters as alternative
		       ;; translations.
		       (if (and (stringp s) (> (length s) 1))
			   (setq trans (vector s))
			 (setq trans s))))
		   trans)))

(quail-define-package
 "japanese-hiragana" "Japanese" "あ"
 nil
 "Japanese hiragana input method by Roman transliteration."
 nil t t nil nil nil nil nil
 'quail-japanese-update-translation)

;; Use the same map as that of `japanese'.
(setcar (cdr (cdr quail-current-package))
	(nth 2 (assoc "japanese" quail-package-alist)))

;; Update Quail translation region while converting Hiragana to Katakana.
(defun quail-japanese-katakana-update-translation (control-flag)
  (setq control-flag
	(quail-japanese-update-translation control-flag))
  (if (or (and (stringp quail-current-str)
	       (> (length quail-current-str) 0))
	  (integerp quail-current-str))
      (setq quail-current-str (japanese-katakana quail-current-str)))
  control-flag)

(quail-define-package
 "japanese-katakana" "Japanese" "ア"
 nil
 "Japanese katakana input method by Roman transliteration."
 nil t t nil nil nil nil nil
 'quail-japanese-katakana-update-translation)

(dolist (elt quail-japanese-transliteration-rules)
  (quail-defrule (car elt)
		 (let ((trans (nth 1 elt)))
		   (cond ((stringp trans)
			  (japanese-katakana trans))
			 ((vectorp trans)
			  (vector (japanese-katakana (aref trans 0))))
			 (t trans)))))

;;; japanese.el ends here
