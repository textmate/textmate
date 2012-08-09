;;; symbol-ksc.el --- Quail-package for Korean Symbol (KSC5601) -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1997, 2001-2012  Free Software Foundation, Inc.
;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

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

;;; 94.10.24   Written for Mule Ver.2.0 (koaunghi.un@zdv.uni-tuebingen.de)
;;; 94.11.04   Updated for Mule Ver.2.1 (koaunghi.un@zdv.uni-tuebingen.de)
;;; 96.09.23   Updated for emacs-19.33-beta (koaunghi.un@zdv.uni-tuebingen.de)

;;; Commentary:

;;; Code:

(require 'quail)
(require 'korea-util)

(quail-define-package
 "korean-symbol" "Korean" "심벌" t
 "한글심벌입력표:
  【(】괄호열기【arrow】화살【sex】♂♀【index】첨자  【accent】악센트
  【)】괄호닫기【music】음악【dot】점  【quote】따옴표【xtext】§※¶¡¿
  【Unit】℃Å￠℉【math】수학기호【pic】상형문자【line】선문자
  【unit】단위    【frac】분수    【textline】­―∥＼∼
  【wn】㈜【ks】㉿【No】№【㏇】㏇ 【dag】† 【ddag】‡【percent】‰
  【am】㏂【pm】㏘【™】™【Tel】℡【won】￦ 【yen】￥ 【pound】￡
  【Eng】ＡＢＣ… 【enum】０１２… 【Russ】АБВ…【Greek】ΑΒΓ…
  【eng】ａｂｃ… 【easc】영어ASCII【russ】абв…【greek】αβγ…
  【Rom】ⅠⅡⅢ… 【Scan】ÆÐª… 【hira】ぁあぃ
  【rom】ⅰⅱⅲ… 【scan】æđð… 【kata】ァアィ
  【ojaso】㉠∼㉭【pjaso】㈀∼㈍【oeng】ⓐ∼ⓩ【peng】⒜∼⒵
  【ogana】㉮∼㉻【pgana】㈎∼㈛【onum】①∼⑮【pnum】⑴∼⒂
  【자소】2벌식 + ㅥ(S) ㅿ(t_) ㆁ(D) ㆀ(DD) ㅱ(aD) ㆆ(_d) ㆅ(G) ㆍ(uk)")

(quail-define-rules
 ("("	"〔〈《「『【")
 (")"	"〕〉》」』】")
 ("math"	"±×÷≠≤≥∞∴∠⊥⌒∂∇≡≒〓≪≫√∽∝∵∫∬∈∋⊆⊇⊂⊃∪∩∧∨￢⇒⇔∀∃∮∑∏")
 ("pic"	"☆★○●◎◇◆□■△▲▽▼◁◀▷▶♤♠♡♥♧♣⊙◈▣◐◑▒▤▥▨▧▦▩♨☏☎☜☞¤")
 ("arrow"	"→←↑↓↔↕↗↙↖↘")
 ("music"	"♭♩♪♬")
 ("won"	"￦")
 ("yen"	"￥")
 ("pound"	"￡")
 ("xtext"	"§※¶¡¿")
 ("dot"	"·‥…¨ː")
 ("quote"	"、。〃‘’“”°′″´˝")
 ("textline"	"­―∥＼∼")
 ("Unit"	"℃Å￠℉")
 ("sex"	"♂♀")
 ("accent"	"～ˇ˘˚˙¸˛")
 ("percent"	"‰")
 ("dag"	"†")
 ("ddag"	"‡")
 ("wn"	"㈜")
 ("ks"	"㉿")
 ("No"	"№")
 ("Co"	"㏇")
 ("TM"	"™")
 ("am"	"㏂")
 ("pm"	"㏘")
 ("Tel"	"℡")
 ("easc"	"！＂＃＄％＆＇（）＊＋，－．／：；＜＝＞？＠［］＾＿｀｛｜｝￣")
 ("enum"	"０１２３４５６７８９")
 ("Eng"	"ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ")
 ("eng"	"ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ")
 ("r"	"ㄱ")
 ("R"	"ㄲ")
 ("rt"	"ㄳ")
 ("s"	"ㄴ")
 ("sw"	"ㄵ")
 ("sg"	"ㄶ")
 ("e"	"ㄷ")
 ("E"	"ㄸ")
 ("f"	"ㄹ")
 ("fr"	"ㄺ")
 ("fa"	"ㄻ")
 ("fq"	"ㄼ")
 ("ft"	"ㄽ")
 ("fx"	"ㄾ")
 ("fv"	"ㄿ")
 ("fg"	"ㅀ")
 ("a"	"ㅁ")
 ("q"	"ㅂ")
 ("Q"	"ㅃ")
 ("qt"	"ㅄ")
 ("t"	"ㅅ")
 ("T"	"ㅆ")
 ("d"	"ㅇ")
 ("w"	"ㅈ")
 ("W"	"ㅉ")
 ("c"	"ㅊ")
 ("z"	"ㅋ")
 ("x"	"ㅌ")
 ("v"	"ㅍ")
 ("g"	"ㅎ")
 ("k"	"ㅏ")
 ("o"	"ㅐ")
 ("i"	"ㅑ")
 ("O"	"ㅒ")
 ("j"	"ㅓ")
 ("p"	"ㅔ")
 ("u"	"ㅕ")
 ("P"	"ㅖ")
 ("h"	"ㅗ")
 ("hk"	"ㅘ")
 ("ho"	"ㅙ")
 ("hl"	"ㅚ")
 ("y"	"ㅛ")
 ("n"	"ㅜ")
 ("nj"	"ㅝ")
 ("np"	"ㅞ")
 ("nl"	"ㅟ")
 ("b"	"ㅠ")
 ("m"	"ㅡ")
 ("ml"	"ㅢ")
 ("l"	"ㅣ")
 ("S"	"ㅥ")
 ("se"	"ㅦ")
 ("st"	"ㅧ")
 ("st_"	"ㅨ")
 ("frt"	"ㅩ")
 ("fqt"	"ㅫ")
 ("fe"	"ㅪ")
 ("ft_"	"ㅬ")
 ("f_d"	"ㅭ")
 ("aq"	"ㅮ")
 ("at"	"ㅯ")
 ("at_"	"ㅰ")
 ("aD"	"ㅱ")
 ("qr"	"ㅲ")
 ("qe"	"ㅳ")
 ("qtr"	"ㅴ")
 ("qte"	"ㅵ")
 ("qw"	"ㅶ")
 ("qx"	"ㅷ")
 ("qD"	"ㅸ")
 ("QD"	"ㅹ")
 ("tr"	"ㅺ")
 ("ts"	"ㅻ")
 ("te"	"ㅼ")
 ("tq"	"ㅽ")
 ("tw"	"ㅾ")
 ("t_"	"ㅿ")
 ("DD"	"ㆀ")
 ("D"	"ㆁ")
 ("Dt"	"ㆂ")
 ("Dt_"	"ㆃ")
 ("vD"	"ㆄ")
 ("G"	"ㆅ")
 ("_d"	"ㆆ")
 ("yi"	"ㆇ")
 ("yO"	"ㆈ")
 ("yl"	"ㆉ")
 ("bu"	"ㆊ")
 ("bP"	"ㆋ")
 ("bl"	"ㆌ")
 ("uk"	"ㆍ")
 ("ukl"	"ㆎ")
 ("Rom"	"ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩ")
 ("rom"	"ⅰⅱⅲⅳⅴⅵⅶⅷⅸⅹ")
 ("Greek"	"ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ")
 ("greek"	"αβγδεζηθικλμνξοπρστυφχψω")
 ("line"	"─│┌┐┘└├┬┤┴┼━┃┏┓┛┗┣┳┫┻╋┠┯┨┷┿┝┰┥┸╂┒┑┚┙┖┕┎┍┞┟┡┢┦┧┩┪┭┮┱┲┵┶┹┺┽┾╀╁╃╄╅╆╇╈╉╊")
 ("unit"	"㎕㎖㎗ℓ㎘㏄㎣㎤㎥㎦㎙㎚㎛㎜㎝㎞㎟㎠㎡㎢㏊㎍㎎㎏㏏㎈㎉㏈㎧㎨㎰㎱㎲㎳㎴㎵㎶㎷㎸㎹㎀㎁㎂㎃㎄㎺㎻㎼㎽㎾㎿㎐㎑㎒㎓㎔Ω㏀㏁㎊㎋㎌㏖㏅㎭㎮㎯㏛㎩㎪㎫㎬㏝㏐㏓㏃㏉㏜㏆")
 ("Scan"	"ÆÐªĦĲĿŁØŒºÞŦŊ")
 ("ojaso"	"㉠㉡㉢㉣㉤㉥㉦㉧㉨㉩㉪㉫㉬㉭")
 ("ogana"	"㉮㉯㉰㉱㉲㉳㉴㉵㉶㉷㉸㉹㉺㉻")
 ("oeng"	"ⓐⓑⓒⓓⓔⓕⓖⓗⓘⓙⓚⓛⓜⓝⓞⓟⓠⓡⓢⓣⓤⓥⓦⓧⓨⓩ")
 ("onum"	"①②③④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮")
 ("frac"	"½⅓⅔¼¾⅛⅜⅝⅞")
 ("scan"	"æđðħıĳĸŀłøœßþŧŋŉ")
 ("pjaso"	"㈀㈁㈂㈃㈄㈅㈆㈇㈈㈉㈊㈋㈌㈍>")
 ("pgana"	"㈎㈏㈐㈑㈒㈓㈔㈕㈖㈗㈘㈙㈚㈛")
 ("peng"	"⒜⒝⒞⒟⒠⒡⒢⒣⒤⒥⒦⒧⒨⒩⒪⒫⒬⒭⒮⒯⒰⒱⒲⒳⒴⒵")
 ("pnum"	"⑴⑵⑶⑷⑸⑹⑺⑻⑼⑽⑾⑿⒀⒁⒂")
 ("index"	"¹²³⁴ⁿ₁₂₃₄")
 ("hira"	"ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをん")
 ("kata"	"ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ")
 ("Russ"	"АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ")
 ("russ"	"абвгдеёжзийклмнопрстуфхцчшщъыьэюя"))

;;; symbol-ksc.el ends here
