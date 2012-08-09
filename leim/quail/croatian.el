;;; quail/croatian.el -- Quail package for inputting Croatian  -*-coding: iso-8859-2;-*-

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author: Hrvoje Nik¹iæ <hniksic@xemacs.org>
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

;; Modeled after czech.el by Milan Zamazal.

;;; Code:

(require 'quail)

(quail-define-package
 "croatian" "Croatian" "HR" nil
 "\"Standard\" Croatian keyboard."
  nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("@" ?\")
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("-" ?\')
 ("_" ??)
 ("=" ?+)
 ("+" ?*)
 ("[" ?¹)
 ("{" ?©)
 ("]" ?ð)
 ("}" ?Ð)
 (";" ?è)
 (":" ?È)
 ("'" ?æ)
 ("\"" ?Æ)
 ("\\" ?¾)
 ("|" ?®)
 ("<" ?\;)
 (">" ?:)
 ("/" ?-)
 ("?" ?_)
 ("y" ?z)
 ("Y" ?Z)
 ("z" ?y)
 ("Z" ?Y))

(quail-define-package
 "croatian-qwerty" "Croatian" "HR" nil
 "Croatian keyboard without the y/z swap."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("@" ?\")
 ("^" ?&)
 ("&" ?/)
 ("*" ?\()
 ("(" ?\))
 (")" ?=)
 ("-" ?\')
 ("_" ??)
 ("=" ?+)
 ("+" ?*)
 ("[" ?¹)
 ("{" ?©)
 ("]" ?ð)
 ("}" ?Ð)
 (";" ?è)
 (":" ?È)
 ("'" ?æ)
 ("\"" ?Æ)
 ("\\" ?¾)
 ("|" ?®)
 ("<" ?\;)
 (">" ?:)
 ("/" ?-)
 ("?" ?_))

(quail-define-package
 "croatian-prefix" "Croatian" "HR" nil
 "Croatian input method, postfix.

\"c -> è
'c -> æ
\"s -> ¹
\"z -> ¾
/d -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("\"c" ?è)
 ("\"C" ?È)
 ("'c" ?æ)
 ("'C" ?Æ)
 ("\"s" ?¹)
 ("\"S" ?©)
 ("\"z" ?¾)
 ("\"Z" ?®)
 ("/d" ?ð)
 ("/D" ?Ð))

(quail-define-package
 "croatian-postfix" "Croatian" "HR" nil
 "Croatian input method, postfix.

c\" -> è
c' -> æ
s\" -> ¹
z\" -> ¾
d/ -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("c\"" ?è)
 ("C\"" ?È)
 ("c'" ?æ)
 ("C'" ?Æ)
 ("s\"" ?¹)
 ("S\"" ?©)
 ("z\"" ?¾)
 ("Z\"" ?®)
 ("d/" ?ð)
 ("D/" ?Ð))

(quail-define-package
 "croatian-xy" "Croatian" "HR" nil
 "An alternative Croatian input method.

cx -> è
cy -> æ
sx -> ¹
zx -> ¾
dy -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("cx" ?è)
 ("CX" ?È)
 ("Cx" ?È)
 ("cy" ?æ)
 ("CY" ?Æ)
 ("Cy" ?Æ)
 ("sx" ?¹)
 ("SX" ?©)
 ("Sx" ?©)
 ("zx" ?¾)
 ("ZX" ?®)
 ("Zx" ?®)
 ("dy" ?ð)
 ("DY" ?Ð)
 ("Dy" ?Ð))

(quail-define-package
 "croatian-cc" "Croatian" "HR" nil
 "Another alternative Croatian input method.

cc -> è
ch -> æ
ss -> ¹
zz -> ¾
dd -> ð"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("cc" ?è)
 ("CC" ?È)
 ("Cc" ?È)
 ("ch" ?æ)
 ("CH" ?Æ)
 ("Ch" ?Æ)
 ("ss" ?¹)
 ("SS" ?©)
 ("Ss" ?©)
 ("zz" ?¾)
 ("ZZ" ?®)
 ("Zz" ?®)
 ("dd" ?ð)
 ("DD" ?Ð)
 ("Dd" ?Ð))

;;; croatian.el ends here
