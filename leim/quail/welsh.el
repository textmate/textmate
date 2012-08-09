;;; welsh.el --- Quail package for inputting Welsh characters  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Dave Love <fx@gnu.org>
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

;; Welsh input following the Yudit map by david@sheetmusic.org.uk.

;;; Code:

(require 'quail)

(quail-define-package
 "welsh" "Welsh" "$,1!4(B" t
 "Welsh postfix input method"
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("A\\" ?,A@(B)
 ("A/" ?,AA(B)
 ("A^" ?,AB(B)
 ("A+" ?,AB(B)
 ("A\"" ?,AD(B)
 ("a\\" ?,A`(B)
 ("a/" ?,Aa(B)
 ("a^" ?,Ab(B)
 ("a+" ?,Ab(B)
 ("a\"" ?,Ad(B)

 ("E\\" ?,AH(B)
 ("E/" ?,AI(B)
 ("E^" ?,AJ(B)
 ("E+" ?,AJ(B)
 ("E\"" ?,AK(B)
 ("e\\" ?,Ah(B)
 ("e/" ?,Ai(B)
 ("e^" ?,Aj(B)
 ("e+" ?,Aj(B)
 ("e\"" ?,Ak(B)

 ("I\\" ?,AL(B)
 ("I/" ?,AM(B)
 ("I^" ?,AN(B)
 ("I+" ?,AN(B)
 ("I\"" ?,AO(B)
 ("i\\" ?,Al(B)
 ("i/" ?,Am(B)
 ("i^" ?,An(B)
 ("i+" ?,An(B)
 ("i\"" ?,Ao(B)

 ("O\\" ?,AR(B)
 ("O/" ?,AS(B)
 ("O^" ?,AT(B)
 ("O+" ?,AT(B)
 ("O\"" ?,AV(B)
 ("o\\" ?,Ar(B)
 ("o/" ?,As(B)
 ("o^" ?,At(B)
 ("o+" ?,At(B)
 ("o\"" ?,Av(B)

 ("U\\" ?,AY(B)
 ("U/" ?,AZ(B)
 ("U^" ?,A[(B)
 ("U+" ?,A[(B)
 ("U\"" ?,A\(B)
 ("u\\" ?,Ay(B)
 ("u/" ?,Az(B)
 ("u^" ?,A{(B)
 ("u+" ?,A{(B)
 ("u\"" ?,A|(B)

 ("Y\\" ?$,1or(B)
 ("Y/" ?,A](B)
 ("Y^" ?$,1!6(B)
 ("Y+" ?$,1!6(B)
 ("Y\"" ?$,1!8(B)
 ("y\\" ?$,1os(B)
 ("y/" ?,A}(B)
 ("y\"" ?,A(B)
 ("y^" ?$,1!7(B)
 ("y+" ?$,1!7(B)

 ("W\\" ?$,1n`(B)
 ("W/" ?$,1nb(B)
 ("W^" ?$,1!4(B)
 ("W+" ?$,1!4(B)
 ("W\"" ?$,1nd(B)
 ("w\\" ?$,1na(B)
 ("w/" ?$,1nc(B)
 ("w^" ?$,1!5(B)
 ("w+" ?$,1!5(B)
 ("w\"" ?$,1ne(B)

 ;; "hawlfraint" (copyright).  Dyma arwyddlun hawlfraint.
 ("(h)" ?$,1W(B))

;; (quail-define-package
;;  "welsh" "Welsh" "$,1!4(B" t
;;  "Welsh postfix input method, using Latin-8"
;;  nil t nil nil nil nil nil nil nil nil t)

;; (quail-define-rules
;;  ("A\\" ?,A@(B)
;;  ("A/" ?,AA(B)
;;  ("A^" ?,AB(B)
;;  ("A+" ?,AB(B)
;;  ("A\"" ?,AD(B)
;;  ("a\\" ?,A`(B)
;;  ("a/" ?,Aa(B)
;;  ("a^" ?,Ab(B)
;;  ("a+" ?,Ab(B)
;;  ("a\"" ?,Ad(B)

;;  ("E\\" ?,AH(B)
;;  ("E/" ?,AI(B)
;;  ("E^" ?,AJ(B)
;;  ("E+" ?,AJ(B)
;;  ("E\"" ?,AK(B)
;;  ("e\\" ?,Ah(B)
;;  ("e/" ?,Ai(B)
;;  ("e^" ?,Aj(B)
;;  ("e+" ?,Aj(B)
;;  ("e\"" ?,Ak(B)

;;  ("I\\" ?,AL(B)
;;  ("I/" ?,AM(B)
;;  ("I^" ?,AN(B)
;;  ("I+" ?,AN(B)
;;  ("I\"" ?,AO(B)
;;  ("i\\" ?,Al(B)
;;  ("i/" ?,Am(B)
;;  ("i^" ?,An(B)
;;  ("i+" ?,An(B)
;;  ("i\"" ?,Ao(B)

;;  ("O\\" ?,AR(B)
;;  ("O/" ?,AS(B)
;;  ("O^" ?,AT(B)
;;  ("O+" ?,AT(B)
;;  ("O\"" ?,AV(B)
;;  ("o\\" ?,Ar(B)
;;  ("o/" ?,As(B)
;;  ("o^" ?,At(B)
;;  ("o+" ?,At(B)
;;  ("o\"" ?,Av(B)

;;  ("U\\" ?,AY(B)
;;  ("U/" ?,AZ(B)
;;  ("U^" ?,A[(B)
;;  ("U+" ?,A[(B)
;;  ("U\"" ?,A\(B)
;;  ("u\\" ?,Ay(B)
;;  ("u/" ?,Az(B)
;;  ("u^" ?,A{(B)
;;  ("u+" ?,A{(B)
;;  ("u\"" ?,A|(B)

;;  ("Y\\" ?,A,(B)
;;  ("Y/" ?,A](B)
;;  ("Y^" ?,A^(B)
;;  ("Y+" ?,A^(B)
;;  ("Y\"" ?,A/(B)
;;  ("y\\" ?,A<(B)
;;  ("y/" ?,A}(B)
;;  ("y\"" ?,A(B)
;;  ("y^" ?,A~(B)
;;  ("y+" ?,A~(B)

;;  ("W\\" ?,A((B)
;;  ("W/" ?,A*(B)
;;  ("W^" ?,AP(B)
;;  ("W+" ?,AP(B)
;;  ("W\"" ?,A=(B)
;;  ("w\\" ?,A8(B)
;;  ("w/" ?,A:(B)
;;  ("w^" ?,Ap(B)
;;  ("w+" ?,Ap(B)
;;  ("w\"" ?,A>(B))


;;; welsh.el ends here
