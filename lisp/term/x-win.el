;;; x-win.el --- parse relevant switches and set up for X  -*-coding: iso-2022-7bit;-*-

;; Copyright (C) 1993-1994, 2001-2012 Free Software Foundation, Inc.

;; Author: FSF
;; Keywords: terminals, i18n

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

;; X-win.el: this file defines functions to initialize the X window
;; system and process X-specific command line parameters before
;; creating the first X frame.

;; Beginning in Emacs 23, the act of loading this file should not have
;; the side effect of initializing the window system or processing
;; command line arguments (this file is now loaded in loadup.el).  See
;; the variables `handle-args-function-alist' and
;; `window-system-initialization-alist' for more details.

;; startup.el will then examine startup files, and eventually call the hooks
;; which create the first window(s).

;;; Code:

;; These are the standard X switches from the Xt Initialize.c file of
;; Release 4.

;; Command line		Resource Manager string

;; +rv			*reverseVideo
;; +synchronous		*synchronous
;; -background		*background
;; -bd			*borderColor
;; -bg			*background
;; -bordercolor		*borderColor
;; -borderwidth		.borderWidth
;; -bw			.borderWidth
;; -display		.display
;; -fg			*foreground
;; -fn			*font
;; -font		*font
;; -foreground		*foreground
;; -geometry		.geometry
;; -iconic		.iconic
;; -name		.name
;; -reverse		*reverseVideo
;; -rv			*reverseVideo
;; -selectionTimeout    .selectionTimeout
;; -synchronous		*synchronous
;; -xrm

;; An alist of X options and the function which handles them.  See
;; ../startup.el.

(if (not (fboundp 'x-create-frame))
    (error "%s: Loading x-win.el but not compiled for X" (invocation-name)))

(require 'frame)
(require 'mouse)
(require 'scroll-bar)
(require 'faces)
(require 'select)
(require 'menu-bar)
(require 'fontset)
(require 'x-dnd)

(defvar x-invocation-args)
(defvar x-keysym-table)
(defvar x-selection-timeout)
(defvar x-session-id)
(defvar x-session-previous-id)

(defun x-handle-no-bitmap-icon (switch)
  (setq default-frame-alist (cons '(icon-type) default-frame-alist)))

;; Handle the --parent-id option.
(defun x-handle-parent-id (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq initial-frame-alist (cons
                             (cons 'parent-id
                                   (string-to-number (car x-invocation-args)))
                             initial-frame-alist)
        x-invocation-args (cdr x-invocation-args)))

;; Handle the --smid switch.  This is used by the session manager
;; to give us back our session id we had on the previous run.
(defun x-handle-smid (switch)
  (or (consp x-invocation-args)
      (error "%s: missing argument to `%s' option" (invocation-name) switch))
  (setq x-session-previous-id (car x-invocation-args)
	x-invocation-args (cdr x-invocation-args)))

(defvar emacs-save-session-functions nil
  "Special hook run when a save-session event occurs.
The functions do not get any argument.
Functions can return non-nil to inform the session manager that the
window system shutdown should be aborted.

See also `emacs-session-save'.")

(defun emacs-session-filename (session-id)
  "Construct a filename to save the session in based on SESSION-ID.
If the directory ~/.emacs.d exists, we make a filename in there, otherwise
a file in the home directory."
  (let ((basename (concat "session." session-id))
	(emacs-dir user-emacs-directory))
    (expand-file-name (if (file-directory-p emacs-dir)
			  (concat emacs-dir basename)
			(concat "~/.emacs-" basename)))))

(defun emacs-session-save ()
  "This function is called when the window system is shutting down.
If this function returns non-nil, the window system shutdown is canceled.

When a session manager tells Emacs that the window system is shutting
down, this function is called.  It calls the functions in the hook
`emacs-save-session-functions'.  Functions are called with the current
buffer set to a temporary buffer.  Functions should use `insert' to insert
lisp code to save the session state.  The buffer is saved in a file in the
home directory of the user running Emacs.  The file is evaluated when
Emacs is restarted by the session manager.

If any of the functions returns non-nil, no more functions are called
and this function returns non-nil.  This will inform the session manager
that it should abort the window system shutdown."
  (let ((filename (emacs-session-filename x-session-id))
	(buf (get-buffer-create (concat " *SES " x-session-id))))
    (when (file-exists-p filename)
      (delete-file filename))
    (with-current-buffer buf
      (let ((cancel-shutdown (condition-case nil
				 ;; A return of t means cancel the shutdown.
				 (run-hook-with-args-until-success
				  'emacs-save-session-functions)
			       (error t))))
	(unless cancel-shutdown
	  (write-file filename))
	(kill-buffer buf)
	cancel-shutdown))))

(defun emacs-session-restore (previous-session-id)
  "Restore the Emacs session if started by a session manager.
The file saved by `emacs-session-save' is evaluated and deleted if it
exists."
  (let ((filename (emacs-session-filename previous-session-id)))
    (when (file-exists-p filename)
      (load-file filename)
      (delete-file filename)
      (message "Restored session data"))))




;;
;; Standard X cursor shapes, courtesy of Mr. Fox, who wanted ALL of them.
;;

(defconst x-pointer-X-cursor 0)
(defconst x-pointer-arrow 2)
(defconst x-pointer-based-arrow-down 4)
(defconst x-pointer-based-arrow-up 6)
(defconst x-pointer-boat 8)
(defconst x-pointer-bogosity 10)
(defconst x-pointer-bottom-left-corner 12)
(defconst x-pointer-bottom-right-corner 14)
(defconst x-pointer-bottom-side 16)
(defconst x-pointer-bottom-tee 18)
(defconst x-pointer-box-spiral 20)
(defconst x-pointer-center-ptr 22)
(defconst x-pointer-circle 24)
(defconst x-pointer-clock 26)
(defconst x-pointer-coffee-mug 28)
(defconst x-pointer-cross 30)
(defconst x-pointer-cross-reverse 32)
(defconst x-pointer-crosshair 34)
(defconst x-pointer-diamond-cross 36)
(defconst x-pointer-dot 38)
(defconst x-pointer-dotbox 40)
(defconst x-pointer-double-arrow 42)
(defconst x-pointer-draft-large 44)
(defconst x-pointer-draft-small 46)
(defconst x-pointer-draped-box 48)
(defconst x-pointer-exchange 50)
(defconst x-pointer-fleur 52)
(defconst x-pointer-gobbler 54)
(defconst x-pointer-gumby 56)
(defconst x-pointer-hand1 58)
(defconst x-pointer-hand2 60)
(defconst x-pointer-heart 62)
(defconst x-pointer-icon 64)
(defconst x-pointer-iron-cross 66)
(defconst x-pointer-left-ptr 68)
(defconst x-pointer-left-side 70)
(defconst x-pointer-left-tee 72)
(defconst x-pointer-leftbutton 74)
(defconst x-pointer-ll-angle 76)
(defconst x-pointer-lr-angle 78)
(defconst x-pointer-man 80)
(defconst x-pointer-middlebutton 82)
(defconst x-pointer-mouse 84)
(defconst x-pointer-pencil 86)
(defconst x-pointer-pirate 88)
(defconst x-pointer-plus 90)
(defconst x-pointer-question-arrow 92)
(defconst x-pointer-right-ptr 94)
(defconst x-pointer-right-side 96)
(defconst x-pointer-right-tee 98)
(defconst x-pointer-rightbutton 100)
(defconst x-pointer-rtl-logo 102)
(defconst x-pointer-sailboat 104)
(defconst x-pointer-sb-down-arrow 106)
(defconst x-pointer-sb-h-double-arrow 108)
(defconst x-pointer-sb-left-arrow 110)
(defconst x-pointer-sb-right-arrow 112)
(defconst x-pointer-sb-up-arrow 114)
(defconst x-pointer-sb-v-double-arrow 116)
(defconst x-pointer-shuttle 118)
(defconst x-pointer-sizing 120)
(defconst x-pointer-spider 122)
(defconst x-pointer-spraycan 124)
(defconst x-pointer-star 126)
(defconst x-pointer-target 128)
(defconst x-pointer-tcross 130)
(defconst x-pointer-top-left-arrow 132)
(defconst x-pointer-top-left-corner 134)
(defconst x-pointer-top-right-corner 136)
(defconst x-pointer-top-side 138)
(defconst x-pointer-top-tee 140)
(defconst x-pointer-trek 142)
(defconst x-pointer-ul-angle 144)
(defconst x-pointer-umbrella 146)
(defconst x-pointer-ur-angle 148)
(defconst x-pointer-watch 150)
(defconst x-pointer-xterm 152)
(defconst x-pointer-invisible 255)


;;;; Keysyms

(defun vendor-specific-keysyms (vendor)
  "Return the appropriate value of `system-key-alist' for VENDOR.
VENDOR is a string containing the name of the X Server's vendor,
as returned by `x-server-vendor'."
  (cond ((or (string-equal vendor "Hewlett-Packard Incorporated")
	     (string-equal vendor "Hewlett-Packard Company"))
	 '((  168 . mute-acute)
	   (  169 . mute-grave)
	   (  170 . mute-asciicircum)
	   (  171 . mute-diaeresis)
	   (  172 . mute-asciitilde)
	   (  175 . lira)
	   (  190 . guilder)
	   (  252 . block)
	   (  256 . longminus)
	   (65388 . reset)
	   (65389 . system)
	   (65390 . user)
	   (65391 . clearline)
	   (65392 . insertline)
	   (65393 . deleteline)
	   (65394 . insertchar)
	   (65395 . deletechar)
	   (65396 . backtab)
	   (65397 . kp-backtab)))
	;; Fixme: What about non-X11/NeWS sun server?
	((or (string-equal vendor "X11/NeWS - Sun Microsystems Inc.")
	     (string-equal vendor "X Consortium"))
	 '((392976 . f36)
	   (392977 . f37)
	   (393056 . req)
	   ;; These are for Sun under X11R6
	   (393072 . props)
	   (393073 . front)
	   (393074 . copy)
	   (393075 . open)
	   (393076 . paste)
	   (393077 . cut)))
	(t
	 ;; This is used by DEC's X server.
	 '((65280 . remove)))))

;; Latin-1
(let ((i 160))
  (while (< i 256)
    (puthash i i x-keysym-table)
    (setq i (1+ i))))

;; Table from Kuhn's proposed additions to the `KEYSYM Encoding'
;; appendix to the X protocol definition.
(dolist
     (pair
      '(
	;; Latin-2
	(#x1a1 . ?,B!(B)
	(#x1a2 . ?,B"(B)
	(#x1a3 . ?,B#(B)
	(#x1a5 . ?,B%(B)
	(#x1a6 . ?,B&(B)
	(#x1a9 . ?,B)(B)
	(#x1aa . ?,B*(B)
	(#x1ab . ?,B+(B)
	(#x1ac . ?,B,(B)
	(#x1ae . ?,B.(B)
	(#x1af . ?,B/(B)
	(#x1b1 . ?,B1(B)
	(#x1b2 . ?,B2(B)
	(#x1b3 . ?,B3(B)
	(#x1b5 . ?,B5(B)
	(#x1b6 . ?,B6(B)
	(#x1b7 . ?,B7(B)
	(#x1b9 . ?,B9(B)
	(#x1ba . ?,B:(B)
	(#x1bb . ?,B;(B)
	(#x1bc . ?,B<(B)
	(#x1bd . ?,B=(B)
	(#x1be . ?,B>(B)
	(#x1bf . ?,B?(B)
	(#x1c0 . ?,B@(B)
	(#x1c3 . ?,BC(B)
	(#x1c5 . ?,BE(B)
	(#x1c6 . ?,BF(B)
	(#x1c8 . ?,BH(B)
	(#x1ca . ?,BJ(B)
	(#x1cc . ?,BL(B)
	(#x1cf . ?,BO(B)
	(#x1d0 . ?,BP(B)
	(#x1d1 . ?,BQ(B)
	(#x1d2 . ?,BR(B)
	(#x1d5 . ?,BU(B)
	(#x1d8 . ?,BX(B)
	(#x1d9 . ?,BY(B)
	(#x1db . ?,B[(B)
	(#x1de . ?,B^(B)
	(#x1e0 . ?,B`(B)
	(#x1e3 . ?,Bc(B)
	(#x1e5 . ?,Be(B)
	(#x1e6 . ?,Bf(B)
	(#x1e8 . ?,Bh(B)
	(#x1ea . ?,Bj(B)
	(#x1ec . ?,Bl(B)
	(#x1ef . ?,Bo(B)
	(#x1f0 . ?,Bp(B)
	(#x1f1 . ?,Bq(B)
	(#x1f2 . ?,Br(B)
	(#x1f5 . ?,Bu(B)
	(#x1f8 . ?,Bx(B)
	(#x1f9 . ?,By(B)
	(#x1fb . ?,B{(B)
	(#x1fe . ?,B~(B)
	(#x1ff . ?,B(B)
	;; Latin-3
	(#x2a1 . ?,C!(B)
	(#x2a6 . ?,C&(B)
	(#x2a9 . ?,C)(B)
	(#x2ab . ?,C+(B)
	(#x2ac . ?,C,(B)
	(#x2b1 . ?,C1(B)
	(#x2b6 . ?,C6(B)
	(#x2b9 . ?,C9(B)
	(#x2bb . ?,C;(B)
	(#x2bc . ?,C<(B)
	(#x2c5 . ?,CE(B)
	(#x2c6 . ?,CF(B)
	(#x2d5 . ?,CU(B)
	(#x2d8 . ?,CX(B)
	(#x2dd . ?,C](B)
	(#x2de . ?,C^(B)
	(#x2e5 . ?,Ce(B)
	(#x2e6 . ?,Cf(B)
	(#x2f5 . ?,Cu(B)
	(#x2f8 . ?,Cx(B)
	(#x2fd . ?,C}(B)
	(#x2fe . ?,C~(B)
	;; Latin-4
	(#x3a2 . ?,D"(B)
	(#x3a3 . ?,D#(B)
	(#x3a5 . ?,D%(B)
	(#x3a6 . ?,D&(B)
	(#x3aa . ?,D*(B)
	(#x3ab . ?,D+(B)
	(#x3ac . ?,D,(B)
	(#x3b3 . ?,D3(B)
	(#x3b5 . ?,D5(B)
	(#x3b6 . ?,D6(B)
	(#x3ba . ?,D:(B)
	(#x3bb . ?,D;(B)
	(#x3bc . ?,D<(B)
	(#x3bd . ?,D=(B)
	(#x3bf . ?,D?(B)
	(#x3c0 . ?,D@(B)
	(#x3c7 . ?,DG(B)
	(#x3cc . ?,DL(B)
	(#x3cf . ?,DO(B)
	(#x3d1 . ?,DQ(B)
	(#x3d2 . ?,DR(B)
	(#x3d3 . ?,DS(B)
	(#x3d9 . ?,DY(B)
	(#x3dd . ?,D](B)
	(#x3de . ?,D^(B)
	(#x3e0 . ?,D`(B)
	(#x3e7 . ?,Dg(B)
	(#x3ec . ?,Dl(B)
	(#x3ef . ?,Do(B)
	(#x3f1 . ?,Dq(B)
	(#x3f2 . ?,Dr(B)
	(#x3f3 . ?,Ds(B)
	(#x3f9 . ?,Dy(B)
	(#x3fd . ?,D}(B)
	(#x3fe . ?,D~(B)
	;; Kana: Fixme: needs conversion to Japanese charset -- seems
	;; to require jisx0213, for which the Unicode translation
	;; isn't clear.
	(#x47e . ?(J~(B)
	(#x4a1 . ?$A!#(B)
	(#x4a2 . ?\$A!8(B)
	(#x4a3 . ?\$A!9(B)
	(#x4a4 . ?$A!"(B)
	(#x4a5 . ?$A!$(B)
	(#x4a6 . ?$A%r(B)
	(#x4a7 . ?$A%!(B)
	(#x4a8 . ?$A%#(B)
	(#x4a9 . ?$A%%(B)
	(#x4aa . ?$A%'(B)
	(#x4ab . ?$A%)(B)
	(#x4ac . ?$A%c(B)
	(#x4ad . ?$A%e(B)
	(#x4ae . ?$A%g(B)
	(#x4af . ?$A%C(B)
	(#x4b0 . ?$B!<(B)
	(#x4b1 . ?$A%"(B)
	(#x4b2 . ?$A%$(B)
	(#x4b3 . ?$A%&(B)
	(#x4b4 . ?$A%((B)
	(#x4b5 . ?$A%*(B)
	(#x4b6 . ?$A%+(B)
	(#x4b7 . ?$A%-(B)
	(#x4b8 . ?$A%/(B)
	(#x4b9 . ?$A%1(B)
	(#x4ba . ?$A%3(B)
	(#x4bb . ?$A%5(B)
	(#x4bc . ?$A%7(B)
	(#x4bd . ?$A%9(B)
	(#x4be . ?$A%;(B)
	(#x4bf . ?$A%=(B)
	(#x4c0 . ?$A%?(B)
	(#x4c1 . ?$A%A(B)
	(#x4c2 . ?$A%D(B)
	(#x4c3 . ?$A%F(B)
	(#x4c4 . ?$A%H(B)
	(#x4c5 . ?$A%J(B)
	(#x4c6 . ?$A%K(B)
	(#x4c7 . ?$A%L(B)
	(#x4c8 . ?$A%M(B)
	(#x4c9 . ?$A%N(B)
	(#x4ca . ?$A%O(B)
	(#x4cb . ?$A%R(B)
	(#x4cc . ?$A%U(B)
	(#x4cd . ?$A%X(B)
	(#x4ce . ?$A%[(B)
	(#x4cf . ?$A%^(B)
	(#x4d0 . ?$A%_(B)
	(#x4d1 . ?$A%`(B)
	(#x4d2 . ?$A%a(B)
	(#x4d3 . ?$A%b(B)
	(#x4d4 . ?$A%d(B)
	(#x4d5 . ?$A%f(B)
	(#x4d6 . ?$A%h(B)
	(#x4d7 . ?$A%i(B)
	(#x4d8 . ?$A%j(B)
	(#x4d9 . ?$A%k(B)
	(#x4da . ?$A%l(B)
	(#x4db . ?$A%m(B)
	(#x4dc . ?$A%o(B)
	(#x4dd . ?$A%s(B)
	(#x4de . ?$B!+(B)
	(#x4df . ?$B!,(B)
	;; Arabic
	(#x5ac . ?,G,(B)
	(#x5bb . ?,G;(B)
	(#x5bf . ?,G?(B)
	(#x5c1 . ?,GA(B)
	(#x5c2 . ?,GB(B)
	(#x5c3 . ?,GC(B)
	(#x5c4 . ?,GD(B)
	(#x5c5 . ?,GE(B)
	(#x5c6 . ?,GF(B)
	(#x5c7 . ?,GG(B)
	(#x5c8 . ?,GH(B)
	(#x5c9 . ?,GI(B)
	(#x5ca . ?,GJ(B)
	(#x5cb . ?,GK(B)
	(#x5cc . ?,GL(B)
	(#x5cd . ?,GM(B)
	(#x5ce . ?,GN(B)
	(#x5cf . ?,GO(B)
	(#x5d0 . ?,GP(B)
	(#x5d1 . ?,GQ(B)
	(#x5d2 . ?,GR(B)
	(#x5d3 . ?,GS(B)
	(#x5d4 . ?,GT(B)
	(#x5d5 . ?,GU(B)
	(#x5d6 . ?,GV(B)
	(#x5d7 . ?,GW(B)
	(#x5d8 . ?,GX(B)
	(#x5d9 . ?,GY(B)
	(#x5da . ?,GZ(B)
	(#x5e0 . ?,G`(B)
	(#x5e1 . ?,Ga(B)
	(#x5e2 . ?,Gb(B)
	(#x5e3 . ?,Gc(B)
	(#x5e4 . ?,Gd(B)
	(#x5e5 . ?,Ge(B)
	(#x5e6 . ?,Gf(B)
	(#x5e7 . ?,Gg(B)
	(#x5e8 . ?,Gh(B)
	(#x5e9 . ?,Gi(B)
	(#x5ea . ?,Gj(B)
	(#x5eb . ?,Gk(B)
	(#x5ec . ?,Gl(B)
	(#x5ed . ?,Gm(B)
	(#x5ee . ?,Gn(B)
	(#x5ef . ?,Go(B)
	(#x5f0 . ?,Gp(B)
	(#x5f1 . ?,Gq(B)
	(#x5f2 . ?,Gr(B)
	;; Cyrillic
	(#x680 . ?$,1)R(B)
	(#x681 . ?$,1)V(B)
	(#x682 . ?$,1)Z(B)
	(#x683 . ?$,1)\(B)
	(#x684 . ?$,1)b(B)
	(#x685 . ?$,1)n(B)
	(#x686 . ?$,1)p(B)
	(#x687 . ?$,1)r(B)
	(#x688 . ?$,1)v(B)
	(#x689 . ?$,1)x(B)
	(#x68a . ?$,1)z(B)
	(#x68c . ?$,1*8(B)
	(#x68d . ?$,1*B(B)
	(#x68e . ?$,1*H(B)
	(#x68f . ?$,1*N(B)
	(#x690 . ?$,1)S(B)
	(#x691 . ?$,1)W(B)
	(#x692 . ?$,1)[(B)
	(#x693 . ?$,1)](B)
	(#x694 . ?$,1)c(B)
	(#x695 . ?$,1)o(B)
	(#x696 . ?$,1)q(B)
	(#x697 . ?$,1)s(B)
	(#x698 . ?$,1)w(B)
	(#x699 . ?$,1)y(B)
	(#x69a . ?$,1){(B)
	(#x69c . ?$,1*9(B)
	(#x69d . ?$,1*C(B)
	(#x69e . ?$,1*I(B)
	(#x69f . ?$,1*O(B)
	(#x6a1 . ?,Lr(B)
	(#x6a2 . ?,Ls(B)
	(#x6a3 . ?,Lq(B)
	(#x6a4 . ?,Lt(B)
	(#x6a5 . ?,Lu(B)
	(#x6a6 . ?,Lv(B)
	(#x6a7 . ?,Lw(B)
	(#x6a8 . ?,Lx(B)
	(#x6a9 . ?,Ly(B)
	(#x6aa . ?,Lz(B)
	(#x6ab . ?,L{(B)
	(#x6ac . ?,L|(B)
	(#x6ae . ?,L~(B)
	(#x6af . ?,L(B)
	(#x6b0 . ?,Lp(B)
	(#x6b1 . ?,L"(B)
	(#x6b2 . ?,L#(B)
	(#x6b3 . ?,L!(B)
	(#x6b4 . ?,L$(B)
	(#x6b5 . ?,L%(B)
	(#x6b6 . ?,L&(B)
	(#x6b7 . ?,L'(B)
	(#x6b8 . ?,L((B)
	(#x6b9 . ?,L)(B)
	(#x6ba . ?,L*(B)
	(#x6bb . ?,L+(B)
	(#x6bc . ?,L,(B)
	(#x6be . ?,L.(B)
	(#x6bf . ?,L/(B)
	(#x6c0 . ?,Ln(B)
	(#x6c1 . ?,LP(B)
	(#x6c2 . ?,LQ(B)
	(#x6c3 . ?,Lf(B)
	(#x6c4 . ?,LT(B)
	(#x6c5 . ?,LU(B)
	(#x6c6 . ?,Ld(B)
	(#x6c7 . ?,LS(B)
	(#x6c8 . ?,Le(B)
	(#x6c9 . ?,LX(B)
	(#x6ca . ?,LY(B)
	(#x6cb . ?,LZ(B)
	(#x6cc . ?,L[(B)
	(#x6cd . ?,L\(B)
	(#x6ce . ?,L](B)
	(#x6cf . ?,L^(B)
	(#x6d0 . ?,L_(B)
	(#x6d1 . ?,Lo(B)
	(#x6d2 . ?,L`(B)
	(#x6d3 . ?,La(B)
	(#x6d4 . ?,Lb(B)
	(#x6d5 . ?,Lc(B)
	(#x6d6 . ?,LV(B)
	(#x6d7 . ?,LR(B)
	(#x6d8 . ?,Ll(B)
	(#x6d9 . ?,Lk(B)
	(#x6da . ?,LW(B)
	(#x6db . ?,Lh(B)
	(#x6dc . ?,Lm(B)
	(#x6dd . ?,Li(B)
	(#x6de . ?,Lg(B)
	(#x6df . ?,Lj(B)
	(#x6e0 . ?,LN(B)
	(#x6e1 . ?,L0(B)
	(#x6e2 . ?,L1(B)
	(#x6e3 . ?,LF(B)
	(#x6e4 . ?,L4(B)
	(#x6e5 . ?,L5(B)
	(#x6e6 . ?,LD(B)
	(#x6e7 . ?,L3(B)
	(#x6e8 . ?,LE(B)
	(#x6e9 . ?,L8(B)
	(#x6ea . ?,L9(B)
	(#x6eb . ?,L:(B)
	(#x6ec . ?,L;(B)
	(#x6ed . ?,L<(B)
	(#x6ee . ?,L=(B)
	(#x6ef . ?,L>(B)
	(#x6f0 . ?,L?(B)
	(#x6f1 . ?,LO(B)
	(#x6f2 . ?,L@(B)
	(#x6f3 . ?,LA(B)
	(#x6f4 . ?,LB(B)
	(#x6f5 . ?,LC(B)
	(#x6f6 . ?,L6(B)
	(#x6f7 . ?,L2(B)
	(#x6f8 . ?,LL(B)
	(#x6f9 . ?,LK(B)
	(#x6fa . ?,L7(B)
	(#x6fb . ?,LH(B)
	(#x6fc . ?,LM(B)
	(#x6fd . ?,LI(B)
	(#x6fe . ?,LG(B)
	(#x6ff . ?,LJ(B)
	;; Greek
	(#x7a1 . ?,F6(B)
	(#x7a2 . ?,F8(B)
	(#x7a3 . ?,F9(B)
	(#x7a4 . ?,F:(B)
	(#x7a5 . ?,FZ(B)
	(#x7a7 . ?,F<(B)
	(#x7a8 . ?,F>(B)
	(#x7a9 . ?,F[(B)
	(#x7ab . ?,F?(B)
	(#x7ae . ?,F5(B)
	(#x7af . ?,F/(B)
	(#x7b1 . ?,F\(B)
	(#x7b2 . ?,F](B)
	(#x7b3 . ?,F^(B)
	(#x7b4 . ?,F_(B)
	(#x7b5 . ?,Fz(B)
	(#x7b6 . ?,F@(B)
	(#x7b7 . ?,F|(B)
	(#x7b8 . ?,F}(B)
	(#x7b9 . ?,F{(B)
	(#x7ba . ?,F`(B)
	(#x7bb . ?,F~(B)
	(#x7c1 . ?,FA(B)
	(#x7c2 . ?,FB(B)
	(#x7c3 . ?,FC(B)
	(#x7c4 . ?,FD(B)
	(#x7c5 . ?,FE(B)
	(#x7c6 . ?,FF(B)
	(#x7c7 . ?,FG(B)
	(#x7c8 . ?,FH(B)
	(#x7c9 . ?,FI(B)
	(#x7ca . ?,FJ(B)
	(#x7cb . ?,FK(B)
	(#x7cc . ?,FL(B)
	(#x7cd . ?,FM(B)
	(#x7ce . ?,FN(B)
	(#x7cf . ?,FO(B)
	(#x7d0 . ?,FP(B)
	(#x7d1 . ?,FQ(B)
	(#x7d2 . ?,FS(B)
	(#x7d4 . ?,FT(B)
	(#x7d5 . ?,FU(B)
	(#x7d6 . ?,FV(B)
	(#x7d7 . ?,FW(B)
	(#x7d8 . ?,FX(B)
	(#x7d9 . ?,FY(B)
	(#x7e1 . ?,Fa(B)
	(#x7e2 . ?,Fb(B)
	(#x7e3 . ?,Fc(B)
	(#x7e4 . ?,Fd(B)
	(#x7e5 . ?,Fe(B)
	(#x7e6 . ?,Ff(B)
	(#x7e7 . ?,Fg(B)
	(#x7e8 . ?,Fh(B)
	(#x7e9 . ?,Fi(B)
	(#x7ea . ?,Fj(B)
	(#x7eb . ?,Fk(B)
	(#x7ec . ?,Fl(B)
	(#x7ed . ?,Fm(B)
	(#x7ee . ?,Fn(B)
	(#x7ef . ?,Fo(B)
	(#x7f0 . ?,Fp(B)
	(#x7f1 . ?,Fq(B)
	(#x7f2 . ?,Fs(B)
	(#x7f3 . ?,Fr(B)
	(#x7f4 . ?,Ft(B)
	(#x7f5 . ?,Fu(B)
	(#x7f6 . ?,Fv(B)
	(#x7f7 . ?,Fw(B)
	(#x7f8 . ?,Fx(B)
	(#x7f9 . ?,Fy(B)
	 ;; Technical
	(#x8a1 . ?$,1|W(B)
	(#x8a2 . ?$A)0(B)
	(#x8a3 . ?$A)$(B)
	(#x8a4 . ?$,1{ (B)
	(#x8a5 . ?$,1{!(B)
	(#x8a6 . ?$A)&(B)
	(#x8a7 . ?$,1|A(B)
	(#x8a8 . ?$,1|C(B)
	(#x8a9 . ?$,1|D(B)
	(#x8aa . ?$,1|F(B)
	(#x8ab . ?$,1|;(B)
	(#x8ac . ?$,1|=(B)
	(#x8ad . ?$,1|>(B)
	(#x8ae . ?$,1|@(B)
	(#x8af . ?$,1|H(B)
	(#x8b0 . ?$,1|L(B)
	(#x8bc . ?$A!\(B)
	(#x8bd . ?$A!Y(B)
	(#x8be . ?$A!](B)
	(#x8bf . ?$A!R(B)
	(#x8c0 . ?$A!`(B)
	(#x8c1 . ?$A!X(B)
	(#x8c2 . ?$A!^(B)
	(#x8c5 . ?$B"`(B)
	(#x8c8 . ?$(G"D(B)
	(#x8c9 . ?$(O"l(B)
	(#x8cd . ?$B"N(B)
	(#x8ce . ?$B"M(B)
	(#x8cf . ?$A!T(B)
	(#x8d6 . ?$A!L(B)
	(#x8da . ?$B">(B)
	(#x8db . ?$B"?(B)
	(#x8dc . ?$A!I(B)
	(#x8dd . ?$A!H(B)
	(#x8de . ?$A!D(B)
	(#x8df . ?$A!E(B)
	(#x8ef . ?$B"_(B)
	(#x8f6 . ?$,1!R(B)
	(#x8fb . ?$A!{(B)
	(#x8fc . ?$A!|(B)
	(#x8fd . ?$A!z(B)
	(#x8fe . ?$A!}(B)
	;; Special
	(#x9e0 . ?$A!t(B)
	(#x9e1 . ?$(C"F(B)
	(#x9e2 . ?$(GB*(B)
	(#x9e3 . ?$(GB-(B)
	(#x9e4 . ?$(GB.(B)
	(#x9e5 . ?$(GB+(B)
	(#x9e8 . ?$,1}d(B)
	(#x9e9 . ?$(GB,(B)
	(#x9ea . ?$A)<(B)
	(#x9eb . ?$A)4(B)
	(#x9ec . ?$A)0(B)
	(#x9ed . ?$A)8(B)
	(#x9ee . ?$A)`(B)
	(#x9ef . ?$,1|Z(B)
	(#x9f0 . ?$,1|[(B)
	(#x9f1 . ?$A)$(B)
	(#x9f2 . ?$,1|\(B)
	(#x9f3 . ?$,1|](B)
	(#x9f4 . ?$A)@(B)
	(#x9f5 . ?$A)H(B)
	(#x9f6 . ?$A)X(B)
	(#x9f7 . ?$A)P(B)
	(#x9f8 . ?$A)&(B)
	;; Publishing
	(#xaa1 . ?$,1rc(B)
	(#xaa2 . ?$,1rb(B)
	(#xaa3 . ?$,1rd(B)
	(#xaa4 . ?$,1re(B)
	(#xaa5 . ?$,1rg(B)
	(#xaa6 . ?$,1rh(B)
	(#xaa7 . ?$,1ri(B)
	(#xaa8 . ?$,1rj(B)
	(#xaa9 . ?$(G!7(B)
	(#xaaa . ?$(G!9(B)
	(#xaae . ?$A!-(B)
	(#xaaf . ?$(G!-(B)
	(#xab0 . ?$(O'x(B)
	(#xab1 . ?$(O'y(B)
	(#xab2 . ?$(O'z(B)
	(#xab3 . ?$,1v6(B)
	(#xab4 . ?$,1v7(B)
	(#xab5 . ?$,1v8(B)
	(#xab6 . ?$,1v9(B)
	(#xab7 . ?$,1v:(B)
	(#xab8 . ?$(G""(B)
	(#xabb . ?$,1rr(B)
	(#xabc . ?$,1{)(B)
	(#xabe . ?$,1{*(B)
	(#xac3 . ?$(C({(B)
	(#xac4 . ?$(C(|(B)
	(#xac5 . ?$(C(}(B)
	(#xac6 . ?$(C(~(B)
	(#xac9 . ?$(D"o(B)
	(#xaca . ?$,2"s(B)
	(#xacc . ?$(O##(B)
	(#xacd . ?$(O#!(B)
	(#xace . ?$A!p(B)
	(#xacf . ?$,2!o(B)
	(#xad0 . ?,F!(B)
	(#xad1 . ?,F"(B)
	(#xad2 . ?,Y4(B)
	(#xad3 . ?,Y!(B)
	(#xad4 . ?$,1u^(B)
	(#xad6 . ?$A!d(B)
	(#xad7 . ?$A!e(B)
	(#xad9 . ?$,2%](B)
	(#xadb . ?$,2!l(B)
	(#xadc . ?$(O#$(B)
	(#xadd . ?$(O#"(B)
	(#xade . ?$A!q(B)
	(#xadf . ?$,2!n(B)
	(#xae0 . ?$(O#?(B)
	(#xae1 . ?$,2!k(B)
	(#xae2 . ?$,2!m(B)
	(#xae3 . ?$A!w(B)
	(#xae4 . ?$(G!}(B)
	(#xae5 . ?$A!n(B)
	(#xae6 . ?$(O#@(B)
	(#xae7 . ?$,2!j(B)
	(#xae8 . ?$A!x(B)
	(#xae9 . ?$(G!~(B)
	(#xaea . ?$(C"P(B)
	(#xaeb . ?$(O-~(B)
	(#xaec . ?$(O&@(B)
	(#xaed . ?$(O&<(B)
	(#xaee . ?$(O&>(B)
	(#xaf0 . ?$,2%`(B)
	(#xaf1 . ?$B"w(B)
	(#xaf2 . ?$B"x(B)
	(#xaf3 . ?$(O'{(B)
	(#xaf4 . ?$,2%W(B)
	(#xaf5 . ?$B"t(B)
	(#xaf6 . ?$B"u(B)
	(#xaf7 . ?$A!a(B)
	(#xaf8 . ?$A!b(B)
	(#xaf9 . ?$(O&g(B)
	(#xafa . ?$,1zu(B)
	(#xafb . ?$,1uW(B)
	(#xafc . ?$,1s8(B)
	(#xafd . ?$,1rz(B)
	(#xafe . ?,Y%(B)
	;; APL
	(#xba3 . ?<)
	(#xba6 . ?>)
	(#xba8 . ?$A!E(B)
	(#xba9 . ?$A!D(B)
	(#xbc0 . ?,A/(B)
	(#xbc2 . ?$A!M(B)
	(#xbc3 . ?$A!I(B)
	(#xbc4 . ?$,1zj(B)
	(#xbc6 . ?_)
	(#xbca . ?$,1x8(B)
	(#xbcc . ?$,1|5(B)
	(#xbce . ?$,1yd(B)
	(#xbcf . ?$A!p(B)
	(#xbd3 . ?$,1zh(B)
	(#xbd6 . ?$A!H(B)
	(#xbd8 . ?$B"?(B)
	(#xbda . ?$B">(B)
	(#xbdc . ?$,1yb(B)
	(#xbfc . ?$,1yc(B)
	;; Hebrew
	(#xcdf . ?,H_(B)
	(#xce0 . ?,H`(B)
	(#xce1 . ?,Ha(B)
	(#xce2 . ?,Hb(B)
	(#xce3 . ?,Hc(B)
	(#xce4 . ?,Hd(B)
	(#xce5 . ?,He(B)
	(#xce6 . ?,Hf(B)
	(#xce7 . ?,Hg(B)
	(#xce8 . ?,Hh(B)
	(#xce9 . ?,Hi(B)
	(#xcea . ?,Hj(B)
	(#xceb . ?,Hk(B)
	(#xcec . ?,Hl(B)
	(#xced . ?,Hm(B)
	(#xcee . ?,Hn(B)
	(#xcef . ?,Ho(B)
	(#xcf0 . ?,Hp(B)
	(#xcf1 . ?,Hq(B)
	(#xcf2 . ?,Hr(B)
	(#xcf3 . ?,Hs(B)
	(#xcf4 . ?,Ht(B)
	(#xcf5 . ?,Hu(B)
	(#xcf6 . ?,Hv(B)
	(#xcf7 . ?,Hw(B)
	(#xcf8 . ?,Hx(B)
	(#xcf9 . ?,Hy(B)
	(#xcfa . ?,Hz(B)
	;; Thai
	(#xda1 . ?,T!(B)
	(#xda2 . ?,T"(B)
	(#xda3 . ?,T#(B)
	(#xda4 . ?,T$(B)
	(#xda5 . ?,T%(B)
	(#xda6 . ?,T&(B)
	(#xda7 . ?,T'(B)
	(#xda8 . ?,T((B)
	(#xda9 . ?,T)(B)
	(#xdaa . ?,T*(B)
	(#xdab . ?,T+(B)
	(#xdac . ?,T,(B)
	(#xdad . ?,T-(B)
	(#xdae . ?,T.(B)
	(#xdaf . ?,T/(B)
	(#xdb0 . ?,T0(B)
	(#xdb1 . ?,T1(B)
	(#xdb2 . ?,T2(B)
	(#xdb3 . ?,T3(B)
	(#xdb4 . ?,T4(B)
	(#xdb5 . ?,T5(B)
	(#xdb6 . ?,T6(B)
	(#xdb7 . ?,T7(B)
	(#xdb8 . ?,T8(B)
	(#xdb9 . ?,T9(B)
	(#xdba . ?,T:(B)
	(#xdbb . ?,T;(B)
	(#xdbc . ?,T<(B)
	(#xdbd . ?,T=(B)
	(#xdbe . ?,T>(B)
	(#xdbf . ?,T?(B)
	(#xdc0 . ?,T@(B)
	(#xdc1 . ?,TA(B)
	(#xdc2 . ?,TB(B)
	(#xdc3 . ?,TC(B)
	(#xdc4 . ?,TD(B)
	(#xdc5 . ?,TE(B)
	(#xdc6 . ?,TF(B)
	(#xdc7 . ?,TG(B)
	(#xdc8 . ?,TH(B)
	(#xdc9 . ?,TI(B)
	(#xdca . ?,TJ(B)
	(#xdcb . ?,TK(B)
	(#xdcc . ?,TL(B)
	(#xdcd . ?,TM(B)
	(#xdce . ?,TN(B)
	(#xdcf . ?,TO(B)
	(#xdd0 . ?,TP(B)
	(#xdd1 . ?,TQ(B)
	(#xdd2 . ?,TR(B)
	(#xdd3 . ?,TS(B)
	(#xdd4 . ?,TT(B)
	(#xdd5 . ?,TU(B)
	(#xdd6 . ?,TV(B)
	(#xdd7 . ?,TW(B)
	(#xdd8 . ?,TX(B)
	(#xdd9 . ?,TY(B)
	(#xdda . ?,TZ(B)
	(#xddf . ?,T_(B)
	(#xde0 . ?,T`(B)
	(#xde1 . ?,Ta(B)
	(#xde2 . ?,Tb(B)
	(#xde3 . ?,Tc(B)
	(#xde4 . ?,Td(B)
	(#xde5 . ?,Te(B)
	(#xde6 . ?,Tf(B)
	(#xde7 . ?,Tg(B)
	(#xde8 . ?,Th(B)
	(#xde9 . ?,Ti(B)
	(#xdea . ?,Tj(B)
	(#xdeb . ?,Tk(B)
	(#xdec . ?,Tl(B)
	(#xded . ?,Tm(B)
	(#xdf0 . ?,Tp(B)
	(#xdf1 . ?,Tq(B)
	(#xdf2 . ?,Tr(B)
	(#xdf3 . ?,Ts(B)
	(#xdf4 . ?,Tt(B)
	(#xdf5 . ?,Tu(B)
	(#xdf6 . ?,Tv(B)
	(#xdf7 . ?,Tw(B)
	(#xdf8 . ?,Tx(B)
	(#xdf9 . ?,Ty(B)
	;; Korean
	(#xea1 . ?$(C$!(B)
	(#xea2 . ?$(C$"(B)
	(#xea3 . ?$(C$#(B)
	(#xea4 . ?$(C$$(B)
	(#xea5 . ?$(C$%(B)
	(#xea6 . ?$(C$&(B)
	(#xea7 . ?$(C$'(B)
	(#xea8 . ?$(C$((B)
	(#xea9 . ?$(C$)(B)
	(#xeaa . ?$(C$*(B)
	(#xeab . ?$(C$+(B)
	(#xeac . ?$(C$,(B)
	(#xead . ?$(C$-(B)
	(#xeae . ?$(C$.(B)
	(#xeaf . ?$(C$/(B)
	(#xeb0 . ?$(C$0(B)
	(#xeb1 . ?$(C$1(B)
	(#xeb2 . ?$(C$2(B)
	(#xeb3 . ?$(C$3(B)
	(#xeb4 . ?$(C$4(B)
	(#xeb5 . ?$(C$5(B)
	(#xeb6 . ?$(C$6(B)
	(#xeb7 . ?$(C$7(B)
	(#xeb8 . ?$(C$8(B)
	(#xeb9 . ?$(C$9(B)
	(#xeba . ?$(C$:(B)
	(#xebb . ?$(C$;(B)
	(#xebc . ?$(C$<(B)
	(#xebd . ?$(C$=(B)
	(#xebe . ?$(C$>(B)
	(#xebf . ?$(C$?(B)
	(#xec0 . ?$(C$@(B)
	(#xec1 . ?$(C$A(B)
	(#xec2 . ?$(C$B(B)
	(#xec3 . ?$(C$C(B)
	(#xec4 . ?$(C$D(B)
	(#xec5 . ?$(C$E(B)
	(#xec6 . ?$(C$F(B)
	(#xec7 . ?$(C$G(B)
	(#xec8 . ?$(C$H(B)
	(#xec9 . ?$(C$I(B)
	(#xeca . ?$(C$J(B)
	(#xecb . ?$(C$K(B)
	(#xecc . ?$(C$L(B)
	(#xecd . ?$(C$M(B)
	(#xece . ?$(C$N(B)
	(#xecf . ?$(C$O(B)
	(#xed0 . ?$(C$P(B)
	(#xed1 . ?$(C$Q(B)
	(#xed2 . ?$(C$R(B)
	(#xed3 . ?$(C$S(B)
	(#xed4 . ?$,1LH(B)
	(#xed5 . ?$,1LI(B)
	(#xed6 . ?$,1LJ(B)
	(#xed7 . ?$,1LK(B)
	(#xed8 . ?$,1LL(B)
	(#xed9 . ?$,1LM(B)
	(#xeda . ?$,1LN(B)
	(#xedb . ?$,1LO(B)
	(#xedc . ?$,1LP(B)
	(#xedd . ?$,1LQ(B)
	(#xede . ?$,1LR(B)
	(#xedf . ?$,1LS(B)
	(#xee0 . ?$,1LT(B)
	(#xee1 . ?$,1LU(B)
	(#xee2 . ?$,1LV(B)
	(#xee3 . ?$,1LW(B)
	(#xee4 . ?$,1LX(B)
	(#xee5 . ?$,1LY(B)
	(#xee6 . ?$,1LZ(B)
	(#xee7 . ?$,1L[(B)
	(#xee8 . ?$,1L\(B)
	(#xee9 . ?$,1L](B)
	(#xeea . ?$,1L^(B)
	(#xeeb . ?$,1L_(B)
	(#xeec . ?$,1L`(B)
	(#xeed . ?$,1La(B)
	(#xeee . ?$,1Lb(B)
	(#xeef . ?$(C$](B)
	(#xef0 . ?$(C$a(B)
	(#xef1 . ?$(C$h(B)
	(#xef2 . ?$(C$o(B)
	(#xef3 . ?$(C$q(B)
	(#xef4 . ?$(C$t(B)
	(#xef5 . ?$(C$v(B)
	(#xef6 . ?$(C$}(B)
	(#xef7 . ?$(C$~(B)
	(#xef8 . ?$,1M+(B)
	(#xef9 . ?$,1M0(B)
	(#xefa . ?$,1M9(B)
	(#xeff . ?$,1tI(B)
	;; Latin-5
	;; Latin-6
	;; Latin-7
	;; Latin-8
	;; Latin-9
	(#x13bc . ?,b<(B)
	(#x13bd . ?,b=(B)
	(#x13be . ?,_/(B)
	;; Currency
	(#x20a0 . ?$,1t@(B)
	(#x20a1 . ?$,1tA(B)
	(#x20a2 . ?$,1tB(B)
	(#x20a3 . ?$,1tC(B)
	(#x20a4 . ?$,1tD(B)
	(#x20a5 . ?$,1tE(B)
	(#x20a6 . ?$,1tF(B)
	(#x20a7 . ?$,1tG(B)
	(#x20a8 . ?$,1tH(B)
	(#x20aa . ?$,1tJ(B)
	(#x20ab . ?$,1tK(B)
	(#x20ac . ?,b$(B)))
  (puthash (car pair) (cdr pair) x-keysym-table))

;; The following keysym codes for graphics are listed in the document
;; as not having unicodes available:

;; #x08b1	TOP LEFT SUMMATION	Technical
;; #x08b2	BOTTOM LEFT SUMMATION	Technical
;; #x08b3	TOP VERTICAL SUMMATION CONNECTOR	Technical
;; #x08b4	BOTTOM VERTICAL SUMMATION CONNECTOR	Technical
;; #x08b5	TOP RIGHT SUMMATION	Technical
;; #x08b6	BOTTOM RIGHT SUMMATION	Technical
;; #x08b7	RIGHT MIDDLE SUMMATION	Technical
;; #x0aac	SIGNIFICANT BLANK SYMBOL	Publish
;; #x0abd	DECIMAL POINT	Publish
;; #x0abf	MARKER	Publish
;; #x0acb	TRADEMARK SIGN IN CIRCLE	Publish
;; #x0ada	HEXAGRAM	Publish
;; #x0aff	CURSOR	Publish
;; #x0dde	THAI MAIHANAKAT	Thai


;;;; Selections

;; We keep track of the last text selected here, so we can check the
;; current selection against it, and avoid passing back our own text
;; from x-selection-value.  We track both
;; separately in case another X application only sets one of them
;; we aren't fooled by the PRIMARY or CLIPBOARD selection staying the same.
(defvar x-last-selected-text-clipboard nil
  "The value of the CLIPBOARD X selection last time we selected or
pasted text.")
(defvar x-last-selected-text-primary nil
  "The value of the PRIMARY X selection last time we selected or
pasted text.")

(defcustom x-select-enable-primary nil
  "Non-nil means cutting and pasting uses the primary selection."
  :type 'boolean
  :group 'killing
  :version "24.1")

(defcustom x-select-request-type nil
  "Data type request for X selection.
The value is one of the following data types, a list of them, or nil:
  `COMPOUND_TEXT', `UTF8_STRING', `STRING', `TEXT'

If the value is one of the above symbols, try only the specified type.

If the value is a list of them, try each of them in the specified
order until succeed.

The value nil is the same as the list (UTF8_STRING COMPOUND_TEXT STRING)."
  :type '(choice (const :tag "Default" nil)
		 (const COMPOUND_TEXT)
		 (const UTF8_STRING)
		 (const STRING)
		 (const TEXT)
		 (set :tag "List of values"
		      (const COMPOUND_TEXT)
		      (const UTF8_STRING)
		      (const STRING)
		      (const TEXT)))
  :group 'killing)

;; Get a selection value of type TYPE by calling x-get-selection with
;; an appropriate DATA-TYPE argument decided by `x-select-request-type'.
;; The return value is already decoded.  If x-get-selection causes an
;; error, this function return nil.

(defun x-selection-value-internal (type)
  (let ((request-type (or x-select-request-type
			  '(UTF8_STRING COMPOUND_TEXT STRING)))
	text)
    (if (consp request-type)
	(while (and request-type (not text))
	  (condition-case nil
	      (setq text (x-get-selection type (car request-type)))
	    (error nil))
	  (setq request-type (cdr request-type)))
      (condition-case nil
	  (setq text (x-get-selection type request-type))
	(error nil)))
    (if text
	(remove-text-properties 0 (length text) '(foreign-selection nil) text))
    text))

;; Return the value of the current X selection.
;; Consult the selection.  Treat empty strings as if they were unset.
;; If this function is called twice and finds the same text,
;; it returns nil the second time.  This is so that a single
;; selection won't be added to the kill ring over and over.
(defun x-selection-value ()
  ;; With multi-tty, this function may be called from a tty frame.
  (when (eq (framep (selected-frame)) 'x)
    (let (clip-text primary-text)
      (when x-select-enable-clipboard
        (setq clip-text (x-selection-value-internal 'CLIPBOARD))
        (if (string= clip-text "") (setq clip-text nil))

        ;; Check the CLIPBOARD selection for 'newness', is it different
        ;; from what we remembered them to be last time we did a
        ;; cut/paste operation.
        (setq clip-text
              (cond ;; check clipboard
               ((or (not clip-text) (string= clip-text ""))
                (setq x-last-selected-text-clipboard nil))
               ((eq      clip-text x-last-selected-text-clipboard) nil)
               ((string= clip-text x-last-selected-text-clipboard)
                ;; Record the newer string,
                ;; so subsequent calls can use the `eq' test.
                (setq x-last-selected-text-clipboard clip-text)
                nil)
               (t (setq x-last-selected-text-clipboard clip-text)))))

      (when x-select-enable-primary
	(setq primary-text (x-selection-value-internal 'PRIMARY))
	;; Check the PRIMARY selection for 'newness', is it different
	;; from what we remembered them to be last time we did a
	;; cut/paste operation.
	(setq primary-text
	      (cond ;; check primary selection
	       ((or (not primary-text) (string= primary-text ""))
		(setq x-last-selected-text-primary nil))
	       ((eq      primary-text x-last-selected-text-primary) nil)
	       ((string= primary-text x-last-selected-text-primary)
		;; Record the newer string,
		;; so subsequent calls can use the `eq' test.
		(setq x-last-selected-text-primary primary-text)
		nil)
	       (t
		(setq x-last-selected-text-primary primary-text)))))

      ;; As we have done one selection, clear this now.
      (setq next-selection-coding-system nil)

      ;; At this point we have recorded the current values for the
      ;; selection from clipboard (if we are supposed to) and primary.
      ;; So return the first one that has changed
      ;; (which is the first non-null one).
      ;;
      ;; NOTE: There will be cases where more than one of these has
      ;; changed and the new values differ.  This indicates that
      ;; something like the following has happened since the last time
      ;; we looked at the selections: Application X set all the
      ;; selections, then Application Y set only one of them.
      ;; In this case since we don't have
      ;; timestamps there is no way to know what the 'correct' value to
      ;; return is.  The nice thing to do would be to tell the user we
      ;; saw multiple possible selections and ask the user which was the
      ;; one they wanted.
      (or clip-text primary-text)
      )))

(define-obsolete-function-alias 'x-cut-buffer-or-selection-value
  'x-selection-value "24.1")

;; Arrange for the kill and yank functions to set and check the clipboard.
(setq interprogram-cut-function 'x-select-text)
(setq interprogram-paste-function 'x-selection-value)

;; Make paste from other applications use the decoding in x-select-request-type
;; and not just STRING.
(defun x-get-selection-value ()
  "Get the current value of the PRIMARY selection.
Request data types in the order specified by `x-select-request-type'."
  (x-selection-value-internal 'PRIMARY))

(defun x-clipboard-yank ()
  "Insert the clipboard contents, or the last stretch of killed text."
  (interactive "*")
  (let ((clipboard-text (x-selection-value-internal 'CLIPBOARD))
	(x-select-enable-clipboard t))
    (if (and clipboard-text (> (length clipboard-text) 0))
	(kill-new clipboard-text))
    (yank)))

(declare-function accelerate-menu "xmenu.c" (&optional frame) t)

(defun x-menu-bar-open (&optional frame)
  "Open the menu bar if `menu-bar-mode' is on, otherwise call `tmm-menubar'."
  (interactive "i")
  (if (and menu-bar-mode
	   (fboundp 'accelerate-menu))
      (accelerate-menu frame)
    (tmm-menubar)))


;;; Window system initialization.

(defun x-win-suspend-error ()
  ;; Don't allow suspending if any of the frames are X frames.
  (if (memq 'x (mapcar 'window-system (frame-list)))
      (error "Cannot suspend Emacs while running under X")))

(defvar x-initialized nil
  "Non-nil if the X window system has been initialized.")

(declare-function x-open-connection "xfns.c"
		  (display &optional xrm-string must-succeed))
(declare-function x-server-max-request-size "xfns.c" (&optional terminal))
(declare-function x-get-resource "frame.c"
		  (attribute class &optional component subclass))
(declare-function x-parse-geometry "frame.c" (string))
(defvar x-resource-name)
(defvar x-display-name)
(defvar x-command-line-resources)

(defun x-initialize-window-system ()
  "Initialize Emacs for X frames and open the first connection to an X server."
  ;; Make sure we have a valid resource name.
  (or (stringp x-resource-name)
      (let (i)
	(setq x-resource-name (invocation-name))

	;; Change any . or * characters in x-resource-name to hyphens,
	;; so as not to choke when we use it in X resource queries.
	(while (setq i (string-match "[.*]" x-resource-name))
	  (aset x-resource-name i ?-))))

  (x-open-connection (or x-display-name
			 (setq x-display-name (or (getenv "DISPLAY" (selected-frame))
						  (getenv "DISPLAY"))))
		     x-command-line-resources
		     ;; Exit Emacs with fatal error if this fails and we
		     ;; are the initial display.
		     (eq initial-window-system 'x))

  ;; Create the default fontset.
  (create-default-fontset)

  ;; Create the standard fontset.
  (condition-case err
	(create-fontset-from-fontset-spec standard-fontset-spec t)
    (error (display-warning
	    'initialization
	    (format "Creation of the standard fontset failed: %s" err)
	    :error)))

  ;; Create fontset specified in X resources "Fontset-N" (N is 0, 1, ...).
  (create-fontset-from-x-resource)

  ;; Set scroll bar mode to right if set by X resources. Default is left.
  (if (equal (x-get-resource "verticalScrollBars" "ScrollBars") "right")
      (customize-set-variable 'scroll-bar-mode 'right))

  ;; Apply a geometry resource to the initial frame.  Put it at the end
  ;; of the alist, so that anything specified on the command line takes
  ;; precedence.
  (let* ((res-geometry (x-get-resource "geometry" "Geometry"))
	 parsed)
    (if res-geometry
	(progn
	  (setq parsed (x-parse-geometry res-geometry))
	  ;; If the resource specifies a position,
	  ;; call the position and size "user-specified".
	  (if (or (assq 'top parsed) (assq 'left parsed))
	      (setq parsed (cons '(user-position . t)
				 (cons '(user-size . t) parsed))))
	  ;; All geometry parms apply to the initial frame.
	  (setq initial-frame-alist (append initial-frame-alist parsed))
	  ;; The size parms apply to all frames.  Don't set it if there are
	  ;; sizes there already (from command line).
	  (if (and (assq 'height parsed)
		   (not (assq 'height default-frame-alist)))
	      (setq default-frame-alist
		    (cons (cons 'height (cdr (assq 'height parsed)))
			  default-frame-alist)))
	  (if (and (assq 'width parsed)
		   (not (assq 'width default-frame-alist)))
	      (setq default-frame-alist
		    (cons (cons 'width (cdr (assq 'width parsed)))
			  default-frame-alist))))))

  ;; Check the reverseVideo resource.
  (let ((case-fold-search t))
    (let ((rv (x-get-resource "reverseVideo" "ReverseVideo")))
      (if (and rv
	       (string-match "^\\(true\\|yes\\|on\\)$" rv))
	  (setq default-frame-alist
		(cons '(reverse . t) default-frame-alist)))))

  ;; Set x-selection-timeout, measured in milliseconds.
  (let ((res-selection-timeout (x-get-resource "selectionTimeout"
					       "SelectionTimeout")))
    (setq x-selection-timeout
	  (if res-selection-timeout
	      (string-to-number res-selection-timeout)
	    5000)))

  ;; Don't let Emacs suspend under X.
  (add-hook 'suspend-hook 'x-win-suspend-error)

  ;; During initialization, we defer sending size hints to the window
  ;; manager, because that can induce a race condition:
  ;; http://lists.gnu.org/archive/html/emacs-devel/2008-10/msg00033.html
  ;; Send the size hints once initialization is done.
  (add-hook 'after-init-hook 'x-wm-set-size-hint)

  ;; Turn off window-splitting optimization; X is usually fast enough
  ;; that this is only annoying.
  (setq split-window-keep-point t)

  ;; Motif direct handling of f10 wasn't working right,
  ;; So temporarily we've turned it off in lwlib-Xm.c
  ;; and turned the Emacs f10 back on.
  ;; ;; Motif normally handles f10 itself, so don't try to handle it a second time.
  ;; (if (featurep 'motif)
  ;;     (global-set-key [f10] 'ignore))

  ;; Enable CLIPBOARD copy/paste through menu bar commands.
  (menu-bar-enable-clipboard)

  ;; ;; Override Paste so it looks at CLIPBOARD first.
  ;; (define-key menu-bar-edit-menu [paste]
  ;;   (append '(menu-item "Paste" x-clipboard-yank
  ;; 			:enable (not buffer-read-only)
  ;; 			:help "Paste (yank) text most recently cut/copied")
  ;; 	    nil))

  (setq x-initialized t))

(add-to-list 'handle-args-function-alist '(x . x-handle-args))
(add-to-list 'frame-creation-function-alist '(x . x-create-frame-with-faces))
(add-to-list 'window-system-initialization-alist '(x . x-initialize-window-system))

;; Initiate drag and drop
(add-hook 'after-make-frame-functions 'x-dnd-init-frame)
(define-key special-event-map [drag-n-drop] 'x-dnd-handle-drag-n-drop-event)

(defcustom x-gtk-stock-map
  (mapcar (lambda (arg)
	    (cons (purecopy (car arg)) (purecopy (cdr arg))))
  '(
    ("etc/images/new" . "gtk-new")
    ("etc/images/open" . "gtk-open")
    ("etc/images/diropen" . "n:system-file-manager")
    ("etc/images/close" . "gtk-close")
    ("etc/images/save" . "gtk-save")
    ("etc/images/saveas" . "gtk-save-as")
    ("etc/images/undo" . "gtk-undo")
    ("etc/images/cut" . "gtk-cut")
    ("etc/images/copy" . "gtk-copy")
    ("etc/images/paste" . "gtk-paste")
    ("etc/images/search" . "gtk-find")
    ("etc/images/print" . "gtk-print")
    ("etc/images/preferences" . "gtk-preferences")
    ("etc/images/help" . "gtk-help")
    ("etc/images/left-arrow" . "gtk-go-back")
    ("etc/images/right-arrow" . "gtk-go-forward")
    ("etc/images/home" . "gtk-home")
    ("etc/images/jump-to" . "gtk-jump-to")
    ("etc/images/index" . "gtk-index")
    ("etc/images/search" . "gtk-find")
    ("etc/images/exit" . "gtk-quit")
    ("etc/images/cancel" . "gtk-cancel")
    ("etc/images/info" . "gtk-info")
    ("etc/images/bookmark_add" . "n:bookmark_add")
    ;; Used in Gnus and/or MH-E:
    ("etc/images/attach" . "gtk-attach")
    ("etc/images/connect" . "gtk-connect")
    ("etc/images/contact" . "gtk-contact")
    ("etc/images/delete" . "gtk-delete")
    ("etc/images/describe" . "gtk-properties")
    ("etc/images/disconnect" . "gtk-disconnect")
    ;; ("etc/images/exit" . "gtk-exit")
    ("etc/images/lock-broken" . "gtk-lock_broken")
    ("etc/images/lock-ok" . "gtk-lock_ok")
    ("etc/images/lock" . "gtk-lock")
    ("etc/images/next-page" . "gtk-next-page")
    ("etc/images/refresh" . "gtk-refresh")
    ("etc/images/sort-ascending" . "gtk-sort-ascending")
    ("etc/images/sort-column-ascending" . "gtk-sort-column-ascending")
    ("etc/images/sort-criteria" . "gtk-sort-criteria")
    ("etc/images/sort-descending" . "gtk-sort-descending")
    ("etc/images/sort-row-ascending" . "gtk-sort-row-ascending")
    ("images/gnus/toggle-subscription" . "gtk-task-recurring")
    ("images/mail/compose" . "gtk-mail-compose")
    ("images/mail/copy" . "gtk-mail-copy")
    ("images/mail/forward" . "gtk-mail-forward")
    ("images/mail/inbox" . "gtk-inbox")
    ("images/mail/move" . "gtk-mail-move")
    ("images/mail/not-spam" . "gtk-not-spam")
    ("images/mail/outbox" . "gtk-outbox")
    ("images/mail/reply-all" . "gtk-mail-reply-to-all")
    ("images/mail/reply" . "gtk-mail-reply")
    ("images/mail/save-draft" . "gtk-mail-handling")
    ("images/mail/send" . "gtk-mail-send")
    ("images/mail/spam" . "gtk-spam")
    ;; Used for GDB Graphical Interface
    ("images/gud/break" . "gtk-no")
    ("images/gud/recstart" . "gtk-media-record")
    ("images/gud/recstop" . "gtk-media-stop")
    ;; No themed versions available:
    ;; mail/preview (combining stock_mail and stock_zoom)
    ;; mail/save    (combining stock_mail, stock_save and stock_convert)
    ))
  "How icons for tool bars are mapped to Gtk+ stock items.
Emacs must be compiled with the Gtk+ toolkit for this to have any effect.
A value that begins with n: denotes a named icon instead of a stock icon."
  :version "22.2"
  :type '(choice (repeat (choice symbol
				 (cons (string :tag "Emacs icon")
				       (string :tag "Stock/named")))))
  :group 'x)

(defcustom icon-map-list '(x-gtk-stock-map)
  "A list of alists that map icon file names to stock/named icons.
The alists are searched in the order they appear.  The first match is used.
The keys in the alists are file names without extension and with two directory
components.  For example, to map /usr/share/emacs/22.1.1/etc/images/open.xpm
to stock item gtk-open, use:

  (\"etc/images/open\" . \"gtk-open\")

Themes also have named icons.  To map to one of those, use n: before the name:

  (\"etc/images/diropen\" . \"n:system-file-manager\")

The list elements are either the symbol name for the alist or the
alist itself.

If you don't want stock icons, set the variable to nil."
  :version "22.2"
  :type '(choice (const :tag "Don't use stock icons" nil)
		 (repeat (choice symbol
				 (cons (string :tag "Emacs icon")
				       (string :tag "Stock/named")))))
  :group 'x)

(defconst x-gtk-stock-cache (make-hash-table :weakness t :test 'equal))

(defun x-gtk-map-stock (file)
  "Map icon with file name FILE to a Gtk+ stock name.
This uses `icon-map-list' to map icon file names to stock icon names."
  (when (stringp file)
    (or (gethash file x-gtk-stock-cache)
	(puthash
	 file
	 (save-match-data
	   (let* ((file-sans (file-name-sans-extension file))
		  (key (and (string-match "/\\([^/]+/[^/]+/[^/]+$\\)"
					  file-sans)
			    (match-string 1 file-sans)))
		  (icon-map icon-map-list)
		  elem value)
	     (while (and (null value) icon-map)
	       (setq elem (car icon-map)
		     value (assoc-string (or key file-sans)
					 (if (symbolp elem)
					     (symbol-value elem)
					   elem))
		     icon-map (cdr icon-map)))
	     (and value (cdr value))))
	 x-gtk-stock-cache))))

(provide 'x-win)

;;; x-win.el ends here
