;;; iris-ansi.el --- configure Emacs for SGI xwsh and winterm apps -*- no-byte-compile: t -*-

;; Copyright (C) 1997, 2001-2012 Free Software Foundation, Inc.

;; Author: Dan Nicolaescu <dann@ics.uci.edu>

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

(defvar iris-function-map
  (let ((map (make-sparse-keymap)))

    (define-key map "\e[120q" [S-escape])
    (define-key map "\e[121q" [C-escape])

    (define-key map "\e[001q" [f1])
    (define-key map "\e[013q" [S-f1])
    (define-key map "\e[025q" [C-f1])


    (define-key map "\e[002q" [f2])
    (define-key map "\e[014q" [S-f2])
    (define-key map "\e[026q" [C-f2])
    (define-key map "\e[038q" [M-f2])

    (define-key map "\e[003q" [f3])
    (define-key map "\e[015q" [S-f3])
    (define-key map "\e[027q" [C-f3])


    (define-key map "\e[004q" [f4])
    (define-key map "\e[016q" [S-f4])
    (define-key map "\e[028q" [C-f4])


    (define-key map "\e[005q" [f5])
    (define-key map "\e[017q" [S-f5])
    (define-key map "\e[029q" [C-f5])


    (define-key map "\e[006q" [f6])
    (define-key map "\e[018q" [S-f6])
    (define-key map "\e[030q" [C-f6])


    (define-key map "\e[007q" [f7])
    (define-key map "\e[019q" [S-f7])
    (define-key map "\e[031q" [C-f7])


    (define-key map "\e[008q" [f8])
    (define-key map "\e[020q" [S-f8])
    (define-key map "\e[032q" [C-f8])


    (define-key map "\e[009q" [f9])
    (define-key map "\e[021q" [S-f9])
    (define-key map "\e[033q" [C-f9])


    (define-key map "\e[010q" [f10])
    (define-key map "\e[022q" [S-f10])
    (define-key map "\e[034q" [C-f10])


    (define-key map "\e[011q" [f11])
    (define-key map "\e[023q" [S-f11])
    (define-key map "\e[035q" [C-f11])
    (define-key map "\e[047q" [M-f11])

    (define-key map "\e[012q" [f12])
    (define-key map "\e[024q" [S-f12])
    (define-key map "\e[036q" [C-f12])
    (define-key map "\e[048q" [M-f12])


    (define-key map "\e[057q" [?\C-`])
    (define-key map "\e[115q" [?\M-`])

    (define-key map "\e[049q" [?\C-1])
    (define-key map "\e[058q" [?\M-1])


    (define-key map "\e[059q" [?\M-2])

    (define-key map "\e[050q" [?\C-3])
    (define-key map "\e[060q" [?\M-3])

    (define-key map "\e[051q" [?\C-4])
    (define-key map "\e[061q" [?\M-4])

    (define-key map "\e[052q" [?\C-5])
    (define-key map "\e[062q" [?\M-5])


    (define-key map "\e[063q" [?\M-6])

    (define-key map "\e[053q" [?\C-7])
    (define-key map "\e[064q" [?\M-7])

    (define-key map "\e[054q" [?\C-8])
    (define-key map "\e[065q" [?\M-8])

    (define-key map "\e[055q" [?\C-9])
    (define-key map "\e[066q" [?\M-9])

    (define-key map "\e[056q" [?\C-0])
    (define-key map "\e[067q" [?\M-0])

    (define-key map "\e[068q" [?\M--])

    (define-key map "\e[069q" [?\C-=])
    (define-key map "\e[070q" [?\M-=])

    ;; I don't know what to do with those.
    ;;(define-key map "^H" [<del>])
    ;;(define-key map "^H" [S-<del>])
    ;;(define-key map "\177" [C-<del>])
    ;;(define-key map "\e[071q" [M-<del>])

    (define-key map "\e[Z" [?\S-\t])
    (define-key map "\e[072q" [?\C-\t])
    ;; This only works if you remove the M-TAB keybinding from the system.4Dwmrc
    ;; or your ~/.4Dwmrc, if you use the 4Dwm window manager.
    (define-key map "\e[073q" [?\M-\t])

    (define-key map "\e[074q" [?\M-q])

    (define-key map "\e[075q" [?\M-w])

    (define-key map "\e[076q" [?\M-e])

    (define-key map "\e[077q" [?\M-r])

    (define-key map "\e[078q" [?\M-t])

    (define-key map "\e[079q" [?\M-y])

    (define-key map "\e[080q" [?\M-u])

    (define-key map "\e[081q" [?\M-i])

    (define-key map "\e[082q" [?\M-o])

    (define-key map "\e[083q" [?\M-p])

    (define-key map "\e[084q" [?\M-\[])

    (define-key map "\e[085q" [?\M-\]])

    (define-key map "\e[086q" [?\M-\\])

    (define-key map "\e[087q" [?\M-a])

    (define-key map "\e[088q" [?\M-s])

    (define-key map "\e[089q" [?\M-d])

    (define-key map "\e[090q" [?\M-f])

    (define-key map "\e[091q" [?\M-g])

    (define-key map "\e[092q" [?\M-h])

    (define-key map "\e[093q" [?\M-j])

    (define-key map "\e[094q" [?\M-k])

    (define-key map "\e[095q" [?\M-l])

    (define-key map "\e[096q" [?\C-\;])
    ;; We are cheating here, this is really M-;, but M-: generates the same
    ;; string and is more useful.
    (define-key map "\e[097q" [?\M-:])

    (define-key map "\e[098q" [?\C-'])
    (define-key map "\e[099q" [?\M-'])

    (define-key map "\e[100q" [?\M-\n])

    (define-key map "\e[101q" [?\M-z])

    (define-key map "\e[102q" [?\M-x])

    (define-key map "\e[103q" [?\M-c])

    (define-key map "\e[104q" [?\M-v])

    (define-key map "\e[105q" [?\M-b])

    (define-key map "\e[106q" [M-n])

    (define-key map "\e[107q" [M-m])

    (define-key map "\e[108q" [?\C-,])
    (define-key map "\e[109q" [?\M-,])

    (define-key map "\e[110q" [?\C-.])
    (define-key map "\e[111q" [?\M-.])

    (define-key map "\e[112q" [?\C-/])
    (define-key map "\e[113q" [?\M-/])

    (define-key map "\e[139q" [insert])
    (define-key map "\e[139q" [S-insert])
    (define-key map "\e[140q" [C-insert])
    (define-key map "\e[141q" [M-insert])

    (define-key map "\e[H" [home])
    (define-key map "\e[143q" [S-home])
    (define-key map "\e[144q" [C-home])


    (define-key map "\e[150q" [prior])
    (define-key map "\e[151q" [S-prior]) ;Those don't seem to generate anything.
    (define-key map "\e[152q" [C-prior])


    ;; (define-key map "^?" [delete]) ?? something else seems to take care of this.
    (define-key map "\e[P" [S-delete])
    (define-key map "\e[142q" [C-delete])
    (define-key map "\e[M" [M-delete])

    (define-key map "\e[146q" [end])
    (define-key map "\e[147q" [S-end]) ; Those don't seem to generate anything.
    (define-key map "\e[148q" [C-end])

    (define-key map "\e[154q" [next])
    (define-key map "\e[155q" [S-next])
    (define-key map "\e[156q" [C-next])


    (define-key map "\e[161q" [S-up])
    (define-key map "\e[162q" [C-up])
    (define-key map "\e[163q" [M-up])

    (define-key map "\e[158q" [S-left])
    (define-key map "\e[159q" [C-left])
    (define-key map "\e[160q" [M-left])

    (define-key map "\e[164q" [S-down])
    (define-key map "\e[165q" [C-down])
    (define-key map "\e[166q" [M-down])

    (define-key map "\e[167q" [S-right])
    (define-key map "\e[168q" [C-right])
    (define-key map "\e[169q" [M-right])

    ;; Keypad functions, most of those are untested.
    (define-key map "\e[179q" [?\C-/])
    (define-key map "\e[180q" [?\M-/])

    (define-key map "\e[187q" [?\C-*])
    (define-key map "\e[188q" [?\M-*])

    (define-key map "\e[198q" [?\C--])
    (define-key map "\e[199q" [?\M--])

    ;; Something else takes care of home, up, prior, down, left, right, next
    ;;(define-key map "\e[H" [home])
    (define-key map "\e[172q" [C-home])

    ;;(define-key map "\e[A" [up])
    (define-key map "\e[182q" [C-up])


    ;;(define-key map "\e[150q" [prior])
    (define-key map "\e[190q" [C-prior])


    (define-key map "\e[200q" [?\C-+])
    (define-key map "\e[201q" [?\M-+])

    ;;(define-key map "\e[D" [left])
    (define-key map "\e[174q" [C-left])


    (define-key map "\e[000q" [begin])
    (define-key map "\e[184q" [C-begin])


    ;;(define-key map "\e[C" [right])
    (define-key map "\e[192q" [C-right])

    ;;(define-key map "\e[146q" [end])
    (define-key map "\e[176q" [C-end])

    ;;(define-key map "\e[B" [down])
    (define-key map "\e[186q" [C-down])

    ;;(define-key map "\e[154q" [next])
    (define-key map "\e[194q" [C-next])

    (define-key map "\e[100q" [M-enter])

    (define-key map "\e[139q" [insert])
    (define-key map "\e[178q" [C-inset])

    (define-key map "\e[P" [delete])
    (define-key map "\e[196q" [C-delete])
    (define-key map "\e[197q" [M-delete])
    map)
  "Function key definitions for SGI xwsh and winterm apps.")

(defun terminal-init-iris-ansi ()
  "Terminal initialization function for iris-ansi."
  ;; Use inheritance to let the main keymap override these defaults.
  ;; This way we don't override terminfo-derived settings or settings
  ;; made in the .emacs file.
  (let ((m (copy-keymap iris-function-map)))
    (set-keymap-parent m (keymap-parent input-decode-map))
    (set-keymap-parent input-decode-map m)))

;;; iris-ansi.el ends here
