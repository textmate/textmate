;;; mixal-mode.el --- Major mode for the mix asm language.

;; Copyright (C) 2003-2012  Free Software Foundation, Inc.

;; Author: Pieter E.J. Pareit <pieter.pareit@gmail.com>
;; Maintainer: Pieter E.J. Pareit <pieter.pareit@gmail.com>
;; Created: 09 Nov 2002
;; Version: 0.1
;; Keywords: languages, Knuth, mix, mixal, asm, mixvm, The Art Of Computer Programming

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
;; Major mode for the mix asm language.
;; The mix asm language is described in "The Art Of Computer Programming".
;;
;; For optimal use, also use GNU MDK.  Compiling needs mixasm, running
;; and debugging needs mixvm and mixvm.el from GNU MDK.  You can get
;; GNU MDK from `https://savannah.gnu.org/projects/mdk/' and
;; `ftp://ftp.gnu.org/pub/gnu/mdk'.
;;
;; To use this mode, place the following in your .emacs file:
;; `(load-file "/PATH-TO-FILE/mixal-mode.el")'.
;; When you load a file with the extension .mixal the mode will be started
;; automatic.  If you want to start the mode manual, use `M-x mixal-mode'.
;; Font locking will work, the behavior of tabs is the same as Emacs's
;; default behavior.  You can compile a source file with `C-c c' you can
;; run a compiled file with `C-c r' or run it in debug mode with `C-c d'.
;; You can get more information about a particular operation code by using
;; mixal-describe-operation-code or `C-h o'.
;;
;; Have fun.

;;; History:
;; Version 0.3:
;; 12/10/05: Stefan Monnier <monnier@iro.umontreal.ca>
;;           Use font-lock-syntactic-keywords to detect/mark comments.
;;           Use [^ \t\n]+ to match the operand part of a line.
;;           Drop mixal-operation-codes.
;;           Build the mixal-operation-codes-alist immediately.
;;           Use `interactive' in mixal-describe-operation-code.
;;           Remove useless ".*$" at the end of some regexps.
;;           Fix the definition of comment-start-skip.
;; 08/10/05: sync mdk and emacs cvs
;;           from emacs: compile-command and require-final-newline
;;           from mdk:   see version 0.2
;;           correct my email address
;; Version 0.2:
;; 06/04/05: mixasm no longer needs -g option
;;           fontlocking of comments works in all? cases now
;;           added some more mixal-operation-codes
;; Version 0.1:
;; Version 0.1.1:
;; 22/11/02: bugfix in fontlocking, needed to add a '-' to the regex.
;; 19/11/02: completed implementing mixal-describe-operation-code.
;; 13/11/02: implemented compile, mixal-run and mixal-debug.
;; 10/11/02: implemented font-locking and syntax table.
;; 09/11/02: started mixal-mode.

;;; Code:
(defvar compile-command)

;;; Key map
(defvar mixal-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'compile)
    (define-key map "\C-c\C-r" 'mixal-run)
    (define-key map "\C-c\C-d" 'mixal-debug)
    (define-key map "\C-h\C-o" 'mixal-describe-operation-code)
    map)
  "Keymap for `mixal-mode'.")
;; (makunbound 'mixal-mode-map)

;;; Syntax table
(defvar mixal-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; We need to do a bit more to make fontlocking for comments work.
    ;; See use of syntax-propertize-function.
    ;; (modify-syntax-entry ?* "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `mixal-mode'.")

(defvar mixal-font-lock-label-face 'font-lock-variable-name-face
  "Face name to use for label names.
Default value is that of `font-lock-variable-name-face', but you can modify
its value.")

(defvar mixal-font-lock-operation-code-face 'font-lock-keyword-face
  "Face name to use for operation code names.
Default value is that of `font-lock-keyword-face', but you can modify its
value.")

(defvar mixal-font-lock-assembly-pseudoinstruction-face 'font-lock-builtin-face
  "Face name to use for assembly pseudoinstruction names.
Default value is that of `font-lock-builtin-face', but you can modify its
value.")

(defvar mixal-assembly-pseudoinstructions
  '("ORIG" "EQU" "CON" "ALF" "END")
  "List of possible assembly pseudoinstructions.")

;;;; Compilation
;; Output from mixasm is compatible with default behavior of emacs,
;; I just added a key (C-cc) and modified the make-command.

;;;; Indentation
;; Tabs works well by default.

;;;; Describe
(defvar mixal-operation-codes-alist
  ;; FIXME: the codes FADD, FSUB, FMUL, FDIV, JRAD, and FCMP were in
  ;; mixal-operation-codes but not here.  They should probably be added here.
  ;;
  ;; We used to define this with a backquote and subexps like ,(+ 8 3) for
  ;; better clarity, but the resulting code was too big and caused the
  ;; byte-compiler to eat up all the stack space.  Even using
  ;; `eval-when-compile' didn't help because the byte-compiler insists on
  ;; compiling the code before evaluating it.
  '((LDA loading "load A" 8 field
         "Put in rA the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word."
         2)

    (LDX loading "load X" 15 field
         "Put in rX the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word."
         2)

    (LD1 loading "load I1" 9 field
         "Put in rI1 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign; trying
to set anything more than that will result in undefined behavior."
         2)

    (LD2 loading "load I2" 10 field
         "Put in rI2 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign; trying
to set anything more than that will result in undefined behavior."
         2)

    (LD3 loading "load I3" 11 field
         "Put in rI3 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign; trying
to set anything more than that will result in undefined behavior."
         2)

    (LD4 loading "load I4" 12 field
         "Put in rI4 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign; trying
to set anything more than that will result in undefined behavior."
         2)

    (LD5 loading "load I5" 13 field
         "Put in rI5 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign; trying
to set anything more than that will result in undefined behavior."
         2)

    (LD6 loading "load I6" 14 field
         "Put in rI6 the contents of cell no. M.
Uses a + when there is no sign in subfield. Subfield is left padded with
zeros to make a word. Index registers only have 2 bytes and a sign; trying
to set anything more than that will result in undefined behavior."
         2)

    (LDAN loading "load A negative" 16 field
          "Put in rA the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word."
          2)

    (LDXN loading "load X negative" 23 field
          "Put in rX the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word."
          2)

    (LD1N loading "load I1 negative" 17 field
          "Put in rI1 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign; trying to set anything more than that will result
in undefined behavior."
          2)

    (LD2N loading "load I2 negative" 18 field
          "Put in rI2 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign; trying to set anything more than that will result
in undefined behavior."
          2)

    (LD3N loading "load I3 negative" 19 field
          "Put in rI3 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign; trying to set anything more than that will result
in undefined behavior."
          2)

    (LD4N loading "load I4 negative" 20 field
          "Put in rI4 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign; trying to set anything more than that will result
in undefined behavior."
          2)

    (LD5N loading "load I5 negative" 21 field
          "Put in rI5 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign; trying to set anything more than that will result
in undefined behavior."
          2)

    (LD6N loading "load I6 negative" 22 field
          "Put in rI6 the contents of cell no. M, with opposite sign.
Uses a + when there is no sign in subfield, otherwise use the opposite sign.
Subfield is left padded with zeros to make a word. Index registers only
have 2 bytes and a sign; trying to set anything more than that will result
in undefined behavior."
          2)

    (STA storing "store A" 24 field
         "Store in cell Nr. M the contents of rA.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield."
         2)

    (STX storing "store X" 31 field
         "Store in cell Nr. M the contents of rX.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield."
         2)

    (ST1 storing "store I1" 25 field
         "Store in cell Nr. M the contents of rI1.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
         2)

    (ST2 storing "store I2" 26 field
         "Store in cell Nr. M the contents of rI2.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
         2)

    (ST3 storing "store I3" 27 field
         "Store in cell Nr. M the contents of rI3.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
         2)

    (ST4 storing "store I4" 28 field
         "Store in cell Nr. M the contents of rI4.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
         2)

    (ST5 storing "store I5" 29 field
         "Store in cell Nr. M the contents of rI5.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
         2)

    (ST6 storing "store I6" 30 field
         "Store in cell Nr. M the contents of rI6.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The
sign of the memory cell is not changed, unless it is part of the subfield.
Because index registers only have 2 bytes and a sign, the rest of the bytes
are assumed to be 0."
         2)

    (STJ storing "store J" 32 field
         "Store in cell Nr. M the contents of rJ.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with bytes from a register. These
bytes are taken beginning by the rightmost side of the register. The sign
of rJ is always +, sign of the memory cell is not changed, unless it is
part of the subfield. The default field for STJ is (0:2)."
         2)

    (STZ storing "store zero" 33 field
         "Store in cell Nr. M '+ 0'.
The modification of the operation code represents the subfield of the
memory cell that is to be overwritten with zeros."
         2)

    (ADD arithmetic "add" 1 field
         "Add to A the contents of cell Nr. M.
Subfield is padded with zero to make a word.
If the result is to large, the operation result modulo 1,073,741,823 (the
maximum value storable in a MIX word) is stored in `rA', and the overflow
toggle is set to TRUE."
         2)

    (SUB arithmetic "subtract" 2 field
         "Subtract to A the contents of cell Nr. M.
Subfield is padded with zero to make a word.
If the result is to large, the operation result modulo 1,073,741,823 (the
maximum value storable in a MIX word) is stored in `rA', and the overflow
toggle is set to TRUE."
         2)

    (MUL arithmetic "multiply" 3 field
         "Multiplies the contents of cell Nr. M with A, result is 10 bytes and stored in rA and rX.
The sign is + if the sign of rA and cell M where the same, otherwise, it is -"
         10)

    (DIV arithmetic "divide" 4 field
         "Both rA and rX are taken together and divided by cell Nr. M, quotient is placed in rA, remainder in rX.
The sign is taken from rA, and after the divide the sign of rA is set to + when
both the sign of rA and M where the same. Divide by zero and overflow of rA
result in undefined behavior."
         12)

    (ENTA address-transfer "enter A" 48
          "Literal value is stored in rA.
Indexed, stores value of index in rA."
          1)

    (ENTX address-transfer "enter X" 55
          "Literal value is stored in rX.
Indexed, stores value of index in rX."
          1)

    (ENT1 address-transfer "Enter rI1" 49
          "Literal value is stored in rI1.
Indexed, stores value of index in rI1."
          1)

    (ENT2 address-transfer "Enter rI2" 50
          "Literal value is stored in rI2.
Indexed, stores value of index in rI2."
          1)

    (ENT3 address-transfer "Enter rI3" 51
          "Literal value is stored in rI3.
Indexed, stores value of index in rI3."
          1)

    (ENT4 address-transfer "Enter rI4" 52
          "Literal value is stored in rI4.
Indexed, stores value of index in rI4."
          1)

    (ENT5 address-transfer "Enter rI5" 53
          "Literal value is stored in rI5.
Indexed, stores value of index in rI5."
          1)

    (ENT6 address-transfer "Enter rI6" 54
          "Literal value is stored in rI6.
Indexed, stores value of index in rI6."
          1)

    (ENNA address-transfer "enter negative A" 48
          "Literal value is stored in rA with opposite sign.
Indexed, stores value of index in rA with opposite sign."
          1)

    (ENNX address-transfer "enter negative X" 55
          "Literal value is stored in rX with opposite sign.
Indexed, stores value of index in rX with opposite sign."
          1)

    (ENN1 address-transfer "Enter negative rI1" 49
          "Literal value is stored in rI1 with opposite sign.
Indexed, stores value of index in rI1 with opposite sign."
          1)

    (ENN2 address-transfer "Enter negative rI2" 50
          "Literal value is stored in rI2 with opposite sign.
Indexed, stores value of index in rI2 with opposite sign."
          1)

    (ENN3 address-transfer "Enter negative rI3" 51
          "Literal value is stored in rI3 with opposite sign.
Indexed, stores value of index in rI3 with opposite sign."
          1)

    (ENN4 address-transfer "Enter negative rI4" 52
          "Literal value is stored in rI4 with opposite sign.
Indexed, stores value of index in rI4 with opposite sign."
          1)

    (ENN5 address-transfer "Enter negative rI5" 53
          "Literal value is stored in rI5 with opposite sign.
Indexed, stores value of index in rI5 with opposite sign."
          1)

    (ENN6 address-transfer "Enter negative rI6" 54
          "Literal value is stored in rI6 with opposite sign.
Indexed, stores value of index in rI6 with opposite sign."
          1)

    (INCA address-transfer "increase A" 48
          "Increase register A with the literal value of M.
On overflow the overflow toggle is set."
          1)

    (INCX address-transfer "increase X" 55
          "Increase register X with the literal value of M.
On overflow the overflow toggle is set."
          1)

    (INC1 address-transfer "increase I1" 49
          "Increase register I1 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (INC2 address-transfer "increase I2" 50
          "Increase register I2 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (INC3 address-transfer "increase I3" 51
          "Increase register I3 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (INC4 address-transfer "increase I4" 52
          "Increase register I4 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (INC5 address-transfer "increase I5" 53
          "Increase register I5 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (INC6 address-transfer "increase I6" 54
          "Increase register I6 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (DECA address-transfer "decrease A" 48
          "Decrease register A with the literal value of M.
On overflow the overflow toggle is set."
          1)

    (DECX address-transfer "decrease X" 55
          "Decrease register X with the literal value of M.
On overflow the overflow toggle is set."
          1)

    (DEC1 address-transfer "decrease I1" 49
          "Decrease register I1 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (DEC2 address-transfer "decrease I2" 50
          "Decrease register I2 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (DEC3 address-transfer "decrease I3" 51
          "Decrease register I3 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (DEC4 address-transfer "decrease I4" 52
          "Decrease register I4 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (DEC5 address-transfer "decrease I5" 53
          "Decrease register I5 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (DEC6 address-transfer "decrease I6" 54
          "Decrease register I6 with the literal value of M.
The result is undefined when the result does not fit in
2 bytes."
          1)

    (CMPA comparison "compare A" 56 field
          "Compare contents of A with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome."
          2)

    (CMPX comparison "compare X" 63 field
          "Compare contents of rX with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome."
          2)

    (CMP1 comparison "compare I1" 57 field
          "Compare contents of rI1 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
          2)

    (CMP2 comparison "compare I2" 58 field
          "Compare contents of rI2 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
          2)

    (CMP3 comparison "compare I3" 59 field
          "Compare contents of rI3 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
          2)

    (CMP4 comparison "compare I4" 60 field
          "Compare contents of rI4 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
          2)

    (CMP5 comparison "compare I5" 61 field
          "Compare contents of rI5 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
          2)

    (CMP6 comparison "compare I6" 62 field
          "Compare contents of rI6 with contents of M.
The field specifier works on both fields. The comparison indicator
is set to LESS, EQUAL or GREATER depending on the outcome. Bit 1,2 and 3
have a value of 0."
          2)

    (JMP jump "jump" 39
         "Unconditional jump.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JSJ jump "jump, save J" 39
         "Unconditional jump, but rJ is not modified."
         1)

    (JOV jump "jump on overflow" 39
         "Jump if OV is set (and turn it off).
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JNOV jump "Jump on no overflow" 39
          "Jump if OV is not set (and turn it off).
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (JL jump "Jump on less" 39
        "Jump if '[CM] = L'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
        1)

    (JE jump "Jump on equal" 39
        "Jump if '[CM] = E'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
        1)

    (JG jump "Jump on greater" 39
        "Jump if '[CM] = G'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
        1)

    (JGE jump "Jump on not less" 39
         "Jump if '[CM]' does not equal 'L'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JNE jump "Jump on not equal" 39
         "Jump if '[CM]' does not equal 'E'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JLE jump "Jump on not greater" 39
         "Jump if '[CM]' does not equal 'G'.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JAN jump "jump A negative" 40
         "Jump if the content of rA is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JAZ jump "jump A zero" 40
         "Jump if the content of rA is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JAP jump "jump A positive" 40
         "Jump if the content of rA is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JANN jump "jump A non-negative" 40
          "Jump if the content of rA is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (JANZ jump "jump A non-zero" 40
          "Jump if the content of rA is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (JANP jump "jump A non-positive" 40
          "Jump if the content of rA is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (JXN jump "jump X negative" 47
         "Jump if the content of rX is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JXZ jump "jump X zero" 47
         "Jump if the content of rX is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JXP jump "jump X positive" 47
         "Jump if the content of rX is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (JXNN jump "jump X non-negative" 47
          "Jump if the content of rX is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (JXNZ jump "jump X non-zero" 47
          "Jump if the content of rX is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (JXNP jump "jump X non-positive" 47
          "Jump if the content of rX is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J1N jump "jump I1 negative" 41
         "Jump if the content of rI1 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J1Z jump "jump I1 zero" 41
         "Jump if the content of rI1 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J1P jump "jump I1 positive" 41
         "Jump if the content of rI1 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J1NN jump "jump I1 non-negative" 41
          "Jump if the content of rI1 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J1NZ jump "jump I1 non-zero" 41
          "Jump if the content of rI1 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J1NP jump "jump I1 non-positive" 41
          "Jump if the content of rI1 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J2N jump "jump I2 negative" 41
         "Jump if the content of rI2 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J2Z jump "jump I2 zero" 41
         "Jump if the content of rI2 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J2P jump "jump I2 positive" 41
         "Jump if the content of rI2 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J2NN jump "jump I2 non-negative" 41
          "Jump if the content of rI2 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J2NZ jump "jump I2 non-zero" 41
          "Jump if the content of rI2 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J2NP jump "jump I2 non-positive" 41
          "Jump if the content of rI2 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J3N jump "jump I3 negative" 41
         "Jump if the content of rI3 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J3Z jump "jump I3 zero" 41
         "Jump if the content of rI3 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J3P jump "jump I3 positive" 41
         "Jump if the content of rI3 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J3NN jump "jump I3 non-negative" 41
          "Jump if the content of rI3 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J3NZ jump "jump I3 non-zero" 41
          "Jump if the content of rI3 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J3NP jump "jump I3 non-positive" 41
          "Jump if the content of rI3 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J4N jump "jump I4 negative" 41
         "Jump if the content of rI4 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J4Z jump "jump I4 zero" 41
         "Jump if the content of rI4 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J4P jump "jump I4 positive" 41
         "Jump if the content of rI4 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J4NN jump "jump I4 non-negative" 41
          "Jump if the content of rI4 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J4NZ jump "jump I4 non-zero" 41
          "Jump if the content of rI4 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J4NP jump "jump I4 non-positive" 41
          "Jump if the content of rI4 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J5N jump "jump I5 negative" 41
         "Jump if the content of rI5 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J5Z jump "jump I5 zero" 41
         "Jump if the content of rI5 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J5P jump "jump I5 positive" 41
         "Jump if the content of rI5 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J5NN jump "jump I5 non-negative" 41
          "Jump if the content of rI5 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J5NZ jump "jump I5 non-zero" 41
          "Jump if the content of rI5 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J5NP jump "jump I5 non-positive" 41
          "Jump if the content of rI5 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J6N jump "jump I6 negative" 41
         "Jump if the content of rI6 is negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J6Z jump "jump I6 zero" 41
         "Jump if the content of rI6 is zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J6P jump "jump I6 positive" 41
         "Jump if the content of rI6 is positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
         1)

    (J6NN jump "jump I6 non-negative" 41
          "Jump if the content of rI6 is non-negative.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J6NZ jump "jump I6 non-zero" 41
          "Jump if the content of rI6 is non-zero.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (J6NP jump "jump I6 non-positive" 41
          "Jump if the content of rI6 is non-positive.
Register J is set to the value of the next instruction that would have
been executed when there was no jump."
          1)

    (SLA miscellaneous "shift left A" 6
         "Shift to A, M bytes left.
Hero's will be added to the right."
         2)

    (SRA miscellaneous "shift right A" 6
         "Shift to A, M bytes right.
Zeros will be added to the left."
         2)

    (SLAX miscellaneous "shift left AX" 6
          "Shift AX, M bytes left.
Zeros will be added to the right."
          2)


    (SRAX miscellaneous "shift right AX" 6
          "Shift AX, M bytes right.
Zeros will be added to the left."
          2)

    (SLC miscellaneous "shift left AX circularly" 6
         "Shift AX, M bytes left circularly.
The bytes that fall off to the left will be added to the right."
         2)

    (SRC miscellaneous "shift right AX circularly" 6
         "Shift AX, M bytes right circularly.
The bytes that fall off to the right will be added to the left."
         2)

    (MOVE miscellaneous "move" 7 number
          "Move MOD words from M to the location stored in rI1."
          (+ 1 (* 2 number)))

    (NOP miscellaneous "no operation" 0 ignored
         "No operation, M and F are not used by the machine."
         1)

    (HLT miscellaneous "halt" 5
         "Halt.
Stop instruction fetching."
         1)

    (IN input-output "input" 36 unit
        "Transfer a block of words from the specified unit to memory.
The transfer starts at address M."
        1)

    (OUT input-output "output" 37 unit
         "Transfer a block of words from memory.
The transfer starts at address M to the specified unit."
         1)

    (IOC input-output "input-output control" 35 unit
         "Perform a control operation.
The control operation is given by M on the specified unit."
         1)

    (JRED input-output "jump ready" 38 unit
          "Jump to M if the specified unit is ready."
          1)

    (JBUS input-output "jump busy" 34 unit
          "Jump to M if the specified unit is busy."
          1)

    (NUM conversion "convert to numeric" 5
         "Convert rAX to its numerical value and store it in rA.
the register rAX is assumed to contain a character representation of
a number."
         10)

    (CHAR conversion "convert to characters" 5
          "Convert the number stored in rA to a character representation.
The converted character representation is stored in rAX."
          10))

  "Alist that contains all the possible operation codes for mix.
Each elt has the form
  (OP-CODE GROUP FULL-NAME C-BYTE F-BYTE DESCRIPTION EXECUTION-TIME)
Where OP-CODE is the text of the opcode as a symbol,
FULL-NAME is the human readable name as a string,
C-BYTE is the operation code telling what operation is to be performed,
F-BYTE holds a modification of the operation code which can be a symbol
  or a number,
DESCRIPTION contains an string with a description about the operation code and
EXECUTION-TIME holds info about the time it takes, number or string.")
;; (makunbound 'mixal-operation-codes-alist)


;;; Font-locking:
(defconst mixal-syntax-propertize-function
  (syntax-propertize-rules
   ;; Normal comments start with a * in column 0 and end at end of line.
   ("^\\*" (0 "<"))
   ;; Every line can end with a comment which is placed after the operand.
   ;; I assume here that mnemonics without operands can not have a comment.
   ("^[[:alnum:]]*[ \t]+[[:alnum:]]+[ \t]+[^ \n\t]+[ \t]*\\([ \t]\\)[^\n \t]"
    (1 "<"))))

(defvar mixal-font-lock-keywords
  `(("^\\([A-Z0-9a-z]+\\)"
     (1 mixal-font-lock-label-face))
    (,(regexp-opt (mapcar (lambda (x) (symbol-name (car x)))
                          mixal-operation-codes-alist) 'words)
     . mixal-font-lock-operation-code-face)
    (,(regexp-opt mixal-assembly-pseudoinstructions 'words)
     . mixal-font-lock-assembly-pseudoinstruction-face)
    ("^[A-Z0-9a-z]*[ \t]+[A-ZO-9a-z]+[ \t]+\\(=.*=\\)"
     (1 font-lock-constant-face)))
  "Keyword highlighting specification for `mixal-mode'.")
;; (makunbound 'mixal-font-lock-keywords)

(defvar mixal-describe-operation-code-history nil
  "History list for describe operation code.")

(defun mixal-describe-operation-code (op-code)
  "Display the full documentation of OP-CODE."
  (interactive
   (list
    (let* ((completion-ignore-case t)
	   ;; we already have a list, but it is not in the right format
	   ;; transform it to a valid table so completion can use it
	   (table (mapcar (lambda (elm) (cons (symbol-name (car elm)) nil))
			  mixal-operation-codes-alist))
	   ;; prompt is different depending on we are close to a valid op-code
	   (have-default (assq (intern-soft (current-word))
                               mixal-operation-codes-alist))
	   (prompt (concat "Describe operation code "
			   (if have-default
			       (concat "(default " (current-word) "): ")
			     ": "))))
      ;; As the operation code to the user.
      (completing-read prompt table nil t nil
                       'mixal-describe-operation-code-history
                       (current-word)))))
  ;; get the info on the op-code and output it to the help buffer
  (let ((op-code-help (assq (intern-soft op-code) mixal-operation-codes-alist)))
    (when op-code-help
      (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
	(princ op-code) (princ " is an mix operation code\n\n")
	(princ (nth 5 op-code-help)) (terpri) (terpri)
	(princ "      group: ") (princ (nth 1 op-code-help)) (terpri)
	(princ "  nice name: ") (princ (nth 2 op-code-help)) (terpri)
	(princ " OPCODE / C: ") (princ (nth 3 op-code-help)) (terpri)
	(princ "    MOD / F: ") (princ (nth 4 op-code-help)) (terpri)
	(princ "       time: ") (princ (nth 6 op-code-help)) (terpri)))))

;;;; Running
(defun mixal-run ()
  "Run mixal file in current buffer, assumes that file has been compiled."
  (interactive)
  (if (fboundp 'mixvm)
      (mixvm (concat "mixvm -r -t -d "
		     (file-name-sans-extension (buffer-file-name))))
    (error "mixvm.el needs to be loaded to run `mixvm'")))

(defun mixal-debug ()
  "Start mixvm for debugging.
Assumes that file has been compiled with debugging support."
  (interactive)
  (if (fboundp 'mixvm)
      (mixvm (concat "mixvm "
		     (file-name-sans-extension (buffer-file-name))))
    (error "mixvm.el needs to be loaded to run `mixvm'")))

;;;###autoload
(define-derived-mode mixal-mode prog-mode "mixal"
  "Major mode for the mixal asm language."
  (set (make-local-variable 'comment-start) "*")
  (set (make-local-variable 'comment-start-skip) "^\\*[ \t]*")
  (set (make-local-variable 'font-lock-defaults)
       `(mixal-font-lock-keywords))
  (set (make-local-variable 'syntax-propertize-function)
       mixal-syntax-propertize-function)
  ;; might add an indent function in the future
  ;;  (set (make-local-variable 'indent-line-function) 'mixal-indent-line)
  (set (make-local-variable 'compile-command) (concat "mixasm "
						      buffer-file-name)))

(provide 'mixal-mode)

;;; mixal-mode.el ends here
