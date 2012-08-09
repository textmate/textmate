# cp51932.awk -- Generate a translation table for CP51932.
# Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
#   National Institute of Advanced Industrial Science and Technology (AIST)
#   Registration Number H13PRO009

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

# Commentary:

# Generate a translation table for CP51932 (EUC-JP of MicroSoft Version).
# It maps invalid JISX0208 code points used by CP51932 to Unicode.
# 4th field of the input has these meanings:
#   0: JISX0208 characters.
#   1: NEC special characters.
#   2: IBM extension characters.
#   3: NEC selection of IBM extension characters.
# Among them, 1 and 3 are the target characters.  2 should have
# already been mapped to 1 or 3.

BEGIN {
  print ";;; cp51932.el -- translation table for CP51932. -*- no-byte-compile: t -*-";
  print ";;; Automatically generated from CP932-2BYTE.map";
  print "(let ((map";
  printf "       '(;JISEXT<->UNICODE";
}

/# [13]/ {
  printf "\n	 (#x%s . #x%s)", $5 ,substr($2, 3, 4);
}

END {
  print ")))";
  print "  (mapc #'(lambda (x)";
  print "	    (setcar x (decode-char 'japanese-jisx0208 (car x))))";
  print "	map)";
  print "  (define-translation-table 'cp51932-decode map)";
  print "  (mapc #'(lambda (x)";
  print "	    (let ((tmp (car x)))";
  print "	      (setcar x (cdr x)) (setcdr x tmp)))";
  print "	map)";
  print "  (define-translation-table 'cp51932-encode map))";
}
