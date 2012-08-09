# eucjp-ms.awk -- Generate a translation table for eucJP-ms.
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

# eucJP-ms is one of eucJP-open encoding defined at this page:
#  http://home.m05.itscom.net/numa/cde/ucs-conv/appendix.html
# This program reads the mapping file EUC-JP-MS (of glibc) and
# generates the Elisp file eucjp-ms.el that defines two translation
# tables `eucjp-ms-decode' and `eucjp-ms-encode'.

BEGIN {
  FS = "[ \t][ \t]*"

  # STATE: 0/ignore, 1/JISX0208, 2/JISX0208 target range
  #        3/JISX0212 4/JISX0212 target range
  state = 0;

  JISX0208_FROM1 = "/xad/xa1";
  JISX0208_TO1 = "/xad/xfc";
  JISX0208_FROM2 = "/xf5/xa1";
  JISX0212_FROM = "/x8f/xf3/xf3";

  print ";;; eucjp-ms.el -- translation table for eucJP-ms. -*- no-byte-compile: t -*-";
  print ";;; Automatically generated from /usr/share/i18n/charmaps/EUC-JP-MS.gz";
  print "(let ((map";
  print "       '(;JISEXT<->UNICODE";
}

function write_entry (unicode) {
    if (state == 1) {
	if ($2 == JISX0208_FROM1 || $2 == JISX0208_FROM2)
	    state = 2;
    } else if (state == 3) {
	if ($2 == JISX0212_FROM)
	    state = 4;
    }
    if (state == 2) {
	jis = $2
	gsub("/x", "", jis);
	printf "\n	 (#x%s . #x%s)", jis, unicode;
	if ($2 == JISX0208_TO1)
	    state = 1;
    } else if (state == 4) {
	jis = substr($2, 5, 8);
	gsub("/x", "", jis);
	printf "\n	 (#x%s #x%s)", jis, unicode;
    }
}


/^% JIS X 0208/ {
    state = 1;
    next;
}

/^% JIS X 0212/ {
    state = 3;
    next;
}

/^END CHARMAP/ {
    state = 0;
    next;
}

/^<U[0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z]>/ {
    if (state > 0)
	write_entry(substr($1, 3, 4));
}

/^%IRREVERSIBLE%<U[0-9A-Z][0-9A-Z][0-9A-Z][0-9A-Z]>/ {
    if (state > 0)
	write_entry(substr($1, 17, 4));
}

END {
  print ")))";
  print "  (mapc #'(lambda (x)";
  print "	    (let ((code (logand (car x) #x7F7F)))";
  print "	      (if (integerp (cdr x))";
  print "		  (setcar x (decode-char 'japanese-jisx0208 code))";
  print "		(setcar x (decode-char 'japanese-jisx0212 code))";
  print "		(setcdr x (cadr x)))))";
  print "	map)";
  print "  (define-translation-table 'eucjp-ms-decode map)";
  print "  (mapc #'(lambda (x)";
  print "	    (let ((tmp (car x)))";
  print "	      (setcar x (cdr x)) (setcdr x tmp)))";
  print "	map)";
  print "  (define-translation-table 'eucjp-ms-encode map))";
}

