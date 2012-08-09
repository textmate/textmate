# Makefile -- Makefile to generate charset maps in etc/charsets.
# Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
#   National Institute of Advanced Industrial Science and Technology (AIST)
#   Registration Number H13PRO009
#
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

#  If your system doesn't have the directory /usr/share/i18n/charmaps,
#  get the source of the latest glibc, gzip all the charmap files in
#  the directory "localedate/charmaps", and set the variable
#  GLIBC_CHARMAPS to that directory.

GLIBC_CHARMAPS=/usr/share/i18n/charmaps

CHARSETS = ${ISO8859} ${IBM} ${CODEPAGE} ${CJK} ${MISC} ${MULE}

# Note: We can not prepend "ISO-" to these map files because of file
# name limits on DOS.
ISO8859 = \
	8859-2.map 8859-3.map 8859-4.map 8859-5.map 8859-6.map 8859-7.map \
	8859-8.map 8859-9.map 8859-10.map 8859-11.map 8859-13.map 8859-14.map \
	8859-15.map 8859-16.map

IBM = \
	IBM037.map IBM038.map \
	IBM256.map IBM273.map IBM274.map IBM275.map IBM277.map IBM278.map \
	IBM280.map IBM281.map IBM284.map IBM285.map IBM290.map IBM297.map \
	IBM420.map IBM423.map IBM424.map IBM437.map IBM500.map IBM850.map \
	IBM851.map IBM852.map IBM855.map IBM856.map IBM857.map IBM860.map \
	IBM861.map IBM862.map IBM863.map IBM864.map IBM865.map IBM866.map \
	IBM868.map IBM869.map IBM870.map IBM871.map IBM874.map IBM875.map \
	IBM880.map IBM891.map IBM903.map IBM904.map IBM905.map IBM918.map \
	IBM1004.map IBM1026.map IBM1047.map

CODEPAGE = \
	CP737.map CP775.map CP1125.map\
	CP1250.map CP1251.map CP1252.map CP1253.map CP1254.map \
	CP1255.map CP1256.map CP1257.map CP1258.map \
	CP10007.map \
	CP720.map CP858.map

CJK =   GB2312.map GBK.map GB180302.map GB180304.map \
	BIG5.map BIG5-HKSCS.map\
	CNS-1.map CNS-2.map CNS-3.map CNS-4.map CNS-5.map CNS-6.map CNS-7.map \
	CNS-F.map \
	JISX0201.map JISX0208.map JISX0212.map JISX2131.map JISX2132.map \
	JISC6226.map CP932-2BYTE.map JISX213A.map\
	KSC5601.map KSC5636.map JOHAB.map

MISC =  KOI-8.map KOI8-R.map KOI8-U.map KOI8-T.map ALTERNATIVNYJ.map \
	MIK.map PTCP154.map \
	TIS-620.map VISCII.map VSCII.map VSCII-2.map\
	KA-PS.map KA-ACADEMY.map \
	HP-ROMAN8.map NEXTSTEP.map MACINTOSH.map EBCDICUK.map EBCDICUS.map \
	stdenc.map symbol.map \
	CP949-2BYTE.map \
	BIG5-1.map BIG5-2.map

# Emacs-mule charsets.
MULE =	MULE-ethiopic.map MULE-ipa.map MULE-is13194.map \
	MULE-sisheng.map MULE-tibetan.map \
	MULE-lviscii.map MULE-uviscii.map

TRANS_TABLE = cp51932.el eucjp-ms.el

all: ${CHARSETS} ${TRANS_TABLE}

AWK = gawk

# Rules for each charset

VSCII.map: ${GLIBC_CHARMAPS}/TCVN5712-1.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[0-9a-f].[ 	]/' GLIBC-1 compact.awk > $@

VSCII-2.map: ${GLIBC_CHARMAPS}/TCVN5712-1.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[2-7a-f].[ 	]/' GLIBC-1 compact.awk \
	  | sed 's/0x20-0x7F.*/0x00-0x7F 0x0000/' > $@

ALTERNATIVNYJ.map: IBM866.map
	# Generating $@...
	@echo "# Modified from IBM866.map according to the chart at" > $@
	@echo "# http://www.cyrillic.com/ref/cyrillic/koi-8alt.html," >> $@
	@echo "# with guesses for the Unicodes of the glyphs." >> $@
	@sed -e '1 d' \
	     -e '/0xF2/ s/ .*/ 0x2019/' \
	     -e '/0xF3/ s/ .*/ 0x2018/' \
	     -e '/0xF4/ s/ .*/ 0x0301/' \
	     -e '/0xF5/ s/ .*/ 0x0300/' \
	     -e '/0xF6/ s/ .*/ 0x203A/' \
	     -e '/0xF7/ s/ .*/ 0x2039/' \
	     -e '/0xF8/ s/ .*/ 0x2191/' \
	     -e '/0xF9/ s/ .*/ 0x2193/' \
	     -e '/0xFA/ s/ .*/ 0x00B1/' \
	     -e '/0xFB/ s/ .*/ 0x00F7/' < $< >> $@

MIK.map: mapfiles/bulgarian-mik.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '1,$$' CZYBORRA compact.awk > $@

PTCP154.map: mapfiles/PTCP154.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^0x/' IANA compact.awk > $@

stdenc.map: mapfiles/stdenc.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^[0-9A-Fa-f]/' UNICODE compact.awk > $@

symbol.map: mapfiles/symbol.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^[0-9A-Fa-f]/' UNICODE compact.awk > $@

CP720.map: mapfiles/CP720.map.gz
	# Generating $@...
	@zcat $< > $@

CP858.map: mapfiles/CP858.map.gz
	# Generating $@...
	@zcat $< > $@

CP949-2BYTE.map: ${GLIBC_CHARMAPS}/CP949.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[89a-f]/' GLIBC-2 compact.awk > $@

GB2312.map: ${GLIBC_CHARMAPS}/GB2312.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[a-f]/' GLIBC-2-7 compact.awk > $@

GBK.map: ${GLIBC_CHARMAPS}/GBK.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[89a-f]/' GLIBC-2 compact.awk > $@

GB180302.map: ${GLIBC_CHARMAPS}/GB18030.gz mapconv gb180302.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x..\/x..[ 	]/' GLIBC-2 gb180302.awk > $@

GB180304.map: GB180302.map gb180304.awk
	# Generating $@...
	@$(AWK) -f gb180304.awk < $< > $@

JISX0201.map: ${GLIBC_CHARMAPS}/JIS_X0201.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[0-9]/' GLIBC-1 compact.awk > $@
	@echo "# Generated by hand" >> $@
	@echo "0xA1-0xDF 0xFF61" >> $@

JISX0208.map: ${GLIBC_CHARMAPS}/EUC-JP.gz mapconv
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[a-f]/' GLIBC-2-7 \
	  | sed 's/0x2015/0x2014/' > $@

JISX0212.map: ${GLIBC_CHARMAPS}/EUC-JP.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x8f/ s,/x8f,,' GLIBC-2-7 compact.awk > $@

JISX2131.map: ${GLIBC_CHARMAPS}/EUC-JISX0213.gz mapconv
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[a-f]/' GLIBC-2-7 \
	  | sed -e 's/0x2015/0x2014/' -e 's/0x2299/0x29BF/' > $@

JISX2132.map: ${GLIBC_CHARMAPS}/EUC-JISX0213.gz mapconv
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x8f/ s,/x8f,,' GLIBC-2-7 > $@

JISX213A.map: mapfiles/JISX213A.map.gz
	# Generating $@
	@zcat $< > $@

CP932-2BYTE.map: mapfiles/CP932.TXT.gz mapconv cp932.awk
	# Generating $@...
	@mapconv $< '/^0x[89A-F][0-9A-F][0-9A-F]/' UNICODE2 cp932.awk > $@

cp51932.el: CP932-2BYTE.map cp51932.awk
	@$(AWK) -f cp51932.awk < CP932-2BYTE.map > $@

eucjp-ms.el: ${GLIBC_CHARMAPS}/EUC-JP-MS.gz eucjp-ms.awk
	@zcat $< | $(AWK) -f eucjp-ms.awk > $@

JISC6226.map : mapfiles/Uni2JIS.gz mapconv kuten.awk
	# Generating $@...
	@mapconv $< '/^[^#].*0-/' YASUOKA kuten.awk > $@

KSC5601.map: ${GLIBC_CHARMAPS}/EUC-KR.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[a-f]/' GLIBC-2-7 compact.awk > $@

BIG5.map: ${GLIBC_CHARMAPS}/BIG5.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[a-f]/' GLIBC-2 > $@

BIG5-1.map: BIG5.map mapconv big5.awk
	# Generating $@...
	@echo "# Generated from $<" > $@
	@sed -n -e '/0xa140/,/0xc8fe/p' < $< | gawk -f big5.awk >> $@

BIG5-2.map: BIG5.map mapconv big5.awk
	# Generating $@...
	@echo "# Generated from $<" > $@
	@sed -n -e '/0xc940/,$$ p' < $< | gawk -f big5.awk >> $@

BIG5-HKSCS.map: ${GLIBC_CHARMAPS}/BIG5-HKSCS.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[89a-f].\//' GLIBC-2 compact.awk > $@

JOHAB.map: ${GLIBC_CHARMAPS}/JOHAB.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[89a-f]/' GLIBC-2 compact.awk > $@

CNS-1.map: ${GLIBC_CHARMAPS}/EUC-TW.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x[a-f]/' GLIBC-2-7 compact.awk > $@

# CNS-1.map: mapfiles/cns2ucsdkw.txt mapconv compact.awk
# 	# Generating $@...
# 	@mapconv $< '/^C1/' KANJI-DATABASE compact.awk > $@

CNS-2.map: mapfiles/cns2ucsdkw.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^C2/' KANJI-DATABASE compact.awk > $@

CNS-3.map: mapfiles/cns2ucsdkw.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^C3/' KANJI-DATABASE compact.awk > $@

CNS-4.map: mapfiles/cns2ucsdkw.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^C4/' KANJI-DATABASE compact.awk > $@

CNS-5.map: mapfiles/cns2ucsdkw.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^C5/' KANJI-DATABASE compact.awk > $@

CNS-6.map: mapfiles/cns2ucsdkw.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^C6/' KANJI-DATABASE compact.awk > $@

CNS-7.map: mapfiles/cns2ucsdkw.txt.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^C7/' KANJI-DATABASE compact.awk > $@

CNS-F.map: ${GLIBC_CHARMAPS}/EUC-TW.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*\/x8e\/xaf/ s,/x8e/xaf,,' GLIBC-2-7 compact.awk > $@

# General target to produce map files for mule charsets.
MULE-%.map: mapfiles/MULE-%.map.gz
	# Generating $@...
	@zcat $< > $@

# General target to produce map files for ISO-8859, GEORGIAN, and
# EBCDIC charsets.  We can not use the original file name because of
# file name limit on DOS.  "KA" is ISO 639 language code for Georgian.

8859-%.map: ${GLIBC_CHARMAPS}/ISO-8859-%.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x/' GLIBC-1 compact.awk > $@

KA-%.map: ${GLIBC_CHARMAPS}/GEORGIAN-%.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x/' GLIBC-1 compact.awk > $@

EBCDIC%.map: ${GLIBC_CHARMAPS}/EBCDIC-%.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x/' GLIBC-1 compact.awk > $@

# General target to produce map files for single-byte charsets.

%.map: ${GLIBC_CHARMAPS}/%.gz mapconv compact.awk
	# Generating $@...
	@mapconv $< '/^<.*[ 	]\/x/' GLIBC-1 compact.awk > $@

install:
	@for f in ${CHARSETS}; do \
	  if test -r $$f; then \
	    if ! cmp -s $$f ../../etc/charsets/$$f; then \
	      echo updating $$f; \
	      cp $$f ../../etc/charsets; \
	    fi; \
	  fi; \
	done
	@for f in ${TRANS_TABLE}; do \
	  if test -r $$f; then \
	    if ! cmp -s $$f ../../lisp/international/$$f; then \
	      echo updating $$f; \
	      cp $$f ../../lisp/international; \
	    fi; \
	  fi; \
	done

# Clear files that are automatically generated.
clean:
	rm -f ${CHARSETS} ${TRANS_TABLE}

