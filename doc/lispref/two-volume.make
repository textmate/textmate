# Copyright (C) 2007-2012 Free Software Foundation, Inc.
# See end for copying conditions.

# although it would be nice to use tex rather than pdftex to avoid
# colors, spurious warnings about names being referenced but not
# existing, etc., dvips | ps2pdf doesn't preserve the page size.
# Instead of creating a special dvips config file, put up with the warnings.
texinfodir=../misc
emacsdir=../emacs

tex = TEXINPUTS=".:$(texinfodir):${emacsdir}:${TEXINPUTS}" pdftex -interaction=nonstopmode

all: vol1.pdf vol2.pdf

# vol1.texi and vol2.texi specially define \tocreadfilename so we can
# use our premade .toc's.
# 
vol1.pdf: elisp1med-fns-ready elisp1med-aux-ready elisp1med-toc-ready
	@echo -e "\f Final TeX run for volume 1..."
	cp elisp1med-toc-ready elisp1-toc-ready.toc
	cp elisp1med-fns-ready vol1.fns
	cp elisp1med-aux-ready vol1.aux
	$(tex) vol1.texi
#
vol2.pdf: elisp2med-fns-ready elisp2med-aux-ready elisp2med-toc-ready
	@echo "Final TeX run for volume 2..."
	cp elisp2med-toc-ready elisp2-toc-ready.toc
	cp elisp2med-fns-ready vol2.fns
	cp elisp2med-aux-ready vol2.aux
	$(tex) vol2.texi

#  intermediate toc files.
# 
# vol1 toc: volume 1, page break, volume 2 (with II: prepended).
elisp1med-toc-ready: elisp1med-init elisp2med-init
	echo '@unnchapentry{@b{Volume 1}}{10001}{vol1}{}' >$@
	cat elisp1med-toc >>$@
	echo '@page' >>$@
	echo '@unnchapentry{@b{Volume 2}}{10001}{vol2}{}' >>$@
	sed 's/{\([^}]*\)}$$/{II:\1}/' elisp2med-toc >>$@	
#
# vol2 toc: volume 1 (with I: prepended), page break, volume 2.
elisp2med-toc-ready: elisp1med-init elisp2med-init
	echo '@unnchapentry{@b{Volume 1}}{10001}{vol1}{}' >$@
	sed 's/{\([^}]*\)}$$/{I:\1}/' elisp1med-toc >>$@	
	echo '@page' >>$@
	echo '@unnchapentry{@b{Volume 2}}{10001}{vol2}{}' >>$@
	cat elisp2med-toc >>$@


#  intermediate aux files.
# 
# append vol2's fixed aux to normal vol1.  
elisp1med-aux-ready: elisp2med-aux-vol-added
	cat elisp1med-aux $< >$@
#
# prepend vol1's fixed aux to vol2.
elisp2med-aux-ready: elisp1med-aux-vol-added
	cat $< elisp2med-aux >$@

# on -pg entries, append volume number after page number.
elisp1med-aux-vol-added: elisp1med-init
	sed 's/-pg}{\(.*\)}$$/-pg}{\1, vol.@tie1}/' elisp1med-aux >$@
#
elisp2med-aux-vol-added: elisp2med-init
	sed 's/-pg}{\(.*\)}$$/-pg}{\1, vol.@tie2}/' elisp2med-aux >$@

#  intermediate index (fns) file.
# 
elisp1med-fns-ready: elisp1med-fn-vol-added elisp2med-fn-vol-added
	cat elisp2med-fn-vol-added >>vol1.fn
	texindex vol1.fn
	cp vol1.fns $@
#
elisp2med-fns-ready: elisp1med-fn-vol-added elisp2med-fn-vol-added
	cat elisp1med-fn-vol-added >>vol2.fn
	texindex vol2.fn
	cp vol2.fns $@

# Insert volume number (I: or II:) into index file.
elisp1med-fn-vol-added: elisp1med-init
	cp vol1.fn elisp1med-fn
	sed 's/}{/}{I:/' elisp1med-fn >$@
#
elisp2med-fn-vol-added: elisp2med-init
	cp vol2.fn elisp2med-fn
	sed 's/}{/}{II:/' elisp2med-fn >$@

# -----------------------------------------------------------------------------
# everything above is essentially a duplicate of everything below. sorry.
# -----------------------------------------------------------------------------

#  intermediate TeX runs.
# 
# this generates what would be the final versions -- except the page
# numbers aren't right.  The process of adding the I: and II: changes
# the page breaks, so a few index entries, at least are wrong.  (In
# 2007, x-meta-keysym in vol.II ended up on page 374 when the index had
# it on page 375 from the initial run.)
# 
# So, we start all over again, from these fns/aux/toc files.
# 
elisp1med-init: elisp1-fns-ready elisp1-aux-ready elisp1init-toc-ready $(texinfodir)/texinfo.tex
	@echo -e "\f Intermediate TeX run for volume 1..."
	cp elisp1init-toc-ready elisp1-toc-ready.toc
	cp elisp1-fns-ready vol1.fns
	cp elisp1-aux-ready vol1.aux
	$(tex) vol1.texi
	texindex vol1.??
	mv vol1.aux elisp1med-aux
	mv vol1.toc elisp1med-toc
#
elisp2med-init: elisp2-fns-ready elisp2-aux-ready elisp2init-toc-ready $(texinfodir)/texinfo.tex
	@echo "Final TeX run for volume 2..."
	cp elisp2init-toc-ready elisp2-toc-ready.toc
	cp elisp2-fns-ready vol2.fns
	cp elisp2-aux-ready vol2.aux
	$(tex) vol2.texi
	texindex vol2.??
	mv vol2.aux elisp2med-aux
	mv vol2.toc elisp2med-toc


#  initial toc files.
# 
# vol1 toc: volume 1, page break, volume 2 (with II: prepended).
elisp1init-toc-ready: elisp1-init elisp2-init
	echo '@unnchapentry{@b{Volume 1}}{10001}{vol1}{}' >$@
	cat elisp1-toc >>$@
	echo '@page' >>$@
	echo '@unnchapentry{@b{Volume 2}}{10001}{vol2}{}' >>$@
	sed 's/{\([^}]*\)}$$/{II:\1}/' elisp2-toc >>$@	
#
# vol2 toc: volume 1 (with I: prepended), page break, volume 2.
elisp2init-toc-ready: elisp1-init elisp2-init
	echo '@unnchapentry{@b{Volume 1}}{10001}{vol1}{}' >$@
	sed 's/{\([^}]*\)}$$/{I:\1}/' elisp1-toc >>$@	
	echo '@page' >>$@
	echo '@unnchapentry{@b{Volume 2}}{10001}{vol2}{}' >>$@
	cat elisp2-toc >>$@


#  initial aux files.
# 
# append vol2's fixed aux to normal vol1.  The initial runs saved
# elisp1-aux and elisp2-aux.
elisp1-aux-ready: elisp2-aux-vol-added
	cat elisp1-aux $< >$@
#
# prepend vol1's fixed aux to vol2.
elisp2-aux-ready: elisp1-aux-vol-added
	cat $< elisp2-aux >$@

# on -pg entries, append volume number after page number.
elisp1-aux-vol-added: elisp1-init
	sed 's/-pg}{\(.*\)}$$/-pg}{\1, vol.@tie1}/' elisp1-aux >$@
#
elisp2-aux-vol-added: elisp2-init
	sed 's/-pg}{\(.*\)}$$/-pg}{\1, vol.@tie2}/' elisp2-aux >$@

#  initial index (fns) file.
# 
# Append other volume's index entries to this one's.
# Index entries in this volume will then take precedence.
elisp1-fns-ready: elisp1-fn-vol-added elisp2-fn-vol-added
	cat elisp2-fn-vol-added >>vol1.fn
	texindex vol1.fn
	cp vol1.fns $@
#
elisp2-fns-ready: elisp1-fn-vol-added elisp2-fn-vol-added
	cat elisp1-fn-vol-added >>vol2.fn
	texindex vol2.fn
	cp vol2.fns $@

# Insert volume number (I: or II:) into index file.
elisp1-fn-vol-added: elisp1-init
	cp vol1.fn elisp1-fn
	sed 's/}{/}{I:/' elisp1-fn >$@
#
elisp2-fn-vol-added: elisp2-init
	cp vol2.fn elisp2-fn
	sed 's/}{/}{II:/' elisp2-fn >$@


#  initial TeX runs.
# 
# We use the .fn, .aux, and .toc files created here in subsequent
# processing.  The page numbers generated here will not be correct yet,
# but we run texindex and TeX a second time just to get them closer.
# Otherwise it might take even longer for them to converge.
# 
elisp1-init: vol1.texi
	@echo -e "\f Initial TeX run for volume 1..."
	rm -f vol1.aux vol1.toc
	$(tex) $<
	texindex vol1.??
	mv vol1.aux elisp1-aux
	mv vol1.toc elisp1-toc
	touch $@
#
elisp2-init: vol2.texi
	@echo "Initial TeX run for volume 2..."
	rm -f vol2.aux vol2.toc
	$(tex) $<
	texindex vol2.??
	mv vol2.aux elisp2-aux
	mv vol2.toc elisp2-toc
	touch $@

# COPYING CONDITIONS
# 
# This file is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <http://www.gnu.org/licenses/>.
 
