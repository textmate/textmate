# cp932.awk -- Add sort keys and append user defined area to CP932-2BYTE.map.
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

# Add a sort key 0, 1, 2, or 3 at the tail of each line as a comment
# to realize the round trip mapping to Unicode works as described in
# this page:
#	http://support.microsoft.com/default.aspx?scid=kb;EN-US;170559
# Each sort key means as below:
#   0: JISX0208 characters.
#   1: NEC special characters.
#   2: IBM extension characters.
#   3: NEC selection of IBM extension characters.
#   4: user defined area

BEGIN {
  tohex["A"] = 10;
  tohex["B"] = 11;
  tohex["C"] = 12;
  tohex["D"] = 13;
  tohex["E"] = 14;
  tohex["F"] = 15;
}

function decode_hex(str) {
  n = 0;
  len = length(str);
  for (i = 1; i <= len; i++)
    {
      c = substr(str, i, 1);
      if (c >= "0" && c <= "9")
	n = n * 16 + (c - "0");
      else
	n = n * 16 + tohex[c];
    }
  return n;
}

function sjis_to_jis_ku(code)
{
  s1 = int(code / 256);
  s2 = code % 256;
  if (s2 >= 159)		# s2 >= 0x9F
    {
      if (s1 >= 224)		# s1 >= 0xE0
	j1 = s1 * 2 - 352;	# j1 = s1 * 2 - 0x160
      else
	j1 = s1 * 2 - 224;	# j1 = s1 * 2 - 0xE0
      j2 = s2 - 126		# j2 = s2 - #x7E
    }
  else
    {
      if (s1 >= 224)
	j1 = s1 * 2 - 353;	# j1 = s1 * 2 - 0x161
      else
	j1 = s1 * 2 - 225;	# j1 = s1 * 2 - 0xE1
      if (s2 >= 127)		# s2 >= #x7F
	j2 = s2 - 32;
      else
	j2 = s2 - 31;
    }
  return j1 - 32;
}

/^0x[89E]/ {
  sjis=decode_hex(substr($1, 3, 4))
  ku=sjis_to_jis_ku(sjis);
  if (ku == 13)
    printf "%s # 1 %02X%02X\n", $0, j1, j2;
  else if (ku >= 89 && ku <= 92)
    printf "%s # 3 %02X%02X\n", $0, j1, j2;
  else
    printf "%s # 0 %02X%02X\n", $0, j1, j2;
  next;
}

/^0xF/ {
  printf "%s # 2\n", $0;
  next;
}

{
  print;
}

END {
  code = 57344;			# 0xE000
  for (i = 240; i < 250; i++)
    {
      for (j = 64; j <= 126; j++)
	printf "0x%02X%02X 0x%04X # 4\n", i, j, code++;
      for (j = 128; j <= 158; j++)
	printf "0x%02X%02X 0x%04X # 4\n", i, j, code++;
      for (; j <= 252; j++)
	printf "0x%02X%02X 0x%04X # 4\n", i, j, code++;
    }
}

