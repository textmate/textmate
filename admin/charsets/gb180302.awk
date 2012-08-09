# Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

BEGIN {
  tohex["A"] = 10;
  tohex["B"] = 11;
  tohex["C"] = 12;
  tohex["D"] = 13;
  tohex["E"] = 14;
  tohex["F"] = 15;
  tohex["a"] = 10;
  tohex["b"] = 11;
  tohex["c"] = 12;
  tohex["d"] = 13;
  tohex["e"] = 14;
  tohex["f"] = 15;
  from_gb = 0;
  to_gb = -1;
  to_unicode = 0;
  from_unicode = 0;
}

function decode_hex(str) {
  n = 0;
  len = length(str);
  for (i = 1; i <= len; i++)
    {
      c = substr (str, i, 1);
      if (c >= "0" && c <= "9")
	n = n * 16 + (c - "0");
      else
	n = n * 16 + tohex[c];
    }
  return n;
}

function gb_to_index(gb) {
  b0 = int(gb / 256);
  b1 = gb % 256;
  idx = (((b0 - 129)) * 191 + b1 - 64); 
#  if (b1 >= 128)
#    idx--;
  return idx
}

function index_to_gb(idx) {
  b0 = int(idx / 191) + 129;
  b1 = (idx % 191) + 64;
#  if (b1 >= 127)
#    b1++;
  return (b0 * 256 + b1);
}

/^\#/ {
  print;
  next;
}

{
  gb = gb_to_index(decode_hex(substr($1, 3, 4)));
  unicode = decode_hex(substr($2, 3, 4));
  if ((gb == to_gb + 1) && (unicode == to_unicode + 1))
    {
      to_gb++;
      to_unicode++;
    }
  else
    {
      if (from_gb == to_gb)
	printf "0x%04X 0x%04X\n", index_to_gb(from_gb), from_unicode;
      else if (from_gb < to_gb)
	printf "0x%04X-0x%04X 0x%04X\n",
	  index_to_gb(from_gb), index_to_gb(to_gb), from_unicode;
      from_gb = to_gb = gb;
      from_unicode = to_unicode = unicode;
    }
}

END {
  if (from_gb <= to_gb)
    printf "0x%04X-0x%04X 0x%04X\n",
      index_to_gb(from_gb), index_to_gb(to_gb), from_unicode;
}

