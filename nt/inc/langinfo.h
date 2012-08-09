/* Replacement langinfo.h file for building GNU Emacs on Windows.

Copyright (C) 2006-2012  Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef _LANGINFO_H
#define _LANGINFO_H

#include <nl_types.h>

enum {
  CODESET,
  DAY_1, DAY_2, DAY_3, DAY_4, DAY_5, DAY_6, DAY_7,
  MON_1, MON_2, MON_3, MON_4, MON_5, MON_6, MON_7, MON_8, MON_9, MON_10,
  MON_11, MON_12,

  /* Number of enumerated values.  */
  _NL_NUM
};

#define CODESET CODESET

#define DAY_1	DAY_1
#define DAY_2	DAY_2
#define DAY_3	DAY_3
#define DAY_4	DAY_4
#define DAY_5	DAY_5
#define DAY_6	DAY_6
#define DAY_7	DAY_7

#define MON_1	MON_1
#define MON_2	MON_2
#define MON_3	MON_3
#define MON_4	MON_4
#define MON_5	MON_5
#define MON_6	MON_6
#define MON_7	MON_7
#define MON_8	MON_8
#define MON_9	MON_9
#define MON_10	MON_10
#define MON_11	MON_11
#define MON_12	MON_12

extern char *nl_langinfo (nl_item);

#endif /* _LANGINFO_H */

