/* Replacement inntypes.h file for building GNU Emacs on Windows with MSVC.

Copyright (C) 2011-2012  Free Software Foundation, Inc.

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

#ifndef _REPL_INTTYPES_H
#define _REPL_INTTYPES_H

#ifdef __MINGW32__
#include_next <inttypes.h>
#else  /* !__MINGW32__ */
#include "stdint.h"
#ifdef _WIN64
#define strtoumax _strtoui64
#define strtoimax _strtoi64
#else
#define strtoumax strtoul
#define strtoimax strtol
#endif
#endif	/* !__MINGW32__ */

#endif
