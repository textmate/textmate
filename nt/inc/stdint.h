/* Replacement stdint.h file for building GNU Emacs on Windows.

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

#ifndef _NT_STDINT_H_
#define _NT_STDINT_H_

#ifdef __GNUC__
# include_next <stdint.h> /* use stdint.h if available */
#else	/* !__GNUC__ */

/* Minimum definitions to allow compilation with tool chains where
   stdint.h is not available, e.g. Microsoft Visual Studio.  */

#ifdef _WIN64
typedef __int64 intptr_t;
typedef unsigned int uint32_t;
typedef unsigned __int64 uint64_t;
#define UINT64_MAX (18446744073709551615i64)
#define UINT64_MIN 0
/* "i64" is the non-standard suffix used by MSVC for 64-bit constants.  */
#define INT64_MAX 9223372036854775807i64
#define INT64_MIN (~INT64_MAX)
#define INTPTR_MAX INT64_MAX
#define UINTMAX_MAX UINT64_MAX
#define UINTMAX_MIN UINT64_MIN
#define INTMAX_MAX INT64_MAX
#define INTMAX_MIN INT64_MIN
#define uintmax_t unsigned __int64
#define intmax_t __int64
#else
typedef int intptr_t;
typedef unsigned int uint32_t;
#define UINT32_MAX 4294967295
#define UINT32_MIN 0
#define INT32_MAX 2147483647
#define INT32_MIN (~INT32_MAX)
#define INTPTR_MAX INT32_MAX
#define UINTMAX_MAX UINT32_MAX
#define UINTMAX_MIN UINT32_MIN
#define INTMAX_MAX INT32_MAX
#define INTMAX_MIN INT32_MIN
#define uintmax_t unsigned long
#define intmax_t long
#endif

#define PTRDIFF_MAX INTPTR_MAX

#endif	/* !__GNUC__ */

#endif /* _NT_STDINT_H_ */
