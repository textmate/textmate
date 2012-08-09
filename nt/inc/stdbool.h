#ifndef _NT_STDBOOL_H_
#define _NT_STDBOOL_H_
/*
 * stdbool.h exists in GCC, but not in MSVC.
 */

#ifdef __GNUC__
# include_next <stdbool.h>
#else
# define _Bool signed char
# define bool _Bool
# define false 0
# define true 1
#endif

#endif	/* _NT_STDBOOL_H_ */
