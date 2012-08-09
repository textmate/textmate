/*
 * sys\file.h doesn't exist on NT - only needed for these constants
 */

#ifndef D_OK
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
#define D_OK 8
#endif

