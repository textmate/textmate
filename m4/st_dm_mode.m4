# serial 6

# Copyright (C) 1998-1999, 2001, 2009-2011 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Define HAVE_ST_DM_MODE if struct stat has an st_dm_mode member.

AC_DEFUN([AC_STRUCT_ST_DM_MODE],
 [AC_CACHE_CHECK([for st_dm_mode in struct stat], [ac_cv_struct_st_dm_mode],
   [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <sys/types.h>
#include <sys/stat.h>]], [[struct stat s; s.st_dm_mode;]])],
     [ac_cv_struct_st_dm_mode=yes],
     [ac_cv_struct_st_dm_mode=no])])

  if test $ac_cv_struct_st_dm_mode = yes; then
    AC_DEFINE([HAVE_ST_DM_MODE], [1],
              [Define if struct stat has an st_dm_mode member. ])
  fi
 ]
)
