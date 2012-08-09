/^VPATH *=/c\
# This works only in GNU make.  Using the patterns avoids\
# object files being found by VPATH, and thus permits building\
# when $srcdir is configured itself.\
vpath %.c $(srcdir)\
vpath %.h $(srcdir)\
\

