#include "hpux10-20.h"

/* SA_RESTART resets the timeout of `select', so don't use it.  */
#define BROKEN_SA_RESTART

/* It does work on HPUX to open the pty's tty in the parent (Emacs),
   then close and reopen it in the child.  */
#define USG_SUBTTY_WORKS

