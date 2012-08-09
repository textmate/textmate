/* System file for openbsd.  */

/* Nearly the same as NetBSD.  Note there are differences in configure.  */
#include "netbsd.h"

/* The symbol SIGIO is defined, but the feature doesn't work in the
   way Emacs needs it to.  See
   <http://article.gmane.org/gmane.os.openbsd.ports/46831>.  */
#define BROKEN_SIGIO
