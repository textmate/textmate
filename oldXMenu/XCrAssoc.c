/* Copyright    Massachusetts Institute of Technology    1985	*/
#include "copyright.h"


#include <config.h>
#include <X11/Xlib.h>
#include <errno.h>
#include "X10.h"

#ifndef NULL
#define NULL 0
#endif

/*
 * XCreateAssocTable - Create an XAssocTable.  The size argument should be
 * a power of two for efficiency reasons.  Some size suggestions: use 32
 * buckets per 100 objects;  a reasonable maximum number of object per
 * buckets is 8.  If there is an error creating the XAssocTable, a NULL
 * pointer is returned.
 */
XAssocTable *XCreateAssocTable(register int size)
	                  		/* Desired size of the table. */
{
	register XAssocTable *table;	/* XAssocTable to be initialized. */
	register XAssoc *buckets;	/* Pointer to the first bucket in */
					/* the bucket array. */

	/* Malloc the XAssocTable. */
	if ((table = (XAssocTable *)malloc(sizeof(XAssocTable))) == NULL) {
		/* malloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}

	/* calloc the buckets (actually just their headers). */
	buckets = (XAssoc *)calloc((unsigned)size, (unsigned)sizeof(XAssoc));
	if (buckets == NULL) {
		/* calloc call failed! */
		errno = ENOMEM;
		return(NULL);
	}

	/* Insert table data into the XAssocTable structure. */
	table->buckets = buckets;
	table->size = size;

	while (--size >= 0) {
		/* Initialize each bucket. */
		buckets->prev = buckets;
		buckets->next = buckets;
		buckets++;
	}

	return(table);
}

