/*
 * Copyright 1985, 1986, 1987 by the Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission. M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * The X Window System is a Trademark of MIT.
 *
 */


/*
 *	X10.h - Header definition and support file for the C subroutine
 *	interface library for V10 support routines.
 */
#ifndef _X10_H_
#define _X10_H_

/* Used in XDraw and XDrawFilled */

typedef struct {
	short x, y;
	unsigned short flags;
} Vertex;

/* The meanings of the flag bits.  If the bit is 1 the predicate is true */

#define VertexRelative		0x0001		/* else absolute */
#define VertexDontDraw		0x0002		/* else draw */
#define VertexCurved		0x0004		/* else straight */
#define VertexStartClosed	0x0008		/* else not */
#define VertexEndClosed		0x0010		/* else not */
/*#define VertexDrawLastPoint	0x0020 	*/      /* else don't */

/*
The VertexDrawLastPoint option has not been implemented in XDraw and
XDrawFilled so it shouldn't be defined.
*/

/*
 * XAssoc - Associations used in the XAssocTable data structure.  The
 * associations are used as circular queue entries in the association table
 * which is contains an array of circular queues (buckets).
 */
typedef struct _XAssoc {
	struct _XAssoc *next;	/* Next object in this bucket. */
	struct _XAssoc *prev;	/* Previous object in this bucket. */
	Display *display;	/* Display which owns the id. */
	XID x_id;		/* X Window System id. */
	void *data;		/* Pointer to untyped memory. */
} XAssoc;

/*
 * XAssocTable - X Window System id to data structure pointer association
 * table.  An XAssocTable is a hash table whose buckets are circular
 * queues of XAssoc's.  The XAssocTable is constructed from an array of
 * XAssoc's which are the circular queue headers (bucket headers).
 * An XAssocTable consists an XAssoc pointer that points to the first
 * bucket in the bucket array and an integer that indicates the number
 * of buckets in the array.
 */
typedef struct {
    XAssoc *buckets;		/* Pointer to first bucket in bucket array.*/
    int size;			/* Table size (number of buckets). */
} XAssocTable;

XAssocTable *XCreateAssocTable(int size);
char *XLookUpAssoc(Display *dpy, XAssocTable *table, XID x_id);

#endif /* _X10_H_ */
