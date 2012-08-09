/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


#include <config.h>
#include "XMenuInt.h"
#include <X11/Xresource.h>
#include <errno.h>

#ifndef NULL
#define NULL 0
#endif

/*
 * XMakeAssoc - Insert data into an XAssocTable keyed on an XId.
 * Data is inserted into the table only once.  Redundant inserts are
 * meaningless (but cause no problems).  The queue in each association
 * bucket is sorted (lowest XId to highest XId).
 */
void
XMakeAssoc(register Display *dpy, register XAssocTable *table, register XID x_id, register void *data)
{
	int hash;
	register XAssoc *bucket;
	register XAssoc *Entry;
	register XAssoc *new_entry;

	/* Hash the XId to get the bucket number. */
	hash = x_id & (table->size - 1);
	/* Look up the bucket to get the entries in that bucket. */
	bucket = &table->buckets[hash];
	/* Get the first entry in the bucket. */
	Entry = bucket->next;

	/* If (Entry != bucket), the bucket is empty so make */
	/* the new entry the first entry in the bucket. */
	/* if (Entry == bucket), the we have to search the */
	/* bucket. */
	if (Entry != bucket) {
		/* The bucket isn't empty, begin searching. */
		/* If we leave the for loop then we have either passed */
		/* where the entry should be or hit the end of the bucket. */
		/* In either case we should then insert the new entry */
		/* before the current value of "Entry". */
		for (; Entry != bucket; Entry = Entry->next) {
			if (Entry->x_id == x_id) {
				/* Entry has the same XId... */
				if (Entry->display == dpy) {
					/* Entry has the same Display... */
					/* Therefore there is already an */
					/* entry with this XId and Display, */
					/* reset its data value and return. */
					Entry->data = data;
					return;
				}
				/* We found an association with the right */
				/* id but the wrong display! */
				continue;
			}
			/* If the current entry's XId is greater than the */
			/* XId of the entry to be inserted then we have */
			/* passed the location where the new XId should */
			/* be inserted. */
			if (Entry->x_id > x_id) break;
		}
        }

	/* If we are here then the new entry should be inserted just */
	/* before the current value of "Entry". */
	/* Create a new XAssoc and load it with new provided data. */
	new_entry = (XAssoc *) malloc(sizeof(XAssoc));
	new_entry->display = dpy;
	new_entry->x_id = x_id;
	new_entry->data = data;

	/* Insert the new entry. */
	emacs_insque((struct qelem *)new_entry, (struct qelem *)Entry->prev);
}
