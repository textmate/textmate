/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"

#include "XMenuInt.h"

/*
 * XDestroyAssocTable - Destroy (free the memory associated with)
 * an XAssocTable.
 */
void
XDestroyAssocTable(register XAssocTable *table)
{
	register int i;
	register XAssoc *bucket;
	register XAssoc *Entry, *entry_next;

	/* Free the buckets. */
	for (i = 0; i < table->size; i++) {
		bucket = &table->buckets[i];
		for (
	        	Entry = bucket->next;
			Entry != bucket;
			Entry = entry_next
		) {
		        entry_next = Entry->next;
			free((char *)Entry);
		}
	}

	/* Free the bucket array. */
	free((char *)table->buckets);

	/* Free the table. */
	free((char *)table);
}
