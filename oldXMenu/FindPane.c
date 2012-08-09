/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuFindPane - Find the first menu pane who's label matches a
 *			particular string.
 *
 *	Author:		Tony Della Fera, DEC
 *			January 22, 1986
 *
 */

#include "XMenuInt.h"
#include <string.h>

int
XMenuFindPane(register XMenu *menu, register char *label)
{
    register XMPane *p_ptr;
    register int i = 0;

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Find the pane who's label matches the given label.
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_ptr->next
    ){
	if (p_ptr->label_length == 0) {
	    if (*label == '\0') {
		_XMErrorCode = XME_NO_ERROR;
		return (i);
	    }
	}
	else {
	    if (strncmp (label, p_ptr->label, p_ptr->label_length) == 0) {
		_XMErrorCode = XME_NO_ERROR;
		return (i);
	    }
	}
	i++;
    }

    /*
     * If we get here then we have not found
     * a match.
     */
    _XMErrorCode = XME_P_NOT_FOUND;
    return (XM_FAILURE);
}
