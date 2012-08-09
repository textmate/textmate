/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetSelection - Set a menu selection to be active or inactive.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

int
XMenuSetSelection(register XMenu *menu, register int p_num, register int s_num, int active)
                         	/* Menu object to be modified. */
                       		/* Pane number to be modified. */
                       		/* Selection number to modified. */
               			/* Make selection active? */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Find the right selection.
     */
    s_ptr = _XMGetSelectionPtr(p_ptr, s_num);
    if (s_ptr == NULL) return(XM_FAILURE);

    /*
     * Set its active switch.
     */
    s_ptr->active = active;

    /*
     * Return the selection number just set.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}

