/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuChangePane - Change the label of a  menu pane.
 *
 *	Author:		Tony Della Fera, DEC
 *			December 19, 1985
 *
 */

#include <config.h>
#include "XMenuInt.h"

int
XMenuChangePane(register XMenu *menu, register int p_num, char *label)
                         	/* Menu object to be modified. */
                       		/* Pane number to be modified. */
                		/* Selection label. */
{
    register XMPane *p_ptr;	/* XMPane pointer. */

    int label_length;		/* Label length in characters. */
    int label_width;		/* Label width in pixels. */

    /*
     * Check for NULL pointers!
     */
    if (label == NULL) {
	_XMErrorCode = XME_ARG_BOUNDS;
	return(XM_FAILURE);
    }

    /*
     * Find the right pane.
     */
    p_ptr = _XMGetPanePtr(menu, p_num);
    if (p_ptr == NULL) return(XM_FAILURE);

    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->p_fnt_info, label, label_length);

    /*
     * Change the pane data.
     */
    p_ptr->label = label;
    p_ptr->label_width = label_width;
    p_ptr->label_length = label_length;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the pane number just changed.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(p_num);
}

