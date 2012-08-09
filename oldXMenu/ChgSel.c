/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuChangeSelection - Change a menu selection.
 *
 *	Author:		Tony Della Fera, DEC
 *			December 19, 1985
 *
 */

#include <config.h>
#include "XMenuInt.h"

int
XMenuChangeSelection(Display *display, register XMenu *menu, register int p_num, register int s_num, char *data, int data_sw, char *label, int label_sw)
                     		/* previously opened display. */
                         	/* Menu object to be modified. */
                       		/* Pane number to be modified. */
                       		/* Selection number to modified. */
               			/* Data value. */
                		/* Change to new data value? */
                		/* Selection label. */
                 		/* Change to new label? */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */

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
     * Find the right selection.
     */
    s_ptr = _XMGetSelectionPtr(p_ptr, s_num);
    if (s_ptr == NULL) return(XM_FAILURE);

    /*
     * Reset the label?
     */
    if (label_sw) {
	/*
	 * Determine label size.
	 */
	label_length = strlen(label);
	label_width = XTextWidth(menu->s_fnt_info, label, label_length);

	/*
	 * Change the selection data.
	 */
	s_ptr->label = label;
	s_ptr->label_width = label_width;
	s_ptr->label_length = label_length;

	/*
	 * Schedule a recompute.
	 */
	menu->recompute = 1;
    }

    /*
     * Reset the data?
     */
    if (data_sw) s_ptr->data = data;

    /*
     * Return successfully.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}

