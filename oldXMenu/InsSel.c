/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuInsertSelection - Inserts a selection into an XMenu object
 *
 *	Author:		Tony Della Fera, DEC
 *			20-Nov-85
 *
 */

#include <config.h>
#include "XMenuInt.h"

int
XMenuInsertSelection(register XMenu *menu, register int p_num, register int s_num, char *data, char *label, int active)
                         	/* Menu object to be modified. */
                       		/* Pane number to be modified. */
                       		/* Selection number of new selection. */
               			/* Data value. */
                		/* Selection label. */
               			/* Make selection active? */
{
    register XMPane *p_ptr;	/* XMPane pointer. */
    register XMSelect *s_ptr;	/* XMSelect pointer. */

    XMSelect *sel;		/* Newly created selection. */

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
     * Find the selection number one less than the one specified since that
     * is the selection after which the insertion will occur.
     */
    s_ptr = _XMGetSelectionPtr(p_ptr, (s_num - 1));
    if (s_ptr == NULL) return(XM_FAILURE);

    /*
     * Calloc the XMSelect structure.
     */
    sel = (XMSelect *)calloc(1, sizeof(XMSelect));
    if (sel == NULL) {
	_XMErrorCode = XME_CALLOC;
	return(XM_FAILURE);
    }

    /*
     * Determine label size.
     */
    label_length = strlen(label);
    label_width = XTextWidth(menu->s_fnt_info, label, label_length);


    /*
     * Fill the XMSelect structure.
     */
    if (!strcmp (label, "--") || !strcmp (label, "---"))
      {
	sel->type = SEPARATOR;
	sel->active = 0;
      }
    else
      {
	sel->type = SELECTION;
	sel->active = active;
      }

    sel->active = active;
    sel->serial = -1;
    sel->label = label;
    sel->label_width = label_width;
    sel->label_length = label_length;
    sel->data = data;
    sel->parent_p = p_ptr;

    /*
     * Insert the selection after the selection with the selection
     * number one less than the desired number for the new selection.
     */
    emacs_insque(sel, s_ptr);

    /*
     * Update the selection count.
     */
    p_ptr->s_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the selection number just inserted.
     */
    _XMErrorCode = XME_NO_ERROR;
    return(s_num);
}
