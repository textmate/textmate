/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 * 	XMenuAddSelection - Adds a selection to an XMenu object.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include <config.h>
#include "XMenuInt.h"

int
XMenuAddSelection(Display *display, register XMenu *menu, register int p_num, char *data, char *label, int active, char const *help)

                         	/* Menu object to be modified. */
                       		/* Pane number to be modified. */
               			/* Data value. */
                		/* Selection label. */
               			/* Make selection active? */
               			/* Help string */
{
    register XMPane *pane;	/* Pane containing the new selection. */
    register XMSelect *sel;	/* Newly created selection. */


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
    pane = _XMGetPanePtr(menu, p_num);
    if (pane == NULL) return(XM_FAILURE);

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

    sel->serial = -1;
    sel->label = label;
    sel->label_width = label_width;
    sel->label_length = label_length;
    sel->data = data;
    sel->parent_p = pane;
    sel->help_string = help;

    /*
     * Insert the selection at the end of the selection list.
     */
    emacs_insque(sel, pane->s_list->prev);

    /*
     * Update the selection count.
     */
    pane->s_count++;

    /*
     * Schedule a recompute.
     */
    menu->recompute = 1;

    /*
     * Return the selection number just added.
     */
    _XMErrorCode = XME_NO_ERROR;
    return((pane->s_count - 1));
}
