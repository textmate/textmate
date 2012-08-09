/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuSetAEQ - Set Asynchronous event queuing mode.
 *		      When enabled asynchronous events will be queue while
 *		      a menu is being displayed and restored to the X
 *		      event queue when the menu is taken down.
 *
 *	Author:		Tony Della Fera, DEC
 *			March 12, 1986
 *
 */

#include "XMenuInt.h"

void
XMenuSetAEQ(register XMenu *menu, register int aeq)
                         	/* Menu object to be modified. */
                     		/* AEQ mode? */
{
    /*
     * Set the AEQ mode switch.
     */
    menu->aeq = aeq;
}
