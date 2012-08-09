/* Copyright    Massachusetts Institute of Technology    1985	*/

#include "copyright.h"


/*
 * XMenu:	MIT Project Athena, X Window system menu package
 *
 *	XMenuDestroy - Free all resources associated with and XMenu.
 *
 *	Author:		Tony Della Fera, DEC
 *			August, 1985
 *
 */

#include "XMenuInt.h"

void
XMenuDestroy(Display *display, register XMenu *menu)

                         	/* Menu object to destroy. */
{
    register XMPane *p_ptr;	/* Pointer to the current pane. */
    register XMPane *p_next;	/* Pointer to the next pane. */
    register XMSelect *s_ptr;	/* Pointer to the current selection. */
    register XMSelect *s_next;  /* Pointer to the next selection. */

    /*
     * Destroy the selection and pane X windows and free
     * their corresponding XMWindows.
     */
    for (
	p_ptr = menu->p_list->next;
	p_ptr != menu->p_list;
	p_ptr = p_next
    ) {
	for (
	    s_ptr = p_ptr->s_list->next;
	    s_ptr != p_ptr->s_list;
	    s_ptr = s_next
	) {
	    s_next = s_ptr->next;
	    free(s_ptr);
	}
	if (p_ptr->window) {
	    XDestroySubwindows(display, p_ptr->window);
	    XDestroyWindow(display, p_ptr->window);
	}
	p_next = p_ptr->next;
	free(p_ptr);
    }

    /*
     * Destroy the association table.
     */
    XDestroyAssocTable(menu->assoc_tab);

    /*
     * Free the mouse cursor.
     */
    XFreeCursor(display, menu->mouse_cursor);

    /*
     * Free the fonts.
     */
    XFreeFont(display, menu->p_fnt_info);
    XFreeFont(display, menu->s_fnt_info);

    /*
     * Free the pixmaps.
     */
/*    XFreePixmap(display, menu->p_bdr_pixmap);
    XFreePixmap(display, menu->s_bdr_pixmap);
    XFreePixmap(display, menu->p_frg_pixmap);
    XFreePixmap(display, menu->s_frg_pixmap);
    XFreePixmap(display, menu->bkgnd_pixmap); */
    XFreePixmap(display, menu->inact_pixmap);

    /*
     * Free the color cells.
     */
    if ((menu->p_bdr_color != BlackPixel(display, DefaultScreen(display))) && (menu->p_bdr_color != WhitePixel(display, DefaultScreen(display))))
	XFreeColors(
		    display,
		    DefaultColormap(display, DefaultScreen(display)),
		    &menu->p_bdr_color,
		    1, 0);
    if ((menu->s_bdr_color != BlackPixel(display, DefaultScreen(display))) && (menu->s_bdr_color != WhitePixel(display, DefaultScreen(display))))
	XFreeColors(
		    display,
		    DefaultColormap(display, DefaultScreen(display)),
		    &menu->s_bdr_color,
		    1, 0);
    if ((menu->p_frg_color != BlackPixel(display, DefaultScreen(display))) && (menu->p_frg_color != WhitePixel(display, DefaultScreen(display))))
	XFreeColors(
		    display,
		    DefaultColormap(display, DefaultScreen(display)),
		    &menu->p_frg_color,
		    1, 0);
    if ((menu->s_frg_color != BlackPixel(display, DefaultScreen(display))) && (menu->s_frg_color != WhitePixel(display, DefaultScreen(display))))
	XFreeColors(
		    display,
		    DefaultColormap(display, DefaultScreen(display)),
		    &menu->s_frg_color,
		    1, 0);
    if ((menu->bkgnd_color != BlackPixel(display, DefaultScreen(display))) && (menu->bkgnd_color != WhitePixel(display, DefaultScreen(display))))
	XFreeColors(
		    display,
		    DefaultColormap(display, DefaultScreen(display)),
		    &menu->bkgnd_color,
		    1, 0);

    /*
     * Free the XMenu.
     */
    free(menu);
}
