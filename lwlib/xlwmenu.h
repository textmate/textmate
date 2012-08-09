/* Interface of a lightweight menubar widget.

Copyright (C) 2002-2012  Free Software Foundation, Inc.
Copyright (C) 1992 Lucid, Inc.

This file is part of the Lucid Widget Library.

The Lucid Widget Library is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The Lucid Widget Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#ifndef _XlwMenu_h
#define _XlwMenu_h

/***********************************************************************
 *
 * XlwMenu Widget
 *
 ***********************************************************************/

#include "lwlib.h"

/* Resource names used by the XlwMenu widget */
#define XtNdisabledForeground "disabledForeground"
#define XtCDisabledForeground "DisabledForeground"
#define XtNbuttonForeground "buttonForeground"
#define XtCButtonForeground "ButtonForeground"
#define XtNmargin "margin"
#define XtNhorizontalSpacing "horizontalSpacing"
#define XtNverticalSpacing "verticalSpacing"
#define XtNarrowSpacing "arrowSpacing"
#define XtNmenu "menu"
#define XtCMenu "Menu"
#define XtNopen "open"
#define XtNselect "select"
#define XtNhighlightCallback "highlightCallback"
#define XtNenterCallback "enterCallback"
#define XtNleaveCallback "leaveCallback"
#define XtNmenuBorderWidth "menuBorderWidth"
#define XtNhorizontal "horizontal"
#define XtCHorizontal "Horizontal"
#define XtNcursor "cursor"
#define XtNCursor "Cursor"
#define XtNshowGrip "showGrip"
#define XtCShowGrip "ShowGrip"
#define XtNresizeToPreferred "resizeToPreferred"
#define XtCResizeToPreferred "ResizeToPreferred"
#define XtNallowResize "allowResize"
#define XtCAllowResize "AllowResize"

/* Motif-compatible resource names */
#define XmNshadowThickness	"shadowThickness"
#define XmCShadowThickness	"ShadowThickness"
#define XmNtopShadowColor	"topShadowColor"
#define XmCTopShadowColor	"TopShadowColor"
#define XmNbottomShadowColor	"bottomShadowColor"
#define XmCBottomShadowColor	"BottomShadowColor"
#define XmNtopShadowPixmap	"topShadowPixmap"
#define XmCTopShadowPixmap	"TopShadowPixmap"
#define XmNbottomShadowPixmap	"bottomShadowPixmap"
#define XmCBottomShadowPixmap	"BottomShadowPixmap"
#define XmRHorizontalDimension	"HorizontalDimension"

typedef struct _XlwMenuRec *XlwMenuWidget;
typedef struct _XlwMenuClassRec *XlwMenuWidgetClass;

extern WidgetClass xlwMenuWidgetClass;

extern int xlwmenu_window_p (Widget w, Window window);
extern void xlwmenu_redisplay (Widget);

#endif /* _XlwMenu_h */

