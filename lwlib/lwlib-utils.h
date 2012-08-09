#ifndef _LWLIB_UTILS_H_
#define _LWLIB_UTILS_H_

void XtNoClearRefreshWidget (Widget);

typedef void (*XtApplyToWidgetsProc) (Widget, XtPointer);
typedef void* (*XtApplyUntilToWidgetsProc) (Widget, XtPointer);

void XtApplyToWidgets (Widget, XtApplyToWidgetsProc, XtPointer);
void *XtApplyUntilToWidgets (Widget, XtApplyUntilToWidgetsProc, XtPointer);

Widget *XtCompositeChildren (Widget, unsigned int *);

/* returns True is the widget is being destroyed, False otherwise */
Boolean
XtWidgetBeingDestroyedP (Widget widget);

#endif /* _LWLIB_UTILS_H_ */
