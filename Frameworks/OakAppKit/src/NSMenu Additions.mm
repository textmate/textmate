#import "NSMenu Additions.h"

extern "C" MenuRef _NSGetCarbonMenu (NSMenu* aMenu);

static void* const kUserDataNormalFont = (void*)0;
static void* const kUserDataSmallFont  = (void*)1;

static CFStringRef GetTabTriggerForMenuItem (MenuRef menuRef, MenuItemIndex item)
{
	size_t len = 0;
	if(noErr == GetMenuItemProperty(menuRef, item, 'TxMt', 'TbLn', sizeof(len), NULL, &len) && len)
	{
		UInt8 data[len];
		if(noErr == GetMenuItemProperty(menuRef, item, 'TxMt', 'TbTr', len, NULL, &data[0]))
			return CFStringCreateWithBytes(kCFAllocatorDefault, data, len, kCFStringEncodingUTF8, false);
	}
	return NULL;
}

static float GetTabTriggerWidth (CFStringRef tabTrigger)
{
	float res = 0;
	if(tabTrigger)
	{
		HIThemeTextInfo textInfo = { kHIThemeTextInfoVersionZero, kThemeStateActive, kThemeSmallSystemFont, kHIThemeTextHorizontalFlushRight, kHIThemeTextVerticalFlushCenter, 0, kHIThemeTextTruncationNone, 1, 0 };
		HIThemeGetTextDimensions(tabTrigger, 0, &textInfo, &res, NULL, NULL);
	}
	return res;
}

static OSStatus LeopardEventHandler (EventHandlerCallRef inCaller, EventRef inEvent, void* userdata)
{
	OSStatus res = CallNextEventHandler(inCaller, inEvent);

	MenuRef menuRef = NULL;
	MenuItemIndex item = 0;
	GetEventParameter(inEvent, kEventParamDirectObject, typeMenuRef, NULL, sizeof(menuRef), NULL, &menuRef);
	GetEventParameter(inEvent, kEventParamMenuItemIndex, typeMenuItemIndex, NULL, sizeof(item), NULL, &item);

	MenuTrackingData data = { };
	GetMenuTrackingData(menuRef, &data);
	BOOL isSelected        = data.itemSelected == item;
	BOOL isEnabled         = item != 0 && IsMenuItemEnabled(menuRef, item);
	BOOL smallFont         = userdata == kUserDataSmallFont;
	CFStringRef tabTrigger = GetTabTriggerForMenuItem(menuRef, item);
#if !defined(MAC_OS_X_VERSION_10_7) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7)
	switch(GetEventKind(inEvent))
	{
		case kEventMenuMeasureItemWidth:
		{
			SInt16 width = 0;
			GetEventParameter(inEvent, kEventParamMenuItemWidth, typeShortInteger, NULL, sizeof(width), NULL, &width);
			width += GetTabTriggerWidth(tabTrigger) + 25;

			CFStringRef label = NULL;
			CopyMenuItemTextAsCFString(menuRef, item, &label);
			ThemeFontID fontID = smallFont ? kThemeSmallSystemFont : kThemeMenuItemFont;
			HIThemeTextInfo textInfo = { kHIThemeTextInfoVersionZero, kThemeStateActive, fontID, kHIThemeTextHorizontalFlushLeft, kHIThemeTextVerticalFlushCenter, 0, kHIThemeTextTruncationNone, 1, 0 };
			float w = 0;
			HIThemeGetTextDimensions(label, 0, &textInfo, &w, NULL, NULL);
			width += w;
			CFRelease(label);

			SetEventParameter(inEvent, kEventParamMenuItemWidth, typeShortInteger, sizeof(width), &width);
		}
		break;

		case kEventMenuMeasureItemHeight:
		{
			SInt16 height = 0;
			GetEventParameter(inEvent, kEventParamMenuItemHeight, typeShortInteger, NULL, sizeof(height), NULL, &height);
			height += smallFont ? 12 : 15;

			SetEventParameter(inEvent, kEventParamMenuItemHeight, typeShortInteger, sizeof(height), &height);
		}
		break;

		case kEventMenuDrawItemContent:
		{
			CGContextRef context = NULL;
			GetEventParameter(inEvent, kEventParamCGContextRef, typeCGContextRef, NULL, sizeof(context), NULL, &context);

			Rect portBounds;
			GetPortBounds(NULL, &portBounds);

			CGContextSaveGState(context);
			CGContextTranslateCTM(context, 0, portBounds.bottom - portBounds.top);
			CGContextScaleCTM(context, 1, -1);

			HIRect rect;
			GetEventParameter(inEvent, kEventParamMenuItemBounds, typeHIRect, NULL, sizeof(rect), NULL, &rect);
			NSRect r = NSMakeRect(rect.origin.x, rect.origin.y - 1.0, rect.size.width, rect.size.height + 2.0);

			NSGraphicsContext* nscontext = [NSGraphicsContext graphicsContextWithGraphicsPort:context flipped:NO];
			[NSGraphicsContext setCurrentContext:nscontext];

			if(tabTrigger)
			{
				HIThemeTextInfo textInfo = { kHIThemeTextInfoVersionZero, kThemeStateActive, kThemeSmallSystemFont, kHIThemeTextHorizontalFlushRight, kHIThemeTextVerticalFlushCenter, 0, kHIThemeTextTruncationNone, 1, 0 };
				float width = 0;
				HIThemeGetTextDimensions(tabTrigger, 0, &textInfo, &width, NULL, NULL);

				[[NSColor colorWithCalibratedRed:0 green:0 blue:0 alpha:isEnabled ? 0.15 : 0.075] set];
				NSRect podRect = smallFont ? NSMakeRect(NSMaxX(r) - width - 15, NSMinY(r) + 1, width + 10, NSHeight(r)) : NSMakeRect(NSMaxX(r) - width - 15, NSMinY(r) + 2, width + 10, NSHeight(r) - 2);
				[[NSBezierPath bezierPathWithRoundedRect:podRect xRadius:4 yRadius:4] fill];

				HIRect bounds = { { r.origin.x, r.origin.y }, { r.size.width - 10, r.size.height } };
				[(isEnabled ? (isSelected ? [NSColor selectedMenuItemTextColor] : [NSColor textColor]) : [NSColor disabledControlTextColor]) set];
				HIThemeDrawTextBox(tabTrigger, &bounds, &textInfo, context, kHIThemeOrientationNormal);
			}

			SInt32 leading = 0, mark_width = 0, trailing = 0;
			GetThemeMetric(kThemeMetricMenuTextLeadingEdgeMargin, &leading);
			GetThemeMetric(kThemeMetricMenuMarkColumnWidth, &mark_width);
			GetThemeMetric(kThemeMetricMenuTextTrailingEdgeMargin, &trailing);

			ThemeFontID fontID = smallFont ? kThemeSmallSystemFont : kThemeMenuItemFont;
			HIThemeTextInfo textInfo = { kHIThemeTextInfoVersionZero, kThemeStateActive, fontID, kHIThemeTextHorizontalFlushLeft, kHIThemeTextVerticalFlushCenter, 0, kHIThemeTextTruncationNone, 1, 0 };

			CFStringRef label = NULL;
			CopyMenuItemTextAsCFString(menuRef, item, &label);
			HIRect bounds = { { r.origin.x + leading + mark_width, r.origin.y }, { r.size.width - leading - mark_width - trailing, r.size.height } };
			[(isEnabled ? (isSelected ? [NSColor selectedMenuItemTextColor] : [NSColor textColor]) : [NSColor disabledControlTextColor]) set];
			HIThemeDrawTextBox(label, &bounds, &textInfo, context, kHIThemeOrientationNormal);
			CFRelease(label);

			CGContextRestoreGState(context);
		}
		break;
	}
#endif
	if(tabTrigger)
		CFRelease(tabTrigger);

	return res;
}

static OSStatus SnowLeopardEventHandler (EventHandlerCallRef inCaller, EventRef inEvent, void* userdata)
{
	OSStatus res = CallNextEventHandler(inCaller, inEvent);

	MenuRef menuRef = NULL;
	MenuItemIndex item = 0;
	GetEventParameter(inEvent, kEventParamDirectObject, typeMenuRef, NULL, sizeof(menuRef), NULL, &menuRef);
	GetEventParameter(inEvent, kEventParamMenuItemIndex, typeMenuItemIndex, NULL, sizeof(item), NULL, &item);

	CFStringRef tabTrigger = GetTabTriggerForMenuItem(menuRef, item);
	if(!tabTrigger)
		return res;
#if !defined(MAC_OS_X_VERSION_10_7) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_7)
	switch(GetEventKind(inEvent))
	{
		case kEventMenuMeasureItemWidth:
		{
			SInt16 width = 0;
			GetEventParameter(inEvent, kEventParamMenuItemWidth, typeShortInteger, NULL, sizeof(width), NULL, &width);
			width += GetTabTriggerWidth(tabTrigger) + 25;
			SetEventParameter(inEvent, kEventParamMenuItemWidth, typeShortInteger, sizeof(width), &width);
		}
		break;

		case kEventMenuDrawItemContent:
		{
			MenuTrackingData data = { };
			GetMenuTrackingData(menuRef, &data);
			BOOL isSelected        = data.itemSelected == item;
			BOOL isEnabled         = item != 0 && IsMenuItemEnabled(menuRef, item);
			BOOL smallFont         = userdata == kUserDataSmallFont;

			CGContextRef context = NULL;
			GetEventParameter(inEvent, kEventParamCGContextRef, typeCGContextRef, NULL, sizeof(context), NULL, &context);

			Rect portBounds;
			GetPortBounds(NULL, &portBounds);

			CGContextSaveGState(context);
			CGContextTranslateCTM(context, 0, portBounds.bottom - portBounds.top);
			CGContextScaleCTM(context, 1, -1);

			HIRect rect;
			GetEventParameter(inEvent, kEventParamMenuItemBounds, typeHIRect, NULL, sizeof(rect), NULL, &rect);
			NSRect r = NSMakeRect(rect.origin.x, rect.origin.y - 1.0, rect.size.width, rect.size.height + 2.0);

			NSGraphicsContext* nscontext = [NSGraphicsContext graphicsContextWithGraphicsPort:context flipped:NO];
			[NSGraphicsContext setCurrentContext:nscontext];

			HIThemeTextInfo textInfo = { kHIThemeTextInfoVersionZero, kThemeStateActive, kThemeSmallSystemFont, kHIThemeTextHorizontalFlushRight, kHIThemeTextVerticalFlushCenter, 0, kHIThemeTextTruncationNone, 1, 0 };
			float width = 0;
			HIThemeGetTextDimensions(tabTrigger, 0, &textInfo, &width, NULL, NULL);

			[[NSColor colorWithCalibratedRed:0 green:0 blue:0 alpha:isEnabled ? 0.15 : 0.075] set];
			NSRect podRect = smallFont ? NSMakeRect(NSMaxX(r) - width - 15, NSMinY(r) + 1, width + 10, NSHeight(r)) : NSMakeRect(NSMaxX(r) - width - 15, NSMinY(r) + 2, width + 10, NSHeight(r) - 2);
			[[NSBezierPath bezierPathWithRoundedRect:podRect xRadius:4 yRadius:4] fill];

			HIRect bounds = { { r.origin.x, r.origin.y }, { r.size.width - 10, r.size.height } };
			[(isEnabled ? (isSelected ? [NSColor selectedMenuItemTextColor] : [NSColor textColor]) : [NSColor disabledControlTextColor]) set];
			HIThemeDrawTextBox(tabTrigger, &bounds, &textInfo, context, kHIThemeOrientationNormal);

			CGContextRestoreGState(context);
		}
		break;
	}
#endif
	CFRelease(tabTrigger);
	return res;
}

@implementation NSMenu (Additions)
- (NSMenuItem*)parentMenuItem;
{
	NSMenuItem* superItem = nil;
	for(NSMenuItem* item in [[self supermenu] itemArray])
	{
		if([item submenu] == self)
		{
			superItem = item;
			break;
		}
	}
	return superItem;
}

static bool OnSnowLeopardOrHigher ()
{
	static SInt32 osVersion = 0;
	if(osVersion == 0)
		Gestalt(gestaltSystemVersion, &osVersion);
	return osVersion >= 0x1060;
}

- (void)enableTabTriggers
{
	if(MenuRef menu = _NSGetCarbonMenu(self))
	{
		OSStatus (*eventHandler) (EventHandlerCallRef, EventRef, void*);
		eventHandler = OnSnowLeopardOrHigher() ? &SnowLeopardEventHandler : &LeopardEventHandler;

		static const EventTypeSpec menuEvents [] = {
			{ kEventClassMenu, kEventMenuMeasureItemWidth  },
			{ kEventClassMenu, kEventMenuMeasureItemHeight },
			{ kEventClassMenu, kEventMenuDrawItemContent   },
		};

		void* contextInfo = [[self title] isEqualToString:@"_small"] ? kUserDataSmallFont : kUserDataNormalFont;
		EventHandlerRef menuHandler = NULL;
		InstallMenuEventHandler(menu, eventHandler, GetEventTypeCount(menuEvents), menuEvents, contextInfo, &menuHandler);
	}
}
@end
