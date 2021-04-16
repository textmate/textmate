#import "NSMenuItem Additions.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <text/case.h>
#import <text/utf8.h>
#import <ns/ns.h>

@interface MenuAttributedString : NSAttributedString
@property (nonatomic) NSAttributedString* wrappedAttributedString;
@property (nonatomic) CGSize desiredSize;
@end

@implementation MenuAttributedString
- (instancetype)init
{
	if(self = [super init])
		_wrappedAttributedString = [[NSAttributedString alloc] init];
	return self;
}

- (instancetype)initWithAttributedString:(NSAttributedString*)anAttributedString
{
	if(self = [self init])
	{
		if(anAttributedString)
			_wrappedAttributedString = [anAttributedString copy];
	}
	return self;
}

- (NSString*)string
{
	return [_wrappedAttributedString string];
}

- (NSDictionary*)attributesAtIndex:(NSUInteger)location effectiveRange:(NSRange*)range
{
	return [_wrappedAttributedString attributesAtIndex:location effectiveRange:range];
}

- (id)copyWithZone:(NSZone*)zone
{
	MenuAttributedString* copy = [[MenuAttributedString allocWithZone:zone] initWithAttributedString:_wrappedAttributedString];
	copy.desiredSize = _desiredSize;
	return copy;
}

// We overload this method to return a height without the newline that
// is required to make the attributed string use the menu’s full width

- (NSRect)boundingRectWithSize:(NSSize)aSize options:(NSStringDrawingOptions)options context:(NSStringDrawingContext*)context
{
	return NSMakeRect(0, 0, _desiredSize.width, _desiredSize.height);
}

- (NSRect)boundingRectWithSize:(NSSize)aSize options:(NSStringDrawingOptions)options // Not called after MAC_OS_X_VERSION_10_14
{
	return [self boundingRectWithSize:aSize options:options context:nil];
}
@end

static char const* kOakMenuItemKeyEquivalent = "OakMenuItemKeyEquivalent";
static char const* kOakMenuItemTabTrigger    = "OakMenuItemTabTrigger";

@implementation NSMenuItem (FileIcon)
- (void)setIconForFile:(NSString*)path;
{
	NSImage* icon = nil;
	if([NSFileManager.defaultManager fileExistsAtPath:path])
		icon = [NSWorkspace.sharedWorkspace iconForFile:path];
	else if(OakNotEmptyString([path pathExtension]))
		icon = [NSWorkspace.sharedWorkspace iconForFileType:[path pathExtension]];
	else
		icon = [NSWorkspace.sharedWorkspace iconForFileType:NSFileTypeForHFSTypeCode(kUnknownFSObjectIcon)];

	if(icon)
	{
		[icon setSize:NSMakeSize(16, 16)];
		[self setImage:icon];
	}
}

- (void)setActivationString:(NSString*)anActivationString withFont:(NSFont*)aFont
{
	NSFont* menuFont = self.menu.font ?: [NSFont menuFontOfSize:0];

	NSString* leftString  = self.title;
	NSString* rightString = anActivationString;

	NSSize leftSize  = [leftString sizeWithAttributes:@{ NSFontAttributeName: menuFont }];
	NSSize rightSize = [rightString sizeWithAttributes:@{ NSFontAttributeName: aFont ?: menuFont }];

	NSTextTable* table = [[NSTextTable alloc] init];
	table.numberOfColumns = 2;

	NSTextTableBlock* leftBlock  = [[NSTextTableBlock alloc] initWithTable:table startingRow:0 rowSpan:1 startingColumn:0 columnSpan:1];
	NSTextTableBlock* rightBlock = [[NSTextTableBlock alloc] initWithTable:table startingRow:0 rowSpan:1 startingColumn:1 columnSpan:1];

	leftBlock.verticalAlignment  = NSTextBlockMiddleAlignment;
	rightBlock.verticalAlignment = aFont && aFont.pointSize >= 13 ? NSTextBlockBottomAlignment : NSTextBlockMiddleAlignment;

	[rightBlock setContentWidth:rightSize.width type:NSTextBlockAbsoluteValueType];

	NSMutableParagraphStyle* leftPStyle = [[NSMutableParagraphStyle alloc] init];
	leftPStyle.textBlocks    = @[ leftBlock ];
	leftPStyle.alignment     = NSTextAlignmentLeft;
	leftPStyle.lineBreakMode = NSLineBreakByClipping;

	NSMutableParagraphStyle* rightPStyle = [[NSMutableParagraphStyle alloc] init];
	rightPStyle.textBlocks    = @[ rightBlock ];
	rightPStyle.alignment     = NSTextAlignmentRight;
	rightPStyle.lineBreakMode = NSLineBreakByClipping;

	// This is required to make the attributed string use the menu’s full width
	leftString = [leftString stringByAppendingString:@"\n"];

	NSMutableAttributedString* attributedString = [[NSMutableAttributedString alloc] init];

	[attributedString appendAttributedString:[[NSAttributedString alloc] initWithString:leftString attributes:@{
		NSParagraphStyleAttributeName: leftPStyle,
		NSFontAttributeName:           menuFont,
	}]];

	NSColor* shortcutTextColor = NSColor.textColor;
	if(@available(macos 11.0, *))
		shortcutTextColor = NSColor.tertiaryLabelColor;

	[attributedString appendAttributedString:[[NSAttributedString alloc] initWithString:rightString attributes:@{
		NSParagraphStyleAttributeName:  rightPStyle,
		NSFontAttributeName:            aFont ?: menuFont,
		NSForegroundColorAttributeName: shortcutTextColor,
	}]];

	MenuAttributedString* attributedTitle = [[MenuAttributedString alloc] initWithAttributedString:attributedString];
	// Set the string’s bounding box to the height *excluding* the newline appended above
	attributedTitle.desiredSize = NSMakeSize(leftSize.width + rightSize.width, MAX(leftSize.height, rightSize.height));

	NSString* plainTitle = self.title;
	self.attributedTitle = attributedTitle;
	self.title = plainTitle;
}

- (void)updateTitle:(NSString*)newTitle
{
	if([self.title isEqualToString:newTitle])
		return;

	self.title = newTitle;
	if(NSString* keyEquivalent = objc_getAssociatedObject(self, kOakMenuItemKeyEquivalent))
		[self setInactiveKeyEquivalentCxxString:to_s(keyEquivalent)];
	if(NSString* tabTrigger = objc_getAssociatedObject(self, kOakMenuItemTabTrigger))
		[self setTabTriggerCxxString:to_s(tabTrigger)];
}

- (void)setKeyEquivalentCxxString:(std::string const&)aKeyEquivalent
{
	if(aKeyEquivalent == NULL_STR || aKeyEquivalent.empty())
	{
		[self setKeyEquivalent:@""];
		[self setKeyEquivalentModifierMask:0];
		return;
	}

	NSUInteger modifiers = 0;

	size_t i = 0;
	while(true)
	{
		if(i+1 >= aKeyEquivalent.size() || !strchr("$^~@#", aKeyEquivalent[i]))
			break;

		switch(aKeyEquivalent[i++])
		{
			case '$': modifiers |= NSEventModifierFlagShift;      break;
			case '^': modifiers |= NSEventModifierFlagControl;    break;
			case '~': modifiers |= NSEventModifierFlagOption;     break;
			case '@': modifiers |= NSEventModifierFlagCommand;    break;
			case '#': modifiers |= NSEventModifierFlagNumericPad; break;
		}
	}

	[self setKeyEquivalent:[NSString stringWithCxxString:aKeyEquivalent.substr(i)]];
	[self setKeyEquivalentModifierMask:modifiers];
}

- (void)setInactiveKeyEquivalentCxxString:(std::string const&)aKeyEquivalent
{
	objc_setAssociatedObject(self, kOakMenuItemKeyEquivalent, [NSString stringWithCxxString:aKeyEquivalent], OBJC_ASSOCIATION_RETAIN);
	if(aKeyEquivalent != NULL_STR && !aKeyEquivalent.empty())
		[self setActivationString:[NSString stringWithCxxString:" " + ns::glyphs_for_event_string(aKeyEquivalent)] withFont:nil];
}

- (void)setTabTriggerCxxString:(std::string const&)aTabTrigger
{
	objc_setAssociatedObject(self, kOakMenuItemTabTrigger, [NSString stringWithCxxString:aTabTrigger], OBJC_ASSOCIATION_RETAIN);
	if(aTabTrigger != NULL_STR)
		[self setActivationString:[NSString stringWithCxxString:(" "+aTabTrigger+"⇥")] withFont:[NSFont menuBarFontOfSize:floor([(self.menu.font ?: [NSFont menuFontOfSize:0]) pointSize] * 0.85)]];
}

- (void)setModifiedState:(BOOL)flag
{
	if(NSImage* image = [NSImage imageNamed:@"NSMenuItemBullet"])
		[self setMixedStateImage:image];
	[self setState:flag ? NSControlStateValueMixed : NSControlStateValueOff];
}

- (void)setDynamicTitle:(NSString*)plainTitle
{
	if(OakNotEmptyString(self.userKeyEquivalent))
	{
		NSString* title = plainTitle;
		plainTitle = self.title;

		if([title isEqualToString:plainTitle])
		{
			self.attributedTitle = nil;
		}
		else
		{
			NSFont* font = self.menu.font ?: [NSFont menuFontOfSize:0];
			self.attributedTitle = [[NSAttributedString alloc] initWithString:title attributes:@{ NSFontAttributeName: font }];
		}
	}
	self.title = plainTitle;
}
@end
