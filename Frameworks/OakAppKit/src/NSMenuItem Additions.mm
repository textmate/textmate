#import "NSMenu Additions.h"
#import "OakFileIconImage.h"
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/NSString Additions.h>
#import <text/case.h>
#import <text/utf8.h>
#import <ns/ns.h>

@interface MenuMutableAttributedString : NSMutableAttributedString
{
	NSMutableAttributedString* contents;
	CGSize size;
}
- (void)appendTableCellWithString:(NSString*)string table:(NSTextTable*)table textAlignment:(NSTextAlignment)textAlignment verticalAlignment:(NSTextBlockVerticalAlignment)verticalAlignment font:(NSFont*)font row:(NSInteger)row column:(NSInteger)column;
- (CGSize)size;
@end

@implementation MenuMutableAttributedString

// Methods to override in subclass

- (instancetype)init
{
	if(self = [super init])
		contents = [[NSMutableAttributedString alloc] init];
	return self;
}

- (instancetype)initWithAttributedString:(NSAttributedString*)anAttributedString
{
	if(self = [self init])
	{
		if(anAttributedString)
			[contents setAttributedString:anAttributedString];
	}
	return self;
}

- (NSString*)string
{
	return [contents string];
}

- (NSDictionary*)attributesAtIndex:(NSUInteger)location effectiveRange:(NSRange*)range
{
	return [contents attributesAtIndex:location effectiveRange:range];
}

- (void)replaceCharactersInRange:(NSRange)range withString:(NSString*)string
{
	[contents replaceCharactersInRange:range withString:string];
}

- (void)setAttributes:(NSDictionary*)attributes range:(NSRange)range
{
	[contents setAttributes:attributes range:range];
}

- (id)copyWithZone:(NSZone*)zone
{
	MenuMutableAttributedString* copy = [MenuMutableAttributedString allocWithZone:zone];
	copy->contents = [contents copyWithZone:zone];
	copy->size = size;
	return copy;
}

// NOTE: AppKit additions produce invalid values here, provide our own implementation

- (NSRect)boundingRectWithSize:(NSSize)aSize options:(NSStringDrawingOptions)options
{
	return NSMakeRect(0, 0, size.width, size.height);
}

// Helper method for adding table cell into the attributed string

- (void)appendTableCellWithString:(NSString*)string table:(NSTextTable*)table textAlignment:(NSTextAlignment)textAlignment verticalAlignment:(NSTextBlockVerticalAlignment)verticalAlignment font:(NSFont*)font row:(NSInteger)row column:(NSInteger)column;
{
	CGSize stringSize = [string sizeWithAttributes:@{ NSFontAttributeName : font }];

	NSTextTableBlock* block = [[NSTextTableBlock alloc] initWithTable:table startingRow:row rowSpan:1 startingColumn:column columnSpan:1];

	if(column > 0)
		[block setContentWidth:stringSize.width type:NSTextBlockAbsoluteValueType];

	block.verticalAlignment = verticalAlignment;

	NSMutableParagraphStyle* paragraphStyle = [NSMutableParagraphStyle new];
	[paragraphStyle setTextBlocks:@[ block ]];
	[paragraphStyle setAlignment:textAlignment];

	string = [string stringByAppendingString:@"\n"];

	NSMutableAttributedString* cellString = [[NSMutableAttributedString alloc] initWithString:string];
	[cellString addAttribute:NSParagraphStyleAttributeName value:paragraphStyle range:NSMakeRange(0, [cellString length])];
	[cellString addAttribute:NSFontAttributeName value:font range:NSMakeRange(0, [cellString length])];

	size.width += stringSize.width;
	if(size.height < stringSize.height)
		size.height = stringSize.height;

	[self appendAttributedString:cellString];
}

- (CGSize)size
{
	return size;
}

@end

static char const* kOakMenuItemKeyEquivalent = "OakMenuItemKeyEquivalent";
static char const* kOakMenuItemTabTrigger    = "OakMenuItemTabTrigger";

@implementation NSMenuItem (FileIcon)
- (void)setIconForFile:(NSString*)path;
{
	NSImage* icon = nil;
	if([[NSFileManager defaultManager] fileExistsAtPath:path])
		icon = [OakFileIconImage fileIconImageWithPath:path size:NSMakeSize(16, 16)];
	else if(OakNotEmptyString([path pathExtension]))
		icon = [[NSWorkspace sharedWorkspace] iconForFileType:[path pathExtension]];
	else
		icon = [[NSWorkspace sharedWorkspace] iconForFileType:NSFileTypeForHFSTypeCode(kUnknownFSObjectIcon)];

	if(icon)
	{
		[icon setSize:NSMakeSize(16, 16)];
		[self setImage:icon];
	}
}

- (void)setActivationString:(NSString*)anActivationString withFont:(NSFont*)aFont
{
	MenuMutableAttributedString* attributedTitle = [MenuMutableAttributedString new];
	NSTextTable* table = [NSTextTable new];
	[table setNumberOfColumns:2];

	NSFont* font = self.menu.font ?: [NSFont menuFontOfSize:0];
	[attributedTitle appendTableCellWithString:self.title table:table textAlignment:NSLeftTextAlignment verticalAlignment:NSTextBlockMiddleAlignment font:font row:0 column:0];
	[attributedTitle appendTableCellWithString:anActivationString table:table textAlignment:NSRightTextAlignment verticalAlignment:aFont && aFont.pointSize >= 13 ? NSTextBlockBottomAlignment : NSTextBlockMiddleAlignment font:(aFont ?: font) row:0 column:1];

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
			case '$': modifiers |= NSShiftKeyMask;      break;
			case '^': modifiers |= NSControlKeyMask;    break;
			case '~': modifiers |= NSAlternateKeyMask;  break;
			case '@': modifiers |= NSCommandKeyMask;    break;
			case '#': modifiers |= NSNumericPadKeyMask; break;
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
	[self setState:flag ? NSMixedState : NSOffState];
}

- (void)setDynamicTitle:(NSString*)plainTitle
{
	if(self.userKeyEquivalent && ![self.userKeyEquivalent isEqualToString:@""])
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
			self.attributedTitle = [[NSAttributedString alloc] initWithString:title attributes:@{ NSFontAttributeName : font }];
		}
	}
	self.title = plainTitle;
}
@end
