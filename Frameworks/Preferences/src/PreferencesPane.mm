#import "PreferencesPane.h"
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <ns/ns.h>
#import <settings/settings.h>

NSView* OakSetupGridViewWithSeparators (NSGridView* gridView, std::vector<NSUInteger> rows)
{
	gridView.rowAlignment = NSGridRowAlignmentFirstBaseline;
	[gridView rowAtIndex:0].topPadding                                  = 20;
	[gridView rowAtIndex:gridView.numberOfRows-1].bottomPadding         = 20;
	[gridView columnAtIndex:0].xPlacement                               = NSGridCellPlacementTrailing;
	[gridView columnAtIndex:0].leadingPadding                           = 8;
	[gridView columnAtIndex:0].width                                    = 200;
	[gridView columnAtIndex:gridView.numberOfColumns-1].trailingPadding = 8;
	[gridView columnAtIndex:gridView.numberOfColumns-1].width           = 400;

	for(NSUInteger row : rows)
	{
		[gridView mergeCellsInHorizontalRange:NSMakeRange(0, gridView.numberOfColumns) verticalRange:NSMakeRange(row, 1)];
		[gridView cellAtColumnIndex:0 rowIndex:row].contentView = OakCreateNSBoxSeparator();
		[gridView cellAtColumnIndex:0 rowIndex:row].xPlacement = NSGridCellPlacementFill;
		[gridView cellAtColumnIndex:0 rowIndex:row].yPlacement = NSGridCellPlacementCenter;
		[gridView rowAtIndex:row].topPadding = 8;
		[gridView rowAtIndex:row].bottomPadding = 8;
		[gridView rowAtIndex:row].rowAlignment = NSGridRowAlignmentNone;
	}

	NSSize size = gridView.fittingSize;
	gridView.frame = { .size = size };
	[gridView addConstraints:@[
		[NSLayoutConstraint constraintWithItem:gridView attribute:NSLayoutAttributeWidth  relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:size.width],
		[NSLayoutConstraint constraintWithItem:gridView attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:size.height],
	]];

	return gridView;
}

@interface PreferencesPane ()
@property (nonatomic, readwrite) NSString* toolbarItemLabel;
@property (nonatomic, readwrite) NSImage*  toolbarItemImage;
@end

@implementation PreferencesPane
- (NSString*)viewIdentifier { return _toolbarItemLabel; }

- (id)initWithNibName:(NSString*)aNibName label:(NSString*)aLabel image:(NSImage*)anImage
{
	if(self = [super initWithNibName:aNibName bundle:[NSBundle bundleForClass:[self class]]])
	{
		_toolbarItemLabel = aLabel;
		_toolbarItemImage = anImage;
	}
	return self;
}

- (void)setValue:(id)newValue forUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [_defaultsProperties objectForKey:aKey])
	{
		return [[NSUserDefaults standardUserDefaults] setObject:newValue forKey:key];
	}
	else if(NSString* key = [_tmProperties objectForKey:aKey])
	{
		newValue = newValue ?: @"";
		if([newValue isKindOfClass:[NSString class]])
			return settings_t::set(to_s(key), to_s(newValue));
		NSLog(@"%s wrong type for %@: ‘%@’", sel_getName(_cmd), aKey, newValue);
	}
	[super setValue:newValue forUndefinedKey:aKey];
}

- (id)valueForUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [_defaultsProperties objectForKey:aKey])
		return [[NSUserDefaults standardUserDefaults] objectForKey:key];
	else if(NSString* key = [_tmProperties objectForKey:aKey])
		return [NSString stringWithCxxString:settings_t::raw_get(to_s(key))];
	return [super valueForUndefinedKey:aKey];
}

- (IBAction)help:(id)sender
{
	NSString* anchor = [sender isKindOfClass:[NSButton class]] ? [sender alternateTitle] : nil;
	if(anchor)
		[[NSHelpManager sharedHelpManager] openHelpAnchor:anchor inBook:[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleHelpBookName"]];
}
@end
