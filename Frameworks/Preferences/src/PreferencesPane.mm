#import "PreferencesPane.h"
#import <OakFoundation/src/NSString Additions.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <ns/src/ns.h>
#import <settings/src/settings.h>

NSView* OakSetupGridViewWithSeparators (NSGridView* gridView, std::vector<NSUInteger> rows)
{
	gridView.rowAlignment = NSGridRowAlignmentFirstBaseline;
	gridView.rowSpacing   = 8;

	[gridView rowAtIndex:0].topPadding                                  = 20;
	[gridView rowAtIndex:gridView.numberOfRows-1].bottomPadding         = 20;
	[gridView columnAtIndex:0].xPlacement                               = NSGridCellPlacementTrailing;
	[gridView columnAtIndex:0].leadingPadding                           = 8;
	[gridView columnAtIndex:0].width                                    = 200;
	[gridView columnAtIndex:gridView.numberOfColumns-1].trailingPadding = 8;
	[gridView columnAtIndex:gridView.numberOfColumns-1].width           = 400;

	for(NSUInteger row = 0; row < gridView.numberOfRows; ++row)
		[gridView cellAtColumnIndex:0 rowIndex:row].yPlacement = NSGridCellPlacementNone;

	for(NSUInteger row : rows)
	{
		[gridView mergeCellsInHorizontalRange:NSMakeRange(0, gridView.numberOfColumns) verticalRange:NSMakeRange(row, 1)];
		[gridView cellAtColumnIndex:0 rowIndex:row].contentView = OakCreateNSBoxSeparator();
		[gridView cellAtColumnIndex:0 rowIndex:row].xPlacement  = NSGridCellPlacementFill;
		[gridView cellAtColumnIndex:0 rowIndex:row].yPlacement  = NSGridCellPlacementCenter;
		[gridView rowAtIndex:row].topPadding    = 12;
		[gridView rowAtIndex:row].bottomPadding = 12;
		[gridView rowAtIndex:row].rowAlignment  = NSGridRowAlignmentNone;
	}

	[gridView setContentHuggingPriority:NSLayoutPriorityDefaultHigh-2 forOrientation:NSLayoutConstraintOrientationVertical];
	gridView.frame = { .size = gridView.fittingSize };

	return gridView;
}

@interface PreferencesPane ()
@property (nonatomic, readwrite) NSImage* toolbarItemImage;
@end

@implementation PreferencesPane
- (id)initWithNibName:(NSNibName)aNibName label:(NSString*)aLabel image:(NSImage*)anImage
{
	if(self = [super initWithNibName:aNibName bundle:[NSBundle bundleForClass:[self class]]])
	{
		self.identifier   = aLabel;
		self.title        = aLabel;
		_toolbarItemImage = anImage;
	}
	return self;
}

- (void)setValue:(id)newValue forUndefinedKey:(NSString*)aKey
{
	if(NSString* key = [_defaultsProperties objectForKey:aKey])
	{
		return [NSUserDefaults.standardUserDefaults setObject:newValue forKey:key];
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
		return [NSUserDefaults.standardUserDefaults objectForKey:key];
	else if(NSString* key = [_tmProperties objectForKey:aKey])
		return [NSString stringWithCxxString:settings_t::raw_get(to_s(key))];
	return [super valueForUndefinedKey:aKey];
}

- (IBAction)help:(id)sender
{
	NSString* anchor = [sender isKindOfClass:[NSButton class]] ? [sender alternateTitle] : nil;
	if(anchor)
		[NSHelpManager.sharedHelpManager openHelpAnchor:anchor inBook:[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleHelpBookName"]];
}
@end
