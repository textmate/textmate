#import "OakPreview.h"
#import <Quartz/Quartz.h>

@interface OakPreviewItemHelper : NSObject//<QLPreviewItem>
{
	NSURL* previewItemURL;
}
@property (nonatomic, retain) NSURL* previewItemURL;
@end

@implementation OakPreviewItemHelper
@synthesize previewItemURL;

- (id)initWithURL:(NSURL*)aURL
{
	if((self = [super init]))
		self.previewItemURL = aURL;
	return self;
}

- (void)dealloc
{
	self.previewItemURL = nil;
	[super dealloc];
}
@end

@interface OakPreviewDelegateHelper : NSObject <QLPreviewPanelDelegate, QLPreviewPanelDataSource>
{
	NSArray* items;
}
@property (nonatomic, retain) NSArray* items;
@end

@implementation OakPreviewDelegateHelper
@synthesize items;

- (id)initWithItems:(NSArray*)someItems
{
	if((self = [super init]))
		self.items = someItems;
	return self;
}

- (void)dealloc
{
	self.items = nil;
	[super dealloc];
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	id panel = [aNotification object];
	[panel setDelegate:nil];
	[panel setDataSource:nil];
	[self autorelease];
}

- (NSInteger)numberOfPreviewItemsInPreviewPanel:(QLPreviewPanel*)panel
{
	return [items count];
}

- (id <QLPreviewItem>)previewPanel:(QLPreviewPanel*)panel previewItemAtIndex:(NSInteger)index
{
	return [items objectAtIndex:index];
}
@end

PUBLIC void OakShowPreviewForURLs (NSArray* someURLs)
{
	if(QLPreviewPanel* panel = [QLPreviewPanel sharedPreviewPanel])
	{
		// FIXME one is not allowed to set datasource/delegate — instead we need to be “first responder” when the QL preview panel opens
		OakPreviewDelegateHelper* helper = [[OakPreviewDelegateHelper alloc] initWithItems:someURLs];
		[panel setDataSource:helper];
		[panel setDelegate:helper];
		[panel makeKeyAndOrderFront:nil];
	}
}
