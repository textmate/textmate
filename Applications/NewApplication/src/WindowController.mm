#import "WindowController.h"
#import "ViewController.h"

@interface WindowController () <NSWindowDelegate>
@property (nonatomic) NSWindowController* retainedSelf;
@property (nonatomic) ViewController* northEastViewController;
@property (nonatomic) ViewController* northWestViewController;
@property (nonatomic) ViewController* southEastViewController;
@property (nonatomic) ViewController* southWestViewController;
@end

@implementation WindowController
+ (void)initialize
{
	NSWindow.allowsAutomaticWindowTabbing = NO;
}

- (instancetype)init
{
	if(self = [self initWithWindow:[[NSWindow alloc] initWithContentRect:NSZeroRect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskResizable|NSWindowStyleMaskClosable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO]])
	{
		_retainedSelf = self;

		self.northEastViewController = [[ViewController alloc] init];
		self.northWestViewController = [[ViewController alloc] init];
		self.southEastViewController = [[ViewController alloc] init];
		self.southWestViewController = [[ViewController alloc] init];

		NSWindow* window = self.window;
		window.title    = NSProcessInfo.processInfo.processName;
		window.delegate = self;

		NSDictionary* views = @{
			@"northEast": self.northEastViewController.view,
			@"northWest": self.northWestViewController.view,
			@"southEast": self.southEastViewController.view,
			@"southWest": self.southWestViewController.view,
		};

		NSView* contentView = window.contentView;
		for(NSView* view in views.allValues)
		{
			view.translatesAutoresizingMaskIntoConstraints = NO;
			[contentView addSubview:view];
		}

		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[northEast(==northWest@55)]-[northWest]-|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[southEast(==southWest@55)]-[southWest]-|" options:NSLayoutFormatAlignAllTop|NSLayoutFormatAlignAllBottom metrics:nil views:views]];
		[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[northEast(==southEast@55)]-[southEast]-|" options:0 metrics:nil views:views]];

		[window layoutIfNeeded];
		[window center];

		window.frameAutosaveName = @"Main";
	}
	return self;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	_retainedSelf = nil;
	[NSApp performSelector:@selector(terminate:) withObject:nil afterDelay:0];
}
@end
