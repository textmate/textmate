#import <oak/debug.h>

@class OakFilterListView;

extern PUBLIC NSString* const FLDataSourceItemsDidChangeNotification;
extern PUBLIC NSString* const FLDataSourceItemsShouldDescendNotification;
extern PUBLIC NSString* const FLDataSourceItemsShouldAscendNotification;

@protocol FilterListDataSource <NSObject>
// This will be set as the window title and observed for changes.
- (NSString*)title;

// Returns a view controller for which a “filter string view” can be created
// The data source will post FLDataSourceItemsDidChangeNotification if values are changed via the view controller
- (NSViewController*)viewController;

// Returns an array of identifiers for all of the items matching the given filter string (which may be empty)
// If -displayStringForItem: is not implemented, the array elements should be valid strings for display
- (NSArray*)items;

@optional

- (NSButtonCell*)accessoryButton;

// If implemented and returns true, the filter view will attempt to preserve the currently selected items when reloading data
// (e.g. when the user changes the filter string).
- (BOOL)preservesSelectionWhenFiltering;

// If implemented and returns true, the user will be able to select (and accept) multiple items in the list.
- (BOOL)allowsMultipleSelection;

// If implemented, the NSTextFieldCell subclass returned will be used to display items.
- (NSTextFieldCell*)itemDataCell;

// This method can be used to set up custom itemDataCell properties prior to display.
- (void)willDisplayCell:(NSTextFieldCell*)aCell forItem:(id)anItem;

// If implemented, returns the text to display in the list for the given item (an NSString or NSAttributedString)
// If not implemented, the item is assumed to be a valid string for display
- (NSAttributedString*)displayStringForItem:(id)anItem;

// If implemented the returned string will be displayed in the status bar when the item is selected
- (NSAttributedString*)infoStringForItem:(id)anItem;

// Return YES if there may be more items to come
- (BOOL)moreItemsToCome;

// Instructs the data source to block until all items are available
- (void)waitForAllItems;

// Instructs the data source to stop loading new items
- (void)stopLoading;

// Allows the data source to associate a selected item with a filter string
- (void)makeItemsBestFitForCurrentSearch:(NSArray*)items;

- (void)descendIntoItem:(id)anItem;
@end

PUBLIC @interface OakFilterWindowController : NSWindowController
{
	OBJC_WATCH_LEAKS(OakFilterWindowController);
	IBOutlet OakFilterListView* filterView;
	IBOutlet NSView* filterControlsView;
}
+ (id)filterWindow;
- (void)showWindowRelativeToWindow:(NSWindow*)parentWindow;
@property (nonatomic, retain) id <FilterListDataSource> dataSource;
@property (nonatomic, retain) id target;
@property (nonatomic, assign) SEL action;
@property (nonatomic, assign) SEL accessoryAction;
@property (nonatomic, assign) BOOL sendActionOnSingleClick;
@property (nonatomic, assign) BOOL allowsMultipleSelection;
@property (nonatomic, readonly) NSArray* selectedItems;
@end
