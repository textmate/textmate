#import <MASPreferences/MASPreferencesViewController.h>
#import <MGScopeBar/MGScopeBarDelegateProtocol.h>

@class BundlesManager;

@interface BundlesPreferences : NSViewController <MASPreferencesViewController, MGScopeBarDelegate>
{
	IBOutlet MGScopeBar* categoriesScopeBar;
	IBOutlet NSTableView* bundlesTableView;
	IBOutlet NSSearchField* searchField;
	IBOutlet NSArrayController* arrayController;
}
@property (nonatomic, readonly) NSString* viewIdentifier;
@property (nonatomic, readonly) NSImage*  toolbarItemImage;
@property (nonatomic, readonly) NSString* toolbarItemLabel;
- (IBAction)didClickBundleLink:(id)sender;
- (IBAction)filterStringDidChange:(id)sender;
@end
