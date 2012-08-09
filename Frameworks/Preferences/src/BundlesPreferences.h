#import <MASPreferences/MASPreferencesViewController.h>
#import <MGScopeBar/MGScopeBarDelegateProtocol.h>
#import <updater/updater.h>

@class BundlesManager;

@interface BundlesPreferences : NSViewController <MASPreferencesViewController, MGScopeBarDelegate>
{
	IBOutlet MGScopeBar* categoriesScopeBar;
	IBOutlet NSTableView* bundlesTableView;
	IBOutlet NSTextField* activityTextField;
	BundlesManager* bundlesManager;

	std::vector<std::string> categories;
	std::set<std::string> enabledCategories;
	std::vector<bundles_db::bundle_ptr> bundles;
}
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, readonly) NSImage*  toolbarItemImage;
@property (nonatomic, readonly) NSString* toolbarItemLabel;
@end
