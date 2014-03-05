#import <MASPreferences/MASPreferencesViewController.h>

@interface VariablesPreferences : NSViewController <MASPreferencesViewController>
{
	IBOutlet NSTableView* variablesTableView;
	BOOL canRemove;
}
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic, readonly) NSImage*  toolbarItemImage;
@property (nonatomic, readonly) NSString* toolbarItemLabel;

@property (nonatomic) BOOL canRemove;

- (IBAction)addVariable:(id)sender;
- (IBAction)delete:(id)sender;
@end
