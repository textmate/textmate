@interface GenieTableViewController : NSViewController
@property (nonatomic) NSArrayController* arrayController;
@property (nonatomic) NSTableView* tableView;
- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows;
- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows showHeaderView:(BOOL)showHeaderView;
- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows showHeaderView:(BOOL)showHeaderView prototype:(NSDictionary*)prototype;
@end

