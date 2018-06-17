@interface GeneralViewController : NSViewController
@end

@interface TableViewController : GeneralViewController
@property (nonatomic) NSArrayController* arrayController;
@property (nonatomic) NSTableView* tableView;
- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows;
- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows showHeaderView:(BOOL)showHeaderView;
- (instancetype)initWithColumnNames:(NSArray*)columnNames visibleRows:(NSUInteger)visibleRows showHeaderView:(BOOL)showHeaderView prototype:(NSDictionary*)prototype;
@end

@interface TreeViewController : GeneralViewController
- (instancetype)initWithTreeController:(NSTreeController*)aTreeController;
@end

@interface CatalogViewController : TreeViewController
@property (nonatomic) NSOutlineView* outlineView;
@property (nonatomic) NSButton* addButton;
@property (nonatomic) NSButton* removeButton;
@end

@interface BasicProperties : TreeViewController
@property (nonatomic) NSButton* changeImageButton;
@end

@interface Properties : TreeViewController
@property (nonatomic) NSView* containerView;
@property (nonatomic) NSButton* advancedButton;
@end

@interface URLProperties : BasicProperties
@end

@interface FileProperties : BasicProperties
@end

@interface ShellProperties : BasicProperties
@end

@interface SpotlightProperties : TreeViewController
@end

@interface SqliteProperties : TreeViewController
@end

@interface RecentDocumentsProperties : TreeViewController
@end

@interface PredicateGroupProperties : TreeViewController
@end

@interface ExecDataSourceProperties : TreeViewController
@end