@interface TreeViewController : NSViewController
- (instancetype)initWithTreeController:(NSTreeController*)aTreeController;
@end

@interface BasicProperties : TreeViewController
@property (nonatomic) NSButton* changeImageButton;
@end

@interface URLProperties : BasicProperties
@end

@interface FileProperties : BasicProperties
@end

@interface ShellProperties : BasicProperties
@end

@interface SpotlightProperties : TreeViewController
@end

@interface SQLiteProperties : TreeViewController
@end

@interface RecentDocumentsProperties : TreeViewController
@end

@interface PredicateGroupProperties : TreeViewController
@end

@interface ExecDataSourceProperties : TreeViewController
@end
