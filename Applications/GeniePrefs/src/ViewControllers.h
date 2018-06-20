@interface GeneralViewController : NSViewController
@end

@interface TreeViewController : GeneralViewController
- (instancetype)initWithTreeController:(NSTreeController*)aTreeController;
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
