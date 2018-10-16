@interface FileItemTableCellView : NSTableCellView
@property (nonatomic) NSButton* openButton;
@property (nonatomic, weak) id target;
@property (nonatomic) SEL closeAction;
@end
