#import "OakRunCommandWindowController.h"
#import <OakFoundation/OakHistoryList.h>
#import <OakFoundation/OakFoundation.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

@interface OakRunCommandWindowController () <NSWindowDelegate>
@property (nonatomic) NSTextField*         commandLabel;
@property (nonatomic) NSComboBox*          commandComboBox;
@property (nonatomic) NSTextField*         resultLabel;
@property (nonatomic) NSPopUpButton*       resultPopUpButton;
@property (nonatomic) NSButton*            executeButton;
@property (nonatomic) NSButton*            cancelButton;
@property (nonatomic) NSObjectController*  objectController;
@property (nonatomic) OakHistoryList*      commandHistoryList;
@property (nonatomic) NSMutableArray*      myConstraints;
@end

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

@implementation OakRunCommandWindowController
+ (OakRunCommandWindowController*)sharedInstance
{
	static OakRunCommandWindowController* instance = [OakRunCommandWindowController new];
	return instance;
}

- (id)init
{
	if((self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSZeroRect styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSResizableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO]]))
	{
		self.myConstraints        = [NSMutableArray new];

		self.commandLabel         = OakCreateLabel(@"Command:");
		self.commandComboBox      = OakCreateComboBox();
		self.resultLabel          = OakCreateLabel(@"Result:");
		self.resultPopUpButton    = OakCreatePopUpButton();
		self.executeButton        = OakCreateButton(@"Execute");
		self.cancelButton         = OakCreateButton(@"Cancel");

		for(NSString* title in @[ @"Replace Input", @"Insert After Input" ])
			[self.resultPopUpButton addItemWithTitle:title];

		self.executeButton.action = @selector(execute:);
		self.cancelButton.action  = @selector(cancel:);

		self.objectController     = [[NSObjectController alloc] initWithContent:self];
		self.commandHistoryList   = [[OakHistoryList alloc] initWithName:@"Filter Through Command History" stackSize:10 defaultItems:@"cat -n", nil];

		self.window.title         = @"Filter Through Command";
		self.window.delegate      = self;

		[self.commandComboBox bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.commandHistoryList.head" options:nil];
		[self.commandComboBox bind:NSContentValuesBinding toObject:_objectController withKeyPath:@"content.commandHistoryList.list" options:nil];

		NSDictionary* views = @{
			@"commandLabel" : self.commandLabel,
			@"command"      : self.commandComboBox,
			@"resultLabel"  : self.resultLabel,
			@"result"       : self.resultPopUpButton,
			@"execute"      : self.executeButton,
			@"cancel"       : self.cancelButton,
		};

		NSView* contentView = self.window.contentView;
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[contentView addSubview:view];
		}

		CONSTRAINT(@"H:|-(>=20,==20@75)-[commandLabel]-[command(>=250)]-|", NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:|-(>=20,==20@75)-[resultLabel]-[result]-(>=20)-|", NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:|-(>=20)-[cancel]-[execute]-|", NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"V:|-[command]-[result]", NSLayoutFormatAlignAllLeft);
		CONSTRAINT(@"V:[result]-[execute]-|", 0);

		[self.window.contentView addConstraints:_myConstraints];

		NSView* keyViewLoop[] = { self.commandComboBox, self.resultPopUpButton, self.cancelButton, self.executeButton };
		for(size_t i = 0; i < sizeofA(keyViewLoop); ++i)
			keyViewLoop[i].nextKeyView = keyViewLoop[(i + 1) % sizeofA(keyViewLoop)];

		self.window.initialFirstResponder = self.commandComboBox;
		self.window.defaultButtonCell     = self.executeButton.cell;
	}
	return self;
}

- (IBAction)execute:(id)sender
{
	NSLog(@"%s", sel_getName(_cmd));

	if([self.objectController commitEditing])
	{
		NSString* command = self.commandComboBox.stringValue;
		if(NSNotEmptyString(command))
			NSLog(@"%s run ‘%@’", sel_getName(_cmd), command);
	}
	[self close];
}

- (IBAction)cancel:(id)sender
{
	NSLog(@"%s", sel_getName(_cmd));
	[self close];
}
@end
