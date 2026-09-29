#import "OakRunCommandWindowController.h"
#import <OakFoundation/src/OakHistoryList.h>
#import <OakFoundation/src/OakFoundation.h>
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <OakTextView/src/OakTextView.h>

static NSString* const kUserDefaultsFilterOutputType = @"filterOutputType";

@interface OakRunCommandWindowController () <NSWindowDelegate>
@property (nonatomic) NSTextField*               commandLabel;
@property (nonatomic) NSComboBox*                commandComboBox;
@property (nonatomic) NSTextField*               resultLabel;
@property (nonatomic) NSPopUpButton*             resultPopUpButton;
@property (nonatomic) NSButton*                  executeButton;
@property (nonatomic) NSButton*                  cancelButton;
@property (nonatomic) NSObjectController*        objectController;
@property (nonatomic) OakHistoryList<NSString*>* commandHistoryList;
@property (nonatomic) NSMutableArray*            myConstraints;
@property (nonatomic) output::type               outputType;
@end

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [_myConstraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

@implementation OakRunCommandWindowController
+ (instancetype)sharedInstance
{
	static OakRunCommandWindowController* sharedInstance = [self new];
	return sharedInstance;
}

- (id)init
{
	if((self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSZeroRect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskResizable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO]]))
	{
		self.outputType           = output::replace_input;
		self.myConstraints        = [NSMutableArray new];

		self.commandLabel         = OakCreateLabel(@"Command:", nil, NSTextAlignmentRight);
		self.commandComboBox      = OakCreateComboBox();
		self.resultLabel          = OakCreateLabel(@"Result:", nil, NSTextAlignmentRight);
		self.resultPopUpButton    = OakCreatePopUpButton();
		self.executeButton        = OakCreateButton(@"Execute");
		self.cancelButton         = OakCreateButton(@"Cancel");

		struct { NSString* title; NSString* keyEquivalent; NSInteger outputOption; } outputOptions[] =
		{
			{ @"Replace Input",      @"1", output::replace_input },
			{ @"Insert After Input", @"2", output::after_input   },
			{ @"New Document",       @"3", output::new_window    },
			{ @"Tool Tip",           @"4", output::tool_tip      },
		};

		NSMenu* menu = [self.resultPopUpButton menu];
		[menu removeAllItems];
		for(auto const& info : outputOptions)
			[[menu addItemWithTitle:info.title action:@selector(takeOutputTypeFrom:) keyEquivalent:info.keyEquivalent] setTag:info.outputOption];

		self.executeButton.action = @selector(execute:);
		self.cancelButton.action  = @selector(cancel:);

		self.objectController     = [[NSObjectController alloc] initWithContent:self];
		self.commandHistoryList   = [[OakHistoryList alloc] initWithName:@"Filter Through Command History" stackSize:10 defaultItems:@"sort|uniq -c", @"seq 100", @"cat -n", nil];

		self.window.title         = @"Filter Through Command";
		self.window.delegate      = self;

		[self.commandComboBox bind:NSValueBinding         toObject:_objectController withKeyPath:@"content.commandHistoryList.head" options:nil];
		[self.commandComboBox bind:NSContentValuesBinding toObject:_objectController withKeyPath:@"content.commandHistoryList.list" options:nil];

		[NSNotificationCenter.defaultCenter addObserver:self selector:@selector(commandChanged:) name:NSControlTextDidChangeNotification object:self.commandComboBox];
		[self commandChanged:nil];

		NSDictionary* views = @{
			@"commandLabel": self.commandLabel,
			@"command":      self.commandComboBox,
			@"resultLabel":  self.resultLabel,
			@"result":       self.resultPopUpButton,
			@"execute":      self.executeButton,
			@"cancel":       self.cancelButton,
		};

		NSView* contentView = self.window.contentView;
		OakAddAutoLayoutViewsToSuperview([views allValues], contentView);

		CONSTRAINT(@"H:|-[commandLabel]-[command(>=250)]-|", NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:|-[resultLabel(==commandLabel)]-[result]-(>=20)-|", NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"H:|-(>=20)-[cancel]-[execute]-|", NSLayoutFormatAlignAllBaseline);
		CONSTRAINT(@"V:|-[command]-[result]", NSLayoutFormatAlignAllLeft);
		CONSTRAINT(@"V:[result]-[execute]-|", 0);

		[self.window.contentView addConstraints:_myConstraints];
		self.window.defaultButtonCell = self.executeButton.cell;

		self.outputType = (output::type)[NSUserDefaults.standardUserDefaults integerForKey:kUserDefaultsFilterOutputType];
	}
	return self;
}

- (void)takeOutputTypeFrom:(id)sender
{
	self.outputType = (output::type)[sender tag];
}

- (void)setOutputType:(output::type)newOutputType
{
	if(_outputType == newOutputType)
		return;

	_outputType = newOutputType;
	[self.resultPopUpButton selectItemWithTag:_outputType];

	if(_outputType)
			[NSUserDefaults.standardUserDefaults setInteger:_outputType forKey:kUserDefaultsFilterOutputType];
	else	[NSUserDefaults.standardUserDefaults removeObjectForKey:kUserDefaultsFilterOutputType];
}

- (void)commandChanged:(NSNotification*)notification
{
	self.executeButton.enabled = OakNotEmptyString([self.commandComboBox.stringValue stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]]);
}

- (IBAction)execute:(id)sender
{
	if(![self.objectController commitEditing])
		return;

	NSString* command = self.commandComboBox.stringValue;
	if(id textView = [NSApp targetForAction:@selector(filterDocumentThroughCommand:input:output:)])
		[textView filterDocumentThroughCommand:command input:input::selection output:self.outputType];

	[self close];
}

- (IBAction)cancel:(id)sender
{
	[self close];
}
@end
