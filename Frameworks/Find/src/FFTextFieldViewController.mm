#import "FFTextFieldViewController.h"
#import <OakAppKit/OakSyntaxFormatter.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakPasteboardSelector.h>
#import <OakAppKit/OakUIConstructionFunctions.h>

// ==========================
// = OakAutoSizingTextField =
// ==========================

@interface OakAutoSizingTextField : NSTextField
@property (nonatomic) NSSize myIntrinsicContentSize;
@end

@implementation OakAutoSizingTextField
- (NSSize)intrinsicContentSize
{
	return NSEqualSizes(self.myIntrinsicContentSize, NSZeroSize) ? [super intrinsicContentSize] : self.myIntrinsicContentSize;
}

- (void)updateIntrinsicContentSizeToEncompassString:(NSString*)aString
{
	NSTextFieldCell* cell = [self.cell copy];
	cell.stringValue = aString ?: @"";

	self.myIntrinsicContentSize = NSMakeSize(NSViewNoIntrinsicMetric, MAX(22, MIN([cell cellSizeForBounds:NSMakeRect(0, 0, NSWidth([self bounds]), CGFLOAT_MAX)].height, 225)));
	[self invalidateIntrinsicContentSize];
}
@end

// =============================
// = FFTextFieldViewController =
// =============================

static void* kFirstResponderContext = &kFirstResponderContext;

@interface FFTextFieldViewController () <NSTextFieldDelegate, NSTextStorageDelegate, NSPopoverDelegate>
{
	OakAutoSizingTextField* _textField;
	OakSyntaxFormatter*     _syntaxFormatter;
	OakPasteboard*          _pasteboard;
	NSString*               _grammarName;
	NSPopover*              _popover;
}
@end

@implementation FFTextFieldViewController
- (instancetype)initWithPasteboard:(OakPasteboard*)pasteboard grammarName:(NSString*)grammarName
{
	if(self = [self initWithNibName:nil bundle:nil])
	{
		_pasteboard  = pasteboard;
		_grammarName = grammarName;
	}
	return self;
}

- (void)showHistory:(id)sender
{
	if(!OakPasteboardSelector.sharedInstance.window.isVisible)
		[_pasteboard selectItemForControl:_textField];
}

- (void)showPopoverWithString:(NSString*)aString
{
	if(aString)
	{
		if(!_popover)
		{
			NSViewController* viewController = [[NSViewController alloc] init];
			viewController.view = OakCreateLabel();

			_popover = [[NSPopover alloc] init];
			_popover.behavior = NSPopoverBehaviorTransient;
			_popover.contentViewController = viewController;
			_popover.delegate = self;
		}

		NSTextField* textField = (NSTextField*)_popover.contentViewController.view;
		textField.stringValue = aString;
		[textField sizeToFit];

		[_popover showRelativeToRect:NSZeroRect ofView:_textField preferredEdge:NSMaxYEdge];
	}
	else
	{
		[_popover close];
		_popover = nil;
	}
}

- (void)popoverDidClose:(NSNotification*)aNotification
{
	_popover = nil;
}

- (void)setSyntaxHighlightEnabled:(BOOL)flag
{
	if(_syntaxHighlightEnabled == flag)
		return;

	_syntaxHighlightEnabled  = flag;
	_syntaxFormatter.enabled = flag;

	// Re-format current value
	if(!_textField.currentEditor)
	{
		NSString* currentString = [_textField.stringValue copy];
		_textField.objectValue = nil;
		_textField.objectValue = currentString;
	}

	[self addStylesToFieldEditor];
}

- (OakSyntaxFormatter*)syntaxFormatter
{
	if(!_syntaxFormatter)
		_syntaxFormatter = [[OakSyntaxFormatter alloc] initWithGrammarName:_grammarName];
	return _syntaxFormatter;
}

- (OakAutoSizingTextField*)textField
{
	if(!_textField)
	{
		_textField = [[OakAutoSizingTextField alloc] initWithFrame:NSZeroRect];
		_textField.font       = OakControlFont();
		_textField.formatter  = self.syntaxFormatter;
		_textField.delegate   = self;
		_textField.cell.wraps = YES;
	}
	return _textField;
}

- (void)loadView
{
	self.view = self.textField;
}

- (void)viewDidAppear
{
	[self.view.window addObserver:self forKeyPath:@"firstResponder" options:0 context:kFirstResponderContext];
	[self.textField bind:NSValueBinding toObject:self withKeyPath:@"stringValue" options:@{ NSContinuouslyUpdatesValueBindingOption: @YES }];
}

- (void)viewWillDisappear
{
	[self.textField unbind:NSValueBinding];
	[self.view.window removeObserver:self forKeyPath:@"firstResponder" context:kFirstResponderContext];
}

- (void)setStringValue:(NSString*)newStringValue
{
	if([_stringValue isEqualToString:newStringValue])
		return;
	_stringValue = newStringValue;
	[self showPopoverWithString:nil];
	[_textField updateIntrinsicContentSizeToEncompassString:newStringValue];

	if(NSDictionary* info = [self infoForBinding:@"stringValue"])
	{
		id controller     = info[NSObservedObjectKey];
		NSString* keyPath = info[NSObservedKeyPathKey];
		if(controller && controller != [NSNull null] && keyPath && (id)keyPath != [NSNull null])
		{
			id oldValue = [controller valueForKeyPath:keyPath];
			if(!oldValue || ![oldValue isEqual:newStringValue])
				[controller setValue:newStringValue forKeyPath:keyPath];
		}
	}
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if(context == kFirstResponderContext)
	{
		id firstResponder = self.view.window.firstResponder;
		self.hasFocus = firstResponder == _textField || firstResponder == _textField.currentEditor;
	}
}

- (void)setHasFocus:(BOOL)flag
{
	_hasFocus = flag;
	if(_hasFocus && [_textField.currentEditor isKindOfClass:[NSTextView class]])
	{
		NSTextView* textView = (NSTextView*)_textField.currentEditor;
		textView.textStorage.delegate = self;
		[self addStylesToFieldEditor];
	}
}

- (BOOL)control:(NSControl*)control textView:(NSTextView*)textView doCommandBySelector:(SEL)command
{
	if(command == @selector(moveDown:))
	{
		NSRange lastNewline    = [textView.string rangeOfString:@"\n" options:NSBackwardsSearch];
		NSRange insertionPoint = textView.selectedRanges.lastObject.rangeValue;

		if(lastNewline.location == NSNotFound || lastNewline.location < NSMaxRange(insertionPoint))
			return [self showHistory:self], YES;
	}
	return NO;
}

- (void)textStorageDidProcessEditing:(NSNotification*)aNotification
{
	[self addStylesToFieldEditor];
}

- (void)addStylesToFieldEditor
{
	[_syntaxFormatter addStylesToString:((NSTextView*)_textField.currentEditor).textStorage];
}
@end
