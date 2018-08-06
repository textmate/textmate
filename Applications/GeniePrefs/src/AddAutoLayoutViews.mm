#import "AddAutoLayoutViews.h"
#import <os/log.h>

void GenieAddAutoLayoutViewsToSuperview (NSDictionary* views, NSView* superview)
{
	for(NSString* key in [views allKeys])
	{
		__kindof NSView* view = views[key];
		if([view isEqual:[NSNull null]])
			continue;

		if([key hasSuffix:@"Label"] && [view isKindOfClass:[NSTextField class]])
		{
			[view setAlignment:NSTextAlignmentRight];
			[view setContentHuggingPriority:NSLayoutPriorityDefaultLow+1 forOrientation:NSLayoutConstraintOrientationHorizontal];
			[view setContentCompressionResistancePriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
		}

		[view setTranslatesAutoresizingMaskIntoConstraints:NO];
		[superview addSubview:view];
	}
}

void GenieSetupKeyViewLoop (NSArray<__kindof NSResponder*>* superviews, BOOL setFirstResponder)
{
	std::set<id> seen;
	for(NSView* candidate in superviews)
		seen.insert(candidate);

	NSMutableArray* views = [NSMutableArray new];
	for(NSView* view in superviews)
	{
		if([view isEqual:[NSNull null]])
			continue;

		[views addObject:view];
		NSView* subview = view;
		while((subview = subview.nextKeyView) && [subview isDescendantOf:view] && seen.insert(subview).second)
			[views addObject:subview];
	}

	if(views.count == 1)
	{
		[views.firstObject setNextKeyView:nil];
	}
	else
	{
		for(size_t i = 0; i < views.count; ++i)
			[views[i] setNextKeyView:views[(i + 1) % views.count]];
	}

	if(setFirstResponder)
	{
		if(NSView* view = views.firstObject)
			view.window.initialFirstResponder = view;
	}
}

NSScrollView* GenieCreateTextView (BOOL editable)
{
	NSTextView* textView = [[NSTextView alloc] initWithFrame:NSZeroRect];

	textView.autoresizingMask      = NSViewWidthSizable|NSViewHeightSizable;
	textView.horizontallyResizable = YES;
	textView.verticallyResizable   = YES;

	textView.allowsUndo            = editable;
	textView.editable              = editable;
	textView.richText              = NO;
	textView.usesFindBar           = YES;

	textView.automaticQuoteSubstitutionEnabled  = NO;
	textView.automaticDashSubstitutionEnabled   = NO;
	textView.automaticTextReplacementEnabled    = NO;
	textView.automaticSpellingCorrectionEnabled = NO;

	NSScrollView* scrollView = [[NSScrollView alloc] initWithFrame:NSZeroRect];
	scrollView.hasVerticalScroller = YES;
	scrollView.autohidesScrollers  = YES;
	scrollView.borderType          = NSBezelBorder;
	scrollView.documentView        = textView;

	NSDictionary* fontAttributes = @{
		NSFontAttributeName : [NSFont userFixedPitchFontOfSize:0],
	};

	NSAttributedString* content = [[NSAttributedString alloc] initWithString:@"\n\n\n\n" attributes:fontAttributes];
	[textView.textStorage setAttributedString:content];

	[textView sizeToFit];
	CGFloat height = MAX(NSHeight(textView.frame), content.size.height);

	NSLayoutConstraint* heightConstraint = [NSLayoutConstraint constraintWithItem:scrollView attribute:NSLayoutAttributeHeight relatedBy:NSLayoutRelationGreaterThanOrEqual toItem:nil attribute:NSLayoutAttributeNotAnAttribute multiplier:1 constant:height];
	[scrollView addConstraint:heightConstraint];

	return scrollView;
}

#define keyFileSender   'FSnd'
#define kODBEditorSuite 'R*ch'
#define kAEModifiedFile 'FMod'
#define kAEClosedFile   'FCls'

@interface TextViewController ()
{
	NSURL* _temporaryURL;
	NSUInteger _didSetupODBEventHandlers;
	BOOL _editable;
}
@end

@implementation TextViewController
+ (instancetype)textViewController
{
	return [[TextViewController alloc] initAsEditable:YES];
}

+ (instancetype)readOnlyTextViewController
{
	return [[TextViewController alloc] initAsEditable:NO];
}

- (instancetype)initAsEditable:(BOOL)editableFlag
{
	if(self = [super init])
	{
		_editable = editableFlag;
	}
	return self;
}

- (void)loadView
{
	NSScrollView* scrollView = GenieCreateTextView(_editable);
	self.view     = scrollView;
	self.textView = scrollView.documentView;
}

// ===========================
// = Edit in External Editor =
// ===========================

- (NSString*)editorBundleIdentifier
{
	return [[NSUserDefaults standardUserDefaults] stringForKey:@"editorBundleIdentifier"] ?: @"com.macromates.TextMate";
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	if(aMenuItem.action == @selector(editInTextMate:))
	{
		if(NSURL* url = [NSWorkspace.sharedWorkspace URLForApplicationWithBundleIdentifier:self.editorBundleIdentifier])
		{
			if(NSString* displayName = [NSFileManager.defaultManager displayNameAtPath:url.path])
				aMenuItem.title = [NSString stringWithFormat:@"Edit in %@", displayName];
		}
	}
	return YES;
}

- (void)setupODBEventHandlers
{
	if(++_didSetupODBEventHandlers == 1)
	{
		[NSAppleEventManager.sharedAppleEventManager setEventHandler:self andSelector:@selector(handleModifiedFileEvent:withReplyEvent:) forEventClass:kODBEditorSuite andEventID:kAEModifiedFile];
		[NSAppleEventManager.sharedAppleEventManager setEventHandler:self andSelector:@selector(handleClosedFileEvent:withReplyEvent:) forEventClass:kODBEditorSuite andEventID:kAEClosedFile];
	}
}

- (void)removeODBEventHandlers
{
	if(--_didSetupODBEventHandlers == 0)
	{
		[NSAppleEventManager.sharedAppleEventManager removeEventHandlerForEventClass:kODBEditorSuite andEventID:kAEModifiedFile];
		[NSAppleEventManager.sharedAppleEventManager removeEventHandlerForEventClass:kODBEditorSuite andEventID:kAEClosedFile];
	}
}

- (BOOL)launchAppWithBundleIdentifier:(NSString*)bundleIdentifier
{
	NSArray* runningApps = [NSRunningApplication runningApplicationsWithBundleIdentifier:bundleIdentifier];
	return runningApps.count > 0 || [NSWorkspace.sharedWorkspace launchAppWithBundleIdentifier:bundleIdentifier options:NSWorkspaceLaunchDefault additionalEventParamDescriptor:nil launchIdentifier:nil];
}

- (void)editInTextMate:(id)sender
{
	if(![self launchAppWithBundleIdentifier:self.editorBundleIdentifier])
	{
		os_log_error(OS_LOG_DEFAULT, "Unable to launch application with bundle identifier: %{public}@", self.editorBundleIdentifier);
		return NSBeep();
	}

	[self setupODBEventHandlers];

	_temporaryURL = [NSURL fileURLWithPath:[NSTemporaryDirectory() stringByAppendingPathComponent:[[NSUUID UUID] UUIDString]]];
	[self.textView.textStorage.string writeToURL:_temporaryURL atomically:YES encoding:NSUTF8StringEncoding error:nil];

	NSAppleEventDescriptor* targetDescriptor = [NSAppleEventDescriptor descriptorWithDescriptorType:typeApplicationBundleID data:[self.editorBundleIdentifier dataUsingEncoding:NSUTF8StringEncoding]];
	NSAppleEventDescriptor* appleEvent = [NSAppleEventDescriptor appleEventWithEventClass:kCoreEventClass eventID:kAEOpenDocuments targetDescriptor:targetDescriptor returnID:kAutoGenerateReturnID transactionID:kAnyTransactionID];
	[appleEvent setParamDescriptor:[NSAppleEventDescriptor descriptorWithDescriptorType:typeFileURL data:[_temporaryURL.absoluteString dataUsingEncoding:NSUTF8StringEncoding]] forKeyword:keyDirectObject];
	[appleEvent setParamDescriptor:[NSAppleEventDescriptor descriptorWithDescriptorType:typeApplicationBundleID data:[NSBundle.mainBundle.bundleIdentifier dataUsingEncoding:NSUTF8StringEncoding]] forKeyword:keyFileSender];

	AEDesc reply = { typeNull, NULL };
	OSStatus status = AESend(appleEvent.aeDesc, &reply, kAEWaitReply, kAENormalPriority, kAEDefaultTimeout, NULL, NULL);
	if(status == noErr)
	{
		NSAppleEventDescriptor* replyDescriptor  = [[NSAppleEventDescriptor alloc] initWithAEDescNoCopy:&reply];
		NSAppleEventDescriptor* errorDescriptor  = [replyDescriptor paramDescriptorForKeyword:keyErrorNumber];
		if(errorDescriptor != nil)
			status = [errorDescriptor int32Value];
	}

	if(status != noErr)
	{
		os_log_error(OS_LOG_DEFAULT, "Error sending Apple Event to TextMate: %d", status);
		NSBeep();
	}
}

- (void)handleModifiedFileEvent:(NSAppleEventDescriptor*)event withReplyEvent:(NSAppleEventDescriptor*)replyEvent
{
	NSAppleEventDescriptor* fileURL = [[event paramDescriptorForKeyword:keyDirectObject] coerceToDescriptorType:typeFileURL];
	if(NSString* urlString = [[NSString alloc] initWithData:fileURL.data encoding:NSUTF8StringEncoding])
	{
		if(NSURL* url = [NSURL URLWithString:urlString])
		{
			if(NSString* content = [NSString stringWithContentsOfURL:url encoding:NSUTF8StringEncoding error:nil])
			{
				NSDictionary* bindingOptions = [self.textView infoForBinding:NSValueBinding];
				id controller = bindingOptions[@"NSObservedObject"];
				NSString* keyPath = bindingOptions[@"NSObservedKeyPath"];
				if(controller && keyPath)
						[controller setValue:content forKeyPath:keyPath];
				else	[self.textView.textStorage.mutableString setString:content];
			}
		}
	}
}

- (void)handleClosedFileEvent:(NSAppleEventDescriptor*)event withReplyEvent:(NSAppleEventDescriptor*)replyEvent
{
	if(_temporaryURL)
		[NSFileManager.defaultManager removeItemAtURL:_temporaryURL error:nil];
	_temporaryURL = nil;

	[NSApp activateIgnoringOtherApps:YES];
	[self removeODBEventHandlers];
}
@end
