#import "OakTextView_Private.h"
#import "OakTextView_LSPUtilities.h"
#import <lsp/CopilotManager.h>
#import <lsp/LSPManager.h>
#import <CoreText/CoreText.h>

@implementation OakTextView (Copilot)

- (void)lspCopilotComplete:(id)sender
{
	if(!documentView)
		return;

	[_ghostTextTimer invalidate];
	_ghostTextTimer = nil;
	[self cancelCopilotGhostTextRequest];
	[self clearGhostText];

	OakDocument* doc = self.document;
	if(!doc)
		return;

	CopilotManager* copilot = CopilotManager.sharedManager;
	switch(copilot.status)
	{
		case CopilotStatusAuthRequired:
			[copilot signIn];
			return;
		case CopilotStatusDisabled:
			[OakNotificationManager.shared showWithMessage:@"Copilot: Disabled" type:3];
			return;
		case CopilotStatusReady:
			break;
		default:
			[OakNotificationManager.shared showWithMessage:@"Copilot: Not ready" type:3];
			return;
	}

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	NSLog(@"[Copilot] lspCopilotComplete: triggered at %lu:%lu", (unsigned long)pos.line, (unsigned long)pos.column);

	__weak OakTextView* weakSelf = self;
	NSUInteger cursorChar = pos.column;
	[copilot requestCompletionForDocument:doc line:pos.line character:cursorChar completion:^(NSArray<NSDictionary*>* items) {
		OakTextView* strongSelf = weakSelf;
		if(!strongSelf || !strongSelf->documentView)
			return;

		NSLog(@"[Copilot] Completion callback: %lu items", (unsigned long)items.count);

		if(!items || items.count == 0)
		{
			[OakNotificationManager.shared showWithMessage:@"Copilot: No suggestions" type:3];
			return;
		}

		if(items.count == 1)
			[strongSelf insertCopilotCompletion:items[0]];
		else
			[strongSelf showCopilotCompletionPopup:items cursorCharacter:cursorChar];
	}];
}

- (void)insertCopilotCompletion:(NSDictionary*)item
{
	AUTO_REFRESH;

	NSString* insertText = item[@"insertText"] ?: item[@"text"] ?: item[@"displayText"];
	if(!insertText.length)
		return;

	NSDictionary* range = item[@"range"];
	NSLog(@"[Copilot] Inserting completion: %lu chars, range: %@, text: '%.80s…'",
		(unsigned long)insertText.length, range, insertText.UTF8String);

	if(range)
	{
		int startLine = [range[@"start"][@"line"] intValue];
		int startChar = [range[@"start"][@"character"] intValue];
		int endLine   = [range[@"end"][@"line"] intValue];
		int endChar   = [range[@"end"][@"character"] intValue];

		NSLog(@"[Copilot] Range: %d:%d → %d:%d", startLine, startChar, endLine, endChar);

		size_t from = documentView->convert(text::pos_t(startLine, startChar));
		size_t to   = documentView->convert(text::pos_t(endLine, endChar));
		documentView->set_ranges(ng::range_t(from, to));
	}
	else
	{
		NSLog(@"[Copilot] No range, inserting at cursor");
	}

	documentView->insert(to_s(insertText));

	CopilotManager* copilot = [CopilotManager sharedManager];
	[copilot sendDidShowCompletion:item];
	[copilot sendAcceptanceTelemetry:item];
}

- (void)showCopilotCompletionPopup:(NSArray<NSDictionary*>*)items cursorCharacter:(NSUInteger)cursorChar
{
	[self ensureCompletionPopup];
	_lspCompletionPopup.supportsResolve = YES;

	NSMutableArray<OakCompletionItem*>* completionItems = [NSMutableArray new];
	for(NSDictionary* item in items)
	{
		NSString* fullText = item[@"insertText"] ?: item[@"text"] ?: @"";
		NSDictionary* range = item[@"range"];

		NSString* label = fullText;
		NSUInteger rangeStartChar = [range[@"start"][@"character"] unsignedIntegerValue];
		if(cursorChar > rangeStartChar)
		{
			NSUInteger prefixLen = cursorChar - rangeStartChar;
			if(prefixLen < fullText.length)
				label = [fullText substringFromIndex:prefixLen];
		}

		NSString* firstLine = [label componentsSeparatedByString:@"\n"].firstObject;
		NSArray* fullLines = [fullText componentsSeparatedByString:@"\n"];
		NSString* detail = fullLines.count > 1
			? [NSString stringWithFormat:@"Copilot · %lu lines", (unsigned long)fullLines.count]
			: @"Copilot";

		OakCompletionItem* ci = [[OakCompletionItem alloc] initWithLabel:firstLine
		                                                      insertText:fullText
		                                                          detail:detail
		                                                            kind:15];
		ci.multiline = YES;
		ci.originalItem = (NSDictionary*)item;

		NSString* grammarScope = documentView ? to_ns(documentView->file_type()) : nil;
		ci.documentation = [self syntaxHighlight:fullText withGrammar:grammarScope];
		ci.isResolved = YES;
		[completionItems addObject:ci];
	}

	_lspInitialPrefixLength = 0;
	_lspFilterPrefix = @"";
	_copilotCompletionActive = YES;

	[_lspCompletionPopup showIn:self at:[self caretPointForCompletionPopup] items:completionItems];

	CopilotManager* copilot = [CopilotManager sharedManager];
	for(NSDictionary* item in items)
		[copilot sendDidShowCompletion:item];
}

// ========================
// = Copilot Ghost Text =
// ========================

- (void)drawGhostText:(CGContextRef)ctx inRect:(NSRect)aRect
{
	NSFont* font = self.font ?: [NSFont userFixedPitchFontOfSize:12];
	CGFloat fontSize = font.pointSize * (documentView ? documentView->font_scale_factor() : 1.0);
	NSFont* scaledFont = [NSFont fontWithDescriptor:font.fontDescriptor size:fontSize];

	NSColor* ghostColor;
	if(self.theme)
	{
		auto styles = self.theme->styles_for_scope("comment");
		CGColorRef fg = styles.foreground();
		if(fg)
			ghostColor = [[NSColor colorWithCGColor:fg] colorWithAlphaComponent:0.4];
	}
	if(!ghostColor)
		ghostColor = [NSColor.secondaryLabelColor colorWithAlphaComponent:0.4];

	NSDictionary* attrs = @{
		NSFontAttributeName: scaledFont,
		NSForegroundColorAttributeName: ghostColor,
	};

	CGRect caretRect = documentView->rect_at_index(ng::index_t(_ghostTextCaret));

	NSArray<NSString*>* lines = [_ghostText componentsSeparatedByString:@"\n"];
	CGFloat x = CGRectGetMinX(caretRect);
	CGFloat y = CGRectGetMinY(caretRect);
	CGFloat lineHeight = CGRectGetHeight(caretRect);

	text::pos_t caretPos = documentView->convert(_ghostTextCaret);
	size_t bol = documentView->begin(caretPos.line);
	CGRect bolRect = documentView->rect_at_index(ng::index_t(bol));
	CGFloat bolX = CGRectGetMinX(bolRect);

	for(NSUInteger i = 0; i < lines.count; i++)
	{
		NSString* line = lines[i];
		if(!line.length && i > 0)
		{
			y += lineHeight;
			continue;
		}

		CGFloat drawX = (i == 0) ? x : bolX;

		if(y + lineHeight >= NSMinY(aRect) && y <= NSMaxY(aRect))
		{
			NSAttributedString* attrStr = [[NSAttributedString alloc] initWithString:line attributes:attrs];
			CTLineRef ctLine = CTLineCreateWithAttributedString((__bridge CFAttributedStringRef)attrStr);

			CGContextSaveGState(ctx);
			CGContextSetTextMatrix(ctx, CGAffineTransformMakeScale(1.0, -1.0));
			CGFloat baseline = y + scaledFont.ascender;
			CGContextSetTextPosition(ctx, drawX, baseline);
			CTLineDraw(ctLine, ctx);
			CGContextRestoreGState(ctx);

			CFRelease(ctLine);
		}

		y += lineHeight;
	}
}

- (void)scheduleCopilotGhostText
{
	[self clearGhostText];

	if(!documentView)
		return;

	if(documentView->disallow_tab_expansion())
		return;

	if(documentView->ranges().size() > 1)
		return;

	CopilotManager* copilot = CopilotManager.sharedManager;
	if(copilot.status != CopilotStatusReady)
		return;

	if([_lspCompletionPopup isVisible])
		return;

	__weak OakTextView* weakSelf = self;
	_ghostTextTimer = [NSTimer scheduledTimerWithTimeInterval:0.5
	                                                 repeats:NO
	                                                   block:^(NSTimer* t) {
		[weakSelf requestCopilotGhostText];
	}];
}

- (void)requestCopilotGhostText
{
	if(!documentView)
		return;

	[self cancelCopilotGhostTextRequest];

	CopilotManager* copilot = CopilotManager.sharedManager;
	if(copilot.status != CopilotStatusReady)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	__weak OakTextView* weakSelf = self;
	size_t requestCaret = caret;
	_ghostTextRequestId = [copilot requestCompletionForDocument:doc
	                                                       line:pos.line
	                                                  character:pos.column
	                                                 completion:^(NSArray<NSDictionary*>* items) {
		OakTextView* strongSelf = weakSelf;
		if(!strongSelf || !strongSelf->documentView)
			return;

		strongSelf->_ghostTextRequestId = 0;

		size_t currentCaret = strongSelf->documentView->ranges().last().last.index;
		if(currentCaret != requestCaret)
			return;

		if(!items || items.count == 0)
			return;

		BOOL suppressPopup = [[NSUserDefaults standardUserDefaults] boolForKey:@"CopilotSuppressAutoPopup"];
		if(items.count == 1 || suppressPopup)
			[strongSelf showGhostText:items[0]];
		else
			[strongSelf showCopilotCompletionPopup:items cursorCharacter:pos.column];
	}];
}

- (void)cancelCopilotGhostTextRequest
{
	if(_ghostTextRequestId)
	{
		[[CopilotManager sharedManager] cancelCompletionRequest:_ghostTextRequestId];
		_ghostTextRequestId = 0;
	}
}

- (void)showGhostText:(NSDictionary*)item
{
	NSString* insertText = item[@"insertText"] ?: item[@"text"] ?: item[@"displayText"];
	if(!insertText.length)
		return;

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t caretPos = documentView->convert(caret);
	size_t eol = documentView->eol(caretPos.line);
	std::string lineAfterCursor = documentView->substr(caret, eol);
	if(!lineAfterCursor.empty() && lineAfterCursor.find_first_not_of(" \t)}]>;,") != std::string::npos)
		return;

	NSDictionary* range = item[@"range"];
	if(range)
	{
		size_t caret = documentView->ranges().last().last.index;
		text::pos_t caretPos = documentView->convert(caret);
		NSUInteger rangeStartChar = [range[@"start"][@"character"] unsignedIntegerValue];
		if(caretPos.column > rangeStartChar)
		{
			NSUInteger prefixLen = caretPos.column - rangeStartChar;
			if(prefixLen < insertText.length)
				insertText = [insertText substringFromIndex:prefixLen];
		}
	}

	_ghostText = insertText;
	_ghostTextItem = item;
	_ghostTextCaret = documentView->ranges().last().last.index;

	NSUInteger lineCount = [[insertText componentsSeparatedByString:@"\n"] count];
	if(lineCount > 1)
	{
		CGRect caretRect = documentView->rect_at_index(ng::index_t(_ghostTextCaret));
		_ghostTextExtraHeight = (lineCount - 1) * CGRectGetHeight(caretRect);

		NSRect r = [[self enclosingScrollView] documentVisibleRect];
		NSSize newSize = NSMakeSize(std::max(NSWidth(r), documentView->width()), std::max(NSHeight(r), documentView->height() + _ghostTextExtraHeight));
		[self setFrameSize:newSize];
	}
	else
	{
		_ghostTextExtraHeight = 0;
	}

	[[CopilotManager sharedManager] sendDidShowCompletion:item];

	[self setNeedsDisplay:YES];
}

- (void)clearGhostText
{
	if(!_ghostText)
		return;

	BOOL hadExtraHeight = _ghostTextExtraHeight > 0;

	_ghostText = nil;
	_ghostTextItem = nil;
	_ghostTextCaret = 0;
	_ghostTextExtraHeight = 0;
	[_ghostTextTimer invalidate];
	_ghostTextTimer = nil;
	_ghostTextRequestId = 0;

	if(hadExtraHeight && documentView)
	{
		NSRect r = [[self enclosingScrollView] documentVisibleRect];
		NSSize newSize = NSMakeSize(std::max(NSWidth(r), documentView->width()), std::max(NSHeight(r), documentView->height()));
		[self setFrameSize:newSize];
	}

	[self setNeedsDisplay:YES];
}

- (BOOL)hasGhostText
{
	return _ghostText != nil;
}

- (CGFloat)ghostTextExtraHeight
{
	return _ghostTextExtraHeight;
}

- (void)acceptGhostText
{
	if(!_ghostText || !documentView)
		return;

	AUTO_REFRESH;

	NSDictionary* item = _ghostTextItem;
	NSString* fullInsertText = item[@"insertText"] ?: item[@"text"] ?: item[@"displayText"];
	NSDictionary* range = item[@"range"];

	if(range && fullInsertText)
	{
		int startLine = [range[@"start"][@"line"] intValue];
		int startChar = [range[@"start"][@"character"] intValue];
		int endLine   = [range[@"end"][@"line"] intValue];
		int endChar   = [range[@"end"][@"character"] intValue];

		size_t from = documentView->convert(text::pos_t(startLine, startChar));
		size_t to   = documentView->convert(text::pos_t(endLine, endChar));
		documentView->set_ranges(ng::range_t(from, to));
		documentView->insert(to_s(fullInsertText));
	}
	else
	{
		documentView->insert(to_s(_ghostText));
	}

	[[CopilotManager sharedManager] sendAcceptanceTelemetry:item];

	BOOL hadExtraHeight = _ghostTextExtraHeight > 0;

	_ghostText = nil;
	_ghostTextItem = nil;
	_ghostTextCaret = 0;
	_ghostTextExtraHeight = 0;
	_ghostTextRequestId = 0;

	if(hadExtraHeight)
	{
		NSRect r = [[self enclosingScrollView] documentVisibleRect];
		NSSize newSize = NSMakeSize(std::max(NSWidth(r), documentView->width()), std::max(NSHeight(r), documentView->height()));
		[self setFrameSize:newSize];
	}
}

@end
