#import "OakTextView_Private.h"
#import "OakTextView_LSPUtilities.h"
#import <lsp/LSPManager.h>
#import <lsp/LSPClient.h>

@implementation OakTextView (Completion)

- (void)lspComplete:(id)sender
{
	if(!documentView)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	BOOL isExplicitTrigger = !_lspCompletionPopup || ![_lspCompletionPopup isVisible];

	if(![[LSPManager sharedManager] hasClientForDocument:doc])
	{
		if(isExplicitTrigger)
			[OakNotificationManager.shared showWithMessage:@"No LSP server for this document" type:3];
		return;
	}

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	size_t bol = documentView->begin(pos.line);
	std::string lineText = documentView->substr(bol, caret);
	size_t prefixStart = lineText.size();
	while(prefixStart > 0 && (isalnum(lineText[prefixStart-1]) || lineText[prefixStart-1] == '_'))
		--prefixStart;
	NSString* prefix = to_ns(lineText.substr(prefixStart));

	[[LSPManager sharedManager] flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;
	NSUInteger prefixLen = prefix.length;
	[[LSPManager sharedManager] requestCompletionsForDocument:doc
		line:pos.line
		character:pos.column
		prefix:prefix
		completion:^(NSArray<NSDictionary*>* suggestions) {
			OakTextView* strongSelf = weakSelf;
			if(!strongSelf)
				return;

			if(suggestions.count == 0)
			{
				if(isExplicitTrigger)
					[OakNotificationManager.shared showWithMessage:@"No completions available" type:3];
				return;
			}

			[strongSelf showLSPCompletionPopupWithSuggestions:suggestions prefixLength:prefixLen autoInsertSingle:isExplicitTrigger];
		}];
}

- (void)ensureCompletionPopup
{
	OakThemeEnvironment* theme = [self lspTheme];

	if(!_lspCompletionPopup)
	{
		_lspCompletionPopup = [[OakCompletionPopup alloc] initWithTheme:theme];
		_lspCompletionPopup.delegate = (id<OakCompletionPopupDelegate>)self;
	}
}

- (NSPoint)caretPointForCompletionPopup
{
	CGRect caretRect = documentView->rect_at_index(documentView->ranges().last().last.index);
	return NSMakePoint(NSMinX(caretRect), NSMaxY(caretRect) + 4);
}

- (void)showLSPCompletionPopupWithSuggestions:(NSArray<NSDictionary*>*)suggestions prefixLength:(NSUInteger)prefixLen autoInsertSingle:(BOOL)autoInsertSingle
{
	if(!documentView)
		return;

	[self ensureCompletionPopup];
	_lspCompletionPopup.supportsResolve = [[LSPManager sharedManager] serverSupportsCompletionResolveForDocument:self.document];

	NSMutableArray<OakCompletionItem*>* items = [NSMutableArray arrayWithCapacity:suggestions.count];
	for(NSDictionary* s in suggestions)
	{
		NSString* label = s[@"label"] ?: s[@"display"] ?: @"";
		NSString* insert = s[@"insert"];
		NSString* detail = s[@"detail"] ?: @"";
		int kind = [s[@"kind"] intValue];
		BOOL isSnippet = [s[@"insertTextFormat"] intValue] == 2;

		if((kind == 2 || kind == 3 || kind == 4) && detail.length > 0)
		{
			NSRange parenRange = [detail rangeOfString:@"("];
			if(parenRange.location != NSNotFound)
			{
				NSString* afterParen = [detail substringFromIndex:parenRange.location + 1];
				NSRange closeRange = [afterParen rangeOfString:@")"];
				if(closeRange.location != NSNotFound && closeRange.location > 0)
				{
					NSString* paramStr = [afterParen substringToIndex:closeRange.location];
					paramStr = [paramStr stringByReplacingOccurrencesOfString:@"[" withString:@""];
					paramStr = [paramStr stringByReplacingOccurrencesOfString:@"]" withString:@""];

					NSRegularExpression* regex = [NSRegularExpression regularExpressionWithPattern:@"\\$([a-zA-Z_][a-zA-Z0-9_]*)" options:0 error:nil];
					NSArray<NSTextCheckingResult*>* matches = [regex matchesInString:paramStr options:0 range:NSMakeRange(0, paramStr.length)];

					if(matches.count > 0)
					{
						NSMutableString* snippet = [NSMutableString stringWithFormat:@"%@(", label];
						for(NSUInteger i = 0; i < matches.count; i++)
						{
							NSString* paramName = [paramStr substringWithRange:[matches[i] rangeAtIndex:1]];
							if(i > 0) [snippet appendString:@", "];
							[snippet appendString:@"\\$"];
						[snippet appendFormat:@"${%lu:%@}", (unsigned long)(i + 1), paramName];
						}
						[snippet appendString:@")"];
						insert = snippet;
						isSnippet = YES;
					}
				}
			}
		}

		OakCompletionItem* item = [[OakCompletionItem alloc]
			initWithLabel:label insertText:insert detail:detail kind:kind];
		item.isSnippet = isSnippet;
		item.originalItem = s[@"_originalItem"];
		[items addObject:item];
	}

	NSString* prefixLower = [to_ns(documentView->substr(
		documentView->begin(documentView->convert(documentView->ranges().last().last.index).line),
		documentView->ranges().last().last.index)) lowercaseString];
	NSRange wordRange = [prefixLower rangeOfCharacterFromSet:[NSCharacterSet alphanumericCharacterSet].invertedSet options:NSBackwardsSearch];
	NSString* wordPrefix = wordRange.location == NSNotFound ? prefixLower : [prefixLower substringFromIndex:wordRange.location + 1];

	if(wordPrefix.length > 0)
	{
		[items sortUsingComparator:^NSComparisonResult(OakCompletionItem* a, OakCompletionItem* b) {
			BOOL aPrefix = [a.label.lowercaseString hasPrefix:wordPrefix];
			BOOL bPrefix = [b.label.lowercaseString hasPrefix:wordPrefix];
			if(aPrefix != bPrefix)
				return aPrefix ? NSOrderedAscending : NSOrderedDescending;
			return [a.label caseInsensitiveCompare:b.label];
		}];
	}

	_lspInitialPrefixLength = prefixLen;
	_lspFilterPrefix = @"";

	if(autoInsertSingle && items.count == 1)
	{
		OakCompletionItem* item = items.firstObject;

		AUTO_REFRESH;
		size_t caret = documentView->ranges().last().last.index;
		NSUInteger deleteCount = _lspInitialPrefixLength;
		size_t from = caret - deleteCount;
		documentView->set_ranges(ng::range_t(from, caret));

		if(item.isSnippet)
		{
			documentView->insert("");
			[self insertSnippetWithOptions:@{ @"content": item.effectiveInsertText }];
		}
		else
		{
			documentView->insert(to_s(item.effectiveInsertText));
		}

		_lspFilterPrefix = nil;
		return;
	}

	[_lspCompletionPopup showIn:self at:[self caretPointForCompletionPopup] items:items];
}

- (void)completionPopup:(OakCompletionPopup*)popup didSelectItem:(OakCompletionItem*)item
{
	if(!documentView)
		return;

	AUTO_REFRESH;

	if(_copilotCompletionActive)
	{
		[self insertCopilotCompletion:(NSDictionary*)item.originalItem];
		_copilotCompletionActive = NO;
		_lspFilterPrefix = nil;
		return;
	}

	size_t caret = documentView->ranges().last().last.index;
	NSUInteger deleteCount = _lspInitialPrefixLength + _lspFilterPrefix.length;
	size_t from = caret - deleteCount;
	documentView->set_ranges(ng::range_t(from, caret));

	if(item.isSnippet)
	{
		documentView->insert("");
		[self insertSnippetWithOptions:@{ @"content": item.effectiveInsertText }];
	}
	else
	{
		documentView->insert(to_s(item.effectiveInsertText));
	}

	_lspFilterPrefix = nil;
}

- (void)completionPopupDidDismiss:(OakCompletionPopup*)popup
{
	_copilotCompletionActive = NO;
	_lspFilterPrefix = nil;
}

- (void)completionPopup:(OakCompletionPopup*)popup resolveItem:(OakCompletionItem*)item
{
	if(!item.originalItem)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	if(![[LSPManager sharedManager] serverSupportsCompletionResolveForDocument:doc])
		return;

	__weak OakTextView* weakSelf = self;
	[[LSPManager sharedManager] resolveCompletionItem:item.originalItem forDocument:doc completion:^(NSDictionary* resolved) {
		OakTextView* strongSelf = weakSelf;
		if(!strongSelf || !resolved)
			return;

		NSString* rawDocumentation = nil;
		id docValue = resolved[@"documentation"];
		if([docValue isKindOfClass:[NSString class]])
		{
			rawDocumentation = docValue;
		}
		else if([docValue isKindOfClass:[NSDictionary class]])
		{
			rawDocumentation = docValue[@"value"];
		}


		NSString* newInsertText = nil;
		if(resolved[@"insertText"])
			newInsertText = resolved[@"insertText"];
		else if(resolved[@"textEdit"] && [resolved[@"textEdit"] isKindOfClass:[NSDictionary class]])
			newInsertText = resolved[@"textEdit"][@"newText"];

		dispatch_async(dispatch_get_main_queue(), ^{
			NSAttributedString* parsedDocs = rawDocumentation.length > 0 ? [strongSelf parseMarkdownDocumentation:rawDocumentation] : nil;
			[strongSelf->_lspCompletionPopup resolveCompletedFor:item documentation:parsedDocs insertText:newInsertText];
		});
	}];
}

@end
