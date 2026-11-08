#import "OakTextView_Private.h"
#import "OakTextView_LSPUtilities.h"
#import "OakDocumentView.h"
#import <lsp/LSPManager.h>
#import <lsp/LSPClient.h>
#import <Preferences/Keys.h>
#import <os/log.h>

static NSDictionary<NSString*, NSArray<NSDictionary*>*>* editsFromWorkspaceEdit (NSDictionary* workspaceEdit)
{
	NSMutableDictionary<NSString*, NSMutableArray<NSDictionary*>*>* editsByUri = [NSMutableDictionary new];

	NSArray* documentChanges = workspaceEdit[@"documentChanges"];
	if(documentChanges)
	{
		for(NSDictionary* docChange in documentChanges)
		{
			NSString* uri = docChange[@"textDocument"][@"uri"];
			NSArray* edits = docChange[@"edits"];
			if(uri && edits)
				editsByUri[uri] = [edits mutableCopy];
		}
		return editsByUri;
	}

	NSDictionary* changes = workspaceEdit[@"changes"];
	if(changes)
	{
		for(NSString* uri in changes)
			editsByUri[uri] = [changes[uri] mutableCopy];
	}

	return editsByUri;
}

@implementation OakTextView (LSP)

// R4: Unified lazy accessor for the shared theme environment
- (OakThemeEnvironment*)lspTheme
{
	if(!_lspTheme)
	{
		_lspTheme = [[OakThemeEnvironment alloc] init];
		NSFont* f = self.font ?: [NSFont userFixedPitchFontOfSize:12];
		[_lspTheme applyTheme:@{
			@"fontName": f.fontName,
			@"fontSize": @(f.pointSize),
		}];
	}
	return _lspTheme;
}

// R1: Configurable definition filtering via .tm_properties
- (NSDictionary*)bestDefinitionLocation:(NSArray<NSDictionary*>*)locations currentURI:(NSString*)currentUri
{
	if(locations.count <= 1)
		return locations.firstObject;

	std::string filePath = to_s(self.document.path ?: @"");
	std::string fileType = to_s(self.document.fileType ?: @"");
	std::string directory = to_s(self.document.directory ?: @"");
	settings_t const settings = settings_for_path(filePath, fileType, directory);
	std::string excludePattern = settings.get(kSettingsLSPDefinitionExcludePatternKey, "");

	NSRegularExpression* regex = nil;
	if(!excludePattern.empty())
	{
		NSString* pattern = to_ns(excludePattern);
		regex = [NSRegularExpression regularExpressionWithPattern:pattern options:0 error:nil];
		if(!regex)
			[self showToolTip:[NSString stringWithFormat:@"Invalid lspDefinitionExcludePattern: %@", pattern]];
	}

	NSDictionary* best = locations.firstObject;

	for(NSUInteger i = 1; i < locations.count; i++)
	{
		NSDictionary* l = locations[i];
		NSString* uri = l[@"uri"];
		NSString* bestUri = best[@"uri"];

		BOOL bestIsCurrent = currentUri && [bestUri isEqualToString:currentUri];
		BOOL curIsCurrent = currentUri && [uri isEqualToString:currentUri];

		// Prefer location in the current file
		if(!bestIsCurrent && curIsCurrent)
		{
			best = l;
			continue;
		}
		if(bestIsCurrent && !curIsCurrent)
			continue;

		// Among equally-ranked locations, prefer non-excluded over excluded
		if(regex)
		{
			BOOL bestExcluded = [regex firstMatchInString:bestUri options:0 range:NSMakeRange(0, bestUri.length)] != nil;
			BOOL curExcluded = [regex firstMatchInString:uri options:0 range:NSMakeRange(0, uri.length)] != nil;

			if(bestExcluded && !curExcluded)
				best = l;
		}
	}

	return best;
}

// ========================
// = LSP Go to Definition =
// ========================

- (void)lspGoToDefinition:(id)sender
{
	if(!documentView)
		return;

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	OakDocument* doc = self.document;
	if(!doc)
		return;

	[[LSPManager sharedManager] flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;
	[[LSPManager sharedManager] requestDefinitionForDocument:doc
		line:pos.line
		character:pos.column
		completion:^(NSArray<NSDictionary*>* locations) {
			OakTextView* strongSelf = weakSelf;
			if(!strongSelf)
				return;

			if(locations.count == 0)
			{
				NSLog(@"[LSP] No definition found");
				return;
			}

			NSString* currentUri = strongSelf.document.path ? [NSURL fileURLWithPath:strongSelf.document.path].absoluteString : nil;
			NSDictionary* loc = [strongSelf bestDefinitionLocation:locations currentURI:currentUri];

			NSString* uri = loc[@"uri"];
			NSUInteger line = [loc[@"line"] unsignedIntegerValue];
			NSUInteger character = [loc[@"character"] unsignedIntegerValue];

			NSURL* url = [NSURL URLWithString:uri];
			NSString* filePath = url.path;
			if(!filePath)
				return;

			OakDocument* targetDoc = [OakDocumentController.sharedInstance documentWithPath:filePath];
			text::range_t selection(text::pos_t(line, character));
			[OakDocumentController.sharedInstance showDocument:targetDoc andSelect:selection inProject:nil bringToFront:YES];
		}];
}

// = LSP References =
// ==================

- (void)lspFindReferences:(id)sender
{
	if(!documentView)
		return;

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	OakDocument* doc = self.document;
	if(!doc)
		return;

	std::string const buf = documentView->substr();
	size_t wordStart = caret, wordEnd = caret;
	while(wordStart > 0 && (isalnum(buf[wordStart - 1]) || buf[wordStart - 1] == '_'))
		--wordStart;
	while(wordEnd < buf.size() && (isalnum(buf[wordEnd]) || buf[wordEnd] == '_'))
		++wordEnd;
	NSString* symbolName = to_ns(buf.substr(wordStart, wordEnd - wordStart));

	NSString* docPath = doc.path;
	NSString* baseDir = docPath ? [docPath stringByDeletingLastPathComponent] : nil;

	[[LSPManager sharedManager] flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;
	[[LSPManager sharedManager] requestReferencesForDocument:doc
		line:pos.line
		character:pos.column
		completion:^(NSArray<NSDictionary*>* locations) {
			OakTextView* strongSelf = weakSelf;
			if(!strongSelf)
				return;

			if(locations.count == 0)
			{
				NSBeep();
				return;
			}

			if(locations.count == 1)
			{
				NSDictionary* loc = locations.firstObject;
				NSString* uri = loc[@"uri"];
				NSUInteger line = [loc[@"line"] unsignedIntegerValue];
				NSUInteger character = [loc[@"character"] unsignedIntegerValue];

				NSURL* url = [NSURL URLWithString:uri];
				NSString* filePath = url.path;
				if(!filePath)
					return;

				OakDocument* targetDoc = [OakDocumentController.sharedInstance documentWithPath:filePath];
				text::range_t selection(text::pos_t(line, character));
				[OakDocumentController.sharedInstance showDocument:targetDoc andSelect:selection inProject:nil bringToFront:YES];
				return;
			}

			NSMutableArray<OakReferenceItem*>* items = [NSMutableArray arrayWithCapacity:locations.count];
			NSMutableDictionary<NSString*, NSArray<NSString*>*>* fileLines = [NSMutableDictionary new];

			for(NSDictionary* loc in locations)
			{
				NSString* uri = loc[@"uri"];
				NSURL* url = [NSURL URLWithString:uri];
				NSString* filePath = url.path;
				if(!filePath)
					continue;

				NSUInteger line = [loc[@"line"] unsignedIntegerValue];
				NSUInteger character = [loc[@"character"] unsignedIntegerValue];

				NSString* displayPath = filePath;
				if(baseDir && [filePath hasPrefix:baseDir])
					displayPath = [filePath substringFromIndex:baseDir.length + 1];

				NSArray<NSString*>* lines = fileLines[filePath];
				if(!lines)
				{
					NSString* fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
					fileContent = [fileContent stringByReplacingOccurrencesOfString:@"\r" withString:@""];
					lines = fileContent ? [fileContent componentsSeparatedByString:@"\n"] : @[];
					fileLines[filePath] = lines;
				}

				NSString* lineContent = @"";
				if(line < lines.count)
				{
					lineContent = [lines[line] stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];
				}

				OakReferenceItem* item = [[OakReferenceItem alloc]
					initWithFilePath:filePath
					     displayPath:displayPath
					            line:line
					          column:character
					         content:lineContent];
				[items addObject:item];
			}

			if(!strongSelf->_lspReferencesPanel)
			{
				strongSelf->_lspReferencesPanel = [[OakReferencesPanel alloc] initWithTheme:[strongSelf lspTheme]];
				strongSelf->_lspReferencesPanel.delegate = (id<OakReferencesPanelDelegate>)strongSelf;
			}

			[strongSelf->_lspReferencesPanel showIn:strongSelf items:items symbol:symbolName ?: @"symbol"];
		}];
}

- (void)referencesPanel:(OakReferencesPanel*)panel didSelectItem:(OakReferenceItem*)item
{
	OakDocument* targetDoc = [OakDocumentController.sharedInstance documentWithPath:item.filePath];
	text::range_t selection(text::pos_t(item.line, item.column));
	[OakDocumentController.sharedInstance showDocument:targetDoc andSelect:selection inProject:nil bringToFront:YES];
}

- (void)referencesPanelDidClose:(OakReferencesPanel*)panel
{
}

// ==============
// = LSP Rename =
// ==============

- (void)lspRename:(id)sender
{
	if(!documentView)
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	LSPManager* lsp = [LSPManager sharedManager];
	if(![lsp serverSupportsRenameForDocument:doc])
	{
		NSBeep();
		return;
	}

	size_t caret = documentView->ranges().last().last.index;
	text::pos_t pos = documentView->convert(caret);

	std::string const buf = documentView->substr();
	size_t wordStart = caret, wordEnd = caret;
	while(wordStart > 0 && (isalnum(buf[wordStart - 1]) || buf[wordStart - 1] == '_'))
		--wordStart;
	while(wordEnd < buf.size() && (isalnum(buf[wordEnd]) || buf[wordEnd] == '_'))
		++wordEnd;
	NSString* fallbackName = to_ns(buf.substr(wordStart, wordEnd - wordStart));

	if(fallbackName.length == 0)
	{
		NSBeep();
		return;
	}

	_renameCaret = caret;
	_renamePos = pos;

	[lsp flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;

	[lsp requestPrepareRenameForDocument:doc line:pos.line character:pos.column completion:^(NSDictionary* result) {
		OakTextView* strongSelf = weakSelf;
		if(!strongSelf || !strongSelf->documentView)
			return;

		NSString* placeholder = fallbackName;

		if(result)
		{
			if(result[@"placeholder"])
				placeholder = result[@"placeholder"];
			else if(result[@"start"] && result[@"end"])
			{
				NSDictionary* start = result[@"start"];
				NSDictionary* end = result[@"end"];
				size_t startIdx = lspPositionToOffset(*strongSelf->documentView, [start[@"line"] integerValue], [start[@"character"] integerValue]);
				size_t endIdx = lspPositionToOffset(*strongSelf->documentView, [end[@"line"] integerValue], [end[@"character"] integerValue]);
				std::string const freshBuf = strongSelf->documentView->substr();
				if(startIdx < endIdx && endIdx <= freshBuf.size())
					placeholder = to_ns(freshBuf.substr(startIdx, endIdx - startIdx));
			}
			else if(result[@"range"] && !result[@"placeholder"])
			{
				NSDictionary* range = result[@"range"];
				NSDictionary* start = range[@"start"];
				NSDictionary* end = range[@"end"];
				if(start && end)
				{
					size_t startIdx = lspPositionToOffset(*strongSelf->documentView, [start[@"line"] integerValue], [start[@"character"] integerValue]);
					size_t endIdx = lspPositionToOffset(*strongSelf->documentView, [end[@"line"] integerValue], [end[@"character"] integerValue]);
					std::string const freshBuf = strongSelf->documentView->substr();
					if(startIdx < endIdx && endIdx <= freshBuf.size())
						placeholder = to_ns(freshBuf.substr(startIdx, endIdx - startIdx));
				}
			}
		}
		else
		{
			NSBeep();
			return;
		}

		strongSelf->_pendingRenameOldName = placeholder;
		[strongSelf showRenameFieldWithPlaceholder:placeholder];
	}];
}

- (void)showRenameFieldWithPlaceholder:(NSString*)placeholder
{
	OakThemeEnvironment* theme = [self lspTheme];

	if(!_lspRenameField)
	{
		_lspRenameField = [[OakRenameField alloc] initWithTheme:theme];
		_lspRenameField.delegate = (id<OakRenameFieldDelegate>)self;
	}

	NSPoint caretPoint = [self positionForWindowUnderCaret];
	[_lspRenameField showIn:self at:caretPoint placeholder:placeholder];
}

- (void)renameField:(OakRenameField*)field didConfirmWithName:(NSString*)newName
{
	if(!documentView)
		return;

	if([newName isEqualToString:_pendingRenameOldName])
		return;

	OakDocument* doc = self.document;
	if(!doc)
		return;

	_pendingRenameNewName = newName;
	_renameRevision = documentView->revision();

	[[LSPManager sharedManager] flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;

	[[LSPManager sharedManager] requestRenameForDocument:doc line:_renamePos.line character:_renamePos.column newName:newName completion:^(NSDictionary* workspaceEdit) {
		OakTextView* strongSelf = weakSelf;
		if(!strongSelf || !strongSelf->documentView)
			return;

		if(strongSelf->documentView->revision() != strongSelf->_renameRevision)
			return;

		if(!workspaceEdit)
		{
			NSBeep();
			return;
		}

		strongSelf->_pendingRenameEdits = workspaceEdit;
		[strongSelf showRenamePreviewWithEdit:workspaceEdit];
	}];
}

- (void)renameFieldDidDismiss:(OakRenameField*)field
{
	_pendingRenameOldName = nil;
}

- (void)showRenamePreviewWithEdit:(NSDictionary*)workspaceEdit
{
	NSDictionary<NSString*, NSArray<NSDictionary*>*>* editsByUri = editsFromWorkspaceEdit(workspaceEdit);
	if(editsByUri.count == 0)
	{
		NSBeep();
		return;
	}

	NSMutableArray<OakRenameItem*>* items = [NSMutableArray new];
	NSMutableDictionary<NSString*, NSArray<NSString*>*>* fileLines = [NSMutableDictionary new];

	OakDocument* doc = self.document;
	NSString* docPath = doc.path;
	NSString* baseDir = docPath ? [docPath stringByDeletingLastPathComponent] : nil;

	for(NSString* uri in editsByUri)
	{
		NSURL* url = [NSURL URLWithString:uri];
		NSString* filePath = url.path;
		if(!filePath)
			continue;

		NSString* displayPath = filePath;
		if(baseDir && [filePath hasPrefix:baseDir])
			displayPath = [filePath substringFromIndex:baseDir.length + 1];

		NSArray<NSString*>* lines = fileLines[filePath];
		if(!lines)
		{
			NSString* fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
			fileContent = [fileContent stringByReplacingOccurrencesOfString:@"\r" withString:@""];
			lines = fileContent ? [fileContent componentsSeparatedByString:@"\n"] : @[];
			fileLines[filePath] = lines;
		}

		for(NSDictionary* edit in editsByUri[uri])
		{
			NSDictionary* range = edit[@"range"];
			NSDictionary* start = range[@"start"];
			NSString* newText = edit[@"newText"];
			if(!start || !newText)
				continue;

			NSUInteger line = [start[@"line"] unsignedIntegerValue];
			NSString* fullOldLine = (line < lines.count) ? lines[line] : @"";
			NSString* oldLineText = [fullOldLine stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];

			NSDictionary* end = range[@"end"];
			NSUInteger startChar = [start[@"character"] unsignedIntegerValue];
			NSUInteger endChar = end ? [end[@"character"] unsignedIntegerValue] : startChar;
			NSString* newLineText = fullOldLine;
			if(startChar <= fullOldLine.length && endChar <= fullOldLine.length)
			{
				NSRange replaceRange = NSMakeRange(startChar, endChar - startChar);
				newLineText = [fullOldLine stringByReplacingCharactersInRange:replaceRange withString:newText];
			}
			newLineText = [newLineText stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceCharacterSet]];

			OakRenameItem* item = [[OakRenameItem alloc]
				initWithFilePath:filePath
				     displayPath:displayPath
				            line:(int)line
				         oldText:oldLineText
				         newText:newLineText];
			[items addObject:item];
		}
	}

	if(items.count == 0)
	{
		NSBeep();
		return;
	}

	if(!_lspRenamePreviewPanel)
	{
		_lspRenamePreviewPanel = [[OakRenamePreviewPanel alloc] initWithTheme:[self lspTheme]];
		_lspRenamePreviewPanel.delegate = (id<OakRenamePreviewPanelDelegate>)self;
	}

	[_lspRenamePreviewPanel showWithItems:items
		oldName:_pendingRenameOldName ?: @""
		newName:_pendingRenameNewName ?: @""
		parentWindow:self.window];
}

- (void)renamePreviewPanelDidConfirm:(OakRenamePreviewPanel*)panel
{
	NSDictionary* workspaceEdit = _pendingRenameEdits;
	_pendingRenameEdits = nil;
	_pendingRenameOldName = nil;
	_pendingRenameNewName = nil;

	if(!workspaceEdit || !documentView)
		return;

	if(documentView->revision() != _renameRevision)
		return;

	[self applyWorkspaceEdit:workspaceEdit];
}

- (void)renamePreviewPanelDidCancel:(OakRenamePreviewPanel*)panel
{
	_pendingRenameEdits = nil;
	_pendingRenameOldName = nil;
	_pendingRenameNewName = nil;
}

- (void)applyWorkspaceEdit:(NSDictionary*)workspaceEdit
{
	NSDictionary<NSString*, NSArray<NSDictionary*>*>* editsByUri = editsFromWorkspaceEdit(workspaceEdit);

	for(NSString* uri in editsByUri)
	{
		NSURL* url = [NSURL URLWithString:uri];
		NSString* filePath = url.path;
		if(!filePath)
			continue;

		NSArray<NSDictionary*>* edits = editsByUri[uri];
		if(edits.count == 0)
			continue;

		if(![[NSFileManager defaultManager] isWritableFileAtPath:filePath])
			continue;

		OakDocument* currentDoc = self.document;
		BOOL isCurrentDocument = currentDoc.path && [currentDoc.path isEqualToString:filePath];

		if(isCurrentDocument && documentView)
		{
			AUTO_REFRESH;
			documentView->perform_replacements(replacementsFromTextEdits(*documentView, edits));

			OakDocumentView* docView = (OakDocumentView*)[self enclosingScrollView].superview;
			if([docView respondsToSelector:@selector(invalidateCodeActionProbe)])
				[docView invalidateCodeActionProbe];
		}
		else
		{
			NSString* content = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
			if(!content)
				continue;

			NSArray* sorted = [edits sortedArrayUsingComparator:^NSComparisonResult(NSDictionary* a, NSDictionary* b) {
				NSDictionary* aStart = a[@"range"][@"start"];
				NSDictionary* bStart = b[@"range"][@"start"];
				NSInteger aLine = [aStart[@"line"] integerValue];
				NSInteger bLine = [bStart[@"line"] integerValue];
				if(aLine != bLine)
					return bLine < aLine ? NSOrderedAscending : NSOrderedDescending;
				NSInteger aChar = [aStart[@"character"] integerValue];
				NSInteger bChar = [bStart[@"character"] integerValue];
				return bChar < aChar ? NSOrderedAscending : NSOrderedDescending;
			}];

			NSArray<NSString*>* lines = [content componentsSeparatedByString:@"\n"];
			NSMutableArray<NSString*>* mutableLines = [lines mutableCopy];

			for(NSDictionary* edit in sorted)
			{
				NSDictionary* range = edit[@"range"];
				NSDictionary* start = range[@"start"];
				NSDictionary* end = range[@"end"];
				NSString* newText = edit[@"newText"];
				if(!start || !end || !newText)
					continue;

				NSUInteger startLine = [start[@"line"] unsignedIntegerValue];
				NSUInteger startChar = [start[@"character"] unsignedIntegerValue];
				NSUInteger endLine = [end[@"line"] unsignedIntegerValue];
				NSUInteger endChar = [end[@"character"] unsignedIntegerValue];

				if(startLine >= mutableLines.count)
					continue;

				if(endLine >= mutableLines.count)
				{
					endLine = mutableLines.count - 1;
					endChar = mutableLines[endLine].length;
				}

				NSString* prefix = [mutableLines[startLine] substringToIndex:MIN(startChar, mutableLines[startLine].length)];
				NSString* suffix = [mutableLines[endLine] substringFromIndex:MIN(endChar, mutableLines[endLine].length)];
				NSString* replacement = [NSString stringWithFormat:@"%@%@%@", prefix, newText, suffix];

				NSRange lineRange = NSMakeRange(startLine, endLine - startLine + 1);
				NSArray* replacementLines = [replacement componentsSeparatedByString:@"\n"];
				[mutableLines replaceObjectsInRange:lineRange withObjectsFromArray:replacementLines];
			}

			NSString* result = [mutableLines componentsJoinedByString:@"\n"];

			// R3: Error handling for file writes
			NSError* writeError = nil;
			NSURL* fileURL = [NSURL fileURLWithPath:filePath];
			if(![result writeToURL:fileURL atomically:YES encoding:NSUTF8StringEncoding error:&writeError])
			{
				os_log_error(OS_LOG_DEFAULT, "[LSP] Failed to write %{public}@: %{public}@", filePath, writeError.localizedDescription);
				[[NSNotificationCenter defaultCenter] postNotificationName:@"OakShowNotificationNotification"
					object:self userInfo:@{@"message": [NSString stringWithFormat:@"Write failed: %@", fileURL.lastPathComponent]}];
				continue;
			}
		}
	}
}

// = LSP Code Actions =
// ====================

- (void)handleApplyEditRequest:(NSNotification*)notification
{
	id requestId = notification.userInfo[@"requestId"];
	LSPClient* client = notification.userInfo[@"client"];

	if(!client || !requestId || requestId == [NSNull null])
		return;

	// S3: Instance-scoped dedup instead of static local
	if(_lastHandledWorkspaceEditRequestId && [requestId isEqual:_lastHandledWorkspaceEditRequestId])
		return;

	// S2: Completion handler replaces boolean flag
	if(_codeActionEditCompletion)
	{
		_codeActionEditCompletion();
		_codeActionEditCompletion = nil;
		_lastHandledWorkspaceEditRequestId = requestId;
		[client respondToApplyEdit:requestId applied:YES failureReason:nil];
		return;
	}

	NSDictionary* workspaceEdit = notification.userInfo[@"workspaceEdit"];
	if(workspaceEdit && documentView)
	{
		NSDictionary<NSString*, NSArray<NSDictionary*>*>* editsByUri = editsFromWorkspaceEdit(workspaceEdit);
		OakDocument* currentDoc = self.document;
		BOOL ownsEditedDocument = NO;
		for(NSString* uri in editsByUri)
		{
			NSString* filePath = [NSURL URLWithString:uri].path;
			if(currentDoc.path && [currentDoc.path isEqualToString:filePath])
			{
				ownsEditedDocument = YES;
				break;
			}
		}

		if(!ownsEditedDocument)
		{
			if(self.window != [NSApp keyWindow] || self != [self.window firstResponder])
				return;
		}
	}

	_lastHandledWorkspaceEditRequestId = requestId;

	BOOL applied = NO;
	if(workspaceEdit)
	{
		[self applyWorkspaceEdit:workspaceEdit];
		applied = YES;
	}

	[client respondToApplyEdit:requestId applied:applied failureReason:applied ? nil : @"No workspace edit provided"];
}

- (BOOL)canRequestCodeActions
{
	OakDocument* doc = self.document;
	if(!doc)
		return NO;

	auto const settings = settings_for_path(doc.virtualPath ? to_s(doc.virtualPath) : to_s(doc.path), to_s(doc.fileType), to_s(doc.directory ?: @""));
	if(!settings.get("lspCodeActions", true))
		return NO;

	return [[LSPManager sharedManager] serverSupportsCodeActionsForDocument:doc];
}

- (void)lspCodeActions:(id)sender
{
	if(!documentView || ![self canRequestCodeActions])
		return;

	OakDocument* doc = self.document;
	LSPManager* lsp = [LSPManager sharedManager];

	ng::range_t sel = documentView->ranges().last();
	text::pos_t startPos = documentView->convert(sel.min().index);
	text::pos_t endPos   = documentView->convert(sel.max().index);

	[lsp flushPendingChangesForDocument:doc];

	__weak OakTextView* weakSelf = self;
	[lsp requestCodeActionsForDocument:doc
		line:startPos.line character:startPos.column
		endLine:endPos.line endCharacter:endPos.column
		completion:^(NSArray<NSDictionary*>* actions) {
			dispatch_async(dispatch_get_main_queue(), ^{
				OakTextView* strongSelf = weakSelf;
				if(!strongSelf)
					return;
				if(!actions || actions.count == 0)
					return;
				[strongSelf showCodeActionsMenu:actions];
			});
		}];
}

- (void)showCodeActionsMenu:(NSArray<NSDictionary*>*)actions
{
	NSPoint pos = [self positionForWindowUnderCaret];
	pos = [self convertPoint:[self.window convertRectFromScreen:(NSRect){ pos, NSZeroSize }].origin fromView:nil];
	[self showCodeActionsMenu:actions atPoint:pos];
}

- (void)showCodeActionsMenu:(NSArray<NSDictionary*>*)actions atPoint:(NSPoint)point
{
	NSMenu* menu = [[NSMenu alloc] initWithTitle:@"Code Actions"];
	menu.autoenablesItems = NO;

	NSMutableArray* quickFixes = [NSMutableArray array];
	NSMutableArray* refactors  = [NSMutableArray array];
	NSMutableArray* sources    = [NSMutableArray array];
	NSMutableArray* other      = [NSMutableArray array];

	for(NSDictionary* action in actions)
	{
		NSMutableDictionary* item = [NSMutableDictionary dictionaryWithDictionary:action];
		if(!item[@"kind"] && !item[@"edit"] && item[@"command"] && [item[@"command"] isKindOfClass:[NSString class]])
			item[@"_isCommand"] = @YES;

		NSString* kind = item[@"kind"];
		if([kind hasPrefix:@"quickfix"])
			[quickFixes addObject:item];
		else if([kind hasPrefix:@"refactor"])
			[refactors addObject:item];
		else if([kind hasPrefix:@"source"])
			[sources addObject:item];
		else
			[other addObject:item];
	}

	void (^addSection)(NSMenu*, NSString*, NSArray*) = ^(NSMenu* m, NSString* header, NSArray* items) {
		if(items.count == 0)
			return;
		if(m.numberOfItems > 0)
			[m addItem:[NSMenuItem separatorItem]];
		if(header)
		{
			NSMenuItem* headerItem = [[NSMenuItem alloc] initWithTitle:header action:nil keyEquivalent:@""];
			headerItem.enabled = NO;
			NSDictionary* attrs = @{
				NSFontAttributeName: [NSFont systemFontOfSize:11 weight:NSFontWeightMedium],
				NSForegroundColorAttributeName: [NSColor secondaryLabelColor]
			};
			headerItem.attributedTitle = [[NSAttributedString alloc] initWithString:header attributes:attrs];
			[m addItem:headerItem];
		}
		for(NSDictionary* codeAction in items)
		{
			NSString* title = codeAction[@"title"] ?: @"Untitled";
			NSMenuItem* menuItem = [[NSMenuItem alloc] initWithTitle:title action:@selector(performCodeAction:) keyEquivalent:@""];
			menuItem.target = self;
			menuItem.representedObject = codeAction;

			if([codeAction[@"isPreferred"] boolValue])
			{
				NSDictionary* boldAttrs = @{NSFontAttributeName: [NSFont boldSystemFontOfSize:0]};
				menuItem.attributedTitle = [[NSAttributedString alloc] initWithString:title attributes:boldAttrs];
			}

			if(codeAction[@"disabled"])
			{
				menuItem.enabled = NO;
				menuItem.toolTip = codeAction[@"disabled"][@"reason"];
			}

			[m addItem:menuItem];
		}
	};

	addSection(menu, @"Quick Fix", quickFixes);
	addSection(menu, @"Refactor", refactors);
	addSection(menu, @"Source", sources);
	addSection(menu, nil, other);

	[menu popUpMenuPositioningItem:nil atLocation:point inView:self];

	NSEventModifierFlags modifiers = [NSEvent modifierFlags] & (NSEventModifierFlagOption | NSEventModifierFlagCommand);
	self.showDefinitionCursor = (modifiers == NSEventModifierFlagCommand) && [[LSPManager sharedManager] hasClientForDocument:self.document];
}

- (void)performCodeAction:(NSMenuItem*)sender
{
	NSDictionary* action = sender.representedObject;
	if(!action)
		return;

	OakDocument* doc = self.document;
	LSPManager* lsp = [LSPManager sharedManager];

	void(^commandErrorHandler)(id) = ^(id result){ };

	if([action[@"_isCommand"] boolValue])
	{
		[lsp executeCommand:action[@"command"] arguments:action[@"arguments"] forDocument:doc completion:commandErrorHandler];
		return;
	}

	if(action[@"edit"])
	{
		// S2: Set completion handler instead of boolean flag
		_codeActionEditCompletion = ^{ };
		[self applyWorkspaceEdit:action[@"edit"]];
		if(action[@"command"] && [action[@"command"] isKindOfClass:[NSDictionary class]])
		{
			NSDictionary* cmd = action[@"command"];
			[lsp executeCommand:cmd[@"command"] arguments:cmd[@"arguments"] forDocument:doc completion:commandErrorHandler];
		}
		return;
	}

	if([lsp serverSupportsCodeActionResolveForDocument:doc])
	{
		__weak OakTextView* weakSelf = self;
		[lsp resolveCodeAction:action forDocument:doc completion:^(NSDictionary* resolved) {
			dispatch_async(dispatch_get_main_queue(), ^{
				OakTextView* strongSelf = weakSelf;
				if(!strongSelf)
					return;
				if(!resolved)
				return;
				if(resolved[@"edit"])
				{
					// S2: Set completion handler instead of boolean flag
					strongSelf->_codeActionEditCompletion = ^{ };
					[strongSelf applyWorkspaceEdit:resolved[@"edit"]];
				}
				if(resolved[@"command"] && [resolved[@"command"] isKindOfClass:[NSDictionary class]])
				{
					NSDictionary* cmd = resolved[@"command"];
					[lsp executeCommand:cmd[@"command"] arguments:cmd[@"arguments"] forDocument:doc completion:commandErrorHandler];
				}
			});
		}];
	}
	else if(action[@"command"] && [action[@"command"] isKindOfClass:[NSDictionary class]])
	{
		NSDictionary* cmd = action[@"command"];
		[lsp executeCommand:cmd[@"command"] arguments:cmd[@"arguments"] forDocument:doc completion:commandErrorHandler];
	}
}

@end
