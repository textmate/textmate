#import "BufferEditProposal.h"
#import "EditorContextService.h"
#import <document/OakDocument Private.h>
#import <OakFoundation/NSString Additions.h>
#include <xdiff.h>
#import <ns/ns.h>

@implementation BufferEditProposal
{
	NSDictionary* _context;
	std::vector<std::pair<size_t, size_t>> _ranges;
	NSArray<NSString*>* _replacements;
}

- (instancetype)initWithEditPlan:(NSDictionary*)plan summary:(NSString*)summary error:(NSString**)error
{
	if(error) *error = nil;
	if((self = [super init])) {
		_identifier = NSUUID.UUID.UUIDString;
		_documentId = plan[@"document"][@"documentId"];
		_status = @"awaiting_proposal";
		NSMutableArray* selections = [NSMutableArray array];
		NSMutableArray* replacements = [NSMutableArray array];
		for(NSDictionary* edit in plan[@"edits"]) {
			_ranges.emplace_back([edit[@"startByte"] unsignedIntegerValue], [edit[@"endByte"] unsignedIntegerValue]);
			[selections addObject:@{ @"start": edit[@"start"], @"text": edit[@"text"] }];
			[replacements addObject:edit[@"replacement"]];
		}
		_context = @{ @"document": plan[@"document"], @"selections": selections };
		NSString* problem = [self prepareReplacements:replacements summary:summary];
		if(problem) { if(error) *error = problem; return nil; }
	}
	return self;
}

- (BOOL)matchesDocument:(OakDocument*)document
{
	return document.isLoaded && [document.identifier.UUIDString isEqual:self.documentId] &&
		[[EditorContextService.sharedService revisionForDocument:document] isEqual:_context[@"document"][@"revision"]] &&
		[_context[@"document"][@"path"] isEqual:document.path ?: (id)NSNull.null];
}

- (NSString*)prepareReplacements:(NSArray<NSString*>*)replacements summary:(NSString*)summary
{
	if(![self.status isEqual:@"awaiting_proposal"]) return @"This edit proposal has already been prepared or has finished.";
	if(replacements.count != _ranges.size()) return @"Supply exactly one replacement for each edit range.";
	NSMutableString* diff = [NSMutableString string];
	BOOL changed = NO;
	for(NSUInteger i = 0; i < replacements.count; ++i)
	{
		NSString* original = _context[@"selections"][i][@"text"];
		if([original isEqual:replacements[i]]) continue;
		changed = YES;
		[diff appendFormat:@"Edit %lu · line %@\n--- Original\n+++ Proposed\n", (unsigned long)i+1, _context[@"selections"][i][@"start"][@"line"]];
		std::string before = to_s(original), after = to_s(replacements[i]), output;
		mmfile_t a = { before.data(), (long)before.size() }, b = { after.data(), (long)after.size() };
		xpparam_t parameters = {};
		parameters.flags = XDF_HISTOGRAM_DIFF;
		xdemitconf_t config = {};
		config.ctxlen = 3;
		xdemitcb_t callback = {};
		callback.priv = &output;
		callback.out_line = [](void* context, mmbuffer_t* buffers, int count) -> int {
			auto& text = *static_cast<std::string*>(context);
			for(int j = 0; j < count; ++j) text.append(buffers[j].ptr, buffers[j].size);
			return 0;
		};
		if(xdl_diff(&a, &b, &parameters, &config, &callback) < 0) return @"The proposed change could not be compared. Try a smaller edit.";
		[diff appendString:to_ns(output)];
		[diff appendString:@"\n"];
	}
	if(!changed) return @"The proposal does not change the buffer text.";
	_replacements = [replacements copy];
	_summary = [summary copy];
	_diff = diff;
	_status = @"pending_review";
	return nil;
}

- (BOOL)applyToDocument:(OakDocument*)document
{
	if(![self.status isEqual:@"pending_review"]) return NO;
	if(![self matchesDocument:document]) { [self markStale]; return NO; }
	std::multimap<std::pair<size_t, size_t>, std::string> edits;
	for(size_t i = 0; i < _ranges.size(); ++i)
	{
		auto const& range = _ranges[i];
		if(range.second > [document buffer].size() ||
			[document buffer].substr(range.first, range.second) != to_s(_context[@"selections"][i][@"text"]))
		{
			[self markStale];
			return NO;
		}
		edits.emplace(range, to_s(_replacements[i]));
	}
	// Loaded documents use OakDocumentEditor, which groups all replacements
	// into one undo action and notifies every editor displaying this buffer.
	if(![document performReplacements:edits checksum:0]) return NO;
	_status = @"applied";
	return YES;
}

- (void)discard { if([self.status isEqual:@"pending_review"]) _status = @"discarded"; }
- (void)markStale { _status = @"stale"; }

- (EditReviewWindowController*)reviewControllerWithCompletion:(void (^)(NSModalResponse))completion
{
	EditReviewWindowController* controller = [[EditReviewWindowController alloc] initWithTitle:@"Review Buffer Edits" documentName:_context[@"document"][@"name"] applyTitle:@"Apply Edits" completion:completion];
	NSScrollView* scroll = controller.diffScrollView;
	NSTextView* text = [[NSTextView alloc] initWithFrame:scroll.bounds];
	text.editable = NO;
	text.richText = NO;
	text.font = [NSFont monospacedSystemFontOfSize:12 weight:NSFontWeightRegular];
	text.textColor = NSColor.textColor;
	text.backgroundColor = NSColor.textBackgroundColor;
	text.horizontallyResizable = YES;
	text.verticallyResizable = YES;
	text.textContainer.containerSize = NSMakeSize(CGFLOAT_MAX, CGFLOAT_MAX);
	text.textContainer.widthTracksTextView = NO;
	NSString* preview = [NSString stringWithFormat:@"%@\n\n%@", self.summary, self.diff];
	text.string = preview;
	text.accessibilityLabel = @"Proposed code changes. Minus lines are removed; plus lines are added.";
	NSUInteger offset = 0;
	for(NSString* line in [preview componentsSeparatedByString:@"\n"])
	{
		NSColor* color = [line hasPrefix:@"+"] ? NSColor.systemGreenColor : [line hasPrefix:@"-"] ? NSColor.systemRedColor : NSColor.textColor;
		[text.textStorage addAttribute:NSForegroundColorAttributeName value:color range:NSMakeRange(offset, line.length)];
		offset += line.length+1;
	}
	scroll.documentView = text;
	return controller;
}
@end

// A separate window keeps the editor usable while the user reads and positions the diff.
@implementation EditReviewWindowController
{
	void (^_completion)(NSModalResponse);
}
- (instancetype)initWithTitle:(NSString*)title documentName:(NSString*)name applyTitle:(NSString*)applyTitle completion:(void (^)(NSModalResponse))completion
{
	NSWindow* window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 760, 500) styleMask:NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskResizable | NSWindowStyleMaskMiniaturizable backing:NSBackingStoreBuffered defer:NO];
	if((self = [super initWithWindow:window])) {
		_completion = [completion copy];
		window.title = title;
		window.releasedWhenClosed = NO;
		window.contentMinSize = NSMakeSize(440, 260);
		window.delegate = self;
		NSView* content = window.contentView;
		NSTextField* label = [NSTextField labelWithString:name ?: @"Untitled"];
		label.frame = NSMakeRect(20, 466, 720, 20);
		label.lineBreakMode = NSLineBreakByTruncatingMiddle;
		label.autoresizingMask = NSViewWidthSizable | NSViewMinYMargin;
		[content addSubview:label];
		_diffScrollView = [[NSScrollView alloc] initWithFrame:NSMakeRect(20, 64, 720, 390)];
		_diffScrollView.autoresizingMask = NSViewWidthSizable | NSViewHeightSizable;
		_diffScrollView.hasVerticalScroller = YES;
		_diffScrollView.hasHorizontalScroller = YES;
		[content addSubview:_diffScrollView];
		NSTextField* hint = [NSTextField labelWithString:@"Apply changes the buffer. Save when ready."];
		hint.frame = NSMakeRect(20, 16, 460, 32);
		hint.autoresizingMask = NSViewWidthSizable;
		hint.font = [NSFont systemFontOfSize:NSFont.smallSystemFontSize];
		hint.textColor = NSColor.secondaryLabelColor;
		[content addSubview:hint];
		NSButton* discard = [NSButton buttonWithTitle:@"Discard" target:self action:@selector(discard:)];
		discard.frame = NSMakeRect(500, 18, 110, 32);
		discard.autoresizingMask = NSViewMinXMargin;
		discard.keyEquivalent = @"\e";
		[content addSubview:discard];
		NSButton* apply = [NSButton buttonWithTitle:applyTitle target:self action:@selector(apply:)];
		apply.frame = NSMakeRect(620, 18, 120, 32);
		apply.autoresizingMask = NSViewMinXMargin;
		// Proposals may arrive while typing: plain Return must never apply edits.
		apply.keyEquivalent = @"\r";
		apply.keyEquivalentModifierMask = NSEventModifierFlagCommand;
		[content addSubview:apply];
	}
	return self;
}
- (void)finish:(NSModalResponse)response
{
	void (^completion)(NSModalResponse) = _completion;
	_completion = nil;
	[self close];
	if(completion) completion(response);
}
- (IBAction)apply:(id)sender { [self finish:NSAlertFirstButtonReturn]; }
- (IBAction)discard:(id)sender { [self finish:NSAlertSecondButtonReturn]; }
- (void)windowWillClose:(NSNotification*)notification
{
	void (^completion)(NSModalResponse) = _completion;
	_completion = nil;
	if(completion) completion(NSAlertSecondButtonReturn);
}
@end
