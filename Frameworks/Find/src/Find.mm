#import "Find.h"
#import "FindWindowController.h"
#import "FFDocumentSearch.h"
#import "Strings.h"
#import "attr_string.h"
#import <OakFoundation/OakFindProtocol.h>
#import <OakFoundation/NSArray Additions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakAppKit/NSAlert Additions.h>
#import <OakAppKit/OakAppKit.h>
#import <OakAppKit/OakFileIconImage.h>
#import <OakAppKit/OakPasteboard.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <ns/ns.h>
#import <text/tokenize.h>
#import <text/types.h>
#import <text/format.h>
#import <text/utf8.h>
#import <regexp/format_string.h>
#import <editor/editor.h>
#import <document/collection.h>

OAK_DEBUG_VAR(Find_Base);

NSString* const FFSearchInDocument  = @"FFSearchInDocument";
NSString* const FFSearchInSelection = @"FFSearchInSelection";
NSString* const FFSearchInOpenFiles = @"FFSearchInOpenFiles";

enum FindActionTag
{
	FindActionFindNext = 1,
	FindActionFindPrevious,
	FindActionCountMatches,
	FindActionFindAll,
	FindActionReplaceAll,
	FindActionReplaceAndFind,
	FindActionReplaceSelected,
};

static NSAttributedString* PathComponentString (std::string const& path, std::string const& base)
{
	std::vector<std::string> components;
	std::string str = path::relative_to(path, base);
	for(auto const& component : text::tokenize(str.begin(), str.end(), '/'))
		components.push_back(component);
	if(components.front() == "")
		components.front() = path::display_name("/");
	components.back() = "";

	return ns::attr_string_t()
		<< ns::style::line_break(NSLineBreakByTruncatingMiddle)
		<< [NSFont controlContentFontOfSize:0]
		<< [NSColor darkGrayColor]
		<< text::join(std::vector<std::string>(components.begin(), components.end()), " ▸ ")
		<< ns::style::bold
		<< [NSColor blackColor]
		<< (path::is_absolute(path) ? path::display_name(path) : path);
}

static std::string tabs_to_em_spaces (std::string const& str)
{
	std::string res;
	for(char const& ch : str)
	{
		if(ch == '\t')
				res += "\u2003";
		else	res += ch;
	}
	return res;
}

static NSAttributedString* AttributedStringForMatch (std::string const& text, size_t from, size_t to, size_t n)
{
	ns::attr_string_t str;
	str.add(ns::style::line_break(NSLineBreakByTruncatingTail));
	str.add([NSColor darkGrayColor]);
	str.add([NSFont controlContentFontOfSize:11]);

	str.add(text::pad(++n, 4) + ": ");

	bool inMatch = false;
	size_t last = text.size();
	for(size_t it = 0; it != last; )
	{
		size_t eol = std::find(text.begin() + it, text.end(), '\n') - text.begin();

		if(oak::cap(it, from, eol) == from)
		{
			str.add(tabs_to_em_spaces(text.substr(it, from-it)));
			it = from;
			inMatch = true;
		}

		if(inMatch)
		{
			str.add(ns::style::bold);
			str.add([NSColor blackColor]);
		}

		if(inMatch && oak::cap(it, to, eol) == to)
		{
			str.add(tabs_to_em_spaces(text.substr(it, to-it)));
			it = to;
			inMatch = false;

			str.add([NSColor darkGrayColor]);
			str.add(ns::style::unbold);
		}

		str.add(tabs_to_em_spaces(text.substr(it, eol-it)));

		if(eol != last)
		{
			str.add("¬");

			if(inMatch)
			{
				str.add([NSColor darkGrayColor]);
				str.add(ns::style::unbold);
			}

			if(++eol == to)
				inMatch = false;

			if(eol != last)
				str.add("\n" + text::pad(++n, 4) + ": ");
		}

		it = eol;
	}

	return str;
}

@interface FFResultNode : NSObject
{
	document::document_t::callback_t* _callback;
}
@property (nonatomic, weak) FFResultNode* parent;
@property (nonatomic, weak) FFResultNode* next;
@property (nonatomic, weak) FFResultNode* previous;
@property (nonatomic) NSUInteger countOfLeafs;
@property (nonatomic) NSUInteger countOfExcluded;

@property (nonatomic) FFMatch* match;
@property (nonatomic) NSArray* children;
@property (nonatomic) BOOL excluded;
@property (nonatomic) BOOL replacementDone;
@property (nonatomic) NSImage* icon;

@property (nonatomic, readonly) document::document_ptr document;
@property (nonatomic, readonly) NSString* path;
@property (nonatomic, readonly) NSString* identifier;
@property (nonatomic) NSAttributedString* displayPath;
@end

@implementation FFResultNode
+ (FFResultNode*)resultNodeWithMatch:(FFMatch*)aMatch baseDirectory:(NSString*)base
{
	FFResultNode* res = [FFResultNode new];
	res.match       = aMatch;
	res.children    = [NSMutableArray array];
	res.displayPath = PathComponentString(base ? [aMatch match].document->path() : [aMatch match].document->display_name(), to_s(base));
	return res;
}

+ (FFResultNode*)resultNodeWithMatch:(FFMatch*)aMatch
{
	FFResultNode* res = [FFResultNode new];
	res.countOfLeafs = 1;
	res.match = aMatch;
	return res;
}

- (void)dealloc
{
	if(_callback)
	{
		self.document->remove_callback(_callback);
		delete _callback;
	}
}

- (void)setCountOfLeafs:(NSUInteger)countOfLeafs
{
	if(_countOfLeafs == countOfLeafs)
		return;
	_parent.countOfLeafs += countOfLeafs - _countOfLeafs;
	_countOfLeafs = countOfLeafs;
}

- (void)setCountOfExcluded:(NSUInteger)countOfExcluded
{
	if(_countOfExcluded == countOfExcluded)
		return;
	_parent.countOfExcluded += countOfExcluded - _countOfExcluded;
	_countOfExcluded = countOfExcluded;
}

- (void)addResultNode:(FFResultNode*)aMatch
{
	if(!_children)
	{
		_children = [NSMutableArray array];
		if(_countOfLeafs)
			self.countOfLeafs -= 1;
	}

	aMatch.previous      = self.lastResultNode;
	aMatch.previous.next = aMatch;
	aMatch.parent        = self;

	[(NSMutableArray*)_children addObject:aMatch];
	self.countOfLeafs    += aMatch.countOfLeafs;
	self.countOfExcluded += aMatch.countOfExcluded;
}

- (void)removeFromParent
{
	_next.previous = _previous;
	_previous.next = _next;
	[(NSMutableArray*)_parent.children removeObject:self];
	_parent.countOfExcluded -= _countOfExcluded;
	_parent.countOfLeafs    -= _countOfLeafs;
}

- (void)setExcluded:(BOOL)flag
{
	if(_children)
	{
		for(FFResultNode* child in _children)
			child.excluded = flag;
	}
	else
	{
		self.countOfExcluded = flag ? 1 : 0;
	}
}

- (BOOL)excluded
{
	return !_children && _countOfExcluded == 1;
}

- (FFResultNode*)firstResultNode   { return [_children firstObject]; }
- (FFResultNode*)lastResultNode    { return [_children lastObject]; }
- (document::document_ptr)document { return [_match match].document; }
- (NSString*)path                  { return [NSString stringWithCxxString:self.document->path()]; }
- (NSString*)identifier            { return [NSString stringWithCxxString:self.document->identifier()]; }

- (NSUInteger)lineSpan
{
	text::pos_t const from = [_match match].range.from;
	text::pos_t const to   = [_match match].range.to;
	return to.line - from.line + (from == to || to.column != 0 ? 1 : 0);
}

- (NSAttributedString*)excerptWithReplacement:(NSString*)replacementString
{
	find::match_t const& m = [_match match];

	size_t from = m.first - m.excerpt_offset;
	size_t to   = m.last  - m.excerpt_offset;

	std::string prefix = m.excerpt.substr(0, from);
	std::string middle = m.excerpt.substr(from, to - from);
	std::string suffix = m.excerpt.substr(to);

	if(replacementString)
		middle = m.captures.empty() ? to_s(replacementString) : format_string::expand(to_s(replacementString), m.captures);

	if(!utf8::is_valid(prefix.begin(), prefix.end()) && utf8::is_valid(middle.begin(), middle.end()) && utf8::is_valid(suffix.begin(), suffix.end()))
	{
		return ns::attr_string_t()
			<< [NSColor darkGrayColor] << [NSFont controlContentFontOfSize:11]
			<< ns::style::line_break(NSLineBreakByTruncatingTail)
			<< text::format("%zu-%zu: Range is not valid UTF-8, please contact: http://macromates.com/support", m.first, m.last);
	}

	return AttributedStringForMatch(prefix + middle + suffix, prefix.size(), prefix.size() + middle.size(), m.line_number);
}

- (NSImage*)icon
{
	struct document_callback_t : document::document_t::callback_t
	{
		WATCH_LEAKS(document_callback_t);
		document_callback_t (FFResultNode* self) : _self(self) {}
		void handle_document_event (document::document_ptr document, event_t event)
		{
			if(event == did_change_modified_status)
				_self.icon = nil;
		}

	private:
		__weak FFResultNode* _self;
	};

	if(!_icon)
		_icon = [OakFileIconImage fileIconImageWithPath:self.path isModified:self.document->is_modified()];
	if(!_callback)
		self.document->add_callback(_callback = new document_callback_t(self));

	return _icon;
}
@end

@interface OakAttributedStringTextFieldCell : NSTextFieldCell
@property (nonatomic) NSAttributedString* monoColoredAttributedString;
@end

@implementation OakAttributedStringTextFieldCell
- (void)setBackgroundStyle:(NSBackgroundStyle)style
{
	if(_monoColoredAttributedString)
	{
		if(style == NSBackgroundStyleDark)
		{
			NSMutableAttributedString* str = [_monoColoredAttributedString mutableCopy];
			[str addAttribute:NSForegroundColorAttributeName value:[NSColor alternateSelectedControlTextColor] range:NSMakeRange(0, [str length])];
			[super setAttributedStringValue:str];
		}
		else
		{
			[super setAttributedStringValue:_monoColoredAttributedString];
		}
	}
	[super setBackgroundStyle:style];
}

- (void)setAttributedStringValue:(NSAttributedString*)str
{
	_monoColoredAttributedString = str;
	[super setAttributedStringValue:str];
}
@end

@interface OakSearchResultsHeaderCellView : NSTableCellView
@property (nonatomic) NSString* countOfLeafs;
@property (nonatomic) NSButton* countOfLeafsButton;
@property (nonatomic) NSButton* removeButton;
@end

@implementation OakSearchResultsHeaderCellView
- (id)initWithOutlineView:(NSOutlineView*)anOutlineView
{
	if((self = [super init]))
	{
		NSImageView* imageView = [NSImageView new];
		NSTextField* textField = OakCreateLabel();
		textField.font = [NSFont controlContentFontOfSize:0];

		NSButton* countOfLeafs = [NSButton new];
		[[countOfLeafs cell] setHighlightsBy:NSNoCellMask];
		countOfLeafs.alignment  = NSCenterTextAlignment;
		countOfLeafs.bezelStyle = NSInlineBezelStyle;
		countOfLeafs.font       = [NSFont labelFontOfSize:0];
		countOfLeafs.identifier = @"countOfLeafs";

		NSButton* remove = [NSButton new];
		[[remove cell] setControlSize:NSSmallControlSize];
		remove.bezelStyle = NSRoundRectBezelStyle;
		remove.buttonType = NSMomentaryPushInButton;
		remove.image      = [NSImage imageNamed:NSImageNameRemoveTemplate];

		NSDictionary* views = @{ @"icon" : imageView, @"text" : textField, @"count" : countOfLeafs, @"remove" : remove };
		for(NSView* child in [views allValues])
		{
			[child setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:child];
		}

		[textField setContentHuggingPriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[countOfLeafs setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(6)-[icon(==16)]-(3)-[text]-(>=8)-[remove(==16)]-(12)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[text]-(4)-[count]"                                        options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[count]-(>=4)-[remove]"                                    options:0 metrics:nil views:views]];
		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[icon(==16,==remove)]-(3)-|"                               options:0 metrics:nil views:views]];

		[imageView bind:NSValueBinding toObject:self withKeyPath:@"objectValue.icon" options:nil];
		[textField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.displayPath" options:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(outlineViewItemDidExpandCollapse:) name:NSOutlineViewItemDidExpandNotification object:anOutlineView];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(outlineViewItemDidExpandCollapse:) name:NSOutlineViewItemDidCollapseNotification object:anOutlineView];

		self.imageView          = imageView;
		self.textField          = textField;
		self.countOfLeafsButton = countOfLeafs;
		self.removeButton       = remove;
	}
	return self;
}

- (void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
}

- (void)setCountOfLeafs:(NSString*)aString
{
	_countOfLeafs = aString;
	_countOfLeafsButton.title  = aString ?: @"0";
	_countOfLeafsButton.hidden = aString == nil;
}

- (void)outlineViewItemDidExpandCollapse:(NSNotification*)aNotification
{
	NSOutlineView* outlineView = [aNotification object];
	NSDictionary* userInfo = [aNotification userInfo];
	FFResultNode* item = userInfo[@"NSObject"];
	if(item == self.objectValue)
		_countOfLeafsButton.hidden = [outlineView isItemExpanded:item];
}

- (void)outlineViewItemDidExpand:(NSNotification*)aNotification   { [self outlineViewItemDidExpandCollapse:aNotification]; }
- (void)outlineViewItemDidCollapse:(NSNotification*)aNotification { [self outlineViewItemDidExpandCollapse:aNotification]; }
@end

@interface Find () <OakFindServerProtocol, NSOutlineViewDataSource, NSOutlineViewDelegate>
@property (nonatomic) FindWindowController* windowController;
@property (nonatomic) FFDocumentSearch* documentSearch;
@property (nonatomic) FFResultNode* results;
@property (nonatomic) NSUInteger countOfMatches;
@property (nonatomic) NSUInteger countOfExcludedMatches;
@property (nonatomic) BOOL closeWindowOnSuccess;
@property (nonatomic) BOOL performingFolderSearch;
@property (nonatomic) BOOL performedReplaceAll;
@property (nonatomic) BOOL performedSaveAll;

// =========================
// = OakFindProtocolServer =
// =========================

@property (nonatomic) find_operation_t findOperation;
@property (nonatomic) find::options_t  findOptions;

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap;
- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString;
@end

NSString* const FFFindWasTriggeredByEnter = @"FFFindWasTriggeredByEnter";

@implementation Find
+ (Find*)sharedInstance
{
	static Find* instance = [Find new];
	return instance;
}

- (FindWindowController*)windowController
{
	if(!_windowController)
	{
		_windowController = [FindWindowController new];
		_windowController.nextResponder = self;
		_windowController.resultsOutlineView.action       = @selector(didSingleClickResultsOutlineView:);
		_windowController.resultsOutlineView.doubleAction = @selector(didDoubleClickResultsOutlineView:);
		_windowController.resultsOutlineView.target       = self;
		_windowController.resultsOutlineView.dataSource   = self;
		_windowController.resultsOutlineView.delegate     = self;

		[_windowController.replaceAllButton bind:@"title" toObject:self withKeyPath:@"replaceAllButtonTitle" options:nil];
		[_windowController.replaceAllButton bind:@"enabled" toObject:self withKeyPath:@"canReplaceAll" options:nil];
		[_windowController.window bind:@"documentEdited" toObject:self withKeyPath:@"hasUnsavedChanges" options:nil];

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(windowWillClose:) name:NSWindowWillCloseNotification object:_windowController.window];
	}
	return _windowController;
}

- (void)windowWillClose:(NSNotification*)aNotification
{
	[self stopSearch:self];
}

// ====================================
// = Actions for displaying the panel =
// ====================================

- (void)showFindWindowFor:(NSString*)searchScope
{
	self.windowController.searchIn = searchScope;
	[self.windowController showWindow:self];
}

- (IBAction)showFolderSelectionPanel:(id)sender
{
	NSOpenPanel* openPanel = [NSOpenPanel openPanel];
	openPanel.title = @"Find in Folder";
	openPanel.canChooseFiles = NO;
	openPanel.canChooseDirectories = YES;
	if(NSString* folder = self.searchFolder)
		openPanel.directoryURL = [NSURL fileURLWithPath:folder];
	if([self.windowController isWindowLoaded] && [self.windowController.window isVisible])
	{
		[openPanel beginSheetModalForWindow:self.windowController.window completionHandler:^(NSInteger result) {
			if(result == NSOKButton)
				[self showFindWindowFor:[[[[openPanel URLs] lastObject] filePathURL] path]];
			else if([self isVisible]) // Reset selected item in pop-up button
				self.windowController.searchIn = self.windowController.searchIn;
		}];
	}
	else
	{
		[openPanel beginWithCompletionHandler:^(NSInteger result) {
			if(result == NSOKButton)
				[self showFindWindowFor:[[[[openPanel URLs] lastObject] filePathURL] path]];
		}];
	}
}

// ================
// = Find actions =
// ================

+ (NSSet*)keyPathsForValuesAffectingCanReplaceAll         { return [NSSet setWithArray:@[ @"performedReplaceAll", @"countOfMatches", @"countOfExcludedMatches", @"windowController.showsResultsOutlineView" ]]; }
+ (NSSet*)keyPathsForValuesAffectingHasUnsavedChanges     { return [NSSet setWithArray:@[ @"performedReplaceAll", @"performedSaveAll" ]]; }
+ (NSSet*)keyPathsForValuesAffectingReplaceAllButtonTitle { return [NSSet setWithArray:@[ @"canReplaceAll", @"countOfExcludedMatches", @"windowController.showsResultsOutlineView" ]]; }

- (BOOL)canReplaceAll                { return _windowController.showsResultsOutlineView ? (!_performedReplaceAll && _countOfExcludedMatches < _countOfMatches) : YES; }
- (BOOL)hasUnsavedChanges            { return _performedReplaceAll && !_performedSaveAll; }
- (NSString*)replaceAllButtonTitle   { return _windowController.showsResultsOutlineView && _countOfExcludedMatches && self.canReplaceAll ? @"Replace Selected" : @"Replace All"; }

- (IBAction)countOccurrences:(id)sender   { [self performFindAction:FindActionCountMatches   withWindowController:self.windowController]; }
- (IBAction)findAll:(id)sender            { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findAllInSelection:(id)sender { [self performFindAction:FindActionFindAll        withWindowController:self.windowController]; }
- (IBAction)findNext:(id)sender           { [self performFindAction:FindActionFindNext       withWindowController:self.windowController]; }
- (IBAction)findPrevious:(id)sender       { [self performFindAction:FindActionFindPrevious   withWindowController:self.windowController]; }
- (IBAction)replaceAll:(id)sender         { [self performFindAction:FindActionReplaceAll     withWindowController:self.windowController]; }
- (IBAction)replaceAndFind:(id)sender     { [self performFindAction:FindActionReplaceAndFind withWindowController:self.windowController]; }

- (IBAction)stopSearch:(id)sender
{
	if(_performingFolderSearch)
	{
		[_documentSearch stop];
		self.windowController.statusString = @"Stopped.";
	}
}

// These are disabled via menu validation
- (IBAction)saveDocument:(id)sender   { }
- (IBAction)saveDocumentAs:(id)sender { }

- (IBAction)saveAllDocuments:(id)sender
{
	NSUInteger fileCount = 0;
	std::vector<document::document_ptr> failedDocs;

	for(FFResultNode* parent in _results.children)
	{
		for(FFResultNode* child in parent.children)
		{
			if(!child.replacementDone)
				continue;

			if(document::document_ptr doc = child.document)
			{
				if(doc->sync_save(kCFRunLoopDefaultMode))
						++fileCount;
				else	failedDocs.push_back(doc);
			}
			break;
		}
	}

	if(failedDocs.empty())
	{
		self.windowController.statusString = [NSString stringWithFormat:@"%lu file%s saved.", fileCount, fileCount == 1 ? "" : "s"];
		self.performedSaveAll = YES;
	}
	else
	{
		NSBeep();
		self.windowController.statusString = [NSString stringWithFormat:@"%zu file%s failed to save.", failedDocs.size(), failedDocs.size() == 1 ? "" : "s"];
	}
}

- (void)showSaveWarningWithCompletionHandler:(void(^)())callback
{
	NSAlert* alert = [NSAlert tmAlertWithMessageText:@"Do you want to save the changes from your replace operation?" informativeText:@"Your changes will be lost if you don’t save them." buttons:@"Save All", @"Cancel", @"Don’t Save", nil];
	OakShowAlertForWindow(alert, _windowController.window, ^(NSInteger returnCode){
		if(returnCode == NSAlertFirstButtonReturn) // Save All
		{
			[self saveAllDocuments:self];
		}
		else if(returnCode == NSAlertThirdButtonReturn) // Discard
		{
			// FIXME Undo replacements
			self.performedReplaceAll = NO;
		}
		else if(returnCode == NSAlertSecondButtonReturn) // Cancel
			;

		if(!self.hasUnsavedChanges)
			callback();
	});
}

- (BOOL)windowShouldClose:(id)sender
{
	if(!self.hasUnsavedChanges)
		return YES;

	[self showSaveWarningWithCompletionHandler:^{ [_windowController close]; }];
	return NO;
}

- (void)performFindAction:(FindActionTag)action withWindowController:(FindWindowController*)controller
{
	[controller updateFindErrorString];
	if(controller.findErrorString != nil)
		return;

	if(self.hasUnsavedChanges)
		return [self showSaveWarningWithCompletionHandler:^{ [self performFindAction:action withWindowController:controller]; }];

	_findOptions = (controller.regularExpression ? find::regular_expression : find::none) | (controller.ignoreWhitespace ? find::ignore_whitespace : find::none) | (controller.fullWords ? find::full_words : find::none) | (controller.ignoreCase ? find::ignore_case : find::none) | (controller.wrapAround ? find::wrap_around : find::none);
	if(action == FindActionFindPrevious)
		_findOptions |= find::backwards;
	else if(action == FindActionCountMatches || action == FindActionFindAll || action == FindActionReplaceAll)
		_findOptions |= find::all_matches;

	NSString* folder = controller.searchFolder;
	if(folder || [controller.searchIn isEqualToString:FFSearchInOpenFiles] || (action == FindActionFindAll && [controller.searchIn isEqualToString:FFSearchInDocument] && self.documentIdentifier))
	{
		switch(action)
		{
			case FindActionFindAll:
			{
				[self clearMatches];

				FFDocumentSearch* folderSearch = [FFDocumentSearch new];
				folderSearch.searchString = controller.findString;
				folderSearch.options      = _findOptions;

				if(self.documentIdentifier && [controller.searchIn isEqualToString:FFSearchInDocument])
				{
					folderSearch.documentIdentifier = self.documentIdentifier;
				}
				else if([controller.searchIn isEqualToString:FFSearchInOpenFiles])
				{
					folderSearch.directory = [NSString stringWithCxxString:find::kSearchOpenFiles];
				}
				else
				{
					auto const settings = settings_for_path(NULL_STR, "", to_s(folder));

					path::glob_list_t globs;
					globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesInFolderSearchKey, NULL_STR), path::kPathItemDirectory);
					globs.add_exclude_glob(settings.get(kSettingsExcludeDirectoriesKey,               NULL_STR), path::kPathItemDirectory);
					globs.add_exclude_glob(settings.get(kSettingsExcludeFilesInFolderSearchKey,       NULL_STR), path::kPathItemFile);
					globs.add_exclude_glob(settings.get(kSettingsExcludeFilesKey,                     NULL_STR), path::kPathItemFile);
					for(auto key : { kSettingsExcludeInFolderSearchKey, kSettingsExcludeKey, kSettingsBinaryKey })
						globs.add_exclude_glob(settings.get(key, NULL_STR));
					globs.add_include_glob(controller.searchHiddenFolders ? "{,.}*" : "*", path::kPathItemDirectory);
					globs.add_include_glob(to_s(controller.globString), path::kPathItemFile);

					folderSearch.directory         = folder;
					folderSearch.globList          = globs;
					folderSearch.searchFolderLinks = controller.searchFolderLinks;
					folderSearch.searchFileLinks   = controller.searchFileLinks;
					folderSearch.searchBinaryFiles = controller.searchBinaryFiles;
				}

				self.documentSearch = folderSearch;
			}
			break;

			case FindActionReplaceAll:
			case FindActionReplaceSelected:
			{
				NSUInteger replaceCount = 0, fileCount = 0;
				std::string replaceString = to_s(controller.replaceString);

				for(FFResultNode* parent in _results.children)
				{
					std::multimap<std::pair<size_t, size_t>, std::string> replacements;
					for(FFResultNode* child in parent.children)
					{
						if(child.excluded || child.replacementDone)
							continue;
						child.replacementDone = YES;
						replacements.emplace(std::make_pair([child.match match].first, [child.match match].last), controller.regularExpression ? format_string::expand(replaceString, [child.match match].captures) : replaceString);
					}

					if(replacements.empty())
						continue;

					if(document::document_ptr doc = parent.document)
					{
						if(doc->is_open())
						{
							ng::editor_ptr editor = ng::editor_for_document(doc);
							doc->undo_manager().begin_undo_group(editor->ranges());
							editor->perform_replacements(replacements);
							doc->undo_manager().end_undo_group(editor->ranges());
						}
						else
						{
							doc->replace(replacements);
						}

						replaceCount += replacements.size();
						++fileCount;
					}
				}
				self.windowController.statusString = [NSString stringWithFormat:MSG_REPLACE_ALL_RESULTS, replaceCount, fileCount];
				self.performedReplaceAll = YES;
				[_windowController.resultsOutlineView reloadDataForRowIndexes:[NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, [_windowController.resultsOutlineView numberOfRows])] columnIndexes:[NSIndexSet indexSetWithIndex:1]];
			}
			break;

			case FindActionFindNext:     [self selectNextResult:self];     break;
			case FindActionFindPrevious: [self selectPreviousResult:self]; break;
		}
	}
	else
	{
		bool onlySelection = [controller.searchIn isEqualToString:FFSearchInSelection];
		switch(action)
		{
			case FindActionFindNext:
			case FindActionFindPrevious:
			case FindActionFindAll:        _findOperation = onlySelection ? kFindOperationFindInSelection       : kFindOperationFind;       break;
			case FindActionCountMatches:   _findOperation = onlySelection ? kFindOperationCountInSelection      : kFindOperationCount;      break;
			case FindActionReplaceAll:     _findOperation = onlySelection ? kFindOperationReplaceAllInSelection : kFindOperationReplaceAll; break;
			case FindActionReplaceAndFind: _findOperation = kFindOperationReplaceAndFind;                                                   break;
		}

		self.closeWindowOnSuccess = action == FindActionFindNext && [[NSApp currentEvent] type] == NSKeyDown && to_s([NSApp currentEvent]) == utf8::to_s(NSCarriageReturnCharacter);
		[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = nil;
		[NSApp sendAction:@selector(performFindOperation:) to:nil from:self];
	}
}

- (NSString*)findString    { return self.windowController.findString    ?: @""; }
- (NSString*)replaceString { return self.windowController.replaceString ?: @""; }

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap
{
	static std::string const formatStrings[4][3] = {
		{ "No more occurrences of “${found}”.", "Found “${found}”${line:+ at line ${line}, column ${column}}.",               "${count} occurrences of “${found}”." },
		{ "No more matches for “${found}”.",    "Found one match for “${found}”${line:+ at line ${line}, column ${column}}.", "${count} matches for “${found}”."    },
	};

	std::map<std::string, std::string> variables;
	variables["count"]  = std::to_string(aNumber);
	variables["found"]  = to_s(aFindString);
	variables["line"]   = aPosition ? std::to_string(aPosition.line + 1)   : NULL_STR;
	variables["column"] = aPosition ? std::to_string(aPosition.column + 1) : NULL_STR;
	self.windowController.statusString = [NSString stringWithCxxString:format_string::expand(formatStrings[(_findOptions & find::regular_expression) ? 1 : 0][std::min<size_t>(aNumber, 2)], variables)];

	NSResponder* keyView = [[NSApp keyWindow] firstResponder];
	id element = [keyView respondsToSelector:@selector(cell)] ? [keyView performSelector:@selector(cell)] : keyView;
	if([element respondsToSelector:@selector(accessibilityIsIgnored)] && ![element accessibilityIsIgnored])
		NSAccessibilityPostNotificationWithUserInfo(element, NSAccessibilityAnnouncementRequestedNotification, @{ NSAccessibilityAnnouncementKey : self.windowController.statusString });

	if(self.closeWindowOnSuccess && aNumber != 0)
		return [self.windowController close];
}

- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString
{
	static NSString* const formatStrings[2][3] = {
		{ @"Nothing replaced (no occurrences of “%@”).", @"Replaced one occurrence of “%@”.", @"Replaced %2$ld occurrences of “%@”." },
		{ @"Nothing replaced (no matches for “%@”).",    @"Replaced one match of “%@”.",      @"Replaced %2$ld matches of “%@”."     }
	};
	NSString* format = formatStrings[(_findOptions & find::regular_expression) ? 1 : 0][aNumber > 2 ? 2 : aNumber];
	self.windowController.statusString = [NSString stringWithFormat:format, aFindString, aNumber];
}

// =============
// = Accessors =
// =============

- (void)setProjectFolder:(NSString*)folder { self.windowController.projectFolder = folder; }
- (NSString*)projectFolder                 { return self.windowController.projectFolder; }
- (NSString*)searchFolder                  { return self.windowController.searchFolder; }
- (BOOL)isVisible                          { return self.windowController.window.isVisible; }

// ===========
// = Options =
// ===========

- (IBAction)takeFindOptionToToggleFrom:(id)sender
{
	ASSERT([sender respondsToSelector:@selector(tag)]);

	find::options_t option = find::options_t([sender tag]);
	switch(option)
	{
		case find::full_words:         self.windowController.fullWords         = !self.windowController.fullWords;         break;
		case find::ignore_case:        self.windowController.ignoreCase        = !self.windowController.ignoreCase;        break;
		case find::ignore_whitespace:  self.windowController.ignoreWhitespace  = !self.windowController.ignoreWhitespace;  break;
		case find::regular_expression: self.windowController.regularExpression = !self.windowController.regularExpression; break;
		case find::wrap_around:        self.windowController.wrapAround        = !self.windowController.wrapAround;        break;
		default:
			ASSERTF(false, "Unknown find option tag %d\n", option);
	}

	if([[[[OakPasteboard pasteboardWithName:NSFindPboard] current] string] isEqualToString:self.windowController.findString])
		[self.windowController commitEditing]; // update the options on the pasteboard immediately if the find string has not been changed
}

// ====================
// = Search in Folder =
// ====================

- (void)clearMatches
{
	if(_results)
	{
		for(FFResultNode* parent in _results.children)
		{
			if(document::document_ptr doc = parent.document)
				doc->remove_all_marks("search");
		}

		[self unbind:@"countOfMatches"];
		[self unbind:@"countOfExcludedMatches"];
	}

	_results = [FFResultNode new];
	[self bind:@"countOfMatches" toObject:_results withKeyPath:@"countOfLeafs" options:nil];
	[self bind:@"countOfExcludedMatches" toObject:_results withKeyPath:@"countOfExcluded" options:nil];
	[_windowController.resultsOutlineView reloadData];
}

- (void)setDocumentSearch:(FFDocumentSearch*)newSearcher
{
	if(_documentSearch)
	{
		[_documentSearch removeObserver:self forKeyPath:@"currentPath"];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] removeObserver:self name:FFDocumentSearchDidFinishNotification object:_documentSearch];
	}

	if(_documentSearch = newSearcher)
	{
		self.performedReplaceAll = NO;
		self.performedSaveAll    = NO;

		self.windowController.busy                     = YES;
		self.windowController.statusString             = MSG_SEARCHING_FMT;
		self.windowController.showsResultsOutlineView  = YES;
		self.windowController.disableResultsCheckBoxes = _documentSearch.documentIdentifier != nil;

		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidReceiveResults:) name:FFDocumentSearchDidReceiveResultsNotification object:_documentSearch];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(folderSearchDidFinish:) name:FFDocumentSearchDidFinishNotification object:_documentSearch];
		[_documentSearch addObserver:self forKeyPath:@"currentPath" options:(NSKeyValueObservingOptionNew|NSKeyValueObservingOptionOld) context:NULL];
		self.performingFolderSearch = YES;
		[_documentSearch start];
	}
}

- (void)folderSearchDidReceiveResults:(NSNotification*)aNotification
{
	NSUInteger countOfExistingItems = _results.children.count;

	NSArray* matches = [aNotification userInfo][@"matches"];
	FFResultNode* parent = nil;
	for(FFMatch* match in matches)
	{
		if(document::document_ptr doc = [match match].document)
			doc->add_mark([match match].range, "search");

		FFResultNode* node = [FFResultNode resultNodeWithMatch:match];
		if(!parent || parent.document->identifier() != node.document->identifier())
			[_results addResultNode:(parent = [FFResultNode resultNodeWithMatch:match baseDirectory:_documentSearch.directory])];
		[parent addResultNode:node];
	}

	NSIndexSet* newItemsIndexes = [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(countOfExistingItems, _results.children.count - countOfExistingItems)];
	[self.windowController.resultsOutlineView beginUpdates];
	[self.windowController.resultsOutlineView insertItemsAtIndexes:newItemsIndexes inParent:nil withAnimation:0];
	for(FFResultNode* item in [_results.children objectsAtIndexes:newItemsIndexes])
		[self.windowController.resultsOutlineView expandItem:item];
	[self.windowController.resultsOutlineView endUpdates];
}

- (void)addResultsToPasteboard:(id)sender
{
	NSMutableArray* documents = [NSMutableArray array];
	for(FFResultNode* parent in _results.children)
	{
		[documents addObject:@{
			@"identifier"      : [NSString stringWithCxxString:parent.firstResultNode.document->identifier()],
			@"firstMatchRange" : [NSString stringWithCxxString:[parent.firstResultNode.match match].range],
			@"lastMatchRange"  : [NSString stringWithCxxString:[parent.lastResultNode.match match].range],
		}];
	}
	[OakPasteboard pasteboardWithName:NSFindPboard].auxiliaryOptionsForCurrent = @{ @"documents" : documents };
}

- (void)folderSearchDidFinish:(NSNotification*)aNotification
{
	self.performingFolderSearch = NO;
	self.windowController.busy = NO;
	if(!_documentSearch)
		return;

	[self addResultsToPasteboard:self];

	NSString* fmt = MSG_ZERO_MATCHES_FMT;
	switch(self.countOfMatches)
	{
		case 0:  fmt = MSG_ZERO_MATCHES_FMT;     break;
		case 1:  fmt = MSG_ONE_MATCH_FMT;        break;
		default: fmt = MSG_MULTIPLE_MATCHES_FMT; break;
	}

	NSString* msg = [NSString stringWithFormat:fmt, [_documentSearch searchString], [NSNumberFormatter localizedStringFromNumber:@(self.countOfMatches) numberStyle:NSNumberFormatterDecimalStyle]];
	if(_documentSearch.documentIdentifier)
	{
		self.windowController.statusString = msg;
	}
	else
	{
		self.windowController.statusString          = [msg stringByAppendingFormat:([_documentSearch scannedFileCount] == 1 ? MSG_SEARCHED_FILES_ONE : MSG_SEARCHED_FILES_MULTIPLE), [NSNumberFormatter localizedStringFromNumber:@([_documentSearch scannedFileCount]) numberStyle:NSNumberFormatterDecimalStyle], [_documentSearch searchDuration]];
		self.windowController.alternateStatusString = [msg stringByAppendingFormat:MSG_SEARCHED_BYTES, [NSString stringWithCxxString:text::format_size([_documentSearch scannedByteCount])], [_documentSearch searchDuration]];
	}
}

- (void)takeSearchResultToRemoveFrom:(id)sender
{
	FFResultNode* item;

	NSInteger row = [_windowController.resultsOutlineView rowForView:sender];
	if(row != -1)
		item = [_windowController.resultsOutlineView itemAtRow:row];
	else if([sender respondsToSelector:@selector(objectValue)] && [[sender objectValue] isKindOfClass:[FFResultNode class]])
		item = [sender objectValue];

	if(item)
	{
		NSUInteger index = [item.parent.children indexOfObject:item];
		if(index == NSNotFound)
			return;

		if(OakIsAlternateKeyOrMouseEvent())
		{
			if(item.document->path() != NULL_STR)
			{
				std::string path = path::relative_to(item.document->path(), to_s(_documentSearch.directory));
				NSString* newGlob = [_windowController.globString stringByAppendingFormat:@"~%@", [NSString stringWithCxxString:path]];
				_windowController.globString = newGlob;
			}
		}

		[item removeFromParent];
		[self addResultsToPasteboard:self];

		NSString* fmt = MSG_SHOWING_ZERO_MATCHES_FMT;
		switch(self.countOfMatches)
		{
			case 0:  fmt = MSG_SHOWING_ZERO_MATCHES_FMT;     break;
			case 1:  fmt = MSG_SHOWING_ONE_MATCH_FMT;        break;
			default: fmt = MSG_SHOWING_MULTIPLE_MATCHES_FMT; break;
		}
		_windowController.statusString = [NSString stringWithFormat:fmt, [_documentSearch searchString], [NSNumberFormatter localizedStringFromNumber:@(self.countOfMatches) numberStyle:NSNumberFormatterDecimalStyle]];

		[_windowController.resultsOutlineView removeItemsAtIndexes:[NSIndexSet indexSetWithIndex:index] inParent:nil withAnimation:NSTableViewAnimationEffectFade|NSTableViewAnimationSlideDown];
	}
}

- (void)observeValueForKeyPath:(NSString*)keyPath ofObject:(id)object change:(NSDictionary*)change context:(void*)context
{
	if([keyPath isEqualToString:@"currentPath"])
	{
		id newValue = [change objectForKey:NSKeyValueChangeNewKey], oldValue = [change objectForKey:NSKeyValueChangeOldKey];
		std::string searchPath     = [newValue respondsToSelector:@selector(UTF8String)] ? [newValue UTF8String] : "";
		std::string lastSearchPath = [oldValue respondsToSelector:@selector(UTF8String)] ? [oldValue UTF8String] : "";

		// Show only the directory part unless the file name hasn’t changed since last poll of the scanner
		if(searchPath != lastSearchPath && !path::is_directory(searchPath))
			searchPath = path::parent(searchPath);

		std::string relative = path::relative_to(searchPath, to_s(self.searchFolder));
		if(path::is_directory(searchPath))
			relative += "/";

		self.windowController.statusString = [NSString localizedStringWithFormat:MSG_SEARCHING_FOLDER_FMT, [NSString stringWithCxxString:relative]];
	}
}

// ============================
// = Outline view data source =
// ============================

- (NSInteger)outlineView:(NSOutlineView*)outlineView numberOfChildrenOfItem:(FFResultNode*)item
{
	return [(item ?: _results).children count];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isItemExpandable:(FFResultNode*)item
{
	return [self outlineView:outlineView isGroupItem:item];
}

- (id)outlineView:(NSOutlineView*)outlineView child:(NSInteger)childIndex ofItem:(FFResultNode*)item
{
	return [(item ?: _results).children objectAtIndex:childIndex];
}

- (BOOL)outlineView:(NSOutlineView*)outlineView isGroupItem:(FFResultNode*)item
{
	return [outlineView levelForItem:item] == 0;
}

- (CGFloat)outlineView:(NSOutlineView*)outlineView heightOfRowByItem:(FFResultNode*)item
{
	return [self outlineView:outlineView isGroupItem:item] ? 22 : item.lineSpan * [outlineView rowHeight];
}

// ============================
// = View-based NSOutlineView =
// ============================

- (void)toggleExcludedCheckbox:(NSButton*)sender
{
	NSInteger row = [_windowController.resultsOutlineView rowForView:sender];
	if(row != -1)
	{
		FFResultNode* item = [_windowController.resultsOutlineView itemAtRow:row];

		BOOL toggleAllInGroup = OakIsAlternateKeyOrMouseEvent();
		if(toggleAllInGroup)
			item.parent.excluded = item.excluded;

		if(_windowController.showReplacementPreviews)
		{
			NSRange range = NSMakeRange(row, 1);
			if(toggleAllInGroup)
				range = NSMakeRange([_windowController.resultsOutlineView rowForItem:item.parent.firstResultNode], item.parent.countOfLeafs);
			[_windowController.resultsOutlineView reloadDataForRowIndexes:[NSIndexSet indexSetWithIndexesInRange:range] columnIndexes:[NSIndexSet indexSetWithIndex:1]];
		}
	}
}

- (NSView*)outlineView:(NSOutlineView*)outlineView viewForTableColumn:(NSTableColumn*)tableColumn item:(FFResultNode*)item
{
	NSString* identifier = tableColumn.identifier ?: @"group";
	id res = [outlineView makeViewWithIdentifier:identifier owner:self];

	if([identifier isEqualToString:@"checkbox"])
	{
		NSButton* button = res;
		if(!button)
		{
			res = button = OakCreateCheckBox(nil);
			button.identifier = identifier;
			[[button cell] setControlSize:NSSmallControlSize];
			[button bind:@"enabled" toObject:self withKeyPath:@"performedReplaceAll" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
			button.action = @selector(toggleExcludedCheckbox:);
			button.target = self;
		}
		else
		{
			[button unbind:NSValueBinding];
		}

		[button bind:NSValueBinding toObject:item withKeyPath:@"excluded" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
		button.state = item.excluded ? NSOffState : NSOnState;
	}
	else if([identifier isEqualToString:@"match"])
	{
		NSTextField* textField = res;
		if(!textField)
		{
			res = textField = OakCreateLabel(@"");
			textField.identifier = identifier;
			[textField setCell:[OakAttributedStringTextFieldCell new]];
		}
		[textField.cell setAttributedStringValue:[item excerptWithReplacement:(item.replacementDone || !item.excluded && self.windowController.showReplacementPreviews) ? self.replaceString : nil]];
	}
	else
	{
		OakSearchResultsHeaderCellView* cellView = res;
		if(!cellView)
		{
			res = cellView = [[OakSearchResultsHeaderCellView alloc] initWithOutlineView:outlineView];
			cellView.identifier = identifier;
			cellView.removeButton.action = @selector(takeSearchResultToRemoveFrom:);
			cellView.removeButton.target = self;
		}

		cellView.objectValue = item;
		cellView.countOfLeafsButton.title = [NSString stringWithFormat:@"%lu", item.countOfLeafs];
		cellView.countOfLeafsButton.hidden = [outlineView isItemExpanded:item];
	}
	return res;
}

// ======================
// = Should Select Item =
// ======================

- (BOOL)outlineView:(NSOutlineView*)outlineView shouldSelectItem:(FFResultNode*)item
{
	if([self outlineView:outlineView isGroupItem:item])
		return NO;

	NSInteger clickedColumn = [outlineView clickedColumn];
	NSTableColumn* col = clickedColumn != -1 ? [[outlineView tableColumns] objectAtIndex:clickedColumn] : nil;
	if([[col identifier] isEqualToString:@"checkbox"])
		return NO;

	return YES;
}

// =============================
// = Selecting Results Actions =
// =============================

- (void)didDoubleClickResultsOutlineView:(id)sender
{
	NSOutlineView* outlineView = self.windowController.resultsOutlineView;
	NSInteger clickedColumn = [outlineView clickedColumn];
	NSTableColumn* col = clickedColumn != -1 ? [[outlineView tableColumns] objectAtIndex:clickedColumn] : nil;
	if([[col identifier] isEqualToString:@"checkbox"])
		return;

	[self.windowController close];
}

- (void)openDocumentForSelectedRow:(id)sender
{
	NSOutlineView* outlineView = self.windowController.resultsOutlineView;
	if([outlineView numberOfSelectedRows] == 1)
	{
		NSUInteger selectionIndex  = [[outlineView selectedRowIndexes] firstIndex];
		FFResultNode* selectedNode = [outlineView itemAtRow:selectionIndex];
		auto doc = selectedNode.document;
		if(!doc->is_open())
			doc->set_recent_tracking(false);
		document::show(doc, self.projectIdentifier ? oak::uuid_t(to_s(self.projectIdentifier)) : document::kCollectionAny, [selectedNode.match match].range, false);
	}
}

- (void)didSingleClickResultsOutlineView:(id)sender
{
	[self openDocumentForSelectedRow:self];
}

- (void)outlineViewSelectionDidChange:(NSNotification*)aNotification
{
	if([[NSApp currentEvent] type] != NSLeftMouseUp)
		[self openDocumentForSelectedRow:self];
}

// ===========================
// = Expand/Collapse Results =
// ===========================

- (BOOL)resultsCollapsed
{
	NSUInteger expanded = 0;
	for(FFResultNode* parent in _results.children)
		expanded += [_windowController.resultsOutlineView isItemExpanded:parent] ? 1 : 0;
	return [_results.children count] && 2 * expanded <= [_results.children count];
}

- (IBAction)takeLevelToFoldFrom:(id)sender
{
	if(self.resultsCollapsed)
			[_windowController.resultsOutlineView expandItem:nil expandChildren:YES];
	else	[_windowController.resultsOutlineView collapseItem:nil collapseChildren:YES];

	[_windowController.resultsOutlineView setNeedsDisplay:YES];
}

// ==================
// = Go to… Submenu =
// ==================

- (void)showResultNode:(FFResultNode*)aResultNode
{
	if(!aResultNode)
		return;

	NSOutlineView* outlineView = _windowController.resultsOutlineView;
	if(![outlineView isItemExpanded:aResultNode.parent])
		[outlineView expandItem:aResultNode.parent];
	[outlineView scrollRowToVisible:[outlineView rowForItem:aResultNode.parent]];

	NSInteger row = [outlineView rowForItem:aResultNode];
	if(row != -1)
	{
		[outlineView selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
		[outlineView scrollRowToVisible:row];
		[outlineView.window makeFirstResponder:outlineView];
	}
}

- (IBAction)takeSelectedPathFrom:(id)sender
{
	FFResultNode* item = [sender representedObject];
	if([item isKindOfClass:[FFResultNode class]])
		[self showResultNode:item.firstResultNode];
}

- (void)updateGoToMenu:(NSMenu*)aMenu
{
	if(self.countOfMatches == 0)
	{
		[[aMenu addItemWithTitle:@"No Results" action:@selector(nop:) keyEquivalent:@""] setEnabled:NO];
	}
	else
	{
		char key = 0;
		for(FFResultNode* parent in _results.children)
		{
			if(document::document_ptr doc = parent.document)
			{
				NSMenuItem* item = [aMenu addItemWithTitle:[NSString stringWithCxxString:doc->path() == NULL_STR ? doc->display_name() : path::relative_to(doc->path(), to_s(self.searchFolder))] action:@selector(takeSelectedPathFrom:) keyEquivalent:key < 10 ? [NSString stringWithFormat:@"%c", '0' + (++key % 10)] : @""];
				[item setImage:parent.icon];
				[item setRepresentedObject:parent];
			}
		}
	}
}

// =====================
// = Copy Find Results =
// =====================

- (void)copyEntireLines:(BOOL)entireLines withFilename:(BOOL)withFilename
{
	std::vector<std::string> res;

	NSOutlineView* outlineView = self.windowController.resultsOutlineView;
	NSIndexSet* selectedRows = [outlineView numberOfSelectedRows] == 0 ? [NSIndexSet indexSetWithIndexesInRange:NSMakeRange(0, [outlineView numberOfRows])] : [outlineView selectedRowIndexes];
	for(NSUInteger index = [selectedRows firstIndex]; index != NSNotFound; index = [selectedRows indexGreaterThanIndex:index])
	{
		FFResultNode* item = [outlineView itemAtRow:index];
		if([item.children count])
			continue;

		find::match_t const& m = [item.match match];
		std::string str = m.excerpt;

		if(!entireLines)
			str = str.substr(m.first - m.excerpt_offset, m.last - m.first);
		else if(str.size() && str.back() == '\n')
			str.erase(str.size()-1);

		if(withFilename)
			str = text::format("%s:%lu\t", [item.path UTF8String], m.line_number + 1) + str;

		res.push_back(str);
	}

	[[NSPasteboard generalPasteboard] declareTypes:@[ NSStringPboardType ] owner:nil];
	[[NSPasteboard generalPasteboard] setString:[NSString stringWithCxxString:text::join(res, "\n")] forType:NSStringPboardType];
}

- (void)copy:(id)sender                          { [self copyEntireLines:YES withFilename:NO ]; }
- (void)copyMatchingParts:(id)sender             { [self copyEntireLines:NO  withFilename:NO ]; }
- (void)copyMatchingPartsWithFilename:(id)sender { [self copyEntireLines:NO  withFilename:YES]; }
- (void)copyEntireLines:(id)sender               { [self copyEntireLines:YES withFilename:NO ]; }
- (void)copyEntireLinesWithFilename:(id)sender   { [self copyEntireLines:YES withFilename:YES]; }

// =====================
// = Check/Uncheck All =
// =====================

- (void)allMatchesSetExclude:(BOOL)exclude
{
	_results.excluded = exclude;
}

- (IBAction)checkAll:(id)sender
{
	[self allMatchesSetExclude:NO];
}

- (IBAction)uncheckAll:(id)sender
{
	[self allMatchesSetExclude:YES];
}

- (IBAction)selectNextResult:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];

	item = item ? (item.next ?: item.parent.next.firstResultNode) : _results.firstResultNode.firstResultNode;
	if(!item && _windowController.wrapAround)
		item = _results.firstResultNode.firstResultNode;

	[self showResultNode:item];
}

- (IBAction)selectPreviousResult:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];

	item = item ? (item.previous ?: item.parent.previous.lastResultNode) : _results.lastResultNode.lastResultNode;
	if(!item && _windowController.wrapAround)
		item = _results.lastResultNode.lastResultNode;

	[self showResultNode:item];
}

- (IBAction)selectNextTab:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];
	[self showResultNode:item.parent.next.firstResultNode ?: _results.firstResultNode.firstResultNode];
}

- (IBAction)selectPreviousTab:(id)sender
{
	NSInteger row = [_windowController.resultsOutlineView selectedRow];
	FFResultNode* item = row == -1 ? nil : [_windowController.resultsOutlineView itemAtRow:row];
	[self showResultNode:item.parent.previous.firstResultNode ?: _results.lastResultNode.firstResultNode];
}

- (BOOL)validateMenuItem:(NSMenuItem*)aMenuItem
{
	static std::set<SEL> const copyActions = { @selector(copy:), @selector(copyMatchingParts:), @selector(copyMatchingPartsWithFilename:), @selector(copyEntireLines:), @selector(copyEntireLinesWithFilename:) };
	if(copyActions.find(aMenuItem.action) != copyActions.end())
		return [self.windowController.resultsOutlineView numberOfRows] != 0;
	else if(aMenuItem.action == @selector(saveAllDocuments:))
		return self.hasUnsavedChanges;
	else if(aMenuItem.action == @selector(saveDocument:) || aMenuItem.action == @selector(saveDocumentAs:))
		return NO;
	else if(aMenuItem.action == @selector(takeLevelToFoldFrom:) && aMenuItem.tag == -1)
		[aMenuItem setTitle:self.resultsCollapsed ? @"Expand Results" : @"Collapse Results"];
	else if(aMenuItem.action == @selector(checkAll:))
		return self.countOfExcludedMatches > 0;
	else if(aMenuItem.action == @selector(uncheckAll:) )
		return self.countOfExcludedMatches < self.countOfMatches;
	return YES;
}
@end
