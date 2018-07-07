#import "ViewControllers.h"
#import "AddAutoLayoutViews.h"
#import "GenieTableViewController.h"
#import <GenieManager/GenieManager.h>
#import <GenieManager/GenieItem.h>
#import <OakFoundation/OakFoundation.h>
#import <OakFoundation/OakStringListTransformer.h>

@interface TreeViewController ()
{
	BOOL _didLoadView;
}
@property (nonatomic) NSTreeController* treeController;
@end

@implementation TreeViewController
- (instancetype)initWithTreeController:(NSTreeController*)aTreeController
{
	if(self = [super init])
		_treeController = aTreeController;
	return self;
}

- (void)loadView
{
	if(!_didLoadView)
	{
		NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];
		contentView.autoresizingMask = NSViewWidthSizable|NSViewHeightSizable;
		if([self respondsToSelector:@selector(makeView:)])
			[self performSelector:@selector(makeView:) withObject:contentView];
		self.view = contentView;
	}
	_didLoadView = YES;
}
@end

@interface ChangeIconViewController : NSViewController
{
	NSPopUpButton* _popUpButton;
	NSTextField* _textField;
	NSDictionary* _currentItem;
}
@property (nonatomic) NSObjectController* objectController;
@property (nonatomic) SEL action;
@property (nonatomic, weak) id target;
@end

@implementation ChangeIconViewController
- (instancetype)initWithContent:(NSDictionary*)iconInfo
{
	if(self = [super init])
		_objectController = [[NSObjectController alloc] initWithContent:[(iconInfo ?: @{ }) mutableCopy]];
	return self;
}

- (void)popUpButtonDidChange:(NSMenuItem*)sender
{
	[_objectController commitEditing];

	if(_currentItem)
		[_textField unbind:NSValueBinding];

	if(_currentItem = sender.representedObject)
	{
		_textField.placeholderString = _currentItem[@"placeholder"];
		[_textField bind:NSValueBinding toObject:_objectController withKeyPath:[NSString stringWithFormat:@"content.%@", _currentItem[@"property"]] options:@{ NSNullPlaceholderBindingOption: _currentItem[@"placeholder"] }];
	}
}

- (void)loadView
{
	NSArray* popUpItems = @[
		@{ @"title": @"Image Path",     @"placeholder": @"Image file path",           @"property": @"image"       },
		@{ @"title": @"File Type",      @"placeholder": @"Universal type identifier", @"property": @"fileType"    },
		@{ @"title": @"Application",    @"placeholder": @"Bundle identifier",         @"property": @"application" },
		@{ @"title": @"Text",           @"placeholder": @"String",                    @"property": @"text"        },
		@{ @"title": @"Image Named",    @"placeholder": @"Named system image",        @"property": @"name"        },
		@{ @"title": @"Icon from File", @"placeholder": @"File path",                 @"property": @"file"        },
		@{ @"title": @"Favorite Icon",  @"placeholder": @"URL",                       @"property": @"url"         },
	];

	NSPopUpButton* popUpButton = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	for(NSDictionary* item in popUpItems)
		[popUpButton.menu addItemWithTitle:item[@"title"] action:@selector(popUpButtonDidChange:) keyEquivalent:@""].representedObject = item;
	NSTextField* textField = [NSTextField textFieldWithString:@""];

	_popUpButton = popUpButton;
	_textField   = textField;

	NSMenuItem* selectItem = _popUpButton.itemArray.firstObject;
	for(NSMenuItem* menuItem in _popUpButton.itemArray)
	{
		if(id value = [_objectController valueForKeyPath:[NSString stringWithFormat:@"content.%@", menuItem.representedObject[@"property"]]])
		{
			if([value isKindOfClass:[NSString class]] && OakNotEmptyString(value))
			{
				selectItem = menuItem;
				break;
			}
		}
	}

	[_popUpButton selectItem:selectItem];
	[self popUpButtonDidChange:selectItem];

	NSButton* acceptButton = [NSButton buttonWithTitle:@"OK" target:self action:@selector(acceptChangedIcon:)];
	NSButton* cancelButton = [NSButton buttonWithTitle:@"Cancel" target:self action:@selector(cancelChangedIcon:)];

	[acceptButton setKeyEquivalent:@"\r"];
	[cancelButton setKeyEquivalent:@"\e"];

	NSView* contentView = [[NSView alloc] initWithFrame:NSZeroRect];

	NSDictionary* views = @{
		@"popUp":         popUpButton,
		@"textField":     textField,
		@"ok":            acceptButton,
		@"cancel":        cancelButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ textField, cancelButton, acceptButton, popUpButton ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[popUp]-[textField(>=200)]-|"    options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(>=8)-[cancel]-[ok(==cancel)]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[textField]-(20)-[ok]-|"         options:0 metrics:nil views:views]];

	self.view = contentView;
}

- (void)viewWillAppear
{
	self.view.window.initialFirstResponder = _textField;
	[super viewDidAppear];
}

- (void)acceptChangedIcon:(id)sender
{
	[self.objectController commitEditing];
	[self.presentingViewController dismissViewController:self];

	NSDictionary* dict = @{ };
	if(NSString* key = _currentItem[@"property"])
	{
		NSString* value = [_objectController.content valueForKey:key];
		if(OakNotEmptyString(value))
			dict = @{ key: value };
	}
	[_objectController setContent:[dict mutableCopy]];

	[NSApp sendAction:self.action to:self.target from:self];
}

- (void)cancelChangedIcon:(id)sender
{
	[self.presentingViewController dismissViewController:self];
}
@end

@implementation BasicProperties
{
	NSTextField* _matchTextField;
	ChangeIconViewController* _changeIconViewController;
}

- (void)changeImage:(id)sender
{
	if(GenieItem* selectedItem = self.treeController.selectedObjects.firstObject)
	{
		_changeIconViewController = [[ChangeIconViewController alloc] initWithContent:selectedItem.iconDescription];
		_changeIconViewController.action = @selector(acceptChangedIcon:);
		_changeIconViewController.target = self;
		[self presentViewControllerAsSheet:_changeIconViewController];
	}
}

- (void)acceptChangedIcon:(id)sender
{
	if(GenieItem* selectedItem = self.treeController.selectedObjects.firstObject)
	{
		NSDictionary* updatedIconInfo = _changeIconViewController.objectController.content;
		selectedItem.iconDescription = updatedIconInfo.count ? updatedIconInfo : nil;
	}
}

- (void)realMakeView:(NSView*)contentView
{
	NSTextField* titleLabel    = [NSTextField labelWithString:@"Title:"];
	NSTextField* subtitleLabel = [NSTextField labelWithString:@"Subtitle:"];
	NSTextField* matchLabel    = [NSTextField labelWithString:@"Match:"];

	NSTextField* titleTextField    = [NSTextField textFieldWithString:@""];
	NSTextField* subtitleTextField = [NSTextField textFieldWithString:@""];
	NSTextField* matchTextField    = [NSTextField textFieldWithString:@""];
	_matchTextField = matchTextField;

	NSImageView* imageView = [NSImageView imageViewWithImage:[NSImage imageNamed:NSImageNameApplicationIcon]];
	imageView.imageFrameStyle = NSImageFrameGrayBezel;

	_changeImageButton = [NSButton buttonWithTitle:@"Change…" target:self action:@selector(changeImage:)];

	[imageView setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow forOrientation:NSLayoutConstraintOrientationVertical];
	[imageView setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationVertical];

	[_changeImageButton setContentHuggingPriority:NSLayoutPriorityDefaultHigh forOrientation:NSLayoutConstraintOrientationHorizontal];
	[_changeImageButton setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

	NSDictionary* views = @{
		@"titleLabel":      titleLabel,
		@"subtitleLabel":   subtitleLabel,
		@"matchLabel":      matchLabel,
		@"title":           titleTextField,
		@"subtitle":        subtitleTextField,
		@"match":           matchTextField,
		@"imageView":       imageView,
		@"changeImage":     _changeImageButton,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, titleTextField, subtitleTextField, matchTextField, _changeImageButton ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]"                              options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[subtitleLabel]-[subtitle]"                        options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[matchLabel]-[match]"                              options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[title]-[imageView]-|"                               options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[subtitle]-[imageView]-|"                            options:NSLayoutFormatAlignAllBottom metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[match]-[changeImage]-|"                             options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[subtitle]-[match]"                        options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	NSLayoutConstraint* imageViewWidth = [NSLayoutConstraint constraintWithItem:imageView attribute:NSLayoutAttributeWidth relatedBy:NSLayoutRelationGreaterThanOrEqual toItem:imageView attribute:NSLayoutAttributeHeight multiplier:1 constant:0];
	[contentView addConstraint:imageViewWidth];

	[titleTextField    bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle"    options:nil];
	[subtitleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveSubtitle" options:nil];
	[matchTextField    bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveMatch"    options:@{ NSNullPlaceholderBindingOption: @"Optional" }];
	[imageView         bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.iconImage"         options:nil];
}

- (void)makeView:(NSView*)contentView
{
	[self realMakeView:contentView];
}

- (void)makeView:(NSView*)contentView aboveView:(NSView*)aView
{
	[self realMakeView:contentView];

	NSDictionary* views = @{
		@"match":    _matchTextField,
		@"nextView": aView,
	};

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[match]-[nextView]" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];
}
@end

@implementation URLProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* urlLabel     = [NSTextField labelWithString:@"URL:"];
	NSTextField* urlTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"urlLabel":  urlLabel,
		@"url":       urlTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, urlTextField ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[urlLabel]-[url]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[url]-(>=20)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[urlTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveUrl" options:nil];

	[super makeView:contentView aboveView:urlTextField];
}
@end

@implementation FileProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* fileLabel     = [NSTextField labelWithString:@"File:"];
	NSTextField* fileTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"fileLabel":  fileLabel,
		@"file":       fileTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, fileTextField ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[fileLabel]-[file]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[file]-(>=20)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[fileTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveFile" options:nil];

	[super makeView:contentView aboveView:fileTextField];
}
@end

@implementation ShellProperties
{
	TextViewController* _textViewController;
	GenieTableViewController* _argvTable;
}

- (void)makeView:(NSView*)contentView
{
	_textViewController = [TextViewController textViewController];
	_argvTable = [[GenieTableViewController alloc] initWithColumnNames:@[ @"value" ] visibleRows:3 showHeaderView:NO prototype:@{ @"value": @"argument" }];
	[_argvTable.arrayController bind:NSContentArrayBinding toObject:self.treeController withKeyPath:@"selection.mutableScriptArguments" options:nil];

	NSTextField* scriptLabel = [NSTextField labelWithString:@"Script:"];
	NSTextField* argvLabel   = [NSTextField labelWithString:@"Arguments:"];

	NSView* scriptScrollView   = _textViewController.view;
	NSTextView* scriptTextView = _textViewController.textView;

	NSDictionary* views = @{
		@"scriptLabel": scriptLabel,
		@"argvLabel":   argvLabel,
		@"script":      scriptScrollView,
		@"argv":        _argvTable.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, scriptTextView, _argvTable.view ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scriptLabel]-[script]-|"              options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[argvLabel]-[argv]-|"                  options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:[script]-[argv]-|"                       options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[scriptTextView bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.script" options:nil];

	[super makeView:contentView aboveView:scriptScrollView];
}
@end

@implementation SpotlightProperties
{
	GenieTableViewController* scope;
}

- (void)makeView:(NSView*)contentView
{
	static NSArray<NSString*>* const metaDataFields = @[
		NSMetadataItemCFBundleIdentifierKey, NSMetadataItemContentCreationDateKey, NSMetadataItemContentModificationDateKey, NSMetadataItemContentTypeKey, NSMetadataItemDateAddedKey, NSMetadataItemDisplayNameKey, NSMetadataItemFSContentChangeDateKey, NSMetadataItemFSNameKey, NSMetadataItemKindKey, NSMetadataItemLastUsedDateKey, NSMetadataItemPathKey, NSMetadataItemURLKey, NSMetadataItemVersionKey
	];

	NSTextField* titleLabel     = [NSTextField labelWithString:@"Title:"];
	NSTextField* queryLabel     = [NSTextField labelWithString:@"Query:"];
	NSTextField* sortByLabel    = [NSTextField labelWithString:@"Sort By:"];
	NSTextField* scopeLabel     = [NSTextField labelWithString:@"Folders:"];

	NSTextField* titleTextField = [NSTextField textFieldWithString:@""];
	NSTextField* queryTextField = [NSTextField textFieldWithString:@""];

	NSPopUpButton* sortByPopUp = [[NSPopUpButton alloc] initWithFrame:NSZeroRect pullsDown:NO];
	for(NSString* field in metaDataFields)
		[sortByPopUp.menu addItemWithTitle:field action:NULL keyEquivalent:@""];

	NSButton* ascendingCheckBox = [NSButton checkboxWithTitle:@"Ascending" target:nil action:NULL];

	scope = [[GenieTableViewController alloc] initWithColumnNames:@[ @"disabled", @"path" ] visibleRows:3 showHeaderView:NO prototype:@{ @"path": @"/some/path" }];
	[scope.arrayController bind:NSContentArrayBinding toObject:self.treeController withKeyPath:@"selection.mdScope" options:nil];

	NSDictionary* views = @{
		@"titleLabel":   titleLabel,
		@"queryLabel":   queryLabel,
		@"sortByLabel":  sortByLabel,
		@"scopeLabel":   scopeLabel,
		@"title":        titleTextField,
		@"query":        queryTextField,
		@"sortBy":       sortByPopUp,
		@"ascending":    ascendingCheckBox,
		@"scope":        scope.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, titleTextField, queryTextField, sortByPopUp, ascendingCheckBox, scope.view ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"            options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[queryLabel]-[query]-|"            options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[sortByLabel]-[sortBy]-(>=20)-|"   options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:[ascending]-(>=20)-|"                options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scopeLabel]-[scope]-|"            options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[query]-(20)-[sortBy]-[ascending]-(20)-[scope]-(>=20)-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[queryTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.mdQuery" options:nil];
	[sortByPopUp bind:NSSelectedValueBinding toObject:self.treeController withKeyPath:@"selection.sortBy" options:@{ NSNullPlaceholderBindingOption: NSMetadataItemLastUsedDateKey }];
	[ascendingCheckBox bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.descending" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
}
@end

@interface SQLiteProperties ()
{
	TextViewController* _textViewController;
	GenieTableViewController* _bindingsTable;
}
@end

@implementation SQLiteProperties
- (void)makeView:(NSView*)contentView
{
	_textViewController = [TextViewController textViewController];
	_bindingsTable = [[GenieTableViewController alloc] initWithColumnNames:@[ @"value" ] visibleRows:3 showHeaderView:NO prototype:@{ @"value": @"binding" }];
	[_bindingsTable.arrayController bind:NSContentArrayBinding toObject:self.treeController withKeyPath:@"selection.mutableSqlBindings" options:nil];

	NSTextField* titleLabel        = [NSTextField labelWithString:@"Title:"];
	NSTextField* databaseLabel     = [NSTextField labelWithString:@"Database:"];
	NSTextField* queryLabel        = [NSTextField labelWithString:@"Query:"];
	NSTextField* bindingsLabel     = [NSTextField labelWithString:@"Bindings:"];
	NSTextField* titleTextField    = [NSTextField textFieldWithString:@""];
	NSTextField* databaseTextField = [NSTextField textFieldWithString:@""];
	NSView* queryScrollView        = _textViewController.view;
	NSTextView* queryTextView      = _textViewController.textView;

	NSDictionary* views = @{
		@"titleLabel":     titleLabel,
		@"databaseLabel":  databaseLabel,
		@"queryLabel":     queryLabel,
		@"bindingsLabel":  bindingsLabel,
		@"title":          titleTextField,
		@"database":       databaseTextField,
		@"query":          queryScrollView,
		@"bindings":       _bindingsTable.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, titleTextField, databaseTextField, queryTextView ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"                         options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[databaseLabel]-[database]-|"                   options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[queryLabel]-[query]-|"                         options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[bindingsLabel]-[bindings]-|"                   options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[database]-[query(>=100)]-[bindings]-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[databaseTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveSqlDatabase" options:nil];
	[queryTextView bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.sqlQuery" options:nil];
}
@end

@implementation RecentDocumentsProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* titleLabel                = [NSTextField labelWithString:@"Title:"];
	NSTextField* bundleIdentifierLabel     = [NSTextField labelWithString:@"Bundle Identifier:"];

	NSTextField* titleTextField            = [NSTextField textFieldWithString:@""];
	NSTextField* bundleIdentifierTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"titleLabel":             titleLabel,
		@"bundleIdentifierLabel":  bundleIdentifierLabel,
		@"title":                  titleTextField,
		@"bundleIdentifier":       bundleIdentifierTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, titleTextField, bundleIdentifierTextField ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"                       options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[bundleIdentifierLabel]-[bundleIdentifier]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[bundleIdentifier]-(>=20)-|" options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[bundleIdentifierTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.primitiveBundleIdentifier" options:nil];
}
@end

@implementation PredicateGroupProperties
- (void)makeView:(NSView*)contentView
{
	NSTextField* titleLabel         = [NSTextField labelWithString:@"Title:"];
	NSTextField* predicateLabel     = [NSTextField labelWithString:@"Predicate:"];

	NSTextField* titleTextField     = [NSTextField textFieldWithString:@""];
	NSTextField* predicateTextField = [NSTextField textFieldWithString:@""];

	NSDictionary* views = @{
		@"titleLabel":      titleLabel,
		@"predicateLabel":  predicateLabel,
		@"title":           titleTextField,
		@"predicate":       predicateTextField,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, titleTextField, predicateTextField ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"         options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[predicateLabel]-[predicate]-|" options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[predicate]-(>=20)-|"   options:NSLayoutFormatAlignAllLeading|NSLayoutFormatAlignAllTrailing metrics:nil views:views]];

	[titleTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.title" options:nil];
	[predicateTextField bind:NSValueBinding toObject:self.treeController withKeyPath:@"selection.predicate" options:nil];
}
@end

@implementation ExecDataSourceProperties
{
	TextViewController* _textViewController;
	GenieTableViewController* _argvTable;
}

- (void)makeView:(NSView*)contentView
{
	_textViewController = [TextViewController textViewController];
	_argvTable = [[GenieTableViewController alloc] initWithColumnNames:@[ @"value" ] visibleRows:3 showHeaderView:NO prototype:@{ @"value": @"argument" }];

	NSTextField* titleLabel  = [NSTextField labelWithString:@"Title:"];
	NSTextField* scriptLabel = [NSTextField labelWithString:@"Script:"];
	NSTextField* argvLabel   = [NSTextField labelWithString:@"Arguments:"];

	NSTextField* titleTextField = [NSTextField textFieldWithString:@""];
	NSView* scriptScrollView    = _textViewController.view;
	NSTextView* scriptTextView  = _textViewController.textView;

	NSDictionary* views = @{
		@"titleLabel":   titleLabel,
		@"scriptLabel":  scriptLabel,
		@"argvLabel":    argvLabel,
		@"title":        titleTextField,
		@"script":       scriptScrollView,
		@"argv":         _argvTable.view,
	};

	GenieAddAutoLayoutViewsToSuperview(views, contentView);
	GenieSetupKeyViewLoop(@[ contentView, titleTextField, scriptTextView, _argvTable.view ]);

	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[titleLabel]-[title]-|"                options:NSLayoutFormatAlignAllBaseline metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[scriptLabel]-[script]-|"              options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-[argvLabel]-[argv]-|"                  options:NSLayoutFormatAlignAllTop metrics:nil views:views]];
	[contentView addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"V:|-[title]-[script]-[argv]-|" options:NSLayoutFormatAlignAllLeading metrics:nil views:views]];

	[titleTextField             bind:NSValueBinding        toObject:self.treeController withKeyPath:@"selection.primitiveTitle" options:nil];
	[scriptTextView             bind:NSValueBinding        toObject:self.treeController withKeyPath:@"selection.script" options:nil];
	[_argvTable.arrayController bind:NSContentArrayBinding toObject:self.treeController withKeyPath:@"selection.mutableScriptArguments" options:nil];
}
@end
