#import "FilesPreferences.h"
#import "Keys.h"
#import <OakAppKit/NSMenu Additions.h>
#import <OakAppKit/OakEncodingPopUpButton.h>
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/NSString Additions.h>
#import <OakFoundation/OakStringListTransformer.h>
#import <MenuBuilder/MenuBuilder.h>
#import <settings/settings.h>
#import <bundles/bundles.h>
#import <ns/ns.h>
#import <text/ctype.h>
#import <oak/oak.h>

@implementation FilesPreferences
- (id)init
{
	if(self = [super initWithNibName:nil label:@"Files" image:[NSImage imageNamed:NSImageNameMultipleDocuments]])
	{
		[OakStringListTransformer createTransformerWithName:@"OakLineEndingsSettingsTransformer" andObjectsArray:@[ @"\\n", @"\\r", @"\\r\\n" ]];

		self.defaultsProperties = @{
			@"disableSessionRestore":         kUserDefaultsDisableSessionRestoreKey,
			@"disableDocumentAtStartup":      kUserDefaultsDisableNewDocumentAtStartupKey,
			@"disableDocumentAtReactivation": kUserDefaultsDisableNewDocumentAtReactivationKey,
		};

		self.tmProperties = @{
			@"encoding":    [NSString stringWithCxxString:kSettingsEncodingKey],
			@"lineEndings": [NSString stringWithCxxString:kSettingsLineEndingsKey],
		};
	}
	return self;
}

- (void)selectNewFileType:(NSMenuItem*)sender
{
	settings_t::set(kSettingsFileTypeKey, to_s([sender representedObject]), "attr.untitled");
}

- (void)selectUnknownFileType:(NSMenuItem*)sender
{
	settings_t::set(kSettingsFileTypeKey, to_s([sender representedObject]), "attr.file.unknown-type");
}

- (void)loadView
{
	NSButton* restoreDocumentsCheckBox       = OakCreateCheckBox(@"Open documents from last session");
	NSButton* createAtStartupCheckBox        = OakCreateCheckBox(@"Create one at startup");
	NSButton* createOnActivationCheckBox     = OakCreateCheckBox(@"Create one when re-activated");
	NSPopUpButton* newDocumentTypesPopUp     = OakCreatePopUpButton();
	NSPopUpButton* unknownDocumentTypesPopUp = OakCreatePopUpButton();
	OakEncodingPopUpButton* encodingPopUp    = [[OakEncodingPopUpButton alloc] init];
	NSPopUpButton* lineEndingsPopUp          = OakCreatePopUpButton();

	MBMenu const items = {
		{ @"LF (recommended)", .tag = 0 },
		{ @"CR (Mac Classic)", .tag = 1 },
		{ @"CRLF (Windows)",   .tag = 2 },
	};
	MBCreateMenu(items, lineEndingsPopUp.menu);

	NSFont* smallFont = [NSFont messageFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeSmall]];
	NSGridView* gridView = [NSGridView gridViewWithViews:@[
		@[ OakCreateLabel(@"At startup:"),             restoreDocumentsCheckBox   ],
		@[ NSGridCell.emptyContentView,                OakCreateLabel(@"Hold shift (⇧) to bypass", smallFont) ],
		@[ OakCreateLabel(@"With no open documents:"), createAtStartupCheckBox    ],
		@[ NSGridCell.emptyContentView,                createOnActivationCheckBox ],

		@[ ],

		@[ OakCreateLabel(@"New document type:"),      newDocumentTypesPopUp     ],
		@[ OakCreateLabel(@"Unknown document type:"),  unknownDocumentTypesPopUp ],
		@[ OakCreateLabel(@"Encoding:"),               encodingPopUp             ],
		@[ OakCreateLabel(@"Line endings:"),           lineEndingsPopUp          ],
	]];

	NSView* label = [gridView cellAtColumnIndex:1 rowIndex:0].contentView;
	NSGridCell* sublabel = [gridView cellAtColumnIndex:1 rowIndex:1];
	sublabel.xPlacement = NSGridCellPlacementNone;
	sublabel.customPlacementConstraints = @[ [sublabel.contentView.leadingAnchor constraintEqualToAnchor:label.leadingAnchor constant:19] ];

	for(NSView* popUpButton in @[ unknownDocumentTypesPopUp, encodingPopUp, lineEndingsPopUp ])
		[popUpButton.widthAnchor constraintEqualToAnchor:newDocumentTypesPopUp.widthAnchor].active = YES;

	self.view = OakSetupGridViewWithSeparators(gridView, { 4 });

	[restoreDocumentsCheckBox   bind:NSValueBinding       toObject:self withKeyPath:@"disableSessionRestore"         options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[createAtStartupCheckBox    bind:NSValueBinding       toObject:self withKeyPath:@"disableDocumentAtStartup"      options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[createOnActivationCheckBox bind:NSValueBinding       toObject:self withKeyPath:@"disableDocumentAtReactivation" options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[encodingPopUp              bind:@"encoding"          toObject:self withKeyPath:@"encoding"                      options:nil];
	[lineEndingsPopUp           bind:NSSelectedTagBinding toObject:self withKeyPath:@"lineEndings"                   options:@{ NSValueTransformerNameBindingOption: @"OakLineEndingsSettingsTransformer" }];

	// ================================
	// = Create Language Pop-up Menus =
	// ================================

	NSMenu* newDocumentTypesMenu     = newDocumentTypesPopUp.menu;
	NSMenu* unknownDocumentTypesMenu = unknownDocumentTypesPopUp.menu;

	[newDocumentTypesMenu removeAllItems];
	[unknownDocumentTypesMenu removeAllItems];

	NSMenuItem* item = [unknownDocumentTypesMenu addItemWithTitle:@"Prompt for type" action:@selector(selectUnknownFileType:) keyEquivalent:@""];
	[item setRepresentedObject:nil];
	[item setTarget:self];
	[unknownDocumentTypesMenu addItem:[NSMenuItem separatorItem]];

	std::multimap<std::string, bundles::item_ptr, text::less_t> grammars;
	for(auto const& item : bundles::query(bundles::kFieldAny, NULL_STR, scope::wildcard, bundles::kItemTypeGrammar))
	{
		if(!item->hidden_from_user())
			grammars.emplace(item->name(), item);
	}

	if(!grammars.empty())
	{
		std::string const defaultNewFileType     = settings_t::raw_get(kSettingsFileTypeKey, "attr.untitled");
		std::string const defaultUnknownFileType = settings_t::raw_get(kSettingsFileTypeKey, "attr.file.unknown-type");

		for(auto const& pair : grammars)
		{
			std::string const& fileType = pair.second->value_for_field(bundles::kFieldGrammarScope);
			if(fileType == NULL_STR)
				continue;

			NSMenuItem* item = [newDocumentTypesMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(selectNewFileType:) keyEquivalent:@""];
			[item setRepresentedObject:[NSString stringWithCxxString:fileType]];
			[item setTarget:self];

			if(fileType == defaultNewFileType)
				[newDocumentTypesPopUp selectItem:item];

			item = [unknownDocumentTypesMenu addItemWithTitle:[NSString stringWithCxxString:pair.first] action:@selector(selectUnknownFileType:) keyEquivalent:@""];
			[item setRepresentedObject:[NSString stringWithCxxString:fileType]];
			[item setTarget:self];

			if(fileType == defaultUnknownFileType)
				[unknownDocumentTypesPopUp selectItem:item];
		}
	}
}
@end
