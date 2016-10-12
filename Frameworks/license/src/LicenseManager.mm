#import "LicenseManager.h"
#import "license.h"
#import <OakAppKit/OakUIConstructionFunctions.h>
#import <OakFoundation/OakFoundation.h>
#import <ns/ns.h>

@interface AddLicenseViewController : NSViewController
@property (nonatomic) NSTextField*        ownerLabel;
@property (nonatomic) NSTextField*        ownerTextField;
@property (nonatomic) NSTextField*        licenseLabel;
@property (nonatomic) NSTextField*        licenseTextField;
@property (nonatomic) NSTextField*        statusTextField;
@property (nonatomic) NSButton*           buyButton;
@property (nonatomic) NSButton*           cancelButton;
@property (nonatomic) NSButton*           registerButton;
@property (nonatomic) NSObjectController* objectController;

@property (nonatomic) NSString* ownerString;
@property (nonatomic) NSString* licenseString;
@property (nonatomic) NSString* statusString;
@property (nonatomic) BOOL canRegister;
@end

static NSTextField* OakCreateTextField ()
{
	NSTextField* res = [[NSTextField alloc] initWithFrame:NSZeroRect];
	res.font = OakControlFont();
	[[res cell] setWraps:YES];
	return res;
}

#ifndef CONSTRAINT
#define CONSTRAINT(str, align) [constraints addObjectsFromArray:[NSLayoutConstraint constraintsWithVisualFormat:str options:align metrics:nil views:views]]
#endif

@implementation AddLicenseViewController
- (void)loadView
{
	self.ownerLabel       = OakCreateLabel(@"Owner:", nil, NSRightTextAlignment);
	self.ownerTextField   = OakCreateTextField();
	self.licenseLabel     = OakCreateLabel(@"License:", nil, NSRightTextAlignment);
	self.licenseTextField = OakCreateTextField();
	self.licenseTextField.font = [NSFont userFixedPitchFontOfSize:12];
	self.statusTextField  = OakCreateLabel(@"", [NSFont labelFontOfSize:0]);
	self.buyButton        = OakCreateButton(@"Online Store");
	self.cancelButton     = OakCreateButton(@"Cancel");
	self.registerButton   = OakCreateButton(@"Add License");

	self.registerButton.keyEquivalent = @"\r";
	self.cancelButton.keyEquivalent = @"\033";

	NSString* const ownerPlaceholder = @"name";
	NSString* const licensePlaceholder = @""
		@"IFWGYIDDN5WXA33TNF2GKIDQNBSW433NMVXGCIDBOJSSA2LNOBSX-\n"
		@"E3LBNZSW45BOEAQCACSBNRWCAY3PNZ2GC3LJNZQXIZLEEB2GQ2LO-\n"
		@"M5ZSAYLSMUQHK3TTMF2GS43GMFRXI33SPEXAUQLMNQQHA2DFNZXW-\n"
		@"2ZLOMEQGC4TFEBSW24DUPEQGC3TEEBZWK3DGNRSXG4ZOEAQAU";

	self.objectController = [[NSObjectController alloc] initWithContent:self];
	[self.ownerTextField   bind:NSValueBinding   toObject:_objectController withKeyPath:@"content.ownerString"   options:@{ NSContinuouslyUpdatesValueBindingOption: @YES, NSNullPlaceholderBindingOption: ownerPlaceholder }];
	[self.licenseTextField bind:NSValueBinding   toObject:_objectController withKeyPath:@"content.licenseString" options:@{ NSContinuouslyUpdatesValueBindingOption: @YES, NSNullPlaceholderBindingOption: licensePlaceholder }];
	[self.statusTextField  bind:NSValueBinding   toObject:_objectController withKeyPath:@"content.statusString"  options:nil];
	[self.registerButton   bind:NSEnabledBinding toObject:_objectController withKeyPath:@"content.canRegister"   options:nil];

	self.registerButton.action = @selector(addLicense:);
	self.registerButton.target = self;
	self.cancelButton.action   = @selector(cancel:);
	self.buyButton.action      = @selector(visitOnlineStore:);
	self.buyButton.target      = self;

	OakSetupKeyViewLoop(@[ self.ownerTextField, self.licenseTextField, self.buyButton, self.cancelButton, self.registerButton ]);
	NSDictionary* views = @{
		@"ownerLabel"   : self.ownerLabel,
		@"owner"        : self.ownerTextField,
		@"licenseLabel" : self.licenseLabel,
		@"license"      : self.licenseTextField,
		@"status"       : self.statusTextField,
		@"buy"          : self.buyButton,
		@"cancel"       : self.cancelButton,
		@"register"     : self.registerButton,
	};

	self.view = [[NSView alloc] initWithFrame:NSMakeRect(0, 0, 100, 100)];
	OakAddAutoLayoutViewsToSuperview([views allValues], self.view);

	NSMutableArray* constraints = [NSMutableArray array];
	CONSTRAINT(@"H:|-[ownerLabel(==licenseLabel)]-[owner(==license)]-|", NSLayoutFormatAlignAllBaseline);
	CONSTRAINT(@"H:|-[licenseLabel]-[license(==400)]-|",                 0);
	CONSTRAINT(@"H:[status(==license)]-|",                               0);
	CONSTRAINT(@"H:|-[buy]-(>=12)-[cancel]-[register]-|",                NSLayoutFormatAlignAllTop);
	CONSTRAINT(@"V:|-[owner]-[license(==98)]-[status]-[register]-|",     0);
	[constraints addObject:[NSLayoutConstraint constraintWithItem:self.licenseLabel attribute:NSLayoutAttributeTop relatedBy:NSLayoutRelationEqual toItem:self.licenseTextField attribute:NSLayoutAttributeTop multiplier:1 constant:3]];
	[self.view addConstraints:constraints];

	self.ownerString = NSFullUserName();
}

- (void)viewWillAppear
{
	self.objectController.content = self;
}

- (void)viewDidDisappear
{
	self.objectController.content = nil;
}

- (NSString*)trimmedOwnerString
{
	return [self.ownerString stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
}

- (void)validateOwnerAndLicense
{
	bool hasContent   = OakNotEmptyString(self.trimmedOwnerString) && OakNotEmptyString(self.licenseString);
	bool validLicense = hasContent && license::is_valid(license::decode(to_s(self.licenseString)), to_s(self.trimmedOwnerString));

	self.canRegister  = validLicense;
	self.statusString = validLicense || !hasContent ? nil : to_ns(license::error_description(to_s(self.licenseString), to_s(self.trimmedOwnerString)));

	if(validLicense)
	{
		auto const license = license::decode(to_s(self.licenseString));
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			if(license::is_revoked(license))
			{
				dispatch_async(dispatch_get_main_queue(), ^{
					self.canRegister  = NO;
					self.statusString = @"This license has been revoked.";
				});
			}
		});
	}
}

- (void)setOwnerString:(NSString*)aString
{
	if(_ownerString != aString && ![_ownerString isEqualToString:aString])
	{
		_ownerString = aString;
		[self validateOwnerAndLicense];
	}
}

- (void)setLicenseString:(NSString*)aString
{
	if(_licenseString != aString && ![_licenseString isEqualToString:aString])
	{
		_licenseString = aString;
		[self validateOwnerAndLicense];
	}
}

- (void)visitOnlineStore:(id)sender
{
	[[NSWorkspace sharedWorkspace] openURL:[NSURL URLWithString:@"https://shop.macromates.com"]];
}

- (void)addLicense:(id)sender
{
	[self.view.window performClose:self];
	if(self.canRegister)
	{
		auto const license = license::decode(to_s(self.licenseString));
		dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
			bool revoked = license::is_revoked(license);
			dispatch_async(dispatch_get_main_queue(), ^{
				std::string error = "Unknown error.";
				if(revoked)
					NSRunAlertPanel(@"License Has Been Revoked", @"The license provided is no longer valid.\n\nThe most likely reason for revocation is that a chargeback was issued for your credit card transaction.", @"Continue", nil, nil);
				else if(license::add(to_s(self.trimmedOwnerString), to_s(self.licenseString), &error))
					NSRunAlertPanel(@"License Added to Keychain", @"Thanks for your support!", @"Continue", nil, nil);
				else
					NSRunAlertPanel(@"Failure Adding License to Keychain", @"%@", @"Continue", nil, nil, to_ns(error));
			});
		});
	}
}
@end

// ======================
// = Add License Window =
// ======================

@interface AddLicenseWindowController : NSWindowController <NSWindowDelegate, NSPopoverDelegate>
@property (nonatomic) AddLicenseViewController* viewController;
@end

@implementation AddLicenseWindowController
- (instancetype)init
{
	if((self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSZeroRect styleMask:(NSTitledWindowMask|NSClosableWindowMask|NSMiniaturizableWindowMask) backing:NSBackingStoreBuffered defer:NO]]))
	{
		self.window.title    = @"Add License";
		self.window.level    = NSModalPanelWindowLevel;
		self.window.delegate = self;

		self.viewController = [[AddLicenseViewController alloc] init];
		self.window.contentView = self.viewController.view;
	}
	return self;
}

- (IBAction)showWindow:(id)sender
{
	if(![self.window isVisible])
	{
		[self.window layoutIfNeeded];
		[self.window center];
	}
	[self.window makeKeyAndOrderFront:self];
}
@end

// ===================
// = License Manager =
// ===================

@implementation LicenseManager
+ (instancetype)sharedInstance
{
	static LicenseManager* sharedInstance = [self new];
	return sharedInstance;
}

- (void)showAddLicenseWindow:(id)sender
{
	static AddLicenseWindowController* windowController = [[AddLicenseWindowController alloc] init];
	[windowController showWindow:self];
}
@end
