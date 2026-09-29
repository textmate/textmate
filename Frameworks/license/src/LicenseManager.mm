#import "LicenseManager.h"
#import "license.h"
#import <OakAppKit/src/OakUIConstructionFunctions.h>
#import <OakFoundation/src/OakFoundation.h>
#import <ns/src/ns.h>

@interface License : NSObject
@property (nonatomic) NSString* owner;
@property (nonatomic) NSString* licenseAsBase32;
@property (nonatomic) NSString* status;
@property (nonatomic, getter = isValid) BOOL valid;
@end

@interface LicenseManager ()
{
	id _owner;
}
- (BOOL)addLicense:(License*)license;
@end

@implementation License
- (void)validateOwnerAndLicense
{
	BOOL valid = NO;
	NSString* status = nil;

	NSString* owner = [_owner stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
	if(OakNotEmptyString(owner) && OakNotEmptyString(_licenseAsBase32))
	{
		auto const decoded = license::decode(to_s(_licenseAsBase32));
		if(!license::is_valid(decoded, to_s(owner)))
			status = to_ns(license::error_description(to_s(_licenseAsBase32), to_s(owner)));
		else if(license::is_revoked(decoded))
			status = @"This license has been revoked.";
		else
			valid = YES;
	}

	self.valid  = valid;
	self.status = status;
}

- (void)setOwner:(NSString*)newOwner
{
	if(_owner != newOwner && ![_owner isEqualToString:newOwner])
	{
		_owner = newOwner;
		[self validateOwnerAndLicense];
	}
}

- (void)setLicenseAsBase32:(NSString*)newLicense
{
	if(_licenseAsBase32 != newLicense && ![_licenseAsBase32 isEqualToString:newLicense])
	{
		_licenseAsBase32 = newLicense;
		[self validateOwnerAndLicense];
	}
}
@end

// ====================
// = Add License View =
// ====================

@interface AddLicenseViewController : NSViewController
@property (nonatomic) NSTextField* ownerLabel;
@property (nonatomic) NSTextField* ownerTextField;
@property (nonatomic) NSTextField* licenseLabel;
@property (nonatomic) NSTextField* licenseTextField;
@property (nonatomic) NSTextField* statusTextField;
@property (nonatomic) NSButton*    buyButton;
@property (nonatomic) NSButton*    cancelButton;
@property (nonatomic) NSButton*    registerButton;
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
	self.ownerLabel       = OakCreateLabel(@"Owner:", nil, NSTextAlignmentRight);
	self.ownerTextField   = OakCreateTextField();
	self.licenseLabel     = OakCreateLabel(@"License:", nil, NSTextAlignmentRight);
	self.licenseTextField = OakCreateTextField();
	self.licenseTextField.font = [NSFont userFixedPitchFontOfSize:12];
	self.statusTextField  = OakCreateLabel(@"", [NSFont labelFontOfSize:0]);
	self.buyButton        = OakCreateButton(@"Online Store");
	self.cancelButton     = OakCreateButton(@"Cancel");
	self.registerButton   = OakCreateButton(@"Add License");

	self.registerButton.keyEquivalent = @"\r";
	self.cancelButton.keyEquivalent = @"\033";

	NSString* const ownerPlaceholder   = @"name";
	NSString* const licensePlaceholder = @"license key (4 lines)";

	[self.ownerTextField   bind:NSValueBinding   toObject:self withKeyPath:@"representedObject.owner"           options:@{ NSContinuouslyUpdatesValueBindingOption: @YES, NSNullPlaceholderBindingOption: ownerPlaceholder }];
	[self.licenseTextField bind:NSValueBinding   toObject:self withKeyPath:@"representedObject.licenseAsBase32" options:@{ NSContinuouslyUpdatesValueBindingOption: @YES, NSNullPlaceholderBindingOption: licensePlaceholder }];
	[self.statusTextField  bind:NSValueBinding   toObject:self withKeyPath:@"representedObject.status"          options:nil];
	[self.registerButton   bind:NSEnabledBinding toObject:self withKeyPath:@"representedObject.valid"           options:nil];

	self.registerButton.action = @selector(addLicense:);
	self.registerButton.target = self;
	self.cancelButton.action   = @selector(cancel:);
	self.buyButton.action      = @selector(visitOnlineStore:);
	self.buyButton.target      = self;

	NSDictionary* views = @{
		@"ownerLabel":   self.ownerLabel,
		@"owner":        self.ownerTextField,
		@"licenseLabel": self.licenseLabel,
		@"license":      self.licenseTextField,
		@"status":       self.statusTextField,
		@"buy":          self.buyButton,
		@"cancel":       self.cancelButton,
		@"register":     self.registerButton,
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
}

- (void)visitOnlineStore:(id)sender
{
	[NSWorkspace.sharedWorkspace openURL:[NSURL URLWithString:@"https://shop.macromates.com"]];
}

- (void)addLicense:(id)sender
{
	if([LicenseManager.sharedInstance addLicense:self.representedObject])
		[self.view.window performClose:self];
}
@end

// ======================
// = Add License Window =
// ======================

@interface AddLicenseWindowController : NSWindowController <NSWindowDelegate, NSPopoverDelegate>
@property (nonatomic) AddLicenseViewController* viewController;
@end

@implementation AddLicenseWindowController
- (instancetype)initWithLicense:(License*)license
{
	if((self = [super initWithWindow:[[NSPanel alloc] initWithContentRect:NSZeroRect styleMask:(NSWindowStyleMaskTitled|NSWindowStyleMaskClosable|NSWindowStyleMaskMiniaturizable) backing:NSBackingStoreBuffered defer:NO]]))
	{
		self.window.title    = @"Add License";
		self.window.level    = NSModalPanelWindowLevel;
		self.window.delegate = self;

		self.viewController = [[AddLicenseViewController alloc] init];
		self.viewController.representedObject = license;
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

@interface LicenseManager ()
@property (nonatomic) License* license;
@end

@implementation LicenseManager
+ (instancetype)sharedInstance
{
	static LicenseManager* sharedInstance = [self new];
	return sharedInstance;
}

- (instancetype)init
{
	if(self = [super init])
	{
		_license = [[License alloc] init];
		_license.owner = NSFullUserName();
	}
	return self;
}

- (NSString*)owner
{
	if(_owner == nil)
	{
		_owner = [NSNull null];
		for(auto owner : license::find_all())
		{
			if(license::is_valid(license::decode(license::find(owner)), owner))
			{
				_owner = to_ns(owner);
				break;
			}
		}
	}
	return _owner != [NSNull null] ? _owner : nil;
}

- (BOOL)addLicense:(License*)info
{
	if(info.isValid == NO)
		return NO;

	std::string error = "Unknown error.";
	if(license::add(to_s([info.owner stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet]), to_s(info.licenseAsBase32), &error))
	{
		_owner = info.owner;

		NSAlert* alert        = [[NSAlert alloc] init];
		alert.messageText     = @"License Added to Keychain";
		alert.informativeText = @"Thanks for your support!";
		[alert addButtonWithTitle:@"Continue"];
		[alert runModal];
	}
	else
	{
		NSAlert* alert        = [[NSAlert alloc] init];
		alert.messageText     = @"Failure Adding License to Keychain";
		alert.informativeText = to_ns(error);
		[alert addButtonWithTitle:@"Continue"];
		[alert runModal];
	}

	return YES;
}

- (void)showAddLicenseWindow:(id)sender
{
	static AddLicenseWindowController* windowController = [[AddLicenseWindowController alloc] initWithLicense:_license];
	[windowController showWindow:self];
}
@end
