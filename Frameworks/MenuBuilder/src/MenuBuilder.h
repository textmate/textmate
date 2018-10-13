#include <oak/misc.h>

typedef NS_ENUM(NSInteger, MBMenuType) {
	MBMenuTypeRegular = 0,
	MBMenuTypeServices,
	MBMenuTypeOpenRecent,
	MBMenuTypeFont,
	MBMenuTypeWindows,
	MBMenuTypeHelp,
};

struct MBMenuItem;
typedef std::vector<MBMenuItem> MBMenu;

struct PUBLIC MBMenuItem
{
	NSString*             title             = nil;
	SEL                   action            = NULL;
	NSString*             keyEquivalent     = @"";
	NSUInteger            modifierFlags     = NSEventModifierFlagCommand;
	NSInteger             tag               = 0;
	NSInteger             indent            = 0;
	NSCellStateValue      state             = NSOffState;
	id                    target            = nil;
	id                    delegate          = nil;
	unichar               key               = 0;
	BOOL                  separator         = NO;
	BOOL                  alternate         = NO;
	BOOL                  enabled           = YES;
	BOOL                  hidden            = NO;
	MBMenuType            systemMenu        = MBMenuTypeRegular;
	id                    representedObject = nil;
	NSMenuItem* __strong* ref               = nullptr;
	NSMenu* __strong*     submenuRef        = nullptr;
	MBMenu                submenu;
};

PUBLIC NSMenu* MBCreateMenu (MBMenu const& menu, NSMenu* existingMenu = nil);
PUBLIC NSString* MBDumpMenu (NSMenu* menu);
