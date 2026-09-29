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

struct MBMenuItem
{
	MBMenuItem withModifierFlags (NSUInteger flags) const      { MBMenuItem res = *this; res.modifierFlags = flags; return res; }
	MBMenuItem withTag (NSInteger value) const                 { MBMenuItem res = *this; res.tag = value; return res; }
	MBMenuItem withIndent (NSInteger value) const              { MBMenuItem res = *this; res.indent = value; return res; }
	MBMenuItem withKey (unichar value) const                   { MBMenuItem res = *this; res.key = value; return res; }
	MBMenuItem withAlternate (BOOL flag) const                 { MBMenuItem res = *this; res.alternate = flag; return res; }
	MBMenuItem withHidden (BOOL flag) const                    { MBMenuItem res = *this; res.hidden = flag; return res; }
	MBMenuItem withSystemMenu (MBMenuType type) const          { MBMenuItem res = *this; res.systemMenu = type; return res; }
	MBMenuItem withRepresentedObject (id value) const          { MBMenuItem res = *this; res.representedObject = value; return res; }
	MBMenuItem withTarget (id value) const                     { MBMenuItem res = *this; res.target = value; return res; }
	MBMenuItem withDelegate (id value) const                   { MBMenuItem res = *this; res.delegate = value; return res; }
	MBMenuItem withRef (NSMenuItem* __strong* value) const      { MBMenuItem res = *this; res.ref = value; return res; }
	MBMenuItem withSubmenuRef (NSMenu* __strong* value) const  { MBMenuItem res = *this; res.submenuRef = value; return res; }
	MBMenuItem withSubmenu (MBMenu const& value) const         { MBMenuItem res = *this; res.submenu = value; return res; }

	NSString*             title             = nil;
	SEL                   action            = NULL;
	NSString*             keyEquivalent     = @"";
	NSUInteger            modifierFlags     = NSEventModifierFlagCommand;
	NSInteger             tag               = 0;
	NSInteger             indent            = 0;
	NSControlStateValue   state             = NSControlStateValueOff;
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

NSMenu* MBCreateMenu (MBMenu const& menu, NSMenu* existingMenu = nil);
NSString* MBDumpMenu (NSMenu* menu);
