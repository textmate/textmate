// ============================
// = From the macOS 10.13 SDK =
// ============================

#if !defined(MAC_OS_X_VERSION_10_13) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_13)
typedef NSString *NSAppearanceName;
#endif

// ============================
// = From the macOS 10.14 SDK =
// ============================

#if !defined(MAC_OS_X_VERSION_10_14) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_14)
#define NSVisualEffectMaterialHeaderView (NSVisualEffectMaterial)10
#define NSVisualEffectMaterialToolTip (NSVisualEffectMaterial)17

extern NSAppearanceName const NSAppearanceNameDarkAqua __attribute__((weak_import));

@interface NSAppearance (Mojave)
- (NSAppearanceName)bestMatchFromAppearancesWithNames:(NSArray<NSAppearanceName> *)appearances;
@end

@interface NSApplication (Mojave)
@property(readonly, strong) NSAppearance *effectiveAppearance;
@end

@interface NSView (Mojave)
- (void)viewDidChangeEffectiveAppearance;
@end

@interface NSColor (Mojave)
+ (NSColor*)colorNamed:(NSString*)colorName;
+ (NSColor*)separatorColor;
@end
#endif

// ===========================
// = From the macOS 11.0 SDK =
// ===========================

#if !defined(MAC_OS_X_VERSION_10_16) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_16)
typedef NS_ENUM(NSInteger, NSWindowToolbarStyle) {
	NSWindowToolbarStyleAutomatic,
	NSWindowToolbarStyleExpanded,
	NSWindowToolbarStylePreference,
	NSWindowToolbarStyleUnified,
	NSWindowToolbarStyleUnifiedCompact
};

@interface NSWindow (BigSur)
@property NSWindowToolbarStyle toolbarStyle API_AVAILABLE(macos(11.0));
@end
#endif
