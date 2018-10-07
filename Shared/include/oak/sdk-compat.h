#if !defined(MAC_OS_X_VERSION_10_12) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12)
@interface NSWindow (Sierra)
+ (void)setAllowsAutomaticWindowTabbing:(BOOL)flag;
@end

@protocol CAAnimationDelegate <NSObject>
@end

#define NSAlertStyleWarning       NSAlertStyleWarning
#define NSAlertStyleInformational NSAlertStyleInformational
#define NSAlertStyleCritical      NSAlertStyleCritical
#endif

#if !defined(MAC_OS_X_VERSION_10_10) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_10)
typedef NS_ENUM(NSInteger, NSVisualEffectMaterial) {
    NSVisualEffectMaterialTitlebar = 3,
    NSVisualEffectMaterialSelection = 4,
    NSVisualEffectMaterialMenu = 5,
    NSVisualEffectMaterialPopover = 6,
    NSVisualEffectMaterialSidebar = 7,
    NSVisualEffectMaterialHeaderView = 10,
    NSVisualEffectMaterialSheet = 11,
    NSVisualEffectMaterialWindowBackground = 12,
    NSVisualEffectMaterialHUDWindow = 13,
    NSVisualEffectMaterialFullScreenUI = 15,
    NSVisualEffectMaterialToolTip = 17,
    NSVisualEffectMaterialContentBackground = 18,
    NSVisualEffectMaterialUnderWindowBackground = 21,
    NSVisualEffectMaterialUnderPageBackground = 22,
    NSVisualEffectMaterialAppearanceBased = 0,
    NSVisualEffectMaterialLight = 1,
    NSVisualEffectMaterialDark = 2,
    NSVisualEffectMaterialMediumLight = 8,
    NSVisualEffectMaterialUltraDark = 9,
};

typedef NS_ENUM(NSInteger, NSVisualEffectBlendingMode) {
    NSVisualEffectBlendingModeBehindWindow,
    NSVisualEffectBlendingModeWithinWindow,
};

typedef NS_ENUM(NSInteger, NSVisualEffectState) {
    NSVisualEffectStateFollowsWindowActiveState,
    NSVisualEffectStateActive,
    NSVisualEffectStateInactive,
};

@interface NSVisualEffectView : NSView
@property NSVisualEffectMaterial material;
@property NSVisualEffectBlendingMode blendingMode;
@property NSVisualEffectState state;
@end
#endif

#if !defined(MAC_OS_X_VERSION_10_13) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_13)
typedef NSString *NSAppearanceName;
#endif

#if !defined(MAC_OS_X_VERSION_10_14) || (MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_14)
extern NSAppearanceName const NSAppearanceNameDarkAqua __attribute__((weak_import));

@interface NSAppearance (Mojave)
- (NSAppearanceName)bestMatchFromAppearancesWithNames:(NSArray<NSAppearanceName> *)appearances;
@end

@interface NSApplication (Mojave)
@property(readonly, strong) NSAppearance *effectiveAppearance;
@end
#endif
