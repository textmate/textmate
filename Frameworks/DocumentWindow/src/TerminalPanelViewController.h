#import <Cocoa/Cocoa.h>

// Last-used panel edge, seeding new windows like the file browser's placement/width.
extern NSString* const kUserDefaultsTerminalPanelPlacementKey;

// Values of panelPosition and of the saved placement.
extern NSString* const kTerminalPanelPositionLeft;
extern NSString* const kTerminalPanelPositionRight;
extern NSString* const kTerminalPanelPositionBottom;

// Hosts the existing project layout alongside an independently docked terminal panel.
@interface TerminalPanelViewController : NSViewController
@property (nonatomic) BOOL panelVisible;
@property (nonatomic, copy) NSString* (^terminalDirectoryProvider)(void);
@property (nonatomic, copy) NSDictionary<NSString*, NSString*>* (^terminalEnvironmentProvider)(NSString* directory); // base environment for new shells
@property (nonatomic, copy) NSDictionary<NSString*, NSString*>* (^contextEnvironmentProvider)(NSUUID*);
@property (nonatomic, copy) void (^contextSessionClosed)(NSUUID*);
@property (nonatomic, copy) NSString* panelPosition; // left, right, bottom
@property (nonatomic) CGFloat panelWidth;
@property (nonatomic) CGFloat panelHeight;
- (void)takeTerminalSession:(NSUUID*)identifier fromPanel:(TerminalPanelViewController*)source;
// Placement precedence: user defaults seed new windows, saved session state overrides them on restore, and a detached window copies its source panel.
- (void)takePlacementFromPanel:(TerminalPanelViewController*)source;
@property (nonatomic, copy) void (^detachSessionHandler)(NSUUID*);
@property (nonatomic, copy) void (^lastSessionClosedHandler)(void); // not called when sessions are moved out or the panel is stopped
@property (nonatomic, copy) void (^openTerminalFile)(NSURL*, NSInteger, NSInteger);
@property (nonatomic, copy) NSString* terminalShellPath;                // overrides the account's login shell; for tests
@property (nonatomic, copy) NSArray<NSString*>* terminalShellArguments; // used with terminalShellPath
@property (nonatomic, readonly) NSArray<NSUUID*>* terminalSessionIdentifiers;
- (void)closeTerminalSessions:(NSArray<NSUUID*>*)identifiers; // asks for confirmation when work is running
// Nil unless a session is holding an unseen completion or a blocked program; feeds the window subtitle.
// Recomputed from the session list, so it is correct while the panel is hidden and after a session moves away.
@property (nonatomic, readonly) NSString* terminalAttentionSubtitle;
// Called whenever the session list or one session's state changed, including while the panel is hidden.
@property (nonatomic, copy) void (^terminalAttentionDidChange)(void);
// Test hook: raises or clears one session's unseen flag (state: done/blocked, nil clears) without a program report.
- (void)simulateSessionAttentionAtIndex:(NSInteger)index state:(NSString*)state kind:(NSString*)kind;
@property (nonatomic, readonly) BOOL hasTerminal; // YES once a shell has been started or received
- (void)startTerminal; // starts the first shell in the directory from terminalDirectoryProvider
- (void)focusTerminal;
- (void)applyTerminalFont:(NSFont*)font;
- (void)applyTerminalBackground:(NSColor*)background foreground:(NSColor*)foreground caret:(NSColor*)caret selection:(NSColor*)selection syntaxColors:(NSArray<NSColor*>*)syntaxColors;
@property (nonatomic, readonly) BOOL terminalHasRunningWork; // a session is running a foreground program
- (void)confirmTerminalCloseInWindow:(NSWindow*)window completionHandler:(void(^)(BOOL canClose))callback; // sheet on window; immediate when idle
- (void)stopTerminal;
- (instancetype)initWithContentView:(NSView*)contentView;
@end
