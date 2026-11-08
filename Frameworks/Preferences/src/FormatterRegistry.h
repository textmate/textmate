#import <Foundation/Foundation.h>

@interface FormatterEntry : NSObject
@property (nonatomic, copy) NSString* fileTypeGlob;    // e.g. "*.swift", "*.{js,ts,jsx,tsx}"
@property (nonatomic, copy) NSString* formatterName;   // e.g. "swiftformat"
@property (nonatomic, copy) NSString* command;          // e.g. "swiftformat --stdinpath \"$TM_FILEPATH\""
@property (nonatomic, copy) NSString* executableName;   // e.g. "swiftformat" (binary to search in PATH)
@property (nonatomic, copy) NSString* detectedPath;     // e.g. "/opt/homebrew/bin/swiftformat" or nil
@property (nonatomic) BOOL enabled;
@end

@interface FormatterRegistry : NSObject
+ (instancetype)sharedInstance;

// Returns the format command for a file path, or nil if none detected/enabled
- (NSString*)formatCommandForPath:(NSString*)filePath;

// Returns all known formatter entries (for Preferences UI)
- (NSArray<FormatterEntry*>*)allEntries;

// Update an entry's enabled state or command (from Preferences UI)
- (void)setEnabled:(BOOL)enabled forEntryAtIndex:(NSUInteger)index;
- (void)setCommand:(NSString*)command forEntryAtIndex:(NSUInteger)index;

// Re-detect all executables in PATH
- (void)detectAll;
@end
