# LSP File Watching — `workspace/didChangeWatchedFiles`

## Problem

LSP servers like Intelephense require `workspace/didChangeWatchedFiles` notifications to detect new/deleted/renamed files. Without this, newly created classes (e.g. a Laravel Request class) aren't indexed until the file is manually opened or the server is restarted. Intelephense has no polling mode or server-side file watching — this capability is required.

## Approach

FSEvents + directory snapshot diffing, fully native. When the server dynamically registers for file watching via `client/registerCapability`, we set up an FSEvents watcher on the project root and maintain a snapshot of matching files. On each FSEvent, we recursively scan the affected directory subtree, diff against the snapshot, and send Created/Changed/Deleted notifications to the server.

## Design

### 1. Capability Declaration

Add `workspace` capabilities to `sendInitialize` in LSPClient:

```objc
{"workspace", {
    {"didChangeWatchedFiles", {
        {"dynamicRegistration", true}
    }}
}}
```

### 2. Registration Handling

`client/registerCapability` is a server-to-client **request** (has an `id`). Handle it as a **named branch** in `handleMessage` (like `workspace/applyEdit`), not via the generic delegate path. This keeps protocol handling in `LSPClient` and makes the response explicit.

```objc
else if(method == "client/registerCapability")
{
    [self handleRegisterCapability:msg["params"]];
    json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", json::object()}};
    [self sendMessage:response];
}
```

The handler must:

- Iterate `registrations[]`, check each `method` for `workspace/didChangeWatchedFiles`
- Parse `registerOptions.watchers[]`:
  - `globPattern` — can be a string (`"**/*.php"`) or a `RelativePattern` object (`{baseUri, pattern}`). For strings, extract file extensions. For `RelativePattern` objects, extract from the `pattern` field and scope to `baseUri`. Log and skip unrecognized formats.
  - Handle brace expansion: `**/*.{php,inc}` → extensions `.php`, `.inc`
  - `kind` bitmask (1=Create, 2=Change, 4=Delete, default 7=All)
- Store registration by ID in a dictionary (supports multiple registrations)
- Set up FSEvents watcher and perform initial snapshot scan
- Pass through non-file-watching registrations to the existing generic handler

Data model per registration:
```objc
@interface LSPFileWatchRegistration : NSObject
@property NSString* registrationId;
@property NSSet<NSString*>* extensions;  // e.g. {".php", ".inc"}
@property int watchKind;                  // bitmask
@property NSString* basePath;             // nil = workingDirectory, or from RelativePattern
@end
```

### 3. Extension Extraction from Glob Patterns

Parse glob patterns to extract file extensions:
- `**/*.php` → `.php`
- `*.php` → `.php`
- `**/*.{php,inc}` → `.php`, `.inc` (brace expansion)
- `RelativePattern` object → extract from `pattern` field using same rules
- Unrecognized patterns (e.g. `Makefile`, complex globs) → store full pattern for exact filename matching, log a warning

### 4. FSEvents Watcher

- Single `FSEventsManager` observer per LSPClient, watching `_workingDirectory` with `observeSubdirectories:YES`
- Set up on first registration, torn down when all registrations are removed or on shutdown
- **Threading**: `FSEventsManager` schedules the FSEventStream on `CFRunLoopGetCurrent()` at the time the observer is added. The observer MUST be added from the main thread to ensure callbacks fire on the main run loop. Wrap the FSEvent handler with `dispatch_async(dispatch_get_main_queue(), ...)` as a safety net.

**Note on `resetObservers`**: Adding/removing an observer on the shared `FSEventsManager` singleton causes ALL FSEvents streams to be torn down and recreated (including FileBrowser's). This creates a brief gap where events could theoretically be lost. In practice the gap is negligible (<1ms) and acceptable.

**Note on FSEvents coalescing**: FSEvents with `kFSEventStreamCreateFlagNone` delivers directory-level events and may coalesce rapid changes, delivering a parent directory instead of the exact leaf. The scan handler must account for this by scanning recursively from the reported directory downward (respecting excludes).

### 5. Snapshot Management

**Threading model**: All snapshot reads and writes happen on the main thread. The initial scan runs file enumeration on a background queue but delivers results to the main thread via `dispatch_async(dispatch_get_main_queue(), ...)` before populating `_fileSnapshot`. The FSEvents observer is added from the main thread (§4), ensuring callbacks also arrive on main. No synchronization primitives needed.

**Initial scan** (on registration):
- Recursively scan each registration's `basePath` (defaulting to `_workingDirectory`) for files matching that registration's extensions
- Skip excluded directories (see §7)
- Store as `NSMutableDictionary<NSString*, NSValue*>` mapping absolute path → modification date as `struct timespec` (from `stat.st_mtimespec` — both `tv_sec` and `tv_nsec` for APFS sub-second precision). Wrap in `NSValue` via `+[NSValue valueWithBytes:objCType:]`.
- Run enumeration on a background dispatch queue; deliver results dict to main thread; populate snapshot; THEN add the FSEvents observer. Events during the scan window are accepted as a known gap (typically <100ms).

**On FSEvent** (directory changed):
- Recursively scan the changed directory subtree (FSEvents may coalesce to a parent)
- Filter for files matching registered extensions, skip excluded directories
- Diff against snapshot for all paths under the changed directory:
  - Path in scan but not in snapshot → **Created** (type 1)
  - Path in snapshot but not in scan → **Deleted** (type 3)
  - Path in both but modDate differs → **Changed** (type 2)
- Update snapshot with new state
- Filter by `watchKind` bitmask: a change is included if ANY registration's watchKind matches the change type AND the file path falls under that registration's `basePath` (union semantics, scoped per-registration). One notification is sent with the merged changes array.
- **Skip open documents (Changed only)**: For **Changed** (type 2) events only, query `LSPClient.delegate` (LSPManager) for currently open file paths via a new delegate method `openDocumentPaths` and suppress the notification. The server already receives content changes for open documents via `textDocument/didChange`. **Do NOT suppress Created or Deleted events** — these serve a different purpose (workspace indexing) than `didOpen`/`didClose` (editor buffer tracking).
- Send notification

### 6. Open Document Filtering API

Add a new optional delegate method:

```objc
@protocol LSPClientDelegate
// ... existing methods ...
- (NSSet<NSString*>*)lspClientOpenDocumentPaths:(LSPClient*)client;
@end
```

LSPManager implements this by mapping `_openDocuments` UUIDs to file paths via `[OakDocument documentWithIdentifier:].path`. LSPFileWatcher calls this through LSPClient's delegate to filter changes.

### 7. Notification

```objc
[self sendNotification:@"workspace/didChangeWatchedFiles" params:params];
```

Where `params`:
```json
{
    "changes": [
        {"uri": "file:///path/to/File.php", "type": 1}
    ]
}
```

**Debouncing**: Use a 200ms debounce timer to batch rapid changes into a single notification. FSEvents already coalesces at 0.5s, so a shorter debounce avoids stacking latency (worst case: ~700ms from file change to server notification).

**Batch size limit**: Cap each notification at 500 changes. If more changes are pending, send multiple notifications in sequence. This prevents overwhelming servers after bulk operations like `composer install`.

### 8. Excluded Directories

Hardcoded default excludes (always applied, non-removable):
- `.git/`, `.hg/`, `.svn/`
- `build/`, `dist/`, `.cache/`

VCS internals and build output directories that should never be watched. All other directories (including `vendor/`, `node_modules/`) are NOT excluded by default — the server's glob patterns are the authority on what to watch. Intelephense specifically watches `vendor/` for Composer autoloaded packages.

Configurable via `.tm_properties`:
```
lspFileWatchExclude = node_modules/,storage/framework/,tmp/
```

The setting is **additive** — user-specified directories are added to the VCS defaults.

### 9. Unregistration

Handle `client/unregisterCapability` as a **named branch** in `handleMessage` (alongside `registerCapability`):

```objc
else if(method == "client/unregisterCapability")
{
    [self handleUnregisterCapability:msg["params"]];
    json response = {{"jsonrpc", "2.0"}, {"id", requestId}, {"result", json::object()}};
    [self sendMessage:response];
}
```

The handler must:
- Iterate `unregisterations[]`, check each `method` for `workspace/didChangeWatchedFiles`
- Remove the registration by ID
- If no file-watch registrations remain, remove FSEvents observer and clear snapshot
- Pass through non-file-watching unregistrations to the delegate

### 10. Cleanup

- On LSP shutdown (`shutdown`/`exit`): remove FSEvents observer, clear snapshot, invalidate debounce timer
- On `NSApplicationWillTerminateNotification`: same

### 11. Framework Dependency: Move FSEventsManager

`FSEventsManager` currently lives in `Frameworks/FileBrowser/`. It's a general-purpose utility. Move it to `Frameworks/io/` (which already contains `events.h`/`events.cc` for kqueue-based watching).

Steps:
- Move `FSEventsManager.h` and `FSEventsManager.mm` from `FileBrowser/src/` to `io/src/`
- Update `FileBrowser` CMakeLists to remove the files
- Update `io` CMakeLists to add the files
- Update `#import` paths in FileBrowser consumers (FileItemObserver.mm, etc.)
- Add `io` to `lsp` framework's dependency list in CMakeLists (currently depends on `document settings text ns` — `io` is NOT listed)

### 12. Implementation Class: LSPFileWatcher

Extract snapshot + diffing logic into a standalone `LSPFileWatcher` class (in `Frameworks/lsp/src/`). This keeps `LSPClient` focused on protocol handling and allows unit testing independently.

```objc
@interface LSPFileWatcher : NSObject
- (instancetype)initWithRootDirectory:(NSString*)root excludes:(NSArray<NSString*>*)excludes;
- (void)addExtensions:(NSSet<NSString*>*)exts;
- (void)performInitialScanOnQueue:(dispatch_queue_t)queue completion:(void(^)(NSDictionary<NSString*, NSValue*>*))completion;
- (NSArray<NSDictionary*>*)diffForChangedDirectory:(NSString*)dirPath currentSnapshot:(NSMutableDictionary<NSString*, NSValue*>*)snapshot;
@end
```

`LSPClient` creates and owns the `LSPFileWatcher`, feeds it FSEvent callbacks, and sends the resulting notifications.

### 13. Performance Considerations

- **Recursive scan scoped to changed subtree**: FSEvents reports the changed directory; we scan from there downward, not the whole tree
- **Initial scan**: Background queue, extension-filtered, excludes applied
- **Large projects**: Log a warning if snapshot exceeds 10,000 files. Future: `lspFileWatchMaxFiles` setting to cap
- **Debouncing**: 200ms timer batches rapid changes (e.g. `composer install`)
- **ModDate comparison**: Uses `struct timespec` (sec + nsec) for APFS sub-second precision
- **Symlinks**: Not followed (FSEvents + NSFileManager default behavior). Documented as known limitation.

## Known Limitations

- Full glob pattern matching not supported (extension extraction + brace expansion only)
- No `.gitignore` parsing
- Symlinks not followed into directories outside the watched root
- Single workspace root only (no multi-root workspace support)
- `RelativePattern` with `baseUri` outside `_workingDirectory` is logged and skipped
- `workspace/didChangeWorkspaceFolders` is a separate feature
- `kFSEventStreamCreateFlagFileEvents` could eliminate snapshot diffing but requires separate FSEventStream — deferred to future optimization

## Testing

- Manual: create a PHP file via terminal while Intelephense is running → verify it's indexed without opening
- Manual: delete a PHP file → verify diagnostics clear
- Manual: rename a PHP file → verify old diagnostics clear, new file indexed
- Manual: `composer install` / bulk file creation → verify no performance issues or notification flooding
- Manual: large project (e.g. Laravel app) → verify no performance issues
- Unit: extension extraction from glob patterns (string, brace expansion, RelativePattern)
- Unit: snapshot diffing logic (created/changed/deleted detection)
- Unit: exclude directory filtering (defaults + additive custom)
- Unit: open document filtering
