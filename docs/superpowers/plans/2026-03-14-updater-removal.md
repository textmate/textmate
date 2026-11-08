# Updater Removal Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove the self-update system (SoftwareUpdate framework, updater framework, bl app) and the C++ network framework, while preserving utilities used elsewhere (OakCompareVersionStrings → OakFoundation, OakDownloadManager → BundlesManager, tbz extraction → io framework).

**Architecture:** Three dead-code directories are deleted outright (`Frameworks/updater/`, `Applications/bl/`, `Frameworks/network/`). Two utility classes are relocated from `Frameworks/SoftwareUpdate/` before it is deleted: `OakCompareVersionStrings` moves to `OakFoundation`, and `OakDownloadManager` moves to `BundlesManager` (its only remaining consumer). The `tbz_t` class moves from `network` to `io` (its only non-updater consumer is AppController's first-launch bundle extraction). The SoftwareUpdate preferences pane is split: update UI is removed, crash-report UI is preserved as a standalone pane.

**Tech Stack:** Objective-C++, CMake, CxxTest

---

## Chunk 1: Relocate Utilities and Remove Dead Code

### Task 1: Move `OakCompareVersionStrings` to OakFoundation

This function is a general-purpose semver comparator used by BundlesManager and TerminalPreferences. It does not belong in a SoftwareUpdate framework.

**Files:**
- Create: `Frameworks/OakFoundation/src/OakCompareVersionStrings.mm`
- Create: `Frameworks/OakFoundation/src/OakCompareVersionStrings.h`
- Modify: `Frameworks/SoftwareUpdate/src/SoftwareUpdate.h:20` (remove declaration)
- Modify: `Frameworks/SoftwareUpdate/src/OakCompareVersionStrings.mm:1` (change header include)
- Modify: `Frameworks/BundlesManager/src/Bundle.mm:3` (change import)
- Modify: `Frameworks/Preferences/src/TerminalPreferences.mm:7` (change import)
- Modify: `Frameworks/SoftwareUpdate/tests/t_OakCompareVersionStrings.mm:1` (change import)

- [ ] **Step 1: Create the new header `OakCompareVersionStrings.h` in OakFoundation**

```objc
// Frameworks/OakFoundation/src/OakCompareVersionStrings.h
#ifndef OAK_COMPARE_VERSION_STRINGS_H_Y3LQS7KP
#define OAK_COMPARE_VERSION_STRINGS_H_Y3LQS7KP

#import <Foundation/Foundation.h>

NSComparisonResult OakCompareVersionStrings (NSString* lhsString, NSString* rhsString);

#endif /* end of include guard: OAK_COMPARE_VERSION_STRINGS_H_Y3LQS7KP */
```

- [ ] **Step 2: Move the implementation to OakFoundation**

Copy `Frameworks/SoftwareUpdate/src/OakCompareVersionStrings.mm` to `Frameworks/OakFoundation/src/OakCompareVersionStrings.mm`.

Change line 1 from:
```objc
#import "SoftwareUpdate.h"
```
to:
```objc
#import "OakCompareVersionStrings.h"
```

The rest of the file (static helpers `is_numeric`, `components`, `strip_trailing_zeroes`, `namespace version`, and the `OakCompareVersionStrings` function) remains unchanged.

- [ ] **Step 3: Move the test to OakFoundation**

Create `Frameworks/OakFoundation/tests/t_OakCompareVersionStrings.mm` by copying from `Frameworks/SoftwareUpdate/tests/t_OakCompareVersionStrings.mm`.

Change line 1 from:
```objc
#import <SoftwareUpdate/SoftwareUpdate.h>
```
to:
```objc
#import <OakFoundation/OakCompareVersionStrings.h>
```

- [ ] **Step 4: Add tests to OakFoundation CMakeLists.txt**

Append the following line at the end of `Frameworks/OakFoundation/CMakeLists.txt`:
```cmake
textmate_add_tests(OakFoundation)
```

- [ ] **Step 5: Update consumers to import from OakFoundation**

In `Frameworks/BundlesManager/src/Bundle.mm:3`, change:
```objc
#import <SoftwareUpdate/SoftwareUpdate.h> // OakCompareVersionStrings()
```
to:
```objc
#import <OakFoundation/OakCompareVersionStrings.h>
```

In `Frameworks/Preferences/src/TerminalPreferences.mm:7`, change:
```objc
#import <SoftwareUpdate/SoftwareUpdate.h> // OakCompareVersionStrings()
```
to:
```objc
#import <OakFoundation/OakCompareVersionStrings.h>
```

- [ ] **Step 6: Build and run tests**

Run: `cd /Users/fenrir/code/textmate && make debug`
Expected: Build succeeds (OakCompareVersionStrings now compiles from OakFoundation).

Run: `cd build-debug && ctest -R OakFoundation --output-on-failure`
Expected: t_OakCompareVersionStrings tests pass.

- [ ] **Step 7: Commit**

```
git add Frameworks/OakFoundation/src/OakCompareVersionStrings.h \
        Frameworks/OakFoundation/src/OakCompareVersionStrings.mm \
        Frameworks/OakFoundation/tests/t_OakCompareVersionStrings.mm \
        Frameworks/OakFoundation/CMakeLists.txt \
        Frameworks/BundlesManager/src/Bundle.mm \
        Frameworks/Preferences/src/TerminalPreferences.mm
git commit -m "Move OakCompareVersionStrings from SoftwareUpdate to OakFoundation

General-purpose version comparator used by BundlesManager and
TerminalPreferences — doesn't belong in the update subsystem."
```

---

### Task 2: Move `OakDownloadManager` to BundlesManager

OakDownloadManager is a signed-archive downloader. After removing SoftwareUpdate, its only consumer is BundlesManager. Move it there.

**Files:**
- Move: `Frameworks/SoftwareUpdate/src/OakDownloadManager.h` → `Frameworks/BundlesManager/src/OakDownloadManager.h`
- Move: `Frameworks/SoftwareUpdate/src/OakDownloadManager.mm` → `Frameworks/BundlesManager/src/OakDownloadManager.mm`
- Modify: `Frameworks/BundlesManager/src/BundlesManager.mm:7` (change import path)
- Modify: `Frameworks/BundlesManager/CMakeLists.txt:5-6` (remove SoftwareUpdate dep, add Security framework)

- [ ] **Step 1: Copy OakDownloadManager files to BundlesManager**

Copy `Frameworks/SoftwareUpdate/src/OakDownloadManager.h` to `Frameworks/BundlesManager/src/OakDownloadManager.h`.
Copy `Frameworks/SoftwareUpdate/src/OakDownloadManager.mm` to `Frameworks/BundlesManager/src/OakDownloadManager.mm`.

No content changes needed — the .mm file only imports its own header and `<oak/misc.h>`.

- [ ] **Step 2: Update BundlesManager.mm import**

In `Frameworks/BundlesManager/src/BundlesManager.mm:7`, change:
```objc
#import <SoftwareUpdate/OakDownloadManager.h>
```
to:
```objc
#import "OakDownloadManager.h"
```

- [ ] **Step 3: Update BundlesManager CMakeLists.txt**

Replace `Frameworks/BundlesManager/CMakeLists.txt` contents:
```cmake
add_library(BundlesManager STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(BundlesManager PRIVATE ${_src})
textmate_framework(BundlesManager)
target_link_libraries(BundlesManager PUBLIC
  OakAppKit OakFoundation bundles io ns regexp text "-framework Foundation" "-framework Security")
```

Changes: removed `SoftwareUpdate` from link list, added `-framework Security` (needed by OakDownloadManager for `SecItemImport`, `SecVerifyTransformCreate`, etc.).

- [ ] **Step 4: Build to verify**

Run: `cd /Users/fenrir/code/textmate && make debug`
Expected: Build succeeds.

- [ ] **Step 5: Commit**

```
git add Frameworks/BundlesManager/src/OakDownloadManager.h \
        Frameworks/BundlesManager/src/OakDownloadManager.mm \
        Frameworks/BundlesManager/src/BundlesManager.mm \
        Frameworks/BundlesManager/CMakeLists.txt
git commit -m "Move OakDownloadManager from SoftwareUpdate to BundlesManager

BundlesManager is the only remaining consumer after removing the
self-update system. Security framework link added for signature
verification."
```

---

### Task 3: Move `tbz_t` from network to io framework

The `network::tbz_t` class is a tar-bzip2 extractor that uses `io::spawn`. It's used only by AppController for first-launch bundle extraction. The rest of the `network` framework (libcurl download, signature checking, key chain) is only used by the disabled `updater` framework. Move `tbz_t` to `io` and delete `network`.

**Files:**
- Move: `Frameworks/network/src/tbz.h` → `Frameworks/io/src/tbz.h` (rename namespace)
- Move: `Frameworks/network/src/tbz.cc` → `Frameworks/io/src/tbz.cc` (rename namespace)
- Modify: `Applications/TextMate/src/AppController.mm:33` (change import, rename usage)
- Modify: `Applications/TextMate/CMakeLists.txt:10,16-19` (remove network dep & private include)

- [ ] **Step 1: Create io/tbz.h**

Create `Frameworks/io/src/tbz.h`:
```cpp
#ifndef IO_TBZ_H_NEU56OWR
#define IO_TBZ_H_NEU56OWR

#include <io/exec.h>
namespace io
{
	struct tbz_t
	{
		tbz_t (std::string const& dest);
		~tbz_t ();

		bool wait_for_tbz (std::string* output = nullptr, std::string* error = nullptr);

		int input_fd () const  { return _process.in; }
		operator bool () const { return _process.pid != -1; }

	private:
		dispatch_group_t _group = nullptr;
		io::process_t _process;
		std::string _output, _error;
		int _status;
	};

} /* io */

#endif /* end of include guard: IO_TBZ_H_NEU56OWR */
```

- [ ] **Step 2: Create io/tbz.cc**

Create `Frameworks/io/src/tbz.cc`:
```cpp
#include "tbz.h"
#include <text/format.h>
#include <text/trim.h>

namespace io
{
	tbz_t::tbz_t (std::string const& dest)
	{
		if(_group = dispatch_group_create())
		{
			if(_process = io::spawn(std::vector<std::string>{ "/usr/bin/tar", "-jxmkC", dest, "--strip-components", "1", "--disable-copyfile", "--exclude", "._*" }))
			{
				dispatch_group_async(_group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
					io::exhaust_fd(_process.out, &_output);
				});
				dispatch_group_async(_group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
					io::exhaust_fd(_process.err, &_error);
				});
				dispatch_group_async(_group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
					if(waitpid(_process.pid, &_status, 0) != _process.pid)
						perror("tbz_t: waitpid");
				});
			}
		}
	}

	tbz_t::~tbz_t ()
	{
		if(_group)
		{
			dispatch_group_wait(_group, DISPATCH_TIME_FOREVER);
			dispatch_release(_group);
		}
	}

	bool tbz_t::wait_for_tbz (std::string* output, std::string* error)
	{
		if(!_process)
			return false;

		close(_process.in);
		dispatch_group_wait(_group, DISPATCH_TIME_FOREVER);

		if(output)
			output->swap(_output);
		if(error)
			error->swap(_error);

		return WIFEXITED(_status) && WEXITSTATUS(_status) == 0;
	}

} /* io */
```

Note: The file extension must be `.cc` not `.mm` to match the existing io framework pattern. However, it uses `dispatch_group_async` with blocks (^{}), which requires Objective-C or `-fblocks`. Check: io framework's CMakeLists already compiles `.cc` files — this should work because clang enables blocks by default on macOS even in C++ mode.

- [ ] **Step 3: Update AppController.mm**

In `Applications/TextMate/src/AppController.mm:33`, change:
```objc
#import "tbz.h"
```
to:
```objc
#import <io/tbz.h>
```

At line 518, change:
```objc
network::tbz_t tbz(dest);
```
to:
```objc
io::tbz_t tbz(dest);
```

- [ ] **Step 4: Update TextMate app CMakeLists.txt**

In `Applications/TextMate/CMakeLists.txt`, remove `network` from the target_link_libraries line 10:
```cmake
  crash document io kvdb license ns plist regexp scm settings text theme
```

Remove lines 16-19 (the network private include directory block):
```cmake
# network headers need special handling — can't use <network/...> include
# path because it shadows Apple's Network.framework on case-insensitive macOS
target_include_directories(TextMate PRIVATE
  "${CMAKE_SOURCE_DIR}/Frameworks/network/src")
```

- [ ] **Step 5: Build to verify**

Run: `cd /Users/fenrir/code/textmate && make debug`
Expected: Build succeeds.

- [ ] **Step 6: Commit**

```
git add Frameworks/io/src/tbz.h Frameworks/io/src/tbz.cc \
        Applications/TextMate/src/AppController.mm \
        Applications/TextMate/CMakeLists.txt
git commit -m "Move tbz_t from network to io framework, fix error output bug

Only non-updater usage is first-launch bundle extraction in
AppController. Namespace changed from network:: to io::.
Also fixes wait_for_tbz writing error string to output pointer."
```

---

### Task 4: Remove SoftwareUpdate framework

With OakCompareVersionStrings and OakDownloadManager relocated, the SoftwareUpdate framework only contains the auto-updater (SoftwareUpdate class, Update Badge image). Remove it entirely.

**Files:**
- Delete: `Frameworks/SoftwareUpdate/` (entire directory)
- Modify: `CMakeLists.txt:119` (remove add_subdirectory)
- Modify: `Applications/TextMate/CMakeLists.txt:9` (remove SoftwareUpdate from link list)
- Modify: `Applications/TextMate/CMakeLists.txt:82` (remove SoftwareUpdate resource glob)
- Modify: `Frameworks/Preferences/CMakeLists.txt:1,8` (remove SoftwareUpdate dep)

- [ ] **Step 1: Remove add_subdirectory from root CMakeLists.txt**

In `CMakeLists.txt:119`, remove the line:
```cmake
add_subdirectory(Frameworks/SoftwareUpdate)
```

- [ ] **Step 2: Remove SoftwareUpdate from TextMate app link list**

In `Applications/TextMate/CMakeLists.txt:9`, remove `SoftwareUpdate` from the target_link_libraries list:
```cmake
  OakSystem OakTextView Preferences authorization bundles cf command
```

- [ ] **Step 3: Remove SoftwareUpdate resource glob**

In `Applications/TextMate/CMakeLists.txt:82`, remove the line:
```cmake
  "${CMAKE_SOURCE_DIR}/Frameworks/SoftwareUpdate/resources/*.tiff"
```

- [ ] **Step 4: Remove SoftwareUpdate from Preferences link list**

In `Frameworks/Preferences/CMakeLists.txt`, update lines 1 and 7-8:

Line 1 (comment), remove `SoftwareUpdate`:
```cmake
# rave: require BundlesManager OakAppKit OakFoundation MenuBuilder bundles io ns regexp settings text
```

Lines 7-8, remove `SoftwareUpdate`:
```cmake
target_link_libraries(Preferences PUBLIC
  BundlesManager OakAppKit OakFoundation MenuBuilder
  bundles io ns regexp settings text)
```

- [ ] **Step 5: Delete the SoftwareUpdate framework directory**

```
rm -rf Frameworks/SoftwareUpdate
```

- [ ] **Step 6: Build to verify** (will fail — SoftwareUpdate consumers in AppController and Preferences not yet updated; those are Tasks 5 and 6)

Skip build for now; proceed to Tasks 5 and 6.

---

### Task 5: Remove update UI from AppController

**Files:**
- Modify: `Applications/TextMate/src/AppController.h:27` (remove performSoftwareUpdateCheck declaration)
- Modify: `Applications/TextMate/src/AppController.mm:27,104-105,491-498,713-716` (remove SoftwareUpdate import, menu items, channel setup, action method)

- [ ] **Step 1: Remove performSoftwareUpdateCheck from AppController.h**

In `Applications/TextMate/src/AppController.h:27`, remove the line:
```objc
- (IBAction)performSoftwareUpdateCheck:(id)sender;
```

- [ ] **Step 2: Remove SoftwareUpdate import from AppController.mm**

In `Applications/TextMate/src/AppController.mm:27`, remove:
```objc
#import <SoftwareUpdate/SoftwareUpdate.h>
```

- [ ] **Step 3: Remove "Check for Update" menu items**

In `Applications/TextMate/src/AppController.mm:104-105`, remove these two lines from the mainMenu method:
```objc
				{ @"Check for Update",      @selector(performSoftwareUpdateCheck:)         },
				{ @"Check for Test Build",  @selector(performSoftwareUpdateCheck:),       .modifierFlags = NSEventModifierFlagCommand|NSEventModifierFlagOption, .alternate = YES },
```

- [ ] **Step 4: Remove SoftwareUpdate channel setup from applicationWillFinishLaunching**

In `Applications/TextMate/src/AppController.mm`, remove lines 491-498 (the `parms` variable and `SoftwareUpdate.sharedInstance.channels` block):
```objc
	NSOperatingSystemVersion osVersion = NSProcessInfo.processInfo.operatingSystemVersion;
	NSString* parms = [NSString stringWithFormat:@"v=%@&os=%ld.%ld.%ld", [[[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleShortVersionString"] stringByAddingPercentEncodingWithAllowedCharacters:NSCharacterSet.URLQueryAllowedCharacterSet], osVersion.majorVersion, osVersion.minorVersion, osVersion.patchVersion];

	SoftwareUpdate.sharedInstance.channels = @{
		kSoftwareUpdateChannelRelease:    [NSURL URLWithString:[NSString stringWithFormat:@"" REST_API "/releases/release?%@", parms]],
		kSoftwareUpdateChannelPrerelease: [NSURL URLWithString:[NSString stringWithFormat:@"" REST_API "/releases/beta?%@", parms]],
		kSoftwareUpdateChannelCanary:     [NSURL URLWithString:[NSString stringWithFormat:@"" REST_API "/releases/nightly?%@", parms]],
	};
```

- [ ] **Step 5: Remove performSoftwareUpdateCheck action method**

In `Applications/TextMate/src/AppController.mm`, remove lines 713-716:
```objc
- (IBAction)performSoftwareUpdateCheck:(id)sender
{
	[SoftwareUpdate.sharedInstance checkForUpdate:self];
}
```

- [ ] **Step 6: Commit** (defer build to after Task 6)

---

### Task 6: Rework SoftwareUpdatePreferences into CrashReportPreferences

The SoftwareUpdatePreferences pane contains both update settings (remove) and crash report settings (keep). Replace it with a stripped-down crash-report-only pane.

**Files:**
- Delete: `Frameworks/Preferences/src/SoftwareUpdatePreferences.h`
- Delete: `Frameworks/Preferences/src/SoftwareUpdatePreferences.mm`
- Delete: `Frameworks/Preferences/icons/Software Update.png`
- Delete: `Frameworks/Preferences/icons/Software Update@2x.png`
- Create: `Frameworks/Preferences/src/CrashReportPreferences.h`
- Create: `Frameworks/Preferences/src/CrashReportPreferences.mm`
- Modify: `Frameworks/Preferences/src/Preferences.mm:6,100` (replace SoftwareUpdatePreferences with CrashReportPreferences)

- [ ] **Step 1: Create CrashReportPreferences.h**

```objc
// Frameworks/Preferences/src/CrashReportPreferences.h
#import "PreferencesPane.h"

@interface CrashReportPreferences : PreferencesPane
@end
```

- [ ] **Step 2: Create CrashReportPreferences.mm**

```objc
// Frameworks/Preferences/src/CrashReportPreferences.mm
#import "CrashReportPreferences.h"
#import "Keys.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

@implementation CrashReportPreferences
- (id)init
{
	if(self = [super initWithNibName:nil label:@"Crash Reports" image:[NSImage imageSystemSymbolName:@"exclamationmark.triangle" accessibilityDescription:@"Crash Reports"]])
	{
	}
	return self;
}

- (void)loadView
{
	NSButton* submitCrashReportsCheckBox = OakCreateCheckBox(@"Submit to MacroMates");

	NSFont* smallFont = [NSFont messageFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeSmall]];
	NSTextField* contactTextField = [NSTextField textFieldWithString:@"Anonymous"];
	contactTextField.font        = smallFont;
	contactTextField.controlSize = NSControlSizeSmall;

	NSStackView* contactStackView = [NSStackView stackViewWithViews:@[
		OakCreateLabel(@"Contact:", smallFont), contactTextField
	]];
	contactStackView.alignment  = NSLayoutAttributeFirstBaseline;
	contactStackView.edgeInsets = { .left = 18 };
	[contactStackView setHuggingPriority:NSLayoutPriorityDefaultHigh-1 forOrientation:NSLayoutConstraintOrientationVertical];

	NSGridView* gridView = [NSGridView gridViewWithViews:@[
		@[ OakCreateLabel(@"Crash reports:"), submitCrashReportsCheckBox ],
		@[ NSGridCell.emptyContentView,       contactStackView          ],
	]];

	self.view = OakSetupGridViewWithSeparators(gridView, { });

	[submitCrashReportsCheckBox bind:NSValueBinding   toObject:NSUserDefaultsController.sharedUserDefaultsController withKeyPath:[NSString stringWithFormat:@"values.%@", kUserDefaultsDisableCrashReportingKey]   options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
	[contactTextField           bind:NSValueBinding   toObject:NSUserDefaultsController.sharedUserDefaultsController withKeyPath:[NSString stringWithFormat:@"values.%@", kUserDefaultsCrashReportsContactInfoKey] options:nil];
	[contactTextField           bind:NSEnabledBinding toObject:NSUserDefaultsController.sharedUserDefaultsController withKeyPath:[NSString stringWithFormat:@"values.%@", kUserDefaultsDisableCrashReportingKey]   options:@{ NSValueTransformerNameBindingOption: NSNegateBooleanTransformerName }];
}
@end
```

- [ ] **Step 3: Update Preferences.mm**

In `Frameworks/Preferences/src/Preferences.mm:6`, change:
```objc
#import "SoftwareUpdatePreferences.h"
```
to:
```objc
#import "CrashReportPreferences.h"
```

At line 100, change:
```objc
			[[SoftwareUpdatePreferences alloc] init],
```
to:
```objc
			[[CrashReportPreferences alloc] init],
```

- [ ] **Step 4: Delete old files**

```
rm Frameworks/Preferences/src/SoftwareUpdatePreferences.h
rm Frameworks/Preferences/src/SoftwareUpdatePreferences.mm
rm "Frameworks/Preferences/icons/Software Update.png"
rm "Frameworks/Preferences/icons/Software Update@2x.png"
```

- [ ] **Step 5: Build and verify**

Run: `cd /Users/fenrir/code/textmate && make debug`
Expected: Build succeeds with no SoftwareUpdate references remaining.

- [ ] **Step 6: Commit tasks 4, 5, and 6 together**

Stage the specific changed/deleted files:
```
git add CMakeLists.txt \
        Applications/TextMate/CMakeLists.txt \
        Applications/TextMate/src/AppController.h \
        Applications/TextMate/src/AppController.mm \
        Frameworks/Preferences/CMakeLists.txt \
        Frameworks/Preferences/src/Preferences.mm \
        Frameworks/Preferences/src/CrashReportPreferences.h \
        Frameworks/Preferences/src/CrashReportPreferences.mm
git rm -r Frameworks/SoftwareUpdate
git rm Frameworks/Preferences/src/SoftwareUpdatePreferences.h \
       Frameworks/Preferences/src/SoftwareUpdatePreferences.mm \
       "Frameworks/Preferences/icons/Software Update.png" \
       "Frameworks/Preferences/icons/Software Update@2x.png"
git commit -m "Remove SoftwareUpdate framework and update UI

Delete the auto-update system: SoftwareUpdate framework, update
menu items, channel configuration, and preferences pane.
Crash report preferences preserved as standalone CrashReportPreferences
pane using a system symbol icon."
```

---

### Task 7: Delete dead code — updater, bl, network

These are already disabled in CMakeLists (commented out). Delete the directories entirely and clean up references.

**Files:**
- Delete: `Frameworks/updater/` (entire directory)
- Delete: `Applications/bl/` (entire directory)
- Delete: `Frameworks/network/` (entire directory)
- Modify: `CMakeLists.txt:109-110,139` (remove commented-out lines and active network line)

- [ ] **Step 1: Remove CMakeLists.txt references**

In `CMakeLists.txt`, remove these three lines:
```cmake
add_subdirectory(Frameworks/network)
# add_subdirectory(Frameworks/updater)  # disabled — not needed
```
and:
```cmake
# add_subdirectory(Applications/bl)  # disabled — depends on updater
```

- [ ] **Step 2: Delete the directories**

```
rm -rf Frameworks/updater Frameworks/network Applications/bl
```

- [ ] **Step 3: Build and run all tests**

Run: `cd /Users/fenrir/code/textmate && make debug`
Expected: Clean build with no updater/network/bl references.

Run: `cd build-debug && ctest --output-on-failure`
Expected: All tests pass.

- [ ] **Step 4: Commit**

```
git rm -r Frameworks/updater Frameworks/network Applications/bl
git add CMakeLists.txt
git commit -m "Delete dead code: updater, bl, and network frameworks

These were already disabled in CMake. With tbz_t moved to io and
OakDownloadManager moved to BundlesManager, nothing depends on them."
```

---

### Task 8: Clean up metadata and documentation

**Files:**
- Modify: `CLAUDE.md` (remove references to updater, network, bl, SoftwareUpdate; update framework count)
- Modify: `.tm_properties:14` (remove `network`, `SoftwareUpdate`, `updater` from `TM_FRAMEWORKS`)
- Modify: `Applications/TextMate/Info.plist` (remove `TMSigningKeys` dictionary — ~3.5KB of PEM keys only read by deleted SoftwareUpdate.mm)

- [ ] **Step 1: Update CLAUDE.md**

In the Build Quirks section, remove:
```
- `network` framework include path is PRIVATE to avoid case-insensitive collision with Apple's Network.framework
- `Frameworks/updater` and `Applications/bl` disabled (not needed, avoids network collision)
```

In the Repository Structure section, update the app count from `10 apps` to `9 apps` and framework count as needed.

In the Support Frameworks list, remove:
```
- **network/** - Networking
```

In the Applications list, remove `bl` from the parenthetical.

- [ ] **Step 2: Update .tm_properties**

In `.tm_properties:14`, remove `network`, `SoftwareUpdate`, and `updater` from the `TM_FRAMEWORKS` variable value.

- [ ] **Step 3: Remove TMSigningKeys from Info.plist**

In `Applications/TextMate/Info.plist`, remove the entire `TMSigningKeys` key and its `<dict>` value (contains PEM public keys for `org.textmate.duff` and `org.textmate.msheets`). This data was only read by `SoftwareUpdate.mm` (now deleted). BundlesManager has its own hardcoded copy of these keys in its `publicKeys` method.

- [ ] **Step 4: Commit**

```
git add CLAUDE.md .tm_properties Applications/TextMate/Info.plist
git commit -m "Clean up metadata after updater removal

Remove stale TM_FRAMEWORKS entries, dead TMSigningKeys from
Info.plist, and update CLAUDE.md documentation."
```

---

### Task 9: Final verification

- [ ] **Step 1: Verify no stale references remain**

Run: `grep -r "SoftwareUpdate\|Frameworks/updater\|Frameworks/network\|Applications/bl" --include='*.{mm,h,cc,cmake,txt,plist,html,strings,xib}' .`
Expected: No matches (or only in docs/plans/ and historical changelogs like Changes.html).

- [ ] **Step 2: Clean build from scratch**

Run: `cd /Users/fenrir/code/textmate && make clean && make debug`
Expected: Clean build succeeds.

- [ ] **Step 3: Run full test suite**

Run: `cd build-debug && ctest --output-on-failure`
Expected: All tests pass.
