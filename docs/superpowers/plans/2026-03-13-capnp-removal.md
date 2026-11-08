# Cap'n Proto Removal Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove Cap'n Proto dependency by replacing serialization in `plist::cache_t` and `encoding::classifier_t` with `NSKeyedArchiver`/`NSKeyedUnarchiver`.

**Architecture:** Both capnp uses are simple serialize/deserialize of C++ data structures to disk cache files. Replace each with an ObjC helper that bridges C++ ↔ Foundation types, using `NSKeyedArchiver` for persistence. The public C++ API (`load`/`save` taking `std::string` path) stays unchanged. Cache files change extension to `.plist` so old capnp caches are silently ignored and regenerated.

**Tech Stack:** NSKeyedArchiver (Foundation), Objective-C++ bridging

**Spec:** `docs/superpowers/specs/2026-03-13-cmake-migration-design.md` (follow-up item #1)

---

## File Map

| File | Action | Responsibility |
|------|--------|---------------|
| `Frameworks/encoding/src/encoding.mm` | Modify | Replace capnp load/save with NSKeyedArchiver |
| `Frameworks/encoding/src/frequencies.capnp` | Delete | No longer needed |
| `Frameworks/encoding/CMakeLists.txt` | Modify | Remove capnp |
| `Frameworks/plist/src/fs_cache.cc` → `fs_cache.mm` | Rename | Needs ObjC for NSKeyedArchiver |
| `Frameworks/plist/src/fs_cache.h` | Modify | Unify API to load/save only |
| `Frameworks/plist/src/cache.capnp` | Delete | No longer needed |
| `Frameworks/plist/CMakeLists.txt` | Modify | Remove capnp, add .mm glob, add Foundation |
| `Frameworks/BundlesManager/src/BundlesManager.mm` | Modify | Drop legacy plist migration, update API calls and path |
| `Applications/QuickLookGenerator/src/generate.mm` | Modify | Update API call and path |
| `Applications/gtm/src/gtm.cc` | Modify | Update API call and path |
| `Applications/SyntaxMate/src/main.mm` | Modify | Update cache path |
| `CMakeLists.txt` | Modify | Remove `find_package(CapnProto REQUIRED)` |
| `CLAUDE.md` | Modify | Remove Cap'n Proto from deps list |

---

## Chunk 1: encoding framework (simpler, validates the approach)

### Task 1: Replace capnp serialization in encoding::classifier_t

The encoding classifier is the simpler case — small data, already in an ObjC++ file.

**Files:**
- Modify: `Frameworks/encoding/src/encoding.mm`
- Modify: `Frameworks/encoding/CMakeLists.txt`
- Delete: `Frameworks/encoding/src/frequencies.capnp`

- [ ] **Step 1: Replace capnp includes and load/save in encoding.mm**

Remove these includes:
```cpp
#include "frequencies.capnp.h"
#include <capnp/message.h>
#include <capnp/serialize-packed.h>
```

Remove `kCapnpClassifierFormatVersion` constant.

Replace `classifier_t::real_load` with:
```objcpp
void classifier_t::real_load (std::string const& path)
{
   NSData* data = [NSData dataWithContentsOfFile:@(path.c_str())];
   if(!data)
      return;

   NSError* error = nil;
   NSSet* classes = [NSSet setWithObjects:NSDictionary.class, NSString.class, NSNumber.class, nil];
   NSDictionary* root = [NSKeyedUnarchiver unarchivedObjectOfClasses:classes fromData:data error:&error];
   if(!root)
   {
      os_log_error(OS_LOG_DEFAULT, "Failed to load '%{public}s': %{public}@", path.c_str(), error);
      return;
   }
   if([root[@"version"] unsignedIntegerValue] != 1)
      return;

   NSDictionary* charsets = root[@"charsets"];
   for(NSString* charset in charsets)
   {
      NSDictionary* rec = charsets[charset];
      record_t r;
      NSDictionary* words = rec[@"words"];
      for(NSString* word in words)
         r.words.emplace(word.UTF8String, [words[word] unsignedLongLongValue]);
      NSDictionary* bytes = rec[@"bytes"];
      for(NSNumber* byte in bytes)
         r.bytes.emplace(byte.unsignedCharValue, [bytes[byte] unsignedLongLongValue]);
      _charsets.emplace(charset.UTF8String, r);
   }

   for(auto& pair : _charsets)
   {
      for(auto const& word : pair.second.words)
      {
         _combined.words[word.first] += word.second;
         _combined.total_words += word.second;
         pair.second.total_words += word.second;
      }
      for(auto const& byte : pair.second.bytes)
      {
         _combined.bytes[byte.first] += byte.second;
         _combined.total_bytes += byte.second;
         pair.second.total_bytes += byte.second;
      }
   }
}
```

Note: `classifier_t::load` (the public wrapper with try/catch) is preserved unchanged — it continues to call `real_load`.

Replace `classifier_t::save` with:
```objcpp
void classifier_t::save (std::string const& path) const
{
   NSMutableDictionary* charsets = [NSMutableDictionary dictionary];
   for(auto const& pair : _charsets)
   {
      NSMutableDictionary* words = [NSMutableDictionary dictionary];
      for(auto const& word : pair.second.words)
         words[@(word.first.c_str())] = @(word.second);

      NSMutableDictionary* bytes = [NSMutableDictionary dictionary];
      for(auto const& byte : pair.second.bytes)
         bytes[@(byte.first)] = @(byte.second);

      charsets[@(pair.first.c_str())] = @{ @"words": words, @"bytes": bytes };
   }

   NSDictionary* root = @{ @"version": @1, @"charsets": charsets };
   NSError* error = nil;
   NSData* data = [NSKeyedArchiver archivedDataWithRootObject:root requiringSecureCoding:YES error:&error];
   if(!data)
   {
      os_log_error(OS_LOG_DEFAULT, "Failed to save '%{public}s': %{public}@", path.c_str(), error);
      return;
   }
   [data writeToFile:@(path.c_str()) atomically:YES];
}
```

- [ ] **Step 2: Update cache file path in EncodingClassifier**

Change the path in `-init` from `EncodingFrequencies.binary` to `EncodingFrequencies.plist` so old capnp files are ignored:
```objcpp
_path = [NSSearchPathForDirectoriesInDomains(NSCachesDirectory, NSUserDomainMask, YES).firstObject stringByAppendingPathComponent:@"com.macromates.TextMate/EncodingFrequencies.plist"];
```

- [ ] **Step 3: Update encoding CMakeLists.txt**

Remove capnp lines, keep everything else. Result:
```cmake
add_library(encoding STATIC)
file(GLOB _src src/*.mm)
target_sources(encoding PRIVATE ${_src})
textmate_framework(encoding)
```

- [ ] **Step 4: Delete frequencies.capnp**

```bash
git rm Frameworks/encoding/src/frequencies.capnp
```

- [ ] **Step 5: Build and verify**

Run: `make debug`
Expected: Build succeeds with no capnp references in encoding framework.

- [ ] **Step 6: Commit**

```bash
git add -A Frameworks/encoding/
git commit -m "Replace capnp with NSKeyedArchiver in encoding framework"
```

---

## Chunk 2: plist framework

### Task 2: Replace capnp serialization in plist::cache_t

The plist cache is more complex — it stores a typed union (file/directory/link/missing) and embeds binary plist blobs for complex values.

**Files:**
- Rename: `Frameworks/plist/src/fs_cache.cc` → `Frameworks/plist/src/fs_cache.mm`
- Modify: `Frameworks/plist/src/fs_cache.h`
- Modify: `Frameworks/plist/CMakeLists.txt`
- Delete: `Frameworks/plist/src/cache.capnp`

- [ ] **Step 1: Rename fs_cache.cc → fs_cache.mm**

```bash
git mv Frameworks/plist/src/fs_cache.cc Frameworks/plist/src/fs_cache.mm
```

- [ ] **Step 2: Replace capnp load/save in fs_cache.mm**

Remove capnp includes:
```cpp
#include "cache.capnp.h"
#include <capnp/message.h>
#include <capnp/serialize-packed.h>
```

Add Foundation import:
```objcpp
#import <Foundation/Foundation.h>
```

Remove `kCapnpCacheFormatVersion` constant.

Replace `cache_t::real_load` with:
```objcpp
void cache_t::real_load (std::string const& path)
{
   NSData* data = [NSData dataWithContentsOfFile:@(path.c_str())];
   if(!data)
      return;

   NSError* error = nil;
   NSSet* classes = [NSSet setWithObjects:NSDictionary.class, NSString.class, NSNumber.class, NSData.class, NSArray.class, nil];
   NSDictionary* root = [NSKeyedUnarchiver unarchivedObjectOfClasses:classes fromData:data error:&error];
   if(!root)
   {
      os_log_error(OS_LOG_DEFAULT, "Failed to load '%{public}s': %{public}@", path.c_str(), error);
      return;
   }
   if([root[@"version"] unsignedIntegerValue] != 2)
      return;

   NSDictionary* entries = root[@"entries"];
   for(NSString* pathKey in entries)
   {
      NSDictionary* node = entries[pathKey];
      entry_t entry(pathKey.UTF8String);

      NSString* type = node[@"type"];
      if([type isEqualToString:@"file"])
      {
         entry.set_type(entry_type_t::file);
         entry.set_modified([node[@"modified"] unsignedLongLongValue]);

         plist::dictionary_t plist;
         NSDictionary* content = node[@"content"];
         for(NSString* key in content)
         {
            id value = content[key];
            if([value isKindOfClass:NSString.class])
            {
               plist.emplace(key.UTF8String, std::string(((NSString*)value).UTF8String));
            }
            else if([value isKindOfClass:NSData.class])
            {
               plist.emplace(key.UTF8String, plist::parse(std::string((char const*)((NSData*)value).bytes, ((NSData*)value).length)));
            }
         }
         entry.set_content(plist);
      }
      else if([type isEqualToString:@"directory"])
      {
         entry.set_type(entry_type_t::directory);
         entry.set_event_id([node[@"eventId"] unsignedLongLongValue]);
         entry.set_glob_string(((NSString*)node[@"glob"]).UTF8String ?: "");

         std::vector<std::string> v;
         for(NSString* item in node[@"items"])
            v.push_back(item.UTF8String);
         entry.set_entries(v);
      }
      else if([type isEqualToString:@"link"])
      {
         entry.set_type(entry_type_t::link);
         entry.set_link(((NSString*)node[@"link"]).UTF8String);
      }
      else if([type isEqualToString:@"missing"])
      {
         entry.set_type(entry_type_t::missing);
      }

      if(entry.type() != entry_type_t::unknown)
         _cache.emplace(pathKey.UTF8String, entry);
   }
}
```

Replace `cache_t::save_capnp` (keeping the name temporarily, renamed in Task 3):
```objcpp
void cache_t::save_capnp (std::string const& path) const
{
   NSMutableDictionary* entries = [NSMutableDictionary dictionary];
   for(auto const& pair : _cache)
   {
      NSMutableDictionary* node = [NSMutableDictionary dictionary];
      auto const& e = pair.second;

      if(e.is_file())
      {
         node[@"type"] = @"file";
         node[@"modified"] = @(e.modified());

         NSMutableDictionary* content = [NSMutableDictionary dictionary];
         for(auto const& kv : e.content())
         {
            if(std::string const* str = boost::get<std::string>(&kv.second))
            {
               content[@(kv.first.c_str())] = @(str->c_str());
            }
            else
            {
               if(CFPropertyListRef cfPlist = plist::create_cf_property_list(kv.second))
               {
                  if(CFDataRef cfData = CFPropertyListCreateData(kCFAllocatorDefault, cfPlist, kCFPropertyListBinaryFormat_v1_0, 0, nullptr))
                  {
                     content[@(kv.first.c_str())] = (__bridge_transfer NSData*)cfData;
                  }
                  CFRelease(cfPlist);
               }
            }
         }
         node[@"content"] = content;
      }
      else if(e.is_directory())
      {
         node[@"type"] = @"directory";
         node[@"glob"] = @(e.glob_string().c_str());
         node[@"eventId"] = @(e.event_id());

         NSMutableArray* items = [NSMutableArray array];
         for(auto const& s : e.entries())
            [items addObject:@(s.c_str())];
         node[@"items"] = items;
      }
      else if(e.is_link())
      {
         node[@"type"] = @"link";
         node[@"link"] = @(e.link().c_str());
      }
      else if(e.is_missing())
      {
         node[@"type"] = @"missing";
      }

      entries[@(pair.first.c_str())] = node;
   }

   NSDictionary* root = @{ @"version": @2, @"entries": entries };
   NSError* error = nil;
   NSData* data = [NSKeyedArchiver archivedDataWithRootObject:root requiringSecureCoding:YES error:&error];
   if(!data)
   {
      os_log_error(OS_LOG_DEFAULT, "Failed to save '%{public}s': %{public}@", path.c_str(), error);
      return;
   }
   [data writeToFile:@(path.c_str()) atomically:YES];
}
```

Note: `cache_t::load_capnp` (the public wrapper with try/catch) is preserved unchanged — it continues to call `real_load`.

- [ ] **Step 3: Update plist CMakeLists.txt**

Remove capnp lines, add `.mm` glob and Foundation link. The binary dir include paths were only needed for capnp-generated headers — ragel outputs are added as target sources directly by `target_ragel_sources`:
```cmake
add_library(plist STATIC)
file(GLOB _src src/*.cc src/*.mm)
target_sources(plist PRIVATE ${_src})
textmate_framework(plist)
target_include_directories(plist PRIVATE
  "${CMAKE_CURRENT_SOURCE_DIR}/src")
target_link_libraries(plist PUBLIC text cf io "-framework CoreFoundation" "-framework Foundation")

# Ragel sources
target_ragel_sources(plist src/ascii.rl)

textmate_add_tests(plist)
```

- [ ] **Step 4: Delete cache.capnp**

```bash
git rm Frameworks/plist/src/cache.capnp
```

- [ ] **Step 5: Build and verify**

Run: `make debug`
Expected: Build succeeds with no capnp references in plist framework.

- [ ] **Step 6: Commit**

```bash
git add -A Frameworks/plist/
git commit -m "Replace capnp with NSKeyedArchiver in plist cache"
```

---

## Chunk 3: Clean up API and remove capnp from build

### Task 3: Unify cache API and drop legacy plist migration

Now that both frameworks use NSKeyedArchiver, rename the API from `load_capnp`/`save_capnp` to `load`/`save` and drop the legacy plist migration code.

**Files:**
- Modify: `Frameworks/plist/src/fs_cache.h`
- Modify: `Frameworks/plist/src/fs_cache.mm`
- Modify: `Frameworks/BundlesManager/src/BundlesManager.mm`
- Modify: `Applications/QuickLookGenerator/src/generate.mm`
- Modify: `Applications/gtm/src/gtm.cc`
- Modify: `Applications/SyntaxMate/src/main.mm`

- [ ] **Step 1: Unify fs_cache.h API**

Remove `load_capnp`/`save_capnp` declarations. The public interface becomes:
```cpp
void load (std::string const& path);
void save (std::string const& path) const;
```

- [ ] **Step 2: Update fs_cache.mm**

- Remove old plist-based `cache_t::load` (the one reading `kPropertyCacheFormatVersion`)
- Rename `cache_t::load_capnp` → `cache_t::load` (preserving the try/catch wrapper around `real_load`)
- Rename `cache_t::save_capnp` → `cache_t::save`
- Remove old plist-based `cache_t::save`
- Remove `kPropertyCacheFormatVersion` constant

- [ ] **Step 3: Update all callers**

In `BundlesManager.mm`:
- Replace `cache.save_capnp(bundlesIndexPath)` → `cache.save(bundlesIndexPath)` (2 occurrences: lines 333, 528)
- Replace `cache.load_capnp(bundlesIndexPath)` → `cache.load(bundlesIndexPath)` (line 533)
- Remove the legacy plist migration block entirely (lines 523-529: the `oldPath` variable, `access()` check, `cache.load(oldPath)`, `cache.save_capnp`, `unlink`)
- Change `bundlesIndexPath` from `BundlesIndex.binary` to `BundlesIndex.plist` (line 520)

In `Applications/QuickLookGenerator/src/generate.mm`:
- Replace `cache.load_capnp(...)` → `cache.load(...)`
- Update path from `BundlesIndex.binary` to `BundlesIndex.plist`

In `Applications/gtm/src/gtm.cc`:
- Replace `cache.load_capnp(path)` → `cache.load(path)`
- Update the default path constant from `BundlesIndex.binary` to `BundlesIndex.plist`

In `Applications/SyntaxMate/src/main.mm`:
- Update cache path from `TextMateBundlesIndex.binary` to `TextMateBundlesIndex.plist` (line 20)
- (SyntaxMate already calls `cache.load()`, so no API change needed — it will now invoke the NSKeyedArchiver implementation. On first run after update, the old-format file is silently ignored and the cache regenerates.)

- [ ] **Step 4: Build and verify**

Run: `make debug`
Expected: Clean build, no capnp references anywhere.

- [ ] **Step 5: Commit**

```bash
git add Frameworks/plist/src/fs_cache.h Frameworks/plist/src/fs_cache.mm \
  Frameworks/BundlesManager/src/BundlesManager.mm \
  Applications/QuickLookGenerator/src/generate.mm \
  Applications/gtm/src/gtm.cc \
  Applications/SyntaxMate/src/main.mm
git commit -m "Unify cache API to load/save, drop legacy plist migration"
```

### Task 4: Remove Cap'n Proto from root CMakeLists.txt

**Files:**
- Modify: `CMakeLists.txt`
- Modify: `CLAUDE.md`

- [ ] **Step 1: Remove find_package(CapnProto) from root CMakeLists.txt**

Delete the line:
```cmake
find_package(CapnProto REQUIRED)
```

- [ ] **Step 2: Update CLAUDE.md dependencies list**

Remove "Cap'n Proto" from the dependencies line. New line:
```
ragel, boost, google-sparsehash, ninja, cmake
```

- [ ] **Step 3: Build and verify**

Run: `make clean && make debug`
Expected: Full clean build succeeds. No capnp references in build output.

- [ ] **Step 4: Run tests**

Run: `cd build-debug && ctest --output-on-failure`
Expected: All tests pass.

- [ ] **Step 5: Commit**

```bash
git add CMakeLists.txt CLAUDE.md
git commit -m "Remove Cap'n Proto dependency from build system"
```
