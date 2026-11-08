#import "EditorLanguageIntelligence.h"
#import "EditorContextBuffer.h"
#import <EditorContextProtocol.h>
#import <document/OakDocument Private.h>
#import <lsp/LSPManager.h>

namespace {
static NSString* Revision(OakDocument* document)
{
	return [EditorContextService.sharedService revisionForDocument:document];
}
static NSString* Bounded(id value, NSUInteger limit)
{
	if(![value isKindOfClass:NSString.class]) return nil;
	NSString* text = value;
	if(text.length <= limit) return text;
	if(limit && CFStringIsSurrogateHighCharacter([text characterAtIndex:limit-1])) --limit;
	return [text substringToIndex:limit];
}
static NSDictionary* Range(id raw)
{
	if(![raw isKindOfClass:NSDictionary.class]) return nil;
	NSMutableDictionary* result = [NSMutableDictionary dictionary];
	for(NSString* key in @[@"start", @"end"]) {
		id point = raw[key];
		if(![point isKindOfClass:NSDictionary.class] || !editor_context::integer(point[@"line"], 2147483646) || !editor_context::integer(point[@"character"], 2147483646)) return nil;
		result[key] = @{ @"line": @([point[@"line"] unsignedIntegerValue]+1), @"column": @([point[@"character"] unsignedIntegerValue]+1) };
	}
	NSUInteger first = [result[@"start"][@"line"] unsignedIntegerValue], last = [result[@"end"][@"line"] unsignedIntegerValue];
	if(last < first || (last == first && [result[@"end"][@"column"] unsignedIntegerValue] < [result[@"start"][@"column"] unsignedIntegerValue])) return nil;
	return result;
}
static NSDictionary* Location(id uri, id range, BOOL allowUnresolved)
{
	if(![uri isKindOfClass:NSString.class] || ![uri length] || [uri length] > 4096 || [uri rangeOfString:[NSString stringWithFormat:@"%C", (unichar)0]].location != NSNotFound) return nil;
	NSDictionary* position = Range(range);
	if(!position && !(allowUnresolved && !range)) return nil;
	NSURL* url = [NSURL URLWithString:uri];
	if(!url.scheme.length) return nil;
	NSString* path = url.isFileURL && (!url.host.length || [url.host isEqual:@"localhost"]) ? url.path : nil;
	NSMutableDictionary* result = [@{ @"uri": uri, @"path": path ?: (id)NSNull.null, @"range": position ?: (id)NSNull.null } mutableCopy];
	if(!position) result[@"unresolved"] = @YES;
	if(path && position) result[@"location"] = [NSString stringWithFormat:@"%@:%@", path, position[@"start"][@"line"]];
	return result;
}
static NSDictionary* QueryMetadata(OakDocument* document)
{
	return @{ @"documentId": document.identifier.UUIDString, @"path": document.path ?: (id)NSNull.null,
		@"name": document.displayName ?: @"Untitled", @"revision": Revision(document) };
}
static NSDictionary* Page(NSDictionary* result, NSDictionary* arguments)
{
	NSArray* items = result[@"items"] ?: @[];
	NSUInteger offset = [arguments[@"offset"] unsignedIntegerValue];
	NSUInteger limit = arguments[@"limit"] ? [arguments[@"limit"] unsignedIntegerValue] : 50;
	if(offset > items.count) return editor_context::failure(@"offset is beyond this result.", @"invalid_offset");
	NSMutableDictionary* page = [result mutableCopy];
	NSUInteger end = MIN(items.count, offset+limit);
	page[@"items"] = [items subarrayWithRange:NSMakeRange(offset, end-offset)];
	page[@"offset"] = @(offset);
	page[@"availableCount"] = @(items.count);
	page[@"nextOffset"] = end < items.count ? @(end) : (id)NSNull.null;
	page[@"truncated"] = @(end < items.count || [result[@"resultsTruncated"] boolValue]);
	return page;
}
}

NSDictionary* EditorLanguageNormalize(NSString* method, id raw, NSDictionary* document, double deadline)
{
	if(editor_context::now() >= deadline) return editor_context::failure(@"Language query timed out.", @"timeout");
	if([method isEqual:@"get_hover"]) {
		if(raw == NSNull.null) return @{ @"items": @[], @"hover": NSNull.null };
		if(![raw isKindOfClass:NSDictionary.class]) return editor_context::failure(@"Invalid hover response from the language server.", @"invalid_language_response");
		id content = raw[@"contents"];
		NSArray* parts = [content isKindOfClass:NSArray.class] ? content : content ? @[content] : @[];
		NSMutableArray* texts = [NSMutableArray array];
		NSUInteger remaining = 8192;
		BOOL truncated = NO;
		for(id part in parts) {
			if(editor_context::now() >= deadline) return editor_context::failure(@"Language query timed out.", @"timeout");
			id value = [part isKindOfClass:NSDictionary.class] ? part[@"value"] : part;
			if(![value isKindOfClass:NSString.class]) continue;
			NSString* text = Bounded(value, remaining);
			truncated |= [value length] > text.length;
			[texts addObject:text];
			remaining -= text.length;
			if(remaining < 2) { truncated |= part != parts.lastObject; break; }
			remaining -= 2;
		}
		if(!texts.count) return editor_context::failure(@"Invalid hover contents from the language server.", @"invalid_language_response");
		return @{ @"items": @[], @"hover": @{ @"text": [texts componentsJoinedByString:@"\n\n"], @"range": Range(raw[@"range"]) ?: (id)NSNull.null,
			@"textTruncated": @(truncated) } };
	}
	BOOL symbols = [method isEqual:@"get_document_symbols"] || [method isEqual:@"search_workspace_symbols"];
	NSArray* entries = raw == NSNull.null ? @[] : [raw isKindOfClass:NSArray.class] ? raw : [method isEqual:@"get_definition"] && [raw isKindOfClass:NSDictionary.class] ? @[raw] : nil;
	if(!entries) return editor_context::failure(@"Invalid response from the language server.", @"invalid_language_response");
	NSMutableArray* items = [NSMutableArray array];
	NSUInteger invalid = 0, visited = 0, remaining = 131072;
	BOOL truncated = NO;
	// An explicit stack bounds hierarchy depth and avoids recursive parsing of
	// malicious or unexpectedly deep document-symbol trees.
	NSMutableArray* stack = [NSMutableArray arrayWithObject:@{ @"entries": entries, @"index": @0, @"depth": @0, @"container": @"" }];
	while(stack.count) {
		if(editor_context::now() >= deadline) return editor_context::failure(@"Language query timed out.", @"timeout");
		NSMutableDictionary* frame = [stack.lastObject mutableCopy];
		NSUInteger index = [frame[@"index"] unsignedIntegerValue];
		NSArray* nodes = frame[@"entries"];
		if(index >= nodes.count) { [stack removeLastObject]; continue; }
		if(items.count >= 1000 || ++visited > 5000) { truncated = YES; break; }
		frame[@"index"] = @(index+1); stack[stack.count-1] = frame;
		id node = nodes[index];
		if(![node isKindOfClass:NSDictionary.class]) { ++invalid; continue; }
		NSDictionary* location;
		if(symbols && node[@"location"]) {
			id rawLocation = node[@"location"];
			location = [rawLocation isKindOfClass:NSDictionary.class] ? Location(rawLocation[@"uri"], rawLocation[@"range"], [method isEqual:@"search_workspace_symbols"]) : nil;
		} else if(symbols) {
			NSString* path = [document[@"path"] isKindOfClass:NSString.class] ? document[@"path"] : nil;
			location = path ? Location([NSURL fileURLWithPath:path].absoluteString, node[@"selectionRange"] ?: node[@"range"], NO) : nil;
		} else location = Location(node[@"targetUri"] ?: node[@"uri"], node[@"targetSelectionRange"] ?: node[@"targetRange"] ?: node[@"range"], NO);
		if(!location || (symbols && ![node[@"name"] isKindOfClass:NSString.class])) { ++invalid; continue; }
		NSMutableDictionary* item = [location mutableCopy];
		if(symbols) {
			item[@"name"] = Bounded(node[@"name"], 256);
			item[@"kind"] = editor_context::integer(node[@"kind"], 26) && [node[@"kind"] integerValue] > 0 ? node[@"kind"] : @0;
			item[@"container"] = Bounded(node[@"containerName"], 256) ?: frame[@"container"];
			item[@"textTruncated"] = @([node[@"name"] length] > 256 || ([node[@"containerName"] isKindOfClass:NSString.class] && [node[@"containerName"] length] > 256));
		}
		NSUInteger size = 0;
		for(id value in item.allValues) if([value isKindOfClass:NSString.class]) size += [value length];
		if(size > remaining) { truncated = YES; break; }
		remaining -= size;
		[items addObject:item];
		if(symbols && [node[@"children"] isKindOfClass:NSArray.class] && [node[@"children"] count]) {
			NSUInteger depth = [frame[@"depth"] unsignedIntegerValue];
			if(depth >= 32) truncated = YES;
			else [stack addObject:@{ @"entries": node[@"children"], @"index": @0, @"depth": @(depth+1), @"container": item[@"name"] }];
		}
	}
	return @{ @"items": items, @"invalidCount": @(invalid), @"resultsTruncated": @(truncated) };
}

@implementation EditorLanguageIntelligence
{
	NSMutableDictionary<NSString*, NSDictionary*>* _results;
}
- (instancetype)init { if((self = [super init])) _results = [NSMutableDictionary dictionary]; return self; }

- (EditorContextSnapshot*)snapshotForMethod:(NSString*)method arguments:(NSDictionary*)arguments documents:(NSArray<OakDocument*>* (^)(void))documents current:(OakDocument*)current caret:(NSUInteger)caret windowId:(NSString*)windowId
{
	NSDictionary* previous = arguments[@"resultId"] ? _results[arguments[@"resultId"]] : nil;
	if(arguments[@"resultId"] && !previous) return [EditorContextSnapshot response:editor_context::failure(@"The language result expired. Run the query again without resultId.", @"result_expired")];
	NSString* targetId = arguments[@"documentId"] ?: previous[@"document"][@"documentId"] ?: current.identifier.UUIDString;
	OakDocument* document;
	for(OakDocument* candidate in documents()) if([candidate.identifier.UUIDString isEqual:targetId]) { document = candidate; break; }
	if(!document.isLoaded || !document.path) return [EditorContextSnapshot response:editor_context::failure(@"Choose a loaded, named document in this terminal's window.", @"document_unavailable")];
	NSDictionary* metadata = QueryMetadata(document);
	if(arguments[@"revision"] && ![arguments[@"revision"] isEqual:metadata[@"revision"]]) return [EditorContextSnapshot response:editor_context::failure(@"The document changed. Read its current context and try again.", @"document_changed")];
	if(previous) {
		if(![previous[@"method"] isEqual:method] || ![previous[@"document"] isEqual:metadata]) return [EditorContextSnapshot response:editor_context::failure(@"This result belongs to a different query or document revision.", @"result_expired")];
		for(NSString* key in @[@"line", @"column", @"query", @"includeDeclaration"])
			if(arguments[key] && ![arguments[key] isEqual:previous[@"arguments"][key]]) return [EditorContextSnapshot response:editor_context::failure(@"Use the original query arguments, or just resultId, offset, and limit.", @"invalid_arguments")];
		return [EditorContextSnapshot response:Page(previous, arguments)];
	}
	BOOL position = editor_context::is_position_query(method);
	if(position && !arguments[@"line"] && current != document) return [EditorContextSnapshot response:editor_context::failure(@"Specify line and column for an inactive document.", @"invalid_arguments")];
	EditorContextBuffer* buffer = [EditorContextBuffer bufferForDocument:document revision:metadata[@"revision"]];
	__weak EditorLanguageIntelligence* weakSelf = self;
	return [EditorContextSnapshot action:^(double deadline, BOOL (^authorized)(void), void (^completion)(NSDictionary*)){
		BOOL (^valid)(void) = ^{ return authorized() && [documents() containsObject:document] && document.isLoaded && [QueryMetadata(document) isEqual:metadata]; };
		__block BOOL finished = NO;
		__block dispatch_block_t cancel;
		void (^finish)(NSDictionary*) = ^(NSDictionary* response){ if(finished) return; finished = YES; completion(response); };
		// Reserve time to serialize and deliver the error inside the socket deadline.
		dispatch_after(dispatch_time(DISPATCH_TIME_NOW, MAX(0, deadline-editor_context::now()-0.25)*NSEC_PER_SEC), dispatch_get_main_queue(), ^{
			if(finished) return;
			if(cancel) cancel();
			finish(editor_context::failure(@"The language server did not answer before the editor deadline. Try again when indexing has finished.", @"timeout"));
		});
		dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
			NSDictionary* plan = @{};
			if(position) {
				if(arguments[@"line"]) plan = [buffer navigationForArguments:arguments deadline:deadline];
				else {
					NSDictionary* context = [buffer responseForMethod:@"get_editor_context" arguments:@{} base:@{} document:metadata ranges:ng::ranges_t() caret:caret deadline:deadline];
					plan = context[@"error"] ? context : @{ @"range": @{ @"start": context[@"cursor"] } };
				}
			}
			dispatch_async(dispatch_get_main_queue(), ^{
				if(finished) return;
				if(!valid()) { finish(editor_context::failure(@"The document or terminal session changed during the query.", @"document_changed")); return; }
				if(plan[@"error"]) { finish(plan); return; }
				NSDictionary* methods = @{ @"get_definition": @"textDocument/definition", @"find_references": @"textDocument/references", @"get_hover": @"textDocument/hover", @"get_document_symbols": @"textDocument/documentSymbol", @"search_workspace_symbols": @"workspace/symbol" };
				NSMutableDictionary* params = [NSMutableDictionary dictionary];
				if([method isEqual:@"search_workspace_symbols"]) params[@"query"] = arguments[@"query"];
				else params[@"textDocument"] = @{ @"uri": [NSURL fileURLWithPath:document.path].absoluteString };
				if(position) params[@"position"] = @{ @"line": @([plan[@"range"][@"start"][@"line"] unsignedIntegerValue]-1), @"character": @([plan[@"range"][@"start"][@"column"] unsignedIntegerValue]-1) };
				if([method isEqual:@"find_references"]) params[@"context"] = @{ @"includeDeclaration": arguments[@"includeDeclaration"] ?: @NO };
				cancel = [LSPManager.sharedManager requestLanguageQuery:methods[method] forDocument:document params:params completion:^(id raw, NSString* error){
					if(finished) return;
					if(error) {
						NSDictionary* messages = @{ @"server_unavailable": @"No running language server is attached to this document. Configure LSP and open the document normally first.",
							@"server_starting": @"The language server is still starting. Try again once it is ready.",
							@"unsupported_capability": @"This language server does not advertise support for this query using UTF-16 positions.",
							@"language_request_failed": @"The language server rejected the query or stopped before answering. See the LSP log for details." };
						finish(editor_context::failure(messages[error] ?: @"The language query failed.", error)); return;
					}
					dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INITIATED, 0), ^{
						NSDictionary* normalized = EditorLanguageNormalize(method, raw, metadata, deadline);
						dispatch_async(dispatch_get_main_queue(), ^{
							if(finished) return;
							if(!valid()) { finish(editor_context::failure(@"The document or terminal session changed during the query.", @"document_changed")); return; }
							if(normalized[@"error"]) { finish(normalized); return; }
							NSMutableDictionary* result = [normalized mutableCopy];
							result[@"method"] = method;
							result[@"arguments"] = arguments;
							result[@"document"] = metadata;
							result[@"windowId"] = windowId;
							result[@"coordinateEncoding"] = @"utf-16";
							result[@"lineBase"] = @1; result[@"columnBase"] = @1;
							result[@"serverStatus"] = [LSPManager.sharedManager serverStatusForDocument:document] ?: @"unavailable";
							result[@"resultId"] = NSUUID.UUID.UUIDString;
							EditorLanguageIntelligence* owner = weakSelf;
							if(!owner) { finish(editor_context::failure(@"The editor window closed.", @"window_closed")); return; }
							// Any victim will do: a caller that presents an evicted resultId is
							// told result_expired and repeats the query, so identifying the
							// oldest entry would buy nothing but bookkeeping.
							if(owner->_results.count >= 8) [owner->_results removeObjectForKey:owner->_results.allKeys.firstObject];
							owner->_results[result[@"resultId"]] = result;
							finish(Page(result, arguments));
						});
					});
				}];
			});
		});
	}];
}
@end
