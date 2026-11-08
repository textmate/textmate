#import "EditorContextDiagnostics.h"
#import <EditorContextProtocol.h>

static NSString* bounded_string(id value, NSUInteger maximum, BOOL* truncated)
{
    if(![value isKindOfClass:NSString.class]) return nil;
    NSString* text = value;
    if(text.length <= maximum) return text;
    *truncated = YES;
    if(maximum && CFStringIsSurrogateHighCharacter([text characterAtIndex:maximum-1])) --maximum;
    return [text substringToIndex:maximum];
}

static NSDictionary* diagnostic_entry(NSDictionary* diagnostic, NSDictionary* document)
{
    BOOL truncated = NO;
    NSInteger severity = [diagnostic[@"severity"] isKindOfClass:NSNumber.class] ? [diagnostic[@"severity"] integerValue] : 0;
    NSArray* names = @[@"unknown", @"error", @"warning", @"information", @"hint"];
    NSMutableDictionary* result = [@{
        @"severity": names[severity >= 1 && severity <= 4 ? severity : 0],
        @"severityCode": @(severity >= 1 && severity <= 4 ? severity : 0),
        @"message": bounded_string(diagnostic[@"message"], 4096, &truncated) ?: @"Invalid diagnostic message"
    } mutableCopy];
    if(NSString* source = bounded_string(diagnostic[@"source"], 256, &truncated)) result[@"source"] = source;
    id code = diagnostic[@"code"];
    if([code isKindOfClass:NSString.class]) result[@"code"] = bounded_string(code, 256, &truncated);
    else if([code isKindOfClass:NSNumber.class]) result[@"code"] = code;
    bool valid = true;
    for(NSString* key in @[@"line", @"character", @"endLine", @"endCharacter"])
        if(!editor_context::integer(diagnostic[key], 9007199254740990.0)) valid = false;
    if(valid) {
        NSUInteger line = [diagnostic[@"line"] unsignedIntegerValue], column = [diagnostic[@"character"] unsignedIntegerValue];
        NSUInteger endLine = [diagnostic[@"endLine"] unsignedIntegerValue], endColumn = [diagnostic[@"endCharacter"] unsignedIntegerValue];
        valid = endLine > line || (endLine == line && endColumn >= column);
        if(valid) {
            result[@"range"] = @{ @"start": @{ @"line": @(line+1), @"column": @(column+1) },
                                  @"end": @{ @"line": @(endLine+1), @"column": @(endColumn+1) } };
            if([document[@"path"] isKindOfClass:NSString.class]) {
                // Terminal file links use TextMate's native byte columns, while
                // diagnostics use UTF-16. Link to the line without reinterpreting
                // a cached column against a potentially changed buffer.
                NSString* location = [NSString stringWithFormat:@"%@:%lu", document[@"path"], (unsigned long)(line+1)];
                result[@"location"] = bounded_string(location, 2048, &truncated);
            }
        }
    }
    if(!valid) { result[@"range"] = NSNull.null; result[@"invalidRange"] = @YES; }
    result[@"textTruncated"] = @(truncated);
    // Do not export opaque server data used internally for code actions.
    return result;
}

NSDictionary* EditorContextDiagnostics(NSDictionary* report, NSDictionary* base, NSDictionary* document, NSDictionary* arguments, double deadline)
{
    if(editor_context::now() >= deadline) return editor_context::failure(@"The editor request timed out.", @"timeout");
    if(arguments[@"reportId"] && ![arguments[@"reportId"] isEqual:report[@"reportId"]])
        return editor_context::failure(@"The diagnostic report changed. Start again at offset 0 without reportId.", @"diagnostics_changed");
    NSArray* diagnostics = report[@"diagnostics"];
    NSUInteger offset = [arguments[@"offset"] unsignedIntegerValue];
    NSUInteger limit = arguments[@"limit"] ? [arguments[@"limit"] unsignedIntegerValue] : 50;
    if(offset > diagnostics.count) return editor_context::failure(@"offset is beyond the end of the diagnostic report", @"invalid_offset");
    NSMutableArray* page = [NSMutableArray array];
    NSUInteger remaining = 32768;
    BOOL textTruncated = NO;
    for(NSUInteger index = offset; index < diagnostics.count && page.count < limit; ++index) {
        if(editor_context::now() >= deadline) return editor_context::failure(@"The editor request timed out.", @"timeout");
        NSDictionary* raw = [diagnostics[index] isKindOfClass:NSDictionary.class] ? diagnostics[index] : @{};
        NSDictionary* item = diagnostic_entry(raw, document);
        NSUInteger size = 0;
        for(id value in item.allValues) if([value isKindOfClass:NSString.class]) size += [value length];
        if(size > remaining) break;
        remaining -= size;
        textTruncated |= [item[@"textTruncated"] boolValue];
        [page addObject:item];
    }
    NSMutableDictionary* result = [base mutableCopy];
    for(NSString* key in @[@"status", @"serverStatus", @"pendingChanges", @"freshness", @"reportId"])
        result[key] = report[key];
    result[@"document"] = document;
    result[@"diagnostics"] = page;
    result[@"totalCount"] = @(diagnostics.count);
    result[@"offset"] = @(offset);
    result[@"nextOffset"] = @(offset+page.count);
    result[@"truncated"] = @(offset+page.count < diagnostics.count);
    result[@"textTruncated"] = @(textTruncated);
    return result;
}
