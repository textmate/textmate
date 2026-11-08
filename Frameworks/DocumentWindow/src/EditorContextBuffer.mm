#import "EditorContextBuffer.h"
#import "EditorContextService.h"
#import <document/OakDocument Private.h>
#import <EditorContextProtocol.h>
#import <objc/runtime.h>
#include <mutex>
#import <selection/selection.h>
#include <stdexcept>

namespace {
struct context_timeout : std::runtime_error { context_timeout() : std::runtime_error("context deadline") {} };
struct position_t { size_t byte = 0, units = 0, line = 1, column = 1, lineStart = 0; };
struct span_t { size_t byte, length; char const* data; };
struct context_buffer_t : ng::buffer_api_t {
    ng::detail::storage_t storage;
    std::vector<span_t> spans;
    std::vector<position_t> checkpoints;
    std::once_flag indexed;
    position_t finish;
    text::indent_t indentation;
    explicit context_buffer_t(ng::buffer_t const& source) : storage(source.storage()), indentation(source.indent()) {
        size_t offset = 0;
        for(auto const& piece : storage) {
            spans.push_back({offset, piece.size(), piece.data()});
            offset += piece.size();
        }
    }
    // Every walk over the buffer steps one byte at a time, so a stray continuation
    // byte always costs exactly one byte and no UTF-16 unit.
    static void consume(position_t& point, unsigned char byte) {
        ++point.byte;
        if((byte & 0xC0) == 0x80) return;
        if(byte == '\n') { ++point.units; ++point.line; point.column = 1; point.lineStart = point.byte; }
        else { size_t width = byte >= 0xF0 ? 2 : 1; point.units += width; point.column += width; }
    }
    void index(double deadline) {
        std::call_once(indexed, [&]{
            checkpoints.clear();
            checkpoints.push_back({});
            position_t point;
            for(auto const& span : spans) for(size_t i = 0; i < span.length; ++i) {
                if((point.byte & 4095) == 0 && editor_context::now() >= deadline) throw context_timeout();
                unsigned char byte = span.data[i];
                if(point.byte - checkpoints.back().byte >= 4096 && (byte & 0xC0) != 0x80) checkpoints.push_back(point);
                consume(point, byte);
            }
            finish = point;
        });
        if(editor_context::now() >= deadline) throw context_timeout();
    }
    std::string text(size_t from, size_t to) const {
        std::string result;
        result.reserve(to-from);
        if(from == to) return result;
        auto it = std::upper_bound(spans.begin(), spans.end(), from, [](size_t byte, span_t const& span){ return byte < span.byte; });
        if(it != spans.begin()) --it;
        for(; it != spans.end() && it->byte < to; ++it) {
            size_t start = std::max(from, it->byte), stop = std::min(to, it->byte + it->length);
            result.append(it->data + start-it->byte, stop-start);
        }
        return result;
    }
    position_t position(size_t byte) const {
        byte = std::min(byte, finish.byte);
        auto it = std::upper_bound(checkpoints.begin(), checkpoints.end(), byte, [](size_t value, position_t const& point){ return value < point.byte; });
        position_t point = *--it;
        for(unsigned char ch : text(point.byte, byte)) consume(point, ch);
        return point;
    }
    size_t byte_at(size_t units, bool floor = false) const {
        if(units > finish.units) return NSNotFound;
        auto it = std::upper_bound(checkpoints.begin(), checkpoints.end(), units, [](size_t value, position_t const& point){ return value < point.units; });
        position_t point = *--it;
        size_t stop = it+1 == checkpoints.end() ? finish.byte : (it+1)->byte;
        auto bytes = text(point.byte, stop);
        size_t base = point.byte;
        while(point.byte < stop) {
            unsigned char byte = bytes[point.byte - base];
            // Continuation bytes carry no unit of their own, so an offset is only
            // addressable once the walk reaches a byte that starts one.
            if(point.units == units && (byte & 0xC0) != 0x80) break;
            size_t previous = point.byte;
            consume(point, byte);
            if(point.units > units) return floor ? previous : NSNotFound;
        }
        if(point.units == units || floor) return point.byte;
        return NSNotFound;
    }
    size_t size() const override { return finish.byte; }
    size_t lines() const override { return finish.line; }
    text::indent_t indent() const override { return indentation; }
    size_t begin(size_t line) const override {
        size_t target = std::min(line+1, finish.line);
        auto it = std::upper_bound(checkpoints.begin(), checkpoints.end(), target, [](size_t value, position_t const& point){ return value < point.line; });
        position_t point = *--it;
        if(point.line == target) return point.lineStart;
        size_t stop = it+1 == checkpoints.end() ? finish.byte : (it+1)->byte;
        for(unsigned char ch : text(point.byte, stop)) {
            consume(point, ch);
            if(point.line == target) return point.lineStart;
        }
        return finish.lineStart;
    }
    size_t eol(size_t line) const override { return line+1 < lines() ? begin(line+1)-1 : size(); }
    size_t end(size_t line) const override { return line+1 < lines() ? begin(line+1) : size(); }
    text::pos_t convert(size_t byte) const override {
        auto point = position(byte);
        return text::pos_t(point.line-1, point.byte-point.lineStart);
    }
    size_t convert(text::pos_t const& point) const override {
        size_t line = std::min(point.line, lines()-1);
        return std::min(begin(line)+point.column, eol(line));
    }
    unsigned char at(size_t byte) const {
        auto it = std::upper_bound(spans.begin(), spans.end(), byte, [](size_t value, span_t const& span){ return value < span.byte; });
        if(it == spans.begin()) return 0;
        --it;
        return byte < it->byte+it->length ? it->data[byte-it->byte] : 0;
    }
    size_t sanitize_index(size_t byte) const override {
        byte = std::min(byte, size());
        while(byte && byte < size() && (at(byte) & 0xC0) == 0x80) --byte;
        return byte;
    }
    std::string operator[](size_t byte) const override {
        byte = sanitize_index(byte);
        unsigned char ch = at(byte);
        size_t length = ch < 0x80 ? 1 : ch < 0xE0 ? 2 : ch < 0xF0 ? 3 : 4;
        return text(byte, std::min(size(), byte+length));
    }
    std::string substr(size_t from, size_t to) const override { return text(from, to); }
    std::string xml_substr(size_t, size_t) const override { return {}; }
    bool visit_data(std::function<void(char const*, size_t, size_t, bool*)> const& visitor) const override {
        bool stop = false;
        for(auto const& span : spans) { visitor(span.data,span.byte,span.length,&stop); if(stop) break; }
        return stop;
    }
    scope::context_t scope(size_t, bool) const override { return {}; }
    std::map<size_t, scope::scope_t> scopes(size_t, size_t) const override { return {}; }
    NSString* string(size_t from, size_t to) const {
        auto bytes = text(from, to);
        return [[NSString alloc] initWithBytes:bytes.data() length:bytes.size() encoding:NSUTF8StringEncoding] ?: @"";
    }
};
static NSDictionary* position_json(position_t const& point) {
    return @{ @"offset": @(point.units), @"line": @(point.line), @"column": @(point.column) };
}
}

@interface EditorContextBuffer () {
    std::shared_ptr<context_buffer_t> _buffer;
}
@property NSString* revision;
@end
// A snapshot may only carry the revision it actually observed.
static NSString* context_buffer_revision(OakDocument* document) {
    return [EditorContextService.sharedService revisionForDocument:document];
}
static char contextBufferCacheKey;
@implementation EditorContextBuffer
+ (void)invalidateDocument:(OakDocument*)document { objc_setAssociatedObject(document, &contextBufferCacheKey, nil, OBJC_ASSOCIATION_RETAIN_NONATOMIC); }
+ (instancetype)bufferForDocument:(OakDocument*)document revision:(NSString*)revision {
    // A caller with no revision in hand only needs an immutable snapshot, so label
    // it with the state it is copied from.
    if(!revision) revision = context_buffer_revision(document);
    EditorContextBuffer* snapshot = objc_getAssociatedObject(document, &contextBufferCacheKey);
    if(![snapshot.revision isEqual:revision]) {
        // Copying the buffer now observes the document as it is now, so a caller
        // naming a different revision would get current content under a stale
        // label. Refuse instead of returning a snapshot that misdescribes itself.
        if(![context_buffer_revision(document) isEqual:revision]) return nil;
        snapshot = [[self alloc] init];
        snapshot.revision = revision;
        snapshot->_buffer = std::make_shared<context_buffer_t>([document buffer]);
        objc_setAssociatedObject(document, &contextBufferCacheKey, snapshot, OBJC_ASSOCIATION_RETAIN_NONATOMIC);
    }
    return snapshot;
}
- (NSDictionary*)responseForMethod:(NSString*)method arguments:(NSDictionary*)arguments base:(NSDictionary*)base
                         document:(NSDictionary*)document ranges:(ng::ranges_t const&)ranges caret:(NSUInteger)caret deadline:(double)deadline {
    try {
        auto const& buffer = *_buffer;
        _buffer->index(deadline);
        NSMutableDictionary* result = [base mutableCopy];
        result[@"document"] = document;
        if([method isEqual:@"read_document"]) {
            size_t offset = [arguments[@"offset"] unsignedIntegerValue];
            size_t length = arguments[@"length"] ? [arguments[@"length"] unsignedIntegerValue] : 16384;
            if(offset > buffer.finish.units) return editor_context::failure(@"offset is beyond the end of the document", @"invalid_offset");
            size_t from = buffer.byte_at(offset);
            if(from == NSNotFound) return editor_context::failure(@"offset splits a UTF-16 surrogate pair", @"invalid_offset");
            size_t to = buffer.byte_at(offset + std::min(length, buffer.finish.units-offset), true);
            result[@"offset"] = @(offset);
            result[@"text"] = buffer.string(from, to);
            result[@"totalLength"] = @(buffer.finish.units);
            result[@"nextOffset"] = @(buffer.position(to).units);
            result[@"truncated"] = @(to < buffer.finish.byte);
            return result;
        }
        NSMutableArray* selections = [NSMutableArray array];
        size_t remaining = 8192;
        auto expanded = ng::dissect_columnar(buffer, ranges, 129);
        for(auto const& range : expanded) {
            if(editor_context::now() >= deadline) throw context_timeout();
            if(selections.count == 128) break;
            auto start = buffer.position(range.min().index), end = buffer.position(range.max().index);
            size_t to = buffer.byte_at(start.units + std::min(remaining, end.units-start.units), true);
            NSString* text = buffer.string(start.byte, to);
            remaining -= text.length;
            [selections addObject:@{ @"start": position_json(start), @"end": position_json(end), @"text": text,
                                     @"truncated": @(to < end.byte) }];
        }
        result[@"selections"] = selections;
        result[@"selectionsTruncated"] = @(expanded.size() > selections.count);
        auto cursor = buffer.position(caret);
        result[@"cursor"] = position_json(cursor);
        size_t start = buffer.byte_at(cursor.units > 1024 ? cursor.units-1024 : 0, true);
        size_t end = buffer.byte_at(std::min(buffer.finish.units, cursor.units+1024), true);
        result[@"surroundingText"] = @{ @"offset": @(buffer.position(start).units), @"text": buffer.string(start,end),
                                        @"truncated": @(start > 0 || end < buffer.finish.byte) };
        if(editor_context::now() >= deadline) throw context_timeout();
        return result;
    }
    catch(context_timeout const&) { return editor_context::failure(@"The editor request timed out.", @"timeout"); }
}

- (NSDictionary*)editPlanForArguments:(NSDictionary*)arguments document:(NSDictionary*)document deadline:(double)deadline
{
    if(NSString* error = editor_context::validate_arguments(@"propose_document_edits", arguments)) return editor_context::failure(error, @"invalid_arguments");
    try {
        _buffer->index(deadline);
        auto const& buffer = *_buffer;
        NSMutableArray* edits = [NSMutableArray array];
        std::vector<std::pair<size_t, size_t>> ranges;
        for(NSDictionary* edit in arguments[@"edits"]) {
            if(editor_context::now() >= deadline) throw context_timeout();
            size_t offset = [edit[@"offset"] unsignedIntegerValue], length = [edit[@"length"] unsignedIntegerValue];
            if(offset > buffer.finish.units || length > buffer.finish.units-offset)
                return editor_context::failure(@"An edit range is beyond the end of the buffer.", @"invalid_range");
            size_t start = buffer.byte_at(offset), end = buffer.byte_at(offset+length);
            if(start == NSNotFound || end == NSNotFound)
                return editor_context::failure(@"An edit boundary splits a UTF-16 surrogate pair.", @"invalid_range");
            ranges.emplace_back(start, end);
            [edits addObject:@{ @"startByte": @(start), @"endByte": @(end), @"start": position_json(buffer.position(start)),
                @"text": buffer.string(start, end), @"replacement": edit[@"text"] }];
        }
        std::sort(ranges.begin(), ranges.end());
        for(size_t i = 1; i < ranges.size(); ++i)
            if(ranges[i].first < ranges[i-1].second || ranges[i].first == ranges[i-1].first)
                return editor_context::failure(@"Edit ranges must not overlap or share a start offset. Combine insertions at the same offset into one edit.", @"overlapping_edits");
        return @{ @"document": document, @"edits": edits };
    }
    catch(context_timeout const&) { return editor_context::failure(@"The editor request timed out.", @"timeout"); }
}

- (NSDictionary*)navigationForArguments:(NSDictionary*)arguments deadline:(double)deadline
{
    try {
        auto const& buffer = *_buffer;
        _buffer->index(deadline);
        NSUInteger line = [arguments[@"line"] unsignedIntegerValue];
        NSUInteger column = arguments[@"column"] ? [arguments[@"column"] unsignedIntegerValue] : 1;
        NSUInteger endLine = arguments[@"endLine"] ? [arguments[@"endLine"] unsignedIntegerValue] : line;
        NSUInteger endColumn = arguments[@"endColumn"] ? [arguments[@"endColumn"] unsignedIntegerValue] : column;
        auto byteForPosition = [&](NSUInteger row, NSUInteger col) -> size_t {
            if(!row || row > buffer.lines() || !col) return NSNotFound;
            auto begin = buffer.position(buffer.begin(row-1)), end = buffer.position(buffer.eol(row-1));
            if(col-1 > end.units-begin.units) return NSNotFound;
            return buffer.byte_at(begin.units+col-1);
        };
        size_t start = byteForPosition(line, column), end = byteForPosition(endLine, endColumn);
        if(start == NSNotFound || end == NSNotFound || end < start)
            return editor_context::failure(@"The requested position is outside the buffer or inside a UTF-16 surrogate pair.", @"invalid_position");
        auto nativeStart = buffer.convert(start), nativeEnd = buffer.convert(end);
        NSString* selection = [NSString stringWithFormat:@"%zu:%zu-%zu:%zu", nativeStart.line+1, nativeStart.column+1, nativeEnd.line+1, nativeEnd.column+1];
        return @{ @"selectionString": selection,
                  @"range": @{ @"start": position_json(buffer.position(start)), @"end": position_json(buffer.position(end)) } };
    }
    catch(context_timeout const&) { return editor_context::failure(@"The editor request timed out.", @"timeout"); }
}
@end
