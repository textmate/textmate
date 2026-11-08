#pragma once
#import <EditorContextConnection.h>
#include <condition_variable>
#include <functional>
#include <mutex>
#include <thread>

static NSString* const EditorContextResourceURI = @"textmate://editor/context";

// Poll only an opaque, constant-size token, and only while subscribed. Hosts
// decide when to fetch context or wake a model after a standard invalidation.
class ResourceSubscriptions {
    std::mutex mutex;
    std::condition_variable changed;
    bool enabled = false, stopping = false;
    size_t generation = 0;
    std::function<bool(NSDictionary*)> send;
    std::thread worker;
    void run() {
        std::unique_lock lock(mutex);
        NSString* previous = nil;
        size_t observed = 0;
        while(!stopping) {
            changed.wait(lock, [&]{ return stopping || enabled; });
            if(stopping) break;
            size_t currentGeneration = generation;
            if(observed != currentGeneration) { previous = nil; observed = currentGeneration; }
            lock.unlock();
            @autoreleasepool {
                NSDictionary* value = editor_context::query(@"get_editor_change_token", @{});
                NSString* token = [value[@"cursor"] isKindOfClass:NSString.class] ? [@"cursor:" stringByAppendingString:value[@"cursor"]] :
                    [@"error:" stringByAppendingString:[value[@"code"] isKindOfClass:NSString.class] ? value[@"code"] : @"invalid_response"];
                lock.lock();
                if(enabled && !stopping && generation == currentGeneration && ![previous isEqual:token]) {
                    previous = token;
                    // Hold the subscription lock through output: unsubscribe's
                    // acknowledgement cannot be followed by a stale update.
                    if(!send(@{ @"jsonrpc": @"2.0", @"method": @"notifications/resources/updated",
                                @"params": @{ @"uri": EditorContextResourceURI } })) enabled = false;
                }
            }
            changed.wait_for(lock, std::chrono::milliseconds(500), [&]{ return stopping || !enabled || generation != currentGeneration; });
        }
    }
public:
    explicit ResourceSubscriptions(std::function<bool(NSDictionary*)> output) : send(output), worker([this]{ run(); }) { }
    ~ResourceSubscriptions() { stop(); }
    void setEnabled(bool value) {
        std::lock_guard lock(mutex);
        if(enabled == value) return;
        enabled = value;
        ++generation;
        changed.notify_all();
    }
    void stop() {
        { std::lock_guard lock(mutex); stopping = true; changed.notify_all(); }
        if(worker.joinable()) worker.join();
    }
};
