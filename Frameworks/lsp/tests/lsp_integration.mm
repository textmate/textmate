// Headless integration test for the LSP pull-diagnostics client.
//
// This is a standalone program (not a textmate_add_tests target): the generic
// test runner executes test_* on a background dispatch queue, while LSPClient
// asserts NSThread.isMainThread and dispatches its callbacks to the main queue.
// Everything here therefore runs on the main thread and drives the run loop
// explicitly.

#import <Foundation/Foundation.h>
#import <CoreFoundation/CoreFoundation.h>
#import <lsp/LSPClient.h>

#include <cstdio>
#include <string>

static NSString* const kFixtureDirectory = @"" LSP_TEST_FIXTURE_DIR;

static std::string ToStd (NSString* str)
{
	return str ? std::string(str.UTF8String) : std::string("(nil)");
}

static void PumpRunLoop (NSTimeInterval seconds)
{
	NSDate* deadline = [NSDate dateWithTimeIntervalSinceNow:seconds];
	while([deadline timeIntervalSinceNow] > 0)
		CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.02, false);
}

// Runs the main run loop until predicate returns YES or the timeout expires.
// Every wait in this file goes through here, so no scenario can hang.
static BOOL WaitUntil (NSTimeInterval timeout, BOOL (^predicate)(void))
{
	NSDate* deadline = [NSDate dateWithTimeIntervalSinceNow:timeout];
	while(true)
	{
		if(predicate())
			return YES;
		if([deadline timeIntervalSinceNow] <= 0)
			return NO;
		CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0.02, false);
	}
}

static NSString* ReadTextFile (NSString* path)
{
	NSString* str = [NSString stringWithContentsOfFile:path encoding:NSUTF8StringEncoding error:NULL];
	if(!str.length)
		return nil;
	return [str stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
}

// Parses "SCENARIO <id> PASS|FAIL <detail>" into {scenario,status,detail}.
static NSDictionary* ParseVerdict (NSString* line)
{
	NSArray<NSString*>* tokens = [line componentsSeparatedByString:@" "];
	if(tokens.count < 3 || ![tokens[0] isEqualToString:@"SCENARIO"])
		return nil;

	NSString* detail = tokens.count > 3 ? [[tokens subarrayWithRange:NSMakeRange(3, tokens.count - 3)] componentsJoinedByString:@" "] : @"";
	return @{ @"scenario": tokens[1], @"status": tokens[2], @"detail": detail };
}

// MARK: - Delegate

@interface LSPTestDelegate : NSObject <LSPClientDelegate>
@property (nonatomic) NSUInteger diagnosticsCallbackCount;
@property (nonatomic) NSUInteger lastDiagnosticItemCount;
@property (nonatomic) NSString* lastDiagnosticsURI;
@end

@implementation LSPTestDelegate
- (void)lspClient:(LSPClient*)client didReceiveDiagnostics:(NSArray<NSDictionary*>*)diagnostics forDocumentURI:(NSString*)uri
{
	self.diagnosticsCallbackCount      = self.diagnosticsCallbackCount + 1;
	self.lastDiagnosticItemCount       = diagnostics.count;
	self.lastDiagnosticsURI            = uri;
}
@end

// MARK: - Harness

@interface LSPTestHarness : NSObject
@property (nonatomic, readonly) NSString* directory;
@property (nonatomic, readonly) NSString* resultPath;
@property (nonatomic, readonly) LSPClient* client;
@property (nonatomic, readonly) LSPTestDelegate* delegate;
@property (nonatomic, readonly) OakDocument* document;
@property (nonatomic, readonly) NSString* documentURI;
@property (nonatomic, readonly) NSString* failureReason;
@end

@implementation LSPTestHarness
{
	BOOL _finished;
}

- (instancetype)initWithFixture:(NSString*)fixture scenario:(NSString*)scenario
{
	if(self = [super init])
	{
		_directory = [NSTemporaryDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"lsp_integration_%@_%@", scenario, NSUUID.UUID.UUIDString]];
		[[NSFileManager defaultManager] createDirectoryAtPath:_directory withIntermediateDirectories:YES attributes:nil error:NULL];
		_resultPath = [_directory stringByAppendingPathComponent:@"result.txt"];

		_delegate = [LSPTestDelegate new];
		_client = [[LSPClient alloc] initWithCommand:@"/usr/bin/python3"
		                                  arguments:@[ fixture, scenario, _resultPath ]
		                           workingDirectory:_directory
		                                initOptions:nil];
		_client.delegate = _delegate;
	}
	return self;
}

- (void)dealloc
{
	if(!_finished && _client)
		[_client shutdown];
}

// Waits for initialization, creates a document backed by a real temp file and
// opens it in the client.
- (BOOL)start
{
	if(!_client)
		return [self failWith:@"LSPClient failed to launch /usr/bin/python3"];

	if(!WaitUntil(10.0, ^{ return _client.initialized; }))
		return [self failWith:@"client did not initialize within 10s"];

	if(![self createDocument])
		return NO;

	[_client openDocument:_document languageId:@"typescript"];
	return YES;
}

- (BOOL)failWith:(NSString*)reason
{
	_failureReason = reason;
	return NO;
}

- (BOOL)createDocument
{
	NSString* path    = [_directory stringByAppendingPathComponent:@"case.ts"];
	NSString* content = @"const n: number = 1;\n";

	OakDocument* document = [OakDocument documentWithString:content fileType:@"source.ts" customName:nil];
	if(!document.content)
	{
		// +documentWithString: keeps the text in the buffer; if that ever
		// changes, load an actual file instead of silently testing a nil doc.
		[content writeToFile:path atomically:YES encoding:NSUTF8StringEncoding error:NULL];
		document = [OakDocument documentWithPath:path];
		if(!WaitUntil(10.0, ^{ return document.isLoaded; }))
			return [self failWith:@"document did not load within 10s"];
	}

	document.path = path;
	if(![document.content isEqualToString:content])
		return [self failWith:@"OakDocument content does not match the written text"];

	_document = document;
	// document.path is resolved by OakDocument, so derive the URI from it.
	_documentURI = [NSURL fileURLWithPath:document.path].absoluteString;
	return YES;
}

- (void)changeDocumentContent:(NSString*)content version:(int)version
{
	_document.content = content;
	[_client documentDidChange:_document version:version];
}

// Returns nil when the server has not written a verdict within the timeout.
- (NSDictionary*)waitForVerdict:(NSTimeInterval)timeout
{
	__block NSDictionary* verdict = nil;
	WaitUntil(timeout, ^{
		verdict = ParseVerdict(ReadTextFile(_resultPath));
		return verdict != nil;
	});
	return verdict;
}

- (void)finish
{
	if(_finished)
		return;
	_finished = YES;

	_client.delegate = nil;
	[_client shutdown];
	WaitUntil(5.0, ^{ return !_client.running; });
	_client = nil;

	[[NSFileManager defaultManager] removeItemAtPath:_directory error:NULL];
}
@end

// MARK: - Scenarios

struct TestOutcome
{
	bool passed = false;
	std::string detail;
};

static std::string DescribeVerdict (NSDictionary* verdict, NSString* uri, LSPTestDelegate* delegate)
{
	if(!verdict)
		return "no server verdict within timeout";

	std::string result = ToStd([NSString stringWithFormat:@"verdict=%@ (%@) callbacks=%lu",
		verdict[@"status"], verdict[@"detail"], (unsigned long)delegate.diagnosticsCallbackCount]);
	if(delegate.diagnosticsCallbackCount)
	{
		result += ToStd([NSString stringWithFormat:@" items=%ld uri=%@",
			(long)delegate.lastDiagnosticItemCount,
			[uri isEqualToString:delegate.lastDiagnosticsURI] ? @"match" : @"MISMATCH"]);
	}
	return result;
}

static TestOutcome RunF0Register (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f0_register"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	NSDictionary* verdict = [harness waitForVerdict:10.0];
	outcome.passed = [verdict[@"status"] isEqual:@"PASS"];
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, harness.delegate);

	[harness finish];
	return outcome;
}

static TestOutcome RunF1FullEmpty (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f1_full_empty"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	LSPTestDelegate* delegate = harness.delegate;

	// The didOpen pull must deliver a (full) report before the edit, otherwise
	// we cannot tell the open-time callback from the post-edit one.
	BOOL sawOpenReport = WaitUntil(10.0, ^{ return delegate.diagnosticsCallbackCount >= 1; });

	[harness changeDocumentContent:@"const n: number = 2;\n" version:2];
	BOOL sawEditReport = WaitUntil(10.0, ^{ return delegate.diagnosticsCallbackCount >= 2; });

	NSDictionary* verdict = [harness waitForVerdict:10.0];

	// Exactly two reports: one for the didOpen pull, one for the post-edit pull.
	outcome.passed = sawOpenReport && sawEditReport && delegate.diagnosticsCallbackCount == 2 &&
		delegate.lastDiagnosticItemCount == 0 &&
		[delegate.lastDiagnosticsURI isEqualToString:harness.documentURI] &&
		[verdict[@"status"] isEqual:@"PASS"];
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, delegate) +
		(sawOpenReport && sawEditReport && delegate.lastDiagnosticItemCount == 0 ? "" : " (missing empty-report callback)");

	[harness finish];
	return outcome;
}

static TestOutcome RunF2Unchanged (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f2_unchanged"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	// Let the didOpen pull complete first: an edit inside the 250 ms debounce
	// window is coalesced into the same pull, and f2 needs two separate pulls
	// to distinguish "unchanged honoured" from "stopped pulling".
	PumpRunLoop(0.8);

	[harness changeDocumentContent:@"const n: number = 2;\n" version:2];
	NSDictionary* verdict = [harness waitForVerdict:10.0];

	// Give a late `full` report time to show up before asserting the no-op.
	PumpRunLoop(0.5);

	LSPTestDelegate* delegate = harness.delegate;
	outcome.passed = [verdict[@"status"] isEqual:@"PASS"] && delegate.diagnosticsCallbackCount == 0;
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, delegate) +
		(delegate.diagnosticsCallbackCount == 0 ? "" : " (unchanged report produced a callback)");

	[harness finish];
	return outcome;
}

static TestOutcome RunF3Supersede (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f3_supersede"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	// Let the didOpen pull reach the server; it holds that response so the next
	// burst has to supersede and cancel it.
	PumpRunLoop(0.8);

	[harness changeDocumentContent:@"const n: number = 2;\n" version:2];
	PumpRunLoop(1.0);
	[harness changeDocumentContent:@"const n: number = 3;\n" version:3];

	NSDictionary* verdict = [harness waitForVerdict:15.0];
	LSPTestDelegate* delegate = harness.delegate;

	BOOL sawReport = WaitUntil(10.0, ^{ return delegate.diagnosticsCallbackCount >= 1; });
	outcome.passed = [verdict[@"status"] isEqual:@"PASS"] && sawReport;
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, delegate) +
		(sawReport ? "" : " (no report from the superseding request)");

	[harness finish];
	return outcome;
}

static TestOutcome RunF4ServerCancel (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f4_servercancel"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	NSDictionary* verdict = [harness waitForVerdict:10.0];
	PumpRunLoop(0.5);

	LSPTestDelegate* delegate = harness.delegate;
	outcome.passed = [verdict[@"status"] isEqual:@"PASS"] && delegate.diagnosticsCallbackCount == 0;
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, delegate) +
		(delegate.diagnosticsCallbackCount == 0 ? "" : " (error response produced a callback)");

	[harness finish];
	return outcome;
}

static TestOutcome RunF6Coalesce (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f6_coalesce"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	LSPTestDelegate* delegate = harness.delegate;

	// Wait for the didOpen pull to be answered: only requests after the edit
	// count towards the burst.
	BOOL sawOpenReport = WaitUntil(10.0, ^{ return delegate.diagnosticsCallbackCount >= 1; });

	[harness changeDocumentContent:@"const n: number = 2;\n" version:2];

	// The server evaluates the burst 1.5s after the last diagnostic request.
	NSDictionary* verdict = [harness waitForVerdict:15.0];
	BOOL sawBurstReport = WaitUntil(5.0, ^{ return delegate.diagnosticsCallbackCount >= 2; });

	outcome.passed = sawOpenReport && [verdict[@"status"] isEqual:@"PASS"] && sawBurstReport &&
		delegate.diagnosticsCallbackCount == 2;
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, delegate) +
		(sawBurstReport ? "" : " (no report for the burst)");

	[harness finish];
	return outcome;
}

static TestOutcome RunF7NoProvider (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f7_no_provider"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	[harness changeDocumentContent:@"const n: number = 2;\n" version:2];

	// The server decides PASS 5s after didChange if it saw no pull.
	NSDictionary* verdict = [harness waitForVerdict:15.0];
	PumpRunLoop(0.3);

	LSPTestDelegate* delegate = harness.delegate;
	outcome.passed = [verdict[@"status"] isEqual:@"PASS"] && delegate.diagnosticsCallbackCount == 0;
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, delegate) +
		(delegate.diagnosticsCallbackCount == 0 ? "" : " (pull without diagnosticProvider)");

	[harness finish];
	return outcome;
}

static TestOutcome RunF8Close (NSString* fixture)
{
	TestOutcome outcome;
	LSPTestHarness* harness = [[LSPTestHarness alloc] initWithFixture:fixture scenario:@"f8_close"];
	if(![harness start])
	{
		outcome.detail = ToStd(harness.failureReason);
		[harness finish];
		return outcome;
	}

	LSPTestDelegate* delegate = harness.delegate;

	BOOL sawOpenReport  = WaitUntil(10.0, ^{ return delegate.diagnosticsCallbackCount >= 1; });
	[harness changeDocumentContent:@"const n: number = 2;\n" version:2];
	BOOL sawEditReport  = WaitUntil(10.0, ^{ return delegate.diagnosticsCallbackCount >= 2; });

	[harness.client closeDocument:harness.document];
	NSUInteger callbacksAtClose = delegate.diagnosticsCallbackCount;

	// The server decides PASS 6s after didClose if it saw no pull afterwards.
	NSDictionary* verdict = [harness waitForVerdict:15.0];
	PumpRunLoop(0.5);

	outcome.passed = sawOpenReport && sawEditReport && callbacksAtClose == 2 &&
		[verdict[@"status"] isEqual:@"PASS"] && delegate.diagnosticsCallbackCount == callbacksAtClose;
	outcome.detail = DescribeVerdict(verdict, harness.documentURI, delegate) +
		(delegate.diagnosticsCallbackCount == callbacksAtClose ? "" : " (diagnostics after close)");

	[harness finish];
	return outcome;
}

// MARK: - main

int main ()
{
	int failures = 0;

	@autoreleasepool
	{
		NSString* fixture = [kFixtureDirectory stringByAppendingPathComponent:@"fake_lsp.py"];
		if(![[NSFileManager defaultManager] fileExistsAtPath:fixture])
		{
			printf("lsp_integration: FAIL fixture not found at %s\n", fixture.UTF8String);
			return 1;
		}

		struct { char const* name; TestOutcome (*run)(NSString*); } const cases[] = {
			{ "f0_register",    RunF0Register    },
			{ "f1_full_empty",  RunF1FullEmpty   },
			{ "f2_unchanged",   RunF2Unchanged   },
			{ "f3_supersede",   RunF3Supersede   },
			{ "f4_servercancel", RunF4ServerCancel },
			{ "f6_coalesce",    RunF6Coalesce    },
			{ "f7_no_provider", RunF7NoProvider  },
			{ "f8_close",       RunF8Close       },
		};

		for(auto const& test : cases)
		{
			TestOutcome outcome = test.run(fixture);
			printf("%-16s %s  %s\n", test.name, outcome.passed ? "PASS" : "FAIL", outcome.detail.c_str());
			fflush(stdout);
			if(!outcome.passed)
				++failures;
		}

		printf("lsp_integration: %d scenario%s failed\n", failures, failures == 1 ? "" : "s");
		fflush(stdout);
	}

	return failures == 0 ? 0 : 1;
}
