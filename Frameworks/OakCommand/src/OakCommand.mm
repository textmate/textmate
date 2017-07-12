#import "OakCommand.h"
#import <document/OakDocument.h>
#import <document/OakDocumentController.h>
#import <oak/datatypes.h>
#import <cf/cf.h>
#import <ns/ns.h>
#import <io/environment.h>
#import <io/pipe.h>
#import <text/tokenize.h>
#import <text/trim.h>
#import <command/runner.h> // bundle_command_t, fix_shebang
#import <bundles/wrappers.h>
#import <regexp/format_string.h>
#import <OakAppKit/OakToolTip.h>
#import <HTMLOutput/HTMLOutput.h>
#import <HTMLOutputWindow/HTMLOutputWindow.h>
#import <OakSystem/process.h>
#import <settings/settings.h>
#import <BundleEditor/BundleEditor.h>

NSString* const OakCommandDidTerminateNotification = @"OakCommandDidTerminateNotification";
NSString* const OakCommandErrorDomain              = @"com.macromates.TextMate.ErrorDomain";

static NSString* const kOakFileHandleURLScheme = @"x-txmt-filehandle";

@protocol OakCommandDelegate
- (void)updateEnvironment:(std::map<std::string, std::string>&)res forCommand:(OakCommand*)aCommand;
- (void)saveAllEditedDocuments:(BOOL)includeAllFlag completionHandler:(void(^)(BOOL didSave))callback;

- (OakHTMLOutputView*)htmlOutputView:(BOOL)createFlag forIdentifier:(NSUUID*)identifier;
- (void)discardHTMLOutputView:(OakHTMLOutputView*)htmlOutputView;

- (void)showToolTip:(NSString*)aToolTip;
- (void)showDocument:(OakDocument*)aDocument;

// Missing requirements and execution failure.
- (BOOL)presentError:(NSError*)anError;
@end

static std::tuple<pid_t, int, int> my_fork (char const* cmd, int inputRead, std::map<std::string, std::string> const& environment, char const* workingDir)
{
	for(auto const& pair : environment)
	{
		if(pair.first.size() + pair.second.size() + 2 < ARG_MAX)
			continue;

		std::map<std::string, std::string> newEnv;
		for(auto const& pair : environment)
		{
			if(pair.first.size() + pair.second.size() + 2 < ARG_MAX)
			{
				newEnv.insert(pair);
			}
			else
			{
				newEnv.emplace(pair.first, "(truncated)");
				fprintf(stderr, "*** variable exceeds ARG_MAX: %s\n", pair.first.c_str());
			}
		}
		return my_fork(cmd, inputRead, newEnv, workingDir);
	}

	int outputRead, outputWrite, errorRead, errorWrite;
	std::tie(outputRead, outputWrite) = io::create_pipe();
	std::tie(errorRead,  errorWrite)  = io::create_pipe();

	oak::c_array env(environment);

	pid_t pid = vfork();
	if(pid == 0)
	{
		int const signals[] = { SIGINT, SIGTERM, SIGPIPE, SIGUSR1 };
		for(int sig : signals) signal(sig, SIG_DFL);

		int const oldOutErr[] = { STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO };
		int const newOutErr[] = { inputRead, outputWrite, errorWrite };

		for(int fd = getdtablesize(); --fd > STDERR_FILENO; )
		{
			int flags = fcntl(fd, F_GETFD);
			if((flags == -1 && errno == EBADF) || (flags & FD_CLOEXEC) == FD_CLOEXEC)
				continue;

			if(close(fd) == -1)
			{
				perror("close");
				_exit(EXIT_FAILURE);
			}
		}

		for(int fd : oldOutErr) close(fd);
		for(int fd : newOutErr) dup(fd);

		setpgid(0, getpid());
		chdir(workingDir);

		char* argv[] = { (char*)cmd, NULL };
		execve(argv[0], argv, env);
		perror("execve");
		_exit(EXIT_FAILURE);
	}

	int const fds[] = { outputWrite, errorWrite };
	for(int fd : fds) close(fd);

	return { pid, outputRead, errorRead };
}

static void exhaust_fd_in_queue (dispatch_group_t group, int fd, CFRunLoopRef runLoop, void(^handler)(char const* bytes, size_t len))
{
	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		char buf[8192];
		ssize_t len = 0;
		while((len = read(fd, buf, sizeof(buf))) > 0)
		{
			dispatch_semaphore_t sem = dispatch_semaphore_create(0);
			char const* bytes = buf;
			CFRunLoopPerformBlock(runLoop, kCFRunLoopCommonModes, ^{
				handler(bytes, len);
				dispatch_semaphore_signal(sem);
			});
			CFRunLoopWakeUp(runLoop);
			dispatch_semaphore_wait(sem, DISPATCH_TIME_FOREVER);
		}
		if(len == -1)
			perror("OakCommand: read");
		close(fd);
	});
}

static pid_t run_command (dispatch_group_t rootGroup, std::string const& cmd, int inputFd, std::map<std::string, std::string> const& env, std::string const& cwd, CFRunLoopRef runLoop, void(^stdoutHandler)(char const* bytes, size_t len), void(^stderrHandler)(char const* bytes, size_t len), void(^completionHandler)(int status))
{
	pid_t pid;
	int outputFd, errorFd;
	std::tie(pid, outputFd, errorFd) = my_fork(cmd.c_str(), inputFd, env, cwd.c_str());

	dispatch_group_t group = dispatch_group_create();
	exhaust_fd_in_queue(group, outputFd, runLoop, stdoutHandler);
	exhaust_fd_in_queue(group, errorFd, runLoop, stderrHandler);

	__block int status = 0;
	dispatch_group_async(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		if(waitpid(pid, &status, 0) != pid)
			perror("OakCommand: waitpid");
	});

	dispatch_group_enter(rootGroup);
	dispatch_group_notify(group, dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		CFRunLoopPerformBlock(runLoop, kCFRunLoopCommonModes, ^{
			completionHandler(status);
		});
		CFRunLoopWakeUp(runLoop);
		dispatch_group_leave(rootGroup);
	});

	return pid;
}

@interface OakCommand ()
{
	bundle_command_t _bundleCommand;

	dispatch_group_t _dispatchGroup;
	std::map<std::string, std::string> _environment;
	pid_t _processIdentifier;

	BOOL _didCheckRequirements;
	BOOL _didSaveChanges;
	BOOL _didFindHTMLOutputView;

	OakHTMLOutputView* _htmlOutputView;
	NSMutableURLRequest* _urlRequest;
	NSFileHandle* _fileHandleForWritingHTML;
	dispatch_queue_t _queueForWritingHTML;
	NSMutableData* _htmlData;
	HTMLOutputWindowController* _htmlOutputWindowController;

	BOOL _userDidAbort;
}
@end

@implementation OakCommand
- (instancetype)initWithBundleCommand:(bundle_command_t const&)aCommand
{
	if(self = [super init])
	{
		_bundleCommand = aCommand;
		command::fix_shebang(&_bundleCommand.command);
	}
	return self;
}

- (NSUUID*)identifier
{
	return _bundleCommand.uuid ? [[NSUUID alloc] initWithUUIDString:to_ns(_bundleCommand.uuid)] : nil;
}

- (void)writeHTMLOutput:(char const*)bytes length:(size_t)len
{
	if(bytes && !_htmlOutputView)
		_htmlOutputView = [self htmlOutputView:YES forIdentifier:self.identifier];

	if(_updateHTMLViewAtomically)
	{
		if(bytes)
		{
			if(!_htmlData)
				_htmlData = [NSMutableData dataWithCapacity:len];
			[_htmlData appendBytes:bytes length:len];
		}
		else
		{
			[_htmlOutputView setContent:[[NSString alloc] initWithData:_htmlData encoding:NSUTF8StringEncoding]];
			_htmlData = nil;
		}
		return;
	}

	if(bytes)
	{
		if(!_fileHandleForWritingHTML)
		{
			NSPipe* pipe = [NSPipe pipe];
			_fileHandleForWritingHTML = pipe.fileHandleForWriting;
			_queueForWritingHTML = dispatch_queue_create("org.textmate.write-html", DISPATCH_QUEUE_SERIAL);

			static NSInteger UniqueKey = 0; // Make each URL unique to avoid caching

			_urlRequest = [NSMutableURLRequest requestWithURL:[NSURL URLWithString:[NSString stringWithFormat:@"%@://job/%@/%ld", kOakFileHandleURLScheme, [to_ns(_bundleCommand.name) stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding], ++UniqueKey]] cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:6000];
			[NSURLProtocol setProperty:self.identifier forKey:@"commandIdentifier" inRequest:_urlRequest];
			[NSURLProtocol setProperty:pipe.fileHandleForReading forKey:@"fileHandle" inRequest:_urlRequest];
			[NSURLProtocol setProperty:@(_processIdentifier) forKey:@"processIdentifier" inRequest:_urlRequest];
			[NSURLProtocol setProperty:to_ns(_bundleCommand.name) forKey:@"processName" inRequest:_urlRequest];
			[NSURLProtocol setProperty:self forKey:@"command" inRequest:_urlRequest];

			_htmlOutputView.disableJavaScriptAPI = _bundleCommand.disable_javascript_api;
			[_htmlOutputView loadRequest:_urlRequest environment:_environment autoScrolls:_bundleCommand.auto_scroll_output];
		}

		NSData* data = [NSData dataWithBytes:bytes length:len];
		NSFileHandle* fh = _fileHandleForWritingHTML;
		dispatch_async(_queueForWritingHTML, ^{
			ssize_t bytesWritten = write(fh.fileDescriptor, [data bytes], [data length]);
			if(bytesWritten == -1)
				perror("HTMLOutput: write");
		});
	}
	else
	{
		if(NSFileHandle* fh = std::exchange(_fileHandleForWritingHTML, nil))
		{
			dispatch_async(_queueForWritingHTML, ^{
				[fh closeFile];
			});
			_queueForWritingHTML = nil;
		}

		if(NSMutableURLRequest* request = std::exchange(_urlRequest, nil))
		{
			[NSURLProtocol removePropertyForKey:@"command" inRequest:request];
			[NSURLProtocol removePropertyForKey:@"fileHandle" inRequest:request];
			[NSURLProtocol removePropertyForKey:@"processIdentifier" inRequest:request];
		}
	}
}

- (void)executeWithInput:(NSFileHandle*)fileHandleForReading variables:(std::map<std::string, std::string> const&)someVariables outputHandler:(void(^)(std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, std::map<std::string, std::string> const& environment))handler
{
	_dispatchGroup     = dispatch_group_create();
	_processIdentifier = 0;
	_didSaveChanges    = NO;

	_environment = someVariables;
	_environment << oak::basic_environment();
	[self updateEnvironment:_environment];

	[self executeWithInput:(fileHandleForReading ?: [[NSFileHandle alloc] initWithFileDescriptor:open("/dev/null", O_RDONLY|O_CLOEXEC) closeOnDealloc:YES]) outputHandler:handler];
}

- (void)executeWithInput:(NSFileHandle*)inputFH outputHandler:(void(^)(std::string const& out, output::type placement, output_format::type format, output_caret::type outputCaret, std::map<std::string, std::string> const& environment))handler
{
	if(_didCheckRequirements == NO)
	{
		bundles::required_command_t failedRequirement;
		bundles::item_ptr item = bundles::lookup(_bundleCommand.uuid);
		if(item && bundles::missing_requirement(item, _environment, &failedRequirement))
		{
			std::vector<std::string> paths;
			std::string const tmp = _environment["PATH"];
			for(auto path : text::tokenize(tmp.begin(), tmp.end(), ':'))
			{
				if(path != "" && path::is_directory(path))
					paths.push_back(path::with_tilde(path));
			}

			std::string message;
			if(failedRequirement.variable != NULL_STR)
					message = text::format("This command requires ‘%1$s’ which wasn’t found on your system.\n\nThe following locations were searched:%2$s\n\nIf ‘%1$s’ is installed elsewhere then you need to set %3$s in Preferences → Variables to the full path of where you installed it.", failedRequirement.command.c_str(), ("\n\u2003• " + text::join(paths, "\n\u2003• ")).c_str(), failedRequirement.variable.c_str());
			else	message = text::format("This command requires ‘%1$s’ which wasn’t found on your system.\n\nThe following locations were searched:%2$s\n\nIf ‘%1$s’ is installed elsewhere then you need to set PATH in Preferences → Variables to include the folder in which it can be found.", failedRequirement.command.c_str(), ("\n\u2003• " + text::join(paths, "\n\u2003• ")).c_str());

			NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:@{
				NSLocalizedDescriptionKey             : [NSString stringWithFormat:@"Unable to run “%.*s”.", (int)_bundleCommand.name.size(), _bundleCommand.name.data()],
				NSLocalizedRecoverySuggestionErrorKey : to_ns(message),
			}];

			if(failedRequirement.more_info_url != NULL_STR)
			{
				dict[@"moreInfoURL"] = [NSURL URLWithString:to_ns(failedRequirement.more_info_url)];
				dict[NSLocalizedRecoveryOptionsErrorKey] = @[ @"OK", @"More Info…" ];
				dict[NSRecoveryAttempterErrorKey] = self;
			}

			NSError* error = [NSError errorWithDomain:OakCommandErrorDomain code:OakCommandRequirementsMissingError userInfo:dict];
			[self presentError:error];

			return;
		}
		_didCheckRequirements = YES;
	}

	if(_didSaveChanges == NO)
	{
		if(_bundleCommand.pre_exec != pre_exec::nop)
		{
			[self saveAllEditedDocuments:(_bundleCommand.pre_exec == pre_exec::save_project) completionHandler:^(BOOL didSave){
				if(didSave)
				{
					_didSaveChanges = YES;
					[self executeWithInput:inputFH outputHandler:handler];
				}
			}];
			return;
		}
		_didSaveChanges = YES;
	}

	bool hasHTMLOutput = _bundleCommand.output == output::new_window && _bundleCommand.output_format == output_format::html;
	if(_didFindHTMLOutputView == NO)
	{
		if(hasHTMLOutput && (_bundleCommand.output_reuse == output_reuse::reuse_busy || _bundleCommand.output_reuse == output_reuse::abort_and_reuse_busy))
		{
			_htmlOutputView = [self htmlOutputView:NO forIdentifier:self.identifier];
			if(_htmlOutputView && _htmlOutputView.isRunningCommand)
			{
				BOOL askUser = _bundleCommand.output_reuse != output_reuse::abort_and_reuse_busy;
				[_htmlOutputView stopLoadingWithUserInteraction:askUser completionHandler:^(BOOL didStop){
					if(didStop)
					{
						_didFindHTMLOutputView = YES;
						[self executeWithInput:inputFH outputHandler:handler];
					}
				}];
				return;
			}
		}
		_didFindHTMLOutputView = YES;
	}

	__block std::string out, err;
	auto stdoutHandler  = ^(char const* bytes, size_t len) { out.insert(out.end(), bytes, bytes + len); };
	auto stderrHandler  = ^(char const* bytes, size_t len) { err.insert(err.end(), bytes, bytes + len); };
	auto htmlOutHandler = ^(char const* bytes, size_t len) { [self writeHTMLOutput:bytes length:len]; };

	std::string const directory = format_string::expand("${TM_DIRECTORY:-${TM_PROJECT_DIRECTORY:-$TMPDIR}}", _environment);
	std::string scriptPath = path::temp("command", _bundleCommand.command);
	ASSERT(scriptPath != NULL_STR);

	__block BOOL didTerminate = NO;
	_processIdentifier = run_command(_dispatchGroup, scriptPath, inputFH.fileDescriptor, _environment, directory, CFRunLoopGetCurrent(), hasHTMLOutput ? htmlOutHandler : stdoutHandler, stderrHandler, ^(int status) {
		_processIdentifier = 0;
		unlink(scriptPath.c_str());

		std::string newOut, newErr;
		oak::replace_copy(out.begin(), out.end(), scriptPath.begin(), scriptPath.end(), _bundleCommand.name.begin(), _bundleCommand.name.end(), back_inserter(newOut));
		oak::replace_copy(err.begin(), err.end(), scriptPath.begin(), scriptPath.end(), _bundleCommand.name.begin(), _bundleCommand.name.end(), back_inserter(newErr));
		newOut.swap(out);
		newErr.swap(err);

		if(WIFSIGNALED(status))
			fprintf(stderr, "*** process terminated: %s\n", strsignal(WTERMSIG(status)));
		else if(!WIFEXITED(status))
			fprintf(stderr, "*** process terminated abnormally %d\n", status);

		output::type placement         = _bundleCommand.output;
		output_format::type format     = _bundleCommand.output_format;
		output_caret::type outputCaret = _bundleCommand.output_caret;

		int rc = WIFEXITED(status) ? WEXITSTATUS(status) : (WIFSIGNALED(status) ? 0 : -1);
		enum { exit_discard = 200, exit_replace_text, exit_replace_document, exit_insert_text, exit_insert_snippet, exit_show_html, exit_show_tool_tip, exit_create_new_document };
		switch(rc)
		{
			case exit_discard:             placement = output::discard;                                            break;
			case exit_replace_text:        placement = output::replace_input;     format = output_format::text;    outputCaret = output_caret::heuristic;             break;
			case exit_replace_document:    placement = output::replace_document;  format = output_format::text;    outputCaret = output_caret::interpolate_by_line;   break;
			case exit_insert_text:         placement = output::after_input;       format = output_format::text;    outputCaret = output_caret::after_output;          break;
			case exit_show_html:           placement = output::new_window;        format = output_format::html;    break;
			case exit_show_tool_tip:       placement = output::tool_tip;          format = output_format::text;    break;
			case exit_create_new_document: placement = output::new_window;        format = output_format::text;    break;
			case exit_insert_snippet:
			{
				format = output_format::snippet;
				if(_bundleCommand.input == input::selection)
					placement = output::replace_input;
				else if(_bundleCommand.input == input::entire_document)
					placement = output::at_caret;
				else
					placement = output::after_input;
			}
			break;
		}

		BOOL normalExit = rc == 0 || (200 <= rc && rc <= 207);
		if(normalExit == NO && _userDidAbort == NO)
		{
			NSMutableDictionary* dict = [NSMutableDictionary dictionaryWithDictionary:@{
				NSLocalizedDescriptionKey             : [NSString stringWithFormat:@"Failure running “%@”.", to_ns(_bundleCommand.name)],
				NSLocalizedRecoverySuggestionErrorKey : to_ns(text::trim(err + out).empty() ? text::format("Command returned status code %d.", rc) : err + out) ?: @"Command output is not UTF-8.",
			}];

			if(bundles::lookup(_bundleCommand.uuid))
			{
				dict[NSLocalizedRecoveryOptionsErrorKey] = @[ @"OK", @"Edit Command" ];
				dict[NSRecoveryAttempterErrorKey]        = self;
			}

			NSError* error = [NSError errorWithDomain:OakCommandErrorDomain code:OakCommandAbnormalTerminationError userInfo:dict];
			[self presentError:error];
		}
		else if(placement == output::new_window)
		{
			if(format == output_format::text)
			{
				[self showDocument:[OakDocument documentWithString:to_ns(err + out) fileType:nil customName:nil]];
			}
			else if(format == output_format::html)
			{
				if(!hasHTMLOutput)
					[self writeHTMLOutput:out.data() length:out.size()];

				if(!err.empty())
					[self writeHTMLOutput:err.data() length:err.size()];

				[self writeHTMLOutput:nullptr length:0];
			}
		}
		else if(placement == output::tool_tip)
		{
			std::string str = err + out;
			auto len = str.find_last_not_of(" \t\n");
			if(len != std::string::npos)
				[self showToolTip:to_ns(str.substr(0, len+1))];
		}
		else if(placement != output::discard)
		{
			if(format == output_format::snippet && _bundleCommand.disable_output_auto_indent)
				format = output_format::snippet_no_auto_indent;

			if(handler)
				handler(out, placement, format, outputCaret, _environment);
			else if(out.size() || err.size())
				[self showDocument:[OakDocument documentWithString:to_ns(err + out) fileType:nil customName:nil]];
		}
		else if(_htmlOutputView)
		{
			[self writeHTMLOutput:nullptr length:0];
			[self discardHTMLOutputView:_htmlOutputView];
		}

		if(_terminationHandler)
			_terminationHandler(self, normalExit);

		// Wake potential event loop
		didTerminate = YES;
		[NSApp postEvent:[NSEvent otherEventWithType:NSApplicationDefined location:NSZeroPoint modifierFlags:0 timestamp:0 windowNumber:0 context:NULL subtype:0 data1:0 data2:0] atStart:NO];
		[[NSNotificationCenter defaultCenter] postNotificationName:OakCommandDidTerminateNotification object:self];
	});

	if(_bundleCommand.output == output::new_window && _bundleCommand.output_format == output_format::html)
		return;

	if(_modalEventLoopRunner)
		_modalEventLoopRunner(self, &didTerminate);
}

- (void)terminate
{
	if(_processIdentifier != 0)
	{
		_userDidAbort = YES;
		oak::kill_process_group_in_background(_processIdentifier);
	}
}

- (void)closeHTMLOutputView
{
	[self discardHTMLOutputView:_htmlOutputView];
}

// =============================
// = NSErrorRecoveryAttempting =
// =============================

- (BOOL)attemptRecoveryFromError:(NSError*)error optionIndex:(NSUInteger)recoveryOptionIndex
{
	BOOL didRecover = NO;
	switch(error.code)
	{
		case OakCommandRequirementsMissingError:
		{
			if(recoveryOptionIndex == 1)
				[[NSWorkspace sharedWorkspace] openURL:error.userInfo[@"moreInfoURL"]];
		}
		break;

		case OakCommandAbnormalTerminationError:
		{
			if(recoveryOptionIndex == 1)
			{
				Class cl = NSClassFromString(@"BundleEditor");
				[[cl sharedInstance] revealBundleItem:bundles::lookup(_bundleCommand.uuid)];
			}
		}
		break;
	}
	return didRecover;
}

- (void)attemptRecoveryFromError:(NSError*)error optionIndex:(NSUInteger)recoveryOptionIndex delegate:(id)delegate didRecoverSelector:(SEL)didRecoverSelector contextInfo:(void*)contextInfo
{
	BOOL didRecover = [self attemptRecoveryFromError:error optionIndex:recoveryOptionIndex];
	if(delegate && didRecoverSelector)
	{
		auto fn = (void(*)(id, SEL, BOOL, void*))[delegate methodForSelector:didRecoverSelector];
		fn(delegate, didRecoverSelector, didRecover, contextInfo);
	}
}

// ===========================
// = Call to first Responder =
// ===========================

- (id)targetForAction:(SEL)action
{
	NSResponder* responder = _firstResponder;
	while(responder)
	{
		if([responder respondsToSelector:action])
			return responder;

		if([responder isKindOfClass:[NSWindow class]] || responder == NSApp)
		{
			if([[responder performSelector:@selector(delegate)] respondsToSelector:action])
				return [responder performSelector:@selector(delegate)];
		}

		if(responder == NSApp.keyWindow && NSApp.mainWindow && NSApp.mainWindow != NSApp.keyWindow)
			responder = NSApp.mainWindow.firstResponder ?: NSApp.mainWindow;
		else if([responder isKindOfClass:[NSWindow class]])
			responder = NSApp;
		else
			responder = responder.nextResponder;
	}
	return nil;
}

- (void)updateEnvironment:(std::map<std::string, std::string>&)res
{
	if(id target = [self targetForAction:@selector(updateEnvironment:forCommand:)])
		return [target updateEnvironment:res forCommand:self];
	res = bundles::scope_variables(res); // Bundle items with a shellVariables setting
	res = variables_for_path(res); // .tm_properties
}

- (void)saveAllEditedDocuments:(BOOL)includeAllFlag completionHandler:(void(^)(BOOL didSave))callback
{
	if(id target = [self targetForAction:_cmd])
		[target saveAllEditedDocuments:includeAllFlag completionHandler:callback];
	else if(callback)
		callback(YES);
}

- (OakHTMLOutputView*)htmlOutputView:(BOOL)createFlag forIdentifier:(NSUUID*)identifier
{
	OakHTMLOutputView* view;
	if(id target = [self targetForAction:_cmd])
	{
		view = [target htmlOutputView:createFlag forIdentifier:identifier];
	}
	else
	{
		view = _htmlOutputWindowController.htmlOutputView;
		if(view.needsNewWebView || ![view.commandIdentifier isEqual:identifier])
			view = nil;

		if(createFlag && (!view || view.isRunningCommand))
		{
			_htmlOutputWindowController = [[HTMLOutputWindowController alloc] initWithIdentifier:identifier];
			view = _htmlOutputWindowController.htmlOutputView;
		}
	}

	if(view)
	{
		if([view.window.delegate respondsToSelector:@selector(showWindow:)])
			[view.window.delegate performSelector:@selector(showWindow:) withObject:self];
		[view.window makeFirstResponder:view.webView];
	}

	return view;
}

- (void)discardHTMLOutputView:(OakHTMLOutputView*)htmlOutputView
{
	if(id target = [self targetForAction:_cmd])
		[target discardHTMLOutputView:htmlOutputView];
	else if(id delegate = htmlOutputView.webView.UIDelegate)
		[delegate performSelector:@selector(webViewClose:) withObject:htmlOutputView.webView];
}

- (void)showToolTip:(NSString*)aToolTip
{
	if(id target = [self targetForAction:_cmd])
		return [target showToolTip:aToolTip];
	OakShowToolTip(aToolTip, [NSEvent mouseLocation]);
}

- (void)showDocument:(OakDocument*)aDocument
{
	if(id target = [self targetForAction:_cmd])
		return [target showDocument:aDocument];
	[OakDocumentController.sharedInstance showDocument:aDocument];
}

- (BOOL)presentError:(NSError*)anError
{
	if(id target = [self targetForAction:_cmd])
		return [target presentError:anError];
	return NO;
}
@end

// =====================
// = Custom URL Scheme =
// =====================

@interface OakFileHandleURLProtocol : NSURLProtocol
{
	BOOL _stop;
}
@end

@implementation OakFileHandleURLProtocol
+ (void)load
{
	[self registerClass:self];
	[WebView registerURLSchemeAsLocal:kOakFileHandleURLScheme];
}

+ (BOOL)canInitWithRequest:(NSURLRequest*)request                            { return [request.URL.scheme isEqualToString:kOakFileHandleURLScheme]; }
+ (NSURLRequest*)canonicalRequestForRequest:(NSURLRequest*)request           { return request; }
+ (BOOL)requestIsCacheEquivalent:(NSURLRequest*)a toRequest:(NSURLRequest*)b { return NO; }

// =============================================
// = These methods might be called in a thread =
// =============================================

- (void)startLoading
{
	NSFileHandle* fileHandle = [NSURLProtocol propertyForKey:@"fileHandle" inRequest:self.request];
	if(!fileHandle)
	{
		NSURLResponse* response = [[NSHTTPURLResponse alloc] initWithURL:self.request.URL statusCode:404 HTTPVersion:@"HTTP/1.1" headerFields:nil];
		[self.client URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageNotAllowed];
		[self.client URLProtocolDidFinishLoading:self];
		NSLog(@"No command output for ‘%@’", self.request.URL);
		return;
	}

	NSURLResponse* response = [[NSURLResponse alloc] initWithURL:self.request.URL MIMEType:@"text/html" expectedContentLength:-1 textEncodingName:@"utf-8"];
	[self.client URLProtocol:self didReceiveResponse:response cacheStoragePolicy:NSURLCacheStorageNotAllowed];

	dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
		int len;
		char buf[8192];
		__block BOOL keepRunning = YES;
		@try {
			while(keepRunning && (len = read(fileHandle.fileDescriptor, buf, sizeof(buf))) > 0)
			{
				NSData* data = [NSData dataWithBytes:buf length:len];
				dispatch_sync(dispatch_get_main_queue(), ^{
					if(keepRunning = !_stop)
						[self.client URLProtocol:self didLoadData:data];
				});
			}
		}
		@catch(NSException* e) {
			NSData* data = [[NSString stringWithFormat:@"<p>Exception thrown while reading data: %@.</p>", e.reason] dataUsingEncoding:NSUTF8StringEncoding];
			dispatch_sync(dispatch_get_main_queue(), ^{
				if(!_stop)
					[self.client URLProtocol:self didLoadData:data];
			});
		}

		if(len == -1)
			perror("HTMLOutput: read");

		[fileHandle closeFile];
		[self.client URLProtocolDidFinishLoading:self];
	});
}

- (void)stopLoading
{
	_stop = YES;
	if(pid_t pid = [[NSURLProtocol propertyForKey:@"processIdentifier" inRequest:self.request] intValue])
		oak::kill_process_group_in_background(pid);
}
@end
