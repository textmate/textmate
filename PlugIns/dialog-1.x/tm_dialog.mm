#import <Cocoa/Cocoa.h>
#import <getopt.h>
#import <fcntl.h>
#import <stdio.h>
#import <string.h>
#import <stdlib.h>
#import <unistd.h>
#import <errno.h>
#import <vector>
#import <string>
#import <sys/stat.h>

#import "TMDSemaphore.h"
#include "TMDSemaphore.mm"  // TODO we should really export this from the plugin instead and link against the plugin
#import "Dialog.h"

static char const* const AppName    = "tm_dialog";
static char const* const AppVersion = "1.1";

static char const* csv_escape_string (char const* str)
{
	if(!str)
		return "null";

	size_t cnt = 0;
	char const* loc = str;
	while(loc = strchr(loc, '"'))
	{
		++loc;
		++cnt;
	}

	if(cnt > 0 || strchr(str, ','))
	{
		char* buf = (char*)malloc(strlen(str) + cnt + 3);
		char* dst = buf;
		*dst++ = '"';
		while(*str)
		{
			if(*str == '"')
				*dst++ = '"';
			*dst++ = *str++;
		}
		*dst++ = '"';
		*dst++ = '\0';

		str = buf;
	}
	return str;
}

static void log_usage ()
{
	if(char const* logfile = getenv("TEXTMATE_MIGRATION_LOG"))
	{
		if(FILE* fp = fopen(logfile, "a"))
		{
			if(ftell(fp) == 0)
				fprintf(fp, "date,bundleItemUUID,bundleItemName,component,extra\n");

			time_t t = time(nullptr);
			struct tm* utc = gmtime(&t);
			char date[24];
			strftime(date, sizeof(date), "%FT%TZ", utc);

			char const* bundleItemUUID = getenv("TM_BUNDLE_ITEM_UUID");
			char const* bundleItemName = getenv("TM_BUNDLE_ITEM_NAME");

			fprintf(fp, "%s,%s,%s,%s,\n", csv_escape_string(date), csv_escape_string(bundleItemUUID), csv_escape_string(bundleItemName), csv_escape_string(getprogname()));
			fclose(fp);
		}
	}
}

id read_property_list_from_data (NSData* data)
{
	if([data length] == 0)
		return nil;

	NSError* error = nil;
	id plist = [NSPropertyListSerialization propertyListWithData:data options:NSPropertyListMutableContainersAndLeaves format:nil error:&error];

	if(error || !plist)
	{
		fprintf(stderr, "%s: %s\n", AppName, [[error localizedDescription] UTF8String] ?: "unknown error parsing property list");
		fwrite([data bytes], [data length], 1, stderr);
		fprintf(stderr, "\n");
	}

	return plist;
}

id read_property_list_from_string (char const* str)
{
	return read_property_list_from_data([NSData dataWithBytes:str length:str ? strlen(str) : 0]);
}

id read_property_list_from_file (int fd)
{
	NSMutableData*	data = [NSMutableData data];

	char buf[1024];
	while(size_t len = read(fd, buf, sizeof(buf)))
		[data appendBytes:buf length:len];

	return read_property_list_from_data(data);
}

bool output_property_list (id plist)
{
	bool res = false;
	NSError* error = nil;
	if(NSData* data = [NSPropertyListSerialization dataWithPropertyList:plist format:NSPropertyListXMLFormat_v1_0 options:0 error:&error])
	{
		if(NSFileHandle* fh = [NSFileHandle fileHandleWithStandardOutput])
		{
			[fh writeData:data];
			res = true;
		}
	}
	else
	{
		fprintf(stderr, "%s: %s\n", AppName, [[error localizedDescription] UTF8String] ?: "unknown error serializing returned property list");
		fprintf(stderr, "%s\n", [[plist description] UTF8String]);
	}
	return res;
}

// validate_proxy: return an instance of the TM dialog server proxy object. Return false (and write details to stderr)
// if the TM dialog server is unavailable or the protocol version doesn't match.
BOOL validate_proxy (id* outProxy)
{
	static BOOL proxyValid = NO;
	static id proxy = nil;

	// One shot validate -- if it isn't valid now, presumably it won't be ever
	// (during the very short life of an instance of this tool)
	if(!proxyValid)
	{
		NSString* portName = @"TextMate dialog server";
		if(char const* var = getenv("DIALOG_1_PORT_NAME"))
			portName = [NSString stringWithUTF8String:var];

		proxy = [NSConnection rootProxyForConnectionWithRegisteredName:portName host:nil];
		[proxy setProtocolForProxy:@protocol(TextMateDialogServerProtocol)];

		if([proxy textMateDialogServerProtocolVersion] == TextMateDialogServerProtocolVersion)
		{
			proxyValid = YES;
		}
		else
		{
			if(proxy)
			{
				int pluginVersion = [proxy textMateDialogServerProtocolVersion];
				int toolVersion = TextMateDialogServerProtocolVersion;
				if(pluginVersion < toolVersion)
				{
					fprintf(stderr, "%s: you have updated the tm_dialog tool to v%d but the Dialog plug-in running is still at v%d.\n", AppName, toolVersion, pluginVersion);
					fprintf(stderr, "%s: either checkout the PlugIns folder from the repository or remove your checkout of the Support folder.\n", AppName);
					fprintf(stderr, "%s: if you did checkout the PlugIns folder, you need to relaunch TextMate to load the new plug-in.\n", AppName);
				}
				else
				{
					fprintf(stderr, "%s: you have updated the Dialog plug-in to v%d but the tm_dialog tool is still at v%d\n", AppName, pluginVersion, toolVersion);
				}
			}
			else
			{
				fprintf(stderr, "%s: failed to establish connection with TextMate.\n", AppName);
				if(!getenv("DIALOG_1_PORT_NAME"))
				{
					fprintf(stderr, "%s:\n", AppName);
					fprintf(stderr, "%s: When running outside TextMate you need to set the DIALOG_1_PORT_NAME environment variable.\n", AppName);
					fprintf(stderr, "%s: In a new TextMate document press ^C on a line containing just: echo $DIALOG_1_PORT_NAME\n", AppName);
					fprintf(stderr, "%s: Then in Terminal run: export DIALOG_1_PORT_NAME=«value»\n", AppName);
					fprintf(stderr, "%s: Here «value» is the value you got from TextMate, e.g. ‘com.macromates.dialog_1.45850’.\n", AppName);
				}
			}
		}
	}

	*outProxy = proxy;
	return proxyValid;
}

// contact_server_async_update: update (a subset of) the binding values.
int contact_server_async_update (const char* token, NSMutableDictionary* someParameters)
{
	id proxy;
	int returnCode = -1;

	if(someParameters == nil)
	{
		fprintf(stderr, "%s: no property list given, skipping update\n", AppName);
	}
	else if(validate_proxy(&proxy))
	{
		id result = [proxy updateNib:[NSString stringWithUTF8String:token] withParameters:someParameters];
		returnCode = [[result objectForKey:@"returnCode"] intValue];

		if(returnCode == -43)
		{
			fprintf(stderr, "%s (async_update): Window '%s' doesn't exist\n", AppName, token);
		}
	}
	return returnCode;
}

// contact_server_async_close: close the window
int contact_server_async_close (const char* token, bool ignoreFailure)
{
	id proxy;
	int returnCode = -1;

	if(validate_proxy(&proxy))
	{
		id result = [proxy closeNib:[NSString stringWithUTF8String:token]];
		returnCode = [[result objectForKey:@"returnCode"] intValue];

		if(ignoreFailure)
		{
			returnCode = 0;
		}
		else
		{
			if(returnCode == -43)
			{
				fprintf(stderr, "%s (async_close): Window '%s' doesn't exist\n", AppName, token);
			}
		}
	}

	return returnCode;
}

// contact_server_async_wait: block until returnArgument:, performButtonClick:, or the window is closed,
// then output the resulting plist returned by the dialog server.
int contact_server_async_wait (const char* token)
{
	id proxy;
	int returnCode = -1;

	if(validate_proxy(&proxy))
	{
		id result;
		TMDSemaphore* semaphore = [TMDSemaphore semaphoreForTokenString:token];

		// Validate the window (throw away this result other than the returnCode)
		result = [proxy retrieveNibResults:[NSString stringWithUTF8String:token]];
		returnCode = [[result objectForKey:@"returnCode"] intValue];

		if(returnCode == 0)
		{
			[semaphore wait];

			result = [proxy retrieveNibResults:[NSString stringWithUTF8String:token]];
			output_property_list(result);
		}
		else if(returnCode == -43)
		{
			fprintf(stderr, "%s (async_wait): Window '%s' doesn't exist\n", AppName, token);
		}
		else
		{
			fprintf(stderr, "%s (async_wait): Window '%s' error %d\n", AppName, token, returnCode);
		}
	}

	return returnCode;
}


// contact_server_async_list: print a list of windows to stdout
int contact_server_async_list ()
{
	id proxy;
	int returnCode = -1;

	if(validate_proxy(&proxy))
	{
		id result = [proxy listNibTokens];
		returnCode = [[result objectForKey:@"returnCode"] intValue];

		if(returnCode != 0)
		{
			fprintf(stderr, "%s: Unknown error code '%d'\n", AppName, returnCode);
		}
		else
		{
			NSArray* nibs = [result objectForKey:@"nibs"];
			for(NSDictionary* nib in nibs)
			{
				NSString* windowTitleString = [nib objectForKey:@"windowTitle"];
				const char* windowTitleC    = "<no title>";
				int windowToken             = [[nib objectForKey:@"token"] intValue];

				if(windowTitleString != nil && ![windowTitleString isEqualToString:@""])
					windowTitleC = [windowTitleString UTF8String];

				fprintf(stdout, "%d (%s)\n", windowToken, windowTitleC);
			}
		}
	}

	return returnCode;
}

// contact_server_show_nib: instantiate the nib inside TM
int contact_server_show_nib (std::string nibName, NSMutableDictionary* someParameters, NSDictionary* initialValues, NSDictionary* dynamicClasses, bool center, bool modal, bool quiet, bool async)
{
	int res = -1;
	id proxy;

	if(validate_proxy(&proxy))
	{
		NSString* aNibPath = [NSString stringWithUTF8String:nibName.c_str()];
		NSDictionary* parameters = (NSDictionary*)[proxy showNib:aNibPath withParameters:(someParameters ?: [NSMutableDictionary dictionary]) andInitialValues:initialValues dynamicClasses:dynamicClasses modal:modal center:center async:async];

		const char*	token = [[NSString stringWithFormat:@"%@", [parameters objectForKey:@"token"]] UTF8String];

		res = [[parameters objectForKey:@"returnCode"] intValue];

		// Async mode: print just the token for the new window
		if(res != 0)
		{
			fprintf(stderr, "Error %d creating window\n", res);
		}
		else
		{
			if(async)
			{
				fprintf(stdout, "%d\n", [[parameters objectForKey:@"token"] intValue]);
			}
			else if(modal)
			{
				// Modal: the window has already been ordered out; retrieve the results and close it.
				if(validate_proxy(&proxy) && !quiet)
				{
					id result = [proxy retrieveNibResults:[NSString stringWithUTF8String:token]];
					output_property_list(result);

					res = [[result objectForKey:@"returnCode"] intValue];
				}
				contact_server_async_close(token, false);	// false -> log errors
			}
			else
			{
				// Not async, not modal. The task had better be detached from TM, or TM will hang
				// until the task is killed. Wait until something happens.
				res = contact_server_async_wait(token);

				if(!quiet)
					output_property_list(parameters);

				contact_server_async_close(token, false);	// false -> log errors
			}
		}
	}
	return res;
}

void usage (FILE* io = stderr)
{
	fprintf(io,
		"%1$s %2$s (" __DATE__ ")\n"
		"\n"
		"  *******************************\n"
		"  * This version is deprecated. *\n"
		"  * Instead run: \"$DIALOG\" help *\n"
		"  *******************************\n"
		"\n"
		"Usage (dialog): %1$s [-cdnmqp] nib_file\n"
		"Usage (window): %1$s [-cdnpaxts] nib_file\n"
		"Usage (alert): %1$s [-p] -e [-i|-c|-w]\n"
		"Usage (menu): %1$s [-p] -u\n"
		"\n"
		"Dialog Options:\n"
		" -c, --center                 Center the window on screen.\n"
		" -d, --defaults <plist>       Register initial values for user defaults.\n"
		" -n, --new-items <plist>      A key/value list of classes (the key) which should dynamically be created at run-time for use as the NSArrayController’s object class. The value (a dictionary) is how instances of this class should be initialized (the actual instance will be an NSMutableDictionary with these values).\n"
		" -m, --modal                  Show window as modal (other windows will be inaccessible).\n"
		" -p, --parameters <plist>     Provide parameters as a plist.\n"
		" -q, --quiet                  Do not write result to stdout.\n"
		"\nAlert Options:\n"
		" -e, --alert                  Show alert. Parameters: 'title', 'message', 'buttons'\n"
		"                              'alertStyle' -- can be 'warning,' 'informational',\n"
		"                              'critical'.  Returns the button index.\n"
		"Menu Options:\n"
		" -u, --menu                   Treat parameters as a menu structure.\n"
		"\nAsync Window Options:\n"
		" -a, --async-window           Displays the window and returns a reference token for it\n"
		"                              in the output property list.\n"
		" -l, --list-windows           List async window tokens.\n"
		" -t, --update-window <token>  Update an async window with new parameter values.\n"
		"                              Use the --parameters argument (or stdin) to specify the\n"
		"                              updated parameters.\n"
		" -x, --close-window <token>   Close and release an async window.\n"
		" -w, --wait-for-input <token> Wait for user input from the given async window.\n"
		"\nNote:\n"
		"If you DO NOT use the -m/--modal option,\n"
		"OR you create an async window and then use the wait-for-input subcommand,\n"
		"you must run tm_dialog in a detached/backgrounded process (`mycommand 2&>1 &` in bash).\n"
		"Otherwise, TextMate's UI thread will hang, waiting for your command to complete.\n"
		"You can recover from such a hang by killing the tm_dialog process in Terminal.\n"
		"\n"
		"", AppName, AppVersion);
}

std::string find_nib (std::string nibName)
{
	std::vector<std::string> candidates;

	if(nibName.find(".nib") == std::string::npos)
		nibName += ".nib";

	if(nibName.size() && nibName[0] != '/') // relative path
	{
		if(char const* currentPath = getcwd(NULL, 0))
			candidates.push_back(currentPath + std::string("/") + nibName);

		if(char const* bundleSupport = getenv("TM_BUNDLE_SUPPORT"))
			candidates.push_back(bundleSupport + std::string("/nibs/") + nibName);

		if(char const* supportPath = getenv("TM_SUPPORT_PATH"))
			candidates.push_back(supportPath + std::string("/nibs/") + nibName);
	}
	else
	{
		candidates.push_back(nibName);
	}

	for(decltype(candidates.begin()) it = candidates.begin(); it != candidates.end(); ++it)
	{
		struct stat sb;
		if(stat(it->c_str(), &sb) == 0)
			return *it;
	}

	fprintf(stderr, "nib could not be loaded: %s (does not exist)\n", nibName.c_str());
	abort();
	return NULL;
}

id read_property_list_argument(const char* parameters)
{
	id plist = nil;
	if(parameters)
	{
		plist = read_property_list_from_string(parameters);
	}
	else
	{
		if(isatty(STDIN_FILENO) != 0)
			fprintf(stderr, "%s: Reading parameters from stdin... (press CTRL-D to proceed)\n", AppName);
		plist = read_property_list_from_file(STDIN_FILENO);
	}

	return plist;
}

int main (int argc, char* argv[])
{
	int res = EX_USAGE;

	extern int optind;
	extern char* optarg;

	enum DialogAction
	{
		kShowDialog,
		kShowMenu,
		kShowAlert,
		kAsyncCreate,
		kAsyncClose,
		kAsyncUpdate,
		kAsyncList,
		kAsyncWait,
	};

	static struct option const longopts[] = {
		{ "alert",          no_argument,         0,     'e'   },
		{ "center",         no_argument,         0,     'c'   },
		{ "defaults",       required_argument,   0,     'd'   },
		{ "new-items",      required_argument,   0,     'n'   },
		{ "modal",          no_argument,         0,     'm'   },
		{ "parameters",     required_argument,   0,     'p'   },
		{ "quiet",          no_argument,         0,     'q'   },
		{ "menu",           no_argument,         0,     'u'   },
		{ "async-window",   no_argument,         0,     'a'   },
		{ "close-window",   required_argument,   0,     'x'   },
		{ "update-window",  required_argument,   0,     't'   },
		{ "wait-for-input", required_argument,   0,     'w'   },
		{ "list-windows",   no_argument,         0,     'l'   },
		{ "help",           no_argument,         0,     'h'   },
		{ 0,                0,                   0,     0     }
	};

	bool center = false, modal = false, quiet = false;
	char const* parameters = NULL;
	char const* defaults = NULL;
	char const* dynamicClassesPlist = NULL;
	char const* token = NULL;
	int ch;
	DialogAction dialogAction = kShowDialog;

	setprogname(AppName); // when called from tm_dialog2 (via execv()) our getprogname is wrong, which is used by getopt_long().
	while((ch = getopt_long(argc, argv, "eacd:mn:p:quax:t:w:lh", longopts, NULL)) != -1)
	{
		switch(ch)
		{
			case 'e':   dialogAction = kShowAlert;    break;
			case 'c':   center = true;                break;
			case 'd':   defaults = optarg;            break;
			case 'm':   modal = true;                 break;
			case 'n':   dynamicClassesPlist = optarg; break;
			case 'p':   parameters = optarg;          break;
			case 'q':   quiet = true;                 break;
			case 'u':   dialogAction = kShowMenu;     break;
			case 'a':   dialogAction = kAsyncCreate;  break;
			case 'x':   dialogAction = kAsyncClose; token = optarg;  break;
			case 't':   dialogAction = kAsyncUpdate; token = optarg; break;
			case 'w':   dialogAction = kAsyncWait; token = optarg;   break;
			case 'l':   dialogAction = kAsyncList;    break;
			case 'h':   usage(stdout);                return EX_OK;
			default:    usage();                      return EX_USAGE;
		}
	}

	argc -= optind;
	argv += optind;

	log_usage();

	if(dialogAction != kShowDialog)
	{
		if(modal)
			fprintf(stderr, "%s: warning: Ignoring 'modal' option\n", AppName);

		if(quiet)
			fprintf(stderr, "%s: warning: Ignoring 'quiet' option.\n", AppName);
	}

	@autoreleasepool {
		@try {

		switch(dialogAction)
		{
			case kShowMenu:
				if(argc == 0)
				{
					id proxy;
					if(validate_proxy(&proxy))
					{
						if(id plist = read_property_list_argument(parameters))
						{
							output_property_list([proxy showMenuWithOptions:plist]);
							res = EX_OK;
						}
						else
						{
							fprintf(stderr, "%s: no property list given\n", AppName);
						}
					}
				}
				else
				{
					usage();
				}
				break;
			case kShowAlert:
				if(argc == 0)
				{
					id proxy;
					if(validate_proxy(&proxy))
					{
						id plist = read_property_list_argument(parameters);
						NSDictionary* output = [proxy showAlertForPath:nil withParameters:plist modal:YES];
						printf("%d\n", [[output objectForKey:@"buttonClicked"] intValue]);
						res = EX_OK;
					}
				}
				else
				{
					usage();
				}
				break;
			case kShowDialog:
			case kAsyncCreate:
			{
				id initialValues = read_property_list_from_string(defaults);
				id dynamicClasses = read_property_list_from_string(dynamicClassesPlist);

				if(argc == 1)
				{
					id plist = read_property_list_argument(parameters);
					res = contact_server_show_nib(find_nib(argv[0]), plist, initialValues, dynamicClasses, center, modal, quiet, (dialogAction == kAsyncCreate));
				}
				else
				{
					usage();
				}
			} break;
			case kAsyncUpdate:
			{
				id plist = read_property_list_argument(parameters);
				res = contact_server_async_update(token, plist);
			} break;
			case kAsyncClose:
				res = contact_server_async_close(token, false); // false -> generate errors
				break;
			case kAsyncWait:
				res = contact_server_async_wait(token);
				break;
			case kAsyncList:
				res = contact_server_async_list();
				break;
			default:
				usage();
				break;
		}

		} @catch(NSException* e) {
			fprintf(stderr, "%s: %s\n", AppName, [[e reason] UTF8String]);
		}

	}

	return res;
}
