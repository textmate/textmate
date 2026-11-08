#import "LSPBridge.h"
#import "OakSwiftUI-Swift.h"
#import <lsp/LSPClient.h>

@implementation LSPBridge

+ (void)toggleLogPanel
{
	[[OakLogPanel shared] toggle];
}

+ (void)setup
{
	static __unused LSPBridge* instance = nil;
	static dispatch_once_t onceToken;
	dispatch_once(&onceToken, ^{
		instance = [[LSPBridge alloc] init];
	});
}

- (instancetype)init
{
	if(self = [super init])
	{
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(handleLog:) name:LSPLogNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(handleShowMessage:) name:LSPShowMessageNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(handleProgress:) name:LSPProgressNotification object:nil];
		[[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(handleShowMessageRequest:) name:LSPShowMessageRequestNotification object:nil];
	}
	return self;
}

- (void)handleLog:(NSNotification*)note
{
	NSString* message = note.userInfo[@"message"];
	if(!message) return;

	NSNumber* type = note.userInfo[@"type"];
	int level = type ? type.intValue : 3; // Default to Info
	NSString* source = note.userInfo[@"source"] ?: @"LSP";
	NSString* server = note.userInfo[@"server"];
	if(server.length)
		message = [NSString stringWithFormat:@"[%@] %@", server, message];

	dispatch_async(dispatch_get_main_queue(), ^{
		[OakLogPanel.shared logWithMessage:message level:level source:source];

		if([source isEqualToString:@"response"] && [message containsString:@"initialized"])
			[OakNotificationManager.shared showWithMessage:@"LSP Server Initialized" type:4];
	});
}

- (void)handleShowMessage:(NSNotification*)note
{
	NSString* message = [note.userInfo[@"message"] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
	NSNumber* type = note.userInfo[@"type"];
	if(!message || message.length == 0) return;

	int lspType = type ? type.intValue : 3;

	// LSP type 4 (Log) is too noisy for user-facing toasts — route to log panel only
	if(lspType == 4)
		return;

	dispatch_async(dispatch_get_main_queue(), ^{
		[OakNotificationManager.shared showWithMessage:message type:lspType];
	});
}

- (void)handleShowMessageRequest:(NSNotification*)note
{
	NSString* message = [note.userInfo[@"message"] stringByTrimmingCharactersInSet:NSCharacterSet.whitespaceAndNewlineCharacterSet];
	NSNumber* type = note.userInfo[@"type"];
	NSArray<NSDictionary*>* actions = note.userInfo[@"actions"];
	NSArray<NSString*>* actionTitles = note.userInfo[@"actionTitles"];
	id requestId = note.userInfo[@"requestId"];
	LSPClient* client = (LSPClient*)note.object;

	if(!client || !requestId || requestId == [NSNull null])
		return;

	if(!message || !actionTitles || actionTitles.count == 0)
	{
		[client respondToShowMessageRequest:requestId action:nil];
		return;
	}

	int lspType = type ? type.intValue : 3;

	dispatch_async(dispatch_get_main_queue(), ^{
		[OakNotificationManager.shared showInteractiveWithMessage:message type:lspType actions:actionTitles callback:^(NSString* _Nullable selectedTitle) {
			NSDictionary* selectedAction = nil;
			if(selectedTitle)
			{
				for(NSUInteger i = 0; i < actionTitles.count; i++)
				{
					if([actionTitles[i] isEqualToString:selectedTitle])
					{
						selectedAction = actions[i];
						break;
					}
				}
			}
			[client respondToShowMessageRequest:requestId action:selectedAction];
		}];
	});
}

- (void)handleProgress:(NSNotification*)note
{
	// Progress is logged via LSPLogNotification; toasting is too noisy
	// for ephemeral operations like lint passes that complete instantly.
}

@end
