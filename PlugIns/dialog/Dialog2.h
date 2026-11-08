static NSString* const kDialogServerConnectionName = @"com.macromates.dialog";

@protocol DialogServerProtocol
- (void)connectFromClientWithOptions:(id)anArgument;
@end
