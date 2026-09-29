#import <regexp/src/find.h> // for find::options_t
#import <text/src/types.h>

enum find_operation_t {
	kFindOperationCount,
	kFindOperationCountInSelection,
	kFindOperationFind,
	kFindOperationFindInSelection,
	kFindOperationReplace,
	kFindOperationReplaceAndFind,
	kFindOperationReplaceAll,
	kFindOperationReplaceAllInSelection,
};

@protocol OakFindServerProtocol <NSObject>
@property (nonatomic, readonly) find_operation_t findOperation;
@property (nonatomic, readonly) NSString* findString;
@property (nonatomic, readonly) NSString* replaceString;
@property (nonatomic, readonly) find::options_t findOptions;

- (void)didFind:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString atPosition:(text::pos_t const&)aPosition wrapped:(BOOL)didWrap;
- (void)didReplace:(NSUInteger)aNumber occurrencesOf:(NSString*)aFindString with:(NSString*)aReplacementString;
@end

@protocol OakFindClientProtocol
- (void)performFindOperation:(id <OakFindServerProtocol>)aFindServer;
@end
