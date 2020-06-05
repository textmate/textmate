/*
This class controls a stack of _stackSize_ objects, which will be stored in the app’s defaults with _name_.

If an object that is already in the list is added, it will be moved to the top of the list instead.
If the list grows beyond _stackSize_ objects, the last object will be removed before the new item is added.
*/

@interface OakHistoryList<ObjectType> : NSObject
@property (nonatomic, readonly) NSUInteger stackSize;
@property (nonatomic) ObjectType head;

- (id)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size;
- (id)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size fallbackUserDefaultsKey:(NSString*)fallbackDefaultsName;
- (id)initWithName:(NSString*)defaultsName stackSize:(NSUInteger)size defaultItems:(id)firstItem, ...;
- (void)addObject:(ObjectType)newItem;
- (NSEnumerator<ObjectType>*)objectEnumerator;
- (ObjectType)objectAtIndex:(NSUInteger)index;
- (NSUInteger)count;
@end
