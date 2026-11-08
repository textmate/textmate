//
//  TMDChameleon.mm
//
//  Created by Allan Odgaard on 2007-06-26.
//  Copyright (c) 2007 MacroMates. All rights reserved.
//

#import "TMDChameleon.h"
#import <objc/runtime.h>

static NSMutableDictionary* DefaultValues = [NSMutableDictionary new];

@implementation TMDChameleon
- (id)init
{
	id res = [DefaultValues objectForKey:NSStringFromClass([self class])];
	return [res mutableCopy];
}

+ (BOOL)createSubclassNamed:(NSString*)aName withValues:(NSDictionary*)values
{
	[DefaultValues setObject:values forKey:aName];

	const char* name = [aName UTF8String];

	if(objc_lookUpClass(name))
		return YES;

	Class sub_cl = objc_allocateClassPair([TMDChameleon class], name, 0);

	if(sub_cl == Nil)
		return NO;

	objc_registerClassPair(sub_cl);

	return YES;
}
@end
