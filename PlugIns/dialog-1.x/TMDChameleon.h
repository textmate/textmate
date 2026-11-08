//
//  TMDChameleon.h
//
//  Created by Allan Odgaard on 2007-06-26.
//  Copyright (c) 2007 MacroMates. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface TMDChameleon : NSObject
+ (BOOL)createSubclassNamed:(NSString*)aName withValues:(NSDictionary*)values;
@end
