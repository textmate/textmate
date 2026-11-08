#pragma once
#import <OakTabBarView/OakTabBarView.h>

@interface AgentStatusTabSource : NSObject <OakTabBarViewDataSource>
@property OakTabStatus status;
@property NSString* statusDescription;
@property NSUUID* identifier;
@end
@implementation AgentStatusTabSource
- (NSUInteger)numberOfRowsInTabBarView:(OakTabBarView*)bar { return 1; }
- (NSString*)tabBarView:(OakTabBarView*)bar titleForIndex:(NSUInteger)index { return @"Agent"; }
- (NSString*)tabBarView:(OakTabBarView*)bar pathForIndex:(NSUInteger)index { return @""; }
- (NSUUID*)tabBarView:(OakTabBarView*)bar UUIDForIndex:(NSUInteger)index { return self.identifier; }
- (BOOL)tabBarView:(OakTabBarView*)bar isEditedAtIndex:(NSUInteger)index { return NO; }
- (OakTabStatus)tabBarView:(OakTabBarView*)bar statusForIndex:(NSUInteger)index { return self.status; }
- (NSString*)tabBarView:(OakTabBarView*)bar statusDescriptionForIndex:(NSUInteger)index { return self.statusDescription; }
@end

