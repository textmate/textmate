#import "CWTableCellView.h"
#import "CWStatusStringTransformer.h"
#import <OakAppKit/src/OakUIConstructionFunctions.h>

@implementation CWTableCellView
- (id)init
{
	if((self = [super init]))
	{
		NSTextField* textField = OakCreateLabel(@"", [NSFont controlContentFontOfSize:0]);
		self.textField = textField;

		NSButton* commitCheckBox = OakCreateCheckBox(@"");
		commitCheckBox.controlSize = NSControlSizeSmall;
		_commitCheckBox = commitCheckBox;

		NSTextField* statusTextField = OakCreateLabel();
		_statusTextField = statusTextField;

		NSButton* diffButton = OakCreateButton(@"Diff");
		diffButton.font = [NSFont messageFontOfSize:[NSFont systemFontSizeForControlSize:NSControlSizeMini]];
		diffButton.controlSize = NSControlSizeMini;
		_diffButton = diffButton;

		[textField        bind:NSValueBinding toObject:self withKeyPath:@"objectValue.path"      options:0];
		[_commitCheckBox  bind:NSValueBinding toObject:self withKeyPath:@"objectValue.commit"    options:0];
		[_statusTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.scmStatus" options:@{ NSValueTransformerNameBindingOption: @"CWStatusStringTransformer" }];

		NSDictionary* views = @{ @"commit": _commitCheckBox, @"status": _statusTextField, @"textField": textField, @"diff": _diffButton };
		OakAddAutoLayoutViewsToSuperview([views allValues], self);

		[_commitCheckBox setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_statusTextField setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(5)-[commit]-(5)-[status]-(5)-[textField]-(>=5)-[diff(==40)]-(5)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	}
	return self;
}
@end
