#import "CWTableCellView.h"
#import "CWStatusStringTransformer.h"
#import <OakAppKit/OakUIConstructionFunctions.h>

@implementation CWTableCellView
- (id)init
{
	if((self = [super init]))
	{
		NSTextField* textField = OakCreateLabel();
		textField.font = [NSFont controlContentFontOfSize:0];
		[textField.cell setLineBreakMode: NSLineBreakByTruncatingMiddle];
		self.textField = textField;

		NSButton* commitCheckBox = OakCreateCheckBox(@"");
		[commitCheckBox.cell setControlSize:NSSmallControlSize];
		_commitCheckBox = commitCheckBox;

		NSTextField* statusTextField = OakCreateLabel();
		_statusTextField = statusTextField;

		NSButton* diffButton = OakCreateButton(@"Diff", NSRoundedBezelStyle);
		diffButton.font = [NSFont messageFontOfSize:9];
		[diffButton.cell setControlSize: NSMiniControlSize];
		_diffButton = diffButton;

		[self.textField   bind:NSValueBinding toObject:self withKeyPath:@"objectValue.path"      options:0];
		[_commitCheckBox  bind:NSValueBinding toObject:self withKeyPath:@"objectValue.commit"    options:0];
		[_statusTextField bind:NSValueBinding toObject:self withKeyPath:@"objectValue.scmStatus" options:@{ NSValueTransformerNameBindingOption : @"CWStatusStringTransformer" }];

		NSDictionary* views = @{ @"commit" : _commitCheckBox, @"status" : _statusTextField, @"textField" : self.textField, @"diff" : _diffButton };
		for(NSView* view in [views allValues])
		{
			[view setTranslatesAutoresizingMaskIntoConstraints:NO];
			[self addSubview:view];
		}

		[_commitCheckBox setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];
		[_statusTextField setContentCompressionResistancePriority:NSLayoutPriorityRequired forOrientation:NSLayoutConstraintOrientationHorizontal];

		[self addConstraints:[NSLayoutConstraint constraintsWithVisualFormat:@"H:|-(5)-[commit]-(5)-[status]-(5)-[textField]-(>=5)-[diff(==40)]-(5)-|" options:NSLayoutFormatAlignAllCenterY metrics:nil views:views]];
	}
	return self;
}
@end
