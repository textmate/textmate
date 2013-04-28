title: Bundle Dependencies

# Bundle Dependencies

## Description

The following can be added to a bundle item (setting it for a bundle’s `info.plist` is equivalent to setting it for each item in that bundle):

	require = ( { name = «name»; uuid = «uuid»; }, … );

The UUID is that of the bundle or bundle item required. The name is the name under which the required item should be available (more or less), this only make sense when the UUID is that of a grammar or bundle, see details below. The name defaults to the bundle’s or bundle item’s name, except for grammars, where it is the root scope for the grammar.

If the UUID is that of a bundle item, TextMate ensures that the item and bundle containing it, when the item with the requirement is “executed”, is available.

If the UUID is that of a bundle, TextMate ensures that the bundle is available and will set `TM_«name»_BUNDLE_SUPPORT` when “executing” the item with the requirement. It will uppercase the name provided.

## Examples

To be able to use `include = 'source.c';` in the Objective-C grammar, it should have the following requirement:

	require = (
	   {  name = 'source.c';
	      uuid = '25066DC2-6B1D-11D9-9D5B-000D93589AF6';
	   }
	)

The Subversion bundle’s `info.plist` could contain the following:

	require = (
	   {  name = 'Dialog';
	      uuid = 'F985E884-C6F4-4FB1-B7F6-447A72ECF267';
	   }
	)

This ensures that the bundle providing a ruby interface to the Dialog system is available and that commands in the Subversion bundle written in ruby can do:

	require "#{ENV['TM_DIALOG_BUNDLE_SUPPORT']}/lib/ui"
	⋮
	TextMate::UI.show_menu(…)
