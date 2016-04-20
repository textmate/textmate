title: Selection String Syntax

# Selection String Syntax

	selection    = «range» ('&' «range»)*
	range        = «pos» | «normal_range» | «column_range»
	pos          = «line» (':' «column»)? ('+' «offset»)?
	normal_range = «pos» '-' «pos»
	column_range = «pos» 'x' «pos»
	line         = [1-9][0-9]*
	column       = [1-9][0-9]*
	offset       = [1-9][0-9]*

This can be used with `mate -l`, _Jump to Line…_, and the ⌘T dialog (by putting `:«selection string»` after the file name) — as for the latter, if a string on the find clipboard has this format, it will be the default value in ⌘T, this is useful if you have a line in Terminal like:

	main.cc:32: warning: no return statement.

You can then select `main.cc:32` press ⌘E and jump to Avian and press ⌘T followed by ↩ (one shortcoming is that ⌘T doesn’t show the currently open file, so it will only work if `main.cc` is not already the active file).
