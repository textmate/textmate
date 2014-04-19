title: Glob String Syntax

# Glob String Syntax

	\«char»           -- Literal «char»
	?                 -- Match one character
	*                 -- Match zero or more characters¹
	**                -- Match zero or more path components
	{«a»,«b»,«c»}     -- Match «a» or «b» or «c»
	[«a»«b»«c»]       -- Match an «a», «b» or «c» character
	[«a»-«b»]         -- Match one character in the range «a»-«b»
	[^«a»-«b»]        -- Match one character not in the range «a»-«b»
	«a»!«b» / «a»~«b» -- Match «a» AND NOT «b» («a» can be empty)

Braces can be nested and contain other glob characters. Example:

	{*.{cc,mm,h},target,Makefile,.tm_properties}

Will match these files:

	source.cc
	source.mm
	source.h
	target
	Makefile
	.tm_properties

¹ The asterisk will not match slashes nor a leading period.
