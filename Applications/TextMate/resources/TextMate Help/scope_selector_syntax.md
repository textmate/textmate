title: Scope Selector Syntax

# Scope Selector Syntax

	atom:         «string» | '*'
	scope:        «atom» ('.' «atom»)*
	path:         '^'? «scope» ('>'? «scope»)* '$'?
	group:        '(' «selector» ')'
	filter:       ("L:"|"R:"|"B:") («group» | «path»)
	expression:   '-'? («filter» | «group» | «path»)
	composite:    «expression» ([|&-] «expression»)*
	selector:     «composite» (',' «composite»)*

We need to add priority (to `path` rule): `(':' «integer»)?`. This is when multiple commands handle the same event and are using same scope (which can be the case when chaining e.g. documentation commands).

We probably need `~` for negative-look ahead, e.g.: `text.html ~ meta.embedded`.
