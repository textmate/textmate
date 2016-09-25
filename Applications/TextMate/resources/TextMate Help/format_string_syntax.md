title: Format string syntax

# Format String Syntax

## In Snippets

### Placeholders

	$«int»
	${«int»}
	${«int»:«snippet»}
	${«int»/«regexp»/«format»/«options»}
	${«int»|«choice 1»,…,«choice n»|}

### Code

	`«code»`

## In Format Strings

	$0-n

	\U, \L, \E, \u, \l
	\n, \t

	«variables»

	(?«var»:«if»:«else»)
	(?«var»:«if»)

## In Both

### Variables

	${«var»:?«if»:«else»}
	${«var»:+«if»}
	${«var»:-«else»}
	${«var»:«else»}
	${«var»:[/upcase][/downcase][/(capitalize|titlecase)][/asciify]}
	${«var»/«regexp»/«format»/«options»}
