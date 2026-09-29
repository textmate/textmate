#include "../vendor/oniguruma.h"

__attribute__((constructor)) static void setup ()
{
	onig_init();

	static OnigSyntaxType syntax;
	onig_copy_syntax(&syntax, ONIG_SYNTAX_RUBY);
	onig_set_syntax_options(&syntax, ONIG_OPTION_OFF(syntax.options, ONIG_OPTION_ASCII_RANGE));
	onig_set_default_syntax(&syntax);
}
