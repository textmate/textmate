#include "../vendor/oniguruma.h"

static pthread_mutex_t OnigMutex;

void onig_lock_mutex ()   { pthread_mutex_lock(&OnigMutex); }
void onig_unlock_mutex () { pthread_mutex_unlock(&OnigMutex); }

__attribute__((constructor)) static void setup ()
{
	static OnigSyntaxType syntax;
	onig_copy_syntax(&syntax, ONIG_SYNTAX_RUBY);
	onig_set_syntax_options(&syntax, ONIG_OPTION_OFF(syntax.options, ONIG_OPTION_ASCII_RANGE));
	onig_set_default_syntax(&syntax);

	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&OnigMutex, &attr);
	pthread_mutexattr_destroy(&attr);
}
