#ifndef TEST_SUPPORT_CC_1DYF6K30
#define TEST_SUPPORT_CC_1DYF6K30

#include <parse/src/grammar.h>
#include <parse/src/parse.h>

static std::string to_s (std::string const& buf, std::map<size_t, scope::scope_t> const& scopes)
{
	if(scopes.empty())
		return buf;

	std::string res;
	res.append(xml_difference(scope::scope_t(), scopes.begin()->second, "«", "»"));
	for(auto it = scopes.begin(); it != scopes.end(); )
	{
		auto from = *it;
		auto to = ++it != scopes.end() ? *it : std::map<size_t, scope::scope_t>::value_type(buf.size(), scope::scope_t());
		res.append(buf.substr(from.first, to.first - from.first));
		res.append(xml_difference(from.second, to.second, "«", "»"));
	}
	return res;
}

static std::map<size_t, scope::scope_t> scopes_for (std::string const& buf, parse::grammar_ptr grammar)
{
	std::map<size_t, scope::scope_t> res;
	if(!grammar)
		return res;

	parse::stack_ptr parserState = grammar->seed();
	for(std::string::size_type i = 0; i != buf.size(); )
	{
		auto eol = buf.find('\n', i);
		eol = eol != std::string::npos ? ++eol : buf.size();

		std::string const line = buf.substr(i, eol - i);
		std::map<size_t, scope::scope_t> scopes;
		parserState = parse::parse(line.data(), line.data() + line.size(), parserState, scopes, i == 0);

		for(auto const& pair : scopes)
			res[i + pair.first] = pair.second;

		i = eol;
	}
	return res;
}

std::string markup (parse::grammar_ptr grammar, std::string const& buf)
{
	return to_s(buf, scopes_for(buf, grammar));
}

#endif /* end of include guard: TEST_SUPPORT_CC_1DYF6K30 */
