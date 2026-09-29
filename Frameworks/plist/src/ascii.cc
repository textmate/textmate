
#line 1 "ascii.rl"
#include "ascii.h"
#include <text/src/format.h>
#include <oak/debug.h>

/*
array:    '(' (element ',')* (element)? ')'
dict:     '{' (key '=' value ';')* '}'
integer:  ('-'|'+')? ('0x'|'0')? [0-9]+
float:    '-'? [0-9]* '.' [0-9]+
boolean:  :true | :false
string:   ["] … ["] | ['] … ['] | [a-zA-Z_-]+
data:     <DEADBEEF>
date:     @2010-05-10 20:34:12 +0000
*/

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-const-variable"

#line 22 "ascii.cc"
static const char _string_actions[] = {
	0, 1, 0, 1, 1, 1, 3, 2, 
	0, 1, 2, 0, 3, 2, 1, 3, 
	2, 2, 1, 3, 0, 1, 3
};

static const char _string_key_offsets[] = {
	0, 0, 7, 9, 11, 13, 14, 15, 
	15, 16
};

static const char _string_trans_keys[] = {
	34, 39, 95, 65, 90, 97, 122, 34, 
	92, 34, 92, 34, 92, 39, 39, 39, 
	95, 45, 46, 48, 57, 65, 90, 97, 
	122, 0
};

static const char _string_single_lengths[] = {
	0, 3, 2, 2, 2, 1, 1, 0, 
	1, 1
};

static const char _string_range_lengths[] = {
	0, 2, 0, 0, 0, 0, 0, 0, 
	0, 4
};

static const char _string_index_offsets[] = {
	0, 0, 6, 9, 12, 15, 17, 19, 
	20, 22
};

static const char _string_trans_targs[] = {
	2, 5, 9, 9, 9, 0, 7, 4, 
	3, 7, 4, 3, 3, 3, 3, 8, 
	6, 8, 6, 0, 6, 0, 9, 9, 
	9, 9, 9, 0, 0
};

static const char _string_trans_actions[] = {
	0, 0, 19, 19, 19, 0, 10, 1, 
	7, 5, 0, 3, 3, 3, 16, 10, 
	7, 5, 3, 0, 3, 0, 13, 13, 
	13, 13, 13, 0, 0
};

static const int string_start = 1;
static const int string_first_final = 7;
static const int string_error = 0;

static const int string_en_string = 1;


#line 77 "ascii.cc"
static const char _comment_key_offsets[] = {
	0, 0, 4, 6, 7, 8, 9
};

static const char _comment_trans_keys[] = {
	32, 47, 9, 10, 42, 47, 42, 47, 
	10, 32, 47, 9, 10, 0
};

static const char _comment_single_lengths[] = {
	0, 2, 2, 1, 1, 1, 2
};

static const char _comment_range_lengths[] = {
	0, 1, 0, 0, 0, 0, 1
};

static const char _comment_index_offsets[] = {
	0, 0, 4, 7, 9, 11, 13
};

static const char _comment_trans_targs[] = {
	6, 2, 6, 0, 3, 5, 0, 4, 
	3, 6, 3, 6, 5, 6, 2, 6, 
	0, 0
};

static const int comment_start = 1;
static const int comment_first_final = 6;
static const int comment_error = 0;

static const int comment_en_comment = 1;


#line 48 "ascii.rl"

#pragma clang diagnostic pop

static bool backtrack (char const*& p, char const* bt, plist::any_t& res)
{
	return (res = plist::any_t()), (p = bt), false;
}

static bool parse_ws (char const*& p, char const* pe)
{
	int cs;
	
#line 125 "ascii.cc"
	{
	cs = comment_start;
	}

#line 130 "ascii.cc"
	{
	int _klen;
	unsigned int _trans;
	const char *_keys;

	if ( p == pe )
		goto _test_eof;
	if ( cs == 0 )
		goto _out;
_resume:
	_keys = _comment_trans_keys + _comment_key_offsets[cs];
	_trans = _comment_index_offsets[cs];

	_klen = _comment_single_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + _klen - 1;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + ((_upper-_lower) >> 1);
			if ( (*p) < *_mid )
				_upper = _mid - 1;
			else if ( (*p) > *_mid )
				_lower = _mid + 1;
			else {
				_trans += (unsigned int)(_mid - _keys);
				goto _match;
			}
		}
		_keys += _klen;
		_trans += _klen;
	}

	_klen = _comment_range_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + (_klen<<1) - 2;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + (((_upper-_lower) >> 1) & ~1);
			if ( (*p) < _mid[0] )
				_upper = _mid - 2;
			else if ( (*p) > _mid[1] )
				_lower = _mid + 2;
			else {
				_trans += (unsigned int)((_mid - _keys)>>1);
				goto _match;
			}
		}
		_trans += _klen;
	}

_match:
	cs = _comment_trans_targs[_trans];

	if ( cs == 0 )
		goto _out;
	if ( ++p != pe )
		goto _resume;
	_test_eof: {}
	_out: {}
	}

#line 60 "ascii.rl"
	return true;
}

static bool parse_char (char const*& p, char const*& pe, char ch)
{
	return parse_ws(p, pe) && p != pe && *p == ch ? (++p, true) : false;
}

static bool parse_int (char const*& p, char const* pe, plist::any_t& res)
{
	char const* bt = p;
	parse_ws(p, pe);
	if(p == pe || (!isdigit(*p) && *p != '-' && *p != '+'))
		return backtrack(p, bt, res);

	char* dummy;
	quad_t val = strtoq(p, &dummy, 0);
	p = dummy;
	if(std::clamp<quad_t>(val, INT32_MIN, INT32_MAX) == val)
			res = int32_t(val);
	else	res = uint64_t(val);

	return true;
}

static bool parse_bool (char const*& p, char const* pe, plist::any_t& res)
{
	char const* bt = p;
	parse_ws(p, pe);
	size_t bytes = pe - p;
	if(bytes >= 5 && strncmp(p, ":true", 5) == 0)
		return (res = true), (p += 5), true;
	if(bytes >= 6 && strncmp(p, ":false", 6) == 0)
		return (res = false), (p += 6), true;
	return backtrack(p, bt, res);
}

static bool parse_string (char const*& p, char const* pe, plist::any_t& res)
{
	int cs;
	char const* bt = p;
	bool matched = false;
	std::string& strBuf = boost::get<std::string>(res = std::string());

	parse_ws(p, pe);
	
#line 247 "ascii.cc"
	{
	cs = string_start;
	}

#line 252 "ascii.cc"
	{
	int _klen;
	unsigned int _trans;
	const char *_acts;
	unsigned int _nacts;
	const char *_keys;

	if ( p == pe )
		goto _test_eof;
	if ( cs == 0 )
		goto _out;
_resume:
	_keys = _string_trans_keys + _string_key_offsets[cs];
	_trans = _string_index_offsets[cs];

	_klen = _string_single_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + _klen - 1;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + ((_upper-_lower) >> 1);
			if ( (*p) < *_mid )
				_upper = _mid - 1;
			else if ( (*p) > *_mid )
				_lower = _mid + 1;
			else {
				_trans += (unsigned int)(_mid - _keys);
				goto _match;
			}
		}
		_keys += _klen;
		_trans += _klen;
	}

	_klen = _string_range_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + (_klen<<1) - 2;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + (((_upper-_lower) >> 1) & ~1);
			if ( (*p) < _mid[0] )
				_upper = _mid - 2;
			else if ( (*p) > _mid[1] )
				_lower = _mid + 2;
			else {
				_trans += (unsigned int)((_mid - _keys)>>1);
				goto _match;
			}
		}
		_trans += _klen;
	}

_match:
	cs = _string_trans_targs[_trans];

	if ( _string_trans_actions[_trans] == 0 )
		goto _again;

	_acts = _string_actions + _string_trans_actions[_trans];
	_nacts = (unsigned int) *_acts++;
	while ( _nacts-- > 0 )
	{
		switch ( *_acts++ )
		{
	case 0:
#line 22 "ascii.rl"
	{ strBuf.clear(); }
	break;
	case 1:
#line 23 "ascii.rl"
	{ strBuf.push_back((*p)); }
	break;
	case 2:
#line 24 "ascii.rl"
	{ strBuf.push_back('\\'); }
	break;
	case 3:
#line 25 "ascii.rl"
	{ matched = true; }
	break;
#line 341 "ascii.cc"
		}
	}

_again:
	if ( cs == 0 )
		goto _out;
	if ( ++p != pe )
		goto _resume;
	_test_eof: {}
	_out: {}
	}

#line 106 "ascii.rl"
	return matched || backtrack(p, bt, res);
}

static bool parse_date (char const*& p, char const* pe, plist::any_t& res)
{
	char const* bt = p;
	if(!parse_char(p, pe, '@'))
		return backtrack(p, bt, res);

	size_t bytes = pe - p;
	if(bytes >= 25)
	{
		oak::date_t date(std::string(p, p + 25));
		if(date)
		{
			res = date;
			p += 25;
			return true;
		}
	}
	return backtrack(p, bt, res);
}

static bool parse_element (char const*& p, char const* pe, plist::any_t& res);

static bool parse_array (char const*& p, char const* pe, plist::any_t& res)
{
	// '(' (element ',')* (element)? ')'
	char const* bt = p;
	if(!parse_char(p, pe, '('))
		return backtrack(p, bt, res);

	plist::any_t element;
	std::vector<plist::any_t>& ref = boost::get< std::vector<plist::any_t> >(res = std::vector<plist::any_t>());
	while(parse_element(p, pe, element))
	{
		ref.push_back(element);
		if(!parse_char(p, pe, ','))
			break;
	}
	return parse_char(p, pe, ')') || backtrack(p, bt, res);
}

static bool parse_key (char const*& p, char const* pe, plist::any_t& res)
{
	plist::any_t tmp;
	if(!parse_element(p, pe, tmp))
		return false;
	res = plist::get<std::string>(tmp);
	return !boost::get<std::string>(res).empty();
}

static bool parse_dict (char const*& p, char const* pe, plist::any_t& res)
{
	// '{' (key '=' value ';')* '}'
	char const* bt = p;
	if(!parse_char(p, pe, '{'))
		return backtrack(p, bt, res);

	plist::any_t key, value;
	std::map<std::string, plist::any_t>& ref = boost::get< std::map<std::string, plist::any_t> >(res = std::map<std::string, plist::any_t>());
	for(char const* lp = p; parse_key(lp, pe, key) && parse_char(lp, pe, '=') && parse_element(lp, pe, value) && parse_char(lp, pe, ';'); p = lp)
		ref.emplace(boost::get<std::string>(key), value);

	return parse_char(p, pe, '}') || backtrack(p, bt, res);
}

static bool parse_element (char const*& p, char const* pe, plist::any_t& res)
{
	return parse_string(p, pe, res) || parse_int(p, pe, res) || parse_bool(p, pe, res) || parse_date(p, pe, res) || parse_dict(p, pe, res) || parse_array(p, pe, res);
}

namespace plist
{
	plist::any_t parse_ascii (std::string const& str, bool* success)
	{
		plist::any_t res;
		char const* p  = str.data();
		char const* pe = p + str.size();
		bool didParse = parse_element(p, pe, res) && parse_ws(p, pe) && p == pe;
		if(success)
			*success = didParse;
		return didParse ? res : plist::any_t();
	}

} /* plist */
