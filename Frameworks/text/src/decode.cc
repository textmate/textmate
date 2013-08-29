#include "decode.h"
#include <oak/oak.h>

static std::map<std::string, std::string> const EntityNameMap =
{
	{ "AElig",    "Æ"      },
	{ "Aacute",   "Á"      },
	{ "Acirc",    "Â"      },
	{ "Agrave",   "À"      },
	{ "Alpha",    "Α"      },
	{ "Aring",    "Å"      },
	{ "Atilde",   "Ã"      },
	{ "Auml",     "Ä"      },
	{ "Beta",     "Β"      },
	{ "Ccedil",   "Ç"      },
	{ "Chi",      "Χ"      },
	{ "Dagger",   "‡"      },
	{ "Delta",    "Δ"      },
	{ "ETH",      "Ð"      },
	{ "Eacute",   "É"      },
	{ "Ecirc",    "Ê"      },
	{ "Egrave",   "È"      },
	{ "Epsilon",  "Ε"      },
	{ "Eta",      "Η"      },
	{ "Euml",     "Ë"      },
	{ "Gamma",    "Γ"      },
	{ "Iacute",   "Í"      },
	{ "Icirc",    "Î"      },
	{ "Igrave",   "Ì"      },
	{ "Iota",     "Ι"      },
	{ "Iuml",     "Ï"      },
	{ "Kappa",    "Κ"      },
	{ "Lambda",   "Λ"      },
	{ "Mu",       "Μ"      },
	{ "Ntilde",   "Ñ"      },
	{ "Nu",       "Ν"      },
	{ "OElig",    "Œ"      },
	{ "Oacute",   "Ó"      },
	{ "Ocirc",    "Ô"      },
	{ "Ograve",   "Ò"      },
	{ "Omega",    "Ω"      },
	{ "Omicron",  "Ο"      },
	{ "Oslash",   "Ø"      },
	{ "Otilde",   "Õ"      },
	{ "Ouml",     "Ö"      },
	{ "Phi",      "Φ"      },
	{ "Pi",       "Π"      },
	{ "Prime",    "″"      },
	{ "Psi",      "Ψ"      },
	{ "Rho",      "Ρ"      },
	{ "Scaron",   "Š"      },
	{ "Sigma",    "Σ"      },
	{ "THORN",    "Þ"      },
	{ "Tau",      "Τ"      },
	{ "Theta",    "Θ"      },
	{ "Uacute",   "Ú"      },
	{ "Ucirc",    "Û"      },
	{ "Ugrave",   "Ù"      },
	{ "Upsilon",  "Υ"      },
	{ "Uuml",     "Ü"      },
	{ "Xi",       "Ξ"      },
	{ "Yacute",   "Ý"      },
	{ "Yuml",     "Ÿ"      },
	{ "Zeta",     "Ζ"      },
	{ "aacute",   "á"      },
	{ "acirc",    "â"      },
	{ "acute",    "´"      },
	{ "aelig",    "æ"      },
	{ "agrave",   "à"      },
	{ "alefsym",  "ℵ"      },
	{ "alpha",    "α"      },
	{ "amp",      "&"      },
	{ "and",      "∧"      },
	{ "ang",      "∠"      },
	{ "apos",     "'"      },
	{ "aring",    "å"      },
	{ "asymp",    "≈"      },
	{ "atilde",   "ã"      },
	{ "auml",     "ä"      },
	{ "bdquo",    "„"      },
	{ "beta",     "β"      },
	{ "brvbar",   "¦"      },
	{ "bull",     "•"      },
	{ "cap",      "∩"      },
	{ "ccedil",   "ç"      },
	{ "cedil",    "¸"      },
	{ "cent",     "¢"      },
	{ "chi",      "χ"      },
	{ "circ",     "ˆ"      },
	{ "clubs",    "♣"      },
	{ "cong",     "≅"      },
	{ "copy",     "©"      },
	{ "crarr",    "↵"      },
	{ "cup",      "∪"      },
	{ "curren",   "¤"      },
	{ "dArr",     "⇓"      },
	{ "dagger",   "†"      },
	{ "darr",     "↓"      },
	{ "deg",      "°"      },
	{ "delta",    "δ"      },
	{ "diams",    "♦"      },
	{ "divide",   "÷"      },
	{ "eacute",   "é"      },
	{ "ecirc",    "ê"      },
	{ "egrave",   "è"      },
	{ "empty",    "∅"      },
	{ "emsp",     "\u2003" },
	{ "ensp",     "\u2002" },
	{ "epsilon",  "ε"      },
	{ "equiv",    "≡"      },
	{ "eta",      "η"      },
	{ "eth",      "ð"      },
	{ "euml",     "ë"      },
	{ "euro",     "€"      },
	{ "exist",    "∃"      },
	{ "fnof",     "ƒ"      },
	{ "forall",   "∀"      },
	{ "frac12",   "½"      },
	{ "frac14",   "¼"      },
	{ "frac34",   "¾"      },
	{ "frasl",    "⁄"      },
	{ "gamma",    "γ"      },
	{ "ge",       "≥"      },
	{ "gt",       ">"      },
	{ "hArr",     "⇔"      },
	{ "harr",     "↔"      },
	{ "hearts",   "♥"      },
	{ "hellip",   "…"      },
	{ "iacute",   "í"      },
	{ "icirc",    "î"      },
	{ "iexcl",    "¡"      },
	{ "igrave",   "ì"      },
	{ "image",    "ℑ"      },
	{ "infin",    "∞"      },
	{ "int",      "∫"      },
	{ "iota",     "ι"      },
	{ "iquest",   "¿"      },
	{ "isin",     "∈"      },
	{ "iuml",     "ï"      },
	{ "kappa",    "κ"      },
	{ "lArr",     "⇐"      },
	{ "lambda",   "λ"      },
	{ "lang",     "〈"      },
	{ "laquo",    "«"      },
	{ "larr",     "←"      },
	{ "lceil",    "⌈"      },
	{ "ldquo",    "“"      },
	{ "le",       "≤"      },
	{ "lfloor",   "⌊"      },
	{ "lowast",   "∗"      },
	{ "loz",      "◊"      },
	{ "lrm",      "\u200E" },
	{ "lsaquo",   "‹"      },
	{ "lsquo",    "‘"      },
	{ "lt",       "<"      },
	{ "macr",     "¯"      },
	{ "mdash",    "—"      },
	{ "micro",    "µ"      },
	{ "middot",   "·"      },
	{ "minus",    "−"      },
	{ "mu",       "μ"      },
	{ "nabla",    "∇"      },
	{ "nbsp",     "\u00A0" },
	{ "ndash",    "–"      },
	{ "ne",       "≠"      },
	{ "ni",       "∋"      },
	{ "not",      "¬"      },
	{ "notin",    "∉"      },
	{ "nsub",     "⊄"      },
	{ "ntilde",   "ñ"      },
	{ "nu",       "ν"      },
	{ "oacute",   "ó"      },
	{ "ocirc",    "ô"      },
	{ "oelig",    "œ"      },
	{ "ograve",   "ò"      },
	{ "oline",    "‾"      },
	{ "omega",    "ω"      },
	{ "omicron",  "ο"      },
	{ "oplus",    "⊕"      },
	{ "or",       "∨"      },
	{ "ordf",     "ª"      },
	{ "ordm",     "º"      },
	{ "oslash",   "ø"      },
	{ "otilde",   "õ"      },
	{ "otimes",   "⊗"      },
	{ "ouml",     "ö"      },
	{ "para",     "¶"      },
	{ "part",     "∂"      },
	{ "permil",   "‰"      },
	{ "perp",     "⊥"      },
	{ "phi",      "φ"      },
	{ "pi",       "π"      },
	{ "piv",      "ϖ"      },
	{ "plusmn",   "±"      },
	{ "pound",    "£"      },
	{ "prime",    "′"      },
	{ "prod",     "∏"      },
	{ "prop",     "∝"      },
	{ "psi",      "ψ"      },
	{ "quot",     "\""     },
	{ "rArr",     "⇒"      },
	{ "radic",    "√"      },
	{ "rang",     "〉"      },
	{ "raquo",    "»"      },
	{ "rarr",     "→"      },
	{ "rceil",    "⌉"      },
	{ "rdquo",    "”"      },
	{ "real",     "ℜ"      },
	{ "reg",      "®"      },
	{ "rfloor",   "⌋"      },
	{ "rho",      "ρ"      },
	{ "rlm",      "\u200F" },
	{ "rsaquo",   "›"      },
	{ "rsquo",    "’"      },
	{ "sbquo",    "‚"      },
	{ "scaron",   "š"      },
	{ "sdot",     "⋅"      },
	{ "sect",     "§"      },
	{ "shy",      "\u00AD" },
	{ "sigma",    "σ"      },
	{ "sigmaf",   "ς"      },
	{ "sim",      "∼"      },
	{ "spades",   "♠"      },
	{ "sub",      "⊂"      },
	{ "sube",     "⊆"      },
	{ "sum",      "∑"      },
	{ "sup1",     "¹"      },
	{ "sup2",     "²"      },
	{ "sup3",     "³"      },
	{ "sup",      "⊃"      },
	{ "supe",     "⊇"      },
	{ "szlig",    "ß"      },
	{ "tau",      "τ"      },
	{ "there4",   "∴"      },
	{ "theta",    "θ"      },
	{ "thetasym", "ϑ"      },
	{ "thinsp",   " "      },
	{ "thorn",    "þ"      },
	{ "tilde",    "˜"      },
	{ "times",    "×"      },
	{ "trade",    "™"      },
	{ "uArr",     "⇑"      },
	{ "uacute",   "ú"      },
	{ "uarr",     "↑"      },
	{ "ucirc",    "û"      },
	{ "ugrave",   "ù"      },
	{ "uml",      "¨"      },
	{ "upsih",    "ϒ"      },
	{ "upsilon",  "υ"      },
	{ "uuml",     "ü"      },
	{ "weierp",   "℘"      },
	{ "xi",       "ξ"      },
	{ "yacute",   "ý"      },
	{ "yen",      "¥"      },
	{ "yuml",     "ÿ"      },
	{ "zeta",     "ζ"      },
	{ "zwj",      "\u200D" },
	{ "zwnj",     "\u200C" }
};

static std::string convert_entity (std::string const& str)
{
	auto it = EntityNameMap.find(str);
	return it != EntityNameMap.end() ? it->second : NULL_STR;
}

namespace decode
{
	std::string base32 (std::string const& src)
	{
		std::string dst = "";

		uint32_t value = 0, bits = 0;
		iterate(it, src)
		{
			char ch = toupper(*it);
			if('A' <= ch && ch <= 'Z')
				value = (value << 5) | ch-'A';
			else if('2' <= ch && ch <= '7')
				value = (value << 5) | (('Z'-'A'+1) + ch-'2');
			else
				continue;

			bits += 5;
			for(; bits >= 8; bits -= 8)
				dst.push_back((value >> (bits-8)) & 255);
		}
		return dst;
	}

	std::string base64 (std::string const& src)
	{
		std::string dst = "";

		uint32_t value = 0, bits = 0;
		iterate(it, src)
		{
			static char const Table[65] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
			uint32_t i = std::find(Table, Table + 64, *it) - Table;
			if(i == 64)
				continue;
			value = (value << 6) | i;
			bits += 6;
			for(; bits >= 8; bits -= 8)
				dst.push_back((value >> (bits-8)) & 255);
		}

		return dst;
	}

	std::string rot13 (std::string src)
	{
		iterate(ch, src)
		{
			if('A' <= *ch && *ch <= 'Z')
				*ch = *ch-13 >= 'A' ? *ch-13 : *ch-13+'Z'+1-'A';
			else if('a' <= *ch && *ch <= 'z')
				*ch = *ch-13 >= 'a' ? *ch-13 : *ch-13+'z'+1-'a';
		}
		return src;
	}

	std::string entities (std::string const& src)
	{
		std::string res;
		std::string::size_type i = 0;
		while(true)
		{
			std::string::size_type from = src.find('&', i);
			std::string::size_type to   = src.find(';', from);
			res.append(src.substr(i, from-i));

			if(from == std::string::npos)
				break;

			if(to == std::string::npos)
			{
				res.append(src.substr(from));
				break;
			}

			std::string ch = convert_entity(src.substr(from + 1, to - from - 1));
			if(ch == NULL_STR)
			{
				res.append("&");
				i = from + 1;
				continue;
			}

			res.append(ch);
			i = to + 1;
		}
		return res;
	}

	std::string url_part (std::string const& src)
	{
		std::string res = "";
		for(size_t i = 0; i < src.size(); ++i)
		{
			if(src[i] == '%' && i + 2 < src.size() && isxdigit(src[i+1]) && isxdigit(src[i+2]))
			{
				res.append(1, digittoint(src[i+1]) << 4 | digittoint(src[i+2]));
				i += 2;
			}
			else
			{
				res.append(1, src[i] == '+' ? ' ' : src[i]);
			}
		}
		return res;
	}

} /* decode */
