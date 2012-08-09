#include "filter_check_signature.h"
#include <text/decode.h>
#include <text/format.h>
#include <oak/debug.h>

namespace network
{
	check_signature_t::check_signature_t (key_chain_t const& keyChain, std::string const& signeeHeader, std::string const& signatureHeader) : _key_chain(keyChain), _signee_header(signeeHeader), _signature_header(signatureHeader)
	{
	}

	bool check_signature_t::setup ()
	{
		return EVP_VerifyInit(&ctx, EVP_dss1()) == 1;
	}

	bool check_signature_t::receive_header (std::string const& header, std::string const& value)
	{
		if(header == _signee_header)
			_signee = value;
		else if(header == _signature_header)
			_signature = value;
		return true;
	}

	bool check_signature_t::receive_data (char const* buf, size_t len)
	{
		return EVP_VerifyUpdate(&ctx, buf, len) == 1;
	}

	bool check_signature_t::receive_end (std::string& error)
	{
		if(_signee == NULL_STR)
			return (error = "Missing signee."), false;
		if(_signature == NULL_STR)
			return (error = "Missing signature."), false;

		if(key_chain_t::key_ptr key = _key_chain.find(_signee))
		{
			std::string signature = decode::base64(_signature);
			if(EVP_VerifyFinal(&ctx, (unsigned char*)&signature[0], signature.size(), *key) == 1)
				return true;

			error = text::format("Bad signature.");
		}
		else
		{
			error = text::format("Unknown signee: ‘%s’.", _signee.c_str());
		}

		return false;
	}

	std::string check_signature_t::name ()
	{
		return "signature";
	}

} /* network */