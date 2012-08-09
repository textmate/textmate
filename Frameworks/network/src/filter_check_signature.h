#ifndef PUBKEY_H_VC2ABIZU
#define PUBKEY_H_VC2ABIZU

#include "download.h" // filter_t
#include "key_chain.h"

// ======================
// = Validate signature =
// ======================

namespace network
{
	struct PUBLIC check_signature_t : filter_t
	{
		check_signature_t (key_chain_t const& keyChain, std::string const& signeeHeader, std::string const& signatureHeader);

		bool setup ();
		bool receive_header (std::string const& header, std::string const& value);
		bool receive_data (char const* buf, size_t len);
		bool receive_end (std::string& error);

		std::string name ();

		std::string const& signee () const    { return _signee_header; }
		std::string const& signature () const { return _signature_header; }

	private:
		key_chain_t const _key_chain;
		std::string const _signee_header;
		std::string const _signature_header;

		EVP_MD_CTX ctx;

		std::string _signee    = NULL_STR;
		std::string _signature = NULL_STR;
	};

} /* network */

#endif /* end of include guard: PUBKEY_H_VC2ABIZU */
