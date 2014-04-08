#include <test/bundle_index.h>

void setup_fixtures ()
{
	static std::string TextLanguageGrammar =
		"{	fileTypes      = ( txt );\n"
		"	keyEquivalent  = '^~P';\n"
		"	name           = 'Plain Text';\n"
		"	patterns       = ( );\n"
		"	scopeName      = 'text.plain';\n"
		"	uuid           = '3130E4FA-B10E-11D9-9F75-000D93589AF6';\n"
		"}\n";

	static std::string CLanguageGrammar =
		"{	fileTypes      = ( c, h );\n"
		"	keyEquivalent  = '^~C';\n"
		"	name           = 'C';\n"
		"	firstLineMatch = '-[*]-( Mode:)? C -[*]-';\n"
		"	patterns       = ( );\n"
		"	scopeName      = 'source.c';\n"
		"	uuid           = '25066DC2-6B1D-11D9-9D5B-000D93589AF6';\n"
		"}\n";

	static std::string XMLPlistGrammar =
		"{	name           = 'XML Plist';"
		"	firstLineMatch = '\\A<\\?xml version=\"1.0\" encoding=\"UTF-8\"\\?>\n<!DOCTYPE plist PUBLIC \"-//Apple( Computer)?//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">';"
		"	patterns       = ( );"
		"	scopeName      = 'source.xml.plist';"
		"}";

	static std::string ASCIIPlistGrammar =
		"{	fileTypes      = ( plist, dict );"
		"	name           = 'ASCII Plist';"
		"	patterns       = ( );"
		"	scopeName      = 'source.plist';"
		"}";

	static std::string ExportSHA1Command =
		"{	command = \"#!/bin/sh\n/usr/bin/openssl dgst -sha1 -binary\n\";\n"
		"	semanticClass = 'callback.document.binary-export';\n"
		"	input  = 'document';\n"
		"	name   = 'Hash Document';\n"
		"	output = 'replaceDocument';\n"
		"	scope  = 'attr.rev-path.sha1';\n"
		"	uuid   = 'E2C42B70-5823-49A7-A259-A1622EBD191C';\n"
		"}\n";

	test::bundle_index_t bundleIndex;
	bundleIndex.add(bundles::kItemTypeGrammar, TextLanguageGrammar);
	bundleIndex.add(bundles::kItemTypeGrammar, CLanguageGrammar);
	bundleIndex.add(bundles::kItemTypeGrammar, XMLPlistGrammar);
	bundleIndex.add(bundles::kItemTypeGrammar, ASCIIPlistGrammar);
	bundleIndex.add(bundles::kItemTypeCommand, ExportSHA1Command);
	bundleIndex.commit();
}
