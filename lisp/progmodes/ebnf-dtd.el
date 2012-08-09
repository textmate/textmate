;;; ebnf-dtd.el --- parser for DTD (Data Type Description for XML)

;; Copyright (C) 2001-2012  Free Software Foundation, Inc.

;; Author: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Maintainer: Vinicius Jose Latorre <viniciusjl@ig.com.br>
;; Keywords: wp, ebnf, PostScript
;; Version: 1.1
;; Package: ebnf2ps

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; This is part of ebnf2ps package.
;;
;; This package defines a parser for DTD (Data Type Description for XML).
;;
;; See ebnf2ps.el for documentation.
;;
;;
;; DTD Syntax
;; ----------
;;
;;	See the URLs:
;;	`http://www.w3.org/TR/2004/REC-xml-20040204/'
;;	(Extensible Markup Language (XML) 1.0 (Third Edition))
;;	`http://www.w3.org/TR/html40/'
;;	(HTML 4.01 Specification)
;;	`http://www.w3.org/TR/NOTE-html-970421'
;;	(HTML DTD with support for Style Sheets)
;;
;;
;; /* Document */
;;
;; document ::= prolog element Misc*
;; /* Note that *only* the prolog will be parsed */
;;
;;
;; /* Characters */
;;
;; Char ::= #x9 | #xA | #xD
;;        | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
;; /* any Unicode character, excluding the surrogate blocks, FFFE, and FFFF. */
;;
;; /* NOTE:
;;
;;    Document authors are encouraged to avoid "compatibility characters", as
;;    defined in section 6.8 of [Unicode] (see also D21 in section 3.6 of
;;    [Unicode3]). The characters defined in the following ranges are also
;;    discouraged. They are either control characters or permanently undefined
;;    Unicode characters:
;;
;;    [#x7F-#x84],      [#x86-#x9F],      [#xFDD0-#xFDDF],
;;    [#1FFFE-#x1FFFF], [#2FFFE-#x2FFFF], [#3FFFE-#x3FFFF],
;;    [#4FFFE-#x4FFFF], [#5FFFE-#x5FFFF], [#6FFFE-#x6FFFF],
;;    [#7FFFE-#x7FFFF], [#8FFFE-#x8FFFF], [#9FFFE-#x9FFFF],
;;    [#AFFFE-#xAFFFF], [#BFFFE-#xBFFFF], [#CFFFE-#xCFFFF],
;;    [#DFFFE-#xDFFFF], [#EFFFE-#xEFFFF], [#FFFFE-#xFFFFF],
;;    [#10FFFE-#x10FFFF]. */
;;
;;
;; /* White Space */
;;
;; S ::= (#x20 | #x9 | #xD | #xA)+
;;
;;
;; /* Names and Tokens */
;;
;; NameChar ::= Letter | Digit | '.' | '-' | '_' | ':'
;;            | CombiningChar | Extender
;;
;; Name ::= (Letter | '_' | ':') (NameChar)*
;;
;; Names ::= Name (#x20 Name)*
;;
;; Nmtoken ::= (NameChar)+
;;
;; Nmtokens ::= Nmtoken (#x20 Nmtoken)*
;;
;;
;; /* Literals */
;;
;; EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
;;               | "'" ([^%&'] | PEReference | Reference)* "'"
;;
;; AttValue ::= '"' ([^<&"] | Reference)* '"'
;;            | "'" ([^<&'] | Reference)* "'"
;;
;; SystemLiteral ::= ('"' [^"]* '"')
;;                 | ("'" [^']* "'")
;;
;; PubidLiteral ::= '"' PubidChar* '"'
;;                | "'" (PubidChar - "'")* "'"
;;
;; PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
;;
;; /* NOTE:
;;
;;    Although the EntityValue production allows the definition of a general
;;    entity consisting of a single explicit < in the literal (e.g., <!ENTITY
;;    mylt "<">), it is strongly advised to avoid this practice since any
;;    reference to that entity will cause a well-formedness error. */
;;
;;
;; /* Character Data */
;;
;; CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
;;
;;
;; /* Comments */
;;
;; Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
;;
;;
;; /* Processing Instructions */
;;
;; PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
;;
;; PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
;;
;;
;; /* CDATA Sections */
;;
;; CDSect ::= CDStart CData CDEnd
;;
;; CDStart ::= '<![CDATA['
;;
;; CData ::= (Char* - (Char* ']]>' Char*))
;;
;; CDEnd ::= ']]>'
;;
;;
;; /* Prolog */
;;
;; prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
;;
;; XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
;;
;; VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
;;
;; Eq ::= S? '=' S?
;;
;; VersionNum ::= '1.0'
;;
;; Misc ::= Comment | PI | S
;;
;;
;; /* Document Type Definition */
;;
;; doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
;;                 ('[' intSubset ']' S?)? '>'
;;               [VC: Root Element Type]
;;               [WFC: External Subset]
;;
;; DeclSep ::= PEReference | S
;;           [WFC: PE Between Declarations]
;;
;; intSubset ::= (markupdecl | DeclSep)*
;;
;; markupdecl ::= elementdecl | AttlistDecl | EntityDecl
;;              | NotationDecl | PI | Comment
;;              [VC: Proper Declaration/PE Nesting]
;;              [WFC: PEs in Internal Subset]
;;
;;
;; /* External Subset */
;;
;; extSubset ::= TextDecl? extSubsetDecl
;;
;; extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
;;
;;
;; /* Standalone Document Declaration */
;;
;; SDDecl ::= S 'standalone' Eq
;;            (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
;;          [VC: Standalone Document Declaration]
;;
;;
;; /* Element */
;;
;; element ::= EmptyElemTag | STag content ETag
;;           [WFC: Element Type Match]
;;           [VC: Element Valid]
;;
;;
;; /* Start-tag */
;;
;; STag ::= '<' Name (S Attribute)* S? '>'
;;        [WFC: Unique Att Spec]
;;
;; Attribute ::= Name Eq AttValue
;;             [VC: Attribute Value Type]
;;             [WFC: No External Entity References]
;;             [WFC: No < in Attribute Values]
;;
;;
;; /* End-tag */
;;
;; ETag ::= '</' Name S? '>'
;;
;;
;; /* Content of Elements */
;;
;; content ::= CharData?
;;             ((element | Reference | CDSect | PI | Comment) CharData?)*
;;
;;
;; /* Tags for Empty Elements */
;;
;; EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
;;                [WFC: Unique Att Spec]
;;
;;
;; /* Element Type Declaration */
;;
;; elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
;;               [VC: Unique Element Type Declaration]
;;
;; contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
;;
;;
;; /* Element-content Models */
;;
;; children ::= (choice | seq) ('?' | '*' | '+')?
;;
;; cp ::= (Name | choice | seq) ('?' | '*' | '+')?
;;
;; choice ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
;;          [VC: Proper Group/PE Nesting]
;;
;; seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
;;       [VC: Proper Group/PE Nesting]
;;
;;
;; /* Mixed-content Declaration */
;;
;; Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
;;         | '(' S? '#PCDATA' S? ')'
;;         [VC: Proper Group/PE Nesting]
;;         [VC: No Duplicate Types]
;;
;;
;; /* Attribute-list Declaration */
;;
;; AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
;;
;; AttDef ::= S Name S AttType S DefaultDecl
;;
;;
;; /* Attribute Types */
;;
;; AttType ::= StringType | TokenizedType | EnumeratedType
;;
;; StringType ::= 'CDATA'
;;
;; TokenizedType ::= 'ID'       [VC: ID]
;;                              [VC: One ID per Element Type]
;;                              [VC: ID Attribute Default]
;;                 | 'IDREF'    [VC: IDREF]
;;                 | 'IDREFS'   [VC: IDREF]
;;                 | 'ENTITY'   [VC: Entity Name]
;;                 | 'ENTITIES' [VC: Entity Name]
;;                 | 'NMTOKEN'  [VC: Name Token]
;;                 | 'NMTOKENS' [VC: Name Token]
;;
;;
;; /* Enumerated Attribute Types */
;;
;; EnumeratedType ::= NotationType | Enumeration
;;
;; NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
;;                [VC: Notation Attributes]
;;                [VC: One Notation Per Element Type]
;;                [VC: No Notation on Empty Element]
;;                [VC: No Duplicate Tokens]
;;
;; Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
;;               [VC: Enumeration]
;;               [VC: No Duplicate Tokens]
;;
;;
;; /* Attribute Defaults */
;;
;; DefaultDecl ::= '#REQUIRED' | '#IMPLIED'
;;               | (('#FIXED' S)? AttValue)
;;               [VC: Required Attribute]
;;               [VC: Attribute Default Value Syntactically Correct]
;;               [WFC: No < in Attribute Values]
;;               [VC: Fixed Attribute Default]
;;
;;
;; /* Conditional Section */
;;
;; conditionalSect ::= includeSect | ignoreSect
;;
;; includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
;;               [VC: Proper Conditional Section/PE Nesting]
;;
;; ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
;;              [VC: Proper Conditional Section/PE Nesting]
;;
;; ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
;;
;; Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)
;;
;;
;; /* Character Reference */
;;
;; CharRef ::= '&#' [0-9]+ ';'
;;           | '&#x' [0-9a-fA-F]+ ';'
;;           [WFC: Legal Character]
;;
;;
;; /* Entity Reference */
;;
;; Reference ::= EntityRef | CharRef
;;
;; EntityRef ::= '&' Name ';'
;;             [WFC: Entity Declared]
;;             [VC: Entity Declared]
;;             [WFC: Parsed Entity]
;;             [WFC: No Recursion]
;;
;; PEReference ::= '%' Name ';'
;;               [VC: Entity Declared]
;;               [WFC: No Recursion]
;;               [WFC: In DTD]
;;
;;
;; /* Entity Declaration */
;;
;; EntityDecl ::= GEDecl | PEDecl
;;
;; GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
;;
;; PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
;;
;; EntityDef ::= EntityValue | (ExternalID NDataDecl?)
;;
;; PEDef ::= EntityValue | ExternalID
;;
;;
;; /* External Entity Declaration */
;;
;; ExternalID ::= 'SYSTEM' S SystemLiteral
;;              | 'PUBLIC' S PubidLiteral S SystemLiteral
;;
;; NDataDecl ::= S 'NDATA' S Name
;;             [VC: Notation Declared]
;;
;;
;; /* Text Declaration */
;;
;; TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
;;
;;
;; /* Well-Formed External Parsed Entity */
;;
;; extParsedEnt ::= TextDecl? content
;;
;;
;; /* Encoding Declaration */
;;
;; EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
;;
;; EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
;; /* Encoding name contains only Latin characters */
;;
;;
;; /* Notation Declarations */
;;
;; NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
;;                [VC: Unique Notation Name]
;;
;; PublicID ::= 'PUBLIC' S PubidLiteral
;;
;;
;; /* Characters */
;;
;; Letter ::= BaseChar | Ideographic
;;
;; BaseChar ::= [#x0041-#x005A] | [#x0061-#x007A] | [#x00C0-#x00D6]
;;            | [#x00D8-#x00F6] | [#x00F8-#x00FF] | [#x0100-#x0131]
;;            | [#x0134-#x013E] | [#x0141-#x0148] | [#x014A-#x017E]
;;            | [#x0180-#x01C3] | [#x01CD-#x01F0] | [#x01F4-#x01F5]
;;            | [#x01FA-#x0217] | [#x0250-#x02A8] | [#x02BB-#x02C1]
;;            | #x0386          | [#x0388-#x038A] | #x038C
;;            | [#x038E-#x03A1] | [#x03A3-#x03CE] | [#x03D0-#x03D6]
;;            | #x03DA          | #x03DC          | #x03DE
;;            | #x03E0          | [#x03E2-#x03F3] | [#x0401-#x040C]
;;            | [#x040E-#x044F] | [#x0451-#x045C] | [#x045E-#x0481]
;;            | [#x0490-#x04C4] | [#x04C7-#x04C8] | [#x04CB-#x04CC]
;;            | [#x04D0-#x04EB] | [#x04EE-#x04F5] | [#x04F8-#x04F9]
;;            | [#x0531-#x0556] | #x0559          | [#x0561-#x0586]
;;            | [#x05D0-#x05EA] | [#x05F0-#x05F2] | [#x0621-#x063A]
;;            | [#x0641-#x064A] | [#x0671-#x06B7] | [#x06BA-#x06BE]
;;            | [#x06C0-#x06CE] | [#x06D0-#x06D3] | #x06D5
;;            | [#x06E5-#x06E6] | [#x0905-#x0939] | #x093D
;;            | [#x0958-#x0961] | [#x0985-#x098C] | [#x098F-#x0990]
;;            | [#x0993-#x09A8] | [#x09AA-#x09B0] | #x09B2
;;            | [#x09B6-#x09B9] | [#x09DC-#x09DD] | [#x09DF-#x09E1]
;;            | [#x09F0-#x09F1] | [#x0A05-#x0A0A] | [#x0A0F-#x0A10]
;;            | [#x0A13-#x0A28] | [#x0A2A-#x0A30] | [#x0A32-#x0A33]
;;            | [#x0A35-#x0A36] | [#x0A38-#x0A39] | [#x0A59-#x0A5C]
;;            | #x0A5E          | [#x0A72-#x0A74] | [#x0A85-#x0A8B]
;;            | #x0A8D          | [#x0A8F-#x0A91] | [#x0A93-#x0AA8]
;;            | [#x0AAA-#x0AB0] | [#x0AB2-#x0AB3] | [#x0AB5-#x0AB9]
;;            | #x0ABD          | #x0AE0          | [#x0B05-#x0B0C]
;;            | [#x0B0F-#x0B10] | [#x0B13-#x0B28] | [#x0B2A-#x0B30]
;;            | [#x0B32-#x0B33] | [#x0B36-#x0B39] | #x0B3D
;;            | [#x0B5C-#x0B5D] | [#x0B5F-#x0B61] | [#x0B85-#x0B8A]
;;            | [#x0B8E-#x0B90] | [#x0B92-#x0B95] | [#x0B99-#x0B9A]
;;            | #x0B9C          | [#x0B9E-#x0B9F] | [#x0BA3-#x0BA4]
;;            | [#x0BA8-#x0BAA] | [#x0BAE-#x0BB5] | [#x0BB7-#x0BB9]
;;            | [#x0C05-#x0C0C] | [#x0C0E-#x0C10] | [#x0C12-#x0C28]
;;            | [#x0C2A-#x0C33] | [#x0C35-#x0C39] | [#x0C60-#x0C61]
;;            | [#x0C85-#x0C8C] | [#x0C8E-#x0C90] | [#x0C92-#x0CA8]
;;            | [#x0CAA-#x0CB3] | [#x0CB5-#x0CB9] | #x0CDE
;;            | [#x0CE0-#x0CE1] | [#x0D05-#x0D0C] | [#x0D0E-#x0D10]
;;            | [#x0D12-#x0D28] | [#x0D2A-#x0D39] | [#x0D60-#x0D61]
;;            | [#x0E01-#x0E2E] | #x0E30          | [#x0E32-#x0E33]
;;            | [#x0E40-#x0E45] | [#x0E81-#x0E82] | #x0E84
;;            | [#x0E87-#x0E88] | #x0E8A          | #x0E8D
;;            | [#x0E94-#x0E97] | [#x0E99-#x0E9F] | [#x0EA1-#x0EA3]
;;            | #x0EA5          | #x0EA7          | [#x0EAA-#x0EAB]
;;            | [#x0EAD-#x0EAE] | #x0EB0          | [#x0EB2-#x0EB3]
;;            | #x0EBD          | [#x0EC0-#x0EC4] | [#x0F40-#x0F47]
;;            | [#x0F49-#x0F69] | [#x10A0-#x10C5] | [#x10D0-#x10F6]
;;            | #x1100          | [#x1102-#x1103] | [#x1105-#x1107]
;;            | #x1109          | [#x110B-#x110C] | [#x110E-#x1112]
;;            | #x113C          | #x113E          | #x1140
;;            | #x114C          | #x114E          | #x1150
;;            | [#x1154-#x1155] | #x1159          | [#x115F-#x1161]
;;            | #x1163          | #x1165          | #x1167
;;            | #x1169          | [#x116D-#x116E] | [#x1172-#x1173]
;;            | #x1175          | #x119E          | #x11A8
;;            | #x11AB          | [#x11AE-#x11AF] | [#x11B7-#x11B8]
;;            | #x11BA          | [#x11BC-#x11C2] | #x11EB
;;            | #x11F0          | #x11F9          | [#x1E00-#x1E9B]
;;            | [#x1EA0-#x1EF9] | [#x1F00-#x1F15] | [#x1F18-#x1F1D]
;;            | [#x1F20-#x1F45] | [#x1F48-#x1F4D] | [#x1F50-#x1F57]
;;            | #x1F59          | #x1F5B          | #x1F5D
;;            | [#x1F5F-#x1F7D] | [#x1F80-#x1FB4] | [#x1FB6-#x1FBC]
;;            | #x1FBE          | [#x1FC2-#x1FC4] | [#x1FC6-#x1FCC]
;;            | [#x1FD0-#x1FD3] | [#x1FD6-#x1FDB] | [#x1FE0-#x1FEC]
;;            | [#x1FF2-#x1FF4] | [#x1FF6-#x1FFC] | #x2126
;;            | [#x212A-#x212B] | #x212E          | [#x2180-#x2182]
;;            | [#x3041-#x3094] | [#x30A1-#x30FA] | [#x3105-#x312C]
;;            | [#xAC00-#xD7A3]
;;
;; Ideographic ::= [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]
;;
;; CombiningChar ::= [#x0300-#x0345] | [#x0360-#x0361] | [#x0483-#x0486]
;;                 | [#x0591-#x05A1] | [#x05A3-#x05B9] | [#x05BB-#x05BD]
;;                 | #x05BF          | [#x05C1-#x05C2] | #x05C4
;;                 | [#x064B-#x0652] | #x0670          | [#x06D6-#x06DC]
;;                 | [#x06DD-#x06DF] | [#x06E0-#x06E4] | [#x06E7-#x06E8]
;;                 | [#x06EA-#x06ED] | [#x0901-#x0903] | #x093C
;;                 | [#x093E-#x094C] | #x094D          | [#x0951-#x0954]
;;                 | [#x0962-#x0963] | [#x0981-#x0983] | #x09BC
;;                 | #x09BE          | #x09BF          | [#x09C0-#x09C4]
;;                 | [#x09C7-#x09C8] | [#x09CB-#x09CD] | #x09D7
;;                 | [#x09E2-#x09E3] | #x0A02          | #x0A3C
;;                 | #x0A3E          | #x0A3F          | [#x0A40-#x0A42]
;;                 | [#x0A47-#x0A48] | [#x0A4B-#x0A4D] | [#x0A70-#x0A71]
;;                 | [#x0A81-#x0A83] | #x0ABC          | [#x0ABE-#x0AC5]
;;                 | [#x0AC7-#x0AC9] | [#x0ACB-#x0ACD] | [#x0B01-#x0B03]
;;                 | #x0B3C          | [#x0B3E-#x0B43] | [#x0B47-#x0B48]
;;                 | [#x0B4B-#x0B4D] | [#x0B56-#x0B57] | [#x0B82-#x0B83]
;;                 | [#x0BBE-#x0BC2] | [#x0BC6-#x0BC8] | [#x0BCA-#x0BCD]
;;                 | #x0BD7          | [#x0C01-#x0C03] | [#x0C3E-#x0C44]
;;                 | [#x0C46-#x0C48] | [#x0C4A-#x0C4D] | [#x0C55-#x0C56]
;;                 | [#x0C82-#x0C83] | [#x0CBE-#x0CC4] | [#x0CC6-#x0CC8]
;;                 | [#x0CCA-#x0CCD] | [#x0CD5-#x0CD6] | [#x0D02-#x0D03]
;;                 | [#x0D3E-#x0D43] | [#x0D46-#x0D48] | [#x0D4A-#x0D4D]
;;                 | #x0D57          | #x0E31          | [#x0E34-#x0E3A]
;;                 | [#x0E47-#x0E4E] | #x0EB1          | [#x0EB4-#x0EB9]
;;                 | [#x0EBB-#x0EBC] | [#x0EC8-#x0ECD] | [#x0F18-#x0F19]
;;                 | #x0F35          | #x0F37          | #x0F39
;;                 | #x0F3E          | #x0F3F          | [#x0F71-#x0F84]
;;                 | [#x0F86-#x0F8B] | [#x0F90-#x0F95] | #x0F97
;;                 | [#x0F99-#x0FAD] | [#x0FB1-#x0FB7] | #x0FB9
;;                 | [#x20D0-#x20DC] | #x20E1          | [#x302A-#x302F]
;;                 | #x3099          | #x309A
;;
;; Digit ::= [#x0030-#x0039] | [#x0660-#x0669] | [#x06F0-#x06F9]
;;         | [#x0966-#x096F] | [#x09E6-#x09EF] | [#x0A66-#x0A6F]
;;         | [#x0AE6-#x0AEF] | [#x0B66-#x0B6F] | [#x0BE7-#x0BEF]
;;         | [#x0C66-#x0C6F] | [#x0CE6-#x0CEF] | [#x0D66-#x0D6F]
;;         | [#x0E50-#x0E59] | [#x0ED0-#x0ED9] | [#x0F20-#x0F29]
;;
;; Extender ::= #x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 | #x0E46 | #x0EC6
;;            | #x3005 | [#x3031-#x3035] | [#x309D-#x309E] | [#x30FC-#x30FE]
;;
;;
;; NOTES
;; -----
;;
;; At moment, only the `<!ELEMENT' generates a syntactic chart.  The
;; `<!ATTLIST', `<!NOTATION' and `<!ENTITY' are syntactically checked but they
;; don't generate a syntactic chart.
;;
;; Besides the syntax above, ebnf-dtd also accepts a `pure' dtd file.  An
;; example of a `pure' dtd file is:
;;
;;    <?xml version="1.0" encoding="UTF-8"?>
;;    <!--
;;    The main element.
;;    -->
;;    <!ELEMENT workflow (registers?, trigger-functions?, initial-actions,
;;                        steps, splits?, joins?)>
;;    <!--
;;    An action that can be executed (id must be unique among actions for
;;    the enclosing step).
;;    Used in: actions
;;    -->
;;    <!ELEMENT action (restrict-to, validators?, pre-functions?, results,
;;                      post-functions?)>
;;    <!ATTLIST action
;;    	id CDATA #REQUIRED
;;    	name CDATA #REQUIRED
;;    >
;;
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:


(require 'ebnf-otz)


(defvar ebnf-dtd-lex nil
  "Value returned by `ebnf-dtd-lex' function.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic analyzer


;;; document ::= prolog element Misc*
;;; /* Note that *only* the prolog will be parsed */

(defun ebnf-dtd-parser (start)
  "DTD parser."
  (let ((total (+ (- ebnf-limit start) 1))
	(bias (1- start))
	(origin (point))
	rule-list token rule the-end)
    (goto-char start)
    (setq token (ebnf-dtd-lex))
    (and (eq token 'end-of-input)
	 (error "Empty DTD file"))
    (setq token (ebnf-dtd-prolog token))
    (unless (eq (car token) 'end-prolog)
      (setq the-end (cdr token)
	    token   (car token))
      (while (not (eq token the-end))
	(ebnf-message-float
	 "Parsing...%s%%"
	 (/ (* (- (point) bias) 100.0) total))
	(setq token (ebnf-dtd-intsubset token)
	      rule  (cdr token)
	      token (car token))
	(or (null rule)
	    (ebnf-add-empty-rule-list rule)
	    (setq rule-list (cons rule rule-list))))
      (or (eq the-end 'end-of-input)
	  (eq (ebnf-dtd-lex) 'end-decl)
	  (error "Missing end of DOCTYPE"))
      ;; adjust message, 'cause *only* prolog will be parsed
      (ebnf-message-float "Parsing...%s%%" 100.0))
    (goto-char origin)
    rule-list))


;;; prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
;;;
;;; XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
;;;
;;; VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
;;;
;;; Eq ::= S? '=' S?
;;;
;;; VersionNum ::= '1.0'
;;;
;;; Misc ::= Comment | PI | S
;;;
;;; EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
;;;
;;; EncName ::= [A-Za-z] ([-A-Za-z0-9._])*
;;; /* Encoding name contains only Latin characters */
;;;
;;; SDDecl ::= S 'standalone' Eq
;;;            (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
;;;
;;; doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
;;;                 ('[' intSubset ']' S?)? '>'


(defun ebnf-dtd-prolog (token)
  (when (and (eq token 'begin-pi) (string= ebnf-dtd-lex "xml"))
    ;; version = "1.0"
    (setq token (ebnf-dtd-attribute (ebnf-dtd-lex) 'version-attr
				    "^1\\.0$" "XML version"))
    ;; ( encoding = "encoding name" )?
    (setq token (ebnf-dtd-attribute-optional
		 token 'encoding-attr
		 "^[A-Za-z][-A-Za-z0-9._]*$" "XML encoding"))
    ;; ( standalone = ( "yes" | "no" ) )?
    (setq token (ebnf-dtd-attribute-optional
		 token 'standalone-attr
		 "^yes|no$" "XML standalone"))
    (or (eq token 'end-pi)
	(error "Missing end of XML processing instruction")))
  ;; processing instructions
  (setq token (ebnf-dtd-pi (ebnf-dtd-lex)))
  (cond
   ;; DOCTYPE
   ((eq token 'doctype-decl)
    (or (eq (ebnf-dtd-lex) 'name)
	(error "Document type name is missing"))
    (cons (if (eq (ebnf-dtd-externalid) 'begin-subset)
	      (ebnf-dtd-lex)
	    'end-prolog)
	  'end-subset))
   ((memq token '(element-decl attlist-decl entity-decl notation-decl))
    (cons token 'end-of-input))
   (t
    '(end-prolog . end-subset))
   ))


(defun ebnf-dtd-attribute (token attr match attr-name)
  (or (eq token attr)
      (error "%s attribute is missing" attr-name))
  (ebnf-dtd-attribute-optional token attr match attr-name))


(defun ebnf-dtd-attribute-optional (token attr match attr-name)
  (when (eq token attr)
    (or (and (eq (ebnf-dtd-lex) 'equal)
	     (eq (ebnf-dtd-lex) 'string)
	     (string-match match ebnf-dtd-lex))
	(error "XML %s attribute is invalid" attr-name))
    (setq token (ebnf-dtd-lex)))
  token)


;;; ExternalID ::= 'SYSTEM' S SystemLiteral
;;;              | 'PUBLIC' S PubidLiteral S SystemLiteral


(defun ebnf-dtd-externalid (&optional token)
  (let ((must-have token))
    (or token (setq token (ebnf-dtd-lex)))
    (cond ((eq token 'system)
	   (ebnf-dtd-systemliteral))
	  ((eq token 'public)
	   (ebnf-dtd-pubidliteral)
	   (ebnf-dtd-systemliteral))
	  (must-have
	   (error "Missing `SYSTEM' or `PUBLIC' in external id"))
	  (t
	   token))))


;;; SystemLiteral ::= ('"' [^"]* '"')
;;;                 | ("'" [^']* "'")


(defun ebnf-dtd-systemliteral ()
  (or (eq (ebnf-dtd-lex) 'string)
      (error "System identifier is invalid"))
  (ebnf-dtd-lex))


;;; PubidLiteral ::= '"' PubidChar* '"'
;;;                | "'" (PubidChar - "'")* "'"
;;;
;;; PubidChar ::= [-'()+,./:=?;!*#@$_%\n\r a-zA-Z0-9]


(defun ebnf-dtd-pubidliteral ()
  (or (and (eq (ebnf-dtd-lex) 'string)
	   (string-match "^[-'()+,./:=?;!*#@$_%\n\r a-zA-Z0-9]*$"
			 ebnf-dtd-lex))
      (error "Public identifier is invalid")))


;;; PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
;;;
;;; PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))


(defun ebnf-dtd-pi (token)
  (while (eq token 'begin-pi)
    (and (string-match "^[xX][mM][lL]$" ebnf-dtd-lex)
	 (error "Processing instruction name can not be `XML'"))
    (while (not (eq (ebnf-dtd-lex) 'end-pi)))
    (setq token (ebnf-dtd-lex)))
  token)


;;; doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
;;;                 ('[' intSubset ']' S?)? '>'
;;;
;;; intSubset ::= (markupdecl | DeclSep)*
;;;
;;; DeclSep ::= PEReference | S
;;;
;;; markupdecl ::= elementdecl | AttlistDecl | EntityDecl
;;;              | NotationDecl | PI | Comment


(defun ebnf-dtd-intsubset (token)
  ;; PI - Processing Instruction
  (and (eq token 'begin-pi)
       (setq token (ebnf-dtd-pi token)))
  (cond
   ((memq token '(end-subset end-of-input))
    (cons token nil))
   ((eq token 'pe-ref)
    (cons (ebnf-dtd-lex) nil))		; annotation
   ((eq token 'element-decl)
    (ebnf-dtd-elementdecl))		; rule
   ((eq token 'attlist-decl)
    (ebnf-dtd-attlistdecl))		; annotation
   ((eq token 'entity-decl)
    (ebnf-dtd-entitydecl))		; annotation
   ((eq token 'notation-decl)
    (ebnf-dtd-notationdecl))		; annotation
   (t
    (error "Invalid DOCTYPE element"))
   ))


;;; elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
;;;
;;; contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
;;;
;;; Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
;;;         | '(' S? '#PCDATA' S? ')'
;;;
;;; children ::= (choice | seq) ('?' | '*' | '+')?
;;;
;;; choice ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
;;;
;;; seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
;;;
;;; cp ::= (Name | choice | seq) ('?' | '*' | '+')?


(defun ebnf-dtd-elementdecl ()
  (let ((action ebnf-action)
	name token body)
    (setq ebnf-action nil)
    (or (eq (ebnf-dtd-lex) 'name)
	(error "Invalid ELEMENT name"))
    (setq name  ebnf-dtd-lex
	  token (ebnf-dtd-lex)
	  body  (cond ((memq token '(empty any))
		       (let ((term (ebnf-make-terminal ebnf-dtd-lex)))
			 (cons (ebnf-dtd-lex) term)))
		      ((eq token 'begin-group)
		       (setq token (ebnf-dtd-lex))
		       (if (eq token 'pcdata)
			   (ebnf-dtd-mixed)
			 (ebnf-dtd-children token)))
		      (t
		       (error "Invalid ELEMENT content"))
		      ))
    (or (eq (car body) 'end-decl)
	(error "Missing `>' in ELEMENT declaration"))
    (ebnf-eps-add-production name)
    (cons (ebnf-dtd-lex)
	  (ebnf-make-production name (cdr body) action))))


;;; Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*'
;;;         | '(' S? '#PCDATA' S? ')'


(defun ebnf-dtd-mixed ()
  (let* ((alt             (cons (ebnf-make-terminal ebnf-dtd-lex) nil))
	 (token           (ebnf-dtd-lex))
	 (has-alternative (eq token 'alternative)))
    (while (eq token 'alternative)
      (or (eq (ebnf-dtd-lex) 'name)
	  (error "Invalid name"))
      (setq alt   (cons ebnf-dtd-lex alt)
	    token (ebnf-dtd-lex)))
    (or (eq token 'end-group)
	(error "Missing `)'"))
    (and has-alternative
	 (or (eq (ebnf-dtd-lex) 'zero-or-more)
	     (error "Missing `*'")))
    (ebnf-token-alternative alt (cons (ebnf-dtd-lex) nil))))


;;; children ::= (choice | seq) ('?' | '*' | '+')?


(defun ebnf-dtd-children (token)
  (ebnf-dtd-operators (ebnf-dtd-choice-seq token)))


;;; choice ::= '(' S? cp ( S? '|' S? cp )+ S? ')'
;;;
;;; seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'


(defun ebnf-dtd-choice-seq (token)
  (setq token (ebnf-dtd-cp token))
  (let (elist)
    (cond
     ;; choice
     ((eq (car token) 'alternative)
      (while (eq (car token) 'alternative)
	(setq elist (cons (cdr token) elist)
	      token (ebnf-dtd-cp (ebnf-dtd-lex))))
      (setq elist (ebnf-token-alternative elist token)))
     ;; seq
     ((eq (car token) 'comma)
      (while (eq (car token) 'comma)
	(setq elist (cons (cdr token) elist)
	      token (ebnf-dtd-cp (ebnf-dtd-lex))))
      (setq elist (ebnf-token-sequence (cons (cdr token) elist))))
     ;; only one element
     (t
      (setq elist (cdr token))))
    (or (eq (car token) 'end-group)
	(error "Missing `)' in ELEMENT content"))
    elist))


;;; cp ::= (Name | choice | seq) ('?' | '*' | '+')?


(defun ebnf-dtd-cp (token)
  (ebnf-dtd-operators (cond ((eq token 'name)
			     (ebnf-make-terminal ebnf-dtd-lex))
			    ((eq token 'begin-group)
			     (ebnf-dtd-choice-seq (ebnf-dtd-lex)))
			    (t
			     (error "Invalid element"))
			    )))


;;; elm ('?' | '*' | '+')?


(defun ebnf-dtd-operators (elm)
  (let ((token (ebnf-dtd-lex)))
    (cond ((eq token 'optional)		; ? -  optional
	   (cons (ebnf-dtd-lex) (ebnf-token-optional elm)))
	  ((eq token 'zero-or-more)	; * - zero or more
	   (cons (ebnf-dtd-lex) (ebnf-make-zero-or-more elm)))
	  ((eq token 'one-or-more)	; + - one or more
	   (cons (ebnf-dtd-lex) (ebnf-make-one-or-more elm)))
	  (t				; only element
	   (cons token elm))
	  )))


;;; AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
;;;
;;; AttDef ::= S Name S AttType S DefaultDecl
;;;
;;; AttType ::= StringType | TokenizedType | EnumeratedType
;;;
;;; StringType ::= 'CDATA'
;;;
;;; TokenizedType ::= 'ID'
;;;                 | 'IDREF'
;;;                 | 'IDREFS'
;;;                 | 'ENTITY'
;;;                 | 'ENTITIES'
;;;                 | 'NMTOKEN'
;;;                 | 'NMTOKENS'
;;;
;;; EnumeratedType ::= NotationType | Enumeration
;;;
;;; NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
;;;
;;; Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
;;;
;;; DefaultDecl ::= '#REQUIRED'
;;;               | '#IMPLIED'
;;;               | (('#FIXED' S)? AttValue)
;;;
;;;
;;; AttValue ::= '"' ([^<&"] | Reference)* '"'
;;;            | "'" ([^<&'] | Reference)* "'"
;;;
;;; Reference ::= EntityRef | CharRef
;;;
;;; EntityRef ::= '&' Name ';'
;;;
;;; CharRef ::= '&#' [0-9]+ ';'
;;;           | '&#x' [0-9a-fA-F]+ ';'

;;; "^\\(&\\([A-Za-z_:][-A-Za-z0-9._:]*\\|#\\(x[0-9a-fA-F]+\\|[0-9]+\\)\\);\\|[^<&]\\)*$"


(defun ebnf-dtd-attlistdecl ()
  (or (eq (ebnf-dtd-lex) 'name)
      (error "Invalid ATTLIST name"))
  (let (token)
    (while (eq (setq token (ebnf-dtd-lex)) 'name)
      ;; type
      (setq token (ebnf-dtd-lex))
      (cond
       ((eq token 'notation)
	(or (eq (ebnf-dtd-lex) 'begin-group)
	    (error "Missing `(' in NOTATION type in ATTLIST declaration"))
	(ebnf-dtd-namelist "NOTATION" '(name)))
       ((eq token 'begin-group)
	(ebnf-dtd-namelist "enumeration" '(name name-char)))
       ((memq token
	      '(cdata id idref idrefs entity entities nmtoken nmtokens)))
       (t
	(error "Invalid type in ATTLIST declaration")))
      ;; default value
      (setq token (ebnf-dtd-lex))
      (unless (memq token '(required implied))
	(and (eq token 'fixed)
	     (setq token (ebnf-dtd-lex)))
	(or (and (eq token 'string)
		 (string-match
		  "^\\(&\\([A-Za-z_:][-A-Za-z0-9._:]*\\|#\\(x[0-9a-fA-F]+\\|[0-9]+\\)\\);\\|[^<&]\\)*$"
		  ebnf-dtd-lex))
	    (error "Invalid default value in ATTLIST declaration"))))
    (or (eq token 'end-decl)
	(error "Missing `>' in end of ATTLIST"))
    (cons (ebnf-dtd-lex) nil)))


(defun ebnf-dtd-namelist (type name-list)
  (let (token)
    (while (progn
	     (or (memq (ebnf-dtd-lex) name-list)
		 (error "Invalid name in %s type in ATTLIST declaration" type))
	     (eq (setq token (ebnf-dtd-lex)) 'alternative)))
    (or (eq token 'end-group)
	(error "Missing `)' in %s type in ATTLIST declaration" type))))


;;; EntityDecl ::= GEDecl | PEDecl
;;;
;;; GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
;;;
;;; PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
;;;
;;; EntityDef ::= EntityValue | (ExternalID NDataDecl?)
;;;
;;; PEDef ::= EntityValue | ExternalID
;;;
;;; NDataDecl ::= S 'NDATA' S Name
;;;
;;;
;;; EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"'
;;;               | "'" ([^%&'] | PEReference | Reference)* "'"
;;;
;;; PEReference ::= '%' Name ';'
;;;
;;; Reference ::= EntityRef | CharRef
;;;
;;; EntityRef ::= '&' Name ';'
;;;
;;; CharRef ::= '&#' [0-9]+ ';'
;;;           | '&#x' [0-9a-fA-F]+ ';'

;;; "^\\(%[A-Za-z_:][-A-Za-z0-9._:]*;\\|&\\([A-Za-z_:][-A-Za-z0-9._:]*\\|#\\(x[0-9a-fA-F]+\\|[0-9]+\\)\\);\\|[^%&]\\)*$"


(defun ebnf-dtd-entitydecl ()
  (let* ((token (ebnf-dtd-lex))
	 (pedecl (eq token 'percent)))
    (and pedecl
	 (setq token (ebnf-dtd-lex)))
    (or (eq token 'name)
	(error "Invalid name of ENTITY"))
    (setq token (ebnf-dtd-lex))
    (if (eq token 'string)
	(if (string-match
	     "^\\(%[A-Za-z_:][-A-Za-z0-9._:]*;\\|&\\([A-Za-z_:][-A-Za-z0-9._:]*\\|#\\(x[0-9a-fA-F]+\\|[0-9]+\\)\\);\\|[^%&]\\)*$"
	     ebnf-dtd-lex)
	    (setq token (ebnf-dtd-lex))
	  (error "Invalid ENTITY definition"))
      (setq token (ebnf-dtd-externalid token))
      (when (and (not pedecl) (eq token 'ndata))
	(or (eq (ebnf-dtd-lex) 'name)
	    (error "Invalid NDATA name"))
	(setq token (ebnf-dtd-lex))))
    (or (eq token 'end-decl)
	(error "Missing `>' in end of ENTITY"))
    (cons (ebnf-dtd-lex) nil)))


;;; NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
;;;
;;; PublicID ::= 'PUBLIC' S PubidLiteral


(defun ebnf-dtd-notationdecl ()
  (or (eq (ebnf-dtd-lex) 'name)
      (error "Invalid name NOTATION"))
  (or (eq (ebnf-dtd-externalid-or-publicid) 'end-decl)
      (error "Missing `>' in end of NOTATION"))
  (cons (ebnf-dtd-lex) nil))


;;; ExternalID ::= 'SYSTEM' S SystemLiteral
;;;              | 'PUBLIC' S PubidLiteral S SystemLiteral
;;;
;;; PublicID ::= 'PUBLIC' S PubidLiteral


(defun ebnf-dtd-externalid-or-publicid ()
  (let ((token (ebnf-dtd-lex)))
    (cond ((eq token 'system)
	   (ebnf-dtd-systemliteral))
	  ((eq token 'public)
	   (ebnf-dtd-pubidliteral)
	   (and (eq (setq token (ebnf-dtd-lex)) 'string)
		(setq token (ebnf-dtd-lex)))
	   token)
	  (t
	   (error "Missing `SYSTEM' or `PUBLIC'")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lexical analyzer


(defconst ebnf-dtd-token-table (make-vector 256 'error)
  "Vector used to map characters to a lexical token.")


(defun ebnf-dtd-initialize ()
  "Initialize EBNF token table."
  ;; control character & control 8-bit character are set to `error'
  (let ((char ?\060))
    ;; digits: 0-9
    (while (< char ?\072)
      (aset ebnf-dtd-token-table char 'name-char)
      (setq char (1+ char)))
    ;; printable character: A-Z
    (setq char ?\101)
    (while (< char ?\133)
      (aset ebnf-dtd-token-table char 'name)
      (setq char (1+ char)))
    ;; printable character: a-z
    (setq char ?\141)
    (while (< char ?\173)
      (aset ebnf-dtd-token-table char 'name)
      (setq char (1+ char)))
    ;; European 8-bit accentuated characters:
    (setq char ?\240)
    (while (< char ?\400)
      (aset ebnf-dtd-token-table char 'name)
      (setq char (1+ char)))
    ;; Override name characters:
    (aset ebnf-dtd-token-table ?_ 'name)
    (aset ebnf-dtd-token-table ?: 'name)
    (aset ebnf-dtd-token-table ?. 'name-char)
    (aset ebnf-dtd-token-table ?- 'name-char)
    ;; Override space characters:
    (aset ebnf-dtd-token-table ?\n 'space) ; [NL] linefeed
    (aset ebnf-dtd-token-table ?\r 'space) ; [CR] carriage return
    (aset ebnf-dtd-token-table ?\t 'space) ; [HT] horizontal tab
    (aset ebnf-dtd-token-table ?\  'space) ; [SP] space
    ;; Override other lexical characters:
    (aset ebnf-dtd-token-table ?=  'equal)
    (aset ebnf-dtd-token-table ?,  'comma)
    (aset ebnf-dtd-token-table ?*  'zero-or-more)
    (aset ebnf-dtd-token-table ?+  'one-or-more)
    (aset ebnf-dtd-token-table ?|  'alternative)
    (aset ebnf-dtd-token-table ?%  'percent)
    (aset ebnf-dtd-token-table ?&  'ampersand)
    (aset ebnf-dtd-token-table ?#  'hash)
    (aset ebnf-dtd-token-table ?\? 'interrogation)
    (aset ebnf-dtd-token-table ?\" 'double-quote)
    (aset ebnf-dtd-token-table ?\' 'single-quote)
    (aset ebnf-dtd-token-table ?<  'less-than)
    (aset ebnf-dtd-token-table ?>  'end-decl)
    (aset ebnf-dtd-token-table ?\( 'begin-group)
    (aset ebnf-dtd-token-table ?\) 'end-group)
    (aset ebnf-dtd-token-table ?\[ 'begin-subset)
    (aset ebnf-dtd-token-table ?\] 'end-subset)))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-dtd-name-chars
  (ebnf-range-regexp "-._:0-9A-Za-z" ?\240 ?\377))


(defconst ebnf-dtd-decl-alist
  '(("ATTLIST"  . attlist-decl)
    ("DOCTYPE"  . doctype-decl)
    ("ELEMENT"  . element-decl)
    ("ENTITY"   . entity-decl)
    ("NOTATION" . notation-decl)))


(defconst ebnf-dtd-element-alist
  '(("#FIXED"    . fixed)
    ("#IMPLIED"  . implied)
    ("#PCDATA"   . pcdata)
    ("#REQUIRED" . required)))


(defconst ebnf-dtd-name-alist
  '(("ANY"        . any)
    ("CDATA"      . cdata)
    ("EMPTY"      . empty)
    ("ENTITIES"   . entities)
    ("ENTITY"     . entity)
    ("ID"         . id)
    ("IDREF"      . idref)
    ("IDREFS"     . idrefs)
    ("NDATA"      . ndata)
    ("NMTOKEN"    . nmtoken)
    ("NMTOKENS"   . nmtokens)
    ("NOTATION"   . notation)
    ("PUBLIC"     . public)
    ("SYSTEM"     . system)
    ("encoding"   . encoding-attr)
    ("standalone" . standalone-attr)
    ("version"    . version-attr)))


(defun ebnf-dtd-lex ()
  "Lexical analyzer for DTD.

Return a lexical token.

See documentation for variable `ebnf-dtd-lex'."
  (if (>= (point) ebnf-limit)
      'end-of-input
    (let (token)
      ;; skip spaces and comments
      (while (if (> (following-char) 255)
		 (progn
		   (setq token 'error)
		   nil)
	       (setq token (aref ebnf-dtd-token-table (following-char)))
	       (cond
		((eq token 'space)
		 (skip-chars-forward " \n\r\t" ebnf-limit)
		 (< (point) ebnf-limit))
		((and (eq token 'less-than)
		      (looking-at "<!--"))
		 (ebnf-dtd-skip-comment))
		(t nil)
		)))
      (cond
       ;; end of input
       ((>= (point) ebnf-limit)
	'end-of-input)
       ;; error
       ((eq token 'error)
	(error "Invalid character"))
       ;; beginning of declaration:
       ;; <?name, <!ATTLIST, <!DOCTYPE, <!ELEMENT, <!ENTITY, <!NOTATION
       ((eq token 'less-than)
	(forward-char)
	(let ((char (following-char)))
	  (cond ((= char ?\?)		; <?
		 (forward-char)
		 (setq ebnf-dtd-lex (ebnf-buffer-substring ebnf-dtd-name-chars))
		 'begin-pi)
		((= char ?!)		; <!
		 (forward-char)
		 (let ((decl (ebnf-buffer-substring ebnf-dtd-name-chars)))
		   (or (cdr (assoc decl ebnf-dtd-decl-alist))
		       (error "Invalid declaration name `%s'" decl))))
		(t			; <x
		 (error "Invalid declaration `<%c'" char)))))
       ;; name, namechar
       ((memq  token '(name name-char))
	(setq ebnf-dtd-lex (ebnf-buffer-substring ebnf-dtd-name-chars))
	(or (cdr (assoc ebnf-dtd-lex ebnf-dtd-name-alist))
	    token))
       ;; ?, ?>
       ((eq token 'interrogation)
	(forward-char)
	(if (/= (following-char) ?>)
	    'optional
	  (forward-char)
	  'end-pi))
       ;; #FIXED, #IMPLIED, #PCDATA, #REQUIRED
       ((eq token 'hash)
	(forward-char)
	(setq ebnf-dtd-lex
	      (concat "#" (ebnf-buffer-substring ebnf-dtd-name-chars)))
	(or (cdr (assoc ebnf-dtd-lex ebnf-dtd-element-alist))
	    (error "Invalid element `%s'" ebnf-dtd-lex)))
       ;; "string"
       ((eq token 'double-quote)
	(setq ebnf-dtd-lex (ebnf-dtd-string ?\"))
	'string)
       ;; 'string'
       ((eq token 'single-quote)
	(setq ebnf-dtd-lex (ebnf-dtd-string ?\'))
	'string)
       ;; %, %name;
       ((eq token 'percent)
	(forward-char)
	(if (looking-at "[ \n\r\t]")
	    'percent
	  (setq ebnf-dtd-lex (ebnf-dtd-name-ref "%"))
	  'pe-ref))
       ;; &#...;, &#x...;, &name;
       ((eq token 'ampersand)
	(forward-char)
	(if (/= (following-char) ?#)
	    (progn
	      ;; &name;
	      (setq ebnf-dtd-lex (ebnf-dtd-name-ref "&"))
	      'entity-ref)
	  ;; &#...;, &#x...;
	  (forward-char)
	  (setq ebnf-dtd-lex (if (/= (following-char) ?x)
				 (ebnf-dtd-char-ref "&#" "0-9")
			       (forward-char)
			       (ebnf-dtd-char-ref "&#x" "0-9a-fA-F")))
	  'char-ref))
       ;; miscellaneous: (, ), [, ], =, |, *, +, >, `,'
       (t
	(forward-char)
	token)
       ))))


(defun ebnf-dtd-name-ref (start)
  (ebnf-dtd-char-ref start ebnf-dtd-name-chars))


(defun ebnf-dtd-char-ref (start chars)
  (let ((char (ebnf-buffer-substring chars)))
    (or (= (following-char) ?\;)
	(error "Invalid element `%s%s%c'" start char (following-char)))
    (forward-char)
    (format "%s%s;" start char)))


;; replace the range "\240-\377" (see `ebnf-range-regexp').
(defconst ebnf-dtd-double-string-chars
  (ebnf-range-regexp "\t -!#-~" ?\240 ?\377))
(defconst ebnf-dtd-single-string-chars
  (ebnf-range-regexp "\t -&(-~" ?\240 ?\377))


(defun ebnf-dtd-string (delim)
  (buffer-substring-no-properties
   (progn
     (forward-char)
     (point))
   (progn
     (skip-chars-forward (if (= delim ?\")
			     ebnf-dtd-double-string-chars
			   ebnf-dtd-single-string-chars)
			 ebnf-limit)
     (or (= (following-char) delim)
	 (error "Missing string delimiter `%c'" delim))
     (prog1
	 (point)
       (forward-char)))))


;; replace the range "\177-\237" (see `ebnf-range-regexp').
(defconst ebnf-dtd-comment-chars
  (ebnf-range-regexp "^-\000-\010\013\014\016-\037" ?\177 ?\237))
(defconst ebnf-dtd-filename-chars
  (ebnf-range-regexp "^-\000-\037" ?\177 ?\237))


(defun ebnf-dtd-skip-comment ()
  (forward-char 4)			; <!--
  (cond
   ;; open EPS file
   ((and ebnf-eps-executing (= (following-char) ?\[))
    (ebnf-eps-add-context (ebnf-dtd-eps-filename)))
   ;; close EPS file
   ((and ebnf-eps-executing (= (following-char) ?\]))
    (ebnf-eps-remove-context (ebnf-dtd-eps-filename)))
   ;; EPS header
   ((and ebnf-eps-executing (= (following-char) ?H))
    (ebnf-eps-header-comment (ebnf-dtd-eps-filename)))
   ;; EPS footer
   ((and ebnf-eps-executing (= (following-char) ?F))
    (ebnf-eps-footer-comment (ebnf-dtd-eps-filename)))
   ;; any other action in comment
   (t
    (setq ebnf-action (aref ebnf-comment-table (following-char))))
   )
  (while (progn
	   (skip-chars-forward ebnf-dtd-comment-chars ebnf-limit)
	   (and (< (point) ebnf-limit)
		(not (looking-at "-->"))))
    (skip-chars-forward "-" ebnf-limit))
  ;; check for a valid end of comment
  (cond ((>= (point) ebnf-limit)
	 nil)
	((looking-at "-->")
	 (forward-char 3)
	 t)
	(t
	 (error "Invalid character"))
	))


(defun ebnf-dtd-eps-filename ()
  (forward-char)
  (let (fname)
    (while (progn
	     (setq fname
		   (concat fname
			   (ebnf-buffer-substring ebnf-dtd-filename-chars)))
	     (and (< (point) ebnf-limit)
		  (= (following-char) ?-)	; may be \n, \t, \r
		  (not (looking-at "-->"))))
      (setq fname (concat fname (ebnf-buffer-substring "-"))))
    fname))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'ebnf-dtd)

;;; ebnf-dtd.el ends here
