/* Interface to libxml2.
   Copyright (C) 2010-2012 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#ifdef HAVE_LIBXML2

#include <setjmp.h>
#include <libxml/tree.h>
#include <libxml/parser.h>
#include <libxml/HTMLparser.h>

#include "lisp.h"
#include "buffer.h"

static Lisp_Object
make_dom (xmlNode *node)
{
  if (node->type == XML_ELEMENT_NODE)
    {
      Lisp_Object result = Fcons (intern ((char *) node->name), Qnil);
      xmlNode *child;
      xmlAttr *property;
      Lisp_Object plist = Qnil;

      /* First add the attributes. */
      property = node->properties;
      while (property != NULL)
	{
	  if (property->children &&
	      property->children->content)
	    {
	      char *content = (char *) property->children->content;
	      plist = Fcons (Fcons (intern ((char *) property->name),
				    build_string (content)),
			     plist);
	    }
	  property = property->next;
	}
      result = Fcons (Fnreverse (plist), result);

      /* Then add the children of the node. */
      child = node->children;
      while (child != NULL)
	{
	  result = Fcons (make_dom (child), result);
	  child = child->next;
	}

      return Fnreverse (result);
    }
  else if (node->type == XML_TEXT_NODE || node->type == XML_CDATA_SECTION_NODE)
    {
      if (node->content)
	return build_string ((char *) node->content);
      else
	return Qnil;
    }
  else if (node->type == XML_COMMENT_NODE)
    {
      if (node->content)
	return list3 (intern ("comment"), Qnil,
		      build_string ((char *) node->content));
      else
	return Qnil;
    }
  else
    return Qnil;
}

static Lisp_Object
parse_region (Lisp_Object start, Lisp_Object end, Lisp_Object base_url, int htmlp)
{
  xmlDoc *doc;
  Lisp_Object result = Qnil;
  const char *burl = "";
  EMACS_INT bytes;
  EMACS_INT istart, iend;

  LIBXML_TEST_VERSION;

  validate_region (&start, &end);

  istart = XINT (start);
  iend = XINT (end);

  if (istart < GPT && GPT < iend)
    move_gap (iend);

  if (! NILP (base_url))
    {
      CHECK_STRING (base_url);
      burl = SSDATA (base_url);
    }

  bytes = CHAR_TO_BYTE (iend) - CHAR_TO_BYTE (istart);

  if (htmlp)
    doc = htmlReadMemory ((char *) BYTE_POS_ADDR (CHAR_TO_BYTE (istart)),
			  bytes, burl, "utf-8",
			  HTML_PARSE_RECOVER|HTML_PARSE_NONET|
			  HTML_PARSE_NOWARNING|HTML_PARSE_NOERROR|
			  HTML_PARSE_NOBLANKS);
  else
    doc = xmlReadMemory ((char *) BYTE_POS_ADDR (CHAR_TO_BYTE (istart)),
			 bytes, burl, "utf-8",
			 XML_PARSE_NONET|XML_PARSE_NOWARNING|
			 XML_PARSE_NOBLANKS |XML_PARSE_NOERROR);

  if (doc != NULL)
    {
      /* If the document is just comments, then this should get us the
	 nodes anyway. */
      xmlNode *n = doc->children->next;
      Lisp_Object r = Qnil;

      while (n) {
	if (!NILP (r))
	  result = Fcons (r, result);
	r = make_dom (n);
	n = n->next;
      }

      if (NILP (result)) {
	/* The document isn't just comments, so get the tree the
	   proper way. */
	xmlNode *node = xmlDocGetRootElement (doc);
	if (node != NULL)
	  result = make_dom (node);
      } else
	result = Fcons (intern ("top"),
			Fcons (Qnil, Fnreverse (Fcons (r, result))));

      xmlFreeDoc (doc);
    }

  return result;
}

DEFUN ("libxml-parse-html-region", Flibxml_parse_html_region,
       Slibxml_parse_html_region,
       2, 3, 0,
       doc: /* Parse the region as an HTML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object base_url)
{
  return parse_region (start, end, base_url, 1);
}

DEFUN ("libxml-parse-xml-region", Flibxml_parse_xml_region,
       Slibxml_parse_xml_region,
       2, 3, 0,
       doc: /* Parse the region as an XML document and return the parse tree.
If BASE-URL is non-nil, it is used to expand relative URLs.  */)
  (Lisp_Object start, Lisp_Object end, Lisp_Object base_url)
{
  return parse_region (start, end, base_url, 0);
}


/***********************************************************************
			    Initialization
 ***********************************************************************/
void
syms_of_xml (void)
{
  defsubr (&Slibxml_parse_html_region);
  defsubr (&Slibxml_parse_xml_region);
}

#endif /* HAVE_LIBXML2 */
