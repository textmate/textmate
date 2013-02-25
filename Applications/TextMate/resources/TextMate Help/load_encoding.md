title: Detecting File Encoding

# Encodings

TextMate will do a few tests against your file:

 1. Does it have an UTF-8/16/32 BE/LE BOM?
    _Do not take this to mean that UTF-8 BOMs are fine!_
 2. Does it have a `com.apple.TextEncoding` extended attribute?
 3. Does it have an `encoding` setting via `.tm_properties`?
 4. Is it valid ASCII/UTF-8?

If all of these tests fail, it will ask you to pick the proper encoding.
