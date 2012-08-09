#ifndef ODB_EDITOR_SUITE_H_AT0LJGEF
#define ODB_EDITOR_SUITE_H_AT0LJGEF

bool DidHandleODBEditorEvent (AppleEvent const* event);

// optional paramters to 'aevt'/'odoc'
#define keyServerID                'FSnd'
#define keyFileSenderToken         'FTok'
#define keyFileCustomPath          'Burl'

// suite code for ODB editor suite events
#define kODBEditorSuite            'R*ch'

// ODB editor suite events, sent by the editor to the server.
#define kAEModifiedFile            'FMod'
#define keyNewLocation             'New?'
#define kAEClosedFile              'FCls'

// optional paramter to kAEModifiedFile/kAEClosedFile
#define keySenderToken             'Tokn'

#pragma options align=mac68k
struct PBX_SelectionRange
{
	short unused1;		// 0 (not used)
	short lineNum;		// line to select (<0 to specify range)
	long startRange;	// start of selection range (if line < 0)
	long endRange;		// end of selection range (if line < 0)
	long unused2;		// 0 (not used)
	long theDate;		// modification date/time
};
#pragma options align=reset

#endif /* end of include guard: ODB_EDITOR_SUITE_H_AT0LJGEF */
