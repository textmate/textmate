#import <oak/misc.h>

enum OakSoundIdentifier
{
	OakSoundDidMoveItemUISound,
	OakSoundDidTrashItemUISound,
	OakSoundDidCompleteSomethingUISound
};

PUBLIC void OakPlayUISound (OakSoundIdentifier aSound);
