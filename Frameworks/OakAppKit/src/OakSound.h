#import <oak/misc.h>

enum OakSoundIdentifier
{
	OakSoundDidMoveItemUISound,
	OakSoundDidTrashItemUISound,
	OakSoundDidCompleteSomethingUISound,
	OakSoundDidBeginRecordingUISound,
	OakSoundDidEndRecordingUISound
};

PUBLIC void OakPlayUISound (OakSoundIdentifier aSound);
