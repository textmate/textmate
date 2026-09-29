#import "OakSound.h"
#import <io/src/path.h>

void OakPlayUISound (OakSoundIdentifier aSound)
{
	struct sound_info_t
	{
		OakSoundIdentifier name;
		std::string path;
		bool initialized;
		SystemSoundID sound;
	};

	static sound_info_t sounds[] =
	{
		{ OakSoundDidTrashItemUISound,         "dock/drag to trash.aif"   },
		{ OakSoundDidMoveItemUISound,          "system/Volume Mount.aif"  },
		{ OakSoundDidCompleteSomethingUISound, "system/burn complete.aif" },
		{ OakSoundDidBeginRecordingUISound,    "system/begin_record.caf"  },
		{ OakSoundDidEndRecordingUISound,      "system/end_record.caf"    }
	};

	for(auto sound : sounds)
	{
		if(sound.name == aSound)
		{
			if(!sound.initialized)
			{
				std::string const path_10_6 = path::join("/System/Library/Components/CoreAudio.component/Contents/Resources/SystemSounds", sound.path);
				std::string const path_10_7 = path::join("/System/Library/Components/CoreAudio.component/Contents/SharedSupport/SystemSounds", sound.path);
				std::string const path = path::exists(path_10_7) ? path_10_7 : path_10_6;

				if(CFURLRef url = CFURLCreateFromFileSystemRepresentation(kCFAllocatorDefault, (UInt8 const*)path.data(), path.size(), false))
				{
					AudioServicesCreateSystemSoundID(url, &sound.sound);
					sound.initialized = true;
					CFRelease(url);
				}
			}

			if(sound.sound)
				AudioServicesPlaySystemSound(sound.sound);

			break;
		}
	}
}
