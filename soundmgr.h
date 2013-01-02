#ifndef SOUNDMGR_H
#define SOUNDMGR_H

#include "ooc.h"
#include "threadlib.h"

#include <map>

enum SoundState {
  SOUND_LOADING,
  SOUND_LOADED,
  SOUND_PLAYING,
};

enum SoundKind {
  SOUND_PRELOAD,
  SOUND_STREAM,
};

enum SoundChannels {
  CHANNEL_EVENT,
  CHANNEL_FOLEY,
  CHANNEL_AMBIANCE,
  CHANNEL_STREAM,
  CHANNEL_MAX,
};

class Sound {
public:
  ~Sound();

  long nsamples;
  int16_t* buffer;
};

typedef std::map<const char*, Sound*> NameToSound;

class SoundMgr {
public:
  SoundMgr();
  ~SoundMgr();

  Sound* get_sync(const char* sound, float scale);
  void play(Sound* sound, int channel);
  long stream(const char* sound, long start_sample);

  int channel_next_free[CHANNEL_MAX];
  NameToSound name_to_sound;
};

#endif
