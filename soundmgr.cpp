#include "soundmgr.h"
#include "testlib.h"
#include "utils.h"

Sound::~Sound() {
  free(buffer);
}

SoundMgr::SoundMgr() {
}

SoundMgr::~SoundMgr() {
  for(NameToSound::iterator iter = name_to_sound.begin();
      iter != name_to_sound.end(); ++iter) {
    free((void*)iter->first);
    delete(iter->second);
  }
}

Sound* SoundMgr::get_sync(const char* filename, float scale) {
  NameToSound::iterator iter = name_to_sound.find(filename);
  if(iter != name_to_sound.end()) return iter->second;

  FILE* f = fopen(filename, "rb");
  if(f == NULL) {
    fail_exit("sound file `%s' could not be found\n", filename);
  }

  OggVorbis_File vf;
  if(ov_open(f, &vf, NULL, 0) < 0) {
    fail_exit("sound file `%s' couldn't be opened as vorbis\n", filename);
  }

  Sound* sound = new Sound();
  const size_t temp_buffer_size = 4096;
  int16_t* temp_buffer = (int16_t*)malloc(temp_buffer_size);

  vorbis_info *vi = ov_info(&vf, -1);
  sound->nsamples = ov_pcm_total(&vf, -1);
  sound->buffer = (int16_t*)malloc(sound->nsamples * sizeof(int16_t));

  int channels = vi->channels;
  int rate = vi->rate;

  int section;
  int read = 0;
  while(read < sound->nsamples) {
    int ret = ov_read(&vf, (char*)temp_buffer, temp_buffer_size, &section);
    if(ret == 0) {
      // done
      break;
    } else if(ret < 0) {
      if(ret == OV_EBADLINK) {
        fail_exit("corrupt bitstream in %s\n", filename);
      }
    }

    // pick out the bits we care about
    long samples = (ret / sizeof(int16_t)) / channels;
    int ii;
    for(ii = 0; (ii < samples) && (read + ii < sound->nsamples); ++ii) {
      sound->buffer[read + ii] = temp_buffer[ii * channels] * scale;
    }

    read += ii;
  }

  sound->nsamples = read;

  ov_clear(&vf);
  free(temp_buffer);
  name_to_sound.insert(std::make_pair(strdup(filename), sound));
  return sound;
}

void SoundMgr::play(Sound* sound, int channel) {
  // ignore channel for now
  audio_enqueue(buffersampler_make(sound->buffer, audio_current_sample(),
                                   sound->nsamples));
}

long SoundMgr::stream(const char* sound, long start_sample) {
  Sampler oggsampler = oggsampler_make(sound, start_sample, 0.2);
  audio_enqueue(oggsampler);
  return start_sample + oggsampler->duration_samples;
}
