#include <assert.h>
#include "audio.h"
#include "bcm_host.h"
#include "ilclient.h"

#define NUM_SAMPLES 1024
ILCLIENT_T *client;
COMPONENT_T *audio_render;

void input_buffer_callback(void *data, COMPONENT_T *comp) {
  OMX_BUFFERHEADERTYPE *hdr = NULL;
  hdr = ilclient_get_input_buffer(audio_render, 100, 0);

  if(hdr) {
    OMX_ERRORTYPE error;
    audio_fill_buffer((int16_t*)hdr->pBuffer, NUM_SAMPLES);
    hdr->nOffset = 0;
    hdr->nFilledLen = NUM_SAMPLES;
    error = OMX_EmptyThisBuffer(ILC_GET_HANDLE(audio_render), hdr);
    assert(error == OMX_ErrorNone);
  }
}

void native_audio_init() {
  OMX_ERRORTYPE error;
  OMX_PARAM_PORTDEFINITIONTYPE param;
  OMX_AUDIO_PARAM_PCMMODETYPE pcm;
  int32_t s;
  
  // create and start up everything
  client = ilclient_init();
  assert(client != NULL);
  
  ilclient_set_empty_buffer_done_callback(client, input_buffer_callback, NULL);
  
  error = OMX_Init();
  assert(error == OMX_ErrorNone);
  
  ilclient_create_component(client, &audio_render, "audio_render", ILCLIENT_ENABLE_INPUT_BUFFERS | ILCLIENT_DISABLE_ALL_PORTS);
  assert(audio_render != NULL);
  
  // set up the number/size of buffers
  memset(&param, 0, sizeof(OMX_PARAM_PORTDEFINITIONTYPE));
  param.nSize = sizeof(OMX_PARAM_PORTDEFINITIONTYPE);
  param.nVersion.nVersion = OMX_VERSION;
  param.nPortIndex = 100;
  
  error = OMX_GetParameter(ILC_GET_HANDLE(audio_render),
			   OMX_IndexParamPortDefinition, &param);
  assert(error == OMX_ErrorNone);
  
  param.nBufferSize = NUM_SAMPLES;
  param.nBufferCountActual = 2;
  
  error = OMX_SetParameter(ILC_GET_HANDLE(audio_render),
			   OMX_IndexParamPortDefinition, &param);
  assert(error == OMX_ErrorNone);
  
  // set the pcm parameters
  memset(&pcm, 0, sizeof(OMX_AUDIO_PARAM_PCMMODETYPE));
  pcm.nSize = sizeof(OMX_AUDIO_PARAM_PCMMODETYPE);
  pcm.nVersion.nVersion = OMX_VERSION;
  pcm.nPortIndex = 100;
  pcm.nChannels = 1;
  pcm.eNumData = OMX_NumericalDataSigned;
  pcm.eEndian = OMX_EndianLittle;
  pcm.nSamplingRate = SAMPLE_FREQ;
  pcm.bInterleaved = OMX_TRUE;
  pcm.nBitPerSample = 16;
  pcm.ePCMMode = OMX_AUDIO_PCMModeLinear;
  pcm.eChannelMapping[0] = OMX_AUDIO_ChannelCF;
  
  error = OMX_SetParameter(ILC_GET_HANDLE(audio_render), OMX_IndexParamAudioPcm, &pcm);
  assert(error == OMX_ErrorNone);
  
  assert(ilclient_change_component_state(audio_render, OMX_StateIdle) == 0);
  if(ilclient_enable_port_buffers(audio_render, 100, NULL, NULL, NULL) < 0) {
    // error
    ilclient_change_component_state(audio_render, OMX_StateLoaded);

    COMPONENT_T* list[2];
    list[0] = audio_render;
    list[1] = 0;
    ilclient_cleanup_components(list);
    
    error = OMX_Deinit();
    assert(error == OMX_ErrorNone);
    
    ilclient_destroy(client);
    exit(-1);
  }
  
  assert(ilclient_change_component_state(audio_render, OMX_StateExecuting) == 0);

  // set the destination
  OMX_CONFIG_BRCMAUDIODESTINATIONTYPE ar_dest;
  
  const char* name = "hdmi";
  memset(&ar_dest, 0, sizeof(ar_dest));
  ar_dest.nSize = sizeof(OMX_CONFIG_BRCMAUDIODESTINATIONTYPE);
  ar_dest.nVersion.nVersion = OMX_VERSION;
  strcpy((char *)ar_dest.sName, name);

  error = OMX_SetConfig(ILC_GET_HANDLE(audio_render), OMX_IndexConfigBrcmAudioDestination, &ar_dest);
  assert(error == OMX_ErrorNone);

  // get the buffer flow going
  input_buffer_callback(NULL, NULL);
}
