%include "platform.inc"

extern PUBLIC_FN(Clinkster_GenerateMusic, 0)
extern PUBLIC_FN(Clinkster_GetInstrumentTrigger, 8)

extern PUBLIC_DATA(Clinkster_MusicBuffer)
extern PUBLIC_DATA(Clinkster_TicksPerSecond)
extern PUBLIC_DATA(Clinkster_MusicLength)
extern PUBLIC_DATA(Clinkster_NumTracks)

%define SAMPLE_RATE 44100
; %define SAMPLE_FORMAT_SHORT
