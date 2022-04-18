; If set to 1, timing information is generated during music generation
; which is needed for Clinkster_GetInstrumentTrigger.
; Set it to 0 if you don't need this functionality.
%define CLINKSTER_GENERATE_TIMING_DATA 0

%include "platform.inc"
%include "music.inc"

;; ********** Definitions **********

global PUBLIC_FN(Clinkster_GenerateMusic, 0)
global PUBLIC_FN(Clinkster_GetInstrumentTrigger, 8)

global PUBLIC_DATA(Clinkster_MusicBuffer)
global PUBLIC_DATA(Clinkster_TicksPerSecond)
global PUBLIC_DATA(Clinkster_MusicLength)
global PUBLIC_DATA(Clinkster_NumTracks)
%if CLINKSTER_GENERATE_TIMING_DATA
global PUBLIC_DATA(Clinkster_NoteTiming)
%endif

%define SAMPLE_RATE 44100
%define WAVE_SIZE 65536

;; ********** Public variables **********

SECT_BSS(MusBuf) align=4
PUBLIC_DATA(Clinkster_MusicBuffer):
.align24
%ifdef SAMPLE_FORMAT_SHORT
	resw (TOTAL_SAMPLES*2)
	resw 2 ; padding to catch extra write in conversion
%else
	resd (TOTAL_SAMPLES*2)
	resd 2 ; padding to catch extra write in conversion
%endif

SECT_RDATA(tps) align=4
PUBLIC_DATA(Clinkster_TicksPerSecond):
	dd TICKS_PER_SECOND

SECT_RDATA(muslen) align=4
PUBLIC_DATA(Clinkster_MusicLength):
	dd MUSIC_LENGTH

SECT_RDATA(numtr) align=4
PUBLIC_DATA(Clinkster_NumTracks):
	dd NUMTRACKS

%if CLINKSTER_GENERATE_TIMING_DATA
SECT_BSS(musdat) align=4
PUBLIC_DATA(Clinkster_NoteTiming):
.align16
	resd 2*(NUMTRACKS<<LOGNUMTICKS)

ECT_DATA(timing) align=4
timing_ptr: dd Clinkster_NoteTiming
%endif

;; ********** Internal buffers **********

SECT_BSS(MixBuf) align=4
	resd MAX_DELAY_LENGTH
PUBLIC_DATA(MixingBuffer):
.align24
	resd TOTAL_SAMPLES

SECT_BSS(InstrBuf) align=4
PUBLIC_DATA(InstrumentBuffer):
.align16
	resd MAX_INSTRUMENT_SUBSAMPLES

SECT_BSS(InstrRen) align=4
	resd 256
PUBLIC_DATA(InstrumentRender):
.align16
	resd MAX_INSTRUMENT_SUBSAMPLES

SECT_BSS(InstrSto) align=4
PUBLIC_DATA(InstrumentStore):
.align16
	resd MAX_TOTAL_INSTRUMENT_SAMPLES

SECT_BSS(InstrPoi) align=4
PUBLIC_DATA(InstrumentPointers):
.align16
	resd MAX_TRACK_INSTRUMENT_RENDERS+1

SECT_BSS(wforms) align=4
waveforms:
.align16
	resd	6*WAVE_SIZE


;; ********** Instrument parameter access **********

SECT_RDATA(paramw) align=4
param_weights:
	dd 0.125					; Release
	dd 0.125					; Decay
	dd 0.125					; Attack
%if USES_GAIN
	dd 0.125					; Gain
%endif
%if USES_INDEXDECAY
	dd 0.0009765625				; IndexDecay
%endif
	dd 0.0009765625				; M PitchDecay
	dd 0.0009765625				; B PitchDecay
	dd 0.083333333333			; M Pitch
	dd 0.083333333333			; B Pitch
	dd 0.0000152587890625		; Volume
%if USES_PANNING
	dd 0.0000152587890625		; Volume
%endif
	dd 0.03125					; Sustain
	dd 16307					; RandomSeed
	dd 1						; Layers
	dd 4096.0					; Index
	dd 0.125					; Index Spread
	dd 0.0009765625				; M Detune
	dd 0.0009765625				; B Detune
	dd 65536					; ModWave
	dd 65536					; BaseWave

struc instr_params
	ip_basewave:	resd 1
	ip_modwave:		resd 1
	ip_bdetune:		resd 1
	ip_mdetune:		resd 1
	ip_indexspr:	resd 1
	ip_index:		resd 1
	ip_layers:		resd 1
	ip_randomseed:	resd 1
	ip_sustain:		resd 1
	ip_volume:		resd 1+USES_PANNING
	ip_bpitch:		resd 1
	ip_mpitch:		resd 1
	ip_bpitchd:		resd 1
	ip_mpitchd:		resd 1
%if USES_INDEXDECAY
	ip_indexd:		resd 1
%endif
%if USES_GAIN
	ip_gain:		resd 1
%endif
	ip_attack:		resd 1
	ip_decay:		resd 1
	ip_release:		resd 1
endstruc

%define ip_INT 0
%define ip_FLOAT 0
%define IP(f,t) dword [instrparams + ip_ %+ f + ip_ %+ t]
%define IPI(f,i,t) dword [instrparams + ip_ %+ f + ip_ %+ t + i]


;; ********** Internal constants and tables **********

SECT_RDATA(resamp) align=4
resamplefilter:
	db -1,-2,-4,-4,-2,3,14,30,51,98,116,126
	db 126,116,98,51,30,14,3,-2,-4,-4,-2,-1
resamplefilter_end:
FILTER_SIZE equ (resamplefilter_end-resamplefilter)

SECT_RDATA(wavestep) align=4
c_wavestep:		dd 0.000030517578125
SECT_RDATA(basefreq) align=4
c_basefreq:		dd 2.86698696365342
SECT_RDATA(halfnote) align=4
c_halfnote:		dd 1.05946309435929
SECT_RDATA(finalamp) align=4
c_finalamp:		dd 32767
SECT_RDATA(velfac) align=4
c_velocityfac:	dd 0.007874015748031496
SECT_RDATA(delaystr) align=4
c_delaystr:		dd DELAY_STRENGTH
SECT_RDATA(tempo) align=4
c_ticklength:	dd SUBSAMPLES_PER_TICK/4*4
SECT_RDATA(half) align=4
c_onehalf:		dd 0.5


;; ********** Internal global variables **********

SECT_BSS(vars) align=8
vars_align16

layer_random:	resd	1
stereo:			resd	1	; 0 for left channel, 2 for right channel
noteposptr:		resd	1
notesamptr:		resd	1

instrparams:
	resb	instr_params_size

layer_params:
layer_bfreq:	resq	1
layer_mfreq:	resq	1
layer_index:	resq	1
layer_bpitch:	resq	1
layer_mpitch:	resq	1
layer_bpitchd:	resq	1
layer_mpitchd:	resq	1
%if USES_INDEXDECAY
layer_indexd:	resq	1
%endif
%if USES_GAIN
layer_gain:		resq	1
%endif
layer_attack:	resq	1
layer_decay:	resq	1
layer_release:	resq	1
layer_phasetmp:	resd	1


;; ********** Generate the sound for one layer **********

SECT_TEXT(mklayer) align=1
makelayer:
	mov		edx, layer_params

	; Init random variables for layer
	fild	word [layer_random]
	mov		ecx, dword [layer_random]
	ror		ecx, cl
	dec		ecx
	mov		dword [layer_random], ecx

	fld		IP(bdetune, FLOAT)
	fmul	st0, st0
	fmulp	st1, st0
	fadd	st0, st1
	fstp	qword [edx]
	add		edx, byte 8

	fild	word [layer_random]
	mov		ecx, dword [layer_random]
	ror		ecx, cl
	dec		ecx
	mov		dword [layer_random], ecx

	fld		IP(mdetune, FLOAT)
	fmul	st0, st0
	fmulp	st1, st0
	fadd	st0, st1
	fstp	qword [edx]
	add		edx, byte 8

	fild	word [layer_random]
	mov		ecx, dword [layer_random]
	ror		ecx, cl
	dec		ecx
	mov		dword [layer_random], ecx

	fmul	IP(indexspr, FLOAT)
	fadd	IP(index, FLOAT)
	fstp	qword [edx]
	add		edx, byte 8

	; Init exponentiated variables for layer
	mov		edi, instrparams+ip_bpitch
	mov		ecx, 7+USES_INDEXDECAY+USES_GAIN
.powloop:
	fld		dword [edi]

	fld1
	fld		st1
	fprem
	fstp	st1
	f2xm1
	fld1
	faddp	st1, st0
	fscale

	fstp	qword [edx]
	add		edx, byte 8
	fstp	st0
	scasd
	loop	.powloop

	mov		ebx, edx	; phasetmp

	; Loop over samples
	fldz	; b phase
	fldz	; m phase
	mov		edi, PUBLIC_DATA(InstrumentBuffer)

	; Calculate max note size
	xor		eax, eax
%if USES_VELOCITY
	lodsb	; Skip velocity
%endif
%if USES_LONG_NOTES
	cmp		[esi], byte 0
	jge		near .short_notelen
	lodsb
	not		al
	shl		eax, 8
.short_notelen:
%endif
	lodsb	; Length of longest note with this tone
	mov		edx, SUBSAMPLES_PER_TICK
	mul		edx
	add		eax, MAX_RELEASE_SUBSAMPLES
	xchg	ecx, eax

.sampleloop:
	mov		edx, layer_params

	; Look up mod wave
	fist	dword [ebx]
	mov		eax, IP(modwave,INT)
	mov		ax, [ebx]
	fld		dword [waveforms + eax*4]

	; Adjust by index
	fmul	qword [edx + 2*8] ; layer_index
	fadd	st0, st2

	; Look up base wave
	fistp	dword [ebx]
	mov		eax, IP(basewave,INT)
	mov		ax, [ebx]
	fld		dword [waveforms + eax*4]

	; Update phases
	fld		qword [edx] ; layer_bfreq
	add		edx, byte 8
	fmul	qword [edx + 2*8] ; layer_bpitch
	faddp	st3, st0

	fld		qword [edx] ; layer_mfreq
	add		edx, byte 8
	fmul	qword [edx + 2*8] ; layer_mpitch
	faddp	st2, st0

%if USES_INDEXDECAY
	fld		qword [edx] ; layer_index
	fld1
	fadd	st1, st0
	fsubp	st1, st0
%endif
	add		edx, byte 8

	; Update pitches: p := (p-1)*d+1 
	fld1
	fld		qword [edx] ; layer_bpitch
	fsub	st0, st1
	fmul	qword [edx + 2*8] ; layer_bpitchd
	faddp	st1, st0
	fstp	qword [edx] ; layer_bpitch
	add		edx, byte 8

	fld1
	fld		qword [edx] ; layer_mpitch
	fsub	st0, st1
	fmul	qword [edx + 2*8] ; layer_mpitchd
	faddp	st1, st0
	fstp	qword [edx] ; layer_mpitch
	add		edx, byte 8

%if USES_INDEXDECAY
	fmul	qword [edx + 2*8] ; layer_indexd
	fstp	qword [edx - 3*8] ; layer_index
%endif

	; Add to existing layers
	fadd	dword [edi]
	fstp	dword [edi]
	scasd

	loop	.sampleloop

	fstp	st0
	fstp	st0
	ret


;; ********** Interpolate one section of amplitude envelope **********

SECT_TEXT(adsr) align=1
apply_adsr:
	; On condition g:
	;   st0 = amplitude target
	;   st1 = amplitude
	;	st2 = velocity / nlayers
	;   eax = number of samples
	;   ecx = sample index
	; On condition le:
	;   st0 = number of samples
	;   st1 = amplitude target
	;   st2 = amplitude
	;	st3 = velocity / nlayers
	;   ecx = sample index

	push	eax
	jg		.integer_length
	fimul	dword [c_finalamp]
	fistp	dword [esp]
.integer_length:
	fsub	st0, st1
	fild	dword [esp]
	pop		eax
	add		eax, ecx
	fdivp	st1, st0

.adsrloop:
	fld		dword [PUBLIC_DATA(InstrumentBuffer) + ecx*4]
	fmul	st0, st3	; velocity / nlayers
	fmul	st0, st2	; envelope value

%if USES_GAIN
	fld1
	fsubr	qword [layer_gain]
	fmul	st0, st1
	fmul	st0, st1
	fld1
	faddp	st1, st0
	fdivr	qword [layer_gain]
	fsqrt
	fmulp	st1, st0
%endif

	fstp	dword [PUBLIC_DATA(InstrumentRender) + ecx*4]

	fadd	st1, st0
	inc		ecx

	cmp		ecx, eax
	jl		.adsrloop

	fstp	st0
	ret


;; ********** Main music generation **********

SECT_TEXT(genMus) align=1
PUBLIC_FN(Clinkster_GenerateMusic, 0):
	pusha
	fninit

	; Make waveforms
	mov		edi, waveforms
%if USES_SINE
	fldz
	mov		ecx, WAVE_SIZE
.sineloop:
	fadd	dword [c_wavestep]
	fld		st0
	fldpi
	fmulp	st1, st0
	fsin
	fstp	dword [edi]
	scasd
	loop	.sineloop
	fstp	st0
%endif

%if USES_SAWTOOTH
	fld1
	fchs
	mov		ecx, WAVE_SIZE
.sawtoothloop:
	fadd	dword [c_wavestep]
	fst		dword [edi]
	scasd
	loop	.sawtoothloop
	fstp	st0
%endif

%if USES_SQUARE
	fld1
	fchs
	mov		ecx, WAVE_SIZE
.squareloop:
	cmp		ecx, WAVE_SIZE/2
	jne		.notflipsq
	fabs
.notflipsq:
	fst		dword [edi]
	scasd
	loop	.squareloop
	fstp	st0
%endif

%if USES_PARABOLA
	fld1
	fchs
	mov		ecx, WAVE_SIZE
.parabolaloop:
	fadd	dword [c_wavestep]
	fld		st0
	fmul	st0, st1
	fadd	st0, st0
	fld1
	fsubp	st1, st0
	fstp	dword [edi]
	scasd
	loop	.parabolaloop
	fstp	st0
%endif

%if USES_TRIANGLE
	fld1
	fchs
	mov ecx, WAVE_SIZE
.triangleloop:
	fadd	dword [c_wavestep]
	fld		st0
	fadd	st0, st1
	fabs
	fld1
	fsubp	st1, st0
	fstp	dword [edi]
	scasd
	loop	.triangleloop
	fstp st0
%endif

%if USES_NOISE
	fld1
	fchs
	mov		ecx, WAVE_SIZE
.noiseloop:
	fadd	dword [c_wavestep]
	fldpi
	fmulp	st1, st0
	fsin
	fst		dword [edi]
	scasd
	loop	.noiseloop
	fstp	st0
%endif

.stereoloop:
	mov dword [noteposptr], NotePositions
	mov dword [notesamptr], NoteSamples

	xor		eax, eax
	mov		edi, PUBLIC_DATA(MixingBuffer)
	mov		ecx, TOTAL_SAMPLES
	rep stosd

	mov		esi, InstrumentData
%if USES_DELAY
	jmp		short .trackloop

.delay:
	mov		eax, dword [stereo]
	mov		edx, (LEFT_DELAY_LENGTH-RIGHT_DELAY_LENGTH)*4/2
	mul		edx
	sub		eax, LEFT_DELAY_LENGTH*4
	mov		edi, PUBLIC_DATA(MixingBuffer)
	mov		ecx, TOTAL_SAMPLES
.delayloop:
	fld		dword [edi+eax]
	fmul	dword [c_delaystr]
	fadd	dword [edi]
	fstp	dword [edi]
	scasd
	loop	.delayloop
%endif

.trackloop:
	; ESI = instr data
	mov		edi, instrparams
	mov		ecx, instr_params_size/4
.ploop:
	lodsb
	movsx	eax, al
	push	eax
	fild	dword [esp]
	pop		eax
	fmul	dword [param_weights-4+ecx*4]

	fstp	dword [edi]
	scasd
	loop	.ploop

	mov		edi, instrparams+ip_bpitchd
	mov		ecx, 2+USES_INDEXDECAY
.cubeloop:
	fld		dword [edi]
	fld		st0
	fmul	st0, st0
	fmulp	st1, st0
	fstp	dword [edi]
	scasd
	loop	.cubeloop

	mov		ebx, PUBLIC_DATA(InstrumentPointers)
	mov		dword [ebx], PUBLIC_DATA(InstrumentStore)	; store first instrument instance address
	fld		dword [c_basefreq]

	; Loop over instrument tones
.toneloop:
	xor		eax, eax
	lodsb			; Tone
.freqloop:
	fmul	dword [c_halfnote]
	dec		eax
	jge		.freqloop

	; random seed for channel = RandomSeed * 16307 + channel * 12042
	mov		eax, dword [stereo]
	mov		edx, 12042/2
	mul		edx
	add		eax, IP(randomseed,INT)
	xchg	ecx, eax
	mov		dword [layer_random], ecx

	xor		eax, eax
	mov		edi, PUBLIC_DATA(InstrumentBuffer)
	mov		ecx, MAX_INSTRUMENT_SUBSAMPLES
	rep stosd

	; Loop over layers
	mov		ecx, IP(layers,INT)
.layerloop:
	pusha

	call	makelayer

	popa
	loop	.layerloop

.lengthloop:
%if USES_VELOCITY
	lodsb
	movsx	eax, al
	push	eax
	fild	dword [esp]
	pop		eax
	fmul	dword [c_velocityfac]
%else
	fld1
%endif
	fidiv	IP(layers,INT)

	xor		ecx, ecx			; sample index	
	fldz						; amplitude level

	fld1						; attack amplitude target
	fld		qword [layer_attack]; attack length
	call	apply_adsr

	fld		IP(sustain,FLOAT)	; decay amplitude target
	fld		qword [layer_decay]	; decay length
	call	apply_adsr

	xor		eax, eax
%if USES_LONG_NOTES
	cmp		[esi], byte 0
	jge		near .short_notelen
	lodsb
	not		al
	shl		eax, 8
.short_notelen:
%endif
	lodsb						; note length in ticks
	mov		edx, SUBSAMPLES_PER_TICK
	mul		edx
	sub		eax, ecx			; note length exclusing attack and decay
	jle		.nosustain			; attack + decay overflows note length
	fld		IP(sustain,FLOAT)	; sustain amplitude target
	call	apply_adsr
.nosustain:

	fldz						; release amplitude target
	fld		qword [layer_release];release length
	call	apply_adsr

	fldz						; padding amplitude
	fld1						; padding length
	call	apply_adsr

	fstp	st0
	fstp	st0

	; Resampling
	mov		edi, [ebx]	; instrument instance address
	add		ebx, byte 4

	xchg	edx, eax
	mov		ebp, PUBLIC_DATA(InstrumentRender) - FILTER_SIZE*4
.resampleloop:
	fldz
	mov		ecx, FILTER_SIZE
.filterloop:
	movsx	eax, byte [resamplefilter + ecx - 1]
	push	eax
	fild	dword [esp]
	pop		eax
	fmul	dword [ebp + ecx*4]
	faddp	st1, st0
	loop	.filterloop

%if USES_PANNING
	mov		eax, dword [stereo]
	fmul	IPI(volume,eax*2,FLOAT)
%else
	fmul	IP(volume,FLOAT)
%endif
	fstp	dword [edi]
	scasd
	add		ebp, byte 4*4

	sub		edx, byte 4
	jg		.resampleloop

	mov		[ebx], edi	; store instrument instance address

	cmp		[esi], byte 0
	jne		near .lengthloop
	lodsb

	cmp		[esi], byte 0
	jge		near .toneloop
	lodsb
	fstp	st0

	; Mixing
	mov		ebx, PUBLIC_DATA(InstrumentPointers)
	mov		ebp, PUBLIC_DATA(MixingBuffer)
	xchg	esi, dword [notesamptr]
.noteloop:
	xchg	esi, dword [noteposptr]
	xor		eax, eax
	cmp		[esi], byte 0
	jge		near .short_notepos
	lodsb
	not		al
	shl		eax, 8
.short_notepos:
	lodsb
	mov		edx, SUBSAMPLES_PER_TICK/4*4
	mul		edx
	add		ebp, eax

%if CLINKSTER_GENERATE_TIMING_DATA
	mov		ecx, SUBSAMPLES_PER_TICK/4*4
	div		ecx
	xchg	edx, eax
	mov		edi, [timing_ptr]
	mov		eax, [edi]
	mov		ecx, edx
	rep stosd
	mov		[timing_ptr], edi
	add		eax, edx
	stosd
%endif

	xchg	esi, dword [noteposptr]
	xor		eax, eax
	lodsb
	mov		edx, dword [ebx + eax*4] ; Instrument instance ptr
	mov		edi, ebp

.mixloop:
	fld		dword [edx]
	fadd	dword [edi]
	fstp	dword [edi]
	scasd
	add		edx, byte 4

	cmp		edx, dword [ebx + eax*4 + 4]
	jl		.mixloop

	cmp		[esi], byte 0
	jge		near .noteloop
	lodsb
	xchg	esi, dword [notesamptr]

%if CLINKSTER_GENERATE_TIMING_DATA
	mov		ecx, 1<<LOGNUMTICKS
	mov		edi, [timing_ptr]
	mov		eax, [edi]
	sub		ecx, eax
	rep		stosd
	mov		[timing_ptr], edi
%endif

	cmp		[esi], byte 0
	jge		near .trackloop
	lodsb

%if USES_DELAY
	cmp		[esi], byte 0
	jge		near .delay
%endif
%ifdef SAMPLE_FORMAT_SHORT
	; Clamp and convert to shorts
	fld1
	mov		edi, PUBLIC_DATA(Clinkster_MusicBuffer)
	mov		ecx, TOTAL_SAMPLES
	add		edi, dword [stereo]
.sloop:
	fld		dword [PUBLIC_DATA(MixingBuffer) + ecx*4]
	fcomi	st0, st1
	fcmovnb	st0, st1
	fchs
	fcomi	st0, st1
	fcmovnb	st0, st1
	fchs

	fimul	dword [c_finalamp]
	fistp	word [edi + ecx*4]
	loop	.sloop
	fstp	st0
%else
	mov		edi, PUBLIC_DATA(Clinkster_MusicBuffer)
	mov		ecx, TOTAL_SAMPLES
	mov     eax, dword [stereo]
	shl     eax, 1
	add		edi, eax
.sloop:
	mov     eax, [PUBLIC_DATA(MixingBuffer) + ecx*4]
	mov     [edi + ecx*8], eax
	loop	.sloop
%endif	

	xor		byte [stereo], 2
	jne		.stereoloop

	popa
	ret

;; ********** Get time since instrument trigger **********

%if CLINKSTER_GENERATE_TIMING_DATA
SECT_TEXT(insttrig) align=1
PUBLIC_FN(Clinkster_GetInstrumentTrigger, 8):
	cvttss2si eax, [esp+8]
	mov		ecx, [esp+4]
	shl		ecx, LOGNUMTICKS+2
	fld		dword [esp+8]
	fisub	dword [PUBLIC_DATA(Clinkster_NoteTiming)+ecx+eax*4]
	ret		8
%endif

