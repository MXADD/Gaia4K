;
; Intro stub (c)MX^Addict
;
BITS 32

;
; Resolution
;
WIDTH  equ 1600
HEIGHT equ  900

;
; Fullscreen ?
;
%define FULLSCREEN

;
; Use postprocessing ?
;
; %define POSTPROCESS

;
; Use beat detector ?
;
%define BEATDETECTOR

;
; Use fonts
;
%define FONTSMSG

;
; Music
;
%include "clinkster.inc"
%define  TOTAL_SAMPLES 2883584

;
; Misc defines
;
%include "misc.inc"

;
; Data
;

;
; Screen mode settings (only in fullscreen)
; DEVMODE = 
; { 
; 	{0}, 0, 0, sizeof(DEVMODE), 0, DM_PELSWIDTH|DM_PELSHEIGHT, {0}, 0, 0, 0, 0, 0, {0}, 0, 0, XRES, YRES, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
; };
;
%ifdef FULLSCREEN
section _devmode data align=1
devmode:
	times 9 dd 0
	db 0x9c, 0, 0, 0
	db 0, 0, 0x1c, 0
	times 15 dd 0
	dd 020H, WIDTH, HEIGHT
	times 10 dd 0
%endif

;
; Pixel format descriptor
; PIXELFORMATDESCRIPTOR = 
; {
;     sizeof(PIXELFORMATDESCRIPTOR), 1, PFD_DRAW_TO_WINDOW|PFD_SUPPORT_OPENGL|PFD_DOUBLEBUFFER, PFD_TYPE_RGBA, 32, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 32, 0, 0, PFD_MAIN_PLANE, 0, 0, 0, 0
; };
;
section _pfd data align=1
pfd:
	dw  028H
	dw	01H
	dd	025H
	db	00H
	db	020H
	db	00H
	db	00H
	db	00H
	db	00H
	db	00H
	db	00H
	db	08H
	db	00H
	db	00H
	db	00H
	db	00H
	db	00H
	db	00H
	db	020H
	db	00H
	db	00H
	db	00H
	db	00H
	dd	00H
	dd	00H
	dd	00H

;
; Wave format
;
section _wavefmt data align=1
wavefmt:
	%ifdef SAMPLE_FORMAT_SHORT
	dw 1 					; wFormatTag = WAVE_FORMAT_PCM
	%else
	dw 3 					; wFormatTag = WAVE_FORMAT_IEEE_FLOAT
	%endif
	dw 2 					; nChannels
	dd SAMPLE_RATE 			; nSamplesPerSec
	%ifdef SAMPLE_FORMAT_SHORT
	dd SAMPLE_RATE * 2 * 2	; nAvgBytesPerSec (SAMPLE_RATE * sizeof(SAMPLE) * nChannels)
    dw 2 * 2 				; nBlockAlign (sizeof(SAMPLE) * nChannels)
    dw 16	 				; wBitsPerSample
	%else
	dd SAMPLE_RATE * 4 * 2	; nAvgBytesPerSec (SAMPLE_RATE * sizeof(SAMPLE) * nChannels)
    dw 4 * 2 				; nBlockAlign (sizeof(SAMPLE) * nChannels)
    dw 32	 				; wBitsPerSample
	%endif
    dw 0 					; cbSize

;
; Wave header
;
section _wavehdr data align=1
wavehdr:
	dd PUBLIC_DATA(Clinkster_MusicBuffer); lpData
	%ifdef SAMPLE_FORMAT_SHORT
	dd TOTAL_SAMPLES * 2 * 2			 ; dwBufferLength (TOTAL_SAMPLES * sizeof(SAMPLE) * nChannels)
	%else
	dd TOTAL_SAMPLES * 4 * 2			 ; dwBufferLength (TOTAL_SAMPLES * sizeof(SAMPLE) * nChannels)
	%endif
	times 2 dd 0 						 ; unused stuff
	dd 2 								 ; dwFlags WHDR_PREPARED  =  0x00000002
	dd 0
	times 4 dd 0 						 ; unused stuff
	wavehdr_size EQU ($ - wavehdr)

%ifdef FONTSMSG
;
; Text(s)
;
section _txt00s data align=1
_txt00d: 
	db '        -=:Greetingz:=-        '
	db 'ANADUNE-ANDROMEDA-ALTAIR-COCOON'
	db 'CONSPIRACY-CNCD-ELUDE-FAIRLIGHT'
	db '   FARBRAUSCH-FLOPPY-FUTURIS   '
	db '    GHOSTOWN-HAUJOBB-LAMERS    '
	db 'MADWIZARDS-MERCURY-MYSTIC BYTES'
	db '  THE BLACK LOTUS-WANTED TEAM  '
	db '           AND YOU !           '
_fontface: 	
	db 'Consolas', 0
%endif

;
; BSS sections
;
section _mmtime bss align=1
mmtime: resb 12

section _waveout bss align=1
waveout: resd 8

;
; Shaders
;
section _shader data align=1
%include "fragment.inc"
%ifdef POSTPROCESS
%include "post.inc"
%endif
section _shdrptr data align=1
src_main:
	dd _fragment_frag
%ifdef POSTPROCESS	
src_post:
	dd _post_frag
%endif
	
%ifdef BEATDETECTOR
%ifndef SAMPLE_FORMAT_SHORT
section _btdtcd data align=1
const_beat_treshold:
	dd 0x3ECD0000 ; 0.4f
%endif

section _btdtcb bss align=1
old_slow_beat: resd 1

;
; Beat detector
; in -> ebx = #samples
; out-> beat in edx, preserves all other registers
;
section _btdtc text align=1
_detect_beats:
%ifdef SAMPLE_FORMAT_SHORT

	push   eax
	push   ebx
	push   ecx
	push   esi
	
	; Clamp ebx to >= 2560 && <= TOTAL_SAMPLES-2560
	
	mov    ecx, 2560
	cmp    ebx, ecx
	cmovb  ebx, ecx
	
	mov    ecx, TOTAL_SAMPLES-2560
	cmp	   ebx, ecx
	cmova  ebx, ecx	
	
	; Reset beat indicator
	
	xor    edx, edx
	
	; Prepare loop for 200 iterations
	
	mov    ecx, 200
	lea	   esi, PUBLIC_DATA(Clinkster_MusicBuffer)[ebx*4-(50*16*4)]

	; Main loop
.beatloop:
	mov    ax, [esi]   ; Left channel only
	
	mov    bx, ax      ; store ax in bx
	neg    ax
	cmovl  ax,  bx     ; if ax is now negative, restore its saved value so we have abs(ax) now

	cmp    ax, 13107   ; if (ax > Treshold) edx++;
	jbe	   SHORT .beatnoinc
	inc    edx
.beatnoinc:	

	add	   esi, 16*4
	dec    ecx
	jne	   SHORT .beatloop	
	
	pop    esi
	pop    ecx
	pop    ebx	
	pop    eax

%else

	push   eax
	push   ebx
	push   ecx
	
	; Clamp ebx to >= 2560 && <= TOTAL_SAMPLES-2560
	
	mov    ecx, 2560
	cmp    ebx, ecx
	cmovb  ebx, ecx
	
	mov    ecx, TOTAL_SAMPLES-2560
	cmp	   ebx, ecx
	cmova  ebx, ecx	
	
	; Reset beat indicator
	
	xor    edx, edx
	
	; Prepare loop for 200 iterations
	
	mov    cl, 200
	lea	   ebx, PUBLIC_DATA(Clinkster_MusicBuffer)[ebx*8-(50*16*8)]
	fld    dword [const_beat_treshold]
	
	; Main loop
.beatloop:
	
	fld    dword [ebx]
	fabs

	; if (st0 > Treshold) edx++;
	
	fcomip st0, st1
	jb 	   SHORT .beatnoinc

	inc    edx
.beatnoinc:	

	add	   ebx, 16*8			
	dec    cl
	jne	   SHORT .beatloop	
	
	fstp   st0
	
	pop    ecx
	pop    ebx	
	pop    eax
	
%endif
	ret
%endif
	
;
; Code, Main entry
;
section _text text align=1
_start:
	%define ZERO 0
	
	;
	; Calculate music
	;
	call PUBLIC_FN(Clinkster_GenerateMusic, 0)
;	FNCALL CreateThread, ZERO, ZERO, PUBLIC_FN(Clinkster_GenerateMusic, 0), ZERO, ZERO, ZERO ; may do this in async ...
	
	;
	; Switch to fullscreen & create window
	;
%ifdef FULLSCREEN
	FNCALL ChangeDisplaySettingsA, devmode, 4
	FNCALL ShowCursor, ZERO
	FNCALL CreateWindowExA, ZERO, 0x0000c018, ZERO, 0x91000000, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO
%else
	FNCALL CreateWindowExA, ZERO, 0x0000c018, ZERO, 0x90000000, ZERO, ZERO, WIDTH, HEIGHT, ZERO, ZERO, ZERO, ZERO
%endif

	;
	; Initialize OpenGL
	;
	FNCALL GetDC, eax
	mov ebp, eax ; ebp = HDC
	FNCALL ChoosePixelFormat, ebp, pfd
	FNCALL SetPixelFormat, ebp, eax, pfd
	FNCALL wglCreateContext, ebp
	FNCALL wglMakeCurrent, ebp, eax

%ifdef FONTSMSG
		;
		; Create font
		;
		FNCALL CreateFontA, 48, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, ZERO, _fontface ; First argument is font height, last is font face name (may be null)
		FNCALL SelectObject, ebp, eax
		FNCALL wglUseFontBitmapsA, ebp, ZERO, 255, 1
%endif

	;
	; Create main shader and store it in esi
	;
	FNCALL wglGetProcAddress, glCreateShaderProgramv
	FNCALL eax, GL_FRAGMENT_SHADER, 1, src_main
	mov esi, eax ; esi = main shader

	%ifdef POSTPROCESS
		;
		; Create post shader and store it in edi
		;
		FNCALL wglGetProcAddress, glCreateShaderProgramv
		FNCALL eax, GL_FRAGMENT_SHADER, 1, src_post
		mov edi, eax ; edi = post shader
	%endif

	;
	; Play music
	;
	FNCALL waveOutOpen, waveout, byte -1, wavefmt, ZERO, ZERO, ZERO
	FNCALL waveOutWrite, dword [waveout], wavehdr, wavehdr_size

	;
	; Main loop
	;
	.mainloop:
		;
		; Query position of music & terminate if reached end
		;
		FNCALL waveOutGetPosition, dword [waveout], mmtime, 12
		mov ebx, dword [mmtime + 4]
		%ifdef SAMPLE_FORMAT_SHORT
			shr ebx, 2 ; divide by 4 (sizeof(SAMPLE) * nChannels) to get #samples
		%else
			shr ebx, 3 ; divide by 8 (sizeof(SAMPLE) * nChannels) to get #samples
		%endif
		cmp ebx, TOTAL_SAMPLES
		jge .exit
		
		;
		; Use main shader
		;
		FNCALL wglGetProcAddress, glUseProgram
		FNCALL eax, esi
		
		%ifdef BEATDETECTOR
			FNCALL wglGetProcAddress, glUniform4i

			;
			; Accumulate beats history & calc single beat
			;
			push ebx	
			xor ecx, ecx
			sub ebx, 4096
			call _detect_beats                      ; ebx = #samples, return beat in edx
			add ecx, edx
			add ebx, 2048
			call _detect_beats                      ; ebx = #samples, return beat in edx
			add ecx, edx
			add ebx, 4096
			call _detect_beats                      ; ebx = #samples, return beat in edx
			add ecx, edx
			add ebx, 2048
			call _detect_beats                      ; ebx = #samples, return beat in edx
			add ecx, edx
			add ecx, dword [old_slow_beat]
			shr ecx, 1
			mov dword [old_slow_beat], ecx
			pop ebx			
			call _detect_beats                      ; ebx = #samples, return beat in edx

			FNCALL eax, ZERO, ebx, edx, ecx, ecx    ; ebx = #samples, edx = Beat, ecx = Accumulated beats, last not used, ecx repeated for smaller code
		%else
			FNCALL wglGetProcAddress, glUniform4i
			FNCALL eax, ZERO, ebx, ZERO, ZERO, ZERO ; ebx = #samples, rest is zero
		%endif
		
		FNCALL glRects, byte -1, byte -1, byte 1, byte 1

		%ifdef POSTPROCESS
			;
			; Use post shader
			;
			FNCALL glBindTexture, GL_TEXTURE_2D, 1
			FNCALL glTexParameteri, GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR
			FNCALL glCopyTexImage2D, GL_TEXTURE_2D, ZERO, GL_RGBA8, ZERO, ZERO, WIDTH, HEIGHT, ZERO
			FNCALL wglGetProcAddress, glActiveTexture
			FNCALL eax, GL_TEXTURE0
			FNCALL wglGetProcAddress, glUseProgram
			FNCALL eax, edi
			FNCALL wglGetProcAddress, glUniform1i
			FNCALL eax, ZERO, ZERO
			FNCALL glRects, byte -1, byte -1, byte 1, byte 1
		%endif
		%ifdef FONTSMSG
			;
			; Prepare for write
			;
			FNCALL wglGetProcAddress, glUseProgram
			FNCALL eax, ZERO
			FNCALL glListBase, 1
			FNCALL glPushMatrix
			FNCALL glScalef, 0x3a830000, 0x3a830000, 0x3a830000 ; 0.0099 x 3, to use raster position as int (from -1000 to 1000, both X and Y)

			;
			; Write text(s) at location(s)
			;
			; ebx - #Samples
			
			mov ecx, 8							; Number of text lines
			push esi
			mov esi, _txt00d
.txtloop:
			push ecx
			imul eax, ecx, 100					; 100 - Lines spacing

			; Calculate scrolling offset from #Samples

			mov edx, ebx
			shr edx, 7							; Scroll speed
			sub edx, 21500						; Scroll start time
			add eax, edx

			FNCALL glRasterPos2i, -430, eax     ; First argument is XOffset
			FNCALL glCallLists, 31, GL_UNSIGNED_BYTE, esi

			add esi, 31
			pop ecx
			dec ecx
			jnz .txtloop
			pop esi

			FNCALL glPopMatrix
		%endif
		;
		; Swap, process messages & look at ESC key
		;
		FNCALL SwapBuffers, ebp ; ebp = HDC 
		FNCALL PeekMessageA, ZERO, ZERO, ZERO, ZERO, 1 ; PM_REMOVE = 1
		FNCALL GetAsyncKeyState, 27 ; VK_ESCAPE = 27
	jz .mainloop
.exit:
	call ExitProcess
