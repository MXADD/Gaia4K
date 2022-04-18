;
; (c)MX^Addict
;

;
; WinAPI import
;
%macro WINAPI_FUNC 2
	extern __imp__ %+ %1 %+ @ %+ %2
	%define %1 [__imp__ %+ %1 %+ @ %+ %2]
%endmacro

;
; OpenGL import
;
%macro GL_FUNC 2
;section _ %+ %1 data align=1
section _ %+ %2 data align=1
%1:
%defstr %[%1 %+ __str] %1
	db %1 %+ __str, 0
%endmacro

;
; OpenGL constants
;
GL_TEXTURE_2D 		  EQU 0x0de1
GL_FRAGMENT_SHADER    EQU 0x8b30
GL_LINEAR 			  EQU 0x2601
GL_TEXTURE_MIN_FILTER EQU 0x2801
GL_TEXTURE0 		  EQU 0x84c0
GL_RGBA8 			  EQU 0x8058
GL_BYTE               EQU 0x1400
GL_UNSIGNED_BYTE      EQU 0x1401
GL_SHORT              EQU 0x1402
GL_UNSIGNED_SHORT     EQU 0x1403
GL_INT                EQU 0x1404
GL_UNSIGNED_INT       EQU 0x1405
GL_FLOAT              EQU 0x1406
GL_2_BYTES            EQU 0x1407
GL_3_BYTES            EQU 0x1408
GL_4_BYTES            EQU 0x1409
GL_DOUBLE             EQU 0x140A
GL_ALL_ATTRIB_BITS    EQU 0x000fffff

;
; WinAPI functions imports
;
%ifdef FULLSCREEN
WINAPI_FUNC ChangeDisplaySettingsA, 8
WINAPI_FUNC ShowCursor, 4
%endif
WINAPI_FUNC CreateWindowExA, 48
WINAPI_FUNC GetDC, 4
WINAPI_FUNC ChoosePixelFormat, 8
WINAPI_FUNC SetPixelFormat, 12
WINAPI_FUNC ExitProcess, 4
WINAPI_FUNC GetAsyncKeyState, 4
WINAPI_FUNC PeekMessageA, 20
WINAPI_FUNC SwapBuffers, 4
WINAPI_FUNC waveOutGetPosition, 12
WINAPI_FUNC waveOutOpen, 24
WINAPI_FUNC waveOutWrite, 12
WINAPI_FUNC wglCreateContext, 4
WINAPI_FUNC wglMakeCurrent, 8
WINAPI_FUNC wglGetProcAddress, 4
WINAPI_FUNC glRects, 16
%ifdef POSTPROCESS
WINAPI_FUNC glBindTexture, 8
WINAPI_FUNC glCopyTexImage2D, 32
WINAPI_FUNC glTexParameteri, 12
%endif
%ifdef FONTSMSG
WINAPI_FUNC CreateFontA, 56
WINAPI_FUNC SelectObject, 8
WINAPI_FUNC wglUseFontBitmapsA, 16
WINAPI_FUNC glPushAttrib, 4
WINAPI_FUNC glListBase, 4
WINAPI_FUNC glCallLists, 12
WINAPI_FUNC glRasterPos2i, 8
WINAPI_FUNC glScalef, 12
WINAPI_FUNC glPushMatrix, 0
WINAPI_FUNC glPopMatrix, 0
%endif

GL_FUNC glCreateShaderProgramv, crtshp
GL_FUNC glUseProgram, useprg
GL_FUNC glUniform4i, unif4i
%ifdef POSTPROCESS
GL_FUNC glUniform1i, unif1i
GL_FUNC glActiveTexture, acttex
%endif

;
; WinAPI function caller
;
%macro FNCALL 1-*
	%rep %0-1
		%rotate -1
		push %1
	%endrep
	%rotate -1
	call %1
%endmacro
