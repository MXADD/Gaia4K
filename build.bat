@echo off

IF DEFINED PROJECTCONFIG GOTO PROJECTCONFIGDEFINED
set PROJECTCONFIG=Normal
:PROJECTCONFIGDEFINED

rem Clinkler libs
set LIBS=/LIBPATH:libs opengl32.lib winmm.lib kernel32.lib user32.lib gdi32.lib

rem Clinkler options (fast)
set OPTSFAST=/ENTRY:start /CRINKLER /RANGE:opengl32 /HASHTRIES:100 /COMPMODE:FAST /ORDERTRIES:2000 /REPORT:out.html /HASHSIZE:100 /TINYIMPORT /UNSAFEIMPORT /UNALIGNCODE /SATURATE /NOINITIALIZERS /TRANSFORM:CALLS /TRUNCATEFLOATS:32 /SUBSYSTEM:WINDOWS

rem Clinkler options (medium)
set OPTSMEDIUM=/ENTRY:start /CRINKLER /RANGE:opengl32 /HASHTRIES:512 /COMPMODE:SLOW /ORDERTRIES:9000 /REPORT:out.html /HASHSIZE:1000 /TINYIMPORT /UNSAFEIMPORT /UNALIGNCODE /SATURATE /NOINITIALIZERS /TRANSFORM:CALLS /TRUNCATEFLOATS:32 /SUBSYSTEM:WINDOWS

rem Clinkler options (slow)
set OPTSSLOW=/ENTRY:start /CRINKLER /RANGE:opengl32 /HASHTRIES:1000 /COMPMODE:VERYSLOW /ORDERTRIES:30000 /REPORT:out.html /HASHSIZE:1000 /TINYIMPORT /UNSAFEIMPORT /UNALIGNCODE /SATURATE /NOINITIALIZERS /TRANSFORM:CALLS /TRUNCATEFLOATS:32 /SUBSYSTEM:WINDOWS

rem Minimify MainEffect
shader_minifier.exe -v --format nasm -o fragment.inz fragment.frag
IF ERRORLEVEL 1 GOTO BUILDFAIL
findstr /V /B "_var_" fragment.inz > fragment.inc
del /F /Q fragment.inz

rem Minimify Postprocess
shader_minifier.exe -v --format nasm -o post.inz post.frag
IF ERRORLEVEL 1 GOTO BUILDFAIL
findstr /V /B "_var_" post.inz > post.inc
del /F /Q post.inz

rem Compile music player
echo Compile clinkster.asm
nasm -w-orphan-labels -fwin32 -o clinkster.obj clinkster.asm
IF ERRORLEVEL 1 GOTO BUILDFAIL

rem Compile main module
echo Compile intro.asm
nasm -w-orphan-labels -fwin32 -o intro.obj intro.asm
IF ERRORLEVEL 1 GOTO BUILDFAIL

rem Link all
if "%PROJECTCONFIG%"=="Fast" call set OPTSNOW=%OPTSFAST%
if "%PROJECTCONFIG%"=="Normal" call set OPTSNOW=%OPTSMEDIUM%
if "%PROJECTCONFIG%"=="Slow" call set OPTSNOW=%OPTSSLOW%

crinkler.exe %OPTSNOW% %LIBS% intro.obj clinkster.obj /OUT:add4k-%PROJECTCONFIG%.exe
IF ERRORLEVEL 1 GOTO BUILDFAIL

echo All done!
GOTO BUILDSUCC
:BUILDFAIL
echo Something is fucked!
:BUILDSUCC
pause