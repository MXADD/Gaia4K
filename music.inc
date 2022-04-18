; Clinkster music converted from Music.xrns
; (c)RTS^Addict

%define USES_SINE 1
%define USES_SAWTOOTH 1
%define USES_SQUARE 1
%define USES_PARABOLA 1
%define USES_TRIANGLE 0
%define USES_NOISE 1
%define USES_VELOCITY 0
%define USES_LONG_NOTES 0
%define USES_DELAY 0
%define USES_PANNING 0
%define USES_INDEXDECAY 1
%define USES_GAIN 1

%define SUBSAMPLES_PER_TICK 18900
%define MAX_INSTRUMENT_SUBSAMPLES 1179648
%define MAX_TOTAL_INSTRUMENT_SAMPLES 1638400
%define MAX_RELEASE_SUBSAMPLES 393216
%define TOTAL_SAMPLES 2883584
%define MAX_TRACK_INSTRUMENT_RENDERS 17

%define MAX_DELAY_LENGTH 0
%define LEFT_DELAY_LENGTH 0
%define RIGHT_DELAY_LENGTH 0
%define DELAY_STRENGTH 0.00000000

%define NUMTRACKS 8
%define LOGNUMTICKS 10
%define MUSIC_LENGTH 592
%define TICKS_PER_SECOND 9.33333333


	SECT_DATA(instdata) align=1

InstrumentData:
	; 00:  Track 01 / 00|VST: Clinkster (Basic)
	db	3,3,6,8,8,9,10,2,14,13,0,0,0,0,0,-6,-22,14,12
	db	38,10,4,0,6,4,0,4,4,2,0,1,2,0,0,4,0,3,2,0,0,2,0,1,2,0,1,6,4,2,0,1,2,0,0,2,0,1,2,0,4,2,0,-1
	; 01:  Track 02 / 01|VST: Clinkster (Basic)
	db	3,3,3,3,5,5,5,5,0,13,0,0,0,0,-18,61,-22,-7,2
	db	22,1,0,1,1,0,1,1,0,7,1,0,1,1,0,1,1,0,1,1,0,2,1,0,-1
	; 02:  Track 03 / 07|plainbassdrum01
	db	0,0,0,0,0,0,1,0,0,13,32,0,-77,0,0,104,-34,-9,0
	db	36,1,0,-1
	; 03:  Track 03 / 02|VST: Clinkster (Basic)
	db	0,0,5,5,80,62,7,9,4,13,60,21,-52,-3,-86,27,-39,-7,-11
	db	16,1,0,11,1,0,11,1,0,-1
	; 04:  Track 03 / 06|thinsnare
	db	0,0,26,26,0,36,12,12,0,25,-60,24,0,0,0,-18,-59,-11,0
	db	50,1,0,-1
	; 05:  Track 03 / 05|snare1
	db	2,4,32,12,0,8,20,0,1,13,11,-14,-92,0,0,120,-81,-25,-5
	db	38,1,0,-1
	; 06:  Track 03 / 00|VST: Clinkster (Basic)
	db	3,3,6,8,8,9,10,2,14,13,0,0,0,0,0,-6,-22,14,12
	db	50,10,0,13,4,0,0,4,0,3,4,0,-1
	; 07:  Track 04 / 04|VST: Clinkster (Basic)
	db	1,2,4,4,56,46,16,14,31,13,0,0,0,0,19,6,-37,2,25
	db	65,2,0,3,8,0,2,8,0,1,19,12,8,4,0,1,24,16,0,0,2,0,1,8,0,1,8,1,0,-1
	db	-1

	SECT_DATA(notepos) align=1

NotePositions:
	; 00:  Track 01 / 00|VST: Clinkster (Basic)
	; position 0 - pattern 0
	db	0,2,2,2,2,2,2,2
	; position 1 - pattern 1
	db	2,2,2,2,2,2,2,2
	; position 2 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 3 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 4 - pattern 0
	db	2,2,2,2,2,2,2,2
	; position 5 - pattern 1
	db	2,2,2,2,2,2,2,2
	; position 6 - pattern 3
	db	2,2,2,2,2,2,2,2
	; position 7 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 8 - pattern 0
	db	2,2,2,2,2,2,2,2
	; position 9 - pattern 1
	db	2,2,2,2,2,2,2,2
	; position 10 - pattern 3
	db	2,2,2,2,2,2,2,2
	; position 11 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 12 - pattern 0
	db	2,2,2,2,2,2,2,2
	; position 13 - pattern 4
	db	2,2,2,2,2,2,2,2
	; position 14 - pattern 5
	db	2,2,2,2,2,2,2,2
	; position 15 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 16 - pattern 0
	db	2,2,2,2,2,2,2,2
	; position 17 - pattern 1
	db	2,2,2,2,2,2,2,2
	; position 18 - pattern 3
	db	2,2,2,2,2,2,2,2
	; position 19 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 20 - pattern 0
	db	2,2,2,2,2,2,2,2
	; position 21 - pattern 4
	db	2,2,2,2,2,2,2,2
	; position 22 - pattern 6
	db	2,2,2,2,2,2,2,2
	; position 23 - pattern 7
	db	2,2,2,2,2,2,2,2
	; position 24 - pattern 0
	db	2,2,2,2,2,2,2,2
	; position 25 - pattern 8
	db	2,4,4,4
	; position 26 - pattern 0
	db	4,2,2,2,2,2,2,2
	; position 27 - pattern 1
	db	2,2,2,2,2,2,2,2
	; position 28 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 29 - pattern 7
	db	2,2,2,2,2,2,2,2
	; position 30 - pattern 0
	db	2,2,2,2,2,2,2,2
	; position 31 - pattern 1
	db	2,2,2,2,2,2,2,2
	; position 32 - pattern 2
	db	2,2,2,2,2,2,2,2
	; position 33 - pattern 7
	db	2,2,2,2,2,2,2,2
	; position 34 - pattern 9
	db	2,2,4,4,4,6

	; 01:  Track 02 / 01|VST: Clinkster (Basic)
	; position 4 - pattern 0
	db	64,1,2,2,1,3,1,2,2,1
	; position 5 - pattern 1
	db	1,1,2,2,1,3,1,2,2,1
	; position 6 - pattern 3
	db	1,1,2,2,1,3,1,2,2,1
	; position 7 - pattern 2
	db	1,1,2,2,1,3,1,2,2,1
	; position 8 - pattern 0
	db	1,1,2,2,1,3,1,2,2,1
	; position 9 - pattern 1
	db	1,1,2,2,1,3,1,2,2,1
	; position 10 - pattern 3
	db	1,1,2,2,1,3,1,2,2,1
	; position 11 - pattern 2
	db	1,1,2,2,1,3,1,2,2,1
	; position 12 - pattern 0
	db	1,1,2,2,1,3,1,2,2,1
	; position 13 - pattern 4
	db	1,1,2,2,1,3,1,2,2,1
	; position 14 - pattern 5
	db	1,1,2,2,1,3,1,2,2,1
	; position 15 - pattern 2
	db	1,1,2,2,1,3,1,2,2,1
	; position 16 - pattern 0
	db	1,1,2,2,1,3,1,2,2,1
	; position 17 - pattern 1
	db	1,1,2,2,1,3,1,2,2,1
	; position 18 - pattern 3
	db	1,1,2,2,1,3,1,2,2,1
	; position 19 - pattern 2
	db	1,1,2,2,1,3,1,2,2,1
	; position 20 - pattern 0
	db	1,1,2,2,1,3,1,2,2,1
	; position 21 - pattern 4
	db	1,1,2,2,1,3,1,2,2,1
	; position 22 - pattern 6
	db	1,1,2,2,1,3,1,2,2,1
	; position 23 - pattern 7
	db	1,1,2,2,1,3,1,2,2,1
	; position 24 - pattern 0
	db	1,1,2,2,1,3,1,2,2,1
	; position 25 - pattern 8
	db	1,1,2,3
	; position 26 - pattern 0
	db	10,1,2,2,1,3,1,2,2,1
	; position 27 - pattern 1
	db	1,1,2,2,1,3,1,2,2,1
	; position 28 - pattern 2
	db	1,1,2,2,1,3,1,2,2,1
	; position 29 - pattern 7
	db	1,1,2,2,1,3,1,2,2,1
	; position 30 - pattern 0
	db	1,1,2,2,1,3,1,2,2,1
	; position 31 - pattern 1
	db	1,1,2,2,1,3,1,2,2,1
	; position 32 - pattern 2
	db	1,1,2,2,1,3,1,2,2,1
	; position 33 - pattern 7
	db	1,1,2,2,1,3,1,2,2,1
	; position 34 - pattern 9
	db	1,1,2,3

	; 02:  Track 03 / 07|plainbassdrum01
	; position 4 - pattern 0
	db	64,5,4
	; position 5 - pattern 1
	db	7,5,4
	; position 6 - pattern 3
	db	7,5,4
	; position 7 - pattern 2
	db	7,5,4,2
	; position 8 - pattern 0
	db	5,5,4
	; position 9 - pattern 1
	db	7,5,4
	; position 10 - pattern 3
	db	7,5,4
	; position 11 - pattern 2
	db	7,5,4,2
	; position 12 - pattern 0
	db	5,5,4
	; position 13 - pattern 4
	db	7,5,4
	; position 14 - pattern 5
	db	7,5,4
	; position 15 - pattern 2
	db	7,5,4,2
	; position 16 - pattern 0
	db	5,5,4
	; position 17 - pattern 1
	db	7,5,4
	; position 18 - pattern 3
	db	7,5,4
	; position 19 - pattern 2
	db	7,5,4,2
	; position 20 - pattern 0
	db	5,5,4
	; position 21 - pattern 4
	db	7,5,4
	; position 22 - pattern 6
	db	7,5,4
	; position 23 - pattern 7
	db	7,5,4,2
	; position 24 - pattern 0
	db	5,5,4
	; position 25 - pattern 8
	db	7
	; position 26 - pattern 0
	db	16,5,4
	; position 27 - pattern 1
	db	7,5,4
	; position 28 - pattern 2
	db	7,5,4,2
	; position 29 - pattern 7
	db	5,5,4,2
	; position 30 - pattern 0
	db	5,5,4
	; position 31 - pattern 1
	db	7,5,4
	; position 32 - pattern 2
	db	7,5,4,2
	; position 33 - pattern 7
	db	5,5,4,2
	; position 34 - pattern 9
	db	5

	; 03:  Track 03 / 02|VST: Clinkster (Basic)
	; position 4 - pattern 0
	db	66,1,8,4
	; position 5 - pattern 1
	db	3,1,8,4
	; position 6 - pattern 3
	db	3,1,8,4
	; position 7 - pattern 2
	db	3,1
	; position 8 - pattern 0
	db	15,1,8,4
	; position 9 - pattern 1
	db	3,1,8,4
	; position 10 - pattern 3
	db	3,1,8,4
	; position 11 - pattern 2
	db	3,1
	; position 12 - pattern 0
	db	15,1,8,4
	; position 13 - pattern 4
	db	3,1,8,4
	; position 14 - pattern 5
	db	3,1,8,4
	; position 15 - pattern 2
	db	3,1
	; position 16 - pattern 0
	db	15,1,8,4
	; position 17 - pattern 1
	db	3,1,8,4
	; position 18 - pattern 3
	db	3,1,8,4
	; position 19 - pattern 2
	db	3,1
	; position 20 - pattern 0
	db	15,1,8,4
	; position 21 - pattern 4
	db	3,1,8,4
	; position 22 - pattern 6
	db	3,1,8,4
	; position 23 - pattern 7
	db	3,1
	; position 24 - pattern 0
	db	15,1,8,4
	; position 26 - pattern 0
	db	19,1,8,4
	; position 27 - pattern 1
	db	3,1,8,4
	; position 28 - pattern 2
	db	3,1
	; position 29 - pattern 7
	db	15,1
	; position 30 - pattern 0
	db	15,1,8,4
	; position 31 - pattern 1
	db	3,1,8,4
	; position 32 - pattern 2
	db	3,1
	; position 33 - pattern 7
	db	15,1

	; 04:  Track 03 / 06|thinsnare
	; position 4 - pattern 0
	db	68,3,1,2,3,1
	; position 5 - pattern 1
	db	6,3,1,2,3,1
	; position 6 - pattern 3
	db	6,3,1,2,3,1
	; position 7 - pattern 2
	db	6,3,1,2,3,1
	; position 8 - pattern 0
	db	6,3,1,2,3,1
	; position 9 - pattern 1
	db	6,3,1,2,3,1
	; position 10 - pattern 3
	db	6,3,1,2,3,1
	; position 11 - pattern 2
	db	6,3,1,2,3,1
	; position 12 - pattern 0
	db	6,3,1,2,3,1
	; position 13 - pattern 4
	db	6,3,1,2,3,1
	; position 14 - pattern 5
	db	6,3,1,2,3,1
	; position 15 - pattern 2
	db	6,3,1,2,3,1
	; position 16 - pattern 0
	db	6,3,1,2,3,1
	; position 17 - pattern 1
	db	6,3,1,2,3,1
	; position 18 - pattern 3
	db	6,3,1,2,3,1
	; position 19 - pattern 2
	db	6,3,1,2,3,1
	; position 20 - pattern 0
	db	6,3,1,2,3,1
	; position 21 - pattern 4
	db	6,3,1,2,3,1
	; position 22 - pattern 6
	db	6,3,1,2,3,1
	; position 23 - pattern 7
	db	6,3,1,2,3,1
	; position 24 - pattern 0
	db	6,3,1,2,3,1
	; position 26 - pattern 0
	db	22,3,1,2,3,1
	; position 27 - pattern 1
	db	6,3,1,2,3,1
	; position 28 - pattern 2
	db	6,3,1,2,3,1
	; position 29 - pattern 7
	db	6,3,1,2,3,1
	; position 30 - pattern 0
	db	6,3,1,2,3,1
	; position 31 - pattern 1
	db	6,3,1,2,3,1
	; position 32 - pattern 2
	db	6,3,1,2,3,1
	; position 33 - pattern 7
	db	6,3,1,2,3,1

	; 05:  Track 03 / 05|snare1
	; position 4 - pattern 0
	db	70,6
	; position 5 - pattern 1
	db	10,6
	; position 6 - pattern 3
	db	10,6
	; position 7 - pattern 2
	db	10,6,3
	; position 8 - pattern 0
	db	7,6
	; position 9 - pattern 1
	db	10,6
	; position 10 - pattern 3
	db	10,6
	; position 11 - pattern 2
	db	10,6,3
	; position 12 - pattern 0
	db	7,6
	; position 13 - pattern 4
	db	10,6
	; position 14 - pattern 5
	db	10,6
	; position 15 - pattern 2
	db	10,6,3
	; position 16 - pattern 0
	db	7,6
	; position 17 - pattern 1
	db	10,6
	; position 18 - pattern 3
	db	10,6
	; position 19 - pattern 2
	db	10,6,3
	; position 20 - pattern 0
	db	7,6
	; position 21 - pattern 4
	db	10,6
	; position 22 - pattern 6
	db	10,6
	; position 23 - pattern 7
	db	10,6,3
	; position 24 - pattern 0
	db	7,6
	; position 25 - pattern 8
	db	18,1
	; position 26 - pattern 0
	db	7,6
	; position 27 - pattern 1
	db	10,6
	; position 28 - pattern 2
	db	10,6,3
	; position 29 - pattern 7
	db	7,6,3
	; position 30 - pattern 0
	db	7,6
	; position 31 - pattern 1
	db	10,6
	; position 32 - pattern 2
	db	10,6,3
	; position 33 - pattern 7
	db	7,6,3

	; 06:  Track 03 / 00|VST: Clinkster (Basic)
	; position 34 - pattern 9
	db	-3,36,4,4,4

	; 07:  Track 04 / 04|VST: Clinkster (Basic)
	; position 8 - pattern 0
	db	-1,128
	; position 9 - pattern 1
	db	16,12
	; position 10 - pattern 3
	db	4
	; position 12 - pattern 0
	db	32
	; position 13 - pattern 4
	db	16,12
	; position 14 - pattern 5
	db	4,8,4
	; position 16 - pattern 0
	db	20
	; position 17 - pattern 1
	db	16,12
	; position 18 - pattern 3
	db	4
	; position 20 - pattern 0
	db	32
	; position 21 - pattern 4
	db	16,12
	; position 22 - pattern 6
	db	4
	; position 23 - pattern 7
	db	16
	; position 24 - pattern 0
	db	16
	; position 26 - pattern 0
	db	32
	; position 27 - pattern 1
	db	16,12
	; position 30 - pattern 0
	db	36
	; position 31 - pattern 1
	db	16,12
	; position 33 - pattern 7
	db	20
	; position 34 - pattern 9
	db	24


	SECT_DATA(notesamp) align=1

NoteSamples:
	; 00:  Track 01 / 00|VST: Clinkster (Basic)
	; position 0 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 1 - pattern 1
	db	4,7,12,14,13,4,7,9
	; position 2 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 3 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 4 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 5 - pattern 1
	db	4,7,12,14,13,4,7,9
	; position 6 - pattern 3
	db	4,7,12,14,13,4,7,9
	; position 7 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 8 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 9 - pattern 1
	db	4,7,12,14,13,4,7,9
	; position 10 - pattern 3
	db	4,7,12,14,13,4,7,9
	; position 11 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 12 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 13 - pattern 4
	db	4,7,12,14,13,4,7,9
	; position 14 - pattern 5
	db	4,8,12,14,13,4,8,12
	; position 15 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 16 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 17 - pattern 1
	db	4,7,12,14,13,4,7,9
	; position 18 - pattern 3
	db	4,7,12,14,13,4,7,9
	; position 19 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 20 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 21 - pattern 4
	db	4,7,12,14,13,4,7,9
	; position 22 - pattern 6
	db	4,8,12,14,13,4,8,12
	; position 23 - pattern 7
	db	5,9,13,15,16,5,9,13
	; position 24 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 25 - pattern 8
	db	3,1,6,2
	; position 26 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 27 - pattern 1
	db	4,7,12,14,13,4,7,9
	; position 28 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 29 - pattern 7
	db	5,9,13,15,16,5,9,13
	; position 30 - pattern 0
	db	4,7,12,14,13,4,7,9
	; position 31 - pattern 1
	db	4,7,12,14,13,4,7,9
	; position 32 - pattern 2
	db	4,8,12,14,13,4,8,12
	; position 33 - pattern 7
	db	5,9,13,15,16,5,9,13
	; position 34 - pattern 9
	db	4,11,6,3,10,0
	db	-1

	; 01:  Track 02 / 01|VST: Clinkster (Basic)
	; position 4 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 5 - pattern 1
	db	2,2,2,2,5,5,2,2,4,5
	; position 6 - pattern 3
	db	2,2,2,2,5,5,2,2,4,5
	; position 7 - pattern 2
	db	3,0,3,5,3,3,5,3,0,3
	; position 8 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 9 - pattern 1
	db	2,2,2,2,5,5,2,2,4,5
	; position 10 - pattern 3
	db	2,2,2,2,5,5,2,2,4,5
	; position 11 - pattern 2
	db	3,0,3,5,3,3,5,3,0,3
	; position 12 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 13 - pattern 4
	db	2,2,2,2,5,5,2,2,4,5
	; position 14 - pattern 5
	db	3,0,3,5,3,3,5,3,0,3
	; position 15 - pattern 2
	db	3,0,3,5,3,3,5,3,0,3
	; position 16 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 17 - pattern 1
	db	2,2,2,2,5,5,2,2,4,5
	; position 18 - pattern 3
	db	2,2,2,2,5,5,2,2,4,5
	; position 19 - pattern 2
	db	3,0,3,5,3,3,5,3,0,3
	; position 20 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 21 - pattern 4
	db	2,2,2,2,5,5,2,2,4,5
	; position 22 - pattern 6
	db	3,0,3,5,3,3,5,3,0,3
	; position 23 - pattern 7
	db	4,1,4,6,4,4,7,4,1,4
	; position 24 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 25 - pattern 8
	db	2,2,2,2
	; position 26 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 27 - pattern 1
	db	2,2,2,2,5,5,2,2,4,5
	; position 28 - pattern 2
	db	3,0,3,5,3,3,5,3,0,3
	; position 29 - pattern 7
	db	4,1,4,6,4,4,7,4,1,4
	; position 30 - pattern 0
	db	2,2,2,2,5,5,2,2,4,5
	; position 31 - pattern 1
	db	2,2,2,2,5,5,2,2,4,5
	; position 32 - pattern 2
	db	3,0,3,5,3,3,5,3,0,3
	; position 33 - pattern 7
	db	4,1,4,6,4,4,7,4,1,4
	; position 34 - pattern 9
	db	2,2,2,2
	db	-1

	; 02:  Track 03 / 07|plainbassdrum01
	; position 4 - pattern 0
	db	0,0,0
	; position 5 - pattern 1
	db	0,0,0
	; position 6 - pattern 3
	db	0,0,0
	; position 7 - pattern 2
	db	0,0,0,0
	; position 8 - pattern 0
	db	0,0,0
	; position 9 - pattern 1
	db	0,0,0
	; position 10 - pattern 3
	db	0,0,0
	; position 11 - pattern 2
	db	0,0,0,0
	; position 12 - pattern 0
	db	0,0,0
	; position 13 - pattern 4
	db	0,0,0
	; position 14 - pattern 5
	db	0,0,0
	; position 15 - pattern 2
	db	0,0,0,0
	; position 16 - pattern 0
	db	0,0,0
	; position 17 - pattern 1
	db	0,0,0
	; position 18 - pattern 3
	db	0,0,0
	; position 19 - pattern 2
	db	0,0,0,0
	; position 20 - pattern 0
	db	0,0,0
	; position 21 - pattern 4
	db	0,0,0
	; position 22 - pattern 6
	db	0,0,0
	; position 23 - pattern 7
	db	0,0,0,0
	; position 24 - pattern 0
	db	0,0,0
	; position 25 - pattern 8
	db	0
	; position 26 - pattern 0
	db	0,0,0
	; position 27 - pattern 1
	db	0,0,0
	; position 28 - pattern 2
	db	0,0,0,0
	; position 29 - pattern 7
	db	0,0,0,0
	; position 30 - pattern 0
	db	0,0,0
	; position 31 - pattern 1
	db	0,0,0
	; position 32 - pattern 2
	db	0,0,0,0
	; position 33 - pattern 7
	db	0,0,0,0
	; position 34 - pattern 9
	db	0
	db	-1

	; 03:  Track 03 / 02|VST: Clinkster (Basic)
	; position 4 - pattern 0
	db	1,2,1,0
	; position 5 - pattern 1
	db	1,2,1,0
	; position 6 - pattern 3
	db	1,2,1,0
	; position 7 - pattern 2
	db	1,2
	; position 8 - pattern 0
	db	1,2,1,0
	; position 9 - pattern 1
	db	1,2,1,0
	; position 10 - pattern 3
	db	1,2,1,0
	; position 11 - pattern 2
	db	1,2
	; position 12 - pattern 0
	db	1,2,1,0
	; position 13 - pattern 4
	db	1,2,1,0
	; position 14 - pattern 5
	db	1,2,1,0
	; position 15 - pattern 2
	db	1,2
	; position 16 - pattern 0
	db	1,2,1,0
	; position 17 - pattern 1
	db	1,2,1,0
	; position 18 - pattern 3
	db	1,2,1,0
	; position 19 - pattern 2
	db	1,2
	; position 20 - pattern 0
	db	1,2,1,0
	; position 21 - pattern 4
	db	1,2,1,0
	; position 22 - pattern 6
	db	1,2,1,0
	; position 23 - pattern 7
	db	1,2
	; position 24 - pattern 0
	db	1,2,1,0
	; position 26 - pattern 0
	db	1,2,1,0
	; position 27 - pattern 1
	db	1,2,1,0
	; position 28 - pattern 2
	db	1,2
	; position 29 - pattern 7
	db	1,2
	; position 30 - pattern 0
	db	1,2,1,0
	; position 31 - pattern 1
	db	1,2,1,0
	; position 32 - pattern 2
	db	1,2
	; position 33 - pattern 7
	db	1,2
	db	-1

	; 04:  Track 03 / 06|thinsnare
	; position 4 - pattern 0
	db	0,0,0,0,0,0
	; position 5 - pattern 1
	db	0,0,0,0,0,0
	; position 6 - pattern 3
	db	0,0,0,0,0,0
	; position 7 - pattern 2
	db	0,0,0,0,0,0
	; position 8 - pattern 0
	db	0,0,0,0,0,0
	; position 9 - pattern 1
	db	0,0,0,0,0,0
	; position 10 - pattern 3
	db	0,0,0,0,0,0
	; position 11 - pattern 2
	db	0,0,0,0,0,0
	; position 12 - pattern 0
	db	0,0,0,0,0,0
	; position 13 - pattern 4
	db	0,0,0,0,0,0
	; position 14 - pattern 5
	db	0,0,0,0,0,0
	; position 15 - pattern 2
	db	0,0,0,0,0,0
	; position 16 - pattern 0
	db	0,0,0,0,0,0
	; position 17 - pattern 1
	db	0,0,0,0,0,0
	; position 18 - pattern 3
	db	0,0,0,0,0,0
	; position 19 - pattern 2
	db	0,0,0,0,0,0
	; position 20 - pattern 0
	db	0,0,0,0,0,0
	; position 21 - pattern 4
	db	0,0,0,0,0,0
	; position 22 - pattern 6
	db	0,0,0,0,0,0
	; position 23 - pattern 7
	db	0,0,0,0,0,0
	; position 24 - pattern 0
	db	0,0,0,0,0,0
	; position 26 - pattern 0
	db	0,0,0,0,0,0
	; position 27 - pattern 1
	db	0,0,0,0,0,0
	; position 28 - pattern 2
	db	0,0,0,0,0,0
	; position 29 - pattern 7
	db	0,0,0,0,0,0
	; position 30 - pattern 0
	db	0,0,0,0,0,0
	; position 31 - pattern 1
	db	0,0,0,0,0,0
	; position 32 - pattern 2
	db	0,0,0,0,0,0
	; position 33 - pattern 7
	db	0,0,0,0,0,0
	db	-1

	; 05:  Track 03 / 05|snare1
	; position 4 - pattern 0
	db	0,0
	; position 5 - pattern 1
	db	0,0
	; position 6 - pattern 3
	db	0,0
	; position 7 - pattern 2
	db	0,0,0
	; position 8 - pattern 0
	db	0,0
	; position 9 - pattern 1
	db	0,0
	; position 10 - pattern 3
	db	0,0
	; position 11 - pattern 2
	db	0,0,0
	; position 12 - pattern 0
	db	0,0
	; position 13 - pattern 4
	db	0,0
	; position 14 - pattern 5
	db	0,0
	; position 15 - pattern 2
	db	0,0,0
	; position 16 - pattern 0
	db	0,0
	; position 17 - pattern 1
	db	0,0
	; position 18 - pattern 3
	db	0,0
	; position 19 - pattern 2
	db	0,0,0
	; position 20 - pattern 0
	db	0,0
	; position 21 - pattern 4
	db	0,0
	; position 22 - pattern 6
	db	0,0
	; position 23 - pattern 7
	db	0,0,0
	; position 24 - pattern 0
	db	0,0
	; position 25 - pattern 8
	db	0,0
	; position 26 - pattern 0
	db	0,0
	; position 27 - pattern 1
	db	0,0
	; position 28 - pattern 2
	db	0,0,0
	; position 29 - pattern 7
	db	0,0,0
	; position 30 - pattern 0
	db	0,0
	; position 31 - pattern 1
	db	0,0
	; position 32 - pattern 2
	db	0,0,0
	; position 33 - pattern 7
	db	0,0,0
	db	-1

	; 06:  Track 03 / 00|VST: Clinkster (Basic)
	; position 34 - pattern 9
	db	3,2,1,0
	db	-1

	; 07:  Track 04 / 04|VST: Clinkster (Basic)
	; position 8 - pattern 0
	db	5
	; position 9 - pattern 1
	db	2,0
	; position 10 - pattern 3
	db	1
	; position 12 - pattern 0
	db	5
	; position 13 - pattern 4
	db	11,9
	; position 14 - pattern 5
	db	10,12,3
	; position 16 - pattern 0
	db	5
	; position 17 - pattern 1
	db	2,0
	; position 18 - pattern 3
	db	1
	; position 20 - pattern 0
	db	5
	; position 21 - pattern 4
	db	11,9
	; position 22 - pattern 6
	db	4
	; position 23 - pattern 7
	db	8
	; position 24 - pattern 0
	db	5
	; position 26 - pattern 0
	db	5
	; position 27 - pattern 1
	db	2,0
	; position 30 - pattern 0
	db	5
	; position 31 - pattern 1
	db	2,0
	; position 33 - pattern 7
	db	7
	; position 34 - pattern 9
	db	6
	db	-1

