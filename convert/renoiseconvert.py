#!/usr/bin/env python2

import sys
import zipfile
import clinksterxml
import struct
import ctypes
import math
import datetime

class InputException(Exception):
	def __init__(self, message):
		Exception.__init__(self)
		self.message = message


class Volume:
	def __init__(self, left, right):
		self.left = left
		self.right = right

	def __mul__(self, other):
		return Volume(self.left * other.left, self.right * other.right)

	def isPanned(self):
		return self.left != self.right

def makeVolume(xvolume):
	v = float(xvolume)
	return Volume(v,v)

def makePanning(xpanning):
	p = float(xpanning)
	return Volume(math.sqrt(2.0 * (1.0 - p)), math.sqrt(2.0 * p))



class Instrument:
	NAMES = ["bwave","mwave","bdetune","mdetune",
			 "bpitchs","bpitchd","mpitchs","mpitchd",
			 "index","indexspr","indexd","layers","randomseed",
			 "attack","decay","sustain","release","gain"]
	QUAN = 	[(6,0),(6,0),(101,0),(101,0),
			 (241,120),(201,128),(241,120),(201,128),
			 (101,0),(101,0),(201,128),(51,0),(128,0),
			 (201,128),(201,128),(65,32),(201,128),(201,80)]

	def __init__(self, number, name, params):
		names,quan = Instrument.NAMES,Instrument.QUAN
		self.number = number
		self.name = name
		self.params = params
		self.param_data = [int(p * (quan[i][0]-1) + 0.5) - quan[i][1] for i,p in enumerate(params)]
		for i,p in enumerate(self.param_data):
			self.__dict__[names[i]] = p
		for mp,m in [("layers", 1), ("attack", -120), ("decay", -120), ("release", -120)]:
			if self.__dict__[mp] < m:
				self.__dict__[mp] = m
				print " * Instrument '%s': %s clamped to %d" % (name, mp, m)
		self.chopped = self.sustain == 0
		self.volume = Volume(1.0, 1.0)

class Note:
	NOTEBASES = {
		"C": 0, "D": 2, "E": 4, "F": 5, "G": 7, "A": 9, "B": 11
	}

	NOTENAMES = {
		0: "C-", 1: "C#", 2: "D-", 3: "D#", 4: "E-", 5: "F-",
		6: "F#", 7: "G-", 8: "G#", 9: "A-", 10: "A#", 11: "B-"
	}

	def __init__(self, line, songpos, pat, note, instr, velocity):
		note = str(note)
		self.line = int(line)
		self.songpos = int(songpos)
		self.pat = int(pat)
		self.velocity = 127 if str(velocity) == "" or str(velocity) == ".." else int(str(velocity), 16)
		if note == "OFF":
			self.off = True
			self.tone = None
			self.instr = 0
		else:
			self.off = False
			octave = int(note[2])
			notebase = Note.NOTEBASES[note[0]]
			sharp = int(note[1] == "#")
			self.tone = octave * 12 + notebase + sharp
			self.instr = int(str(instr), 16)


def instplugins(xinst):
	xplugins = xinst.PluginProperties
	if xplugins:
		return xplugins
	return xinst.PluginGenerator

def isactive(xdevice):
	if not xdevice:
		return False
	if xdevice.IsActive.Value:
		return float(xdevice.IsActive.Value) != 0.0
	else:
		return str(xdevice.IsActive) == "true"

def notename(tone):
	return Note.NOTENAMES[tone%12] + str(tone/12)

def multibyte(v):
	return [-1 - (v >> 8), v & 255] if v > 127 else [v]


class Track:
	def __init__(self, number, name, notes, volume, instr, instruments):
		self.number = number
		self.name = name
		self.notes = notes
		self.volume = volume * instruments[instr].volume
		self.instr = instr
		self.notemap = dict()
		self.tal_repr = dict()

		prev = None
		for note in notes:
			if prev is not None and not prev.off and prev.instr == instr:
				length = 1 if instruments[self.instr].chopped else note.line - prev.line
				if length < 0:
					raise InputException("Track '%s' has reversed note order from %d to %d" % (name, prev.line, note.line))
				if prev.tone is None:
					raise InputException("Track '%s' has a toneless note at %d" % (name, prev.line))
				tal = (prev.tone, length, prev.velocity)
				self.notemap[prev] = tal

			prev = note

		if not prev.off and prev.instr == instr:
			if instr.chopped:
				tal = (prev.tone, 1)
				self.notemap[prev] = tal
			elif not prev.off:
				 raise InputException("Track '%s' is not terminated." % name)

		self.tals = sorted(set(self.notemap.values()), key = (lambda (t,l,v) : (t,v,-l)))
		for i,tal in enumerate(self.tals):
			if tal[0] is None:
				raise InputException("Track '%s' has a toneless note" % name)
			self.tal_repr[tal] = i

		self.longest_sample = None
		self.sample_length_sum = None


class Music:
	def __init__(self, version, tracks, instruments, length, ticklength, n_delay_tracks, delay_lengths, delay_strength, master_volume):
		self.version = version
		self.tracks = tracks
		self.instruments = instruments
		self.length = length
		self.ticklength = ticklength
		self.n_delay_tracks = n_delay_tracks
		self.delay_lengths = delay_lengths
		self.delay_strength = delay_strength
		self.master_volume = master_volume

		self.uses_waveform = [False] * 6
		for t in self.tracks:
			inst = self.instruments[t.instr]
			self.uses_waveform[inst.bwave] = True
			self.uses_waveform[inst.mwave] = True
		self.uses_velocity = any(any(tal[2] != 127 for tal in t.tals) for t in self.tracks)
		self.uses_long_notes = any(any(tal[1] > 127 for tal in t.tals) for t in self.tracks)
		self.uses_delay = (self.n_delay_tracks > 0)
		self.uses_panning = any(t.volume.isPanned() for t in self.tracks)
		self.uses_indexdecay = any(self.instruments[t.instr].indexd != 0 for t in self.tracks)
		self.uses_gain = any(self.instruments[t.instr].gain != 0 for t in self.tracks)

		# Calculate longest sample
		self.max_longest_sample = 0.0
		self.max_sample_length_sum = 0.0
		self.max_release_tail = 0.0
		for ti,track in enumerate(self.tracks):
			track.longest_sample = 0.0
			track.sample_length_sum = 0.0
			track.max_release_tail = 0.0

			def envelope(v):
				return pow(2.0, v * 0.125) * 32767.0 / (44100.0 * 4)
			instr = instruments[track.instr]
			attack_length = envelope(instr.attack)
			decay_length = envelope(instr.decay)
			release_length = envelope(instr.release)

			for tal in track.tals:
				note_length = tal[1] * ticklength
				sustain_length = max(note_length, attack_length + decay_length)
				sample_length = sustain_length + release_length + 32767.0 / (44100.0 * 4) + 0.01
				release_tail = sample_length - note_length
				track.longest_sample = max(track.longest_sample, sample_length)
				track.sample_length_sum += sample_length
				track.max_release_tail = max(track.max_release_tail, release_tail)
			self.max_longest_sample = max(self.max_longest_sample, track.longest_sample)
			self.max_sample_length_sum = max(self.max_sample_length_sum, track.sample_length_sum)
			self.max_release_tail = max(self.max_release_tail, track.max_release_tail)

			# Track title, specifying resulting track index, track name and instrument number / name
			instr = self.instruments[track.instr]
			track.title = "%02d:  %s / %02X|%s" % (ti, track.name, instr.number, instr.name)

		self.datainit = None
		self.out = None

	def dataline(self, data):
		if len(data) > 0:
			line = self.datainit
			first = True
			for d in data:
				if not first:
					line += ","
				line += str(d)
				first = False
			line += "\n"
			self.out += line

	def instrparams(self, inst, fields):
		return [inst.__dict__[f] if f in inst.__dict__ else f for f in fields]

	def comment(self, c):
		self.out += "\t; %s\n" % c

	def notelist(self, datafunc, trackterm):
		for t in self.tracks:
			self.comment(t.title)
			prev_n = None
			pat_data = []
			for n in [note for note in t.notes if not note.off and note.instr == t.instr]:
				if prev_n is None or n.songpos != prev_n.songpos:
					self.dataline(pat_data)
					pat_data = []
					self.comment("position %d - pattern %d" % (n.songpos, n.pat))
				pat_data += datafunc(t,prev_n,n)
				prev_n = n
			self.dataline(pat_data)
			self.dataline(trackterm)
			self.out += "\n"

	def notebitmask(self):
		for t in self.tracks:
			self.comment(t.name)
			prev_n = None
			pat_data = []
			pos = 0
			data_byte = 0
			dummy_note = Note(self.length, 0, 0, "C-0", 0, 127)
			for n in [note for note in t.notes if not note.off and note.instr == t.instr] + [dummy_note]:
				while pos <= n.line:
					data_byte = (data_byte << 1) + (1 if pos == n.line else 0)
					pos += 1
					if (pos & 7) == 0:
						if prev_n is None or n.songpos != prev_n.songpos:
							self.dataline(pat_data)
							pat_data = []
							self.comment("position %d - pattern %d" % (n.songpos, n.pat))
							prev_n = n
						pat_data.append(data_byte)
						data_byte = 0
				prev_n = n
			self.dataline(pat_data)
			self.out += "\n"

	def posdata(self, t, pn, n):
		step = n.line-pn.line if pn is not None else n.line
		return multibyte(step)

	def samdata(self, t, pn, n):
		return [t.tal_repr[t.notemap[n]]]

	def exportPC(self, sample_rate):
		self.datainit = "\tdb\t"
		self.out = ""

		sspt = int(self.ticklength * sample_rate)*4

		def roundup(v):
			return (int(v) & -0x10000) + 0x10000

		feature_flags = self.uses_waveform + [self.uses_velocity, self.uses_long_notes, self.uses_delay, self.uses_panning, self.uses_indexdecay, self.uses_gain]
		feature_names = ["SINE", "SAWTOOTH", "SQUARE", "PARABOLA", "TRIANGLE", "NOISE",
						 "VELOCITY", "LONG_NOTES", "DELAY", "PANNING", "INDEXDECAY", "GAIN"]
		print "Features used: " + " ".join(n for f,n in zip(feature_flags, feature_names) if f)
		print

		global infile
		self.out += "; Clinkster music converted from %s %s\n" % (infile, str(datetime.datetime.now())[:-7])
		self.out += "\n"
		for f,fname in zip(feature_flags, feature_names):
			self.out += "%%define USES_%s %d\n" % (fname, int(f))
		self.out += "\n"
		self.out += "%%define SUBSAMPLES_PER_TICK %d\n" % sspt
		self.out += "%%define MAX_INSTRUMENT_SUBSAMPLES %d\n" % roundup((self.max_longest_sample + self.max_release_tail) * (sample_rate * 4.0))
		self.out += "%%define MAX_TOTAL_INSTRUMENT_SAMPLES %d\n" % roundup(self.max_sample_length_sum * sample_rate)
		self.out += "%%define MAX_RELEASE_SUBSAMPLES %d\n" % roundup(self.max_release_tail * (sample_rate * 4.0))
		self.out += "%%define TOTAL_SAMPLES %d\n" % roundup((self.length * self.ticklength + self.max_release_tail) * sample_rate)
		self.out += "%%define MAX_TRACK_INSTRUMENT_RENDERS %d\n" % max(len(t.tals) for t in self.tracks)
		self.out += "\n"
		self.out += "%%define MAX_DELAY_LENGTH %d\n" % int(max(self.delay_lengths) * sample_rate)
		self.out += "%%define LEFT_DELAY_LENGTH %d\n" % int(self.delay_lengths[0] * sample_rate)
		self.out += "%%define RIGHT_DELAY_LENGTH %d\n" % int(self.delay_lengths[1] * sample_rate)
		self.out += "%%define DELAY_STRENGTH %0.8f\n" % self.delay_strength
		self.out += "\n"
		self.out += "%%define NUMTRACKS %d\n" % len(self.tracks)
		self.out += "%%define LOGNUMTICKS %d\n" % int(math.log(self.length, 2) + 1)
		self.out += "%%define MUSIC_LENGTH %d\n" % self.length
		self.out += "%%define TICKS_PER_SECOND %0.8f\n" % (1.0 / self.ticklength)

		# Remap used instruments
		wmap = []
		j = 0
		for i in range(6):
			wmap.append(j)
			if self.uses_waveform[i]:
				j += 1
		for inst in self.instruments:
			if inst is not None:
				for wave in ["bwave", "mwave"]:
					inst.__dict__[wave] = wmap[inst.__dict__[wave]]

		# Instrument data
		self.out += "\n\n\tSECT_DATA(instdata) align=1\n"
		self.out += "\nInstrumentData:\n"
		for ti,track in enumerate(self.tracks):
			track_volume = track.volume * self.master_volume * makeVolume(32.0)
			if self.n_delay_tracks > 0 and ti == self.n_delay_tracks:
				self.dataline([-1])
			self.comment(track.title)
			params = self.instrparams(
				self.instruments[track.instr],
				 ["bwave","mwave","bdetune","mdetune",
				  "indexspr","index","layers","randomseed",
				  "sustain"] +
				([int(track_volume.left), int(track_volume.right)]
				 if self.uses_panning else
				 [int(track_volume.left)]) +
				 ["bpitchs","mpitchs","bpitchd","mpitchd"] +
				(["indexd"] if self.uses_indexdecay else []) +
				(["gain"] if self.uses_gain else []) +
				 ["attack","decay","release"])
			self.dataline(params)

			# List tones and lengths
			taldata = []
			prev_t = -1
			first = True
			for t,l,v in track.tals:
				if t > prev_t:
					if not first:
						taldata += [0]
					taldata += [t-prev_t-1]
					prev_t = t
				if self.uses_velocity:
					taldata += [v]
				taldata += multibyte(l)
				prev_v = v
				first = False
			taldata += [0,-1]
			self.dataline(taldata)
		if self.uses_delay:
			self.dataline([-1,-1])
		else:
			self.dataline([-1])

		# Positions of notes
		self.out += "\n\tSECT_DATA(notepos) align=1\n"
		self.out += "\nNotePositions:\n"
		self.notelist(self.posdata, [])

		# Samples for notes
		self.out += "\n\tSECT_DATA(notesamp) align=1\n"
		self.out += "\nNoteSamples:\n"
		self.notelist(self.samdata, [-1])

		return self.out

	def makeDeltas(self, init_delta, lines_per_beat):
		beats_per_line = 1.0/lines_per_beat
		deltas = []
		for t in self.tracks:
			tdeltas = []
			delta = init_delta
			note_i = 0
			for p in range(0, self.length):
				while t.notes[note_i].line <= p:
					if not t.notes[note_i].off:
						delta = p * beats_per_line
					note_i += 1
				tdeltas.append(delta)
			deltas.append(tdeltas)
		return deltas


def extractTrackNotes(xsong, tr, col):
	outside_pattern = 0
	xsequence = xsong.PatternSequence.PatternSequence
	if not xsequence:
		xsequence = xsong.PatternSequence.SequenceEntries.SequenceEntry
	xpatterns = xsong.PatternPool.Patterns.Pattern
	tname = str(xsong.Tracks.SequencerTrack[tr].Name)

	notes = []

	pattern_top = 0
	prev_instr = None
	for posn,xseq in enumerate(xsequence):
		patn = int(xseq.Pattern)
		xpat = xpatterns[patn]
		nlines = int(xpat.NumberOfLines)
		if tr in [int(xmt) for xmt in xseq.MutedTracks.MutedTrack]:
			off = Note(pattern_top, posn, patn, "OFF", None, 127)
			notes.append(off)
		else:
			xtrack = xpat.Tracks.PatternTrack[tr]
			for xline in xtrack.Lines.Line:
				index = int(xline("index"))
				if index < nlines:
					line = pattern_top + index
					xcol = xline.NoteColumns.NoteColumn[col]
					if xcol.Note and str(xcol.Note) != "---":
						instr = str(xcol.Instrument)
						if instr == "..":
							if prev_instr is None and str(xcol.Note) != "OFF":
								raise InputException("Track '%s' pattern %d position %d: Unspecified instrument" % (tname, patn, index))
							instr = prev_instr
						prev_instr = instr

						note = Note(line, posn, patn, xcol.Note, instr, xcol.Volume)
						notes.append(note)

						if note.velocity == 0 or note.velocity > 127:
							raise InputException("Track '%s' pattern %d position %d: Illegal velocity value" % (tname, patn, index))

					# Check for illegal uses of panning, delay and effect columns
					def checkColumn(x, msg):
						if x and not str(x) in ["", "..", "00"]:
							raise InputException("Track '%s' pattern %d position %d: %s" % (tname, patn, index, msg))
					checkColumn(xcol.Delay, "Delay column used")
					for xeff in xline.EffectColumns.EffectColumn.Number:
						checkColumn(xeff, "Effect column used")
				else:
					outside_pattern += 1
		pattern_top += nlines
	notes.append(Note(pattern_top, len(xsequence), len(xpatterns), "OFF", 0, 127))

	if outside_pattern > 0:
		print " * Track '%s': %d note%s outside patterns ignored" % (tname, outside_pattern, "s" * (outside_pattern > 1))

	return notes

def pickupDelay(xdevices, delay_lengths, delay_strength, tname, ticklength):
	if isactive(xdevices.DelayDevice):
		send = float(xdevices.DelayDevice.TrackSend.Value) / 127.0
		lfeedback = float(xdevices.DelayDevice.LFeedback.Value)
		rfeedback = float(xdevices.DelayDevice.RFeedback.Value)
		if float(xdevices.DelayDevice.LineSync.Value):
			lsynctime = float(xdevices.DelayDevice.LSyncTime.Value)
			lsyncoffset = float(xdevices.DelayDevice.LSyncOffset.Value)
			ldelay = (lsynctime + lsyncoffset) * ticklength
			rsynctime = float(xdevices.DelayDevice.RSyncTime.Value)
			rsyncoffset = float(xdevices.DelayDevice.RSyncOffset.Value)
			rdelay = (rsynctime + rsyncoffset) * ticklength
		else:
			ldelay = float(xdevices.DelayDevice.LDelay.Value) / 1000.0
			rdelay = float(xdevices.DelayDevice.RDelay.Value) / 1000.0
		if abs(lfeedback - send) > 0.05:
			print " * Track '%s': Left feedback (%0.2f) is different from send value (%0.2f)" % (tname, lfeedback, send)
		if abs(rfeedback - send) > 0.05:
			print " * Track '%s': Right feedback (%0.2f) is different from send value (%0.2f)" % (tname, rfeedback, send)
		if delay_lengths != [0.0, 0.0] and ([ldelay,rdelay] != delay_lengths or send != delay_strength):
			print " * Track '%s' has different delay parameters from earlier track" % tname
		return [ldelay,rdelay],send
	return delay_lengths,delay_strength

def makeTracks(version, xsong, ticklength):
	instruments = []
	delay_tracks = []
	non_delay_tracks = []
	delay_lengths = [0.0, 0.0]
	delay_strength = 0.0

	for ii,xinst in enumerate(xsong.Instruments.Instrument):
		params = [float(v) for v in instplugins(xinst).PluginDevice.Parameters.Parameter.Value]
		if params:
			instrument = Instrument(ii, str(xinst.Name), params)
			instrument.volume = makeVolume(instplugins(xinst).Volume)
			instruments.append(instrument)

		else:
			instruments.append(None)

	for tr,xtrack in enumerate(xsong.Tracks.SequencerTrack):
		tname = str(xtrack.Name)
		ncols = int(xtrack.NumberOfVisibleNoteColumns)
		xdevices = xtrack.FilterDevices.Devices
		xdevice = xdevices.SequencerTrackDevice
		if not xdevice:
			xdevice = xdevices.TrackMixerDevice
		volume = makeVolume(xdevice.Volume.Value)
		volume *= makePanning(xdevice.Panning.Value)
		while isactive(xdevices.SendDevice):
			if isactive(xdevices.DelayDevice):
				raise InputException("Track '%s' uses both delay and send" % tname);
			if str(xdevices.SendDevice.MuteSource) != "true":
				raise InputException("Track '%s' uses send without Mute Source" % tname);
			volume *= makeVolume(xdevices.SendDevice.SendAmount.Value)
			volume *= makePanning(xdevices.SendDevice.SendPan.Value)
			dest = int(float(xdevices.SendDevice.DestSendTrack.Value))
			xdevices = xsong.Tracks.SequencerSendTrack[dest].FilterDevices.Devices
			xdevice = xdevices.SequencerSendTrackDevice
			if not xdevice:
				xdevice = xdevices.SendTrackMixerDevice
			volume *= makeVolume(xdevice.Volume.Value)
			volume *= makePanning(xdevice.Panning.Value)
		volume *= makeVolume(xdevice.PostVolume.Value)
		volume *= makePanning(xdevice.PostPanning.Value)

		for col in range(0,ncols):
			notes = extractTrackNotes(xsong, tr, col)

			track_instrs = []
			for note in notes:
				if not note.off:
					instr = instruments[note.instr]
					if instr is None:
						raise InputException("Track '%s' uses undefined instrument (%d)" % (tname, note.instr));
					if note.instr not in track_instrs:
						track_instrs.append(note.instr)

			for instr in track_instrs:
				track = Track(tr, tname, notes, volume, instr, instruments)
				if isactive(xdevices.DelayDevice):
					delay_tracks.append(track)
				else:
					non_delay_tracks.append(track)

		delay_lengths,delay_strength = pickupDelay(xdevices, delay_lengths, delay_strength, tname, ticklength)

	for xtrack in xsong.Tracks.SequencerSendTrack:
		xdevices = xtrack.FilterDevices.Devices
		if xdevices.DelayDevice:
			delay_lengths,delay_strength = pickupDelay(xdevices, delay_lengths, delay_strength, tname, ticklength)

	#delay_tracks = sorted(delay_tracks, key = (lambda t : t.instr))
	#non_delay_tracks = sorted(non_delay_tracks, key = (lambda t : t.instr))

	return (delay_tracks + non_delay_tracks), len(delay_tracks), delay_lengths, delay_strength, instruments

def makeMusic(xsong):
	vstnames = set(str(v) for v in instplugins(xsong.Instruments.Instrument).PluginDevice.PluginIdentifier)
	if len(vstnames) > 1:
		raise InputException("More than one VST used: %s" % list(vstnames))
	vstname = list(vstnames)[0]
	vstmap = { "Clinkster" : 1 }
	if vstname not in vstmap:
		raise InputException("Unknown VST used: %s" % vstname)
	vstversion = vstmap[vstname]
	print "VST version: %d" % vstversion
	if vstversion != 1:
		raise InputException("Only Clinkster version 1 supported")

	xgsd = xsong.GlobalSongData
	if xgsd.PlaybackEngineVersion and int(xgsd.PlaybackEngineVersion) >= 4:
		lines_per_minute = float(xgsd.BeatsPerMin) * float(xgsd.LinesPerBeat)
		print "New timing format: %d ticks per minute" % lines_per_minute
	else:
		lines_per_minute = float(xgsd.BeatsPerMin) * 24.0 / float(xgsd.TicksPerLine)
		print "Old timing format: %d ticks per minute" % lines_per_minute
	ticklength = 60.0 / lines_per_minute
	print

	tracks,n_delay_tracks,delay_lengths,delay_strength,instruments = makeTracks(vstversion, xsong, ticklength)

	xpositions = xsong.PatternSequence.PatternSequence.Pattern
	if not xpositions:
		xpositions = xsong.PatternSequence.SequenceEntries.SequenceEntry.Pattern
	xpatterns = xsong.PatternPool.Patterns.Pattern
	length = 0
	for xpos in xpositions:
		patn = int(xpos)
		xpat = xpatterns[patn]
		nlines = int(xpat.NumberOfLines)
		length += nlines

	xmstdev = xsong.Tracks.SequencerMasterTrack.FilterDevices.Devices.SequencerMasterTrackDevice
	if not xmstdev:
		xmstdev = xsong.Tracks.SequencerMasterTrack.FilterDevices.Devices.MasterTrackMixerDevice
	master_volume = makeVolume(xmstdev.Volume.Value)
	master_volume *= makePanning(xmstdev.Panning.Value)
	master_volume *= makeVolume(xmstdev.PostVolume.Value)
	master_volume *= makePanning(xmstdev.PostPanning.Value)

	return Music(vstversion, tracks, instruments, length, ticklength, n_delay_tracks, delay_lengths, delay_strength, master_volume)


def printMusicStats(music):
	print "Music length: %d ticks at %0.2f ticks per minute" % (music.length, 60.0 / music.ticklength)
	print
	for ti,track in enumerate(music.tracks):
		tnotes = ""
		for t,l,v in track.tals:
			num_notes = 0
			for n in [note for note in track.notes if not note.off and note.instr == track.instr]:
				if track.notemap[n] == (t,l,v):
					num_notes += 1
			if v < 127:
				tnotes += " %s/%02X:%d(%d)" % (notename(t), v, l, num_notes)
			else:
				tnotes += " %s:%d(%d)" % (notename(t), l, num_notes)
		if music.n_delay_tracks > 0 and ti == 0:
			print "Tracks with delay:"
			print
		if music.n_delay_tracks > 0 and ti == music.n_delay_tracks:
			print
			print "Tracks without delay:"
			print
		print track.title
		print tnotes
	#print "Max: longest %f, total %f" % (music.max_longest_sample, music.max_sample_length_sum)

def writefile(filename, s):
	f = open(filename, "wb")
	f.write(s)
	f.close()
	print "Wrote file %s" % filename


if len(sys.argv) < 3:
	print "Usage: %s <input xrns file> <output asm file>" % sys.argv[0]
	sys.exit(1)

infile = sys.argv[1]
outfile = sys.argv[2]

x = clinksterxml.makeXML(zipfile.ZipFile(infile).read("Song.xml"))
try:
	music = makeMusic(x.RenoiseSong)
	print
	printMusicStats(music)
	print

	writefile(outfile, music.exportPC(44100.0))

	if len(sys.argv) > 3:
		deltas = music.makeDeltas(0.0, 1.0)
		syncfile = sys.argv[3]
		header = ""
		header += struct.pack('I', 1)
		header += struct.pack('I', music.length*4)
		header += struct.pack('I', len(music.tracks)*music.length*4)
		body = ""
		for t,tdeltas in enumerate(deltas):
			body += struct.pack("%df" % len(tdeltas), *tdeltas)
		data = header + body
		writefile(syncfile, data)

except InputException, e:
	print "Error in input song: %s" % e.message

