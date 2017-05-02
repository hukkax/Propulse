# Propulse Tracker

A crossplatform tracker for making Amiga ProTracker compatible
tracker modules using an Impulse/Schism Tracker style user interface.

Currently tested to build and run on Windows, Linux and macOS.

Features:

- Super accurate playback engine based on work by 8bitbubsy (itself based on a disassembly of the original Amiga ProTracker); things like black_queen.mod and MPT test cases play correctly. You can also choose whether the EFx effect should be played like in ProTracker or like the PT playroutine plays it
- Familiar Impulse/Schism Trackerish interface with familiar keyboard commands
- User configurable keybindings, colors, fonts of any size, even screen layouts...
- Show and edit effects in either Impulse or ProTracker format (e.g. Dxy vs. Axy)
- WAV export with optional looping and fade out
- Integrated mouse-driven sample editor 

Supported formats:

- MOD - Loads and saves Amiga ProTracker modules (including load support for 15-sample Ultimate SoundTracker mods, NoiseTracker, and PowerPacked files)
- P61A - Imports The Player 6.1a crunched modules
- IT - Imports Impulse Tracker modules
- Samples: IFF 8SVX, WAV, MP3, Ogg Vorbis, raw
