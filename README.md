# WebSid (WebAudio version of Tiny'R'Sid)

This is a JavaScript/WebAudio based C64 emulator version of Tiny'R'Sid. This plugin is designed to work with version 1.0 of my 
generic WebAudio ScriptProcessor music player (see separate project). 

It allows to play RSID and PSID format *.sid music files. (Respective music files can be found here: http://www.hvsc.c64.org/)

There also is a WordPress plugin that allows to easily integrate the player: https://wordpress.org/plugins/tinyrsid-adapter/

![alt text](https://github.com/wothke/websid/raw/master/tinyrsid.jpg "Tiny'R'Sid HVSC Explorer")

An online demo of the emulator can be accessed using: http://www.wothke.ch/tinyrsid/index.php/explorer?playlistId=103 or http://www.wothke.ch/websid/


## Credits

* original TinySid PSID emulator code (there is not much left of it..) - Copyright (C) 1999-2012 T. Hinrichs, R. Sinsch
* "combined waveform" generation, "waveform anti-aliasing" and "filter" implementation by Hermit (see http://hermit.sidrip.com)
* various Tiny'R'Sid PSID & RSID emulator extensions - Copyright (C) 2017 Juergen Wothke 

Terms of Use: This software is licensed under a CC BY-NC-SA (http://creativecommons.org/licenses/by-nc-sa/4.0/)


## Howto build:

You'll need Emscripten; see http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html (I did not 
need to perform ANY additions or manual changes on the installation). The below instructions assume a command prompt has been 
opened within the "websid" folder, and that the Emscripten environment vars have been set (run emsdk_env.bat).

Running the makeEmscripten.bat will generate a JavaScript 'Tiny'R'Sid' library (backend_tinyrsid.js) including 
necessary interface APIs. This lib is directly written into the web-page example in the "htdocs" sub-folder. (This 
generated lib is used from some manually written JavaScript/WebAudio code, see htdocs/stdlib/scriptprocessor_player.min.js). 
Study the example in "htdocs" for how the player is used.


## Change log 

(earlier versions were designed for use of Alchemy/Flash - with the rise of HTML5 I switched to WebAudio)

version 0.82  - merged some of Hermit's work into the sid emulation

version 0.81  - refactored building blocks / various cleanups (dependencies, visibility, naming conventions)

version 0.8   - replaced original envelope generator with new impl that also handles ADSR-bug, fixed bugs in 
                6502 emu (additional ILLEGAL ops, wrong V-flag calculation), refactored digi-sample handling, 
                various cleanups, removed Storebror.sid hack (no longer needed)

version 0.77: - refactored & improved nanocia.c to fix issues with some of the songs of Kris Hatlelid; added hack  
                for mixed raster/timer IRQ scenarios; added Lame_Tune.sid hack; DC0D polling hack to make Kai Walter 
                stuff play; added D41B polling hack (e.g. Ring_Ring_Ring.sid), added support for main-loop sid play (e.g. Dane's 'Crush.sid');
                added handling for 60hz songs; removed d012 polling specific hacks and improved d011/d012 handling (see 
                Swallow's stuff); fixed original TinySid filter bug (see Dancing_in_the_Moonshine.sid)
				
version 0.76: - added poor man's combined pulse/triangle waveform impl: for songs like Kentilla.sid; improved IRQ digi 
                filter to fix noise in Coma_Light_13_tune_4.sid; added handling for "pulse width modulation" 
                digis as used by Swallow, Cyberbrain, Danko_Tomas, etc

version 0.75: - added poor man's badline cycle handling in sidplayer.c - as a workaround for noise artefacts in THCM's new stuff
              - added "new digi" handling for "main" (see Wonderland_XI-End.sid) - also see SEI handling

version 0.74: - fixed sizzling noise (e.g. Christoph Bergmann's tunes) due to bug in digi-scaling (see "genDigi()")
                added TOD sim, fixed 'memset' bug, added minimal support for rasterline polling, added 
                NTSC playback speed adjustment, added impl for "Voice #3 ADSR output"

version 0.73: - fixed digi sizzling noise caused by main prog, added 'sbx' op for Artefacts.sid

version 0.72: - fixed "IO area" handling bug which had corrupted RAM (see Digital_Music.sid problems)
              - added handling for "test bit" driven digis (see Storebror.sid)
              - added hack to give Storebror.sif a "valid" $DC06 counter for use in NMI vector
              - added "enough digi samples in main prog" interrupt condition (see Suicide_Express.sid)
              - fixed wrong stack handling
              - added fake timer to make Delta_Mix-E-Load_loader.sid PSID happy 

version 0.71: - added missing RSID $01 handling (fixed IK+)
              - added "read" d012 emulation (fix endless loop in Storebror.sid)
              - added handling for illegal op SLO
              - improved stack/register handling
              - fixed IRQ init for PSID
              - added filtering for "fake" digi signals
              - added $01 handling for faulty PSIDs
              - reset SID synth before each new song.. see GreenBeret.sid problem

version 0.7:  - added poor man's VIC and CIA emulation
              - added interleaving of NMI and IRQ calls

changes 0.6:  - added full 6510 instruction set timing support
              - added NMI and main program based digi-player support

changes 0.5:  - initial version with PSID-only support
              - merged latest TinySid fixes from RockBox version
              - added loadAddress handling for 'non original C64 file format handling'
              - fixed handling of 'sPlaySpeed' flags
              - patched original 'sidSynthRender' PSID digi-playback for louder digi playback			