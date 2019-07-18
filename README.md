# WebSid (WebAudio version of Tiny'R'Sid)

This is a JavaScript/WebAudio based C64 emulator / music player. This plugin is designed to work with version 1.0 of my 
generic WebAudio ScriptProcessor music player (see separate project). 

It allows to play .mus and RSID/PSID format *.sid music files. (Respective music files can be found here: http://www.hvsc.c64.org/)

There also is a WordPress plugin that allows to easily integrate the player: https://wordpress.org/plugins/tinyrsid-adapter/


This current version uses a cycle-by-cycle emulation approach (i.e. it has been substantially rewritten). As compared 
to the previously used "predictive" approach it allows for a much simpler implementation and it simply avoids most of 
the problems that had haunted the old implementation (e.g. correct timing of ADSR-delay bug, etc). Unfortunately the 
new implementation is computationally much more expensive (50-100% more CPU).

The emulator logic is written in C/C++. This code is then cross-compiled into a JavaScript library suitable for the Web.

Known limitations: The emulator does not include the Commodore ROM code and music programs written in BASIC therefore are not 
supported. Also VIC timing with regard to the use of "sprites" is not modelled. There are very few songs that actually depend 
on respective "sprite" timing delays (e.g. Vicious_SID_2-15638Hz.sid - which now plays too fast due to the "missing" delays) 
but most songs do not, and there is no point in making the emulation more CPU intensive for everybody else. 


![alt text](https://github.com/wothke/websid/raw/master/tinyrsid.jpg "Tiny'R'Sid HVSC Explorer")

An online demo of the emulator can be accessed using: http://www.wothke.ch/tinyrsid/index.php/explorer?playlistId=103 or http://www.wothke.ch/websid/


## Credits

* old/original TinySid PSID emulator code (there is not much left of it..) - Copyright (C) 1999-2012 T. Hinrichs, R. Sinsch
* "combined waveform" generation, "waveform anti-aliasing" and "filter" implementation by Hermit (see http://hermit.sidrip.com)
* based on various reverse engineering information by: Christian Bauer, Wolfgang Lorenz, resid team, etc


## Howto build

You'll need Emscripten; see http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html . In case you want to 
compile to WebAssembly (see respective "WASM" switch in the make-scripts) then you'll need at least emscripten 1.37.29 (for
use of WASM you'll then also need a recent version of my respective "WebAudio ScriptProcessor music player").

The below instructions assume a command prompt has been opened within the "websid" folder, and that the Emscripten environment 
vars have been set (run emsdk_env.bat).

Running the makeEmscripten.bat will generate a JavaScript 'Tiny'R'Sid' library (backend_tinyrsid.js - and a separate tinyrsid.wasm file
if you are compiling to WebAssembly) including necessary interface APIs. This output is directly written into the web-page example in 
the "htdocs" sub-folder. (This generated lib is used from some manually written JavaScript/WebAudio code, see 
htdocs/stdlib/scriptprocessor_player.min.js). Study the example in "htdocs" for how the player is used. 

Running the makeEmscriptenTest.bat will generate test version which is directly written into the web-page example in 
the "htdocs_test" sub-folder (the respective page then allows to run the various tests from Wolfgang Lorenz's "test-suite").

Disclaimer: the .sh version of the make-script has been contributed by somebody else and I am not maintaing it or verifying that it still works.

Note: Due to some bug in Apple's JavaScriptCore (as of iOS 11.4) the regularily built version will not work on iOS devices. As
a workaround you can compile using "-s SAFE_HEAP=1". This will make for slower code but it seems to be the only way around 
the iCrap bug.

## Depencencies

The current version requires version 1.02 (older versions will not
support WebAssembly) of my https://github.com/wothke/webaudio-player (use of the most recent version is recommended).

## License
Terms of Use: This software is licensed under a CC BY-NC-SA (http://creativecommons.org/licenses/by-nc-sa/4.0/)
	