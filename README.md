/*
* WebSid (Web version of Tiny'R'Sid - see live demo: http://www.wothke.ch/websid/)
* ======
*
* original TinySid PSID emulator code (see sidengine.c)
*	Copyright (C) 1999-2012 T. Hinrichs, R. Sinsch (original licensing applies)
*
* Tiny'R'Sid PSID & RSID emulator extensions
*   Copyright (C) 2013 Juergen Wothke 
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

Howto build:

You'll need Emscripten (I used the win installer on WinXP: emsdk-1.13.0-full-32bit.exe which could be found here: 
http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html) I did not need to perform 
ANY additions or manual changes on the installation. The below instructions assume a command prompt has been 
opened within the "websid" folder, and that the Emscripten environment vars have been set (run emsdk_env.bat).

Running the makeEmscripten.bat will generate a JavaScript 'Tiny'R'Sid' library (tinyrsid.js) including necessary 
interface APIs. This lib is directly written into the web-page example in the "htdocs" sub-folder. (This generated lib is 
used from some manually written JavaScript/WebAudio code, see htdocs/tinyrsid_player.js). Study the example in "htdocs" 
for how the player is used.
