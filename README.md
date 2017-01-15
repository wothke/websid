/*
* WebSid (Web version of Tiny'R'Sid - see live demo: http://www.wothke.ch/websid/)
* ======
*
* original TinySid PSID emulator code (fragments may still remain in cpu.cpp and sid.cpp)
*	Copyright (C) 1999-2012 T. Hinrichs, R. Sinsch (original licensing applies)
*
* Tiny'R'Sid PSID & RSID emulator extensions
*   Copyright (C) 2017 Juergen Wothke 
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/


This is a JavaScript/WebAudio plugin of Tiny'R'Sid. This plugin is designed to work with version 1.0 of my 
generic WebAudio ScriptProcessor music player (see separate project). 

It allows to play RSID and PSID format *.sid music files. (Respective music files can be found here: http://www.hvsc.c64.org/)

There also is a plugin that allows to easily integrate the player into WordPress: https://wordpress.org/plugins/tinyrsid-adapter/

Howto build:

You'll need Emscripten; see http://kripken.github.io/emscripten-site/docs/getting_started/downloads.html (I did not 
need to perform ANY additions or manual changes on the installation). The below instructions assume a command prompt has been 
opened within the "websid" folder, and that the Emscripten environment vars have been set (run emsdk_env.bat).

Running the makeEmscripten.bat will generate a JavaScript 'Tiny'R'Sid' library (backend_tinyrsid.js) including 
necessary interface APIs. This lib is directly written into the web-page example in the "htdocs" sub-folder. (This 
generated lib is used from some manually written JavaScript/WebAudio code, see htdocs/stdlib/scriptprocessor_player.min.js). 
Study the example in "htdocs" for how the player is used.
