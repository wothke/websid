#!/bin/sh
emcc -s VERBOSE=0 -Wno-pointer-sign -I./src -Os -O3 --closure 1 --llvm-lto 1 -s NO_FILESYSTEM=1 src/hacks.c src/nanocia.c src/nanovic.c src/rsidengine.c src/sidengine.c src/sidplayer.c -s EXPORTED_FUNCTIONS="['_alloc','_loadSidFile', '_playTune', '_getMusicInfo', '_getSampleRate', '_getSoundBuffer', '_getSoundBufferLen', '_computeAudioSamples']" -o htdocs/tinyrsid2.html && cat shell-pre.js htdocs/tinyrsid2.js shell-post.js > htdocs/tinyrsid.js && rm htdocs/tinyrsid2.html && rm htdocs/tinyrsid2.js && cat htdocs/tinyrsid.js tinyrsid_adapter.js > htdocs/backend_tinyrsid.js

