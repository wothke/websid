emcc.bat -s VERBOSE=0 -Wno-pointer-sign -I./src -Os -O3 --closure 1 --llvm-lto 1 -s NO_FILESYSTEM=1 src/hacks.c src/nanocia.c src/nanovic.c src/rsidengine.c src/sidengine.c src/sidplayer.c -s EXPORTED_FUNCTIONS="['_alloc','_loadSidFile', '_playTune', '_getSoundBuffer', '_computeAudioSamples']" -o htdocs/tinyrsid.html && del htdocs\tinyrsid.html



