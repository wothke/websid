/*
 tinyrsid_adapter.js: Adapts Tiny'R'Sid backend to generic WebAudio/ScriptProcessor player.
 
 version 1.01
 
 	Copyright (C) 2018 Juergen Wothke

 LICENSE
 
 This software is licensed under a CC BY-NC-SA 
 (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
SIDBackendAdapter = (function(){ var $this = function () { 
		$this.base.call(this, backend_SID.Module, 1);
		this.playerSampleRate;
		
		this._scopeEnabled= false;
	}; 
	// TinyRSid's sample buffer contains 2-byte (signed short) sample data 
	// for 1 channel
	extend(EmsHEAP16BackendAdapter, $this, {
		enableScope: function(enable) {
			this._scopeEnabled= enable;
		},		
		getAudioBuffer: function() {
			var ptr=  this.Module.ccall('getSoundBuffer', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getAudioBufferLength: function() {
			var len= this.Module.ccall('getSoundBufferLen', 'number');
			return len;
		},
		printMemDump: function(name, startAddr, endAddr) {	// util for debugging
			var text= "const unsigned char "+name+"[] =\n{\n";
			var line= "";
			var j= 0;
			for (var i= 0; i<(endAddr-startAddr+1); i++) {
				var d= this.Module.ccall('getRAM', 'number', ['number'], [startAddr+i]);
				line += "0x"+(("00" + d.toString(16)).substr(-2).toUpperCase())+", ";
				if (j  == 11) {						
					text+= (line + "\n");
					line= "";
					j= 0;
				}else {
					j++;
				}
			}		
			text+= (j?(line+"\n"):"")+"}\n";
			console.log(text);
		},
		computeAudioSamples: function() {
			var len= this.Module.ccall('computeAudioSamples', 'number');
			if (len <= 0) {			
				return 1; // >0 means "end song"
			}		
			return 0;	
		},
		getPathAndFilename: function(filename) {
			return ['/', filename];
		},
		registerFileData: function(pathFilenameArray, data) {
			return 0;	// FS not used in Tiny'R'Sid
		},
		loadMusicData: function(sampleRate, path, filename, data, options) {
			var buf = this.Module._malloc(data.length);
			this.Module.HEAPU8.set(data, buf);
			
			// try to use native sample rate to avoid resampling
			this.playerSampleRate= (typeof window._gPlayerAudioCtx == 'undefined') ? 0 : window._gPlayerAudioCtx.sampleRate;	
			
			var isMus= filename.endsWith(".mus") || filename.endsWith(".str");	// Compute! Sidplayer file (stereo files not supported)
			var ret = this.Module.ccall('loadSidFile', 'number', ['number', 'number', 'number', 'number'], [isMus, buf, data.length, this.playerSampleRate]);
			this.Module._free(buf);

			if (ret == 0) {
				this.playerSampleRate = this.Module.ccall('getSampleRate', 'number');
				this.resetSampleRate(sampleRate, this.playerSampleRate); 
			}
			return ret;			
		},
		evalTrackOptions: function(options) {
			if (typeof options.timeout != 'undefined') {
				ScriptNodePlayer.getInstance().setPlaybackTimeout(options.timeout*1000);
			}
			var traceSID= this._scopeEnabled;
			if (typeof options.traceSID != 'undefined') {
				traceSID= options.traceSID;
			}
			if (typeof options.track == 'undefined') {
				options.track= -1;
			}
			return this.Module.ccall('playTune', 'number', ['number', 'number'], [options.track, traceSID]);
		},
		teardown: function() {
			// nothing to do
		},
		getSongInfoMeta: function() {
			return {			
					loadAddr: Number,
					playSpeed: Number,
					maxSubsong: Number,
					actualSubsong: Number,
					songName: String,
					songAuthor: String, 
					songReleased: String 
					};
		},
		getExtAsciiString: function(heapPtr) {
			// Pointer_stringify cannot be used here since UTF-8 parsing 
			// messes up original extASCII content
			var text="";
			for (var j= 0; j<32; j++) {
				var b= this.Module.HEAP8[heapPtr+j] & 0xff;
				if(b ==0) break;
				
				if(b < 128){
					text = text + String.fromCharCode(b);
				} else {
					text = text + "&#" + b + ";";
				}
			}
			return text;
		},
		updateSongInfo: function(filename, result) {
		// get song infos (so far only use some top level module infos)
			var numAttr= 7;
			var ret = this.Module.ccall('getMusicInfo', 'number');
						
			var array = this.Module.HEAP32.subarray(ret>>2, (ret>>2)+7);
			result.loadAddr= this.Module.HEAP32[((array[0])>>2)]; // i32
			result.playSpeed= this.Module.HEAP32[((array[1])>>2)]; // i32
			result.maxSubsong= this.Module.HEAP8[(array[2])]; // i8
			result.actualSubsong= this.Module.HEAP8[(array[3])]; // i8
			result.songName= this.getExtAsciiString(array[4]);			
			result.songAuthor= this.getExtAsciiString(array[5]);
			result.songReleased= this.getExtAsciiString(array[6]);			
		},
		// for debugging.. disable voices (0-3) by clearing respective bit
		enableVoices: function(mask) {
			this.Module.ccall('enableVoices', 'number', ['number'], [mask]);
		},
		
		// C64 emu specific accessors (that might be useful in GUI)
		isSID6581: function() {
			return this.Module.ccall('envIsSID6581', 'number');
		},
		setSID6581: function(is6581) {
			this.Module.ccall('envSetSID6581', 'number', ['number'], [is6581]);
		},
		isNTSC: function() {
			return this.Module.ccall('envIsNTSC', 'number');
		},
		setNTSC: function(ntsc) {
			this.Module.ccall('envSetNTSC', 'number', ['number'], [ntsc]);
		},
		
		// To activate the below output a song must be started with the "traceSID" option set to 1:
		// At any given moment the below getters will then correspond to the output of getAudioBuffer
		// and what has last been generated by computeAudioSamples. They expose some of the respective
		// underlying internal SID state (the "filter" is NOT reflected in this data).
		getNumberTraceStreams: function() {
			return this.Module.ccall('getNumberTraceStreams', 'number');			
		},
		getTraceStreams: function() {
			var result= [];
			var n= this.getNumberTraceStreams();

			var ret = this.Module.ccall('getTraceStreams', 'number');			
			var array = this.Module.HEAP32.subarray(ret>>2, (ret>>2)+n);
			
			for (var i= 0; i<n; i++) {
				result.push(array[i] >> 1);	// pointer to int16 array
			}
			return result;
		},

		readFloatTrace: function(buffer, idx) {
			return (this.Module.HEAP16[buffer+idx])/0x8000;
		},

		/**
		* This just queries the *current* state of the emulator. It
		* is less precisely correlated to the music that is currently playing (than the above
		* buffers), i.e. it represents the state *after* the last emulator call (respective data
		* may not yet have been fed to WebAudio or if it has already been fed then 
		* WebAudio may not yet be playing it yet). The lag should normally not be very large 
		* (<0.2s) and when using it for display purposes it would be hard to see a difference anyway.
		*/
		getRegisterSID: function(offset) {
			return this.Module.ccall('getRegisterSID', 'number', ['number'], [offset]);
		},
		getRAM: function(offset) {
			return this.Module.ccall('getRAM', 'number', ['number'], [offset]);
		},
		/**
		* Diagnostics digi-samples (if any).
		*/
		getDigiType: function() {
			return this.Module.ccall('getDigiType', 'number');
		},
		getDigiTypeDesc: function() {
			return this.getExtAsciiString(this.Module.ccall('getDigiTypeDesc', 'number'));
		},
		getDigiRate: function() {
			return this.Module.ccall('getDigiRate', 'number');
		},
		
		/*
		* @deprecated APIs below - use getTraceStreams/getNumberTraceStreams instead
		*/
		getBufferVoice1: function() {
			var ptr=  this.Module.ccall('getBufferVoice1', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getBufferVoice2: function() {
			var ptr=  this.Module.ccall('getBufferVoice2', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getBufferVoice3: function() {
			var ptr=  this.Module.ccall('getBufferVoice3', 'number');			
			return ptr>>1;	// 16 bit samples			
		},
		getBufferVoice4: function() {
			var ptr=  this.Module.ccall('getBufferVoice4', 'number');			
			return ptr>>1;	// 16 bit samples			
		},				
	});	return $this; })();
	