/*
 tinyrsid_adapter.js: Adapts Tiny'R'Sid backend to generic WebAudio/ScriptProcessor player.
 
 version 1.02
 
 	Copyright (C) 2020 Juergen Wothke

 LICENSE
 
 This software is licensed under a CC BY-NC-SA 
 (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/
SIDBackendAdapter = (function(){ var $this = function (basicROM, charROM, kernalROM, nextFrameCB) { 
		$this.base.call(this, backend_SID.Module, 2);	// use stereo (for the benefit of multi-SID songs)
		this.playerSampleRate;
		
		this._scopeEnabled= false;

		this._chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
		this._ROM_SIZE= 0x2000;
		this._CHAR_ROM_SIZE= 0x1000;
		
		this._nextFrameCB= (typeof nextFrameCB == 'undefined') ? this.nopCB : nextFrameCB;
		
		this._basicROM= this.base64DecodeROM(basicROM, this._ROM_SIZE);
		this._charROM= this.base64DecodeROM(charROM, this._CHAR_ROM_SIZE);
		this._kernalROM= this.base64DecodeROM(kernalROM, this._ROM_SIZE);
		
		this._digiShownLabel= "";
		this._digiShownRate= 0;
		
		this.resetDigiMeta();
	}; 
	// TinyRSid's sample buffer contains 2-byte (signed short) sample data 
	// for 1 channel
	extend(EmsHEAP16BackendAdapter, $this, {
		nopCB: function() {
		},
		resetDigiMeta: function() {
			this._digiTypes= {};
			this._digiRate= 0;
			this._digiBatches= 0;
			this._digiEmuCalls= 0;
		},
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
		updateDigiMeta: function() {
			// get a "not so jumpy" status describing digi output
			
			var dTypeStr= this.getExtAsciiString(this.Module.ccall('getDigiTypeDesc', 'number'));
			var dRate= this.Module.ccall('getDigiRate', 'number');
			// "computeAudioSamples" correspond to 50/60Hz, i.e. to show some
			// status for at least half a second, labels should only be updated every
			// 25/30 calls..

			if (!isNaN(dRate) && dRate) {
				this._digiBatches++;
				this._digiRate+= dRate;
				this._digiTypes[dTypeStr]= 1;	// collect labels
			}
			
			this._digiEmuCalls++;
			if (this._digiEmuCalls == 20) {
				this._digiShownLabel= "";
				
				if (!this._digiBatches) {
					this._digiShownRate= 0;
				} else {
					this._digiShownRate= Math.round(this._digiRate/this._digiBatches);
					
					const arr = Object.keys(this._digiTypes).sort();
					for (const key of arr) {
						if (key.length && (key != "NONE"))
							this._digiShownLabel+= (this._digiShownLabel.length ? "&"+key : key);
					}
				}
				this.resetDigiMeta();
			}
		},
		computeAudioSamples: function() {
			/*
			if (typeof window.sid_measure_runs == 'undefined') {
				window.sid_measure_sum= 0;
				window.sid_measure_runs= 0;
			}
			var t = performance.now();
			*/
//			console.profile(); // if compiled using "emcc.bat --profiling"
			
			this._nextFrameCB(this);	// used for "interactive mode"
			
			var len= this.Module.ccall('computeAudioSamples', 'number');
			if (len <= 0) {
				this.resetDigiMeta();
				return 1; // >0 means "end song"
			}
			this.updateDigiMeta();
			
//			console.profileEnd();
			/*
			window.sid_measure_sum+= performance.now() - t;
			if (window.sid_measure_runs++ == 100) {
				console.log("time; " + window.sid_measure_sum/window.sid_measure_runs);
				window.sid_measure_sum = window.sid_measure_runs = 0;
			}
			*/
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
			
			var basicBuf= 0;
			if (this._basicROM) { basicBuf = this.Module._malloc(this._ROM_SIZE); this.Module.HEAPU8.set(this._basicROM, basicBuf);}
			
			var charBuf= 0;
			if (this._charROM) { charBuf = this.Module._malloc(this._CHAR_ROM_SIZE); this.Module.HEAPU8.set(this._charROM, charBuf);}
			
			var kernalBuf= 0;
			if (this._kernalROM) { kernalBuf = this.Module._malloc(this._ROM_SIZE); this.Module.HEAPU8.set(this._kernalROM, kernalBuf);}
			
			// try to use native sample rate to avoid resampling
			this.playerSampleRate= (typeof window._gPlayerAudioCtx == 'undefined') ? 0 : window._gPlayerAudioCtx.sampleRate;	
			
			var isMus= filename.endsWith(".mus") || filename.endsWith(".str");	// Compute! Sidplayer file (stereo files not supported)
			var ret = this.Module.ccall('loadSidFile', 'number', ['number', 'number', 'number', 'number', 'string', 'number', 'number', 'number'], 
										[isMus, buf, data.length, this.playerSampleRate, filename, basicBuf, charBuf, kernalBuf]);

			if (kernalBuf) this.Module._free(kernalBuf);
			if (charBuf) this.Module._free(charBuf);
			if (basicBuf) this.Module._free(basicBuf);
			
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
			this.resetDigiMeta();
		
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

		// XXX FIXME UNUSED?
		readFloatTrace: function(buffer, idx) {
			return (this.Module.HEAP16[buffer+idx])/0x8000;
		},
		// perf optimization: XXX FIXME UNUSED?
		getCopiedScopeStream: function(input, len, output) {
			for(var i= 0; i<len; i++){
				output[i]=  this.Module.HEAP16[input+i]; // will be scaled later anyway.. avoid the extra division here /0x8000;
			}
			return len;
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
		setRegisterSID: function(offset, value) {
			this.Module.ccall('setRegisterSID', 'number', ['number', 'number'], [offset, value]);
		},
		/**
		* Diagnostics digi-samples (if any).
		*/
		getDigiType: function() {
			return this.Module.ccall('getDigiType', 'number');
		},
		getDigiTypeDesc: function() {
			return this._digiShownLabel;
		},
		getDigiRate: function() {
			return this._digiShownRate;
		},
		enableVoice: function(sidIdx, voice, on) {
			this.Module.ccall('enableVoice', 'number', ['number', 'number', 'number'], [sidIdx, voice, on]);
		},

		
		/*
		* @deprecated APIs below - use getTraceStreams/getNumberTraceStreams instead
		*/
		// disable voices (0-3) by clearing respective bit
		enableVoices: function(mask) {
			this.Module.ccall('enableVoices', 'number', ['number'], [mask]);
		},
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
		// base64 decoding util
		findChar: function(str, c) {
			for (var i= 0; i<str.length; i++) {
				if (str.charAt(i) == c) {
					return i;
				}
			}
			return -1;
		},
		alphanumeric: function(inputtxt) {
			var letterNumber = /^[0-9a-zA-Z]+$/;
			return inputtxt.match(letterNumber);
		},
		is_base64: function(c) {
		  return (this.alphanumeric(""+c) || (c == '+') || (c == '/'));
		},
		base64DecodeROM: function(encoded, romSize) {
			if (typeof encoded == 'undefined') return 0;
			
			var in_len= encoded.length;
			var i= j= in_= 0;
			var arr4= new Array(4);
			var arr3= new Array(3);
			
			var ret= new Uint8Array(romSize);
			var ri= 0;

			while (in_len-- && ( encoded.charAt(in_) != '=') && this.is_base64(encoded.charAt(in_))) {
				arr4[i++]= encoded.charAt(in_); in_++;
				if (i ==4) {
					for (i = 0; i <4; i++) {
						arr4[i] = this.findChar(this._chars, arr4[i]);
					}
					arr3[0] = ( arr4[0] << 2       ) + ((arr4[1] & 0x30) >> 4);
					arr3[1] = ((arr4[1] & 0xf) << 4) + ((arr4[2] & 0x3c) >> 2);
					arr3[2] = ((arr4[2] & 0x3) << 6) +   arr4[3];

					for (i = 0; (i < 3); i++) {
						var val= arr3[i];
						ret[ri++]= val;
					}
					i = 0;
				}
			}
			if (i) {
				for (j = 0; j < i; j++) {
					arr4[j] = this.findChar(this._chars, arr4[j]);
				}
				arr3[0] = (arr4[0] << 2) + ((arr4[1] & 0x30) >> 4);
				arr3[1] = ((arr4[1] & 0xf) << 4) + ((arr4[2] & 0x3c) >> 2);

				for (j = 0; (j < i - 1); j++) {
					var val= arr3[j];
					ret[ri++]= val;
				}
			}
			if (ri == romSize) {
				return ret;
			}
			return 0;
		},
	});	return $this; })();
	