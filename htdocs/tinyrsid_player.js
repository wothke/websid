/**
* tinyrsid_player.js
*
* 	Copyright (C) 2014 Juergen Wothke
*
* Terms of Use: This software is licensed under a CC BY-NC-SA 
* (http://creativecommons.org/licenses/by-nc-sa/4.0/).
*/

const SAMPLES_PER_BUFFER = 8192;	// allowed: buffer sizes: 256, 512, 1024, 2048, 4096, 8192, 16384

var fetchSamples= function(e) { 	
	// it seems that it is necessary to keep this explicit reference to the event-handler
	// in order to pervent the dumbshit Chrome GC from detroying it eventually
	
	var f= window.player['generateSamples'].bind(window.player); // need to re-bind the instance.. after all this 
																	// joke language has no real concept of OO	
	f(e);
}

SamplePlayer = function(onEnd) {
	this.onEnd= onEnd;
	
	try {
		window.AudioContext = window.AudioContext||window.webkitAudioContext;
		this.sampleRate = new AudioContext().sampleRate;
	} catch(e) {
		alert('Web Audio API is not supported in this browser (get Chrome 18 or Firefox 26)');
	}	
	this.playSpeed;
	this.maxSubsong;
	this.actualSubsong;
	this.songName;
	this.songAuthor;
	this.songReleased;
	this.sourceBuffer;

	this.sourceBuffer;	// NTSC: 735*8 PAL: 882*8
	this.resampleBuffer= new Float32Array(882*8 *this.sampleRate/44100);
	this.numberOfSamplesRendered= 0;
	this.numberOfSamplesToRender= 0;
	this.sourceBufferIdx=0;

	this.isPaused= false;
	this.currentTimeout= -1;
	this.newSampleRate= -1;

	window.player= this;
};

SamplePlayer.prototype = {
	getDefaultSampleRate: function() {
		return new AudioContext().sampleRate;
	},
	setSampleRate: function(s) {
		this.newSampleRate= s;
	},
	playSong: function(data, track, timeout) {
		this.isPaused= true;
		this.loadData(data);  
		this.selectSong(track<0?this.actualSubsong: track);
		this.currentTimeout= timeout;
		this.currentPlaytime= 0;
		
		this.isPaused= 0;
	},
	setPauseMode: function (pauseOn) {
		this.isPaused= pauseOn;
	},
	createScriptProcessor: function(audioCtx) {
		var scriptNode = audioCtx.createScriptProcessor(SAMPLES_PER_BUFFER, 0, 1);
		scriptNode.onaudioprocess = fetchSamples;
		return scriptNode;
	},
	loadData: function(arrayBuffer) {
	  if (arrayBuffer) {
		var byteArray = new Uint8Array(arrayBuffer);

		var buf = Module._malloc(byteArray.length);
		Module.HEAPU8.set(byteArray, buf);
		var ret = Module.ccall('loadSidFile', 'number', ['number', 'number'], [buf, byteArray.length]);
		Module._free(buf);
														
		var array = Module.HEAP32.subarray(ret>>2, (ret>>2)+7);
		var loadAddr= Module.HEAP32[((array[0])>>2)]; // i32
		this.playSpeed= Module.HEAP32[((array[1])>>2)]; // i32
		this.maxSubsong= Module.HEAP8[(array[2])]; // i8
		this.actualSubsong= Module.HEAP8[(array[3])]; // i8
		this.songName= Module.Pointer_stringify(array[4]);
		this.songAuthor= Module.Pointer_stringify(array[5]);
		this.songReleased= Module.Pointer_stringify(array[6]);
		
		this.sourceBuffer= Module.ccall('getSoundBuffer', 'number');
	  }
	},
	selectSong: function(id) {
		Module.ccall('playTune', 'number', ['number'], [id]);
	},
	getResampledAudio: function(len) {
		var resampleLen= Math.round(len *this.sampleRate/44100);
		
		if (resampleLen > this.resampleBuffer.length) this.resampleBuffer= new Float32Array(resampleLen);
		
		// Bresenham algorithm based resampling
		var x0= 0;
		var y0= 0;
		var x1= resampleLen;
		var y1= len;

		var dx =  Math.abs(x1-x0), sx = x0<x1 ? 1 : -1;
		var dy = -Math.abs(y1-y0), sy = y0<y1 ? 1 : -1;
		var err = dx+dy, e2;

		var srcBufI32= this.sourceBuffer>>2;
		for(;;){
			this.resampleBuffer[x0]= Module.HEAP32[srcBufI32+y0]/(0x8000);

			if (x0>=x1 && y0>=y1) break;
			e2 = 2*err;
			if (e2 > dy) { err += dy; x0 += sx; }
			if (e2 < dx) { err += dx; y0 += sy; }
		}
		return resampleLen;
	},
	resetSampleRate: function() {
		if (this.newSampleRate > 0) {
			this.sampleRate= this.newSampleRate;
						
			var s= 882*8 *this.sampleRate/44100;
			
			if (s > this.resampleBuffer.length)
				this.resampleBuffer= new Float32Array(s);
				
			this.numberOfSamplesRendered= 0;
			this.numberOfSamplesToRender= 0;
			this.sourceBufferIdx=0;
			
			this.newSampleRate= -1;
		}
	},
	generateSamples: function(event) {
		this.resetSampleRate();	// perform here to avoid "concurrency" issues
		
		var output = event.outputBuffer.getChannelData(0);

		if (this.isPaused) {
			var i;
			for (i= 0; i<output.length; i++) {
				output[i]= 0;
			}		
		} else {
			var outSize= output.length;
			this.numberOfSamplesRendered = 0;		
			
			while (this.numberOfSamplesRendered < outSize)
			{
				if (this.numberOfSamplesToRender == 0) {				
					// tinyrid backend is designed for fixed 44100 frequency so we may need to resample..
					var len = Module.ccall('computeAudioSamples', 'number');
					this.numberOfSamplesToRender = this.getResampledAudio(len);					
					this.sourceBufferIdx=0;			
					
					this.currentPlaytime+= len;
					
					if ((this.currentTimeout>0) && this.currentPlaytime/44100 > this.currentTimeout) {
						this.isPaused= true;
						if (this.onEnd) this.onEnd();
						break;
					}					
				}

				if (this.numberOfSamplesRendered + this.numberOfSamplesToRender > outSize) {
					var availableSpace = outSize-this.numberOfSamplesRendered;
					
					var i;
					for (i= 0; i<availableSpace; i++) {
						output[i+this.numberOfSamplesRendered]= this.resampleBuffer[this.sourceBufferIdx++];
					}				
					this.numberOfSamplesToRender -= availableSpace;
					this.numberOfSamplesRendered = outSize;
				} else {
					var i;
					for (i= 0; i<this.numberOfSamplesToRender; i++) {
						output[i+this.numberOfSamplesRendered]= this.resampleBuffer[this.sourceBufferIdx++];
					}						
					this.numberOfSamplesRendered += this.numberOfSamplesToRender;
					this.numberOfSamplesToRender = 0;
				} 
			}
		}	
	}	
};




